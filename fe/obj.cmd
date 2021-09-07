// obj.cmd

// Surface Evolver script for creating Wavefront OBJ format 3D graphics file.
// Result file requires EvolverOBJcolors.mtl file in same directory.

// File format reference: http://www.martinreddy.net/gfx/3d/OBJ.spec

// Edges are shown as 4-sided tubes.  The relative tube diameter
// can be adjusted where actual_tube_radius is set below.

/* Usage:
     Set which edges and facets you want to show with the
     "show edge where ..." and "show facet where ..." commands.
     Set obj_double_sided to 1 if you want back sides of facets.
     Set obj_edge_flag to 0 if you want no edges shown.
     Run obj and redirect output to file, for example
         obj >>> "filename.obj"
*/

define vertex attribute obj_vnum integer

obj_double_sided := 0  // set to 1 to do front and back facets
obj_edge_flag := 1    // set to 1 to do small tubes around shown edges
                      // or 0 to do edges as lines
obj_tube_radius := 0.002;  // relative size of edge tubes.

obj_edge_tubes := {

 local tube_sides,t_angle,actual_tube_radius;
 local ax,ay,az,bx,by,bz,mag,inx,cinx;

 // Doing edges one by one, using negative relative vertex indexing, so
 // vertices can be listed with the tube faces.

 tube_sides := 4;
 t_angle := 2*pi/tube_sides;
 actual_tube_radius := obj_tube_radius*sqrt(max(vertex,x^2+y^2+z^2));

 for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
 { printf "usemtl color%d\n",cinx;
   foreach edge ee where color == cinx and show do
   {
     // find two orthogonal directions to edge; if possible,
     // one along adjacent facet
     if ee.valence > 0 then
     { ax := ee.facet[1].x;
       ay := ee.facet[1].y;
       az := ee.facet[1].z;
     }
     else
     { if abs(ee.x) < abs(ee.y) and abs(ee.x) < abs(ee.z) then
       { ax := 1; ay := 0; az := 0;}
       else if abs(ee.y) < abs(ee.z) then
       { ax := 0; ay := 1; az := 0; }
       else
       { ax := 0; ay := 0; az := 1; };
     };
     bx := ay*ee.z - az*ee.y;
     by := az*ee.x - ax*ee.z;
     bz := ax*ee.y - ay*ee.x;
     ax := by*ee.z - bz*ee.y;
     ay := bz*ee.x - bx*ee.z;
     az := bx*ee.y - by*ee.x;
     // normalize to radius
     mag := sqrt(ax^2+ay^2+az^2);
     if mag <= 0 then continue;
     ax *= actual_tube_radius/mag;
     ay *= actual_tube_radius/mag;
     az *= actual_tube_radius/mag;
     mag := sqrt(bx^2+by^2+bz^2);
     if mag <= 0 then continue;
     bx *= actual_tube_radius/mag;
     by *= actual_tube_radius/mag;
     bz *= actual_tube_radius/mag;
  
     for ( inx := 0 ; inx < tube_sides ; inx += 1 )
     {
        printf "v %f %f %f\n",ee.vertex[1].x+ax*cos(inx*t_angle)+bx*sin(inx*t_angle),
           ee.vertex[1].y+ay*cos(inx*t_angle)+by*sin(inx*t_angle),
           ee.vertex[1].z+az*cos(inx*t_angle)+bz*sin(inx*t_angle);
        printf "v %f %f %f\n",ee.vertex[1].x+ee.x+ax*cos(inx*t_angle)+bx*sin(inx*t_angle),
          ee.vertex[1].y+ee.y+ay*cos(inx*t_angle)+by*sin(inx*t_angle),
          ee.vertex[1].z+ee.z+az*cos(inx*t_angle)+bz*sin(inx*t_angle);
     };
     
     /* quads should work, but Meshlab interprets them as single triangles!
     printf "f  -6 -5 -7 -8\n";
     printf "f  -4 -3 -5 -6\n";
     printf "f  -2 -1 -3 -4\n";
     printf "f  -8 -7 -1 -2\n";  
     */
     printf "f  -6 -5 -7\n";
     printf "f  -6 -7 -8\n";
     printf "f  -4 -3 -5\n";
     printf "f  -4 -5 -6\n";
     printf "f  -2 -1 -3\n";
     printf "f  -2 -3 -4\n";
     printf "f  -8 -7 -1\n";
     printf "f  -8 -1 -2\n";  
   }
 }

} // end obj_edge_tubes

// Edges as lines
obj_edge_lines := {
 local cinx;
 for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
 { printf "usemtl color%d\n",cinx;
   foreach edge ee where color == cinx and show do
   {
      printf "l %d %d\n",ee.vertex[1].obj_vnum,
           ee.vertex[2].obj_vnum;
   }
 }

} // end obj_edge_lines

obj := {
  local vnumber,cinx;

  if rgb_colors then
  { errprintf "obj error: obj script does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

  if torus then
  { errprintf "Cannot run 'obj' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'obj' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'obj' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'obj' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'obj' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'obj' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  printf "# OBJ version of %s \n",datafilename;
  printf "# Produced by Surface Evolver with obj.cmd script\n\n";

  printf "mtllib EvolverOBJcolors.mtl\n\n";

  vnumber := 0;
  foreach vertex vv do
  { printf "v %f %f %f\n",vv.x,vv.y,vv.z;
    vnumber += 1;
    vv.obj_vnum := vnumber;
  };
  
  for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
  { printf "usemtl color%d\n",cinx;
    foreach facet ff where show and frontcolor == cinx do
      printf "f %d %d %d\n",ff.vertex[1].obj_vnum,
           ff.vertex[2].obj_vnum,ff.vertex[3].obj_vnum;
    if obj_double_sided then
      foreach facet ff where show and backcolor == cinx do
        printf "f %d %d %d\n",ff.vertex[1].obj_vnum,
           ff.vertex[3].obj_vnum,ff.vertex[2].obj_vnum;
  };

  if obj_edge_flag then
    obj_edge_tubes
  else
    obj_edge_lines;

} // end obj

/* Usage:
     Set which edges and facets you want to show with the
     "show edge where ..." and "show facet where ..." commands.
     Set obj_double_sided to 1 if you want back sides of facets.
     Set obj_edge_flag to 0 if you want no edges shown.
     Run obj and redirect output to file, for example
         obj >>> "filename.obj"
*/
