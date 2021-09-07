// embox.cmd

// Surface Evolver command to create body-enclosing facets around
// outside of foam section created by detorus.

// Usage: set torus mode view to "clipped", run "detorus", then "embox".

/* Assumptions:
     Space dimension 3
     Surface dimension 2
     Foam fills unit cell
     No interior edges with valence 1
     torus_periods and inverse_periods still hold valid values, so
       embox cannot be run on a dump file made after detorus done.
*/

/* WARNING: embox does not nicely handle facets that are exactly on the
   bounding box walls, so if there are any such facets, it is advised
   to move the surface BEFORE doing detorus, i.e.
       clipped;
       set vertex x x+0.1;
       set vertex y y+0.1;
       set vertex z z+0.1;
       detorus;
       embox;
*/

// Some useful attributes
define edge attribute embox_edge_mark integer;
define body attribute embox_body_mark integer;

// Create one face, with given starting edge e_id and starting
// existing facet f_id, to bound body b_id.

procedure embox_face(integer start_edge, integer b_id)
{ local newv,prev_radial,start_radial;
  local newf,dim,per,next_edge,this_edge,ecount;
  local face_type,newv_coord,midpers;
  local head_vertex,new_radial;

  define per real[space_dimension];
  define face_type integer[space_dimension]; 
  define newv_coord real[space_dimension];
  define midpers real[space_dimension];

  face_type := 0; // bitmap for box faces contacted
  newv_coord := 0; // for coordinate of middle vertex of new face
  newv := new_vertex(0,0,0);  // temporary coordinates 
  prev_radial := new_edge(newv,edge[start_edge].vertex[1].id);
  start_radial := prev_radial;

  // Walk around face
  ecount := 0;
  this_edge := start_edge;
  do
  { ecount += 1;
    head_vertex := edge[this_edge].vertex[2].id;
    newv_coord += vertex[head_vertex].__x;
    if head_vertex == edge[start_edge].vertex[1].id then
      new_radial := start_radial
    else 
      new_radial := new_edge(newv,head_vertex);
    newf := new_facet(new_radial,-this_edge,-prev_radial);
    facet[newf].frontbody := b_id;
   
    // mark which bounding box faces contacted
    per := vertex[head_vertex].__x * inverse_periods;
    for ( dim := 1 ; dim <= space_dimension ; dim += 1 )
    { if per[dim] < .00001 then face_type[dim] := -1;
      if per[dim] > .99999 then face_type[dim] := 1;
    };

    next_edge := 0;
    foreach vertex[head_vertex].edge ee where ee.embox_edge_mark and
        ee.oid != -this_edge do
    { foreach ee.facet ff where ff.frontbody == b_id do
      { next_edge := ee.oid;
        break 2;
      }
    };
    if next_edge == 0 then
    { printf "embox error: Cannot find next edge in face loop, body %d.\n",b_id;
      return;
    };
    this_edge := next_edge;
    prev_radial := new_radial;
  } while this_edge != start_edge;

  // Set middle vertex coordinates
  vertex[newv].__x := (1/ecount)*newv_coord;

  // Project middle vertex to proper corner, edge, or face of bounding box
  midpers := vertex[newv].__x * inverse_periods;
  for ( dim := 1 ; dim <= space_dimension ; dim += 1 )
  { if face_type[dim] == -1 then midpers[dim] := 0;  
    if face_type[dim] ==  1 then midpers[dim] := 1;  
  };
  vertex[newv].__x := midpers * torus_periods;


  body[b_id].embox_body_mark := 1;
  return;

} // end embox_face()


embox := {
  local dim,minper,maxper;

  // Check things are reasonable.
  if torus then
  { printf "\nERROR: embox should not be run in torus mode;\n";
    printf "   do detorus in clipped mode first.\n\n";
    return;
  };
  if space_dimension != 3 then
  { printf "\nERROR: embox only implemented for 3 space dimensions;\n";
    return;
  };
  if surface_dimension != 2 then
  { printf "\nERROR: embox only implemented for 2 surface dimensions;\n";
    return;
  };

  // Check we still have torus periods and inverse periods.
  // And check everything is in bounding box.
  for ( dim := 1 ; dim <= space_dimension ; dim += 1 )
  { minper := min(vertex vv, inverse_periods[1][dim]*vv.x +
          inverse_periods[2][dim]*vv.y + inverse_periods[3][dim]*vv.z);
    if minper < -0.0001 then
    { printf "ERROR: embox: vertices extend beyond unit cell. \n";
      return;
    };
    maxper := max(vertex vv, inverse_periods[1][dim]*vv.x +
          inverse_periods[2][dim]*vv.y + inverse_periods[3][dim]*vv.z);
    if maxper >  1.0001 then
    { printf "ERROR: embox: vertices extend beyond unit cell. \n";
      return;
    };
  };

  // Now trace out exterior faces.
  set edge embox_edge_mark (valence==1);  // mark edges we need to do, since
                                     // valence won't be 1 after a face added.
  set body embox_body_mark 0;
  foreach edge ee where embox_edge_mark do
  { 
    if ee.valence == 3 then continue;  // all done with this edge

    foreach ee.facet ff where ff.frontbody and ff.backbody do
    {
      if body[ff.frontbody].embox_body_mark == 0 then
        embox_face(ee.oid,ff.frontbody);
      if body[ff.backbody].embox_body_mark == 0 then
        embox_face(-ee.oid,ff.backbody);
    };
  };
  
} // end embox
