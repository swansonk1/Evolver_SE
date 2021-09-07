// vrml.cmd
// Makes VRML file for surface.

// Usage: Set edge_flag to 1 if you want do see all edges.
//        Run "vrml" and re-direct output to file, e.g.
//        Enter command: vrml >>> "myfile.wrl";
 
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define vertex attribute order_num integer
edge_flag := 0 // 1 for all edges, 0 for special edges only
vrml := {
   local counter;

if torus then
  { errprintf "Cannot run 'vrml' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'vrml' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'vrml' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'vrml' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'vrml' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'vrml' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'vrml' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

   counter := 0;
   printf "#VRML V1.0 ascii\n\n";
   printf "Separator {\n";
   printf "  DEF Title Info { string \"%s\" }\n",datafilename;
   printf "  DEF SceneInfo Info { string \"Created by Surface Evolver\" }\n";
   printf "  DEF BackgroundColor Info { string \".5 .6 1\" } \n";
   printf "  DirectionalLight { intensity .5 direction 0 0 -1 } \n";
   printf "  MaterialBinding { value PER_FACE_INDEXED }\n";
   printf "  Material { \n";
   printf "   diffuseColor [ 0.0 0.0 0.0    , 0.0 0.0 0.5 ,\n";
   printf "   0.0 0.5 0.0 , 0.0 0.5 0.5  ,  0.5 0.0 0.0    , 0.5 0.0 0.5    ,\n";
   printf "   0.5 0.25 0.  , .3 .3 .3 , .15 .15 .15  , .25 .25 .5 , .25 .5 .25 ,\n";
   printf "   .25 .5 .5 , .5 .25 .25 , .5 .25 .5 , .5 .5 .0 , .5 .5 .5   ] \n";
   printf "   emissiveColor [ 0.0 0.0 0.0    , 0.0 0.0 0.5 ,\n";
   printf "   0.0 0.5 0.0 , 0.0 0.5 0.5  ,  0.5 0.0 0.0    , 0.5 0.0 0.5    ,\n";
   printf "   0.5 0.25 0.  , .3 .3 .3 , .15 .15 .15  , .25 .25 .5 , .25 .5 .25 ,\n";
   printf "   .25 .5 .5 , .5 .25 .25 , .5 .25 .5 , .5 .5 .0 , .5 .5 .5   ] \n";
   printf "  }\n";
   printf "  Separator {\n";
   printf "    Coordinate3 { point [\n";
   foreach vertex jvv do { printf "        %f %f %f,\n",jvv.x,jvv.y,jvv.z;
            set jvv order_num counter; counter := counter + 1; };
   printf "        ]\n         }\n";
   printf "    IndexedFaceSet { coordIndex [\n";
   foreach facet jff where show and color >= 0 do printf "        %g,%g,%g,-1,\n",
      jff.vertex[1].order_num,jff.vertex[2].order_num,jff.vertex[3].order_num;
   printf "           ] \n";
   printf "    materialIndex [\n";
   foreach facet jff where show and color >= 0 do printf "   %g,\n",jff.color;
   printf "    ]\n";
   printf "     }\n";
   printf "  Material { ambientColor 0 0 0 diffuseColor 0 0 0 }\n";
   printf "    IndexedLineSet { coordIndex [\n";
   foreach edge jee where show do printf "       %g,%g,-1,\n",
      jee.vertex[1].order_num,jee.vertex[2].order_num;
   printf "          ] } \n";
   printf "   }\n";
   printf "}\n";

} // end vrml


// End vrml.cmd

// Usage:  vrml >>> "filename.wrl"

