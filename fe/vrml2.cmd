// vrml2.cmd
// Makes VRML2 file for surface.

/* Usage: Use the "show edge where ..."  and "show facet where ... "
   commands to control which edges and facet are shown.
   Run "vrml2" and re-direct output to file, e.g.
      Enter command: vrml2 >>> "myfile.wrl";
*/
 
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke
// VRML ref: http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/ch3-301.htm

define vertex attribute order_num integer
vrml2 := {
   local counter;
   local minx,miny,minz,maxsize,eyex,eyey,eyez,dirx,diry,dirz,angle;
   local maxx,maxy,maxz;

   counter := 0;

  if torus then
  { errprintf "Cannot run 'vrml2' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'vrml2' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'vrml2' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'vrml2' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'vrml2' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'vrml2' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'vrml2' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

   printf "#VRML V2.0 utf8\n\n";

   // Figure out viewpoint, from bounding box, giving same view
   // as initial view of Evolver.
   minx := min(vertex,x); maxx := max(vertex,x);
   miny := min(vertex,x); maxy := max(vertex,y);
   minz := min(vertex,x); maxz := max(vertex,z);
   maxsize := maximum(maxx-minx,maximum(maxy-miny,maxz-minz));
   // eye position
   eyex := (maxx+minx)/2 + 2*maxsize; 
   eyey := (maxy+miny)/2; 
   eyez := (maxz+minz)/2;
   // eye orientation, rotation from default (0,0,-1)
   dirx := 1; 
   diry := 1; 
   dirz := 1;
   angle := 2*pi/3;

   printf "Viewpoint { description \"Initial view\" position %g %g %g\n",
         eyex,eyey,eyez;
   printf "               orientation %g %g %g %g }\n",dirx,diry,dirz,angle;
   printf "NavigationInfo { type \"EXAMINE\" }\n";
   printf "Transform {\n";
   printf " children [\n";
   printf " DirectionalLight {\n";
   printf "    ambientIntensity 0.39\n";
   printf "    direction -.56 0.34 -0.75\n";
   printf "  }\n";
   printf "   Background { skyColor [ .4 .8 1 ] }\n";
   printf "   Shape {\n";
   printf "   appearance DEF A Appearance { material Material { } }\n";
   printf "   geometry DEF IFS IndexedFaceSet {\n";
   printf "     coord DEF TheVertices Coordinate {\n";
   printf "      point [\n";
   foreach vertex jvv do 
   { printf "        %g %g %g,\n",jvv.x,jvv.y,jvv.z;
     set jvv order_num counter; counter := counter + 1; 
   };
   printf "       ]  # end point\n      }  # end coord\n";

   printf "  coordIndex [ # each facet in two orientations, since only front lit\n"; 
   foreach facet jff where show and color >= 0 do 
     printf "       %g,%g,%g,-1,  %g,%g,%g,-1\n",
      jff.vertex[1].order_num,jff.vertex[2].order_num,jff.vertex[3].order_num,
      jff.vertex[3].order_num,jff.vertex[2].order_num,jff.vertex[1].order_num;
   printf "     ]  # end coordIndex\n";

   printf "   color DEF EvolverColors Color { \n";
   printf "       color [ 0.0 0.0 0.0 , 0.0 0.0 0.5 , 0.0 0.5 0.0 ,\n";
   printf "               0.0 0.5 0.5 , 0.5 0.0 0.0 , 0.5 0.0 0.5 ,\n";
   printf "               0.5 0.25 0. , 0.3 0.3 0.3 , .15 .15 .15 ,\n";
   printf "               .25 .25 0.5 , .25 0.5 .25 , .25 0.5 0.5 ,\n";
   printf "               0.5 .25 .25 , 0.5 .25 0.5 , .5 .5 .0 , .5 .5 .5 ] \n";
   printf "  }\n";
   printf "  colorPerVertex FALSE  # per face instead\n";
   printf "  colorIndex [\n";
   foreach facet jff where show and color >= 0 do 
      printf "    %d, %d,\n",jff.backcolor,
        jff.frontcolor;
   printf "   ]  # end colorIndex\n";
   printf "     } # end IndexedFaceSet\n";
   printf "   }  # end Shape\n";
   printf "  ]  # end children\n";
   printf " } # end Transform \n\n";

   printf "Transform { children [ Shape { geometry IndexedLineSet {\n";
   printf "   coord USE TheVertices\n";
   printf "  coordIndex [\n";
   foreach edge jee where show do printf "       %g,%g,-1,\n",
      jee.vertex[1].order_num,jee.vertex[2].order_num;
   printf "          ]   # end coordIndex\n";
   printf "   color Color {  # standard Evolver colors 0 to 15 \n";
   printf "       color [ 0.0 0.0 0.0 , 0.0 0.0 0.5 , 0.0 0.5 0.0 ,\n";
   printf "               0.0 0.5 0.5 , 0.5 0.0 0.0 , 0.5 0.0 0.5 ,\n";
   printf "               0.5 0.25 0. , 0.3 0.3 0.3 , .15 .15 .15 ,\n";
   printf "               .25 .25 0.5 , .25 0.5 .25 , .25 0.5 0.5 ,\n";
   printf "               0.5 .25 .25 , 0.5 .25 0.5 , .5 .5 .0 , .5 .5 .5 ] \n";
   printf "  }\n";
   printf "  colorPerVertex FALSE  # per edge instead\n";
   printf "  colorIndex [\n";

   foreach edge jee where show do printf "       %d,\n",
      jee.color;

   printf "   ]  # end colorIndex\n";
   printf "     } # end IndexedLineSet\n";
   printf "   }  # end Shape \n";
   printf "  ]  # end children \n";
   printf " } # end Transform \n\n";

} // end vrml2

/* vrml2 usage: Use the "show edge where ..."  and "show facet where ... "
   commands to control which edges and facet are shown.
   Run "vrml2" and re-direct output to file, e.g.
      Enter command: vrml2 >>> "myfile.wrl";
*/
