// x3d.cmd
// Makes x3d file for surface.

// Usage: Use the edge and facet "show" commands to control which are done. 
//        Facets with different front and back colors are included twice.
//        Run "x3d" and re-direct output to file, e.g.
//        Enter command: x3d >>> "myfile.x3d";
 
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define vertex attribute order_num integer  // for x3d vertex index
x3d := {
   local counter,minx, maxx, miny, maxy, minz, maxz, maxsize, eyex, eyey, eyez;
   local dirx, diry, dirz, angle, perlinecount;

   counter := 0;


  if torus then
  { errprintf "Cannot run 'x3d' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'x3d' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'x3d' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'x3d' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'x3d' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'x3d' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'x3d' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };


   foreach vertex jvv do 
   { 
     set jvv order_num counter; counter := counter + 1; 
   };
   printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
   printf "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.1//EN\"\n";
   printf "      \"http://www.web3d.org/specifications/x3d-3.1.dtd\">\n";
   printf "<X3D profile='Immersive' version='3.0'>\n";
   printf "  <head>\n";
   printf "    <meta name=\"title\" content=\"cube.x3d\"/>\n";
   printf "    <meta name=\"description\" content=\"triangulated surface\"/>\n";
   printf "    <meta name=\"generator\" content=\"Surface Evolver, x3d.cmd\"/>\n";
   printf "  </head>\n";
   printf "  <Scene>\n";
   printf "    <NavigationInfo type=\"EXAMINE\"/>\n";

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

   printf "    <Viewpoint description=\"Initial view\" position='%g %g %g'\n",
         eyex,eyey,eyez;
   printf "               orientation='%g %g %g %g'/>\n",dirx,diry,dirz,angle;
   printf "    <Transform>\n";
   printf "      <DirectionalLight ambientIntensity='0.39' direction='-.56 0.34 -0.75'/>\n";
   printf "      <Background skyColor='.4 .8 1'/>\n";

   // Facets
   {
     printf "       <Shape>\n";
     printf "         <Appearance> <Material diffuseColor='1 1 1'/></Appearance>\n";
     printf "         <IndexedFaceSet \n";
     printf "            coordIndex=' \n"; 
     foreach facet jff where show and color >= 0 do 
     {  printf "            %g,%g,%g,-1, %g,%g,%g,-1, \n",
               jff.vertex[1].order_num,
               jff.vertex[2].order_num,jff.vertex[3].order_num,
               jff.vertex[3].order_num,jff.vertex[2].order_num,
               jff.vertex[1].order_num;
     };
     printf "            ' \n";
  
     printf "            colorIndex='\n              ";
     perlinecount := 0;
     foreach facet jff where show and color >= 0 do
     { 
       printf "%d,%d, ",jff.frontcolor,jff.backcolor;
       perlinecount += 1;
       if perlinecount >= 10 then
       { printf "\n              ";
         perlinecount := 0;
       };
     };
     if perlinecount == 0 then
        printf "               '  \n"
     else printf " '\n";
  
     printf "           colorPerVertex='false'\n";
     printf "           solid='true'\n";
     printf "         >\n";
  

   printf "           colorPerVertex='false'\n";
   printf "           solid='false'\n";
   printf "        >\n";

   printf "          <Coordinate DEF='TheVertices' point='\n";
   foreach vertex jvv do 
   { printf "          %g %g %g,\n",jvv.x,jvv.y,jvv.z;
   };
   printf "              ' /> \n";

   printf "          <Color  DEF='EvolverColors' \n";
   printf "            color=' 0.0 0.0 0.0 , 0.0 0.0 0.5 , 0.0 0.5 0.0 ,\n";
   printf "                  0.0 0.5 0.5 , 0.5 0.0 0.0 , 0.5 0.0 0.5 ,\n";
   printf "                  0.5 0.25 0. , 0.3 0.3 0.3 , .15 .15 .15 ,\n";
   printf "                  .25 .25 0.5 , .25 0.5 .25 , .25 0.5 0.5 ,\n";
   printf "                  0.5 .25 .25 , 0.5 .25 0.5 , .5 .5 .0 , .5 .5 .5'/>\n";

//     printf "         <Coordinate USE='TheVertices'/>\n";
//     printf "         <Color USE='EvolverColors' /> \n";
  
     printf "       </IndexedFaceSet>\n";
     printf "     </Shape>\n";
   };

   // Edges
   printf "      <Shape>\n";
   printf "        <Appearance> <Material diffuseColor='0 0 0'/></Appearance>\n";
   printf "        <IndexedLineSet \n";
   printf "           coordIndex=' \n"; 
   foreach edge jff where show and color >= 0 do 
   {  printf "            %g,%g,-1, \n",jff.vertex[1].order_num,
          jff.vertex[2].order_num;
   };
   printf "            ' \n";

   printf "           colorIndex='\n             ";
   perlinecount := 0;
   foreach edge jff where show and color >= 0 do
   { printf "%d,",jff.color;
     perlinecount += 1;
     if perlinecount >= 20 then
     { printf "\n               ";
       perlinecount := 0;
     }
   };
   if perlinecount == 0 then
     printf "               '\n"
   else
     printf " '\n";


   printf "          colorPerVertex='false'\n";
   printf "        >\n";

   printf "        <Coordinate USE='TheVertices'/>\n";
   printf "        <Color USE='EvolverColors' /> \n";

   printf "      </IndexedLineSet>\n";
   printf "    </Shape>\n";
   printf "  </Transform>\n\n";


   printf " </Scene>\n";
   printf "</X3D>\n";

} // end x3d

// End x3d.cmd

// Usage: Use the edge and facet "show" commands to control which are done. 
//        Run "x3d" and re-direct output to file, e.g.
//        Enter command: x3d >>> "myfile.x3d";
