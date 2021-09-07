// povray.cmd

// Surface Evolver command for producing POV-Ray input file.
// Usage:
//        Use the "show edge where ..." command to declare which
//        edges are to be depicted as thin cylinders.
//        Set "edge_radius" to desired radius of edge cylinders.
//        Use the "show facet where ..." command to set facets to do.
//        Run "povray" and redirect to desired file, e.g.
//            Enter command: povray >>> "something.pov"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

edge_radius := 0.003; // adjust this for desired radius of edge cylinders

procedure print_color(integer ev_color) {
      if ( ev_color == white ) then printf " t_white "
      else if ( ev_color == black ) then printf " t_black "
      else if ( ev_color == blue) then printf " t_blue "
      else if ( ev_color == green ) then printf " t_green "
      else if ( ev_color == cyan ) then printf " t_cyan "
      else if ( ev_color == red ) then printf " t_red "
      else if ( ev_color == magenta ) then printf " t_magenta "
      else if ( ev_color == brown ) then printf " t_brown "
      else if ( ev_color == lightgray ) then printf " t_lightgray "
      else if ( ev_color == darkgray ) then printf " t_darkgray "
      else if ( ev_color == lightblue ) then printf " t_lightblue "
      else if ( ev_color == lightgreen ) then printf " t_lightgreen "
      else if ( ev_color == lightcyan ) then printf " t_lightcyan "
      else if ( ev_color == lightred ) then printf " t_lightred "
      else if ( ev_color == lightmagenta ) then printf " t_lightmagenta "
      else if ( ev_color == yellow ) then printf " t_yellow ";
}

povray := {


  // Check assumptions
  if torus then
  { errprintf "Cannot run 'povray' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'povray' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'povray' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'povray' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'povray' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'povray' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'povray' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };


   printf "// %s in POV-Ray format.\n\n",datafilename;

   printf "light_source { <0,0,300> color rgb <1,1,1> }\n";
   printf "light_source { <100,0,0> color rgb <1,1,1> }\n";
   printf "camera { location <12,0,0> sky <0,0,1>  // right handed \n";
   printf "         up <0,0,1> right <1.3,0,0> look_at <0,0,0> angle 15 }\n";
   printf "background { color <0.3,0.8,1.0> } // light blue\n\n";
   printf "// Textures corresponding to Evolver colors\n\n";
  
   printf "#declare t_black = texture { pigment { rgb <0.0,0.0,0.0> }}\n";
   printf "#declare t_blue = texture { pigment { rgb <0.0,0.0,1.,> }}\n";
   printf "#declare t_green = texture { pigment { rgb <0.0,1.,0.0,> }}\n";
   printf "#declare t_cyan = texture { pigment { rgb <0.0,1.,1.,> }}\n"; 
   printf "#declare t_red = texture { pigment { rgb <1.,0.0,0.0,> }}\n";
   printf "#declare t_magenta = texture { pigment { rgb <1.,0.0,1.,> }}\n";
   printf "#declare t_brown = texture { pigment { rgb <1.,0.5,0.,> }}\n";
   printf "#declare t_lightgray = texture { pigment { rgb <.6,.6,.6,> }}\n";
   printf "#declare t_darkgray = texture { pigment { rgb <.3,.3,.3,> }}\n";
   printf "#declare t_lightblue = texture { pigment { rgb <.3,.8,1.,> }}\n"; 
   printf "#declare t_lightgreen = texture { pigment { rgb <.5,1.,.5,> }}\n";
   printf "#declare t_lightcyan = texture { pigment { rgb <.5,1.,1.,> }}\n";
   printf "#declare t_lightred = texture { pigment { rgb <1.,.5,.5,> }}\n";
   printf "#declare t_lightmagenta = texture { pigment { rgb <1.,.5,1.,> }}\n";
   printf "#declare t_yellow = texture { pigment { rgb <1.,1.,.0,> }}\n";
   printf "#declare t_white = texture { pigment { rgb <1.,1.,1.,> }}\n"; 

   printf "\n//One overall object.\n";
   printf "union {\n";
   printf "// All facets in one big mesh object for efficiency.\n";
   printf "   mesh { \n";
   foreach facet ff where show and color >= 0 do {
     printf "   triangle { <%f,%f,%f>,<%f,%f,%f>,<%f,%f,%f> texture {",
        ff.vertex[1].x,ff.vertex[1].y,ff.vertex[1].z, 
        ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z, 
        ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z; 
     if view_matrix[1][1]*ff.x + view_matrix[1][2]*ff.y + view_matrix[1][3]*ff.z > 0
     then
       print_color(ff.frontcolor)
     else
       print_color(ff.backcolor);
     printf " } }\n";
   };
   printf "  }  // end of mesh object\n";

   // Do desired edges
   printf "#declare edge_radius = %f;\n",edge_radius; 
   foreach edge ee where ee.show and color >= 0 do
   {  printf "cylinder { <%f,%f,%f>,<%f,%f,%f> edge_radius texture {",
      ee.vertex[1].x,ee.vertex[1].y,ee.vertex[1].z,
      ee.vertex[2].x,ee.vertex[2].y,ee.vertex[2].z;
      print_color(ee.color);
      printf "} }\n";
   };

   // Windup
   printf "// overall viewing transformation\n";
   printf "  matrix < %f,%f,%f,\n",
        view_matrix[1][1],view_matrix[2][1],view_matrix[3][1];
   printf "           %f,%f,%f,\n",
        view_matrix[1][2],view_matrix[2][2],view_matrix[3][2];
   printf "           %f,%f,%f,\n",
        view_matrix[1][3],view_matrix[2][3],view_matrix[3][3];
   printf "           %f,%f,%f>\n",
        view_matrix[1][4],view_matrix[2][4],view_matrix[3][4];
   printf " }  // end of all objects\n";
}

// End povray.cmd

// Usage:
//        Use the "show edge where ..." command to declare which
//        edges are to be depicted as thin cylinders.
//        Set "edge_radius" to desired radius of edge cylinders.
//        Use the "show facet where ..." command to set facets to do.
//        Run "povray" and redirect to desired file, e.g.
//            Enter command: povray >>> "something.pov"
