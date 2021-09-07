// rib.cmd
// Surface Evolver command to write RenderMan RIB file for surface
// Usage: rib >>> "filename.rib"

// This version does facets only, on a light blue background.
// Viewpoint is the same as 's' command.  Orthogonal projection.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

rib := {

  // Check assumptions
  if torus then
  { errprintf "Cannot run 'rib' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'rib' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'rib' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'rib' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'rib' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'rib' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };
  if rgb_colors then
  { errprintf "The 'rib' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };


  printf"##RenderMan RIB-Structure 1.0\n";
  printf "version 3.03\n";
  printf "\n";
  printf "Option \"searchpath\" \"shader\" [\".:../shaders:&\"]\n";
  printf "Display \"%s.tif\" \"file\" \"rgba\"\n",datafilename;
  printf "Format 512 512 -1\n";
  printf "PixelSamples 1 1\n";
  printf "Orientation \"rh\"\n";
  printf "Clipping 0.1 10.0\n";
  printf "\n";

  printf "WorldBegin\n";
  printf "\n";

  printf "Surface \"constant\"\n";  /* blue background */
  printf "Color [ 0.3 0.8 1.0 ]\n";
  printf "Polygon \"P\" [ 1 1 9  -1 1 9  -1 -1 9  1 -1 9]\n";
  printf "\n";

  printf "Translate 0 0 5 \n";
  printf "Rotate 240 1 1 1 \n";
  printf "Scale -1 1 1 \n";
  printf "Scale .66667 .66667 .66667 \n";
  printf "ConcatTransform [ %f %f %f %f\n", 
    view_matrix[1][1],view_matrix[2][1],view_matrix[3][1],view_matrix[4][1];
  printf "            %f %f %f %f\n",
    view_matrix[1][2],view_matrix[2][2],view_matrix[3][2],view_matrix[4][2];
  printf "            %f %f %f %f\n",
    view_matrix[1][3],view_matrix[2][3],view_matrix[3][3],view_matrix[4][3];
  printf "            %f %f %f %f ]\n",
    view_matrix[1][4],view_matrix[2][4],view_matrix[3][4],view_matrix[4][4];

  printf "LightSource \"ambientlight\" 1 \"intensity\" 0.30\n";
  printf "LightSource \"distantlight\" 1 \"from\" [1 0 1] \"to\" [0 0 0] \"intensity\" 0.7\n";
  printf "\n";
  printf "AttributeBegin\n";
  printf "  Surface \"constant\"\n";
  foreach facet ff where show do {
     if ( ff.color == CLEAR ) then continue;
     if ( ff.color == WHITE ) then printf "  Color [ 1.0 1.0 1.0 ]\n"
     else if ( ff.color == WHITE ) then printf "  Color [ 1.0 1.0 1.0 ]\n"
     else if ( ff.color == BLACK ) then printf "  Color [ 0.0 0.0 0.0 ]\n"
     else if ( ff.color == BLUE ) then printf "  Color [ 0.0 0.0 1. ]\n"
     else if ( ff.color == GREEN ) then printf "  Color [ 0.0 1. 0.0 ]\n"
     else if ( ff.color == CYAN ) then printf "  Color [ 0.0 1. 1. ]\n"
     else if ( ff.color == RED ) then printf "  Color [ 1. 0.0 0.0 ]\n"
     else if ( ff.color == MAGENTA ) then printf "  Color [ 1. 0.0 1. ]\n"
     else if ( ff.color == BROWN ) then printf "  Color [ 1. 0.5 0. ]\n"
     else if ( ff.color == LIGHTGRAY ) then printf "  Color [ .6 .6 .6 ]\n"
     else if ( ff.color == DARKGRAY ) then printf "  Color [ .3 .3 .3 ]\n"
     else if ( ff.color == LIGHTBLUE ) then printf "  Color [ .5 .5 1. ]\n"
     else if ( ff.color == LIGHTGREEN ) then printf "  Color [ .5 1. .5 ]\n"
     else if ( ff.color == LIGHTCYAN ) then printf "  Color [ .5 1. 1. ]\n"
     else if ( ff.color == LIGHTRED ) then printf "  Color [ 1. .5 .5 ]\n"
     else if ( ff.color == LIGHTMAGENTA ) then printf "  Color [ 1. .5 1. ]\n"
     else if ( ff.color == YELLOW ) then printf "  Color [ 1. 1. .0 ]\n";
     printf "  Polygon \"P\" [ %f %f %f  %f %f %f  %f %f %f ]\n",
     ff.vertex[1].x,ff.vertex[1].y,ff.vertex[1].z,
     ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z,
     ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z;
  };
  printf "AttributeEnd\n";
  printf "\n";
  printf "WorldEnd\n";
}


// End rib.cmd

// Usage: rib >>> "filename.rib"

