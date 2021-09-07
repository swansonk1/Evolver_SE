// ply.cmd

// Surface Evolver command to create PLY file (Stanford Polygon File Format)
// ASCII ply format; does only front color of each facet.

// Programmer: Ken Brakke, brakke@susqu.edu,   Jan. 9, 2009

// Format documentation: http://local.wasp.uwa.edu.au/~pbourke/dataformats/ply/

/* Usage:
      Set the edges and facets you want to output using the
      "show edges where ... " and "show facets where ... " commands.
      Run ply and redirect to a file, like this:
         Enter command: ply >>> "filename.ply"

*/

// Evolver color components; note first index is color+1
define rgb_ply integer[16][3];    
rgb_ply[1][1] := 0; rgb_ply[1][2] := 0; rgb_ply[1][3] := 0;
rgb_ply[2][1] := 0; rgb_ply[2][2] := 0; rgb_ply[2][3] := 255;
rgb_ply[3][1] := 0; rgb_ply[3][2] := 255; rgb_ply[3][3] := 0;
rgb_ply[4][1] := 0; rgb_ply[4][2] := 255; rgb_ply[4][3] := 255;
rgb_ply[5][1] := 255; rgb_ply[5][2] := 0; rgb_ply[5][3] := 0;
rgb_ply[6][1] := 255; rgb_ply[6][2] := 0; rgb_ply[6][3] := 255;
rgb_ply[7][1] := 255; rgb_ply[7][2] := 127; rgb_ply[7][3] := 0;
rgb_ply[8][1] := 160; rgb_ply[8][2] := 160; rgb_ply[8][3] := 160;
rgb_ply[9][1] := 80; rgb_ply[9][2] := 80; rgb_ply[9][3] := 80;
rgb_ply[10][1] := 80; rgb_ply[10][2] := 200; rgb_ply[10][3] := 255;
rgb_ply[11][1] := 127; rgb_ply[11][2] := 255; rgb_ply[11][3] := 127;
rgb_ply[12][1] := 127; rgb_ply[12][2] := 255; rgb_ply[12][3] := 255;
rgb_ply[13][1] := 255; rgb_ply[13][2] := 127; rgb_ply[13][3] := 127;
rgb_ply[14][1] := 255; rgb_ply[13][2] := 127; rgb_ply[13][3] := 255;
rgb_ply[15][1] := 255; rgb_ply[15][2] := 255; rgb_ply[15][3] := 0;
rgb_ply[16][1] := 255; rgb_ply[16][2] := 255; rgb_ply[16][3] := 255;

// Numbering attribute for vertices
define vertex attribute ply_vnumber integer

ply := {
  local vnum;

  // Check assumptions
  if torus then
  { errprintf "Cannot run 'ply' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'ply' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'ply' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'ply' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'ply' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'ply' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'ply' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

  // Header
  printf "ply\n";
  printf "format ascii 1.0\n";
  printf "comment PLY version of %s\n",datafilename; 
  printf "comment Made by Surface Evolver ply.cmd\n";
  printf "element vertex %d\n",vertex_count;
  printf "property float x\n";
  printf "property float y\n";
  printf "property float z\n";

  printf "element face %d\n",sum(facet,show and color >= 0);
  printf "property list uchar int vertex_index\n";
  printf "property uchar red\n";
  printf "property uchar green\n";
  printf "property uchar blue\n";

  printf "element edge %d\n",sum(edge,show and color >= 0);
  printf "property int vertex1\n";
  printf "property int vertex2\n";
  printf "property uchar red\n";
  printf "property uchar green\n";
  printf "property uchar blue\n";
  printf "end_header\n"; 

  // vertex list
  vnum := 0;
  foreach vertex vv do
  { printf"%f %f %f\n",vv.x,vv.y,vv.z;
    vv.ply_vnumber := vnum;
    vnum += 1;
  };

  // facet list
  foreach facet ff where show and color >= 0 do
  { printf "3 %d %d %d %d %d %d\n",ff.vertex[1].ply_vnumber,
         ff.vertex[2].ply_vnumber,ff.vertex[3].ply_vnumber,
         rgb_ply[ff.color+1][1],rgb_ply[ff.color+1][2],rgb_ply[ff.color+1][3];
  };

  // edge list
  foreach edge ee where show and color >= 0 do
  { printf "%d %d %d %d %d\n",ee.vertex[1].ply_vnumber,
         ee.vertex[2].ply_vnumber,
         rgb_ply[ee.color+1][1],rgb_ply[ee.color+1][2],rgb_ply[ee.color+1][3];
  };
}

// end ply.cmd

/* Usage:
      Set the edges and facets you want to output using the
      "show edges where ... " and "show facets where ... " commands.
      Run ply and redirect to a file, like this:
         Enter command: ply >>> "filename.ply"

*/
