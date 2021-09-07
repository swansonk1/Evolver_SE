// 3ds.cmd

/* Surface Evolver script to produce Autodesk 3DS format file.

   Usage: do_3ds >>> "filename.3ds"

   Assumptions: 3D soapfilm model, linear model, not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Also limited to 65535 facets.
   Does facets only, not edges. 
   Facet color is frontcolor on both sides.
    
   Programmer: Ken Brakke, brakke@susqu.edu, www.susqu.edu/brakke

   Format references:
   http://www.jalix.org/ressources/graphics/3DS/_unofficials/3fs-unofficial.htm
   http://faydoc.tripod.com/formats/3ds.htm
   http://www.martinreddy.net/gfx/3d/3DS.spec
*/

// Strategy: Each chunk type has two functions: one
// to calculate its total length, and one to actually 
// print it.

define rgb_3ds integer[16][3];     // color definitions
rgb_3ds[1][1] := 0; rgb_3ds[1][2] := 0; rgb_3ds[1][3] := 0;
rgb_3ds[2][1] := 0; rgb_3ds[2][2] := 0; rgb_3ds[2][3] := 255;
rgb_3ds[3][1] := 0; rgb_3ds[3][2] := 255; rgb_3ds[3][3] := 0;
rgb_3ds[4][1] := 0; rgb_3ds[4][2] := 255; rgb_3ds[4][3] := 255;
rgb_3ds[5][1] := 255; rgb_3ds[5][2] := 0; rgb_3ds[5][3] := 0;
rgb_3ds[6][1] := 255; rgb_3ds[6][2] := 0; rgb_3ds[6][3] := 255;
rgb_3ds[7][1] := 255; rgb_3ds[7][2] := 127; rgb_3ds[7][3] := 0;
rgb_3ds[8][1] := 160; rgb_3ds[8][2] := 160; rgb_3ds[8][3] := 160;
rgb_3ds[9][1] := 80; rgb_3ds[9][2] := 80; rgb_3ds[9][3] := 80;
rgb_3ds[10][1] := 80; rgb_3ds[10][2] := 200; rgb_3ds[10][3] := 255;
rgb_3ds[11][1] := 127; rgb_3ds[11][2] := 255; rgb_3ds[11][3] := 127;
rgb_3ds[12][1] := 127; rgb_3ds[12][2] := 255; rgb_3ds[12][3] := 255;
rgb_3ds[13][1] := 255; rgb_3ds[13][2] := 127; rgb_3ds[13][3] := 127;
rgb_3ds[14][1] := 255; rgb_3ds[13][2] := 127; rgb_3ds[13][3] := 255;
rgb_3ds[15][1] := 255; rgb_3ds[15][2] := 255; rgb_3ds[15][3] := 0;
rgb_3ds[16][1] := 255; rgb_3ds[16][2] := 255; rgb_3ds[16][3] := 255;

function integer m3d_version_chunk_length() { return 0x0A; }

function integer mat_chunk_length() { return 0xb9; }

function integer point_array_length()
{ 
  return 6 + 2 + 12*vertex_count;
}

function integer mesh_matrix_length()
{ 
  return 6 + 12*4;
}

function integer smooth_group_length()
{
  return 6 + 4*sum(facet where show and color>=0,1);
}

function integer mesh_material_length()
{ 
  return 16*(6 + 13 + 2) + 2*sum(facets where show and color>=0,1):
}

function integer face_array_length()
{
  return 6 + 2 + 8*sum(facet where show and color>=0,1)
     + mesh_material_length() + smooth_group_length();
}

function integer named_triangle_object_length()
{
  return 6 + point_array_length() + mesh_matrix_length() 
     + face_array_length();
}

function integer named_object_length()
{ 
  return 6 + sizeof("mesh") + 1 + named_triangle_object_length();
}

function integer main_chunk_length()
{
  return 6 + 0xA + 0xA + 16*mat_chunk_length() + named_object_length();
}

function integer kfdata_chunk_length() { return 0x9E; }

m3d_version_chunk := {
  binary_printf "%d%ld%ld",0x0002,m3d_version_chunk_length(),3;
}

procedure material_chunk(integer inx)
{ // modelled after output of Meshlab
  local matname;
  binary_printf "%d%ld",0xafff,0xb9;  // MAT_ENTRY
  matname := sprintf"Material-%03d",inx;
  binary_printf "%d%ld%s%c",0xa000,0x13,matname,0; // MAT_NAME
  binary_printf "%d%ld",0xa010,0x18;  // MAT_AMBIENT
    binary_printf "%d%ld%c%c%c",0x0011,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // COLOR_24
    binary_printf "%d%ld%c%c%c",0x0012,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // LIN_COLOR_24
  binary_printf "%d%ld",0xa020,0x18;  // MAT_DIFFUS
    binary_printf "%d%ld%c%c%c",0x0011,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // COLOR_24
    binary_printf "%d%ld%c%c%c",0x0012,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // LIN_COLOR_24
  binary_printf "%d%ld",0xa030,0x18;  // MAT_SPECULAR
    binary_printf "%d%ld%c%c%c",0x0011,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // COLOR_24
    binary_printf "%d%ld%c%c%c",0x0012,0x9,rgb_3ds[inx][1],rgb_3ds[inx][2],rgb_3ds[inx][3]; // LIN_COLOR_24
  binary_printf "%d%ld",0xa040,0x0e;  // MAT_SHININESS
    binary_printf "%d%ld%d",0x0030,0x8,0; // INT_PERCENTAGE
  binary_printf "%d%ld",0xa041,0x0e;  // MAT_SHIN2PCT
    binary_printf "%d%ld%d",0x0030,0x8,0; // INT_PERCENTAGE
  binary_printf "%d%ld",0xa050,0x0e;  // MAT_TRANPARENCY
    binary_printf "%d%ld%d",0x0030,0x8,0; // INT_PERCENTAGE
  binary_printf "%d%ld",0xa052,0x0e;  // MAT_SPFALL
    binary_printf "%d%ld%d",0x0030,0x8,0; // INT_PERCENTAGE
  binary_printf "%d%ld%d",0xa100,0x8,0x3; // MAT_SHADING
  binary_printf "%d%ld",0xa053,0x0e;  // MAT_REFBLUR
    binary_printf "%d%ld%d",0x0030,0x8,0; // INT_PERCENTAGE
  binary_printf "%d%ld%f",0xa087,0xA,1.0; // MAT_WIRESIZE
}

material_list := {
  local inx;
  for ( inx := 1 ; inx <= 16 ; inx += 1 )
    material_chunk(inx);
}

define vertex attribute vnumber_3ds integer  // vertices might have noncontiguous id's
define facet attribute fnumber_3ds integer  // consecutive numbering of shown facets.

point_array := {
  local vnum;

  vnum := 0;
  binary_printf "%d%ld%d",0x4110,point_array_length(),vertex_count; // POINT_ARRAY
  foreach vertex vv do
  { binary_printf "%f%f%f",vv.x,vv.y,vv.z;
    vv.vnumber_3ds := vnum;
    vnum += 1;
  }
}

mesh_matrix := {
  binary_printf "%d%ld",0x4160,6+4*3*4; // transformation matrix

/*
   Local axis info.
   The three first blocks of three floats are the definition
   (in the absolute axis) of the local axis X Y Z of the object.
   And the last block of three floats is the local center of the object.
   (so looks like transpose of view matrix, I guess; but not using 
    view matrix since this is a physical transformation matrix,
    not a view matrix)
*/
  binary_printf "%f%f%f",1.0,0.0,0.0;
  binary_printf "%f%f%f",0.0,1.0,0.0;
  binary_printf "%f%f%f",0.0,0.0,1.0;
  binary_printf "%f%f%f",0.0,0.0,0.0;
}

mesh_material_groups := {
  local inx,matname;

  for ( inx := 0 ; inx < 16 ; inx += 1 )  // inx is facet color
  {
    binary_printf "%d%ld",0x4130,6+13+2+2*sum(facet where show,color==inx); // MSH_MAT_GROUP
    matname := sprintf "Material-%03d",inx;
    binary_printf "%s%c%d",matname,0,sum(facet where show and color>=0,color==inx);
    foreach facet where show and color==inx do
    { binary_printf "%d",fnumber_3ds;
    };
  }
}

smooth_group := {
  binary_printf "%d%ld",0x4150,6+4*sum(facet where show and color>=0,1); // SMOOTH_GROUP 
  foreach facet ff where show and color>=0 do binary_printf "%c%c%c%c",0xA,0,0,0;
}

face_array := {
  local facenum;

  binary_printf "%d%ld",0x4120,face_array_length(); // FACE_ARRAY
  binary_printf "%d",sum(facet where show and color>=0,1);
  facenum := 0;
  foreach facet ff where show and color>=0 do
  { binary_printf "%d%d%d%d",ff.vertex[1].vnumber_3ds,ff.vertex[2].vnumber_3ds,ff.vertex[3].vnumber_3ds,0;
    ff.fnumber_3ds := facenum;
    facenum += 1;
  };
  mesh_material_groups;
  smooth_group;
}

named_triangle_object := {
  binary_printf "%d%ld",0x4100,named_triangle_object_length(); // N_TRI_OBJECT
  point_array;
  mesh_matrix;
  face_array;
}

named_object := {
  // NAMED_OBJECT
  binary_printf "%d%ld%s%c",0x4000,named_object_length(),"mesh",0;
  named_triangle_object;

}

main_chunk := {
  binary_printf "%d%ld",0x3D3D,main_chunk_length();
  binary_printf "%d%ld%ld",0x3D3E,0xA,0x3;  // MESH_VERSION
  binary_printf "%d%ld%f",0x0100,0xA,1.0;  // MASTER_SCALE
  material_list;
  named_object;

}

object_node_tag_chunk := {
  binary_printf "%d%ld",0xB002,0x6D;
  binary_printf "%d%ld%d",0xB030,0x8,0; // NODE_ID
  binary_printf "%d%ld%s%c%d%d%d",0xB010,0x11,"mesh",0,0,0,-1; // NODE_ID
  binary_printf "%d%ld%f%f%f",0xB013,0x12,0,0,0; // PIVOT
  binary_printf "%d%ld%d%d%d%d%d%d%d",0xB020,0x14,0,0,0,0,0,0,0; // POS_TRACK_TAG
  binary_printf "%d%ld%d%d%d%d%d%d%d",0xB021,0x14,0,0,0,0,0,0,0; // ROT_TRACK_TAG
  binary_printf "%d%ld%d%d%d%d%d%d%d",0xB022,0x14,0,0,0,0,0,0,0; // SCL_TRACK_TAG
}


kfdata_chunk := {
  binary_printf "%d%ld",0xB000,kfdata_chunk_length();
  binary_printf "%d%ld%d%s%c%ld",0xB00A,0x13,5,"LIB3DS",0,0x64; // KFHDR
  binary_printf "%d%ld%ld%ld",0xB008,0xE,0,0x64; // KFSEG
  binary_printf "%d%ld%ld",0xB009,0xA,0; // KFCURTIME
  object_node_tag_chunk;
}

procedure primary_chunk()
{ local primary_length;

  primary_length := 6;  // chunk header
  primary_length += m3d_version_chunk_length();
  primary_length += main_chunk_length();
  primary_length += kfdata_chunk_length();
  binary_printf "%d%ld",0x4D4D,primary_length;
  m3d_version_chunk;
  main_chunk;
  kfdata_chunk;
}
  
  
do_3ds := {
   // Check maximum number of elements allowed
   if vertex_count > 0xffff then
   { errprintf 
      "do_3ds error: 3DS format can do at most %f vertices.  Aborting.\n",  
         0xffff;
     abort;
   };
   if sum(facet where show and color>=0,1) > 0xffff then
   { errprintf 
       "do_3ds error: 3DS format can do at most %f facets.  Aborting.\n",0xffff;
     abort;
   };

  if transform_count > 1 then
  { errprintf 
      "Cannot run 'do_3ds' command with view transforms. Do 'detorus' first.\n";
    abort;
  };


  if torus then
  { errprintf 
     "Cannot run 'do_3ds' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf 
    "Cannot run 'do_3ds' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'do_3ds' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'do_3ds' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'do_3ds' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'do_3ds' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf 
     "The 'do_3ds' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

  primary_chunk();
}
// End 3ds.cmd.

// Usage: do_3ds >>> "filename.3ds"

