// dxf.cmd

// Evolver command to produce AutoCad DXF files

/*
   Assumptions: 3D soapfilm model, linear model, not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Does facets only, not edges. 
   Does facets satisfying "show" criterion.
   Facet color is frontcolor only.
*/

// Programmer: Ken Brakke, brakke@susqu.edu

// References:
// http://en.wikipedia.org/wiki/AutoCAD_DXF
// http://images.autodesk.com/adsk/files/acad_dxf.pdf

// Usage:
//     dxf >>> "filename.dxf"

define dxf_colors integer[16];
dxf_colors[1] := 5 // blue
dxf_colors[2] := 3 // green
dxf_colors[3] := 4 // cyan
dxf_colors[4] := 10 // red
dxf_colors[5] := 6 // magenta
dxf_colors[6] := 1 // brown
dxf_colors[7] := 9 // lightgray
dxf_colors[8] := 8 // darkgray
dxf_colors[9] := 8 // lightblue
dxf_colors[10] := 3 // lightgreen
dxf_colors[11] := 4 // lightcyan
dxf_colors[12] := 14 // lightred
dxf_colors[13] := 6 // lightmagenta
dxf_colors[14] := 2 // yellow
dxf_colors[15] := 7 // white

dxf := {

  if torus then
  { errprintf "Cannot run 'dxf' command in torus mode. Do 'detorus' first.\n";
    errprintf "    WARNING: 'detorus' will modify the surface; save it!\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'dxf' command in symmetry group mode. Do 'detorus' first.\n";
    errprintf "    WARNING: 'detorus' will modify the surface; save it!\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'dxf' command must be run in three-dimensional space.\n";
    abort;
  };


  if surface_dimension == 1 then
  { errprintf "The 'dxf' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'dxf' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'dxf' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };



 printf "  0\nSECTION\n  2\nENTITIES\n";
 foreach facet ff where show  and color >= 0 do
 { printf "  0\n3DFACE\n  8\n0main\n";
   printf " 10\n%8.6f\n 20\n%8.6f\n 30\n%8.6f\n",ff.vertex[1].x,
             ff.vertex[1].y,ff.vertex[1].z;
   printf " 11\n%8.6f\n 21\n%8.6f\n 31\n%8.6f\n",ff.vertex[2].x,
             ff.vertex[2].y,ff.vertex[2].z;
   printf " 12\n%8.6f\n 22\n%8.6f\n 32\n%8.6f\n",ff.vertex[3].x,
             ff.vertex[3].y,ff.vertex[3].z;
   printf " 13\n%8.6f\n 23\n%8.6f\n 33\n%8.6f\n",ff.vertex[3].x,
             ff.vertex[3].y,ff.vertex[3].z;
   // try color
   if ff.color > 0 and ff.color < 16 then
     printf " 62\n%3d\n",dxf_colors[ff.color]
   else
     printf " 62\n%3d\n",dxf_colors[8];  // black?
 };
 printf "  0\nENDSEC\n  0\nEOF\n";
}

// End dxf.cmd

// Usage:  dxf >>> "filename.dxf"


