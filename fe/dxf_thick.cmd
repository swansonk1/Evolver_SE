// dxf_thick.cmd

// Evolver command to produce AutoCad DXF files
// Produces two surfaces displaced normally from 
// actual surface by distance dxf_thickness.
/*
   Assumptions: 3D soapfilm model, linear model, not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Does facets only, not edges. 
   Does facets satisfying "show" criterion.
   Facet frontcolor and backcolor, by doing two surfaces.
*/
// Usage:
//     dxf_thickness := 0.01   // set to total thickness desired
//     dxf_thick >>> "filename.dxf"

// References:
// http://en.wikipedia.org/wiki/AutoCAD_DXF
// http://images.autodesk.com/adsk/files/acad_dxf.pdf

dxf_thickness := 0.01;  // User should set this!

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

dxf_thick := {
 local vn;
 define vn real[3];
 printf "  0\nSECTION\n  2\nENTITIES\n";
 foreach facet ff where show and color >= 0 do
 { 

   // Facet displaced one way
   printf "  0\n3DFACE\n  8\n0main\n";
   vn := ff.vertex[1].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 10\n%8.6f\n 20\n%8.6f\n 30\n%8.6f\n",ff.vertex[1].x+vn[1],
             ff.vertex[1].y+vn[2],ff.vertex[1].z+vn[3];
   vn := ff.vertex[2].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 11\n%8.6f\n 21\n%8.6f\n 31\n%8.6f\n",ff.vertex[2].x+vn[1],
             ff.vertex[2].y+vn[2],ff.vertex[2].z+vn[3];
   vn := ff.vertex[3].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 12\n%8.6f\n 22\n%8.6f\n 32\n%8.6f\n",ff.vertex[3].x+vn[1],
             ff.vertex[3].y+vn[2],ff.vertex[3].z+vn[3];
   printf " 13\n%8.6f\n 23\n%8.6f\n 33\n%8.6f\n",ff.vertex[3].x+vn[1],
             ff.vertex[3].y+vn[2],ff.vertex[3].z+vn[3];
   // try color
   printf " 62\n%3d\n", ff.frontcolor > 0 ? dxf_colors[ff.frontcolor] : 0;

   // Facet displaced other way, also reversed orientation
   printf "  0\n3DFACE\n  8\n0main\n";
   vn := ff.vertex[2].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 11\n%8.6f\n 21\n%8.6f\n 31\n%8.6f\n",ff.vertex[2].x-vn[1],
             ff.vertex[2].y-vn[2],ff.vertex[2].z-vn[3];
   vn := ff.vertex[1].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 10\n%8.6f\n 20\n%8.6f\n 30\n%8.6f\n",ff.vertex[1].x-vn[1],
             ff.vertex[1].y-vn[2],ff.vertex[1].z-vn[3];
   vn := ff.vertex[3].vertex_normal;  // displacement vector
   vn := (dxf_thickness/2)*vn;
   printf " 12\n%8.6f\n 22\n%8.6f\n 32\n%8.6f\n",ff.vertex[3].x-vn[1],
             ff.vertex[3].y-vn[2],ff.vertex[3].z-vn[3];
   printf " 13\n%8.6f\n 23\n%8.6f\n 33\n%8.6f\n",ff.vertex[3].x-vn[1],
             ff.vertex[3].y-vn[2],ff.vertex[3].z-vn[3];
   // try color
   printf " 62\n%3d\n",ff.backcolor > 0 ? dxf_colors[ff.backcolor] : 0;
 };
 printf "  0\nENDSEC\n  0\nEOF\n";
}

// End dxf_thick.cmd

// Usage: dxf_thick >>> "filename.dxf"

