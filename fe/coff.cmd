// Surface Evolver command to print color OFF file.
// usage:
//   coff >>> "filename.off"


// Surface Evolver command to print Geomview OFF file in ascii format.  
// Only facets that qualify under the current "show facets" criterion
// are listed.   Facet frontcolor is used; backcolor ignored.

// Edges are not done, since the OFF format does not include them.

/* usage: read in this command file
          set the "show facets" expression (optional)
          run "coff" and redirect output to file.
   Example:
     Enter command: read "coff.cmd"
     Enter command: show facet where not fixed
     Enter command: coff >>> "filename.coff"
*/
     
// Reference: http://www.geom.uiuc.edu/software/geomview/geomview_6.html#SEC36


// Evolver's RGB components for color numbers. Fortunately, the Evolver
// command line has convenient array initializer syntax.
define rgb_coff real[16][4];
rgb_coff := {{ 0.0, 0.0, 0.0, 1.},{ 0.0, 0.0, 1.0, 1.0},{ 0.0, 1.0, 0.0, 1.0},
             { 0.0, 1.0, 1.0, 1.0},{ 1.0, 0.0, 0.0, 1.0},{ 1.0, 0.0, 1.0, 1.0},
             { 1.0, 0.5, 0.0, 1.0},{ 0.6, 0.6, 0.6, 1.0},{ 0.3, 0.3, 0.3, 1.0},
             { 0.3, 0.8, 1.0, 1.0},{ 0.5, 1.0, 0.5, 1.0},{ 0.5, 1.0, 1.0, 1.0},
             { 1.0, 0.5, 0.5, 1.0},{ 1.0, 0.5, 1.0, 1.0},{ 1.0, 1.0, 0.0, 1.0},
             { 1.0, 1.0, 1.0, 1.0}};

coff := {
   local inx,fcount,ecount;

  // Check assumptions
  if torus then
  { errprintf "Cannot run 'coff' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'coff' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'coff' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'coff' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'coff' command is not meant for the simplex model.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'coff' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'coff' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

   // Consecutive index numbers for vertices
   local v_inx;
   define v_inx integer[max(vertex,id)];
   inx := 0;
   foreach vertex vv do 
   { v_inx[vv.id] := inx;
     inx += 1;
   };
   fcount := sum(facet,show and color >= 0);
   ecount := sum(edge,show and color >= 0);

   // file header is ascii
   printf "COFF\n";
   printf "%d %d %d\n",vertex_count,fcount,ecount;

   // vertex list
   foreach vertex do { printf "%f %f %f\n",x,y,z };

   // Triangle list.  Note rgb color indexes use +1 since
   // colors are 0-15 but Evolver array indexes start at 1.
   foreach facet ff where ff.show and ff.color >= 0 do 
   { printf "%d ",3;
     foreach ff.vertex vv do printf "%d ",v_inx[vv.id];
     printf "%f %f %f\n",rgb_coff[ff.color+1][1],
          rgb_coff[ff.color+1][2],rgb_coff[ff.color+1][3];
   };

   define v_inx integer[0];  // deallocate v_inx storage
}

// End coff.cmd
// Usage:  coff >>> "filename.coff"

