// off_show.cmd

// Surface Evolver command to print OFF file.
// Only facets that qualify under the current "show facets" criterion
// are listed.   No color in this file; see coff.cmd for color OFF.

// Edges are not done, since the OFF format does not include them.

/* usage: read in this command file
          set the "show facets" expression (optional)
          run "off_show" and redirect output to file.
   Example:
     Enter command: read "off_show.cmd"
     Enter command: show facet where not fixed
     Enter command: off_show >>> "filename.off"
*/
     

off_show := {
   local inx,fcount,ecount,v_inx;

  if torus then
  { errprintf "Cannot run 'off_show' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'off_show' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'off_show' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'off_show' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'off_show' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'off_show' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'off_show' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };
   // Consecutive index numbers for vertices
   define v_inx integer[max(vertex,id)];
   inx := 0;
   foreach vertex vv do 
   { v_inx[vv.id] := inx;
     inx += 1;
   };
   fcount := sum(facet,show);
   ecount := sum(edge,show);

   // file header is ascii
   printf "OFF\n";
   printf "%d %d %d\n",vertex_count,fcount,ecount;

   // vertex list
   foreach vertex do { printf "%f %f %f\n",x,y,z };

   // Triangle list.  
   foreach facet ff where ff.show do 
   { printf "%d ",3;
     foreach ff.vertex vv do printf "%d ",v_inx[vv.id];
     printf "\n";
   };

   define v_inx integer[0];  // deallocate v_inx storage
}

// end off_show.cmd

// Usage: off_show >>> "filename.off"

