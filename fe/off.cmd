// Surface Evolver command to print Geomview OFF file.  No colors or normals.
// Does not require vertex order to be sequential.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// usage:
//   do_off >>> "filename.off"

define vertex attribute off_number integer

do_off := {
  local offnum;

  if torus then
  { errprintf "Cannot run 'off' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'off' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'off' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'off' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'off' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'off' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };


   // file header
   printf "OFF\n%g %g %g\n",vertex_count,
      sum(facet,show and color >= 0),0;

   // vertex list
   offnum := 0;
   foreach vertex vv do 
   { printf "%f %f %f \n",vv.x,vv.y,vv.z;
     vv.off_number := offnum;
     offnum += 1;
   };

   // triangle list
   foreach facet ff where show and color >= 0 do 
   { printf "3 ";
     foreach ff.vertex vv do printf "%g ",vv.off_number; printf "\n";
   }
}

// usage:
//   Set "show" criterion for facets, if desired. Then
//   do_off >>> "filename.off"

