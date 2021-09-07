// wavefront.cmd

// Surface Evolver command file for producing Wavefront format file
// for surface, suitable for feeding to JavaView.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage:
// Set "show facet where ..." to show desired facets.
//    wavefront >>> "filename.obj"
// or, for version with normals at vertices,
//    wavefrontn >>> "filename.obj"

wavefront_assumptions := {
  if torus then
  { errprintf "Cannot run 'wavefront' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'wavefront' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'wavefront' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'wavefront' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'wavefront' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'wavefront' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

}

define vertex attribute v_num integer
wavefront := { 
   local new_v_num;

   wavefront_assumptions;

   // Make sure vertices consecutively numbered
   new_v_num := 1;
   foreach vertex vv do { vv.v_num := new_v_num; new_v_num += 1 };
   
   printf "# Wavefront .obj file for %s\n",datafilename;
   printf "# Produced by the Surface Evolver.\n\n";

   foreach vertex vv do printf "v %f %f %f\n",vv.x,vv.y,vv.z;
   printf "\n";
   foreach facet ff where show do 
     printf "f %g %g %g\n",ff.vertex[1].v_num,
      ff.vertex[2].v_num, ff.vertex[3].v_num;

}

// version with normals at vertices
wavefrontn := { 
   local new_v_num;

   wavefront_assumptions;

   // Make sure vertices consecutively numbered
   new_v_num := 1;
   foreach vertex vv do { vv.v_num := new_v_num; new_v_num += 1 };
   
   printf "# Wavefront .obj file for %s\n",datafilename;
   printf "# Produced by the Surface Evolver.\n\n";

   foreach vertex vv do printf "v %f %f %f\n",vv.x,vv.y,vv.z;
   printf "\n";
   foreach vertex vv do printf "vn %f %f %f\n",vv.vertexnormal[1],
      vv.vertexnormal[2],vv.vertexnormal[3];
   printf "\n";
   foreach facet ff where show do 
    printf "f %g/%g %g/%g %g/%g\n",ff.vertex[1].v_num,
      ff.vertex[1].v_num, ff.vertex[2].v_num,
      ff.vertex[2].v_num, ff.vertex[3].v_num,
      ff.vertex[3].v_num;

}

// End wavefront.cmd

/* Usage:
     Do "show facets where ... " to show desired facets.
     wavefront
 or  wavefrontn
*/



