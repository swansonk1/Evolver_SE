// adjoint.cmd

/* Surface Evolver script for the calculation of the adjoint minimal surface 
   using Konrad Polthier's discrete conjugate method of Bonnet rotation.  See
      Ulrich Pinkall and Konrad Polthier, Computing Discrete Minimal Surfacee 
      and Their Conjugates, Experim. Math. 2(1) (1993) 15-36
      http://page.mi.fu-berlin.de/polthier/articles/diri/diri_jem.pdf
   and 
      Konrad Polthier, Conjugate Harmonic Maps and Minimal Surfaces.
      http://page.mi.fu-berlin.de/polthier/articles/harmonic/Harmonic_preprint.pdf

   The "adjoint" of a smooth minimal surface is another minimal surface 
   isometric to the first that has normals in the same direction at each
   point but the infinitesimal surface element rotated 90 degrees about
   the normal.  Schwartz' P and D surfaces are adjoints of each other.

   A "discrete minimal surface" is made up of flat triangles.  It is called
   "conforming" if the vertices of adjacent triangles coincide, and 
   "nonconforming" if adjacent triangles only meet at their midpoints.
   Polthier shows that if a conforming discrete minimal surface minimizes
   area, then it has an exact nonconforming adjoint.

   The "adjoint" command in this file takes a conforming surface and 
   calculates the nonconforming adjoint, then tweaks vertices to make it 
   conforming, so it is a legal Surface Evolver surface.  

   Assumptions: 3D soapfilm model, linear model, no torus or symmetry group.
   Each edge has only one or two adjacent facets.
   Facets have consistent orientation.
   The original surface should be free of all level set constraints and 
   boundaries before doing "adjoint"; "fixed" vertices and edges are ok.

   Usage:  adjoint

   Before running "adjoint", you can set the variable "starte" to the number
   of the edge whose midpoint will remain fixed in place; default is edge[1].

   After "adjoint" has been run, you can flip back and forth between the 
   original and the adjoint with the "flip" command.

   Usage: flip

   The angle of the Bonnet rotation is set by the variable "bangle", which
   you may set to any degree value before doing "adjoint".  The default
   value of bangle is 90 degrees.

   Also this file has the command "write_conjugate" to write a datafile to 
   stdout that is the nonconforming discrete adjoint, with separated vertices.
   Use this to see Polthier's idea in its true form.

   Usage: write_conjugate >>> "filename.fe"

   For a slightly more elaborate approach using complex numbers for the
   adjoint coordinates, which is faster for doing multiple arbitrary
   rotations, see adjointc.cmd.  This file and adjointc.cmd have 
   non-overlapping namespaces, so both can be loaded simultaneously.

   Programmer: Ken Brakke, brakke@susqu.edu, www.susqu.edu/brakke
*/

// For converting to adjoint
define vertex attribute ad_newx real [3];
define edge attribute ad_eflag integer;
define edge attribute ad_enewx real [3];

// Angle of Bonnet rotation, degrees
bangle := 90

// Swaps conjugate sets of coordinates.
flip := {
   local tmp;
   foreach vertex vv do 
   { tmp := vv.x; vv.x := vv.ad_newx[1]; vv.ad_newx[1] := tmp; 
     tmp := vv.y; vv.y := vv.ad_newx[2]; vv.ad_newx[2] := tmp; 
     tmp := vv.z; vv.z := vv.ad_newx[3]; vv.ad_newx[3] := tmp; 
   }
}


// Conjugation from conforming to nonconforming.  This should be done
// after removing all constraints and boundaries from vertices.
starte := 0    // user should set starte to set vertex that doesn't move.
conconj := {
   local bs,bc,ecount,endflag,loopcount,enum,thise,nexte,othere;
   local nx,ny,nz,norm,vva,vvb,vvc,kk,nbrs;

   // check valence
   foreach edge ee where valence > 2 do
   { errprintf "ERROR: adjointc: Some edges have more than two facets.\n";
     abort;
   };

   // check all facets same orientation
   foreach edge ee where valence == 2 and
      ee.facet[1].oid*ee.facet[2].oid > 0 do
   { errprintf "ERROR: adjointc: Facets have inconsistent orientations.\n";
     abort;
   };

   set edge ad_eflag 0;
   if not valid_element(edge[starte]) then
     foreach vertex ee do { starte := ee.id; break; };  // just to get starter
   edge[starte].ad_enewx[1] := avg(edge[starte].vertex,x);
   edge[starte].ad_enewx[2] := avg(edge[starte].vertex,y);
   edge[starte].ad_enewx[3] := avg(edge[starte].vertex,z);
   edge[starte].ad_eflag := 1;

   bs := sin(bangle*pi/180);
   bc := cos(bangle*pi/180);  
   ecount := 1;
   endflag := 0;
   loopcount := 1;
   while ( !endflag ) do
   { endflag := 1; 
     foreach facet ff do
     { enum := 1; 
       while ( enum <= 5 ) do
       { thise := (enum imod 3) + 1;
         nexte := ((enum+1) imod 3) + 1;
         othere := ((enum+2) imod 3) + 1;
         if ( ff.edge[thise].ad_eflag>=loopcount and !ff.edge[nexte].ad_eflag ) then
         { nx := ff.x;
           ny := ff.y;
           nz := ff.z;
           norm := sqrt(nx^2+ny^2+nz^2);
           nx := nx/norm; ny := ny/norm; nz := nz/norm;
           ff.edge[nexte].ad_enewx[1] := 
              ff.edge[thise].ad_enewx[1] - (bc*ff.edge[othere].x 
                 + bs*(ff.edge[othere].y*nz - ff.edge[othere].z*ny))/2;
           ff.edge[nexte].ad_enewx[2] := 
              ff.edge[thise].ad_enewx[2] - (bc*ff.edge[othere].y 
                 + bs*(ff.edge[othere].z*nx - ff.edge[othere].x*nz))/2;
           ff.edge[nexte].ad_enewx[3] := 
              ff.edge[thise].ad_enewx[3] - (bc*ff.edge[othere].z 
                 + bs*(ff.edge[othere].x*ny - ff.edge[othere].y*nx))/2;

           ff.edge[nexte].ad_eflag := loopcount+1;
           endflag := 0; 
           ecount += 1;
         };
         enum += 1;
       };  // end while
     };
     if !quiet then printf "%g edges done.\n",ecount;
     loopcount += 1;
   };
   set vertex ad_newx[1] 0;
   set vertex ad_newx[2] 0;
   set vertex ad_newx[3] 0;
   foreach facet ff do 
   { // extend center facet to original vertex; can't simply average
     //   midedge vertices around an original vertex since that doesn't
     //   work for vertices on the boundary.
     vva := 1; while ( vva <= 3 ) do
     { vvb := vva==3 ? 1 : vva+1;
       vvc := vva==1 ? 3 : vva-1;
       kk := 1; while ( kk <= 3 ) do
       {
         ff.vertex[vva].ad_newx[kk] += ff.edge[vva].ad_enewx[kk] 
               - ff.edge[vvb].ad_enewx[kk] + ff.edge[vvc].ad_enewx[kk]; 
         kk += 1;
       };
       vva += 1;
     }
   };
   foreach vertex vv do
   { 
     nbrs := sum(vv.facet, 1);
     if ( nbrs != 0 ) then
     { vv.ad_newx[1] /= nbrs;
       vv.ad_newx[2] /= nbrs;
       vv.ad_newx[3] /= nbrs;
     };
   };
  
} // end conconj

// Write split-vertex adjoint datafile.  To be done after conconj;
// uses ad_enewx values to compute vertices.
write_conjugate := {
  printf "// split-vertex discrete adjoint of %s, rotation angle %f.\n",
    datafilename,bangle;
  printf "\nvertices\n";
  foreach facet ff do
  { printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id-2,
      ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]-ff.edge[3].ad_enewx[1],
      ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]-ff.edge[3].ad_enewx[2],
      ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]-ff.edge[3].ad_enewx[3];
    printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id-1,
     -ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1],
     -ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2],
     -ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
    printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id,
      ff.edge[1].ad_enewx[1]-ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1],
      ff.edge[1].ad_enewx[2]-ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2],
      ff.edge[1].ad_enewx[3]-ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
  };
  printf "\nedges\n";
  foreach facet ff do
  { printf "%d   %d %d\n",3*ff.id-2,3*ff.id-2,3*ff.id-1;
    printf "%d   %d %d\n",3*ff.id-1,3*ff.id-1,3*ff.id;
    printf "%d   %d %d\n",3*ff.id,3*ff.id,3*ff.id-2;
  };
  printf "\nfaces\n";
  foreach facet ff do 
    printf "%d    %d %d %d\n",ff.id,3*ff.id-2,3*ff.id-1,3*ff.id;

} // end write_conjugate

adjoint := { 
   local autodisplay_state;

   // Check assumption
  
   if torus then
   { errprintf "Cannot run 'adjoint' command in torus mode.\n";
     abort;
   };
  
   if symmetry_group then
   { errprintf "Cannot run 'adjoint' command in symmetry group mode.\n";
     abort;
   };
  
   if space_dimension != 3 then
   { errprintf "The 'adjoint' command must be run in three-dimensional space.\n";
     abort;
   };
  
   if surface_dimension == 1 then
   { errprintf "The 'adjoint' command is not meant for the string model.\n";
     abort;
   };
  
   if simplex_representation then
   { errprintf "The 'adjoint' command is not meant for the simplex model.\n";
     abort;
   };
  
   if lagrange_order >= 2 then
   { errprintf "The 'adjoint' command is meant for the linear model, not quadratic or Lagrange.\n";
     abort;
   };

   if min(edge,valence) < 1 or max(edge,valence) > 2 then
   { errprintf "The 'adjoint' command requires all edges to have one or two adjacent facets.\n";
     abort;
   };

  
   // Now to business.
   autodisplay_state := (autodisplay);
   autodisplay off;
   conconj;
   flip;
   if ( autodisplay_state ) then autodisplay;
}

// End adjoint.cmd
// Usage:  adjoint
   
