// adjointc.cmd

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

   Before running "adjointc", you can set the variable "starte" to the number
   of the edge whose midpoint will remain fixed in place; default is edge[1].

   Also this file has the command "write_conj" to write a datafile to 
   stdout that is the nonconforming discrete adjoint, with separated vertices.
   Use this to see Polthier's idea in its true form.

   Usage: write_conj >>> "filename.fe"

   For a slightly more elaborate approach using complex numbers for the
   adjoint coordinates, which is faster for doing multiple arbitrary
   rotations, see adjointc.cmd.  This file and adjointc.cmd have 
   non-overlapping namespaces, so both can be loaded simultaneously.

   Programmer: Ken Brakke, brakke@susqu.edu, www.susqu.edu/brakke
*/



// Also has command "write_conj" to write a datafile to stdout that
// is the official discrete adjoint, with separated vertices.

// Adjointc.cmd differs from adjoint.cmd in carrying through complex
// coordinates, so different Bonnet rotations can be generated simply
// by setting a phase angle and calculating the real coordinates from
// the complex coordinates.

/* Usage: 

   procedure adjointc(real bangle, integer origin_vertex ) 

     Arguments:
        bangle - Bonnet rotation angle in degrees.
        origin_vertex - vertex to be kept fixed.

     Description:
        Calculates complex Bonnet rotation coordinates and applies
        rotation by given angle.
      


   procedure bonnet_rotation(real bangle, integer origin_vertex ) 

     Arguments:
        bangle - Bonnet rotation angle in degrees.
        origin_vertex - vertex to be kept fixed.

     Description:
        Uses the complex Bonnet rotation coordinates previously
        calculated by adjointc to compute rotation by given angle.
        Note the angle is absolute, from the state input to 
        adjointc, rather than cumulative.

*/

// For converting to adjoint
define vertex attribute adc_newx real [3][2]; // coordinates of vertices
define edge attribute adc_eflag integer;
define edge attribute adc_enewx real [3][2]; // coordinates of edge midpoints


// Conjugation from conforming to nonconforming.  This should be done
// after removing all constraints and boundaries from vertices.
startv := 0    // user should set startv to set vertex that doesn't move.
conconjc := {
   local starte;
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
   set edge adc_eflag 0;
   if starte == 0 then
     foreach edge ee do { starte := ee.id; break; };  // just to get starter
   edge[starte].adc_enewx[1] := avg(edge[starte].vertex,x);
   edge[starte].adc_enewx[2] := avg(edge[starte].vertex,y);
   edge[starte].adc_enewx[3] := avg(edge[starte].vertex,z);
   edge[starte].adc_eflag := 1;

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
         if ( ff.edge[thise].adc_eflag >= loopcount and !ff.edge[nexte].adc_eflag ) then
         { nx := ff.x;
           ny := ff.y;
           nz := ff.z;
           norm := sqrt(nx^2+ny^2+nz^2);
           nx := nx/norm; ny := ny/norm; nz := nz/norm;
           ff.edge[nexte].adc_enewx[1][1] := 
              ff.edge[thise].adc_enewx[1][1] - ff.edge[othere].x/2; 
           ff.edge[nexte].adc_enewx[1][2] := 
              ff.edge[thise].adc_enewx[1][2] - 
                 (ff.edge[othere].y*nz - ff.edge[othere].z*ny)/2;
           ff.edge[nexte].adc_enewx[2][1] := 
              ff.edge[thise].adc_enewx[2][1] - ff.edge[othere].y/2;
           ff.edge[nexte].adc_enewx[2][2] := 
              ff.edge[thise].adc_enewx[2][2] - 
                 (ff.edge[othere].z*nx - ff.edge[othere].x*nz)/2;
           ff.edge[nexte].adc_enewx[3][1] := 
              ff.edge[thise].adc_enewx[3][1] - ff.edge[othere].z/2;
           ff.edge[nexte].adc_enewx[3][2] := 
              ff.edge[thise].adc_enewx[3][2] - 
                 (ff.edge[othere].x*ny - ff.edge[othere].y*nx)/2;

           ff.edge[nexte].adc_eflag := loopcount+1;
           endflag := 0; 
           ecount += 1;
         };
         enum += 1;
       };  // end while
     };
     if !quiet then printf "%g edges done.\n",ecount;
     loopcount += 1;
   };
   set vertex adc_newx 0;
   foreach facet ff do 
   { // extend center facet to original vertex; can't simply average
     //   midedge vertices around an original vertex since that doesn't
     //   work for vertices on the boundary.
     vva := 1; while ( vva <= 3 ) do
     { vvb := vva==3 ? 1 : vva+1;
       vvc := vva==1 ? 3 : vva-1;
       ff.vertex[vva].adc_newx += ff.edge[vva].adc_enewx 
               - ff.edge[vvb].adc_enewx + ff.edge[vvc].adc_enewx; 
       vva += 1;
     }
   };
   foreach vertex vv do
   { 
     nbrs := sum(vv.facet, 1);
     if ( nbrs != 0 ) then
       vv.adc_newx /= nbrs;
   };
  
} // end conconjc

// Write split-vertex adjoint datafile.  To be done after conconj;
// uses adc_enewx values to compute vertices.
// Input: bangle, rotation angle in degrees.
procedure write_conj(real bangle) 
{ local bc,bs;

  printf "// split-vertex discrete adjoint of %s, rotation angle %f.\n",
    datafilename,bangle;
  bc := cos(bangle*pi/180);
  bs := sin(bangle*pi/180);
  printf "\nvertices\n";
  foreach facet ff do
  { printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id-2,
      bc*(ff.edge[1].adc_enewx[1][1]+ff.edge[2].adc_enewx[1][1]-ff.edge[3].adc_enewx[1][1]) +
      bs*(ff.edge[1].adc_enewx[1][2]+ff.edge[2].adc_enewx[1][2]-ff.edge[3].adc_enewx[1][2]) ,
      bc*(ff.edge[1].adc_enewx[2][1]+ff.edge[2].adc_enewx[2][1]-ff.edge[3].adc_enewx[2][1]) +
      bs*(ff.edge[1].adc_enewx[2][2]+ff.edge[2].adc_enewx[2][2]-ff.edge[3].adc_enewx[2][2]) ,
      bc*(ff.edge[1].adc_enewx[3][1]+ff.edge[2].adc_enewx[3][1]-ff.edge[3].adc_enewx[3][1]) +
      bs*(ff.edge[1].adc_enewx[3][2]+ff.edge[2].adc_enewx[3][2]-ff.edge[3].adc_enewx[3][2]) ;
    printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id-1,
     bc*(-ff.edge[1].adc_enewx[1][1]+ff.edge[2].adc_enewx[1][1]+ff.edge[3].adc_enewx[1][1]) +
     bs*(-ff.edge[1].adc_enewx[1][2]+ff.edge[2].adc_enewx[1][2]+ff.edge[3].adc_enewx[1][2]) ,
     bc*(-ff.edge[1].adc_enewx[2][1]+ff.edge[2].adc_enewx[2][1]+ff.edge[3].adc_enewx[2][1]) +
     bs*(-ff.edge[1].adc_enewx[2][2]+ff.edge[2].adc_enewx[2][2]+ff.edge[3].adc_enewx[2][2]) ,
     bc*(-ff.edge[1].adc_enewx[3][1]+ff.edge[2].adc_enewx[3][1]+ff.edge[3].adc_enewx[3][1]) +
     bs*(-ff.edge[1].adc_enewx[3][2]+ff.edge[2].adc_enewx[3][2]+ff.edge[3].adc_enewx[3][2]) ;
    printf "%d   %15.10f %15.10f %15.10f\n",3*ff.id,
      bc*(ff.edge[1].adc_enewx[1][1]-ff.edge[2].adc_enewx[1][1]+ff.edge[3].adc_enewx[1][1]) +
      bs*(ff.edge[1].adc_enewx[1][2]-ff.edge[2].adc_enewx[1][2]+ff.edge[3].adc_enewx[1][2]) ,
      bc*(ff.edge[1].adc_enewx[2][1]-ff.edge[2].adc_enewx[2][1]+ff.edge[3].adc_enewx[2][1]) +
      bs*(ff.edge[1].adc_enewx[2][2]-ff.edge[2].adc_enewx[2][2]+ff.edge[3].adc_enewx[2][2]) ,
      bc*(ff.edge[1].adc_enewx[3][1]-ff.edge[2].adc_enewx[3][1]+ff.edge[3].adc_enewx[3][1]) +
      bs*(ff.edge[1].adc_enewx[3][2]-ff.edge[2].adc_enewx[3][2]+ff.edge[3].adc_enewx[3][2]) ;
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
}

procedure bonnet_rotate(real bangle /* degrees */, integer origin_vertex )
{ local rotarray,origin,displacement;
  define rotarray real[2];
  define origin real[3];
  define displacement real[3];
  origin := vertex[origin_vertex].__x;
  rotarray[1] := cos(bangle*pi/180);
  rotarray[2] := sin(bangle*pi/180);
  set vertex __x adc_newx*rotarray;
  displacement := vertex[origin_vertex].__x - origin;
  set vertex __x __x - displacement;
}

procedure adjointc(real bangle /* degrees */, integer origin_vertex ) 
{ 
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
   conconjc;
   bonnet_rotate(bangle,origin_vertex);
   if ( autodisplay_state ) then autodisplay;

} // end adjointc()

// End adjointc.cmd
// Usage:  adjointc(bonnet_angle,origin_vertex)
//         bonnet_rotate(bonnet_angle,origin_vertex)   
