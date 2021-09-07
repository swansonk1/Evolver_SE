// offn.cmd
// Surface Evolver command to print Geomview OFF file with virtual vertices to 
// separate films meeting at triple lines, permitting
// local normal calculation for smoothing.

// Usage: offn >>> "filename.off"

// Algorithm: Goes through facets, and for each vertex on a facet
// compares the facet normal to existing virtual vertex normal. If
// none is close enough, a new virtual vertex is created.

// Limits: Assumes no more than 6 virtual vertices are needed at a vertex.

define vertex attribute vircount integer // number of virtual vertices at vertex
define vertex attribute virlist  real[6] // ID's of virtual vertices
define vertex attribute virnorm  real[18] // virtual normals
define facet attribute virvertex integer[3]

cosine_cutoff := 0.8 // criterion for matching normals

makevir := { 
   local  nx,ny,nz,norm,kk,vn,cosang;

   offn_virnum := 0;
   set vertex vircount 0;
   foreach facet ff where show and color >= 0 do
   { nx := ff.x; ny := ff.y; nz := ff.z;
     norm := sqrt(nx^2 + ny^2 + nz^2);
     nx := nx/norm; ny := ny/norm; nz := nz/norm;
     kk := 1; // which facet vertex
     foreach ff.vertex vv do
     { vn := 1;
       while ( vn <= vv.vircount ) do
       { cosang := nx*vv.virnorm[3*vn-2] + ny*vv.virnorm[3*vn-1] 
                     + nz*vv.virnorm[3*vn];
         if ( abs(cosang) >= cosine_cutoff ) then break;
         vn := vn+1;
       };
       if ( vn > vv.vircount ) then
       { vv.vircount := vv.vircount + 1;
         if ( vv.vircount > 6 ) then
         { printf"Too many virtual vertices at vertex %g facet %g\n",vv.id,ff.id;
           return;
         };
         vv.virlist[vv.vircount] := offn_virnum;
         ff.virvertex[kk] := offn_virnum;
         offn_virnum := offn_virnum + 1;
         vv.virnorm[3*vn-2] := nx;
         vv.virnorm[3*vn-1] := ny;
         vv.virnorm[3*vn] := nz;
       }
       else ff.virvertex[kk] := vv.virlist[vn];
       kk := kk+1;
     }
   }
}

flist := { 
  foreach facet ff where show and color >= 0 do 
  { printf "3 %g %g %g\n",ff.virvertex[1],ff.virvertex[2],ff.virvertex[3];
  }
}
printvir := {
   local vn;

   offn_virnum := 0;
   foreach facet ff do
   { foreach ff.vertex vv do
     { vn := 1;
       while ( vn <= vv.vircount ) do
       { if ( offn_virnum == vv.virlist[vn] ) then
         { printf "%f %f %f\n",vv.x,vv.y,vv.z; // add to output vertex list
           offn_virnum := offn_virnum + 1;
           break;
         };
         vn := vn+1;
       };
     }
   }
}

offn := {  makevir;
           printf "OFF\n%g %g %g\n",offn_virnum,
              sum(facet,show and color >= 0),
              sum(edge,show and color >= 0);
           printvir;
           flist
        }

/* Usage:
    Set cosine_cutoff (default 0.8) for detection of ridges.
    Set facet "show" criterion, if desired. Then
     offn >>> "filename.off"
*/

