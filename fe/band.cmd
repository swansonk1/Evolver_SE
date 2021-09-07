// band.cmd
// Surface Evolver command to create triangulated band bordering
// deisgnated edges and vertices.  Purpose is to make extremely
// accurate PostScript files without notching on thick edges.

// WARNING: This command modifies the surface by creating a lot of
// tiny facets.  You should use only on a disposable copy of your surface.

/* Assumptions:
     3D soapfilm model.
     Linear model.
*/
// Usage: set bandcolor and bandwidth to desired values; bandwidth is
//           the width of the band in surface coordinates on one
//           side of an edge.
//        set the edge attribute inband to nonzero for those edges
//           to have band drawn along them.
//        run makeband

/* To make PostScript file of the resulting surface, do like this:
      Enter command: show edge where 0
      Enter command: P 3
      Show grid lines? n
      Do colors? y
      Do labels? (i for ids, o for originals) n
      Enter file name (.ps will be added): xxx.ps

      or in scripts:
         show edge where 0;
         ps_gridflag off;
         ps_colorflag on;
         ps_labelflag off;
         postscript "xxx.ps"
*/

// IMPORTANT NOTE:  The algorithm may have trouble where inband edges meet
// at an angle, since each vertex has a single band distance attribute
// and the vertex may be a neighbor to two different bands.  This results
// in ragged bands near corners.  A cure is to do banding in stages,
// doing one "original" edge at a time.  But try banding all desired edges
// at once, since you may not have any trouble.

// Not wise to run more than once along a given edge.  Numerical
// variations due to new vertices lead to a lot more unnecessary
// refinements.

define edge attribute inband integer     // 1 if band center, 0 if not
define vertex attribute banddist real    // distance from band center.
define facet attribute bandused integer  // so use each facet at most once
define vertex attribute band_generation integer

bandcolor := black  // Set this to desired color before makeband.
bandwidth := 0.010  // Set this to desired half-width before makeband.


init_bandcalc := {
   // initialize vertex distances from band
   maxbanddist := 100000*bandwidth;
   set vertex banddist maxbanddist;
   set vertex band_generation 100000;
   foreach edge ee where inband do 
   { set ee.vertex banddist 0;
     set ee.vertex band_generation 0;
   };

   // Refine any edge whose endpoints are in band center but edge is not
   foreach edge ee where (ee.inband==0) and (ee.vertex[1].banddist==0)
       and (ee.vertex[2].banddist==0)  
    do { refine ee; ee.vertex[2].banddist := maxbanddist; } ;

}

function integer calc_v_banddist(integer vv_id)
{ local changed,ddiff,ss1,ss2,lambda1,lambda2;
  local xx,yy,newd,ddd;

  changed := 0;
  foreach vertex[vv_id] vv do
  {
       foreach vv.edge ee where ee.vertex[2].banddist < bandwidth do
       { foreach ee.vertex[2].edge eee where eee.id != ee.id and
                eee.vertex[2].banddist < maxbanddist do
         { 

           ddiff := eee.vertex[2].banddist - eee.vertex[1].banddist;
           ss1 := eee.length;
           if abs(ddiff) > ss1*1.0000000001 then
           { // wrapping around something
             if ddiff > 0 then
               eee.vertex[2].banddist := eee.vertex[1].banddist + ss1
             else
               eee.vertex[1].banddist := eee.vertex[2].banddist + ss1;
             changed += 1;
             continue;
           }
           else if abs(ddiff) == ss1 then continue;
           
           ss2 := ee.length;
           lambda1 := -(ee.edge_vector * eee.edge_vector)/ss1^2;
           lambda2 := sqrt(ss2^2 - lambda1^2*ss1^2);
           xx := lambda1*ss1*sqrt(ss1^2 - ddiff^2)/ss1 - lambda2*ddiff/ss1;
           yy := lambda1*ss1*ddiff/ss1 + lambda2*sqrt(ss1^2-ddiff^2)/ss1;
           if xx < 0 then
           { // add radius from tail
             newd := eee.vertex[1].banddist + ss2;
           }
           else if xx > sqrt(ss1^2 - ddiff^2) then
           { // add radius from head
             ddd := (vv.__x - eee.vertex[2].__x)*(vv.__x - eee.vertex[2].__x); 
             newd := eee.vertex[2].banddist + sqrt(ddd);
           }
           else 
           { // between endpoints
             newd := eee.vertex[1].banddist + yy;
           };
           if newd < vv.banddist*0.99999999 then 
           { vv.banddist := newd;
             changed += 1;
           }
         };
       }
  };
  return changed;
}


calc_banddist := {

   local changed,this_generation;

   init_bandcalc;

   // Calculate vertex distances from band center, using
   // only vertices of previous generation, to prevent using
   // vertex distance value that is not finalized yet. 
   for ( this_generation := 1 ; ; this_generation += 1 ) 
   { changed := 0;
     foreach vertex vv where banddist > 0 do
     {
       changed += calc_v_banddist(vv.id);
     };
     if not changed then break;
   };

} // end calc_band_dist

makeband := {
   local eps;

   calc_banddist;

   // Subdivide edges spanning band boundary
   eps := 1e-4*bandwidth;  // numerical margin for error
   foreach edge ee do
   { local d1,d2;
     local new_x,new_y,new_z;
     local lambda;

     d1 := ee.vertex[1].banddist;
     d2 := ee.vertex[2].banddist;
     if ( ((d1<=bandwidth+eps)&&(d2<=bandwidth+eps)) || 
           ((d1>=bandwidth-eps)&&(d2>=bandwidth-eps)))
       then continue;
     lambda := (d1 - bandwidth)/(d1 - d2);
     if ( lambda < .00001 or lambda > .99999 ) then continue;
     new_x := ee.vertex[1].x + lambda*ee.x;  // suitable for torus model
     new_y := ee.vertex[1].y + lambda*ee.y;
     new_z := ee.vertex[1].z + lambda*ee.z;
     refine ee;
     ee.vertex[2].x := new_x;
     ee.vertex[2].y := new_y;
     ee.vertex[2].z := new_z;
     ee.vertex[2].banddist := bandwidth;
   };

   // Color band triangles
   foreach facet ff do
   { if avg(ff.vertex,banddist) < bandwidth then set ff color bandcolor; };

}

// to do edges on different constraints one constraint at a time.
cband := {
   local connum;
   for ( connum := 1 ; connum <= high_constraint ; connum += 1 ) 
   { if valid_constraint(connum) then
     { set edge inband on_constraint connum;
       makeband;
     }
   };
}

// End band.cmd
// makeband usage:
//   set bandwidth to desired half-width (in surface coordinates)
//   set edge inband to 1 along band center, 0 elsewhere
//   set bandcolor to desired color
//   run makeband
// In making postscript, answer N to gridlines, and y to colors.


