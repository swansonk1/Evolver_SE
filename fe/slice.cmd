// slice.cmd --- Calculate length of intersection of plane with surface.
// Does not modify surface.
// plane eq: aa*x + bb*y + cc*z = dd
// output: prints length of slice, and area inside slice.
// Note all area inside slice is counted as positive!
// Try not to slice exactly through vertices!!

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: Set plane coefficients, then do "slice".
// Results: Intersection length printed out, left in variable lensum.

slice_aa := 0; slice_bb := 0; slice_cc := 1; slice_dd := .1;  // set these for desired plane
slice := { 
       local any,xx1,yy1,zz1,xx2,yy2,zz2;
       local denom,lambda,zaa,zbb,darea;
       local xa,ya,za,xb,yb,zb,dx,dy,dz;

       lensum := 0; areasum := 0;
       foreach facet ff do 
       { any := 0; 
         foreach ff.edge ee do 
         {
           xx1 := ee.vertex[1].x; 
           yy1 := ee.vertex[1].y; 
           zz1 := ee.vertex[1].z; 
           xx2 := ee.vertex[2].x; 
           yy2 := ee.vertex[2].y; 
           zz2 := ee.vertex[2].z;
           denom := slice_aa*(xx1-xx2)+slice_bb*(yy1-yy2)+slice_cc*(zz1-zz2);
           if ( denom != 0.0 ) then 
           { 
             lambda := (slice_dd-slice_aa*xx2-slice_bb*yy2-slice_cc*zz2)/denom; 
             if ( (lambda >= 0) and (lambda <= 1) ) then 
             { 
               xa := xb; ya := yb; za := zb;
               xb := lambda*xx1+(1-lambda)*xx2; 
               yb := lambda*yy1+(1-lambda)*yy2;
               zb := lambda*zz1+(1-lambda)*zz2; 
               any := any+1; 
             } 
           }
        } ; 
        if any == 2 then
        { 
          dx := xa-xb; dy := ya-yb; dz := za - zb;
          lensum := lensum + sqrt(dx^2+dy^2+dz^2);
          zaa := za - slice_dd/slice_cc; zbb := zb - slice_dd/slice_cc;
          darea := sqrt((ya*zbb-yb*zaa)^2+(zaa*xb-zbb*xa)^2+(xa*yb-ya*xb)^2);
          areasum := areasum + darea/2;
        }
      };
      printf "Circumference: %18.15g  Area: %18.15g\n",lensum,areasum;
    } // end slice

// end slice.cmd

/* Usage:
   set  slice_aa, slice_bb, slice_cc, slice_dd
   call slice
*/



