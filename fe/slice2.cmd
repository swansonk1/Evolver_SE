// slice2.cmd --- produce intersection of surface with plane
// plane eq: aa*x + bb*y + cc*z = dd
// output: length of slice, and area inside slice.
// Does not modify surface.
// Note all area inside slice is counted as positive!
// Do not to slice exactly through vertices!!  Avoid special
//  values for dd like 0 or .5!

// Fixed to do oriented facets of a particular body. 

// Usage: set plane coefficients, then do slice2(body number).
// Results: Intersection length printed out, left in variable lensum.
//          Intersection area printed out, left in variable areasum.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

slice_aa := 1;
slice_bb := 0;
slice_cc := 0;
slice_dd := 0;

procedure slice2(integer body_num) { 
       local any,xx1,yy1,zz1,xx2,yy2,zz2;
       local denom,lambda,zaa,zbb,darea;
       local xb,yb,zb,xa,ya,za,dx,dy,dz,fx,fy,fz;

       lensum := 0; areasum := 0;

       xb := 0; yb := 0; zb := 0;  // just for declaration before use.
       foreach body[body_num].facet ff do 
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
           local triple;
           dx := xa-xb; dy := ya-yb; dz := za - zb;
           lensum := lensum + sqrt(dx^2+dy^2+dz^2);
           zaa := za - slice_dd/slice_cc; zbb := zb - slice_dd/slice_cc;
           fx := ff.x; fy := ff.y; fz := ff.z;
           triple := fx*(dy*slice_cc-dz*slice_bb)+fy*(dz*slice_aa-dx*slice_cc)+fz*(dx*slice_bb-dy*slice_aa);

           darea := (xa*yb-xb*ya)/2;
           if ( triple > 0 ) then areasum := areasum + darea
           else areasum := areasum - darea;
         }
       };
       areasum := areasum*sqrt(slice_aa*slice_aa+slice_bb*slice_bb+slice_cc*slice_cc)/slice_cc;
       printf "Circumference: %18.15g  Area: %18.15g\n",lensum,areasum;
     } // end slice


// end slice2.cmd

/* Usage:
   set  slice_aa, slice_bb, slice_cc, slice_dd
   call slice2(integer body_id)
*/






