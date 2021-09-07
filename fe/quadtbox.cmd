// quadtbox.cmd
// finds bounding box for quadratic facets 
// Suitable for torus model.

// Tricky to define a bounding box in a torus, with the wraps.  
// So the bounding box is based on unwrapped edges and facets
// relative to the first vertex.;

// Bounding box calculations are done in normalized unit cell
// coordinates, so bounding box is really parallelogram
// parallel to unit cell. 

// Programmer: Ken Brakke, brakke@susqu.edu  www.susqu.edu/brakke

// xmin,xmax,ymin,ymax,zmin,zmax     
define edge attribute ebox real [3][2] 
define facet attribute fbox real [3][2]

eboxes := { 
  local ii,v0,v1,v2,v4,uopt,denom;
  local u0,u1,u2;
  define u0 real[space_dimension];
  define u1 real[space_dimension];
  define u2 real[space_dimension];

  if ( !quadratic ) then 
  { errprintf "eboxes: Must be in quadratic mode.\n";
    abort;
  };
  if ( !torus ) then  
  { errprintf "eboxes: Must be in torus model.\n";
    abort;
  };

  foreach edge ee do 
  { 
    u0 := ee.vertex[1].__x * inverse_periods;
    u1 := ee.edge_vector * inverse_periods;
    u2 := ee.vertex[3].__x * inverse_periods;
    for ( ii := 1; ii <= space_dimension; ii++ )
    {
      // get normalized vertex coordinates, recalling midv is third vertex
      // of the edge, and on the same wrap as the tail.
      v0 := u0[ii];
      v1 := v0 + u1[ii];
      v2 := u2[ii];

      // now set bounding boxes
      if ( v0 > v1 )
        then { ee.ebox[ii][1] := v1; ee.ebox[ii][2] := v0; }
        else { ee.ebox[ii][1] := v0; ee.ebox[ii][2] := v1; };
      if ( v2 < ee.ebox[ii][1] ) then ee.ebox[ii][1] := v2;
      if ( v2 > ee.ebox[ii][2] ) then ee.ebox[ii][2] := v2;
      denom := v0 - 2*v1 + v2;
      if ( denom == 0.0 ) then continue; 
      uopt := (-1.5*v0 + 2*v1 - .5*v2)/denom;
      if ( uopt <= 0.0 or uopt >= 1.0 )
         then continue; 
      v4 := 0.5*(1-uopt)*(2-uopt)*v0 + uopt*(2-uopt)*v1
               + 0.5*uopt*(uopt-1)*v2;
      if ( v4 < ee.ebox[ii][1] ) then ee.ebox[ii][1] := v4;
      if ( v4 > ee.ebox[ii][2] ) then ee.ebox[ii][2] := v4;
     }
   }
} // end eboxes

fboxes := { 
    local ewraps,ii,ww,jj,www,tempbox,vwrap,v0,v1,v2,v3,v4,v5;
    local a11,a12,b1,a21,a22,b2,denom,uopt,vopt,vcrit;

    define ewraps integer[3][space_dimension];
    local u0,u1,u2,u3,u4,u5;
    define u0 real[space_dimension];
    define u1 real[space_dimension];
    define u2 real[space_dimension];
    define u3 real[space_dimension];
    define u4 real[space_dimension];
    define u5 real[space_dimension];

    eboxes;

    if ( ! quadratic ) then
    { print "fboxes: Must be in quadratic mode.\n";
      abort;
    };

    foreach facet ff do
    { // extract wraps on edges
      for ( ii := 1; ii <= 3 ; ii++ )
      { ww := ff.edge[ii].wrap;
        for ( jj := 1 ; jj <= space_dimension ; jj++ )
        { www := ww imod 64;
          if www == 0 then ewraps[ii][jj] := 0
          else if www < 16 then ewraps[ii][jj] := www
          else ewraps[ii][jj] := www-32;
          ww /= 64;
        }
      };

      u0 := ff.edge[1].vertex[1].__x * inverse_periods;
      u1 := ff.edge[1].vertex[3].__x * inverse_periods;
      u2 := ff.edge[1].edge_vector * inverse_periods;
      u3 := ff.edge[3].vertex[3].__x * inverse_periods;
      u4 := ff.edge[2].vertex[3].__x * inverse_periods;
      u5 := ff.edge[2].edge_vector * inverse_periods;
 
      for ( ii := 1; ii <= space_dimension ; ii++ ) 
      { define tempbox real[2];
        /* first, edge boxes */
        ff.fbox[ii] := ff.edge[1].ebox[ii];
        if ff.edge[1].oid < 0 then
          ff.fbox[ii] += ewraps[1][ii];

        vwrap := ewraps[1][ii];
        for ( jj := 2 ; jj <= 3; jj++ )
        { tempbox := ff.edge[jj].ebox[ii];  

          tempbox += vwrap;
          if ff.edge[jj].oid < 0 then
            tempbox += ewraps[jj][ii]; 
          if ( tempbox[1] < ff.fbox[ii][1] )
             then ff.fbox[ii][1] := tempbox[1];
          if ( tempbox[2] > ff.fbox[ii][2] )
             then ff.fbox[ii][2] := tempbox[2];

          vwrap += ewraps[jj][ii];
        };
        v0 := u0[ii];
        v1 := u1[ii];
        v2 := v0 + u2[ii];
        v3 := u3[ii];
        v4 := u4[ii];
        v5 := v2 + u5[ii];
        // fix up midpoint to be near endpoints, since don't have edge vector to midpoint
        v1 := ((7+v1-(v0+v2-1)/2) mod 1) + (v0+v2-1)/2;
        v3 := ((7+v3-(v0+v5-1)/2) mod 1) + (v0+v5-1)/2;
        v4 := ((7+v4-(v5+v2-1)/2) mod 1) + (v5+v2-1)/2;

        // x_u coeff of u
        a11 := v0 - 2*v1 + v2;
        // x_u coeff of v
        a12 := v0 - v1 - v3 + v4;
        // x_u rhs
        b1 := -(-1.5*v0 + 2*v1 - .5*v2);
        // x_v coeff of u
        a21 := v0 - v1 - v3 + v4;
        // x_v coeff of v
        a22 := v0 - 2*v3 + v5;
        // x_v rhs
        b2 := -(-1.5*v0 + 2*v3 - 0.5*v5);
        // solve for critical point
        denom := a11*a22 - a12*a21;
        
        if ( denom == 0.0 ) then continue;
        uopt := (b1*a22 - b2*a12)/denom;
        vopt := (a11*b2 - a21*b1)/denom;
        if ( uopt <= 0.0 ) then continue;
        if ( vopt <= 0.0 ) then continue;
        if ( uopt+vopt >= 2.0 ) then continue;
        vcrit := 0.5*(uopt+vopt-1)*(uopt+vopt-2)*v0
              + uopt*(2-uopt-vopt)*v1
              + 0.5*uopt*(uopt-1)*v2
              + vopt*(2-uopt-vopt)*v3
              + uopt*vopt*v4
              + 0.5*vopt*(vopt-1)*v5;
        if ( vcrit < ff.fbox[ii][1] ) then ff.fbox[ii][1] := vcrit;
        if ( vcrit > ff.fbox[ii][2] ) then ff.fbox[ii][2] := vcrit;
       } 
    } 
} 


// End quadtbox.cmd

// Usage: fboxes

