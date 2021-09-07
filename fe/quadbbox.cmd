// quadbbox.cmd
// Finds bounding box for each facet or edge in quadratic model.
// Not suitable for torus model.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: Run eboxes or fboxes.  Results left in the arrays ebox or fbox
// attributes.

// xmin,xmax,ymin,ymax,zmin,zmax     
define edge attribute ebox real [space_dimension][2]   
define facet attribute fbox real [space_dimension][2]

eboxes := { 
            local ii,v0,v1,v2,v4,uopt,denom;
            if ( !quadratic ) then 
            { print "eboxes: Must be in quadratic mode.\n";
              abort;
            };
            foreach edge ee do 
             { 
               for ( ii := 1; ii <= space_dimension ; ii++ )
               {
                 v0 := ee.vertex[1].__x[ii];
                 v2 := ee.vertex[2].__x[ii];
                 v1 := ee.vertex[3].__x[ii];
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
            local ii,jj,v0,v1,v2,v3,v4,v5,uopt,denom;
            local a11,a12,b1,a21,a22,b2,vopt,vcrit;

            eboxes;

            if ( ! quadratic ) then print "fboxes: Must be in quadratic mode.\n"
            else foreach facet ff do
            { 
              for ( ii := 1; ii < space_dimension ; ii++ )
              { 
                /* first, edge boxes */
                ff.fbox[ii] := ff.edge[1].ebox[ii];
                for ( jj := 2 ; jj <= 3; jj++ )
                { if ( ff.edge[jj].ebox[ii][1] < ff.fbox[ii][1] )
                     then ff.fbox[ii][1] := ff.edge[jj].ebox[ii][1];
                  if ( ff.edge[jj].ebox[ii][2] > ff.fbox[ii][2] )
                     then ff.fbox[ii][2] := ff.edge[jj].ebox[ii][2];
                };
                v0 := ff.edge[1].vertex[1].__x[ii];
                v1 := ff.edge[1].vertex[3].__x[ii];
                v2 := ff.edge[1].vertex[2].__x[ii];
                v3 := ff.edge[3].vertex[3].__x[ii];
                v4 := ff.edge[2].vertex[3].__x[ii];
                v5 := ff.edge[2].vertex[2].__x[ii];
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
        }  // end fboxes


// End quadbbox.cmd

// Usage: eboxes
//        fboxes
