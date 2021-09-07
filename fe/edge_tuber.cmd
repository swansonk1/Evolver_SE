// edge_tuber.fe
// Tubing edges.  Idea is to get the edge network you want by refining
// facets, deleting facets, deleting edges, then tubing what is left.
// Does nice junctions at vertices, and caps at valence 1 vertices.
// Same radius on all tubes.
  
/* Usage:
   Set edge_tuber_sides to desired value of sides around edge, default 6.
   Set edge_tuber_cap_zones to number of latitudinal zones wanted on edge caps,
      default 2.

   Do one of the following:
   a. To simply tube a given set of edges:
        set edge tuber_status tuber_edge_to_do where ...
        edge_tuber(radius) 
   b. To dissolve all facets and tube all edges:
        edge_tuber_naked(radius)
   c. To create dual edges and tube them:
        edge_tuber_dual(radius)

   Limitations: Only works with 3D soapfilm linear model, no torus or symmetry mode.
*/

edge_tuber_sides := 6;
edge_tuber_cap_zones := 2;
edge_tuber_maxv := edge_tuber_sides+15

// Vertices created at each vertex for each edge, stored per edge end

define edge attribute ehub integer[2][edge_tuber_maxv];  // vertex id's of hub vertices
define edge attribute ehub_count integer[2];    // how many hub vertices
define edge attribute tuber_status integer;     // edges to be tubed
tuber_edge_to_do := 1
tuber_edge_done  := 2

/* for debugging */
define facet attribute tuber_orig_edge integer  // edge the facet came from


// For adjusting tube radius after construction
define vertex attribute tuber_spot real[3]  // zero radius spot
define vertex attribute tuber_vec real[3]   // radius parametric vector

/**********************************************************************************************/

// Cap at end of edge at valence 1 vertex.
procedure edge_tuber_cap(integer v_id, real radius)
{ local sides,evec,tipv,lintelv,vx,ribe,roofe,lintele,diage,perp1,perp2;
  local inx,jnx,cangle,cc,ss,iangle,iinx,f_id;

  sides := edge_tuber_sides;

  define vx real[3];
  define evec real[3];
  define perp1 real[3];
  define perp2 real[3];
  define lintelv integer[edge_tuber_cap_zones+1][sides];
  define ribe integer[edge_tuber_cap_zones][sides];    
  define lintele integer[edge_tuber_cap_zones][sides];
  define diage integer[edge_tuber_cap_zones][sides];

  foreach vertex[v_id].edge ee where tuber_status == tuber_edge_to_do do
  {
    evec := ee.edge_vector;
    evec /= -sqrt(evec*evec);
   
    // get orthogonal basis
    if abs(evec[1]) > 0.5 then  // cross with (0,1,0)
      perp1 := { evec[3],0,-evec[1]}
    else 
      perp1 := { 0, -evec[3], evec[2]};
    perp1 /= sqrt(perp1*perp1);
    perp2 := { evec[2]*perp1[3]-evec[3]*perp1[2],
               evec[3]*perp1[1]-evec[1]*perp1[3],
               evec[1]*perp1[2]-evec[2]*perp1[1]};
    perp2 /= sqrt(perp2*perp2);
  
    // Create vertices
  
    // Tip vertex
    vx := vertex[v_id].__x + radius*evec;
    tipv := new_vertex(vx[1],vx[2],vx[3]);
    vertex[tipv].tuber_spot := vertex[v_id].__x;
    vertex[tipv].tuber_vec  := evec;
    lintelv[edge_tuber_cap_zones+1] := tipv;  // handy to have
  
    // Rings of vertices
    for ( inx := 1 ; inx <= sides ; inx++ )
    { iangle := 2*pi*inx/edge_tuber_sides;
      for ( jnx := 0 ; jnx < edge_tuber_cap_zones ; jnx++ )
      { cangle := pi/2*jnx/edge_tuber_cap_zones;
        ss := sin(cangle);
        cc := cos(cangle);
  
        vx := vertex[v_id].__x + radius*(ss*evec + cc*cos(iangle)*perp1 + cc*sin(iangle)*perp2);
        lintelv[jnx+1][inx] := new_vertex(vx[1],vx[2],vx[3]);
        vertex[lintelv[jnx+1][inx]].tuber_spot := vertex[v_id].__x;
        vertex[lintelv[jnx+1][inx]].tuber_vec := ss*evec + cc*cos(iangle)*perp1 + cc*sin(iangle)*perp2;
      }
    };
  
    // Set hub vertices
    if ee.oid > 0 then
    { ee.ehub[1] := lintelv[1];
      ee.ehub_count[1] := sides;
    }
    else
    { for ( inx := 1 ; inx <= sides ; inx++ )
        ee.ehub[2][sides-inx+1] := lintelv[1][inx];
      ee.ehub_count[2] := sides;
    };
    
    // Create edges
    for ( inx := 1 ; inx <= sides ; inx++ )
    { iinx := inx == sides ? 1 : inx+1;
      for ( jnx := 1 ; jnx <= edge_tuber_cap_zones ; jnx++ )
      {
        ribe[jnx][inx] := new_edge(lintelv[jnx][inx],lintelv[jnx+1][inx]);
        if jnx < edge_tuber_cap_zones then
          diage[jnx][inx] := new_edge(lintelv[jnx][inx],lintelv[jnx+1][iinx]);
        lintele[jnx][inx] := new_edge(lintelv[jnx][inx],lintelv[jnx][iinx]);
      }
    };
  
    // Create facets
    for ( inx := 1 ; inx <= sides ; inx++ )
    { iinx := inx == sides ? 1 : inx+1;
      for ( jnx := 1 ; jnx < edge_tuber_cap_zones ; jnx++ )
      { f_id := new_facet(lintele[jnx][inx],ribe[jnx][iinx],-diage[jnx][inx]);
        set facet[f_id] tuber_orig_edge ee.id;
        f_id := new_facet(diage[jnx][inx],-lintele[jnx+1][inx],-ribe[jnx][inx]);
        set facet[f_id] tuber_orig_edge ee.id;
      };
      f_id := new_facet(lintele[jnx][inx],ribe[jnx][iinx],-ribe[jnx][inx]);
      set facet[f_id] tuber_orig_edge ee.id;
    };
   
  } // end ee

} // end edge_tuber_cap

/**********************************************************************************************/

define vertex attribute voffset real[3] //  due to single edges

// Creating hubs at given vertex for each adjacent edge
// by finding convex set of separating planes.
// Developed from outer_vertex_tube() of dualtuber.cmd.
procedure calc_vertex_hubs(integer v_id, real radius)
{
  local plane_normals, evecs, e_oids,inx,jnx,knx,single_count,outvec;
  local mnx,ecount,pnx,hidden,found,unx,loopsize,rnx,znx,loopspot,snx; 
  local changes,sides,mminx,jeps,mray,bestj,bestdot,swangle,bestm,minx;
  local fudge, starter, maxdot, thisdot,cc,ss,mangle;
  define evecs real[20][3];  // unit edge vectors
  define plane_normals real[20][20][3]; // separating plane unit normals
  define e_oids integer[20];  // oid numbers for special edges in order found.
  
  local NOT_VISIBLE,PART_VISIBLE,WHOLE_VISIBLE;
  NOT_VISIBLE := 0;
  PART_VISIBLE := 1;
  WHOLE_VISIBLE := 2;

  // jiggle size, small enough things get identified by dtorus
  jeps := 1e-7;

  // gather edge unit vectors
  e_oids := 0;
  inx := 1; single_count := 0;
  foreach vertex[v_id].edge ee where tuber_status == tuber_edge_to_do do
  { evecs[inx] := ee.edge_vector/ee.length;
    // jiggle a bit to avoid degneracy
    evecs[inx] += { jeps*random,jeps*random,jeps*random };
    evecs[inx] /= sqrt(evecs[inx]*evecs[inx]);
    e_oids[inx] := ee.oid;
    inx++;
  };
  ecount := inx-1;

  if ecount == 0 then return;
  if ecount == 1 then { edge_tuber_cap(v_id, radius); return; };

  // calculate separating plane normals; note these point towards the inx edge, not away
  for ( inx := 1 ; inx <= ecount ; inx++ )
    for ( jnx := 1 ; jnx <= ecount ; jnx++ )
    { if inx == jnx then continue;
      plane_normals[inx][jnx] := evecs[inx]-evecs[jnx];
      plane_normals[inx][jnx] /= sqrt(plane_normals[inx][jnx]*plane_normals[inx][jnx]);
    };


  // Hub for each adjacent edge
  for ( inx := 1 ; inx <= ecount ; inx++ )
  { 
    local visible,lowray,highray,other,othercount;
    define visible integer[20];
    define lowray real[20][3];
    define highray real[20][3];
    define other integer[20];
    local invec,ray,cross,refvec,bestrefvec;
    local rays,kinx,jinx,low_neighbor,high_neighbor,raymag;
    local startjinx,other_count,mindot,bestother;
    define rays real[20][3];
    define ray real[3];
    define mray real[3];

    if ecount == 2 then 
    { othercount := 1;
    }
    else
    {
      // For each other plane, find its visibility and its neighbors
      lowray := 0; highray := 0; visible := 0; other := 0; rays := 0;

      // start with plane closest to inx edge
      bestj := 0;
      bestdot := -1;
      for ( jinx := 1 ; jinx <= ecount ; jinx++ )
      { if jinx == inx then continue;
        thisdot := evecs[jinx]*evecs[inx];
        if thisdot > bestdot then
        { bestj := jinx;
          bestdot := thisdot;
        };
      };

      kinx := bestj;
      ray := evecs[inx] + evecs[kinx];
      ray /= sqrt(ray*ray); // starter ray

      other[1] := kinx;
      othercount := 1;

      for(;;)
      {
        // now find next counterclockwise intersection ray with kinx plane
        swangle := 2*pi; 
        bestm := 0;
        for ( minx := 1 ; minx <= ecount ; minx++ )
        { if minx == inx or minx == kinx then continue;
          // intersection ray
          mray[1] := plane_normals[inx][kinx][2]*plane_normals[inx][minx][3]
                   - plane_normals[inx][kinx][3]*plane_normals[inx][minx][2];
          mray[2] := plane_normals[inx][kinx][3]*plane_normals[inx][minx][1]
                   - plane_normals[inx][kinx][1]*plane_normals[inx][minx][3];
          mray[3] := plane_normals[inx][kinx][1]*plane_normals[inx][minx][2]
                   - plane_normals[inx][kinx][2]*plane_normals[inx][minx][1];
          raymag := sqrt(mray*mray); 
          mray /= raymag;
          cc := mray*ray;
          ss := ray[1]*mray[2]*plane_normals[inx][kinx][3] +
                ray[2]*mray[3]*plane_normals[inx][kinx][1] +
                ray[3]*mray[1]*plane_normals[inx][kinx][2] -
                ray[3]*mray[2]*plane_normals[inx][kinx][1] -
                ray[1]*mray[3]*plane_normals[inx][kinx][2] -
                ray[2]*mray[1]*plane_normals[inx][kinx][3]; 
          mangle := atan2(ss,cc);
          if mangle < 0 then mangle += 2*pi;
          if mangle < swangle then
          { swangle := mangle;
            bestm := minx;
            rays[othercount] := mray;
          }
        };
        // detect loop
        if  bestm = other[1] then
          break;

        // no loop, so add to list
        othercount++;
        other[othercount] := bestm;
 
        // prepare for next go-round
        kinx := bestm;
        ray := rays[othercount-1];
      };
    }; // end ecount > 2
    
    // Create vertices and edges
    local vspot,perp1,perp2,newv,minx,cinx,fudge1,sinx,basev,mag,sidesS,zinx,zones;
    define invec real[3];
    define vspot real[3];
    define perp1 real[3];
    define perp2 real[3];
    define basev integer[edge_tuber_maxv];

    if othercount == 1 then
    { // Simple butt junction of two edges

      invec := evecs[1] + evecs[2];
      mag := sqrt(invec*invec);
      if mag < 0.00001 then
      { // straight junction
        if abs(evecs[1][1]) > 0.5 then
          invec := { -evecs[1][3], 0, evecs[1][1]}
        else
          invec := { 0, -evecs[1][3], evecs[1][2]};
        mag := sqrt(invec*invec);
      };
      perp1 := invec/mag;
      perp2 := { evecs[1][2]*perp1[3]-evecs[1][3]*perp1[2],
                 evecs[1][3]*perp1[1]-evecs[1][1]*perp1[3],
                 evecs[1][1]*perp1[2]-evecs[1][2]*perp1[1]};
      perp2 /= sqrt(perp2*perp2);
       
      sides := edge_tuber_sides;
      fudge := sqrt(2)/sqrt(1-evecs[1]*evecs[2]);
      for ( zinx := 1 ; zinx <= sides ; zinx++ )
      { vspot := vertex[v_id].__x + radius*(cos(zinx*2*pi/sides)*fudge*perp1 + sin(zinx*2*pi/sides)*perp2);
        basev[zinx] := new_vertex(vspot[1],vspot[2],vspot[3]);  
        vertex[basev[zinx]].tuber_vec := (cos(zinx*2*pi/sides)*fudge*perp1 + sin(zinx*2*pi/sides)*perp2);
        vertex[basev[zinx]].tuber_spot := vertex[v_id].__x;
      };
      foreach edge[e_oids[1]] ee do
      { 
        if ee.oid > 0 then
        { for ( zinx := 1 ; zinx <= sides ; zinx++ )
            ee.ehub[1][sides-zinx+1] := basev[zinx];
          ee.ehub_count[1] := sides;
        }
        else
        { ee.ehub[2] := basev;
          ee.ehub_count[2] := sides;
        };
      };
      foreach edge[e_oids[2]] ee do
      { 
        if ee.oid > 0 then
        { ee.ehub[1] := basev;
          ee.ehub_count[1] := sides;
        }
        else
        { for ( zinx := 1 ; zinx <= sides ; zinx++ )
            ee.ehub[2][sides-zinx+1] := basev[zinx];
          ee.ehub_count[2] := sides;
        };
      };
    }
    else 
    { // more complicated junction
      for ( minx := 1 ; minx <= othercount ; minx++ )
      { local theta1,theta2,dtheta,cc,ss,det;

        mminx := (minx == othercount) ? 1 : minx+1;
        // invec is third vector in basis of plane_normal and ray1
        cinx := other[mminx];

        invec := evecs[inx] + evecs[cinx];
        mag := sqrt(invec*invec);
        if mag < 0.00001 then
        { // straight junction
          invec := rays[minx];
          mag := sqrt(invec*invec);
        };
        invec := invec/mag;
        perp1[1] := plane_normals[inx][cinx][2]*invec[3] - plane_normals[inx][cinx][3]*invec[2];
        perp1[2] := plane_normals[inx][cinx][3]*invec[1] - plane_normals[inx][cinx][1]*invec[3];
        perp1[3] := plane_normals[inx][cinx][1]*invec[2] - plane_normals[inx][cinx][2]*invec[1];
        perp1 /= -sqrt(perp1*perp1);

        // stretch factor of intersection circle along invec
        fudge1 := sqrt(2)/sqrt(1 - (evecs[cinx]*evecs[inx]));

        // angle of ray1 
        theta1 := atan2((invec*rays[minx])/fudge1,perp1*rays[minx]);

        // angle of ray2 
        theta2 := atan2((invec*rays[mminx])/fudge1,perp1*rays[mminx]);

        dtheta := theta2 - theta1;
        if dtheta < 0 then dtheta += 2*pi;
        zones := ceil(dtheta/(2*pi)*edge_tuber_sides - 0.4978342);  // so round to nearest whole number,
                                                                    // with inexact adjustor to prevent
                                                                    // coincidences giving two different
                                                                    // zone numbers for different edges
                                                                    // at the same vertex.
        if zones == 0 then zones := 1;

        for ( jnx := 0 ; jnx < zones ; jnx++ )
        { ss := sin(theta1 + jnx/zones*dtheta);
          cc := cos(theta1 + jnx/zones*dtheta);

          vspot := vertex[v_id].__x + radius*vertex[v_id].voffset +  radius*(cc*perp1 + fudge1*ss*invec);
          newv := new_vertex(vspot[1],vspot[2],vspot[3]); 
          vertex[newv].tuber_spot := vertex[v_id].__x;
          vertex[newv].tuber_vec  := vertex[v_id].voffset + cc*perp1 + fudge1*ss*invec;
        

          foreach edge[e_oids[inx]] ee do
          { 
            if ee.oid > 0 then
            { 
              ee.ehub[1][edge_tuber_maxv - ee.ehub_count[1]] := newv;
              ee.ehub_count[1] += 1;
            }
            else
            { 
              ee.ehub_count[2] += 1;
              ee.ehub[2][ee.ehub_count[2]] := newv;
            };
          };
      
        }

      };  // end minx

      // Have to adjust hub list if tail of edge
      foreach edge[e_oids[inx]] ee do
      { 
        if ee.oid > 0 then
        {  for ( jnx := 1 ; jnx <= ee.ehub_count[1] ; jnx++ )
             ee.ehub[1][jnx] := ee.ehub[1][jnx + edge_tuber_maxv - ee.ehub_count[1]];
        }
      };
    };
 
  }; // end inx
} // end calc_vertex_hubs()

/*************************************************************************************/

// Checking for too-thick tubes, by checking for overlap of hub vertices at ends of edges.
// Returns 1 for all OK, 0 for not.
function integer edge_tuber_check()
{ local headmin,tailmax,evec,spot;
  local inx;
  define evec real[3];
 
  foreach edge ee where tuber_status == tuber_edge_to_do do
  { 
    evec := ee.edge_vector;

    tailmax := -1e30;
    for ( inx := 1; inx <= ee.ehub_count[1] ; inx++ )
    { spot := evec*vertex[ee.ehub[1][inx]].__x;
      if spot > tailmax then tailmax := spot;
    };

    headmin := 1e30;
    for ( inx := 1; inx <= ee.ehub_count[2] ; inx++ )
    { spot := evec*vertex[ee.ehub[2][inx]].__x;
      if spot < headmin then headmin := spot;
    };

    if tailmax > headmin then
    { errprintf "edge_tube_check: tube radius too small on edge %d\n",ee.id;
      return 0;
    };
  };

  return 1;

} // end edge_tuber_check()

/*************************************************************************************/

procedure make_edge_tube(integer e_id)
{ // join hubs at ends of edge, calculating explicitly the best way to join them up.
  local maxdot,bestheadhub,inx,iinx,taile,heade,newf,diagedge,evec,det;
  local tailspot,headspot,tinx,hinx,mat,sidevec,sidedot,prevedge,dotSS;
  local hinxv,tinxv;
  define heade integer[edge_tuber_maxv];
  define taile integer[edge_tuber_maxv];
  define evec real[3];
  define sidevec real[3];
  define mat real[3][3];
  define hinxv real[3];
  define tinxv real[3];


  foreach edge[e_id] ee do  // just to get handy ee name
  {
   // Check hubs have been found.
   if ee.ehub_count[1] <= 0 then
   { errprintf "edge_tuber internal error: edge %d tail hub not found.\n",e_id;
     recalc;
     abort;
   };
   if ee.ehub_count[2] <= 0 then
   { errprintf "edge_tuber internal error: edge %d head hub not found.\n",e_id;
     return;
   };

   ee.tuber_status := tuber_edge_done;

   // Find head hub vertex most aligned with first tail hub
   local evec;
   define evec real[3];
   evec := ee.edge_vector;
   maxdot := -1;
   bestheadhub := 0;
   for ( inx := 1 ; inx <= ee.ehub_count[2] ; inx++ )
   { sidevec := vertex[ee.ehub[2][inx]].__x - vertex[ee.ehub[1][1]].__x;
     sidedot := (sidevec*evec)/sqrt(sidevec*sidevec);
     if sidedot > maxdot then
     { bestheadhub := inx;
       maxdot := sidedot;
     }
   };
   if maxdot < 0.0 then
   { errprintf "edge_tuber error: can't align hubs on edge %d; edge too short for radius?\n",e_id;
     return;
   };

   // Make round-the-hub edges
   for ( inx := 1 ; inx <= ee.ehub_count[1] ; inx++ )
   { iinx := inx==ee.ehub_count[1] ? 1 : inx+1;
     taile[inx] := new_edge(ee.ehub[1][inx],ee.ehub[1][iinx]);
   };
   for ( inx := 1 ; inx <= ee.ehub_count[2] ; inx++ )
   { iinx := inx==ee.ehub_count[2] ? 1 : inx+1;
     heade[inx] := new_edge(ee.ehub[2][inx],ee.ehub[2][iinx]);
   };
     
   // Make longitudinal edges and facets, zigzagging in such a way as to keep edges
   // as close as possible to parallel to edge.  This creates an extra longitudinal edge
   // but that is eliminated in the detorus phase.
   tailspot := 1;
   headspot := bestheadhub;
   prevedge := new_edge(ee.ehub[1][tailspot],ee.ehub[2][headspot]);
   do
   {

     hinx := (headspot == ee.ehub_count[2]) ? 1 : headspot + 1;
     tinx := (tailspot == ee.ehub_count[1]) ? 1 : tailspot + 1;

     // decide on basis of which way is more parallel
     hinxv := vertex[ee.ehub[2][hinx]].__x - vertex[ee.ehub[1][tailspot]].__x;
     tinxv := vertex[ee.ehub[2][headspot]].__x - vertex[ee.ehub[1][tinx]].__x;
     if (hinxv*evec)/sqrt(hinxv*hinxv) > (tinxv*evec)/sqrt(tinxv*tinxv) then
     { // use head edge
       diagedge := new_edge(ee.ehub[1][tailspot],ee.ehub[2][hinx]);
       newf := new_facet(prevedge,heade[headspot],-diagedge);
       headspot := hinx;
       prevedge := diagedge;
     }
     else
     { // use tail edge
       diagedge := new_edge(ee.ehub[1][tinx],ee.ehub[2][headspot]);
       newf := new_facet(prevedge,-diagedge,-taile[tailspot]);
       tailspot := tinx;
       prevedge := diagedge;
     };

/* not guaranteed to make progress!

     // decide to zig or zag based on convexity of tube
     mat[1] := vertex[ee.ehub[2][hinx]].__x     - vertex[ee.ehub[1][tailspot]].__x;
     mat[2] := vertex[ee.ehub[2][headspot]].__x - vertex[ee.ehub[1][tailspot]].__x;
     mat[3] := vertex[ee.ehub[1][tinx]].__x     - vertex[ee.ehub[1][tailspot]].__x;
     det := matrix_determinant(mat);

     if det > 0 then
     { // use head edge
       diagedge := new_edge(ee.ehub[1][tailspot],ee.ehub[2][hinx]);
       newf := new_facet(prevedge,heade[headspot],-diagedge);
       headspot := hinx;
       prevedge := diagedge;
     }
     else
     { // use tail edge
       diagedge := new_edge(ee.ehub[1][tinx],ee.ehub[2][headspot]);
       newf := new_facet(prevedge,-diagedge,-taile[tailspot]);
       tailspot := tinx;
       prevedge := diagedge;
     };
*/


     set facet[newf] tuber_orig_edge e_id;

   } while ((tailspot != 1) or (headspot != bestheadhub)) ;
 } // end for

} // end make_edge_tube()

/**********************************************************************************************/

// Adjust radius of existing tubes.
procedure edge_tuber_adjust_radius(real radius)
{
  foreach vertex vv do
    vv.__x := vv.tuber_spot + radius*vv.tuber_vec;

}

/************************************************************************************/

edge_tuber_model_checks := { 

  if torus then
  { errprintf "Cannot run 'edge_tuber' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'edge_tuber' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'edge_tuber' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'edge_tuber' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'edge_tuber' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'edge_tuber' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

}

/*******************************************************************************/

// Basic tuber procedure.  Set edge attribute tuber_status to tuber_edge_to_do
// on edges desired to be tubed before calling. Leaves such edges with
// tuber_status set to tuber_edge_done.
procedure edge_tuber(real radius)
{ edge_tuber_model_checks;
  edge_tuber_maxv := edge_tuber_sides + 15; // room for a few extra vertices if needed
  define edge attribute ehub integer[2][edge_tuber_maxv];  // vertex id's of hub vertices
  set vertex tuber_spot __x;
  set vertex tuber_vec 0;
  set edge ehub 0;
  set edge ehub_count 0;
  foreach vertex vv do calc_vertex_hubs(vv.id,radius);
  if edge_tuber_check() == 0 then
  { dissolve vertices;
    abort;
  };

  foreach edge ee where tuber_status == tuber_edge_to_do do make_edge_tube(ee.id);
  detorus; // to identify together all the matching hub vertices and edges.
}

// Tubing all the existing edges as a naked lattice, deleting all original edges and facets.
procedure edge_tuber_naked(real radius)
{
  edge_tuber_model_checks;
  set edge tuber_status tuber_edge_to_do;
  dissolve facets;
  edge_tuber(radius);
  dissolve edges;
  dissolve vertices; 
} 

// Tubing the dual skeleton, plus special edges.
procedure edge_tuber_dual(real radius)
{
  edge_tuber_model_checks;
  set edge tuber_status tuber_edge_to_do; // mark original edges
  refine facet;  // get hub points in each facet
  edgeswap edge where tuber_status == tuber_edge_to_do and valence == 2; // dual edges
  set edge tuber_status 4 where tuber_status != tuber_edge_to_do;
  refine edge where valence != 2;
  set edge tuber_status tuber_edge_to_do where tuber_status == 0;
  set edge tuber_status 0 where tuber_status != tuber_edge_to_do;
  dissolve facets;
  edge_tuber(radius);
  dissolve edges;
  dissolve vertices; 
}


if surface_dimension != 2 then
  errprintf "\nedge_tuber: Error: tubing only works in soapfilm model.\n\n";
  
/* Usage:
   Set edge_tuber_sides to desired value of sides around edge, default 6.
   Set edge_tuber_cap_zones to number of latitudinal zones wanted on edge caps,
      default 2.

   Do one of the following:
   a. To simply tube a given set of edges:
        set edge tuber_status tuber_edge_to_do where ...
        edge_tuber(radius) 
   b. To dissolve all facets and tube all edges:
        edge_tuber_naked(radius)
   c. To create dual edges and tube them:
        edge_tuber_dual(radius)

   To adjust the tube radius afterward (no checks on overlap):
      edge_tuber_adjust_radius(radius)

   Limitations: Only works with 3D soapfilm linear model, no torus or symmetry mode.
   Will check for too-large tube radius and abort tubing.
*/

