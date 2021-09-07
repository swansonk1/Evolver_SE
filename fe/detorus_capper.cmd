// detorus_capper.cmd

quiet on;  // suppress text output while loading.

// Contents: detorus_capper - put caps on sides of unit cell after detorus.
//           slice_and_cap  - slice any surface on one plane and cap.

//   Author: Ken Brakke, brakke@susqu.edu
//   Date: March 22,2011


/* detorus_capper

   Evolver command to cap off bodies sliced by detorus in "clipped" mode.
   Works by finding polygons of sliced facets on each plane,
   and applying tessellation algorithm to polygons (which
   may be nested, so it's nontrivial).

   detorus_capper usage:
      clipped;
      detorus;
      capper_set_constraints; // suggested 
      detorus_capper_groom;   // suggested, if did capper_set_constraints
      detorus_capper;
  

   Assumptions: Body or bodies must be defined; only bodies are capped.

                The bodies may include the corners and edges of the unit cell;
                temporary edges are created along all the edges of the unit cell
                in case they may be needed.  These are deleted at the end if not
                used, so do not be alarmed by "dissolve" messages after
                detorus_capper ends.

                "detorus" has been done in "clipped" mode, and
                the torus_periods array is still valid, so you can't
                detorus, dump, and reload before detorus_capper.

                There are only valence 1 edges on the unit cell planes.  If you have
                facets in the unit cell planes, then you should shift the surface
                "set vertex x x+0.1" etc. before doing "detorus".
          
   
   Side effects: New edges and faces are created to cap bodies on the unit cell planes.
                 An edge attribute "ecaptype" and a facet attribute "fcaptype" are created.
                 The new facets are colored yellow, and have fcaptype value new_captype.

   Warnings: Use only with Evolver version 2.39t or later; earlier versions have a bug
             in detorus that can mess things up.

             Check for success afterwards, visually, using the "C" command, and checking
             for uncapped edges, say "print sum(edge,valence==1)".
*/

/* slice_and_cap
   
   Evolver command to slice any surface (not necessarily derived from torus model)
   on arbitrary plane and cap the bodies cut by the plane.

   Usage: slice_and_cap(aa,bb,cc,rhs)

   where (aa,bb,cc) is the outward normal of the slice plane aa*x + bb*y + cc*z = rhs
   (i.e. the remaining surface is in the half-plane aa*x + bb*y + cc*z <= rhs).

   Warnings:  Try not to slice through existing vertices.

              Be cautious if bodies are not completely enclosed by facets, for example
              mound.fe.   It may appear to work, but still leave unsealed edges.

*/

/*****************************************************************************************/

// Using "dc_" prefix on globals for name space isolation.

// direction vector for sorting, meant to avoid two vertices at the same level
define dc_sortvec real[3];
dc_sortvec := { 1.124511234521624, 2.0212352424523141, 3.235626245454545}
define dc_scanvec real[3];
define dc_planenormal real[3];

define dc_verlist integer[1];
define dc_verkey  real[1];

// vertex attributes for keeping track of nearest-level vertices visible left and right
define edge attribute ecaptype integer;  
define facet attribute fcaptype integer;
bodied_captype := 1 // original edges on face with adjacent facets
celledge_captype := 2  // bounding box edges from capper_unit_cell()
new_captype := 3 // newly created edges in sweep 3
bare_captype := 4;  // shouldn't happen

capper_debug_flag := 0;  // set this for verbose printing during execution

/*****************************************************************************************/

define constraint unit_cell_xpos_con formula x*inverse_periods[1][1] +
                           y*inverse_periods[2][1] + z*inverse_periods[3][1] - 1;
define constraint unit_cell_xneg_con formula x*inverse_periods[1][1] +
                           y*inverse_periods[2][1] + z*inverse_periods[3][1];
define constraint unit_cell_ypos_con formula x*inverse_periods[1][2] +
                           y*inverse_periods[2][2] + z*inverse_periods[3][2] - 1;
define constraint unit_cell_yneg_con formula x*inverse_periods[1][2] +
                           y*inverse_periods[2][2] + z*inverse_periods[3][2];
define constraint unit_cell_zpos_con formula x*inverse_periods[1][3] +
                           y*inverse_periods[2][3] + z*inverse_periods[3][3] - 1;
define constraint unit_cell_zneg_con formula x*inverse_periods[1][3] +
                           y*inverse_periods[2][3] + z*inverse_periods[3][3];

capper_set_constraints := 
{
  set vertex constraint unit_cell_xpos_con where abs(x*inverse_periods[1][1] +
                           y*inverse_periods[2][1] + z*inverse_periods[3][1] - 1) < 1e-6;

  set vertex constraint unit_cell_xneg_con where abs(x*inverse_periods[1][1] +
                           y*inverse_periods[2][1] + z*inverse_periods[3][1]) < 1e-6;

  set vertex constraint unit_cell_ypos_con where abs(x*inverse_periods[1][2] +

                           y*inverse_periods[2][2] + z*inverse_periods[3][2] - 1) < 1e-6;

  set vertex constraint unit_cell_yneg_con where abs(x*inverse_periods[1][2] +
                           y*inverse_periods[2][2] + z*inverse_periods[3][2]) < 1e-6;

  set vertex constraint unit_cell_zpos_con where abs(x*inverse_periods[1][3] +
                           y*inverse_periods[2][3] + z*inverse_periods[3][3] - 1) < 1e-6;

  set vertex constraint unit_cell_zneg_con where abs(x*inverse_periods[1][3] +
                           y*inverse_periods[2][3] + z*inverse_periods[3][3]) < 1e-6;

  
}

/*****************************************************************************************/

// For grooming short edges that can be produced by detorus.  
// Dissolves facets on the face constraints that may be produced by capper_set_constraints.
detorus_capper_groom := 
{ local inx,jnx,diff,side;
         
  // dissolve facets on the face constraints
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_xpos_con)==3;
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_xneg_con)==3;
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_ypos_con)==3;
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_yneg_con)==3;
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_zpos_con)==3;
  dissolve facet ff where sum(ff.vertex, on_constraint unit_cell_zneg_con)==3;
  dissolve edge where valence==0 and ecaptype != celledge_captype;
  dissolve vertex;
}

/*****************************************************************************************/

// Make sure each valence 1 edge is on a unit cell face
capper_edge_check := 
{ local inx,jnx,side,mark,bad;


  // get normal vectors to unit cell faces from inverse_periods
  local cell_normals;
  define cell_normals real[3][3];
  for ( inx := 1 ; inx <= 3 ; inx++ )
    for ( jnx := 1 ; jnx <= 3 ; jnx++ )
      cell_normals[inx][jnx] := inverse_periods[jnx][inx];
  
  define mark integer[3][2];  // for recording incidence on unit cell faces.
  bad := 0;
  foreach edge ee where valence == 1 do
  { mark := 0;
    for ( inx := 1 ; inx <= 3 ; inx++ )
      for ( side := 0 ; side <= 1 ; side++ )
      { foreach ee.vertex vv do
        { diff := vv.__x * cell_normals[inx] - side;
          if abs(diff) < .000000001 then 
          { if mark[inx][side+1] then
            { // have two ends on same face
              continue 4;
            };
            mark[inx][side+1] := 1;
          }
        }
      };
    if inx == 4 then // didn't find unit cell with two of the edge vertices
    { set ee color red;
      bad++;
    };
  } ;

  printf "\ncapper_edge_check: %d valence 1 edges found without vertices on one face.\n\n",bad;
} // end capper_edge_check

/*****************************************************************************************/

// Quicksort of vertex list, used by cap_one_plane()
procedure versort_recur(integer start,integer end)
{
  local lowspot,hispot,inx,jnx,pivot,keysum,temp;

  if end - start <= 5 then  // bubble sort
  { for (inx := start ; inx < end ; inx++ )
      for ( jnx := inx+1 ; jnx <= end ; jnx++ )
        if dc_verkey[inx] > dc_verkey[jnx] then
        { // swap
          temp := dc_verlist[inx]; dc_verlist[inx] := dc_verlist[jnx]; dc_verlist[jnx] := temp;
          temp := dc_verkey[inx]; dc_verkey[inx] := dc_verkey[jnx]; dc_verkey[jnx] := temp;
        };
   return;
  };


  // pick mean as pivot; random too prone to picking max
  keysum := 0;
  for ( inx := start; inx <= end ; inx++ )
    keysum += dc_verkey[inx];
  pivot := keysum / (end-start+1);

  // Move pointers in from ends, swapping when get reversed values
  lowspot := start;
  hispot := end;
  do
  { while lowspot <= hispot and dc_verkey[lowspot] <= pivot and lowspot < end do { lowspot++ };
    while hispot >= lowspot and dc_verkey[hispot] >  pivot and hispot > start do { hispot-- };
    if lowspot >= hispot then break;
    // now swap
    temp := dc_verlist[lowspot]; dc_verlist[lowspot] := dc_verlist[hispot]; dc_verlist[hispot] := temp;
    temp := dc_verkey[lowspot]; dc_verkey[lowspot] := dc_verkey[hispot]; dc_verkey[hispot] := temp;
  } while lowspot < hispot;
  versort_recur(start,hispot);
  versort_recur(lowspot,end);
    
} // end versort_recur()

/*****************************************************************************************/

// Procedure to check whether potential facet being created should be,
// and create if so.  Used by cap_one_plane().

procedure  capper_facet_maker(integer edge1,integer edge2,integer edge3)
{ local fnorm,f_id,ea,eb,ec,dotprod,newf,temp;
  define fnorm real[3];

  if edge[edge1].ecaptype == bodied_captype then
  { ea := edge1; eb := edge2; ec := edge3; }
  else if edge[edge2].ecaptype == bodied_captype then
  { ea := edge2; eb := edge3; ec := edge1 }
  else if edge[edge3].ecaptype == bodied_captype then
  { ea := edge3; eb := edge1 ; ec := edge2; }
  else
  { 
    return;
  };

  // Get outward orientation
  if dc_planenormal[1]*(edge[eb].y*edge[ec].z-edge[eb].z*edge[ec].y)
   + dc_planenormal[2]*(edge[eb].z*edge[ec].x-edge[eb].x*edge[ec].z)
   + dc_planenormal[3]*(edge[eb].x*edge[ec].y-edge[eb].y*edge[ec].x) < 0 then
  { ea := -ea; temp := eb; eb := -ec; ec := -temp; };

  // Find the original facet on the edge with body information.
  f_id := 0;
  foreach edge[ea].facet ff where fcaptype == bodied_captype do
  { f_id := ff.oid;
    break;
  };
  if f_id == 0 then { printf "No bodied facet.\n"; return; };

  // Test if facet is on body and create if so.
  fnorm := facet[f_id].facet_normal;
  // test sidedness with dot product
  dotprod := fnorm*edge[eb].edge_vector;
  if facet[f_id].frontbody and dotprod < 0 then
  { newf := new_facet(ea,eb,ec);
    facet[newf].color := yellow;
    facet[newf].fcaptype := new_captype;
    facet[newf].frontbody := facet[f_id].frontbody;
  };
  if facet[f_id].backbody and dotprod > 0 then
  { newf := new_facet(ea,eb,ec);
    facet[newf].color := yellow;
    facet[newf].fcaptype := new_captype;
    facet[newf].frontbody := facet[f_id].backbody;
  }; 

} // end capper_facet_maker()

/*****************************************************************************************/

// Cap one plane.  Arguments are plane equation coefficients,
// for ax + by + cz = d. It is assumed (a,b,c) is an outward normal.
// This procedure is written to not refer to any torus information, so it may
// be invoked to cap the surface on any plane, as long as assumptions are met:
//   Only valence 1 edges.
//   Edges form closed polygons, possible nested, possibly disjoint.
//   No vertices beyond plane (well, there can be, but effects unpredictable).

/* Algorithm: I use the "regularization and triangulation" algorithm described on
   pp. 567-8 of the Handbook of Discrete and Computational Geometry (2nd ed),
   which references Computational Geometry by Preparata and Shamos (1985).

   Vertices on the plane are sorted according to some linear order (I use sortvec).
   Three sweeps are done:
   Sweep 1: Scan left to right, keeping track of edges that intersect the scan line.
            Also keep track of a vertex for each interval between edges that is 
            visible from the scan line interval.  When reaching a vertex with no
            leftward edges, add a leftward edge to the visible vertex.
   Sweep 2: Same, but going right to left.  The result is that each polygon now
            intersects each scan line in at most two edges, i.e. all polygons 
            are "monotone".
   Sweep 3: Scan left to right, triangulating each polygon.  A polygon side may
            be "reflex", i.e. concave inward, for a while, so for each polygon
            a stack is kept of reflex edges.  Actually, only one side at a time
            may be untriangulatably reflex, so only one stack is needed, and
            a notation of which side the reflex side is on.
*/

procedure cap_one_plane(real acoeff, real bcoeff, real ccoeff, real rhs)
{
  // local declarations
  local epsilon,dc_planenormalmag,rhsmin,rhs_cutoff,vercount,inx,jnx,sweep,sign,face_count;
  local scancount,listspot,rightcount,delcount,inspot,scanspot,knx,ttnew,ss,ss1,ss2;
  local tt1,tt2,tt,newe,startspot,endspot,dir1,dir2,temp,RIGHTSIDE,LEFTSIDE,face_counter;
  local bigbig,facenum,lastedge,spot,tripprod,snx,leftedge,rightedge,other_edge,tnx;
  local fnorm,evec;
  define fnorm real[3];
  define evec real[3];
  define dc_verlist integer[edge_count];
  define dc_verkey real[edge_count];

  if torus then 
  { errprintf "cap_one_plane ERROR: Cannot proceed in torus mode.  Do detorus.\n";
    return;
  };

  epsilon := 0.000001;  // relative favoritism for reflex bends in edges

  // Clear ecaptype of existing edges (but not unit cell auxiliary edges)
  set edge ecaptype 0 where ecaptype != celledge_captype;

  // mark existing facets
  set facet fcaptype bodied_captype;

  // Vectorize plane normal 
  dc_planenormal := {acoeff,bcoeff,ccoeff};
  dc_planenormalmag := sqrt(dc_planenormal * dc_planenormal);

  // Get a margin of error for considering vertices on the plane
  rhsmin := min(vertex vv, acoeff*vv.x + bcoeff*vv.y + ccoeff*vv.z);
  rhs_cutoff := rhs - (rhs-rhsmin)/100000;

  // Gather list of vertices on the plane
  vercount := 0;
  foreach vertex vv where vv.__x * dc_planenormal > rhs_cutoff do
  { vercount++;
    dc_verlist[vercount] := vv.id;
    dc_verkey[vercount] := vv.__x * dc_sortvec;
  };

  if capper_debug_flag then
    printf "Found %d vertices in plane %g*x + %g*y + %g*z = %g.\n",vercount,
   acoeff,bcoeff,ccoeff,rhs;

  // Sort list by skew height (to avoid having to deal with two vertices at the same height)
  versort_recur(1,vercount);

  // double-check sort
  for ( inx := 1 ; inx < vercount ; inx++ )
    if dc_verkey[inx] > dc_verkey[inx+1] then
    { printf "cap_one_plane INTERNAL ERROR: vertex sort failed.\n";
      return;
    };

  if capper_debug_flag then
    printf "Vertices sorted.\n";


  // scan line direction vector, cross product of plane normal and sort vector.
  dc_scanvec[1] := dc_sortvec[2]*ccoeff - dc_sortvec[3]*bcoeff;
  dc_scanvec[2] := dc_sortvec[3]*acoeff - dc_sortvec[1]*ccoeff;
  dc_scanvec[3] := dc_sortvec[1]*bcoeff - dc_sortvec[2]*acoeff;
  if capper_debug_flag then
  { printf "dc_planenormal: "; print dc_planenormal;
    printf "dc_sortvec:  "; print dc_sortvec;
    printf "dc_scanvec:  "; print dc_scanvec;
  };

  // Now work bottom up, making sure every vertex has a down edge on the plane.
  // Using auxiliary list of visible vertices for each interval between edges
  // on scan line 
  
  // Note: vislist[] has entries for regions between edges on scan list, including
  // outside, so edge scanlist[k] is between vislist[k] and vislist[k+1].

  // Note: "right" and "left" sometimes refer to right and left along the scan line,
  // rather than along the scan direction.

  // scanlist[] entries are edge id's
  local scanlist,vislist;
  define scanlist integer[vercount];  // edges intersected by scan line, oriented id's.
  define vislist integer[vercount]; // visible vertices between scanlist[k] and scanlist[k+1]
  local rightlist;
  define rightlist integer[100];

  for ( sweep := 1 ; sweep <= 2 ; sweep++ )
  { // sweep 1 is left to right, sweep 2 is right to left
    if capper_debug_flag then
      printf "Starting sweep %d\n",sweep;

    sign := sweep==1 ? 1 : -1;

    face_count := 0; // so third sweep knows how many stacks needed
    scancount := 0; // number of entries in scan line intersection
  
    for ( listspot := 1 ; listspot <= vercount ; listspot++ )
    { foreach vertex[dc_verlist[sweep==1 ? listspot : vercount+1-listspot]] vv do
      { // lists of left and right edges
        rightcount := 0; delcount := 0; inspot := 1; endspot := -1;
        foreach vv.edge ee where ee.vertex[2].__x * dc_planenormal > rhs_cutoff do
        { if sign*(ee.edge_vector * dc_sortvec) < 0.0 then
          { // left edge, so delete from scan list
            if capper_debug_flag then
              printf"vertex %d left edge %d\n",vv.id,ee.oid;
            for ( scanspot := 1 ; scanspot <= scancount ; scanspot++ )
              if scanlist[scanspot] == -ee.oid then
              { for ( knx := scanspot ; knx < scancount+1 ; knx++ )
                { scanlist[knx] := scanlist[knx+1];
                  vislist[knx] := vislist[knx+1];
                };
                scancount--;
                delcount++;
                inspot := scanspot; // for later insertion
                if capper_debug_flag then
                  printf "deleted edge %d inspot %d\n",-ee.oid,inspot;
                break;
              }
          }
          else // add to right edge list
          { rightcount++;
            rightlist[rightcount] := ee.oid;
            if capper_debug_flag then
              printf"vertex %d right edge %d\n",vv.id,ee.oid;
          };
        };
        if delcount == 0 and scancount > 0 then
        { // add left edge to vv
          // find proper interval
  
          ttnew := sign*(vv.__x * dc_scanvec);
          ss := sign*(vv.__x * dc_sortvec);
          for ( inspot := 1 ; inspot <= scancount ; inspot++ )
          { ss1 := sign*(edge[scanlist[inspot]].vertex[1].__x * dc_sortvec);
            ss2 := sign*(edge[scanlist[inspot]].vertex[2].__x * dc_sortvec);
            tt1 := sign*(edge[scanlist[inspot]].vertex[1].__x * dc_scanvec);
            tt2 := sign*(edge[scanlist[inspot]].vertex[2].__x * dc_scanvec);
            tt  := (ss-ss1)/(ss2-ss1)*(tt2-tt1) + tt1;
            if tt > ttnew then { break; }  // have just gone past, so this is the spot to insert
          };
          if capper_debug_flag then
            printf "inspot %d  tt %f  ttnew %f\n",inspot,tt,ttnew;
          newe := new_edge(vv.id,vislist[inspot]);
          edge[newe].color := red;
          edge[newe].ecaptype := new_captype;
          face_count++;
        };
        if rightcount then
        { // insert right edges into scanlist
  
          // open gap in scanlist
          for ( jnx := scancount+1 ; jnx >= inspot ; jnx-- )
          { scanlist[jnx+rightcount] := scanlist[jnx];
            vislist[jnx+rightcount] := vislist[jnx];
          };
          // insert new edges
          for ( {jnx := 1; knx := inspot} ; jnx <= rightcount ; {jnx++;knx++} )
          { scanlist[knx] := rightlist[jnx];
          vislist[knx]  := vv.id;
          }; 
          vislist[knx] := vv.id;

          scancount += rightcount;
          face_count += rightcount-1;
          if capper_debug_flag then
          { printf "Inserted edges "; 
            for ( jnx := inspot ; jnx < inspot+rightcount ; jnx++ ) printf "%d ",
              scanlist[jnx]; printf "\n";
            printf "Scanlist:      "; for ( jnx := 1 ; jnx <= scancount ; jnx++ ) printf "%d ",
              scanlist[jnx]; printf "\n";
          };
          // set up for order sorting
          startspot := inspot;
          endspot := inspot + rightcount - 1;
        }
        else
        { // set visible vertex to vv
          if inspot > 1 then
            vislist[inspot] := vv.id
        };
        // sort new edges in scan order, just bubble sort 
        for ( inx := startspot ; inx < endspot ; inx++ )
          for ( jnx := inx+1 ; jnx <= endspot ; jnx++ )
          { 
            dir1 := sign*(dc_scanvec*edge[scanlist[inx]].edge_vector)/edge[scanlist[inx]].length;
            dir2 := sign*(dc_scanvec*edge[scanlist[jnx]].edge_vector)/edge[scanlist[jnx]].length;
            if dir1 > dir2 then 
            { // swap
              temp := scanlist[inx];
              scanlist[inx] := scanlist[jnx];
              scanlist[jnx] := temp;
            };
          };

        if capper_debug_flag then
        { printf "Sorted Scanlist:          "; 
          for ( jnx := 1 ; jnx <= scancount ; jnx++ ) printf "%4d ",scanlist[jnx]; printf "\n";
          printf "Visibility list:      ";
          for ( jnx := 1 ; jnx <= scancount+1 ; jnx++ ) printf "%4d ",vislist[jnx]; printf "\n";
        };

      };  // end vv
    };   // end dc_verlist
  }; // end sweep


  // Polygons all "monotone" now, each vertical line crosses at most two edges of a face.
  // Now sweep again, triangulating faces as we go.

 if capper_debug_flag then
   printf "\nThird sweep - faceting the faces.  Face_count: %d\n",face_count;

    // stacks for face reflex chains.
    local face_list,facestacksize,facestacks,facestacktops,face_sidedness,face_edge_stacks;
    facestacksize := 10;  // can increase later if needed
    define face_list integer[face_count+5]; // faces between scanlist edges
    define facestacks integer[face_count][facestacksize]; // vertices
    define face_edge_stacks integer[face_count][facestacksize]; // reflex edges
    define facestacktops integer[face_count];
    define face_sidedness integer[face_count];
    RIGHTSIDE := 101;
    LEFTSIDE  := 102;
    local veca,vecb;
    define veca real[3];
    define vecb real[3];

    scancount := 0; // number of entries in scan line intersection
    facestacktops := 0;
    face_sidedness := 0;
    face_counter := 0;
    bigbig := 1000000000;
  
    for ( listspot := 1 ; listspot <= vercount ; listspot++ )
    { foreach vertex[dc_verlist[listspot]] vv do
      { 
        // lists of left and right edges
        rightcount := 0; delcount := 0; inspot := bigbig;
        foreach vv.edge ee where ee.vertex[2].__x * dc_planenormal > rhs_cutoff do
        { if (ee.edge_vector * dc_sortvec) < 0.0 then
          { // left edge, so delete from scan list
            if capper_debug_flag then
               printf"vertex %d left edge %d\n",vv.id,ee.oid;
            if not ee.ecaptype then 
              ee.ecaptype := ee.valence ? bodied_captype : bare_captype;
            for ( scanspot := 1 ; scanspot <= scancount ; scanspot++ )
              if scanlist[scanspot] == -ee.oid then
              { 
                delcount++;
                if scanspot < inspot then
                  inspot := scanspot; // for later insertion
                break;
              }
        }
        else // add to right edge list
        { rightcount++;
          rightlist[rightcount] := ee.oid;
          if capper_debug_flag then
            printf"vertex %d right edge %d\n",vv.id,ee.oid;
        };
      };
      if inspot == bigbig then inspot := 1;

      // take care of faces to left and right
      // left
      if rightcount > 0 and inspot > 1 then
      { facenum := face_list[inspot-1]; 
        // vv adjacent to topmost vertex on stack?
        if face_sidedness[facenum] == RIGHTSIDE and facestacktops[facenum] > 1 then
        { // peel backwards on reflex curve
          lastedge := scanlist[inspot];
          for ( spot := facestacktops[facenum] ; spot >= 2 ; spot-- )
          { // see if reflex curve or not
            veca := vertex[facestacks[facenum][spot]].__x - vv.__x;
            vecb := vertex[facestacks[facenum][spot-1]].__x - vv.__x;
            tripprod := dc_planenormal[1]*(veca[2]*vecb[3] - veca[3]*vecb[2]) 
                      + dc_planenormal[2]*(veca[3]*vecb[1] - veca[1]*vecb[3]) 
                      + dc_planenormal[3]*(veca[1]*vecb[2] - veca[2]*vecb[1]);
            if tripprod >= -epsilon*dc_planenormalmag*sqrt(veca*veca)*sqrt(vecb*vecb) then break;
            newe := new_edge(vv.id,facestacks[facenum][spot-1]);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;

            // make face. need to find previous edge.
            capper_facet_maker(lastedge,newe,face_edge_stacks[facenum][spot-1]);
            lastedge := -newe;
            if capper_debug_flag then
              printf "left, RIGHTSIDE, new edge %d from %d to %d.\n",newe,vv.id,facestacks[facenum][spot-1];

          };
          facestacktops[facenum] := spot+1;
          if spot+1 > facestacksize then
          { facestacksize *= 2;
            define facestacks integer[face_count][facestacksize]; // vertices
            define face_edge_stacks integer[face_count][facestacksize]; // reflex edges
          };
          facestacks[facenum][spot+1] := vv.id; 
          face_edge_stacks[facenum][spot] := lastedge; 
          face_sidedness[facenum] := RIGHTSIDE;
        }
        else if face_sidedness[facenum] == LEFTSIDE  then
        { // adjacent to first vertex on stack
          // triangulate to opposite side
          lastedge := scanlist[inspot];
          for ( inx := 2 ; inx <= facestacktops[facenum] ; inx++ )
          { newe := new_edge(facestacks[facenum][inx],vv.id);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;
            if capper_debug_flag then
              printf "left, LEFTSIDE, new edge %d from %d to %d.\n",newe,facestacks[facenum][inx],vv.id;
            capper_facet_maker(lastedge,-newe,-face_edge_stacks[facenum][inx-1]);
            lastedge := newe;
          };
          facestacks[facenum][1] := facestacks[facenum][facestacktops[facenum]];
          facestacks[facenum][2] := vv.id;
          face_edge_stacks[facenum][1] := lastedge;
          facestacktops[facenum] := 2;
          face_sidedness[facenum] := RIGHTSIDE;
        }
        else 
        { // push on stack
          facestacktops[facenum] += 1;
          facestacks[facenum][facestacktops[facenum]] := vv.id;
          face_edge_stacks[facenum][facestacktops[facenum]-1] := scanlist[inspot];
          face_sidedness[facenum] := RIGHTSIDE;
        };
      };

      // take care of faces ending in the middle
      for ( snx := inspot ; snx < inspot+delcount-1 ; snx++ )
      { // vv is last vertex, so make edges to all but first vertex on stack
        facenum := face_list[snx];
        leftedge := scanlist[snx];
        rightedge := scanlist[snx+1];
        if face_sidedness[facenum] == LEFTSIDE then
        { 
          other_edge := rightedge;
          for ( tnx := 2 ; tnx <= facestacktops[facenum]-1 ; tnx++ )
          { newe := new_edge(vv.id,facestacks[facenum][tnx]);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;
            if capper_debug_flag then
              printf "middle, LEFTSIDE, new edge %d from %d to %d.\n",newe,vv.id,facestacks[facenum][tnx];
            capper_facet_maker(other_edge,newe,-face_edge_stacks[facenum][tnx-1]);
            other_edge := -newe;
          };
          capper_facet_maker(other_edge,-leftedge,-face_edge_stacks[facenum][tnx-1]);
        }
        else if face_sidedness[facenum] == RIGHTSIDE then
        { 
          other_edge := leftedge;
          for ( tnx := 2 ; tnx <= facestacktops[facenum]-1 ; tnx++ )
          { newe := new_edge(vv.id,facestacks[facenum][tnx]);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;
            if capper_debug_flag then
              printf "middle, RIGHTSIDE, new edge %d from %d to %d.\n",newe,vv.id,facestacks[facenum][tnx];
            capper_facet_maker(-other_edge,face_edge_stacks[facenum][tnx-1],-newe);
            other_edge := -newe;
          };
          capper_facet_maker(-other_edge,face_edge_stacks[facenum][tnx-1],rightedge);
        }
        else
        { // this should not happen
          errprintf "Bad face termination.  Vertex %d, leftedge %d rightedge %d.\n",vv.id,leftedge,rightedge;
          recalc;
          pickvnum := vv.id;
          set edge[leftedge] color magenta;
          set edge[rightedge] color magenta;
          subcommand;
        };
         
        facestacktops[facenum] := 0;
      };

      // take care of face to the right
      if rightcount > 0 and inspot+delcount-1 < scancount then
      { facenum := face_list[inspot+delcount-1];
        // vv adjacent to topmost vertex on stack?
        if face_sidedness[facenum] == LEFTSIDE and facestacktops[facenum] > 1 then
        {
          lastedge := scanlist[inspot+delcount-1];
          for ( spot := facestacktops[facenum] ; spot >= 2 ; spot-- )
          { // see if reflex curve or not
            veca := vertex[facestacks[facenum][spot]].__x - vv.__x;
            vecb := vertex[facestacks[facenum][spot-1]].__x - vv.__x;
            tripprod := dc_planenormal[1]*(veca[2]*vecb[3] - veca[3]*vecb[2]) 
                      + dc_planenormal[2]*(veca[3]*vecb[1] - veca[1]*vecb[3]) 
                      + dc_planenormal[3]*(veca[1]*vecb[2] - veca[2]*vecb[1]);
            if tripprod <= epsilon*dc_planenormalmag*sqrt(veca*veca)*sqrt(vecb*vecb) then break;
            newe := new_edge(vv.id,facestacks[facenum][spot-1]);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;

            // make face. 
            capper_facet_maker(lastedge,newe,face_edge_stacks[facenum][spot-1]);
            lastedge := -newe;

            if capper_debug_flag then
              printf "right, LEFTSIDE, new edge %d from %d to %d.\n",newe,vv.id,facestacks[facenum][spot-1];

          };
          facestacktops[facenum] := spot+1;
          if spot+1 > facestacksize then
          { facestacksize *= 2;
            define facestacks integer[face_count][facestacksize]; // vertices
            define face_edge_stacks integer[face_count][facestacksize]; // reflex edges
          };
          facestacks[facenum][spot+1] := vv.id; 
          face_edge_stacks[facenum][spot] := lastedge; 
          face_sidedness[facenum] := LEFTSIDE;
        }
        else if face_sidedness[facenum] == RIGHTSIDE then
        { // adjacent to first vertex on stack
          // triangulate to opposite side
          lastedge := scanlist[inspot+delcount-1];
          for ( inx := 2 ; inx <= facestacktops[facenum] ; inx++ )
          { newe := new_edge(facestacks[facenum][inx],vv.id);
            edge[newe].color := red;
            edge[newe].ecaptype := new_captype;
            if capper_debug_flag then
              printf "right, RIGHTSIDE, new edge %d from %d to %d.\n",newe,facestacks[facenum][inx],vv.id;
            capper_facet_maker(newe,-lastedge,face_edge_stacks[facenum][inx-1]); 
            lastedge := newe;
          };
          facestacks[facenum][1] := facestacks[facenum][facestacktops[facenum]];
          facestacks[facenum][2] := vv.id;
          face_edge_stacks[facenum][1] := lastedge;
          facestacktops[facenum] := 2;
          face_sidedness[facenum] := LEFTSIDE;
        }
        else 
        { // push on stack
          facestacktops[facenum] += 1;
          facestacks[facenum][facestacktops[facenum]] := vv.id;
          face_edge_stacks[facenum][facestacktops[facenum]-1] := scanlist[inspot+delcount-1];
          face_sidedness[facenum] := LEFTSIDE;
        };
      };

      if delcount == 0 and scancount > 0 then
      { // this should never happen on this scan. 
        errprintf "cap_one_plane internal error: no left edge on vertex %d\n",vv.id;
        abort;
      };

      // delete the left edges from scanlist (leaving one in, so face list ok)
      for ( knx := inspot+1 ; knx <= scancount-delcount+1 ; knx++ )
      { scanlist[knx] := scanlist[knx+delcount-1];
        face_list[knx-1] := face_list[knx-1+delcount-1];
      };
      scancount -= delcount-1;


      if rightcount then
      { // insert right edges into scanlist
  
        // open gap in scanlist (note we saved a gap of size one in deletions)
        if rightcount > 1 then
          for ( jnx := scancount ; jnx >= inspot ; jnx-- )
          { scanlist[jnx+rightcount-1] := scanlist[jnx];
            face_list[jnx+rightcount-1] := face_list[jnx];
          };
        // insert new edges
        for ( {jnx := 1; knx := inspot} ; jnx <= rightcount ; {jnx++;knx++} )
        { scanlist[knx] := rightlist[jnx];
          if jnx < rightcount then 
          { // new face starts
            face_counter++;
            face_list[knx] := face_counter;
            facestacktops[face_counter] := 1;
            facestacks[face_counter][1] := vv.id;
          };
        }; 

        scancount += rightcount-1;
        if capper_debug_flag then
        { printf "Inserted edges ";
          for ( jnx := inspot ; jnx < inspot+rightcount ; jnx++ ) printf "%d ",
            scanlist[jnx]; printf "\n";
          printf "Scanlist:      "; for ( jnx := 1 ; jnx <= scancount ; jnx++ ) printf "%d ",
            scanlist[jnx]; printf "\n";
        };
        // set up for order sorting
        startspot := inspot;
        endspot := inspot + rightcount - 1;
      }
      else
      { // only way to get here is at the very end, and nothing to do.
      };
    
      // sort new edges in scan order, just bubble sort 
      for ( inx := startspot ; inx < endspot ; inx++ )
        for ( jnx := inx+1 ; jnx <= endspot ; jnx++ )
        { 
          dir1 := (dc_scanvec*edge[scanlist[inx]].edge_vector)/edge[scanlist[inx]].length;
          dir2 := (dc_scanvec*edge[scanlist[jnx]].edge_vector)/edge[scanlist[jnx]].length;
          if dir1 > dir2 then 
          { // swap
            temp := scanlist[inx];
            scanlist[inx] := scanlist[jnx];
            scanlist[jnx] := temp;
          };
        };
      if capper_debug_flag then
      { printf "Sorted Scanlist: "; 
        for ( jnx := 1 ; jnx <= scancount ; jnx++ ) printf "%4d ",scanlist[jnx]; printf "\n";
        printf "face list:         "; 
        for ( jnx := 1 ; jnx < scancount ; jnx++ ) printf "%4d ",face_list[jnx]; printf "\n";
        for ( jnx := 1 ; jnx <= face_counter ; jnx++ )
        { if face_sidedness[jnx] == LEFTSIDE then
            printf "face %d LEFTSIDE stack:  ",jnx
          else if face_sidedness[jnx] == RIGHTSIDE then
            printf "face %d RIGHTSIDE stack: ",jnx
          else
            printf "face %d UNSIDED stack:   ",jnx;
          for ( knx := 1 ; knx <= facestacktops[jnx] ; knx++ )
             printf "%4d ",facestacks[jnx][knx];
          printf "\n";
        };
      }
    };
  };   // end dc_verlist 
  if capper_debug_flag then
    printf "End third sweep. face_counter %d\n",face_counter;

  // end third sweep sweep

  // Now fill in interior cracks
  local changes,found_flag,f_id,newf;
   do 
   { changes := 0;
     for ( listspot := 1 ; listspot <= vercount ; listspot++ )
     { foreach vertex[dc_verlist[listspot]] vv do
       { 
         foreach vv.edge ee where ee.ecaptype == new_captype and ee.valence==1 do
         { found_flag := 0;
           f_id := ee.facet[1].oid;
           // find two new edges making a triangle
           foreach ee.vertex[1].edge eee where (eee.ecaptype == new_captype or 
               eee.ecaptype==celledge_captype or eee.valence==1) and eee.valence <= 1 do
             foreach ee.vertex[2].edge eeee where (eeee.ecaptype == new_captype or 
                eeee.ecaptype==celledge_captype or eeee.valence==1) and eee.valence <= 1 do
             { if eee.vertex[2].id == eeee.vertex[2].id then
               { 
                 // Make new facet. First, test orientation.
                 if dc_planenormal[1]*(ee.y*eeee.z-ee.z*eeee.y) +
                    dc_planenormal[2]*(ee.z*eeee.x-ee.x*eeee.z) +
                    dc_planenormal[3]*(ee.x*eeee.y-ee.y*eeee.x) > 0.0 then
                   newf := new_facet(ee.oid,eeee.oid,-eee.oid)
                 else
                   newf := new_facet(eee.oid,-eeee.oid,-ee.oid);
                 facet[newf].color := yellow;
                 facet[newf].fcaptype := new_captype;
                 if facet[f_id].frontbody then
                   facet[newf].frontbody := facet[f_id].frontbody
                 else
                   facet[newf].frontbody := facet[f_id].backbody;
                 found_flag := 1;
                 break 2;
               };
             };
           if not found_flag then
           {  errprintf "Cannot find edges to construct facet on edge %d\n",ee.id;
              ee.color := magenta;
              recalc;
              pickvnum := vv.id;
              // subcommand;
              // abort;
           }
           else
             changes++;
         };
       };
     }; 
   } while changes;

   // get rid of all the unused new edges
   dissolve edge where ecaptype == new_captype and valence==0;

   // prettify
   do 
   { reset_counts;
     equiangulate edge where ecaptype == new_captype;
   } while equi_count;

} // end cap_one_plane

/****************************************************************************************/

// Add boundary lines around unit cell.  Maybe change to only add edges in interior of
// bodies?  But entire edge might be interior, so couldn't tell locally.

define capper_corner_vlist integer[8]; // so we can delete these later
capper_corner_vcount := 0;

capper_unit_cell := {
  local  inx,jnx,knx,jj,kk,vcount,vlist,spot,newv,newe,vlevel;
  define vlist integer[100];
  local epsilon;
  epsilon := 1e-8; // small margin of error for numerical inaccuracy

  // get normal vectors to unit cell faces from inverse_periods
  local cell_normals;
  define cell_normals real[3][3];
  for ( inx := 1 ; inx <= 3 ; inx++ )
    for ( jnx := 1 ; jnx <= 3 ; jnx++ )
      cell_normals[inx][jnx] := inverse_periods[jnx][inx];

  capper_corner_vcount := 0;
  for ( inx := 1 ; inx <= 3 ; inx++ ) // direction of edge
  { jj := inx == 3 ? 1 : inx+1; // the two other dimensions
    kk := inx == 1 ? 3 : inx-1;
    for ( jnx := 0 ; jnx <= 1 ; jnx++ ) // bary coord in one other direction
      for ( knx := 0 ; knx <= 1 ; knx++ ) // bary coord in the remaining direction
      { // gather list of existing points on edge
        vcount := 0;
        foreach vertex vv where abs(vv.__x * cell_normals[jj] - jnx) < epsilon and
           abs(vv.__x * cell_normals[kk] - knx) < epsilon do
           { vcount++;
             // insertion sort
             vlevel := vv.__x * cell_normals[inx];
             spot := vcount;
             while spot > 1 and vertex[vlist[spot-1]].__x*cell_normals[inx] > vlevel do
             { vlist[spot] := vlist[spot-1];
               spot--;
             };
             vlist[spot] := vv.id;
           };
        // Have sorted list. See if we need bottom vertex
        if vcount==0 or vertex[vlist[1]].__x * cell_normals[inx] > epsilon then
        { // need it
          newv := new_vertex(jnx*torus_periods[jj][1]+knx*torus_periods[kk][1],
                             jnx*torus_periods[jj][2]+knx*torus_periods[kk][2],
                             jnx*torus_periods[jj][3]+knx*torus_periods[kk][3]);
          // insert it
          for ( spot := vcount ; spot >= 1 ; spot-- )
            vlist[spot+1] := vlist[spot];
          vlist[1] := newv;
          vcount++;
          capper_corner_vcount++;
          capper_corner_vlist[capper_corner_vcount] := newv;
        };
        // See if we need top vertex
        if vertex[vlist[vcount]].__x * cell_normals[inx] < 1-epsilon then
        { // need it
          newv := new_vertex(torus_periods[inx][1]+jnx*torus_periods[jj][1]+knx*torus_periods[kk][1],
                             torus_periods[inx][2]+jnx*torus_periods[jj][2]+knx*torus_periods[kk][2],
                             torus_periods[inx][3]+jnx*torus_periods[jj][3]+knx*torus_periods[kk][3]);
          // insert it
          vcount++;
          vlist[vcount] := newv;
          capper_corner_vcount++;
          capper_corner_vlist[capper_corner_vcount] := newv;
        };

        // Now put in any connecting edges needed
        for ( spot := 1 ; spot < vcount ; spot++ )
          if max(vertex[vlist[spot]].edge ee, ee.vertex[2].id == vlist[spot+1]) <= 0 then
          { newe := new_edge(vlist[spot],vlist[spot+1]);
            edge[newe].color := green;
            edge[newe].ecaptype := celledge_captype;
            set edge[newe] bare;
          };
      }; // end for knx
  } // end for inx

} // end capper_unit_cell

// test on wettest1_0.fe
// capper_unit_cell;
// set facet fcaptype bodied_captype;
// cap_one_plane(1.0, 0.0, 0.0, 1.0);

/*****************************************************************************************/

// Single-plane slice and cap.
read "slicer.cmd"

procedure slice_and_cap(real aa, real bb, real cc, real rhs)
{
     // sanity checks
     if space_dimension != 3 then
     { errprintf "slice_and_cap ERROR: space dimension must be 3.\n";
       return; 
     };

     if torus then 
     { errprintf "slice_and_cap ERROR: Cannot proceed in torus mode.  Do 'detorus' first.\n";
       return;
     };

     // "slicer" uses inward normal convention, so reverse signs
     slice_a := -aa;
     slice_b := -bb;
     slice_c := -cc;
     slice_d := -rhs;
     slicer;
     
     cap_one_plane(aa,bb,cc,rhs);

} // end slice_and_cap()

/*****************************************************************************************/


// Main command.  Assumes inverse_periods holds valid data.
detorus_capper := {
     local inx;

     // sanity checks
     if space_dimension != 3 then
     { errprintf "detorus_capper ERROR: space dimension must be 3.\n";
       return; 
     };

     if sizeof(torus_periods) != 9 then
     { errprintf "detorus_capper ERROR: torus_periods not valid.  Must load datafile in torus mode.\n";
       return;
     };

     if torus then 
     { errprintf "detorus_capper ERROR: Cannot proceed in torus mode.  Do 'detorus' first.\n";
       return;
     };

     // create auxiliary unit cell edges
     capper_unit_cell;

     // cap the six faces of the unit cell
     for ( inx := 1 ; inx <= space_dimension ; inx++ )
     {
       cap_one_plane(inverse_periods[1][inx],inverse_periods[2][inx],inverse_periods[3][inx],1);
       cap_one_plane(-inverse_periods[1][inx],-inverse_periods[2][inx],-inverse_periods[3][inx],0);
     };

     // Get rid of the auxiliary unit cell edges that were not used.
     dissolve edge where ecaptype == celledge_captype and valence==0;
     for ( inx := 1 ; inx <= capper_corner_vcount ; inx++ )
       dissolve vertex[capper_corner_vlist[inx]];

     // Get rid of new triangulation edges that weren't used
     dissolve edge where ecaptype == new_captype and valence==0;

     unset edge bare where valence >= 1;  // used unit cell edges

} // end detorus_capper

quiet off;

/* detorus_capper usage:
      clipped;
      detorus;
      capper_set_constraints; // suggested 
      detorus_capper_groom;   // suggested, if did capper_set_constraints
      detorus_capper;
*/

