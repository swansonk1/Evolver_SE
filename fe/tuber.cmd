// tuber.cmd

// Surface Evolver command to put tubes around certain edges, for more
// reliable display of chosen edges.  Note that this procedure does
// modify the current surface, rather than write a datafile.
// Also note the tubes created are just separate tubes for each
// edge; they are not connected to each other in any way,  so
// they are really meant just for display. 

// "tuber" works in the torus model, a new body is created for all
// the tube facets to be on, so they will display when "connected"
// display mode is in effect.  However, the tubes are displayed
// independently of the other bodies, and tend to stick out in 
// various directions.


// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

/* Usage:  Define the "intube" edge attribute to be 1 for the edges
           you want tubed (simply reading in this file automatically
           creates the intube attribute for you), set the edge colors 
           to the colors you want the tubes to be, and call "tuber" 
           with appropriate parameters,
                 real tube_radius,   // radius of tube
                 integer tube_sides, // how many sides on each tube, >= 3
                 integer tube_caps   // 0 for no caps, 1 for cone caps

           For example, 

    Enter command: set edge intube 0
    Enter command: set edge intube 1 where valence != 2;
    Enter command: set edge color green where intube==1;   
    Enter command: tuber(0.01,4,1)

 If you want to display just the tubes, you can do
    Enter command: show facet where color == green

*/

define edge attribute intube integer  // set positive if want tube

procedure tuber (real tube_radius,   // radius of tube
                 integer tube_sides, // how many sides on each tube, >= 3
                 integer tube_caps   // 0 for no caps, 1 for cone caps
                )
{
  local ax,ay,az,bx,by,bz,mag,elength,tailcapv,headcapv,t_angle,vv1,ww1;
  local ee1,start_tailcape,start_headcape,prev_tailcape,prev_headcape;
  local vstart,wstart,starte,inx,vv2,ww2,ee2,vend,wend,diag,newf;
  local tailcape,headcape;
  local newb;

  // Some parameter checking
  if tube_sides < 3 then tube_sides := 3;
  if tube_radius <= 0 then
  { errprintf "tuber: tube_radius is %f; must be positive.\n",tube_radius;
    return;
  };

  // Make a new body for the tubes to be on, so they
  // show up in "connected" mode.
  if torus then
     newb := new_body;

  // Now the tubes.
  foreach edge ee where intube > 0 do
  { 
    // find two orthogonal directions to edge; if possible,
    // one along adjacent facet
    if ee.valence > 0 then
    { ax := ee.facet[1].x;
      ay := ee.facet[1].y;
      az := ee.facet[1].z;
    }
    else
    { if abs(ee.x) < abs(ee.y) and abs(ee.x) < abs(ee.z) then
      { ax := 1; ay := 0; az := 0;}
      else if abs(ee.y) < abs(ee.z) then
      { ax := 0; ay := 1; az := 0; }
      else
      { ax := 0; ay := 0; az := 1; };
    };
    bx := ay*ee.z - az*ee.y;
    by := az*ee.x - ax*ee.z;
    bz := ax*ee.y - ay*ee.x;
    ax := by*ee.z - bz*ee.y;
    ay := bz*ee.x - bx*ee.z;
    az := bx*ee.y - by*ee.x;
    // normalize to radius
    mag := sqrt(ax^2+ay^2+az^2);
    if mag <= 0 then continue;
    ax *= tube_radius/mag;
    ay *= tube_radius/mag;
    az *= tube_radius/mag;
    mag := sqrt(bx^2+by^2+bz^2);
    if mag <= 0 then continue;
    bx *= tube_radius/mag;
    by *= tube_radius/mag;
    bz *= tube_radius/mag;

    // Tube cap tips, if wanted
    if tube_caps then
    { elength := ee.length;
      tailcapv := new_vertex(ee.vertex[1].x - ee.x/elength*tube_radius,
                             ee.vertex[1].y - ee.y/elength*tube_radius,
                             ee.vertex[1].z - ee.z/elength*tube_radius);
      headcapv := new_vertex(ee.vertex[1].x + ee.x + ee.x/elength*tube_radius,
                             ee.vertex[1].y + ee.y + ee.y/elength*tube_radius,
                             ee.vertex[1].z + ee.z + ee.z/elength*tube_radius);
      fix vertex[tailcapv];
      fix vertex[headcapv];
    };

    // Construct tubes
    t_angle := 2*pi/tube_sides;
    vv1 := new_vertex(ee.vertex[1].x+ax*cos(0*t_angle)+bx*sin(0*t_angle),
                     ee.vertex[1].y+ay*cos(0*t_angle)+by*sin(0*t_angle),
                     ee.vertex[1].z+az*cos(0*t_angle)+bz*sin(0*t_angle));
    ww1 := new_vertex(ee.vertex[1].x+ee.x+ax*cos(0*t_angle)+bx*sin(0*t_angle),
                     ee.vertex[1].y+ee.y+ay*cos(0*t_angle)+by*sin(0*t_angle),
                     ee.vertex[1].z+ee.z+az*cos(0*t_angle)+bz*sin(0*t_angle));
    fix vertex[vv1];
    fix vertex[ww1];
    ee1 := new_edge(vv1,ww1);
    edge[ee1].color := ee.color;
    if tube_caps then
    { start_tailcape := new_edge(tailcapv,vv1);
      start_headcape := new_edge(ww1,headcapv);
      edge[start_tailcape].color := ee.color;
      edge[start_headcape].color := ee.color;
      prev_tailcape := start_tailcape;
      prev_headcape := start_headcape;
    };
    vstart := vv1; wstart := ww1; starte := ee1;
    for ( inx := 1 ; inx <= tube_sides ; inx += 1 )
    {
      if inx == tube_sides then
      { vv2 := vstart; ww2 := wstart; ee2 := starte; }
      else
      { vv2 := new_vertex(ee.vertex[1].x+ax*cos(inx*t_angle)+bx*sin(inx*t_angle),
                     ee.vertex[1].y+ay*cos(inx*t_angle)+by*sin(inx*t_angle),
                     ee.vertex[1].z+az*cos(inx*t_angle)+bz*sin(inx*t_angle));
        ww2 := new_vertex(ee.vertex[1].x+ee.x+ax*cos(inx*t_angle)+bx*sin(inx*t_angle),
                     ee.vertex[1].y+ee.y+ay*cos(inx*t_angle)+by*sin(inx*t_angle),
                     ee.vertex[1].z+ee.z+az*cos(inx*t_angle)+bz*sin(inx*t_angle));
        ee2 := new_edge(vv2,ww2);
        edge[ee2].color := ee.color;
      };
      vend := new_edge(vv1,vv2);
      wend := new_edge(ww1,ww2);
      diag := new_edge(vv2,ww1);
      edge[vend].color := ee.color;
      edge[wend].color := ee.color;
      edge[diag].color := ee.color;

      set edge[vend] no_refine;
      set edge[wend] no_refine;
      set edge[diag] no_refine;
      fix edge[vend];
      fix edge[wend];
      fix edge[diag];

      newf := new_facet(vend,diag,-ee1);
      set facet[newf] color ee.color;
      fix facet[newf];
      set facet[newf] no_refine;
      set facet[newf] tension 0;
      if torus then set facet[newf] frontbody newb;


      newf := new_facet(ee2,-wend,-diag);
      set facet[newf] color ee.color;
      fix facet[newf];
      set facet[newf] no_refine;
      set facet[newf] tension 0;
      if torus then set facet[newf] frontbody newb;

      if tube_caps then
      { if inx == tube_sides then
        { tailcape := start_tailcape;
          headcape := start_headcape;
        }
        else
        { tailcape := new_edge(tailcapv,vv2);
          headcape := new_edge(ww2,headcapv);
          edge[tailcape].color := ee.color;
          edge[headcape].color := ee.color;
        };
        fix edge[tailcape];
        fix edge[headcape];
        newf := new_facet(tailcape,-vend,-prev_tailcape);
        set facet[newf] color ee.color;
        fix facet[newf];
        set facet[newf] no_refine;
        set facet[newf] tension 0;
        if torus then set facet[newf] frontbody newb;
        newf := new_facet(headcape,-prev_headcape,wend);
        set facet[newf] color ee.color;
        fix facet[newf];
        set facet[newf] no_refine;
        set facet[newf] tension 0;
        if torus then set facet[newf] frontbody newb;

        prev_tailcape := tailcape;
        prev_headcape := headcape;
      };

      vv1 := vv2; ww1 := ww2; ee1 := ee2;
    }
  }
 
} // end tuber()
   

// End tuber.cmd

/* Usage:
     set edge intube  (boolean expression for the edges you want tubed)
     tuber(real tube_radius,integer tube_sides, integer tube_caps) 

*/

