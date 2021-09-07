// strips.cmd
// Creates strips as for OpenGL GL_TRIANGLE_STRIPS.  Just colors the strips.

// But also see the graphics window 'S' and 'Y' commands.

// Programmer:  Ken Brakke  brakke@susqu.edu  www.susqu.edu/brakke

// "strips" just takes one stab at a strip for each facet.
strips := {
    local stripcolor,stripcount,testcolor;
    local trystart,whichwaystart,whichway;
    local eid,fid,fffid,eeid,goodstart,striplength;
    local firstcount,secondcount,trucfacet;

    set facet color white;
    stripcolor := 1;
    stripcount := 0;
    foreach facet ff where  color == white do
    { ff.color := stripcolor;

      whichwaystart := 0;
      while whichwaystart <= 1 do
      { 
        eid := ff.edge[whichwaystart+1].oid;
        fid := ff.id;
        whichway := whichwaystart;   // alternating directions of triangles

        while edge[eid].valence >= 2 do
        {
          foreach edge[eid].facet fff do 
          { if fff.id != fid then {fffid := fff.id; break;} };
          if facet[fffid].color != white then break;
          facet[fffid].color := stripcolor;
          foreach facet[fffid].edge ee do 
          { eeid := ee.oid;
            if whichway then
            { if ee.vertex[2].id == edge[eid].vertex[2].id then break; }
            else 
            { if ee.vertex[1].id == edge[eid].vertex[1].id then break; }
          };
          eid := eeid;
          fid := fffid;
          whichway := !whichway;
        };
        whichwaystart += 1;
      };


      stripcolor := (stripcolor imod 14) + 1;
      stripcount += 1;
    };
  printf "%g strips, average %g facets.\n",stripcount,facet_count/stripcount;
}
     
// "strips2" searches for longest strip through facet, out of 3 ways
strips2 := {
    local stripcolor,stripcount,testcolor;
    local trystart,whichwaystart,whichway;
    local eid,fid,fffid,eeid,maxlength,goodstart,striplength;

    set facet color white;
    stripcolor := 1;
    stripcount := 0;
    testcolor := 100;
    foreach facet ff where  color == white do
    { ff.color := stripcolor;

      // three-way trial first
      trystart := 1; maxlength := 0; goodstart := 1;
      while trystart <= 3 do
      { striplength := 1;

      whichwaystart := 0;
      while whichwaystart <= 1 do
      { 
        eid := ff.edge[((whichwaystart+trystart)imod 3)+1].oid;
        fid := ff.id;
        whichway := whichwaystart;   // alternating directions of triangles

        while edge[eid].valence >= 2 do
        {
          foreach edge[eid].facet fff do 
          { if fff.id != fid then {fffid := fff.id; break;} };
          if facet[fffid].color != white then break;
          facet[fffid].color := testcolor; striplength += 1;
          foreach facet[fffid].edge ee do 
          { eeid := ee.oid;
            if whichway then
            { if ee.vertex[2].id == edge[eid].vertex[2].id then break; }
            else 
            { if ee.vertex[1].id == edge[eid].vertex[1].id then break; }
          };
          eid := eeid;
          fid := fffid;
          whichway := !whichway;
        };
        whichwaystart += 1;
      };
      if ( striplength > maxlength ) then
      { maxlength := striplength; goodstart := trystart; };
      trystart += 1;
      set facet color white where color==testcolor;
      };


      // now for real
      whichwaystart := 0;
      while whichwaystart <= 1 do
      { 
        eid := ff.edge[((whichwaystart+goodstart)imod 3)+1].oid;
        fid := ff.id;
        whichway := whichwaystart;   // alternating directions of triangles

        while edge[eid].valence >= 2 do
        {
          foreach edge[eid].facet fff do 
          { if fff.id != fid then {fffid := fff.id; break;} };
          if facet[fffid].color != white then break;
          facet[fffid].color := stripcolor;
          foreach facet[fffid].edge ee do 
          { eeid := ee.oid;
            if whichway then
            { if ee.vertex[2].id == edge[eid].vertex[2].id then break; }
            else 
            { if ee.vertex[1].id == edge[eid].vertex[1].id then break; }
          };
          eid := eeid;
          fid := fffid;
          whichway := !whichway;
        };
        whichwaystart += 1;
      };



      stripcolor := (stripcolor imod 14) + 1;
      stripcount += 1;
    };
  printf "%g strips, average %g facets.\n",stripcount,facet_count/stripcount;
}
     
// for testing whether order of trying facets matters much
   define vertex attribute vertex_order_key real
   define edge attribute edge_order_key real
   define facet attribute facet_order_key real
   define body attribute body_order_key real
   define facetedge attribute facetedge_order_key real


   reorder := {
     set vertex vertex_order_key x+y+z;
     set edge ee edge_order_key min(ee.vertex,vertex_order_key);
     set facetedge fe facetedge_order_key fe.edge[1].edge_order_key;
     set facet ff facet_order_key min(ff.vertex,vertex_order_key);
     set body bb body_order_key min(bb.facet,facet_order_key);
     reorder_storage;
     }
     
// searches for longest strip through facet, out of 3 ways
// Uses same strip truncation as Evolver to get properly oriented strip.
strips3 := {
    local  fffid, eeid, maxlength, goodstart, striplength, testcolor, stripcolor;
    local  stripcount, trystart, whichwaystart, eid, fid, whichway, firstcount;
    local  secondcount, truncfacet;

    testcolor := 100;
    set facet color white;
    stripcolor := 1;
    stripcount := 0;
    foreach facet ff where  color == white do
    { ff.color := stripcolor;

      // three-way trial first
      trystart := 1; maxlength := 0; goodstart := 1; 
      while trystart <= 3 do
      { striplength := 1;

      whichwaystart := 0;
      while whichwaystart <= 1 do
      { 
        eid := ff.edge[((whichwaystart+trystart)imod 3)+1].oid;
        fid := ff.id;
        whichway := whichwaystart;   // alternating directions of triangles

        while edge[eid].valence >= 2 do
        {
          firstcount := striplength;
          foreach edge[eid].facet fff do 
          { if fff.id != fid then {fffid := fff.id; break;} };
          if facet[fffid].color != white then break;
          facet[fffid].color := testcolor; striplength += 1;
          foreach facet[fffid].edge ee do 
          { eeid := ee.oid;
            if whichway then
            { if ee.vertex[2].id == edge[eid].vertex[2].id then break; }
            else 
            { if ee.vertex[1].id == edge[eid].vertex[1].id then break; }
          };
          eid := eeid;
          fid := fffid;
          whichway := !whichway;
        };
        secondcount := striplength - firstcount;
        whichwaystart += 1;
      };
      if ( (firstcount imod 2) and (secondcount imod 2) ) then striplength -= 1;
      if ( striplength > maxlength ) then
      { maxlength := striplength; goodstart := trystart; };
      trystart += 1;
      set facet color white where color==testcolor;
      };


      // now for real
      truncfacet := ff.id;
      whichwaystart := 0;
      while whichwaystart <= 1 do
      { 
        firstcount := striplength;
        eid := ff.edge[((whichwaystart+goodstart)imod 3)+1].oid;
        fid := ff.id;
        whichway := whichwaystart;   // alternating directions of triangles

        while edge[eid].valence >= 2 do
        {
          foreach edge[eid].facet fff do 
          { if fff.id != fid then {fffid := fff.id; break;} };
          if facet[fffid].color != white then break;
          facet[fffid].color := stripcolor;
          if ( whichwaystart == 1 ) then truncfacet := fffid;
          foreach facet[fffid].edge ee do 
          { eeid := ee.oid;
            if whichway then
            { if ee.vertex[2].id == edge[eid].vertex[2].id then break; }
            else 
            { if ee.vertex[1].id == edge[eid].vertex[1].id then break; }
          };
          eid := eeid;
          fid := fffid;
          whichway := !whichway;
        };
        secondcount := striplength - firstcount;
        whichwaystart += 1;
      };
      if ( (firstcount imod 2) and (secondcount imod 2) ) then
           set facet[truncfacet] color white;



      stripcolor := (stripcolor imod 14) + 1;
      stripcount += 1;
    };
  printf "%g strips, average %g facets.\n",stripcount,facet_count/stripcount;
}
     

// end strips.cmd

/* Usage:
      strips
      strips2
*/

