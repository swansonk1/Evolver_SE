// unseam.cmd

// Splits a surface along marked edges.

// Programmer: Ken Brakke, brakke@susqu.edu  www.susqu.edu/brakke

define edge attribute unseam_mark integer

unseam := {

  local facet2, doomed_facet, midvertex, fix_status, oldcoord, nextedge;
  local oldedge, oldfacet, oldvertex, nextvertex, nextfacet;

  // Check assumptions
  if space_dimension != 3 then
  { errprintf "The 'unseam' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'unseam' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'unseam' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'unseam' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  foreach edge ee where unseam_mark do
  {
    // Make sure valence is 2
    if ee.valence != 2 then
    { errprintf "unseam: valence of marked edge %d is not 2. Aborting.\n",ee.id;
      abort;
    };
  };

  foreach edge ee where unseam_mark do
  {
    // Split edge while keeping same endpoints, by refining one facet
    // and dissolving part next to marked edge and deleting one edge.
    facet2 := ee.facet[2].id;
    refine ee.facet[1]; 
    foreach ee.facet ff where ff.id != facet2 do 
    { doomed_facet := ff.id;
      break;
    };
    foreach facet[doomed_facet].vertex vv where vv.id != ee.vertex[1].id
      and vv.id != ee.vertex[2].id do
    { midvertex := vv.id;
      break;
    };
    dissolve facet[doomed_facet];
    fix_status := ee.vertex[1].fixed;
    fix ee.vertex[1];
    delete ee.vertex[1].edge eee where eee.vertex[2].id == midvertex;
    if not fix_status then unfix ee.vertex[1];
  };
return;
  // Now have to pop vertices that have two gaps
  foreach vertex vv where sum(vv.edge,valence==1) >= 4 and
    sum(vv.edge,unseam_mark) > 0 do
  while sum(vv.edge,valence==1) > 2 do
  { define oldcoord real[3];
    oldcoord := vv.__x;
    // pick one valence 1 edge and propagate deletable edges
    foreach vv.edge ee where valence==1 do
    { 
      nextedge := ee.id;
      break;
    };
    oldedge := 0;
    oldfacet := 0;
    oldvertex := 0;
    while nextedge do
    {
      refine edge[nextedge];  // nextedge should be tail half
      nextvertex := edge[nextedge].vertex[2].id;
      if oldfacet then
      { delete vertex[nextvertex].edge ee where ee.vertex[2].id == oldvertex;
        if valid_element(edge[nextedge]) then
          oldedge := nextedge;
      }
      else oldedge := nextedge;
      if edge[oldedge].valence == 0 then break;
      oldvertex := edge[oldedge].vertex[2].id;
      nextfacet := edge[oldedge].facet[1].id;
      nextedge := 0;
      foreach facet[nextfacet].edge eee where eee.vertex[2].id == vv.id do
      { nextedge := -eee.id;  
        break;
      };
    };

  }
}
// End unseam.cmd

/* Usage:
     set edge unseam_mark (boolean for those edges you want to split)
     unseam
*/
