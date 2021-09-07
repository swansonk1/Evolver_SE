// cut.cmd

// Cut multiply connected surface into simply connected surface.

// Algorithm: Grow simply connected region, adding triangles, and
// zipping up seams.

/* Usage: cut
   Leaves the cut edges with ecutmark = 1
*/

// Programmer: Ken Brakke, brakke@susqu.edu  www.susqu.edu/brakke

define edge attribute ecutmark integer;
/* 0 for not a cut, 1 for possible cut, 2 for valence 1 edge */


define facet attribute fcutmark integer;


// G
cut_groom := {
  // smooth out sawteeth by moving cut on two sides of a facet
  // to one side, if vertex involved only has two cut edges.
  local changes;

  do 
  { changes := 0;
    foreach facet ff where sum(ff.edge,ecutmark==1)==2 do
    { foreach ff.vertex vv where
        sum(vv.edge ee,ecutmark==1) == 2 and
         sum(vv.edge ee where sum(ee.facet,id==ff.id),ecutmark==1)==2 do
      { 
        set ff.edge ecutmark (1-ecutmark) where ecutmark!=2;
        changes++;
        break;
      }
    }
  } while changes;
  recalc;

} 

cut := {
  local changes,endcrit;
  
  if max(edge, valence) > 2 then
  { errprintf "ERROR: 'cut' requires maximum edge valence 2.\n";
    abort;
  };

  set edge ecutmark (3-valence);  // all start as cuts
  set facet fcutmark 0;

  foreach facet ff do { ff.fcutmark := 1; break; };
  

  // grow connected region
  do
  { changes := 0; 
    foreach edge ee where ee.ecutmark == 1 do
    { if sum(ee.facet,fcutmark)==1 then
      { set ee.facet fcutmark 1;
        ee.ecutmark := 0;
        changes++;
      };
      if sum(ee.facet,fcutmark)==2 then
      { // see if can sew up
        endcrit := min(ee.vertex vv, sum(vv.edge eee,eee ecutmark)==1);
        if endcrit == 1 then
        { ee.ecutmark := 0;
          changes++;
        }
      };
    };
  } while changes;
  set edge ecutmark 0 where ecutmark == 2; // unmark valence 1 edges

  cut_groom;
}

// End cut.cmd

// Usage: cut

