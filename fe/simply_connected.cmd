// simply_connected.cmd

// Surface Evolver command to find seams for splitting a multiply-connected
// surface into a simply connected surface.  Meant for setting up Bonnet
// rotations of triply-periodic minimal surfaces.

define vertex attribute sc_vmark integer 
define edge attribute sc_emark integer 
define facet attribute sc_fmark integer
define vertex attribute sc_vdistance integer
define edge attribute sc_edistance integer
define facet attribute sc_fdistance integer

sc_startf := 0
sc_startv := 0

mark_distances := {

  local changes,current_distance,unmarked_edges,unmarked_vertices;

  // sanity checks
  if max(edge,valence) > 2 then
  { printf "mark_simply_connected error: there are edges with valence > 2.\n";
    abort;
  };
  
set facet color white;
set edge color black;

  // starting vertex  
  if not valid_element(vertex[sc_startv]) then  // find a starting facet
    foreach vertex vv do 
    { sc_startv := vv.id;
      break;
    };


  // initialize
  set vertex sc_vdistance -1;
  set edge sc_edistance -1;
  set facet sc_fdistance -1;
  foreach vertex[sc_startv].facet ff do
  {
    ff.sc_fdistance := 1;
    set ff.vertex sc_vdistance 1;
    set ff.edge sc_edistance 1;
  };
  vertex[sc_startv].sc_vdistance := 0;

  current_distance := 1;

  // sweep with increasing distance, adding further facets that are
  // edge neighbors as long as Euler sum is ok.
  do
  { 
    changes := 0;
    foreach edge ee where valence == 2 and 
      ee.sc_edistance == current_distance and
        (ee.facet[1].sc_fdistance*ee.facet[2].sc_fdistance < 0) do
    { foreach ee.facet fff where fff.sc_fdistance == -1 do
      { // add facet
        fff.sc_fdistance := current_distance + 1;
        set fff.edges.sc_edistance current_distance + 1 where sc_edistance < 0;
        set fff.vertices.sc_vdistance current_distance + 1 where sc_vdistance < 0;
        changes += 1;
fff.color := (current_distance imod 14) + 1;
      }
    };
    current_distance += 1;
printf "distance %d\n",current_distance;
  } while changes;


}

// old version calculates its own distances, but those can be messed
// up by the simply-connected mechanism.
mark_simply_connected_old := {

  local changes,current_distance,unmarked_edges,unmarked_vertices;

  // sanity checks
  if max(edge,valence) > 2 then
  { printf "mark_simply_connected error: there are edges with valence > 2.\n";
    abort;
  };
  
set facet color white;
set edge color black;

  // starting vertex  
  if not valid_element(vertex[sc_startv]) then  // find a starting facet
    foreach vertex vv do 
    { sc_startv := vv.id;
      break;
    };


  // initialize
  set vertex sc_vmark -1;
  set edge sc_emark -1;
  set facet sc_fmark -1;
  set facet sc_fdistance -1;
  foreach vertex[sc_startv].facet ff do
  {
    ff.sc_fdistance := 1;
    set ff.vertex sc_vmark 1;
    set ff.edge sc_emark 1;
  };
  current_distance := 1;

  // sweep with increasing distance, adding further facets that are
  // edge neighbors as long as Euler sum is ok.
  do
  { changes := 0;
    foreach facet ff where sc_fdistance == current_distance do
    { foreach ff.edge ee where valence == 2 and 
          (ee.facet[1].sc_fdistance*ee.facet[2].sc_fdistance < 0) do
      { foreach ee.facet fff where fff.sc_fdistance == -1 do
        { local unmarked_edges,unmarked_vertices;
          unmarked_edges := sum(fff.edge, sc_emark < 0);
          unmarked_vertices := sum(fff.vertex, sc_vmark < 0);
          if unmarked_vertices - unmarked_edges + 1 == 0 then
          { // add facet
            fff.sc_fdistance := current_distance + 1;
            set fff.edges.sc_emark current_distance + 1 where sc_emark < 0;
            set fff.vertices.sc_vmark current_distance + 1 where sc_vmark < 0;
            changes += 1;
fff.color := (current_distance imod 14) + 1;
          }
        }

      }
    };
    current_distance += 1;
printf "distance %d\n",current_distance;
  } while changes;

  // distance numbers for the open band facets
  set facet sc_fmark (sc_fdistance > 0);
  do
  { changes := 0;
    foreach facet ff where sc_fmark == 0 do
    { local newdist;
      newdist := min(ff.edge ee where sc_emark > 0, sc_emark);
      if newdist > 0 and ((newdist < ff.sc_fdistance) or (ff.sc_fdistance < 0))
      then
      { ff.sc_fdistance := newdist + 1;
        set ff.edge sc_emark newdist + 1;
ff.color := (newdist imod 14) + 1;
        changes += 1;
      };
    };
printf "changes %d\n",changes;
  } while changes;



set edge ee color white where valence==2 and 
 ee.facet[1].sc_fdistance == ee.facet[2].sc_fdistance
 and max(ee.facet, sc_fmark==0);

set edge ee color green where valence==2 and 
   abs(ee.facet[1].sc_fdistance-ee.facet[2].sc_fdistance)==2
     and max(ee.facet, sc_fmark==0);
} 




  


  
  
mark_simply_connected := {

  local max_dist;

  // assuming mark_distances has already been run.

  // sanity checks
  if max(edge,valence) > 2 then
  { printf "mark_simply_connected error: there are edges with valence > 2.\n";
    abort;
  };
  
set facet color white;
set edge color black;

  max_dist := max(facet,sc_fdistance);

  // starting vertex  
  if not valid_element(vertex[sc_startv]) then  // find a starting facet
    foreach vertex vv do 
    { sc_startv := vv.id;
      break;
    };


  // initialize
  set vertex sc_vmark -1;
  set edge sc_emark -1;
  set facet sc_fmark -1;
  foreach vertex[sc_startv].facet ff do
  {
    ff.sc_fmark := 1;
    set ff.vertex sc_vmark 1;
    set ff.edge sc_emark 1;
    ff.color := 2;
  };

  // sweep with increasing distance, adding further facets that are
  // edge neighbors as long as Euler sum is ok.
  do
  { changes := 0;
    current_distance := 1;
    for ( current_distance := 1 ; current_distance <= max_dist ; 
              current_distance += 1 )
    { 
      foreach edge ee where sc_emark > 0 and valence == 2 and 
          sc_edistance == current_distance do

// make it so only go UP in distance!

      { foreach ee.facet fff where fff.sc_fmark == -1
          and fff.sc_fdistance > current_distance do
        { unmarked_edges := sum(fff.edge, sc_emark < 0);
          unmarked_vertices := sum(fff.vertex, sc_vmark < 0);
          if unmarked_vertices - unmarked_edges + 1 == 0 then
          { // add facet
            fff.sc_fmark := 1;
            set fff.edges.sc_emark 1;
            set fff.vertices.sc_vmark 1;
            changes += 1;
            fff.color := (current_distance imod 14) + 1;
          }
        }

      };
printf "distance %d\n",current_distance;
    }

  } while changes;


set edge ee color white where valence==2 and 
 ee.facet[1].sc_fdistance == ee.facet[2].sc_fdistance
 and max(ee.facet, sc_fmark==0);

set edge ee color green where valence==2 and 
   abs(ee.facet[1].sc_fdistance-ee.facet[2].sc_fdistance)==2
     and max(ee.facet, sc_fmark==0);
} 

// end simply_connected.cmd

// Usage: mark_simply_connected
