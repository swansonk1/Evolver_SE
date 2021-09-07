// foamface.cmd

// Finding areas of full foam faces (rather than facets) in a foam.
// Also triple edge lengths between tetrahedral points.

// Programmer: Ken Brakke, brakke@susqu.edu www.susqu.edu/brakke
// Date: June 24, 2008

/* Usage:
   There are five useful commands:
   foamface_mark: finds contiguous faces, and marks the facets of
      each contiguous face with the facet attribute "fmark" value
      from 1 up to foamface_count.
   foamface_min: finds the minimum area contiguous face.  Runs
      foamface_mark itself, so you don't have to.
   foamedge_mark: finds contiguous triple edges, and marks the edges of
      each contiguous triple edge with the edge attribute "emark" value
      from 1 up to foamedge_count.
   foamedge_min: finds the minimum length contiguous triple edge.  Runs
      foamedge_mark itself, so you don't have to.
   foam_signature: finds number of polygons of each type on each body.
      When done, array body_poly_counts holds number of faces
      of given sides for each body.  After foam_signature,
      you can do 
         print body_poly_counts
      to see the results.  For example, for twointor.fe, the result is
         {{0,0,0,6,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
          {0,0,0,6,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
      Calls needed prerequisites, foamface_mark and foamedge_mark.
*/

// Attributes for marking faces and edges as we visit them.
define edge attribute emark integer
define facet attribute fmark integer

// Idea is to mark facets or edges in the same face or triple edge
// with the same fmark or emark number.

foamfacet_mark_done := 0  // detect whether marks done
foamedge_mark_done := 0  // detect whether marks done

procedure foamfacet_recur(integer f_id, integer face_num)
{
   facet[f_id].fmark := face_num;
   foreach facet[f_id].edge ee where valence == 2 do
   { foreach ee.facet ff where ff.fmark == 0 do
       foamfacet_recur(ff.id,face_num);
   }
} // foamfacet_recur

foamface_mark := {
   set facet fmark 0;
   foamface_count := 0;
   foreach facet ff where ff.fmark == 0 do
   { foamface_count += 1;
     foamfacet_recur(ff.id,foamface_count);
   };

   printf "foamface_mark found %d faces.\n",foamface_count;

   foamfacet_mark_done := 1;

} // end foamface_mark

// Find minimum area foam face
foamface_min := {
   local inx,face_areas,min_area,min_face;

   foamface_mark;
   define face_areas real[foamface_count];
   face_areas := 0;
   foreach facet ff do
     face_areas[ff.fmark] += ff.area;
   min_area := 1e30;
   min_face := 0;
   for ( inx := 1 ; inx <= foamface_count ; inx += 1)
     if face_areas[inx] < min_area then
     { min_area := face_areas[inx];
       min_face := inx;
     };
   printf "Minimum face area is %f for fmark == %d\n",min_area,min_face;

} // end foamface_min



// Now the same thing for triple edges.

procedure foamedge_recur(integer e_id, integer edge_num)
{
   edge[e_id].emark := edge_num;
   foreach edge[e_id].vertex vv where sum(vv.edge eee,eee.valence==3) == 2 do
   { foreach vv.edge ee where ee.valence==3 and ee.emark == 0 do
       foamedge_recur(ee.id,edge_num);
   }
} // end foamedge_recur()

foamedge_mark := {
   set edge emark 0;
   foamedge_count := 0;
   foreach edge ee where ee.valence==3 and ee.emark == 0 do
   { foamedge_count += 1;
     foamedge_recur(ee.id,foamedge_count);
   };

   printf "foamedge_mark found %d triple edges.\n",foamedge_count;

   foamedge_mark_done := 1;

}  // end foamedge_mark

// Find minimum length foam triple edge
foamedge_min := {
   local edge_lengths,min_length,min_edge;

   foamedge_mark;
   define edge_lengths real[foamedge_count];
   edge_lengths := 0;
   foreach edge ee where valence == 3 do
     edge_lengths[ee.emark] += ee.length;
   min_length := 1e30;
   min_edge := 0;
   for ( inx := 1 ; inx <= foamedge_count ; inx += 1)
     if edge_lengths[inx] < min_length then
     { min_length := edge_lengths[inx];
       min_edge := inx;
     };
   printf "Minimum triple edge length is %f for emark == %d\n",min_length,min_edge;

} // end foamedge_min


/* Finding polygon types and body signatures.
   Calls needed prerequisites.
   When done, array body_poly_counts holds number of faces
   of given sides for each body.  After foam_signature,
   you can do 
      print body_poly_counts
   to see the results.  For example, for twointor.fe, the result is
      {{0,0,0,6,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
       {0,0,0,6,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
*/ 
foam_signature := {
  local edge_counted,max_poly_sides,body_poly_counts,face_counted;
  local face_edge_counts;

  // mark macroscopic triple edges and faces
  foamface_mark;
  foamedge_mark;

  // Count edges on each face

  // initialize array
  define face_edge_counts integer[foamface_count]; 
  face_edge_counts := 0;
  define edge_counted integer[foamedge_count];
  edge_counted := 0;

  // count each triple edge once per face
  foreach edge ee where emark > 0 and edge_counted[ee.emark] == 0 do
  { foreach ee facet ff do
      face_edge_counts[ff.fmark] += 1;
    edge_counted[ee.emark] := 1;
  };


  // Now tally counts of polygon sides for each body
  max_poly_sides := 20;
  define body_poly_counts integer[body_count][max_poly_sides];
  body_poly_counts := 0;
  define face_counted integer[foamface_count];
  face_counted := 0;

  foreach facet ff where face_counted[ff.fmark] == 0 do
  { if ff.frontbody then
      body_poly_counts[ff.frontbody][face_edge_counts[ff.fmark]] += 1;
    if ff.backbody then
      body_poly_counts[ff.backbody][face_edge_counts[ff.fmark]] += 1;
    face_counted[ff.fmark] := 1;
  };

  printf "foam_signature results are in array body_poly_counts.\n";

} // end foam_signature

// End foamface.cmd

/* Usage:
     foamface_mark
     foamedge_mark
     foamface_min
     foamedge_min
     foam_signature
*/
