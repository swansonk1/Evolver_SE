// reorder.cmd

// Surface Evolver command script illustrating how to re-order
// element storage in memory, to test the effect of memory
// storage on execution speed.  Also, in preparation for 
// using the "renumber_all" command to re-number elements.

// This example reorders vertices in diagonal order, by their
// distance along the (1,1,1) direction.  Other elements are
// reordered according to their vertices.

// Programmer: Ken Brakke, brakke@susqu.edu  www.susqu.edu/brakke

// Usage: reorder

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

// End reorder.cmd

// Usage: reorder




