// percolate.cmd

/* Surface Evolver commands for percolation calculation on 2D foams.
   "Percolation" refers to the connected of subnets of a network, as
   in water percolating through porous stone.  Each edge of the network
   is deemed to be in an "on" or "off" state.  A set of nodes that
   can be connected together with "on" edges is called a "cluster".
   Thus a particular choice of "on" edges partitions the original 
   network into a set of disjoint clusters.  The interesting phenomenon
   is that when edges are set "on" at random with a particular
   probability, there is a critical probability above which most of
   the network falls in one big cluster.

   This file considers facets to be the "nodes".
   Works in both string and soapfilm models.  
   Works in string and soapfilm models.
   Only edges with two adjacent facets are considered.  In particular,
       triple edges in foams will not percolate.

   Contents:
   percolate - calculates clusters of facets, and numbers each
     facet with a cluster number in the attribute "cluster_number".
   slow_percolate - slower, alternate algorithm for checking.
   color_clusters - colors facets according to cluster number (mod 16),
      with internal cluster edges the same color as the cluster and 
      edges between clusters black.
   
   Usage:
   1. Set the "state" of each edge to 0 for "off" or 1 for "on", for example
          set edge state (random > 0.3).
   2. Do "percolate" or "slow_percolate".
   3. Do "color_clusters" to see the clusters. In the string model, you
      should do "show facets where 1" to see the facets.

   Programmer: Ken Brakke, brakke@susqu.edu  www.susqu.edu/brakke
*/

define edge attribute state integer  // link or not between adjacent facets
define facet attribute cluster_number integer
define facet attribute cluster_size integer

// It's up to you to set the "state" attribute to 0 or 1 for each edge.

color_clusters := {
  // coloring facets by cluster, with internal edges same color as
  // cluster facets, and black edges between clusters. 
  set facet ff color (ff.cluster_number imod 15) + 1; 
  set edge ee color (ee.facet[1].cluster_number imod 15) + 1;
  foreach edge ee where ee.valence == 2 and
     ee.facet[1].cluster_number != ee.facet[2].cluster_number do
  { set ee color black;
  };
}

// Slow method, for checking.  Makes repeated sweeps, propagating
// lowest facet id number as cluster number.
slow_percolate := {
  local changes;

  set facet cluster_number id;
  set facet cluster_size 0;
  do 
  { changes := 0;
    foreach edge ee where valence == 2 and  state do
    { if ee.facet[1].cluster_number < ee.facet[2].cluster_number then
      {  ee.facet[2].cluster_number := ee.facet[1].cluster_number;
         changes+=1;
      }
      else
      if ee.facet[2].cluster_number < ee.facet[1].cluster_number then
      {  ee.facet[1].cluster_number := ee.facet[2].cluster_number;
         changes+=1;
      };
    };
    printf "Changes: %d\n",changes;
  } while changes;
  
  // count cluster sizes
  foreach facet ff do
    facet[ff.cluster_number].cluster_size+=1;

  // report
  printf "Total number of clusters: %d \n",sum(facet,cluster_size > 0 );
  printf "Maximum cluster size: %d \n",max(facet,cluster_size); 


}
  
// Faster method
percolate := {
  local root,cluster_count,cluster_parent,parent,nonempty_clusters,largest_cluster;
  local inx,changes;

  define root integer[2];
  define cluster_count integer[facet_count];
  define cluster_parent integer[facet_count];
  cluster_count := 1;
  cluster_parent := 0;

  // initialize each facet to be single-node cluster
  // with cluster id numbers in ascending order
  inx := 1;
  foreach facet ff do { ff.cluster_number := inx; inx += 1; };

  // use edges to set links
  foreach edge ee where state and valence == 2 do
  { // get root clusters of each endpoint
    for ( inx := 1 ; inx <= 2 ; inx += 1 )
    { parent := ee.facet[inx].cluster_number;
      while ( cluster_parent[parent] ) do
        parent := cluster_parent[parent];
      if parent != ee.facet[inx].cluster_number then
        cluster_parent[ee.facet[inx].cluster_number] := parent;
      root[inx] := parent;
    };
    // set cluster of higher number root to lower 
    if root[1] < root[2] then
      cluster_parent[root[2]] := root[1]
    else if root[2] < root[1] then
      cluster_parent[root[1]] := root[2];
  };
      
  // sweep through facets, merging clusters to roots
  for ( inx := 1 ; inx <= facet_count ; inx += 1 )
  { if cluster_parent[inx] == 0 then continue;
    if cluster_parent[cluster_parent[inx]] then
      cluster_parent[inx] := cluster_parent[cluster_parent[inx]];
    cluster_count[cluster_parent[inx]] += cluster_count[inx];
    cluster_count[inx] := 0;
  };

  // set facet cluster numbers
  set facet cluster_number cluster_parent[cluster_number] 
     where cluster_parent[cluster_number] != 0;

  // tally results
  nonempty_clusters := 0;
  largest_cluster := 0;
  for ( inx := 1 ; inx <= facet_count ; inx += 1 )
  { if cluster_count[inx] == 0 then continue;
    nonempty_clusters += 1;
    if cluster_count[inx] > largest_cluster then
      largest_cluster := cluster_count[inx];
  };
  
  // report
  printf "Number of clusters: %10d\n",nonempty_clusters;
  printf "Largest cluster:    %10d\n",largest_cluster;
}
