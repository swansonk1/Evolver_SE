// ansys.cmd
// Surface Evolver command to produce file of ANSYS input for 
//   vertices, edges,  and faces to produce a surface
//   for ANSYS meshing.  Beware this is a very simple-minded
//   translation to ANSYS format.
// Modified Feb. 2006 to not assume consecutive numbering of elements.
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

/* Assumptions and limitations:
   3D soapfilm model.
   Linear or quadratic model.
   No torus or symmetry group.
   Does not do color.
   Does all bodies as individual ANSYS volume.
   Does edges and facets according to "show" status, but you want to
     make sure you are showing all the body facets you need.

   usage:  ansys >>> "filename"
*/

// Attributes to hold ANSYS numbering of elements
define vertex attribute ansys_knumber real
define edge attribute ansys_lnumber real
define facet attribute ansys_alnumber real

// vertices as ANSYS keypoints
ansys_nodes := { 
       local knumber;
       knumber := 0;
       foreach vertex vv do
       { printf "k,,%20.15g,%20.15g,%20.15g\n",x,y,z;
         knumber += 1;
         vv.ansys_knumber := knumber;
       }
     }

ansys_edges := {
       local lnumber;
       lnumber := 0;
       if (quadratic) then 
       foreach edge ee where show do
       { printf "larc,%g,%g,%g\n",ee.vertex[1].ansys_knumber,
           ee.vertex[2].ansys_knumber,ee.vertex[3].ansys_knumber;
         lnumber += 1;
         ee.ansys_lnumber := lnumber;
       }
       else
       foreach edge ee where show do
       { printf "l,%g,%g\n",ee.vertex[1].ansys_knumber,
             ee.vertex[2].ansys_knumber;
         lnumber += 1;
         ee.ansys_lnumber := lnumber;
       }
     }

ansys_faces := {
       local alnumber;
       alnumber := 0;
       foreach facet ff where show do
       { printf "al,%g,%g,%g\n",
           ff.edge[1].ansys_lnumber,ff.edge[2].ansys_lnumber,
             ff.edge[3].ansys_lnumber;
         alnumber += 1;
         ff.ansys_alnumber := alnumber; 
       }
 }

// define volumes, one per body
ansys_bodies := { foreach body bb do
     { // select areas
       local flag;
       flag := 0;
       foreach bb.facet ff where show do
       { if flag then printf "ASEL,A,AREA,,%g,%g\n",ff.ansys_alnumber,
           ff.ansys_alnumber
         else printf "ASEL,S,AREA,,%g,%g\n",ff.ansys_alnumber,
           ff.ansys_alnumber;
         flag := 1;
       };
       printf "VA,ALL\n";
     }
  }

// The main command
ansys := { 

  // Check assumptions.
  if torus then
  { errprintf "Cannot run 'ansys' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'ansys' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'ansys' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'ansys' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'ansys' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'ansys' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };


  printf "/PREP7\n";
            printf "/NOPR\n";
            ansys_nodes;
            ansys_edges;
            ansys_faces;
            ansys_bodies;
        }

// End ansys.cmd
// Usage: ansys >>> "ansys_file.ans"



