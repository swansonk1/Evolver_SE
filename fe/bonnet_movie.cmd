// bonnet_movie.cmd

// Makes in-memory movie of Bonnet rotation of minimal surface,
// one frame per degree for 360 degrees.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: Evolve initial minimal surface, remove all level-set constraints
//        and boundaries, get nice view in graphics window.
//        Run "make_movie" to calculate Bonnet rotations.
//        Run "show_movie" to see screen display of Bonnet rotation.
//        Run "movie" to see endlessly repeating Bonnet rotation.
//        Run "postscript_movie" to create sequence of PostScript files.

read "adjoint.cmd"

frame_delay := 0.03  // seconds between frames

// Store coordinates for complete Bonnet rotation
define vertex attribute bonnet real[360][3]
movie_made_flag := 0;
make_movie := {
  local midx,midy,midz;

  // Check assumptions
if torus then
  { errprintf "Cannot run 'make_movie' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'make_movie' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'make_movie' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'make_movie' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'make_movie' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'make_movie' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };


  // Center of original surface.
  midx := avg(vertex,x);
  midy := avg(vertex,y);
  midz := avg(vertex,z);

  quiet on;
  for ( bangle := 0 ; bangle < 359.5 ; bangle += 1 )
  { local bmidx,bmidy,bmidz; // Center of rotated surface
    adjoint;
    bmidx := avg(vertex,x);
    bmidy := avg(vertex,y);
    bmidz := avg(vertex,z);
   
    foreach vertex vv do
    { vv.bonnet[bangle+1][1] := vv.x-bmidx+midx;
      vv.bonnet[bangle+1][2] := vv.y-bmidy+midx;
      vv.bonnet[bangle+1][3] := vv.z-bmidz+midx;
    };
    flip;
  };
  movie_made_flag := 1;
  quiet off;
}

procedure set_bonnet(integer angle)
{ 
  if !movie_made_flag then
  { errprintf "Error.  Run \"make_movie\" before set_bonnet.\n";
    abort;
  };

   foreach vertex vv do
     vv.__x := vv.bonnet[angle+1];

   recalc;
}

show_movie := {
  local next_time;
  next_time := clock;
  for ( angle := 0; angle < 359.5 ; angle += 1 )
  { while clock < next_time do {};
    next_time += frame_delay;
    set_bonnet(angle);
  } 
}

// write postscript file for each frame
postscript_movie := {
  local angle;
  full_bounding_box on;
  for ( angle := 0; angle < 359.5 ; angle += 1 )
  {
    set_bonnet(angle);
    postscript sprintf"%s.%03d",datafilename,angle;
  } 
}
  
// continuous loop
movie := { for (;;) show_movie; }


// Write split-vertex "nonconforming" adjoint datafile.  To be done after conconj;
// uses ad_enewx from "adjoint" to compute vertices.
define facet attribute split_bonnet real[3][360][3]
write_split_movie := {
  printf "// split-vertex discrete adjoint of %s, rotation angle %f.\n",
    datafilename,bangle;
  printf "\ndefine vertex attribute bonnet real[360][3]\n\n";
  printf "\nvertices\n";
  foreach facet ff do
  { printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id-2,
      ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]-ff.edge[3].ad_enewx[1],
      ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]-ff.edge[3].ad_enewx[2],
      ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]-ff.edge[3].ad_enewx[3];
    print ff.split_bonnet[1];
    printf "\n";
    printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id-1,
     -ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1],
     -ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2],
     -ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
    print ff.split_bonnet[2];
    printf "\n";
    printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id,
      ff.edge[1].ad_enewx[1]-ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1],
      ff.edge[1].ad_enewx[2]-ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2],
      ff.edge[1].ad_enewx[3]-ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
    print ff.split_bonnet[3];
    printf "\n";
  };
  printf "\nedges\n";
  foreach facet ff do
  { printf "%d   %d %d\n",3*ff.id-2,3*ff.id-2,3*ff.id-1;
    printf "%d   %d %d\n",3*ff.id-1,3*ff.id-1,3*ff.id;
    printf "%d   %d %d\n",3*ff.id,3*ff.id,3*ff.id-2;
  };
  printf "\nfaces\n";
  foreach facet ff do 
    printf "%d    %d %d %d\n",ff.id,3*ff.id-2,3*ff.id-1,3*ff.id;
  printf "\nread\n";
  printf "read \"bonnet_movie.cmd\"\n";
}

make_split_movie := {
  local midx,midy,midz;
  midx := avg(edge,ad_enewx[1]);
  midy := avg(edge,ad_enewx[2]);
  midz := avg(edge,ad_enewx[3]);
  quiet on;
  for ( bangle := 0 ; bangle < 359.5 ; bangle += 1 )
  { adjoint;
   
    foreach edge ee  do
    { ee.ad_enewx[1] -= midx;
      ee.ad_enewx[2] -= midy;
      ee.ad_enewx[3] -= midz;
    };
    foreach facet ff do
    { 
      ff.split_bonnet[1][bangle+1][1] :=
        ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]-ff.edge[3].ad_enewx[1];
      ff.split_bonnet[1][bangle+1][2] :=
        ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]-ff.edge[3].ad_enewx[2];
      ff.split_bonnet[1][bangle+1][3] :=
        ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]-ff.edge[3].ad_enewx[3];
      ff.split_bonnet[2][bangle+1][1] :=
       -ff.edge[1].ad_enewx[1]+ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1];
      ff.split_bonnet[2][bangle+1][2] :=
       -ff.edge[1].ad_enewx[2]+ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2];
      ff.split_bonnet[2][bangle+1][3] :=
       -ff.edge[1].ad_enewx[3]+ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
      ff.split_bonnet[3][bangle+1][1] :=
        ff.edge[1].ad_enewx[1]-ff.edge[2].ad_enewx[1]+ff.edge[3].ad_enewx[1];
      ff.split_bonnet[3][bangle+1][2] :=
        ff.edge[1].ad_enewx[2]-ff.edge[2].ad_enewx[2]+ff.edge[3].ad_enewx[2];
      ff.split_bonnet[3][bangle+1][3] :=
        ff.edge[1].ad_enewx[3]-ff.edge[2].ad_enewx[3]+ff.edge[3].ad_enewx[3];
    };
    flip;
  };
  quiet off;
}


// End bonnet_movie.cmd

// Usage: Evolve initial minimal surface, remove all level-set constraints
//        and boundaries, get nice view in graphics window.
//        Run "make_movie" to calculate Bonnet rotations.
//        Run "show_movie" to see screen display of Bonnet rotation.
//        Run "movie" to see endlessly repeating Bonnet rotation.
//        Run "postscript_movie" to create sequence of PostScript files.
