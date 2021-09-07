// cube_views.cmd

// Standard transforms and viewing angles using the transform
// generators in cube_transforms.inc.  Meant to be read in at
// the bottom of datafile.

// Various sets of transforms are defined according to the symmetry
// group of the fundamental region.  The datafile should define 
// the variable cube_symmetry_type to be one of these values,
// defined in cube_transforms.inc:

if not is_defined("cube_symmetry_type") then
{ errprintf "ERROR: cube_views.cmd requires the variable cube_symmetry_type to be defined.\n";
  errprintf "       See cube_transforms.inc for values.\n";
  abort;
}

if not is_defined("quadri_full") then
{ errprintf "ERROR: The file 'cube_transforms.inc' was not included in the top of the datafile.\n";
  abort;
}

// default assignments
pair := { errprintf"This transform not implemented for this symmetry type.";};
tetra := { errprintf"This transform not implemented for this symmetry type.";};
cubelet := { errprintf"This transform not implemented for this symmetry type.";};  /* positive orthant */
cube := { errprintf"This transform not implemented for this symmetry type.";};
rhombic := { errprintf"This transform not implemented for this symmetry type.";};
cube2 := { errprintf"This transform not implemented for this symmetry type.";};
cube4 := { errprintf"This transform not implemented for this symmetry type.";};
cube8 := { errprintf"This transform not implemented for this symmetry type.";};
unitcell := { errprintf"This transform not implemented for this symmetry type.";};
four := {errprintf"This transform not implemented for this symmetry type.";};

if cube_symmetry_type == quadri_full  then
{ pair := { transform_expr "" };
  tetra := { transform_expr "" };
  cubelet := { transform_expr "lal" };  /* positive orthant */
  cube := { transform_expr "cdelal" };
  othercube := { transform_expr "jhflal" };
  rhombic := { transform_expr "cdelalj" };
  cube2 := { transform_expr "fcdelal" };
  cube4 := { transform_expr "gfcdelal" };
  cube8 := { transform_expr "hgfcdelal" };
  unitcell := { cube };
} else if cube_symmetry_type == quadri_up_half  then
{ pair := { transform_expr "b" };
  tetra := { transform_expr "b" };
  cubelet := { transform_expr "ababab" };  /* positive orthant */
  cube := { transform_expr "cdeababab" };
  rhombic := { transform_expr "cdeabababjb" };
  cube2 := { transform_expr "fcdeababab" };
  cube4 := { transform_expr "gfcdeababab" };
  cube8 := { transform_expr "hgfcdeababab" };
  unitcell := { cube };
  four := {transform_expr "jhedcababab"};
} else if cube_symmetry_type == quadri_down_half  then
{} else if cube_symmetry_type == trirect_full  then
{} else if cube_symmetry_type == disphenoid_full then
{} else if cube_symmetry_type == disphenoid_short then
{} else if cube_symmetry_type == disphenoid_long  then
{} else if cube_symmetry_type == disphenoid_short_long then
{ pair := { transform_expr "b" };
  tetra := { transform_expr "bm" };
  cubelet := { transform_expr "blblbl" };  /* positive orthant */
  cube := { transform_expr "noblblbl" };
  rhombic := { transform_expr "noblblblm" };
  cube2 := { transform_expr "fcdeababab" };
  cube4 := { transform_expr "gfcdeababab" };
  cube8 := { transform_expr "hgfcdeababab" };
  unitcell := { rhombic };
} else if cube_symmetry_type == disphenoid_two_long then
{} else if cube_symmetry_type == disphenoid_three then
{} else if cube_symmetry_type == cube_sym then
{ pair := { transform_expr "" };
  cubelet := { transform_expr "" };  /* positive orthant */
  cube := { transform_expr "" };
  rhombic := { transform_expr "" };
  cube2 := { transform_expr "h" };
  cube4 := { transform_expr "jh" };
  cube8 := { transform_expr "jhf" };
  unitcell := { cube };
} else if cube_symmetry_type == cubelet_sym then
{ pair := { transform_expr "" };
  cubelet := { transform_expr "" };  /* positive orthant */
  cube := { transform_expr "cde" };
  rhombic := { transform_expr "" };
  cube2 := { transform_expr "hcde" };
  cube4 := { transform_expr "jhcde" };
  cube8 := { transform_expr "jhfcde" };
  unitcell := { cube };
}
 
ps_fixededgewidth := 0.0015
ps_conedgewidth   := 0.0015
ps_bareedgewidth  := 0.002

// Marking some edge types after multiplicate, which preserved original types.  
mark_edges := { 
   set edge ee edgetype cubeedge where ee.bare and ((min(ee.vertex,x) > .99)
       + (min(ee.vertex,y) > .99) + (min(ee.vertex,z) > .99) >= 2 );
   set edge ee edgetype cubeletedge where ee.bare and  (edgetype!=cubeedge)
    and  ((abs(ee.x) < .01) + (abs(ee.y) < .01) + (abs(ee.z) < .01) >= 2);
   set edge ee edgetype mirroredge where ee.valence == 1;
}

// Commands to create datafiles for larger pieces
quiet on;
read "multiplicate.cmd"
quiet off;
basename := datafilename;  // user can change this before running these.

make_tetra := {
  tetra;
  outfilename := sprintf "%s_tetra.fe",basename;
  multiplicate >>> outfilename;
  printf "\nread\n" >> outfilename;
  if cube_symmetry_type == quadri_up_half or  
          cube_symmetry_type == quadri_down_half then
  { printf "cube_symmetry_type := quadri_full;\n" >> outfilename;}
  else if cube_symmetry_type == disphenoid_short or
          cube_symmetry_type == disphenoid_short_long or
          cube_symmetry_type == disphenoid_three or
            cube_symmetry_type == disphenoid_two_long then
  { printf "cube_symmetry_type := quadri_full;\n" >> outfilename;}
  else 
  { printf "cube_symmetry_type := %d;\n",cube_symmetry_type >> outfilename;};

  printf "read \"cube_views.cmd\";\n" >> outfilename;
  printf "mark_edges\n" >> outfilename;
  printf "basename := \"%s\"\n",basename >> outfilename;
}

make_cubelet := {
  cubelet;
  outfilename := sprintf "%s_cubelet.fe",basename;
  multiplicate >>> outfilename;
  printf "\nread\n" >> outfilename;
  printf "cube_symmetry_type := cubelet_sym;\n" >> outfilename;
  printf "read \"cube_views.cmd\";\n" >> outfilename;
  printf "mark_edges\n" >> outfilename;
  printf "basename := \"%s\"\n",basename >> outfilename;
}


make_cube := {
  cube;
  outfilename := sprintf "%s_cube.fe",basename;
  multiplicate >>> outfilename;
  printf "\nread\n" >> outfilename;
  printf "cube_symmetry_type := cube_sym;\n" >> outfilename;
  printf "read \"cube_views.cmd\";\n" >> outfilename;
  printf "mark_edges\n" >> outfilename;
  printf "basename := \"%s\"\n",basename >> outfilename;
}

// Standard views, originally made with saveview.cmd

cube_view := {
oldautodisplay := (autodisplay);
autodisplay off;
view_matrix[1][1] := 0.971846;
view_matrix[1][2] := 0.315278;
view_matrix[1][3] := 0.218986;
view_matrix[1][4] := 0.000000;
view_matrix[2][1] := -0.317500;
view_matrix[2][2] := 0.995222;
view_matrix[2][3] := -0.023792;
view_matrix[2][4] := 0.000000;
view_matrix[3][1] := -0.215751;
view_matrix[3][2] := -0.044412;
view_matrix[3][3] := 1.021430;
view_matrix[3][4] := 0.000000;
view_matrix[4][1] := 0.000000;
view_matrix[4][2] := 0.000000;
view_matrix[4][3] := 0.000000;
view_matrix[4][4] := 1.000000;
if oldautodisplay then autodisplay on;
}

cubelet_view := {
oldautodisplay := (autodisplay);
autodisplay off;
view_matrix[1][1] := 2.001325;
view_matrix[1][2] := 0.649253;
view_matrix[1][3] := 0.450958;
view_matrix[1][4] := 0.000000;
view_matrix[2][1] := -0.653829;
view_matrix[2][2] := 2.049463;
view_matrix[2][3] := -0.048995;
view_matrix[2][4] := -0.672000;
view_matrix[3][1] := -0.444297;
view_matrix[3][2] := -0.091458;
view_matrix[3][3] := 2.103434;
view_matrix[3][4] := -0.833000;
view_matrix[4][1] := 0.000000;
view_matrix[4][2] := 0.000000;
view_matrix[4][3] := 0.000000;
view_matrix[4][4] := 1.000000;
if oldautodisplay then autodisplay on;
}

// commands for viewing certain configurations
wire_cube := { 
        show_expr edges where (edgetype == cubeedge) or 
           (not bare and edgetype != gridedge);
        cube_view;
        cube;
      }

nowire_cube := {
      show_expr edges where not bare and (edgetype != gridedge);
      cube_view;
      cube;
     }

wire_cubelet := { 
        show_expr edges where (edgetype == cubeedge) or
          (valence == 1 ) or (edgetype==cubeletedge);
        cubelet_view;
        cubelet;
      }

nowire_cubelet := {
      show_expr edges where (valence==1);
      cubelet_view;
      cubelet;
     }

wire_tetra := { show_expr edges where edgetype != gridedge and
                    edgetype != outlineedge;
                tetra;
               }
nowire_tetra := { show_expr edges where (edgetype==c2edge) or 
                       (valence == 1);
                  tetra;
                }

// Make standard images.
quadri_image := { gridflag off; pscolorflag on; labelflag off;
    wire_tetra; read "quadri.view"; show_expr edges;
    set facet backcolor yellow;
    postscript sprintf "%s_quadri.eps",basename;
}
wire_cube_image := { gridflag off; pscolorflag on; labelflag off;
    wire_cube; read "cube.view";  set facet backcolor yellow;
    postscript sprintf "%s_wirecube.eps",basename;
}
nowire_cube_image := { gridflag off; pscolorflag on; labelflag off;
    nowire_cube; read "cube.view"; set facet backcolor yellow;
    postscript sprintf "%s_nowirecube.eps",basename;
}
cube2_image := { gridflag off; pscolorflag on; labelflag off;
    nowire_cube; cube2; read "twocube.view"; set facet backcolor yellow;
    postscript sprintf "%s_cube2.eps",basename;
}
cube4_image := { gridflag off; pscolorflag on; labelflag off;
    nowire_cube; cube4; read "fourcube.view"; set facet backcolor yellow;
    postscript sprintf "%s_cube4.eps",basename;
}
cube8_image := { gridflag off; pscolorflag on; labelflag off;
    nowire_cube; cube8; read "eightcube.view"; set facet backcolor yellow;
    postscript sprintf "%s_cube8.eps",basename;
}

// create outline tetrahedron

create_outline := {
  if cube_symmetry_type == quadri_full then
  {
    va := new_vertex(0,0,0); fix vertex[va];
    vb := new_vertex(1,0,0); fix vertex[vb];
    vc := new_vertex(1,0,1); fix vertex[vb];
    vd := new_vertex(1,1,1); fix vertex[vb];
    newe := new_edge(va,vb); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
    newe := new_edge(va,vc); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
    newe := new_edge(va,vd); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
    newe := new_edge(vb,vc); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
    newe := new_edge(vb,vd); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
    newe := new_edge(vc,vd); set edge[newe] no_refine; set edge[newe] bare; set edge[newe] fixed;
  }
  else
  { errprintf "create_outline not implemented for this type cell yet.\n";
    return;
  };
}

