// collada.cmd

// Surface Evolver script for creating Collada format file for surface.

// Collada format reference: http://www.khronos.org/files/collada_spec_1_4.pdf
// This particular format modelled after output from MeshLab.

/*
   Assumptions: 3D soapfilm model, linear model, not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Does facets only, not edges. 
   Does facet frontcolor and backcolor unless collada_front_and_back is 
     set to 0.  If collada_front_and_back is 1, then a second facet
     with reverse orientation is generated for each facet, but without
     any normal displacement, so in display programs you may have to 
     turn on backface culling to get a decent display.
*/

/* Remarks:
    In ColladaLoader, you must turn on back face culling when you 
    use collada_front_and_back, otherwise you see black backs.
*/

/* Usage:
          collada >>> "filename.dae"

*/

collada_front_and_back := 1;  // set to 0 if only want front surfaces

define rgb_cld integer[16][3];     // color definitions
rgb_cld[1][1] := 0; rgb_cld[1][2] := 0; rgb_cld[1][3] := 0;
rgb_cld[2][1] := 0; rgb_cld[2][2] := 0; rgb_cld[2][3] := 255;
rgb_cld[3][1] := 0; rgb_cld[3][2] := 255; rgb_cld[3][3] := 0;
rgb_cld[4][1] := 0; rgb_cld[4][2] := 255; rgb_cld[4][3] := 255;
rgb_cld[5][1] := 255; rgb_cld[5][2] := 0; rgb_cld[5][3] := 0;
rgb_cld[6][1] := 255; rgb_cld[6][2] := 0; rgb_cld[6][3] := 255;
rgb_cld[7][1] := 255; rgb_cld[7][2] := 127; rgb_cld[7][3] := 0;
rgb_cld[8][1] := 160; rgb_cld[8][2] := 160; rgb_cld[8][3] := 160;
rgb_cld[9][1] := 80; rgb_cld[9][2] := 80; rgb_cld[9][3] := 80;
rgb_cld[10][1] := 80; rgb_cld[10][2] := 200; rgb_cld[10][3] := 255;
rgb_cld[11][1] := 127; rgb_cld[11][2] := 255; rgb_cld[11][3] := 127;
rgb_cld[12][1] := 127; rgb_cld[12][2] := 255; rgb_cld[12][3] := 255;
rgb_cld[13][1] := 255; rgb_cld[13][2] := 127; rgb_cld[13][3] := 127;
rgb_cld[14][1] := 255; rgb_cld[13][2] := 127; rgb_cld[13][3] := 255;
rgb_cld[15][1] := 255; rgb_cld[15][2] := 255; rgb_cld[15][3] := 0;
rgb_cld[16][1] := 255; rgb_cld[16][2] := 255; rgb_cld[16][3] := 255;

define vertex attribute collada_vnum integer  // in case of nonconsecutive numbering
define facet  attribute collada_fnum integer  // in case of nonconsecutive numbering


collada := {
  local facet_color_counts,collada_total_facets,cinx,vnumber,stupid_collada_fudge,fnumber;

  define facet_color_counts integer[16];

  if torus then
  { errprintf "Cannot run 'collada' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'collada' command in symmetry group mode. Do 'detorus' before.\n";
    errprintf "  WARNING: 'detorus' alters the surface, so save it first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'collada' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'collada' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'collada' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'collada' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  // count number of facets of each color
  facet_color_counts := 0;
  foreach facet ff where show do
  { if ff.color >= 0 then facet_color_counts[ff.color+1] += 1;
    if collada_front_and_back and ff.backcolor >= 0 then 
       facet_color_counts[ff.backcolor+1] += 1;
  };
  collada_total_facets := 0;
  for ( cinx := 1 ; cinx <= 16 ; cinx += 1 )
    collada_total_facets += facet_color_counts[cinx]; 

  //  Generate file
  printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  printf "<COLLADA xmlns=\"http://www.collada.org/2005/11/COLLADASchema\" version=\"1.4.1\">\n";
  printf "  <asset>\n";
  printf "    <contributor>\n";
  printf "      <author>Surface Evolver</author>\n";
  printf "      <authoring_tool>Surface Evolver collada.cmd script</authoring_tool>\n";
  printf "    </contributor>\n";
  printf "    <created>%s</created>\n",date_and_time;
  printf "    <modified>%s</modified>\n",date_and_time;
  printf "    <title>%s</title>\n",datafilename;
  printf "    <up_axis>Z_UP</up_axis>\n";
  printf "  </asset>\n";
  printf "  <library_effects>\n";
  for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
  { if facet_color_counts[cinx+1] == 0 then continue;
    printf "    <effect id=\"colorid%d\">\n",cinx;
    printf "      <profile_COMMON>\n";
    printf "      <technique sid=\"flatcolor%d\">\n",cinx;
    printf "        <constant>\n";
    printf "          <diffuse>\n";
    printf "            <color>%g %g %g 1.0</color>\n",rgb_cld[cinx+1][1]/255,
                            rgb_cld[cinx+1][2]/255,rgb_cld[cinx+1][3]/255;
    printf "          </diffuse>\n";
    printf "        </constant>\n";
    printf "      </technique>\n";
    printf "      </profile_COMMON>\n";
    printf "    </effect>\n";
  };
  printf "    </library_effects>\n";
  printf "    <library_materials>\n";
  for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
  { if facet_color_counts[cinx+1] == 0 then continue;
    printf "     <material id=\"color%dMaterial\">\n",cinx;
    printf "       <instance_effect url=\"#colorid%d\"/>\n",cinx;
    printf "     </material>\n";
  };
  printf "    </library_materials>\n";
  printf "    <library_geometries>\n";
  printf "      <geometry id=\"shape0-lib\" name=\"shape0\">\n";
  printf "        <mesh>\n";

  printf "          <source id=\"shape0-lib-positions\" name=\"position\">\n";
  printf "            <float_array id=\"shape0-lib-positions-array\" count=\"%d\">\n",3*vertex_count;
  vnumber := 0;
// Note: Collada wants space separators, even with newlines, but not before first or after last.
  stupid_collada_fudge := 0;
  foreach vertex vv do 
  { if stupid_collada_fudge then printf " \n";
    printf "%g %g %g",vv.x,vv.y,vv.z;
    vv.collada_vnum := vnumber;
    vnumber += 1;
    stupid_collada_fudge := 1;
  };
  printf "</float_array>\n";
  printf "            <technique_common>\n";
  printf "              <accessor count=\"%d\" source=\"#shape0-lib-positions-array\" stride=\"3\">\n",vertex_count;
  printf "                <param name=\"X\" type=\"float\"></param>\n";
  printf "                <param name=\"Y\" type=\"float\"></param>\n";
  printf "                <param name=\"Z\" type=\"float\"></param>\n";
  printf "              </accessor>\n";
  printf "            </technique_common>\n";
  printf "          </source>\n";

  printf "          <source id=\"shape0-normal\" name=\"normal\">\n";
  printf "            <float_array id=\"shape0-normal-array\" count=\"%d\">\n",
                          sum(facet where show,1)*3*(collada_front_and_back ? 2 : 1);
  fnumber := 0;
// Note: Collada wants space separators, even with newlines, but not before first or after last.
  stupid_collada_fudge := 0;
  foreach facet ff where show and color >= 0 do 
  { if stupid_collada_fudge then printf " \n";
    ff.collada_fnum := fnumber;
    printf "%g %g %g",ff.x,ff.y,ff.z;
    if collada_front_and_back then 
    { printf " %g %g %g",-ff.x,-ff.y,-ff.z;
      fnumber += 1;
    };
    fnumber += 1;
    stupid_collada_fudge := 1;
  };
  printf "</float_array>\n";
  printf "            <technique_common>\n";
  printf "              <accessor count=\"%d\" source=\"#shape0-normal-array\" stride=\"3\">\n",
    sum(facet where show and color >= 0,1)*(collada_front_and_back ? 2 : 1);
  printf "                 <param name=\"X\" type=\"float\"></param>\n";
  printf "                 <param name=\"Y\" type=\"float\"></param>\n";
  printf "                 <param name=\"Z\" type=\"float\"></param>\n";
  printf "              </accessor>\n";
  printf "            </technique_common>\n";
  printf "          </source>\n";

  printf "          <vertices id=\"shape0-lib-vertices\">\n";
  printf "            <input semantic=\"POSITION\" source=\"#shape0-lib-positions\"></input>\n";
  printf "          </vertices>\n";

  for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
  { if facet_color_counts[cinx+1] == 0 then continue;
    printf "        <triangles count=\"%d\" material=\"COLOR%d\">\n",
         facet_color_counts[cinx+1],cinx;
    printf "          <input offset=\"0\" semantic=\"VERTEX\" source=\"#shape0-lib-vertices\"></input>\n";
    printf "          <input offset=\"1\" semantic=\"NORMAL\" source=\"#shape0-normal\"></input>\n";
    printf "          <p>\n";
    stupid_collada_fudge := 0;
    foreach facet ff where show and color == cinx do
    { if stupid_collada_fudge then printf " \n";
      printf "%d %d %d %d %d %d",ff.vertex[1].collada_vnum,ff.collada_fnum,
         ff.vertex[2].collada_vnum,ff.collada_fnum,ff.vertex[3].collada_vnum,
           ff.collada_fnum;
      stupid_collada_fudge :=1;
    };
    if collada_front_and_back then
      foreach facet ff where show and backcolor == cinx do
      { if stupid_collada_fudge then printf " \n";
        printf "%d %d %d %d %d %d",ff.vertex[1].collada_vnum,ff.collada_fnum+1,
           ff.vertex[3].collada_vnum,ff.collada_fnum+1,
           ff.vertex[2].collada_vnum,ff.collada_fnum+1;
        stupid_collada_fudge :=1;
      };
    printf "</p>\n";
    printf "        </triangles>\n";
  };
  printf "        </mesh>\n";
  printf "      </geometry>\n";
  printf "    </library_geometries>\n";
  printf "    <library_visual_scenes>\n";
  printf "      <visual_scene id=\"VisualSceneNode\" name=\"VisualScene\">\n";
  printf "        <node id=\"node\" name=\"node\">\n";
  printf "          <instance_geometry url=\"#shape0-lib\">\n";
  printf "            <bind_material>\n";
  printf "              <technique_common>\n";
  for ( cinx := 0 ; cinx <= 15 ; cinx += 1 )
  { if facet_color_counts[cinx+1] == 0 then continue;
    printf "              <instance_material symbol=\"COLOR%d\" target=\"#color%dMaterial\"/>\n",
          cinx,cinx;
  };
  printf "              </technique_common>\n";
  printf "            </bind_material>\n";
  printf "          </instance_geometry>\n";
  printf "        </node>\n";
  printf "      </visual_scene>\n";
  printf "    </library_visual_scenes>\n";
  printf "    <scene>\n";
  printf "        <instance_visual_scene url=\"#VisualSceneNode\"></instance_visual_scene>\n";
  printf "    </scene>\n";
  printf "</COLLADA>\n";
}

// End collada.cmd
/* Usage:
          collada >>> "filename.dae"
*/
