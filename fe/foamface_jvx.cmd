// foamface_jvx.cmd

// Produces JavaView jvx file with foam cell faces as single jvx faces.

// Usage: foamface_jvx >>> "filename.jvx"

// WARNING!!!! Due to use of "detorus", foamface_jvx will alter the
// surface irreversibly, so save your surface first!!

// Programmer: Ken Brakke, brakke@susqu.edu, www.susqu.edu/brakke

jvx_transparency := 0.3   // Change this if you desire.

define facet attribute fmark integer;   // for foamface_mark, number of face
define vertex attribute jvx_number integer;    // for implicit vertex order
define facet attribute fjvxnum integer;    // for implicit facet order
define edge attribute edge_kind integer;  // for marking triple edges,
                                          // so know after detorus.
triple_edge := 1;  // for edge_kind

define facetcolorcount integer[16];  // to see which color facets are present
define edgecolorcount integer[16];  // to see which color edges are present
define rgb_jvx integer[16][3];     // color definitions
rgb_jvx[1][1] := 0; rgb_jvx[1][2] := 0; rgb_jvx[1][3] := 0;
rgb_jvx[2][1] := 0; rgb_jvx[2][2] := 0; rgb_jvx[2][3] := 255;
rgb_jvx[3][1] := 0; rgb_jvx[3][2] := 255; rgb_jvx[3][3] := 0;
rgb_jvx[4][1] := 0; rgb_jvx[4][2] := 255; rgb_jvx[4][3] := 255;
rgb_jvx[5][1] := 255; rgb_jvx[5][2] := 0; rgb_jvx[5][3] := 0;
rgb_jvx[6][1] := 255; rgb_jvx[6][2] := 0; rgb_jvx[6][3] := 255;
rgb_jvx[7][1] := 255; rgb_jvx[7][2] := 127; rgb_jvx[7][3] := 0;
rgb_jvx[8][1] := 160; rgb_jvx[8][2] := 160; rgb_jvx[8][3] := 160;
rgb_jvx[9][1] := 80; rgb_jvx[9][2] := 80; rgb_jvx[9][3] := 80;
rgb_jvx[10][1] := 80; rgb_jvx[10][2] := 200; rgb_jvx[10][3] := 255;
rgb_jvx[11][1] := 127; rgb_jvx[11][2] := 255; rgb_jvx[11][3] := 127;
rgb_jvx[12][1] := 127; rgb_jvx[12][2] := 255; rgb_jvx[12][3] := 255;
rgb_jvx[13][1] := 255; rgb_jvx[13][2] := 127; rgb_jvx[13][3] := 127;
rgb_jvx[14][1] := 255; rgb_jvx[13][2] := 127; rgb_jvx[13][3] := 255;
rgb_jvx[15][1] := 255; rgb_jvx[15][2] := 255; rgb_jvx[15][3] := 0;
rgb_jvx[16][1] := 255; rgb_jvx[16][2] := 255; rgb_jvx[16][3] := 255;

// The following routines are adapted from foamface.cmd
// to find polygon faces AFTER detorus, using edge_kind.
procedure foamfacet_recur(integer f_id, integer face_num)
{
   facet[f_id].fmark := face_num;
   foreach facet[f_id].edge ee where edge_kind != triple_edge do
   { foreach ee.facet ff where ff.fmark == 0 do
       foamfacet_recur(ff.id,face_num);
   }
}

foamface_mark := {
   set facet fmark 0;
   foamface_count := 0;
   foreach facet ff where ff.fmark == 0 do
   { foamface_count += 1;
     foamfacet_recur(ff.id,foamface_count);
   };

}

foamface_jvx := { 
  local maxx,minx,maxy,miny,maxz,minz,jvxnum,facetnum;
  local colornum,numcolors;

  // Check assumptions.

  if space_dimension != 3 then
  { errprintf "The 'foamface_jvx' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'foamface_jvx' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'foamface_jvx' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'foamface_jvx' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'foamface_jvx' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };


  // Maybe problem with multiple faces with same fmark?

  // unwrap foam
  set edge edge_kind triple_edge where valence >= 3;
  connected;
  detorus;
  // sets fmark on each facet to face number.
  foamface_mark;  

  // Header
  printf "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>\n";
  printf "<jvx-model>\n";
  printf "<meta generator=\"Surface Evolver 2.40; foamface_jvx.cmd\"/>\n";
  printf "<title> %s </title>\n",datafilename;
  printf "<version>1.0</version>\n";
  printf "<geometries>\n";
  printf "  <geometry name=\"surface1\">\n";

  // Vertices needed
  printf "    <pointSet dim=\"3\" point=\"hide\">\n";
  printf "      <points num=\"%d\">\n",sum(vertex vv,
        max(vv.edge,edge_kind==triple_edge) > 0);
  set vertex jvx_number -1;
  jvxnum := 0;
  foreach vertex vv where max(vv.edge,edge_kind==triple_edge) >= 1 do
  { printf "          <p> %f %f %f </p>\n",vv.x,vv.y,vv.z; 
    vv.jvx_number := jvxnum;
    jvxnum += 1;
  };
  printf "        <thickness>2.0</thickness>\n";
  printf "      </points>\n";
  printf "    </pointSet>\n";

  // Facets.  On each triple edge, pick a face and follow around,
  // marking used facets.
  set facet fjvxnum 0;
  printf "<faceSet edge=\"show\" color=\"show\"  face=\"show\"\n";
  printf "    colorBackLocal=\"show\" \n";
  printf "    colorBackGlobal=\"hide\"  \n";
  printf "    colorEdge=\"hide\"\n"; 
  printf "    colorEdgeInduced=\"hide\"  \n";
  printf "    normal=\"hide\"  \n";
  printf "    normalArrow=\"hide\"\n";
  printf "    texture=\"hide\"  \n";
  printf "    backface=\"show\" \n";
  printf "    boundary=\"hide\"\n";
  printf "    faceMark=\"show\"  \n";
  printf "    silhouette=\"hide\" \n";
  printf "   > \n";
  printf "  <faces num=\"%d\">\n",max(facet,fmark);
  foreach edge ee where edge_kind == triple_edge do
    foreach ee.facet ff where fjvxnum == 0 do
    { local this_fmark,this_e,headv,next_e;
      printf "   <f> ";
      this_fmark := ff.fmark;
      this_e := ee.oid;
      do
      { headv := edge[this_e].vertex[2].id;
        printf "%d ",vertex[headv].jvx_number;
        next_e := 0;
        foreach vertex[headv].edge eee where (eee.edge_kind == triple_edge) 
          and (eee.oid != -this_e) do
        { foreach eee.facet fff where fmark == this_fmark do
          { next_e := eee.oid;
            fff.fjvxnum := -1;
            break 2;
          }
        };
        if next_e == 0 then
        { errprintf "Cannot find next_e.\n";
          abort;
        };
        this_e := next_e;
      } while next_e != ee.oid;
      printf "</f>\n";
      ff.fjvxnum := ff.fmark;
    };
  printf "   </faces>\n";

  // Facet colors
  printf "   <colors>\n";
  foreach edge ee where edge_kind == triple_edge do
    foreach ee.facet ff where fjvxnum == fmark do
    { 
      printf "   <c> %d %d %d </c>\n",
          (rgb_jvx[ff.frontcolor+1][1] + rgb_jvx[ff.backcolor+1][1])/2,
          (rgb_jvx[ff.frontcolor+1][2] + rgb_jvx[ff.backcolor+1][2])/2,
          (rgb_jvx[ff.frontcolor+1][3] + rgb_jvx[ff.backcolor+1][3])/2;
      ff.fjvxnum := -1;
    };
  printf "   </colors>\n";


  printf "  </faceSet>\n";

  printf "  <material shading=\"flat\">\n";
  printf "    <ambientIntensity>0.5</ambientIntensity>\n";
  printf "    <shininess>1.0</shininess>\n";
  printf "    <specular> <color type=\"rgb\">0 0 0</color></specular>\n";
  printf "    <transparency visible=\"show\">%g</transparency>\n",
      jvx_transparency;
  printf "  </material>\n";

  printf " </geometry>\n";


  printf "</geometries>\n";
  printf "</jvx-model>\n";
}

  
// End foamface_jvx.cmd

// Usage:  foamface_jvx >>> "filename.jvx"

