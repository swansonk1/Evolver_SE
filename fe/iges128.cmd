// iges128.cmd
// Surface Evolver script to write IGES file for surface, using IGES
// rational B-spline entity (type 128).

// Documentation on IGES format: http://www.iges5x.org

/*
   Assumptions: 3D soapfilm model, linear model, not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Does facets only, not edges. 
   Does facets satisfying "show" criterion.
   Facet color is frontcolor and backcolor, if iges_double_sided is set to 1
     (default 0).  Backcolor facet is a second facet with reverse orientation
     but no normal offset, so you may have to turn on backface culling in
     the display program to get a decent view.
*/

/* usage: 
   Set which edges and facets you want to show with the
   "show edges where ..." and "show facets where ... " commands.
   Set iges_double_sided to 1 if you want front and back versions of each facet.
   Set iges_edge_tubes to 1 if you want each edge to be a long square tube
     instead of a line (good for programs that don't do edges as such).
   Then run iges128 and re-direct output to a file, like this:

     Enter command: iges128 >>> "filename.igs"
*/

// Output mode flags:
iges_double_sided := 0
iges_edge_tubes := 0

// Set up color translation array
define iges_colors integer [16];
iges_black := 1
//iges_colors[black] := 1
iges_colors[red]   := 2
iges_colors[green] := 3
iges_colors[blue]  := 4
iges_colors[yellow] := 5
iges_colors[magenta] := 6
iges_colors[cyan]  := 7
iges_colors[white] := 8
iges_colors[brown]  :=  -1  // using colors defined at start of directory
iges_colors[lightgray]  := -3
iges_colors[darkgray]  := -5
iges_colors[lightblue]  := -7
iges_colors[lightgreen]  := -9
iges_colors[lightcyan]  := -11
iges_colors[lightred]  := -13
iges_colors[lightmagenta]  := -15

iges128 := { 
  // Flag section
  // Don't need this since not doing binary or compressed format.

  if torus then
  { errprintf "Cannot run 'iges128' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if symmetry_group then
  { errprintf "Cannot run 'iges128' command in symmetry group mode. Do 'detorus' first.\n";
    abort;
  };

  if space_dimension != 3 then
  { errprintf "The 'iges128' command must be run in three-dimensional space.\n";
    abort;
  };

  if surface_dimension == 1 then
  { errprintf "The 'iges128' command is not meant for the string model.\n";
    abort;
  };

  if simplex_representation then
  { errprintf "The 'iges128' command is not meant for the simplex model.\n";
    abort;
  };

  if lagrange_order >= 2 then
  { errprintf "The 'iges128' command is meant for the linear model, not quadratic or Lagrange.\n";
    abort;
  };

  if rgb_colors then
  { errprintf "The 'iges128' command does not do RGB colors; do rgb_colors off.\n";
    abort;
  };

  local start_counter,global_counter,message;
  local xmax,ymax,zmax,maxsize,entype,paramdata,structure,linefont,level,view;
  local transmat,label,status,directory_counter,lineweight,colornum,paramcount;
  local form,reserved,entlabel,entsubscr,approxcolor,cinx,parameter_counter;
  local dirnum,k_1,m_1,prop1,prop2,prop3,prop4,minu,maxu,k_2,m_2,prop5,minv,maxv;

  // Start section
  start_counter := 0;

  start_counter += 1;
  printf "%-72sS%07d\n","IGES version of Surface Evolver surface",start_counter;
  start_counter += 1;
  printf "   %-69sS%07d\n",datafilename,start_counter;
  start_counter += 1;
  printf "%-72sS%07d\n","Created using iges.cmd Evolver script.",
     start_counter;

  // Global section
  global_counter := 0;

  global_counter += 1;
  printf "%-72sG%07d\n","1H,,1H;,",global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  message := sprintf "%s,",datafilename;
  printf "%02dH%-69sG%07d\n",sizeof(datafilename),message,global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","32,75,6,75,15,,1.0,1,2HIN,32768,0.0394,",
    global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","15H00000000.000000,", global_counter; // date

  xmax := max(vertex,abs(x));
  ymax := max(vertex,abs(y));
  zmax := max(vertex,abs(z));
  maxsize := (xmax > ymax) ? xmax : ymax;
  maxsize := (zmax > maxsize) ? zmax : maxsize;
  message := sprintf "%g,%g,",maxsize/10000000,maxsize;
  global_counter += 1;
  printf "%-72sG%07d\n",message, global_counter; 
  
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Name of author"),"Name of author,", 
       global_counter; 
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Author's organization"),
      "Author's organization,", global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","15H00000000.000000;", global_counter; // date

  // Directory entry section. Each entry 20 8-char fields on two lines.
  // Fields with default values
  entype := 0;      // 1 and 11
  paramdata := 1;   // 2
  structure := 0;   // 3
  linefont  := 0;   // 4
  level     := 0;   // 5
  view      := 0;   // 6
  transmat  := 0;   // 7
  label     := 0;   // 8
  status    := "00000000";   // 9; actually 4 two-digit numbers
  directory_counter := 0;  // 10 and 20
  lineweight := 0;   // 12
  colornum   := 0;   // 13
  paramcount := 0;   // 14
  form       := 0;   // 15
  reserved   := "       0";   // 16 and 17
  entlabel   := "entity";  // 18
  entsubscr  := 0;   // 19
 
  // Color definitions for those Evolver colors not supplied in IGES
  entype := 314;
  paramcount := 1;
  status := "00000200";
  approxcolor := 7; // incompetent systems default to cyan
  entlabel := "COLOR";
  form := 0;
  for ( cinx := 1 ; cinx <= 8 ; cinx += 1 )
  { directory_counter += 1;
    printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
      linefont,level,view,transmat,label,status,directory_counter;
    directory_counter += 1;
    printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,
      approxcolor,
      paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
    paramdata += 1;
  };


  if iges_edge_tubes then
  { // edge as 4-walled tube
  }
  else
  {
    // Edges as "rational b-spline" line types
    entype := 126;
    status := "00000000";
    paramcount := 5;  // lines of parameters
    form := 1;
    entlabel := "    EDGE";
    define edge attribute epdata integer;
    define edge attribute edir integer;
    foreach edge ee where show do 
    { ee.epdata := paramdata;
      entsubscr  := ee.id; 
      directory_counter += 1;  ee.edir := directory_counter;
      printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
        linefont,level,view,transmat,label,status,directory_counter;
      directory_counter += 1;
      printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,
        ee.color > 0 ? iges_colors[ee.color] : iges_black,
        paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
      paramdata += paramcount;
    }; 
  };

  // Facets as "rational b-spline" types
  entype := 128;
  status := "00000000";
  paramcount := 7;  // lines of parameters
  form := 8;
  entlabel := "   FACET";
  define facet attribute fpdata integer;
  define facet attribute fdir integer;
  foreach facet ff where show and color >= 0 do 
  { ff.fpdata := paramdata;
    entsubscr  := ff.id; 
    directory_counter += 1;  ff.fdir := directory_counter;
    printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
      linefont,level,view,transmat,label,status,directory_counter;
    directory_counter += 1;
    printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,
      (ff.color ? iges_colors[ff.color+1] : iges_black),
      paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
    paramdata += paramcount;

    if iges_double_sided and ff.backcolor >= 0 then
    {
      directory_counter += 1;  
      printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
        linefont,level,view,transmat,label,status,directory_counter;
      directory_counter += 1;
      printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,
        (ff.backcolor ? iges_colors[ff.backcolor] : iges_black),
        paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
      paramdata += paramcount;
    };

  }; 


  // Parameter data section
  parameter_counter := 1;

  // Color definitions
  dirnum := 1;  // corresponding directory line
  printf "%-64s%8dP%7d\n","314,100.,50.,0.;",dirnum,parameter_counter; // brown
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,60.,60.,60.;",dirnum,parameter_counter; // l. gray
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,30.,30.,30.;",dirnum,parameter_counter; // d. gray
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,30,80,100;",dirnum,parameter_counter; // l. blue
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,50,100,50;",dirnum,parameter_counter; // l. grn
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,50,100,100;",dirnum,parameter_counter; // l. cyan
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,100,50,50;",dirnum,parameter_counter; // l. red
  parameter_counter += 1; dirnum += 2;
  printf "%-64s%8dP%7d\n","314,100,50,100;",dirnum,parameter_counter; // l. mag.

  // Edges
  if iges_edge_tubes then
  { // Edges as 4-walled tubes
  }
  else
  {
    entype := 126;
    k_1 := 1;  // knots-1
    m_1 := 1;  // degree
    prop1 := 0;  // nonplanar
    prop2 := 0;  // not closed
    prop3 := 1;  // polynomial instead of rational
    prop4 := 0;  // nonperiodic
    minu := 0;
    maxu := 1;
    foreach edge ee where show do
    { parameter_counter += 1;
      if ee.epdata != parameter_counter then
         errprintf
       "ERROR: bad facet parameter line number, facet %d. Is %d, should be %d.\n",
           ee.id,parameter_counter,ee.fpdata;
      message := sprintf "%d,%d,%d,%d,%d,%d,%d,",entype,k_1,m_1,
           prop1,prop2,prop3,prop4;
      printf "%-64s%8dP%7d\n",message,ee.edir,parameter_counter;
  
      message := "0.0,0.0,1.0,1.0,1.0,1.0,";
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ee.edir,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ee.vertex[1].x,ee.vertex[1].y,ee.vertex[1].z;
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ee.edir,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ee.vertex[2].x,ee.vertex[2].y,ee.vertex[2].z; 
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ee.edir,parameter_counter;
  
      message :=  "0.0,1.0;"; //minu,maxu;
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ee.edir,parameter_counter;
  
    };
  };
 
  // Facets
  entype := 128;
  k_1 := 1;  // knots-1
  k_2 := 1;  // 
  m_1 := 1;  // degree
  m_2 := 1;  // 
  prop1 := 0;  // not closed in first paramter
  prop2 := 0;  // not closed in second parameter
  prop3 := 1;  // polynomial instead of rational
  prop4 := 0;  // nonperiodic in first parameter
  prop5 := 0;  // nonperiodic in second parameter
  minu := 0;
  maxu := 1;
  minv := 0;
  maxv := 1;
  foreach facet ff where show and ff.color >= 0 do
  { parameter_counter += 1;
    if ff.fpdata != parameter_counter then
       errprintf
     "ERROR: bad facet parameter line number, facet %d. Is %d, should be %d.\n",
         ff.id,parameter_counter,ff.fpdata;
    message := sprintf "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,",entype,k_1,k_2,m_1,m_2,
         prop1,prop2,prop3,prop4,prop5;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message := "0.0,0.0,1.0,1.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,";
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message := sprintf "%9.7f,%9.7f,%9.7f,",
       ff.vertex[1].x,ff.vertex[1].y,ff.vertex[1].z;
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message := sprintf "%9.7f,%9.7f,%9.7f,",
       ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z; 
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message := sprintf "%9.7f,%9.7f,%9.7f,",
       ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z; 
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message := sprintf "%9.7f,%9.7f,%9.7f,",
       ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z; 
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    message :=  "0.0,1.0,0.0,1.0;"; //minu,maxu,minv,maxv;
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;

    if iges_double_sided and ff.backcolor >= 0 then
    {
      parameter_counter += 1;
      message := sprintf "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,",entype,k_1,k_2,m_1,m_2,
           prop1,prop2,prop3,prop4,prop5;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message := "0.0,0.0,1.0,1.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,";
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ff.vertex[1].x,ff.vertex[1].y,ff.vertex[1].z;
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z; 
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z; 
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message := sprintf "%9.7f,%9.7f,%9.7f,",
         ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z; 
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
      message :=  "0.0,1.0,0.0,1.0;"; //minu,maxu,minv,maxv;
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",message,ff.fdir+2,parameter_counter;
  
  
    };

  };
 
  // Terminate section
  printf "S%07dG%07dD%07dP%07d%40sT0000001\n",start_counter,global_counter,
     directory_counter,parameter_counter," ";

} // end iges128

/* usage: 
   Set which edges and facets you want to show with the
   "show edges where ..." and "show facets where ... " commands.
   Set iges_double_sided to 1 if you want front and back versions of each facet.
   Set iges_edge_tubes to 1 if you want each edge to be a long square tube
     instead of a line (good for programs that don't do edges as such).
   Then run iges128 and re-direct output to a file, like this:

     Enter command: iges128 >>> "filename.igs"
*/

