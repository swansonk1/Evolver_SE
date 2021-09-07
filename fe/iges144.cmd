// iges144.cmd
// Surface Evolver script to write IGES file for surface, using IGES
// entity type 144, which handles linear, quadratic,
// and cubic triangles (among many other types not of interest).

// This version uses IGES element 144, a trimmed parametric surface.
// Uses polynomial spline element 114 as parametric surface.

/*
   Assumptions: 3D soapfilm model, up to cubic model, 
   not torus or symmetry group
    (use the "detorus" command if necessary to convert torus or symmetry
     to unwrapped surface, but remember that detorus alters the surface)
   Does facets only, not edges. 
   Does facets satisfying "show" criterion.
   Facet color is frontcolor on both sides.
*/
// This version automatically handles quadratic and cubic models.

// usage: iges144 >>> "filename.igs"

// Set up color translation function
define iges_colors_list integer[16]
iges_black := 1
iges_colors_list[red]   := 2
iges_colors_list[green] := 3
iges_colors_list[blue]  := 4
iges_colors_list[yellow] := 5
iges_colors_list[magenta] := 6
iges_colors_list[cyan]  := 7
iges_colors_list[white] := 8
iges_colors_list[brown]  := 2 // red
iges_colors_list[lightgray]  := 8 // white
iges_colors_list[darkgray]  := 1 // black
iges_colors_list[lightblue]  := 4 // blue
iges_colors_list[lightcyan]  := 7 // cyan
iges_colors_list[lightred]  := 2 // red
iges_colors_list[lightmagenta]  := 7
iges_colors_list[lightgreen]  := 3
function integer iges_color_translate(integer evolver_color) 
{ if evolver_color == black then return iges_black
  else return iges_colors_list[evolver_color];
}

iges_edges_show := 1;  // but doesn't seem to have effect

// Coefficients of the 2D cubic polynomial for coordinate jj
procedure iges114_coeff(integer fid, integer jj)
{ local v1,v2,v3,v4,v5,v6,v7,v8,v9,vA;

  foreach facet[fid] ff where show and color >= 0 do
  if linear then
  { c0c0 := ff.vertex[1].x[jj];
    c1c0 := ff.vertex[2].x[jj]-ff.vertex[1].x[jj];
    c0c1 := ff.vertex[3].x[jj]-ff.vertex[1].x[jj];
    c1c1 := 0;
    c0c2 := 0;
    c0c3 := 0;
    c1c2 := 0;
    c1c3 := 0;
    c2c0 := 0;
    c2c1 := 0;
    c2c2 := 0;
    c2c3 := 0;
    c3c0 := 0;
    c3c1 := 0;
    c3c2 := 0;
    c3c3 := 0;

  }
  else if quadratic then
  {
    v1 := ff.vertex[1].x[jj];
    v2 := ff.vertex[4].x[jj];
    v3 := ff.vertex[2].x[jj];
    v4 := ff.vertex[6].x[jj];
    v5 := ff.vertex[5].x[jj];
    v6 := ff.vertex[3].x[jj];
    c0c0 := v1;
    c0c1 := (-3*v1)/2. + 2*v4 - v6/2.;
    c0c2 := v1/2. - v4 + v6/2.;
    c1c0 := (-3*v1)/2. + 2*v2 - v3/2.;
    c1c1 := v1 - v2 - v4 + v5;
    c1c2 := 0;
    c2c0 := v1/2. - v2 + v3/2.;
    c2c1 := 0;
    c2c2 := 0;
    c0c3 := 0;
    c1c3 := 0;
    c2c3 := 0;
    c3c0 := 0;
    c3c1 := 0;
    c3c2 := 0;
    c3c3 := 0;

  }
  else if lagrange and lagrange_order == 2 then
  {
    v1 := ff.vertex[1].x[jj];
    v2 := ff.vertex[2].x[jj];
    v3 := ff.vertex[3].x[jj];
    v4 := ff.vertex[4].x[jj];
    v5 := ff.vertex[5].x[jj];
    v6 := ff.vertex[6].x[jj];
    c0c0 := v1;
    c0c1 := (-3*v1)/2. + 2*v4 - v6/2.;
    c0c2 := v1/2. - v4 + v6/2.;
    c1c0 := (-3*v1)/2. + 2*v2 - v3/2.;
    c1c1 := v1 - v2 - v4 + v5;
    c1c2 := 0;
    c2c0 := v1/2. - v2 + v3/2.;
    c2c1 := 0;
    c2c2 := 0;
    c0c3 := 0;
    c1c3 := 0;
    c2c3 := 0;
    c3c0 := 0;
    c3c1 := 0;
    c3c2 := 0;
    c3c3 := 0;

  }
  else if lagrange and lagrange_order == 3 then
  {
    v1 := ff.vertex[1].x[jj];
    v2 := ff.vertex[2].x[jj];
    v3 := ff.vertex[3].x[jj];
    v4 := ff.vertex[4].x[jj];
    v5 := ff.vertex[5].x[jj];
    v6 := ff.vertex[6].x[jj];
    v7 := ff.vertex[7].x[jj];
    v8 := ff.vertex[8].x[jj];
    v9 := ff.vertex[9].x[jj];
    vA := ff.vertex[10].x[jj];

    c0c0 := v1;
    c0c1 := (-11*v1)/6. + 3*v5 - (3*v8)/2. + vA/3.;
    c0c2 := v1 - (5*v5)/2. + 2*v8 - vA/2.;
    c0c3 := -v1/6. + v5/2. - v8/2. + vA/6.;
    c1c0 := (-11*v1)/6. + 3*v2 - (3*v3)/2. + v4/3.;
    c1c1 := 2*v1 - (5*v2)/2. + v3/2. - (5*v5)/2. + 3*v6 - v7/2. + v8/2. - v9/2.;
    c1c2 := -v1/2. + v2/2. + v5 - v6 - v8/2. + v9/2.;
    c1c3 := 0;
    c2c0 := v1 - (5*v2)/2. + 2*v3 - v4/2.;
    c2c1 := -v1/2. + v2 - v3/2. + v5/2. - v6 + v7/2.;
    c2c2 := 0;
    c2c3 := 0;
    c3c0 := -v1/6. + v2/2. - v3/2. + v4/6.;
    c3c1 := 0;
    c3c2 := 0;
    c3c3 := 0;

  }

  
} // end iges114_coeff
   
iges_parameter_counter := 0;

procedure print_coeffs(integer dirnum)
{   local message;

    message := sprintf "%18.15f,%18.15f,",c0c0,c1c0;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c2c0,c3c0;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c0c1,c1c1;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c2c1,c3c1;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c0c2,c1c2;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c2c2,c3c2;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c0c3,c1c3;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

    message := sprintf "%18.15f,%18.15f,",c1c3,c2c3;
    iges_parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,dirnum,iges_parameter_counter;

} // end print_coeffs

iges144 := { 

  local start_counter,global_counter,message,xmax,ymax,zmax;
  local maxsize,paramdata,structure,linefont,level,view;
  local transmat,label,status,directory_counter,lineweight;
  local colornum,paramcount,form,reserved,entlabel,entsubscr;
  local ctype,ptype,mvalue,nvalue,minu,maxu;
  local minv,maxv,co00,co10,co01,co11,line,subcount;
  local entype, ptr102;

  if torus then
  { errprintf "Cannot run 'iges144' command in torus mode. Do 'detorus' first.\n";
    abort;
  };

  if surface_dimension != 2 then
  { errprintf "iges144 requires soapfilm model surface, not string model.\n"; 
    abort;
  };
  if !linear and !quadratic and (!lagrange_order==2) and (!lagrange_order==3)
  then
  {  errprintf 
    "iges144 requires surface to be linear, quadratic, or cubic.\n";
     abort;
  };

  // Flag section
  // Don't need this since not doing binary or compressed format.

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
  reserved   := "        ";   // 16 and 17
  entlabel   := "entity";     // 18
  entsubscr  := 0;            // 19
 
  /* (u,v) parameter ranges */
  minu := 0;
  maxu := lagrange_order;
  minv := 0;
  maxv := lagrange_order;

  // One set of trim lines
  printf"     110       1       0       1       0       0       0       000110001D      1\n";
  printf"     110       1       0       1       0       0       0               0D      2\n";
  printf"     110       2       0       1       0       0       0       000110001D      3\n";
  printf"     110       1       0       1       0       0       0               0D      4\n";
  printf"     110       3       0       1       0       0       0       000110001D      5\n";
  printf"     110       1       0       1       0       0       0               0D      6\n";
  directory_counter += 6;

  // Composite curve entity made from trim lines
  printf"     102       4       0       1       0       0       0       000010501D      7\n";
  printf"     102       1       0       1       0       0       0               0D      8\n";
  ptr102 := directory_counter+1;
 
  directory_counter += 2;
  paramdata += 4;

  // Facets as "parametric surface" types
  status := "00010001";
  entlabel := "   FACET";
  define facet attribute fpdata integer;
  define facet attribute fdir integer;
  foreach facet ff where show and color >= 0 do 
  { 
    // Spline element
    ff.fpdata := paramdata;
    entsubscr  := ff.id; 
    directory_counter += 1;  ff.fdir := directory_counter;
    printf "     114%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",paramdata,structure,
      linefont,level,view,transmat,label,status,directory_counter;
    directory_counter += 1;
    paramcount := 25;
    printf "     114%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",lineweight,
      iges_color_translate(ff.color),
      paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
    paramdata += paramcount;   // 25 lines per facet in data section

    // Curve on parametric surface element 142
    directory_counter += 1;
    printf"     142%8d       0       1       0       0       0       000010501D%7d\n",
       paramdata, directory_counter;
    paramdata += 1;
    directory_counter += 1;
    printf"     142       1%8d       1       0       0       0               0D%7d\n",
      iges_edges_show ? iges_color_translate(black) : iges_color_translate(ff.color), // no effect
       directory_counter;

    // Trimmed surface, finally.
    directory_counter += 1;
    printf"     144%8d       0       1       0       0       0       000000001D%7d\n",
       paramdata, directory_counter;
    paramdata += 1;
    directory_counter += 1;
    printf"     144       1%8d       1       0       0       0   face2       0D%7d\n",
      iges_color_translate(ff.color),
       directory_counter;
  }; 
  // Geometry element
  status := "00000301";
  paramcount := ceil(facet_count/10);
  form := 7;
  entlabel := " SURFACE";
  entsubscr  := 1; 
  directory_counter += 1;  
  printf "     402%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",paramdata,structure,
     linefont,level,view,transmat,label,status,directory_counter;
  directory_counter += 1;
  printf "     402%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",lineweight,colornum,
     paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;


  // Parameter data section
  iges_parameter_counter := 0;

  // Trim lines
  message:= sprintf "110,0.0,0.0,0.0,%g,0.0,0.0;",maxu;
  iges_parameter_counter += 1;
  printf "%-64s%8dP%7d\n",message,1,iges_parameter_counter;
  message:= sprintf "110,%g,0.0,0.0,0.0,%g,0.0;",maxu,maxv;
  iges_parameter_counter += 1;
  printf "%-64s%8dP%7d\n",message,3,iges_parameter_counter;
  message:= sprintf "110,0.0,%g,0.0,0.0,0.0,0.0;",maxv;
  iges_parameter_counter += 1;
  printf "%-64s%8dP%7d\n",message,5,iges_parameter_counter;
  
  // Composite curve
  message:="102,3,1,3,5;";
  iges_parameter_counter += 1;
  printf "%-64s%8dP%7d\n",message,7,iges_parameter_counter;

  // Facets
  ptype := 0;  // unspecified
  mvalue := 1;  // number of u segments
  nvalue := 1;  // number of v segments
  if linear then { ctype := 1;  }
  else if quadratic or (lagrange_order == 2)  then
    { ctype := 2;}
  else { ctype := 3; };
  foreach facet ff where show and color >= 0 do
  { iges_parameter_counter += 1;
    if ff.fpdata != iges_parameter_counter then
       errprintf
     "ERROR: bad facet parameter line number, facet %d. Is %d, should be %d.\n",
         ff.id,iges_parameter_counter,ff.fpdata;
    message := sprintf "114,%d,%d,%d,%d,%g,%g,%g,%g,",ctype,ptype,
         mvalue,nvalue,minu,maxu,minv,maxv;
    printf "%-64s%8dP%7d\n",message,ff.fdir,iges_parameter_counter;
    // x coefficients, 2 per line
    iges114_coeff(ff.id,1);
    print_coeffs(ff.fdir);
    // y coefficients, 2 per line
    iges114_coeff(ff.id,2);
    print_coeffs(ff.fdir);
    // z coefficients, 2 per line
    iges114_coeff(ff.id,3);
    print_coeffs(ff.fdir);

    // Curve on parametric surface
    iges_parameter_counter += 1;
    message := sprintf"142,3,%d,%d,0,1;",ff.fdir,ptr102;
    printf "%-64s%8dP%7d\n",message,ff.fdir+2,iges_parameter_counter;

    // Trimmed parametric surface entity
    iges_parameter_counter += 1;
    message := sprintf"144,%d,1,0,%d;",ff.fdir,ff.fdir+2;
    printf "%-64s%8dP%7d\n",message,ff.fdir+4,iges_parameter_counter;

  };


  // Geometry element
  line := sprintf "402,%d,",facet_count;
  subcount := 0;
  foreach facet ff where show and color >= 0 do
  { if subcount == 10 then
    { 
      iges_parameter_counter += 1;
      printf "%-64s%8dP%7d\n",line,directory_counter-1,iges_parameter_counter;
      line := "" ;
      subcount := 0;
    };
    line := sprintf"%s%d,",line,ff.fdir+4;
    subcount += 1;
  };
  line := sprintf "%s0,0;",line;  // sample files ended with 2 extra 0's
  iges_parameter_counter += 1;
  printf "%-64s%8dP%7d\n",line,directory_counter-1,iges_parameter_counter;

 
  // Terminate section
  printf "S%07dG%07dD%07dP%07d%40sT0000001\n",start_counter,global_counter,
     directory_counter,iges_parameter_counter," ";
}

// End iges144.cmd

// Usage: iges144 >>> "filename.igs"

