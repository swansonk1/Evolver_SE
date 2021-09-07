// rewrap.cmd

// Commands to rewrap torus vertices and edges to get them nicely
// within unit cell.  This version does 3D and 2D. 
// Moves vertices at most one period at a time, so you may have to
// repeat if things are very bad to start with.

// Uses torus wrap representation and wrap_vertex builtin command.

// Usage: rewrap

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

rewrap := { 
  if space_dimension != 3 then
  { errprintf"rewrap is for space dimension 3; for 2D use rewrap2.cmd.\n";
    return;
  };
  local ucoord, dim, ups, downs;
  define ucoord real[space_dimension];  // period-based coordinates
  define body attribute old_volume real; // so can adjust volconst
  define ups integer[3]; // numeric torus wraps
  define downs integer[3];
  ups := { 1, 64, 4096 };
  downs := { 31, 1984, 126976 };

  set body old_volume volume;
  foreach vertex vv do
  { ucoord := vv.__x * inverse_periods;
    for ( dim := 1; dim <= space_dimension ; dim++ )
    { if ucoord[dim] < 0 then wrap_vertex(vv.id,ups[dim])
      else if ucoord[dim] > 1 then wrap_vertex(vv.id,downs[dim]);
    };
  };
  local torvol;
  torvol :=  abs(matrix_determinant(torus_periods));
  set body volconst floor((old_volume - volume - volconst)/torvol+.5)*torvol;

}

// End rewrap.cmd

// Usage: rewrap


