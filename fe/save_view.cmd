// save_view.cmd

// Surface Evolver command to save the view matrix in a form from which
// it can be read in to restore the view.  
// Requires Evolver version 2.40 or later, since it uses array initialization
// syntax.  Replaces saveview.cmd.

/* Usage:
   Enter command: read "save_view.cmd"
   Enter command: save_view >>> "something.view"
   ...
   Enter command: read "something.view"
*/

// Author: Ken Brakke, brakke@susqu.edu

save_view := {printf "view_matrix := "; print view_matrix }

