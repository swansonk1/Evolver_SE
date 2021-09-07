// complex.cmd

// Surface Evolver command file.
// Programmer: Ken Brakke,  brakke@susqu.edu, http://www.susqu.edu/brakke

/* Some complex functions of complex arguments, mostly for use with i
   Weierstrass representation of minimal surfaces.
   Contents:
     re_sqrt, im_sqrt
     re_sin, im_sin
     re_arcsin, im_arcsin
     re_incompleteEllipticF, im_incompleteEllipticF

   Each complex function is implemented as two real functions, since
   Evolver doesn't have a complex or array return type for functions,
   and using global variables for returns would not be thread safe.

   Arguments:
     rex,imx: real and imaginary parts of the complex argument
   But see re_incompleteEllipticF for its further arguments.
*/

// Sqrt, cut on negative real axis
function real re_sqrt(real rex, real imx)
{
  return sqrt((rex + sqrt(rex^2+imx^2))/2);
}

function real im_sqrt(real rex, real imx)
{
  if imx > 0 then
     return sqrt((-rex + sqrt(rex^2+imx^2))/2)
  else
     return -sqrt((-rex + sqrt(rex^2+imx^2))/2);
}

// Sin
function real re_sin(real rex, real imx)
{
  return sin(rex)*cosh(imx);
}

function real im_sin(real rex, real imx)
{
  return cos(rex)*sinh(imx);
}

// Arcsin; multiple valued - (x+2*pi*k,y),(pi-x+2*pi*k,-y)
function real re_arcsin(real rex, real imx)
{
  local term;
  term := rex^2 + imx^2 + 1;
  if rex < 0 then
    return -asin(sqrt((term-sqrt(term^2-4*rex^2))/2))
  else
    return asin(sqrt((term-sqrt(term^2-4*rex^2))/2));
}

function real im_arcsin(real rex, real imx)
{
  local term,mag,sign;
  mag := rex^2 + imx^2;
  term := mag - 1;
  if imx < 0 then sign := -1
  else sign := 1;
  if mag < 0.5 then  // alternate forms to avoid catastrophic cancellation
    return sign*asinh(sqrt(2*imx^2/(sqrt(term^2 + 4*imx^2) - term)))
  else
    return sign*asinh(sqrt((term+sqrt(term^2+4*imx^2))/2));
}



// incompleteEllipticF, Abramowitz and Stegun 17.4.11
// Have to beware branch points at +/- arcsin(1/sqrt(m)) + 2*pi*k,
// so nbr_value input argument for picking the 
// proper branch by continuity; if 0, then principle branch
// is picked.  Branch values differ by 2*ellipticK(m).
// nbr_test is boolean for whether to apply the continuity test.
function real re_incompleteEllipticF(real rex, real imx, real m_param,
   real nbr_value,integer nbr_test)
{
  local term,cotsq,lambda;
  if abs(sin(rex)) < 1e-12 then newvalue := 0.0
  else
  {
    rrex := rex;
    ss := floor(rex/pi + .5);
    rex -= ss*pi;
    term := 1/tan(rex)^2 + m_param*sinh(imx)^2/sin(rex)^2 - (1-m_param);
    cotsq := (term + sqrt(term^2 + 4*(1-m_param)/tan(rex)^2))/2;
    if cotsq == 0 then lambda := pi/2
    else lambda := atan(1/sqrt(cotsq));
    if rex < 0 then lambda := -lambda;
    newvalue := incompleteEllipticF(lambda+ss*pi,m_param);
  };
  if nbr_test then
  { period := 2*ellipticK(m_param);
    tt := floor((newvalue-nbr_value)/period + .5);
    newvalue -= tt*period;
    alt_tt := floor((-newvalue-nbr_value)/period + .5);
    alt_newvalue := -newvalue - alt_tt*period;
    if ( abs(alt_newvalue - nbr_value) < abs(newvalue - nbr_value) ) then
      newvalue := alt_newvalue;
  };
  return newvalue;
}

function real im_incompleteEllipticF(real rex, real imx, real m_param, 
  real nbr_value, integer nbr_test)
{
  local term,cotsq,mu;

  term := cos(rex)^2 + m_param*sinh(imx)^2 - (1-m_param)*sin(rex)^2;
  if term > 0 then
    mtansqp1 := (term + sqrt(term^2 + 4*(1-m_param)*sin(rex)^2*cos(rex)^2))/
                   2/cos(rex)^2
  else 
    mtansqp1 := 2*(1-m_param)*sin(rex)^2/
      (sqrt(term^2 + 4*(1-m_param)*sin(rex)^2*cos(rex)^2) - term);
  tansq := (mtansqp1 - 1)/m_param;
  mu := atan(sqrt(tansq));
  if imx < 0 then mu := -mu;
  return incompleteEllipticF(mu,1-m_param);
}


