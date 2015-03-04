Mon Jan 18 22:02:11 2010  jpmorgen@sandusky

The Parameter Function Objection system (PFO) is designed to create
and evaluate functions for the purpose of non-linear least-squares
fitting.  At the heart of PFO is PFO_FUNCT, which is the function
interpreter.  PFO_FUNCT glides along an array of IDL structures which
define the function.  PFO_FUNCT finds tags in the structure that
determine which function is currently being evaluated, collects the
appropriate strand of parameters and ships them off to the function
primitive that evaluates the function.  To keep things together, the
functions that evaulate the parameters also contain the code that
creates the parinfo array (the array of structures that define the
function) and prints the parameters in various ways.

