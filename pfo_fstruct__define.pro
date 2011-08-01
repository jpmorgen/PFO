;+
; NAME: pfo_fstruct__define
;
; PURPOSE: create and initialize the structure that associates PFO
; function names, function numbers, numbers of parameters, and
; documentation
;
; CATEGORY: PFO functions

; CALLING SEQUENCE: This is managed by pfo_fdefine

; DESCRIPTION: The pfo_fstruct allows PFO to keep track of the
; relationship between function names and ftype numbers dynamically.
; Other information, like number of parameters (0 if not known) and
; the function documentation are also kept track of in this struct.

; The system works by keeping an array of type pfo_struct either in
; the relevant pfo_obj (object-oriented mode) or in the PFO COMMON
; block (non object-oriented mode).  The position in the array
; determines the integer ftype (the fnum).  The procedure pfo_fdefine
; populates the pfo_fstruct_array.  The functions pfo_fnum, pfo_fname,
; pfo_fnpars, and pfo_fdescr read and return the appropriate
; information from the pfo_fstruct_array.

; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_fstruct__define.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_fstruct__define.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_fstruct__init, $
  fname=fname, $ ;; function name
  fnpars=fnpars, $ ;; number of parameters in function (0 if unknown)
  fdescr=fdescr, $ ;; function documentation
  descr=descr ;; descr is a return keyword that contains the structure documentation
  
  ;; Read in our pfo tokens
  init = {pfo_sysvar}

  ;; Create our struct initialized with generic IDL null values.  We
  ;; could also have called:
  ;; pfo_struct = create_struct(name='pfo_fstruct')
  pfo_fstruct = {pfo_fstruct}

  ;; Set our defaults to encourage people to use keywords
  pfo_fstruct.fnpars = -1
  pfo_fstruct.fdescr = 'Not documented'

  ;; Process our keywords
  if N_elements(fname) gt 0 then $
    pfo_fstruct.fname = fname
  if N_elements(fnpars) gt 0 then $
    pfo_fstruct.fnpars = fnpars
  if N_elements(fdescr) gt 0 then $
    pfo_fstruct.fdescr = fdescr

  descr = $
    {README	: 'This structure holds basic information about each PFO-enabled function', $
     fname	: 'Function name', $
     fnpars	: 'Number of parameters in function (0 if not known or variable, like a polynomial)', $
     fdescr	: 'Function documentation'}

  return, pfo_fstruct

end

pro pfo_fstruct__define
  pfo_fstruct = $
    {pfo_fstruct, $
     fname	: '', $
     fnpars	: 0L, $
     fdescr	: ''}
end
