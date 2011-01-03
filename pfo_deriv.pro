;+
; NAME: pfo_deriv
;
; PURPOSE: Take the derivative of a function in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:

; DESCRIPTION: This works by assigning a unique ID, stored in the
; pfo_deriv.ID tag to all of the functions you want to take the
; derivative of.  Set that tag in the functions and in the pfo_deriv
; function so that they can be matched to each other.  Indicate the
; axis over which the differentiation will occur with the pfo_deriv.dx
; tag and the axis that will be differentiated with the pfo_deriv.dy
; tag.  The output of the derivative function is combined with the
; pfo_funct in the normal way using pfo.outaxis and pfo.fop.  By
; setting fop = !pfo.noop in the function that is being
; differentiated, you can create the derivative of a function without
; having to include the function itself in the calculations.  You will
; probably want to leave pfo.inaxis = !pfo.Xin so that the function
; you are working with gets fed the raw Xin axis.

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
; $Id: pfo_deriv.pro,v 1.1 2011/01/03 21:48:14 jpmorgen Exp $
;
; $Log: pfo_deriv.pro,v $
; Revision 1.1  2011/01/03 21:48:14  jpmorgen
; Initial revision
;
;-
function pfo_deriv, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                    create=create, print=print, widget=widget, $
                    deriv_ID=deriv_ID, dx=dx, dy=dy, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.deriv

  ;; This section is specific to each PFO function definition
  if keyword_set(create) then begin
     ;; /CREATE

     ;; Use pfo_null to get all the error checking on whatever
     ;; template parinfo structure we have.  Make sure to set
     ;; !pfo.fnpars for each new function in pfo_sysvar__define: it is
     ;; used in pfo_funct.
     new_parinfo = replicate(pfo_null(/create, parinfo=parinfo, idx=idx, $
                                     _EXTRA=extra), !pfo.fnpars[fn])

     ;; Check to see if we have the correct structure
     junk = where(tag_names(new_parinfo) eq 'PFO_DERIV', ispfo)
     if ispfo ne 1 then $
       message, 'ERROR: no PFO_DERIV sub-structure in parinfo.  See pfo_deriv_struct__define.pro'

     ;; PARNAME
     ;; Don't put function name in parname, since it can be
     ;; reconstructed from pfo.ftype.  Packages built on top of pfo
     ;; will want to set !pfo.longnames = 0 to use these short names     
     new_parinfo[0].parname = 'd'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'deriv'
     endif

     ;; VALUE
     ;; This is vestigial
     new_parinfo[*].value = 0

     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.  

     ;; FTYPE
     new_parinfo.pfo.ftype = fn

     ;; LIMITS, LIMITED

     ;; FIXED
     ;; The derivative has no free parameters
     new_parinfo.fixed = 1

     ;; ID -- if specified, point to the ID of the function(s) that
     ;; will be differentiated
     if N_elements(deriv_ID) eq 1 then $
       new_parinfo.pfo_deriv.ID = deriv_ID

     ;; Set the default axis over which the derivative is calculated
     if N_elements(dx) eq 0 then $
       dx = !pfo.Xin

     ;; Set the default axis that is differentiated
     if N_elements(dy) eq 0 then $
       dy = !pfo.Yaxis

     new_parinfo.pfo_deriv.dx = dx
     new_parinfo.pfo_deriv.dy = dy

     ;; In this case return the new_parinfo records
     return, new_parinfo

  endif ;; /CREATE

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  ;; PRINT or WIDGET.  pfo_null can handle these once the parameters
  ;; are in order
  if keyword_set(print) or keyword_set(widget) then $
    return, pfo_null([0], params, parinfo=parinfo, idx=f_idx, print=print, $
                    widget=widget, _EXTRA=extra)

  ;; CALCULATE
  ;; Our default derivative is 0 if we don't find anything to differentiate
  result = Xin * 0.

  ;; Here is the ID we are looking for.  It can be in as many
  ;; functions as we want.  Let the pfo_funct error checking catch
  ;; problems with not tagging a sensible subsection of all the
  ;; parameters in a particular function.  For now, bubble up the
  ;; error to higher-level routines
  ID =     parinfo[f_idx].pfo_deriv.ID
  dx =     parinfo[f_idx].pfo_deriv.dx
  dy =     parinfo[f_idx].pfo_deriv.dy
  inaxis = parinfo[f_idx].pfo.inaxis
  ;; The pfo_deriv.status is set in the function you want differentiated
  IDidx = where(parinfo.pfo_deriv.ID eq ID and parinfo.pfo_deriv.status eq !pfo.active, count)

  ;; Check to see if there are any functions attached to this
  ;; derivative.  If not, return quietly
  if count eq 0 then $
    return, result

  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL
     message, /CONTINUE, !error_state.msg
     message, 'ERROR: pfo_deriv.ID = ' + strtrim(ID, 2) + ' caused the above error'
  endif

  ;; Note that the function(s) might return yaxis (continuum and
  ;; peaks) or xaxis (dispersion/gain).  Also note that by calculating
  ;; the derivative in this way, functions that act on the accumulated
  ;; x- or y-axis are not properly accounted for unless you include
  ;; all the functions that accumulate the x- or y-axis in your ID.
  yaxis = pfo_funct(Xin, params, parinfo=parinfo, idx=IDidx, xaxis=xaxis)

  ;; dy determines the dependent variable (e.g. the numerator of
  ;; the differentiation).  dx determines the denominator

  case dy of
     !pfo.Xin	: y = Xin ;; Somewhat silly case, but be general, just in case
     !pfo.Xaxis	: y = xaxis
     !pfo.Yaxis	: y = yaxis
     else	: message, 'ERROR: improperly set pfo.inaxis value in derivative function'
  endcase

  case dx of 
     !pfo.Xin	: x = Xin
     !pfo.Xaxis	: x = xaxis
     !pfo.Yaxis	: x = yaxis
     else	: message, 'ERROR: improperly set pfo_deriv.dx value in derivative function'
  endcase

  ;; Use IDL's internal deriv function for calculation
  result = deriv(x, y)
  return, result
  
end

