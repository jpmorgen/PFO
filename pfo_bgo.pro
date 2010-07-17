;+
; NAME:
;
; PURPOSE:
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
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
; $Id: pfo_bgo.pro,v 1.2 2010/07/17 18:58:11 jpmorgen Exp $
;-
function pfo_bgo, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                  create=create, print=print, widget=widget, $
                  E0=E0, area=area, width=width_in, f_step=f_step, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.bgo

  ;; This section is specific to each PFO function definition
  if keyword_set(create) then begin
     ;; /CREATE

     ;; Use pfo_null to get all the error checking on whatever
     ;; template parinfo structure we have.  Make sure to set
     ;; !pfo.fnpars for each new function in pfo_sysvar__define: it is
     ;; used in pfo_funct.
     new_parinfo = replicate(pfo_null(/create, parinfo=parinfo, idx=idx, $
                                     _EXTRA=extra), !pfo.fnpars[fn])

     ;; PARNAME
     ;; Don't put function name in parname, since it can be
     ;; reconstructed from pfo.ftype.  Packages built on top of pfo
     ;; will want to set !pfo.longnames = 0 to use these short names     
     new_parinfo[0].parname = 'E0'
     new_parinfo[1].parname = 'A'
     new_parinfo[2].parname = 'W0'
     new_parinfo[3].parname = 'W1'
     new_parinfo[4].parname = 'fs'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'Peak energy'
        new_parinfo[1].parname = 'Area'
        new_parinfo[2].parname = 'W0'
        new_parinfo[3].parname = 'W1'
        new_parinfo[4].parname = 'f step'
     endif

     ;; VALUE
     if N_elements(E0) eq 0 then $
       E0 = 0
     if N_elements(area) eq 0 then $
       area = 0

     ;; Don't clobber our input when we vectorize
     if N_elements(width_in) ne 0 then $
       width = width_in
     if N_elements(width) eq 0 then $
       width = 1
     if N_elements(width) eq 1 then $
       width = [0, width]
     if N_elements(f_step) eq 0 then $
       f_step = 0

     new_parinfo[0].value = E0
     new_parinfo[1].value = area
     new_parinfo[2].value = width[0]
     new_parinfo[3].value = width[1]
     new_parinfo[4].value = f_step
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn
     
     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.

     ;; FTYPE
     new_parinfo[0].pfo.ftype = 0.01 ;; E0       
     new_parinfo[1].pfo.ftype = 0.02 ;; area     
     new_parinfo[2].pfo.ftype = 0.03 ;; W0
     new_parinfo[3].pfo.ftype = 0.04 ;; W1
     new_parinfo[4].pfo.ftype = 0.05 ;; f_step   
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn

     ;; LIMITS, LIMITED
     ;; Keep all parameters positive
     new_parinfo.limits = [0,0]
     new_parinfo.limited = [1,0]
     ;; --> I suppose W0 could be negative, but assume no for now

     ;; In this case return the new_parinfo records
     return, new_parinfo

  endif ;; /CREATE

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  ;; FUNCTION SPECIFIC FTYPE HANDLING
  ;; BGO sub ftypes are in numeric order, so put them back if they
  ;; have been scrambled.
  sidx = sort(parinfo[f_idx].pfo.ftype)
  ;; Make an easy handle for the parameters
  pidx = f_idx[sidx]

  ;; PRINT or WIDGET.  pfo_null can handle these once the parameters
  ;; are in order
  if keyword_set(print) or keyword_set(widget) then $
    return, pfo_null([0], params, parinfo=parinfo, idx=pidx, print=print, $
                    widget=widget, _EXTRA=extra)

  ;; CALCULATE

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  E0	= params[pidx[0]]
  area  = params[pidx[1]]
  width	= params[pidx[2:3]]
  f_step= params[pidx[4]]

  return, gnd_bgo(Xin, E0, area, width, f_step, _EXTRA=extra)

end

