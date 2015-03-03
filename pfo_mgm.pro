;+
; NAME:
;	pfo_mgm
; PURPOSE:
;
;	pfo primitive used to calculate a modified Gaussian
;	distribution as derived by Sunshine et al. 1990
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
; $Id: pfo_mgm.pro,v 1.1 2015/03/03 22:04:46 jpmorgen Exp $
;-

function pfo_mgm, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                  create=create, print=print, center=center, $
                  strength=strength, width=width, wave_number=wave_number, $
                  log_refl=log_refl, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.mgm
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
     new_parinfo[0].parname = 'C'
     new_parinfo[1].parname = 'S'
     new_parinfo[2].parname = 'W'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'center'
        new_parinfo[1].parname = 'strength'
        new_parinfo[2].parname = 'width'
     endif

     ;; VALUE
     new_parinfo[*].value = 0
     if N_elements(center) eq 1 then $
       new_parinfo[0].value = center
     if N_elements(strength) eq 1 then $
       new_parinfo[1].value = strength
     if N_elements(width) eq 1 then $
       new_parinfo[2].value = width

     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.

     ;; FTYPE
     new_parinfo[0].pfo.ftype = 0.1
     new_parinfo[1].pfo.ftype = 0.2
     new_parinfo[2].pfo.ftype = 0.3
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn

     ;; LIMITS, LIMITED
     ;; These are absorption features, so the strength should be
     ;; negative, but we always think of them as positive strengths,
     ;; so make the negative sign when we calculate the function below
     new_parinfo[1].limits = [0,0]
     new_parinfo[1].limited = [1,0]
     ;; Keep sigma positive, but no upper limit for now
     new_parinfo[2].limits = [0,0]
     new_parinfo[2].limited = [1,0]

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

  ;; FUNCTION SPECIFIC FTYPE HANDLING
  ;; Voigt sub ftypes are in numeric order, so put them back if they
  ;; have been scrambled.
  sidx = sort(parinfo[f_idx].pfo.ftype)
  ;; Make an easy handle for the parameters
  pidx = f_idx[sidx]

  ;; PRINT.  pfo_null can print parameters once they are in order
  if keyword_set(print) then $
    return, pfo_null([0], params, parinfo=parinfo, idx=pidx, print=print, $
                    _EXTRA=extra)
     
  ;; CALCULATE
  x0 		= params[pidx[0]]
  strength	= params[pidx[1]]
  width		= params[pidx[2]]

  ;; We want to give the user options as to whether the calculations
  ;; are done in wavenumber or wavelength and whether or not they
  ;; return reflectance or log reflectance.  The original model is in
  ;; wavenumber vs. log reflectance, where the function is a Gaussian
  ;; with X, X0 and sigma reciprocated.  Given the equivalence of
  ;; wavelength and 1/wavenumber, the calculation can be done with a Gaussian
  if keyword_set(wave_number) then $
    log_refl = -1d * strength * $
               exp(-1d * ((1d/Xin - 1d/x0)^2d)/(2d*width^(-2d))) $
  else $
    log_refl = -1d * strength * exp(-1d * ((Xin - x0)^2d)/(2d*width^2d))

  if keyword_set(log_refl) then $
    return, log_refl

  return, exp(log_refl)

end
