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
; $Id: pfo_czt.pro,v 1.3 2015/03/03 21:58:56 jpmorgen Exp $
;-
function pfo_czt, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                  create=create, print=print, widget=widget, $
                  E0=E0, area=area, sigma=sigma, alpha=alpha_in, mu=mu_in, $
                  f_wings=f_in, f_step=f_step, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.czt

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
     new_parinfo[2].parname = 'S'
     new_parinfo[3].parname = 'Al'
     new_parinfo[4].parname = 'Ah'
     new_parinfo[5].parname = 'ml'
     new_parinfo[6].parname = 'mh'
     new_parinfo[7].parname = 'fl'
     new_parinfo[8].parname = 'fhi'
     new_parinfo[9].parname = 'fs'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'Peak energy'
        new_parinfo[1].parname = 'Area'
        new_parinfo[2].parname = 'Sigma'
        new_parinfo[3].parname = 'Alpha low'
        new_parinfo[4].parname = 'Alpha hi'
        new_parinfo[5].parname = 'mu lo'
        new_parinfo[6].parname = 'mu hi'
        new_parinfo[7].parname = 'f low'
        new_parinfo[8].parname = 'f hi'
        new_parinfo[9].parname = 'f step'
     endif

     ;; VALUE
     if N_elements(E0) eq 0 then $
       E0 = 0
     if N_elements(area) eq 0 then $
       area = 0
     ;; Sigma has to be non-zero or else the function blows up
     if N_elements(sigma) eq 0 then $
       sigma = 1d

     ;; Don't clobber our inputs when we vectorize
     if N_elements(alpha_in) ne 0 then $
       alpha = alpha_in
     ;; Alpha needs to be non-zero or else the function blows up
     if N_elements(alpha) eq 0 then $
       alpha = 1d
     if N_elements(alpha) eq 1 then $
       alpha = [alpha, alpha]
     if N_elements(mu_in) ne 0 then $
       mu = mu_in
     if N_elements(mu) eq 0 then $
       mu = 0
     if N_elements(mu) eq 1 then $
       mu = [mu, mu]
     if N_elements(f_in) ne 0 then $
       f = f_in
     if N_elements(f) eq 0 then $
       f = 0
     if N_elements(f) eq 1 then $
       f = [f, f]

     if N_elements(f_step) eq 0 then $
       f_step = 0

     new_parinfo[0].value = E0
     new_parinfo[1].value = area
     new_parinfo[2].value = sigma
     new_parinfo[3].value = alpha[0]
     new_parinfo[4].value = alpha[1]
     new_parinfo[5].value = mu[0]
     new_parinfo[6].value = mu[1]
     new_parinfo[7].value = f[0]
     new_parinfo[8].value = f[1]
     new_parinfo[9].value = f_step
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn
     
     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.

     ;; FTYPE
     new_parinfo[0].pfo.ftype = 0.01 ;; E0       
     new_parinfo[1].pfo.ftype = 0.02 ;; area     
     new_parinfo[2].pfo.ftype = 0.03 ;; sigma    
     new_parinfo[3].pfo.ftype = 0.04 ;; alpha[0] 
     new_parinfo[4].pfo.ftype = 0.05 ;; alpha[1] 
     new_parinfo[5].pfo.ftype = 0.06 ;; mu[0]    
     new_parinfo[6].pfo.ftype = 0.07 ;; mu[1]    
     new_parinfo[7].pfo.ftype = 0.08 ;; f[0]     
     new_parinfo[8].pfo.ftype = 0.09 ;; f[1]     
     new_parinfo[9].pfo.ftype = 0.10 ;; f_step   
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn

     ;; LIMITS, LIMITED
     ;; Keep all parameters positive
     new_parinfo.limits = [0,0]
     new_parinfo.limited = [1,0]
     ;; Once we get up to an f[_wings] = 1, the gaussian is totally
     ;; dominated by the wings, so there is not much point in going
     ;; any further.
     new_parinfo[7:8].limits = [0,1]
     new_parinfo[7:8].limited = [1,1]



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
  ;; CZT sub ftypes are in numeric order, so put them back if they
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

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  E0	= params[pidx[0]]
  area  = params[pidx[1]]
  sigma	= params[pidx[2]]
  alpha = params[pidx[3:4]]
  mu	= params[pidx[5:6]]
  f 	= params[pidx[7:8]]
  f_step= params[pidx[9]]

  return, gnd_czt(Xin, E0, area, sigma, alpha, mu, f, f_step, _EXTRA=extra)

end

