; +
; $Id: pfo_voigt.pro,v 1.2 2004/01/15 17:10:25 jpmorgen Exp $

; pfo_voigt.pro 

;; pfo primitive used to calculate a voigt profile.  Also creates
;; parinfo records (/CREATE) and prints mpfit parameters (PRINT=?)

; -

function pfo_voigt, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                    create=create, print=print, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.voigt
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
     new_parinfo[1].parname = 'A'
     new_parinfo[2].parname = 'Gw'
     new_parinfo[3].parname = 'Lw'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'center'
        new_parinfo[1].parname = 'area'
        new_parinfo[2].parname = 'Gaussian width'
        new_parinfo[3].parname = 'Lorentzian width'
     endif

     ;; VALUE
     ;; For Voigts, the Gaussian width must be non-zero or else the
     ;; function returns NAN
     new_parinfo[*].value = 0
     new_parinfo[2].value = 1

     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.

     ;; FTYPE
     new_parinfo[0].pfo.ftype = 0.1
     new_parinfo[1].pfo.ftype = 0.2
     new_parinfo[2].pfo.ftype = 0.3
     new_parinfo[3].pfo.ftype = 0.4
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn

     ;; LIMITS, LIMITED
     ;; Keep widths positive, but no upper limit for now
     new_parinfo[2:3].limits = [0,0]
     new_parinfo[2:3].limited = [1,0]

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
  ;; This code is taken from Carey Woodward's V-Fudgit documentation
  x0 		= params[pidx[0]]
  area		= params[pidx[1]]
  dopFWHM	= params[pidx[2]]
  lorFWHM 	= params[pidx[3]]
  
  rln2 = sqrt(alog(2d))         ; Make sure result is double precision
  x = 2d*rln2 * (Xin - x0)/dopFWHM
  y = rln2 * lorFWHM/dopFWHM
  ;; Use IDL's internal Voigt profile for now even though Carey
  ;; complains of a problem with it.  --> look into this
  return, 2d*rln2/sqrt(!pi) * area/dopFWHM * voigt(y,x) 

end
