;+
; NAME: pfo_fit
;
; PURPOSE: 
;	fit a function described by a parinfo array to data described
;	in x, y, and yerr using mpfitfun.  Function is described by
;	parainfo.  Results are  returned in parinfo.value and
;	parinfo.error
;
; CATEGORY: Parameter Function Object (PFO)
;
; CALLING SEQUENCE:
;	pfo_fit, x, y, yerr, parinfo, no_status=no_status, [keyword args to mpfitfun])
;
; INPUTS:
;	x: x-axis values
;	y: y-axis values
;	yerr: error estimates for y
;	parinfo: parinfo array that describes the function
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;	no_status: don't print out the interpretation of the
;	mpfitfun return status keyword
;
; OUTPUTS:
;	parinfo.value and parinfo.error are set to the values
;	determined by mpfitfun unless mpfitfun returns an error
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
;	Note in the following example, the /quiet keyword is passed to
;	mpfitfun so that iteration information is not printed.  The
;	mpfitfun status is printed unless /no_status is specified.
;	good_idx = where(yerr ne 0)
;	pfo_fit, x[good_idx], y[good_idx], yerr[good_idx], parinfo, /quiet
;
; MODIFICATION HISTORY:
;
; $Id:$
;-
pro pfo_fit, x, y, yerr, parinfo, ftol=ftol, xtol=xtol, $
             resdamp=resdamp, gtol=gtol, maxiter=maxiter, $
             quiet=quiet, nprint=nprint, iterproc=iterproc, $
             iterargs=iterargs, status=status, no_status=no_status, $
             niter=niter, bestnorm=chisq, _EXTRA=extra


  functargs = {parinfo:parinfo}
  if keyword_set(extra) then $
    functargs = {parinfo:parinfo, _EXTRA:extra}

  ;; I wish that mpfitfun automatically returned these, but it
  ;; doesn't, so borrow them from mpfit, where Craig defines them
  if n_elements(ftol) EQ 0 then ftol = 1.D-10
  if n_elements(xtol) EQ 0 then xtol = 1.D-10
  if n_elements(gtol) EQ 0 then gtol = 1.D-10
  if n_elements(maxiter) EQ 0 then maxiter = 200L

  if NOT keyword_set(iterproc) then $
    iterproc=!pfo.iterproc

  params = $
    mpfitfun('pfo_funct', x, y, yerr, $
             parinfo=parinfo, functargs=functargs, $
             autoderivative=1, ftol=ftol, xtol=xtol, $
             resdamp=resdamp, gtol=gtol, maxiter=maxiter, $
             quiet=quiet, nprint=nprint, iterproc=iterproc, $
             iterargs=iterargs, perror=perror, $
             status=status, niter=niter, bestnorm=chisq)


  ;; mpfitfun is usually robust with its errors, so if we
  ;; made it here, it has something useful to say in the
  ;; status variable.  For our purposes, we either want to
  ;; keep what it did, or throw it away
  keep = 1
  case status of
     0: begin
        fmesg = 'ERROR: internal coding error: improper input parameters to mpfit'
        keep = 0
     end
     !pfo.iterstop: begin
        fmesg = 'WARNING: user interrupted fit, KEEPING parameters, but errors can''t be calculated'
        perror=!values.d_nan
     end
     !pfo.iterquit: begin
        fmesg = 'WARNING: user interrupted fit, RESETTING parameters'
        keep = 0
     end
     1: fmesg = 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2)
     2: fmesg = 'which means parameters are not changing by more than XTOL' + strtrim(xtol, 2)
     3: fmesg = 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2) + ' AND the parameters are not changing by more than XTOL' + strtrim(xtol, 2)
     4: fmesg = 'which means the abs value of the cosine of the angle between fvec and any column of the jacobian is at most GTOL=' + strtrim(gtol, 2)
     5: fmesg = 'WARNING: this means MAXITER=' + strtrim(maxiter,2) + ' was reached'
     6: fmesg = 'WARNING: this means FTOL=' + strtrim(ftol,2) + ' is too small no further reduction in the sum of squares is possible.'
     7: fmesg = 'WARNING: this means XTOL=' + strtrim(xtol,2) + ' is too small no further improvement in the approximate solution x is possible.'
     8: fmesg = 'WARNING: this means GTOL=' + strtrim(gtol,2) + ' is too small fvec is orthogonal to the columns of the jacobian to the specified precision.'
     9: message, 'ERROR: code not set up to handle external procedure'
     else: begin
        if status le 0 then begin
           fmesg = 'ERROR: STATUS value le 0, which looks bad.  Resetting parameters to their original values.'
           keep = 0
        endif else begin
           fmesg = 'NOTE: STATUS it is positive, so I am assuming it is OK'
        endelse
     end
  endcase

  if keep then begin
     ;; Set return values in parinfo to fitted params.  This clobbers
     ;; whatever was in parinfo.value and .perror.  If the users
     ;; wans to save these, they can copy parinfo themselves
     parinfo.value = params
     parinfo.error = perror
  endif
  if NOT keyword_set(no_status) then $
    message, /CONTINUE, 'NOTE: mpfitfun returned status ' + strtrim(status, 2) + ' ' + fmesg

end
