;+
; NAME: pfo_mpfit_obj
;
; PURPOSE: Create, initialize, and work with the pfo_mpfit_obj, which
; encapsulates the necessary data and methods to fit fuctions to data
; in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
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
; $Id: pfo_mpfit_obj__define.pro,v 1.1 2011/07/29 13:40:14 jpmorgen Exp $
;
; $Log: pfo_mpfit_obj__define.pro,v $
; Revision 1.1  2011/07/29 13:40:14  jpmorgen
; Initial revision
;
;-

;; Wrapper so that mpfit can call the pfo_obj->deviates method
function pfo_mpfit_obj_kernel, $
   params, $ 		;; Required positional parameter passed by MPFIT
   pfo_obj=pfo_obj, $	;; from MPFIT functargs keyword
   _REF_EXTRA=extra

   init = {pfo_sysvar}
   init = {tok_sysvar}

   ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
   if !pfo.debug le 0 then begin
      ;; Return to the calling routine with our error
      ON_ERROR, !tok.return
      CATCH, err
      if err ne 0 then begin
         CATCH, /CANCEL
         message, /NONAME, !error_state.msg, /CONTINUE
         message, 'ERROR: Caught above error.  Returning NaN to calling routine.'
         return, !values.d_nan
      endif
   endif ;; not debugging

  return, pfo_obj->deviates(params=params, _EXTRA=extra)
end

;; MPFIT Iterproc customized for pfo
forward_function mpfit_call, mpfit_enorm
pro pfo_mpfit_iterproc, $
   myfunct, $ ;; name of kernel function
   params, $ ;; parameters from MPFIT
   iter, $ ;; iteration number
   fnorm, $ ;; return value: sum of squares of deviates
   FUNCTARGS=fcnargs, $ ;; function args to myfunct
   quiet=quiet, $ ;; essentially skips this routine
   iterstop=iterstop, $ ;; allow keyboard entry to stop iteration
   plot=plot, $ ;; call pfo_obj->plot for each iteration
   parinfo=parinfo, $ ;; parinfo (function definition)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, plot methods, etc.
   dof=dof0, $ ;; number of degrees of freedom of fit
   _EXTRA=iterargs ;; arguments to user functions (e.g. pfo_obj-> plot)

  ;; NOTE: Craig calls params, x and myfunct fcn

  ;; COMMON block required by MPFIT.  This is how the signal gets sent
  ;; for termination
  COMMON mpfit_error, mperr
  mperr = 0
  if keyword_set(quiet) then return
  if n_params() EQ 3 then begin
     ;; This is general code from MPFIT, but with "fvec" replaced by "deviates"
     deviates = mpfit_call(myfunct, params, _EXTRA=fcnargs)
     fnorm = mpfit_enorm(deviates)^2
  endif

  if n_elements(dof0) EQ 0 then dof = 1L else dof = floor(dof0(0))

  ;; Print the definitions of the functions used before the first iteration.
  if iter eq 1 then begin
     print, pfo_parinfo_parse(parinfo, params, /print, /param_names_only, pfo_obj=pfo_obj)
  endif
  print, '-------------------------------------------------'
  print, iter, fnorm, dof, $
    format='("Iter ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
  print, '-------------------------------------------------'
  print, pfo_parinfo_parse(parinfo, params, /print, /brief, pfo_obj=pfo_obj)

  if obj_valid(pfo_obj) and keyword_set(plot) then $
     pfo_obj->plot, params=params, _EXTRA=iterargs

;;  if !pfo.plotwin gt 0 and !pfo.plotproc ne '' $
;;    and N_elements(spec) ne 0 then begin
;;     ;; Change to the plotwin with wset.  If that raises an error,
;;     ;; make the window afresh
;;     CATCH, err
;;     if err ne 0 then begin
;;        CATCH, /CANCEL
;;        message, /NONAME, !error_state.msg, /CONTINUE
;;        window, !pfo.plotwin
;;     endif else begin
;;        wset, !pfo.plotwin
;;        call_procedure, !pfo.plotproc, Xin, parinfo, params=params, spec, $
;;                        err_spec, _EXTRA=iterargs
;;        CATCH, /CANCEL
;;     endelse
;;
;;  endif

  if keyword_set(iterstop) then begin
     print, ' '
     message, /CONTINUE, 'PRESS RETURN TO PAUSE AND SHOW MENU NEXT TIME AROUND'

     answer = get_kbrd(0)
     for ki = 0,1000 do flush_input = get_kbrd(0)
     if byte(answer) eq 10 then begin
        print, ' '
        message, /CONTINUE, 'Fitting menu:'
        print, 'Stop fit, saving parameters at present values'
        print, 'Quit fitting, resetting parameters to previous values'
        print, 'do Nothing, keep fitting'

        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'S, Q, [N]'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'N'
           answer = strupcase(answer)
           for ki = 0,1000 do flush_input = get_kbrd(0)
        endrep until $
          answer eq 'S' or $
          answer eq 'Q' or $
          answer eq 'N'

        case answer of 
           'S'	:	mperr = !pfo.iterstop
           'Q'	:	mperr = !pfo.iterquit
           'N'	:	message, /CONTINUE, 'Continuing'
           else	:	message, 'ERROR: internal coding problem'
        endcase

     endif ;; fitting menu
  endif ;; iterstop set in iterargs

  return
end

;; FIT

;; This function interfaces MPFIT to the pfo system.  It is designed
;; to shield the casual user from the details of MPFIT but provide all
;; of the hooks necessary for the advanced user to make full use of
;; MPFIT.  The function returns 1 if successful, where successful
;; depends on some details of how the output of the MPFIT status
;; variable is handled.  If successful, the parinfo.value values are
;; modified to reflect the new fitted parameter values.  If you want
;; access to the old parinfo for the purposes of undo, simply specify
;; the "undo" keyword
function pfo_mpfit_obj::fit, $
   undo=undo, $ ;; (output) parinfo before the fit is returned in this variable
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   mpfit_msg=mpfit_msg, $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
   params, $ ;; starting parameters, if different than parinfo.value
   FUNCTARGS=functargs_in, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   NFEV=nfev, $ ;; (output) number of mpfit_kernel calls
   MAXITER=maxiter, $ ;; maximum number of fit iterations
   mpfit_errmsg=mpfit_errmsg, $ ;; (output) string containing MPFIT output error message
   NPRINT=nprint, $ ;; Iterproc is called every nprint iterations
   mpfit_QUIET=mpfit_quiet, $ ;; MPFIT's quiet keyword
   FTOL=ftol, $ ;; stop criterion for reduction in sum of squares
   XTOL=xtol, $ ;; stop criterion for relative error between two consecutive iterations
   GTOL=gtol, $ ;; stop criterion for cos of angle between fvec and any column of the jacobian 
   NITER=niter, $ ;; (output) number of MPFIT iterations completed
   mpfit_STATUS=mpfit_status, $ ;; (output) MPFIT exit status code
   ITERPROC=iterproc, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   ITERARGS=iterargs_in, $ ;; keyword arguments passed to iterproc
   COVAR=covar, $ ;; (output) covariance matrix for best-fit params
   BESTNORM=bestnorm ;; (output) chi sq = total(deviates^2)

   init = {pfo_sysvar}
   init = {tok_sysvar}

   ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
   if !pfo.debug le 0 then begin
      ;; Return to the calling routine with our error
      ON_ERROR, !tok.return
      CATCH, err
      if err ne 0 then begin
         CATCH, /CANCEL
         message, /NONAME, !error_state.msg, /CONTINUE
         message, 'ERROR: Caught above error.  Returning 0', /CONTINUE
         return, 0
      endif
   endif ;; not debugging

   ;; MPFIT_MAX_STATUS.  Maximum number of fit iterations
   if N_elements(mpfit_max_status) eq 0 then mpfit_max_status = self.mpfit_max_status

   ;; MPFIT_KERNEL.  Let the user select a different kernel function.
   ;; This is one way to connect to a different way of calculating the
   ;; deviates.  The other would be to defined a new pfo_obj that
   ;; would override the deviates method....
   if N_elements(mpfit_kernel) eq 0 then mpfit_kernel = self.mpfit_kernel

   ;; FUNCTARGS.  Handle any extra arguments that the user wants to
   ;; pass to the routines that do the calculations.  Note that the
   ;; externally provided functargs should not include pfo_obj
   functargs = {pfo_obj:self}
   ;; Prefer command-line specified functargs over property (e.g. do
   ;; not concatenate command line and property functargs)
   if N_elements(functargs_in) ne 0 then begin
      pfo_struct_append, functargs, functargs_in
   endif else begin
      if N_elements(*self.pFunctargs) ne 0 then $
         pfo_struct_append, functargs, *self.pFunctargs
   endelse

   ;; MAXITER.  Maximum number of fit iterations
   if N_elements(maxiter) eq 0 then maxiter = self.maxiter

   ;; NPRINT.  Iterproc is called every nprint iterations
   if N_elements(nprint) eq 0 then nprint = self.nprint
   
   ;; MPFIT_QUIET.  MPFIT's quiet keyword
   if N_elements(mpfit_quiet) eq 0 then mpfit_quiet = self.mpfit_quiet
   
   ;; FTOL.  Stop criterion for reduction in sum of squares
   if N_elements(ftol) eq 0 then ftol = self.ftol

   ;; XTOL.  Stop criterion for relative error between two consecutive iterations
   if N_elements(xtol) eq 0 then xtol = self.xtol

   ;; GTOL.  Stop criterion for cos of angle between fvec and any column of the jacobian 
   if N_elements(gtol) eq 0 then gtol = self.gtol

   ;; ITERPROC.  Procedure to call every niter iterations
   if N_elements(iterproc) eq 0 then iterproc = self.iterproc

   ;; ITERARGS.  Handle any extra arguments that the user wants to
   ;; pass to the iterproc.  Note that the externally provided
   ;; iterargs should not include pfo_obj
   iterargs = {pfo_obj:self}
   ;; Prefer command-line specified iterargs over property (e.g. do
   ;; not concatenate command line and property iterargs)
   if N_elements(iterargs_in) ne 0 then begin
      pfo_struct_append, iterargs, iterargs_in
   endif else begin
      if N_elements(*self.pIterargs) ne 0 then $
         pfo_struct_append, iterargs, *self.pIterargs
   endelse

   ;; Initialize output property
   self.nfev = 0
   self.mpfit_errmsg = ''
   self.niter = 0
   self.mpfit_status = !tok.nowhere
   ptr_free, self.pcovar
   self.pcovar = ptr_new(/allocate_heap)
   self.bestnorm = 0D

   new_params = mpfit(mpfit_kernel, params, functargs=functargs, NFEV=nfev, MAXITER=maxiter, ERRMSG=mpfit_errmsg, NPRINT=nprint, QUIET=mpfit_quiet, ftol=ftol, xtol=xtol, gtol=gtol, niter=niter, status=mpfit_status, iterproc=iterproc, iterargs=iterargs, covar=covar, perror=perror, bestnorm=bestnorm, parinfo=*self.pparinfo)

   ;; Save these output keywords into property, if MPFIT provides then
   if N_elements(nfev		) gt 0 then self.nfev 		= nfev
   if N_elements(mpfit_errmsg	) gt 0 then self.mpfit_errmsg 	= mpfit_errmsg
   if N_elements(niter		) gt 0 then self.niter 		= niter
   if N_elements(mpfit_status	) gt 0 then self.mpfit_status 	= mpfit_status

   mpfit_msg = 'mpfit returned status ' + strtrim(mpfit_status, 2) + ' '

   ;; Translate our mpfit_status
   case mpfit_status of
      -18: mpfit_msg += mpfit_errmsg
      -16: mpfit_msg += 'A parameter or function value has become infinite or undefined.  ' + mpfit_errmsg
      !pfo.iterstop: begin
         mpfit_msg += 'WARNING: user interrupted fit, KEEPING parameters, but errors can''t be calculated'
         (*self.pparinfo).value = new_params
      end
      !pfo.iterquit: mpfit_msg += 'WARNING: user interrupted fit, RESETTING parameters'
      !tok.nowhere:  mpfit_msg += 'ERROR: fit does not seem to have been attempted!'
      0: mpfit_msg += mpfit_errmsg ;; (bad input to mpfit)
      1: mpfit_msg += 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2)
      2: mpfit_msg += 'which means parameters are not changing by more than XTOL' + strtrim(xtol, 2)
      3: mpfit_msg += 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2) + ' AND the parameters are not changing by more than XTOL' + strtrim(xtol, 2)
      4: mpfit_msg += 'which means the abs value of the cosine of the angle between fvec and any column of the jacobian is at most GTOL=' + strtrim(gtol, 2)
      5: mpfit_msg += 'WARNING: this means MAXITER=' + strtrim(maxiter,2) + ' was reached'
      6: mpfit_msg += 'WARNING: this means FTOL=' + strtrim(ftol,2) + ' is too small no further reduction in the sum of squares is possible.'
      7: mpfit_msg += 'WARNING: this means XTOL=' + strtrim(xtol,2) + ' is too small no further improvement in the approximate solution x is possible.'
      8: mpfit_msg += 'WARNING: this means GTOL=' + strtrim(gtol,2) + ' is too small fvec is orthogonal to the columns of the jacobian to the specified precision.'
      9: message, 'ERROR: code not set up to handle external procedure'
      else: begin
         if status le 0 then begin
            mpfit_msg += 'ERROR: STATUS value is in the range of a user-defined error'
         endif
      end
   endcase

   ;; Put our mpfit_msg into our property
   self.mpfit_msg = mpfit_msg

   ;; Check to see if our fit had some problem
   if mpfit_status le 0 or mpfit_status gt mpfit_max_status then $
      return, 0

   ;; If we made it here, we have a good fit

   ;; First save off our old parinfo, if desired
   if arg_present(undo) or N_elements(undo) ne 0 then $
      undo = *self.pparinfo
   ;; Put our new params and errors into the parinfo.  Try this two
   ;; ways so that we can (1) test it and (2) use
   ;; its code to invalidate our cache.  The commented out line is
   ;; slower but more concise
   ;;self->parinfo_call_procedure, 'pfo_struct_setget_tag', /set, value=new_params, error=perror, pfo_obj=self
   (*self.pparinfo).value = new_params
   (*self.pparinfo).error = perror
   self->invalidate_cache

   *self.pcovar = covar
   self.bestnorm = bestnorm
   ;; Return 1 to indicate successful fit
   return, 1

end


;;
pro pfo_mpfit_obj::get_property, $
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   mpfit_msg=mpfit_msg, $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
   functargs=functargs, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   nfev=nfev, $ ;; (output) number of mpfit_kernel calls
   maxiter=maxiter, $ ;; maximum number of fit iterations
   mpfit_errmsg=mpfit_errmsg, $ ;; string containing MPFIT output error message
   nprint=nprint, $ ;; Iterproc is called every nprint iterations
   mpfit_quiet=mpfit_quiet, $ ;; MPFIT's quiet keyword
   ftol=ftol, $ ;; Stop criterion for reduction in sum of squares
   xtol=xtol, $ ;; Stop criterion for relative error between two consecutive iterations
   gtol=gtol, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian
   niter=niter, $ ;; (output) number of MPFIT iterations completed
   mpfit_status=mpfit_status, $ ;; (output) MPFIT exit status code
   iterproc=iterproc, $ ;; Procedure to call every niter iterations
   iterargs=iterargs, $ ;; keyword arguments passed to iterproc
   covar=covar, $ ;; (output) covariance matrix for best-fit params
   bestnorm=bestnorm, $ ;; (output) chi sq = total(deviates^2)
   _REF_EXTRA=extra

  if arg_present(mpfit_max_status)or N_elements(mpfit_max_status) gt 0 then mpfit_max_status 	= self.mpfit_max_status
  if arg_present(mpfit_kernel	) or N_elements(mpfit_kernel	) gt 0 then mpfit_kernel 	= self.mpfit_kernel
  if arg_present(mpfit_msg	) or N_elements(mpfit_msg	) gt 0 then mpfit_msg		= self.mpfit_msg
  if arg_present(functargs	) or N_elements(functargs	) gt 0 then functargs		= *self.pFunctargs
  if arg_present(nfev		) or N_elements(nfev		) gt 0 then nfev		= self.nfev
  if arg_present(maxiter	) or N_elements(maxiter		) gt 0 then maxiter		= self.maxiter
  if arg_present(mpfit_errmsg	) or N_elements(mpfit_errmsg	) gt 0 then mpfit_errmsg	= self.mpfit_errmsg
  if arg_present(nprint		) or N_elements(nprint		) gt 0 then nprint		= self.nprint
  if arg_present(mpfit_quiet	) or N_elements(mpfit_quiet	) gt 0 then mpfit_quiet		= self.mpfit_quiet
  if arg_present(ftol	  	) or N_elements(ftol		) gt 0 then ftol		= self.ftol
  if arg_present(xtol	  	) or N_elements(xtol		) gt 0 then xtol		= self.xtol
  if arg_present(gtol	  	) or N_elements(gtol		) gt 0 then gtol		= self.gtol
  if arg_present(nite		) or N_elements(niter		) gt 0 then niter 		= self.niter
  if arg_present(mpfit_status	) or N_elements(mpfit_status	) gt 0 then mpfit_status 	= self.mpfit_status
  if arg_present(iterproc	) or N_elements(iterproc	) gt 0 then iterproc 		= self.iterproc
  if arg_present(iterargs	) or N_elements(iterargs	) gt 0 then iterargs 		= *self.pIterargs
  if arg_present(covar		) or N_elements(covar		) gt 0 then covar 		= *self.pcovar
  if arg_present(bestnorm	) or N_elements(bestnorm	) gt 0 then bestnorm 		= self.bestnorm

  self->pfo_calc_obj::get_property, _EXTRA=extra

end

;; 
pro pfo_mpfit_obj::set_property, $
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   functargs=functargs, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   maxiter=maxiter, $ ;; maximum number of fit iterations
   nprint=nprint, $ ;; Iterproc is called every nprint iterations
   mpfit_quiet=mpfit_quiet, $ ;; MPFIT's quiet keyword
   ftol=ftol, $ ;; Stop criterion for reduction in sum of squares
   xtol=xtol, $ ;; Stop criterion for relative error between two consecutive iterations
   gtol=gtol, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian 
   iterproc=iterproc, $ ;; Procedure to call every niter iterations
   iterargs=iterargs, $ ;; keyword arguments passed to iterproc
   bestnorm=bestnorm, $ ;; (output) chi sq = total(deviates^2)
   _REF_EXTRA=extra

  if N_elements(mpfit_max_status) gt 0 then self.mpfit_max_status 	= mpfit_max_status
  if N_elements(mpfit_kernel	) gt 0 then self.mpfit_kernel 		= mpfit_kernel
  if N_elements(functargs	) gt 0 then *self.pfunctargs 		= functargs
  if N_elements(maxiter		) gt 0 then self.maxiter 		= maxiter
  if N_elements(nprint		) gt 0 then self.nprint 		= nprint
  if N_elements(mpfit_quiet	) gt 0 then self.mpfit_quiet 		= mpfit_quiet
  if N_elements(ftol		) gt 0 then self.ftol 			= ftol
  if N_elements(xtol		) gt 0 then self.xtol 			= xtol
  if N_elements(gtol		) gt 0 then self.gtol 			= gtol
  if N_elements(iterproc	) gt 0 then self.iterproc 		= iterproc
  if N_elements(iterargs	) gt 0 then *self.piterargs 		= iterargs
  if N_elements(bestnorm	) gt 0 then self.bestnorm 		= bestnorm

  ;; Pass everything onto inherited routines
  self->pfo_calc_obj::set_property, _EXTRA=extra

end

;; Each inherited class should have a descr method.
function pfo_mpfit_obj::descr

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.', /CONTINUE
        return, 'Not documented.'
     endif
  endif ;; not debugging

  descr = *self.ppfo_mpfit_obj_descr
  pfo_struct_append, descr, {pfo_calc_obj: self->pfo_calc_obj::descr()}
  return, descr

end

pro pfo_mpfit_obj::cleanup
  ptr_free, self.ppfo_mpfit_obj_descr
  ptr_free, self.pFunctargs
  ptr_free, self.pIterargs
  ptr_free, self.pcovar

  self->pfo_calc_obj::cleanup
end

function pfo_mpfit_obj::init, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}
  
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object not properly initialized ', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Create our documentation string
  self.ppfo_mpfit_obj_descr $
     = ptr_new( $
     {README	: 'pfo_mpfit_obj encapsulates all of the shared information necessary for fitting PFO functions to data using MPFIT.  NO GRAPHICAL INTERFACE IS ASSUMED.', $
      SUPERCLASSES:'pfo_calc_obj', $
      PROPERTY	: 'mpfit_kernel', $
      METHODS	: ''} $
              )

  ;; This is a default value that is unlikely to change
  self.mpfit_kernel = 'pfo_mpfit_obj_kernel'
  ;; Start by accepting pretty much any positive MPFIT termination code
  self.mpfit_max_status = 8
  ;; From MPFIT
  self.maxiter = 200
  self.nprint = 1
  self.ftol = 1D-10
  self.xtol = 1D-10
  self.gtol = 1D-10
  ;; We we !tok.nowhere to indicate that MPFIT has not been run yet in
  ;; this object.
  self.mpfit_status = !tok.nowhere
  ;; --> eventually this is going to be pfo_iterproc, which will then
  ;; be beefed up to a pfo_obj_iterproc or something like that
  self.iterproc = 'pfo_mpfit_iterproc'  

  ;; Turn our null reference pointers into undefined variables
  self.pFunctargs = ptr_new(/allocate_heap)
  self.pIterargs = ptr_new(/allocate_heap)
  self.pcovar = ptr_new(/allocate_heap)

  ;; Call our set_property routine to convert any keywords to property
  self->set_property, _EXTRA=extra

  ;; Call our superclass init methods
  ok = self->pfo_calc_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then $
     return, 0
  return, 1

end

pro pfo_mpfit_obj__define
  objectClass = $
     {pfo_mpfit_obj, $
      ppfo_mpfit_obj_descr: ptr_new(), $  ;; Pointer to description structure
      mpfit_max_status	: 0B, $ ;; maximum value of MPFIT output status code
      mpfit_kernel	: '', $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
      mpfit_msg		: '', $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
      pFunctargs	: ptr_new(), $ ;; pointer to arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
      nfev		: 0L, $ ;; (output) number of calls to kernel function
      maxiter		: 0L, $ ;; maximum number of fit iterations
      mpfit_errmsg	: '', $ ;; string containing MPFIT output error message
      nprint		: 0L, $ ;; Iterproc is called every nprint iterations
      mpfit_quiet	: 0B, $ ;; MPFIT's quiet keyword
      ftol		: 0D, $ ;; Stop criterion for reduction in sum of squares
      xtol		: 0D, $ ;; Stop criterion for relative error between two consecutive iterations
      gtol		: 0D, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian 
      niter		: 0L, $ ;; (output) number of MPFIT iterations completed
      mpfit_status	: 0,  $ ;; (output) MPFIT exit status code
      iterproc		: '', $ ;; Procedure to call every niter iterations
      pIterargs		: ptr_new(), $ ;; pointer to keyword arguments passed to iterproc
      pcovar		: ptr_new(), $ ;; (output) pointer to covariance matrix for best-fit params
      bestnorm		: 0D, $ ;; (output) chi sq = total(deviates^2)
      inherits pfo_calc_obj $	;; pfo_calc has data and parinfo
     }
end
