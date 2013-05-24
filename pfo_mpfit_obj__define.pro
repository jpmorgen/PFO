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
; $Id: pfo_mpfit_obj__define.pro,v 1.9 2013/05/24 22:41:48 jpmorgen Exp $
;
; $Log: pfo_mpfit_obj__define.pro,v $
; Revision 1.9  2013/05/24 22:41:48  jpmorgen
; WOrkin with backgroun idx, give to Ron
;
; Revision 1.8  2012/01/13 20:55:38  jpmorgen
; Change keyword for disambiguation
;
; Revision 1.7  2011/11/18 15:33:13  jpmorgen
; Change call to pfo_parinfo_parse, add some MPFIT keywords to property
;
; Revision 1.6  2011/09/08 19:59:36  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.5  2011/09/01 22:13:26  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.4  2011/08/02 18:21:23  jpmorgen
; Release to Tom
; Fix property name
;
; Revision 1.3  2011/08/02 15:39:58  jpmorgen
; Release to Tom
; Improved property names
;
; Revision 1.2  2011/08/01 19:18:16  jpmorgen
; *** empty log message ***
;
; Revision 1.1  2011/07/29 13:40:14  jpmorgen
; Initial revision
;
;-

;; Wrapper so that mpfit can call the pfo_obj->deviates method
function pfo_mpfit_obj_kernel, $
   params, $ 		;; Required positional parameter passed by MPFIT

   pfo_obj=pfo_obj, $	;; from MPFIT functargs keyword
   _REF_EXTRA=extra ;; keyword arguments passed to pfo_obj->deviates()

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

  ;; The hide_NAN and hide_infinity flags (if present) are passed to
  ;; pfo_obj->deviates() via _EXTRA.  These flags can help keep MPFIT
  ;; ignore the regions between ROIs, ignore points with Yerr=0, and
  ;; avoid areas where the parinfo function becomes infinite.  The
  ;; last of these might generate some strange results, since the
  ;; effective number of datapoints may change drastically.
  return, pfo_obj->deviates(params=params, _EXTRA=extra)
end

;; MPFIT Iterproc customized for pfo
forward_function mpfit_call, mpfit_enorm
pro pfo_mpfit_obj_iterproc, $
   myfunct, $ ;; name of kernel function
   params, $ ;; parameters from MPFIT
   iter, $ ;; iteration number
   fnorm, $ ;; return value: sum of squares of deviates
   FUNCTARGS=fcnargs, $ ;; function args to myfunct
   keyboard_iterstop=keyboard_iterstop, $ ;; allow keyboard entry to stop iteration
   iterstop_widget=iterstop_widget, $ ;; raise a widget that allows the user to stop the fit
   plot=plot, $ ;; call pfo_obj->plot for each iteration
   widget=widget, $ ;; call pfo_obj->widget_refresh for each iteration
   quiet=quiet, $ ;; no printed output
   parinfo=parinfo, $ ;; parinfo (function definition)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, plot methods, etc.
   dof=dof0, $ ;; number of degrees of freedom of fit
   _EXTRA=iterargs ;; arguments to user functions (e.g. pfo_obj-> plot)

  
  ;; If we have a pfo_obj, we can plot and refresh the widget
  if obj_valid(pfo_obj) then begin
     if keyword_set(plot) then $
        pfo_obj->plot, params=params, _EXTRA=iterargs
     if keyword_set(widget) then $
        pfo_obj->refresh, params=params, _EXTRA=iterargs
  endif ;; working from pfo_obj

  ;; Print the definitions of the functions used before the first iteration.
  if keyword_set(iterstop_widget) and iter eq 1 then begin
     ;; Create mpfit_itrestop widget
     message, 'ERROR: --> code not written yet.'
  endif

  ;; The rest of this routine is for printing
  if keyword_set(quiet) then $
     return

  ;; COMMON block required by MPFIT.  This is how the signal gets sent
  ;; for termination

  COMMON mpfit_error, mperr
  mperr = 0
  ;; Calculate fnorm, if we need to (code from MPFIT) NOTE: Craig
  ;; calls "params" "x", "myfunct" "fcn," and "deviates" "fvec"
  if n_params() EQ 3 then begin
     ;; This is general code from MPFIT.  mpfit_enorm does things
     ;; faster than standard calcs
     deviates = mpfit_call(myfunct, params, _EXTRA=fcnargs)
     fnorm = mpfit_enorm(deviates)^2
  endif

  if n_elements(dof0) EQ 0 then dof = 1L else dof = floor(dof0(0))

  ;; Print the definitions of the functions used before the first iteration.
  if iter eq 1 then begin
     print, pfo_parinfo_parse(parinfo=parinfo, params=params, /print, /param_names_only, pfo_obj=pfo_obj, _EXTRA=fcnargs)
  endif
  print, '-------------------------------------------------'
  print, iter, fnorm, dof, $
         format='("Iter ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
  print, '-------------------------------------------------'
  print, pfo_parinfo_parse(parinfo=parinfo, params=params, /print, /brief, pfo_obj=pfo_obj, _EXTRA=fcnargs)

  if keyword_set(keyboard_iterstop) then begin
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
   quiet=quiet, $ ;; suppress all printed output from this routine (finer control available with mpfit_quiet)
   mpfit_QUIET=mpfit_quiet, $ ;; MPFIT's quiet keyword.  Unless /quiet is used, this routine still ouputs the mpfit status message
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   mpfit_msg=mpfit_msg, $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
   params, $ ;; starting parameters, if different than parinfo.value
   FUNCTARGS=functargs_in, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   hide_NAN=hide_NAN, $ ;; Hide instances of NAN from calculation of deviates (default = yes or property value)
   hide_infinity=hide_infinity, $ ;; Hide instances of NAN from calculation of deviates (default = yes or property value)
   NFEV=nfev, $ ;; (output) number of mpfit_kernel calls
   MAXITER=maxiter, $ ;; maximum number of fit iterations
   mpfit_errmsg=mpfit_errmsg, $ ;; (output) string containing MPFIT output error message
   NPRINT=nprint, $ ;; Iterproc is called every nprint iterations
   FTOL=ftol, $ ;; stop criterion for reduction in sum of squares
   XTOL=xtol, $ ;; stop criterion for relative error between two consecutive iterations
   GTOL=gtol, $ ;; stop criterion for cos of angle between fvec and any column of the jacobian 
   NITER=niter, $ ;; (output) number of MPFIT iterations completed
   mpfit_STATUS=mpfit_status, $ ;; (output) MPFIT exit status code
   ITERPROC=iterproc, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   keyboard_iterstop=keyboard_iterstop, $ ;; allow keyboard entry to stop iteration
   iterstop_widget=iterstop_widget, $ ;; raise a widget that allows the user to stop the fit
   plot=plot, $ ;; plot is drawn every niter
   parinfo_widget=parinfo_widget, $ ;; refresh parinfo widgets every niter
   ITERARGS=iterargs_in, $ ;; other keyword arguments passed to iterproc
   COVAR=covar, $ ;; (output) covariance matrix for best-fit params
   BESTNORM=bestnorm, $ ;; (output) chi sq = total(deviates^2)
   NFREE=nfree, $ ;; (output) number of free parameters
   npegged=npegged, $ ;; (output) number of parameters pegged
   background=background, $ ;; fit only background functions (and X-axis transformations)
   idx=idx, $ ;; 
   ispec=ispec, $ ;;
   iROI=iROI, $ ;;
   _REF_EXTRA=extra

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

  ;; HIDE_INFINITY.  Determines if we should hide instances of
  ;; infinity from our deviates.  Default is !tok.yes, since then we
  ;; can use Yerr = 0 to mark missing data points.  Also is a little
  ;; polite to functions that are blowing up on portions of their
  ;; ranges.  You may wish to use !tok.no, 
  if N_elements(hide_infinity) eq 0 then hide_infinity = self.fit_hide_infinity

  ;; HIDE NAN.  Like hide_infinity, but a little more fundamental to
  ;; PFO.  The Yaxis is initialized to NAN.  If you use ROIs,
  ;; you won't calculate Yaxis between ROIs.  Leaving this at
  ;; the default value of !tok.yes assures that these points are left
  ;; out of the deviates so
  if N_elements(hide_NAN) eq 0 then hide_NAN = self.fit_hide_NAN
  
  ;; FUNCTARGS.  These are the arguments to the MPFIT kernel function,
  ;; which basically just calls self->deviates() with all these
  ;; keywords except self.
  functargs = {pfo_obj:self, hide_infinity:hide_infinity, hide_NAN:hide_NAN}

  ;; Allow user to add _either_ background idx (including any X-axis
  ;; transformations) with /background _or_ their own customized
  ;; ispec, iROI, idx
  if keyword_set(background) then begin
     ;; Pass pfo_obj and possible instances of ispec, iROI and idx to
     ;; back_idx this way
     ;; Get our background idx
     back_idx = pfo_back_idx(_EXTRA=functargs)
     ;; If there is no background, do a normal fit
     if back_idx[0] ne !tok.nowhere then begin
        ;; We have a background.  Make sure we include any X-axis
        ;; transformations.
        self->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, _EXTRA=extra, $
           taglist_series='pfo', outaxis=outaxis, fop=fop
        X_idx = where(outaxis eq !pfo.Xaxis, count)
        if count ne 0 then $
           pfo_array_append, back_idx, X_idx
        ;; Add in any functions that don't appear to operate on
        ;; anything, but might affect us (e.g. ROIs)
        more_idx = where(fop eq !pfo.none, count)
        if count ne 0 then $
           pfo_array_append, back_idx, more_idx
        ;; Put our back_idx and any helpers in the functargs
        pfo_struct_append, functargs, {idx:back_idx}
     endif ;; background idx found
  endif else begin
     ;; Allow user to limit by ispec, iROI and idx
     if N_elements(ispec) ne 0 then $
        pfo_struct_append, functargs, {ispec:ispec}
     if N_elements(iROI) ne 0 then $
        pfo_struct_append, functargs, {iROI:iROI}
     if N_elements(idx) ne 0 then $
        pfo_struct_append, functargs, {idx:idx}
  endelse ;; ispec, iROI, idx


  ;; Prefer command-line specified functargs over property (e.g. do
  ;; not concatenate command line and property functargs)
  if N_elements(functargs_in) ne 0 then begin
     pfo_struct_append, functargs, functargs_in
  endif else begin
     if N_elements(*self.pmpfit_functargs) ne 0 then $
        pfo_struct_append, functargs, *self.pmpfit_functargs
  endelse

  ;; MAXITER.  Maximum number of fit iterations
  if N_elements(maxiter) eq 0 then maxiter = self.mpfit_maxiter

  ;; NPRINT.  Iterproc is called every nprint iterations
  if N_elements(nprint) eq 0 then nprint = self.mpfit_nprint
  
  ;; MPFIT_QUIET.  MPFIT's quiet keyword
  if N_elements(mpfit_quiet) eq 0 then mpfit_quiet = self.mpfit_quiet

  ;; QUIET (only for this routine)
  if keyword_set(quiet) then $
     mpfit_quiet = 1
  
  ;; FTOL.  Stop criterion for reduction in sum of squares
  if N_elements(ftol) eq 0 then ftol = self.mpfit_ftol

  ;; XTOL.  Stop criterion for relative error between two consecutive iterations
  if N_elements(xtol) eq 0 then xtol = self.mpfit_xtol

  ;; GTOL.  Stop criterion for cos of angle between fvec and any column of the jacobian 
  if N_elements(gtol) eq 0 then gtol = self.mpfit_gtol

  ;; ITERPROC.  Procedure to call every niter iterations
  if N_elements(iterproc) eq 0 then iterproc = self.mpfit_iterproc

  ;; KEYBOARD_ITERSTOP: allow keyboard entry to stop iteration
  if N_elements(keyboard_iterstop) eq 0 then keyboard_iterstop = self.mpfit_keyboard_iterstop

  ;; ITERSTOP_WIDGET: raises a widget that will stop iteration
  if N_elements(iterstop_widget) eq 0 then iterstop_widget = self.mpfit_iterstop_widget

  ;; PLOT plot is drawn every niter
  if N_elements(plot) eq 0 then plot = self.mpfit_plot

  ;; PARINFO_WIDGET: refresh parinfo widgets every niter
  if N_elements(parinfo_widget) eq 0 then parinfo_widget = self.mpfit_parinfo_widget

  ;; ITERARGS.  These get passed to iterproc.  We have several a
  ;; couple we deal with ourselves.
  iterargs = {pfo_obj:self, $
              keyboard_iterstop:keyboard_iterstop, $
              iterstop_widget:iterstop_widget, $
              plot:plot, $
              parinfo_widget:parinfo_widget}
  ;; Concatenate command line and property iterargs
  if N_elements(iterargs_in) ne 0 then $
     pfo_struct_append, iterargs, iterargs_in
  if N_elements(*self.pmpfit_Iterargs) ne 0 then $
     pfo_struct_append, iterargs, *self.pmpfit_Iterargs

  ;; Initialize output property
  self.mpfit_nfev = 0
  self.mpfit_errmsg = ''
  self.mpfit_niter = 0
  self.mpfit_status = !tok.nowhere
  ptr_free, self.pmpfit_covar
  self.pmpfit_covar = ptr_new(/allocate_heap)
  self.mpfit_bestnorm = 0D
  self.mpfit_nfree = 0
  self.mpfit_npegged = 0

  new_params = mpfit(mpfit_kernel, params, functargs=functargs, NFEV=nfev, MAXITER=maxiter, ERRMSG=mpfit_errmsg, NPRINT=nprint, QUIET=mpfit_quiet, ftol=ftol, xtol=xtol, gtol=gtol, niter=niter, status=mpfit_status, iterproc=iterproc, iterargs=iterargs, covar=covar, perror=perror, bestnorm=bestnorm, nfree=nfree, npegged=npegged, parinfo=*self.pparinfo, _EXTRA=extra)

  ;; Save these output keywords into property, if MPFIT provides then
  if N_elements(nfev		) gt 0 then self.mpfit_nfev 	= nfev
  if N_elements(mpfit_errmsg	) gt 0 then self.mpfit_errmsg 	= mpfit_errmsg
  if N_elements(niter		) gt 0 then self.mpfit_niter	= niter
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
     2: mpfit_msg += 'which means parameters are not changing by more than XTOL=' + strtrim(xtol, 2)
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

  ;; Display the mpfit_msg, unless the user has asked not to
  if NOT keyword_set(quiet) then $
     message, /CONTINUE, mpfit_msg

  ;; Check to see if our fit had some problem
  if mpfit_status le 0 or mpfit_status gt mpfit_max_status then $
     return, 0

  ;; If we made it here, we have a good fit
  
  ;; Prepare our undo, which is necessary for deciding if we are going
  ;; to repopulate or refresh
  self->prepare_update, undo

  ;; Put our new params and errors into the parinfo.
  (*self.pparinfo).value = new_params
  (*self.pparinfo).error = perror
  
  ;; Run the update method, which checks the new parinfo and refreshes
  ;; or repopulates the display
  self->update, undo, /save_undo

  *self.pmpfit_covar = covar
  self.mpfit_bestnorm = bestnorm
  self.mpfit_nfree = nfree
  self.mpfit_npegged = npegged

  ;; Return 1 to indicate successful fit
  return, 1

end


;;
pro pfo_mpfit_obj::get_property, $
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   mpfit_msg=mpfit_msg, $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
   mpfit_functargs=mpfit_functargs, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   mpfit_hide_NAN=mpfit_hide_NAN, $ ;; Hide instances of NAN from calculation of deviates (default = yes or property value)
   mpfit_hide_infinity=mpfit_hide_infinity, $ ;; Hide instances of infinity from calculation of deviates (default = yes or property value)
   mpfit_nfev=mpfit_nfev, $ ;; (output) number of mpfit_kernel calls
   mpfit_maxiter=mpfit_maxiter, $ ;; maximum number of fit iterations
   mpfit_errmsg=mpfit_errmsg, $ ;; string containing MPFIT output error message
   mpfit_nprint=mpfit_nprint, $ ;; Iterproc is called every nprint iterations
   mpfit_quiet=mpfit_quiet, $ ;; MPFIT's quiet keyword
   mpfit_ftol=mpfit_ftol, $ ;; Stop criterion for reduction in sum of squares
   mpfit_xtol=mpfit_xtol, $ ;; Stop criterion for relative error between two consecutive iterations
   mpfit_gtol=mpfit_gtol, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian
   mpfit_niter=mpfit_niter, $ ;; (output) number of MPFIT iterations completed
   mpfit_status=mpfit_status, $ ;; (output) MPFIT exit status code
   mpfit_iterproc=mpfit_iterproc, $ ;; Procedure to call every niter iterations
   mpfit_keyboard_iterstop=mpfit_keyboard_iterstop, $ ;; allow keyboard entry to stop iteration
   mpfit_iterstop_widget=mpfit_iterstop_widget, $ ;; raise a widget that allows the user to stop the fit
   mpfit_plot=mpfit_plot, $ ;; plot is drawn every niter
   mpfit_parinfo_widget=mpfit_parinfo_widget, $ ;; refresh parinfo widgets every niter
   mpfit_iterargs=mpfit_iterargs, $ ;; other keyword arguments passed to iterproc
   mpfit_covar=mpfit_covar, $ ;; (output) covariance matrix for best-fit params
   mpfit_bestnorm=mpfit_bestnorm, $ ;; (output) chi sq = total(deviates^2)
   mpfit_nfree=mpfit_nfree, $ ;; (output) number of free parameters
   mpfit_npegged=mpfit_npegged, $ ;; (output) number of parameters pegged at limits
   _REF_EXTRA=extra

  if arg_present(mpfit_max_status)or N_elements(mpfit_max_status) gt 0 then mpfit_max_status 	= self.mpfit_max_status
  if arg_present(mpfit_kernel	) or N_elements(mpfit_kernel	) gt 0 then mpfit_kernel 	= self.mpfit_kernel
  if arg_present(mpfit_msg	) or N_elements(mpfit_msg	) gt 0 then mpfit_msg		= self.mpfit_msg
  if arg_present(mpfit_functargs) or N_elements(mpfit_functargs ) gt 0 then mpfit_functargs	= *self.pmpfit_functargs
  if arg_present(hide_NAN	) or N_elements(hide_NAN	) gt 0 then hide_NAN		= self.fit_hide_NAN
  if arg_present(hide_infinity	) or N_elements(hide_infinity	) gt 0 then hide_infinity	= self.fit_hide_infinity
  if arg_present(mpfit_nfev	) or N_elements(mpfit_nfev	) gt 0 then mpfit_nfev		= self.mpfit_nfev
  if arg_present(mpfit_maxiter	) or N_elements(mpfit_maxiter	) gt 0 then mpfit_maxiter	= self.mpfit_maxiter
  if arg_present(mpfit_errmsg	) or N_elements(mpfit_errmsg	) gt 0 then mpfit_errmsg	= self.mpfit_errmsg
  if arg_present(mpfit_nprint	) or N_elements(mpfit_nprint	) gt 0 then mpfit_nprint	= self.mpfit_nprint
  if arg_present(mpfit_quiet	) or N_elements(mpfit_quiet	) gt 0 then mpfit_quiet		= self.mpfit_quiet
  if arg_present(mpfit_ftol	) or N_elements(mpfit_ftol	) gt 0 then mpfit_ftol		= self.mpfit_ftol
  if arg_present(mpfit_xtol	) or N_elements(mpfit_xtol	) gt 0 then mpfit_xtol		= self.mpfit_xtol
  if arg_present(mpfit_gtol	) or N_elements(mpfit_gtol	) gt 0 then mpfit_gtol		= self.mpfit_gtol
  if arg_present(mpfit_niter	) or N_elements(mpfit_niter	) gt 0 then mpfit_niter		= self.mpfit_niter
  if arg_present(mpfit_status	) or N_elements(mpfit_status	) gt 0 then mpfit_status 	= self.mpfit_status
  if arg_present(mpfit_iterproc	) or N_elements(mpfit_iterproc	) gt 0 then mpfit_iterproc 	= self.mpfit_iterproc
  if arg_present(mpfit_keyboard_iterstop) or N_elements(mpfit_keyboard_iterstop) gt 0 then mpfit_keyboard_iterstop= self.mpfit_keyboard_iterstop
  if arg_present(mpfit_iterstop_widget) or N_elements(mpfit_iterstop_widget) gt 0 then mpfit_iterstop_widget= self.mpfit_iterstop_widget
  if arg_present(mpfit_plot	) or N_elements(mpfit_plot	) gt 0 then mpfit_plot 		= self.mpfit_plot
  if arg_present(mpfit_parinfo_widget) or N_elements(mpfit_parinfo_widget) gt 0 then mpfit_parinfo_widget= self.mpfit_parinfo_widget
  if arg_present(mpfit_iterargs	) or N_elements(mpfit_iterargs	) gt 0 then mpfit_iterargs 	= *self.pmpfit_Iterargs
  if arg_present(mpfit_covar	) or N_elements(mpfit_covar	) gt 0 then mpfit_covar 	= *self.pmpfit_covar
  if arg_present(mpfit_bestnorm	) or N_elements(mpfit_bestnorm	) gt 0 then mpfit_bestnorm	= self.mpfit_bestnorm
  if arg_present(mpfit_nfree	) or N_elements(mpfit_nfree	) gt 0 then mpfit_nfree	= self.mpfit_nfree
  if arg_present(mpfit_npegged	) or N_elements(mpfit_npegged	) gt 0 then mpfit_npegged	= self.mpfit_npegged

  self->pfo_calc_obj::get_property, _EXTRA=extra

end

;; 
pro pfo_mpfit_obj::set_property, $
   mpfit_max_status=mpfit_max_status, $ ;; maximum value of MPFIT output status code
   mpfit_kernel=mpfit_kernel, $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
   mpfit_functargs=mpfit_functargs, $ ;; arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
   fit_hide_NAN=fit_hide_NAN, $ ;; Hide instances of NAN from calculation of deviates (default = yes or property value)
   fit_hide_infinity=fit_hide_infinity, $ ;; Hide instances of infinity from calculation of deviates (default = yes or property value)
   mpfit_maxiter=mpfit_maxiter, $ ;; maximum number of fit iterations
   mpfit_nprint=mpfit_nprint, $ ;; Iterproc is called every nprint iterations
   mpfit_quiet=mpfit_quiet, $ ;; MPFIT's quiet keyword
   mpfit_ftol=fit_ftol, $ ;; Stop criterion for reduction in sum of squares
   mpfit_xtol=fit_xtol, $ ;; Stop criterion for relative error between two consecutive iterations
   mpfit_gtol=fit_gtol, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian 
   mpfit_iterproc=mpfit_iterproc, $ ;; Procedure to call every niter iterations
   mpfit_keyboard_iterstop=mpfit_keyboard_iterstop, $ ;; allow keyboard entry to stop iteration
   mpfit_iterstop_widget=mpfit_iterstop_widget, $ ;; raise a widget that allows the user to stop the fit
   mpfit_plot=mpfit_plot, $ ;; plot is drawn every niter
   mpfit_parinfo_widget=mpfit_parinfo_widget, $ ;; refresh parinfo widgets every niter
   mpfit_iterargs=mpfit_iterargs, $ ;; other keyword arguments passed to iterproc
   mpfit_bestnorm=mpfit_bestnorm, $ ;; (output) chi sq = total(deviates^2)
   mpfit_nfree=mpfit_nfree, $ ;; (output) number of free parameters
   mpfit_npegged=mpfit_npegged, $ ;; (output) number of parameters pegged at limits
   Yerr=Yerr, $ ;; override pfo_data_obj handling of Yerr in order to print message about deviates
   keep_local=keep_local, $ ;; keep local during init
   _REF_EXTRA=extra

  if N_elements(mpfit_max_status) gt 0 then self.mpfit_max_status 	= mpfit_max_status
  if N_elements(mpfit_kernel	) gt 0 then self.mpfit_kernel 		= mpfit_kernel
  if N_elements(mpfit_functargs	) gt 0 then *self.pfunctargs 		= mpfit_functargs
  if N_elements(hide_NAN	) gt 0 then self.fit_hide_NAN 		= fit_hide_NAN
  if N_elements(hide_infinity	) gt 0 then self.fit_hide_infinity 	= fit_hide_infinity
  if N_elements(maxiter		) gt 0 then self.mpfit_maxiter 		= mpfit_maxiter
  if N_elements(nprint		) gt 0 then self.mpfit_nprint 		= mpfit_nprint
  if N_elements(mpfit_quiet	) gt 0 then self.mpfit_quiet 		= mpfit_quiet
  if N_elements(mpfit_ftol	) gt 0 then self.mpfit_ftol 		= mpfit_ftol
  if N_elements(mpfit_xtol	) gt 0 then self.mpfit_xtol 		= mpfit_xtol
  if N_elements(mpfit_gtol	) gt 0 then self.mpfit_gtol 		= mpfit_gtol
  if N_elements(mpfit_iterproc	) gt 0 then self.mpfit_iterproc 	= mpfit_iterproc
  if N_elements(mpfit_keyboard_iterstop) gt 0 then self.mpfit_keyboard_iterstop	= mpfit_keyboard_iterstop
  if N_elements(mpfit_iterstop_widget) gt 0 then self.mpfit_iterstop_widget	= mpfit_iterstop_widget
  if N_elements(mpfit_plot	) gt 0 then self.mpfit_plot	 	= mpfit_plot
  if N_elements(mpfit_parinfo_widget) gt 0 then self.mpfit_parinfo_widget= mpfit_parinfo_widget
  if N_elements(mpfit_iterargs	) gt 0 then *self.piterargs 		= mpfit_iterargs
  if N_elements(mpfit_bestnorm	) gt 0 then self.bestnorm 		= mpfit_bestnorm
  if N_elements(mpfit_nfree	) gt 0 then self.nfree 			= mpfit_nfree
  if N_elements(mpfit_npegged	) gt 0 then self.npegged 		= mpfit_npegged

  ;; Work with Yerr=0 values in the context of our property in this object
  if N_elements(Yerr) gt 0 then begin
     junk = where(Yerr eq 0, count)
     if count gt 0 and NOT keyword_set(self.fit_hide_infinity) then begin
        message, /INFORMATIONAL, 'WARNING: Yerr=0 for ' + strtrim(count, 2) + ' out of ' + strtrim(N_elements(Yerr), 2) + ' points and the hide_infinity flag is not set.  When deviates are calculated for these points, they will be infinite and crash MPFIT.  Setting hide_infinity effectively removes these points from the data (and any function points that happen to evaluate to infinity)'
     endif ;; some zero Yerrs
  endif ;; Yerr

  ;; Return now if we are called from the init method of this object
  if keyword_set(keep_local) then $
     return

  ;; Pass everything onto inherited routines
  self->pfo_calc_obj::set_property, Yerr=Yerr, _EXTRA=extra

end

;; Each inherited class should have a descr method.
function pfo_mpfit_obj::descr

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
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_mpfit_obj::cleanup
  ptr_free, self.ppfo_mpfit_obj_descr
  ptr_free, self.pmpfit_functargs
  ptr_free, self.pmpfit_Iterargs
  ptr_free, self.pmpfit_covar

  self->pfo_calc_obj::cleanup
end

function pfo_mpfit_obj::init, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   _REF_EXTRA=extra

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
     {README	: 'pfo_mpfit_obj encapsulates all of the shared information necessary for fitting PFO functions to data using MPFIT.', $
      SUPERCLASSES:'pfo_calc_obj', $
      METHODS	: 'fit()'} $
              )
  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_mpfit_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_mpfit_obj_descr, $
                        {PROPERTY: property[good_idx]}

  ;; This is a default value that is unlikely to change
  self.mpfit_kernel = 'pfo_mpfit_obj_kernel'
  ;; Hide instances of NAN and infinity from calculation of deviates
  ;; for mpfit.  See explanations of hide* in pfo_mpfit_obj::fit.
  self.fit_hide_NAN = !tok.yes
  self.fit_hide_infinity = !tok.yes
  ;; Start by accepting pretty much any positive MPFIT termination code
  self.mpfit_max_status = 8
  ;; From MPFIT
  self.mpfit_maxiter = 200
  self.mpfit_nprint = 1
  self.mpfit_ftol = 1D-10
  self.mpfit_xtol = 1D-10
  self.mpfit_gtol = 1D-10
  ;; We we !tok.nowhere to indicate that MPFIT has not been run yet in
  ;; this object.
  self.mpfit_status = !tok.nowhere
  ;; --> eventually this is going to be pfo_iterproc, which will then
  ;; be beefed up to a pfo_obj_iterproc or something like that
  self.mpfit_iterproc = 'pfo_mpfit_obj_iterproc'  

  ;; Turn our null reference pointers into undefined variables
  self.pmpfit_functargs = ptr_new(/allocate_heap)
  self.pmpfit_Iterargs = ptr_new(/allocate_heap)
  self.pmpfit_covar = ptr_new(/allocate_heap)

  ;; Call our _local_ set_property routine to convert any keywords to
  ;; property.  Keeping it local makes sure we don't get any
  ;; order-dependent cross-talk during initialization of multiple
  ;; inherited objects
  self->pfo_mpfit_obj::set_property, /keep_local, _EXTRA=extra

  ;; Call our superclass init methods
  ok = self->pfo_calc_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then $
     return, 0

  return, 1

end

pro pfo_mpfit_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_mpfit_obj, $
      ppfo_mpfit_obj_descr: ptr_new(), $  ;; Pointer to description structure
      mpfit_max_status	: 0B, $ ;; maximum value of MPFIT output status code
      mpfit_kernel	: '', $ ;; name of kernel function (MYFUNCT) used by MPFIT to call residual method
      mpfit_msg		: '', $ ;; (output) mpfit status code translated into text (used when status is positive.  mpfit_errmsg contains error message for negative status values
      pmpfit_functargs	: ptr_new(), $ ;; pointer to arguments (in addition to pfo_obj) that will be passed to mpfit_kernel
      fit_hide_NAN	: 0B, $ ;; Hide instances of NAN from calculation of deviates (default = yes or property value)
      fit_hide_infinity	: 0B, $ ;; Hide instances of infinity from calculation of deviates (default = yes or property value)
      mpfit_nfev	: 0L, $ ;; (output) number of calls to kernel function
      mpfit_maxiter	: 0L, $ ;; maximum number of fit iterations
      mpfit_errmsg	: '', $ ;; string containing MPFIT output error message
      mpfit_nprint	: 0L, $ ;; Iterproc is called every nprint iterations
      mpfit_quiet	: 0B, $ ;; MPFIT's quiet keyword
      mpfit_ftol	: 0D, $ ;; Stop criterion for reduction in sum of squares
      mpfit_xtol	: 0D, $ ;; Stop criterion for relative error between two consecutive iterations
      mpfit_gtol	: 0D, $ ;; Stop criterion for cos of angle between fvec and any column of the jacobian 
      mpfit_niter	: 0L, $ ;; (output) number of MPFIT iterations completed
      mpfit_status	: 0,  $ ;; (output) MPFIT exit status code
      mpfit_iterproc	: '', $ ;; Procedure to call every niter iterations
      mpfit_keyboard_iterstop: 0B, $ ;; flag to indicate if keyboard input will allow iteration to be stopped
      mpfit_iterstop_widget: 0B, $ ;; flag to indicate if a widget should be raised that will allow iteration to be stopped
      mpfit_plot	: 0B, $ ;; plot is drawn every niter
      mpfit_parinfo_widget: 0B, $ ;; refresh parinfo widgets every niter
      pmpfit_Iterargs	: ptr_new(), $ ;; pointer to other keyword arguments passed to iterproc
      pmpfit_covar	: ptr_new(), $ ;; (output) pointer to covariance matrix for best-fit params
      mpfit_bestnorm	: 0D, $ ;; (output) chi sq = total(deviates^2)
      mpfit_nfree	: 0L, $ ;; (output) number of free parameters
      mpfit_npegged	: 0L, $ ;; (output) number of parameters pegged at limits
      inherits pfo_calc_obj $	;; pfo_calc has data, parinfo, and plot
     }
end
