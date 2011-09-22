;+
; NAME: pfo_fit
;
; PURPOSE: 
;	fit a function described by a parinfo array to data described
;	in x, y, and yerr using mpfitfun.  Function is described by
;	parainfo.  Results are  returned in parinfo.value and
;	parinfo.error.  Scaler outputs of MPFIT are captured in the
;	mpfit_info output keyword.  All other keywords are passed to
;	and from mpfitfun/mpfit via the _EXTRA=extra mechanism
;
; CATEGORY: Parameter Function Object (PFO)
;
; CALLING SEQUENCE:
;	pfo_fit, x, y, yerr, parinfo, no_status=no_status, [keyword args to mpfitfun])
;
; INPUTS:
;	x: x-axis values
;	y: y-axis values
;	yerr: error estimates for y --> should be generalized for
;	better compatability with MPFIT in case there are no yerrs
;	parinfo: parinfo array that describes the function
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;	no_status: don't print out the interpretation of the
;	mpfitfun return status keyword
;
;	mpfit_info: all scaler outputs of MPFIT/MPFITFUN.  Mostly
;	diagnostic stuff
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
; $Id: pfo_fit.pro,v 2.0 2011/09/22 23:54:49 jpmorgen Exp $
;
; $Log: pfo_fit.pro,v $
; Revision 2.0  2011/09/22 23:54:49  jpmorgen
; Using pfo_obj and pfo_fit_widget
;
; Revision 1.4  2011/09/22 17:45:01  jpmorgen
; Preparing to adapt to PFO of 2011
;
; Revision 1.3  2011/02/10 22:27:31  jpmorgen
; Fix bug in propagation of _EXTRA keywords!
;
; Revision 1.2  2010/12/31 21:57:13  jpmorgen
; Change error reporting a little
;
;-
pro pfo_fit, $
   p0, $ ;; either the name of the object to create or one of the data vectors
   p1, $ ;; one of the data vectors ([x], y, [yerr])
   p2, $
   p3, $
   parinfo=parinfo, $ ;; optional input or output
   pfo_obj=pfo_obj, $ ;; optional input or output
   $ ;; By defailt, the widget is non-blocking (other events and the IDL command line are processed).
   $ ;; You may wish to set no_block=0 to force user to finish with the widget before other things happen.  
   $ ;; When pfo_obj is created on the fly, no_block is always set to 0
   no_block=no_block_in, $ ;;  be polite to calling code
   _REF_EXTRA=extra ;; passed to underlying routines

  ;; Make sure custom system variables are read in.  These provide,
  ;; among other things, tokens for referring to numeric values.  They
  ;; are therefore a little like INCLUDE statements in C
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; For debugging purposes, make sure we are running in
  ;; object-oriented mode only, politely saving off old state of flag
  oobjects_only = !pfo.objects_only
  !pfo.objects_only = 1

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Remember to restore old state of objects_only
        !pfo.objects_only = oobjects_only
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_fit...'
     endif
  endif ;; not debugging

  ;; Create a pfo_obj if needed
  if NOT obj_valid(pfo_obj) then begin
     pfo_obj = pfo_obj_new(p0, p1, p2, p3, parinfo_array=parinfo, _EXTRA=extra)
     created_pfo_obj = 1
     no_block = arg_present(pfo_obj) or arg_present(parinfo)
     ;; Make sure we are a blocking widget if the user expects output
     if (arg_present(pfo_obj) or arg_present(parinfo) or N_elements(parinfo) ne 0) then begin
        if keyword_set(no_block) then $
           message, /CONTINUE, 'NOTE: forcing no_block=0 so parinfo and/or pfo_obj can be captured and returned (if possible -- IDL XMANAGER cannot block if the first widget was non-blocking)'
        no_block = 0
     endif ;; adjusting no_block to wait for return of parinfo and/or pfo_obj
  endif

  ;; This is the meat of our code.  It brings up the pfo_fit widget
  pfo_obj->edit, no_block=no_block, _EXTRA=extra

  ;; If we created our own pfo_obj, get rid of it_obj to prevent
  ;; memory leaks, unless the user really wants it
  if keyword_set(created_pfo_obj) and NOT arg_present(pfo_obj) then $
        obj_destroy, pfo_obj

  ;; Remember to restore old state of objects_only
  !pfo.objects_only = oobjects_only

end
