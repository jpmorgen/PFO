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
; $Id: pfo_fit.pro,v 2.1 2011/11/03 01:47:22 jpmorgen Exp $
;
; $Log: pfo_fit.pro,v $
; Revision 2.1  2011/11/03 01:47:22  jpmorgen
; About to replace with pfo_fit_widget
;
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

  ;; Handle no_block in a polite way to the caller, since we may mess
  ;; with it.  Default to a non-blocking widget and check extensively
  ;; to see if we need to block
  no_block = 1
  if N_elements(no_block_in) ne 0 then $
     no_block = no_block_in

  ;; Force no_block if the user wants parinfo filled by widget
  if arg_present(parinfo) then begin
     if keyword_set(no_block) then begin
        no_block = 0
        message, /INFORMATIONAL, 'NOTE: return of parinfo is expected based on the presence of an undefined parinfo keyword in the call.  Forcing no_block=0 so that the parinfo created in the widget can be returned.  Use pfo_quiet to supress message or handle the transmission of parinfos via pfo_obj.'
     endif ;; forcing no_block = 0
  endif ;; parinfo expected back

  ;; Check to see if we have an existing parinfo and raise a
  ;; warning if it looks like things are going to get out of sync
  if N_elements(parinfo) ne 0 and keyword_set(no_block) then $
     message, /INFORMATIONAL, 'WARNING: input parinfo specified, but no_block=1.  This means the widget will return immediately and changes to parinfo in the widget will not be reflected in the calling parinfo.  If you just expect parinfo to be an input variable, this is OK.  Use pfo_quiet to supress message or handle the transmission of parinfos via pfo_obj.'

  ;; Create a pfo_obj if needed
  if NOT obj_valid(pfo_obj) then begin
     ;; Do some blocking checking with pfo_obj
     if arg_present(pfo_obj) or (N_elements(pfo_obj) ne 0) then begin
        ;; The user has specified pfo_obj, which means that they can
        ;; extract information from it, so it is safe to return.
        ;; Print a message in both cases of no_block
        message, /INFORMATIONAL, 'NOTE: an uninitialized pfo_obj was supplied by the caller.  Creating the pfo_obj and preparing to return it to the caller.  Remember to issue the command "obj_destroy, pfo_obj" when you are done with it.  Use pfo_quiet to supress message.'
        if keyword_set(no_block) then begin
              message, /INFORMATIONAL, 'NOTE: no_block=1, so I am going to return immediately.  The pfo_obj will be available for command-line use in parallel with the widget.  Use pfo_quiet to supress message.'
        endif else begin
              message, /INFORMATIONAL, 'NOTE: no_block=0.  I will try to raise a blocking widget to wait for all the changes initiated by the widget to be collected in the pfo_obj.  This may not work if the caller is itself a blocking widget (IDL can only handle one blocking widget at a time).  Use pfo_quiet to supress message.'
           endelse ;; messages regarding blocking
     endif else begin
        ;; We need to create a pfo_obj, but we won't be returning it.
        message, /INFORMATIONAL, 'NOTE: we need to create a object called "pfo_obj" to help do our widget work.  To prevent memory leaks, this object must be destroyed when you exit the widget ("obj_destroy, pfo_obj").  In order to issue this command from within this routine, the widget is going to try to "block" and not let anything else happen in IDL until you exit (no command line prompt, no other widget, etc.).  There may be trouble raising a blocking if another blocking widget is running.  In this case, you may need to capture the output of this widget in the pfo_obj keyword.  Use pfo_quiet to supress message.'
        no_block = 0
     endelse ;; pfo_obj present vs. absent on command line

     ;; Create our pfo_obj
     pfo_obj = pfo_obj_new(p0, p1, p2, p3, parinfo_array=parinfo, _EXTRA=extra)
     created_pfo_obj = 1

  endif else begin

     ;; We have a valid pfo_obj.  In this case p0 should not be our
     ;; pfo_obj class name, so it is safe to use new_data
     pfo_obj->new_data, p0, p1, p2
     ;; Pass our parinfo (if specified) and any other args to set_property
     pfo_obj->set_property, parinfo_array=parinfo, _EXTRA=extra
  endelse

  ;; This is the meat of our code.  It brings up the pfo_fit widget
  pfo_obj->edit, no_block=no_block, _EXTRA=extra

  ;; Check to see if the user wanted the parinfo
  if (arg_present(parinfo) or N_elements(parinfo) ne 0) then begin
     ;; Check to see if the user was going to keep pfo_obj around.  If
     ;; not, just move the parinfo out (if there is one)
     N_parinfo = pfo_obj->parinfo_call_function(/no_update, 'N_elements')
     if N_parinfo ne 0 then $
        parinfo = pfo_obj->parinfo(no_copy=(keyword_set(created_pfo_obj) and NOT arg_present(pfo_obj)))
  endif

  ;; If we created our own pfo_obj, get rid of it_obj to prevent
  ;; memory leaks, unless the user really wants it
  if keyword_set(created_pfo_obj) and NOT arg_present(pfo_obj) then $
        obj_destroy, pfo_obj

  ;; Remember to restore old state of objects_only
  !pfo.objects_only = oobjects_only

end
