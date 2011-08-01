;+
; NAME: mpfit_parinfo_struct__define (and mpfit_parinfo_struct__init)
;
; PURPOSE: Define and initialize the mpfit and related tags used in
; the PFO parinfo structure
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: parinfo1 = pfo_struct_new('mpfit_parinfo_struct')

; DESCRIPTION: Designed for use with pfo_struct_new.  Returns an
; initialized top-level parinfo structure with all of the tags
; necessary for parinfo to interface a single parameter with Craig
; Markward's MPFIT system.

; INPUTS: none
;
; OPTIONAL INPUTS: none
;
; KEYWORD PARAMETERS: parinfo -- the output parinfo structure
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
;  pfo_parinfo__define, parinfo=pfo_parinfo
;  pfo_link = {pfo_link : {pfo_link_struct}}
;  pfo_struct_append, parinfo, pfo_link
;
; MODIFICATION HISTORY:
;
; $Id: mpfit_parinfo_struct__define.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: mpfit_parinfo_struct__define.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;
;-

pro mpfit_parinfo_struct__get_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $
   taglist_strict= taglist_strict, $
   _REF_EXTRA  	= extra, $
   value	= value	   , $
   error	= error	   , $
   fixed	= fixed	   , $
   limited	= limited  , $     
   limits	= limits   , $     
   parname	= parname  , $     
   step		= step	   , $
   relstep	= relstep  , $     
   mpside	= mpside   , $     
   mpmaxstep	= mpmaxstep, $	
   tied		= tied	   , $
   mpprint	= mpprint

  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: mpfit_parinfo_struct__get_tag, parinfo, idx=idx, [tag=tag, ...]'
     endif
  endif ;; not debugging

  ;; This code is a little different for a top-level struct
  ;; Get the top-level tag and structure names
  tn = tag_names(parinfo)

  ;; Check to see if we are likely to have the right list of tags
  tag_idx = where(tn eq 'TOP_LEVEL_STRUCT', count)
  if count eq 0 then $
    message, 'ERROR: TOP_LEVEL_STRUCT tag not found.  Incompatible parinfo structure.'

  if parinfo[0].top_level_struct ne 'MPFIT_PARINFO_STRUCT' then $
    message, 'ERROR:  TOP_LEVEL_STRUCT = ' + strtrim(parinfo.top_level_struct, 2) + ' expecting MPFIT_PARINFO_STRUCT.'

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags
  if N_elements(idx) eq 0 then $
    idx = lindgen(N_elements(parinfo))

  if arg_present(value	 )  or N_elements(value   ) ne 0 then value	= parinfo[idx].value
  if arg_present(error	 )  or N_elements(error	  ) ne 0 then error	= parinfo[idx].error 
  if arg_present(fixed	 )  or N_elements(fixed	  ) ne 0 then fixed	= parinfo[idx].fixed
  if arg_present(limited )  or N_elements(limited ) ne 0 then limited   = parinfo[idx].limited
  if arg_present(limits	 )  or N_elements(limits  ) ne 0 then limits    = parinfo[idx].limits
  if arg_present(parname )  or N_elements(parname ) ne 0 then parname   = parinfo[idx].parname
  if arg_present(step	 )  or N_elements(step	  ) ne 0 then step	= parinfo[idx].step
  if arg_present(relstep )  or N_elements(relstep ) ne 0 then relstep   = parinfo[idx].relstep
  if arg_present(mpside	 )  or N_elements(mpside  ) ne 0 then mpside    = parinfo[idx].mpside
  if arg_present(mpmaxstep) or N_elements(mpmaxstep) ne 0 then mpmaxstep= parinfo[idx].mpmaxstep
  if arg_present(tied	 )  or N_elements(tied	  ) ne 0 then tied	= parinfo[idx].tied
  if arg_present(mpprint )  or N_elements(mpprint ) ne 0 then mpprint   = parinfo[idx].mpprint
  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                      taglist_series=taglist_series, $
                      taglist_strict=taglist_strict, $
                      _EXTRA=extra

end


;; A set_tag routine is optional.  It lets you map keyword parameters
;; passed to routines such as pfo_fname__init to code which sets tag
;; values in the parinfo.  You could just as easily take the output
;; parinfo of you pfo_fname__init routine, initialized to generic
;; values and manipulate the tags directly in the calling code.  Since
;; everyone uses the PFO structure, I go to the effort here.

;; For the set_tag routines, it is important to explicitly name these
;; keywords, rather than trying to make a general set_tag routine
;; using _EXTRA, since _EXTRA copies everything into a structure.  In
;; other words, going to the trouble of making a separate __set_tag
;; routine for each struct helps save memory.
pro mpfit_parinfo_struct__set_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $
   taglist_strict= taglist_strict, $
   _REF_EXTRA  	= extra, $
   value	= value	   , $
   error	= error	   , $
   fixed	= fixed	   , $
   limited	= limited  , $     
   limits	= limits   , $     
   parname	= parname  , $     
   step		= step	   , $
   relstep	= relstep  , $     
   mpside	= mpside   , $     
   mpmaxstep	= mpmaxstep, $	
   tied		= tied	   , $
   mpprint	= mpprint

  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: mpfit_parinfo_struct__set_tag, parinfo, idx=idx, [tag=tag, ...]'
     endif
  endif ;; not debugging

  ;; This code is a little different for a top-level struct
  ;; Get the top-level tag and structure names
  tn = tag_names(parinfo)

  ;; Check to see if we are likely to have the right list of tags
  tag_idx = where(tn eq 'TOP_LEVEL_STRUCT', count)
  if count eq 0 then $
    message, 'ERROR: TOP_LEVEL_STRUCT tag not found.  Incompatible parinfo structure.'

  if parinfo[0].top_level_struct ne 'MPFIT_PARINFO_STRUCT' then $
    message, 'ERROR:  TOP_LEVEL_STRUCT = ' + strtrim(parinfo.top_level_struct, 2) + ' expecting MPFIT_PARINFO_STRUCT.'

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags
  if N_elements(idx) eq 0 then $
    idx = lindgen(N_elements(parinfo))

  if N_elements(value	 ) ne 0 then parinfo[idx].value		= value	 
  if N_elements(error	 ) ne 0 then parinfo[idx].error	        = error	 
  if N_elements(fixed	 ) ne 0 then parinfo[idx].fixed	        = fixed	 
  if N_elements(limited	 ) ne 0 then parinfo[idx].limited	= limited	 
  if N_elements(limits	 ) ne 0 then parinfo[idx].limits	= limits	 
  if N_elements(parname	 ) ne 0 then parinfo[idx].parname	= parname	 
  if N_elements(step	 ) ne 0 then parinfo[idx].step	        = step	 
  if N_elements(relstep	 ) ne 0 then parinfo[idx].relstep	= relstep	 
  if N_elements(mpside	 ) ne 0 then parinfo[idx].mpside	= mpside	 
  if N_elements(mpmaxstep) ne 0 then parinfo[idx].mpmaxstep     = mpmaxstep
  if N_elements(tied	 ) ne 0 then parinfo[idx].tied	        = tied	 
  if N_elements(mpprint	 ) ne 0 then parinfo[idx].mpprint	= mpprint	 
  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      taglist_strict=taglist_strict, $
                      _EXTRA=extra

end

function mpfit_parinfo_struct__init, descr=descr, _REF_EXTRA=extra
  ;; Get our mpfit parinfo, initialized somewhat inconveniently by IDL
  ;; with null values
  parinfo = {mpfit_parinfo_struct}
  ;; Put in our default values.  The top_level_struct tag lets
  ;; pfo_struct_set_tag gain access to our __set_tag routine.  Note
  ;; that we can use a different parameter optimizer with an MPFIT
  ;; parinfo.
  parinfo.top_level_struct = 'MPFIT_PARINFO_STRUCT'
  parinfo.value = !values.d_nan
  parinfo.error = !values.d_nan
  parinfo.mpprint = 1
  
  ;; Create our description
  descr = $
    {README	: 'The top-level tags "value" and "fixed" through "mpprint" are used by the parameter optimizer, MPFIT.  Tags that are structures should be documented in their own README tags.', $
     top_level_struct: 'Full structure name of this structure in its original form.', $
     value	: 'Parameter value.  PFO and MPFIT use this', $
     error	: 'Parameter error.  PFO uses this !values.d_nan when not assigned (e.g. no fit has been calculated yet)', $
     fixed	: '0=free, 1=fixed', $
     limited	: 'a two-element boolean array.  If the first/second element is set, then the parameter is bounded on the lower/upper side.  A parameter can be bounded on both sides.  Both LIMITED and LIMITS must be given together.', $
     limits	: 'a two-element float or double array.  Gives the parameter limits on the lower and upper sides, respectively.  Zero, one or two of these values can be set, depending on the values of LIMITED.  Both LIMITED and LIMITS must be given together.', $
     parname	: 'parameter name for printing purposes', $
     step	: 'the step size to be used in calculating the numerical derivatives.  If set to zero, then the step size is computed automatically.  Ignored when MPFIT is given AUTODERIVATIVE=0 keyword.  This value is superceded by the RELSTEP value.', $
     relstep	: 'the *relative* step size to be used in calculating the numerical derivatives.  This number is the fractional size of the step, compared to the parameter value.  This value supercedes the STEP setting.  If the parameter is zero, then a default step size is chosen.', $
     mpside	: 'the sidedness of the finite difference when computing numerical derivatives.  This field can take four values: 0 - one-sided derivative computed automatically; 1 - one-sided derivative (f(x+h) - f(x)  )/h; -1 - one-sided derivative (f(x)   - f(x-h))/h; 2 - two-sided derivative (f(x+h) - f(x-h))/(2*h) Where H is the STEP parameter described above.  The "automatic" one-sided derivative method will chose a direction for the finite difference which does not violate any constraints.  The other methods do not perform this check.  The two-sided method is in principle more precise, but requires twice as many function evaluations.', $
     mpmaxstep	: 'the maximum change to be made in the parameter value.  During the fitting process, the parameter will never be changed by more than this value in one iteration.  A value of 0 indicates no maximum.', $
     tied	: 'a string expression which "ties" the parameter to other free or fixed parameters.  In "raw" MPFIT, this can be used to generate arbitrarily complex algebraic expressions (it is basically passed to the "execute" function).  In PFO, this is mamanged by pfo_link and is just used to set parameters equal to each other.  Anything more complex should be done at the PFO function level, ', $
     mpprint	: 'boolean value indicating that a parameter should be printed when MPFIT is iterating'}  

  ;; The last thing we do is pass on any other keywords to our
  ;; __set_tag "method."  Do this with _STRICT_EXTRA to make sure that
  ;; no bogus keywords are passed.  Be careful with debugging and make
  ;; sure user gets the best effort case when we are trying to ignore
  ;; the error.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /INFORMATIONAL, 'WARNING: the above error was produced.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting (not recommended).' 
        return, parinfo 
     endif ;; CATCH
  endif ;; debugging
  mpfit_parinfo_struct__set_tag, parinfo, _STRICT_EXTRA=extra

  return, parinfo 

end

;; NOTES:

;; There are two things that, by including here, we are forcing
;; ourseles to pay attention to: value and mpprint.  mpprint, if
;; present, needs to be 1, or else, by default, mpfit won't print our
;; parameters during iterations.  Value is a little trickier.  MPFIT
;; uses value as an input: the initial value of the parameter.  It can
;; be superceded by the command line argument 'start_params' to mpfit.
;; PFO stores the successfully fitted parameter value in .value.
;; Thus, it is also an output value.  What with the iterative way
;; fitting usually work, this should not be a problem, but you should
;; be aware that there is some "slight of hand" going on here.

pro mpfit_parinfo_struct__define
  parinfo = {mpfit_parinfo_struct, $   ; Any modifications to parinfo turn it into an anonymous structure...
             top_level_struct: '', $ ; ... so preseve the full name of this structure in this key (see __init "method")
             value	: 0D, $ ; PFO and MPFIT use this: see NOTE, above
             error	: 0D, $ ; PFO-specific
             fixed	: 0B, $  ; see MPFIT documentation
             limited	: [0B,0B], $ ; this and below are explained in MPFIT documentation
             limits	: [0.D,0.D], $
             parname	: '', $
             step	: 0D, $
             relstep	: 0D, $
             mpside	: 0B, $
             mpmaxstep	: 0D, $
             tied	: '', $
             mpprint	: 0B}

end
