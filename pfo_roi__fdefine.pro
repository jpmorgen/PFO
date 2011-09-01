;+
; NAME: pfo_ROI_fdefine
;
; PURPOSE: define, initialize and work with the PFO_ROI function
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: All functionality is best accessed from
; higher-level PFO routines, like pfo_funct and pfo_fidx
;
; DESCRIPTION:

; This code is organized into "methods," such as __fdefine, __init,
; __calc and __indices.  Note that some of the code is
; interdependent (e.g. __calc calls __indices, __fdefine is the
; first routine called), so the order these routines are listed in the
; file is important.  Brief documentation on each "method" is provided
; in the code.  The parameter/keyword lists are also annotated.

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
;   See pfo_finfo.
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
; $Id: pfo_roi__fdefine.pro,v 1.2 2011/09/01 22:10:11 jpmorgen Exp $
;
; $Log: pfo_roi__fdefine.pro,v $
; Revision 1.2  2011/09/01 22:10:11  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 18:27:12  jpmorgen
; Initial revision
;
;-

;; The __indices "method" maps keyword names to the indices of those
;; parameters.  It is intended to be called ONLY for one instance of
;; the function at a time.  The pfo_funct /INDICES "method" can be
;; used to process multiple function instances.
function pfo_ROI__indices, $
   parinfo, $    ;; parinfo containing function
   idx=idx, $	;; idx into parinfo over which to search for function
   ROI_left, $   ;; verbose parameter names for disambiguation
   ROI_right, $
   left=left, $  ;; general parameter names, useful for grouping functions
   right=right, $
   ROI=ROI, $    ;; Region of interest [left, right]
   terminate_idx=terminate_idx, $ ;; append !tok.nowhere to each index variable after we are done
   pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
   _REF_EXTRA=extra ;; soak up any extra parameters

  ;; Do basic idx error checking and collect our function indices
  idx = pfo_fidx(parinfo, 'pfo_ROI', idx=idx, pfo_obj=pfo_obj, $
                 npar=npar, nfunct=nfunct)
  if nfunct ne 1 then $
     message, 'ERROR: ' + strtrim(nfunct, 2) + ' instances of function found, I can only work with one.  Use pfo_parinfo_parse and/or pfo_fidx to help process multiple instances of a function.'

  ;; Get the fractional part of ftype, which determines which
  ;; parameter we are.  Multiply by 10 and round, to prevent precision errors
  ffrac = round(10*pfo_frac(parinfo[idx].pfo.ftype))

  ;; Translate fractional ftype into disambiguated keywords, raising
  ;; errors if we don't get what we expect.
  ROI_left_local = where(ffrac eq 1, count)
  if count eq 0 then $
     message, 'ERROR: missing ROI_left'
  ;; unwrap and make into a scaler
  ROI_left_local = idx[ROI_left_local[0]]

  ROI_right_local = where(ffrac eq 2, count)
  if count eq 0 then $
     message, 'ERROR: missing ROI_right'
  ;; unwrap and make into a scaler
  ROI_right_local = idx[ROI_right_local[0]]

  ;; If we made it here, everything should be in order

  ;; Add our terminate_idx markers, if desired
  if keyword_set(terminate_idx) then begin
     pfo_array_append, ROI_left_local, !tok.nowhere
     pfo_array_append, ROI_right_local, !tok.nowhere
  endif ;; terminators

  ;; Append our indices onto the running arrays, if present
  if N_elements(ROI_left) gt 0 or arg_present(ROI_left) then $
     pfo_array_append, ROI_left, ROI_left_local
  if N_elements(ROI_right) gt 0 or arg_present(ROI_right) then $
     pfo_array_append, ROI_right, ROI_right_local

  ;; Copy disambiguated keywords into others, if desired
  if N_elements(left) gt 0 or arg_present(left) then $
     pfo_array_append, left, ROI_left_local
  if N_elements(ROI_right) gt 0 or arg_present(ROI_right) then $
     pfo_array_append, right, ROI_right_local

  ;; ROI is a little different.  A ROI list is always just pairs of
  ;; numbers.  But note these are lists of idx, not values!
  if N_elements(ROI) gt 0 or arg_present(ROI) then begin
     pfo_array_append, ROI, [ROI_left_local, ROI_right_local]
  endif ;; ROI

  ;; Return indices sorted in order of ffrac
  retval = idx[sort(ffrac)] 
  if keyword_set(terminate_idx) then $
     pfo_array_append, retval, !pfo.nowhere
  return, retval

end

;; The __calc "method" returns the calculated value of the
;; function
function pfo_ROI__calc, $
   Xin, $ 	;; Input X axis in natural units 
   params, $ 	;; parameters (entire array)
   parinfo=parinfo, $ ;; parinfo array (whole array)
  idx=idx, $    ;; idx into parinfo of parinfo segment defining this function
   Xaxis=Xaxis, $ ;; transformed from Xin by pfo_funct, in case ROI boundary is cast in terms of "real" units.  Passing it this way saves recalculation in this routine
   count=count, $ ;; number of xaxis points found in ROI
   pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
   _REF_EXTRA=extra ;; passed to pfo_Xaxis

  ;; Use shared routines to do basic error checking.  These error
  ;; messages are pretty verbose.

  ;; Common error checking and initialization code
  pfo_calc_check, Xin, params, parinfo=parinfo, idx=idx, pfo_obj=pfo_obj

  ;; If we made it here, our inputs should be in reasonably good shape
  ;; --> Assume that we have been called from pfo_parinfo_parse so
  ;; that our idx are sorted in ftype order
  inaxis = parinfo[idx].pfo.inaxis

  ;; Check to see if we need to bother calculating an Xaxis.  If we
  ;; were called from pfo_funct, this should have been done for us
  ;; already and passed as the Xaxis keyword.
  junk = where(inaxis eq !pfo.Xaxis, nXaxis)
  if nXaxis gt 0 and N_elements(Xaxis) eq 0 then begin
     ;; Use pfo_Xaxis to calculate the Xaxis.  Make sure we remove
     ;; ROIs from the calculation to avoid an infinite loop (I
     ;; actually though of this before the first loop occured... :-)
     Xaxis = Xin
     no_ROI_idx = where(floor(parinfo.pfo.ftype) ne $
                        pfo_fnum('pfo_ROI', pfo_obj=pfo_obj), count)
     if count gt 0 then $
        Xaxis = pfo_Xaxis(parinfo, Xin=Xin, params=params, idx=no_ROI_idx, $
                          _EXTRA=extra)
  endif

  ;; Make our ROI return keyword.  This will read in whatever units
  ;; the pfo.inaxis tags specify.  --> Assume that we have been called
  ;; from pfo_parinfo_parse so that our idx are sorted in ftype order
  ROI = params[idx]

  ;; Prepare to handle ispec.  ispec is defined as the second
  ;; dimension of Xin (if any).  --> assume ispec is the same for both
  ;; parameters
  Xin_dims = size(/dimensions, Xin)
  ispec = parinfo[idx[0]].pfo_ROI.ispec
  if ispec lt 0 then begin
     ;; Handle our !pfo.allspec case.  Doesn't matter until we
     ;; have Xin of 2 dimensions
     case N_elements(Xin_dims) of
        0: ispec = 0
        1 : ispec = 0
        2 : ispec = lindgen(Xin_dims[1])
        else: message, 'ERROR: Xin has ' + strtrim(N_elements(Xin_dims), 2) + ' dimensions.  Expecting no more than 2.'
     endcase
  endif ;; ispec out of range (e.g. !pfo.allspec)
     

  ;; --> I don't htink this is going to work in the
  ;; multi-dimensional ispec case unless I loop over ispec

  ;; Do this with actual statements rather than piecing together some
  ;; text for an execute statement.  This ensures software works with
  ;; IDLVM and may be faster.
  case inaxis[0] of
     !pfo.Xin:     left_idx = where(ROI[0] le Xin[*,ispec], count)
     !pfo.Xaxis:   left_idx = where(ROI[0] le Xaxis[*,ispec], count)
     else: message, 'ERROR: parinfo.pfo.inaxis must be !pfo.Xin or !pfo.Xaxis'
  endcase

  ;; Check to see if any of the Xin/Xaxis is bounded on the left by
  ;; ROI[0].  If not, left_idx will be -1, which is a reasonable
  ;; return value.  User should check for count, as with where
  if count eq 0 then $
     return, left_idx
  
  case inaxis[1] of
     !pfo.Xin:     right_idx = where(  Xin[left_idx, ispec] le ROI[1], count)
     !pfo.Xaxis:   right_idx = where(xaxis[left_idx, ispec] le ROI[1], count)
     else: message, 'ERROR: parinfo.pfo.inaxis must be !pfo.Xin or !pfo.Xaxis'
  endcase

  ;; Prepare our return value.  In both cases, offset our indices by
  ;; the ispec * the length of one Xin spectrum so that we can pretend
  ;; Xin is one dimensional everywhere else

  ;; Check to see if any of the Xin/Xaxis is bounded on the right by
  ;; ROI[1].  If not, idx will be -1, which is a reasonable return
  ;; value.  User should check for count, as with where
  if count eq 0 then $
     return, right_idx + ispec*Xin_dims[0]

  ;; If we made it here, we have a section of our Xin/Xaxis that falls
  ;; within this ROI.  Return the indices into this section,
  ;; remembering to unwrap, being as polite with memory as we can
  return, left_idx[temporary(right_idx)] + ispec*Xin_dims[0]
  
end

;; Create the parinfo strand for this function and initialize it
function pfo_ROI__init, $
   value=value, $	;; catch value to make sure no conflict with other keywords
   ftype=ftype, $	;; catch ftype to make sure no conflict in pfo_struct_setget_tag
   pfo_obj=pfo_obj, $	;; pfo_obj for parinfo_template
   _REF_EXTRA=extra, $	;; _REF_EXTRA passed to pfo_struct_setget_tag
   ROI=ROI, $		;; ROI will be the usual way the ROI boundaries are specified
   ROI_left, $   	;; verbose parameter names for disambiguation
   ROI_right, $
   left=left, $  	;; general parameter names, useful for grouping functions
   right=right


   ;; Initialize parinfo in case early error.
  parinfo = !tok.nowhere

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning what I have so far.', /CONTINUE
        return, parinfo
     endif
  endif ;; not debugging

  ;; Catch improper use of value and ftype keywords
  if N_elements(value) + N_elements(ftype) ne 0 then $
     message, 'ERROR: value and ftype are set internally: they are therefore invalid keywords'

  ;; Get critical information about this function from pfo_finfo
  pfo_finfo, fname=pfo_fname(), fnum=fnum, fnpars=fnpars, pfo_obj=pfo_obj

  ;; Create our parinfo strand for this function, making sure to
  ;; include any substructure we need with required tags.  Start with
  ;; one parinfo element.
  parinfo = pfo_parinfo_template(pfo_obj=pfo_obj, $
                                 required_tags=['pfo', pfo_fname()])
  ;; ... replicate it by fnpars
  parinfo = replicate(temporary(parinfo), fnpars)

  ;; Set attributes/defaults that are unique to this function.  Note
  ;; that some attributes are already defined in pfo_struct__init
  ;; (found in pfo_struct__define.pro)

  ;; PARNAME
  ;; Don't put function name in parname, since it can be
  ;; reconstructed from pfo.ftype.  Packages built on top of pfo
  ;; may want to set !pfo.longnames = 0 to use these short names
  parinfo.parname = ['L', $
                     'R']
  if !pfo.longnames ne 0 then $
     parinfo.parname = ['Left', $
                        'Right']

  ;; FTYPE -- fractional part (ffrac).  A different decimal value can
  ;;          be associated with each parameter of the function.  For
  ;;          complicated associations, create a new tag with the
  ;;          pfo_struct system (e.g. pfo_ROI_struct__define.pro)
  parinfo.pfo.ftype = [0.1, $ ;; left
                       0.2]   ;; right
  ;; FTYPE -- integer part (fnum).  Dynamically assigned by fdefine
  parinfo.pfo.ftype += fnum

  ;; VALUE
  ;; Break all the possible inputs down to our disambiguated parameter
  ;; names, making sure the user doesn't over-specify
  if N_elements(ROI) ne 0 then begin
     if N_elements(ROI) ne 2 then $
        message, 'ERROR: ROI must have 2 elements'
     if N_elements(ROI_left) + N_elements(ROI_right) ne 0 then $
        message, 'ERROR: do not specify ROI and ROI_left or ROI_right'
     if N_elements(left) + N_elements(right) ne 0 then $
        message, 'ERROR: do not specify ROI and left or right'
     parinfo.value = ROI
  endif ;; value
  if N_elements(ROI_left) + N_elements(left) gt 1 then $
     message, 'ERROR: specify one and only one of ROI_left or left'
  if N_elements(ROI_right) + N_elements(right) gt 1 then $
     message, 'ERROR: specify one and only one of ROI_right or right'
  if N_elements(left) gt 0 then $
     parinfo[0].value = left
  if N_elements(right) gt 0 then $
     parinfo[1].value = right

  ;; INAXIS: ROIs are generally defined in terms of raw detector
  ;; coordinates, but they can be a function of Xaxis
  parinfo.pfo.inaxis = !pfo.Xin

  ;; FOP: ROI does not operate (in this sense) on any axis.  It
  ;; filters the indices of Xin before other functions operate.
  parinfo.pfo.fop = !pfo.none

  ;; OAXIS: Just like fop, there is no real out axis, in the operation
  ;; sense.  A ROI is just a special beast
  parinfo.pfo.outaxis = !pfo.none

  ;; FIXED
  ;; In general, we want our ROI boundaries to be fixed, otherwise
  ;; weird things might happen, like having them collapse (less data
  ;; = smaller chi^2).  Use pfo_mode to do this.  Make the ROI
  ;; parameters "permanently" fixed so that a casual call to pfo_mode,
  ;; parinfo, 'free' doesn't free them
  pfo_mode, parinfo, 'fixed', /permanent
  
  ;; Convert keywords on the command line to tag assignments in the
  ;; parinfo.  This is a little risky, since we might have duplicate
  ;; tags in the different sub-structures (e.g. status is a popular
  ;; one) and, depending on what order the substructures were added
  ;; with required_tags, the behavior might vary.  It is always safer
  ;; to make assignments in your calling code explicitly with the tags
  ;; in the returned parinfo.  Note, we pass all of the _EXTRA to
  ;; pfo_struct_setget_tag, so the calling routine gets to choose
  ;; parallel or series, strict, etc.
  pfo_struct_setget_tag, /set, parinfo, _EXTRA=extra

  return, parinfo

end

;; Map function name to function number and number of parameters in
;; the PFO system
pro pfo_ROI__fdefine, pfo_obj=pfo_obj

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_fdefine, pfo_obj=pfo_obj, fname=pfo_fname(), fnpars=2, $
               fdescr='Region of interest (ROI) function.  This 2-parameter function defines a ROI between two points in the data.  The ROI endpoints can be individually specified in units of Xin or Xaxis, depending on value of parinfo.pfo,inaxis of each parameter (Xin is the default).  The function returns the indices into Xin over which the ROI is valid (inclusive of the endpoints).'
  
end
