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
; $Id: pfo_roi__fdefine.pro,v 1.7 2015/03/03 21:20:13 jpmorgen Exp $
;
; $Log: pfo_roi__fdefine.pro,v $
; Revision 1.7  2015/03/03 21:20:13  jpmorgen
; Summary: Improve auto_ROI_flist
;
; Revision 1.6  2011/11/21 15:27:42  jpmorgen
; Minor improvments/standardizations of __fdefine files
;
; Revision 1.5  2011/11/18 16:10:30  jpmorgen
; Add auto_ROI_flist and automatic ROI incrementing features
;
; Revision 1.4  2011/09/16 11:19:38  jpmorgen
; Fiddled with status_mask
;
; Revision 1.3  2011/09/08 20:17:17  jpmorgen
; Added fname to pfo structure
;
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
   status_mask=status_mask, $ ;; status mask (!pfo.active, inactive, delete, all_status)
   ROI_left, $   ;; verbose parameter names for disambiguation
   ROI_right, $
   left=left, $  ;; general parameter names, useful for grouping functions
   right=right, $
   ROI=ROI, $    ;; Region of interest [left, right]
   terminate_idx=terminate_idx, $ ;; append !tok.nowhere to each index variable after we are done
   pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
   _REF_EXTRA=extra ;; soak up any extra parameters

  ;; Do basic idx error checking and collect our function indices
  idx = pfo_fidx(parinfo, pfo_fname(), idx=idx, status_mask=status_mask, pfo_obj=pfo_obj, $
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
     pfo_array_append, retval, !tok.nowhere
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
   parinfo=eparinfo, $	;; pass existing parinfo to avoid collision of iROIs
   min_iROI=min_iROI, $ ;; minimum iROI to use when assigning unique (mathematically independent) iROIs
   auto_ROI_flist=auto_ROI_flist, $ ;; [list of] function[s] which will be added to the returned parinfo segment and have the same ispec and iROI as the created ROI
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
                                 required_tags=['pfo', 'pfo_ROI'])
  ;; ... replicate it by fnpars
  parinfo = replicate(temporary(parinfo), fnpars)

  ;; Set attributes/defaults that are unique to this function.  Note
  ;; that some attributes are already defined in pfo_struct__init
  ;; (found in pfo_struct__define.pro)

  ;; FNAME 
  ;; It is inefficient to use 'where' on strings, so FTYPE is used
  ;; instead (see below).  fname should only be used by
  ;; pfo_struct__update to synchronize ftypes between parinfos created
  ;; at different times.
  parinfo.pfo.fname = pfo_fname()

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

  ;; iROI

  ;; The default iROI for all functions, assigned by
  ;; pfo_ROI_struct__init, is !pfo.allROI (-1).  This makes
  ;; mathematically linked ROIs.  In other words, a function with iROI
  ;; = !pfo.allROI is valid across all ROIs that are marked as
  ;; !pfo.allROI.  This is useful for a continuum or a peak with
  ;; extended wings.  iROI = !pfo.allROI also happens to avoid any
  ;; troubles with duplicate iROIs, since there can be many ROIs with
  ;; iROI = !pfo.allROI, but not more than one ROI with iROI ge 0.

  ;; In some applications, it is useful to have mathematically
  ;; independent ROIs.  This includes analysis of spectra which have
  ;; complicated continua that need to be chopped up on an almost
  ;; peak-by-peak basis.  To enable this, iROI can be set to a value =
  ;; !pfo.allROI + 1 or higher.  min_iROI is an input parameter to
  ;; this routine that determines the starting point for a run of
  ;; unique iROIs, assuming that the parinfo that is being built up is
  ;; passed as the parinfo=parinfo keyword to this routine.
  
  ;; NOTE: This code sets the default iROI.  The actual iROI can still
  ;; be manipulated by setting the iROI keyword in the call to
  ;; pfo_parinfo_new, since that keyword is converted to a parinfo
  ;; tag assignment, below.

  ;;  Re-assign the default iROI so we have it for auto_ROI_flist
  ;;  code, below
  iROI = !pfo.allROI
  if N_elements(min_iROI) gt 0 then begin
     ;; min_iROI = !pfo.allROI is valid, but means we revert to the
     ;; allROI case
     if min_iROI ne !pfo.allROI then begin
        if min_iROI lt !pfo.allROI then $
           message, 'ERROR: min_iROI lt !pfo.allROI.  Incrementing min_iROI will create create strange results.  I suggest that you use min_iROI = !pfo.allROI + 0 or 1 when calling this routine.'

        ;; If we made it here, we have a sensible min_iROI.  Set the
        ;; default iROI to our min_iROI
        iROI = min_iROI
        ;; If the user passed in the existing parinfo (recommended!),
        ;; we can make sure to choose our new parinfo's iROI so that
        ;; doesn't conflict with any others.
        eROI_idx = pfo_fidx(eparinfo, pfo_fname(), status_mask=!pfo.all_status, $
                            pfo_obj=pfo_obj, nfunct=nROI)
        if nROI gt 0 then $
           iROI = max(eparinfo[eROI_idx].pfo_ROI.iROI) + 1
        ;; Put iROI into the new parinfo segment
        parinfo.pfo_ROI.iROI = iROI
     endif ;; a min_iROI 
  endif ;; min_iROI specified

  ;; AUTO_ROI_FLIST

  ;; Automatically create functions in this ROI, if desired (usually
  ;; pfo_poly).  This may get a little convoluted if the function init
  ;; "methods" call update.  They shouldn't.

  ;; Automatically added functions can cause problems if we are adding
  ;; functions to more than one !pfo.allROI and those functions
  ;; replace the Y-axis.  This is an error, since you don't know which
  ;; function has priority.  In the case of this automatic stuff,
  ;; assume the first one is going to have priority and let the user
  ;; poke around with the rest.

  N_auto_ROI_flist = N_elements(auto_ROI_flist)

  if N_auto_ROI_flist gt 0 then begin
     ;; Carefully construct the auto_ROI_flist in a test parinfo
     ;; constructed to match the external parinfo structure (if provided)
     for ifn=0,N_auto_ROI_flist-1 do begin
        pfo_array_append, $
           test_parinfo, $
           pfo_parinfo_new(auto_ROI_flist[ifn], parinfo=eparinfo, pfo_obj=pfo_obj, _EXTRA=extra)
        test_parinfo.pfo_ROI.iROI = iROI
     endfor ;; constructing parinfo of auto-added functions

     ;; Check to see if the auto-added functions are going into
     ;; !pfo.allROI and we have an external parinfo to check for fop =
     ;; !pfo.repl problems
     if iROI eq !pfo.allROI and N_elements(eparinfo) ne 0 then begin
        ;; Check to see if we are an additional instance of
        ;; !pfo.allROI in the existing parinfo
        allROI_idx = where(eparinfo.pfo_ROI.iROI eq !pfo.allROI, count)
        if count gt 0 then begin
           ;; There is at least one more instance of !pfo.allROI.  Check
           ;; to see if any of the existing functions replace their axis
           repl_idx = where(eparinfo[allROI_idx].pfo.fop eq !pfo.repl, count)
           if count gt 0 then begin
              ;; The existing parinfo does a replacement operation.
              ;; Collect the idx of functions that _don't_ replace
              no_repl_idx = where(test_parinfo.pfo.fop ne !pfo.repl, count)
              ;; Delete test_parinfo if we have no non-replacing functions
              if count eq 0 then $
                 junk = temporary(test_parinfo) $
              else $
                 test_parinfo = test_parinfo(no_repl_idx)
           endif ;; repl in eparinfo
        endif    ;; allROI in eparinfo
     endif ;; This ROI is an allROI and there is eparinfo to worry about

     ;; Append our auto function(s).  Quiet keeps pfo_array_append
     ;; from complaining if test_parinfo is empty
     pfo_array_append, parinfo, test_parinfo, /quiet     
  endif    ;; auto_ROI_flist

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
