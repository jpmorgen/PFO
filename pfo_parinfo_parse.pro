;; -*- mode: idlwave; fill-column: 125 -*-
;+
; NAME: pfo_parinfo_parse
;
; PURPOSE: interpret functions (parinfo arrays) in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:  One of the following, depending on the desired action:

; yaxis = pfo_parinfo_parse(/CALC, parinfo [, params=params] Xin=Xin[, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, pfo_obj=pfo_obj][, xaxis=xaxis][, ROI_Xin_idx=ROI_Xin_idx][, keyowrds to pfo_<fname> routines])

; to_print = pfo_parinfo_parse(/PRINT, parinfo[, params=params][, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, status_mask=status_mask][, pfo_obj=pfo_obj][, keyowrds to pfo_<fname> routines])

; widget_id = pfo_parinfo_parse(/WIDGET, parinfo[, params=params][, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, pfo_obj=pfo_obj][, status_mask=status_mask][, keyowrds to pfo_<fname>__widget routines])

; indices = pfo_parinfo_parse(/INDICES, parinfo[, params=params][, idx=idx][, expand_idx=expand_idx][, /terminate_idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, status_mask=status_mask][, pfo_obj=pfo_obj][, keyowrds to pfo_<fname>__indices routines])

; DESCRIPTION:

; pfo_parinfo_parse parses a parinfo array according to the following
; hierarchy:

;   ispec, iROI, fseq, inaxis, outaxis, fop, ftype, ID

; The final level is an individual instance of a PFO-enabled function.
; The appropriate "method" of this function is called ("__CALC,"
; "__PRINT," "__WIDGET," or "__INDICES") depending on which action
; switch was specified to pfo_parinfo_parse.  If no such method is
; found for the PFO-enabled function, the appropriate method from
; pfo_null is called.  For instance, the methods pfo_null__print and
; pfo_null__widget should take care of display of parameters from most
; simple functions.

; The return value of the PFO-enabled function is combined with the
; previous function return values in a manner specified by tags in
; parinfo.pfo (/CALC case) by keywords (/INDICES case).  In the /PRINT
; case, all output is collected into one string.  For /WIDGET, one
; widgetID is returned and information which enables higher-level
; routines to refresh (update individual widgets) or repopulate
; (completely recreate the widget) is stored in pfo_obj.

; INPUTS:

;  parinfo: array of type structure  which describes the function
;
; OPTIONAL INPUTS:

;   params: parameter values in place of parinfo.value.  Usually
;	Provided by parameter optimization routine.


; KEYWORD PARAMETERS:

;   idx: indices into parinfo over which operation will be performed.
;   	If not specified, the entire parinfo array will be used
;   	(subject to ispec and iROI selections)

;   ispec: the spectrum number(s) (second dimension of Xin) which will
;   be selected from within the parinfo selected by idx (see pfo_ROI_idx)

;   iROI: the ROI number(s) (sections along Xin) which will
;   be selected in each ispec (see pfo_ROI_idx)

;   allspec: a special code is assigned to functions that are
;   intended to operate on all spectra.  When selecting a particular
;   ispec, it is easy to forget to include functions that are intended
;   to operate on all spectra, so this keyword is provided as a
;   reminder.  Using it is equivalent to ispec=[!pfo.allspec, ispec]

;   allROI: same as allspec, but includes the !pfo.allROI in iROI.
;   The iROI = !pfo.allROI is a special case which lets us have
;   multiple ROI functions across the X-axis _with the same iROI_.
;   The advantage is that we can can assign functions to
;   iROI=!pfo.allROI and have them over all of those ROIs at once.
;   Otherwise, we would need to define the function on each ROI and
;   link the parameters together.

;   status_mask: passed on to pfo_fidx.  By default, only active
;   parameters are handled.  status_mask allows parameters marked
;   !pfo.inactive and !pfo.delete to be used in the function too.
;   Essential for printing and widget display.

;   pfo_obj: pfo_obj which contains parinfo array and other
;		information necessary for the PFO system

;   Additional keywords for /CALC

;       Xin: X-axis intended to be in natural units of a detector
;       (e.g. CCD pixels or MCA channels)

;       xaxis: internal xaxis.  In pfo_parinfo_parse, Xin, the input X-axis is
;       assumed to be in natural detector units.  It can be
;       transformed by operations with parinfo.pfo.inaxis = !pfo.Xin
;       and parinfo.pfo.outaxis = !pfo.xaxis into an internal xaxis
;       which itself can then be used in calculations to output to the
;       yaxis.  This keyword provides axis to the internal xaxis.

;	ROI_Xin_idx (output): indices into Xin selected by ROI function(s)

;   Additional keywords for /INDICES

;       expand_idx: this allows the index of a single parameter into
;       parinfo to return all of the indices into the function in
;       which it is contained.  Return value of pfo_parinfo_parse is
;       expanded index list.  Multiple functions can be expanded if
;       expand_idx is a vector.  If more expand_idx contains more than
;       one index into a particular function, the indices into that
;       function will only be included once in the returned index
;       list.  Example:

;	;; Return indices of just the peak parameter(s)
;	junk = pfo_parinfo_parse(/INDICES, parinfo, peak_idx=peak_idx)
;	;; Expand peak parameter indices into indices for the entire function(s)
;	peak_fn_idx = pfo_parinfo_parse(/INDICES, parinfo, expand_idx=peak_idx)

;       terminate_idx: put termination marks (!tok.nowhere) after each
;       set of indices in all returned index lists.  These cab be
;       parsed with pfo_idx_parse

;   Additional keywords for /WIDGET

;	parentID (required): the parentID of a widget in which this
;	compound widget will be displayed.  If you don't have
;	one handy, you can use pfo_parinfo_widget, which creates a
;	generic top level base

; OUTPUTS:

;   CALC: returns calucated value of function given the Xin input axis

;   PRINT: returns a string that displays the function in parinfo.
;   Keywords can be used to change verbosity

;   WIDGET: returns the widgetID of a compound widget that allows
;   interactive modification and dynamic updates of information stored
;   in parinfo.  Keywords and subsequent widget events affect
;   look-and-feel of widget

;   INDICES: returns list of indices into parinfo after parsing has
;   been done.  Subset of indices is returned depending on input
;   keywords.  Indices corresponding to particular parameters in
;   individual functions can be returned by specifying the
;   corresponding keywords (see __INDICES "methods" in individual PFO
;   fdefine files).


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
; $Id: pfo_parinfo_parse.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_parinfo_parse.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; We need to make sure pfo_ROI__calc doesn't get interpreted as an array if we haven't compiled the file pfo_roi_fdefine.pro
forward_function pfo_ROI__calc

function pfo_parinfo_parse, $
   parinfo, params=params_in, idx=idx, ispec=ispec, iROI=iROI, pfo_obj=pfo_obj, $
   status_mask=status_mask, $
   calc=calc, Xin=Xin, xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, convol_center=convol_center, $
   print=print, $
   indices=indices, expand_idx=expand_idx, terminate_idx=terminate_idx, $
   widget=widget, parentID=parentID, $
   _REF_EXTRA=extra

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH invocation errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: return_value = pfo_parinfo_parse(/CALC | /PRINT | /WIDGET | /INDICES, parinfo [, params=params][, Xin=Xin][, idx=idx]][, pfo_obj=pfo_obj][, xaxis=xaxis][, keyowrds to pfo_<fname> routines])'
     endif
  endif ;; not debugging

  ;; Do some setup work

  ;; Make sure parinfo and params are defined.  Also keep params local.
  pfo_parinfo2params, parinfo, params, params_in

  ;; Use pfo_fidx to get indices
  use_idx = pfo_fidx(parinfo, idx=idx, npar=n_use, status_mask=status_mask)

  ;; Narrow with pfo_ROI_idx
  use_idx = pfo_ROI_idx(parinfo, idx=use_idx, ispec=ispec, iROI=iROI, allspec=allspec, allROI=allROI, count=n_use)

  ;; Set up our different actions for the meat of the code.  Doing it this way allows us to use case statements and saves
  ;; huge indents.  Chose actions as a bitmask so that they don't overlap when we add them together.  This way we can check
  ;; for invalid multiple actions
  action = 0
  if keyword_set(calc) then $
    action += !pfo.calc
  if keyword_set(print) then $
    action += !pfo.print
  if keyword_set(indices) then $
    action += !pfo.indices
  if keyword_set(widget) then $
    action += !pfo.widget

  ;; Do some initial setup

  ;; For printing and widget display, we might want to have an introduction to the overall function (e.g. column headings).
  ;; We will pass this to the __print and __widget methods with call_function, below and then set it to 0.
  first_funct = 1

  case action of

     !pfo.print : begin
        ;; PRINTING
        action_method = '__print'
        ;; Initialize output
        toprint = ''
;;        ;; Default is verbose
;;        toprint = 'X = Xin; Y = 0' + !tok.newline
;;        firstprint = 1          ; flag for newline stuff
;;        ;; don't be verbose if we just want the parameters
;;        if size(print, /type) ne !tok.string then begin
;;           ;; Single-line basic parameter concise print
;;           if print eq !pfo.print then $
;;             toprint = ''
;;        endif ;; --> don't remember why we were passed a string
        ;; Make sure at least one parameter wants to be printed.
        if n_use eq 0 or total(parinfo[use_idx].mpprint) eq 0 then $
          return, toprint
     end ;; print

     !pfo.widget : begin
        ;; WIDGET
        action_method = '__widget'
        ;; Check to see if we need to create our own top-level widget
        if N_elements(parentID) eq 0 then $
          message, 'ERROR: parentID must be set'


        message, 'ERROR: need new code'

        ;; Here we benefit from being passed a heap pointer.  The widget code can handle pointers or regular variables,
        ;; so switch back to pointer mode if that is what we were passed
        if keyword_set(pparinfo) then begin
           ;; Move the data back to the heap
           *pparinfo = temporary(parinfo)
           ;; Point parinfo at it.  Except for a local pointer variable, pparinfo, we are back in the original
           ;; configuration.
           parinfo = pparinfo
        endif else begin
           ;; For syntactical purposes in the widget code, allow us to always work with heap pointers.
           pparinfo = ptr_new(parinfo, /no_copy)
           parinfo = pparinfo
           just_created_pparinfo = 1
        endelse

        ;; widget=0 is destroy, /widget is create or refill
        if widget eq 0 then begin
           ;;  Signal to destroy the widget.
           pfo_widget_destroy, pparinfo
           ;; Clean up after our temporary pparinfo, if appropriate
           if keyword_set(just_created_pparinfo) then begin
              parinfo = temporary(*pparinfo)
              ptr_free, pparinfo
           endif
           ;;  We return the return value of pfo_widget, which is the tlbID of the widget if it is still running or
           ;;  parinfo if it was the only widget on the block.  We couldn't destroying it through this method
           ;;  unless other code is running, so returning 0 is appropriate.
           return, 0L
        endif ;; destroy widget
        ;; If we got here, we are going to create or fill the widget.  This call does either depending on what it
        ;; finds in the pparinfo.
        pfo_widget_tlbID = $
          pfo_widget(parinfo, idx=use_idx, parinfo_containerID=parinfo_containerID, $
                     pfo_widget_pparinfo=pfo_widget_pparinfo, _EXTRA=extra)
        ;; Set widgetID for the primitives.  Note this will be the tlbID of an existing widget that is going to be
        ;; refilled or the parinfo_containerID (the widget with the scroll bars) of a widget that is being created
        widgetID = pfo_widget_tlbID
        if keyword_set(parinfo_containerID) then begin
           widgetID = parinfo_containerID
           ;; Start with our generic axis initializations.  I don't plan to have any changes possible in this, so
           ;; don't bother storing it anywhere.
           rowID = widget_base(parinfo_containerID, row=1)
           junkID = widget_label(rowID, value='X = Xin; Y = 0')
        end

        ;; Check to see if there is anything in our parinfo.  If not, just raise the empty frame
        if n_use le 0 then begin
           retval = pfo_widget(parinfo, idx=idx, $
                               parinfo_containerID=parinfo_containerID, $
                               pfo_widget_pparinfo=pfo_widget_pparinfo, _EXTRA=extra)
           ;; Clean up after our temporary pparinfo, if appropriate
           if keyword_set(just_created_pparinfo) then begin
              if N_elements(*pparinfo) gt 0 then $
                parinfo = temporary(*pparinfo)
              ptr_free, pparinfo
           endif
           return, retval

        endif

        ;; Put parinfo stuff back into non-pointer mode so all the code below works smoothly
        if keyword_set(pparinfo) then $
          parinfo = temporary(*pparinfo)

     end ;; widget

     !pfo.indices : begin
        ;; INDICES
        action_method = '__indices'
     end ;; indices

     !pfo.calc : begin
        ;; CALCULATING
        action_method = '__calc'
        ;; OBSELETE
        ;;        ;; Raise an error if we don't have a good parinfo.  We might want to change this to return 0 or something
        ;;        ;; else more quietly, but doing it this way make sure the callers are conscientious about checking for
        ;;        ;; size(parinfo, /type) eq !tok.struct
        ;;        if n_use lt 0 then $
        ;;          message, 'ERROR: improperly formatted function.  Check parinfo.'

        ;; Internal xaxis for calculations like dispersion relation.  Start from Xin.  Naturally raises an error if no Xin
        xaxis = Xin

        ;; Initialize our Y axis.  Make sure it is the same dimensions as Xin
        yaxis = Xin
        ;; Set all the values to NaN.  --> First function acting on Y must replace Y axis
        yaxis[*] = !values.d_NAN

        ;; Check to see if we have any calculation to do (note xaxis return keyword is properly initialized)
        if n_use eq 0 then $
          return, yaxis

        ;; Initialize our internal calculation axes in case we have *axis = !pfo.none
        x = xaxis
        y = yaxis

        ;; For efficiency sake, calculate ROI_Xaxis here if we are going to be working with ROIs that depend on Xaxis
        if pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
           ;; Check to see if we have any ROIs.  Do this "in the raw" so that we can get the 
           ;; rest of the parameters we need for calculating the Xaxis.
           ROI_idx = where(floor(parinfo[use_idx].pfo.ftype) eq $
                               pfo_fnum('pfo_ROI', pfo_obj=pfo_obj), nROI_pars, $
                               complement=no_ROI_idx, ncomplement=n_non_ROI)
           if nROI_pars gt 0 then begin
              ;; unwrap
              ROI_idx = use_idx[ROI_idx]
              ;; Check to see if any ROIs are referenced to Xaxis
              junk = where(parinfo[ROI_idx].pfo.inaxis eq !pfo.Xaxis, nXaxis)
              if nXaxis gt 0 then begin
                 ;; Use pfo_Xaxis to calc the Xaxis.  Make sure we
                 ;; remove ROIs from the calculation to avoid an infinite
                 ;; loop (I actually though of this before the first loop
                 ;; occured... :-)
                 ROI_Xaxis = Xin
                 if n_non_ROI gt 0 then begin
                    ;; unwrap
                    no_ROI_idx = use_idx[no_ROI_idx]
                    ;; pfo_Xaxis takes care of filtering out the no_ROI_idx that don't contribute to the Xaxis
                    ROI_Xaxis = pfo_Xaxis(Xin, params, parinfo=parinfo, idx=no_ROI_idx, $
                                          _EXTRA=extra)
                 endif ;; some non-ROI parameters in the parinfo
              endif ;; some ROIs in Xaxis coordinates
           endif ;; some ROIs
        endif ;; ispec/ROI tag present

     end ;; /CALC

     else : message, 'ERROR: specify one of /CALC, /PRINT, /WIDGET, /INDICES'

  endcase ;; setting up for actions

  ;; Now that our inputs have been verified, no longer CATCH errors at this level.  If an error does occur, allow the normal
  ;; IDL stop for easier interactive debugging.  Of course, any errors can still be caught by the calling routine.
  CATCH, /CANCEL
  ON_ERROR, !tok.stop

  ;; Now untangle the various levels of ispec, iROI, fseq, axes, etc.
  ;; This is the basic interpreter, so it is common to printing and
  ;; calculating, widget, and whatever else needs it.

  ;; Get ready to handle the Regions of Interested (ROIs) and multiple
  ;; spectra.  This happens on two levels.  (1) we have some ROI
  ;; functions laying around that aren't associated with any
  ;; particular functions (2) we have the pfo_ROI structure
  ;; appended, which enables that association.  Handle both cases.

  ;; Cycle through the ispecs.

  ;; Use pfo_uniq to get a reverse index list which allows easy access to groups of identical ispec.  NOTE: potentially easy
  ;; to miss variable naming convention: reverse index list from pfo_uniq is stored in arrays ending in _r_idx.  Indices
  ;; pulled out from the _r_idx just end in _idx.  In the case of ispec and iROI, to save memory, do this in two cases: one
  ;; if are using ROIs and one if we are not.
  if pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
     ;; This is going to be the general pattern of how we work through the parinfo.pfo[_ROI] structure
     junk = parinfo[use_idx].pfo_ROI.ispec
     junk = pfo_uniq(junk, sort(junk), reverse_indices=ispec_r_idx, N_uniq=N_ispec)
     if N_ispec gt 1 then $
        message, 'ERROR: multiple ispecs are not yet supported'
  endif else begin
     ;; If we don't have a pfo_ROI, use pfo_uniq to get all the right answers, but try to do so with the minimum of memory
     junk = pfo_uniq(make_array(n_use, value=0B), reverse_indices=ispec_r_idx, N_uniq=N_ispec)
  endelse
  for iu_ispec=0, N_ispec-1 do begin
     ;; Pick out the ith set of indices that point to identical values
     ispec_idx = ispec_r_idx[ispec_r_idx[iu_ispec]:ispec_r_idx[iu_ispec+1]-1]
     ;; unwrap so that these are indices into parinfo
     ispec_idx = use_idx[temporary(ispec_idx)]

     ;; Now check ROIs within this ispec.
     if pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
        junk = parinfo[ispec_idx].pfo_ROI.iROI
        junk = pfo_uniq(junk, sort(junk), reverse_indices=iROI_r_idx, N_uniq=N_iROI)
     endif else begin
        ;; If we don't have a pfo_ROI, use pfo_uniq to get all the right answers, but try to do so with the minimum of memory
        junk = pfo_uniq(make_array(n_use, value=0B), reverse_indices=iROI_r_idx, N_uniq=N_iROI)
     endelse

     for iu_iROI=0, N_iROI-1 do begin
        ;; Pick out the ith set of indices that point to identical values
        iROI_idx = iROI_r_idx[iROI_r_idx[iu_iROI]:iROI_r_idx[iu_iROI+1]-1]
        ;; unwrap so that these are indices into parinfo
        iROI_idx = ispec_idx[temporary(iROI_idx)]
;;if iu_iROI eq 1 then stop
        ;; Seach for ROI functions in our set of indices
        ROI_idx = pfo_fidx(parinfo, 'pfo_ROI', idx=iROI_idx, nfunct=nROI, pfo_obj=pfo_obj)

        ;; The iROI = !pfo.allROI is a special case which lets us have multiple ROI functions across the X-axis _with the
        ;; same iROI_.  The advantage is that we can can assign functions to iROI=!pfo.allROI and have them over all of
        ;; those ROIs at once.  Otherwise, we would need to define the function on each ROI and link the parameters
        ;; together.  Check here to make sure the user isn't trying to have more than one ROI that is not !pfo.allROI
        if nROI gt 1 then $
          if parinfo[ROI_idx].pfo_ROI.iROI ne !pfo.allROI then $
            message, 'ERROR: more than one ROI function associated with iROI = ' + strtrim(iROI, 2)

        ;; Get our ROI parameters in order.  We know that each ROI has the same number of parameters, so we can just sort and
        ;; reform
        if nROI gt 0 then begin
           ;; This should put all of the lefts together and then rights.  The way sort works, the first listed left ends up
           ;; first, etc.  Reform then quickly puts the array in the format we want: each instance of the function occupies a
           ;; column and the rows correspond to parameter numbers.  The array values themselves are indices into ROI_idx.
           fidx_array = sort(parinfo[ROI_idx].pfo.ftype)
           fidx_array = reform(fidx_array, nROI, pfo_fnpars('pfo_ROI', pfo_obj=pfo_obj), /overwrite)
        endif ;; have some ROIs

        ;; Loop through our ROI(s).  If no ROIs were found, the whole
        ;; Xin axis is a ROI, so we need to go through at least once.
        for itROI=0, max([1, nROI])-1 do begin
           ;; When we are calculating, set up the indices into Xin/xaxis
           if action eq !pfo.calc then begin
              if nROI eq 0 then begin
                 Xin_ROI_idx = lindgen(N_elements(Xin))
              endif else begin
                 ;; Use pfo_ROI to get the indices into Xin/Xaxis for this ROI.  Make sure that pfo_ROI__fdefine is compiled,
                 ;; since pfo_ROI__calc is likely hiding inside of it
                 resolve_routine, 'pfo_ROI__fdefine', /no_recompile
                 Xin_ROI_idx = $
                   pfo_ROI__calc(Xin, params, parinfo=parinfo, $
                                 idx=ROI_idx[fidx_array[itROI, *]], $
                                 count=count, Xaxis=ROI_Xaxis, pfo_obj=pfo_obj)
                 ;; Check to see if the ROI resulted in any valid idx.  If not, skip to the next ROI
                 if count eq 0 then $
                   CONTINUE
              endelse ;; multi-ROI over whole X-axis
              nXin_ROI_idx = N_elements(Xin_ROI_idx)
              ;; Take care of our ROI_Xin_idx output keyword, being polite not to build it up if the user doesn't want it.
              ;; If they specified it on the command line, the first time through it is either an undefined variable or a
              ;; variable they want reinitialized
              if NOT keyword_set(after_first_ROI_Xin_idx) and $
                 (N_elements(ROI_Xin_idx) ne 0 or arg_present(ROI_Xin_idx)) then begin
                 ;; Initilize ROI_Xin_idx for first and subsequent calls to pfo_array_append
                 ROI_Xin_idx = ''
              endif ;; initializing ROI_Xin_idx to a null list our first time around
              if N_elements(ROI_Xin_idx) ne 0 then begin
                 ;; Build up output ROI_Xin_idx
                 pfo_array_append, ROI_Xin_idx, Xin_ROI_idx
                 after_first_ROI_Xin_idx = 1
              endif ;; Building up output ROI_Xin_idx
           endif ;; calc ROI setup


           ;; Call the functions by sequence within each ROI
           junk = parinfo[iROI_idx].pfo.fseq
           junk = pfo_uniq(junk, sort(junk), reverse_indices=fseq_r_idx, N_uniq=N_fseq)
           for iseq=0, N_fseq-1 do begin
              ;; Pick out the ith set of indices that point to identical values
              fseq_idx = fseq_r_idx[fseq_r_idx[iseq]:fseq_r_idx[iseq+1]-1]
              ;; unwrap so that these are indices into parinfo
              fseq_idx = iROI_idx[temporary(fseq_idx)]

              ;; Decend to the next level: inaxis
              junk = parinfo[fseq_idx].pfo.inaxis
              junk = pfo_uniq(junk, sort(junk), reverse_indices=inaxis_r_idx, N_uniq=N_inaxis)
              for iinaxis=0, N_inaxis-1 do begin
                 ;; Pick out the ith set of indices that point to identical values
                 inaxis_idx = inaxis_r_idx[inaxis_r_idx[iinaxis]:inaxis_r_idx[iinaxis+1]-1]
                 ;; unwrap so that these are indices into parinfo
                 inaxis_idx = fseq_idx[temporary(inaxis_idx)]

                 ;; Outaxis
                 junk = parinfo[inaxis_idx].pfo.outaxis
                 junk = pfo_uniq(junk, sort(junk), reverse_indices=outaxis_r_idx, N_uniq=N_outaxis)
                 for ioutaxis=0, N_outaxis-1 do begin
                    outaxis_idx = outaxis_r_idx[outaxis_r_idx[ioutaxis]:outaxis_r_idx[ioutaxis+1]-1]
                    outaxis_idx = inaxis_idx[temporary(outaxis_idx)]

                    ;; Operation
                    junk = parinfo[outaxis_idx].pfo.fop
                    junk = pfo_uniq(junk, sort(junk), reverse_indices=fop_r_idx, N_uniq=N_fop)
                    for ifop=0, N_fop-1 do begin
                       fop_idx = fop_r_idx[fop_r_idx[ifop]:fop_r_idx[ifop+1]-1]
                       fop_idx = outaxis_idx[temporary(fop_idx)]

                       if parinfo[fop_idx[ifop]].pfo.fop eq !pfo.repl and N_fop gt 1 then $
                         message, 'ERROR: more than one transforming/replacing operation in this sequence acting on the same axis combination.'
                       ;; Functions: Recall that ftype is a real number, with the integer determining the function type and
                       ;; the decimal the parameter of that function.  We want to work with the integer part, the fnum.
                       ;; Save them for use below
                       fnums = floor(parinfo[fop_idx].pfo.ftype)
                       junk = pfo_uniq(fnums, sort(fnums), reverse_indices=fnum_r_idx, N_uniq=N_fnums)
                       for ifnum=0, N_fnums-1 do begin
                          fnum_idx = fnum_r_idx[fnum_r_idx[ifnum]:fnum_r_idx[ifnum+1]-1]
                          fnum_idx = fop_idx[temporary(fnum_idx)]

                          ;; Collect some information on our function.  Remember to reset fname, or else finfo will
                          ;; complain....
                          fnum = fnums[ifnum]
                          fname = ''
                          pfo_finfo, fnum=fnum, fname=fname, fnpars=fnpars, pfo_obj=pfo_obj

                          ;; As advertised, the last thing used for unique identification is ID.  If the user is careful to
                          ;; assign a unique ID to each set of parameters that represent a function, the parameter list can
                          ;; be totally randomized and we can put it back together again here
                          junk = parinfo[fnum_idx].pfo.ID
                          junk = pfo_uniq(junk, sort(junk), reverse_indices=ID_r_idx, N_uniq=N_IDs)
                          for iID=0, N_IDs-1 do begin
                             ID_idx = ID_r_idx[ID_r_idx[iID]:ID_r_idx[iID+1]-1]
                             ID_idx = fnum_idx[temporary(ID_idx)]

                             ;; Even with all this work, there still might be multiple instances of function fnum.  npar is
                             ;; the total number of parameters of function fnum
                             npar = N_elements(ID_idx)

                             ;; If we know the number of parameters of our function, we are able to take a decent guess at
                             ;; putting multiple instances of a function together.  NOTE: parameters will be assembled in the
                             ;; order in which they are listed in the original parinfo array
                             if fnpars ne 0 then begin
                                ;; Check to see that we have the right number of parameters
                                if npar mod fnpars ne 0 then $
                                  message, 'ERROR: incorrect number of parameters for function type ' + fname + ': ' + strtrim(npar, 2)
                                

                                ;; This should put all of the decimal ftypes of the same kind together (see similar code in
                                ;; ROI section, above).  The way sort works, the each individual instance of an ftype gets
                                ;; listed listed in the order they were originally found in the parinfo.  The next set of
                                ;; f_array_indices point to the next decimal ftype, etc.
                                fidx_array = sort(parinfo[ID_idx].pfo.ftype)
                                ;; Having stacked up all of the indices in a long line in memory, use reform to just pretend
                                ;; that each row corresponds to a parameter (IDL is a row major language) and each column to
                                ;; an instance of the function
                                fidx_array = reform(fidx_array, npar/fnpars, fnpars, /overwrite)
                             endif else begin
                                ;; This is the fnpars = 0 case, which signals that we don't know the number of parameters our
                                ;; function is supposed to have.  In this case, we can only assume that all of the parameters
                                ;; of this fnum and ID belong to one instance of the function.  Put together fidx_array in
                                ;; the same row-major way we do for multiple instances of the same function
                                fidx_array = transpose(indgen(npar, type=size(/type, npar)))
                             endelse
                             ;; The number of functions is the number of columns we have in our fidx_array
                             nfunct = (size(/dimensions, fidx_array))[0]

                             ;; Call or print the functions in parray one at a time.
                             for iIDfn=long(0), nfunct-1 do begin
                                ;; Assemble parameters for this particular instance of the function.  Reform is necessary to
                                ;; remove vestigial dimensions
                                fidx = reform(ID_idx[fidx_array[iIDfn, *]])

                                case action of
                                   ;; INDICES
                                   !pfo.indices : begin
                                      ;; Check to see if the user wants to expand an idx into all of the idx of that
                                      ;; particular function.  If the user does not want to do that, just accumulate all of
                                      ;; the idx (see else)
                                      if N_elements(expand_idx) gt 0 then begin
                                         ;; Use a flag variable to determine if one or more indices in expand_idx are found
                                         ;; in this function.  Do it this way so we don't get multiple copies of fidx!
                                         expand_this_funct = 0
                                         for ie=0, N_elements(expand_idx)-1 do begin
                                            junk = where(expand_idx[ie] eq fidx, count)
                                            if count gt 0 then $
                                              expand_this_funct = 1
                                         endfor ;; checking all expand_idx against this function
                                         if keyword_set(expand_this_funct) then $
                                           pfo_array_append, indices_idx, fidx
                                      endif else begin
                                         ;; If we are not expanding a particular (set of) idx, just accumulate our set of
                                         ;; parsed fidx in indices_idx
                                         pfo_array_append, indices_idx, fidx
                                      endelse 
                                      ;; In either case, put in our parsing marks, if desired.
                                      if keyword_set(terminate_idx) then $
                                        pfo_array_append, indices_idx, !tok.nowhere
                                   end ;; indices

                                   ;; CALCULATING                          
                                   !pfo.calc : begin
                                      ;; Make temporary variables x and y, which are the generic input and output of the
                                      ;; function
                                      case parinfo[fidx[0]].pfo.inaxis of
                                         0            : ; no change in x
                                         !pfo.Xin     : x = Xin[Xin_ROI_idx]
                                         !pfo.Xaxis   : x = xaxis[Xin_ROI_idx]
                                         !pfo.Yaxis   : x = yaxis[Xin_ROI_idx]
                                         else : message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                                      endcase
                                      ;; Do our X-axis transformation
                                      if keyword_set(parinfo[fidx[0]].pfo.infunct) then $
                                        x = call_function(parinfo[fidx[0]].pfo.infunct, x)
                                      case parinfo[fidx[0]].pfo.outaxis of
                                         0            : ; no change in y
                                         !pfo.Xin     : message, 'ERROR: cannot write to input axis'
                                         !pfo.Xaxis   : y = xaxis[Xin_ROI_idx]
                                         !pfo.Yaxis   : y = yaxis[Xin_ROI_idx]
                                         else : message, 'ERROR: unrecognized outaxis value ' + string(oaxis)
                                      endcase
                                   end ;; calculating
                                   else :
                                endcase ;; action

                                ;; ALL ACTIONS 

                                ;; Make a catch in case the local copy of pfo doesn't have this __fdefine
                                CATCH, err
                                if err ne 0 then begin
                                   CATCH, /CANCEL
                                   message, /CONTINUE, 'ERROR: file ' + fname + '__fdefine.pro not found.  Skipping this function with possibly disasterous results....'
                                   CONTINUE
                                endif ;; catching missing __fdefine

                                ;; Create a command that calls the pfo primitive.  First, make sure that all methods for this
                                ;; primitive are compiled.  This assumes that they are all collected into fname__fdefine.
                                ;; The user may not have created the parinfo with pfo_parinfo_new in this session
                                ;; --> I might need a catch here
                                resolve_routine, fname + '__fdefine', /no_recompile

                                ;; Now call fname__action.  If that doesn't exist, call pfo_null__action.  This allows
                                ;; pfo_null to be a fallback for things like printing and widget for most simple functions.
                                ;; CATCH makes this work, but the code reads backwards: else executes first.
                                CATCH, err
                                if err ne 0 then begin
                                   CATCH, /CANCEL
                                   funct_name = 'pfo_null' + action_method
                                endif else begin
                                   ;; Use IDL's routine_info to raise an error if fname__action_method is not found.
                                   ;; Unfortunately, this bombs if the routine has yet to be compiled, hence the need to
                                   ;; resolve_routine.  We do resolve_routine on two levels.  One assuming that all of the
                                   ;; fname code is in fname__fdefine.pro (above) and once assuming the individual methods
                                   ;; are separate.
                                   funct_name = fname + action_method
                                   resolve_routine, funct_name, /no_recompile, /either
                                   init_params = routine_info(funct_name, /functions, /parameters)
                                endelse ;; first time through checking for fname__action
                                CATCH, /CANCEL

                                ;; Catch errors in our primitive functions unless the user is in PFO debugging mode
                                if !pfo.debug le 0 then begin
                                   CATCH, err
                                   if err ne 0 then begin
                                      CATCH, /CANCEL
                                      ;; For actions that result in text output, put error in text
                                      case action of 
                                         !pfo.print : begin
                                            toprint = toprint + ' %PFO_PARINFO_PARSE: ERROR: Caught the following error while calling ' + funct_name + ': ' + !error_state.msg + ')'
                                         end
                                         !pfo.widget : begin
                                            junk = widget_text(widgetID, value='Caught the following error while calling ' + funct_name + ': ' + !error_state.msg)
                                         end
                                         else:
                                      endcase ;; putting error into text output methods
                                      ;; For all actions, print message to terminal
                                      message, /CONTINUE, /NONAME, !error_state.msg
                                      message, /CONTINUE, 'ERROR: caught the above error while calling ' + funct_name + '.  Ignoring this function and trying to continue'
                                      CONTINUE
                                   endif ;; caught error
                                endif  ;; not debugging


                                ;; call function is apparently faster than execute and is appropriate if you don't have to
                                ;; construct arguments.  The CATCH in mpfit also works better this way.  In case functions
                                ;; need to use more than just the input axis (e.g. convlution by an X dependent instrument
                                ;; profile), pass all axes.  Make sure your pfo_ function has _EXTRA, so these can be safely
                                ;; ignored.
                                fy = call_function(funct_name, x, params, $
                                                   parinfo=parinfo, idx=fidx, $
                                                   indices=indices, $
                                                   fname=fname, first_funct=first_funct, $
                                                   pfo_obj=pfo_obj, $
                                                   _EXTRA=extra, $
                                                   widget=widgetID, $
                                                   Xorig=Xin, xaxis=xaxis)

                                ;; Set our first_funct flag to 0, since we are done with it
                                first_funct = 0

                                case action of 
                                   !pfo.print : toprint += fy
                                   !pfo.widget : begin
                                      ;; Add the closing parenthesis to our function
                                      if keyword_set(parinfo_containerID) then begin
                                         rowID = widget_base(parinfo_containerID, row=1)
                                         junkID = $
                                           widget_label(rowID, value=')', uvalue={tlbID : pfo_widget_tlbID})
                                      endif
                                   end
                                   !pfo.calc : begin
                                      ;; CALCULATING

                                      ;; Do our simple tranformation on the
                                      ;; output axis, if specified
                                      if keyword_set(parinfo[fidx[0]].pfo.outfunct) then $
                                        fy = call_function(parinfo[fidx[0]].pfo.outfunct, fy)
                                      
                                      case parinfo[fidx[0]].pfo.fop of 
                                         0		: ; no output
                                         !pfo.repl	: y = fy
                                         !pfo.add	: y = y + fy
                                         !pfo.mult	: y = y * fy
                                         !pfo.convol: y = convol(y, fy, center=convol_center, _EXTRA=extra)
                                         else	: message, 'ERROR: unrecognized operation ' + string(fop)
                                      endcase ;; fop

                                      ;; Put y into the desired output axis
                                      case parinfo[fidx[0]].pfo.outaxis of
                                         0		: ; no change in y
                                         !pfo.Xin	: message, 'ERROR: cannot write to input axis'
                                         !pfo.Xaxis	: xaxis[Xin_ROI_idx] = y
                                         !pfo.Yaxis	: yaxis[Xin_ROI_idx] = y
                                         else	: message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                                      endcase ;; oaxis
                                   end ;; calculating
                                   else :
                                endcase ;; action

                             endfor  ;; each instance of fnum with this ID (iIDfn)
                          endfor ;; iID
                       endfor ;; ifnum
                    endfor ;; ifop
                 endfor ;; ioutaxis
              endfor ;; iinaxis
           endfor ;; iseq
        endfor ;; itROI (each ROI)
     endfor ;; iu_iROI (each iROI)
  endfor ;; iu_ispec


  ;; Return value depends on our action
  case action of
     !pfo.print : return, toprint
     !pfo.widget : begin
        retval = pfo_widget(parinfo, idx=idx, $
                            parinfo_containerID=parinfo_containerID, $
                            pfo_widget_pparinfo=pfo_widget_pparinfo, _EXTRA=extra)
        ;; Clean up after our temporary pparinfo, if appropriate
        if keyword_set(just_created_pparinfo) then begin
           parinfo = temporary(*pparinfo)
           ptr_free, pparinfo
        endif
        return, retval

     end
     !pfo.indices : return, indices_idx
     !pfo.calc : return, yaxis
     else :
  endcase

end
