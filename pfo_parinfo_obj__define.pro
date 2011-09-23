;+
; NAME: pfo_parinfo_obj__define
;
; PURPOSE: define the object that stores "parinfo," the data structure
; that defines the function in the PFO system
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
; $Id: pfo_parinfo_obj__define.pro,v 1.7 2011/09/23 13:08:18 jpmorgen Exp $
;
; $Log: pfo_parinfo_obj__define.pro,v $
; Revision 1.7  2011/09/23 13:08:18  jpmorgen
; Add edit, parinfo_edit methods and minor changes
;
; Revision 1.6  2011/09/16 11:26:16  jpmorgen
; Added debug level 2 to pfo_debug, so CATCH in update is disabled
;
; Revision 1.5  2011/09/15 20:52:06  jpmorgen
; Improved update to handle transition from inactive to delete
;
; Revision 1.4  2011/09/08 19:59:41  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.3  2011/09/01 22:14:11  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:15:59  jpmorgen
; Release to Tom
; Init bug fixes, improved messages, improved descr
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; Provide print, widget and indices methods for the parinfo
;; encapsulated in the pfo_obj.  Also allow the ability to swap a
;; parinfo in on the fly.  This ia a little dangerous, since we
;; don't know where the external parinfo comes from, whether or
;; not it has the same pfo_fstruct_array as encapsulated in the
;; pfo_obj, etc.
function pfo_parinfo_obj::print, $
   parinfo=parinfo, $ ;; Capture parinfo so we can get it into the object properly
   pfo_fstruct_array=pfo_fstruct_array, $ ;; If external parinfo is being used, its pfo_fstruct_array should be passed too
   _REF_EXTRA=extra

  ;; Handle parinfo separately by swapping it in to the object
  if N_elements(parinfo) ne 0 then begin
     Eparinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(parinfo)
  endif

  ;; This is the meat of our code
  retval = pfo_parinfo_parse(/print, *self.pparinfo, pfo_obj=self, /full, _EXTRA=extra)

  ;; pfo_parinfo_parse should return gently even if there is an error
  ;; (at least in pfo_debug, 0), so we should get here and replace our
  ;; calling routine's parinfo
  if N_elements(Eparinfo) ne 0 then begin
     parinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(Eparinfo)
  endif

  return, retval

end

;; The widget method returns the widget ID of a function editing
;; widget.  It is going to get overridden by higher-level widgets
function pfo_parinfo_obj::widget, $
   parinfo=parinfo, $ ;; Capture parinfo so we can get it into the object properly
   _REF_EXTRA=extra

  ;; Handle parinfo separately by swapping it in to the object
  if N_elements(parinfo) ne 0 then begin
     Eparinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(parinfo)
  endif

  ;; This is the meat of our code
  retval = pfo_parinfo_parse(/widget, *self.pparinfo, pfo_obj=self, _EXTRA=extra)

  ;; pfo_parinfo_parse should return gently even if there is an error
  ;; (at least in pfo_debug, 0), so we should get here and replace our
  ;; calling routine's parinfo
  if N_elements(Eparinfo) ne 0 then begin
     parinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(Eparinfo)
  endif

  return, retval

end

;; Define a parinfo_edit method that is just a wrapper around
;; self->widget() _of this object_, since it is handy to sometimes
;; just edit the parinfo
pro pfo_parinfo_obj::parinfo_edit, $
   _REF_EXTRA=extra
  junk = self->pfo_parinfo_obj::widget(_EXTRA=extra)
end

;; The edit method points to parinfo_edit at this level.  The edit
;; method will be overridden by inheriting objects to reflect the
;; complexity of the object
pro pfo_parinfo_obj::edit, $ $
   _REF_EXTRA=extra
  self->parinfo_edit, _EXTRA=extra
end

function pfo_parinfo_obj::indices, $
   parinfo=parinfo, $ ;; Capture parinfo so we can get it into the object properly
   _REF_EXTRA=extra

  ;; Handle parinfo separately by swapping it in to the object
  if N_elements(parinfo) ne 0 then begin
     Eparinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(parinfo)
  endif

  ;; This is the meat of our code
  retval = pfo_parinfo_parse(/indices, *self.pparinfo, pfo_obj=self, _EXTRA=extra)

  ;; pfo_parinfo_parse should return gently even if there is an error
  ;; (at least in pfo_debug, 0), so we should get here and replace our
  ;; calling routine's parinfo
  if N_elements(Eparinfo) ne 0 then begin
     parinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(Eparinfo)
  endif

  return, retval

end


function pfo_parinfo_obj::parinfo, $
   no_copy=no_copy ;; Dangerous!  This guts the pfo_parinfo_obj of parinfo, so any additional calls to this object that require knowledge of parinfo will fail!

  self->get_property, parinfo_array=parinfo, no_copy=no_copy
  return, parinfo

end

;; This is a handy hack that calls a procedure, whose name is passed
;; as a string, proc.  It allows external procedures to access
;; parinfo, which can be rather large, by reference.  Proc is called
;; with parinfo as its first positional parameter and optionally
;; parameters p1...p14 and keywords passed by the calling routine.  I
;; wish there was a more general way to handle positional parameters,
;; but I think this is nicer than requiring only keywords.  If you
;; need to do more fiddling with internal self tags other than
;; parinfo, write another method (e.g. parinfo_template)
pro pfo_parinfo_obj::parinfo_call_procedure, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   no_update=no_update, $ ;; (DANGEROUS!) call does not modify parinfo or caller will call update by hand
   save_undo=save_undo, $ ;; Save current parinfo (before call, but after sucessful update) in undo list (if active)
   _REF_EXTRA=extra

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far.', /CONTINUE
        return
     endif
  endif ;; not debugging

  if NOT keyword_set(no_update) then $
     self->prepare_update, undo

  case N_params() of
     0: message, 'ERROR: Procedure name not supplied'
     1: begin
        if N_elements(extra) eq 0 then begin
           call_procedure, proc, *self.pparinfo
        endif else begin
           call_procedure, proc, *self.pparinfo, _EXTRA=extra
        endelse
     end
     2: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1 $
        else $
          call_procedure, proc, *self.pparinfo, p1, _EXTRA=extra
     end
     3: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, _EXTRA=extra
     end
     4: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, _EXTRA=extra
     end
     5: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, _EXTRA=extra
     end
     6: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, _EXTRA=extra
     end
     7: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, _EXTRA=extra
     end
     8: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra
     end

     else: message, 'ERROR: too many positional parameters: ' + strtrim(N_params(), 2)
  endcase 

  if NOT keyword_set(no_update) then $
     self->update, undo, save_undo=save_undo


end

function pfo_parinfo_obj::parinfo_call_function, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   no_update=no_update, $ ;; (DANGEROUS!) call does not modify parinfo or caller will call update by hand
   save_undo=save_undo, $ ;; Save current parinfo (before call, but after sucessful update) in undo list (if active)
   _REF_EXTRA=extra

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning NaN.', /CONTINUE
        return, !values.d_NAN
     endif
  endif ;; not debugging

  if NOT keyword_set(no_update) then $
     self->prepare_update, undo

  case N_params() of 
     0: message, 'ERROR: Function name not supplied'
     1: begin
        if N_elements(extra) eq 0 then begin
           return, call_function(proc, *self.pparinfo)
        endif else begin
           return, call_function(proc, *self.pparinfo, _EXTRA=extra)
        endelse
     end
     2: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1) $
        else $
          return, call_function(proc, *self.pparinfo, p1, _EXTRA=extra)
     end
     3: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, _EXTRA=extra)
     end
     4: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, _EXTRA=extra)
     end
     5: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, _EXTRA=extra)
     end
     6: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, _EXTRA=extra)
     end
     7: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, _EXTRA=extra)
     end
     8: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra)
     end

     else: message, 'ERROR: too many positional parameters: ' + strtrim(N_params(), 2)
  endcase 

  if NOT keyword_set(no_update) then $
     self->update, undo, save_undo=save_undo

end

pro pfo_parinfo_obj::redo

  ;; A valid object is our flag that we are keeping an undo/redo list
  if NOT obj_valid(self.parinfo_redo_obj) then $
     return

  ;; If we made it here, we are keeping an redo list

  ;; See if we have anything in it.
  if self.parinfo_redo_obj->get_count() eq 0 then $
     return

  ;; If we made it here, we have at least one thing on our redo list.
  ;; Prepare to use it.  Our current parinfo will become the local
  ;; undo
  self->prepare_update, undo

  ;; Put our current parinfo on the undo list
  self.parinfo_undo_obj->add, *self.pparinfo
  ;; Replace our parinfo with the one from the undo list.  Make sure
  ;; we copy it so the node can be deleted from the list
  *self.pparinfo = self.parinfo_redo_obj->get_item(/dereference)
  ;; Delete the last item in the redo list
  self.parinfo_redo_obj->delete
  ;; Run update with the undo we saved above to see if we need a
  ;; refresh or repopulate
  self->update, undo

end

pro pfo_parinfo_obj::undo

  ;; A valid object is our flag that we are keeping an undo list
  if NOT obj_valid(self.parinfo_undo_obj) then $
     return

  ;; If we made it here, we are keeping an undo list

  ;; See if we have anything in it.
  if self.parinfo_undo_obj->get_count() eq 0 then $
     return
  
  ;; If we made it here, we have at least one thing on our undo list.
  ;; Prepare to use it.  Our current parinfo will become the local undo
  self->prepare_update, undo

  ;; Put our current parinfo on the redo list.  Don't add an empty parinfo
  if N_elements(*self.pparinfo) ne 0 then $
     self.parinfo_redo_obj->add, *self.pparinfo
  ;; Replace our parinfo with the one from the undo list.  Make sure
  ;; we copy it so the node can be deleted from the list
  *self.pparinfo = self.parinfo_undo_obj->get_item(/dereference)
  ;; Delete the last item in the undo list
  self.parinfo_undo_obj->delete
  ;; Run update with the undo we saved above to see if we need a
  ;; refresh or repopulate
  self->update, undo

end

pro pfo_parinfo_obj::save_undo, $
   undo ;; optional -- current parinfo in self is used if not defined

  ;; A valid object is our flag that we are keeping an undo list
  if NOT obj_valid(self.parinfo_undo_obj) then $
     return

  ;; Add the specified undo parinfo or the current parinfo, if no
  ;; explicit undo was specified
  if N_elements(undo) ne 0 then begin
     self.parinfo_undo_obj->add, undo
  endif else begin
     self.parinfo_undo_obj->add, *self.pparinfo
  endelse

  ;; Doing something and saving it on the undo list implies that we
  ;; cannot redo previous undos, so delete all the elements in the
  ;; redo list
  self.parinfo_redo_obj->delete, /all


end

;; Prepare_update should be run before any changes are made to the
;; parinfo so that a proper undo can be saved.  If widgets are being
;; displayed, prepare_update also makes sure that the pfo_unique tag
;; is in the parinfo so that the proper decision can be made in ::
;; update as to whether the widgets can be refreshed in place or the
;; container widgets need to be repopulated.
pro pfo_parinfo_obj::prepare_update, $
   undo, $ ;; (optional input) previous state of parinfo before calling routine tweaked the encapsulated parinfo
   _REF_EXTRA=extra

  ;; Handle the case where we haven't been given an undo
  if N_elements(undo) eq 0 then begin

     ;; If we don't have a parinfo, we can't create an undo with it!
     if N_elements(*self.pparinfo) eq 0 then $
        return

     ;; If we made it here, we have a parinfo.  Do a basic check to make
     ;; sure that it is valid.

     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Report on the console.  Note that adding to the message beefs
        ;; up !error_state.msg for the widget display case
        message, /NONAME, !error_state.msg + '  Undo will be undefined.', /CONTINUE
        ;; If we have _any_ displayed widgets associated with this
        ;; pfo_obj, report with a widget too
        if N_elements(*self.pcw_obj_list) gt 0 then $
           junk = dialog_message(!error_state.msg)
        ;; return, leaving undo undefined
        return
     endif ;; error parsing existing parinfo

     ;; Turn debugging on so that we can catch and report our
     ;; pfo_parinfo_parse error
     odebug = !pfo.debug
     !pfo.debug = 1

     ;; Make sure that all of the parinfo can parse, since we will
     ;; probably be parsing it in the widget stuff
     junk = pfo_parinfo_parse(/indices, status_mask=!pfo.all_status, $
                              *self.pparinfo, pfo_obj=self, _extra=extra)

     ;; If we made it here, pfo_parinfo_parse worked OK.  Put our debug
     ;; level back to where it was.
     !pfo.debug = odebug

     ;; It is probably safe to assign undo to the existing parinfo
     undo = *self.pparinfo

  endif ;; creating an undo

  ;; If we made it here, we have an undo that parses properly
  
  ;; If we have any widgets that display _parinfo_ stuff, make sure we
  ;; have the pfo_unique tag, so that we can differentiate between the
  ;; repopulate and refresh cases.  Only run update for pfo_unique
  if N_elements(*self.pparinfo_repop_list) + $
     N_elements(*self.pparinfo_refresh_list) gt 0 then begin
     pfo_parinfo_update, undo, required_tags='pfo_unique', $
                         update_only_tags='pfo_unique', pfo_obj=self
  endif ;; have parinfo widgets     

end

;; Run all of the <tag>_struct__update "methods" of the parinfo
;; top-level tags and repopulate or refresh any displayed widgets
pro pfo_parinfo_obj::update, $
   undo, $ ;; (optional input) previous state of parinfo before calling routine tweaked the encapsulated parinfo
   save_undo=save_undo, $ ;; save our undo if the update was successful
   no_widget=no_widget, $ ;; don't refresh or repopulate widgets (this routine must be called again to get them to display right!)
   _REF_EXTRA=extra ;; passed to prepare_update and pfo_parinfo_update

  ;; Handle pfo_debug level.  CATCH errors if debug level is 1 or less
  if !pfo.debug le 1 then begin
     ;; Catch any errors so we can put undo back
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Report error to console
        message, /NONAME, !error_state.msg + '  Returning with whatever I have done and reverting the encapsulated parinfo to previous state, if possible', /CONTINUE
        ;; And with a dialog box if we have any displayed widgets
        if N_elements(*self.pcw_obj_list) gt 0 then $
           junk = dialog_message(!error_state.msg)
        ;; Put our debug level back to where it was.
        !pfo.debug = odebug
        ;; As promised, set parinfo back to undo, if undo exists
        if N_elements(undo) gt 0 then $
           *self.pparinfo = undo
        ;; Redisplay our widget to clear whatever we just changed
        ;; Refresh non-parinfo widgets first
        self->refresh
        ;; Refresh parinfo widgets
        pfo_idx, *self.pparinfo, idx
        self->refresh, idx=idx
        return
     endif ;; catching our errors
  endif ;; debugging

  ;; Turn debugging on so we can catch errors with our code above in
  ;; the !pfo.debug=1 or =0 case
  odebug = !pfo.debug
  !pfo.debug = 1

  ;; Run our updates in the parinfo.  This quietly returns if parinfo is undefined
  pfo_parinfo_update, *self.pparinfo, pfo_obj=self, _EXTRA=extra
  ;; Find the parsed order of our undo parinfo.  This quietly returns
  ;; -1 if undo is undefined.  Make sure that all of the parinfo can
  ;; parse, since we will be parsing it in the widget stuff
  orig_indices = pfo_parinfo_parse(/indices, status_mask=!pfo.all_status, $
                                   undo, pfo_obj=self, _EXTRA=extra)
  ;; Find the new order of our indices.  This is the most likely place
  ;; to get an error, since the user might have created a problem
  new_indices = pfo_parinfo_parse(/indices, status_mask=!pfo.all_status, $
                                  *self.pparinfo, pfo_obj=self, _EXTRA=extra)

  ;; If we made it here, all of our update "methods" ran OK.  Put our debug
  ;; level back to where it was.
  !pfo.debug = odebug

  ;; We are OK to save our undo, if asked to and if we were handed one
  if keyword_set(save_undo) and N_elements(undo) ne 0 then $
     self->save_undo, undo

  ;; If we don't have widgets, our work is done
  if N_elements(*self.pcw_obj_list) eq 0 or keyword_set(no_widget) then $
     return

  ;; If we have widgets that display _parinfo_ stuff, make sure we
  ;; have the pfo_unique tag, on the parinfo (already should be on the
  ;; undo) so that we can differentiate between the repopulate and
  ;; refresh cases.  Only run update for pfo_unique
  if N_elements(*self.pparinfo_repop_list) + $
     N_elements(*self.pparinfo_refresh_list) gt 0 then begin
     pfo_parinfo_update, *self.pparinfo, required_tags='pfo_unique', $
                         update_only_tags='pfo_unique', pfo_obj=self
  endif ;; have parinfo widgets     


  ;; Get the old and new uniqueID
  if N_elements(undo) gt 0 then $
     pfo_struct_setget_tag, /get, undo, taglist_series='pfo_unique', uniqueID=orig_uniqueID
  if N_elements(*self.pparinfo) gt 0 then $
     pfo_struct_setget_tag, /get, *self.pparinfo, taglist_series='pfo_unique', uniqueID=uniqueID

  ;; Decide whether to refresh or repopulate.  Be careful of the case
  ;; where we didn't get any sensible indices or uniqueID arrays back
  ;; (e.g. undo or parinfo are undefined, not properly set up, or just
  ;; don't parse to having any conent).  In this case, all we can do
  ;; is call repopulate
  if N_elements(orig_uniqueID) eq 0 or N_elements(uniqueID) eq 0 $
     or orig_indices[0] eq -1 or new_indices[0] eq -1 then begin
     ;; repopulate doesn't do anything if no widgets are displayed.
     ;; --> Hopefully repopulate methods can deal with the case that
     ;; all of their displayed parinfo[idx] go away
     self->repopulate
     return
  endif ;; parinfo or undo undefined

  ;; If we made it here, we have an undo (old parinfo) and a parinfo
  ;; which both parse properly.

  if array_equal(uniqueID[new_indices], orig_uniqueID[orig_indices]) then begin
     ;; Our parinfo parses in the same order as undo, so a refresh is
     ;; OK.  We don't know what parinfo[idx] or other pfo_obj property
     ;; may have been affected by the upstream changes in the parinfo,
     ;; so just refresh everything

     ;; Refresh non-parinfo widgets first
     self->refresh
     pfo_idx, *self.pparinfo, idx
     ;; Refresh parinfo widgets
     self->refresh, idx=idx

  endif else begin
     ;; We have had a major change in the parinfo.  Individual widgets
     ;; can't be refreshed.  All parinfo display has to be redone from
     ;; the top down.  Repopulate also calls refresh --> working on
     ;; trying to speed up repopulate
     self->repopulate
  endelse

end

;; Delete the parinfo
pro pfo_parinfo_obj::delete_parinfo
  ;; Save the existing parinfo, if undo is enabled
  self->save_undo
  ;; Delete the parinfo from memory
  ptr_free, self.pparinfo
  ;; Reallocate an undefined variable
  self.pparinfo = ptr_new(/allocate_heap)
  self->update
end

pro pfo_parinfo_obj::get_property, $
   parinfo_array=parinfo, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   enable_undo=enable_undo, $
   no_copy=no_copy ;; Dangerous!  This guts the pfo_parinfo_obj of its property, so calls to methods referring to that property won't work

  ;; parinfo
  if (arg_present(parinfo) or N_elements(parinfo) ne 0) and $
    N_elements(*self.pparinfo) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo = temporary(*self.pparinfo) $
     else $
       parinfo = *self.pparinfo
  endif ;; parinfo

  ;; parinfo_template
  if (arg_present(parinfo_template) or N_elements(parinfo_template)) ne 0 and $
    N_elements(*self.pparinfo_template) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo_template = temporary(*self.pparinfo_template) $
     else $
       parinfo_template = *self.pparinfo_template
  endif ;; parinfo_template

  ;; parinfo_descr
  if (arg_present(parinfo_descr) or N_elements(parinfo_descr) ne 0) and $
    N_elements(*self.pparinfo_descr) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo_descr = temporary(*self.pparinfo_descr) $
     else $
       parinfo_descr = *self.pparinfo_descr
  endif ;; parinfo_descr

  ;; pfo_fstruct_array
  if (arg_present(pfo_fstruct_array) or N_elements(pfo_fstruct_array)) ne 0 and $
    N_elements(*self.ppfo_fstruct_array) ne 0 then begin
     if keyword_set(no_copy) then $
       pfo_fstruct_array = temporary(*self.ppfo_fstruct_array) $
     else $
       pfo_fstruct_array = *self.ppfo_fstruct_array
  endif ;; pfo_fstruct_array

  ;; pfo_fstruct_descr
  if (arg_present(pfo_fstruct_descr) or N_elements(pfo_fstruct_descr)) ne 0 and $
    N_elements(*self.ppfo_fstruct_descr) ne 0 then begin
     if keyword_set(no_copy) then $
       pfo_fstruct_descr = temporary(*self.ppfo_fstruct_descr) $
     else $
       pfo_fstruct_descr = *self.ppfo_fstruct_descr
  endif ;; pfo_fstruct_descr

  enable_undo = obj_valid(self.parinfo_undo_obj)

end ;; get_property

pro pfo_parinfo_obj::set_property, $
   parinfo_array=parinfo, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   enable_undo=enable_undo, $ ;; use obj_valid(self.parinfo_undo_obj) to test state of undo
   no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
   no_update=no_update ;; (DANGEROUS!) caller is taking care of updating parinfo, presumably after several operations

  ;; pfo_fstruct_array.  Do this first for the benefit of the parinfo
  ;; and pfo_finfo system
  if N_elements(pfo_fstruct_array) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.ppfo_fstruct_array = temporary(pfo_fstruct_array) $
     else $
       *self.ppfo_fstruct_array = pfo_fstruct_array
  endif ;; pfo_fstruct_array

  ;; parinfo
  if N_elements(parinfo) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pparinfo = temporary(parinfo) $
     else $
       *self.pparinfo = parinfo

     ;; The parinfo has changed, so we need to do an update repopulate on any
     ;; widgets, unless the calling routine is taking care of this
     if NOT keyword_set(no_update) then $
        self->update

  endif ;; parinfo

  ;; parinfo_template
  if N_elements(parinfo_template) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pparinfo_template = temporary(parinfo_template) $
     else $
       *self.pparinfo_template = parinfo_template
  endif ;; parinfo_template

  ;; parinfo_descr
  if N_elements(parinfo_descr) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pparinfo_descr = temporary(parinfo_descr) $
     else $
       *self.pparinfo_descr = parinfo_descr
  endif ;; parinfo_descr

  ;; pfo_fstruct_descr
  if N_elements(pfo_fstruct_descr) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.ppfo_fstruct_descr = temporary(pfo_fstruct_descr) $
     else $
       *self.ppfo_fstruct_descr = pfo_fstruct_descr
  endif ;; pfo_fstruct_descr

  ;; undo
  if N_elements(enable_undo) gt 0 then begin
     if enable_undo eq 0 then begin
        ;; Turn off undo capability and delete undo and redo lists
        obj_destroy, self.parinfo_undo_obj
        obj_destroy, self.parinfo_redo_obj
     endif else begin
        ;; We make it here if enable_undo is set.  Create the undo
        ;; list if it doesn't exist.
        if NOT obj_valid(self.parinfo_undo_obj) then begin
           self.parinfo_undo_obj = obj_new('linkedlist')
           self.parinfo_redo_obj = obj_new('linkedlist')
        endif ;; need to create lists
     endelse
  endif ;; undo


end ;; set_property

function pfo_parinfo_obj::descr
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

  descr = *self.ppfo_parinfo_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_parinfo_obj::cleanup
  ptr_free, self.ppfo_parinfo_obj_descr
  ptr_free, self.pparinfo	
  ptr_free, self.pparinfo_template
  ptr_free, self.pparinfo_descr
  ptr_free, self.ppfo_fstruct_array
  ptr_free, self.ppfo_fstruct_descr
  obj_destroy, self.parinfo_undo_obj
  obj_destroy, self.parinfo_redo_obj

  self->pfo_parinfo_obj_cw_obj::cleanup

end

function pfo_parinfo_obj::init, $
   parinfo_array=parinfo_array, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   enable_undo=enable_undo, $ ;; enable_undo = !tok.yes by default
   no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

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
  self.ppfo_parinfo_obj_descr $
     = ptr_new( $
     {README	: 'pfo_parinfo_obj stores the parinfo and associated information used by the PFO system', $
      SUPERCLASSES: 'pfo_parinfo_obj_cw_obj', $
      METHODS	: 'print, widget, indices, parinfo, edit, parinfo_edit, parinfo_call_procedure, parinfo_call_function, redo, undo, save_undo, prepare_update, update, delete_parinfo'} $
              )
  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_parinfo_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_parinfo_obj_descr, $
                        {PROPERTY: property[good_idx]}

  ;; Turn our null reference pointers into undefined variables
  self.pparinfo = ptr_new(/allocate_heap)
  self.pparinfo_template = ptr_new(/allocate_heap)
  self.pparinfo_descr = ptr_new(/allocate_heap)
  self.ppfo_fstruct_array = ptr_new(/allocate_heap)
  self.ppfo_fstruct_descr = ptr_new(/allocate_heap)

  ;; Initilize default values (nothing to do for now)

  ;; Call our set_property routine to convert any keywords to property
  self->set_property, $
     parinfo_array=parinfo_array, $
     parinfo_template=parinfo_template, $
     parinfo_descr=parinfo_descr, $
     pfo_fstruct_array=pfo_fstruct_array, $
     pfo_fstruct_descr=pfo_fstruct_descr, $
     enable_undo=enable_undo, $
     no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

  ;; Call our superclass init methods
  ok = self->pfo_parinfo_obj_cw_obj::init(_EXTRA=extra)
  if NOT ok then return, 0

  ;; If we made it here, our object should be initialized properly
  return, 1

end

pro pfo_parinfo_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
    {pfo_parinfo_obj, $
     ppfo_parinfo_obj_descr:ptr_new(), $ ;; Pointer to description structure
     pparinfo	:	ptr_new(), $ ;; Pointer to parinfo array (function definition)
     pparinfo_template: ptr_new(), $ ;; Pointer to template for creating new parinfo records
     pparinfo_descr:	ptr_new(), $ ;; Pointer to structure that documents the parinfo structure
     ppfo_fstruct_array:ptr_new(), $ ;; Pointer to fstruct_array, which associates function names in parinfo to fnums (integer part of parinfo.pfo.ftype).  Also has number of parameters in function and function definition
     ppfo_fstruct_descr:ptr_new(), $ ;; Pointer to documentation of fstruct_array
     parinfo_undo_obj:	obj_new(), $ ;; undo linked list for changes in parinfo
     parinfo_redo_obj:	obj_new(), $ ;; linked list for redo
     inherits pfo_parinfo_obj_cw_obj}    ;; Object that manages property related to parinfo editing

end
