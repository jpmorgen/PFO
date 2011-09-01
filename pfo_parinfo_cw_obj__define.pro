;+
; NAME: pfo_parinfo_cw_obj__define

; PURPOSE: Define common property and methods which are used by
; comound widgets (cw) that handle the parinfo array encapsulated in
; a pfo_obj

; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:

; DESCRIPTION:  This object interfaces with the object
; pfo_parinfo_obj_cw_obj, which is intended to be inherited into pfo_obj

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
; RESTRICTIONS:  Uses coyote library linked_list__define
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_parinfo_cw_obj__define.pro,v 1.1 2011/09/01 22:14:29 jpmorgen Exp $
;
; $Log: pfo_parinfo_cw_obj__define.pro,v $
; Revision 1.1  2011/09/01 22:14:29  jpmorgen
; Initial revision
;
;-

;; Default repopulate method.  Catch cases where people have forgotten to
;; define their own
pro pfo_parinfo_cw_obj::repopulate
  help, self, output=s
  message, /CONTINUE, 'WARNING: repopulate method for ' + s + ' not specified'
end

;; Repopfresh_check is called _TWICE_ in each event method.  It makes
;; sure that the sets up an undo the first time
pro pfo_parinfo_cw_obj::repopfresh_check, $
   undo

  if N_elements(undo) eq 0 then begin
     ;; First time through
     ;; Make sure we have our pfo_unique tag.  Don't do this
     ;; with pfo_parinfo_update, since that might be unnecessarily
     ;; expensive.  Also, if the calling routines haven't
     ;; properly kept up with updates, it might change the order of
     ;; thing in the parinfo.  We don't need to deal with that here....
     self.pfo_obj->parinfo_call_procedure, $
        'pfo_struct_append', 'pfo_unique'
     self.pfo_obj->parinfo_call_procedure, $
        'pfo_unique_struct__update'
     ;;  _copy_ off our current parinfo into undo
     undo = self.pfo_obj->parinfo()
     return
  endif ;; First time through

  ;; If we made it here, this is our second time through

  ;; Find the parsed order of our old parinfo.  I do not expect an
  ;; error here, but check anyway.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: This should not happen!'
     endif
  endif ;; not debugging
  orig_indices = pfo_parinfo_parse(/indices, undo, pfo_obj=self.pfo_obj)

  ;; Check to see if there is an error parsing our new parinfo.  There
  ;; are many ways this can happen, so be thorough
  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL
     ;; Report on the console.  Note that adding to the message beefs
     ;; up !error_state.msg
     message, /NONAME, !error_state.msg + '.  Restoring to previous state.' , /CONTINUE
     ;; Report the error via a widget
     ok = dialog_message(!error_state.msg)
     ;; Put our debug flag back
     !pfo.debug = odebug
     ;; Reset the parinfo
     self.pfo_obj->set_property, /no_copy, parinfo_array=undo, /no_repopulate
     ;; Redisplay our widget to clear whatever we just changed
     self.pfo_obj->refresh, idx=(*self.pidx[0])
     return
  endif ;; error parsing our new function

  ;; Turn debugging on
  odebug = !pfo.debug
  !pfo.debug = 1
  
  ;; Find the new order of our indices.  This is where I expect to get
  ;; an error
  new_indices = self.pfo_obj->indices()

  ;; If we made it here, pfo_parinfo_parse worked OK.  Put our debug
  ;; level back to where it was.
  !pfo.debug = odebug  

  ;; Get the original uniqueID 
  pfo_struct_setget_tag, /get, undo, taglist_series='pfo_unique', uniqueID=orig_uniqueID
  
  ;; Get the new uniqueID 
  self.pfo_obj->parinfo_call_procedure, $
     'pfo_struct_setget_tag', /get, taglist_series='pfo_unique', uniqueID=uniqueID
    
  ;; Decide whether to refresh or repopulate
  if array_equal(uniqueID[new_indices], orig_uniqueID[orig_indices]) then begin
     ;; Our parinfo doesn't parse in a different order, so a
     ;; refresh is OK.  Remember to provide our encapsulated idx so
     ;; only the widgets displaying this idx are refreshed
     self.pfo_obj->refresh, idx=(*self.pidx[0])
  endif else begin
     ;; We have had a major change in the parinfo.  Individual
     ;; widgets can't be refreshed.  All parinfo display has to
     ;; be redone from the top down.
     self.pfo_obj->repopulate
  endelse

end

;; Local method for registration on repop list.  Repopulation is only
;; possible if we have an independent way of knowing how to generate
;; the keywords listed here.  If any of the following arguments have
;; been passed to the calling routine, presumably the calling routine
;; doesn't know how to generate them, so it can't do a repopulate.  It
;; is essential that all of the invoking arguments of the inheriting
;; widget be passed to this routine so that it can check for the
;; validity of repopulation.  In other words, call with
;; self->register_repop, <captured parameters on this list>, _EXTRA=extra
pro pfo_parinfo_cw_obj::register_repop, $
   parinfo=parinfo, $ ;; Keywords invalidating ability to repopulate
   params=params, $     
   idx=idx, $
   ispec=ispec, $
   iROI=iROI, $
   status_mask=status_mask

  if N_elements(parinfo) ne 0 then $
     message, 'ERROR: I cannot deal with specifying parinfo on the fly at this point.  You must use the one encapsulated in the pfo_obj.  Consider swapping your parinfo for the one in the object, a la pfo_parinfo_obj::widget'

  ;; Get the length of our parinfo in the pfo_obj
  N_parinfo = self.pfo_obj->parinfo_call_function('N_elements')

  ;; Check keywords that would not allow us to repopulate.  Code
  ;; similar to that in pfo_calc_obj::parinfo_cache_ok
  if NOT (N_elements(parinfo) + $
          N_elements(params) + $
          N_elements(ispec) + $
          N_elements(iROI) + $
          N_elements(status_mask) $
          eq 0 $ ;; Be a little more sophisticated with idx since we might have
          and (N_elements(idx) eq 0 or (N_elements(idx) eq N_parinfo)) $
          and (N_elements(*self.pidx) eq 0 or (N_elements(*self.pidx) eq N_parinfo))) then $
             return

  ;; If we made it here, we are safe for repopulation.
  self.pfo_obj->register_repop, self

end


;; Local method for registration on refresh list.  Use a local routine
;; so that we always remember to pass our encapsulated idx properly.
;; Routines that inherit this object can look a lot like routines that
;; inherit just pfo_cw_obj
pro pfo_parinfo_cw_obj::register_refresh, $
   value=value ;; value flag (see pfo_parinfo_obj_cw_obj__define.pro)

  if N_elements(*self.pidx) eq 0 then $
     message, 'ERROR: idx not defined in inheriting object.  Do you mean for me to be a pfo_cw_obj instead of a pfo_parinfo_cw_obj?'

  ;; For groups of idx, only register the first one
  self.pfo_obj->register_refresh, self, idx=(*self.pidx)[0], value=value

end

;; Cleanup method
pro pfo_parinfo_cw_obj::cleanup
  ;; Take ourselves off of the repopulate list.  This does not cause
  ;; problems if we weren't registereed in the first place.
  self.pfo_obj->unregister_repop, self
  ;; Free our heap variables
  ptr_free, self.pidx
  ;; Call our inherited cleaup routines.  This takes care of taking us
  ;; off the appropriate refresh list
  self->pfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_cw_obj::init, $
   parentID, $ 		;; Parent widget ID.  If not specified, or !tok.nowhere, a top-level base is created
   parinfo=parinfo, $   ;; parinfo cannot be specified at this level
   idx=idx, $		;; index/indices into parinfo relevant to the widget begin built
   _REF_EXTRA=extra 	;; all other args, such as pfo_obj, are passed on to pfo_cw_obj::init


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

  if N_elements(parinfo) ne 0 then $
     message, 'ERROR: I cannot deal with specifying parinfo on the fly at this point.  You must use the one encapsulated in the pfo_obj.  Consider swapping your parinfo for the one in the object, a la pfo_parinfo_obj::widget'

  ok = self->pfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Initilize and store our property
  self.pidx = ptr_new(/allocate_heap)
  if N_elements(idx) gt 0 then $
     *self.pidx = idx

  return, 1

end

;; Object class definition
pro pfo_parinfo_cw_obj__define
  objectClass = $
     {pfo_parinfo_cw_obj, $
      pidx	: ptr_new(), $ ;; indices into parinfo array of parinfo segment handled by this widget
      inherits pfo_cw_obj $
     }
end
