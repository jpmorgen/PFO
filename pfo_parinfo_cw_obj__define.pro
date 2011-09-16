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
; $Id: pfo_parinfo_cw_obj__define.pro,v 1.4 2011/09/16 11:16:02 jpmorgen Exp $
;
; $Log: pfo_parinfo_cw_obj__define.pro,v $
; Revision 1.4  2011/09/16 11:16:02  jpmorgen
; Played with status_mask stuff.  Still not sure if I like it
;
; Revision 1.3  2011/09/15 20:52:57  jpmorgen
; Broke off repopulate_ok from register_repop so pfo_parinfo_edit could
; use it
;
; Revision 1.2  2011/09/08 19:57:47  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
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

;; Repopulation.  This helps the local method for registration on the
;; repop list.  Repopulation is only possible if we have an
;; independent way of knowing how to generate the keywords listed
;; here.  If any of the following arguments have been passed to the
;; calling routine, presumably the calling routine doesn't know how to
;; generate them, so it can't do a repopulate.  It is essential that
;; all of the invoking arguments of the inheriting widget be passed to
;; this routine so that it can check for the validity of repopulation.
;; In other words, call with self->register_repop, <captured
;; parameters on this list>, _EXTRA=extra
function pfo_parinfo_cw_obj::repopulate_ok, $
   parinfo=parinfo, $ ;; Keywords invalidating ability to repopulate
   params=params, $     
   idx=idx, $
   ispec=ispec, $
   iROI=iROI, $
   status_mask=status_mask

  if N_elements(parinfo) ne 0 then $
     message, 'ERROR: I cannot deal with specifying parinfo on the fly at this point.  You must use the one encapsulated in the pfo_obj.  Consider swapping your parinfo for the one in the object, a la pfo_parinfo_obj::widget'

  ;; Get the length of our parinfo in the pfo_obj
  N_parinfo = self.pfo_obj->parinfo_call_function(/no_update, 'N_elements')
  
  ;; --> I am starting to have second thoughts about the default value
  ;; of status_mask being !pfo.active.  This code says that
  ;; status_mask is OK if you don't specify it, but if you do,
  ;; it has to be !pfo.all_status
  status_mask_ok = 1
  if N_elements(status_mask) ne 0 then $
     status_mask_ok = status_mask eq !pfo.all_status

  ;; Check keywords that would not allow us to repopulate.  Code
  ;; similar to that in pfo_calc_obj::parinfo_cache_ok
  return, (N_elements(parinfo) + $
           N_elements(params) + $
           N_elements(ispec) + $
           N_elements(iROI))  $
           eq 0 $
           and (status_mask_ok eq 1) $ 
           $ ;; Be a little more sophisticated with idx since we might have run pfo_idx
           and (N_elements(idx) eq 0 or (N_elements(idx) eq N_parinfo)) $
           and (N_elements(*self.pidx) eq 0 or (N_elements(*self.pidx) eq N_parinfo))

end

pro pfo_parinfo_cw_obj::register_repop, $
   _REF_EXTRA=extra

  if self->repopulate_ok(_EXTRA=extra) then $
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
   pfo_obj=pfo_obj, $	;; Encapsulates parinfo that will be displayed (optional: one will be created with pfo_obj_new if not specified)
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
  
  ;; Create pfo_obj on the fly if none provided.  This give the user a
  ;; fully functioning pfo_obj if they start with the parinfo widget
  if N_elements(pfo_obj) eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: creating pfo_obj.  If this is not what you expect, make sure you pass me a pfo_obj.  Use pfo_quiet to suppress message.'
     ;; Create a fully featured pfo_obj.  The user might want a
     ;; different one, which they can specify with
     ;; !pfo.pfo_obj_ClassName or by creating the object themselved
     pfo_obj = pfo_obj_new()
     self.created_pfo_obj = 1
  endif ;; no pfo_obj specified

  ok = self->pfo_cw_obj::init(parentID, pfo_obj=pfo_obj, _EXTRA=extra)
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
