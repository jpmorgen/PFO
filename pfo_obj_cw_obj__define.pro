;+
; NAME: pfo_obj_cw_obj

; PURPOSE: Defines a object which helps link pfo_objs (objects which
; encapsulate information about data and functions in the PFO system)
; to cw_objs (objects which control PFO compound widgets) 

; CATEGORY: PFO_OBJ
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
; $Id: pfo_obj_cw_obj__define.pro,v 1.1 2011/09/01 22:17:37 jpmorgen Exp $
;
; $Log: pfo_obj_cw_obj__define.pro,v $
; Revision 1.1  2011/09/01 22:17:37  jpmorgen
; Initial revision
;
;-

;; Refresh method.  Refreshes all widgets in the refresh list
pro pfo_obj_cw_obj::refresh
  for iw=0,N_elements(*self.pcw_obj_refresh_list)-1 do begin
     if NOT obj_valid((*self.pcw_obj_refresh_list)[iw]) then begin
        help, (*self.pcw_obj_refresh_list)[iw], out=s
        message, /CONTINUE, 'WARNING: object ' + s + ' not properly unregistered from refresh list.  Skipping.  FIX YOUR CODE!'
           CONTINUE
        endif

     (*self.pcw_obj_refresh_list)[iw]->refresh
  endfor
end

;; Unregister a cw_obj from our list of connected cw_objs
pro pfo_obj_cw_obj::unregister_refresh, $
   cw_obj ;; cw_obj of widget to register

  ;; Check syntax of invocation
  if NOT obj_valid(cw_obj) then $
     message, 'ERROR: specify a valid object: cw_obj, presumably one that controls a widget that displays a [set of] parinfo.  "self.pfo_obj->unregister_refresh, self" is the usual call'

  ;; Quietly return if there are no widgets in our list, since people
  ;; are unlikely to check to see if they were registered in the first place
  if N_elements(*self.pcw_obj_refresh_list) eq 0 then $
     return

  ;; Find all the _other_ cw_obj in this list
  good_idx = where(*self.pcw_obj_refresh_list ne cw_obj, count)
  if count eq 0 then begin
     ;; If these is nothing [left] in our repop_list.  Make
     ;; *self.pcw_obj_refresh_list an undefined variable
     ptr_free, self.pcw_obj_refresh_list
     self.pcw_obj_refresh_list = ptr_new(/allocate_heap)
     return
  endif ;; no more repops

  ;; If we made it here, we need to remove our cw_obj.  Just make the
  ;; repop_list everything else.  Note: if our cw_obj wasn't on
  ;; the list to begin with, this just quietly does nothing.
  *self.pcw_obj_refresh_list = (*self.pcw_obj_refresh_list)[good_idx]

end

;; Register a cw_obj from our refresh list
pro pfo_obj_cw_obj::register_refresh, $
   cw_obj

  if NOT obj_valid(cw_obj) then begin
     message, 'WARNING: invalid cw_obj.', /CONTINUE
     return
  endif ;; cw_obj
  pfo_array_append, *self.pcw_obj_refresh_list, cw_obj

end

;; Unregister a cw_obj from our list of connected cw_objs
pro pfo_obj_cw_obj::unregister_cw_obj, $
   cw_obj ;; cw_obj of widget to register

  ;; Check syntax of invocation
  if NOT obj_valid(cw_obj) then $
     message, 'ERROR: specify a valid object: cw_obj.  "self.pfo_obj->unregister_cw_obj, self" is the usual call'

  ;; Quietly return if there are no widgets in our list, since people
  ;; are unlikely to check to see if they were registered in the first place
  if N_elements(*self.pcw_obj_list) eq 0 then $
     return

  ;; Find all the _other_ cw_obj in this list
  good_idx = where(*self.pcw_obj_list ne cw_obj, count)
  if count eq 0 then begin
     ;; If these is nothing [left] in our repop_list.  Make
     ;; *self.pcw_obj_list an undefined variable
     ptr_free, self.pcw_obj_list
     self.pcw_obj_list = ptr_new(/allocate_heap)
     return
  endif ;; no more repops

  ;; If we made it here, we need to remove our cw_obj.  Just make the
  ;; repop_list everything else.  Note: if our cw_obj wasn't on
  ;; the list to begin with, this just quietly does nothing.
  *self.pcw_obj_list = (*self.pcw_obj_list)[good_idx]

end

;; Register a cw_obj in this pfo_obj so that it can be killed in an
;; orderly manner when pfo_obj is killed
pro pfo_obj_cw_obj::register_cw_obj, cw_obj
  if NOT obj_valid(cw_obj) then begin
     message, 'WARNING: invalid cw_obj.', /CONTINUE
     return
  endif ;; cw_obj
  pfo_array_append, *self.pcw_obj_list, cw_obj
end

function pfo_obj_cw_obj::descr
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

  descr = *self.ppfo_obj_cw_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_obj_cw_obj::cleanup
  ;; Kill any cw_objs that are still displayed.  Copy our list to a
  ;; local variable, since it is going to get shredded when the
  ;; cw_objs unregister as they are being killed.  In fact, for a
  ;; well-organized widget structure, just killing the tlb, which
  ;; should be the first registered widget, takes out the whole list.
  ;; obj_destroy politely doesn't raise an error when an already dead
  ;; widget is killed again.
  if N_elements(*self.pcw_obj_list) ne 0 then $
     cw_obj_list = *self.pcw_obj_list
  for ic=0, N_elements(cw_obj_list)-1 do begin
     obj_destroy, cw_obj_list[ic]
  endfor
  ;; Free heap variables.
  ptr_free, self.ppfo_obj_cw_obj_descr
  ptr_free, self.pcw_obj_list
  ptr_free, self.pcw_obj_refresh_list
end

function pfo_obj_cw_obj::init, $
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

  self.ppfo_obj_cw_obj_descr $
     = ptr_new( $
     {README	: 'pfo_obj_cw_obj stores the list of widgets that are displaying pfo_obj property.  This helps to make sure that all widgets die when the pfo_obj dies.  This also holds the list of cw_objs that want to get refreshed when the self->refresh method is issued', $
      METHODS	: 'refresh, register_refresh, unregister_refresh, register_cw_obj, unregister_cw_obj'} $
              )

  ;; Turn our null reference pointers into undefined variables
  self.pcw_obj_list = ptr_new(/allocate_heap)
  self.pcw_obj_refresh_list = ptr_new(/allocate_heap)

  return, 1
end

pro pfo_obj_cw_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_obj_cw_obj, $
      ppfo_obj_cw_obj_descr	: ptr_new(), $  ;; Pointer to description structure
      pcw_obj_list		: ptr_new(), $ ;; list of cw_objs that are displaying property from this pfo_obj
      pcw_obj_refresh_list	: ptr_new() $ ;; list of cw_objs that are searched for matches when self->refresh is called
     }
      
end
