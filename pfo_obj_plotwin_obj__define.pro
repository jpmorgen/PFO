;+
; NAME: pfo_obj_plotwin_obj__define
;
; PURPOSE: Defines an object which helps link pfo_plotwins (objects
; which display PFO information in IDL draw widget) into the pfo_obj
;
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
; $Id: pfo_obj_plotwin_obj__define.pro,v 1.1 2011/09/01 22:13:45 jpmorgen Exp $
;
; $Log: pfo_obj_plotwin_obj__define.pro,v $
; Revision 1.1  2011/09/01 22:13:45  jpmorgen
; Initial revision
;
;-

;; Replot method.  Calls the plot method of all registered plotwins
pro pfo_obj_plotwin_obj::replot, _REF_EXTRA=extra
  for iw=0,N_elements(*self.pplotwin_obj_list)-1 do begin
     if NOT obj_valid((*self.pplotwin_obj_list)[iw]) then begin
        help, (*self.pplotwin_obj_list)[iw], out=s
        message, /CONTINUE, 'WARNING: object ' + s + ' not properly unregistered from refresh list.  Skipping.  FIX YOUR CODE!'
           CONTINUE
        endif

     ;; Make sure that the plot method of the plotwin_obj knows which
     ;; pfo_obj it should be plotting.  If it does not provide its own
     ;; pfo_plot_obj, it should default to the pfo_plot_obj in the pfo_obj
     (*self.pplotwin_obj_list)[iw]->plot, pfo_obj=self, _EXTRA=extra
  endfor
end

;; Unregister a plotwin_obj from our list of connected plotwin_objs
pro pfo_obj_plotwin_obj::unregister_plotwin_obj, $
   plotwin_obj ;; plotwin_obj of widget to register

  ;; Check syntax of invocation
  if NOT obj_valid(plotwin_obj) then $
     message, 'ERROR: specify a valid object: plotwin_obj.  "self.pfo_obj->unregister_plotwin_obj, self" is the usual call'

  ;; Quietly return if there are no widgets in our list, since people
  ;; are unlikely to check to see if they were registered in the first place
  if N_elements(*self.pplotwin_obj_list) eq 0 then $
     return

  ;; Find all the _other_ plotwin_obj in this list
  good_idx = where(*self.pplotwin_obj_list ne plotwin_obj, count)
  if count eq 0 then begin
     ;; If these is nothing [left] in our repop_list.  Make
     ;; *self.pplotwin_obj_list an undefined variable
     ptr_free, self.pplotwin_obj_list
     self.pplotwin_obj_list = ptr_new(/allocate_heap)
     return
  endif ;; no more repops

  ;; If we made it here, we need to remove our plotwin_obj.  Just make the
  ;; repop_list everything else.  Note: if our plotwin_obj wasn't on
  ;; the list to begin with, this just quietly does nothing.
  *self.pplotwin_obj_list = (*self.pplotwin_obj_list)[good_idx]

end


;; Register a plotwin_obj in this pfo_obj so that it can be killed in an
;; orderly manner when pfo_obj is killed
pro pfo_obj_plotwin_obj::register_plotwin_obj, plotwin_obj
  if NOT obj_valid(plotwin_obj) then begin
     message, 'WARNING: invalid plotwin_obj.', /CONTINUE
     return
  endif ;; plotwin_obj
  pfo_array_append, *self.pplotwin_obj_list, plotwin_obj
end

function pfo_obj_plotwin_obj::descr
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

  descr = *self.ppfo_obj_plotwin_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_obj_plotwin_obj::cleanup
  ;; Kill any plotwin_objs that are still displayed.  Copy our list to
  ;; a local variable, since it is going to get shredded when the
  ;; plotwin_objs unregister as they are being killed.  In fact, for a
  ;; well-organized widget structure, just killing the tlb, which
  ;; should be the first registered widget, takes out the whole list.
  ;; obj_destroy politely doesn't raise an error when an already dead
  ;; widget is killed again.
  if N_elements(*self.pplotwin_obj_list) ne 0 then $
     plotwin_obj_list = *self.pplotwin_obj_list
  for ic=0, N_elements(plotwin_obj_list)-1 do begin
     obj_destroy, plotwin_obj_list[ic]
  endfor
  ;; Free heap variables.
  ptr_free, self.ppfo_obj_plotwin_obj_descr
  ptr_free, self.pplotwin_obj_list
end

function pfo_obj_plotwin_obj::init, $
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

  self.ppfo_obj_plotwin_obj_descr $
     = ptr_new( $
     {README	: 'pfo_obj_plotwin_obj stores the list of widgets that are plotting pfo_obj property.  This helps to make sure that all widgets die when the pfo_obj dies.  This list of plotwin_objs is also used to replot when the self->replot method is issued', $
      METHODS	: 'replot, register_plotwin_obj, unregister_plotwin_obj'} $
              )

  ;; Turn our null reference pointers into undefined variables
  self.pplotwin_obj_list = ptr_new(/allocate_heap)

  return, 1
end

pro pfo_obj_plotwin_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_obj_plotwin_obj, $
      ppfo_obj_plotwin_obj_descr: ptr_new(), $ ;; Pointer to description structure
      pplotwin_obj_list		: ptr_new() $ ;; list of plotwin_objs whose plot methods are called when self->replot is called
     }
      
end
