;+
; NAME: pfo_parinfo_new_droplist
;
; PURPOSE: Create a droplist widget for creating a new parinfo segment
; (new PFO sub-function)
;
; CATEGORY:
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
; $Id: pfo_parinfo_new_droplist.pro,v 1.1 2011/09/23 13:04:45 jpmorgen Exp $
;
; $Log: pfo_parinfo_new_droplist.pro,v $
; Revision 1.1  2011/09/23 13:04:45  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_new_droplist_obj::event, event

  ;; Prepare to swallow this event
  retval = !tok.nowhere

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DROPLIST' then $
     message, 'ERROR: unexpected event type ' + sn

  ;; Nothing to do if the default value is selected
  if event.index eq 0 then $
     return, retval
  
  ;; If we made it here, we are really ready to create a new
  ;; function.  

  ;; Put our droplist back the way we like it
  self->refresh

  ;; Create our new function
  parinfo = pfo_parinfo_new((*self.pfname_list)[event.index-1], pfo_obj=self.pfo_obj)

  ;; We don't have any way to initialize our new parinfo segment with
  ;; command line parameters so put it in a new pfo_obj of the same
  ;; object class as our pfo_obj and raise the widget editor from
  ;; inside the new object
  help, self.pfo_obj, output=s
  ;; s = 'PFO_OBJ         OBJREF    = <ObjHeapVar1(PFO_OBJ)>'
  ;; Chop off all but the end
  s = strmid(s, strpos(s, '(') + 1)
  ;; Chop off the end
  s = strmid(s, 0, strpos(s, ')>'))
  tpfo_obj = obj_new(s)
  ;; _Move_ our parinfo into the tpfo_obj
  tpfo_obj->set_property, parinfo_array=parinfo, /no_copy
  ;; Bring up a parinfo editor with our temporary pfo_obj.  Try to
  ;; make it a blocking widget, but this won't work unless all
  ;; the other widgets are non-blocking.  The pfo_finit menu takes
  ;; care of returning the initialized parinfo from tpfo_obj to
  ;; our pfo_obj even if we are non-blocking.  The group leader should
  ;; be the true top of the hierarchy of our original widget, just
  ;; just the tlb of self (which ends up being a containerID)
  tpfo_obj->parinfo_edit, /edit, group_leader=pfo_widget_top(self.tlbID), no_block=0, $
                          /no_finit, menus=['pfo_parinfo_new_exit', 'pfo_parinfo_edit'], $
                          menu_args={orig_pfo_obj:self.pfo_obj}

  ;; Swallow event
  return, retval

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_new_droplist_obj::refresh
  ;; Put our droplist back the way we like it
  widget_control, self.fname_listID, $
                  set_value=['Add a new function', *self.pfname_list]

end

pro pfo_parinfo_new_droplist_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Handle the fname_list.  Build a list if the user hasn't
  ;; provided a (presumably more limited) list
  if N_elements(*self.pfname_list) eq 0 then begin
     ;; This works on files that are already in the IDL !path system.
     ;; If you add a new function and want it to work, you can try
     ;; path_cache, /clear.  If you add a new directory, you might
     ;; need to explicitly add that directory to !path.  It is just
     ;; easier to exit and restart or do a .reset_session.
     
     ;; Found this in the IDL help for file_search.  The strsplit sets
     ;; up an array of the path elements, which are added one by one
     ;; to the regex.  Filesearch finds all files matching the regex
     ;; and returns the fully qualified path
     full = file_search(strsplit(!path, path_sep(/search_path), /extract) + '/*__fdefine' + '.pro')
     ;; Check the oops of not having any PFO functions
     if N_elements(full) eq 0 then begin
        message, /CONTINUE, 'WARNING: no PFO-enabled functions found (name *__fdefine.pro) in the IDL !path.'
        return
     endif ;; no __fdefine.pro files
     ;; Trim off path and .pro
     full = file_basename(full, '.pro')
     ;; Loop through each element of full to extract the fname we are
     ;; accustomed to working with in the PFO system
     for ifn=0, N_elements(full)-1 do begin
        pfo_array_append, *self.pfname_list, strmid(full[ifn], 0, strpos(full[ifn], '__fdefine'))
     endfor ;; each __fname full filename
     
  endif ;; generating our own list of functions

  self.fname_listID = widget_droplist(self.tlbID, value=['Add a new function', *self.pfname_list], $
                                      uvalue={method: 'event', obj:self})

end

;; Cleanup method
pro pfo_parinfo_new_droplist_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ptr_free, self.pfname_list
end

;; Init method
function pfo_parinfo_new_droplist_obj::init, $
   parentID, $ ;; widgetID of parent widget
   fname_list=fname_list, $ ; (optional) fnames to list in sub-function droplist
   _REF_EXTRA=extra ;; All other input parameters are passed to underlying routines via _REF_EXTRA

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

  ;; Set our pointer(s) to null variables
  self.pfname_list = ptr_new(/allocate_heap)
  if N_elements(fname_list) ne 0 then $
     *self.pfname_list = fname_list

  ;; Call our inherited init routine.
  ok = self->pfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register ourselves in the refresh list.
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_parinfo_new_droplist_obj__define
  objectClass = $
     {pfo_parinfo_new_droplist_obj, $
      pfname_list	: ptr_new(), $ ;; list of user-supplied fnames
      fname_listID	: 0L, $ ;; 
      inherits pfo_cw_obj}
end

function pfo_parinfo_new_droplist, $
   parentID, $ ;; Parent widget ID (positional parameter)
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra ;; ;; All other input parameters passed to the init method and underlying routines via _REF_EXTRA mechanism

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere

  ;; Create our controlling object
  cw_obj = pfo_cw_obj_new(parentID, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
  endif ;; valid cw_obj

  return, cwID

end
