;+
; NAME: pfo_parinfo_obj_cw_obj__define
;
; PURPOSE: Defines the repopulate/refresh sub-object for the pfo_obj system
;
; CATEGORY: PFO_OBJ
;
; CALLING SEQUENCE:

; DESCRIPTION:

;   This object is intended to be a superclass of pfo_parinfo_obj

;   This object serves to link widgets that display data from the pfo
;   parinfo together.  Widgets are grouped into two classes.  Those
;   that display information from only one parameter (array element)
;   of the parinfo and those that display information that depends on
;   an interpretation of a segment of parinfo (e.g. pfo_parinfo_cw can
;   display the entire parinfo).

;   REFRESH: Some operations, such as changing a limit from fixed to
;   free, simply act on one parameter and do not change the
;   interpretation of any other parinfo elements.  Widgets that make
;   these changes should register with the refresh list.  Then, when
;   the change is made, pfo_obj->refresh, idx=idx is called, where idx
;   is the idx of the parameter being changed.  The refresh method of
;   this object goes through the list of registered cw_objs that
;   display information from parinfo[idx] and issues the refresh
;   method of each cw_obj in turn.  You can also specify a value,
;   which means that only cw_objs that have been registered as
;   displaying values will be refreshed.  In that case, the value you
;   pass will be used.  If your widget is not acting on any parameter,
;   don't specify an idx or value and the more general routine from
;   pfo_cw_pfo_obj that the refresh method overrides will be used.

;   REPOPULATE: Other operations, such as adding a new function to the
;   parinfo, can result in the parinfo parsing in a different order.
;   In this case, it may not be possible to reuse the widgets
;   currently displayed.  To make this operation reliable, all of the
;   displayed parinfo widgets must be killed and recreated with the
;   new parinfo.  Only top-level widgets which themselves have the
;   code necessary to identity the section of the parinfo they need to
;   display (e.g. a widget that would display only ROI functions) can
;   survive this rehashing.  All other widgets (generally those in the
;   refresh list) will loose the association between their idx and the
;   true parameter they are trying to represent.

;   So when a major change is made to the parinfo, the
;   pfo_obj->repopulate method is called.  This issues the
;   cw_obj->repopulate of all of the cw_objs that have been registered.

;   Additional maintenance, such as registration and unregistration of
;   cw_obj in these lists is accomplished by the methods
;   [un]register_[repop | refresh]

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
; $Id: pfo_parinfo_obj_cw_obj__define.pro,v 1.1 2011/09/01 22:13:33 jpmorgen Exp $
;
; $Log: pfo_parinfo_obj_cw_obj__define.pro,v $
; Revision 1.1  2011/09/01 22:13:33  jpmorgen
; Initial revision
;
;-

;; Repopulate method
pro pfo_parinfo_obj_cw_obj::repopulate
  for iw=0,N_elements(*self.pparinfo_repop_list)-1 do begin
     (*self.pparinfo_repop_list)[iw]->repopulate
  endfor
  ;; Repopulation automatically refreshes the parinfo widgets.  This should
  ;; catch everything else.
  self->refresh
end

;; Unregister a cw_obj from our repopulate list
pro pfo_parinfo_obj_cw_obj::unregister_repop, $
   cw_obj ;; cw_obj of widget to register

  ;; Check syntax of invocation
  if NOT obj_valid(cw_obj) then $
     message, 'ERROR: specify a valid object: cw_obj, presumably one that controls a widget that displays a [set of] parinfo.  "self.pfo_obj->unregister_repop, self" is the usual call'

  ;; Quietly return if there are no widgets in our list, since people
  ;; are unlikely to check to see if they were registered in the first place
  if N_elements(*self.pparinfo_repop_list) eq 0 then $
     return

  ;; Find all the _other_ cw_obj in this list
  good_idx = where(*self.pparinfo_repop_list ne cw_obj, count)
  if count eq 0 then begin
     ;; If these is nothing [left] in our repop_list.  Make
     ;; *self.pparinfo_repop_list an undefined variable
     ptr_free, self.pparinfo_repop_list
     self.pparinfo_repop_list = ptr_new(/allocate_heap)
     return
  endif ;; no more repops

  ;; If we made it here, we need to remove our cw_obj.  Just make the
  ;; repop_list everything else.  Note: if our cw_obj wasn't on
  ;; the list to begin with, this just quietly does nothing.
  *self.pparinfo_repop_list = (*self.pparinfo_repop_list)[good_idx]

end

;; Register a cw_obj in our repopulate list.  
pro pfo_parinfo_obj_cw_obj::register_repop, $
   cw_obj ;; cw_obj of widget to register

  if NOT obj_valid(cw_obj) then begin
     message, 'WARNING: invalid cw_obj.', /CONTINUE
     return
  endif ;; cw_obj

  pfo_array_append, *self.pparinfo_repop_list, cw_obj
end

;; Refresh method.  If given an idx, refreshes all widgets displaying
;; that parinfo[idx].  Refresh only widgets which display
;; parinfo.value or params if value is specified.  In the value case,
;; value of value is used for the value of the widget.  Also in the
;; value case, idx can be omitted if there is one element in value for
;; each element in parinfo (e.g. value=params).  Value-displaying
;; widgets are refreshed using parinfo.value in the normal idx case.
;; On other words, you don't *have* to specify value=value to
;; get the value to refresh.  It is only there if that is all you want
;; to refresh.
pro pfo_parinfo_obj_cw_obj::refresh, $
   idx=idx_in, $ ;; parinfo entry that has changed.  If none provided, all registered (value) widgets are refreshed
   params=params  ;; (optional) parinfo.value (or param) value(s) to display in widgets such as pfo_parinfo_value_cw.

  ;; Check to see if this is really a parinfo cw_obj
  if N_elements(idx_in) + N_elements(params) eq 0 then begin
     ;; If idx or params are not supplied, assume this is a regular
     ;; cw_obj displaying pfo_obj property other than something in
     ;; parinfo[idx]
     self->pfo_obj_cw_obj::refresh
     return
  endif
  
  ;; If we made it here, we are refreshing a widget that is displaying
  ;; parinfo[idx]

  ;; Quietly return if we have no registered widgets
  if N_elements(*self.pparinfo_refresh_list) eq 0 then $
     return

  ;; We shouldn't be asking for a refresh
  if N_elements(*self.pparinfo) eq 0 then begin
     message, /CONTINUE, 'WARNING: no parinfo array defined in this pfo_obj, but refresh was requested for idx = ' + pfo_array2string(idx_in) + ' and/or params = ' + pfo_array2string(params)
     return
  endif ;; no parinfo

  ;; If we made it here, we should have something reasonable to do.

  ;; Make a local copy of our input idx (index into parinfo), since we
  ;; will much with it
  if N_elements(idx_in) ne 0 then $
     idx = idx_in

  ;; Prepare an index into the refresh list.  This is the whole list
  ;; for regular refreshes.  This is just the members of the list
  ;; which have been marked as displaying values when in values mode
  pfo_idx, *self.pparinfo_refresh_list, refresh_list_idx
  if N_elements(params) ne 0 then begin
     ;; Limist refresh_list_idx to widgets displaying parinfo.value
     ;; and do other setup for the value/params case.
     ;; Make sure we have idx.
     pfo_idx, params, idx
     ;; Check for consistency between length of params and idx vectors
     if N_elements(params) ne N_elements(idx) then begin
        message, /CONTINUE, 'WARNING: number of elements in params and idx do not match.  No widgets refreshed.'
        return
     endif ;; params/idx mismatch

     ;; If we made it here, we have a valid params/idx pair

     ;; Check to see if we have any values displayed
     v_idx = where((*self.pparinfo_refresh_list).value ne 0, count)
     ;; Quietly return if we have no widgets displaying values
     if count eq 0 then $
        return
     ;; unwrap.  refresh_idx is now a subset of indices into the
     ;; *self.pparinfo_refresh_list structure
     refresh_list_idx = refresh_list_idx[temporary(v_idx)]
  endif ;; value/params setup

  ;; Loop through each of our idx.  These are idx into parinfo.  There
  ;; is usually just one idx in the case we have a normal refresh
  ;; event; all idx when we have values (e.g. all of the params)
  for i_idx=0, N_elements(idx)-1 do begin
     ;; Find all matches of this idx in our refresh list.  cw_idx is going to
     ;; end up being the set of indices into our refresh list of all
     ;; widgets displaying parinfo[idx[i_idx]]
     cw_idx = where(idx[i_idx] eq $
                    ((*self.pparinfo_refresh_list)[refresh_list_idx]).idx, count)
     if count eq 0 then $
        CONTINUE
     ;; unwrap -- essential in the value refresh case
     cw_idx = refresh_list_idx[temporary(cw_idx)]
     ;; Loop through the elements of cw_idx to call the refresh
     ;; methods of the cw_objs
     for iw=0,count-1 do begin
        if NOT obj_valid((*self.pparinfo_refresh_list)[cw_idx[iw]].cw_obj) then begin
           help, (*self.pparinfo_refresh_list)[cw_idx[iw]].cw_obj, out=s
           message, /CONTINUE, 'WARNING: object ' + s + ' not properly unregistered from refresh list.  Skipping.  FIX YOUR CODE!'
           CONTINUE
        endif
        if N_elements(params) eq 0 then begin
           (*self.pparinfo_refresh_list)[cw_idx[iw]].cw_obj->refresh
        endif else begin
           (*self.pparinfo_refresh_list)[cw_idx[iw]].cw_obj->refresh, params=params[idx[i_idx]]
        endelse
     endfor ;; each matching widget in our refresh list
  endfor ;; Each input idx

end

;; Unregister a cw_obj from our refresh list
pro pfo_parinfo_obj_cw_obj::unregister_refresh, $
   cw_obj ;; cw_obj of widget to register

  ;; Check syntax of invocation
  if NOT obj_valid(cw_obj) then $
     message, 'ERROR: specify a valid object: cw_obj, presumably one that controls a widget that displays a [set of] parinfo.  "self.pfo_obj->unregister_refresh, self" is the usual call'

  ;; Check our inherited list first.  
  self->pfo_obj_cw_obj::unregister_refresh, cw_obj

  ;; Quietly return if there are no widgets in our list, since people
  ;; are unlikely to check to see if they were registered in the first place
  if N_elements(*self.pparinfo_refresh_list) eq 0 then $
     return

  ;; Find all the _other_ cw_obj in this list
  good_idx = where((*self.pparinfo_refresh_list).cw_obj ne cw_obj, count)
  if count eq 0 then begin
     ;; If these is nothing [left] in our refresh_list.  Make
     ;; *self.pparinfo_refresh_list an undefined variable
     ptr_free, self.pparinfo_refresh_list
     self.pparinfo_refresh_list = ptr_new(/allocate_heap)
     return
  endif ;; no more repops

  ;; If we made it here, we need to remove our cw_obj.  Just make the
  ;; refresh_list everything else.  If our cw_obj was not in the list,
  ;; don't complain, since one of our inherited routines might be
  ;; taking care of it.
  *self.pparinfo_refresh_list = (*self.pparinfo_refresh_list)[good_idx]
                     
end

;; Register a cw_obj from our refresh list
pro pfo_parinfo_obj_cw_obj::register_refresh, $
   cw_obj, $ ;; cw_obj of widget to register
   idx=idx, $ ;; index into parinfo
   value=value  ;; boolean indicating whether or not parameter is related to a parinfo.value

  if NOT obj_valid(cw_obj) then begin
     message, 'WARNING: invalid cw_obj.', /CONTINUE
     return
  endif ;; cw_obj

  ;; Check to see if this is really a parinfo_cw_obj
  if N_elements(idx) + N_elements(value) eq 0 then begin
     ;; If idx or value are not supplied, assume this is a regular
     ;; cw_obj displaying pfo_obj property other than something in
     ;; parinfo[idx]
     self->pfo_obj_cw_obj::register_refresh, cw_obj
     return
  endif

  ;; If we made it here, we are registering a widget that is
  ;; displaying a parinfo[idx].  Keep track of cw_obj, idx and value
  ;; in the list by using a structure.
  if N_elements(value) eq 0 then $
     value = 0
  pfo_array_append, *self.pparinfo_refresh_list, $
                    {pfo_parinfo_cw_refresh_list_struct, $
                     cw_obj: cw_obj, $
                     idx:idx, $
                     value:value}

end

function pfo_parinfo_obj_cw_obj::descr
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

  descr = *self.ppfo_parinfo_obj_cw_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_parinfo_obj_cw_obj::cleanup
  ;; Order is important here, since this causes things to unregister
  self->pfo_obj_cw_obj::cleanup

  ptr_free, self.ppfo_parinfo_obj_cw_obj_descr
  ptr_free, self.pparinfo_repop_list
  ptr_free, self.pparinfo_refresh_list

end

function  pfo_parinfo_obj_cw_obj::init, $
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

  self.ppfo_parinfo_obj_cw_obj_descr $
     = ptr_new( $
     {README	: 'pfo_parinfo_obj_cw_obj stores the repopulate and refresh lists for widgets displaying pfo parinfo arrays', $
      METHODS	: 'repopulate, refresh, register_repop, register_refresh, unregister_repop, unregister_refresh'} $
              )

  ;; Turn our null reference pointers into undefined variables
  self.pparinfo_repop_list = ptr_new(/allocate_heap)
  self.pparinfo_refresh_list = ptr_new(/allocate_heap)

  ;; Call our superclass init methods
  ok = self->pfo_obj_cw_obj::init(_EXTRA=extra)

  ;; If we made it here, our object should be initialized properly
  return, 1
end

pro pfo_parinfo_obj_cw_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Define the structures that hold the refresh list
  struct = {pfo_parinfo_cw_refresh_list_struct, $
            cw_obj	:	obj_new(), $
            idx		:	0L, $
            value	:	0B}

  objectClass = $
     {pfo_parinfo_obj_cw_obj, $
      ppfo_parinfo_obj_cw_obj_descr	: ptr_new(), $  ;; Pointer to description structure
      pparinfo_repop_list	: ptr_new(), $ ;; list of cw_objs, preusmably displaying parinfo information that want their repopulate methods called to be called when pfo_obj->repopulate is called
      pparinfo_refresh_list	: ptr_new(), $ ;; list of parinfo-specific cw_objs that are searched for matches when pfo_obj->refresh, idx, params=params is called
     inherits pfo_obj_cw_obj 	   	     $ ;; Links pfo_obj to cw_obj so they both die properly.  Also manages non-parinfo widget refresh list
     }
      
end
