;+
; NAME: pfo_parinfo_update

; PURPOSE: Updates the information in a parinfo, optionally adding
; tags, to make all of the information in the parinfo is
; self-consistent.

; CATEGORY: PFO 
;
; CALLING SEQUENCE: pfo_parinfo_update, parinfo,
; required_tags=required_tags, name=name,
; completed_updates=completed_updates, _REF_EXTRA=extra
;
; DESCRIPTION:

; Uses pfo_struct_append and the pfo_struct_new system to make sure
; that the list of required_tags exists in parinfo.  Updates the
; appropriate parinfo_template.  Also calls the <tag>_struct__update
; methods of each of the tags in the top-level structure to resolve
; any interdependencies between top-level tags (e.g. pfo_link and
; .tied)

; INPUTS:

; parinfo: if an array of type struct (an isolated parinfo
; array), tags are added to the struct and the 
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

; required_tags: tags that should be added to the parinfo structure
; (and template) if not they are not already there.  pfo_struct_append
; and the pfo_struct_new system initialize any appended tags to their
; desired default values

; name: if you want, you can make the resulting parinfo struct a named
; structure.  This is not always a good idea, since once you use a
; name, you can't reuse it for a different structure.  Instead, it is
; recommended that you keep the structure "anonymous" so that it can
; grow organically.

; completed_updates (optional return): assuming that each
; <tag>_struct__update "method" calls pfo_struct_update_complete, this
; keyword will have the list of tags which ran their _struct__update
; methods, in the order in which they ran.

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
; $Id: pfo_parinfo_update.pro,v 1.4 2011/11/18 16:14:29 jpmorgen Exp $
;
; $Log: pfo_parinfo_update.pro,v $
; Revision 1.4  2011/11/18 16:14:29  jpmorgen
; Minor comment update
;
; Revision 1.3  2011/09/08 20:18:40  jpmorgen
; Added completed_updates system to help with interdependencies
;
; Revision 1.2  2011/09/01 22:25:41  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_parinfo_update, $
   parinfo, $
   required_tags=required_tags, $
   completed_updates=completed_updates, $
   name=name, $
   _REF_EXTRA=extra
 
  init = {pfo_sysvar}
  init = {tok_sysvar}

  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with whatever has been done so far.', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Quietly return if we have no parinfo
  if N_elements(parinfo) eq 0 then $
    return

  ;; Make sure our template has the right tags.  This makes sure that
  ;; it has the tags that correspond to this parinfo.  It also adds
  ;; any additional required_tags
  junk = pfo_parinfo_template(set=parinfo, required_tags=required_tags, _EXTRA=extra)

  ;; Use struct_append to add the tags to the parinfo
  if keyword_set(required_tags) then $
    pfo_struct_append, parinfo, required_tags, name=name

  ;; Cycle through all the tags and call the <tag>_struct__update
  ;; "method" for each one so that any interdependencies are
  ;; resolved.  Make sure we pass completed_updates
  pfo_struct_call_procedure, $
     parinfo, 'update', completed_updates=completed_updates, _EXTRA=extra
  
end
