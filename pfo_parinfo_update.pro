;+
; NAME: pfo_parinfo_update

; PURPOSE: Updates the information in a parinfo, optionally adding
; tags, to make all of the information in the parinfo self-consistent.

; CATEGORY: PFO 
;
; CALLING SEQUENCE: pfo_parinfo_update, parinfo,
; required_tags=required_tags, name=name, _REF_EXTRA=extra
;
; DESCRIPTION:

; Uses pfo_struct_append and the pfo_struct_new system to make sure
; that the list of required_tags exists in parinfo.  Updates the
; appropriate parinfo_template.  Also calls
; "pfo_struct_call_procedure, 'update', parinfo" to resolve any
; interdependencies between top-level tags (e.g. pfo_link and .tied)

;
; INPUTS:

; parinfo: if an array of type struct (an isolated parinfo
; array), tags are added to the struct and the 
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
; $Id: pfo_parinfo_update.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_parinfo_update.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_parinfo_update, parinfo, required_tags=required_tags, name=name, _REF_EXTRA=extra
 
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

  ;; Make sure our template has the right tags.
  junk = pfo_parinfo_template(parinfo, required_tags=required_tags)

  ;; Use struct_append to add the tags to the parinfo
  if keyword_set(required_tags) then $
    pfo_struct_append, parinfo, required_tags, name=name

  ;; Cycle through all the tags and call the tag_struct__update
  ;; "method" for each one so that any interdependencies are resolved.
  pfo_struct_call_procedure, 'update', parinfo, _EXTRA=extra
  
end
