;+
; NAME: pfo_struct_call_procedure
;
; PURPOSE: Sequentially call each <tag>_struct__"method" in a
; parinfo structure, where "method" is the second positional argument
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_struct_call_procedure, parinfo, method,
; <keyword args>

; DESCRIPTION: This routine makes use of the naming convention set up
; in the pfo_struct system (see pfo_struct_new) that allows "methods"
; to be defined for each tag in the structure.  pfo_struct_setget_tag
; calls the methods <tag>_struct__set_tag or <tag>_struct__get_tag.
; This routine calls <tag>_struct__"method", where "method" is a
; string given by the second positional parameter.  This is handy for
; things like the update and print methods

; INPUTS: parinfo: any array of type structure
;	  method: "method" to call (e.g. <tag>_struct__method
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: all keywords are passed to and from the
; underlying routines via the _REF_EXTRA mechanism
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
; $Id: pfo_struct_call_procedure.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_struct_call_procedure.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_struct_call_procedure, method, parinfo, _REF_EXTRA=extra

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

  ;; Raise an error if we were asked to work on nothing
  if N_elements(parinfo) + N_elements(method) eq 0 then $
    message, 'ERROR: required input parameter(s) parinfo and/or method are missing'

  ;; Cycle through the top-level tags and call the <tag>_struct__methods
  tns = tag_names(parinfo)
  for it=0, N_elements(tns)-1 do begin
     ;; For pfo debugging, we need to do this in two steps.  Always
     ;; quietly ignore routines that don't resolve. 
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        CONTINUE
     endif
     ;; Make sure that we have compiled the structure definition in
     ;; this session.  If no such definition exists, our work is done
     ;; for this tag
     resolve_routine, tns[it]+'_struct__define', /no_recompile
     ;; If we made it here, we might have the corresponding method in
     ;; this tag.
     to_call = tns[it]+'_struct__' + method
     ;; routine_info throws an error if the routine doesn't exist
     junk = routine_info(to_call, /source)

     ;; If we made it here, we should have a good call procedure to
     ;; call.  Handle the error messages in a configurable way.
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, /INFORMATIONAL, 'WARNING: ' + strtrim(to_call, 2) + ' produced the above error.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting.' 
           CONTINUE
        endif ;; quietly ignore problems in subsequent __set_tag routine
     endif ;; not debugging
     if keyword_set(extra) then $
       call_procedure, to_call, parinfo, _EXTRA=extra $
     else $
       call_procedure, to_call, parinfo
  endfor ;; each top-level tag in parinfo

end
