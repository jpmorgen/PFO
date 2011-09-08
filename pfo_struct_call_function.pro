;+
; NAME: pfo_struct_call_function

; PURPOSE: Sequentially call (as a function) each
; <tag>_struct__"method" in a parinfo structure, where "method" is the
; second positional argument

; CATEGORY: PFO
;
; CALLING SEQUENCE: a = pfo_struct_call_function(parinfo, method,
; <keyword args>)

; DESCRIPTION: This routine makes use of the naming convention set up
; in the pfo_struct system (see pfo_struct_new) that allows "methods"
; to be defined for each tag in the structure.  pfo_struct_setget_tag
; calls the methods <tag>_struct__set_tag or <tag>_struct__get_tag.
; This routine calls <tag>_struct__"method", where "method" is a
; string given by the second positional parameter.  This is handy for
; things like the update and print methods

; INPUTS: parinfo: any array of type structure
;	  method: "method" to call (e.g. <tag>_struct__method)
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 

;    update_only_tags: scaler or vector string.  Instead of running
;    <tag>_struct__<method> for all the tags in parinfo, only run for
;    the indicated tag(s).  This keyword is used, for instance, in
;    individual <tag>_struct__update routines to resolve any order
;    dependent interdependencies.

;    all other keywords are passed to and from the
;    underlying routines via the _REF_EXTRA mechanism

; OUTPUTS: This function puts the results of the functions called into
; an array of type anonymous structure.  One element of this looks
; like:

;  result = {routine: <called routine>, result:result}

;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS: The order of parameters is important.  Parinfo must be
; listed first so that this routine can be used by
; pfo_parinfo_obj::parinfo_call_function
;
; FUNCTION:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_struct_call_function.pro,v 1.2 2011/09/08 20:18:08 jpmorgen Exp $
;
; $Log: pfo_struct_call_function.pro,v $
; Revision 1.2  2011/09/08 20:18:08  jpmorgen
; Added update_only_tags
;
; Revision 1.1  2011/09/01 22:24:46  jpmorgen
; Initial revision
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_struct_call_function, $
   parinfo, $
   method, $
   p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   update_only_tags=update_only_tags, $
   _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Initialize our return value
  retval = !tok.nowhere

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with whatever has been done so far.', /CONTINUE
        return, retval
     endif
  endif ;; not debugging

  ;; Raise an error if we were asked to work on nothing
  if N_elements(parinfo) + N_elements(method) eq 0 then $
     message, 'ERROR: required input parameter(s) parinfo and/or method are missing'


  ;; Default is to cycle through the top-level tags and call any
  ;; defined <tag>_struct__methods
  tns = tag_names(parinfo)
  ;; The default list can be overrides with the update_only_tags keyword
  if N_elements(update_only_tags) ne 0 then $
     tns = update_only_tags

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
     junk = routine_info(to_call, /source, /function)
     ;; Cancel the catch in case we are in debugging mode
     CATCH, /CANCEL
        
     ;; If we made it here, we should have a good call function to
     ;; call.  Handle the error messages in a configurable way.
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, /CONTINUE, 'WARNING: ' + to_call + ' produced the above error.  Use pfo_debug to help fix error.' 
           CONTINUE
        endif ;; quietly ignore problems in subsequent __set_tag routine
     endif ;; not debugging

     case N_params()-1 of
        0: message, 'ERROR: Function name not supplied'
        1: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo) $
           else $
              ret1 = call_function(to_call, parinfo, _EXTRA=extra)
        end
        2: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo, p1) $
           else $
              ret1 = call_function(to_call, parinfo, p1, _EXTRA=extra)
        end
        3: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo, p1, p2) $
           else $
              ret1 = call_function(to_call, parinfo, p1, p2, _EXTRA=extra)
        end
        4: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo, p1, p2, p3) $
           else $
              ret1 = call_function(to_call, parinfo, p1, p2, p3, _EXTRA=extra)
        end
        5: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo, p1, p2, p3, p4) $
           else $
              ret1 = call_function(to_call, parinfo, p1, p2, p3, p4, _EXTRA=extra)
              end
        6: begin
           if N_elements(extra) eq 0 then $
              ret1 = call_function(to_call, parinfo, p1, p2, p3, p4, p5) $
           else $
              ret1 = call_function(to_call, parinfo, p1, p2, p3, p4, p5, _EXTRA=extra)
        end
     endcase

     ;; Combine the retvals in our return structure array.  Gently
     ;; CATCH any errors
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, /CONTINUE, 'WARNING: concatenating results from ' + to_call + ' produced the above error.  Routine ran but probably did not return a value consistent with the return value of other routines.  Continuing without concatenating this result.'
           CONTINUE
        endif ;; quietly ignore problems in subsequent __set_tag routine
     endif ;; not debugging
     pfo_array_append, retval, {routine: to_call, result: ret1}

  endfor ;; each top-level tag in parinfo
  return, retval
end

