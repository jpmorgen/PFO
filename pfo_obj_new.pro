;+
; NAME: pfo_obj_new
;
; PURPOSE: Create a new pfo_obj
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:

;   pfo_obj = pfo_obj_new([ObjectCLassName], keyword arguments to ObjectCLassName init method)

;
; DESCRIPTION: This routine is a wrapper around IDL's obj_new function.  If no
; ObjectClassName is provided, ObjectClassName is set to the value
; stored !pfo.pfo_obj_ClassName
;
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
; $Id: pfo_obj_new.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_obj_new.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_obj_new, $
   p0, $
   p1, $
   p2, $
   p3, $
   _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object "' + pfo_obj_ClassName + '" not properly created ', /CONTINUE
        return, obj_new()
     endif
  endif ;; not debugging

  pfo_obj_ClassName = !pfo.pfo_obj_ClassName

  ;; Check to see if user omitted replacement pfo_obj ClassName.  This
  ;; will usually be the case.  The other positional parameters will
  ;; be (optionally) Xin, Yin, and Yerr.
  if size(/type, p0) ne !tok.string then $
     return, obj_new(pfo_obj_ClassName, p0, p1, p2, _EXTRA=extra)

  ;; If we made it here, the user has built their own pfo_obj
  ;; replacement.
  pfo_obj_ClassName = p0
  return, obj_new(pfo_obj_ClassName, p1, p2, p3, _EXTRA=extra)



end
