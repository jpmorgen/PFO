;+
; NAME: pfo_obj_new
;
; PURPOSE: Create a new pfo_obj
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:

;   pfo_obj = pfo_obj_new([ObjectCLassName or template], keyword arguments to ObjectCLassName init method)

;
; DESCRIPTION: This routine is a wrapper around IDL's obj_new function.  If no
; ObjectClassName is provided, ObjectClassName is set to the value
; stored !pfo.pfo_obj_ClassName
;
;
; INPUTS: 

; OPTIONAL INPUTS: p0, p1, p2, p3.  If p0 is a string, it is taken to
; be the object class name.  If it is an object, the object class name
; copied and a new object is returned.  The [other] positional
; parameters are assumed to be [Xin,] Yin, and [Yerr].

; KEYWORD PARAMETERS:  All keyword parameters are passed on to the
; underlying object class init method(s)
;
; OUTPUTS: newly initialized object (if successful)
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
; $Id: pfo_obj_new.pro,v 1.2 2012/01/13 20:48:30 jpmorgen Exp $
;
; $Log: pfo_obj_new.pro,v $
; Revision 1.2  2012/01/13 20:48:30  jpmorgen
; Added template option
;
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

  ;; Handle the different types of p0
  case size(/type, p0) of
     !tok.string: begin
        ;; Named object.  This is basically just IDL's obj_new case
        return, obj_new(p0, p1, p2, p3, _EXTRA=extra)
     end
     !tok.objref: begin
        ;; Reading our object class name from a template pfo_obj.  Use
        ;; the output of help:
        ;; P0         OBJREF    = <ObjHeapVar1(PFO_OBJ)>
        help, p0, output=s
        ;; Find last open (
        op = strpos(s, '(', /reverse_search)
        ;; Find close of expression
        cp = strpos(s, ')>', /reverse_search)
        return, obj_new(strmid(s, op+1, cp-op-1), p1, p2, p3, _EXTRA=extra)
     end
     else: begin
        ;; Default is to use value in pfo system variable
        return, obj_new(!pfo.pfo_obj_ClassName, p1, p2, p3, _EXTRA=extra)
     end
  endcase

end
