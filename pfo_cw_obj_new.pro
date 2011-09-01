;+
; NAME: pfo_cw_obj_new
;
; PURPOSE: create a new cw_obj
;
; CATEGORY: PFO compound widgets
;
; CALLING SEQUENCE: cw_obj = pfo_cw_obj_new(parentID, _EXTRA=extra)
;
; DESCRIPTION: This is a wrapper around obj_new
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
; $Id: pfo_cw_obj_new.pro,v 1.1 2011/09/01 22:19:43 jpmorgen Exp $
;
; $Log: pfo_cw_obj_new.pro,v $
; Revision 1.1  2011/09/01 22:19:43  jpmorgen
; Initial revision
;
;-
function pfo_cw_obj_new, $
   parentID, $
   _REF_EXTRA=extra

  help, calls=calls
  objname = strmid(calls[1], 0, strpos(calls[1], ' ')) + '_obj'
  return, obj_new(objname, parentID, _EXTRA=extra)

end
