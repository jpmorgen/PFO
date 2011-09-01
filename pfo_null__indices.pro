;+
; NAME: pfo_null__indices
;
; PURPOSE: Provides generic indices services for functions
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
; $Id: pfo_null__indices.pro,v 1.1 2011/09/01 22:22:33 jpmorgen Exp $
;
; $Log: pfo_null__indices.pro,v $
; Revision 1.1  2011/09/01 22:22:33  jpmorgen
; Initial revision
;
;-
function pfo_null__indices, $
   x, params, $
   idx=idx, $
   fname=fname, $
   _REF_EXTRA=extra

  message, /NONAME, !error_state.msg, /CONTINUE
  message, /CONTINUE, 'WARNING: __indices method for function ' + fname + ' is either not defined or generated the above error'
  return, idx

end
