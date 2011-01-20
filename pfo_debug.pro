;+
; NAME:
;
; PURPOSE:
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
; $Id: pfo_debug.pro,v 1.1 2011/01/20 22:59:08 jpmorgen Exp $
; $Log: pfo_debug.pro,v $
; Revision 1.1  2011/01/20 22:59:08  jpmorgen
; Initial revision
;
;-
pro pfo_debug, level
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ON_ERROR, !tok.return
  message, /INFORMATIONAL, 'Current debug level: ' + strtrim(!pfo.debug, 2) 

  if N_params() eq 0 then begin
     message, /CONTINUE, 'USAGE: pfo_debug, level (0 = normal, 1 = CATCH statements skipped, 2, XMANAGER catching turned off)'
     return
  endif

  !pfo.debug = level
  case level of
     0:                         ; normal
     1:                         ; catch statements are skipped
     2: xmanager, catch=0       ; widget catching is skipped
     else: begin
        message, /CONTINUE, 'NOTE: debug levels 0,1,2,3 are supported, setting to level 2'
        pfo_debug, 2
     end
  endcase

  message, /INFORMATIONAL, 'Debug level set to: ' + strtrim(!pfo.debug, 2)

end
