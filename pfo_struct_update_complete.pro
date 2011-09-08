;+
; NAME: pfo_struct_update_complete
;
; PURPOSE: Helps pfo_struct_update system keep track of which tags
; have been updated
;
; CATEGORY: [PFO] structures
;
; CALLING SEQUENCE: pfo_update_complete, completed_updates
;
; DESCRIPTION: appends tag name of the calling <tag>_struct__update
; routine to the completed_updates list maintained by the
; _struct__update system
;
; INPUTS: completed_updates: string array of tag names which have had
; their <tag>_struct__update procedures run already
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS: completed_updates with the calling
; <tag>_struct__update's tag appended
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
; EXAMPLE: In a <tag>_struct__update routine, simply put:

; pfo_update_complete, completed_updates

; before returning.

;
; MODIFICATION HISTORY:
;
; $Id: pfo_struct_update_complete.pro,v 1.1 2011/09/08 20:17:35 jpmorgen Exp $
;
; $Log: pfo_struct_update_complete.pro,v $
; Revision 1.1  2011/09/08 20:17:35  jpmorgen
; Initial revision
;
;-
pro pfo_struct_update_complete, completed_updates
  help, calls=calls
  pfo_array_append, $
     completed_updates, $
     strmid(calls[1], 0, strpos(calls[1], '_struct__update'))
end
