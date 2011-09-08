;+
; NAME: pfo_struct_updated
;
; PURPOSE: test to see if an update has already been done by the
; calling <tag>_struct__update routine
;
; CATEGORY: [PFO] structures
;
; CALLING SEQUENCE: done = pfo_updated(completed_updates)
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
; EXAMPLE: At the beginning of a <tag>_struct__update routine:

; if pfo_updated(completed_updates) then return

; to avoid needlessly re-running code

; MODIFICATION HISTORY:
;
; $Id: pfo_struct_updated.pro,v 1.1 2011/09/08 20:17:50 jpmorgen Exp $
;
; $Log: pfo_struct_updated.pro,v $
; Revision 1.1  2011/09/08 20:17:50  jpmorgen
; Initial revision
;
;-

function pfo_struct_updated, completed_updates
  ;; If we have no updates yet, we obviously haven't done the calling code
  if N_elements(completed_updates) eq 0 then $
     return, 0

  ;; Get our call stack list
  help, calls=calls
  ;; Extract the tagname
  tag = strmid(calls[1], 0, strpos(calls[1], '_struct__update'))
  
  junk = where(tag eq completed_updates, count)
  if count eq 0 then $
     return, 0
  
  ;; If we made it here, we must have previously run the update on the
  ;; calling <tag>_struct__update routine
  return, 1
end
