;+
; NAME: pfo_gc
;
; PURPOSE: Permanently delete or "garbage collect" parameters marked
; for deletion with pfo.status = !pfo.delete.  See pfo_mode
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_gc, parinfo
;
; INPUTS: parinfo
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
; SIDE EFFECTS: If all parinfo parameters are deleted, parinfo is set
; to 'none.'  This is designed to work transparently with
; array_append, when rebulding the parinfo array.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
; init = {pfo_sysvar}
; parinfo = pfo_fcreate(!pfo.voigt)
; parinfo[0:3].pfo.status = !pfo.delete
; pfo_gc, parinfo
; print, parinfo
; moreparinfo = pfo_fcreate(!pfo.voigt)
; parinfo = array_append(moreparinfo, parinfo)
; print, parinfo
;; or you might prefer
; print, pfo_funct(parinfo=parinfo, print=!pfo.pmp)
;
; MODIFICATION HISTORY:
;
; $Id: pfo_gc.pro,v 1.3 2011/09/23 12:57:45 jpmorgen Exp $
;-
pro pfo_gc, parinfo
  init = {pfo_sysvar}
  init = {tok_sysvar}
  ;; Check to see if the parinfo is valid.  Setting parinfo = 'none'
  ;; is the hint that parinfo is empty.  This works well with
  ;; array_append when building up parinfo to begin with too.
  if size(parinfo, /TYPE) ne !tok.struct then $
    return

  ;; Find the parinfo we marked for deletion
  delidx = where(parinfo.pfo.status eq !pfo.delete, count, $
                 complement=keep_idx, ncomplement=nkeep)
  ;; If we find none, just return without doing anything
  if count eq 0 then $
    return
  ;; If we have deleted everything, use temporary to make parinfo
  ;; undefined
  if nkeep eq 0 then begin
     junk = temporary(parinfo)
     return
  endif
  parinfo = parinfo[keep_idx]  
end
