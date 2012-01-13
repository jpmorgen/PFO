;+
; NAME: pfo_back_idx
;
; PURPOSE: return the indicies of the background function in a parinfo
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; DESCRIPTION: Return indices into parinfo of likely background
; functions (functions that replace the Y-axis)
;
; INPUTS: 
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   parinfo: (optional) parinfo array describing the function

;   pfo_obj: (optional) pfo_obj encapsulating parinfo

;   idx: indices into parinfo over which to look for backgroun
;   functions

;  non_back_idx: complement of returned background indices

;
; OUTPUTS: indices into parinfo of likley background functions
; (!tok.nowhere if none found)
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
; $Id: pfo_back_idx.pro,v 1.2 2012/01/13 20:49:29 jpmorgen Exp $
;
; $Log: pfo_back_idx.pro,v $
; Revision 1.2  2012/01/13 20:49:29  jpmorgen
; Documentation
;
; Revision 1.1  2012/01/05 20:01:17  jpmorgen
; Initial revision
;
;-
function pfo_back_idx, $
   parinfo=parinfo, $
   pfo_obj=pfo_obj, $
   non_back_idx=non_back_idx, $
   idx=idx

  ;; Make sure system variables are defined
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize default outputs
  back_idx = !tok.nowhere
  non_back_idx = !tok.nowhere
  
  ;; no parinfo or pfo_obj, return !tok.nowhere
  if N_elements(parinfo) eq 0 and N_elements(pfo_obj) eq 0 then $
     return, back_idx

  ;; no parinfo, return !tok.nowhere
  if N_elements(parinfo) eq 0 and $
     pfo_obj->parinfo_call_function( $
     /no_update, 'N_elements') eq 0 then $
        return, back_idx

  ;; If we made it here, we know we have a parinfo and/or pfo_obj

  ;; Read the tags we need from the parinfo passed on the command line
  ;; or the parinfo encapsulated in the pfo_obj
  if N_elements(parinfo) ne 0 then begin
     ;; Make sure idx is defined
     pfo_idx, parinfo, idx
     outaxis = parinfo[idx].pfo.outaxis
     fop = parinfo[idx].pfo.fop
  endif else begin
     ;; This sets idx, if it doesn't exist already
     pfo_obj->parinfo_call_procedure, $
        /no_update, 'pfo_struct_setget_tag', /get, idx=idx, $
        taglist_series='pfo', outaxis=outaxis, fop=fop
  endelse ;; parinfo vs pfo_obj only

  ;; Get idx of background.  --> This is tough, since people can use
  ;; any kind of function or combination of functions to define the
  ;; background.  For now, lets use functions that replace the y-axis
  back_idx = where(outaxis eq !pfo.Yaxis and fop eq !pfo.repl, count, $
                   complement=non_back_idx)
  ;; unwrap
  if count ne 0 then $
     back_idx = idx[back_idx]
  return, back_idx

end
