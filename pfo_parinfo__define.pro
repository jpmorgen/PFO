;+
; NAME: pfo_parinfo__define
;
; PURPOSE: Define the most basic parinfo structure for the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: parinfo = pfo_struct_new('pfo_parinfo')
;
; DESCRIPTION: This is designed to be used with the pfo_struct_new
; system, since there are non-null default values in some of the tags.
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
; $Id: pfo_parinfo__define.pro,v 1.2 2015/03/03 21:54:06 jpmorgen Exp $
;
; $Log: pfo_parinfo__define.pro,v $
; Revision 1.2  2015/03/03 21:54:06  jpmorgen
; Summary: Obselete
;
;-
; +

function pfo_parinfo__init
  ;; Get an initialized mpfit_parinfo structure
  parinfo = pfo_struct_new('mpfit_parinfo')
  pfo = pfo_struct_new('pfo_struct')
  parinfo = struct_append(mpfit_parinfo, pfo)
end

pro pfo_parinfo__define
  ;; We want pfo_parinfo to be an anonymous structure so that we can
  ;; keep adding to it.
end
