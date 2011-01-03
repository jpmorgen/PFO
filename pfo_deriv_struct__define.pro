;+
; NAME: pfo_deriv_struct__define
;
; PURPOSE: Define the parinfo.pfo_deriv substructure that enables
; derivatives of sections of parinfo to be taken and used in the
; pfo_funct system
;
; CATEGORY: PFO, optional add-in
;
; CALLING SEQUENCE: 
;  pfo_parinfo__define, parinfo=pfo_parinfo
;  pfo_deriv = {pfo_deriv : {pfo_deriv_struct}}
;  pfo_parinfo = struct_append(pfo_parinfo, pfo_deriv)
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
; $Id: pfo_deriv_struct__define.pro,v 1.1 2011/01/03 21:49:16 jpmorgen Exp $
;
; $Log: pfo_deriv_struct__define.pro,v $
; Revision 1.1  2011/01/03 21:49:16  jpmorgen
; Initial revision
;
;-

pro pfo_deriv_struct__define
  pfo_deriv_struct $
    = {pfo_deriv_struct, $
       $ ;; Set ID in function(s) you want to differentiate, one ID
       $ ;; per derivative operation, the same ID for all the
       $ ;; parameters.  In the derivative function itself, set this
       $ ;; to the ID of the function(s) you want to differentiate
       ID	: 0, $
       $ ;; Status, set in the function you want to differentiate,
       $ ;; lets you turn your derivative on and off without 
       $ ;; messing with the IDs.  Use pfo_deriv.status = !pfo.active, !pfo.inactive.  
       $ ;; If you want to turn the derivative itself off, do that with pfo.status
       status	: 0, $ 
       $ ;; dx is the axis ID of the variable over which the
       $ ;; derivative is calculated.  Usually !pfo.Xin or
       $ ;; !pfo.xaxis.  The default is invalid (!pfo.none,) which is
       $ ;; reset to !pfo.Xin in pfo_deriv, /CREATE
       dx	: 0, $
       $ ;; dy is the axis which is differentiated, usually !pfo.Xaxis
       $ ;; or !pfo.Yaxis.  The default in this structure is invalid
       $ ;; (!pfo.none) and is reset to !pfo.Yaxis in pfo_deriv, /create
       dy	: 0} 
end
