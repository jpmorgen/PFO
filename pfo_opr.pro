;+
; NAME: pfo_opr
;
; PURPOSE: perform a mathematical operation on a (set of) parinfo
; records
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_opr, parinfo, opr, value, idx=idx
;
; DESCRIPTION: applies operation "opr value" to parinfo[idx].value and
; associated tags (e.g. limits, etc.)
;
; INPUTS: parinfo: parinfo array
;	opr: operation (see pfo_sysvar__define for tokens)
;	value: value to be added, multiplied, etc.
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: idx: the indices into parinfo for which the
; operations will be done
;
; OUTPUTS: parinfo is modified: tags value, error, limits, step,
; relstep, and mpmaxstep
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
; EXAMPLE: having found the indices into parinfo for a particular
; peak and the energy of the peak (Xaxis value) in the old and new
; gain coordinate systems, this line will transform the peak
; parameters (assuming that they are all proportional to energy) to
; the new coordinate system.
; pfo_opr, parinfo, !pfo.mult, newE/oldE, idx=peak1_idx
;
; MODIFICATION HISTORY:
;
; $Id: pfo_opr.pro,v 1.1 2011/01/03 21:51:56 jpmorgen Exp $
;
; $Log: pfo_opr.pro,v $
; Revision 1.1  2011/01/03 21:51:56  jpmorgen
; Initial revision
;
;-
pro pfo_opr, parinfo, opr, value, idx=idx
  if size(parinfo, /type) ne !tok.struct then $
    return
  case opr of
     !pfo.repl : begin ;; This doesn't make much sense unless value=0
        parinfo[idx].value = value
        parinfo[idx].error = value
        parinfo[idx].limits = value
        parinfo[idx].step = value
        parinfo[idx].relstep = value
        parinfo[idx].mpmaxstep = value
     end
     !pfo.mult : begin
        parinfo[idx].value = parinfo[idx].value*value
        parinfo[idx].error = parinfo[idx].error*value
        parinfo[idx].limits = parinfo[idx].limits*value
        parinfo[idx].step = parinfo[idx].step*value
        parinfo[idx].relstep = parinfo[idx].relstep*value
        parinfo[idx].mpmaxstep = parinfo[idx].mpmaxstep*value
     end
     !pfo.add : begin
        parinfo[idx].value = parinfo[idx].value+value
        parinfo[idx].error = parinfo[idx].error+value
        parinfo[idx].limits = parinfo[idx].limits+value
        parinfo[idx].step = parinfo[idx].step+value
        parinfo[idx].relstep = parinfo[idx].relstep+value
        parinfo[idx].mpmaxstep = parinfo[idx].mpmaxstep+value
     end
  endcase
end

