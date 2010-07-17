;+
; NAME: pfo_link_struct__define
;
; PURPOSE: Define the parinfo.pfo.link substructure that holds
; information on linking parameters together.
;
; CATEGORY: PFO, optional add-in
;
; CALLING SEQUENCE: 
;  pfo_parinfo__define, parinfo=pfo_parinfo
;  pfo_link = {pfo_link : {pfo_link_struct}}
;  pfo_parinfo = struct_append(pfo_parinfo, pfo_link)
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
; $Id: pfo_link_struct__define.pro,v 1.1 2010/07/17 18:59:55 jpmorgen Exp $
;-

pro pfo_link_struct__define
  pfo_link_struct $
    = {pfo_link_struct, $
       ID	: 0, $ ;; Set this in the master function, one ID per function, the same ID for all the parameters, even for the ones that don't participate
       status	: 0, $ ;; See pfo_sysvar__define tokens.  It is not an error to have a slave point to a slave.  This is more a tag for convenience.
       to_ID	: 0, $ ;; set these in the slave functions
       to_ftype	: 0. $ ;; to_ftype cannot be 0 in the slave
      } 
end
