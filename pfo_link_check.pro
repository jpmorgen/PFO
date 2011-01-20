;+
; NAME: pfo_link_check
;
; PURPOSE: check to see if the pfo_link system is in use and create
; the appropriate MPFIT tied relationships.  Also copies .values from
; master parameters into slaves so calls to pfo_funct before calls to
; pfo_fit end up calculating the function correctly
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
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
; $Id: pfo_link_check.pro,v 1.2 2011/01/20 23:03:55 jpmorgen Exp $
;
; $Log: pfo_link_check.pro,v $
; Revision 1.2  2011/01/20 23:03:55  jpmorgen
; Syncronize values in slaves, since we won't necessarily use this with MPFIT....
;
;-
pro pfo_link_check, parinfo
  ;; Check to see if we are using the pfo_link system
  tns = tag_names(parinfo)
  idx = where(tns eq 'PFO_LINK', count)
  if count eq 0 then $
    return

  ;; If we are, be jealous and clear away all other mention of tied
  parinfo.tied = ''  

  ;; Get slave indexes
  slave_idx = where(parinfo.pfo_link.status eq !pfo.slave, nslaves)

  ;; This loop gets skipped if there are no slaves.  Too bad if someone
  ;; sets to_ID and to_ftype and forgets to set status=!pfo.slave
  for is=0,nslaves-1 do begin
     ;; Get our to_ID and to_ftype from the slave
     to_ID = parinfo[slave_idx[is]].pfo_link.to_ID
     to_ftype = parinfo[slave_idx[is]].pfo_link.to_ftype
     ;; Make sure we can find our ID
     ID_idx = where(parinfo.pfo_link.ID eq to_ID, count)
     if count eq 0 then $
       message, 'ERROR: pfo_link.ID ' + strtrim(to_ID, 2) + ' not found'
     ;; Make sure we have just one parameter that we are trying to
     ;; point to
     to_ftype_idx = where(parinfo[ID_idx].pfo.ftype eq to_ftype, count)
     if count ne 1 then $
       message, 'ERROR: found ' + strtrim(count, 2) + ' parameters with ftype = pfo_link.to_ftype = ' + strtrim(to_ftype, 2) + ' Non-unique ID?'
     ;; unwrap
     to_ftype_idx = ID_idx[to_ftype_idx]
     ;; OK, here we are finally ready to make the MPFIT tied assigment
     parinfo[slave_idx[is]].tied = 'P[' + strtrim(to_ftype_idx, 2) + ']'
     ;; While we are here, synchronize the values for the benefit of
     ;; pfo_funct calls independent of mpfit or tnmin
     parinfo[slave_idx[is]].value = parinfo[to_ftype_idx].value
     parinfo[slave_idx[is]].error = !values.d_nan
     ;; For now leave the rest of the tags alone, since just
     ;; specifying tied results in the proper calculation of free
     ;; parameters in mpfit and tnmin.  It also gives the user an
     ;; additional cache of those tags.     
  endfor ;; Each slave

end


