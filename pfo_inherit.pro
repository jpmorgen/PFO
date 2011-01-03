;+
; NAME: pfo_inherit
;
; PURPOSE: Allows one pfo function to inherit values from another.
; Deals properly with the case where the functions have different
; numbers of parameters (e.g. pfo_polys of different order)
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_inherit, source, dest, source_idx=source_idx, dest_idx=dest_idx
;
; DESCRIPTION: Matches parameters by ftype to make the transfer of all
; information from the source parinfo to the destination parinfo.  Any
; additional parameters not in the source function are left
; initialized in whatever way they were created (e.g. you had better
; have consistent code that creates the function even if it has a
; different number of parameters).  This code is particularly useful
; for functions, such as pfo_poly, which do not have a set number of
; parameters.
;
; INPUTS: source -- source parinfo
;	dest -- destination parinfo.  This parinfo is modified
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;	source_idx -- optional indices into source
;	dest_idx -- optional indices into dest
;
; OUTPUTS:
;	dest is the output
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
; $Id: pfo_inherit.pro,v 1.1 2011/01/03 21:37:52 jpmorgen Exp $
;
; $Log: pfo_inherit.pro,v $
; Revision 1.1  2011/01/03 21:37:52  jpmorgen
; Initial revision
;
;-
pro pfo_inherit, source, dest, source_idx=source_idx, dest_idx=dest_idx
  init = {tok_sysvar}
  if !pfo.debug eq 0 then begin
     ;; Error probably makes more sense in calling routine
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_inherit, source, dest, source_idx=source_idx, dest_idx=dest_idx'
     endif
  endif ;; not debugging

  if N_elements(source) eq 0 then $
    message, 'ERROR: source parinfo required'

  if N_elements(dest) eq 0 then $
    message, 'ERROR: destination parinfo required'
  CATCH, /CANCEL
  ;; End of input checking

  if N_elements(source_idx) eq 0 then $
    source_idx = indgen(N_elements(source), $
                        type=size(/type, N_elements(source)))

  if N_elements(dest_idx) eq 0 then $
    dest_idx = indgen(N_elements(dest), $
                        type=size(/type, N_elements(dest)))

  for is=0, N_elements(source_idx)-1 do begin
     ftype = source[source_idx[is]].pfo.ftype
     didx = where(dest[dest_idx].pfo.ftype eq ftype, count)
     ;; Skip to the next source parameter if no match is found.  This
     ;; can continue through the whole source function in the case
     ;; that the functions aren't the same at all.
     if count eq 0 then $
       CONTINUE
     if count gt 1 then $
       message, 'ERROR: improperly formatted parinfo.  Has  ' + strtrim(count, 2) + ' ftype = ' + strtrim(ftype, 2) + ' elements.'
     ;; If we made it here, we have one and only one parameter.  Make
     ;; the assignemnt (remembering to unwrap).  Note that IDL will
     ;; raise an error if the structures don't match.  There is not
     ;; much we can do at this point if that is the case.
     dest[dest_idx[didx]] = source[source_idx]     
  endfor ;; each source

end
