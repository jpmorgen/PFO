; +
; $Id: pfo_fmod.pro,v 1.1 2004/01/15 17:10:57 jpmorgen Exp $

; pfo_fmod.pro 

;; This routine is a primitive of pfo_fcreate that allows command-line
;; arguments to easily fill the pfo_struct-specific tags in the
;; pfo_parinfo structure.  See also pfo_struct__define.pro.  Once a
;; function is created, it is easier to modify it using the tags
;; directly. Extra keywords are passed on to mpfit_pfo_mod

;; WARNING: an entire parinfo array must be passed in order for the
;; values to be permanently changed (see IDL manual "passing by
;; reference" section).  Use the idx positional parameter to pick out
;; individual parinfo records.

; -

pro pfo_fmod, parinfo, idx, status=status, ID=ID, fseq=fseq, $
              inaxis=inaxis, outaxis=outaxis, fop=fop, ftype=ftype, $
              format=format, eformat=eformat, _EXTRA=extra

  ;; If we generate an error, return to the calling code.  THe problem
  ;; _must_ be there, since this code is perfect ;-)
;;  ON_ERROR, 2

  ;; Pathological case, avoids indgen(0) error
  n = N_elements(parinfo)
  if N_elements(parinfo) eq 0 then $
    return

  ;; Set up idx if none specified
  if N_elements(idx) eq 0 then $
    idx = indgen(n)

  ;; Work with the pfo part of parinfo.  We have to be careful not to
  ;; let IDL mess us up with a trivial dimension so don't use:
  ;; pfo = parinfo[idx].pfo
  pfo = make_array(N_elements(idx), value=parinfo[0].pfo)
  for i=0, N_elements(idx)-1 do $
     pfo[i] = parinfo[idx[i]].pfo

  ;; Step through the individual tags.

  if N_elements(status) ne 0 then begin
        struct_array_assign, pfo, tagname='status', tagval=status
  endif
  if N_elements(ID) ne 0 then begin
        struct_array_assign, pfo, tagname='ID', tagval=ID
  endif
  if N_elements(fseq) ne 0 then begin
        struct_array_assign, pfo, tagname='fseq', tagval=fseq
  endif
  if N_elements(inaxis) ne 0 then begin
        struct_array_assign, pfo, tagname='inaxis', tagval=inaxis
  endif
  if N_elements(outaxis) ne 0 then begin
        struct_array_assign, pfo, tagname='outaxis', tagval=outaxis
  endif
  if N_elements(fop) ne 0 then begin
        struct_array_assign, pfo, tagname='fop', tagval=fop
  endif
  if N_elements(ftype) ne 0 then begin
        struct_array_assign, pfo, tagname='ftype', tagval=ftype
  endif
  if N_elements(format) ne 0 then begin
        struct_array_assign, pfo, tagname='format', tagval=format
  endif
  if N_elements(eformat) ne 0 then begin
        struct_array_assign, pfo, tagname='eformat', tagval=eformat
  endif
  
  ;; Put the modified pfo structure back into parinfo.  Array brackets
  ;; are necessary to trick IDL into matching the types since the
  ;; trival array dimension gets added...
  for i=0, N_elements(idx)-1 do $
    parinfo[idx[i]].pfo = [pfo[i]]
  
  ;; Process the mpfit parinfo keywords
  if N_elements(extra) ne 0 then $
    mpfit_pfo_mod, parinfo, idx, _EXTRA=extra

end
