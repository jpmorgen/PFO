; +
; $Id: pfo_fcreate.pro,v 1.1 2003/12/11 20:41:02 jpmorgen Exp $

; pfo_fcreate.pro 

;; Create an array of blank parinfo records for the desired pfo
;; function.  Pass a parinfo template if you want something other than
;; the default pfo parinfo structure.  Could have also been called
;; pfo_parinfo_create.

; -

function pfo_fcreate, ftype, parinfo_template=parinfo_template, ID=ID, $
  inaxis=inaxis, outaxis=outaxis, fseq=fseq, fop=fop, $
  poly_order=poly_order, poly_num=poly_num, $
  poly_ref=poly_ref, value=value, format=format
                     
  init = {pfo_sysvar}
  ;; Defaults and sanity checks
  if NOT keyword_set(parinfo_template) then $
    parinfo_template = !pfo.parinfo
  junk = where(tag_names(parinfo_template) eq 'PFO', ispfo)
  if ispfo ne 1 then $
    message, 'ERROR: parinfo_template does not contain a PFO structure'

  ;; Default command line parameters
  if NOT keyword_set(ftype) then $
    ftype = 0
  if NOT keyword_set(inaxis) then $
    inaxis = !pfo.Xaxis
  if NOT keyword_set(outaxis) then $
    outaxis = !pfo.Yaxis
  if NOT keyword_set(fop) then $
    fop = !pfo.add

  ;; CURRENTLY DEFINED FUNCTIONS
  if ftype lt !pfo.null or ftype gt !pfo.last_fn then $
    message, 'ERROR: function type ' + string(ftype) + 'not recognized.  Use ftype=0 and declare something in your own structure if you are handling this function on your own, or modify this code to include your function.'

  ;; Define any function type decimal stuff.
  case ftype of
     !pfo.null	:	parinfo = parinfo_template
     !pfo.poly	:	begin
        if NOT keyword_set(poly_order) then poly_order=0
        if NOT keyword_set(poly_num) then poly_num=1
        if poly_num gt 9 then $
          message, 'ERROR: pfo polynomials can only have 9 segments'
        ;; First figure out how many parinfo records we need
        if N_elements(poly_ref) eq 0 then begin
           ;; Simple case, no reference values.  Remember to add one
           ;; record for 0th order :-)
           parinfo = replicate(parinfo_template, poly_order+1)
        endif else begin
           ;; Handle fancy segmented polynomial definitions.
           if N_elements(poly_order) eq 1 then begin
              ;; All the segments have the same polynomial order.  Add
              ;; in an extra parinfo element for each poly_ref
              parinfo = replicate(parinfo_template, $
                                  (N_elements(poly_ref) * (poly_order+2))
           endif else begin
              ;; Presumably a unique poly order for each segment
              if N_elements(poly_order) ne N_elements(poly_ref) then $
                message, 'ERROR: poly_order and poly_ref array lengths are not compatible'
              parinfo = replicate(parinfo_template, total(poly_order) + $
                                  2 * N_elements(poly_ref))
           endelse
        endelse

        ;; This doesn't restrict the reference numbers to be anything
        ;; in particular.  Check that when we actually use the polynomial.
        for coef_idx=0,N_elements(poly_ref)-1 do begin
           parinfo[coef_idx].pfo.ftype $
             = float(string('0.', strtrim(string(poly_ref[coef_idx]),2)))
           
        endfor
        for coef_idx=poly_ref,poly_order do begin
           parinfo[coef_idx].pfo.ftype $
             = float(string('0.', strtrim(string(poly_num),2), $
                            strtrim(string(coef_idx),2)))
        endfor
     end
     !pfo.deltafn	:	dvg = 1
     !pfo.gauss		:	dvg = 1
     !pfo.voigt		:	dvg = 1
  endcase

  ;; Check to see if we are a deltafn, Gaussian, or Voigt.  This is
  ;; very specific to how the numbers are arranged and therefore very
  ;; anti-tokenistic.  Shame on me.
  if keyword_set(dvg) then begin
     parinfo = replicate(parinfo_template, ftype)
     sub_idx = (indgen(ftype) + 1)/10.
     parinfo[*].pfo.ftype = sub_idx[*]
     ;; Initial values and formats
     parinfo[*].value = 0
     for i=1,ftype do begin
        case i of
           1 : parname = 'center'
           2 : parname = 'area'
           ;; Avoid negative widths.
           3 : begin
              parname = 'gauss width'
              parinfo[i-1].limited = [1,0]
              parinfo[i-1].limits = [0,0]
           end
           4 : begin
              parname = 'lor width'
              parinfo[i-1].limited = [1,0]
              parinfo[i-1].limits = [0,0]
           end
           else : message, 'ERROR: unrecognized parameter name/ftype'
        endcase
        parinfo[i-1].parname = parname
     endfor ;; assigning parname

  endif

  ;; Now add the main function type onto are decimal
  parinfo[*].pfo.ftype = parinfo[*].pfo.ftype + ftype

  ;; Set other fields
  parinfo[*].pfo.status = !pfo.active
  if keyword_set(ID) then $
    parinfo[*].pfo.ID = ID
  if keyword_set(inaxis) then $
    parinfo[*].pfo.inaxis = inaxis
  if keyword_set(outaxis) then $
    parinfo[*].pfo.outaxis = outaxis
  if keyword_set(fop) then $
    parinfo[*].pfo.fop = fop
  if keyword_set(format) then $
     parinfo[*].pfo.format = format
  
  ;; Copy in initial values, if the user specified them on the command
  ;; line.  Don't complain about mismatched numbers, since functions
  ;; should be initialized just fine with not enough parameters
  if keyword_set(value) then begin
     npfo = N_elements(parinfo)
     nval = N_elements(value)
     idx = indgen(min([npfo, nval]))
     struct_array_assign, parinfo, idx, tagname='value', tagval=value
  endif

  return, parinfo

end
