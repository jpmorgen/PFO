; +
; $Id: pfo_poly.pro,v 1.1 2003/12/19 00:02:05 jpmorgen Exp $

; pfo_voigt.pro 

;; pfo primitive used to calculate a segmented polynomial.  Also
;; creates parinfo records and prints mpfit parameters

; -

function pfo_poly, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                   create=create, print=print, $
                   poly_order=poly_order, poly_num=poly_num, $
                   poly_ref=poly_ref, _EXTRA=extra
  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.poly

  ;; This section is specific to each PFO function definition
  if keyword_set(create) then begin
     ;; /CREATE

     ;; Default poly_order it 0
     if NOT keyword_set(poly_order) then poly_order=0
     ;; If we are explicitly specifying multiple polynomials, we had
     ;; better have references for each, or else we won't know where
     ;; they begin and end
     if N_elements(poly_num) gt 1 then begin
        if N_elements(poly_num) ne N_elements(poly_ref) then $
          message, 'ERROR: poly_num and poly_ref need to have the same number of elements.  See documentation on segmented polynomials'
     endif
     ;; Handle cases where poly_num was not specified explicitly.  My
     ;; peculiar coding style to avoid elses and undo nesting
     if N_elements(poly_num) eq 0 then begin
        poly_num = 1
        if N_elements(poly_ref) gt 0 then $
          poly_num = indgen(N_elements(poly_ref)) + poly_num
     endif

     ;; Make a local copy of poly_order that has one element for each
     ;; polynomial.  po gets changed below if we specified just one
     ;; poly_order for all the polynomials
     po = poly_order

     ;; Figure out how many parinfo records we need
     if N_elements(poly_ref) eq 0 then begin
        ;; Simple case, no reference values.  Remember to add one
        ;; record for 0th order :-)
        npfo = poly_order+1
     endif else begin
        ;; Handle fancy segmented polynomial definitions.
        if N_elements(poly_order) eq 1 then begin
           ;; All the segments have the same polynomial order.  Add
           ;; one record for 0th order and an extra for each poly_ref
           npfo = N_elements(poly_ref) * (poly_order+2)
           po = make_array(N_elements(poly_ref), value=poly_order)
        endif else begin
           ;; Presumably a unique poly order for each segment
           if N_elements(poly_order) ne N_elements(poly_ref) then $
             message, 'ERROR: poly_order and poly_ref array lengths are not compatible'
           ;; Add up all poly_orders in the array and add in 0th order
           ;; and poly_ref records for each
           npfo = total(poly_order) + 2 * N_elements(poly_ref)
        endelse
     endelse
     new_parinfo $
       = replicate(pfo_null(/create, parinfo=parinfo, _EXTRA=extra), npfo)

     ;; We know for sure how many polynomials we need to deal with
     npoly = N_elements(po)

     ;; Fill in the new_parinfo structure
     ;; Default coefficient
     new_parinfo.value = 0
     ipfo = 0
     for ipn=0, npoly-1 do begin
        if poly_num[ipn] lt 1 or poly_num[ipn] gt 9 then $
          message, 'ERROR: poly_num ' + string(ipn) + ' value of ' + string(poly_num[ipn]) + ' is out of range.  1-9 are valid.'
        if po[ipn] lt 0 then $
          message, 'ERROR: poly_order ' + string(ipn) + ' value of ' + string(po[ipn]) + ' is out of range.  Must be >= 0.'

        if N_elements(poly_ref) ne 0 then begin
           ;; Put in the reference value.  Ftype = 1.0x
           new_parinfo[ipfo].pfo.ftype = poly_num[ipn]/100.
           new_parinfo[ipfo].value = poly_ref[ipn]
           new_parinfo[ipfo].parname $
             = string(format='("polyref ", i1)', poly_num[ipn])
           ipfo = ipfo + 1
        endif

        ;; Fill in ftype values for each coefficient.  Ftype = 1.xc
        for ipc=0, po[ipn] do begin
           new_parinfo[ipfo].pfo.ftype $
             = float(string('0.', strtrim(poly_num[ipn],2), $
                            strtrim(ipc,2)))
           ;; Assume that ninth order polynomial is the most we'll
           ;; need for pretty printing :-)
           new_parinfo[ipfo].parname $
             = string(format='("poly", i1, " c", i1)', poly_num[ipn], ipc)
           ipfo = ipfo + 1
        endfor
     endfor ;; Each polynomial

     ;; Don't forget to add on the fn value!
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn

     return, new_parinfo

  endif ;; /CREATE

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  ;; FUNCTION SPECIFIC FTYPE HANDLING
  ;; we can sort the polynomial coefficients too, though they end up
  ;; in a slightly different order than we created them in
  sidx = sort(parinfo[f_idx].pfo.ftype)
  ;; Make an easy handle for the parameters
  pidx = f_idx[sidx]

  ;; Find the poly_refs and set up for output
  pridx = where(parinfo[pidx].pfo.ftype lt fn + 0.1, npoly)
  if keyword_set(print) then $
    toprint = '' $
  else $
    yaxis = pfo_null(Xin, _EXTRA=extra)

  ;; Handle case of no poly_refs
  if npoly eq 0 then begin
     if keyword_set(print) then $
       return, pfo_null([0], params[pidx], parinfo=parinfo[pidx], $
                        print=print, _EXTRA=extra)
     return, poly(Xin, params)
  endif ;; a single polynomial with no reference value

  ;; If we made it here, we have a segmented polynomial.  Get
  ;; poly_refs in an easy to use form
  pridx = pidx[pridx]
  poly_refs = params[pridx]
  ;; Have experienced some wierd numeric errors, so push things just a
  ;; little.  Hopefully I have left enough room for polynomial coefs
  ;; :-)
  poly_num = round((parinfo[pridx].pfo.ftype - fn) * 100.)
  for ipn=0, npoly-1 do begin
     cidx = where(fix((parinfo.pfo.ftype - fn + 1E-6) * 10) eq $
                  poly_num[ipn], count)
     if count eq 0 then $
       message, 'ERROR: poly ref specified with no coefficients.  Use idx=pfo_poly([0], parinfo=parinfo, get_poly=' + string(poly_num[ipn]) + ') &  parinfo[idx].pfo.used = 0 to mark ALL parameters for this polynomial dead.'

     ;; PRINTING
     if keyword_set(print) then begin
        ;; A little bit of a hack, since I would like to handle all
        ;; printing with pfo_null, but the best I can do for now.
        if size(print, /type) ne 7 then $
          if print lt !pfo.pmp and ipn ne 0 then $
          toprint = toprint + '; '
        
        toprint $
          = toprint + $
          pfo_null([0], [params[pridx[ipn]], params[cidx]], $
                   parinfo=[parinfo[pridx[ipn]], parinfo[cidx]], $
                   print=print, _EXTRA=extra)
        CONTINUE        
     endif

     ;; CALCULATING
     lower = max([Xin[0], poly_refs[ipn]])
     if ipn eq npoly-1 then $
       upper = max(Xin)+1 $
     else $
       upper = min([Xin[N_elements(Xin)-1], poly_refs[ipn+1]])
     xidx = where(lower le Xin and Xin lt upper, count)
     ;; Check to see if this polynomial segment is in range at the moment.
     if count eq 0 then CONTINUE
     yaxis[xidx] = yaxis[xidx] + poly(Xin[xidx], params[cidx])

     
  endfor ;; each polynomial

  ;; Return the proper thing
  if keyword_set(print) then $
    return, toprint

  return, yaxis

end
