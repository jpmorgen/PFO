; +
; $Id: pfo_poly.pro,v 1.4 2015/03/03 21:27:02 jpmorgen Exp $

; pfo_poly.pro 

;; pfo primitive used to calculate a segmented polynomial.  Also
;; creates parinfo records and prints mpfit parameters

;; Segmented polynomial scheme

;; This is an attempt at a generic definition for a polynomial
;; function made up of segments useful for e.g. making piecewise fits
;; to data.  There are three types of information that need to be
;; conveyed: the boundary points between segments, the reference
;; values for the polynomial expansion of each segment (e.g. might be
;; the central X value of each segment) and the polynomial
;; coefficients themselves.  Follwing the decimal ftype model, the
;; boundary X values are marked with ftype=1.1, 1.2, etc.  Reference
;; pixels are indicated with ftype=1.01, 1.02, etc. and the
;; coefficients with 1.00001, 1.00002, etc.  Note that the polynomial
;; labeling therefore starts with 1 and can be no higher than 9.
;; Polynomial coefficients start with 0 and in principle could be as
;; large as desired, but since they are stored in a single precision
;; floating point variable, can be no more than 99.  Because I have to
;; round things to get the digits right, the 9th polynomial can't be
;; more than 4th order.  If you don't specify a 1.1 boundary value, it
;; is assumed to be the leftmost point on the X axis.  Similarly, if
;; you don't specify any reference values (1.0x), they are assumed to
;; be the leftmost point of that segment.  If you specify a reference
;; value (e.g. 1.01), then subsequently unspecified reference values
;; are set to that value.  This is meant to be convenient in the case
;; where you have one reference pixel about which all the polynomial
;; expansions are done.

;; Note that if you want more than one instance of pfo_poly (e.g. if
;; you want to use the infunct/outfunct strings on individual
;; segments), you will need to define separate pfo_poly functions and
;; use the pfo.pfoID tag to keep them from all running into one another.
;; Also note that by default, there is no right-hand bound on the
;; calculation of a pfo_poly segment.  If you need to to have more
;; than one instance of pfo_poly, start a second segment, 0th order,
;; with a value that does not contribute to the output axis.

;; For convenience in creating a complex pfo_poly, you can use the
;; poly_order, poly_num, poly_bound, poly_ref, and poly_value
;; keywords.  These keywords accept vectors which allow you to define
;; the whole pfo_poly at once.

;; Also for convenience in picking the pfo_poly apart, you can use the
;; /indices keyword and the indices of the poly_bound, poly_ref, and
;; coefficient value (poly_value) parinfo elements will be returned in
;; those keywords.  You can then pick out the value using something
;; like poly_refs = parinfo[poly_ref].value.  The poly_num keyword
;; returns the actual poly numbers (usually just a sequence starting
;; from 0) and the poly_order keyword returns an array of the
;; polynomial orders.

; -

function pfo_poly, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                   create=create, print=print, widget=widget, $
                   poly_order=poly_order, poly_num=poly_num, $
                   poly_bound=poly_bound, poly_ref=poly_ref, $
                   poly_value=poly_value, indices=indices, $
                   _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.poly

  ;; This section is specific to each PFO function definition
  if keyword_set(create) then begin
     ;; /CREATE

     ;; Check command-line arguments for sanity and assign defaults

     ;; Default poly_order is 0
     if N_elements(poly_order) eq 0 then poly_order=0
     bad_idx = where(poly_order lt 0, count)
     if count gt 0 then $
       message, 'ERROR: polynomial orders must be nonnegative'
     bad_idx = where(poly_order gt 99, count)
     if count gt 0 then $
       message, 'ERROR: unless you change the code so ftype is a double precision floating point, there really aren''t enough significan figures for more than 99 polynomial coefficients.'
     
     ;; Get poly_num in shape using the poly_bound array
     npoly = N_elements(poly_num)
     npb = N_elements(poly_bound)
     if npoly eq 0 then begin
        ;; We know we must have at least one polynomial.  
        poly_num = 1
        if npb gt 0 then begin
           ;; Here we have a little problem with degeneracy.  Are we
           ;; missing the first element of the poly_bound array or
           ;; not?  Assume not and have user set it to NAN if they
           ;; really don't want it.
           poly_num = 1 + lindgen(N_elements(poly_bound))
        endif ;; poly_bound array
     endif

     ;; Poly_nums must be > 0, since 1.0 = 1.00 and no more than 9 for
     ;; a similar reason.
     bad_idx = where(poly_num le 0 or poly_num gt 9, count)
     if count gt 0 then $
       message, 'ERROR: polynomial numbers can only range between 1 and 9'

     ;; We know for sure how many polynomials we are dealing with now
     npoly = N_elements(poly_num)

     ;; Check to make sure we have a boundary for each polynomial with
     ;; the possible exception of the first(/only)
     if npoly ne npb and npoly - 1 ne npb then $
       message, 'ERROR: poly_num and poly_bound are not consistent.  There must be one boundary for each polynomial, with the possible exception of the first polynomial.' 

     ;; Check that poly_ref makes sense too.  Here the specification
     ;; for segmented polynomials allows for a sparse array, but that
     ;; is hard to specify in this context.  Set things to NAN if you
     ;; don't want them specified at the point in time.
     nref = N_elements(poly_ref)
     if nref gt 1 and nref ne npoly then $
       message, 'ERROR: either specify one poly_ref for all the polynomials or one for each polynomial.  Use NAN as a placeholder.'

     ;; Make a local copy of poly_order that has one element for each
     ;; polynomial.  We might have been passed one, so check
     po = poly_order
     if N_elements(poly_order) eq 1 then $
       po = make_array(npoly, value=poly_order)
     if N_elements(po) ne npoly then $
       message, 'ERROR: poly_order not consistent with the number of polynomials'

     ;; Calculate number of parinfo records we need
     npar = npb + nref + total(po + 1)

     new_parinfo $
       = replicate(pfo_null(/create, parinfo=parinfo, _EXTRA=extra), npar)
     ;; pfo_null sets value=NAN, but we want the default coefficient
     ;; to be 0
     new_parinfo.value = 0
     
     ;; Fill in the new_parinfo structure segment by segment.  We will
     ;; need separate counters parinfo element, poly bound, and poly
     ;; number(/ reference)
     ipar = 0
     ipb = 0
     ;; Compensate for the fact that we can skip the first poly_bound
     if npb eq npoly - 1 or npb eq 0 then $
       ipb = ipb - 1
     for ipn=0, npoly-1 do begin
        pn = poly_num[ipn] ;; shorthand for polynomial number
        
        ;; POLY_BOUND.  Skip the first one if npb eq npoly-1.
        if ipb ge 0 then begin
           new_parinfo[ipar].pfo.ftype = pn/10.
           new_parinfo[ipar].value = poly_bound[ipb]
           new_parinfo[ipar].parname $
             = string(format='("boundary ", i1)', pn)
           ipar = ipar + 1
           ipb = ipb + 1
        endif
        ;; Dig ipb out of the hole if we actually have some poly_bounds
        if ipb lt 0 and npb gt 0 then $
          ipb = ipb + 1

        ;; POLY_REF.  If specified, add once or every time.
        if (nref eq 1 and ipn eq 0) or (nref eq npoly) then begin
           new_parinfo[ipar].pfo.ftype = pn/100.
           new_parinfo[ipar].value = poly_ref[ipn]
           new_parinfo[ipar].parname $
             = string(format='("ref ", i1)', pn)
           ipar = ipar + 1
        endif

        ;; Fill in ftype values for each coefficient.
        for ipc=0, po[ipn] do begin
           sval = '0.00' + strtrim(pn,2)
           if ipc lt 10 then $
             sval = sval + '0'
           sval = sval + strtrim(ipc,2)
           new_parinfo[ipar].pfo.ftype $
             = float(sval)
           new_parinfo[ipar].parname $
             = string(format='("seg", i1, " c", i2)', pn, ipc)
           ipar = ipar + 1
        endfor

     endfor ;; Each polynomial

     ;; Save ftypes for calculations below
     ftypes = new_parinfo.pfo.ftype

     ;; Don't forget to add on the fn value!
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + fn
     
     ;; Set the values of the polynomial coefficients (if specified)
     good_idx = where(finite(new_parinfo.value), ngood)
     nv = N_elements(poly_value)
     if nv eq 0 then $
       return, new_parinfo[good_idx]

     ;; It is possible the use really knows what is going on and
     ;; specified all the values, even the poly_bounds on the command
     ;; line
     if nv eq npar then begin
        new_parinfo.value = poly_value
     endif else begin
        if nv eq total(po + 1) then begin
           cidx =  where(round(ftypes * 1000.) le 1)
           new_parinfo[cidx].value = poly_value
        endif else begin
           message, 'ERROR: Incorrect number of elements in the poly_value keyword.  You either need one poly_value for every parameter (including boundaries and references) or one poly_value for each polynomial coefficient.  This polynomial has ' + strtrim(npoly, 2) + ' polynomial(s) described by ' + strtrim(npar, 2) + ' total parameters, ' + strtrim(total(poly_order + 1), 2) + ' of which seem to be poly coefs.'
        endelse
     endelse

     return, new_parinfo[good_idx]

  endif ;; /CREATE

  ;; CALCULATE, PRINT, or return INDICES

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar, any_status=widget)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)

  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  ;; FUNCTION SPECIFIC FTYPE HANDLING
  ;; Subtracting of fn induces a nasty rounding error, so convert
  ;; things into 10s, fix, divide by 10 and round.  p[br]nums will be
  ;; between 1 and 9 when they are valid for that particular parameter
  ;; type.
  ftypes = parinfo[f_idx].pfo.ftype - fn
  pbnums = fix(round(ftypes * 100.  )/10.) ;; Boundaries
  pbidx = where(0 lt pbnums and pbnums lt 10, npb)
  ;; unwrap
  if npb gt 0 then $
    pbidx = f_idx[pbidx]
  ;;prnums = floor(ftypes * 100. ) ;; Reference values
  prnums = fix(round(ftypes * 1000. )/10.) ;; Reference values
  ;; Coefficients are a little more complicated.  In this case, make
  ;; the integer portion the polynomial number and the decimal part
  ;; the coef index (e.g. 0,1,2...)
  cftypes = ftypes * 1000.
  ;; Make sure we don't get confused by poly_refs and poly_bounds
  rcftypes = round(cftypes)
  cidx = where(rcftypes lt 10)

  ;; Pick out the 0th order coefficients and get the polynomial
  ;; numbers from them.
  c0idx = where(round(100.*(cftypes[cidx] - rcftypes[cidx])) eq 0, npoly)
  
  if npoly eq 0 then $
    message, 'ERROR: no 0th order coefficients'

  pnums = round(cftypes[cidx[c0idx]])
  c0idx = f_idx[cidx[c0idx]]


  ;; Initialize output and a flag for calculating.  print_idx is handled using array_append.
  calculate = ~ (keyword_set(print) or keyword_set(widget) or keyword_set(indices))
  if keyword_set(calculate) then begin
     yaxis = pfo_null(Xin, _EXTRA=extra)
  endif
  if keyword_set(print) or keyword_set(widget) then $
    print_idx = !values.d_nan
  ;; I am experimenting with a new method which returns values.  This
  ;; is particularly useful for pfo_poly which is rather convoluted
  if keyword_set(indices) then begin
     ;; Initialize the arrays that I will return the values in.  They
     ;; will stacked up linearly, so the user will still have to do
     ;; some figuring to unpack them if the function is complicated
     poly_order='none'
     poly_num=pnums
     poly_bound='none'
     poly_ref='none'
     poly_value='none'
  endif

  ;; Step through the polynomials one at a time, handling the
  ;; boundaries and reference values.  This gets a little complicated
  ;; in the case of printing, since I often pass Xin=[0] and expect to
  ;; get all the values correctly.
  lower = min(Xin)
  delta = 1d
  nX = N_elements(Xin)
  if nX gt 1 then begin
     delta = median(Xin[1:nX-1]-Xin[0:nX-2])
  endif
  if npb gt 0 and nX eq 1 then begin
     lower = min(parinfo[pbidx].value)
  endif

  ref = lower

  for ipn=0, npoly-1 do begin
     
     ;; POLY_BOUNDS
     ;; The default in all cases is to have the upper bound just
     ;; beyond the end of Xin so we include last point.  But if we
     ;; have a next polynomial, its polybound is our upper.
     upper = max(Xin) + delta
     count = 0
     if ipn lt npoly-1 then $
       pbidx = where(pbnums eq pnums[ipn+1], count)
     if count gt 0 then begin
        ;; unwrap
        pbidx = f_idx[pbidx]
        upper = parinfo[pbidx[0]].value
      endif
     ;; Now for our lower boundary
     pbidx = where(pbnums eq pnums[ipn], count)
     if count gt 0 then begin
        ;; We have user-specified boundaries
        if count gt 1 then $
          message, 'ERROR: more than one poly_bound for polynomial ' + strtrim(pnums[ipn], 2)
        ;; unwrap
        pbidx = f_idx[pbidx]
        lower = params[pbidx]
        ;; Make sure we don't put lower beyond the end of our X-axis
        lower = lower < max(Xin)

        if keyword_set(print) or keyword_set(widget) then $
          print_idx = array_append(pbidx, print_idx)
        if keyword_set(indices) then $
          poly_bound = array_append(pbidx, poly_bound)
     endif ;; poly_bounds

     if keyword_set(calculate) and upper lt lower then $
       message, 'ERROR: poly_bounds are not in the right order'
     xidx = where(lower[0] le Xin and Xin lt upper[0], count)
     ;; Handle the case where we might have a non-monotonically
     ;; increasing Xin.  This is possibly the case if we have been
     ;; called to operate on the Y-axis
     if npb eq 0 then begin
        xidx = lindgen(N_elements(Xin))
     endif


     ;; Don't waste time if this polynomial segment is not in range
     if keyword_set(calculate) and count eq 0 then $
       CONTINUE

     ;; POLY_REFS
     pridx = where(prnums eq pnums[ipn], count)
     if count gt 0 then begin
        if count gt 1 then $
          message, 'ERROR: more than one poly_ref for polynomial ' + string(pnums[ipn])
        ;; unwrap
        pridx = f_idx[pridx]
        ;; Only modify ref if the parameter is not NAN
        if finite(params[pridx]) then begin
           ref = params[pridx]
           if keyword_set(print) or keyword_set(widget)then $
             print_idx = array_append(pridx, print_idx)
        if keyword_set(indices) then $
          poly_ref = array_append(pridx, poly_ref)

        endif ;; not NAN

     endif ;; poly_refs

     ;; COEFFICIENTS
     cidx = where(rcftypes eq pnums[ipn], order)
     order -= 1
     scidx = sort(cftypes[cidx])
     cidx = f_idx[cidx[scidx]]

     if keyword_set(print) or keyword_set(widget) then begin
        print_idx = array_append(cidx, print_idx)
     endif
     if keyword_set(indices) then begin
        poly_order = array_append(order, poly_order)
        poly_value = array_append(cidx, poly_value)
     endif
     if keyword_set(calculate) then begin
        ;; Finally the calculation 
        yaxis[xidx] = yaxis[xidx] + $
                      poly(Xin[xidx] - ref[0], params[cidx])
     endif

  endfor ;; each polynomial

  ;; Return values
  if keyword_set(indices) then $
    return, f_idx
  if keyword_set(print) or keyword_set(widget) then $
    return, pfo_null([0], params, parinfo=parinfo, $                  
                     idx=print_idx, print=print, widget=widget,$
                     _EXTRA=extra)

  return, yaxis

end
