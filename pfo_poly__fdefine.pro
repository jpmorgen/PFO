;+
; NAME: pfo_poly__fdefine
;
; PURPOSE: define, initialize and work with the PFO_POLY function
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE:
;
; DESCRIPTION:

; This code is organized into "methods," such as __fdefine, __init,
; __calc and __indices.  Note that some of the code is
; interdependent (e.g. __calc calls __indices, __fdefine is the
; first routine called), so the order these routines are listed in the
; file is important.  Brief documentation on each "method" is provided
; in the code.  The parameter/keyword lists are also annotated.

;; Segmented polynomial scheme

;; This is an attempt at a generic definition for a polynomial
;; function made up of segments useful for e.g. making piecewise fits
;; to data.  There are three types of information that need to be
;; conveyed: the boundary points between segments, the reference
;; values for the polynomial expansion of each segment (e.g. might be
;; the central X value of each segment) and the polynomial
;; coefficients themselves.  Follwing the decimal ftype model, the
;; boundary X values are marked with ftype=n.1, n.2, etc.  Reference
;; pixels are indicated with ftype=n.01, n.02, etc. and the
;; coefficients with n.00001, n.00002, etc.  Note that the polynomial
;; labeling therefore starts with 1 and can be no higher than 9.
;; Polynomial coefficients start with 0 and, since ftype is stored in
;; a single precision floating point variable, are limited in number.
;; Single-precision floats have a little more than 7 decimal digits of
;; precision
;; (http://en.wikipedia.org/wiki/Single_precision_floating-point_format).
;; As long as there are less than 100 functions registered in the
;; current instance of the pfo_finfo system, the pfo_poly coefs will
;; work properly.  NOTE: there can be more than 100 PFO functions
;; available, they just can't all be in use at once.  --> If this ever
;; becomes a problem, a simpler polynomial function will be needed.
;; Also note: Because "round" is used indiscriminately, the 9th
;; polynomial currently can't be more than 4th order.

;; If you don't specify a n.1 boundary value, it is assumed to be the
;; leftmost point on the Xin axis.  Similarly, if you don't specify
;; any reference values (n.0x), they are assumed to be the leftmost
;; point of that segment.  WANRING: if you don't always pass the same
;; Xin to your polynomial, this will produce unexpected results.  If
;; you specify a reference value (e.g. n.01), then unspecified
;; reference values in subsequent segments are set to that value.
;; This is meant to be convenient in the case where you have one
;; reference pixel about which all the polynomial expansions are done.

;; Note that if you want more than one instance of pfo_poly (e.g. if
;; you want to use the infunct/outfunct strings on individual
;; segments), you will need to define separate pfo_poly functions and
;; use the pfo.pfoID tag to keep them from all running into one
;; another.  This does not solve the coefficient precision problem
;; noted above.  Also note that by default, there is no right-hand
;; bound on the calculation of a pfo_poly segment.  If you want too
;; terminate the calculation of a polynomial segment, start a second
;; segment, 0th order, with a value that does not contribute to the
;; output axis.  Alternately, you use the pfo_ROI (Region of Interest)
;; feature, which ensures that functions are only calculated on the
;; ROI to which they are assigned.

;; For convenience in creating a complex pfo_poly, you can use the
;; poly_order, poly_num, poly_bound, poly_ref, and poly_value
;; keywords.  These keywords accept vectors which allow you to define
;; the whole pfo_poly at once and output values when using the
;; __indices method.

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
; $Id: pfo_poly__fdefine.pro,v 1.5 2011/11/21 15:23:51 jpmorgen Exp $
;
; $Log: pfo_poly__fdefine.pro,v $
; Revision 1.5  2011/11/21 15:23:51  jpmorgen
; Improve documentation
;
; Revision 1.4  2011/11/18 16:06:46  jpmorgen
; Add documentation from old pfo_poly, improve widget so that the order
; of a single polynomial can be changed
;
; Revision 1.3  2011/09/15 20:45:53  jpmorgen
; Fixed bug in order of function definitions
;
; Revision 1.2  2011/09/08 20:14:44  jpmorgen
; Added fname to pfo structure and fixed printing bug
;
; Revision 1.1  2011/09/01 22:05:38  jpmorgen
; Initial revision
;
;-

;; The __calc "method" returns the calculated value of the function.
;; In the case of pfo_poly, we can specify the /indices flag and get
;; indices back.  We do it this way, since that is how the code was
;; originally written.  Note that in the /indices case, the return
;; value is the set of indices parsed in the order that they are
;; interpreted, in other words, suitable for printing or widget display
function pfo_poly__calc, $
   Xin, $       ;; Input X axis in natural units 1
   params, $    ;; parameters (entire array)
   parinfo=parinfo, $ ;; parinfo array (whole array)
   idx=idx, $    ;; idx into parinfo of parinfo segment defining this function
   pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
   indices=indices, $ ;; flag to trigger indices calculation instead of regular calculation: should only be used by __indices method
   terminate_idx=terminate_idx, $ ;; append !tok.nowhere to each index variable after we are done
   poly_bound=poly_bound, $ ;; return value for __indices: left boundary value idx
   poly_ref=poly_ref, $ ;; return value for __indices: reference value idx
   poly_value=poly_value, $ ;; return value for __indices: coef idx
   poly_order=poly_order, $ ;; return value for __indices: polynomial orders **NOT IDX**
   poly_nums=poly_nums, $ ;; return value for __indices: polynomial numbers (tenths place in ftype) **NOT IDX**
   _REF_EXTRA=extra ;; passed to underlying routines

  ;; Use shared routines to do basic error checking.  These error
  ;; messages are pretty verbose.

  ;; Common error checking and initialization code
  pfo_calc_check, Xin, params, parinfo=parinfo, idx=idx, pfo_obj=pfo_obj

  ;; This is code that is mostly borrowed from the original pfo_poly
  ftypes = pfo_frac(parinfo[idx].pfo.ftype)
  pbnums = fix(round(ftypes * 100.  )/10.) ;; Boundaries
  pbidx = where(0 lt pbnums and pbnums lt 10, npb)
  ;; unwrap
  if npb gt 0 then $
     pbidx = idx[pbidx]
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

  poly_nums = round(cftypes[cidx[c0idx]])
  c0idx = idx[cidx[c0idx]]

  ;; Initialize output and a flag for calculating.  print_idx is handled using array_append.
  calculate = ~ keyword_set(indices)

  ;; Initialize our output axis to 0, same size and type as Xin
  if keyword_set(calculate) then begin
     yaxis = Xin * 0d
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
        pbidx = where(pbnums eq poly_nums[ipn+1], count)
     if count gt 0 then begin
        ;; unwrap
        pbidx = idx[pbidx]
        upper = parinfo[pbidx[0]].value
     endif
     ;; Now for our lower boundary
     pbidx = where(pbnums eq poly_nums[ipn], count)
     if count gt 0 then begin
        ;; We have user-specified boundaries
        if count gt 1 then $
           message, 'ERROR: more than one poly_bound for polynomial ' + strtrim(poly_nums[ipn], 2)
        ;; unwrap
        pbidx = idx[pbidx]
        lower = params[pbidx]
        ;; Make sure we don't put lower beyond the end of our X-axis
        lower = lower < max(Xin)

        if keyword_set(indices) then begin
           pfo_array_append, ret_idx, pbidx
           pfo_array_append, poly_bound, pbidx
        endif ;; indices
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
     pridx = where(prnums eq poly_nums[ipn], count)
     if count gt 0 then begin
        if count gt 1 then $
           message, 'ERROR: more than one poly_ref for polynomial ' + string(poly_nums[ipn])
        ;; unwrap
        pridx = idx[pridx]
        ;; Only modify ref if the parameter is not NAN
        if finite(params[pridx]) then begin
           ref = params[pridx]
        endif ;; not NAN
        ;; Always put poly_refs into indices
        if keyword_set(indices) then begin
           pfo_array_append, ret_idx, pridx
           pfo_array_append, poly_ref, pridx
        endif ;; indices

     endif ;; poly_refs

     ;; COEFFICIENTS
     cidx = where(rcftypes eq poly_nums[ipn], order)
     order -= 1
     scidx = sort(cftypes[cidx])
     cidx = idx[cidx[scidx]]

     if keyword_set(indices) then begin
        pfo_array_append, ret_idx, cidx
        pfo_array_append, poly_order, order
        pfo_array_append, poly_value, cidx
        ;; Figure if terminate_idx is set, the polynomials should be separated
        if keyword_set(terminate_idx) then $
           pfo_array_append, poly_value, !tok.nowhere
     endif
     if keyword_set(calculate) then begin
        ;; Finally the calculation 
        yaxis[xidx] = yaxis[xidx] + $
                      poly(Xin[xidx] - ref[0], params[cidx])
     endif

  endfor ;; each polynomial

  ;; Return values
  if keyword_set(indices) then begin
     if keyword_set(terminate_idx) then begin
        pfo_array_append, poly_order, !tok.nowhere
        pfo_array_append, poly_bound, !tok.nowhere
        pfo_array_append, poly_ref  , !tok.nowhere
        pfo_array_append, poly_value, !tok.nowhere
        pfo_array_append, poly_nums,  !tok.nowhere
     endif
     ;; pfo_parinfo_parse takes care of termination of the function indices
     return, ret_idx
  endif

  ;; Normal calculation return value is yaxis
  return, yaxis

end

;; The __indices "method" maps keyword names to the indices of those
;; parameters.  It is intended to be called ONLY for one instance of
;; the function at a time.  The pfo_funct /INDICES "method" can be
;; used to process multiple function instances.
function pfo_poly__indices, $
   parinfo, $    ;; parinfo containing function
   _REF_EXTRA=extra ;; soak up any extra parameters

  ;; Pass everything on to pfo_poly__calc
  return, pfo_poly__calc(/indices, 0, parinfo.value, parinfo=parinfo, _EXTRA=extra)

end

function pfo_poly__widget, $
   parentID, $ ;; Parent widget ID (positional parameter)
   idx=idx, $ ;; input indices of pfo_poly (not necessarily in proper order)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating parinfo
   edit=edit, $ ;; bring up all widgets rather than just text widget with parinfo printed into it
   _REF_EXTRA=extra ;; All other input parameters

  ;; Get the order of parameters from __indices
  ordered_idx = pfo_obj->parinfo_call_function( $
                /no_update, 'pfo_poly__indices', idx=idx, pfo_obj=pfo_obj, $
                poly_bound=poly_bound, $ ;; left boundary value
                poly_ref=poly_ref, $ ;; reference value
                poly_value=poly_value, $ ;; coefs
                poly_order=poly_order, $ ;; polynomial orders
                poly_nums=poly_nums, $ ;; poly numbers (10ths place of ftype)
                /terminate_idx, $ ;; append !tok.nowhere to each index variable after we are done
                _EXTRA=extra)

  ;; Create a container widget
  containerID = widget_base(parentID, /column)

  ;; Cycle through each segment.  We know each segment has a
  ;; poly_order, but don't forget that the poly_order array is
  ;; terminated with -1.
  for iseg=0,N_elements(poly_order)-2 do begin
     coef_idx = pfo_idx_parse(poly_value, iseg)
     ;; Display the polynomial coefs
     ID = pfo_null__widget(containerID, idx=coef_idx, $
                           edit=edit, pfo_obj=pfo_obj, _EXTRA=extra)
     ;; Display the polynomial order if we are in edit mode
     if keyword_set(edit) then $
        ID = pfo_poly_order_cw(parentID, order=poly_order[iseg], idx=coef_idx, $
                               pfo_obj=pfo_obj, _EXTRA=extra)
     
  endfor ;; each polynomial

  ;; Return the container widget ID
  return, containerID
  ;;return, pfo_null__widget(parentID, idx=ordered_idx, $
  ;;                         pfo_obj=pfo_obj, _EXTRA=extra)
  
end

function pfo_poly__print, $
   parinfo, $	  ;; parinfo array (whole array).  This ends up getting ignored in pfo_null__calc
   idx=idx, $     ;; idx into parinfo of our function
   _REF_EXTRA=extra ;; Extra keywords to pass on to *struct__print methods

  ;; Get the order of parameters from __indices
  ordered_idx = pfo_poly__indices(parinfo, idx=idx, _EXTRA=extra)

  return, pfo_null__print(parinfo, idx=idx, _EXTRA=extra)
  
end

;; Create the parinfo strand for this function and initialize it
function pfo_poly__init, $
   value=value, $	;; catch value to make sure no conflict with other keywords
   ftype=ftype, $	;; catch ftype to make sure no conflict in pfo_struct_setget_tag
   parinfo=eparinfo, $	;; pass existing parinfo to avoid collision of pfoIDs
   pfo_obj=pfo_obj, $	;; pfo_obj for parinfo_template
   _REF_EXTRA=extra, $	;; _REF_EXTRA passed to pfo_struct_setget_tag
   poly_bound=poly_bound, $ ;; [array of] left boundaries, one per polynomial
   poly_ref=poly_ref, $	;; reference value of polynomial (optional).  Minimum of input axis used it not specified
   poly_order=poly_order, $ ;; [array of] polynomial orders, one per polynomial
   poly_value=poly_value;; coefficient values or all of the above, if you really know what you are doing

  ;; Initialize parinfo in case early error.
  parinfo = !tok.nowhere

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning what I have so far.', /CONTINUE
        return, parinfo
     endif
  endif ;; not debugging

  ;; Catch improper use of value and ftype keywords
  if N_elements(value) + N_elements(ftype) ne 0 then $
     message, 'ERROR: value and ftype are set internally: they are therefore invalid keywords'

  ;; Get critical information about this function from pfo_finfo
  pfo_finfo, fname=pfo_fname(), fnum=fnum, fnpars=fnpars, pfo_obj=pfo_obj

  ;; Check command-line arguments for sanity and assign defaults

  ;; Default poly_order is 0
  if N_elements(poly_order) eq 0 then $
     poly_order=0
  bad_idx = where(poly_order lt 0, count)
  if count gt 0 then $
     message, 'ERROR: polynomial orders must be nonnegative'
  bad_idx = where(poly_order gt 99, count)
  if count gt 0 then $
     message, 'ERROR: unless you change the code so ftype is a double precision floating point, there really aren''t enough significant figures for more than 99 polynomial coefficients.'

  ;; Get poly_num in shape using the poly_bound array
  npoly = N_elements(poly_num)
  npb = N_elements(poly_bound)
  if npoly eq 0 then begin
     ;; We know we must have at least one polynomial.  
     poly_num = 1
     if npb gt 0 then begin
        ;; Here we have a little problem with degeneracy.  Are we
        ;; missing the first element of the poly_bound array or not?
        ;; Assume not and have user set it to NAN if they really don't
        ;; want to specify it (e.g. just have the first polynomial
        ;; start at the minimum Xin value)
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

  ;; Create our parinfo strand for this function, making sure to
  ;; include any substructure we need with required tags.  Start with
  ;; one parinfo element.
  parinfo = pfo_parinfo_template(pfo_obj=pfo_obj, $
                                 required_tags='pfo')
  ;; ... replicate it by npar
  parinfo = replicate(temporary(parinfo), npar)

  ;; Set attributes/defaults that are unique to this function.  Note
  ;; that some attributes are already defined in pfo_struct__init
  ;; (found in pfo_struct__define.pro)

  ;; FNAME 
  ;; It is inefficient to use 'where' on strings, so FTYPE is used
  ;; instead (see below).  fname should only be used by
  ;; pfo_struct__update to synchronize ftypes between parinfos created
  ;; at different times.
  parinfo.pfo.fname = pfo_fname()

  ;; By default, parinfo.value = NAN, but we want the coefs to start
  ;; out at 0, unless specified.
  parinfo.value = 0

  ;; Fill in the parinfo structure segment by segment.  We will
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
        parinfo[ipar].pfo.ftype = pn/10.
        parinfo[ipar].value = poly_bound[ipb]
        parinfo[ipar].parname $
           = string(format='("boundary ", i1)', pn)
        ;; Default to fixed boundaries.  Make the parameters
        ;; "permanently" fixed so that a casual call to pfo_mode,
        ;; parinfo, 'free' doesn't free them
        pfo_mode, parinfo[ipar], 'fixed', /permanent
        ipar = ipar + 1
        ipb = ipb + 1
     endif
     ;; Dig ipb out of the hole if we actually have some poly_bounds
     if ipb lt 0 and npb gt 0 then $
        ipb = ipb + 1

     ;; POLY_REF.  If specified, add once or every time.
     if (nref eq 1 and ipn eq 0) or (nref eq npoly) then begin
        parinfo[ipar].pfo.ftype = pn/100.
        parinfo[ipar].value = poly_ref[ipn]
        parinfo[ipar].parname $
           = string(format='("ref ", i1)', pn)
        ;; Default to fixed refs.  Make the parameters "permanently"
        ;; fixed so that a casual call to pfo_mode, parinfo, 'free'
        ;; doesn't free them
        pfo_mode, parinfo[ipar], 'fixed', /permanent
        ipar = ipar + 1
     endif

     ;; Fill in ftype values for each coefficient.
     for ipc=0, po[ipn] do begin
        sval = '0.00' + strtrim(pn,2)
        if ipc lt 10 then $
           sval = sval + '0'
        sval = sval + strtrim(ipc,2)
        parinfo[ipar].pfo.ftype $
           = float(sval)
        parinfo[ipar].parname $
           = string(format='("seg", i1, " c", i2)', pn, ipc)
        ipar = ipar + 1
     endfor

  endfor ;; Each polynomial

  ;; Get ready to get rid of user-specified leading NAN for a poly_ref
  ;; placeholder.  Don't get rid of if just yet, since we might
  ;; be using it when we assign values
  good_idx = where(finite(parinfo.value), ngood)

  ;; FTYPE -- save fractional ftypes for calculations below
  ftypes = parinfo.pfo.ftype

  ;; FTYPE -- integer part (fnum).  Dynamically assigned by fdefine
  parinfo.pfo.ftype += fnum
  
  ;; Set the values of the polynomial coefficients (if specified).
  case N_elements(poly_value) of
     0: ;; no values specified, leave them all at defaults
     npar: begin
        ;; It is possible the use really knows what is going on and
        ;; specified all the values, even the poly_bounds on the
        ;; command line
        parinfo.value = poly_value
     end
     total(po + 1): begin
        ;; Normal case of one value per coef
        cidx =  where(round(ftypes * 1000.) le 1)
        parinfo[cidx].value = poly_value
     end
     else: message, 'ERROR: Incorrect number of elements in the poly_value keyword.  You either need one poly_value for every parameter (including boundaries and references) or one poly_value for each polynomial coefficient.  This polynomial has ' + strtrim(npoly, 2) + ' polynomial(s) described by ' + strtrim(npar, 2) + ' total parameters, ' + strtrim(total(poly_order + 1), 2) + ' of which seem to be poly coefs.'
  endcase 

  ;; Now we can get rid of user-specified leading NAN for a poly_ref
  ;; placeholder.
  parinfo = parinfo[good_idx]

  ;; INAXIS and OUTAXIS.  Leave these at the default X, Y, though
  ;; people might want to make then Xin, X, if they are doing a
  ;; gain/dispersion.

  ;; FOP: Polynomials are generally used for backgrounds or
  ;; dispersions/gains, so make them default to replacing the output
  ;; axis
  parinfo.pfo.fop = !pfo.repl

  ;; pfoID

  ;; pfo_polys have an unknown number of parameters, so it is not
  ;; possible to separate out multiple instances of the function
  ;; without some kind of unique identifier.  pfoID provides this
  ;; identifier.  Make sure we choose a unique one if we are given the
  ;; opportunity to.
  epoly_idx = pfo_fidx(eparinfo, pfo_fname(), status_mask=!pfo.all_status, $
                       pfo_obj=pfo_obj, npar=enpar)
  if enpar gt 0 then $ 
     parinfo.pfo.pfoID = max(eparinfo[epoly_idx].pfo.pfoID) + 1
     



  ;; Convert keywords on the command line to tag assignments in the
  ;; parinfo.  This is a little risky, since we might have duplicate
  ;; tags in the different sub-structures (e.g. status is a popular
  ;; one) and, depending on what order the substructures were added
  ;; with required_tags, the behavior might vary.  It is always safer
  ;; to make assignments in your calling code explicitly with the tags
  ;; in the returned parinfo.  Note, we pass all of the _EXTRA to
  ;; pfo_struct_setget_tag, so the calling routine gets to choose
  ;; parallel or series, strict, etc.
  pfo_struct_setget_tag, /set, parinfo, _EXTRA=extra

  return, parinfo

end

;; Map function name to function number and number of parameters in
;; the PFO system
pro pfo_poly__fdefine, pfo_obj=pfo_obj

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_fdefine, pfo_obj=pfo_obj, fname=pfo_fname(), fnpars=0, $
               fdescr='Polynomial function.  '
  
end
