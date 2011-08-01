;+
; NAME: pfo_calc_obj
;
; PURPOSE: Create, initialize, and work with the pfo_calc_obj, which
; encapsulates the necessary data and methods to fit fuctions to data
; in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
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
; $Id: pfo_calc_obj__define.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_calc_obj__define.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; Invalidate cached values frees our heap variables so that we can
;; allocate them anew as undefined variables

pro pfo_calc_obj::invalidate_cache
  ptr_free, self.pXaxis
  ptr_free, self.pYaxis
  ptr_free, self.pdeviates
  ptr_free, self.pdXaxis_dXin
  self.pXaxis = ptr_new(/allocate_heap)
  self.pYaxis = ptr_new(/allocate_heap)
  self.pdeviates = ptr_new(/allocate_heap)
  self.pdXaxis_dXin = ptr_new(/allocate_heap)
end

;; Check arguments to self->calc that would invalidate our cache of
;; quantities calculated from parinfo
function pfo_calc_obj::parinfo_cache_ok, $
   Xin=Xin, $ ;; could be different than encapsulated Xin
   params=params, $ ;; could be different than parinfo.value
   idx=idx, $ ;; could be a subset of parinfo.  If N_elements of idx and parinfo are the same, assume it is safe to cache
   xaxis=xaxis, $ ;; return parameter we know won't affect our cache.  "Soak it up" so it doesn't spill into _EXTRA, just in case someone passed it as a defined value
   ROI_Xin_idx=ROI_Xin_idx, $ ;; return parameter we know won't affect our cache, like xaxis
   _EXTRA=extra ;; any extra parameters may change the way the function is calculated, so it is not safe to cache

  return, (N_elements(Xin) + N_elements(params) + N_elements(extra) eq 0) $
          and (N_elements(idx) eq 0 or (N_elements(idx) eq N_elements(*self.pparinfo)))

end

;; Return the Yaxis *CALCULATED* from the parinfo function
;; encapsulated in this object.  If no Xin is specified, the Xin
;; encapsulated in the object is used.  If the calculation is done
;; directly from encapsulated property, Yaxis, Xaxis, and Xin_ROI_idx
;; are cached.  This is handy for widget display.
function pfo_calc_obj::Yaxis, $
   Xin=Xin, $           ;; if not specified, the Xin stored in the pfo_data_obj will be used
   xaxis=xaxis, $       ;; optional return parameter we get for free from pfo_parinfo_parse, will be cached if caching Yaxis
   ROI_Xin_idx=ROI_Xin_idx, $ ;; optional return parameter we get for free from pfo_parinfo_parse, will be cached if caching Yaxis
   no_copy=no_copy, $   ;; Moves Yaxis value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache
  if N_elements(*self.pYaxis) ne 0 and $
     self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pYaxis)
     endif ;; no_copy
     ;; Normal case of copying Yaxis to return value
     return, *self.pYaxis
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ;; Call pfo_parinfo_parse in such a way as to minimize the number of
  ;; copies of Xin.  Also grab all of the other quantities that are
  ;; calculated "for free."  This only costs a little extra space for
  ;; ROI_Xin_idx, which is only build up in pfo_parinfo_parse when
  ;; requested.
  if N_elements(Xin) eq 0 then begin
     Yaxis = pfo_parinfo_parse(/calc, *self.pparinfo, Xin=*self.pXin, $
                               xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, pfo_obj=self, _EXTRA=extra)
  endif else begin
     Yaxis = pfo_parinfo_parse(/calc, *self.pparinfo, Xin=Xin, $
                               xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, pfo_obj=self, _EXTRA=extra)
  endelse ;; user-supplied Xin or not

  ;; Handle our cache.  Note that we only cache quantities that have
  ;; been calculated so far.  Other quantities derived from these
  ;; (e.g. deviates, dXaxis_dXin) are cached when the user first asks
  ;; for them.  Xaxis and ROI_Xin_idx are also return quantities
  if NOT keyword_set(no_cache) and NOT keyword_set(no_copy) $
     and self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     *self.pXaxis = Xaxis
     *self.pYaxis = Yaxis
     *self.pROI_Xin_idx = ROI_Xin_idx
  endif ;; caching

  return, Yaxis

end

;; Return the Xaxis *CALCULATED* from the parinfo function
;; encapsulated in this object.  If no Xin is specified, the Xin
;; encapsulated in the object is used.  If the calculation is done
;; directly from encapsulated property, Xaxis is cached.  This is
;; handy for widget display.
function pfo_calc_obj::Xaxis, $
   Xin=Xin, $           ;; if not specified, the Xin stored in the pfo_data_obj will be used
   no_copy=no_copy, $   ;; Moves Xaxis value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache
  if N_elements(*self.pXaxis) ne 0 and $
     self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pXaxis)
     endif ;; no_copy
     ;; Normal case of copying Xaxis to return value
     return, *self.pXaxis
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ;; Call pfo_Xaxis in such a way as to minimize the number of
  ;; copies of Xin.
  if N_elements(Xin) eq 0 then begin
     Xaxis = pfo_Xaxis(*self.pparinfo, Xin=*self.pXin, pfo_obj=self, _EXTRA=extra)
  endif else begin
     Xaxis = pfo_Xaxis(*self.pparinfo, Xin=Xin, pfo_obj=self, _EXTRA=extra)
  endelse ;; user-supplied Xin or not

  ;; Handle our cache.  
  if NOT keyword_set(no_cache) and NOT keyword_set(no_copy) $
     and self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     *self.pXaxis = Xaxis
  endif ;; caching

  return, Xaxis

end

;; Return the ROI_Xin_idx *CALCULATED* from the parinfo function
;; encapsulated in this object.  If no Xin is specified, the Xin
;; encapsulated in the object is used.  If the calculation is done
;; directly from encapsulated property, ROI_Xin_idx is cached.  This is
;; handy for widget display.
function pfo_calc_obj::ROI_Xin_idx, $
   Xin=Xin, $           ;; if not specified, the Xin stored in the pfo_data_obj will be used
   no_copy=no_copy, $   ;; Moves ROI_Xin_idx value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache
  if N_elements(*self.pROI_Xin_idx) ne 0 and $
     self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pROI_Xin_idx)
     endif ;; no_copy
     ;; Normal case of copying ROI_Xin_idx to return value
     return, *self.pROI_Xin_idx
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ;; Call pfo_ROI_Xin_idx in such a way as to minimize the number of
  ;; copies of Xin.
  if N_elements(Xin) eq 0 then begin
     ROI_Xin_idx = pfo_ROI_Xin_idx(*self.pparinfo, Xin=*self.pXin, pfo_obj=self, _EXTRA=extra)
  endif else begin
     ROI_Xin_idx = pfo_ROI_Xin_idx(*self.pparinfo, Xin=Xin, pfo_obj=self, _EXTRA=extra)
  endelse ;; user-supplied Xin or not

  ;; Handle our cache.
  if NOT keyword_set(no_cache) and NOT keyword_set(no_copy) $
     and self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     *self.pROI_Xin_idx = ROI_Xin_idx
  endif ;; caching

  return, ROI_Xin_idx

end

;; The deviates are the weighted residuals between the function
;; described by parinfo and the data encapsulated in Yin.  This
;; function calculates the deviates.  If the calculation is done
;; directly from encapsulated property, the result is cached for fast
;; access.  NOTE: by default, NaN and Infinity points are excised from
;; the deviates before they are returned.  This confuses plotting
;; routines which are syncronized with the Xaxis
function pfo_calc_obj::deviates, $
   no_copy=no_copy, $ ;; Moves deviates value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   hide_infinity=hide_infinity, $ ;; Hide instances of infinity from the deviates (changes length of vector!)
   hide_NaN=hide_NaN, $ ;; Hide instances of NaN from the deviates (changes length of vector!)
  _REF_EXTRA=extra
  
  ;; Check to see if we can work from a cache.  This requires no input
  ;; parameters and a valid cached deviates
  if N_elements(*self.pdeviates) ne 0 and $
     self->parinfo_cache_ok(_EXTRA=extra) then begin
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        ;; This quick one-time transfer invalidates our cache
        return, temporary(*self.pdeviates)
     endif ;; no_copy
     ;; Normal case of copying deviates
     return, *self.pdeviates
  endif ;; working from cache

  ;; If we made it here, we need to calculate the deviates.
  ;; Handle the different cases of deviates a la MPFITFUN

  ;; Start with the case that we have no Yerr.  The best deviate we
  ;; can muster in this case is just the residual, with equal
  ;; weighting to everything
  deviates = *self.pYin - self->Yaxis(_EXTRA=extra)

  ;; If we have weights, use them
  if N_elements(*self.pweights) gt 0 then begin
     deviates = temporary(deviates) * *self.pweights
  endif else begin
     ;; We have no weights, check to see if we have Yerr
     if N_elements(*self.pYerr) gt 0 then begin
        deviates = temporary(deviates) / *self.pYerr
     endif ;; Calculating deviates with Yerr
  endelse ;; Deviates with weights or Yerr 

  ;; Get rid of infinities (default)
  if keyword_set(hide_infinity) then begin
     good_idx = where(finite(deviates, /infinity) eq 0, count)
     if count eq 0 then $
        message, 'ERROR: no good points to fit'
     ;; unwrap
     deviates = deviates[good_idx]
  endif
  ;; Get rid of NaNs (default)
  if keyword_set(hide_NaN) then begin
     good_idx = where(finite(deviates, /NAN) eq 0, count)
     if count eq 0 then $
        message, 'ERROR: no good points to fit'
     ;; unwrap
     deviates = deviates[good_idx]
  endif

  ;; Now handle our cache
  if keyword_set(no_copy) + keyword_set(no_cache) + keyword_set(hide_infinity) + keyword_set(hide_NAN) ne 0 $
     or NOT self->parinfo_cache_ok(_EXTRA=extra) then begin
     ;; Not caching
     return, deviates
  endif ;; no_copy
  
  ;; If we made it here, we are caching.  Copy our calculated deviates
  ;; into the cache
  *self.pdeviates = deviates
  return, deviates

end

;; dXaxis/dXin allows Yaxis to read in Xaxis units
function pfo_calc_obj::dXaxis_dXin, $
   Xin=Xin, $         ;; if not specified, the Xin stored in the pfo_data_obj will be used
   no_copy=no_copy, $ ;; Moves dXaxis_dXin value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $    ;; Equivalent meaning to no_copy
  _REF_EXTRA=extra
  
  ;; Check to see if we can work from a cache.  This requires no input
  ;; parameters and a valid cached deviates
  if N_elements(*self.pdXaxis_dXin) ne 0 and $
     self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        ;; This quick one-time transfer invalidates our cache
        return, temporary(*self.pdXaxis_dXin)
     endif ;; no_copy
     ;; Normal case of copying dXaxis_dXin
     return, *self.pdXaxis_dXin
  endif ;; working from cache
  
  ;; If we made it here, we need to calculate dXaxis/dXin.  Catch any
  ;; errors
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Not calculating derivative, returning 1', /CONTINUE
        return, 1d
     endif
  endif ;; not debugging

  ;; Do calculations in such a way as to minimize the number of copies
  ;; of Xin.
  if N_elements(Xin) eq 0 then begin
     dXaxis_dXin = deriv(*self.pXin, self->Xaxis(_EXTRA=extra))
  endif else begin
     dXaxis_dXin = deriv(Xin, self->Xaxis(Xin=Xin, _EXTRA=extra))
  endelse ;; user-supplied Xin or not

  ;; Now handle our cache
  if keyword_set(no_copy) or keyword_set(no_cache) or $
     NOT self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     ;; Not caching
     return, dXaxis_dXin
  endif ;; no_copy
  
  ;; If we made it here, we are caching.  Copy our calculated dXaxis_dXin
  ;; into the cache
  *self.pdXaxis_dXin = dXaxis_dXin
  return, dXaxis_dXin

end

;; If someone runs a procedure on parinfo, assume they have changed
;; it, unless not_modified flag is set
pro pfo_calc_obj::parinfo_call_procedure, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   not_modified=not_modified, $	;; default is to assume parinfo was modified
   _REF_EXTRA=extra

  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _EXTRA=extra

  ;; Working out the double negatives, this basically means we
  ;; invalidate our Yaxis cache unless the user sets /not_modified
  if NOT keyword_set(not_modified) then $
     self->invalidate_cache
 
end

function pfo_calc_obj::parinfo_call_function, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   not_modified=not_modified, $	;; default is to assume parinfo was modified
   _REF_EXTRA=extra

  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _EXTRA=extra)

  ;; Working out the double negatives, this basically means we
  ;; invalidate our Yaxis cache unless the user sets /not_modified
  if NOT keyword_set(not_modified) then $
     self->invalidate_cache
 
end

;; Except for yaxis, which is derived here from the combination of the
;; data (Xin) and function (parinfo), just pass get property onto the
;; inherited routines
pro pfo_calc_obj::get_property, $
   Xaxis=Xaxis, $
   Yaxis=Yaxis, $
   deviates=deviates, $
   dXaxis_dXin=dXaxis_dXin, $
   _REF_EXTRA=extra
  
  ;; X, Y axes and deviates.  Use the self->[XY]axis methods, since
  ;; they are carefully thought out.  Pass the /no_copy onto
  ;; self->[XY]axis with _EXTRA.
  if arg_present(Xaxis) or N_elements(Xaxis) ne 0 then $
     Xaxis = self->Xaxis(_EXTRA=extra)
  if arg_present(Yaxis) or N_elements(Yaxis) ne 0  then $
     Yaxis = self->Yaxis(_EXTRA=extra)
  if arg_present(deviates) or N_elements(deviates) ne 0  then $
     deviates = self->deviates(_EXTRA=extra)
  if arg_present(dXaxis_dXin) or N_elements(dXaxis_dXin) ne 0  then $
     dXaxis_dXin = self->dXaxis_dXin(_EXTRA=extra)

  self->pfo_data_obj::get_property, _EXTRA=extra
  self->pfo_parinfo_obj::get_property, _EXTRA=extra

end

;; Override set property of items that affect the cache
pro pfo_calc_obj::set_property, $
   Xin=Xin, $
   Yin=Yin, $
   Yerr=Yerr, $
   parinfo_array=parinfo_array, $
   _REF_EXTRA=extra

  ;; Check to see if we are setting anything that would invalidate our
  ;; caches
  if N_elements(Xin) + N_elements(Yin) + N_elements(Yerr) + N_elements(parinfo) ne 0 then $
     self->invalidate_cache

  ;; Pass everything onto inherited routines
  self->pfo_data_obj::set_property, $
     Xin=Xin, $
     Yin=Yin, $
     Yerr=Yerr, $
     _EXTRA=extra
  self->pfo_parinfo_obj::set_property, parinfo_array=parinfo_array, _EXTRA=extra

end

;; Each inherited class should have a descr method.
function pfo_calc_obj::descr

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.', /CONTINUE
        return, 'Not documented.'
     endif
  endif ;; not debugging

  descr = *self.ppfo_calc_obj_descr
  pfo_struct_append, descr, {pfo_data_obj: self->pfo_data_obj::descr()}
  pfo_struct_append, descr, {pfo_parinfo_obj: self->pfo_parinfo_obj::descr()}
  return, descr
end

pro pfo_calc_obj::cleanup
  ptr_free, self.ppfo_calc_obj_descr
  ptr_free, self.pXaxis
  ptr_free, self.pYaxis
  ptr_free, self.pROI_Xin_idx
  ptr_free, self.pdeviates
  ptr_free, self.pdXaxis_dXin

  self->pfo_data_obj::cleanup
  self->pfo_parinfo_obj::cleanup
end

function pfo_calc_obj::init, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   _REF_EXTRA=extra

  init = {pfo_sysvar}
  
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object not properly initialized ', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Create our documentation string
  self.ppfo_calc_obj_descr $
     = ptr_new( $
     {README	: 'pfo_calc_obj encapsulates all of the shared information necessary for calculating PFO functions.', $
      SUPERCLASSES: 'pfo_data_obj, pfo_parinfo_obj', $
      PROPERTY	: 'Xaxis, Yaxis', $
      METHODS	: 'Xaxis, Yaxis'} $
              )

  ;; Turn our null reference pointers into undefined variables
  self.pXaxis = ptr_new(/allocate_heap)
  self.pYaxis = ptr_new(/allocate_heap)
  self.pROI_Xin_idx = ptr_new(/allocate_heap)
  self.pdeviates = ptr_new(/allocate_heap)
  self.pdXaxis_dXin = ptr_new(/allocate_heap)

  ;; Call our superclass init methods
  ok = self->pfo_data_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then $
     return, 0
  return, self->pfo_parinfo_obj::init(_EXTRA=extra)

end

pro pfo_calc_obj__define
  objectClass = $
     {pfo_calc_obj, $
      ppfo_calc_obj_descr: ptr_new(), $ ;; Pointer to description structure
      pXaxis		: ptr_new(), $	;; Cached internal Xaxis -- calculated only from internal Xin and parinfo.value
      pYaxis		: ptr_new(), $	;; Cached Yaxis -- calculated only from internal Xin and parinfo.value
      pROI_Xin_idx	: ptr_new(), $  ;; Cached indices into Xin for the active ROI(s)
      pdeviates		: ptr_new(), $	;; Cached deviates -- calculated only from internal Xin and parinfo.value
      pdXaxis_dXin	: ptr_new(), $	;; Cached value of dXaxis/dXin for converting Y-axis of plots to read in Xaxis units
      inherits pfo_data_obj, $ 		;; data
      inherits pfo_parinfo_obj $	;; parinfo (function definition)
     }
end
