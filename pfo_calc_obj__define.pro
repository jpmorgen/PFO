;+
; NAME: pfo_calc_obj

; PURPOSE: Create, initialize, and work with the pfo_calc_obj, which
; encapsulates the data and methods needed to calculate and display
; data and functions in the PFO system.

; CATEGORY: PFO
;
; CALLING SEQUENCE: o = obj_new('pfo_calc_obj', [[Xin,] Yin [Yerr]], keywords))

; DESCRIPTION: This object contains methods which calculate quantities
; such as the Xaxis (not to be confused with the raw data X-axis,
; Xin), Yaxis and deviates between the Yaxis and data Y-axis (Yin).

; WARNING: fundamentally, calculations are intended to be made on the
; encapsulated property of this object.  If keywords overriding this
; property are given, such as Xin, Yin, parinfo, and params (see
; parinfo_cache_ok for full list), the externally supplied quantities
; are temporarily moved into the object and then moved back out again
; with the calculation finishes successfully.  This move is done in a
; somewhat sly manner by pfo_calc_obj::parinfo_cache_ok.  Some efforts
; have been made to CATCH errors in such a way that the external and
; internal property always get swapped back, but these efforts might
; not be perfect.  If is always better to do the swap of internal and
; external quantities such as parinfo, Xin, Yin, etc. with the
; appropriate methods BEFORE these routines are called.  This has the
; side-benefit of making use of the caching features for the
; externally supplied information.  See pfo_plot_obj::plot for an
; example of this in the case of parinfo.

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
; $Id: pfo_calc_obj__define.pro,v 1.3 2011/09/01 22:14:01 jpmorgen Exp $
;
; $Log: pfo_calc_obj__define.pro,v $
; Revision 1.3  2011/09/01 22:14:01  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:18:47  jpmorgen
; Release to Tom
; Inherit pfo_plot_obj here, set_property bug fix, improved descr
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; Print basic information about contents of property
pro pfo_calc_obj::print, $
   _REF_EXTRA=extra

  self->pfo_data_obj::print, _EXTRA=extra
  print, 'Xaxis cache: ' + strtrim(N_elements(*self.pXaxis), 2) + ' points'
  print, 'Yaxis cache: ' + strtrim(N_elements(*self.pYaxis), 2) + ' points'
  print, 'deviates cache: ' + strtrim(N_elements(*self.pdeviates), 2) + ' points'
  print, 'ROI_Xin_idx cache: ' + strtrim(N_elements(*self.pROI_Xin_idx), 2) + ' points'
  print, self->pfo_parinfo_obj::print(_EXTRA=extra)

end

;; Override the refresh from pfo_parinfo_obj_cw_obj.  Refresh is
;; called by repopulate, so don't worry about that.
pro pfo_calc_obj::refresh, $
   _REF_EXTRA=extra ;; idx and value are passed here

  ;; If refresh is called, we probably have invalidated our cache
  self->invalidate_cache
  self->pfo_parinfo_obj_cw_obj::refresh, _EXTRA=extra
  self->pfo_obj_plotwin_obj::replot, _EXTRA=extra

end

;; 
;; function pfo_calc_obj::widget, $
;;    _REF_EXTRA=extra
;; 
;;   ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
;;   if !pfo.debug le 0 then begin
;;      CATCH, err
;;      if err ne 0 then begin
;;         CATCH, /CANCEL
;;         message, /NONAME, !error_state.msg, /CONTINUE
;;         message, 'ERROR: caught the above error.  Widget not properly initialized ', /CONTINUE
;;         return, !tok.nowhere
;;      endif
;;   endif ;; not debugging
;; 
;;   tlbID = pfo_generic_base(title='PFO PLOTWIN', containerID=containerID, $
;;                         mbarID=mbarID, /tlb_size_events, pfo_obj=self)
;;   ID = pfo_plotwin_cw(containerID, window_index=window_index, pfo_obj=self)
;;   self->plot, window_index=window_index
;; 
;;   
;;   return, tlbID
;; 
;; 
;; end


;; Invalidate cached values when we have some change to fundamental
;; property
pro pfo_calc_obj::invalidate_cache
  ptr_free, self.pXaxis
  ptr_free, self.pYaxis
  ptr_free, self.pROI_Xin_idx
  ptr_free, self.pdeviates
  ptr_free, self.pdXaxis_dXin
  self.pXaxis = ptr_new(/allocate_heap)
  self.pYaxis = ptr_new(/allocate_heap)
  self.pROI_Xin_idx = ptr_new(/allocate_heap)
  self.pdeviates = ptr_new(/allocate_heap)
  self.pdXaxis_dXin = ptr_new(/allocate_heap)
end

;; This is a helper routine for the Yaxis, Xaxis, etc. methods that
;; checks keyword arguments to see if anything that would invalidate
;; the cache is being used.  WARNING: when Xin, Yin, parinfo, params,
;; or customized idx are defined, this MUST BE CALLED TWICE.  The
;; first time it is called, it swaps the external parinfo for the
;; encapsulated parinfo, the second time it swaps them back.
function pfo_calc_obj::parinfo_cache_ok, $
   Xin=Xin, $ ;; could be different than encapsulated Xin.  See WARNING above
   Yin=Yin, $ ;; like Xin, but only used in deviates
   parinfo=parinfo, $ ;; could be different than encapsulated Xin. See WARNING above
   params=params, $ ;; could be different than parinfo.value
   idx=idx, $ ;; could be a subset of parinfo.  If N_elements of idx and parinfo are the same, assume it is safe to cache
   error=error, $ ;; pfo_parinfo_parse returned an error, so we shouldn't cache the results
   no_cache=no_cache, $ ;; don't cache if we don't want to cache!
   no_copy=no_copy, $ ;; no_copy is interpreted as moving a value out of the cache, so no_cache is implied
   xaxis=xaxis, $ ;; return parameter we know won't affect our cache.  "Soak it up" so it doesn't spill into _EXTRA, just in case someone passed it as a defined value
   ROI_Xin_idx=ROI_Xin_idx, $ ;; return parameter we know won't affect our cache, like xaxis
   _EXTRA=extra ;; any extra parameters may change the way the function is calculated, so it is not safe to cache

  ;; Swap external Xin with cache Xin
  if N_elements(Xin) ne 0 then begin
     temp = temporary(Xin)
     Xin = temporary(*self.pXin)
     *self.pXin = temporary(temp)
     invalidate_cache = 1
  endif
  ;; Swap external Yin with cache Yin
  if N_elements(Yin) ne 0 then begin
     temp = temporary(Yin)
     Yin = temporary(*self.pYin)
     *self.pYin = temporary(temp)
     invalidate_cache = 1
  endif
  ;; Swap external parinfo with cache parinfo
  if N_elements(parinfo) ne 0 then begin
     temp = temporary(parinfo)
     parinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(temp)
     invalidate_cache = 1
  endif
  if keyword_set(invalidate_cache) then $
     self->invalidate_cache

  ;; Return our boolean value.  NOTE, it is important that the
  ;; *self.pparinfo swap with parinfo happen before this so that the
  ;; provided idx and parinfo are consistent.
  return, (N_elements(Xin) + $		;; Keywords that would invalidate cache
           N_elements(Yin) + $
           N_elements(parinfo) + $
           N_elements(params) + $
           N_elements(error) + $
           N_elements(no_cache) + $
           N_elements(no_copy) + $
           N_elements(extra) $
           eq 0) $ ;; Be a little more sophisticated with idx (see above)
          and (N_elements(idx) eq 0 or (N_elements(idx) eq N_elements(*self.pparinfo)))

end

;; Return the *CALCULATED* Yaxis (not to be confused with the data
;; Y-axis, Yin).  Any pfo_parinfo_parse(/calc) arguments are
;; accepted.  If possible, the resulting Yaxis, Xaxis and Xin_ROI_idx
;; are cached for fast access later (e.g. for plotting).
;; See parinfo_cache_ok for the list of keywords that are known to
;; affect or not affect the ability to cache.
function pfo_calc_obj::Yaxis, $
   xaxis=xaxis, $       ;; optional return parameter we get for free from pfo_parinfo_parse, will be cached if caching Yaxis
   ROI_Xin_idx=ROI_Xin_idx, $ ;; optional return parameter we get for free from pfo_parinfo_parse, will be cached if caching Yaxis
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   no_copy=no_copy, $   ;; Moves Yaxis value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache.  NOTE:
  ;; parinfo_cache_ok exchanges pfo_obj's Xin and parinfo with
  ;; any externally Xin and parinfo values so that all calculation is
  ;; done from encapsulated data
  if N_elements(*self.pYaxis) ne 0 and $
     self->parinfo_cache_ok(_EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pYaxis)
     endif ;; no_copy
     ;; Normal case of copying Yaxis to return value
     return, *self.pYaxis
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ;; Call pfo_parinfo_parse.  Also grab all of the other quantities
  ;; that are calculated "for free."  This only costs a little extra
  ;; space for ROI_Xin_idx, which is only build up in
  ;; pfo_parinfo_parse when requested.
  Yaxis = pfo_parinfo_parse(/calc, *self.pparinfo, Xin=*self.pXin, $
                            xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, pfo_obj=self, $
                            error=error, _EXTRA=extra)

  ;; Handle our cache.  Note that we only cache quantities that have
  ;; been calculated so far.  Other quantities derived from these
  ;; (e.g. deviates, dXaxis_dXin) are cached when the user first asks
  ;; for them.  Xaxis and ROI_Xin_idx are also return quantities.
  ;; NOTE: parinfo_cache_ok puts externally supplied Xin and parinfo
  ;; back into their keywords
  if self->parinfo_cache_ok(error=error, no_cache=no_cache, no_copy=no_copy, _EXTRA=extra) then begin
     *self.pXaxis = Xaxis
     *self.pYaxis = Yaxis
     *self.pROI_Xin_idx = ROI_Xin_idx
  endif ;; caching

  return, Yaxis

end

;; Return the *CALCULATED* Xaxis (not to be confused with the data
;; X-axis, Xin).  Any pfo_Xaxis arguments are accepted.  If possible,
;; the resulting Xaxis is cached for fast access later (e.g. for
;; plotting).  See parinfo_cache_ok for the list of keywords that are
;; known to affect or not affect the ability to cache.
function pfo_calc_obj::Xaxis, $
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   no_copy=no_copy, $   ;; Moves Xaxis value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache.  NOTE:
  ;; parinfo_cache_ok exchanges pfo_obj's Xin and parinfo with any
  ;; externally Xin and parinfo values so that all calculation is done
  ;; from encapsulated data
  if N_elements(*self.pXaxis) ne 0 and $
     self->parinfo_cache_ok(_EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pXaxis)
     endif ;; no_copy
     ;; Normal case of copying Xaxis to return value
     return, *self.pXaxis
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ;; Call pfo_Xaxis.
  Xaxis = pfo_Xaxis(*self.pparinfo, Xin=*self.pXin, pfo_obj=self, $
                    error=error, _EXTRA=extra)

  ;; Handle our cache.  NOTE: parinfo_cache_ok puts externally
  ;; supplied Xin, parinfo, etc. back into their keywords
  if self->parinfo_cache_ok(error=error, no_cache=no_cache, no_copy=no_copy, _EXTRA=extra) then begin
     *self.pXaxis = Xaxis
  endif ;; caching

  return, Xaxis

end

;; Return ROI_Xin_idx, which are the indices into Xin for the active
;; ROI functions.  Any pfo_ROI_Xin_idx artuments are accpeted.  If
;; possible, the resulting ROI_Xin_idx is cached for fast access later
;; (e.g. for plotting).  See parinfo_cache_ok for the list of keywords
;; that are known to affect or not affect the ability to cache.
function pfo_calc_obj::ROI_Xin_idx, $
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   no_copy=no_copy, $   ;; Moves ROI_Xin_idx value out of cache to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   _REF_EXTRA=extra

  ;; Check to see if we can just use our cache.  NOTE:
  ;; parinfo_cache_ok exchanges pfo_obj's Xin and parinfo with any
  ;; externally Xin and parinfo values so that all calculation is done
  ;; from encapsulated data
  if N_elements(*self.pROI_Xin_idx) ne 0 and $
     self->parinfo_cache_ok(_EXTRA=extra) then begin
     ;; If we made it here, we can work with the cache
     ;; Check the no_copy keyword
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        return, temporary(*self.pROI_Xin_idx)
     endif ;; no_copy
     ;; Normal case of copying ROI_Xin_idx to return value
     return, *self.pROI_Xin_idx
  endif ;; Using cache value

  ;; If we made it here, we need to do a fresh calculation
  
  ROI_Xin_idx = pfo_ROI_Xin_idx(*self.pparinfo, Xin=*self.pXin, pfo_obj=self, $
                                error=error, _EXTRA=extra)

  ;; Handle our cache.  NOTE: parinfo_cache_ok puts externally
  ;; supplied Xin, parinfo, etc. back into their keywords
  if self->parinfo_cache_ok(error=error, no_cache=no_cache, no_copy=no_copy, _EXTRA=extra) then begin
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
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   parinfo=parinfo, $ 	;; We need to capture externally supplied parinfo and Xin to make sure they aren't swapped
   Xin=Xin, $ 		;; with property too many times in parinfo_cache_ok due to our call to self->Yaxis()
   no_copy=no_copy, $ ;; Moves deviates value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   hide_infinity=hide_infinity, $ ;; Delete instances of infinity from the deviates.  Changes length of vector, so results can't be cached
   hide_NaN=hide_NaN, $ ;; Delete instances of NaN from the deviates.  Changes length of vector, so results can't be cached
  _REF_EXTRA=extra
  
  ;; Check to see if we can work from a cache.  NOTE: parinfo_cache_ok
  ;; exchanges pfo_obj's Xin and parinfo with any externally Xin and
  ;; parinfo values so that all calculation is done from encapsulated
  ;; data
  if N_elements(*self.pdeviates) ne 0 and $
     self->parinfo_cache_ok(parinfo=parinfo, Xin=Xin, _EXTRA=extra) then begin
     if keyword_set(no_copy) or keyword_set(no_cache) then begin
        ;; This quick one-time transfer invalidates our cache
        return, temporary(*self.pdeviates)
     endif ;; no_copy
     ;; Normal case of copying deviates
     return, *self.pdeviates
  endif ;; working from cache

  ;; If we made it here, we need to calculate the deviates.
  ;; Handle the different cases of deviates a la MPFITFUN

  ;; Start with the case that we have no Yerr or weights.  The best
  ;; deviate we can muster in this case is just the residual, with
  ;; equal weighting to everything.  NOTE: our call to Yaxis() should
  ;; not exchange parinfo and Xin, since we have already done that
  ;; once, above.
  deviates = *self.pYin - self->Yaxis(error=error, _EXTRA=extra)

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

  ;; Now handle our cache.  NOTE: parinfo_cache_ok puts externally
  ;; supplied Xin, parinfo, etc. back into their keywords
  if keyword_set(hide_infinity) + keyword_set(hide_NAN) eq 0 $
     and self->parinfo_cache_ok(error=error, no_cache=no_cache, no_copy=no_copy, parinfo=parinfo, Xin=Xin, _EXTRA=extra) then begin
     *self.pdeviates = deviates
  endif ;; caching

  return, deviates

end

;; dXaxis/dXin allows Yaxis to read in Xaxis units
function pfo_calc_obj::dXaxis_dXin, $
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   parinfo=parinfo, $   ;; We need to capture externally supplied parinfo and Xin to make sure they aren't swapped
   Xin=Xin, $           ;; with property too many times in parinfo_cache_ok due to our call to self->Xaxis()
   no_copy=no_copy, $ ;; Moves dXaxis_dXin value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $    ;; Equivalent meaning to no_copy
  _REF_EXTRA=extra
  
  ;; Check to see if we can work from a cache.  NOTE: parinfo_cache_ok
  ;; exchanges pfo_obj's Xin and parinfo with any externally Xin and
  ;; parinfo values so that all calculation is done from encapsulated
  ;; data
  if N_elements(*self.pdXaxis_dXin) ne 0 and $
     self->parinfo_cache_ok(parinfo=parinfo, Xin=Xin, _EXTRA=extra) then begin
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
        ;; Return our input parinfo and Xin keywords, if necessary
        junk = self->parinfo_cache_ok(parinfo=parinfo, Xin=Xin, _EXTRA=extra)
        return, 1d
     endif
  endif ;; not debugging

  ;; Do calculations in such a way as to minimize the number of copies
  ;; of Xin.  NOTE: our call to Xaxis() should not exchange parinfo
  ;; and Xin, since we have already done that once, above.
  dXaxis_dXin = deriv(*self.pXin, self->Xaxis(error=error, _EXTRA=extra))

  ;; Now handle our cache
  if keyword_set(error) + keyword_set(no_copy) + keyword_set(no_cache) eq 0 $
     and self->parinfo_cache_ok(parinfo=parinfo, Xin=Xin, _EXTRA=extra) then begin
     *self.pdXaxis_dXin = dXaxis_dXin
  endif ;; caching
  
  return, dXaxis_dXin

end

;; If someone runs a procedure on parinfo, assume they have changed
;; it, unless not_modified flag is set
pro pfo_calc_obj::parinfo_call_procedure, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   not_modified=not_modified, $	;; default is to assume parinfo was modified
   _REF_EXTRA=extra

  case N_params() of
     0:  message, 'ERROR: specify name of function (as a string)'
     1:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, _EXTRA=extra
     2:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, _EXTRA=extra
     3:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, _EXTRA=extra
     4:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, _EXTRA=extra
     5:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, _EXTRA=extra
     6:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, _EXTRA=extra
     7:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, _EXTRA=extra
     8:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra
     9:  self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, _EXTRA=extra
     10: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, _EXTRA=extra
     11: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, _EXTRA=extra
     12: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, _EXTRA=extra
     13: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, _EXTRA=extra
     14: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, _EXTRA=extra
     15: self->pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _EXTRA=extra
     else: message, 'ERROR: to many positional parameters'
  endcase 

  ;; Working out the double negatives, this basically means we
  ;; invalidate our Yaxis cache unless the user sets /not_modified
  if NOT keyword_set(not_modified) then $
     self->invalidate_cache
 
end

function pfo_calc_obj::parinfo_call_function, $
   proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, $
   not_modified=not_modified, $	;; default is to assume parinfo was modified
   _REF_EXTRA=extra

  case N_params() of
     0:  message, 'ERROR: specify name of function (as a string)'
     1:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, _EXTRA=extra)
     2:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, _EXTRA=extra)
     3:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, _EXTRA=extra)
     4:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, _EXTRA=extra)
     5:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, _EXTRA=extra)
     6:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, _EXTRA=extra)
     7:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, _EXTRA=extra)
     8:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra)
     9:  return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, _EXTRA=extra)
     10: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, _EXTRA=extra)
     11: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, _EXTRA=extra)
     12: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, _EXTRA=extra)
     13: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, _EXTRA=extra)
     14: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, _EXTRA=extra)
     15: return, self->pfo_parinfo_obj::parinfo_call_function(proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _EXTRA=extra)
     else: message, 'ERROR: to many positional parameters'
  endcase 

  ;; Working out the double negatives, this basically means we
  ;; invalidate our Yaxis cache unless the user sets /not_modified
  if NOT keyword_set(not_modified) then $
     self->invalidate_cache
 
end

;; Except for yaxis, which is derived here from the combination of the
;; data (Xin) and function (parinfo), just pass get property onto the
;; inherited routines
pro pfo_calc_obj::get_property, $
   init_Yaxis=init_Yaxis, $ ;; initial value of Yaxis (0 or NaN)
   Xaxis=Xaxis, $
   Yaxis=Yaxis, $
   deviates=deviates, $
   dXaxis_dXin=dXaxis_dXin, $
   _REF_EXTRA=extra

  if arg_present(init_Yaxis) or N_elements(init_Yaxis) ne 0  then init_Yaxis = self.init_Yaxis
  ;; X, Y axes and deviates.  Use the self->[XY]axis methods, since
  ;; they are carefully thought out.  Pass the /no_copy onto
  ;; self->[XY]axis with _EXTRA.
  if arg_present(Xaxis) or N_elements(Xaxis) ne 0 then Xaxis = self->Xaxis(_EXTRA=extra)
  if arg_present(Yaxis) or N_elements(Yaxis) ne 0  then Yaxis = self->Yaxis(_EXTRA=extra)
  if arg_present(deviates) or N_elements(deviates) ne 0  then deviates = self->deviates(_EXTRA=extra)
  if arg_present(dXaxis_dXin) or N_elements(dXaxis_dXin) ne 0  then dXaxis_dXin = self->dXaxis_dXin(_EXTRA=extra)

  self->pfo_data_obj::get_property, _EXTRA=extra
  self->pfo_parinfo_obj::get_property, _EXTRA=extra
  self->pfo_plot_obj::get_property, _EXTRA=extra

end

;; Override set property of items that affect the cache
pro pfo_calc_obj::set_property, $
   init_Yaxis=init_Yaxis, $ ;; initial value of Yaxis (0 or NaN)
   Xin=Xin, $
   Yin=Yin, $
   Yerr=Yerr, $
   parinfo_array=parinfo_array, $
   _REF_EXTRA=extra

  if N_elements(init_Yaxis) ne 0 then self.init_Yaxis = init_Yaxis

  ;; Check to see if we are setting anything that would invalidate our
  ;; caches
  if N_elements(Xin) + N_elements(Yin) + N_elements(Yerr) + N_elements(parinfo_array) ne 0 then $
     self->invalidate_cache

  ;; Pass everything onto inherited routines
  self->pfo_data_obj::set_property, $
     Xin=Xin, $
     Yin=Yin, $
     Yerr=Yerr, $
     _EXTRA=extra
  self->pfo_parinfo_obj::set_property, parinfo_array=parinfo_array, _EXTRA=extra
  self->pfo_plot_obj::set_property, _EXTRA=extra

  ;; Do a replot just in case data or plot info changed.  NOTE: this
  ;; means that pfo_obj_plotwin_obj needs to be initilized before the
  ;; pfo_data_obj
  self->pfo_obj_plotwin_obj::replot

end

;; Each inherited class should have a descr method.
function pfo_calc_obj::descr

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
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

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
  self->pfo_plot_obj::cleanup
  self->pfo_obj_plotwin_obj::cleanup
end

function pfo_calc_obj::init, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   _REF_EXTRA=extra

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
     {README	: 'pfo_calc_obj encapsulates all of the shared information necessary for calculating, plotting, and printing PFO functions.', $
      SUPERCLASSES: ['pfo_data_obj', 'pfo_parinfo_obj', 'pfo_plot_obj'], $
      METHODS	: ['Xaxis()', 'Yaxis()', 'ROI_Xin_idx()', 'deviates()', 'dXaxis_dXin()', 'parinfo_call_procedure (invalidates cache)', 'parinfo_call_function() (invalidates cache)', 'invalidate_cache', 'print']} $
     )

  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_parinfo_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_calc_obj_descr, $
                        {PROPERTY: property[good_idx]}


  ;; Initialize property
  self.init_Yaxis = !pfo.init_Yaxis

  ;; Turn our null reference pointers into undefined variables
  self.pXaxis = ptr_new(/allocate_heap)
  self.pYaxis = ptr_new(/allocate_heap)
  self.pROI_Xin_idx = ptr_new(/allocate_heap)
  self.pdeviates = ptr_new(/allocate_heap)
  self.pdXaxis_dXin = ptr_new(/allocate_heap)

  ;; Call our superclass init methods.  Call ones that just initilize
  ;; null pointers first
  ok = self->pfo_obj_plotwin_obj::init(_EXTRA=extra)
  if NOT ok then return, 0
  ;; Now call the ones that actually call set_property, which might
  ;; bubble back up to a higher-level routine
  ok = self->pfo_data_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then return, 0
  ok = self->pfo_parinfo_obj::init(_EXTRA=extra)
  if NOT ok then return, 0
  ok = self->pfo_plot_obj::init(_EXTRA=extra)
  if NOT ok then return, 0

  return, 1

end

pro pfo_calc_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_calc_obj, $
      ppfo_calc_obj_descr: ptr_new(), $ ;; Pointer to description structure
      init_Yaxis	: 0d, $		;; Override of !pfo.init_Yaxis, so functions can start from NaN (default) or 0
      pXaxis		: ptr_new(), $	;; Cached internal Xaxis -- calculated only from internal Xin and parinfo.value
      pYaxis		: ptr_new(), $	;; Cached Yaxis -- calculated only from internal Xin and parinfo.value
      pROI_Xin_idx	: ptr_new(), $  ;; Cached indices into Xin for the active ROI(s)
      pdeviates		: ptr_new(), $	;; Cached deviates -- calculated only from internal Xin and parinfo.value
      pdXaxis_dXin	: ptr_new(), $	;; Cached value of dXaxis/dXin for converting Y-axis of plots to read in Xaxis units
      inherits pfo_data_obj, $ 		;; data
      inherits pfo_parinfo_obj, $	;; parinfo (function definition), parinfo widgets and basic widgets
      inherits pfo_obj_plotwin_obj, $	;; enable plots into (multiple) draw widgets (plotwins)
      inherits pfo_plot_obj $		;; plotting commands (no widgets -- makes sure self->plot works)
     }

end
