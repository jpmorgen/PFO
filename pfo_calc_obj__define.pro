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
; encapsulated property of this object.  When keywords overriding this
; property are given, such as Xin, Yin, parinfo, and params (see
; parinfo_cache_ok for full list), IDL's _REF_EXTRA mechanism is used
; to allow these externally supplied values to temporarily override
; the encapsulated property.  The system has not been fully debugged
; in this mode: internal property might still be used in some of the
; calculations.  A better approach is to swap internal and external
; quantities, such as parinfo, Xin, Yin, Yerr, etc. BEFORE these
; routines are called.  This has the side-benefit of making use of the
; caching features for the externally supplied information.  See
; pfo_plot_obj::plot for an example of this in the case of parinfo.

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
; $Id: pfo_calc_obj__define.pro,v 1.5 2011/11/18 15:29:32 jpmorgen Exp $
;
; $Log: pfo_calc_obj__define.pro,v $
; Revision 1.5  2011/11/18 15:29:32  jpmorgen
; Improve deviates code, documentation, get rid of property swapping in
; light of discovery about how _REF_EXTRA overrides keywords
;
; Revision 1.4  2011/09/08 19:59:27  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
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

;; Override the update method from pfo_parinfo_obj so that we can make
;; sure our cache is invalidated properly.  Also add replotting
pro pfo_calc_obj::update, $
   undo, $ ;; (optional input) previous state of parinfo before calling routine tweaked the encapsulated parinfo
   save_undo=save_undo, $ ;; selectively passed to pf_parinfo_obj::update (see code)
   _REF_EXTRA=extra ;; passed to prepare_update and pfo_parinfo_update

  self->invalidate_cache
  ;; Update updates the parinfo and (by default) the widgets.  Since
  ;; widgets can be slow, do the plot first, then the widgets
  self->pfo_parinfo_obj::update, undo, /no_widget, save_undo=save_undo, _EXTRA=extra
  self->replot, _EXTRA=extra
  self->pfo_parinfo_obj::update, undo, _EXTRA=extra

end

;; Override the refresh from pfo_parinfo_obj_cw_obj to add cache clearing.
pro pfo_calc_obj::refresh, $
   _REF_EXTRA=extra

  ;; If refresh is called, we probably have invalidated our cache
  self->invalidate_cache
  ;; Let pfo_parinfo_obj_cw_obj::refresh sort out whether or not to
  ;; refresh parinfo or non-parinfo widgets based on idx and value keywords
  self->pfo_parinfo_obj_cw_obj::refresh, _EXTRA=extra

end

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
;; the cache is being used.
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

  ;; Return our boolean value.
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
  ;; pfo_parinfo_parse when requested.  NOTE: if Xin is passed via
  ;; _REF_EXTRA, it overrides the Xin passed here here.
  Yaxis = pfo_parinfo_parse(/calc, parinfo=*self.pparinfo, Xin=*self.pXin, $
                            xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, pfo_obj=self, $
                            error=error, _EXTRA=extra)

  ;; Handle our cache.  Note that we only cache quantities that have
  ;; been calculated so far.  Other quantities derived from these
  ;; (e.g. deviates, dXaxis_dXin) are cached when the user first asks
  ;; for them.  Xaxis and ROI_Xin_idx are also return quantities.
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
  ;; parinfo_cache_ok exchanges pfo_obj's parinfo with any external
  ;; parinfo so that all calculation is done from encapsulated data
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

  ;; If we made it here, we need to do a fresh calculation.  NOTE: if
  ;; Xin is passed via _REF_EXTRA, it overrides Xin here.
  
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
  ;; parinfo_cache_ok exchanges pfo_obj's parinfo with any external
  ;; parinfo so that all calculation is done from encapsulated data
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

  ;; If we made it here, we need to do a fresh calculation.  NOTE: if
  ;; Xin is passed via _REF_EXTRA, it overrides Xin here.
  
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
;; access.
function pfo_calc_obj::deviates, $
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   Yin=Yin, $         ;; Allow user to supply one-time Yerr
   Yerr=Yerr, $         ;; Allow user to supply one-time Yerr
   weights=weights, $   ;; Allow user to supply one-time weights
   no_copy=no_copy, $ ;; Moves deviates value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $ ;; Equivalent meaning to no_copy
   hide_infinity=hide_infinity, $ ;; Delete instances of infinity from the deviates.  Changes length of vector, so results can't be cached, but handy for MPFIT
   hide_NaN=hide_NaN, $ ;; Delete instances of NaN from the deviates.  Changes length of vector, so results can't be cached, but handy for MPFIT
  _REF_EXTRA=extra
  
  ;; Check to see if we can just use our cache.  NOTE:
  ;; parinfo_cache_ok exchanges pfo_obj's parinfo with any external
  ;; parinfo so that all calculation is done from encapsulated data
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
  ;; Build up deviates using the best information available and
  ;; managing memory carefully.

  ;; Start assuming we have no property or input from which to
  ;; calculate deviates
  deviates = !values.d_NAN
  ;; Prefer command-line Yin over property (if available)
  if N_elements(Yin) ne 0 then $
     deviates = Yin $
  else $
     if N_elements(*self.pYin) ne 0 then $
        deviates = *self.pYin

  ;; Calculate the residual
  deviates = temporary(deviates) - self->Yaxis(error=error, _EXTRA=extra)

  ;; Check to see if we have weights
  if N_elements(weights) gt 0 then begin
     ;; Command line weights
     deviates = temporary(deviates) * weights
  endif else begin
     if N_elements(*self.pweights) gt 0 then begin
        ;; Encapsulated weights
        deviates = temporary(deviates) * *self.pweights
     endif else begin
        ;; We have no weights, check to see if we have Yerr
        if N_elements(Yerr) gt 0 then begin
           ;; Command line Yerr
           deviates = temporary(deviates) / Yerr
        endif else begin
           if N_elements(*self.pYerr) gt 0 then begin
              ;; Encapsulated Yerr
              deviates = temporary(deviates) / *self.pYerr
           endif ;; Deviates with encapsulted Yerr
        endelse ;; command line vs. encapsulated Yerr
     endelse ;; weights vs Yerr
  endelse ;; Command line weights vs everything else

  ;; Get rid of infinities
  if keyword_set(hide_infinity) then begin
     good_idx = where(finite(deviates, /infinity) eq 0, count)
     if count eq 0 then $
        message, 'ERROR: no good points'
     ;; unwrap
     deviates = deviates[good_idx]
  endif
  ;; Get rid of NaNs
  if keyword_set(hide_NaN) then begin
     good_idx = where(finite(deviates, /NAN) eq 0, count)
     if count eq 0 then $
        message, 'ERROR: no good points'
     ;; unwrap
     deviates = deviates[good_idx]
  endif

  ;; Now handle our cache.  NOTE: parinfo_cache_ok puts externally
  ;; supplied parinfo back into its keyword
  if keyword_set(hide_infinity) + keyword_set(hide_NAN) eq 0 $
     and self->parinfo_cache_ok(error=error, no_cache=no_cache, no_copy=no_copy, _EXTRA=extra) then begin
     *self.pdeviates = deviates
  endif ;; caching

  return, deviates

end

;; dXaxis/dXin allows Yaxis to read in Xaxis units
function pfo_calc_obj::dXaxis_dXin, $
   error=error, $ ;; return error if there was a problem in pfo_parinfo_parse, also, don't cache
   Xin=Xin, $           ;; with property too many times in parinfo_cache_ok due to our call to self->Xaxis()
   no_copy=no_copy, $ ;; Moves dXaxis_dXin value out of object to save memory, but it does cost computation the next time around
   no_cache=no_cache, $    ;; Equivalent meaning to no_copy
  _REF_EXTRA=extra
  
  ;; Check to see if we can work from a cache.  NOTE: parinfo_cache_ok
  ;; exchanges pfo_obj's parinfo with any external parinfo so that all
  ;; calculation is done from encapsulated data
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
  ;; errors and return 1, to be gentle on our calling code
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'WARNING: caught the above error.  Not calculating derivative, returning 1.  Use pfo_debug to track down error.', /CONTINUE
        return, 1d
     endif
  endif ;; not debugging

  ;; Do calculations in such a way as to minimize the number of copies
  ;; of Xin.
  if N_elements(Xin) ne 0 then $
     dXaxis_dXin = deriv(Xin, self->Xaxis(Xin=Xin, error=error, _EXTRA=extra)) $
  else $
     dXaxis_dXin = deriv(*self.pXin, self->Xaxis(error=error, _EXTRA=extra))

  ;; Now handle our cache
  if keyword_set(error) + keyword_set(no_copy) + keyword_set(no_cache) eq 0 $
     and self->parinfo_cache_ok(Xin=Xin, _EXTRA=extra) then begin
     *self.pdXaxis_dXin = dXaxis_dXin
  endif ;; caching
  
  return, dXaxis_dXin

end

;; Helper function for convert_coord method

;; The basic idea is to always find the Xin value corresponding to the
;; Xin, Xaxis or Yaxis value stored as "value" in our COMMON block.
;; The COMMON block variable from_axis tells us which axis "value"
;; comes from.
function pfo_convert_coord_funct, Xin
  ;; Common blocks are a pain (in my opinion).  We initilized idx to
  ;; 'none' to make sure idx did not persist between calls to this
  ;; routine and to avoid generating idx for no good reason.  The
  ;; following code makes a local copy of idx if idx is set to
  ;; something other than a string or makes lidx an undefined variable
  ;; and passes that to the underlying routines.
  COMMON pfo_convert_coord, value, from_axis, pfo_obj, idx

  on_error, 0
  lidx = 0
  junk = temporary(lidx)
  if N_elements(idx) ne 0 and size(/type, idx) ne !tok.string then $
     lidx = idx
  case from_axis of 
     !pfo.Xin : return, value - Xin
     !pfo.Xaxis : return, value - pfo_obj->Xaxis(Xin=Xin, idx=lidx)
     !pfo.Yaxis : return, value - pfo_obj->Yaxis(Xin=Xin, idx=lidx)
  endcase
end

;; General PFO coordinate conversion.  If we aren't doing a forward
;; transformation (e.g. Xin to Xaxis or Yaxis), uses IDL's NEWTON
;; function to do conversions from Yaxis or Xaxis to Xin and then go
;; forward to the desired output axis.
function pfo_calc_obj::convert_coord, $
   value_in, $ ;; Input value or vector
   parinfo=parinfo, $ ;; (optional) parinfo encapsulated in pfo_obj is generally preferred
   idx=idx_in, $ ;; subset of parinfo
   initial_guess=initial_guess_in, $ ;; optional, but recommended for multi-valued functions
   from_Xin=from_Xin, $ ;; selectors to determine what axis value sits on and where it is going
   from_Xaxis=from_Xaxis, $
   from_Yaxis=from_Yaxis, $
   to_Xin=to_Xin, $
   to_Xaxis=to_Xaxis, $
   to_Yaxis=to_Yaxis, $
   Xin=Xin, $ 		;; output Xin axis value "for free"
   Xaxis=Xaxis, $ 	;; output Xaxis value "for free"
   max_initial_guess_search=max_initial_guess_search, $ ;; max number of searchers made for initial guess, if none provided
   initial_guess_increment=initial_guess_increment, $ ;; increment to Xin applied to find a working initial guess, if none provided.  Search is made in both positive and negative directions
   _REF_EXTRA=extra ;; args to/from NEWTON (check=check is a useful one for global/local minimum)

  ;; Yes, common blocks are yucky, but IDL's NEWTON requires it.
  ;; Note value is a single value of the function, value_in might be
  ;; multiple values
  COMMON pfo_convert_coord, value, from_axis, pfo_obj, idx

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.', /CONTINUE
        message, 'USAGE: result=pfo_calc_obj::convert_coord(value, parinfo=parinfo, idx=idx, /from_Xin, /from_Xaxis, /from_Yaxis, /to_Xin, /to_Xaxis, /to_Yaxis, Xin=Xin, Xaxis=Xaxis, max_initial_guess_search=max_initial_guess_search, initial_guess_increment=initial_guess_increment'
     endif
  endif ;; not debugging

  ;; Quietly return NaN if we have no input
  npts = N_elements(value_in)
  if npts eq 0 then $
     return, !values.d_NAN

  ;; Set up our from_axis COMMON block variable with a token.  Beware the
  ;; persistence of COMMON block variables.  Fortunately, not in type.
  idx = 'none'
  from_axis = 0
  if keyword_set(from_Xin) then $
    from_axis = !pfo.Xin 
  if keyword_set(from_Xaxis) then $
    from_axis = !pfo.Xaxis 
  if keyword_set(from_Yaxis) then $
    from_axis = !pfo.Yaxis
  if NOT keyword_set(from_axis) then $
    message, 'ERROR: one of /from_Xin, /from_Xaxis, /from_Yaxis must be specified'

  ;; Set up to_axis variable with an axis token
  if keyword_set(to_Xin) then $
    to_axis = !pfo.Xin 
  if keyword_set(to_Xaxis) then $
    to_axis = !pfo.Xaxis 
  if keyword_set(to_Yaxis) then $
    to_axis = !pfo.Yaxis
  if NOT keyword_set(to_axis) then $
    message, 'ERROR: one of /to_Xin, /to_Xaxis, /to_Yaxis must be specified'

  pfo_obj = self

  ;; Check to see if we have a valid parinfo.  If not, quietly return
  ;; our input value
  if N_elements(*self.pparinfo) eq 0 and N_elements(parinfo) eq 0 then $
     return, value_in

  ;; Check other command line parameters
  if N_elements(idx_in) ne 0 then $
     idx = idx_in

  if N_elements(max_initial_guess_search) eq 0 then $
    max_initial_guess_search = 50
  if N_elements(initial_guess_increment) eq 0 then $
    initial_guess_increment = 10

  ;; Check the dimensionality of initial guess
  nig = N_elements(initial_guess_in) 
  if nig ne 0 then $
     if nig ne 1 or nig ne npts then $
        message, 'ERROR: initial_guess must have 0, 1 or the same number of elements as value.'

  CATCH, /CANCEL
  ;; End of command line checking

  
  ;; Check for the trivial case where we are calculating from Xin.  In
  ;; this case, we don't need NEWTON
  if from_axis eq !pfo.Xin then $
     return, value_in


  ;; We will use IDL's NEWTON to find Xin for each point
  Xin = make_array(npts, value=!values.d_NAN)

  ;; Build in the capability to search around a little bit for the
  ;; initial guess.
  initial_guess_search = 0      ; counter for number of times we have tried

  if N_elements(initial_guess_in) ne 0 then $
     initial_guess = initial_guess_in

  ;; Initial guess is required, so if the user hasn't
  ;; supplied one, try to make one up.
  if N_elements(initial_guess) eq 0 then begin
     nig = 1
     ;; This might take several iterations.  Start from 0, since we
     ;; have no other idea
     initial_guess = 0d
     initial_guess_direction = 1 ; direction we are searching in
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Presumably our error is in NEWTON, meaning this initial
        ;; guess didn't work.  Check to see if we have searched too
        ;; many times in both directions
        if initial_guess_search ge max_initial_guess_search and $
           initial_guess_direction lt 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.', /CONTINUE
           message, 'ERROR: more than ' + strtrim(max_initial_guess_search, 2) + ' attempted and NEWTON still seems to be unable to find a solution.  Please supply an initial guess.  Retunring NaN(s)', /CONTINUE
           return, make_array(npts, value=!values.d_NAN)
        endif ;; error in fx_root

        ;; Check to see if we have searched too many times in the
        ;; positive direction
        if initial_guess_search ge max_initial_guess_search and $
           initial_guess_direction gt 0 then begin
           ;; reset our counter
           initial_guess_search = 0
           ;; Start off our guess going in the negative direction
           initial_guess = 0d
           ;; Make sure we continue in the negative direction
           initial_guess_direction = -1
        endif

        ;; Move our initial guess over
        initial_guess += initial_guess_direction*initial_guess_increment

     endif ;; Caught an error, presumably from NEWTON
     ;; Increment our counter
     initial_guess_search += 1
  endif ;; no initial guess supplied

  ;; If the user has specified an initial guess, report errors as
  ;; they happen
  if NOT keyword_set(initial_guess_search) then begin
     ;; Catch errors the first time they happen
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg
        message, 'ERROR: caught the above error.  Returning NaN(s)', /CONTINUE
        return, make_array(npts, value=!values.d_NAN)
     endif ;; error, presumably in NEWTON
  endif ;; not searching for an initial guess

  ;; Find our Xin value(s) that correspond to value
  for i=0,npts-1 do begin
     ;; Save this value in the COMMON block value variable
     value = value_in[i]
     ;; Get the proper initial guess
     if nig eq 1 then $
        ig = initial_guess $
     else $
        ig = initial_guess[i]
     Xin[i] = newton(ig, $
                     'pfo_convert_coord_funct', /double, $
                     _EXTRA=extra)
  endfor ;; each point in Xin

  ;; Return values
  case to_axis of 
     !pfo.Xin:   return, Xin ;; this should have been done above
     !pfo.Xaxis : return, self->Xaxis(Xin=Xin, idx=idx_in)
     !pfo.Yaxis : return, self->Yaxis(Xin=Xin, idx=idx_in)
  endcase

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
   Xin=Xin, $ ;; Property that affects our property (cache) or things we have merged here (update)
   Yin=Yin, $
   Yerr=Yerr, $
   parinfo_array=parinfo_array, $
   no_update=no_update, $ ;; capture flag which is headed toward pfo_parinfo_obj::set_property
   _REF_EXTRA=extra

  ;; Handle our own property
  if N_elements(init_Yaxis) ne 0 then self.init_Yaxis = init_Yaxis
  
  ;; Check to see if we are going to need to run self->update after we
  ;; set our property
  need_update = NOT keyword_set(no_update) and $
                N_elements(Xin) + N_elements(Yin) + N_elements(Yerr) + N_elements(parinfo_array) ne 0 

  if need_update then $
     self->prepare_update, undo

  ;; Pass everything onto inherited routines
  self->pfo_data_obj::set_property, $
     Xin=Xin, $
     Yin=Yin, $
     Yerr=Yerr, $
     _EXTRA=extra
  ;; Don't run update just yet 
  self->pfo_parinfo_obj::set_property, parinfo_array=parinfo_array, /no_update, _EXTRA=extra
  ;; Important to have parinfo stuff updated before plot stuff, since
  ;; plot stuff depends on parinfo.
  self->pfo_plot_obj::set_property, property_set=property_set, _EXTRA=extra

  ;; Do our update if major property has changed
  if need_update then begin
     self->update, undo
     return
  endif ;; updating because of change of property

  ;; If we made it here, nothing major has changed, but the plot
  ;; window(s) and associated widgets might need to be refreshed.
  if keyword_set(property_set) then begin
     self->replot
     self->pfo_obj_cw_obj::refresh
  endif

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
      METHODS	: ['Xaxis()', 'Yaxis()', 'ROI_Xin_idx()', 'deviates()', 'dXaxis_dXin()', 'invalidate_cache', 'print']} $
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
  ok = self->pfo_plot_obj::init(_EXTRA=extra)
  if NOT ok then return, 0
  ok = self->pfo_data_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then return, 0
  ok = self->pfo_parinfo_obj::init(_EXTRA=extra)
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
