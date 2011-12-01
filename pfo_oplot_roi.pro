;+
; NAME: pfo_oplot_ROI
;
; PURPOSE: Overplots ROIs in their various colors on the data 
;
; CATEGORY: PFO Plotting
;
; CALLING SEQUENCE: This routine is intended to be called only from
; within pfo_obj->plot
;
; DESCRIPTION: The PFO plotting system works partially within the
; pfo_obj (see pfo_plot_obj__define) and partially with external oplot
; procedures, such as this one.  The idea is that pfo_obj->plot sets
; up the axes, titles, etc., and the oplot routines put the parinfo,
; function(s), ROIs, etc. on the plot.  

; The oplot procedures are not encapsulated within the pfo_obj in
; order to allow expansion without conflict in object class name
; space.  If you want to manage property relevant to an oplot
; procedure, do so in the object that encapsulates pfo_obj and call
; the pfo_obj->plot procedure with appropriate keywords.  Use this 

; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

; Note that all pfo_oplot_* procedures called by pfo_oplot_obj must
; accept these keywords or otherwise handle them via the [_REF]_EXTRA
; mechanism

;   Xin=Xin, $ ;; Xin from pfo_obj passed by reference to save memory
;   xaxis=xaxis, $ ;; X-axis from pfo_obj->plot, so xaxes lines up
;   Xunits=Xunits, $ ;; determines if Xaxis reads in Xin or Xaxis
;   Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
;   params=params, $ ;; params to use in preference to encapsulated parinfo.value
;   idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
;   ispec=ispec, $ ;; ispec(s) to plot (oplot routine should loop over these, as appropriate)
;   iROI=iROI, $ ;; iROI(s) to plot (oplot routine should loop over these, as appropriate)
;   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, parinfo, etc.
;   calc_args=calc_args, $ ;; arguments to self->[XY]axis(), etc. which eventually get passed down to __calc "methods" of pfo functions

;   Other keywords are optional and handled by the _REF_EXTRA
;   mechanism in the oplot routines.  For this routine, that includes: 


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
; $Id: pfo_oplot_roi.pro,v 1.5 2011/12/01 22:12:49 jpmorgen Exp $
;
; $Log: pfo_oplot_roi.pro,v $
; Revision 1.5  2011/12/01 22:12:49  jpmorgen
; Added better init
;
; Revision 1.4  2011/11/18 16:10:59  jpmorgen
; Fix minor bug
;
; Revision 1.3  2011/09/08 20:15:21  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.2  2011/08/02 15:43:39  jpmorgen
; Release to Tom
; Removed parinfo argument to guarantee encapsulated parinfo is used
;
; Revision 1.1  2011/08/01 18:26:21  jpmorgen
; Initial revision
;
;-
pro pfo_oplot_ROI, $
   Xin=Xin, $ ;; Xin from pfo_obj passed by reference to save memory
   xaxis=xaxis, $ ;; X-axis from pfo_obj->plot, so xaxes lines up
   Xunits=Xunits, $ ;; determines if Xaxis reads in Xin or Xaxis
   Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
   params=params, $ ;; params to use in preference to encapsulated parinfo.value
   idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
   ispec=ispec, $ ;; ispec(s) to plot (oplot routine should loop over these, as appropriate)
   iROI=iROI, $ ;; iROI(s) to plot (oplot routine should loop over these, as appropriate)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, parinfo, etc.
   calc_args=calc_args, $ ;; arguments to self->[XY]axis() et al. which eventually get passed down to __calc "methods" of pfo functions
   oplot_ROI_avoid_color=oplot_ROI_avoid_color, $ ;; colors to avoid
   oplot_ROI_psym=oplot_ROI_psym, $ ;; psym to use for ROI overplots (default = histogram or none (just a line))
   oplot_ROI_thick=oplot_ROI_thick, $ ;; thickness of ROI lines (default = !p.thick+!pfo.oplot_ROI_thick_boost)
   oplot_ROI_allROI_thick=oplot_ROI_allROI_thick, $ ;; thickness of the allROI lines (default = !p.thick+!pfo.oplot_ROI_allROI_thick_boost)
   oplot_ROI_allROI_color=oplot_ROI_allROI_color, $ ;; color of the allROI lines (default = !pfo.oplot_ROI_allROI_color)
   oplot_ROI_extra=oplot_ROI_extra

  ;; Make sure system variables are defined
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Check to see if we have any ROIs to overplot.  If so, ROI_idx
  ;; contains the indices into parinfo of just the ROI functions
  ;; (optionally limited to idx.  We will be working with ROI_idx
  ;; until we want to calculate things like Yaxis or Xin_ROI_idx,
  ;; which might required other parameters
  ROI_idx = pfo_obj->parinfo_call_function( $
            /no_update, 'pfo_fidx', 'pfo_ROI', idx=idx, nfunct=nfunct, pfo_obj=pfo_obj)
  if nfunct eq 0 then $
     return

  ;; If we made it here, we have ROIs to overplot.  We need to call
  ;; oplot one ROI at a time so each ROI can have its own distinct
  ;; color

  ;; Handle our customizations
  if N_elements(oplot_ROI_psym) eq 0 then oplot_ROI_psym = !tok.hist
  if N_elements(oplot_ROI_thick) eq 0 then oplot_ROI_thick = !p.thick+!pfo.oplot_ROI_thick_boost
  if N_elements(oplot_ROI_allROI_thick) eq 0 then oplot_ROI_allROI_thick = !p.thick+!pfo.oplot_ROI_allROI_thick_boost
  if N_elements(oplot_ROI_allROI_color) eq 0 then oplot_ROI_allROI_color = !pfo.oplot_ROI_allROI_color

  ;; The avoid color list would work better if we had an object that
  ;; registered the color property of each oplot 
  if N_elements(oplot_ROI_avoid_color) eq 0 then oplot_ROI_avoid_color = !pfo.oplot_parinfo_color
  ;; Add to that the allROI color we use here
  pfo_array_append, oplot_ROI_avoid_color, oplot_ROI_allROI_color
  ;; And 0 does not make a good color
  pfo_array_append, oplot_ROI_avoid_color, 0

  ;; Get our default yaxis
  Yaxis  = pfo_obj->Yin()

  ;; Now adjust it for the proper units
  if Xunits eq !pfo.Xaxis and Yunits eq !pfo.Xaxis then $
     Yaxis = Yaxis / pfo_obj->dXaxis_dXin(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)

  ;; Make a copy of ispec, tispec, that is local to this procedure.
  ;; If ispec is not defined, tispec is the list of ispecs in parinfo.
  ;; If ispec is defined, tispec is the subset of the the ispec(s) in
  ;; ispec found in parinfo[idx] (if any)
  ok = pfo_obj->parinfo_call_function( $
       /no_update, 'pfo_ROI_list', idx=ROI_idx, ispec_in=ispec, ispec_out=tispec, $
       N_ispec_out=N_tispec)

  ;; Cycle through each ispec.  The entire loop is skipped if our
  ;; input ispec(s) were not found in the parinfo
  for is=0, N_tispec-1 do begin
     ;; Use pfo_ROI_list to get all of the iROIs in this tispec[is]
     ;; that match the input iROI criterion (if any).
     ok  = pfo_obj->parinfo_call_function( $
           /no_update, 'pfo_ROI_list', idx=ROI_idx, ispec_in=tispec[is], iROI_in=iROI, $
           iROI_out=tiROI, N_iroi_out=N_tiROI)
     ;; Loop through any tiROI that we might have found.  This is the
     ;; meat of our code.
     for iR=0, N_tiROI-1 do begin
        ;; Get the parinfo idx of this ROI so we can work with colors
        tROI_idx = pfo_obj->parinfo_call_function( $
                   /no_update, 'pfo_ROI_idx', idx=ROI_idx, ispec_in=tispec[is], iROI=tiROI[iR], $
                   count=count)
        if count eq 0 then $
           message, 'ERROR: valid ispec/iROI found, but now there are no parinfo records.  This should not happen.'

        ;; The thickness of all of the ROIs will be the
        ;; same except allROI, which will be thinner
        thick = oplot_ROI_thick
        ;; Color from pfo_ROI tag.  Just grab the 0th ROI parameter
        pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, idx=tROI_idx[0], $
           taglist_series='pfo_ROI', ROI_color=color

        ;; If color is not set in the pfo_ROI tag, pick a color based
        ;; on iROI
        if color eq !tok.nowhere then begin
           ;; The Rainbow18 color table colors are a little close, so
           ;; make big jumps and allow ourselves to cycle around on
           ;; different colors.
           color = tiROI[iR]*5
        endif ;; default color
        ;; Handle avoid colors by just stepping up in color index.
        ;; This doesn't necessarily avoid identical colors once
        ;; we start wrapping our color index around.
        junk = where(color eq oplot_ROI_avoid_color, count)
        while count ne 0 do begin
           color += 1
           junk = where(color eq oplot_ROI_avoid_color, count)
        endwhile

        ;; Now that we are beyond our avoid_color code, check for our allROI
        if tiROI[iR] eq !pfo.allROI then begin
           thick = oplot_ROI_allROI_thick
           color = oplot_ROI_allROI_color
        endif ;; allROI

        ;; Assuming !pfo.color_table=38, convert the number on our color
        ;; table to color index.  Subtracting 1 from the table size lets us
        ;; get black with parinfo_color=0 and white with
        ;; parinfo_color=!pfo.n_colors
        color = float(color)/!pfo.n_colors*(!d.table_size - 1)
        
        ;; Now get our indices into xaxis and yaxis.  Make sure we use
        ;; the original idx rather than tROI_idx, since we might have
        ;; an Xin to Xaxis transformation to make.
        Xin_idx = pfo_obj->parinfo_call_function( $
                  /no_update, 'pfo_ROI_Xin_idx', Xin=Xin, params=params, idx=idx, $
                  ispec=tispec[is], iROI=tiROI[iR], $
                  pfo_obj=pfo_obj, _EXTRA=calc_args)

        ;; Handle case where there are no points in the ROI
        if N_elements(Xin_idx) eq 1 and Xin_idx[0] eq !tok.nowhere then $
           CONTINUE

        oplot, xaxis[Xin_idx], yaxis[Xin_idx], color=color, psym=oplot_data_psym, $
               thick=thick, _EXTRA=oplot_ROI_extra

     endfor ;; tiROI
  endfor ;; tispec


end
