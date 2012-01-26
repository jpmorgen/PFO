;+
; NAME: pfo_oplot_tROI
;
; PURPOSE: Overplots temporary region of interest (tROI) which can be
; used by zoom and ROI routines.
;
; CATEGORY: PFO Plotting
;
; CALLING SEQUENCE: This routine is intended to be called only from
; within pfo_obj->plot
;
; DESCRIPTION: The PFO plotting system works partially within the
; pfo_obj (see pfo_plot_obj__define) and partially with external oplot
; procedures, such as this one.  The idea is that pfo_obj->plot sets
; up the axes, titles, etc., and the oplot routines put the data,
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

;   oplot_data_psym=oplot_data_psym, $ ;; psym to use for data (default==!tok.hist)
;   oplot_tROI_extra=oplot_tROI_extra ;; a structure containing arguments to be passed to oplot (or other routines) in this routine
; 

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
; $Id: pfo_oplot_troi.pro,v 1.1 2012/01/26 16:22:11 jpmorgen Exp $
;
; $Log: pfo_oplot_troi.pro,v $
; Revision 1.1  2012/01/26 16:22:11  jpmorgen
; Initial revision
;
;
;-
pro pfo_oplot_tROI, $
   Xin=Xin, $ ;; Xin from pfo_obj passed by reference to save memory
   xaxis=xaxis, $ ;; X-axis from pfo_obj->plot, so xaxes lines up
   Xunits=Xunits, $ ;; determines if Xaxis reads in Xin or Xaxis
   Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
   params=params, $ ;; params to use in preference to encapsulated parinfo.value
   idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
   ispec=ispec, $ ;; ispec(s) to plot (oplot routine should loop over these, as appropriate)
   iROI=iROI, $ ;; iROI(s) to plot (oplot routine should loop over these, as appropriate)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, parinfo, etc.
   calc_args=calc_args, $ ;; arguments to self->Xaxis() which eventually get passed down to __calc "methods" of pfo functions
   oplot_data_psym=oplot_data_psym, $ ;; psym to use for data (default==!tok.hist)
   oplot_tROI_thick=oplot_tROI_thick, $ ;; thickness of tROI lines (default = !p.thick+!pfo.oplot_tROI_thick_boost)
   oplot_tROI_color=oplot_tROI_color, $ ;; color of the tROI lines (default = !pfo.oplot_tROI_color)
   oplot_tROI_extra=oplot_tROI_extra, $ ;; a structure containing arguments to be passed to oplot (or other routines) in this routine
   tROI=tROI, $ ;; tROI to plot
   _REF_EXTRA=extra ;; catchall in case other arguments are thrown at us

  ;; Make sure system variables are defined
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Get our default yaxis
  yaxis = pfo_obj->Yin()

  ;; Now adjust it for the proper units
  if Xunits eq !pfo.Xaxis and Yunits eq !pfo.Xaxis then $
     yaxis = yaxis / self->Xaxis_dXin(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)

  ;; Handle our customizations
  if N_elements(oplot_tROI_psym) eq 0 then oplot_tROI_psym = !tok.hist
  if N_elements(oplot_tROI_thick) eq 0 then oplot_tROI_thick = !p.thick+!pfo.oplot_tROI_thick_boost
  if N_elements(oplot_tROI_color) eq 0 then oplot_tROI_color = !pfo.oplot_tROI_color

  ;; Assuming !pfo.color_table=38, convert the number on our color
  ;; table to color index.  Subtracting 1 from the table size lets us
  ;; get black with parinfo_color=0 and white with
  ;; parinfo_color=!pfo.n_colors
  color = float(oplot_tROI_color)/!pfo.n_colors*(!d.table_size - 1)

  ;; Figure out what to oplot.  If we are in the range of the data,
  ;; oplot on top of that
  good_idx = where(tROI[0] le Xin and Xin le tROI[1], count)
  if count gt 0 then begin
     oplot, xaxis[good_idx], yaxis[good_idx], psym=oplot_tROI_psym, thick=oplot_tROI_thick, color=color, _EXTRA=oplot_tROI_extra
  endif


end
