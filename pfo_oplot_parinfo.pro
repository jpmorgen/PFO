;+
; NAME: pfo_oplot_parinfo
;
; PURPOSE: Overplots the function defined by a parinfo onto an already existing set
; of axes
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
;   parinfo=parinfo, $ ;; parinfo array defining function to be plotted
;   params=params, $ ;; params to use in preference to encapsulated parinfo.value
;   idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
;   ispec=ispec, $ ;; ispec(s) to plot (oplot routine should loop over these, as appropriate)
;   iROI=iROI, $ ;; iROI(s) to plot (oplot routine should loop over these, as appropriate)
;   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, parinfo, etc.
;   calc_args=calc_args, $ ;; arguments to self->[XY]axis(), etc. which eventually get passed down to __calc "methods" of pfo functions

;   Other keywords are optional and handled by the _REF_EXTRA
;   mechanism in the oplot routines.  For this routine, that includes: 

;   parinfo_color=parinfo_color, $ ;; color to use for parinfo (default=!pfo.parinfo_color)
;   parinfo_extra=parinfo_extra ;; a structure containing arguments to be passed to oplot (or other routines) in this routine
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
; $Id: pfo_oplot_parinfo.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_oplot_parinfo.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_oplot_parinfo, $
   Xin=Xin, $ ;; Xin from pfo_obj passed by reference to save memory
   xaxis=xaxis, $ ;; X-axis from pfo_obj->plot, so xaxes lines up
   Xunits=Xunits, $ ;; determines if Xaxis reads in Xin or Xaxis
   Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
   parinfo=parinfo, $ ;; parinfo array defining function to be plotted
   params=params, $ ;; params to use in preference to encapsulated parinfo.value
   idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
   ispec=ispec, $ ;; ispec(s) to plot (oplot routine should loop over these, as appropriate)
   iROI=iROI, $ ;; iROI(s) to plot (oplot routine should loop over these, as appropriate)
   pfo_obj=pfo_obj, $ ;; pfo_obj encapsulating data, parinfo, etc.
   calc_args=calc_args, $ ;; arguments to self->Xaxis() which eventually get passed down to __calc "methods" of pfo functions
   oplot_parinfo_color=oplot_parinfo_color, $ ;; color to use for parinfo (default=!pfo.parinfo_color)
   oplot_parinfo_thick=oplot_parinfo_thick, $ ;; thickness to use for parinfo (default = !p.thick+!pfo.oplot_parinfo_thick_boost)
   oplot_parinfo_extra=oplot_parinfo_extra ;; a structure containing arguments to be passed to oplot (or other routines) in this routine

  init = {tok_sysvar}

  ;; Get our default color
  if N_elements(oplot_parinfo_color) eq 0 then oplot_parinfo_color=!pfo.oplot_parinfo_color
  if N_elements(oplot_parinfo_thick) eq 0 then oplot_parinfo_thick = !p.thick+!pfo.oplot_parinfo_thick_boost

  ;; Get our default yaxis
  Yaxis  = pfo_obj->Yaxis(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)

  ;; Now adjust it for the proper units
  if Xunits eq !pfo.Xaxis and Yunits eq !pfo.Xaxis then $
     Yaxis = Yaxis / pfo_obj->dXaxis_dXin(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)

  ;; Assuming !pfo.color_table=38, convert the number on our color
  ;; table to color index.  Subtracting 1 from the table size lets us
  ;; get black with parinfo_color=0 and white with
  ;; parinfo_color=!pfo.n_colors
  color = float(oplot_parinfo_color)/!pfo.n_colors*(!d.table_size - 1)

  oplot, xaxis, yaxis, color=color, thick=oplot_parinfo_thick, _EXTRA=oplot_parinfo_extra

end
