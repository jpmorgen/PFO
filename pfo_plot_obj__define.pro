;+
; NAME: pfo_plot_obj__define
;
; PURPOSE: Define the PFO plot object
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
; $Id: pfo_plot_obj__define.pro,v 1.3 2011/08/02 18:19:41 jpmorgen Exp $
;
; $Log: pfo_plot_obj__define.pro,v $
; Revision 1.3  2011/08/02 18:19:41  jpmorgen
; Release to Tom
; Check for undefined Xin and Yin for graceful exit
;
; Revision 1.2  2011/08/02 15:41:15  jpmorgen
; Release to Tom
; Improved property names, play with quick plotting of an intruding
; parinfo (seems to work!)
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

pro pfo_plot_obj::plot, $
   parinfo=parinfo_in, $ ;; parinfo to use in place of encapsulated parinfo (unusual)
   params=params, $ ;; params to use in preference to encapsulated parinfo.value
   idx=idx, $ ;; indices into parinfo  (unusual)
   ispec=ispec_in, $ ;; ispec(s) to plot 
   iROI=iROI_in, $ ;; iROI(s) to plot
   plot_window=plot_window, $ ;; IDL direct graphics window number into which to display plot
   xsize=xsize, $ ;; X-dimension of plot window (pixels)
   ysize=ysize, $ ;; Y-dimension of plot window (pixels)
   retain=retain, $ ;; retain keyword for IDL window command -- affects backing store
   PS_fname=PS_fname, $ ;; Postscript filename into which to direct output
   PS_charsize=PS_charsize, $ ;; charsize in PS plots
   TT_font_name=TT_font_name, $ ;; TrueType font name
   no_deviates=no_deviates, $ ;; omit deviates frame
   Xin_range=Xin_range    , $  ;; plot range in Xin units  
   Xaxis_range=Xaxis_range, $  ;; plot range in Xaxis units
   Yaxis_range=Yaxis_range, $  ;; plot range in Yaxis units
   Xunits=Xunits, $ ;; determines if X-axis reads in Xin or Xaxis
   Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
   Xin_title   = Xin_title, $   ;; title for X-axis plots when in Xin mode (e.g. channels)      
   Xaxis_title = Xaxis_title, $ ;; title for X-axis plots when in Xaxis mode (e.g. keV)         
   Yin_Xin_title= Yin_Xin_title, $  ;; title for Y-axis plots when Yunits=!pfo.Xin regardless of Xunits value (e.g. counts/channel)
   Yin_Xaxis_title= Yin_Xaxis_title, $ ;; title for X-axis plots when Xunits=!pfo.Xaxis and Yunits=!pfo.Xaxis (e.g. counts/keV)
   plot_xlog=plot_xlog, $	;; main window and deviates reads in log X
   plot_ylog=plot_ylog, $ ;; plot of main window reads in log Y.  --> Deviates always linear
   oplot_call_list=oplot_call_list, $ ;; a list of procedures which will overplot information on the main plot window
   calc_args=calc_args, $ ;; arguments to self->[XY]axis(), etc. which eventually get passed down to __calc "methods" of pfo functions
   _REF_EXTRA=extra ;; EXTRA args passed to oplot routines

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH invocation errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: Caught the above error, returning with whatever I have done so far.', /CONTINUE
        ;; Check to see if we mucked with our encapsulated parinfo.
        ;; If so, put it back
        if N_elements(Eparinfo) ne 0 then begin
           parinfo_in = temporary(*self.pparinfo)
           *self.pparinfo = temporary(Eparinfo)
           ;; Make sure to invalidate any caches that were made with
           ;; that parinfo
           self->invalidate_cache
        endif
        ;; Close down our postscript file, if open
        if keyword_set(PS_fname) then begin
           device, /close
           ;; Set things back to the previous state
           set_plot, device_name
           !p.background = background
           !p.color = color
           !p.font = font
           !p.charsize = charsize
           ;; Reset our PS fname
           self.plot_PS_fname = ''
        endif ;; PS processing
        return
     endif ;; catch
  endif ;; not debugging

  ;; Sometimes it is handy to quickly plot a parinfo stored outside
  ;; the object for comparison purposes.  To enable that, we just save
  ;; off our encapsulated parinfo, put the command line version in and
  ;; put it all back when we are done
  if N_elements(parinfo_in) ne 0 then begin
     Eparinfo = temporary(*self.pparinfo)
     *self.pparinfo = temporary(parinfo_in)
     self->invalidate_cache
  endif

  ;; Check to see if we have anything to plot
  if N_elements(*self.pXin) eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: Xin not defined.  Cannot plot data.'
     return
  endif
  if N_elements(*self.pYin) eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: Yin not defined.  Cannot plot data.'
     return
  endif

  ;; Grab defaults from our property, if not specified on command line

  ;; Make sure we have local copies of ispec and iROI so we
  ;; don't mess with the calling routine's ispec and iROI
  if N_elements(ispec_in) ne 0 then $
     ispec = ispec_in
  if N_elements(ispec) eq 0 then $
     if N_elements(*self.pplot_ispec) ne 0 then $
        ispec = *self.pplot_ispec
  if N_elements(iROI_in) ne 0 then $
     iROI = iROI_in
  if N_elements(iROI) eq 0 then $
     if N_elements(*self.pplot_iROI) ne 0 then $
        iROI = *self.pplot_iROI

  if N_elements(plot_window) eq 0 then plot_window = self.plot_window 
  if N_elements(xsize) eq 0 then xsize = self.plot_xsize 
  if N_elements(ysize) eq 0 then ysize = self.plot_ysize 
  if N_elements(retain) eq 0 then retain = self.plot_retain
  if N_elements(PS_fname) eq 0 then PS_fname = self.plot_PS_fname
  if N_elements(PS_charsize) eq 0 then PS_charsize = self.plot_PS_charsize
  if N_elements(TT_font_name) eq 0 then TT_font_name = self.plot_TT_font_name

  if N_elements(Xunits) eq 0 then Xunits = self.plot_Xunits
  if N_elements(Yunits) eq 0 then Yunits = self.plot_Yunits

  ;; Check for undefined heap variables and calculate ranges if necessary
  if N_elements(Xin_range) eq 0 then begin
     if N_elements(*self.pXin_range) eq 0 then begin
        Xin_range = minmax(*self.pXin, /NAN)
     endif else begin
        Xin_range = *self.pXin_range 
     endelse 
  endif
  if N_elements(Xaxis_range) eq 0 then begin
     if N_elements(*self.pXaxis_range) eq 0 then begin
        ;; Start a local cache of Xaxis
        if N_elements(Xaxis) eq 0 then $
           Xaxis = self->Xaxis(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)
        Xaxis_range = minmax(Xaxis, /NAN)
     endif else begin
        Xaxis_range = *self.pXaxis_range 
     endelse 
  endif
  if N_elements(Yaxis_range) eq 0 then begin
     if N_elements(*self.pYaxis_range) eq 0 then begin
        ;; This ends up being our autoscaling code
        count = 0
        if Xunits eq !pfo.Xin then begin
           good_Xin_idx = where(Xin_range[0] le *self.pXin and $
                                *self.pXin le Xin_range[1], count)
        endif 
        if Xunits eq !pfo.Xaxis then begin
           if N_elements(Xaxis) eq 0 then $
              Xaxis = self->Xaxis(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)
           good_Xin_idx = where(Xaxis_range[0] le Xaxis and $
                                Xaxis le Xaxis_range[1], count)
        endif 
        if count eq 0 then $
           message, 'ERROR: no valid points to plot in Xin_range = [' + strtrim(Xin_range[0], 2) + ', ' + strtrim(Xin_range[1]) + ']'
        Yaxis_range = minmax((*self.pYin)[good_Xin_idx], /NAN)
     endif else begin
        Yaxis_range = *self.pYaxis_range 
     endelse 
  endif

  if N_elements(Xin_title) eq 0 then Xin_title = self.plot_Xin_title
  if N_elements(Xaxis_title) eq 0 then Xaxis_title = self.plot_Xaxis_title
  if N_elements(Yin_Xin_title) eq 0 then Yin_Xin_title = self.plot_Yin_Xin_title
  if N_elements(Yin_Xaxis_title) eq 0 then Yin_Xaxis_title = self.plot_Yin_Xaxis_title
  if N_elements(xlog) eq 0 then xlog = self.plot_xlog
  if N_elements(ylog) eq 0 then ylog = self.plot_ylog
  if N_elements(oplot_call_list) eq 0 then oplot_call_list = *self.poplot_call_list

  ;; COLOR TABLE
  ;; Be polite with the color table, saving off previous value, to
  ;; restore below
  tvlct, user_r, user_g, user_b, /get
  ;; Set color translation table to rainbow18 (38), which maps the has 18 very
  ;; distinguishable colors
  loadct, !pfo.color_table, /silent

  ;; Determine whether plot is going to PS device or a window
  if keyword_set(PS_fname) then begin
     device_name = !d.name
     background = !p.background
     color = !p.color
     ;; set_plot, 'ps' messes with the above (and possibly other)
     ;; system variables
     ;; Tom has preferences for these, so save them off too
     font = !p.font
     charsize = !p.charsize
     device, get_current_font=fontname

     ;; Tom likes the following for plots
     !p.font = 1 ;; TrueType
     !p.charsize = PS_charsize
     set_plot, 'ps'
     device, /portrait, filename=PS_fname, set_font=TT_font_name, $
             /encap, /color, bits=8
  endif else begin
     ;; Regular Xwindow or MS windows display
     device, get_decomposed=user_decomposed
     ;; Make sure our display can use pseudo colors, since we like
     ;; them better
     if (!d.n_colors gt 256) then $
        device, decomposed=0

     ;; Get our double buffer window ready.  This creates a pixmap,
     ;; not a real window on the display
     window, xsize=xsize, ysize=ysize, /pixmap, /free
     buffer_window = !D.window
  endelse ;; PS vs. regular window

  ;; Use the modes to set our xtitle, xaxis, etc.
  case Xunits of 
     !pfo.Xin: begin
        xtitle = Xin_title
        xaxis  = self->Xin()
        xrange = Xin_range 
        ytitle = Yin_Xin_title
     end
     !pfo.Xaxis: begin
        xtitle = Xaxis_title
        xaxis  = self->Xaxis(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)
        xrange = Xaxis_range 
        case Yunits of
           !pfo.Xin: ytitle = Yin_Xin_title
           !pfo.Xaxis: ytitle = Yin_Xaxis_title
           else: message, 'ERROR: invalid Yunits value: ' + strtrim(Xunits, 2) + ' expecting !pfo.Xin or !pfo.Xaxis'
        endcase
     end
     else: message, 'ERROR: invalid Xunits value: ' + strtrim(Xunits, 2) + ' expecting !pfo.Xin or !pfo.Xaxis'
  endcase

  ;; In case we are not plotting the deviates
  main_position = [0.1,0.08, 0.98, 0.95]
  main_xtitle = xtitle
  main_xstyle = !tok.exact

  ;; Do the deviates, if desired
  if NOT keyword_set(no_deviates) then begin
     ;; Since we are doing deviates, we know where the main window
     ;; has to be and how the xtitle should look (none)
     main_position = [0.1,0.25, 0.98, 0.95]
     main_xtitle = ''
     main_xstyle = !tok.exact+!tok.no_axes

     ;; Grab our deviates, which may already be cached and find the
     ;; yrange value for the active ROIs
     deviates = self->deviates(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)
     yrange = minmax(deviates[self->ROI_Xin_idx(params=params, idx=idx, ispec=ispec, iROI=iROI, _EXTRA=calc_args)], /nan)

     resid_position = [0.1,0.08, 0.98, 0.25]
     plot, [0], $
           xtitle=xtitle, $
           ytitle='Deviates', $
           xlog=xlog, $
           xstyle=!tok.exact, ystyle=!tok.exact+!tok.extend, $
           xticklen=!p.ticklen*4, $ ;; small plot makes ticks too small otherwise
           xrange=xrange, yrange=yrange, $
           position=resid_position

     oplot, xaxis, deviates, psym=!tok.hist

  endif ;; deviates plot

  ;; Main plot.  Just make the axes at this point and do everything
  ;; (even the data) with oplot
  plot, [0], $
        xtitle=main_xtitle, $
        ytitle=ytitle, $
        xstyle=main_xstyle, ystyle=!tok.exact+!tok.extend, $
        xlog=xlog, ylog=ylog, $
        xrange=xrange, yrange=Yaxis_range, $
        position=main_position, $
        /noerase
  ;; Put an axis on the top with tick marks pointing down
  axis, xaxis=1, xstyle=!tok.exact

  ;; We are now done setting up our plot axes.

  ;; Call our list of oplot procedures
  for ip=0,N_elements(oplot_call_list)-1 do begin
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging and
     ;; just continue to the next oplot
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Skipping ' + oplot_call_list[ip] + '.  pfo_debug may be helpful in debugging', /CONTINUE
           CONTINUE
        endif
     endif ;; not debugging
     call_procedure, $
        oplot_call_list[ip], $ ;; oplot procedure name
        Xin=*self.pXin, $ ;; Xin from pfo_obj passed by reference to save memory
        xaxis=xaxis, $ ;; X-axis from pfo_obj->plot, so xaxes lines up
        Xunits=Xunits, $ ;; determines if Xaxis reads in Xin or Xaxis
        Yunits=Yunits, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
        params=params, $ ;; params to use in preference to encapsulated parinfo.value
        idx=idx, $ ;; idx into parinfo for which plot is desired (unusual)
        ispec=ispec, $ ;; ispec(s) to plot
        iROI=iROI, $ ;; iROI(s) to plot
        pfo_obj=self, $ ;; pfo_obj encapsulating data, parinfo, etc.
        calc_args=calc_args, $ ;; arguments to self->[XY]axis(), etc. which eventually get passed down to __calc "methods" of pfo functions
        _EXTRA=extra ;; EXTRA args passed to oplot routines
  endfor ;; each oplot procedure

  ;; Finish up 
  if keyword_set(PS_fname) then begin
     ;; Close PS file
     device, /close
     ;; Set things back to the previous state
     set_plot, device_name
     !p.background = background
     !p.color = color
     !p.font = font
     !p.charsize = charsize
     ;; Reset our PS fname
     self.plot_PS_fname = ''
  endif else begin 
     ;; Put everything from our buffer into our real window window.
     ;; Put in a catch in case this this the first time we are doing
     ;; the plot
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        window, plot_window, retain=retain, xsize=xsize, ysize=ysize
     endif ;; Catching no plot_window open yet
     ;; The wset will raise an error if the window is not open
     ;; (e.g. ->plot not issued from a widget)
     wset, plot_window
     device, copy=[0,0,!D.X_size, !D.Y_size, 0, 0, buffer_window]
     wdelete, buffer_window
     ;; Put our decomposed state back
     device, decomposed=user_decomposed
  endelse ;; PS vs regular windows

  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b

  ;; Return our encapsulated and command-line parinfos to their
  ;; original state.
  if N_elements(Eparinfo) ne 0 then begin
     parinfo_in = temporary(*self.pparinfo)
     *self.pparinfo = temporary(Eparinfo)
     ;; Make sure to invalidate any caches that were made with
     ;; that parinfo
     self->invalidate_cache
  endif


end

pro pfo_plot_obj::get_property, $
      plot_window	= plot_window      , $
      plot_xsize	= plot_xsize       , $
      plot_ysize	= plot_ysize       , $
      plot_retain	= plot_retain      , $
      plot_PS_fname	= plot_PS_fname    , $
      plot_PS_charsize  = plot_PS_charsize , $
      plot_TT_font_name	= plot_TT_font_name, $
      plot_win_charsize	= plot_win_charsize, $
      plot_Xin_range    = plot_Xin_range      , $
      plot_Xaxis_range  = plot_Xaxis_range    , $
      plot_Yaxis_range  = plot_Yaxis_range    , $
      plot_Xunits     	= plot_Xunits    , $
      plot_Yunits     	= plot_Yunits    , $
      plot_xlog	      	= plot_xlog      , $      
      plot_ylog	      	= plot_ylog      , $      
      plot_Xin_title	= plot_Xin_title      , $      
      plot_Xaxis_title  = plot_Xaxis_title    , $
      plot_Yin_Xin_title= plot_Yin_Xin_title  , $
      plot_Yin_Xaxis_title= plot_Yin_Xaxis_title, $
      plot_ispec      	= plot_ispec     , $
      plot_iROI	      	= plot_iROI      , $      
      oplot_call_list  	= oplot_call_list

  if arg_present(plot_window      ) or N_elements(plot_window      ) gt 0 then plot_window      = self.plot_window      
  if arg_present(plot_xsize       ) or N_elements(plot_xsize       ) gt 0 then plot_xsize       = self.plot_xsize       
  if arg_present(plot_ysize       ) or N_elements(plot_ysize       ) gt 0 then plot_ysize       = self.plot_ysize       
  if arg_present(plot_retain      ) or N_elements(plot_retain      ) gt 0 then plot_retain      = self.plot_retain      
  if arg_present(plot_PS_fname    ) or N_elements(plot_PS_fname    ) gt 0 then plot_PS_fname    = self.plot_PS_fname    
  if arg_present(plot_PS_charsize ) or N_elements(plot_PS_charsize ) gt 0 then plot_PS_charsize = self.plot_PS_charsize 
  if arg_present(plot_TT_font_name) or N_elements(plot_TT_font_name) gt 0 then plot_TT_font_name= self.plot_TT_font_name
  if arg_present(plot_win_charsize) or N_elements(plot_win_charsize) gt 0 then plot_win_charsize= self.plot_win_charsize
  if arg_present(plot_Xin_range   ) or N_elements(plot_Xin_range   ) gt 0 then plot_Xin_range   = *self.pXin_range        
  if arg_present(plot_Xaxis_range ) or N_elements(plot_Xaxis_range ) gt 0 then plot_Xaxis_range = *self.pXaxis_range      
  if arg_present(plot_Yaxis_range ) or N_elements(plot_Yaxis_range ) gt 0 then plot_Yaxis_range = *self.pYaxis_range      
  if arg_present(plot_Xunits      ) or N_elements(plot_Xunits      ) gt 0 then plot_Xunits      = self.plot_Xunits      
  if arg_present(plot_Yunits      ) or N_elements(plot_Yunits      ) gt 0 then plot_Yunits      = self.plot_Yunits      
  if arg_present(plot_xlog	  ) or N_elements(plot_xlog	   ) gt 0 then plot_xlog	= self.plot_xlog	       
  if arg_present(plot_ylog	  ) or N_elements(plot_ylog	   ) gt 0 then plot_ylog	= self.plot_ylog	       
  if arg_present(plot_Xin_title	  ) or N_elements(plot_Xin_title   ) gt 0 then plot_Xin_title	= self.plot_Xin_title	       
  if arg_present(plot_Xaxis_title ) or N_elements(plot_Xaxis_title ) gt 0 then plot_Xaxis_title = self.plot_Xaxis_title      
  if arg_present(plot_Yin_Xin_titl) or N_elements(plot_Yin_Xin_title) gt 0 then plot_Yin_Xin_title = self.plot_Yin_Xin_title    
  if arg_present(plot_Yin_Xaxis_title) or N_elements(plot_Yin_Xaxis_title) gt 0 then plot_Yin_Xaxis_title= self.plot_Yin_Xaxis_title  
  if arg_present(plot_ispec       ) or N_elements(plot_ispec       ) gt 0 then plot_ispec       = *self.pplot_ispec       
  if arg_present(plot_iROI	  ) or N_elements(plot_iROI	   ) gt 0 then plot_iROI	= *self.pplot_iROI	       
  if arg_present(oplot_call_list  ) or N_elements(oplot_call_list  ) gt 0 then oplot_call_list  = *self.poplot_call_list  

end

pro pfo_plot_obj::set_property, $
      plot_window	= plot_window      , $
      plot_xsize	= plot_xsize       , $
      plot_ysize	= plot_ysize       , $
      plot_retain	= plot_retain      , $
      plot_PS_fname	= plot_PS_fname    , $
      plot_PS_charsize  = plot_PS_charsize , $
      plot_TT_font_name	= plot_TT_font_name, $
      plot_win_charsize	= plot_win_charsize, $
      plot_Xin_range    = plot_Xin_range      , $
      plot_Xaxis_range  = plot_Xaxis_range    , $
      plot_Yaxis_range  = plot_Yaxis_range    , $
      plot_Xunits     	= plot_Xunits    , $
      plot_Yunits     	= plot_Yunits    , $
      plot_xlog	      	= plot_xlog      , $      
      plot_ylog	      	= plot_ylog      , $      
      plot_Xin_title	= plot_Xin_title      , $      
      plot_Xaxis_title  = plot_Xaxis_title    , $
      plot_Yin_Xin_title= plot_Yin_Xin_title  , $
      plot_Yin_Xaxis_title= plot_Yin_Xaxis_title, $
      plot_ispec      	= plot_ispec     , $
      plot_iROI	      	= plot_iROI      , $      
      oplot_call_list  	= oplot_call_list

  if N_elements(plot_window      ) gt 0 then self.plot_window       = plot_window      
  if N_elements(plot_xsize       ) gt 0 then self.plot_xsize        = plot_xsize       
  if N_elements(plot_ysize       ) gt 0 then self.plot_ysize        = plot_ysize       
  if N_elements(plot_retain      ) gt 0 then self.plot_retain       = plot_retain      
  if N_elements(plot_PS_fname    ) gt 0 then self.plot_PS_fname     = plot_PS_fname    
  if N_elements(plot_PS_charsize ) gt 0 then self.plot_PS_charsize  = plot_PS_charsize 
  if N_elements(plot_TT_font_name) gt 0 then self.plot_TT_font_name = plot_TT_font_name
  if N_elements(plot_win_charsize) gt 0 then self.plot_win_charsize = plot_win_charsize
  if N_elements(plot_Xin_range   ) gt 0 then *self.pXin_range       = plot_Xin_range        
  if N_elements(plot_Xaxis_range ) gt 0 then *self.pXaxis_range     = plot_Xaxis_range      
  if N_elements(plot_Yaxis_range ) gt 0 then *self.pYaxis_range     = plot_Yaxis_range      
  if N_elements(plot_Xunits      ) gt 0 then self.plot_Xunits       = plot_Xunits      
  if N_elements(plot_Yunits      ) gt 0 then self.plot_Yunits       = plot_Yunits      
  if N_elements(plot_xlog        ) gt 0 then self.plot_xlog	    = plot_xlog	       
  if N_elements(plot_ylog        ) gt 0 then self.plot_ylog	    = plot_ylog	       
  if N_elements(plot_Xin_title   ) gt 0 then self.plot_Xin_title	    = plot_Xin_title	       
  if N_elements(plot_Xaxis_title ) gt 0 then self.plot_Xaxis_title       = plot_Xaxis_title      
  if N_elements(plot_Yin_Xin_title) gt 0 then self.plot_Yin_Xin_title    = plot_Yin_Xin_title    
  if N_elements(plot_Yin_Xaxis_title) gt 0 then self.plot_Yin_Xaxis_title= plot_Yin_Xaxis_title  
  if N_elements(plot_ispec       ) gt 0 then *self.pplot_ispec      = plot_ispec       
  if N_elements(plot_iROI	 ) gt 0 then *self.pplot_iROI	    = plot_iROI	       
  if N_elements(oplot_call_list  ) gt 0 then *self.poplot_call_list = oplot_call_list  

end


;; Each inherited class should have a descr method.
function pfo_plot_obj::descr

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
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_plot_obj::cleanup
  ptr_free, self.ppfo_plot_obj_descr
  ptr_free, self.pXin_range  
  ptr_free, self.pXaxis_range
  ptr_free, self.pYaxis_range
  ptr_free, self.pplot_ispec
  ptr_free, self.pplot_iROI
  ptr_free, self.poplot_call_list

end

;; There is a lot of plot property.  Be lazy and assume there will be
;; no wierd cross-talk with other inits
function pfo_plot_obj::init, $
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

  self.ppfo_plot_obj_descr $
     = ptr_new( $
     {README	: 'pfo_plot_obj encapsulates all of the information necessary for plotting PFO functions.  It is intended to be inherited along with pfo_calc_obj.', $
      METHODS	: 'plot'} $
              )

  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_plot_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_plot_obj_descr, $
                        {PROPERTY: property[good_idx]}

  ;; Initialize our non-pointer property
  self.plot_window = !pfo.plotwin
  self.plot_xsize = 640
  self.plot_ysize = 512
  self.plot_retain = 2 ;; IDL does the backing store

  self.plot_Xunits = !pfo.Xin
  self.plot_Yunits = !pfo.Xin
  self.plot_PS_charsize = 0.9
  self.plot_win_charsize = 1
  self.plot_TT_font_name = 'Times*24'

  self.plot_Xin_title       = !pfo.Xin_title      
  self.plot_Xaxis_title     = !pfo.Xaxis_title    
  self.plot_Yin_Xin_title   = !pfo.Yin_Xin_title  
  self.plot_Yin_Xaxis_title = !pfo.Yin_Xaxis_title

  ;; Turn our null reference pointers into undefined variables
  self.pXin_range	= ptr_new(/allocate_heap) 
  self.pXaxis_range	= ptr_new(/allocate_heap) 
  self.pYaxis_range	= ptr_new(/allocate_heap) 
  self.pplot_ispec	= ptr_new(/allocate_heap) 
  self.pplot_iROI	= ptr_new(/allocate_heap) 

  ;; Initialize our oplot_call_list to a reasonable set of things
  self.poplot_call_list = ptr_new(['pfo_oplot_data', 'pfo_oplot_parinfo'])

  ;; Call our set_property routine to convert any keywords to property
  self->set_property, _EXTRA=extra

  return, 1

end

pro pfo_plot_obj__define
  ;; Define our pfo_oplot_obj first.  !pfo.pfo_oplot_obj_file should
  ;; define pfo_oplot_obj.  Doing it this way lets us modify the
  ;; default pfo_oplot_obj
  
  objectClass = $
     {pfo_plot_obj, $
      ppfo_plot_obj_descr: ptr_new(), $  ;; Pointer to description structure
      plot_window	: 0, $ ;; window number into which plot will be 
      plot_xsize	: 0, $ ;; X-dimension of plot window (pixels)
      plot_ysize	: 0, $ ;; Y-dimension of plot window (pixels
      plot_retain	: 0, $ ;; 
      plot_PS_fname	: '', $ ;; postscript output filename
      plot_PS_charsize  : 0., $ ;; charsize in PS plots
      plot_TT_font_name	: '', $ ;; TrueType font name
      plot_win_charsize	: 0., $ ;; charsize in regular graphics windows
      pXin_range	: ptr_new(), $  ;; Pointer to plot range in Xin units
      pXaxis_range	: ptr_new(), $  ;; Pointer to plot range in Xaxis units
      pYaxis_range	: ptr_new(), $  ;; Pointer to plot range in Yaxis units
      plot_Xunits	: 0B, $ ;; determines if Xaxis reads in Xin or Xaxis
      plot_Yunits	: 0B, $ ;; When X-axis reads in Xaxis, determines if Yaxis reads in Yin or Yin/(dXaxis/dXin)
      plot_xlog		: 0B, $ ;; xlog keyword to plot
      plot_ylog		: 0B, $ ;; ylog keyword to plot (main window only)
      plot_Xin_title		: '', $ ;; title for X-axis plots when in Xin mode (e.g. channels)
      plot_Xaxis_title	: '', $ ;; title for X-axis plots when in Xaxis mode (e.g. keV)
      plot_Yin_Xin_title	: '', $ ;; title for Y-axis plots when Yunits=!pfo.Xin regardless of Xunits value (e.g. counts/channel)
      plot_Yin_Xaxis_title	: '', $ ;; title for X-axis plots when Xunits=!pfo.Xaxis and Yunits=!pfo.Xaxis (e.g. counts/keV)
      pplot_ispec	: ptr_new(), $ ;; pointer to (list of) ispecs to plot
      pplot_iROI	: ptr_new(), $ ;; pointer to (list of) iROIs to plot
      poplot_call_list	: ptr_new() $ ;; a list of procedures which will overplot information on the main plot window
     }
      
end
