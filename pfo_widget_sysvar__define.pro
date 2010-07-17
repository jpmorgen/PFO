;+
; NAME:
;
; PURPOSE:
;
; CATEGORY:
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
; $Id: pfo_widget_sysvar__define.pro,v 1.1 2010/07/17 18:57:05 jpmorgen Exp $
;-
pro pfo_widget_sysvar__define, num_widgets=num_widgets
  ;; System variables cannot be redefined and named structures cannot
  ;; be changed once they are defined, so it is OK to check this right
  ;; off the bat
  defsysv, '!pfo_widget', exists=pfo_widget_exists
  
  ;; Allow user to call with num_widgets
  if NOT keyword_set(num_widgets) then $
    if pfo_widget_exists then $
      num_widgets = !pfo_widget.num_widgets $
    else $
      num_widgets = 10

  if pfo_widget_exists then begin
     !pfo_widget.num_widgets = num_widgets
     return
  endif


  pfo_widget $
    = {pfo_widget_sysvar, $
       $ ;; Size of the scroll window
       x_scroll:        780, $ 
       y_scroll:        800, $
       ;; number of widgets per param.  This defines the array length
       ;; of pfo_widget.IDs.  It starts out at the minimum value for
       ;; printing via pfo_null or functions that produce the same
       ;; number of widgets.  Any add-ons that need more widgets
       ;; should increment this value _before_ defining
       ;; pfo_widget_struct
       num_widgets:     num_widgets, $
       $  ;; index into pfo_widget.IDs that determines where functions
       $  ;; should start writing their widgetIDs
       first_funct: 	1, $
       $ ;; ID_idx indicates where we are along the pfo_widget.IDs 
       $ ;; while creating or filling the widget
       ID_idx	:	0, $
       delimiters: ['.', '<', '|'], $
       $ ;; delimiters: ['free', 'limited', 'fixed', 'tied'], $
       $ ;; Handy tokens for working with delimiters
       free	:	0, $
       limited  :       1, $
       fixed	:	2, $
       $ ;; Tokens for "side" in pfo_widget_value_event
       left	:	0,   $ ;; sync these with !pfo.left and !pfo.right
       right	:	1,   $
       value	:	2 $
       }
  defsysv, '!pfo_widget', pfo_widget
end
