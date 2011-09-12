;+
; NAME: test_widget_speed
;
; PURPOSE: test speed of widget "repopulation"
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
; $Id: test_widget_speed.pro,v 1.1 2011/09/12 00:43:42 jpmorgen Exp $
;
; $Log: test_widget_speed.pro,v $
; Revision 1.1  2011/09/12 00:43:42  jpmorgen
; Initial revision
;
;-
pro tws_populate, parentID
  for ir=0, 21 do begin
     rowID = widget_base(parentID, /row, /frame)
     for iw=0,10 do begin
        wtype = 4 * randomu(seed)
        case floor(wtype) of
           0 : ID = widget_text(rowID, value='blah', xsize=10)
           1 : ID = widget_droplist(rowID, value=['blah', 'blahdblah', 'blahdblabla'], xsize=1.25, units=1)
           2 : ID = widget_label(rowID, value='blahl', xsize=1, units=1)
           3 : rowID = widget_base(parentID, /row, /frame);;, /base_align_center)
        endcase
     endfor ;; each widget in the row
  endfor ;; each row
end

pro test_widget_speed
  tlbID = widget_base(x_scroll_size=800, y_scroll_size=600)
  containerID = widget_base(tlbID, /column)
  tws_populate, containerID 
  widget_control, /realize, tlbID

  xmanager, 'test_widget_speed', tlbID, /no_block

  t1 = systime(/seconds)
  widget_control, tlbID, update=0
  widget_control, containerID, /destroy
  print, 'Time to clear container = ', systime(/seconds) - t1 
  containerID = widget_base(tlbID, /column)
  tws_populate, containerID 
  print, 'Time for repopulate before update = ', systime(/seconds) - t1 
  widget_control, tlbID, update=1
  t2 = systime(/seconds)

  print, 'Time for repopulate = ', t2 - t1 


  t1 = systime(/seconds)
  widget_control, tlbID, update=1
  widget_control, containerID, /destroy
  print, 'Time to clear container, (update=1) = ', systime(/seconds) - t1 
  containerID = widget_base(tlbID, /column)
  tws_populate, containerID 
  print, 'Time for repopulate before update = ', systime(/seconds) - t1 
  widget_control, tlbID, update=1
  t2 = systime(/seconds)

  print, 'Time for repopulate = ', t2 - t1 

end


