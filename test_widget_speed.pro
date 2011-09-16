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
; $Id: test_widget_speed.pro,v 1.2 2011/09/16 13:54:01 jpmorgen Exp $
;
; $Log: test_widget_speed.pro,v $
; Revision 1.2  2011/09/16 13:54:01  jpmorgen
; Sent to ITT.  They concur that there is a problem with WINDOWS
; vs. LINUX speed.  They will work on it
;
; Revision 1.1  2011/09/12 00:43:42  jpmorgen
; Initial revision
;
;-
function tws_cw, parentID

;; 16 s
;;  ;; This is the base that would store the true user uvalue
;;  base = widget_base(parentID)
;;  ;; The first child has the object that controls the compound
;;  ;; widget.  I set kill_notify and event_func here too, so the "real"
;;  ;; widget needs to be a child of this
;;  first_childID = widget_base(base);;, tab=1)
;;  ;; Might as well have a container here, in case I need it.
;;  ;; Didn't find that it slowed the code down any
;;  containerID = widget_base(first_childID)
;;  rowID = widget_base(containerID)

;; 4 s
;;  rowID = widget_base(parentID)

;; 10 s
  base = widget_base(parentID)
  rowID = widget_base(base)

  ;; Pick some radom "real" widgets.  These are about all I use
  wtype = 3 * randomu(seed)
  case floor(wtype) of
     0 : ID = widget_text(rowID, value='blah', xsize=10)
     1 : ID = widget_droplist(rowID, value=['blah', 'blahdblah', 'blahdblabla'], xsize=1.25, units=1)
     2 : ID = widget_label(rowID, value='blahl', xsize=1, units=1)
  endcase
end

pro tws_populate, parentID
  for ir=0, 21 do begin
     rowID = widget_base(parentID, /row, /frame)
     for iw=0,10 do begin
        ID = tws_cw(rowID)
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


;;  t1 = systime(/seconds)
;;  widget_control, tlbID, update=1
;;  widget_control, containerID, /destroy
;;  print, 'Time to clear container, (update=1) = ', systime(/seconds) - t1 
;;  containerID = widget_base(tlbID, /column)
;;  tws_populate, containerID 
;;  print, 'Time for repopulate before update = ', systime(/seconds) - t1 
;;  widget_control, tlbID, update=1
;;  t2 = systime(/seconds)
;;
;;  print, 'Time for repopulate = ', t2 - t1 

end


