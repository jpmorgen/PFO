;+
; NAME: pfo_widget_top
;
; PURPOSE: find the ID of the top-most widget in a widget hierarchy
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE: topID = pfo_widget_top(ID)
;
; DESCRIPTION: I am surprised that IDL doesn't have this in
; widget_info, but it is easy enough to program up.  You don't
; need it when you are dealing with events, since event.top is always
; the top-level widget.  But when you are at a mid-level and parentage
; is only tracked so far up, this is handy....
;
; INPUTS: ID: widget ID of a widget in the hierarchy
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
; $Id: pfo_widget_top.pro,v 1.1 2011/09/15 16:04:48 jpmorgen Exp $
;
; $Log: pfo_widget_top.pro,v $
; Revision 1.1  2011/09/15 16:04:48  jpmorgen
; Initial revision
;
;-
function pfo_widget_top, ID

  init = {tok_sysvar}
  ;; Errors will make more sense in the calling routine
  ON_ERROR, !tok.return

  ;; Do some basic error checking
  if NOT widget_info(ID, /valid_ID) then $
     message, 'ERROR: invalid widget ID'
  top = ID
  repeat begin
     ;; widget_info /parent returns 0 when it gets to the top
     new_top = widget_info(top, /parent)
     if new_top ne 0 then $
        top = new_top
  endrep until new_top eq 0
  return, top
end
