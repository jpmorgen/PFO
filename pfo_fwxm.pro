;+
; NAME: pfo_FWXM
;
; PURPOSE: find the full width (Half, Tenth, etc.) max of a function
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
; $Id: pfo_fwxm.pro,v 1.1 2011/02/18 22:12:21 jpmorgen Exp $
;
; $Log: pfo_fwxm.pro,v $
; Revision 1.1  2011/02/18 22:12:21  jpmorgen
; Initial revision
;
;-
function pfo_fwxm, parinfo, frac, max_X_guess, idx=idx, Xin=Xin, Xaxis=Xaxis

  if NOT keyword_set(Xin) and NOT keyword_set(Xaxis) then $
    message, 'ERROR: specify /Xin or /Xaxis so I know what your max_X_guess reads in and what you want your output to be.  You can use pfo_convert_coord if you need to mix and match.'

  ;; Sanity checking on max_chan_guess.  Default is just a guess.  It
  ;; might not work well for functions that don't have broad enough
  ;; wings to include 0.
  if N_elements(max_X_guess) eq 0 then $
    max_X_guess = 0
  if N_elements(max_X_guess) ne 1 then $
    message, 'ERROR: Sorry, I can only handle one feature at a time.  max_chan_guess should have just one element.'

  ;; Do our problem in terms of channel
  max_chan_guess = max_X_guess
  if keyword_set(Xaxis) then $
    max_chan_guess = pfo_convert_coord(max_X_guess, parinfo, idx=idx, $
                                       /from_Xaxis, /to_Xin)

  max_chan = pfo_min(/MAXIMIZE, parinfo, max_chan_guess, idx=idx)
  max_val = pfo_convert_coord(max_chan, parinfo, idx=idx, $
                              /from_Xin, /to_Yaxis)
  ;; Brute force way of making sure that the left side is on the left
  ;; and the right side is on the right
  side = dblarr(2)
  for is=0,1 do begin
     case is of 
        0 : direction = -1.
        1 : direction = 1.
     endcase
     dist = 1.

     ;; Check for an error in pfo_convert_coord.  Hopefully it will go
     ;; away if we just get farther from our max_chan
     CATCH, err
     if err ne 0 then begin
        dist += 1
     endif ;; error in pfo_convert_coord
     repeat begin
print, dist * direction
        side[is] = pfo_convert_coord(max_val*frac, parinfo, idx=idx, $
                                     /from_Yaxis, /to_Xin, $
                                     initial_guess=[max_chan + direction*dist, 0, 0])
print, side
        dist += 1
        if dist gt 1E6 then begin
           CATCH, /CANCEL
          message, 'ERROR: tried over 1E6 times to find the ' + strtrim(direction, 2) + ' side of the FW' + strtrim(frac, 2) + 'M.  Giving up.'
       endif
     endrep until (side[is] - max_chan) * direction gt 0
  endfor ;; left and right

  if keyword_set(Xaxis) then $
    side = pfo_convert_coord(side, parinfo, idx=idx, $
                             /from_Xin, /to_Xaxis)

  return, side[1] - side[0]
end

