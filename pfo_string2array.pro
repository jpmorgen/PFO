;+
; NAME: pfo_string2array
;
; PURPOSE: creates an array of strings from a single string that has
; embedded newline (string(10B)) characters
;
; CATEGORY: PFO strings
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
; $Id: pfo_string2array.pro,v 1.1 2011/09/16 13:46:22 jpmorgen Exp $
;
; $Log: pfo_string2array.pro,v $
; Revision 1.1  2011/09/16 13:46:22  jpmorgen
; Initial revision
;
;-

function pfo_string2array, text, maxwidth=maxwidth

  ;; Find out how many lines we have
  last_newline = -1
  maxwidth = 0
  done = 0
  repeat begin
     this_newline = strpos(text, !tok.newline, last_newline + 1)
     if this_newline le 0 then begin
        done = 1
        ;; No (more) newlines.  Make this_newline the last character
        ;; in the string plus 1
        this_newline = strlen(text)
     endif ;; no (more) newlines
     ;; Take the next (or last) chunk of our string and put it onto
     ;; the output array.  Make sure to chop out the newlines.
     pfo_array_append, $
        out_array, $
        strmid(text, last_newline + 1, this_newline - last_newline - 1)
     ;; Get our maxwidth
     maxwidth = maxwidth > (this_newline - last_newline)

     ;; Get ready for the next 
     last_newline = this_newline
  endrep until done

  return, out_array

end
