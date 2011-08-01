;+
; NAME: pfo_array2string
;
; PURPOSE: convert an array to a string for convenient output via,
; e.g. message.  The Coyote library and probably other places probably
; have better routines
;
; CATEGORY: PFO utilities
;
; CALLING SEQUENCE: s = pfo_array2string(array, format=format)
;
; DESCRIPTION:
;
; INPUTS: array to convert into a string
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: format: format of individual array members
;
; OUTPUTS: string in the form [xxx, xxx, xxx]
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
; $Id: pfo_array2string.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_array2string.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_array2string, array, format=format

  init = {tok_sysvar}

  if N_elements(array) eq 0 then $
     return, ''
  if N_elements(format) eq 0 then begin
     case size(/type, array) of
        !tok.byte: format = 'I'
        !tok.int: format = 'I'
        !tok.long: format = 'I'
        !tok.float: format = 'G'
        !tok.double: format = 'G'
        !tok.complex: format = 'G'
        !tok.string: format = 'A'
        !tok.struct: format = ''
        !tok.dcomplex: format = 'G'
        !tok.pointer: format = ''
        !tok.objref: format = ''
        !tok.uint: format = 'I'
        !tok.ulong: format = 'I'
        !tok.long64: format = 'I'
        !tok.ulong64: format = 'I'
     endcase
  endif

  return, "[" + strjoin(string(format='(' + format + ', :, ", ")', array)) + "]"
end
