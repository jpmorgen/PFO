;+
; NAME: pfo_struct_append
;
; PURPOSE: append structures
;
; CATEGORY: General structure manipulation
;
; CALLING SEQUENCE: pfo_struct_append, orig_struct, more_struct, [name=name]
;
; DESCRIPTION: Uses IDL's native create_struct function to
; append more struct to orig_struct.  If orig_struct is undefined, it
; is initialized as more_struct.  NOTE: orig_struct is overwritten
; with the new structure.  This is handy when you want to just build up a
; structure from a bunch of tags in a loop or scattered randomly
; through code
;
; INPUTS: orig_struct -- original structure.  Can be undefined, WILL
;                        BE OVERWRITTEN!
;	more_struct -- structure to append to orig_struct
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: name -- name of new structure
;
; OUTPUTS: orig_struct -- new structure
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
; pfo_struct_append, a, {my_tag: 'my_value'}
; pfo_struct_append, a, {another_tag: 'another_value'}
; pfo_struct_append, a, {last_tag: 'last_value'}
; help, /stru, a
;
; MODIFICATION HISTORY:  This started out life as the function
; struct_append, which was not as efficient with memory
;
; $Id: pfo_struct_append.pro,v 1.1 2011/01/20 23:01:40 jpmorgen Exp $
;
; $Log: pfo_struct_append.pro,v $
; Revision 1.1  2011/01/20 23:01:40  jpmorgen
; Initial revision
;
;-
; +
pro pfo_struct_append, orig_struct, more_struct, name=name
  
  ;; Don't get sophisticated with error handling.  Any errors
  ;; will need to be interpreted on up the calling stack.

  ;; Check to see if we are appending to an empty orig_struct
  if N_elements(orig_struct) eq 0 then $
    orig_struct = more_struct $
  else $
    orig_struct = create_struct(temporary(orig_struct), more_struct)

  ;; Check to see if the new structure needs a name
  if N_elements(name) gt 0 then $
    orig_struct = create_struct(temporary(orig_struct), name=name)

end
