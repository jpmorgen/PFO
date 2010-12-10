;+
; NAME: pfo_array_append
;
; PURPOSE: append arrays
;
; CATEGORY: general array manipulation.  For historical purposes, this
; is not named array_append (yet), but is part of the doc_struct package
;
; CALLING SEQUENCE: pfo_array_append, orig_array, more_array, null_array=null_array
;
; DESCRIPTION: Does IDL array concatenation [orig_array, new_array]
; without having to worry about whether or not orign array is
; defined.  Useful in loops for bulding up arrays
;
; INPUTS: 
;	orig_array: original array.  Need not be defined.
;
;	more_array: array to be concatenated to orig_array.  If
;	more_array is of a different type than orig_array, this is the
;	signal to initialize the return value of orig_array to
;	more_array.  If more_array is a large array, you may wish to
;	use temporary(more_array).
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
;       null_array: if orig_array is only one element long and is
;       equal to null_array, then orig_array is considered "empty" and
;       will be reinitialized with the contents of more_array.  Useful
;       if you don't want to change the type of the arrays.
;
; OUTPUTS:
;	orig_array contains the new array.  If you want to save the
;	original array, plan accordingly.
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

; orig_array = 'none'
;   for i=0,1000L do begin
;      bigi = replicate(i, i+1)
;      pfo_array_append, orig_array, temporary(bigi)
;   end
; 
; help, orig_array
; ORIG_ARRAY      INT       = Array[501501]
;
; pfo_array_append, a, 0, null_array=0
; help, a
; A               INT       =        0
; pfo_array_append, a, 0, null_array=0
; help, a
; A               INT       =        0
; pfo_array_append, a, 2, null_array=0
; help, a
; A               INT       =        2
; pfo_array_append, a, 2, null_array=0
; help, a
; A               INT       = Array[2]
;
; 
; MODIFICATION HISTORY:
;
; $Id: pfo_array_append.pro,v 1.1 2010/12/10 22:02:35 jpmorgen Exp $
;
; $Log: pfo_array_append.pro,v $
; Revision 1.1  2010/12/10 22:02:35  jpmorgen
; Initial revision
;
;-
pro pfo_array_append, orig_array, more_array, null_array=null_array
  init = {tok_sysvar}
  ;; Any errors will probably make more sense in the calling code
  on_error, !tok.return

  ;; Check to see if we need to do anything
  if N_elements(more_array) eq 0 then $
    return

  ;; Trivial case
  if N_elements(orig_array) eq 0 then begin
     orig_array = more_array
  endif
    

  ;; Next easiest case: we are up and running with an existing array.
  ;; Let IDL's error handling take care of type mismatches
  if N_elements(orig_array) gt 1 then begin
     ;; Use the temporary function to save memory
     orig_array = [temporary(orig_array), more_array] 
     return
  endif


  ;; A one element original array is somewhat more ambiguous. 
  if N_elements(orig_array) eq 1 then begin
     ;; If the types don't match, we are starting over with more_array
     if size(orig_array, /type) ne size(more_array, /type) then begin
        orig_array = more_array
        return
     endif

     ;; The types match.  See if orig_array is null_array.
     ;; Don't use keyword_set, since null is usually passed :-0
     if N_elements(null_array) ne 0 then begin
        if orig_array eq null_array then begin
           orig_array = more_array
           return
        endif ;; orig_array eq null_array
     endif ;; null_array specified
  endif ;; one element orig_array

  ;; Having handled the special cases, let IDL do the rest, being
  ;; polite as possible with memory --> here is where we would check
  ;; for type structure and do a relaxed structure assign from the
  ;; orig_array.  If that doesn't work, we do one from the new array.
  orig_array = [temporary(orig_array), more_array] 

end
