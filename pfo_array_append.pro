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
; $Id: pfo_array_append.pro,v 1.2 2011/01/20 23:00:37 jpmorgen Exp $
;
; $Log: pfo_array_append.pro,v $
; Revision 1.2  2011/01/20 23:00:37  jpmorgen
; Fixed bug when orig_array undefined, added error messages
;
; Revision 1.1  2010/12/10 22:02:35  jpmorgen
; Initial revision
;
;-
pro pfo_array_append, orig_array, more_array, null_array=null_array
  init = {tok_sysvar}
  init = {pfo_sysvar}

  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_array_append, orig_array, more_array, null_array=null_array'
     endif
  endif ;; not debugging


  ;; Any errors will probably make more sense in the calling code
  on_error, !tok.return

  ;; Check to see if we need to do anything
  if N_elements(more_array) eq 0 then begin
     ;; --> I am not sure if I really want this message.  Users can
     ;; get rid of it with !quiet = 1     
     message, /INFORMATIONAL, 'NOTE: more_array not specified/undefined'
     return
  endif

  ;; Trivial case
  if N_elements(orig_array) eq 0 then begin
     orig_array = more_array
     return
  endif
    

  ;; Next easiest case: we are up and running with an existing array.
  if N_elements(orig_array) gt 1 then begin
     if size(orig_array, /type) ne size(more_array, /type) then $
       message, 'ERROR: when orig_array has more than one element, orig_array and more_array must have the same type.'
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
