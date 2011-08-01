;+
; NAME: pfo_idx
;
; PURPOSE: makes sure an array is indexed
;
; CATEGORY: arrays
;
; CALLING SEQUENCE: pfo_idx, array[, idx=idx][, type=type]

; DESCRIPTION: 

; This is a labor-saving routine that helps streamline the process of
; working with arrays that are passed by reference in IDL.  The idea
; is that if you just want to work with part of an array in a
; subroutine, you can't pass array[idx] and expect anything back.
; array[idx] becomes an "expression" in the calling routine and is
; _copied_ to the subroutine.  One solution is to pass the whole array
; by reference, which is memory efficient and then pass the indices,
; idx, that you really want to work over.  But if you do it that way,
; the subroutine has to check to see if idx exists before you use it.
; This routine just makes a one-liner out of that.

; This routine is also useful for creating an X-axis in channels for
; an array.

; INPUTS: 

; 	array: array that has been passed by reference to the calling
; 	routine.

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;       idx: optional array of indices into array.  If not defined or
;       = !tok.nowhere, idx will be created with the appropriate type
;       for the current length of array

;       type: force idx to be of the specified type (human-readable
;       IDL type codes can be found in the !tok system variable)

;	created: return keyword set to 1 if idx was created in this
;	routine (useful for memory management in calling routines)

;	N_array: return number of elements in array

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

;   Make sure an array is indexed
;	 pfo_idx, parinfo, idx=idx

;   Create an X-axis, called Xin for Yin. 
;        pfo_idx, Yin, idx=Xin, type=size(/type, Yin)


; MODIFICATION HISTORY:
;
; $Id: pfo_idx.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_idx.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_idx, array, idx=idx, type=type, created=created, N_array=N_array

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_idx, array, idx=idx[, type=type][, created=created]'
     endif
  endif ;; not debugging

  ;; Probably most convenient if we catch the error of no array here.
  if N_elements(array) eq 0 then $
    message, 'ERROR: array is undefined'
  if N_elements(idx) eq 0 and NOT arg_present(idx) then $
    message, 'ERROR: idx must be specified'

  N_idx = N_elements(idx)
  N_type = N_elements(type)
  N_array = N_elements(array)

  ;; Check the for the case of idx=!tok.nowhere.  If so, pretend
  ;; idx doesn't exist
  if N_idx eq 1 then $
    if idx eq !tok.nowhere then $
      N_idx = 0

  ;; If we already have idx and don't care what type it is, we are
  ;; done.  Let calling routine do any other error checking (e.g. idx
  ;; out of bounds)
  if N_idx gt 0 and N_type eq 0 then $
    return
  
  ;; Handle the case where we have idx but want to force it to be of a
  ;; particular type
  if N_idx gt 0 and N_type gt 0 then begin
     ;; Check to see if we are already the correct type
     if size(/type, idx) eq type then $
       return
     
     ;; If we made it here, we need to change the type of our existing
     ;; idx
     idx = make_array(N_idx, type=type, value=idx)
     return
  endif ;; forcing idx to be a particular type

  ;; If we made it here, we have to create our idx from scratch
  if N_type eq 0 then begin
     ;; The type of N_elements(array) is automatically going to be a
     ;; big enough type for to contain the largest idx of the current
     ;; array.  If array grows, this might not be the case.  Actually,
     ;; N_elements seems to return type long and arrays don't seem to
     ;; be able to be created that exceed this, at least in IDL 6.1 with
     ;; 3G of RAM, so the point may be moot
     idx = indgen(N_array, type=size(/type, N_array))
     ;; Set our created return flag
     created = 1
  endif else begin
     idx = indgen(N_array, type=type)
     ;; Set our created return flag
     created = 1
  endelse
    

end
