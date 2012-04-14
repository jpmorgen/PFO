;+
; NAME: pfo_uniq
;
; PURPOSE: returns indices of unique elements of an array and
; (optionally) a reverse lookup array of indices that allow grouping
; of the identical elements.
;
; CATEGORY: array manipulation
;
; CALLING SEQUENCE: u_idx = pfo_uniq(array [, sidx][,
; reverse_indices=reverse_indices][, N_uniq=N_uniq])

; DESCRIPTION: pfo_uniq combines characteristics of IDL's uniq and
; histogram routines by providing the option to return a reverse index
; lookup array of the _identical_ elements.  Using the reverse_indices
; output of histogram or pfo_uniq is much faster than using where when
; looping through many sets of identical elements in an array.
; Basically, the array is only scanned once.  IDL's histrogram suffers
; from the problem that it has to output a histogram, which may be
; very inefficient for the input array (e.g. several values grouped
; around 0 and then one at a very large value).  pfo_uniq just lines
; everything up and creates the reverse lookup array based on
; identical elements.  

; NOTE: the order in which the reverse indices are listed can be
; affected by which OS issues the sort command.  Use the IDLASTRO
; bsort command (or equivalent) to achieve consistent results between
; platforms.

; INPUTS:

;	Array:	Input array.  If array is not itself sorted in
;	monotonic order, sidx must be specified

;
; OPTIONAL INPUTS:

;	sidx: the output of sort(array).  Using array and sidx allows
;	pfo_uniq to work without the need to rewrite the input array.

;
; KEYWORD PARAMETERS:

;       reverse_indices (output): an array, much like that used by
;       IDL's historgam routine, that allows lookup of groups of
;       identical elements of array.  The ith group is given by:

;                  idx = r_idx[r_idx[i]:r_idx[i+1]-1]

;	N_uniq (output): the number of unique elements in the input array

;
; OUTPUTS:

;	array of indices into array of unique elements of that array.
;	If array is sorted monotonically increasing, the last unique
;	element in each set of duplicates is returned.  If the sort is
;	in decreasing order, the first is returned.

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

; see pfo_parinfo_parse

; MODIFICATION HISTORY:
;
; $Id: pfo_uniq.pro,v 1.4 2012/04/14 13:29:05 jpmorgen Exp $
;
; $Log: pfo_uniq.pro,v $
; Revision 1.4  2012/04/14 13:29:05  jpmorgen
; Updated documentation slightly
;
; Revision 1.3  2011/09/23 13:11:04  jpmorgen
; Improve documentation
;
; Revision 1.2  2011/09/01 22:26:01  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_uniq, array, sidx, reverse_indices=reverse_indices, N_uniq=N_uniq

  ;; Run the default IDL version if we don't need the reverse_indices.
  ;; Check for the case where we may be recycling reverse_indices
  if N_elements(reverse_indices) eq 0 and $
    NOT arg_present(reverse_indices) then begin
     u_idx = uniq(array, sidx)
     N_uniq = N_elements(u_idx)
     return, u_idx
  endif ;; don't need reverse_indices

  ;; If we made it here, we want to do the reverse_indices

  ;; Make sure we have indices into array
  pfo_idx, array, sidx, N_array=N_array
  ;; sidx provides the indices into array that we need.  Now we just
  ;; need to come up with the indices into sidx that indicate where
  ;; each chunk of duplicates begin and end.  

  ;; Find the uniq indices, referenced to the sorted array.  This cool
  ;; trick with shift and where marks the beginnings and ends of the
  ;; arrays.  Not sure if it is faster to copy off the array or do
  ;; things with indices implicitly.
  su_idx = where(array[sidx] ne array[shift(sidx, -1)], N_uniq)

  ;; Handle case where all elements are identical
  if N_uniq eq 0 then begin
     N_uniq = 1
     su_idx = N_array-1
     retval = su_idx
  endif ;; all identical

  ;; Build reverse_indices a la IDL's histogram function

  ;; First write down the reverse_indices reference section in plain
  ;; idx.  The su_idx give the indices (into the sorted array) of the
  ;; last occurrence of a group of identical elements.  This would be
  ;; the upper boundary of a "bin" in the histogram reverse_indices
  ;; sense.  Make it a true upper boundary and add one to it and make
  ;; sure the ultimate lower boundary (0) is included
  reverse_indices = [0, su_idx+1]
  ;; Now add an offset to deal with the fact that the indices into
  ;; array are going to be appended to this reference section
  reverse_indices += N_elements(reverse_indices)
  ;; Now put on our sorted array indices.  These point into array,
  ;; which we want, but are sorted, which naturally groups identical
  ;; elements together.
  pfo_array_append, reverse_indices, sidx

  if keyword_set(retval) then $
    return, retval

  return, sidx[su_idx]

end

