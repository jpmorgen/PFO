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
; $Id: pfo_array_append.pro,v 1.3 2011/08/01 18:40:47 jpmorgen Exp $
;
; $Log: pfo_array_append.pro,v $
; Revision 1.3  2011/08/01 18:40:47  jpmorgen
; First reasonably functional version of pfo_obj
; Improved merging of structures
;
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
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_array_append, orig_array, more_array, null_array=null_array'
     endif
  endif ;; not debugging

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
  ;; Just handle the error of mis-matched types for now.  Wait to do
  ;; the concatenation until below.
  if N_elements(orig_array) gt 1 and $
    size(orig_array, /type) ne size(more_array, /type) then $
       message, 'ERROR: when orig_array has more than one element, orig_array and more_array must have the same type.'

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
        endif ;; orig_array is null, signaling restart array
     endif ;; null_array specified
  endif ;; one element orig_array

  ;; If we made it here, we need to concatenate orig_array and
  ;; more_array.  We also know they both have the same type, so things
  ;; should be error free... except if orig_array and more_array are
  ;; structs with different tags, which we handle with a separate CATCH

  CATCH, /CANCEL
  
  ;; Use IDL's error handling to spot the case where the structs are
  ;; not the same.
  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL
     if size(orig_array, /type) ne !tok.struct then $
       message, 'ERROR: IDL was unable to concatenate orig_array and more_array, which are of type ' + size(orig_array, /tname)



     ;; If we made it here, we have two structs that have different
     ;; sets of tags.  Collect tag names from orig_array
     ;; and more_array
     otag_names = tag_names(orig_array)
     mtag_names = tag_names(more_array)
     ;; Create a new 1-element struct that has all of the tags from
     ;; orig_array.
     new_struct1 = orig_array[0]
     ;; Use IDL's struct_assign to copy over any common tags from
     ;; more_array to new_struct1.  This does the appropriate type
     ;; conversion on the tags (actually, it does not do the float to
     ;; double I was hoping for.  The type of more_array wins every
     ;; time.  It also zeros out tags in new_struct1 that are not
     ;; found in more_array.  This is OK, since we will put the right
     ;; values back in, below.
     struct_assign, more_array[0], new_struct1
     ;; Since struct_assign does not add the tags found in more_array
     ;; that are not in new_struct1, we have to do that by hand.
     for it=0, N_elements(mtag_names)-1 do begin
        junk = where(mtag_names[it] eq otag_names, count)
        ;; For tags that match, the call to struct_assign, above, as
        ;; already done the work of matching the types
        if count eq 1 then $
          CONTINUE
        ;; If we made it here, we need to add the more_array structure
        ;; tag mtags_names[it] to new_struct1.  Using the code after
        ;; the CATCH, below, try this with the pfo_struct system
        ;; first, so that we get the most up-to-date version of the
        ;; tag --> this is a little risky, in case there is a tag in
        ;; the pfo_struct system that has the same name as a tag that
        ;; means something totally different.  If there is a problem,
        ;; we catch or error and do it the old-fashioned way.
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /INFORMATIONAL
           message, /INFORMATIONAL, 'NOTE: Caught above error when trying to initialize tag ' + mtag_names[it] + ' with the pfo_stuct system.  Adding tag as a copy of more_array[0].' + mtag_names[it]
           new_struct1 = $
              create_struct(temporary(new_struct1), mtag_names[it], more_array[0].(it))
           CONTINUE
        endif ;; not a pfo_struct_new enabled tag
        ;; Try to create our new tag with the pfo_struct system
        new_tag = pfo_struct_new(mtag_names[it] + '_struct')
        ;; If we made it here, everything should be OK, --> as long as
        ;; tag name in more_array is not accidentally the same as a
        ;; structure definition in the pfo_struct system
        pfo_struct_append, new_struct1, new_tag
        ;; Presumably, pfo_struct_new has been called properly
        ;; inside of the pfo_parinfo_template system so that the
        ;; descr is tracked properly.  pfo_array_append was just
        ;; called to concatenate the results to the running parinfo
     endfor ;; each tag in the more_array structure

     ;; new_struct1 is now a single-element array of type struct that
     ;; is as well initialized to a generic combination of orig_array
     ;; and more_array as we can make it.  Now dump our orig_array and
     ;; more_array into it using relaxed structure assignment
     orig_array_new = replicate(new_struct1, N_elements(orig_array))
     struct_assign, temporary(orig_array), orig_array_new, /nozero
     more_array_new = replicate(new_struct1, N_elements(more_array))
     struct_assign, more_array, more_array_new, /nozero
     orig_array = [temporary(orig_array_new), temporary(more_array_new)]
     return
  endif ;; CATCH where orig_array and more_array are structs with different tags

  ;; Having handled the special cases, let IDL do the rest, being
  ;; polite as possible with memory.  The case where we have structs
  ;; can't nuke orig_array with temporary, since the structs
  ;; might have different tags and might need to be combined with the
  ;; CATCH code above
  if size(orig_array, /type) eq !tok.struct then begin
     ;; Do a trial combination of the first elements of the array so
     ;; that we don't take too much memory but do raise an error
     ;; if the structs don't share all the same tags
     test = [orig_array[0], more_array[0]]
     ;; If we made it here, the code below is pretty much guaranteed
     ;; to work
  endif ;; testing compatible structs

  ;; Do our concatenation, being polite with memory
  orig_array = [temporary(orig_array), more_array] 

end
