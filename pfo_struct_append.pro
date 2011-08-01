;+
; NAME: pfo_struct_append
;
; PURPOSE: append structures and, optionally, their descriptions
;
; CATEGORY: General structure manipulation with extentions for PFO
;
; CALLING SEQUENCE: 

;  pfo_struct_append, orig_struct, more_struct_or_required_tags, [name=name], $
;  [modified=modified], [descr=descr]

; DESCRIPTION: 

; Case 1: more_struct_or_required_tags is a STRUCT.  Let's call it
; "more_struct."  Here we just use IDL's native create_struct function
; to append more_struct to orig_struct.  If orig_struct is undefined,
; it is initialized as more_struct.  NOTE: orig_struct is overwritten
; with the new structure!  This is handy when you want to build up a
; structure inside a loop (like pfo_array_append).

; Case 2: more_struct_or_required_tags is an (array of) STRING.  Lets
; call it "required_tags."  This triggers the creation of tags (each
; of which can be an entire sub-structure) using the pfo_struct_new
; sytem.  New tags are created only if they are not in orig_struct
; already.  NOTE: the format for filenames in the pfo_struct_new
; system is 'tag_struct__define.'  Just pass the 'tag' part in
; more_struct_or_required_tags. pfo_struct_append and pfo_struct_new
; append '_struct__define.'

; DOCUMENTED STRUCTURES (case 2 continued): When new tags are created
; with the pfo_struct_new system the tag_struct __init functions can
; (should!) return a documentation structure in the "descr" keyword
; (see below).  These are accumulated in the descr keyword of
; pfo_struct_append.  If no descr is returned by an __init routine,
; the documentation tag is assigned to !pfo.not_documented.

; DOCUMENTED STRUCTURES example.  If you had a single tag at the top
; level the descr structure would like something like:

; descr = {tag: 'documentation'}

; For tags that define a substructure, such as the PFO substructure
; defined in pfo_struct__define, descr would look something like like:

; descr ={tag: {README: 'overall tag comment', subtag1: 'doc1', subtag2:...}}

; The idea is to have descr look exactly like the tag/structure it is
; documenting, except instead of tags pointing to values, they point
; to strings.  Top-level tags point to structures, hence the need for
; an extra README tag in each substructure.


; INPUTS: 

;   orig_struct: original structure.  Can be undefined, WILL BE
;       OVERWRITTEN!

;   more_struct_or_required_tags: structure to append to orig_struct
;       (Case 1, above) or array of string containing required tag
;       names (Case 2, above)

;   if orig_struct and more_struct_or_required_tags are both
;   	undefined, the routine returns quietly

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 

;       descr -- the parallel structure to orig_struct that contains
;                the documentation of each tag.  If
;                more_struct_or_required_tags is a string that
;                sucessfully generates a structure and descr in
;                pfo_struc_new, the more_struct descr will be appended
;                to descr, thus keeping descr current

;
;     name -- name of new structure.  If not specified, an anonymous
;       structure will be returned.  NOTE: if name corresponds to an
;       existing named structure, unless all of the tags match, you
;       will get an error that looks something like "Wrong number of
;       tags defined for structure."

;	modified -- boolean indicating if orig_struct was modified
;
; OUTPUTS: orig_struct -- the new structure
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
; EXAMPLE (Case 1):
; pfo_struct_append, a, {my_tag: 'my_value'}
; pfo_struct_append, a, {another_tag: 'another_value'}
; pfo_struct_append, a, {last_tag: 'last_value'}
; help, /stru, a

; EXAMPLE (Case 2):
; parinfo = pfo_struct_new('mpfit_parinfo', descr=descr)
; parinfo = replicate(parinfo, 4)
; pfo_struct_append, parinfo, 'PFO', name='basic_parinfo_struct', descr=descr
; help, parinfo
; help, /stru, parinfo

;
; MODIFICATION HISTORY:  This started out life as the function
; struct_append, which was not as efficient with memory
;
; $Id: pfo_struct_append.pro,v 1.3 2011/08/01 18:58:32 jpmorgen Exp $
;
; $Log: pfo_struct_append.pro,v $
; Revision 1.3  2011/08/01 18:58:32  jpmorgen
; First reasonably functional version of pfo_obj
; Improved documentation and error CATCHing
;
; Revision 1.2  2011/04/29 20:48:02  jpmorgen
; Going to change the way I build up the required tags
;
; Revision 1.1  2011/01/20 23:01:40  jpmorgen
; Initial revision
;
;-
; +
pro pfo_struct_append, orig_struct, more_struct_or_required_tags, name=name, $
  modified=modified, descr=descr

  init = {pfo_sysvar}
  init = {tok_sysvar}
  
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_struct_append, orig_struct, more_struct_or_required_tags, [name=name, modified=modified].  If more_struct_or_required_tags is [an array of type] string, the corresponding tags will be created and initialized with pfo_struct_new and appended to orig_struc'
     endif
  endif ;; not debugging

  ;; Set flag to check to see if we did anything
  modified = 0

  Norig_struct = N_elements(orig_struct)
  Nmore_struct = N_elements(more_struct_or_required_tags)
  ;; If we have no inputs, quietly exit
  if Norig_struct eq 0 and Nmore_struct eq 0 then $
    return

  ;; Handle the case when we do not have any more_struct_or_required_tags
  if Nmore_struct eq 0 then begin
     ;; Check to see if we might be fiddling with orig_struct's name.
     if keyword_set(name) then begin
        struct_name = tag_names(orig_struct, /structure_name)
        if strupcase(name) eq struct_name then $ ;; nope, names are the same
          return
        ;; If we made it here, we want orig_struct to have a new
        ;; name.  Catch the case where the name conflicts with an
        ;; existing named structure.  I have seen some strange error
        ;; messages that don't immediately point me to what is going on.
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: I am not sure, but you may have a conflicting structure name=' + strtrim(name, 2)
        endif


        ;; Beware the case where orig_struct has more than one
        ;; element.  create_struct cannot handle arrays
        if Norig_struct eq 1 then begin
           orig_struct = create_struct(temporary(orig_struct), name=name)
        endif else begin
           ;; We need to build up a new structure from one element and
           ;; the new name.
           new_struct1 = orig_struct[0]
           new_struct1 = create_struct(temporary(new_struct1), name=name)
           new_struct = replicate(temporary(new_struct1), Norig_struct)
           ;; Struct_assign is the only way we move things around
           struct_assign, orig_struct, new_struct, /nozero
           ;; Move new_struct back to orig_struct
           orig_struct = temporary(new_struct)

        endelse
        modified = 1
     endif ;; keyword name specified
     return
  endif

  ;; Check to see if more_struct is a string.  In this case, switch to
  ;; required_tags mode.
  if size(more_struct_or_required_tags, /type) eq !tok.string then begin
     ;; The idea here is to create each of our tags and append them to
     ;; a variable "more_struct."  more_struct, once complete will
     ;; eventually be appended to orig_struct in a recursive call.
     ;; Recursive calls from this section of code are done with
     ;; more_struct_or_required_tags as as struct, not a string,
     ;; so we don't get too confused....
     for it=0, Nmore_struct-1 do begin
        ;; Check to see if our tag is present in orig_struct.  Before
        ;; we do this, we need to make sure orig_struct is of type
        ;; struct.  If orig_struct is not defined, this section is
        ;; quietly skipped
        if size(orig_struct, /type) eq !tok.struct then begin
           tag_names = tag_names(orig_struct)
           idx = where(strupcase(more_struct_or_required_tags[it]) eq $
                       tag_names, count)
           ;; ;; IDL should have already done the error checking on this
           ;; ;; when creating orig_struct
           ;; if count gt 1 then $
           ;;   message, 'ERROR: there are ' + strtrim(count, 2) + ' tags with the name ' + strtrim(more_struct_or_required_tags[it], 2) + ' in orig_struct.  There should only be one'
           ;; If the tag name is already there, we don't need to
           ;; create more_struct
           if count eq 1 then $
             CONTINUE
        endif ;; looking through tags in orig_struct
        
        ;; If we made it here, we need to create a new tag which
        ;; contains the structure in our more_struct_or_required_tags
        ;; list.  We are passed a list of tags: the defining routines
        ;; are named tag_struct__define, so we need to add _struct.
        ;; pfo_struct_new adds __define.  Also note, that we just
        ;; create one array element at this time
        this_struct = $
          pfo_struct_new(more_struct_or_required_tags[it] + '_struct', $
                         descr=more_struct_descr)
        ;; Put this newly created piece of more_struct under its tag name and
        ;; append it to orig_struct (which may yet to have been
        ;; initialized).  We can call ourselves recursively, since we
        ;; are no longer dealing with strings.
        pfo_struct_append, orig_struct, $
                           create_struct(more_struct_or_required_tags[it], $
                                         temporary(this_struct))
        ;; Set our flag to indicate that orig_struct has been modified
        modified = 1

        ;; Append the more_struct description to the existing descr
        ;; (if it exists), otherwise, initialize descr
        if keyword_set(more_struct_descr) then $
          pfo_struct_append, descr, $
                             create_struct(more_struct_or_required_tags[it], $
                                           temporary(more_struct_descr)) $
        else $
          pfo_struct_append, descr, $
                             create_struct(more_struct_or_required_tags[it], $
                                           !pfo.not_documented)
     endfor ;; each tag in more_struct_or_required_tags

     ;; Check to see if we have a name.  We need to call ourselves,
     ;; since we are equipped to deal with arrays of struct and
     ;; create_struct is not.  Note that modified is guaranteed be set
     ;; to 1 at this point.
     if keyword_set(name) then $
       pfo_struct_append, orig_struct, name=name

     return

  endif  ;; more_struct_or_required_tags is a string
  
  ;; If we made it here, we have the standard struct case.

  ;; Handle the different cases of orig_struct: non-existant,
  ;; 1-element array, multi-element array.  We have already handled
  ;; the case of Nmore_struct eq 0 above, but we need to handle the
  ;; case where it is gt 1 here.
  case Norig_struct of
     0: begin
        ;; e.g., help kick-start orig_struct in a loop
        orig_struct = more_struct_or_required_tags
        modified = 1
     end
     1: begin
        ;; 1-element array orig_struct
        ;; Check for multi-element array in more_struct
        if Nmore_struct gt 1 then $
          message, 'ERROR: more_struct_or_required_tags is multi-element array of type struct but orig_struct only has one element.  Did you get the order of your arguments mixed up?  If you really want to put structs together like this, you will have to figure things out with replicate and struct_assign yourself.'
        orig_struct = create_struct(temporary(orig_struct), $
                                    more_struct_or_required_tags, name=name)
        modified = 1
     end 
     else: begin
        ;; orig_struct is an array of type struct (e.g. a parinfo
        ;; array).  create_struct thinks that array arguments
        ;; correspond to fancy definition of tags individual tags.
        ;; What I really want is to just augment the 
        ;; rather than how I tend to work with them in parinfo
        if Norig_struct ne Nmore_struct and Nmore_struct ne 1 then $
          message, 'ERROR: orig_struct is an array of ' + strtrim(Norig_struct, 2) + ' elements and more_struct is an array of ' + strtrim(Nmore_struct, 2) + '.  I am not able to figure out how to combine these.  They should have the same number of elements or more_struct should have just one.'
        ;; Create one element of our new appended struct
        new_struct1 = create_struct(orig_struct[0], $
                                    more_struct_or_required_tags[0], name=name)
        ;; Create an empty array of our new struct
        new_struct = replicate(temporary(new_struct1), Norig_struct)
        ;; Dump in the orig_struct, making sure we don't disturb
        ;; any template values from more_struct
        struct_assign, temporary(orig_struct), new_struct, /nozero
        ;; struct_assign seems to handle the one to many mapping with
        ;; no problem, so don't bother checking
        struct_assign, more_struct_or_required_tags, new_struct, /nozero
        ;; Move new_struct back to orig_struct
        orig_struct = temporary(new_struct)
        modified = 1
     end
  endcase ;; Norig_struct

end
