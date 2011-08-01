;+
; NAME: pfo_struct_tag_present
;
; PURPOSE: check to see if a tag is present at the top level of struct
;
; CATEGORY: [pfo] structures
;
; CALLING SEQUENCE: yes_or_no = pfo_struct_tag_present(struct,
; tagname[, tag_idx=tag_idx])
;
; INPUTS:
;	struct: structure to query for tag
;
;	tagname: tagname to search for
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;	tag_idx (output): the "index" of the tag, which allows tags to
;	be referenced by number rather than name
;	(e.g. struct.(tag_idx)).  See IDL reference material.

;
; OUTPUTS: 1=tag found, 0=no tag found
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
; $Id: pfo_struct_tag_present.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_struct_tag_present.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_struct_tag_present, struct, tagname, tag_idx=tag_idx

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH invocation errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'Caught the above error, returning 0', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  tags = tag_names(struct)
  ;; tag_names always returns uppercase
  tag_idx = where(tags eq strupcase(tagname), count)
  return, count
end
