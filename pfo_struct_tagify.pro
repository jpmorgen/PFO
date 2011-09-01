;+
; NAME: pfo_struct_tagify
;
; PURPOSE: make a struct into a tag in a structure
;
; CATEGORY: PFO structures
;
; CALLING SEQUENCE: pfo_struct_tagify, parinfo, tagname, tagified=tagified

; DESCRIPTION: This is a helper routine for the __set_tag and
; __get_tag routines in the pfo_struct system.  It makes sure that
; tagname exists in the structure parinfo so that the code in the
; set/get routines can just assume operations on parinfo.tag.* If the
; tagname doesn't exist, this routine takes the entire structure and
; places it under that tagname in parinfo and sets the tagified flag
; to true.  This routine is intended to be called twice, around the
; code that wants parinfo.tag.* tags.  The second call puts the
; structure back the way it was.

; INPUTS: parinfo: array of type structure
;   tagname: scaler string that corresponds to a tagname in parinfo
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;   tagified: an internal flag that is propagated through the calling
;   routine between two calls.  Makes sure parinfo is put back to its
;   natural state after it has been tagified
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
; $Id: pfo_struct_tagify.pro,v 1.2 2011/09/01 22:20:42 jpmorgen Exp $
;
; $Log: pfo_struct_tagify.pro,v $
; Revision 1.2  2011/09/01 22:20:42  jpmorgen
; Added cool feature to grab fname from calling routine
;
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_struct_tagify, parinfo, tagified=tagified

  ;; Don't do any error catching here.  Let all errors be caught in
  ;; calling code

  ;; Get the top-level tag and structure names
  tn = tag_names(parinfo)
  sn = tag_names(parinfo, /structure_name)
  N_parinfo = N_elements(parinfo)

  ;; Get our tagname from help
  help, calls=calls
  tagname = strmid(calls[1], 0, strpos(calls[1], '_STRUCT__'))

  ;; Check if we need to set our tag back to the top-level
  if keyword_set(tagified) then begin
     tag_idx = where(tn eq strupcase(tagname), count)
     if count eq 0 then $
        message, 'ERROR: tagname ' + tagname + ' does not exist in parinfo'
     parinfo = parinfo.(tag_idx)
     tagified = 0
     return
  endif ;; putting tag back on top-level

  ;; If we made it here, we are being called for the first time

  ;; Check to see if we were handed <tagname>_STRUCT directly, rather
  ;; than our usual case of a parinfo that has <tagname>_STRUCT under
  ;; the tag named <tagname>.
  if sn eq strupcase(tagname)+'_STRUCT' then begin
     ;; Temporarily put the whole parinfo (which is a PFO_ROI_STRUCT)
     ;; into a PFO tag so the rest of our code works.  We have to
     ;; build up our temporary parinfo carefully
     tparinfo = replicate(create_struct(tagname, parinfo[0]), N_parinfo)
     ;; Note that [*] doesn't work with structures in this
     ;; context.  You need an explicit list of indices
     tparinfo[lindgen(N_parinfo)].(0) = parinfo
     parinfo = temporary(tparinfo)
     tn = tag_names(parinfo)
     tagified = 1
  endif ;; we are dealing with the PFO structure itself

  ;; Check to see if we are a standard parinfo with PFO_STRUCT under
  ;; the tag named PFO
  tag_idx = where(tn eq strupcase(tagname), count)
  if count eq 0 then $
    message, 'ERROR: ' + strtrim(tagname, 2) + ' tag not found within input parinfo'

  if tag_names(parinfo.(tag_idx), /structure_name) ne strupcase(tagname)+'_STRUCT' then $
    message, 'ERROR: ' + strtrim(tagname, 2) + ' tag is not of type ' + strtrim(tagname, 2) + '_STRUCT'

end
