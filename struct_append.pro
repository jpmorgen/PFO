; +
; $Id: struct_append.pro,v 1.3 2011/01/20 23:02:06 jpmorgen Exp $

; struct_append  Append tags to a structure. 

;; Tue Aug 31 11:46:56 2010  jpmorgen@sandusky
;; Now that I understand create_struct better and have motivation to
;; avoid the EXECUTE command, I see this is just a wrapper around
;; create_struct.  Leave it defined like array append.

; This works by defining a new structure (named or anonymous) with the
; tags and tags values

;; --> THIS IS OBSELETE.  PFO_STRUCT_APPEND SHOULD BE USED INSTEAD.

function struct_append, orig_struct, more_struct, name=name
  
  ;; Check to see if we are appending to an empty orig_struct
  if N_elements(orig_struct) eq 0 then $
    temp_struct = more_struct $
  else $
    temp_struct = create_struct(orig_struct, more_struct)

  ;; Check to see if the new structure needs a name
  if N_elements(name) gt 0 then $
    temp_struct = create_struct(temporary(temp_struct), name=name)

  return, temp_struct


  ;; Build up an anonymous structure one tag at a time.  We could
  ;; build a command line that did this all at once and call it with
  ;; 'execute,' but then IDLVM woudn't work.

  ;; Here is the dirty work that would be nicer done with
  ;; orig_struct.(*)
;;
;;  if keyword_set(more_struct) then $
;;    temp_struct = create_struct([tag_names(orig_struct), tag_names(more_struct)]) $
;;  else $
;;    temp_struct = create_struct([tag_names(orig_struct))
;;
;;
;;
;;  ;; Since IDL 5 structure tags can be referenced with numbers, but
;;  ;; not ranges or (*), build a string with all the tag numbers
;;  ;; specified one-by-one and pass that to "execute"
;;  if keyword_set(orig_struct) then begin
;;     command = 'new_struct = create_struct(name=name, [tag_names(orig_struct), tag_names(more_struct)]'
;;  endif else begin
;;     ;; Cover the case where you have no orig_struct
;;     command = 'new_struct = create_struct(name=name, [tag_names(more_struct)]'
;;  endelse
;;
;;  ;; Here is the dirty work that would be nicer done with orig_struct.(*)
;;  for i=0,N_tags(orig_struct)-1 do begin
;;     command = command + ', orig_struct.('+strtrim(string(i),2)+')'
;;  endfor
;;  for i=0,N_tags(more_struct)-1 do begin
;;     command = command + ', more_struct.('+strtrim(string(i),2)+')'
;;  endfor
;;  command = command + ')'
;;
;;  ;; Execute the command we have built and check for an error.
;;  if NOT execute(command) then begin
;;     message, command, /CONTINUE
;;     message, 'ERROR: command shown above failed.'
;;  endif
;;
;;  ;; Our command defined new_struct
;;  return, new_struct

end
