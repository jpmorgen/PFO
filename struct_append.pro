; +
; $Id: struct_append.pro,v 1.2 2005/06/21 20:19:50 jpmorgen Exp $

; struct_append  Append tags to a structure

; This works by defining a new structure (named or anonymous) with the
; tags and tags values

function struct_append, orig_struct, more_struct, name=name

  ;; Since IDL 5 structure tags can be referenced with numbers, but
  ;; not ranges or (*), build a string with all the tag numbers
  ;; specified one-by-one and pass that to "execute"
  if keyword_set(orig_struct) then begin
     command = 'new_struct = create_struct(name=name, [tag_names(orig_struct), tag_names(more_struct)]'
  endif else begin
     ;; Cover the case where you have no orig_struct
     command = 'new_struct = create_struct(name=name, [tag_names(more_struct)]'
  endelse

  ;; Here is the dirty work that would be nicer done with orig_struct.(*)
  for i=0,N_tags(orig_struct)-1 do begin
     command = command + ', orig_struct.('+strtrim(string(i),2)+')'
  endfor
  for i=0,N_tags(more_struct)-1 do begin
     command = command + ', more_struct.('+strtrim(string(i),2)+')'
  endfor
  command = command + ')'

  ;; Execute the command we have built and check for an error.
  if NOT execute(command) then begin
     message, command, /CONTINUE
     message, 'ERROR: command shown above failed.'
  endif

  ;; Our command defined new_struct
  return, new_struct

end
