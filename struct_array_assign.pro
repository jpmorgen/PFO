; +
; $Id: struct_array_assign.pro,v 1.1 2003/12/09 21:42:51 jpmorgen Exp $

; pfo_struct_assign

;; This is actually a general routine that resembles IDL's
;; struct_assign, except (1) it handles arrays of structures
;; properly and (2) it doesn't zero out the input structure array.
;; The command line syntax is slightly different:

;; pfo_struct_assign, input_struct_array, idx, tagname=tagname, tagval=tagval

;; WARNING: an entire parinfo array must be passed in order for the
;; values to be permanently changed (see IDL manual "passing by
;; reference" section).  Use the idx keyword to pick out individual
;; parinfo records.

;; If tagval is the only keyword passed, it must be a stucture with
;; tag names matching those in the input structure (code could be
;; added to not raise an error if no match is found).  The tag value
;; of each tag name can be either a single expression that will be
;; copied into all the elements of the input array, or an array of
;; such values, which will be individually assigned to the array
;; elements in the input array.

;; If tagval is not a structure, tagname must be specified and give
;; the name of the tag that tagval represents.  Tagname can be an
;; array of strings, with tagval an array of values (but all must be
;; the same type).  As with the tagval structure case, tagval can be a
;; single value copied into the entire intput array, or an array of
;; values copied one by one.


; -

pro pfo_struct_assign, inparinfo, idx, tagname=tagname, tagval=tagval
                     

  ;; Let IDL do the error checking on array bounds
  ON_ERROR, 2

  npfo = N_elements(inparinfo)
  if npfo eq 0 then return
  CATCH, err
  if err ne 0 then begin
     ;; If there is an error, copy parinfo back onto inparinfo so that
     ;; the calling routine's parinfo doesn't get nuked.
     if nidx ne npfo then begin
        inparinfo[idx] = parinfo
     endif else begin
        ;; Save time by not copying the whole array
        inparinfo = temporary(parinfo)
     endelse
     catch, /cancel
     message, /NONAME, !error_state.msg
  endif

  ;; Set up idx if none specified
  if N_elements(idx) eq 0 then $
    idx = indgen(npfo)

  nidx = N_elements(idx)

  if nidx ne npfo then begin
     parinfo = inparinfo[idx]
  endif else begin
     ;; Save time by not copying the whole array
     parinfo = temporary(inparinfo)
  endelse

  if size(tagval, /type) eq 8 then begin
     ;; Tagval has the entire structure we want to assign.  There are
     ;; two cases: a tag name with an array of tag values
     ;; corresponding to the values to assign in each parinfo record,
     ;; or a simple one-to-one correspondence.  In either case, step
     ;; through the tagnames one at a time.
     tvtagnames = tag_names(tagval)
     pfotagnames = tag_names(parinfo)
     ;; Step through tagval tags one at a time
     for i=0,N_elements(tvtagnames)-1 do begin
        pfotagnum = where(tvtagnames[i] eq pfotagnames, count)
        if count eq 0 then $
          message, 'ERROR: tag ' + tagname[i] + 'not present in parinfo structure'
        pfo_size = size(parinfo.(pfotagnum), /structure)
        tag_size = size(tagval.(i), /structure)
        if pfo_size.N_dimensions ne tag_size.N_dimensions + 1 and $
          pfo_size.N_dimensions ne tag_size.N_dimensions then $
           message, 'ERROR: tagval.' + tvtagnames[i] + ' has the wrong number of dimensions.'

        ;; HERE IS WHERE THE PARINFO ASSIGNMENT IS DONE.  
        if tag_size.type eq 8 then begin
           ;; We need to recursively step down the structure tree.
           ;; IDL insists on putting an extra dimension on arrays that
           ;; are implicitly created, so explicitly create one if we
           ;; have to.
           if npfo eq 1 then begin
              rparinfo = parinfo.(pfotagnum) 
           endif else begin
              rparinfo = make_array(npfo, value=parinfo[0].(pfotagnum))
              rparinfo[*] = parinfo[*].(pfotagnum) 
           endelse
           pfo_struct_assign, rparinfo, tagval=tagval.(i)
           for ipfo=0,npfo-1 do begin
              parinfo[ipfo].(pfotagnum) = rparinfo[ipfo]
           endfor ;; copying back
        endif else begin ;; tagval is not a structure
           ;; Let IDL to the array transfer by itself.  If you see an
           ;; error here, you probably have the wrong number of
           ;; elements in this field of tagval.
           parinfo.(pfotagnum) = tagval.(i)
        endelse


     endfor ;; tagval tagnames 

  endif else begin
     if size(tagname, /type) ne 7 then $
       message, 'ERROR: tagname must be type string.  Fix the code here if you want to add the ability to access tags by their numbers'

     ;; As above, there are two cases: one-to-one and one-to-many

     tagname_size = size(tagname, /structure)
     tagval_size  = size(tagval, /structure)

     ;; Step through tagname one element at a time
     for i=0,tagname_size.N_elements-1 do begin
        if tagval_size.N_dimensions eq tagname_size.N_dimensions + 1 then begin
           command = 'pfo_struct_assign, parinfo, idx, tagval={' + $
                     tagname[i] + '	:	tagval[*,i]}'
           
        endif else begin
           if tagval_size.N_dimensions ne $
             tagname_size.N_dimensions then $
             message, 'ERROR: tagname and tagval have incompatible dimensions'
           command = 'pfo_struct_assign, parinfo, idx, tagval={' + $
                     tagname[i] + '	:	tagval[i]}'

        endelse
        ;; Execute the command we have built and check for an error.
        if NOT execute(command) then begin
           print, command
           message, 'ERROR: command shown above failed.'
        endif

     endfor ;; each tagname

     
  endelse

  
  ;; Copy our results back into the array that has been passed by
  ;; reference (which according to the IDL documentation, might itself
  ;; be copied)
  if nidx ne npfo then begin
     inparinfo[idx] = parinfo
  endif else begin
     ;; Save time by not copying the whole array
     inparinfo = temporary(parinfo)
  endelse


end

