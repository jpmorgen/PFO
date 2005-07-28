; +
; $Id: struct_array_assign.pro,v 1.3 2005/07/28 23:49:18 jpmorgen Exp $

; struct_array_assign

;; This routine was developed for the pfo package and mantains some of
;; its nomenclature, but the routine itself is general.  PFO stands
;; for "parinfo," which is an array of a particular structure.  The
;; parinfo structure contains fields defining attributes of a
;; parameter in a fitting routine.

;; This resembles IDL's struct_assign, except (1) it handles arrays of
;; structures properly and (2) it doesn't zero out the input structure
;; array (this could be changed).  The command line syntax is
;; therefore slightly different:

;; struct_array_assign, struct_array, idx, tagname=tagname, tagval=tagval

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
;; single value copied into the entire input array, or an array of
;; values copied one by one.

;; Only the top level of the structure is searched for matching tags
;; (again, that could be modified, but I would recommend only with an
;; option), so for tags deeper in your structure, you will have to
;; make a temporary copy of the sub-structure, pass it here, and merge
;; it back in later.


;; Example use:
;;
;;    struct_array_assign, parinfo, idx, $
;;               tagval = {value	:	value}
;;
;;    struct_array_assign, parinfo, idx, tagname = 'error', tagval = error


; -

pro struct_array_assign, inparinfo, idx, tagname=tagname, tagval=intagval
                     

  ;; Let IDL do the error checking on array bounds
;;  ON_ERROR, 2

  npfo = N_elements(inparinfo)
  if npfo eq 0 or N_elements(intagval) eq 0 then return
  tagval = intagval

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
     CATCH, /cancel
     message, /NONAME, !error_state.msg
  endif

  ;; Set up idx if none specified
  if N_elements(idx) eq 0 then $
    idx = lindgen(npfo)

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
          message, 'ERROR: tag ' + tvtagnames[i] + ' not present in parinfo structure'
        pfo_size = size(parinfo.(pfotagnum), /structure)
        tag_size = size(tagval.(i), /structure)
;        ;; IDL has an annoying habit of making expressions with
;        ;; dimensions [1,*] instead of [*] when you try to peal
;        ;; something out of the middle of a sub-structure or
;        ;; sub-array.  Trick my code into ignore that for the purposes
;        ;; of this error message.  
;        while pfo_size.dimensions[0] eq 1 do begin
;           nd = pfo_size.N_dimensions
;           pfo_size.dimensions = pfo_size.dimensions[1:nd-2]
;           pfo_size.N_dimensions = nd - 1
;        endwhile
;        while tag_size.dimensions[0] eq 1 do begin
;           nd = tag_size.N_dimensions
;           tag_size.dimensions = tag_size.dimensions[1:nd-2]
;           tag_size.N_dimensions = nd - 1
;        endwhile
;        if pfo_size.dimensions[0] eq 1 then $
;          pfo_size.N_dimensions = pfo_size.N_dimensions - 1
;        if pfo_size.N_dimensions ne tag_size.N_dimensions + 1 and $
;          pfo_size.N_dimensions ne tag_size.N_dimensions then $
;           message, 'ERROR: tagval ' + tvtagnames[i] + ' has the wrong number of dimensions.'
;
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
           struct_array_assign, rparinfo, tagval=tagval.(i)
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
     ;; Tagval is not a structure

     ntn = N_elements(tagname)
     pfotagnames = tag_names(parinfo)
     if ntn gt N_elements(pfotagnames) and size(tn, /type) ne 7 then $
       message, 'ERROR: too many numeric tags specified ' + strtrim(ntn, 2)

     ;; Initialize some variables that will help us figure out the size
     ;; and shape of tagval.  First assume tagval is a scaler (ntv=1)
     tvsize = size(tagval, /structure)
     ntv = tvsize.N_elements
     if ntv gt 1 then begin
        ;; The way IDL handles arrays, the last dimension is the one
        ;; that varies most slowly.  So, if we have a list of things,
        ;; each with 2 dimensions, the size of the last dimension
        ;; tells us how many elements are in the list.
        ntv = tvsize.dimensions[tvsize.N_dimensions-1]
     endif
     ;; Tag value column, in case we have multiple tags and the values
     ;; are listed as rows in tagval
     tvc = 0
     itv = 0

     ;; Loop through command-line specified tag names (if any)
     for itn=0, ntn-1 do begin
        tn=tagname[itn]
        ;; convert from tagname to pfo tag number, if necessary
        if size(tn, /type) eq 7 then begin
           tn = where(strcmp(tn[itn], pfotagnames, /fold_case) eq 1, count)
           if count eq 0 then begin
              message, /INFORMATIONAL, 'WARNING: tagname ' + tagname[itn] + ' not found in structure'
stop
              CONTINUE
           endif
        endif ;; tn converted to numeric form

        ;; Use the parinfo tag to figure out what the dimensionality of
        ;; tagval might be.  
        pfotsize = size(parinfo[0].(tn), /structure)
        pfotnd = pfotsize.N_dimensions
        if pfotnd gt 0 then begin
           ;; Erase trivial trailing dimensions from the pfo tag.
           if pfotsize.dimensions[pfotnd-1] eq 1 then begin
              pfotsize.dimensions = [pfotsize.dimensions[0:pfotnd-2], 0]
              pfotnd = pfotnd - 1
           endif
        endif

        ;; Handle all possible dimensions of tagval.  
        if ntv eq 1 then begin
           ;; Easy case: copying one tag value to all the tags in the list
           itv = 0
        endif
        if ntv eq ntn then begin
           ;; One tag value per tag name
           itv = itn
        endif
        if ntv eq nidx then begin
           ;; One tag value per parinfo.  
           itv = '*'
           if ntn gt 1 then begin
              ;; We need to distribute the values over several
              ;; tagnames.  Grab the right columns of tagval.
              itv = string(tvc, ':', pfotnd-1)
              tvc = tvc + pfotnd
           endif
        endif

        ;; Now we have to build up an IDL statement that extracts the
        ;; right section of tagval
        tvs = 'tagval['
        for id=0, pfotnd-1 do begin
           tvs = tvs + '*, '
        endfor
        if size(itv, /type) eq 7 then begin
           tvs = tvs + itv
        endif else begin
           tvs = tvs + strjoin(itv, ',')
        endelse
        tvs = tvs +  ']'
        command = 'struct_array_assign, parinfo, idx, tagval={' + $
                  tagname[itn] + ' : ' + tvs + '}'

        ;; Execute the command we have built and check for an error.
        if NOT execute(command) then begin
           message, command, /CONTINUE
stop
           message, 'ERROR: command shown above failed.'
        endif

     endfor ;; Each tag name
;
; else begin
;     ;; tagval is not a structure
;
;     ;; As above, there are two cases: one-to-one and one-to-many
;
;     tagname_size = size(tagname, /structure)
;     tagval_size  = size(tagval, /structure)
;
;     tvtagnames = tag_names(tagval)
;
;
;     ;; Step through tagname one element at a time
;     for i=0,tagname_size.N_elements-1 do begin
;        pfotagnum = where(tagname[i] eq pfotagnames, count)
;     
;        ;; --> in the case of path, it does, but for the wrong reason
;        if tagval_size.N_dimensions eq $
;          tagname_size.N_dimensions + 1 then begin
;           case tagval_size.N_dimensions of
;              1 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval}'
;              2 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,i]}'
;              3 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,i]}'
;              4 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,*,i]}'
;              5 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,*,*,i]}'
;              6 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,*,*,*,i]}'
;              7 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,*,*,*,*,i]}'
;              8 : command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                            tagname[i] + ': tagval[*,*,*,*,*,*,*,i]}'
;              else : message, 'ERROR: invalid number of dimensions for tagval ' + string(tagval_size.N_dimensions)
;           endcase
;        endif else begin
;           if tagval_size.N_dimensions ne $
;             tagname_size.N_dimensions then $
;             message, 'ERROR: tagname and tagval have incompatible dimensions'
;           command = 'struct_array_assign, parinfo, idx, tagval={' + $
;                     tagname[i] + '	:	tagval[i]}'
;
;        endelse
;        ;; Execute the command we have built and check for an error.
;        if NOT execute(command) then begin
;           message, command, /CONTINUE
;           message, 'ERROR: command shown above failed.'
;        endif
;
;     endfor ;; each tagname
     
  endelse ;; tagval not a structure

  
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

