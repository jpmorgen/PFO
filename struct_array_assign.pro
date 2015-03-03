; +
; $Id: struct_array_assign.pro,v 1.4 2010/08/31 20:14:45 jpmorgen Exp jpmorgen $

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

pro struct_array_assign, inparinfo, idx, tagname=tagname, tagval=intagval, $
  all_idx=all_idx
                     
  init = {pfo_sysvar}
  init = {tok_sysvar}

  npfo = N_elements(inparinfo)
  ;; Quietly return if we have no parinfo to assign into
  if npfo eq 0 or N_elements(intagval) eq 0 then $
    return
  tagval = intagval

  if !pfo.debug le 0 then begin
     ;; Let IDL do the error checking on array bounds
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        ;; If there is an error, copy parinfo back onto inparinfo so
        ;; that the calling routine's parinfo doesn't get nuked.
        if nidx ne npfo then begin
           inparinfo[idx] = parinfo
        endif else begin
           ;; Save time by not copying the whole array
           inparinfo = temporary(parinfo)
        endelse
        CATCH, /cancel
        message, /NONAME, !error_state.msg
     endif
  endif ;; not debuggin

  ;; Set up idx if none specified
  if N_elements(idx) eq 0 then $
    idx = lindgen(npfo)
  nidx = N_elements(idx)

  ;; Establish our internal parinfo array
  if nidx ne npfo then begin
     parinfo = inparinfo[idx]
  endif else begin
     ;; Save memory and time by not copying the whole array
     parinfo = temporary(inparinfo)
  endelse

  ;; Check to see if we are specifying everything in tagval
  if size(tagval, /type) eq !tok.struct and N_elements(tagname) eq 0 then begin
     ;; Tagval has the entire structure we want to assign.  There are
     ;; two cases: a single tag name with an array of tag values
     ;; corresponding to the values to assign in each parinfo record,
     ;; or an array of tag names with corresponding tag values.  In
     ;; either case, step through the tag names in tagval one at a
     ;; time.
     tvtagnames = tag_names(tagval)
     pfotagnames = tag_names(parinfo)
     ;; Step through tagval tags one at a time
     for i=0,N_elements(tvtagnames)-1 do begin
        pfotagnum = where(tvtagnames[i] eq pfotagnames, count)
        if count eq 0 then $
          message, 'ERROR: tag ' + tvtagnames[i] + ' not present in parinfo structure'
        ;; Work with size structures, since they contain all our
        ;; information.
        pfo_size = size(parinfo.(pfotagnum), /structure)
        tag_size = size(tagval.(i), /structure)

        ;; Check to see if this particular tag of tagval is itself a
        ;; structure.  If so, the easiest way syntactically to get all
        ;; the information over from the tagval struct to the parinfo
        ;; is to recursively step down the structures until we can
        ;; copy non-structure information
        if tag_size.type eq !tok.struct then begin
           ;; We need to recursively step down the structure tree.
           ;; rparinfo will be where we put our recursive output.
           ;; IDL insists on putting an extra dimension on arrays that
           ;; are implicitly created, so explicitly create one if we
           ;; have to.
           if npfo eq 1 then begin
              rparinfo = parinfo.(pfotagnum) 
           endif else begin
              rparinfo = make_array(npfo, value=parinfo[0].(pfotagnum))
              ;; Make sure our array reference is right
              rparinfo[*] = parinfo[*].(pfotagnum) 
           endelse
           ;; Here is the recursive call.  all_idx is used below and
           ;; we don't want to have more copies of that than we
           ;; need, so pass it along by reference to the recursive
           ;; calls
           if N_elements(all_idx) eq 0 then $
             all_idx = lindgen(npfo)
           struct_array_assign, rparinfo, tagval=tagval.(i), all_idx=all_idx
           ;; Now put our recursively generated sub-parinfo back into
           ;; the current tag in parinfo.  Note array in structure
           ;; needs to be indexed this way rather than with *.
           parinfo[all_idx].(pfotagnum) = rparinfo
        endif else begin ;; this particular tag in tagval is not a structure
           ;; Let IDL to the array transfer by itself.  If you see an
           ;; error here, you probably have the wrong number of
           ;; elements in this field of tagval.
           parinfo.(pfotagnum) = tagval.(i)
        endelse

     endfor ;; tagval tagnames 

  endif else begin

     ;; Tagname and tagval are both specified

     ntn = N_elements(tagname)
     pfotagnames = tag_names(parinfo)
     if ntn gt N_elements(pfotagnames) and size(tn, /type) ne !tok.string then $
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
        tn = tagname[itn]
        ;; convert from tagname to pfo tag number, if necessary
        if size(tn, /type) eq !tok.string then begin
           tn = where(strcmp(tn[itn], pfotagnames, /fold_case) eq 1, count)
           if count eq 0 then begin
              message, /INFORMATIONAL, 'WARNING: tagname ' + tagname[itn] + ' not found in structure'
              CONTINUE
           endif
        endif ;; tn converted to numeric form

        ;; Use the parinfo tag to figure out what the dimensionality of
        ;; tagval might be.  
        pfotsize = size(parinfo[0].(tn), /structure)
        pfotnd = pfotsize.N_dimensions
        if pfotnd gt 1 then begin
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
           this_tagval = tagval
        endif
        if ntv eq ntn then begin
           ;; One tag value per tag name
           itv = itn
           this_tagval = tagval[itn]
        endif
        if ntv eq nidx then begin
           ;; One tag value per parinfo element.  
           itv = '*'
           this_tagval = tagval
           if ntn gt 1 then begin
              ;; We need to distribute the values over several
              ;; tagnames.  Grab the right columns of tagval.
              itv = string(tvc, ':', pfotnd-1)
              this_tagval = tagval[tvc:pfotnd-1]
              tvc = tvc + pfotnd
           endif
        endif

        ;; Do the recursive procedure call having converted our
        ;; tagname/tagval array into a structure
        new_tagval = create_struct(tagname[itn], this_tagval)
        call_procedure, 'struct_array_assign', parinfo, idx, tagval=new_tagval

     endfor ;; Each tag name
     
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

