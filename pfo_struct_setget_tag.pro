;+
; NAME: pfo_struct_setget_tag
;
; PURPOSE: Set or get tag(s) in a pfo_struct system (e.g. a parinfo).
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; DESCRIPTION: 

;   This allows conversion of keywords to and from tag assignments in
;   a pfo_struct-enabled environment.  The top-level tags in the
;   parinfo structure (e.g. pfo, pfo_link, pfo_ROI, etc.) should be
;   defined with the pfo_struct_new system and need to have properly
;   formatted <tag>_struct__set_tag and <tag>_struct__get_tag
;   routines.  See the file pfo_struct__define.pro for an example.

; NOTE: individual tags use __set_tag or __get_tag (two underscores),
; this calling routine just uses one

; INPUTS: parinfo: parinfo array

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   idx: indices into parinfo
;   set or get (required) set or get tag

;   taglist_series: a list of top-level tags in the parinfo structure
;       which will be procesed one-by-one, or in series.  The keywords
;       passed with the _REF_EXTRA mechanism are passed on to the
;       __set/get_tag procedures of the tags and "soaked up" as they
;       are encountered.  In other words, the first __set/get_tag
;       routine that uses a keyword prevents subsequent tags from
;       using that keyword.  Note this induces an order-dependency in
;       the listing of tags in the parinfo structure which can have
;       unintended results.

;   taglist_strict: use the a _STRICT_EXTRA-like construct to make
;   	sure _all_ of the tags in a taglist_series are used.

;   taglist_parallel: like taglist_series, but all _REF_EXTRA keywords
;          are always passed to all of the __set/get_tag routines in
;          the taglist_parallel list

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
; $Id: pfo_struct_setget_tag.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_struct_setget_tag.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_struct_setget_tag, parinfo, idx=idx, $
  set=set, get=get, $
  taglist_series= taglist_series, $
  taglist_parallel=taglist_parallel, $
  taglist_strict=taglist_strict, $
  next=next, $
  _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}

  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_struct_setget_tag, /set | /get, parinfo[, idx=idx], [tag=tag, ...], '
     endif
  endif ;; not debugging

  ;; Sanity checking on input
  if keyword_set(set) + keyword_set(get) ne 1 then $
    message, 'ERROR: either /set or /get must be specified'

  ;; Check to see if there are any unprocessed keywords left on our
  ;; command line.  If not, we don't have anything to do
  if N_elements(extra) eq 0 then $
    return

  N_tls = N_elements(taglist_series)
  N_tlp = N_elements(taglist_parallel)
  if N_tls gt 0 and N_tlp gt 0 then $
      message, 'ERROR: specify either taglist_series or taglist_parallel'

  ;; Establish our default, which is to soak up the keywords in the
  ;; order in which they appear (taglist_series).  Note that if we are
  ;; in the middle of processing a taglist_series (called from a
  ;; __set_tag or__get_tag routine), /next should be set, which needs
  ;; to suppress refilling the list (yes, I caught the infinite loop
  ;; before the first debug ;-)
  if N_tls eq 0 and N_tlp eq 0 and NOT keyword_set(next) then begin
     ;; Here is our default taglist, if none was specified
     taglist_series = tag_names(parinfo)
     ;; Prepend TOP_LEVEL_STRUCT, so the top level tags get processed
     ;; too
     junk = where(taglist_series eq 'TOP_LEVEL_STRUCT', count)
     if count gt 0 then $
       taglist_series = [parinfo[0].top_level_struct, temporary(taglist_series)]
  endif ;; default taglist_series
  N_tls = N_elements(taglist_series)

  ;; Here is where we drop out of our semi-recursive loop if we are
  ;; processing tags sequentially in taglist_series
  if keyword_set(next) and N_tls eq 0 then $
    return

  ;; Here is where we differentiate between /set and /get
  if keyword_set(set) then $
    setget = 'set'
  if keyword_set(get) then $
    setget = 'get'

  ;; taglist_series case
  if N_tls gt 0 then begin
     ;; Sanity check taglist
     if size(/type, taglist_series) ne !tok.string then $
       message, 'ERROR: taglist_series must be a string'
     ;; Start with the 0th tag in the list unless
     it = 0

     ;; For pfo debugging, we need to do this in two steps.  Always
     ;; quietly ignore routines that don't resolve. 
     CATCH, err
     if err ne 0 then begin
        ;; Keep the CATCH in effect so we can loop around here until
        ;; we find the first __set/get_tag routine that resolves
        it += 1
        ;; Make sure we don't run past the end of our list
        if it ge N_tls then $
          return
     endif ;; CATCH
     ;; Justify the names of our tags and top-level structure.  The
     ;; top_level_struct is the full structure name (including
     ;; _struct), the tags don't include _struct.  Assume we
     ;; have a tag...
    setget_tag_proc = taglist_series[it]+'_struct__'+setget+'_tag'
     ;; ...and check for the top-level structure
     spos = strpos(taglist_series[it], '_STRUCT', /reverse_search)
     if spos eq strlen(taglist_series[it]) - strlen('_STRUCT') then $
       setget_tag_proc = taglist_series[it]+'__'+setget+'_tag'

     ;; routine_info throws an error if the routine doesn't exist
     junk = routine_info(setget_tag_proc, /source)

     ;; If we made it here, we should have a good setget_tag_proc to
     ;; call.  Handle the error messages in a configurable way.
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, /INFORMATIONAL, 'WARNING: ' + strtrim(setget_tag_proc, 2) + ' produced the above error.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting.' 
           it += 1
           ;; Make sure we don't run past the end of our list
           if it ge N_tls then $
             return
        endif ;; quietly ignore problems in subsequent __set/get_tag routine
     endif ;; not debugging

     ;; Check to see if we have any more tags to process
     if it+1 le N_tls-1 then begin
        call_procedure, setget_tag_proc, $
                        parinfo, idx=idx, $
                        taglist_series=taglist_series[it+1:N_tls-1], $
                        taglist_strict=taglist_strict, _EXTRA=extra
     endif ;; More tags in our list
     ;; Check to see if we are down to just one tag in
     ;; taglist_series.  Use IDL's _STRICT_EXTRA, if we are and
     ;; taglist_strict is set.
     if it eq N_tls-1 then begin
        if keyword_set(taglist_strict) then $
           call_procedure, setget_tag_proc, parinfo, idx=idx, $
                           _STRICT_EXTRA=extra $
        else $
           call_procedure, setget_tag_proc, parinfo, idx=idx, _EXTRA=extra
     endif ;; last tag in taglist_series

  endif ;; taglist_series

  ;; taglist_parallel case.  Just call each <tag>_struct__set/get_tag
  ;; routine with all of the _EXTRA parameters.
  if N_tlp gt 0 then begin
     ;; Check for the /taglist_parallel or taglist_parallel=0 cases
     if size(/type, taglist_parallel) ne !tok.string and N_tlp eq 1 then begin
        ;; If taglist_parallel=0, we are trying to turn off taglist
        ;; processing
        if taglist_parallel eq 0 then $
          return
        ;; Here is our /taglist_parallel case.
        taglist_parallel = tag_names(parinfo) 
     endif ;; checking for /taglist_parallel
     ;; Sanity check taglist
     if size(/type, taglist_parallel) ne !tok.string then $
       message, 'ERROR: taglist_parallel must be a string'
     for it=0, N_tlp-1 do begin
        ;; For pfo debugging, we need to do this in two steps.  Always
        ;; quietly ignore routines that don't resolve. 
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           CONTINUE
        endif
        ;; Look out for out TOP_LEVEL_STRUCT and replace it with the
        ;; structure name so we can properly call its __set/get_tag method
        if taglist_parallel[it] eq 'TOP_LEVEL_STRUCT' then $
          taglist_parallel[it] = parinfo[0].top_level_struct
        ;; Justify the names of our tags and top-level structure.  The
        ;; top_level_struct is the full structure name (including
        ;; _struct), the tags don't include _struct.  Assume we have a
        ;; tag...
        setget_tag_proc = taglist_parallel[it]+'_struct__'+setget+'_tag'
        ;; ...and check for the top-level structure
        spos = strpos(taglist_parallel[it], '_STRUCT', /reverse_search)
        if spos eq strlen(taglist_parallel[it]) - strlen('_STRUCT') then $
          setget_tag_proc = taglist_parallel[it]+'__'+setget+'_tag'

        ;; routine_info throws an error if the routine doesn't exist
        junk = routine_info(setget_tag_proc, /source)

        ;; If we made it here, we should have a good setget_tag_proc to
        ;; call.  Handle the error messages in a configurable way.
        if !pfo.debug le 0 then begin
           CATCH, err
           if err ne 0 then begin
              CATCH, /CANCEL
              message, /NONAME, !error_state.msg, /CONTINUE
              message, /INFORMATIONAL, 'WARNING: ' + strtrim(setget_tag_proc, 2) + ' produced the above error.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting.' 
              CONTINUE
           endif ;; quietly ignore problems in __set_tag routine
        endif ;; not debugging
        call_procedure, setget_tag_proc, parinfo, idx=idx, _EXTRA=extra
     endfor ;; each tag in taglist_parallel
  endif ;; taglist_parallel

end
