;+
; NAME: pfo_link_struct__define
;
; PURPOSE: Define the parinfo.pfo_link substructure that holds
; information that helps link parameters together in the MPFIT system
;
; CATEGORY: PFO, optional add-in
;
; CALLING SEQUENCE: Used in the pfo_struct_new system.  See EXAMPLES
;
; DESCRIPTION: MPFIT links parameters together by using the indices of
; the parameters in the parinfo array.  Hard-coding index values
; violates the "slice and splice" methods of PFO.  PFO_LINK helps
; bridge the gap.  The pfo_link_struct contains tags that help
; identify "master" parameters to which "slave" parameters are tied
; (sorry for the imagery).
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
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
; Unlike tied in MPFIT, no fancy arbitrary expressions are allowed.  If
; you need that, make a PFO function for it ;-).  
;
; IDLVM without a license won't work with linking: The MPFIT
; tied code uses the EXECUTE statement
;
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_link_struct__define.pro,v 1.6 2011/11/21 15:25:01 jpmorgen Exp $
;
; $Log: pfo_link_struct__define.pro,v $
; Revision 1.6  2011/11/21 15:25:01  jpmorgen
; Get intralink working, add hand_tied, calculate effect of precision on
; ftype rounding, improve documentation
;
; Revision 1.5  2011/11/18 15:49:58  jpmorgen
; Got working with basic pfo_scint function, still some work to do
;
; Revision 1.4  2011/09/22 23:47:05  jpmorgen
; Fix bug of no pfo_obj
;
; Revision 1.3  2011/09/16 13:46:02  jpmorgen
; Improved substantially so that this works with an __update method,
; etc. in the pfo_obj system
;
; Revision 1.2  2011/01/03 21:48:43  jpmorgen
; Fixed bug in pfo_funct
;
;-

;;
pro pfo_link_struct__update, $
   parinfo, $
   pfo_obj=pfo_obj, $ ;; We use the pfo_finfo system, which, if we are in object-oriented mode, uses information in the pfo_obj
   completed_updates=completed_updates, $	;; (input/output) list of completed updates
   _REF_EXTRA=extra

  ;; Don't run more than once.
  if pfo_struct_updated(completed_updates) then $
     return

  ;; We need to make sure that fnums (integer part of ftype) are
  ;; defined when when look for identical functions in autolink
  pfo_parinfo_update, parinfo, required_tags='pfo', update_only_tags='pfo', $
                      completed_updates=completed_updates, $
                      pfo_obj=pfo_obj

  ;; Use only active parameters.  Also, don't use parameters "tied" by
  ;; hand.
  use_idx = where((parinfo.pfo.status AND !pfo.active) gt 0 and $
                  (parinfo.pfo_link.link_status AND !pfo.hand_tied) eq 0, npar)
  ;; Quietly return if we don't have any such parameters to manage
  if npar eq 0 then $
     return

  ;; Check for auto_links first
  auto_link_idx = where(parinfo[use_idx].pfo_link.auto_link ne !pfo.not_used, count) 
  if count gt 0 then begin
     ;; unwrap
     auto_link_idx = use_idx[auto_link_idx]

     ;; Clear away all link_status and to_IDs, since we are going to
     ;; assign them automatically
     parinfo[auto_link_idx].pfo_link.link_status = !pfo.not_used
     parinfo[auto_link_idx].pfo_link.to_ID = !tok.nowhere

     ;; Handle interlinks (links between functions) first, otherwise
     ;; intralinks in master function don't get set up properly.
     interlink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink, count)
     ;; We need to cycle through each kind of function
     while count gt 0 do begin
        ;; unwrap
        interlink_idx = use_idx[interlink_idx]
        ;; Find the first function and make it the master.  Making the
        ;; whole function master is why we have to do this first,
        ;; since intralinks may require some of these parameters to be
        ;; slaves
        master_idx = pfo_parinfo_parse(/indices, parinfo=parinfo, expand_idx=interlink_idx[0], pfo_obj=pfo_obj)
        parinfo[master_idx].pfo_link.link_status = !pfo.master
        ;; Check to make sure all parameters have the same ID
        ID = parinfo[master_idx].pfo_link.linkID
        if N_elements(uniq(ID, sort(ID))) gt 1 then begin
           message, /CONTINUE, 'WARNING: parinfo.pfo_link.linkID values are out of sync.  linkID should have the same value for all parameters in each function.  Reinitializing.'
           parinfo[master_idx].pfo_link.linkID = !tok.nowhere
        endif ;; non-syncronized linkIDs
        ;; --> having trouble with functions that already have linkID
        ;; set to the same value?
        ;; Check to see if we need to initialize ID
        if ID[0] lt 0 then begin
           parinfo[master_idx].pfo_link.linkID = max(parinfo.pfo_link.linkID) + 1
        endif ;; initializing ID
        ;; Now make local ID (possibly just initialized) just a scaler
        ID = parinfo[master_idx[0]].pfo_link.linkID
        ;; Find all of our master's slaves
        fnums = floor(parinfo.pfo.ftype)
        master_fnum = fnums[master_idx[0]]
        slave_idx = where(fnums[use_idx] eq master_fnum and $ ;; same functions
                          parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink and $ ;; auto interlink
                          parinfo[use_idx].pfo_link.link_status eq !pfo.not_used, count) ;; not our master
        ;; Check to see if we found slaves.  If not, it is not a
        ;; problem to have marked our first and only function of this
        ;; type as the master, above.
        if count gt 0 then begin
           ;; unwrap
           slave_idx = use_idx[slave_idx]
           ;; We found some slaves.  Mark them as such, link it to the
           ;; master ID we found above.  The to_ftype should be
           ;; already assigned
           parinfo[slave_idx].pfo_link.link_status = !pfo.slave
           parinfo[slave_idx].pfo_link.to_ID = ID
        endif ;; found slaves
        ;; Get ready to handle our next interlinked function (if any)
        interlink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink and $ ;; interlinked functions
                              parinfo[use_idx].pfo_link.link_status ne !pfo.master and $ ;; not a master (which we would have just assigned)
                              parinfo[use_idx].pfo_link.to_ID eq !tok.nowhere, count) ;; slave status not assigned yet
     endwhile

     ;; Handle intralinks (links within one function) next.  This
     ;; enables us to set parameters to slaves that were
     ;; indiscriminately set to master in the interlink code.
     intralink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.intralink, N_intralink)
     if N_intralink gt 0 then begin
        ;; unwrap
        intralink_idx = use_idx[intralink_idx]
        ; Mark intralinks as slaves
        parinfo[intralink_idx].pfo_link.link_status = !pfo.slave
     endif
     ;; Make sure intralink functions have a linkID and set the
     ;; intralink's to_ID to that value
     for ip=0, N_intralink-1 do begin
        f_idx = pfo_parinfo_parse(/indices, parinfo=parinfo, expand_idx=intralink_idx[ip], pfo_obj=pfo_obj)
        ;; Check to make sure all parameters have the same ID
        ID = parinfo[f_idx].pfo_link.linkID
        if N_elements(uniq(ID, sort(ID))) gt 1 then begin
           message, /CONTINUE, 'WARNING: parinfo.pfo_link.linkID values are out of sync.  linkID should have the same value for all parameters in each function.  Reinitializing.'
           parinfo[f_idx].pfo_link.linkID = !tok.nowhere
           ID = parinfo[f_idx].pfo_link.linkID
        endif ;; non-syncronized linkIDs
        ;; Check to see if we need to initialize ID
        if ID[0] lt 0 then begin
           ID = max(parinfo.pfo_link.linkID) + 1
           parinfo[f_idx].pfo_link.linkID = ID
        endif ;; initializing ID
        parinfo[intralink_idx[ip]].pfo_link.to_ID = ID[0]
     endfor ;; each intralinked parameter

  endif ;; auto_link

  ;; Now that we have our masters and slaves set up, populate the
  ;; MPFIT parinfo.tied tag

  ;; Clear away any old mentions of tied unless the user is
  ;; specifically marked them with !pfo.hand_tied.
  parinfo[use_idx].tied = ''  

  ;; Get slave indexes
  slave_idx = where(parinfo[use_idx].pfo_link.link_status eq !pfo.slave, nslaves)
  if nslaves gt 0 then begin
     ;; Unwrap
     slave_idx = use_idx[slave_idx]
     ;; The number of significant digits on single-precision floating
     ;; point is a little over 7
     ;; (http://en.wikipedia.org/wiki/Single_precision_floating-point_format).
     ;; Calculate the number of significant digits we have left for
     ;; the fractional part depending on how many digits our maximum
     ;; fnum has.  Add 1 to fix ftype, just in case all we have is
     ;; fnum=0.  Add 1 to the alog10, since we are counting digits,
     ;; not powers of 10.
     fdigits = 7 - round(alog10(max(fix(parinfo.pfo.ftype)+1))+1)
  endif

  ;; This loop gets skipped if there are no slaves.  Too bad if someone
  ;; sets to_ID and to_ftype and forgets to set status=!pfo.slave
  for is=0,nslaves-1 do begin
     ;; Get our to_ID and to_ftype from the slave
     to_ID = parinfo[slave_idx[is]].pfo_link.to_ID
     to_ftype = parinfo[slave_idx[is]].pfo_link.to_ftype
     ;; Make sure we can find our ID
     ID_idx = where(parinfo.pfo_link.linkID eq to_ID, count)
     if count eq 0 then begin
        message, 'WARNING: pfo_link.linkID ' + strtrim(to_ID, 2) + ' not found.  Skipping.', /CONTINUE
        CONTINUE
     endif ;; to_ID points to missing linkID

     ;; Make sure we have just one parameter that we are trying to
     ;; point to.  
     to_ftype_idx = where(round(10^fdigits * pfo_frac(parinfo[ID_idx].pfo.ftype)) eq $
                          round(10^fdigits * to_ftype), count)
     if count ne 1 then begin
       message, 'WARNING: found ' + strtrim(count, 2) + ' parameters with fractional ftype = pfo_link.to_ftype = ' + strtrim(to_ftype, 2) + ' Non-unique linkID?  Skipping', /CONTINUE
       CONTINUE
    endif ;; duplicate ftype (unlikely) or duplicate linkID (more likely)
     ;; unwrap
     to_ftype_idx = ID_idx[to_ftype_idx]
     ;; OK, here we are finally ready to make the MPFIT tied assigment
     parinfo[slave_idx[is]].tied = 'P[' + strtrim(to_ftype_idx, 2) + ']'
     ;; While we are here, synchronize the values for the benefit of
     ;; pfo_funct calls independent of mpfit or tnmin
     parinfo[slave_idx[is]].value = parinfo[to_ftype_idx].value
     parinfo[slave_idx[is]].error = !values.d_nan
     ;; For now leave the rest of the tags alone, since just
     ;; specifying tied results in the proper calculation of free
     ;; parameters in mpfit and tnmin.  It also gives the user an
     ;; additional cache of those tags.     
  endfor ;; Each slave

  ;; Mark our update as complete regardless of how many errors we generated.
  pfo_struct_update_complete, completed_updates

  
end

pro pfo_link_struct__get_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_setget_tag
   strict= strict, $ ;; See pfo_setget_tag
   _REF_EXTRA   = extra, $
   linkID       = linkID    , $
   link_status     = link_status, $
   to_ID        = to_ID , $
   to_ftype     = to_ftype, $
   auto_link	= auto_link

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our tags into the
  ;; keywords

  if arg_present(linkID   ) or N_elements(linkID   ) ne 0 then linkID    = parinfo[idx].pfo.linkID   
  if arg_present(link_status ) or N_elements(link_status ) ne 0 then link_status  = parinfo[idx].pfo.link_status 
  if arg_present(to_ID    ) or N_elements(to_ID    ) ne 0 then to_ID     = parinfo[idx].pfo.to_ID    
  if arg_present(to_ftype ) or N_elements(to_ftype ) ne 0 then to_ftype  = parinfo[idx].pfo.to_ftype 
  if arg_present(auto_link) or N_elements(auto_link) ne 0 then auto_link = parinfo[idx].pfo.auto_link

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end 

;; This allows easy setting of tags in the pfo_link structure (a
;; top-level structure or as a pfo_link tag in a parinfo) from the
;; command line.  Alternately, you could just set the tags directly by
;; hand in your code.
pro pfo_link_struct__set_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_setget_tag
   strict= strict, $ ;; See pfo_setget_tag
   _REF_EXTRA   	= extra, $
   linkID       = linkID    , $
   link_status     = link_status, $
   to_ID        = to_ID , $
   to_ftype     = to_ftype, $
   auto_link	= auto_link

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags

  if N_elements(linkID	) ne 0 then parinfo[idx].pfo_link.linkID   = linkID	
  if N_elements(link_status) ne 0 then parinfo[idx].pfo_link.link_status   = link_status	
  if N_elements(to_ID	) ne 0 then parinfo[idx].pfo_link.to_ID    = to_ID	
  if N_elements(to_ftype) ne 0 then parinfo[idx].pfo_link.to_ftype = to_ftype
  if N_elements(auto_link) ne 0 then parinfo[idx].pfo_link.auto_link   = auto_link	

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end


;; Using the pfo_struct_new system, this is how the structure can be
;; initialized to non-null values, if necessary.  This also returns
;; the description of the structure for use in pfo_struct_append and
;; pfo_parinfo_template
function pfo_link_struct__init, descr=descr

  ;; Get IDL's version of null values
  pfo_link_struct = {pfo_link_struct}
  ;; Assign our own
  ;; Default [master] ID is -1 as a flag for auto_link stuff
  pfo_link_struct.linkID = !tok.nowhere
  descr = $
    {README	: 'Structure to keep track of which parameters are linked to each other by the MPFIT "tied" system.  There are master parameters and slave parameters (sorry for the imagery).  Master parameters are the ones that are varied by MPFIT, slave parameters come along for the ride', $
     linkID	: 'Set this in the master function, one ID per function, the same ID for all the parameters, even for the ones that don''t participate', $
     link_status	: '0 = not linked, 1 = master, 2 = slave (token available in !pfo)', $
     to_ID	: 'set in the slave functions, indicating a master function ID', $
     to_ftype	: 'set in the slave functions indicating the _fractional_ ftype of the parameter in the master function (pfo_frac is helpful)', $
     auto_link  : 'Used to automatically assign master/slave relationships in identical functions.  0 = no_used, 1 (intralink)= link to parameter in this instance of the function, 2 (interlink) = link to parameter in another instance of this function' $
    }
  
  ;; The last thing we do is pass on any other keywords to our
  ;; __set_tag "method."  Do this with _STRICT_EXTRA to make sure that
  ;; no bogus keywords are passed.  Be careful with debugging and make
  ;; sure user gets the best effort case when we are trying to ignore
  ;; the error.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /INFORMATIONAL, 'WARNING: the above error was produced.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting (not recommended).' 
        return, pfo_link_struct
     endif ;; CATCH
  endif ;; debugging
  pfo_link_struct__set_tag, pfo_link_struct, _STRICT_EXTRA=extra

  return, pfo_link_struct
end

;; Define the basic template for the structure.  This works with IDL's
;; implicit structure definition: ls = {pfo_link_struct}, but forces
;; initialization of the structure tags to IDL null values.
pro pfo_link_struct__define

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_link_struct $
    = {pfo_link_struct, $
       link_status  : 0B, $ ;; [master]/slave/hand_tied status.  It is not an error to have a slave point to a slave.  Master designation is optional
       linkID       : 0, $ ;; Set this in the master function, one ID per function, the same ID for all the parameters, even for the ones that don't participate
       to_ID    : 0, $ ;; for slave functions: indicates linkID of master
       to_ftype : 0., $ ;; for slave functions: fractional ftype of master parameter
       auto_link: 0B $ ;; used in pfo_link_struct__update to help automatically assign master/slave to desired parameter(s)
      } 
end

