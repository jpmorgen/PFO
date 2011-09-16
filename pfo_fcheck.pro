;+
; NAME: pfo_fcheck
;
; PURPOSE: perform common error checking and initialization for PFO functions
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_fcheck, parinfo, fname, params=params, idx=idx, pfo_obj=pfo_obj

; DESCRIPTION: Makes sure that parinfo exists.  Copies over
; parinfo.value if params are not specified, makes sure idx exists
; (set to index entire parinfo, if not), and, most importantly, checks
; parinfo[idx] to make sure it is consistent with fname (one and only
; one function defined).  See code for details and other primitives used.

; INPUTS: 

;   parinfo: parinfo array in which function fname is expected to be
;   found

;   fname (scaler string): function name to check

;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   params (optional output): array of values, usually initilized to
;   parinfo.value.  If exists on input, checked for consistent length
;   with parinfo

;   idx (optional): indices into parinfo.  If undefined on input, will
;   be set to index the entire parinfo array

;   pfo_obj: the object encapsulating the f_struct_array and parinfo,
;   which has been extracted via reference.  the f_struct_array is
;   necessary for converting fname to fnum.

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
; $Id: pfo_fcheck.pro,v 1.2 2011/09/16 13:46:45 jpmorgen Exp $
;
; $Log: pfo_fcheck.pro,v $
; Revision 1.2  2011/09/16 13:46:45  jpmorgen
; Improved documentation
;
; Revision 1.1  2011/09/01 22:20:58  jpmorgen
; Initial revision
;
;-
pro pfo_fcheck, parinfo, fname, params=params, idx=idx, pfo_obj=pfo_obj
  init = {tok_sysvar}

  ;; Return to the calling routine with any error
  ON_ERROR, !tok.return

  ;; Make sure parinfo is defined
  if N_elements(parinfo) eq 0 then $
     message, 'ERROR: parinfo must be defined'

  ;; Check to see if user wants to deal with params
  if arg_present(params) or N_elements(params) eq 0 then begin
     ;; Make sure parinfo and params are defined.
     pfo_parinfo2params, parinfo, params
  endif ;; deal with params

  ;; Make sure idx is defined
  pfo_idx, parinfo, idx

  ;; Do a quick check to make sure we have just one instance of our
  ;; function
  if N_elements(fname) gt 1 then $
     message, 'ERROR: cannot specify more than one fname'

  ;; Grab our fname from the caller, if we are being lazy
  if N_elements(fname) eq 0 then $
     fname = pfo_fname(callstack=1)

  ;; Convert to fnum 
  fnum = pfo_fnum(fname, fnpars=fnpars, pfo_obj=pfo_obj)

  ;; See if our idx just point to one function
  fnums = floor(parinfo[idx].pfo.ftype)
  u_fnums = uniq(fnums, sort(fnums))
  if N_elements(u_fnums) ne 1 then $
     message, 'ERROR: found ' + strtrim(N_elements(u_fnums), 2) + ' different functions in parinfo[idx], expecting one and only one instance of ' +  pfo_fname(fname, pfo_obj=pfo_obj)

  ;; If we don't know how many parameters we have, this is all
  ;; the checking we can do
  if fnpars eq 0 then $
     return

  ;; Check functions with definite numbers of parameters (e.g. Voigt)
  npar = N_elements(idx) 
  if npar mod fnpars ne 0 then $
    message, 'ERROR: incorrect number of parameters for function type ' + pfo_fname(fnum) + ': ' + strtrim(npar, 2)
  

  if npar / fnpars ne 1 then $
     message, 'ERROR: ' + strtrim(npar / fnpars, 2) + ' instances of function type ' + pfo_fname(fnum) + ' found.  Expecting one and only one.'

  
end
