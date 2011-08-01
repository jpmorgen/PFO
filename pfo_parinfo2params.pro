;+
; NAME: pfo_parinfo2params
;
; PURPOSE: Low-level routine that makes sure parameter array (params)
; is initilized by parinfo.value
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_parinfo2params, parinfo, params[, params_in]

; DESCRIPTION: This is a one-line solution that allows PFO primitives
; to work with the array 'params,' instead of parinfo.value.  Verbose
; error messages are given.  Control is returned to the calling
; routine, which is usually repsonsible for the problem.  Note that
; other than parinfo.value, no other checking is done to make sure
; that parinfo is a valid PFO parinfo.

; INPUTS:
;	parinfo: parinfo array (function definition)
;	
;
; OPTIONAL INPUTS:
;	params: if specified as an input parameter, checked to see if
;	has the same number of elements as parinfo.  An error is
;	raised if not

;	params_in: if specified and defined, this value is COPIED to
;	params first.  This helps calling routines keep params local
;	(e.g. accepting a params=params_in keyword)

;
; KEYWORD PARAMETERS:
;	npar: number of elements of parinfo
;
; OUTPUTS:
;	params: if params is undefined on input, params is set to parinfo.value
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
; $Id: pfo_parinfo2params.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_parinfo2params.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_parinfo2params, parinfo, params, params_in, npar=N_parinfo

  init = {tok_sysvar}

  ;; Return to the calling routine with any errors
  ON_ERROR, !tok.return

  ;; The optional 3rd argument helps the calling routine keep params local
  if N_elements(params_in) ne 0 then $
     params = params_in

  N_parinfo = N_elements(parinfo)
  N_params = N_elements(params)

  ;; Here is where we check to make sure we have a parinfo array
  if N_parinfo eq 0 then $
    message, 'ERROR: parinfo array must be defined'
  
  ;; If we have the same number of parinfo and params, our job is done
  if N_parinfo eq N_params then $
    return

  ;; Catch any errors with parinfo not being a struct/being the wrong
  ;; struct so that we can provide a more verbose error message
  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL
     message, /NONAME, !error_state.msg, /CONTINUE
     message, 'ERROR: parinfo does not appear to be a struct with a .value tag.  Did you create it with pfo_parinfo_new?'
  endif ;; catching parinfo not a proper struct

  ;; Handle the case of creating the params array
  if N_params eq 0 then begin
     params = parinfo.value
     return
  endif
  CATCH, /CANCEL

  ;; Check to see if we have an unequal number of params and parinfo
  if N_parinfo ne N_params then $
    message, 'ERROR: parinfo and params arrays are of unequal length.  If you are properly passing parinfo by reference, this should not happen.  In PFO routines, always pass the parinfo array by reference (e.g. parinfo=parinfo, NOT parinfo=parinfo[idx]).  Use the idx keyword to PFO routines to specify a subset of parinfo.'
end
