;+
; NAME: pfo_calc_check
;
; PURPOSE: perform common error checking and initialization for PFO
; function "__calc" methods
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_calc_check, Xin, params, dparams, fname=fname,
;  parinfo=parinfo, idx=idx, pfo_obj=pfo_obj
;
; DESCRIPTION: 
;   This routine checks to make sure that there is one and only one
;   instance of function fname in the strand of parinfo specified by
;   idx.  Also makes sure Xin, parinfo, params, and idx exist (see
;   documentation of each).
;
; INPUTS:
;   Xin: X-axis in "natural" units
;
; OPTIONAL INPUTS:
;   params: if specified, will be checked to make sure that this array
;   is the same length as array passed in parinfo keyword.  If
;   doesn't exist, will be set to parinfo.value
;
;   dparams: currently an error is raised if dparams is specified
;   because analytic derivatives are not supported by the PFO system
;
; KEYWORD PARAMETERS:
;
;   fname (required): function name (or number) of PFO function to be
;	checked
;   parinfo (required): parinfo array in which the function fname is
;	contained
;   idx (optional): indices into parinfo array that isolate the
;	parameters for fname.  If not specified, an idx is generated
;	that points to the entire parinfo array
;   pfo_obj (optional): pfo_obj that stores associations between fnums
;	and fnames
;
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
; $Id: pfo_calc_check.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_calc_check.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_calc_check, Xin, params, dparams, fname=fname, parinfo=parinfo, idx=idx, pfo_obj=pfo_obj

  init = {tok_sysvar}

  ;; Return to the calling routine with any error
  ON_ERROR, !tok.return

  ;; Check for Xin
  if N_elements(Xin) eq 0 then $
    message, 'ERROR: Xin must be defined'

  if N_params() gt 2 then $
    message, 'ERROR: more than two positional inputs implies the use of analytic derivatives, which are not implemented anywhere in the PFO system yet'

  ;; Make sure parinfo and params are defined.
  pfo_parinfo2params, parinfo, params

  ;; Make sure idx is defined
  pfo_idx, parinfo, idx=idx

  ;; Do a quick check to make sure we have just one instance of our
  ;; function
  if N_elements(fname) ne 1 then $
     message, 'ERROR: required keyword parameter fname incorrectly specified: specify one and only one fname'

  fnum = pfo_fnum(fname, fnpars=fnpars, pfo_obj=pfo_obj)

  fnums = floor(parinfo[idx].pfo.ftype)
  u_fnums = uniq(fnums, sort(fnums))
  if N_elements(u_fnums) ne 1 then $
     message, 'ERROR: found ' + strtrim(N_elements(u_fnums), 2) + ' different functions in parinfo, expecting one and only one instance of ' +  pfo_fname(fname, pfo_obj=pfo_obj)

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
