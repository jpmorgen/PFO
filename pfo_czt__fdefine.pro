;+
; NAME: pfo_CZT_fdefine
;
; PURPOSE: define, initialize and work with the pfo_CZT function
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: All functionality is best accessed from
; higher-level PFO routines, like pfo_parinfo_parse
;
; DESCRIPTION:

; This code is organized into "methods," such as __fdefine, __finit,
; __calc and __indices.  Note that some of the code is interdependent
; (e.g. __calc calls __indices, __fdefine is the first routine
; called), so the order these routines are listed in the file is
; important.  Brief documentation on each "method" is provided in the
; code.  Each parameter/keyword list are also annotated.

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
;   See pfo_finfo.
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
; $Id: pfo_czt__fdefine.pro,v 1.2 2011/11/21 15:25:29 jpmorgen Exp $
;
; $Log: pfo_czt__fdefine.pro,v $
; Revision 1.2  2011/11/21 15:25:29  jpmorgen
; Minor debugging
;
; Revision 1.1  2011/11/19 21:03:13  jpmorgen
; Initial revision
;
; Copied from pfo_scint__fdefine.pro
;
; Revision 1.4  2011/09/16 11:19:17  jpmorgen
; Added linking.  Also fiddled with status_mask
;
; Revision 1.3  2011/09/08 20:17:12  jpmorgen
; Added fname to pfo structure
;
; Revision 1.2  2011/09/01 22:10:03  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; The __indices "method" maps keyword names to the *indices* of those
;; parameters in the parinfo.  It is intended to be called on only ONE
;; instance of the function at a time from
;; pfo_parinfo_parse(/INDICES).
function pfo_CZT__indices, $
   parinfo, $    ;; parinfo containing the function
   idx=idx, $   ;; idx into parinfo over which to search for function
   status_mask=status_mask, $ ;; status mask (!pfo.active, !pfo.inactive, !pfo.delete, !pfo.all_status)
   E0=E0, $             ;; peak energy
   area=area, $         ;; area
   sigma=sigma, $	;; Gaussian sigma
   alpha=alpha, $	;; parameters for smoothed exponentials (scaler or 2 vector)
   mu=mu, $		;; parameters for smoothed exponentials (scaler or 2 vector)
   f_wings=f_wings, $	;; parameters for wings (scaler or 2 vector)
   f_step=f_step, $     ;; amplitude of the smothed step
   peak=peak, $         ;; generic for E0
   width=width, $       ;; generic for all width-determining parameters
   wings=wings, $       ;; generic for all wing-determining parameters
   terminate_idx=terminate_idx, $ ;; append !tok.nowhere to each index variable after we are done
   pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
   _REF_EXTRA=extra     ;; soak up any extra parameters

  ;; Do basic idx error checking and collect our function indices
  idx = pfo_fidx(parinfo, pfo_fname(), idx=idx, status_mask=status_mask, pfo_obj=pfo_obj, $
                 npar=npar, nfunct=nfunct)
  if nfunct ne 1 then $
    message, 'ERROR: ' + strtrim(nfunct, 2) + ' instances of function found, I can only work with one.  Use pfo_parinfo_parse and/or pfo_fidx to help process multiple instances of a function.'

  ;; Get the fractional part of ftype, which determines which
  ;; parameter we are.  Multiply by 100 and round, to prevent precision
  ;; errors
  ffrac = round(100*pfo_frac(parinfo[idx].pfo.ftype))

  ;; Translate fractional ftype into disambiguated keywords, raising
  ;; errors if we don't get what we expect.
  E0_local = where(ffrac eq 1, count)
  if count ne 1 then $
    message, 'ERROR: bad E0'
  ;; unwrap and make into a scaler
  E0_local = idx[E0_local[0]]

  area_local = where(ffrac eq 2, count)
  if count ne 1 then $
    message, 'ERROR: bad area'
  ;; unwrap and make into a scaler
  area_local = idx[area_local[0]]

  sigma_local = where(ffrac eq 3, count)
  if count ne 1 then $
    message, 'ERROR: bad sigma'
  ;; unwrap and make into a scaler
  sigma_local = idx[sigma_local[0]]

  ;; Alpha, mu and f are 2-vectors
  alpha_local = where(4 le ffrac and ffrac le 5, count)
  if count ne 2 then $
    message, 'ERROR: bad alpha'
  ;; unwrap
  alpha_local = idx[alpha_local]

  mu_local = where(6 le ffrac and ffrac le 7, count)
  if count ne 2 then $
    message, 'ERROR: bad mu'
  ;; unwrap
  mu_local = idx[mu_local]

  f_local = where(8 le ffrac and ffrac le 9, count)
  if count ne 2 then $
    message, 'ERROR: bad f'
  ;; unwrap
  f_local = idx[f_local]

  f_step_local = where(ffrac eq 10, count)
  if count ne 1 then $
    message, 'ERROR: bad f_step'
  ;; unwrap and make into a scaler
  f_step_local = idx[f_step_local[0]]

  ;; If we made it here, everything should be in order

  ;; Handle width and wings at this point, since there are multiple
  ;; parameters that represent them and we don't want to insert too
  ;; many terminate_idx markers.
  if N_elements(width) gt 0 or arg_present(width) then begin
     pfo_array_append, width, [sigma_local, alpha_local, mu_local]
     ;; Add the terminate_idx marker, if desired
     if keyword_set(terminate_idx) then $
        pfo_array_append, width, !tok.nowhere
  endif ;; width
  if N_elements(wings) gt 0 or arg_present(wings) then begin
     pfo_array_append, wings, f_local
     ;; Add the terminate_idx marker, if desired
     if keyword_set(terminate_idx) then $
        pfo_array_append, wings, !tok.nowhere
  endif ;; wings

  ;; Add our terminate_idx markers, if desired, to the disambiguated
  ;; copies of indices, so that we can easily copy everything to
  ;; "standard" keyword names, below.
  if keyword_set(terminate_idx) then begin
     pfo_array_append, E0_local, !tok.nowhere
     pfo_array_append, area_local, !tok.nowhere
     pfo_array_append, sigma_local, !tok.nowhere
     pfo_array_append, alpha_local, !tok.nowhere
     pfo_array_append, mu_local, !tok.nowhere
     pfo_array_append, f_local, !tok.nowhere
     pfo_array_append, f_step_local, !tok.nowhere
  endif ;; terminators

  ;; Append our indices onto the running arrays, if present
  if N_elements(E0) gt 0 or arg_present(E0) then $
    pfo_array_append, E0, E0_local
  if N_elements(area) gt 0 or arg_present(area) then $
    pfo_array_append, area, area_local
  if N_elements(sigma) gt 0 or arg_present(sigma) then $
    pfo_array_append, sigma, sigma_local
  if N_elements(alpha) gt 0 or arg_present(alpha) then $
    pfo_array_append, alpha, alpha_local
  if N_elements(mu) gt 0 or arg_present(mu) then $
    pfo_array_append, mu, mu_local
  if N_elements(f_wings) gt 0 or arg_present(f_wings) then $
    pfo_array_append, f_wings, f_local
  if N_elements(f_step) gt 0 or arg_present(f_step) then $
    pfo_array_append, f_step, f_step_local

  ;; Copy disambiguated keywords into others, if desired
  if N_elements(peak) gt 0 or arg_present(peak) then $
    pfo_array_append, peak, E0_local

  ;; Return indices sorted in order of ffrac
  retval = idx[sort(ffrac)] 

  ;; Add our terminate_idx marker, if desired
  if keyword_set(terminate_idx) then $
    pfo_array_append, retval, !tok.nowhere
  return, retval

end

;; The __calc "method" returns the calculated value of the
;; function.  It is intended to be called from pfo_parinfo_parse(/CALC)
function pfo_CZT__calc, $
  Xin, $ 	;; Input X axis
  params, $ 	;; parameters (entire array)
  parinfo=parinfo, $ ;; parinfo array (whole array)
  idx=idx, $    ;; idx into parinfo of parinfo segment defining this function
  pfo_obj=pfo_obj, $ ;; pfo_obj for pfo_finfo system, if not defined, PFO COMMON block is used
  _REF_EXTRA=extra ;; passed to underlying routines

  ;; Use shared routines to do basic error checking.  These error
  ;; messages are pretty verbose.

  ;; Common error checking and initialization code
  pfo_calc_check, Xin, params, parinfo=parinfo, idx=idx, pfo_obj=pfo_obj

  ;; If we made it here, our inputs should be in reasonably good shape
  ;; NOTE: We assume that we have been called from pfo_parinfo_parse so
  ;; that our idx are sorted in ftype order.  This saves as extra call
  ;; to our __index method
  E0	= params[idx[0]]
  area  = params[idx[1]]
  sigma	= params[idx[2]]
  alpha = params[idx[3:4]]
  mu	= params[idx[5:6]]
  f 	= params[idx[7:8]]
  f_step= params[idx[9]]

  return, gnd_czt(Xin, E0, area, sigma, alpha, mu, f, f_step, _EXTRA=extra)
  
end

;; Create the parinfo strand for this function and initialize it
function pfo_CZT__init, $
   E0=E0, $		;; peak energy
   area=area, $		;; area
   sigma=sigma, $	;; Gaussian sigma
   alpha=alpha_in, $	;; parameters for smoothed exponentials (scaler or 2 vector)
   mu=mu_in, $		;; parameters for smoothed exponentials (scaler or 2 vector)
   f_wings=f_in, $	;; parameters for wings (scaler or 2 vector)
   f_step=f_step, $	;; amplitude of the smothed step
   value=value, $   	;; catch value to make sure no conflict with other keywords
   ftype=ftype, $ 	;; catch ftype to make sure no conflict in pfo_struct_setget_tag
   pfo_obj=pfo_obj, $	;; pfo_obj for parinfo_template
   peak=peak, $		;; generic for E0
   width=width, $	;; generic for all width-determining parameters
   _REF_EXTRA=extra	;; _REF_EXTRA passed to pfo_struct_setget_tag

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_CZT__init [, pfo_obj=pfo_obj][, E0=E0][, area=area][sigma=sigma][, alpha=alpha][, mu=mu][, f=f][, f_step=f_step][, keywords to convert to tag assignments]'
     endif
  endif ;; not debugging

  ;; Catch improper use of value and ftype keywords
  if N_elements(value) + N_elements(ftype) ne 0 then $
     message, 'ERROR: value and ftype are set internaly: they are therefore invalid keywords'

  ;; Get fnum and fnpars given our fname.
  pfo_finfo, fname=pfo_fname(), fnum=fnum, fnpars=fnpars, pfo_obj=pfo_obj

  ;; Create our parinfo strand for this function, making sure to
  ;; include any substructure we need with required tags.  Start with
  ;; one parinfo element.  We need the pfo_link tag to link our alpha,
  ;; mu, and f parameters together so that the resolution function
  ;; works properly and f is symetric, by default.
  parinfo = pfo_parinfo_template(pfo_obj=pfo_obj, $
                                 required_tags=['pfo', 'pfo_link'])
  ;; Replicate our template parinfo it by fnpars
  parinfo = replicate(temporary(parinfo), fnpars)

  ;; Set attributes/defaults that are unique to this function.  Note
  ;; that some attributes are already defined in pfo_struct__init
  ;; (found in pfo_struct__define.pro)
  
  ;; FNAME 
  ;; It is inefficient to use 'where' on strings, so FTYPE is used
  ;; instead (see below).  fname should only be used by
  ;; pfo_struct__update to synchronize ftypes between parinfos created
  ;; at different times.
  parinfo.pfo.fname = pfo_fname()

  ;; PARNAME
  ;; Don't put function name in parname, since it can be
  ;; reconstructed from pfo.ftype.  Packages built on top of pfo
  ;; may want to set !pfo.longnames = 0 to use these short names
  parinfo.parname = ['E0', $
                     'A', $
                     'S', $
                     'Al', $
                     'Ah', $
                     'ml', $
                     'mh', $
                     'fl', $
                     'fh', $
                     'fs']
  if !pfo.longnames ne 0 then $
    parinfo.parname = ['Peak energy', $
                       'Area', $
                       'Sigma', $
                       'Alpha low', $
                       'Alpha hi', $
                       'mu low', $
                       'mu hi', $
                       'f low', $
                       'f hi', $
                       'f step']

  ;; FTYPE
  ;; Fractional part (ffrac).  A different decimal value can be
  ;; associated with each parameter of the function.  For complicated
  ;; associations, create a new tag with the pfo_struct system
  ;; (e.g. pfo_ROI_struct__define.pro)
  parinfo.pfo.ftype = [0.01, $ ;; Peak energy 
                       0.02, $ ;; Area        
                       0.03, $ ;; Sigma       
                       0.04, $ ;; Alpha low   
                       0.05, $ ;; Alpha hi    
                       0.06, $ ;; mu low      
                       0.07, $ ;; mu hi       
                       0.08, $ ;; f low       
                       0.09, $ ;; f hi        
                       0.10]   ;; f step      
  ;; FTYPE
  ;; Integer part (fnum).  Dynamically assigned by fdefine
  parinfo.pfo.ftype += fnum

  ;; VALUE
  ;; Defaults of 0 would be nice, but some 0s cause math problems.
  if N_elements(E0) + N_elements(peak) gt 1 then $
     message, 'ERROR: specify either E0 or peak'
  if N_elements(peak) ne 0 then $
     E0 = peak
  if N_elements(E0) eq 0 then $
     E0 = 0
  if N_elements(area) eq 0 then $
     area = 0
  ;; Sigma needs to be non-zero or else the function blows up
  if N_elements(sigma) + N_elements(width) gt 1 then $
     message, 'ERROR: specify either sigma or width'
  if N_elements(width) ne 0 then $
     sigma = width
  if N_elements(sigma) eq 0 then $
     sigma = 1d

  ;; Don't clobber our inputs when we vectorize
  if N_elements(alpha_in) ne 0 then $
     alpha = alpha_in
  ;; Alpha needs to be non-zero or else the function blows up
  if N_elements(alpha) eq 0 then $
     alpha = 1d
  if N_elements(alpha) eq 1 then $
     alpha = [alpha, alpha]
  if N_elements(mu_in) ne 0 then $
     mu = mu_in
  if N_elements(mu) eq 0 then $
     mu = 0
  if N_elements(mu) eq 1 then $
     mu = [mu, mu]
  if N_elements(f_in) ne 0 then $
     f = f_in
  if N_elements(f) eq 0 then $
     f = 0
  if N_elements(f) eq 1 then $
     f = [f, f]

  if N_elements(f_step) eq 0 then $
     f_step = 0

  parinfo[0].value = E0
  parinfo[1].value = area
  parinfo[2].value = sigma
  parinfo[3:4].value = alpha
  parinfo[5:6].value = mu
  parinfo[7:8].value = f
  parinfo[9].value = f_step

  ;; LIMITS, LIMITED
  ;; Keep all parameters positive
  parinfo.limits = [0,0]
  parinfo.limited = [1,0]
  ;; Once we get up to an f[_wings] = 1, the gaussian is totally
  ;; dominated by the wings, so there is not much point in going any
  ;; further.
  parinfo[7:8].limits = [0,1]
  parinfo[7:8].limited = [1,1]

  ;; LINKED
  ;; Mark alpha, mu, and f as auto "interlinked" parameters so that as
  ;; pfo_CZTs are added, they automatically connect to one master
  parinfo[3:8].pfo_link.auto_link = !pfo.interlink
  ;; Set "to_ftype" to point to each parameter's own ftype
  parinfo[3:8].pfo_link.to_ftype = pfo_frac(parinfo[3:8].pfo.ftype)
  ;; To make f symmetric, link f hi to f lo in "intralink" mode
  ;; (linked within one function).  NOTE: this overwrites f hi
  ;; interlinking
  parinfo[8].pfo_link.auto_link = !pfo.intralink
  parinfo[8].pfo_link.to_ftype = pfo_frac(parinfo[7].pfo.ftype)

  ;; Convert keywords on the command line to tag assignments in the
  ;; parinfo.  This is a little risky, since we might have duplicate
  ;; tags in the different sub-structures (e.g. status can be a
  ;; popular one) and, depending on what order the substructures were
  ;; added with required_tags, the behavior might vary.  It is always
  ;; safer to make assignments in your calling code explicitly with
  ;; the tags in the returned parinfo.  NOTE, we pass all of the
  ;; _EXTRA to pfo_struct_setget_tag, so the calling routine gets to
  ;; choose parallel or series, strict, etc.
  pfo_struct_setget_tag, /set, parinfo, _EXTRA=extra

  return, parinfo

end

;; Map function name to function number and number of parameters in
;; the PFO system
pro pfo_CZT__fdefine, pfo_obj=pfo_obj

  ;; Define system variables for all routines in this file
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_fdefine, pfo_obj=pfo_obj, fname='pfo_CZT', fnpars=10, $
               fdescr='pfo_CZT implements the function described by "Response Function Analysis Techniques for Coplanar Grid CdZnTe Detectors," Thomas H. Prettyman, Thomas Marks, Jr., David G. Pelowitz, and Morag K. Smith, in Proceedings of the Institute of Nuclear Materials Management 40th Annual Meeting, Phoenix, Arizona, July 25-29, 1999, Volume XXVIII (CD-ROM).  The function has a Gaussian at its core, which represents the fixed electronic noise of the detector.  Thus the first parameters are E0, area, and sigma.  Smoothed exponentials are added to puff out the core of the the function (potentially asymetric) using the parmeters alpha and mu.  f describes exponential wings (usually symetric).  Alpha, mu, and f can each be vectors of two elements each and, for each detector, depend on energy.  Thus, by default, the alpha, mu, and f parameters of multiple instances of the pfo_CZT function are linked to each other.   Finally, a smoothed step is implemented using the erf function and is basically as wide as sigma, and centered so that the 1/e slope point is E0.  Thus, pfo_CZT has 10 parameters, E0, area, W0, W1, alpha low,  alpha hi, mu low, mu high, f low, f high, and and f step.' 
    
end
