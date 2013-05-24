;+
; NAME: pfo_voigt_fdefine
;
; PURPOSE: define, initialize and work with the pfo_voigt function
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
; $Id: pfo_voigt__fdefine.pro,v 1.2 2013/05/24 22:38:35 jpmorgen Exp $
;
; $Log: pfo_voigt__fdefine.pro,v $
; Revision 1.2  2013/05/24 22:38:35  jpmorgen
; Give to Ron
;
; Revision 1.1  2012/09/14 14:26:38  jpmorgen
; Initial revision
;
; Copied from pfo_CZT__fdefine.pro
;
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
function pfo_voigt__indices, $
   parinfo, $    ;; parinfo containing the function
   idx=idx, $   ;; idx into parinfo over which to search for function
   status_mask=status_mask, $ ;; status mask (!pfo.active, !pfo.inactive, !pfo.delete, !pfo.all_status)
   center=center, $	;; line center value on X-axis
   area=area, $		;; area -- negative for absorption lines
   Gauss_FWHM=Gauss_FWHM, $ ;; Gaussian FWHM
   Lor_FWHM=Lor_FWHM, $	;; Lorentzian FWHM
   peak=peak, $         ;; generic for center
   width=width, $       ;; generic for all width-determining parameters
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
  ffrac = pfo_frac(parinfo[idx].pfo.ftype, mult=10, /round)

  ;; Translate fractional ftype into disambiguated keywords, raising
  ;; errors if we don't get what we expect.
  center_local = where(ffrac eq 1, count)
  if count ne 1 then $
    message, 'ERROR: bad center'
  ;; unwrap and make into a scaler
  center_local = idx[center_local[0]]

  area_local = where(ffrac eq 2, count)
  if count ne 1 then $
    message, 'ERROR: bad area'
  ;; unwrap and make into a scaler
  area_local = idx[area_local[0]]

  Gauss_FWHM_local = where(ffrac eq 3, count)
  if count ne 1 then $
     message, 'ERROR: bad Gauss_FWHM'
  ;; unwrap and make into a scaler
  sigma_local = idx[Gauss_FWHM_local[0]]

  Lor_FWHM_local = where(ffrac eq 4, count)
  if count ne 1 then $
     message, 'ERROR: bad Lor_FWHM'
  ;; unwrap and make into a scaler
  sigma_local = idx[Lor_FWHM_local[0]]

  ;; If we made it here, everything should be in order

  ;; Handle width at this point.  There are two width parameters and
  ;; we don't want to insert too many terminate_idx markers.
  if N_elements(width) gt 0 or arg_present(width) then begin
     pfo_array_append, width, [Gauss_FHWM_local, Lor_FHWM_local]
     ;; Add the terminate_idx marker, if desired
     if keyword_set(terminate_idx) then $
        pfo_array_append, width, !tok.nowhere
  endif ;; width

  ;; Add our terminate_idx markers, if desired, to the disambiguated
  ;; copies of indices, so that we can easily copy everything to
  ;; "standard" keyword names, below.
  if keyword_set(terminate_idx) then begin
     pfo_array_append, center_local, !tok.nowhere
     pfo_array_append, area_local, !tok.nowhere
     pfo_array_append, Gauss_FWHM_local, !tok.nowhere
     pfo_array_append, Lor_FWHM_local, !tok.nowhere
  endif ;; terminators

  ;; Append our indices onto the running arrays, if present
  if N_elements(center) gt 0 or arg_present(center) then $
    pfo_array_append, center, center_local
  if N_elements(area) gt 0 or arg_present(area) then $
    pfo_array_append, area, area_local
  if N_elements(Gauss_FWHM) gt 0 or arg_present(Gauss_FWHM) then $
    pfo_array_append, Gauss_FWHM, Gauss_FWHM_local
  if N_elements(Lor_FWHM) gt 0 or arg_present(Lor_FWHM) then $
    pfo_array_append, Lor_FWHM, Lor_FWHM_local

  ;; Copy disambiguated keywords into others, if desired
  if N_elements(peak) gt 0 or arg_present(peak) then $
    pfo_array_append, peak, center_local

  ;; Return indices sorted in order of ffrac
  retval = idx[sort(ffrac)] 

  ;; Add our terminate_idx marker, if desired
  if keyword_set(terminate_idx) then $
    pfo_array_append, retval, !tok.nowhere
  return, retval

end

;; The __calc "method" returns the calculated value of the
;; function.  It is intended to be called from pfo_parinfo_parse(/CALC)
function pfo_voigt__calc, $
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
  center	= params[idx[0]]
  area  	= params[idx[1]]
  Gauss_FWHM	= params[idx[2]]
  Lor_FWHM 	= params[idx[3]]

  rln2 = sqrt(alog(2d))         ; Make sure result is double precision
  x = 2d*rln2 * (Xin - center)/Gauss_FWHM
  y = rln2 * Lor_FWHM/Gauss_FWHM
  ;; Use IDL's internal Voigt profile for now even though Carey
  ;; complains of a problem with it.  --> look into this
  return, 2d*rln2/sqrt(!pi) * area/Gauss_FWHM * voigt(y,x) 
  
end

;; Create the parinfo strand for this function and initialize it
function pfo_voigt__init, $
   center=center, $	;; line center value on X-axis
   area=area, $		;; area -- negative for absorption lines
   Gauss_FWHM=Gauss_FWHM, $ ;; Gaussian FWHM
   Lor_FWHM=Lor_FWHM, $	;; Lorentzian FWHM
   value=value, $   	;; catch value to make sure no conflict with other keywords
   ftype=ftype, $ 	;; catch ftype to make sure no conflict in pfo_struct_setget_tag
   pfo_obj=pfo_obj, $	;; pfo_obj for parinfo_template
   peak=peak, $         ;; generic for center
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
        message, 'USAGE: pfo_voigt__init [, pfo_obj=pfo_obj][, peak=peak][, area=area] [ [Gauss_FWHM=Gauss_FWHM][, Lor_FWHM=Lor_FWHM] | [width=width] ][, keywords to convert to tag assignments]'
     endif
  endif ;; not debugging

  ;; Catch improper use of value and ftype keywords
  if N_elements(value) + N_elements(ftype) ne 0 then $
     message, 'ERROR: value and ftype are set internaly: they are therefore invalid keywords'

  ;; Get fnum and fnpars given our fname.
  pfo_finfo, fname=pfo_fname(), fnum=fnum, fnpars=fnpars, pfo_obj=pfo_obj

  ;; Create our parinfo strand for this function, making sure to
  ;; include any substructure we need with required tags.  Start with
  ;; one parinfo element.
  parinfo = pfo_parinfo_template(pfo_obj=pfo_obj, $
                                 required_tags=['pfo'])
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
  parinfo.parname = ['C', $
                     'A', $
                     'D', $
                     'L' $
                    ]
  if !pfo.longnames ne 0 then $
    parinfo.parname = ['Center', $
                       'Area', $
                       'Doppler FWHM', $
                       'Lorentzian FWHM' $
                      ]

  ;; FTYPE
  ;; Fractional part (ffrac).  A different decimal value can be
  ;; associated with each parameter of the function.  For complicated
  ;; associations, create a new tag with the pfo_struct system
  ;; (e.g. pfo_ROI_struct__define.pro)
  parinfo.pfo.ftype = [0.1, $ ;; line center value on X-axis
                       0.2, $ ;; Area        
                       0.3, $ ;; Doppler FWHM       
                       0.4 $  ;; Lorentzian FWHM
                      ]
  ;; FTYPE
  ;; Integer part (fnum).  Dynamically assigned by fdefine
  parinfo.pfo.ftype += fnum

  ;; VALUE
  ;; Defaults of 0 would be nice, but some 0s cause math problems.
  if N_elements(center) + N_elements(peak) gt 1 then $
     message, 'ERROR: specify either center or peak'
  if N_elements(peak) ne 0 then $
     center = peak
  if N_elements(center) eq 0 then $
     center = 0
  if N_elements(area) eq 0 then $
     area = 0
  ;; Check to see if there is a width array
  if N_elements(width) ne 0 then begin
     ;; Check to see if we are in conflict with Gauss_FWHM or Lor_FWHM
     if N_elements(Gauss_FWHM) + N_elements(Lor_FWHM) gt 0 then $
        message, 'ERROR: specify Gauss_FWHM and Lor_FWHM OR width'
     if N_elements(width) gt 2 then $
        message, 'ERROR: specify no more than 2 widths (Gaussian FHWM and Lorentzian FWHM)'
     if N_elements(wifth) eq 2 then begin
        Gauss_FWHM = width[0]
        Lor_FWHM = width[1]
     endif
     if N_elements(width) eq 1 then begin
        message, /INFORMATIONAL, 'NOTE: only one width parameter specified.  Asusming it is Gauss_FWHM.  Setting Lor_FWHM to 0 and fixing it there.'
        Gauss_FWHM = width
        Lor_FWHM = 0d
        parinfo[3].fixed = 1
     endif     
  endif ;; width
  ;; Make sure Gaussian width is non-zero or else function returns NAN
  if N_elements(Gauss_FWHM) eq 0 then $
     Gauss_FWHM = 1d
  if N_elements(Lor_FWHM) eq 0 then $
     Lor_FWHM = 0d

  parinfo[0].value = center
  parinfo[1].value = area
  parinfo[2].value = Gauss_FWHM
  parinfo[3].value = Lor_FWHM

  ;; LIMITS, LIMITED
  ;; Keep widths positive
  parinfo[2:3].limits = [0, 0]
  parinfo[2:3].limited = [1,0]
  ;; Keep Gaussian width non-zero
  parinfo[2].limits = [1d-10, 0]


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
pro pfo_voigt__fdefine, pfo_obj=pfo_obj

  ;; Define system variables for all routines in this file
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_fdefine, pfo_obj=pfo_obj, fname='pfo_voigt', fnpars=4, $
               fdescr='pfo_voigt implements the Voigt function.  The Voigt is the convolution of a Gaussian and a Lorentzian and is frequently used in spectral line fitting, both in emission and absorption.  The function accepts the following four parameters: peak X-axis value, area, Doppler FWHM, Lorentzian FWHM.  The function is calculated using a call to IDL''s internal Voigt routine' 
    
end
