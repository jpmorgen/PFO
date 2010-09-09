;+
; NAME: pfo_ROI
;
; PURPOSE: implement Region Of Interest (ROI) functionality in PFO.

;       This enables selective calculation of PFO functions over
;       specific subsets of Xin.  pfo_ROI functions can be used with
;       and without the pfo_multi_ROI add-in (see
;       pfo_multi_roi_struct__define.pro).  Without the add-in, the
;       function is calculated on all ROIs.  With the add-in, it is
;       possible to specify which function is calculated on which
;       ROI.  When using the add-in

;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;	inaxis: the ROI boundaries can be calculated in terms of !pfo.Xin
;	or !pfo.Xaxis
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
; $Id: pfo_roi.pro,v 1.1 2010/09/09 19:20:41 jpmorgen Exp $
;-
function pfo_ROI, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                  create=create, print=print, indices=indices, $
                  ROI=ROI, inaxis=inaxis, count=count, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.ROI

  ;; This section is specific to each PFO function definition
  if keyword_set(create) then begin
     ;; /CREATE

     ;; Use pfo_null to get all the error checking on whatever
     ;; template parinfo structure we have.  Make sure to set
     ;; !pfo.fnpars for each new function in pfo_sysvar__define: it is
     ;; used in pfo_funct.

     ;; For pfo_ROI, store the information about which form of the X
     ;; axis (Xin or Xaxis) the ROI boundary operates in to the inaxis
     ;; tag.  Outaxis could be used, but it is an error to have Xin
     ;; there, so just define no outaxis.  This might save some pain
     ;; when we call pfo_funct_check.  
     new_parinfo = $
       replicate(pfo_null(/create, parinfo=parinfo, idx=idx, inaxis=inaxis, $
                          outaxis=!pfo.none, _EXTRA=extra), !pfo.fnpars[fn])

     ;; PARNAME
     ;; Don't put function name in parname, since it can be
     ;; reconstructed from pfo.ftype.  Packages built on top of pfo
     ;; will want to set !pfo.longnames = 0 to use these short names     
     new_parinfo[0].parname = 'L'
     new_parinfo[1].parname = 'R'

     if !pfo.longnames ne 0 then begin
        new_parinfo[0].parname = 'Left'
        new_parinfo[1].parname = 'Right'
     endif

     ;; VALUE
     if N_elements(ROI) ne 0 then begin
        if N_elements(ROI) ne 2 then $
          message, 'ERROR: ROI must have 2 elements'
        new_parinfo.value = ROI
     endif ;; value

     ;; ACTIVE
     ;; pfo_fcreate sets parinfo.pfo.active and other pfo fields, like
     ;; axes and fseq.  Put only function specific things here.

     ;; FTYPE
     new_parinfo[0].pfo.ftype = 0.1 ;; left
     new_parinfo[1].pfo.ftype = 0.2 ;; right     

     ;; FIXED
     ;; In general, we want our ROI boundaries to be fixed, otherwise
     ;; weird things might happen, like having them collapse (less data
     ;; = smaller chi^2)
     new_parinfo.fixed = 1

     return, new_parinfo

  endif ;; /CREATE

  ;; COMMON ERROR CHECKING CODE
  f_idx = pfo_funct_check(fn, Xin=Xin, params=params, parinfo=parinfo, $
                          idx=idx, npar=npar)
  if npar eq 0 then return, pfo_null(Xin, print=print, _EXTRA=extra)


  ;; FUNCTION SPECIFIC ERROR CHECKING CODE
  ;; --> fix this eventually
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives not implemented yet'

  ;; FUNCTION SPECIFIC FTYPE HANDLING
  ;; pfo_ROI sub ftypes are in numeric order, so put them back if they
  ;; have been scrambled.
  sidx = sort(parinfo[f_idx].pfo.ftype)
  ;; Make an easy handle for the parameters
  pidx = f_idx[sidx]


  ;; PRINT or WIDGET.  pfo_null can handle these once the parameters
  ;; are in order
  if keyword_set(print) or keyword_set(widget) then $
    return, pfo_null([0], params, parinfo=parinfo, idx=pidx, print=print, $
                    widget=widget, _EXTRA=extra)

  ;; CALCULATE

  inaxis = parinfo[pidx].pfo.inaxis

  ;; Check to see if we need to bother calculating an Xaxis
  junk = where(inaxis eq !pfo.Xaxis, nXaxis)
  if nXaxis gt 0 then begin
     ;; Use pfo_Xaxis to calculate the Xaxis.  This is done efficiently
     ;; if Xaxis is Xin
     Xaxis = pfo_Xaxis(Xin, params, parinfo=parinfo, _EXTRA=extra)
  endif

  ;; Make our ROI return keyword.  This will read in whatever units
  ;; the pfo.inaxis tags specify
  ROI = param[pidx]
  
  ;; Get ready to piece together a call to "where" which will return
  ;; the indices to our section of the Xin/Xaxis.  Xin and Xaxis are
  ;; indexed the same, so this works
  case inaxis[0] of
     !pfo.Xin:   left = 'ROI[0] le Xin'
     !pfo.Xaxis: left = 'ROI[0] le Xaxis'
     else: message, 'ERROR: parinfo.pfo.inaxis must be !pfo.Xin or !pfo.Xaxis'
  endcase

  case inaxis[1] of
     !pfo.Xin:   right = 'Xin le ROI[1]'
     !pfo.Xaxis: right = 'Xaxis le ROI[1]'
     else: message, 'ERROR: parinfo.pfo.inaxis must be !pfo.Xin or !pfo.Xaxis'
  endcase

  ;; Call "where," piecing together our boundaries, defined above.
  ;; While we are at it, define our default return ROI keyowrd value.
  ;; count is also a keyword return value
  good_idx = call_function('where', left + ' and ' + right, 'count')
  if count eq 0 then $
    return, -1
  
  ;; If we made it here, we have a section of our Xin/Xaxis that falls
  ;; within this ROI.  Return the indices into this section.
  return, good_idx
  
end
