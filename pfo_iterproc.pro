; +
; $Id: pfo_iterproc.pro,v 1.1 2003/12/19 00:04:21 jpmorgen Exp $

; pfo_iterproc.pro 

;; Based on mpfit_defiter, pretty print a pfo function parameter
;; values

; -

pro pfo_iterproc, fcn, params, iter, fnorm, FUNCTARGS=fcnargs, $
                   quiet=quiet, iterstop=iterstop, parinfo=parinfo, $
                   format=fmt, pformat=pformat, dof=dof0, _EXTRA=iterargs

  ;; NOTE: Craig calls params, x.  I'll leave that in his code, but x
  ;; really means params, so I will call it that in my code.
  x=params

  ;; Craig is using a common block for this.  I would prefer to use a
  ;; system variable
  common mpfit_error, mperr
  mperr = 0
  if keyword_set(quiet) then return
  if n_params() EQ 3 then begin
      fvec = mpfit_call(fcn, x, _EXTRA=fcnargs)
      fnorm = mpfit_enorm(fvec)^2
  endif

  if n_elements(dof0) EQ 0 then dof = 1L else dof = floor(dof0(0))

  ;; Print the definitions of the functions used before the first iteration.
  if iter eq 1 then $
    print, pfo_funct([0], params, parinfo=parinfo, print=!pfo.ppname)
  
  print, iter, fnorm, dof, $
    format='("Iter ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
  print, pfo_funct([0], params, parinfo=parinfo, /print)


;  ;; Determine which parameters to print
;  nprint = n_elements(x)
;  iprint = lindgen(nprint)
;
;  if n_elements(dof0) EQ 0 then dof = 1L else dof = floor(dof0(0))
;  print, iter, fnorm, dof, $
;    format='("Iter ",I6,"   CHI-SQUARE = ",G15.8,"          DOF = ",I0)'
;  if n_elements(fmt) GT 0 then begin
;      print, x, format=fmt
;  endif else begin
;      if n_elements(parinfo) GT 0 then begin
;          parinfo_tags = tag_names(parinfo)
;          wh = where(parinfo_tags EQ 'PARNAME', ct)
;          if ct EQ 1 then begin
;              plen = max(strlen(parinfo.parname)) < 25
;              plen = strtrim(plen,2)
;              p = string(parinfo.parname, format='("    ",A'+plen+'," = ")')
;          endif
;          wh = where(parinfo_tags EQ 'MPPRINT', ct)
;          if ct EQ 1 then begin
;              iprint = where(parinfo.mpprint EQ 1, nprint)
;              if nprint EQ 0 then goto, DO_ITERSTOP
;          endif
;      endif
;      
;      if n_elements(p) EQ 0 then $
;        p = '    P('+strtrim(iprint,2)+') = '
;      if n_elements(pformat) EQ 0 then pformat = '(G20.6)'
;      p = p + string(x(iprint),format=string(pformat(0))) + '  '
;      print, p, format='(A)'
;  endelse

  DO_ITERSTOP:
  if keyword_set(iterstop) then begin
      k = get_kbrd(0)
      if k EQ string(byte(7)) then begin
          message, 'WARNING: minimization not complete', /info
          print, 'Do you want to terminate this procedure? (y/n)', $
            format='(A,$)'
          k = ''
          read, k
          if strupcase(strmid(k,0,1)) EQ 'Y' then begin
              message, 'WARNING: Procedure is terminating.', /info
              mperr = -1
          endif
      endif
  endif

  return
end
