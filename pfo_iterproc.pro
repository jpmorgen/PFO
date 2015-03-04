; +
; $Id: pfo_iterproc.pro,v 1.1 2003/12/19 00:04:21 jpmorgen Exp jpmorgen $

; pfo_iterproc.pro 

;; Based on mpfit_defiter, pretty print a pfo function parameter
;; values

; -

pro pfo_iterproc, fcn, params, iter, fnorm, FUNCTARGS=fcnargs, $
                  quiet=quiet, iterstop=iterstop, parinfo=parinfo, $
                  format=fmt, pformat=pformat, dof=dof0, $
                  Xorig=Xin, spec=spec, err_spec=err_spec, $
                  _EXTRA=iterargs

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
  if iter eq 1 then begin
     print, pfo_funct([0], params, parinfo=parinfo, print=!pfo.ppname)
  endif
  print, '-------------------------------------------------'
  print, iter, fnorm, dof, $
    format='("Iter ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
  print, '-------------------------------------------------'
  print, pfo_funct([0], params, parinfo=parinfo, /print)

  if !pfo.plotwin gt 0 and !pfo.plotproc ne '' $
    and N_elements(spec) ne 0 then begin
     ;; Change to the plotwin with wset.  If that raises an error,
     ;; make the window afresh
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, !error_state.msg, /CONTINUE
        window, !pfo.plotwin
     endif else begin
        wset, !pfo.plotwin
        call_procedure, !pfo.plotproc, Xin, parinfo, params=params, spec, $
                        err_spec, _EXTRA=iterargs
        CATCH, /CANCEL
     endelse

  endif

  if keyword_set(iterstop) then begin
     print, ' '
     message, /CONTINUE, 'PRESS RETURN TO PAUSE AND SHOW MENU NEXT TIME AROUND'

     answer = get_kbrd(0)
     for ki = 0,1000 do flush_input = get_kbrd(0)
     if byte(answer) eq 10 then begin
        print, ' '
        message, /CONTINUE, 'Fitting menu:'
        print, 'Stop fit, saving parameters at present values'
        print, 'Quit fitting, resetting parameters to previous values'
        print, 'do Nothing, keep fitting'

        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'S, Q, [N]'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'N'
           answer = strupcase(answer)
           for ki = 0,1000 do flush_input = get_kbrd(0)
        endrep until $
          answer eq 'S' or $
          answer eq 'Q' or $
          answer eq 'N'

        case answer of 
           'S'	:	mperr = !pfo.iterstop
           'Q'	:	mperr = !pfo.iterquit
           'N'	:	message, /CONTINUE, 'Continuing'
           else	:	message, 'ERROR: internal coding problem'
        endcase

     endif ;; fitting menu
  endif ;; iterstop set in iterargs

  return
end
