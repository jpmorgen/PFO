;+
; NAME: pfo_convert_coord
;
; PURPOSE: convert from one of the three PFO axis coordinates to
; another for a given parinfo.
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: 
;	result = pfo_convert_coord(values, parinfo, [idx=idx,]
; 	[initial_guess=initial_guess,] /from_Xin or /from_Xaxis or
; 	/from_Yaxis, /to_Xin, /to_Xaxis, /to_Yaxis, Xaxis=Xaxis, _EXTRA=extra)
;
; DESCRIPTION: Like IDL's native convert_coord for plotting,
; pfo_convert_coord allows points on one axis to be looked up on
; another.  Uses IDL's fx_root function to do the work. -->
; currently using NEWTON
;
; INPUTS: value -- value to convert scaler or vector is OK.  Use
;                  /from_* to specify which axis value belongs to
;	parinfo -- parinfo array that defines the function
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 
;	Specify one "from" and one "to:"
;	from_Xin -- trivial option, input value becomes Xin
;	from_Xaxis -- input value becomes Xaxis value
;	from_Yaxis -- input value becomes Yaxis value
;	to_Xin -- output value corresponds to Xin
;	to_Xaxis -- output value corresponds to Xaxis
;	to_Yaxis -- output value corresponds to Yaxis
;
;	initial_guess -- the Xin value of the initial guess for
;                        IDL's fx_root.  Defaults to vector format of
;                        [0, -1, 1] (start, low, high)
;	idx -- optional indices into parinfo to limit function definition
;	xaxis --  if you are converting to_Yaxis, you can pick up the
;                Xaxis for free in this output keyword
;	_EXTRA -- keywords to be passed to fx_root
;
; OUTPUTS: function returns the value on the output axis corresponding
; to the position of input value on the /from_* axis.  If your function is
; multi-valued, you will have to suppply a decent initial_guess to
; make sure you get the expected result
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.  In this case, we have to use the common block
;   pfo_convert_coord since there is no other way to pass parinfo, the
;   function definition, into the pfo_covert_coord_funct
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
; $Id: pfo_convert_coord.pro,v 1.5 2011/01/20 22:58:24 jpmorgen Exp $
; $Log: pfo_convert_coord.pro,v $
; Revision 1.5  2011/01/20 22:58:24  jpmorgen
; Trying NEWTON instead of FX_ROOT.
;
; Revision 1.4  2011/01/19 19:55:20  jpmorgen
; Changing to Newton
;
; Revision 1.3  2011/01/17 18:57:00  jpmorgen
; About to delete some commented out code that worked but doesn't seem
; to be necessary
;
; Revision 1.2  2011/01/03 21:51:19  jpmorgen
; Improved initial guess code
;
; Revision 1.1  2010/12/10 21:57:53  jpmorgen
; Initial revision
;
;-

;; The basic idea is to always find the Xin value corresponding to the
;; Xin, Xaxis or Yaxis value stored in our COMMON block.  The COMMON
;; block variable from_axis tells us which axis "value" comes from.
;; We are using fx_root, which adjusts Xin in order to find the zero
;; of pfo_convert_coord_funct, so we just return value minus the
;; appropriate function value
function pfo_convert_coord_funct, Xin
  COMMON pfo_convert_coord, value, from_axis, pparinfo, idx
  on_error, 0
  case from_axis of 
     !pfo.Xin : return, value - Xin
     !pfo.Xaxis : return, value - pfo_Xaxis(Xin, parinfo=*pparinfo, idx=idx)
     !pfo.Yaxis : return, value - pfo_funct(Xin, parinfo=*pparinfo, idx=idx)
  endcase
end

function pfo_convert_coord, value_in, parinfo_in, idx=idx_in, $
  initial_guess=initial_guess_in, $
  from_Xin=from_Xin, from_Xaxis=from_Xaxis, from_Yaxis=from_Yaxis, $
  to_Xin=to_Xin, to_Xaxis=to_Xaxis, to_Yaxis=to_Yaxis, Xaxis=Xaxis, $
  _EXTRA=extra

  COMMON pfo_convert_coord, value, from_axis, pparinfo, idx

  init = {pfo_sysvar}
  init = {tok_sysvar}


  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     ;; Note heap leakage if you have !pfo.debug gt 0
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Put our parinfo back
        parinfo_in = temporary(*pparinfo)
        ptr_free, pparinfo
        message, !error_state.msg, /CONTINUE
        message, 'USAGE: result=pfo_convert_coord(values, parinfo, [idx=idx,] [initial_guess=initial_guess,] /from_Xin or /from_Xaxis or /from_Yaxis, /to_Xin, /to_Xaxis, /to_Yaxis, Xaxis=Xaxis, _EXTRA=extra)'
     endif
  endif ;; not debugging

  ;; Common block variables cannot be parameters.  We need to
  ;; explicitly copy our values into the common block.  This means
  ;; that we have bad memory practices for large arrays (e.g. the
  ;; parinfo).  Use a heap variable to get around this.  It would be a
  ;; lot easier if fx_root passed _EXTRA along to its primitive.
  pparinfo = ptr_new(parinfo_in, /no_copy)

  if N_params() ne 2 then $
    message, 'ERROR: supply two positional parameters: the value and the parinfo'
  ;; Beware the persistence of COMMON block variables.  Fortunately
  ;; not in type.
  if keyword_set(idx_in) then $
    idx = idx_in $
  else $
    idx = indgen(N_elements(*pparinfo), type=size(N_elements(*pparinfo), /type))
  if keyword_set(from_Xin) then $
    from_axis = !pfo.Xin 
  if keyword_set(from_Xaxis) then $
    from_axis = !pfo.Xaxis 
  if keyword_set(from_Yaxis) then $
    from_axis = !pfo.Yaxis
  if NOT keyword_set(from_axis) then $
    message, 'ERROR: one of /from_Xin, /from_Xaxis, /from_Yaxis must be specified'

  if keyword_set(to_Xin) then $
    to_axis = !pfo.Xin 
  if keyword_set(to_Xaxis) then $
    to_axis = !pfo.Xaxis 
  if keyword_set(to_Yaxis) then $
    to_axis = !pfo.Yaxis
  if NOT keyword_set(to_axis) then $
    message, 'ERROR: one of /to_Xin, /to_Xaxis, /to_Yaxis must be specified'


  ;; Do a basic check to see if we have a valid parinfo.  If not,
  ;; quietly return value_in
  use_idx = pfo_funct_check(!pfo.null, parinfo=*pparinfo, idx=idx, npar=npar)
  if npar le 0 then begin
     ;; Put pointer back
     parinfo_in = temporary(*pparinfo)
     ptr_free, pparinfo
     return, value_in
  endif

  ;; Check other command line parameters
  if N_elements(max_initial_guess_search) eq 0 then $
    max_initial_guess_search = 50
  if N_elements(initial_guess_range) eq 0 then $
    initial_guess_range = 1
  if N_elements(initial_guess_increment) eq 0 then $
    initial_guess_increment = initial_guess_range* 10

  npts = N_elements(value_in)
  if npts eq 0 then $
    message, 'ERROR: no value(s) specified'
  CATCH, /CANCEL
  ;; End of command line checking
  
  ;; Check for the trivial case where we are calculating from Xin.  In
  ;; this case, we don't need fx_root
  if from_axis eq !pfo.Xin then begin
     Xin = value_in
  endif else begin
     ;; We will use IDL's fx_root to find Xin for each point
     Xin = make_array(npts, value=!values.d_nan)

     ;; Do the dance that preserves the initial_guess input variable and
     ;; replicates it to an array of the same length of the values, if
     ;; necessary.

     ;; Build in the capability to search around a little bit for the
     ;; initial guess.
     initial_guess_search = 0   ; counter for number of times we have tried
     if N_elements(initial_guess_in) eq 0 then begin
        ;; Try to make a good initial guess.  This might take several
        ;; iterations.  Start from 0, since we have no other idea
        initial_guess_in = 0d
        initial_guess_direction = 1 ; direction we are searching in
        CATCH, err
        if err ne 0 then begin
           ;; Presumably our error is in fx_root.  
           ;; normally we CANCEL the CATCH right away, but in this case,
           ;; we actually want to loop through max_initial_guess_search times

           ;; Check to see if we have searched too many times in both
           ;; directions
           if initial_guess_search ge max_initial_guess_search and $
             initial_guess_direction lt 0 then begin
              CATCH, /CANCEL
              ;; Put our command line parameter back and throw the error
              ;; that we could not find a solution
              parinfo_in = temporary(*pparinfo)
              ptr_free, pparinfo

              message, !error_state.msg, /CONTINUE
              message, 'ERROR: more than ' + strtrim(max_initial_guess_search, 2) + ' attempted and fx_root still seems to be unable to find a solution.  Please supply an initial guess.'
           endif ;; error in fx_root

           ;; Check to see if we have searched too many times in the
           ;; positive direction
           if initial_guess_search ge max_initial_guess_search and $
             initial_guess_direction gt 0 then begin
              ;; reset our counter
              initial_guess_search = 0
              ;; Start off our guess going in the negative direction
              initial_guess_in = 0d
              ;; Make sure we continue in the negative direction
              initial_guess_direction = -1
           endif

           ;; Move our initial guess over
           initial_guess_in = initial_guess_in + initial_guess_direction*initial_guess_increment

        endif ;; Caught an error, presumably from fx_root
        ;; Increment our counter
        initial_guess_search += 1
     endif ;; no initial guess supplied

     initial_guess = initial_guess_in
     ;; Check to see if the user is providing initial guess, lower and
     ;; upper bounds
     dims = size(initial_guess, /dimensions)
     if dims[0] eq 0 then $
       initial_guess = [initial_guess, initial_guess - initial_guess_range, $
                        initial_guess + initial_guess_range]
     dims = size(initial_guess, /dimensions)
     if dims[0] ne 3 or N_elements(dims) gt 2 then $
       message, 'ERROR: initial_guess must be in vector format [guess, low, high].  It is OK to have one such 3-element vector for each input value.'

     nig = 1
     if N_elements(dims) eq 2 then $
       nig = dims[1]
     if nig ne npts and nig ne 1 then $
       message, 'ERROR: initial_guess has ' + strtrim(nig, 2) + ' elements but there are ' + strtrim(npts, 2) + ' values'
     
;;  ;; Convert our initial guess to the proper axis
;;  case to_axis of
;;     !pfo.Xin : 
;;     !pfo.Xaxis : initial_guess = $
;;       pfo_Xaxis(temporary(initial_guess), parinfo=*pparinfo, idx=idx)
;;     !pfo.Yaxis : initial_guess = $
;;       pfo_funct(temporary(initial_guess), parinfo=*pparinfo, idx=idx)
;;     endcase

     if NOT keyword_set(initial_guess_search) then begin
        ;; Catch errors the first time they happen
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           ;; Put our command line parameter back and throw the error
           parinfo_in = temporary(*pparinfo)
           ptr_free, pparinfo
           message, !error_state.msg
        endif ;; error, presumably in fx_root
     endif ;; not searching for an initial guess

     for i=0,npts-1 do begin
        value = value_in[i]
        ;; Handle the case where we just want to propagate one initial
        ;; guess for all of the values
        ii = i
        if nig eq 1 then $
          ii = 0
;;        Xin[i] = fx_root(initial_guess[*, ii], $
;;                         'pfo_convert_coord_funct', /double, $
;;                         _EXTRA=extra)
        Xin[i] = newton(initial_guess[0], $
                        'pfo_convert_coord_funct', /double, $
                        _EXTRA=extra)
     endfor
  endelse ;; Using fx_root to find Xin

  ;; Now that we are done with are parinfo heap variable, put it back
  ;; into the command line argument
  parinfo_in = temporary(*pparinfo)
  ptr_free, pparinfo

  ;; Return values
  case to_axis of 
     !pfo.Xin:   return, Xin
     !pfo.Xaxis: return, pfo_Xaxis(Xin, parinfo=parinfo_in, idx=idx)
     !pfo.Yaxis: return, pfo_funct(Xin, parinfo=parinfo_in, idx=idx, Xaxis=Xaxis)
  endcase

end
