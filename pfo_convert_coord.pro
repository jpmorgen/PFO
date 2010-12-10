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
; another.  Uses IDL's fx_root function to do the work.
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
;	initial_guess -- the initial guess for the solver.  Converted
;                       to from_axis.  Defaults to 0.
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
; $Id: pfo_convert_coord.pro,v 1.1 2010/12/10 21:57:53 jpmorgen Exp $
; $Log: pfo_convert_coord.pro,v $
; Revision 1.1  2010/12/10 21:57:53  jpmorgen
; Initial revision
;
;-

;; The basic idea is to always find the Xin value corresponding to the
;; Xaxis or Yaxis value.  We pass the value on the axis we have been
;; given (e.g. some Xaxis value).  We are using fx_root, which needs
;; to find where that is 0
function pfo_convert_coord_funct, Xin
  COMMON pfo_convert_coord, value, axis, pparinfo, idx
  on_error, 0
  case axis of 
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

  COMMON pfo_convert_coord, value, axis, pparinfo, idx

  if !pfo.debug eq 0 then begin
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

  if N_params() ne 2 then $
    message, 'ERROR: supply two positional parameters: the value and the parinfo'
  ;; Common block variables cannot be parameters.  We need to
  ;; explicitly copy our values into the common block.  This means
  ;; that we have bad memory practices for large arrays (e.g. the
  ;; parinfo).  Use a heap variable to get around this.  It would be a
  ;; lot easier if fx_root passed _EXTRA along to its primitive.
  pparinfo = ptr_new(parinfo_in, /no_copy)
  ;; Beware the persistence of COMMON block variables.  Fortunately
  ;; not in type.
  if keyword_set(idx_in) then $
    idx = idx_in $
  else $
    idx = indgen(N_elements(*pparinfo), type=size(N_elements(*pparinfo), /type))
  if keyword_set(from_Xin) then $
    axis = !pfo.Xin 
  if keyword_set(from_Xaxis) then $
    axis = !pfo.Xaxis 
  if keyword_set(from_Yaxis) then $
    axis = !pfo.Yaxis
  if NOT keyword_set(axis) then $
    message, 'ERROR: one of /from_Xin, /from_Xaxis, /from_Yaxis must be specified'

  npts = N_elements(value_in)
  ;; We will use IDL's fx_root to find Xin for each point
  Xin = make_array(npts, value=!values.d_nan)

  ;; Do the dance that preserves the initial_guess input variable and
  ;; replicates it to an array of the same length of the values, if
  ;; necessary.
  if N_elements(initial_guess_in) eq 0 then $
    initial_guess_in = 0d
  initial_guess = initial_guess_in
  ;; --> I want to allow for proper bracketing with the initial guess vector
  nig = N_elements(initial_guess)
  if nig ne npts and nig ne 1 then $
      message, 'ERROR: initial_guess has ' + strtrim(nig, 2) + ' elements but there are ' + strtrim(npts, 2) + ' values'
  
  if nig eq 1 then $
    initial_guess = make_array(npts, value=initial_guess)

  ;; Convert our initial guess to the proper axis
  case axis of
     !pfo.Xin : 
     !pfo.Xaxis : initial_guess = $
       pfo_Xaxis(temporary(initial_guess), parinfo=parinfo, idx=idx)
     !pfo.Yaxis : initial_guess = $
       pfo_funct(temporary(initial_guess), parinfo=parinfo, idx=idx)
     endcase

  for i=0,npts-1 do begin
     value = value_in[i]
     Xin[i] = fx_root([initial_guess[i], initial_guess[i]-100, initial_guess[i]+100], $
                       'pfo_convert_coord_funct', /double, $
                       _EXTRA=extra)
  endfor

  ;; Now that we are done with are parinfo heap variable, put it back
  ;; into the command line argument
  parinfo_in = temporary(*pparinfo)
  ptr_free, pparinfo

  ;; Return values
  if keyword_set(to_Xin) then $
    return, Xin
  if keyword_set(to_Xaxis) then $
    return, pfo_Xaxis(Xin, parinfo=parinfo_in, idx=idx)
  if keyword_set(to_Yaxis) then $
    return, pfo_funct(Xin, parinfo=parinfo_in, idx=idx, Xaxis=Xaxis)

  message, 'ERROR: one of /to_Xin, /to_Xaxis, /to_Yaxis must be specified'

end
