;+
; NAME: pfo_debug
;
; PURPOSE: set debugging level for PFO programs
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_debug, [level]
;
; DESCRIPTION: Works with three system variables in order to set the
; debugging level of PFO programs

; 0: 	All CATCH statements in PFO code are respected
;	Default IDL XMANAGER behavior
;	Default IDL math exception reporting

; 1:	Most CATCH statements in PFO code are turned off, allowing easier
; 	debugging of PFO code
;	Default IDL XMANAGER behavior
;	Default IDL math exception reporting

; 2:	More CATCH statements in PFO code are turned off, in
; 	particular those for the pfo_obj->update method and widget events

; 3:    Like 2 but 	
;	XMANAGER CATCHing is turned off, alowing easier debugging of
;	widget code
;	Default IDL math exception reporting

; 4:	CATCH statements in PFO code are turned off, allowing easier
; 	debugging of PFO code
;	XMANAGER CATCHing is turned off, alowing easier debugging of
;	widget code
;	Math error (e.g. divide by 0, floating underflow, etc.)
;	locations are reported so they can be more easily coded around
;	(e.g. see pfo_scint).

; If level is not specified, 2 is used

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
; $Id: pfo_debug.pro,v 1.3 2011/09/16 11:25:02 jpmorgen Exp $
; $Log: pfo_debug.pro,v $
; Revision 1.3  2011/09/16 11:25:02  jpmorgen
; Changed debug levels to add finer control over PFO CATCH statements.
; Currently level 2 disables CATCH in pfo_parinfo_obj::update method.
; Might want to add event catchning too.
;
; Revision 1.2  2011/08/01 19:00:47  jpmorgen
; First reasonably functional version of pfo_obj
; Added documentation and math reporting on pfo_debug, 3
;
; Revision 1.1  2011/01/20 22:59:08  jpmorgen
; Initial revision
;
;-
pro pfo_debug, level
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ON_ERROR, !tok.return
  message, /INFORMATIONAL, 'Current debug level: ' + strtrim(!pfo.debug, 2) 

  if N_params() eq 0 then begin
     message, /CONTINUE, 'USAGE: pfo_debug, level (0 = normal, 1 = CATCH statements skipped, 2 = XMANAGER catching turned off, 3 = full math exception reporting)'
     return
  endif

  ;; The !pfo.level system variable is interpreted in individual
  ;; routines, generally around CATCH statements.  See case
  ;; documentation for intepretation at different levels
  !pfo.debug = level

  case level of
     0: begin ;; normal
        ;; PFO CATCH statements are fully interpreted

        ;; XMANAGER, catch=1 (IDL default) catches errors in widget
        ;; event processing code, but lets widget event processing
        ;; continue.  This makes it possible to close open widget
        ;; windows and do at least some of the heap variable cleanup.
        ;; But often, XMANAGER generates a bunch of errors about
        ;; undefined "state" variable (or whatever the widget uvalue
        ;; variable that keeps track of internal widget info is
        ;; called).  It also doesn't help debug the offending
        ;; code (see pfo_debug,2)
        xmanager, catch=1
        ;; Invalid but not-fatal math computations (e.g. underflow)
        ;; are reported once per run (IDL default).  For widgets, this
        ;; ends up creating lots of "Program caused arithmetic error"
        ;; messages, since each widget event is effectively an
        ;; independent IDL run (see pfo_quiet and pfo_debug, 3).
        !except = 1
     end
     1: begin
        ;; CATCH statements in PFO_DEBUG-enabled code are skipped,
        ;; allowing errors in PFO primitives code to be more easily
        ;; debugged
        
        ;; Same states for xmanager catch and math exception reporting
        ;; as pfo_debug, 0
        xmanager, catch=1
        !except = 1
     end
     2: begin
        ;; as with 1, but some additional CATCH statements in
        ;; PFO_DEBUG-enabled code are skipped.
        xmanager, catch=1
        ;; Math exception still IDL default
        !except = 1
     end
     3: begin
        ;; Like 2, but turn on full math exception reporting.  This
        ;; reports the location of the offending calculation.  Ideally
        ;; you write your code to avoid math exceptions for expected
        ;; cases, so that when one happens, you know that there is a
        ;; real problem.
        xmanager, catch=0
        !except = 2
     end
     4: begin
        ;; Like 2, but turn on full math exception reporting.  This
        ;; reports the location of the offending calculation.  Ideally
        ;; you write your code to avoid math exceptions for expected
        ;; cases, so that when one happens, you know that there is a
        ;; real problem.
        xmanager, catch=0
        !except = 2
     end
     else: begin
        message, /CONTINUE, 'NOTE: debug levels 0,1,2,3,4 are supported, setting to level 2'
        pfo_debug, 2
     end
  endcase

  message, /INFORMATIONAL, 'Debug level set to: ' + strtrim(!pfo.debug, 2)

end
