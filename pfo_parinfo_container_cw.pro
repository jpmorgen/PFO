;+
; NAME: pfo_parinfo_container_cw

; PURPOSE: This is a special compound widget which is used by
; pfo_parinfo_parse.  See DESCRIPTION

; CATEGORY: PFO widgets
;
; CALLING SEQUENCE: ID = pfo_parinfo_container_cw(parentID,
; containerID=containerID, cw_obj=cw_obj, _EXTRA=extra)

; DESCRIPTION: This creates a compound widget with nothing in it.  The
; container is initilized as a column widget, with base_align_left
; set, appropriate for display of parinfo widgets launched from within
; pfo_parinfo_parse.  Since this does create a persistent object on
; the heap (at least until its parent widget is killed), this object
; is the appropriate place from which to repopulate the widget when
; requested.  Repopulation can only occur if this instance of
; pfo_parinfo_container_cw is displaying all of the parinfo array.

; The routine pfo_parinfo_cw_obj::register_repop has a list of
; keywords that invalidate the ability of this widget to repopulate
; itself with the entire contents of the parinfo.  It is therefore
; important to pass all of these keywords to this widget!
; pfo_parinfo_parse should take care of all of this.

; If pfo_parinfo_parse was called by code that selected idx, ispec,
; etc. (the keywords that invalidate the ability of this widget to
; repopulate), then that calling code needs to provide a repopulate
; method.  This repopulate method needs to be intelligent enough to
; independently derive what section of the parinfo should be
; displayed.

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
; $Id: pfo_parinfo_container_cw.pro,v 1.1 2011/09/01 22:19:31 jpmorgen Exp $
;
; $Log: pfo_parinfo_container_cw.pro,v $
; Revision 1.1  2011/09/01 22:19:31  jpmorgen
; Initial revision
;
;-

;; Repopulate method for pfo_parinfo_container_cw.  We get called if there is a
;; repopulate and we are just displaying the entire parinfo
pro pfo_parinfo_container_cw_obj::repopulate
  ;; Turn off update in the parent so we don't unnecessarily redraw
  ;; widgets
  widget_control, self.parentID, update=0
  ;; Kill the container and all its contents.  Each of the children
  ;; should properly issue pfo_obj->unregister_refresh.  This also
  ;; creates a fresh container into which we will draw the new version
  ;; of the widget
  self->clear_container
  
  ;; Call pfo_parinfo_parse to repopulate our container.  In our init
  ;; method, we have intercept the parameters that would have
  ;; invalidated repopulation (e.g. idx, iROI, etc.).  All other
  ;; parameters need to be passed onto our underlying __widget
  ;; routines so that everything ends up getting displayed the way we
  ;; want it.  --> I am currently not allowing for customization of
  ;; widgets to persist across calls to repopulate.  This might be
  ;; possible, but would take some property to keep track of it.
  junk = self.pfo_obj->parinfo_call_function( $
         'pfo_parinfo_parse', /widget, pfo_obj=self.pfo_obj, $
         containerID=self.containerID, $
         _EXTRA=*self.pextra)

  ;; Redraw the parent widget
  widget_control, self.parentID, update=1
end

;; Cleanup method
pro pfo_parinfo_container_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_container_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   _REF_EXTRA=extra ;; All other input parameters are passed to underlying routines via _REF_EXTRA

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object not properly initialized ', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Call our inherited init routines.  This puts pfo_obj into self,
  ;; among other things
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register in the repopulate list (if possible)
  self->register_repop, _EXTRA=extra

  ;; Make our container as a column base with each row left justified.
  ;; Subsequent widgets decide how to handle each row.
  self->create_container, /column, /base_align_left

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition
pro pfo_parinfo_container_cw_obj__define
  objectClass = $
     {pfo_parinfo_container_cw_obj, $
      inherits pfo_parinfo_cw_obj}
end


;;-------------------------------------------------------------------
;; MAIN ROUTINE
;;-------------------------------------------------------------------

;; Pattern this on ~/pro/coyote/fsc_field.pro
function pfo_parinfo_container_cw, $
   parentID, $ ;; Parent widget ID (positional parameter)
   containerID=containerID, $ ;; (output) parent widget of individual parinfo function widgets
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra ;; All other input parameters passed to the init method and underlying routines via _REF_EXTRA mechanism

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere
  containerID = !tok.nowhere

  ;; Create our controlling object
  cw_obj = obj_new('pfo_parinfo_container_cw_obj', parentID, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
     containerID = cw_obj->containerID()
  endif ;; valid cw_obj

  return, cwID

end
