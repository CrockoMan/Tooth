//////////////////////////////////////////////////////////////////////
//
//  SDIDIALG.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2003. All rights reserved.         
//  
//  Contents:
//      Service routines for SDIDEMO.EXE to access the child list
//   
//////////////////////////////////////////////////////////////////////


#include "Sdidemo.ch"
#include "Appevent.ch"
#include "Xbp.ch"


/*
 * Get child list of an XBP
 */
FUNCTION ChildList( oXbp )
   LOCAL aChild

   IF oXbp:className() == "XbpDialog"
      aChild := oXbp:drawingArea:childList()
   ELSE
      aChild := oXbp:childList()
   ENDIF

RETURN aChild



/*
 * Routine to temporarily disable an Xbase Part
 */
FUNCTION DisableXbp( oDlg, nNameID, aMenuItemIDs )
   LOCAL oXbp := oDlg:childFromName( nNameID ), i, imax

   IF oXbp <> NIL
      IF Valtype( aMenuItemIDs ) == "A" .AND. ;
         "XbpMenu" $ oXbp:className()
         imax := Len( aMenuItemIDs )
         FOR i:=1 TO imax
            oXbp:disableItem( aMenuItemIDs[i] )
         NEXT
      ELSE
         oXbp:disable()
      ENDIF
   ENDIF

RETURN oXbp



/*
 * Routine to enable an Xbase Part
 */
FUNCTION EnableXbp( oDlg, nNameID, aMenuItemIDs )
   LOCAL oXbp := oDlg:childFromName( nNameID ), i, imax

   IF oXbp <> NIL
      IF Valtype( aMenuItemIDs ) == "A" .AND. ;
         "XbpMenu" $ oXbp:className()
         imax := Len( aMenuItemIDs )
         FOR i:=1 TO imax
            oXbp:enableItem( aMenuItemIDs[i] )
         NEXT
      ELSE
         oXbp:enable()
      ENDIF
   ENDIF

RETURN oXbp



/*
 * Change record pointer in dialog
 */
PROCEDURE DlgSkip( oDlg, nSkip )

   IF nSkip == NIL
      nSkip := 0
   ENDIF

   IF nSkip <> 0
      IF ! DlgHasChanged( oDlg ) .OR. DlgWriteData( oDlg )
         DbSkip( nSkip )
      ENDIF
   ELSE
      DbSkip( 0 )
   ENDIF

   oDlg:cargo[1] := Recno()
   DlgReadData( oDlg )

   IF Eof()
      DisableXbp( oDlg, ID_DATA_MENU, {1,4} )
      DisableXbp( oDlg, ID_BTN_NEXT )
      DisableXbp( oDlg, ID_BTN_SAVE )

      EnableXbp ( oDlg, ID_SLE_PRIMKEY )
      EnableXbp ( oDlg, ID_BTN_UNDO )
      EnableXbp ( oDlg, ID_BTN_PREVIOUS )
      EnableXbp ( oDlg, ID_DATA_MENU, {2,3} )

   ELSEIF Bof()
      DisableXbp( oDlg, ID_BTN_PREVIOUS )
      EnableXbp ( oDlg, ID_BTN_NEXT )

   ELSE
      EnableXbp ( oDlg, ID_BTN_PREVIOUS )
      EnableXbp ( oDlg, ID_BTN_NEXT )
   ENDIF

   IF ! Eof()
      EnableXbp ( oDlg, ID_DATA_MENU, {1,4} )
      DisableXbp( oDlg, ID_SLE_PRIMKEY )
      DisableXbp( oDlg, ID_BTN_SAVE )
      DisableXbp( oDlg, ID_BTN_UNDO )
      DisableXbp( oDlg, ID_DATA_MENU, {2,3} )
   ENDIF

RETURN



/*
 * Save edited data and handle enable/disable stuff
 */
PROCEDURE DlgSave( oDlg )
   LOCAL lAppend := Eof()

   IF DlgWriteData( oDlg, lAppend )
      IF lAppend
         EnableXbp ( oDlg, ID_BTN_PREVIOUS )
         DisableXbp( oDlg, ID_SLE_PRIMKEY  )
      ENDIF
      DisableXbp( oDlg, ID_BTN_SAVE )
      DisableXbp( oDlg, ID_BTN_UNDO )
      EnableXbp ( oDlg, ID_DATA_MENU, {1,4} )
      DisableXbp( oDlg, ID_DATA_MENU, {2,3} )
   ENDIF

RETURN



/*
 * Restore changed data
 */
PROCEDURE DlgUndo( oDlg )

   IF Eof()
      DbGoto( oDlg:cargo[2] )
      EnableXbp( oDlg, ID_DATA_MENU, {1,2,4} )
   ENDIF

   DisableXbp( oDlg, ID_DATA_MENU, {3} )
   DlgSkip( oDlg, 0 )

RETURN



/*
 * Add a new record
 */
PROCEDURE DlgNew( oDlg )

   oDlg:cargo[2] := oDlg:cargo[1]
   DbGobottom()
   DlgSkip( oDlg, 1 )

   SetAppFocus( oDlg:childFromName( ID_SLE_PRIMKEY ) )
RETURN



/*
 * Delete a record
 */
PROCEDURE DlgDelete( oDlg )
   LOCAL nButton, oXbp := SetAppFocus()

   nButton := ConfirmBox( , ;
                 "Delete this record ?", ;
                 "Delete", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nButton == XBPMB_RET_YES

      IF Rlock()
         DbDelete()
         DbUnlock()
         DlgSkip( oDlg, 0 )
      ELSE
         MsgBox( "Record is currently locked!" )
      ENDIF
   ENDIF

   SetAppFocus( oXbp )
RETURN



/*
 * Read data from database into the editbuffer of XBPs
 */
PROCEDURE DlgReadData( oDlg )
   LOCAL bError   := ErrorBlock( {|e| Break(e) } )
   LOCAL aChild   := ChildList( oDlg )
   LOCAL i, imax  := Len( aChild )
   LOCAL oXbp

   FOR i:=1 TO imax
      oXbp := aChild[i]
      BEGIN SEQUENCE
         IF ! Empty( oXbp:childList() )
            DlgReadData( oXbp )
         ELSEIF ! "XbpStatic" $ oXbp:className()
            oXbp:setData()
         ENDIF
      ENDSEQUENCE
   NEXT
   ErrorBlock( bError )

RETURN



/*
 * Write data from dialog into database
 */
FUNCTION DlgWriteData( oDlg, lAppend )
   LOCAL lLocked := .F., lDone := .F., oXbp := SetAppFocus()

   IF Valtype( lAppend ) <> "L"
      lAppend := .F.
   ENDIF

   IF lAppend
      IF DlgValidateData( oDlg )    // Validate data before Append
         APPEND BLANK
         lLocked := ! NetErr()
      ELSE                          // Do not write invalid data
         MsgBox("Invalid data")     // to new record
         SetAppFocus( oXbp )
         RETURN .F.                 // *** RETURN ***
      ENDIF
   ELSE
      lLocked := DbRLock( Recno() ) // Lock record
   ENDIF

   IF lLocked
      WriteChildData( oDlg:drawingArea )
      DbRUnlock( Recno() )          // Release record lock
      lDone := .T.
   ELSE
      MsgBox( "Record is currently locked" )
   ENDIF
   SetAppFocus( oXbp )

RETURN lDone



/*
 * Write data from dialog into database
 * Errors caused by an XBP not having the methods :validate(),
 * :getData() and :undo() are handled via Errorblock
 */
STATIC PROCEDURE WriteChildData( oXbp )
   LOCAL aChild := oXbp:childList()
   LOCAL bError := ErrorBlock( {|e| Break(e) } )
   LOCAL i:=0, imax:= Len( aChild )

   FOR i:=1 TO imax
      oXbp := aChild[i]
      BEGIN SEQUENCE
         IF ! Empty( oXbp:childList() )
            WriteChildData( oXbp )  // Recursion
         ELSEIF "XbpStatic" $ oXbp:className()
         ELSEIF oXbp:validate()     // Validate data
            oXbp:getData()          // Write data to file
         ELSE
            oXbp:undo()             // Validation failed
         ENDIF
      ENDSEQUENCE
   NEXT

   ErrorBlock( bError )
RETURN



/*
 * Check if data in a dialog has changed
 */
FUNCTION DlgHasChanged( oDlg )
   LOCAL lChanged := .F.
   LOCAL bError   := ErrorBlock( {|e| Break(e) } )
   LOCAL aChild   := ChildList( oDlg )
   LOCAL i, imax  := Len( aChild )
   LOCAL oXbp

   i:=0
   DO WHILE ++i <= imax .AND. ! lChanged
      oXbp := aChild[i]
      BEGIN SEQUENCE
         IF ! lChanged
            IF ! Empty( oXbp:childList() )
               lChanged := DlgHasChanged( oXbp )
            ELSEIF ! "XbpStatic" $ oXbp:className()
               lChanged := oXbp:changed
            ENDIF
         ENDIF
      ENDSEQUENCE
   ENDDO
   ErrorBlock( bError )

RETURN lChanged



/*
 * Validate editbuffer of XBPs
 */
FUNCTION DlgValidateData( oDlg )
   LOCAL bError   := ErrorBlock( {|e| Break(e) } )
   LOCAL aChild   := ChildList( oDlg )
   LOCAL i, imax  := Len( aChild )
   LOCAL oXbp, lValid := .T.

   i:=0
   DO WHILE ++i <= imax .AND. lValid
      oXbp := aChild[i]
      BEGIN SEQUENCE
         IF ! Empty( oXbp:childList() )
            lValid := DlgValidateData( oXbp )
         ELSEIF ! "XbpStatic" $ oXbp:className()
            lValid := oXbp:validate()
            IF ! lValid
               Tone(1000)             // Set focus to XBP
               SetAppFocus( oXbp )    // with invalid data
            ENDIF
         ENDIF
      ENDSEQUENCE
   ENDDO
   ErrorBlock( bError )

RETURN lValid



/*
 * Delete all children of a dialog except
 * XbpIWindow, XbpMenuBar and XbpMenu
 */
PROCEDURE DelChildList( oDlg )
   LOCAL aChild  := oDlg:drawingArea:childList()
   LOCAL i, imax := Len( aChild ), oXbp

   FOR i:=1 TO imax
      oXbp    := aChild[i]
      IF oXbp:className() $ "XbpIWindow,XbpMenubar"
         LOOP
      ENDIF
      oXbp:hide()
      oDlg:delChild( oXbp )
   NEXT
RETURN



/*
 * Customized key handler
 */
PROCEDURE DlgKeyHandler( nKey, oXbp, aControls, oDlg )
   LOCAL i

   DO CASE
   CASE nKey == xbeK_RETURN

      DO CASE
      CASE oXbp:isDerivedFrom( "XBPPUSHBUTTON" )
         // Activate the pushbutton
         PostAppEvent( xbeP_Activate,,, oXbp )

      CASE oXbp:isDerivedFrom( "XBPMLE" )
         // Line break in MLE - do nothing

      OTHERWISE
          i := AScan( aControls, oXbp )

          IF ++i > Len( aControls )
             i := 1
          ENDIF
          DO WHILE ! aControls[i]:isEnabled()
             // At least one control must be enabled!!
             IF ++i > Len( aControls )
                i := 1
             ENDIF
          ENDDO
          SetAppFocus( aControls[i] )
      ENDCASE

   CASE nKey == xbeK_PGDN
      IF ! Eof()
         DLgSkip( oDlg, 1 )
      ENDIF

      IF Eof()
         Tone( 500 )
         Tone( 500 )
         Tone( 500 )
      ENDIF

   CASE nKey == xbeK_PGUP
      IF ! Bof()
         DlgSkip( oDlg, -1 )
      ENDIF
      IF Bof()
         Tone( 1000 )
         Tone( 1000 )
         Tone( 1000 )
      ENDIF

   CASE Upper( oXbp:className() ) $ "XBPSLE,XBPMLE"
      IF oXbp:changed
         EnableXbp( oDlg, ID_BTN_SAVE )
         EnableXbp( oDlg, ID_BTN_UNDO )
         EnableXbp( oDlg, ID_DATA_MENU, {2,3} )
      ENDIF

   ENDCASE

RETURN
