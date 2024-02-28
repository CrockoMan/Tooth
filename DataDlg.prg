//////////////////////////////////////////////////////////////////////
//
//  DATADLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2003. All rights reserved.         
//  
//  Contents:
//      Class DataDialog and DataDialogMenu
//   
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Dmlb.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Mdidemo.ch"


/*
 * Class declaration of a Seek dialog
 */
CLASS SeekDialog FROM XbpDialog
   PROTECTED:
     VAR staticText           // Text "Enter:"
     VAR sleSeek              // Entry field
     VAR listBox              // Listbox to display found records
     VAR buttonOK             // OK pushbutton
     VAR buttonCancel         // Cancel pushbutton
     VAR aFound               // Array for found records
     METHOD arrayToListBox    // Transfer aFound to Listbox

   EXPORTED:
     VAR fieldList            // Array containing field names
     VAR indexOrder           // Index to be searched
     VAR transform            // Code block to convert the search string
                              // to a valid DbSeek() value
     VAR dataDialog           // A DataDialog window
     VAR lastFocus            // The Xbase Part that had focus

     METHOD init              // Life cycle
     METHOD create
     METHOD destroy
     METHOD handleKey         // Customized key handler
     METHOD show              // Display SeekDialog
     METHOD seek              // Seek all records matching the entered
                              // search string
     METHOD goTo              // Go to selected record
ENDCLASS



/*
 * Initialize the object
 */
METHOD SeekDialog:init( oParent, oOwner, aPos )
   LOCAL bKeyboard

   DEFAULT aPos TO  {260,205}

   ::xbpDialog:init( oParent, oOwner, aPos, {400,280}, NIL, .F. )
   ::title     := "Seek"
   ::border    := XBPDLG_THINBORDER
   ::minButton := .F.
   ::maxButton := .F.
   ::sysMenu   := .F.

   bKeyBoard := {|mp1,mp2,obj| ::handleKey( mp1, obj ) }

   ::staticText := XbpStatic():new( ::drawingArea ,, {10,212}, {80,20} )
   ::staticText:caption := "Enter: "

   ::sleSeek := XbpSLE():new( ::drawingArea ,, {90,210}, {285,22} )
   ::sleSeek:tabStop  := .T.
   ::sleSeek:keyBoard := bKeyboard

   ::listBox := XbpListBox():new( ::drawingArea,, {10,60}, {370,135})
   ::listBox:tabStop := .T.
   ::listBox:itemSelected := {|| ::goTo() }

   ::buttonOk := XbpPushButton():new( ::drawingArea,, {10,13}, {100,30} )
   ::buttonOk:caption      := "Ok"
   ::buttonOk:tabstop      := .T.
   ::buttonOK:keyBoard     := bKeyboard
   ::buttonOK:activate     := {|| ::goTo() }

   ::buttonCancel := XbpPushButton():new( ::drawingArea,, {140,13}, {100,30})
   ::buttonCancel:caption  := "Cancel"
   ::buttonCancel:keyBoard := bKeyboard
   ::buttonCancel:activate := {|| ::destroy() }

   ::transform := {|x| x }
   ::aFound    := {}
RETURN self



/*
 * Request system resources
 */
METHOD SeekDialog:create( nOrder, bTransform, aFields, cTitle )

   IF Valtype( cTitle ) == "C"
      ::title := cTitle
   ENDIF

   ::xbpDialog:create()
   ::staticText:create()
   ::sleSeek:create()
   ::listBox:create()
   ::listBox:setFontCompoundName( FONT_DEFFIXED_MEDIUM )
   ::buttonOk:create()
   ::buttonCancel:create()

   ::indexOrder := nOrder
   ::fieldList  := aFields
   ::transform  := bTransform
   ::setOwner():disable()       // Disable owner so that
                                // Seekdialog becomes modal
RETURN self



/*
 * Release system resources
 */
METHOD SeekDialog:destroy
   ::setOwner():enable()
   ::hide()
   IF ::lastFocus <> NIL
      SetAppFocus( ::lastFocus )
      ::lastFocus := NIL
   ENDIF
   ::dataDialog := NIL
   ::xbpDialog:destroy()
   
RETURN self



/*
 * Process Return and Esc keys
 */
METHOD SeekDialog:handleKey( nKey, obj )

   DO CASE
   CASE nKey == xbeK_ESC
      ::destroy()

   CASE nKey == xbeK_RETURN
      DO CASE
      CASE obj == ::sleSeek
         ::seek()
      CASE obj == ::buttonOK .OR. obj == ::buttonCancel
         PostAppEvent( xbeP_Activate,,, obj )
      ENDCASE
   ENDCASE

RETURN self



/*
 * Display the dialog window and set focus to SLE
 */
METHOD SeekDialog:show
   ::xbpDialog:show()
   SetAppFocus(::sleSeek)
RETURN self



/*
 * Seek data fron SLE in index
 */
METHOD SeekDialog:seek
   LOCAL nOldArea := select()
   LOCAL cData    := Eval( ::transform, ::sleSeek:getData() )
   LOCAL aFound   := {}
   LOCAL i, imax, nOldOrder, nOldRec, nStartRec, nEndRec

   DbSelectArea( ::dataDialog:area )
   DbSuspendNotifications()           // Prevent Notify message to dialog
                                      // during DbSeek() and DbSkip()
   nOldOrder := OrdNumber()
   nOldRec   := Recno()

   OrdSetFocus( ::indexOrder )
   cData := Trim( cData )


   IF DbSeek( cData, .T. )            // Data is found
      nStartRec := nEndRec := Recno()
      IF DbSeekLast( cData )          // Find last matching record
         nEndRec := Recno()
      ENDIF

      DbGoto( nStartRec )             // Load records to array
      AAdd( aFound, { RecordToString( ::fieldList ), Recno() } )
      DO WHILE nEndRec <> Recno()
         DbSkip()
         AAdd( aFound, { RecordToString( ::fieldList ), Recno() } )
      ENDDO

      ::aFound := aFound
      ::arrayToListBox()              // Transfer array to list box
   ELSE
      MsgBox( cData + Chr(13)+Chr(10) + "Not found!" )
   ENDIF

   OrdSetFocus( nOldOrder )
   DbGoto( nOldRec )
   DbResetNotifications()
   DbSelectArea( nOldArea )

RETURN self



/*
 * Transfer found data to listbox
 */
METHOD SeekDialog:arrayToListBox
    LOCAL i, imax := Len( ::aFound )

    ::listBox:clear()

    FOR i:=1 TO imax
       ::listBox:addItem( ::aFound[i,1] )
    NEXT
RETURN self



/*
 * Go to the record selected in the listbox
 */
METHOD SeekDialog:goto
    LOCAL aItems := ::listBox:getData()
    LOCAL nArea  := ::dataDialog:area
    LOCAL nRecno

    IF ! Empty(aItems)
       nRecno := ::aFound[ aItems[1], 2 ]
    ENDIF

    ::destroy()

    IF nRecno <> NIL
       (nArea) -> ( DbGoTo( nRecno ) )
    ENDIF

RETURN self



/*
 * Create one string from multiple fields
 */
FUNCTION RecordToString( aFieldNames )
   LOCAL i, imax := Len( aFieldNames )
   LOCAL cString := "", xValue, cType

   FOR i:=1 TO imax
      xValue := FieldGet( FieldPos( aFieldNames[i] ) )
      cType  := Valtype( xValue )
      DO CASE
      CASE cType == "C"
         cString += xValue
      CASE cType == "D"
         cString += DtoC( xValue )
      CASE cType == "L"
         cString += IIf( xValue, ".T.", ".F." )
      CASE cType == "N"
         cString += Str( xValue )
      ENDCASE
      IF i < imax
         cString += " " + Chr(179) +" "
      ENDIF
   NEXT

RETURN cString



/*
 * Find last record matching a given index value
 * - the index expression MUST return a string
 */
FUNCTION dbSeekLast( cString )
   LOCAL cIndexKey, cIndexVal, nLen

   cIndexKey  := IndexKey(0)
   IF Empty( cIndexKey )               // No index active
      RETURN .F.                       // *** RETURN  ***
   ENDIF

   nLen    := Len( cString )           // Increase last Chr() by 1
   DbSeek( Left(cString,nLen-1) + ;    // for SOFTSEEK
           Chr(Asc(Right(cString,1)) + 1 ), ;
          .T.)                         // SOFTSEEK ON
   DbSkip(-1)                          // skip 1 back
                                       // Get index value
   IF ( cString == Left(&(cIndexKey),nLen) )
      RETURN .T.                       // Ok, we have a match!
   ENDIF
                                    
   DbSkip(1)                           // In case that Eof() was .T.

RETURN .F.



/*
 * Seek record for current child window
 */
PROCEDURE SeekRecord()
   LOCAL oSeekDlg, aPos, aPos1, oDlg := WinMenu():currentWin

   aPos     := oDlg:currentPos()
   aPos1    := RootWindow():currentPos()
   aPos[1]  += 10
   aPos[2]  += 10
   aPos[1]  += aPos1[1]
   aPos[2]  += aPos1[2]
   oSeekDlg := SeekDialog():new( AppDesktop(), ;
                                 RootWindow(), aPos )

   oSeekDlg:create( oDlg:seekOrder , ;
                    oDlg:seekExpr  , ;
                    oDlg:seekFields, ;
                    oDlg:seekTitle   )

   oSeekDlg:lastFocus  := SetAppFocus()
   oSeekDlg:dataDialog := oDlg
   oSeekDlg:show()
RETURN


/*
 * Delete record in current child window
 */
PROCEDURE DeleteRecord()
   LOCAL nButton, oXbp := SetAppFocus()
   LOCAL nArea := Select()

   nButton := ConfirmBox( , ;
                 "Delete this record ?", ;
                 "Delete", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nButton == XBPMB_RET_YES
      DbSelectArea( WinMenu():currentWin:area )
   
      IF Rlock()
         DbDelete()
         DbUnlock()
         DbSkip( 0 )
      ELSE
         MsgBox( "Record is currently locked!" )
      ENDIF
      DbSelectArea( nArea )
   ENDIF

   SetAppFocus( oXbp )
RETURN


//-----------------------------------------------------------------------------------------------------------------------------------------------------------



/*
 * Class declaration
 */
CLASS DataDialog FROM XbpDialog
   PROTECTED:
     VAR appendMode              // Is it a new record?
     VAR editControls            // List of XBPs for editing data
     VAR appendControls          // List of XBPs enabled only
                                 // during APPEND
     VAR keyControls             // List of XBPs for keyboard navigation
     VAR keyboardBlock           // Code block for xbeP_Keyboard messages

   EXPORTED:
     VAR area      READONLY      // current work area
     VAR newTitle                // code block to change window title
     VAR contextMenu             // context menu for data dialog
     VAR windowMenu              // dynamic window menu in
                                 // application window
     VAR currentControl          // references the current Edit control
     VAR seekOrder               // Data for searching a record
     VAR seekExpr                // using the SeekDialog
     VAR seekFields
     VAR seekTitle

     METHOD init                 // overloaded methods
     METHOD create
     METHOD configure
     METHOD destroy
     METHOD addEditControl       // register XBP for edit
     METHOD addAppendControl     // register XBP for append
     METHOD addKeyControl        // register XBP for keyboard handling
     METHOD notify               // process DBO message
     METHOD readData             // read data from DBF
     METHOD validateAll          // validate all data stored in XBPs
     METHOD writeData            // write data from XBPs to DBF
     METHOD isIndexUnique        // check index value for uniqueness
     METHOD setDisplayFocus      // called after window has received focus
     METHOD keyHandler           // key handling for edit controls
ENDCLASS



/*
 * Initialize data dialog
 */
METHOD DataDialog:init( oParent, oOwner , ;
                        aPos   , aSize  , ;
                        aPParam, lVisible )
   LOCAL aPSize

   DEFAULT lVisible TO .F., ;
           oParent  TO SetAppWindow()

  /*
   * Default position: centered on parent
   */
   IF aPos == NIL
      aPSize := oParent:currentSize()
      aPos   := { (aPSize[1] - aSize[1]) / 2, ;
                  (aPSize[2] - aSize[2]) / 2  }
   ENDIF

   ::xbpDialog:init( oParent, oOwner, ;
                     aPos   , aSize , ;
                     aPParam, lVisible )

   ::maxButton       := .F.
   ::area            := 0
   ::border          := XBPDLG_THINBORDER
   ::editControls    := {}
   ::appendControls  := {}
   ::keyControls     := {}
   ::keyboardBlock   := {|nKey,x,obj| ::keyHandler( nKey, obj ) }
   ::appendMode      := .F.
   ::newTitle        := {|obj| obj:getTitle() }

RETURN self



/*
 * Load system resources and
 * register DataDialog in current work area
 */
METHOD DataDialog:create( oParent, oOwner , ;
                          aPos   , aSize  , ;
                          aPParam, lVisible )

   ::xbpDialog:create( oParent, oOwner , ;
                       aPos   , aSize  , ;
                       aPParam, lVisible )

   ::appendMode      := Eof()
   ::area            := Select()

   ::close           := {|mp1,mp2,obj| obj:destroy() }

   DbRegisterClient( self )

RETURN self



/*
 * Configure system resources
 * Register data dialog in new work area if necessary
 */
METHOD DataDialog:configure( oParent, oOwner , ;
                             aPos   , aSize  , ;
                             aPParam, lVisible )
   LOCAL lRegister := (::area <> Select())

   ::xbpDialog:configure( oParent, oOwner , ;
                          aPos   , aSize  , ;
                          aPParam, lVisible )
   IF lRegister
      (::area)->( DbDeRegisterClient( self ) )
   ENDIF

   ::area       := Select()
   ::appendMode := Eof()

   IF lRegister
      DbRegisterClient( self )
   ENDIF

RETURN self



/*
 * Release system resources, unregister data dialog from work area
 */
METHOD DataDialog:destroy()

  /*
   * Ignore call to Destroy() when window is not created.
   */
   IF ::Status() == XBP_STAT_INIT
      RETURN self
   ENDIF

   ::writeData()
   ::hide()
   (::Area)->( DbCloseArea() )

  /*
   * Delete menu item in window menu
   */
   IF ! Empty( ::windowMenu )
      ::windowMenu:delItem( self ) //
      ::windowMenu := NIL
   ENDIF

  /*
   * Delete reference of data dialog and context menu
   */
   IF ! Empty( ::contextMenu )
      ::contextMenu:cargo := NIL
      ::contextMenu       := NIL
   ENDIF

  /*
   * Release system resources and set instance variables
   * to values corresponding to the :init() state
   */
   ::xbpDialog:destroy() 
   ::Area           :=  0
   ::appendMode     := .F.
   ::editControls   := {} 
   ::appendControls := {}
   ::newTitle       := {|obj| obj:getTitle() }

RETURN self



/*
 * Notify method:
 *   - Write data to fields prior to moving the record pointer
 *   - Read data from fields after moving the record pointer
 */
METHOD DataDialog:notify( nEvent, mp1, mp2 )

   IF nEvent <> xbeDBO_Notify
      RETURN self                         // ** RETURN **
   ENDIF

   DbSuspendNotifications()
   DO CASE
   CASE mp1 == DBO_MOVE_PROLOG            // record pointer is about
      ::writeData()                       // to be moved

   CASE mp1 == DBO_MOVE_DONE .OR. ;       // skip is done
        mp1 == DBO_GOBOTTOM  .OR. ;
        mp1 == DBO_GOTOP
      ::readData()

      DO CASE
      CASE Empty( ::contextMenu )
      CASE Eof()                          // Enable/disable items
         ::contextMenu:disableBottom()    // in context menu
         ::contextMenu:disableEof()       // depending on the
      CASE mp1 == DBO_GOBOTTOM            // position of the
         ::contextMenu:enableAll()        // record pointer
         ::contextMenu:disableBottom()
      CASE mp1 == DBO_GOTOP
         ::contextMenu:enableAll()
         ::contextMenu:disableTop()
      OTHERWISE
         ::contextMenu:enableAll()
      ENDCASE

      DO CASE
      CASE Eof()                               // Enable/disable items
         WinMenu():editMenu:disableBottom()    // in edit menu of the window
         WinMenu():editMenu:disableEof()       // depending on the
      CASE mp1 == DBO_GOBOTTOM                 // position of the
         WinMenu():editMenu:enableAll()        // record pointer
         WinMenu():editMenu:disableBottom()
      CASE mp1 == DBO_GOTOP
         WinMenu():editMenu:enableAll()
         WinMenu():editMenu:disableTop()
      OTHERWISE
         WinMenu():editMenu:enableAll()
      ENDCASE

   ENDCASE
   DbResetNotifications()

RETURN self



/*
 * Add an edit control to internal list
 */
METHOD DataDialog:addEditControl( oXbp )
   IF AScan( ::editControls, oXbp ) == 0
      AAdd(  ::editControls, oXbp )
      ::addKeyControl( oXbp )
   ENDIF
RETURN self



/*
 * Add an append control to internal list
 */
METHOD DataDialog:addAppendControl( oXbp )
   IF AScan( ::appendControls, oXbp ) == 0
      AAdd(  ::appendControls, oXbp )
      ::addKeyControl( oXbp )
   ENDIF
RETURN self



/*
 * Add a control to internal list for keyboard handling
 * Each XBP gets the same keyboard code block. It is evaluated
 * after the xbeP_Keyboard message and calls a customized key handler
 */
METHOD DataDialog:addKeyControl( oXbp )
   IF AScan( ::keyControls, oXbp ) == 0
      AAdd(  ::keyControls, oXbp )

      oXbp:keyBoard := ::keyboardBlock
   ENDIF
RETURN self



/*
 * Window has received focus. Select work area, check menu item
 * and set focus to current Edit control
 */
METHOD DataDialog:setDisplayFocus

   DbSelectArea( ::area )

   IF ::windowMenu <> NIL
      ::windowMenu:checkItem( self )
   ENDIF

   IF ::currentControl <> NIL
      SetAppFocus( ::currentControl )
   ENDIF
RETURN self



/*
 * Customized key handling for edit controls
 */
METHOD DataDialog:keyHandler( nKey, oXbp )
   LOCAL i:=AScan( ::keyControls, oXbp )

   DO CASE
   CASE i == 0
   CASE oXbp:isDerivedFrom( "XbpMLE" ) .OR. oXbp:isDerivedFrom( "XbpListbox" )
     // Return, PageUp, PageDown are processed by MLE and list box.
     // Therefore, the keys must be ignored.

   CASE nKey == xbeK_RETURN
      IF ++i > Len( ::keyControls )
         i := 1
      ENDIF
      DO WHILE ! ::keyControls[i]:isenabled()
         i ++
      ENDDO
      SetAppFocus( ::keyControls[i] )

   CASE nKey == xbeK_PGDN
      IF ! Eof()
         SKIP
         IF Eof()
            DbSuspendNotifications()
            SKIP -1                    // Avoid append situation
            DbResetNotifications()
            DbGobottom()               // Trigger notification
         ENDIF
      ENDIF

   CASE nKey == xbeK_PGUP
      IF ! Bof()
         SKIP -1
      ENDIF

   ENDCASE
RETURN self



/*
 * Read current record and transfer data to edit controls
 */
METHOD DataDialog:readData()
   LOCAL i, imax  := Len( ::editControls )

   FOR i:=1 TO imax                       // Transfer data from file
      ::editControls[i]:setData()         // to XBPs
   NEXT

   Eval( ::newTitle, self )               // Set new window title

   IF Eof()                               // enable/disable XBPs
      IF ! ::appendMode                   // active only during
         imax  := Len( ::appendControls ) // APPEND
         FOR i:=1 TO imax                 //
            ::appendControls[i]:enable()  // Hit Eof(), so
         NEXT                             // enable XBPs
      ENDIF
      ::appendMode := .T.
   ELSEIF ::appendMode                    // Record pointer was
      imax  := Len( ::appendControls )    // moved from Eof() to
      FOR i:=1 TO imax                    // an existing record.
         ::appendControls[i]:disable()    // Disable append-only
      NEXT                                // XBPs
      ::appendMode := .F.
   ENDIF

RETURN



/*
 * Write data from edit controls to file
 */
METHOD DataDialog:writeData()
   LOCAL i, imax
   LOCAL lLocked   := .F. , ;       // Is record locked?
         lAppend   := .F. , ;       // Is record new?
         aChanged  := {}  , ;       // XBPs containing changed data
         nOldArea  := Select()      // Current work area

   dbSelectArea( ::area )

   IF Eof()                         // Append a new record
      IF ::validateAll()            // Validate data first
         APPEND BLANK
         lAppend  := .T.
         aChanged := ::editControls // Test all possible changes
         lLocked  := ! NetErr()     // Implicit lock
      ELSE
         MsgBox("Invalid data")     // Do not write invalid data
         DbSelectArea( nOldArea )   // to new record
         RETURN .F.                 // *** RETURN ***
      ENDIF
   ELSE
      imax := Len( ::editControls ) // Find all XBPs containing
      FOR i:=1 TO imax              // changed data
         IF ::editControls[i]:changed
            AAdd( aChanged, ::editControls[i] )
         ENDIF
      NEXT

      IF Empty( aChanged )          // Nothing has changed, so
         DbSelectArea( nOldArea )   // no record lock necessary
         RETURN .T.                 // *** RETURN ***
      ENDIF

      lLocked := DbRLock( Recno() ) // Lock current record
   ENDIF

   IF ! lLocked
      MsgBox( "Record is currently locked" )
      DbSelectArea( nOldArea )      // Record lock failed
      RETURN .F.                    // *** RETURN ***
   ENDIF

   imax := Len( aChanged )          // Write access is necessary
   FOR i:=1 TO imax                 // only for changed data
      IF ! lAppend
         IF ! aChanged[i]:validate()
            aChanged[i]:undo()      // invalid data !
            LOOP                    // undo changes and validate
         ENDIF                      // next XBP
      ENDIF
      aChanged[i]:getData()         // Get data from XBP and
   NEXT                             // write to file

   DbCommit()                       // commit buffers
   DbRUnlock( Recno() )             // Release record lock

   IF ::appendMode                  // Disable append-only XBPs
      imax  := Len( ::appendControls ) // after APPEND
      FOR i:=1 TO imax
         ::appendControls[i]:disable()
      NEXT
      ::appendMode := .F.

      IF ! Empty( ::contextMenu )
         ::contextMenu:disableBottom()
         ::contextMenu:enableEof()
      ENDIF
      WinMenu():editMenu:disableBottom()
      WinMenu():editMenu:enableEof()
   ENDIF

   DbSelectArea( nOldArea )

RETURN .T.



/*
 * Validate data of all edit controls
 * This is necessary prior to appending a new record to the database
 */
METHOD DataDialog:validateAll()
   LOCAL i := 0, imax := Len( ::editControls )
   LOCAL lValid := .T.

   DO WHILE ++i <= imax .AND. lValid
      lValid := ::editControls[i]:validate()
   ENDDO

RETURN lValid



/*
 * Check whether an index value does *not* exist in an index
 */
METHOD DataDialog:isIndexUnique( nOrder, xIndexValue )
   LOCAL nOldOrder := OrdNumber()
   LOCAL nRecno    := Recno()
   LOCAL lUnique   := .F.

   DbSuspendNotifications()         // Suppress notification from DBO
                                    // to self during DbSeek() !!!
   OrdSetFocus( nOrder )

   lUnique := .NOT. DbSeek( xIndexValue )

   OrdSetFocus( nOldOrder )
   DbGoTo( nRecno )

   DbResetNotifications()

RETURN lUnique



/*
 * Class declaration for context menu
 */
CLASS DataDialogMenu FROM XbpMenu
   PROTECTED:
      VAR disabledItems
      METHOD disable
      METHOD enable

   EXPORTED:
      VAR disableTop
      VAR disableBottom
      VAR disableEof

      METHOD init, create
      METHOD goTop, goBottom, skip, seek, append, delete
      METHOD enableAll
      METHOD enableTop
      METHOD enableBottom
      METHOD enableEof
      METHOD disableTop
      METHOD disableBottom
      METHOD disableEof
ENDCLASS



/*
 * Initialize instance variables
 */
METHOD DataDialogMenu:init( oParent, aPresParam, lVisible )
   ::xbpMenu:init( oParent, aPresParam, lVisible )
   ::disabledItems := {}

   ::xbpMenu:title := "~Edit"

   // menu items are disabled after Bof() or GoTop()
   ::disableTop    := { 6, 9 }

   // menu items are disabled after GoBottom()
   ::disableBottom := { 7, 10 }

   // menu items are disabled at Eof()
   ::disableEof    := { 1, 2, 3 }

RETURN self



/*
 * Initialize menu items
 */
METHOD DataDialogMenu:create( oParent, aPresParam, lVisible )
   ::xbpMenu:create( oParent, aPresParam, lVisible )

   ::addItem( { "~New"      , {|| ::append()             } } )
   ::addItem( { "~Seek"     , {|| ::seek()               } } )
   ::addItem( { "~Delete"   , {|| ::delete()             } } )
   ::addItem( { "S~ave"     , {|| WinMenu():currentWin:writeData() } } )

   ::addItem( MENUITEM_SEPARATOR )

   ::addItem( { "~First"    , {|| ::goTop()    } } )
   ::addItem( { "~Last"     , {|| ::gobottom() } } )

   ::addItem( MENUITEM_SEPARATOR )

   ::addItem( { "~Previous" , {|| ::skip(-1) } } )

   ::addItem( { "Ne~xt"     , {|| ::skip( 1) } } )

RETURN self



/*
 * Some standard database operations
 */
METHOD DataDialogMenu:goTop
   (WinMenu():currentWin:area)->(DbGotop())
RETURN self

METHOD DataDialogMenu:skip( nSkip )
   LOCAL nOldArea := Select()
   LOCAL nArea    := WinMenu():currentWin:area

   DEFAULT nSkip TO 1

   DbSelectArea( nArea )
   DbSkip(nSkip)
   IF Eof()
      DbSuspendNotifications()
      SKIP -1                          // Avoid append situation
      DbResetNotifications()
      DbGobottom()                     // Trigger notification
   ENDIF

   DbSelectArea( nOldArea )
RETURN self

METHOD DataDialogMenu:goBottom
   (WinMenu():currentWin:area)->(DbGobottom())
RETURN self

METHOD DataDialogMenu:seek
   SeekRecord()
RETURN self

METHOD DataDialogMenu:append
   (WinMenu():currentWin:area)->(DbGoto(Lastrec()+1))
RETURN self

METHOD DataDialogMenu:delete
   DeleteRecord()
RETURN self



/*
 * Enable all menu items
 */
METHOD DataDialogMenu:enableAll
   LOCAL i, imax

   IF ! Empty( ::disabledItems )
      imax := Len( ::disabledItems )

      FOR i:=1 TO imax
         ::enableItem( ::disabledItems[i] )
      NEXT
     ::disabledItems := {}
   ENDIF

RETURN self



/*
 * Enable some menu items
 */
METHOD DataDialogMenu:enable( aItems )
   LOCAL i, imax, j, jmax

   IF ! Empty( aItems )
      imax := Len( aItems )
      jmax := Len( ::disabledItems )

      FOR i:=1 TO imax
         j := AScan( ::disabledItems, aItems[i] )

         IF j > 0
            ::enableItem( aItems[i] )
            ADel( ::disabledItems, j )
            jmax--
         ENDIF
      NEXT

      ASize( ::disabledItems, jmax )
   ENDIF

RETURN self



/*
 * Disable some menu items
 */
METHOD DataDialogMenu:disable( aItems )
   LOCAL i, imax

   IF ! Empty( aItems )
      imax := Len( aItems )

      FOR i:=1 TO imax
         IF AScan( ::disabledItems, aItems[i] ) == 0
            ::disableItem( aItems[i] )
            AAdd( ::disabledItems, aItems[i] )
         ENDIF
      NEXT
   ENDIF

RETURN self



/*
 * Enable menu items which may be selected after GO TOP
 */
METHOD DataDialogMenu:enableTop
RETURN ::enable( ::disableTop )



/*
 * Enable menu items which may be selected after GO BOTTOM
 */
METHOD DataDialogMenu:enableBottom
RETURN ::enable( ::disableBottom )



/*
 * Enable menu items which may be selected at Eof()
 */
METHOD DataDialogMenu:enableEof
RETURN ::enable( ::disableEof )



/*
 * Disable menu items which may not be selected after GO TOP
 * or at Bof()
 */
METHOD DataDialogMenu:disableTop
RETURN ::disable( ::disableTop )



/*
 * Disable menu items which may not be selected after GO BOTTOM
 */
METHOD DataDialogMenu:disableBottom
RETURN ::disable( ::disableBottom )



/*
 * Disable menu items which may not be selected at Eof()
 */
METHOD DataDialogMenu:disableEof
RETURN ::disable( ::disableEof )


