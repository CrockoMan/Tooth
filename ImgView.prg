//////////////////////////////////////////////////////////////////////
//
//  IMGVIEW.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2006. All rights reserved.         
//  
//  Contents:
//      The program implements three classes made up from different
//      Xbase Parts:
//   
//      o ImageView    -  Displays bitmaps or meta files in an
//                        XbpStatic object
//   
//      o DirSelector  -  Selects drives and directories
//                        (combination of XbpCombobox and XbpTreeView)
//   
//      o FileSelector -  Selects files matching a list of extensions
//                        (combination of XbpCombobox and XbpListbox)
//   
//////////////////////////////////////////////////////////////////////

#include "Appevent.ch"
#include "Directry.ch"
#include "Imgview.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"
#include "Resource.ch"


/*
 * We use user defined events
 */
#define  xbeDS_DirChanged           xbeP_User + 100
#define  xbeFS_FileMarked           xbeP_User + 101
#define  xbeFS_FileSelected         xbeP_User + 102

/*
 * This directive calculates a centered position
 */
#xtrans  CenterPos( <aSize>, <aRefSize> ) => ;
         { Int( (<aRefSize>\[1] - <aSize>\[1]) / 2 ) ;
         , Int( (<aRefSize>\[2] - <aSize>\[2]) / 2 ) }



/*
 * Appsys is empty...
Procedure AppSys
Return
 */



/*
 * Procedure for terminating the program

PROCEDURE AppQuit
   QUIT
RETURN
 */



/*
 * The Main procedure creates the Image view dialog

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL oDlg, drawingArea, aPos, aSize
   LOCAL oDirs, oFiles, oView
   LOCAL oRadio1, oRadio2

   aSize := {590,360}
   aPos  := CenterPos( aSize, AppDesktop():currentSize() )

   oDlg := XbpDialog():new( AppDesktop(), , aPos, aSize, , .F. )
   oDlg:taskList := .T.
   oDlg:maxButton := .F.
   oDlg:border   := XBPDLG_DLGBORDER
   oDlg:title    := "Image viewer"
   oDlg:close    := {|| AppQuit() }
   oDlg:create()
   SetAppWindow( oDlg )

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oDirs  := DirSelector():new ( drawingArea, , {12 ,12}, {168,312} )
   oDirs:tabStop := .T.
   oDirs:startDir := Upper( getenv("XPPRESOURCE") )
   oDirs:create()

   oFiles := FileSelector():new( drawingArea, , {192,12}, {168,312} )
   oFiles:tabStop := .T.
   oFiles:create()

   oFiles:setFilesTo( oDirs:curDir(), { "*.BMP", "*.EMF", "*.GIF", "*.JPG", "*.PNG", "*.BMP *.EMF *.GIF *.JPG *.PNG"  } )

   oView := ImageView():new( drawingArea, , {372,96}, {204,228} ):create()

   oRadio1 := XbpRadioButton():new( drawingArea, , {387,60}, {72,24} )
   oRadio1:caption   := "Scaled"
   oRadio1:tabStop   := .T.
   oRadio1:selection := oView:autoScale
   oRadio1:create()
   oRadio1:selected  := {|| oRadio2:SetData(.F.), oRadio1:SetData(.T.), ;
                            oView:autoScale := .T., oView:display() }

   oRadio2 := XbpRadioButton():new( drawingArea, , {487,60}, {72,24} )
   oRadio2:caption   := "Normal"
   oRadio2:tabStop   := .T.
   oRadio2:selection := ! oView:autoScale
   oRadio2:create()
   oRadio2:selected  := {|| oRadio1:SetData(.F.), oRadio2:SetData(.T.), ;
                            oView:autoScale := .F., oView:display() }

   oXbp := XbpStatic():new( drawingArea, , {372,48}, {204,2} )
   oXbp:type := XBPSTATIC_TYPE_RAISEDLINE
   oXbp:create()

   oXbp := XbpPushButton():new( drawingArea, , {377,12}, {72,24} )
   oXbp:caption  := "Full View"
   oXbp:tabStop  := .T.
   oXbp:create()
   oXbp:activate := {|| FullView( oView:file ) }

   oXbp := XbpPushButton():new( drawingArea, , {497,12}, {72,24} )
   oXbp:caption  := "Quit"
   oXbp:tabStop  := .T.
   oXbp:create()
   oXbp:activate := {|| AppQuit() }

   oDlg:show()
   SetAppFocus( oDirs:dirTree )

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )

      IF nEvent == xbeDS_DirChanged
         oFiles:setFilesTo( mp1 )

      ELSEIF nEvent == xbeFS_FileMarked
         oView:load( mp1 )
         oView:display()

      ELSE
          oXbp:handleEvent( nEvent, mp1, mp2 )
      ENDIF
   ENDDO
RETURN
 */


/*
 * This procedure displays an image file in a separate window
 */
PROCEDURE FullView( cFile, oPrevDlg )
   LOCAL oDlg, oImage, oPS, aSize, aPos
   LOCAL lBGClr := XBPSYSCLR_TRANSPARENT

  /*
   * Only bitmap and meta files are supported
   */
   IF cFile <> NIL            .AND. ;
     ( ".BMP" $ Upper( cFile ) .OR. ;
       ".EMF" $ Upper( cFile ) .OR. ;
       ".GIF" $ Upper( cFile ) .OR. ;
       ".JPG" $ Upper( cFile ) .OR. ;       
       ".PNG" $ Upper( cFile ) .OR. ;
       ".MET" $ Upper( cFile )      )

     /*
      * Create hidden dialog window
      */
//	 oDlg := CreateModalWindow( oPrevDlg, , , "Полноэкранный просмотр"  )
      oDlg := XbpDialog():new( AppDesktop(),,,{100,100} )
      oDlg:taskList   := .F.
      oDlg:visible    := .F.
//   	  oDlg:icon  := ICON_APPLICATION
//      oDlg:title      := cFile
      oDlg:title      := "Полноэкранный просмотр"
      oDlg:sizeRedraw := .T.
      oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
      oDlg:create()

     /*
      * Create a presentation space and connect it with the device
      * context of :drawingArea
      */
      oPS := XbpPresSpace():new():create( oDlg:drawingArea:winDevice() )

      IF ".BMP" $ Upper( cFile ) .OR. ;
         ".GIF" $ Upper( cFile ) .OR. ;
         ".JPG" $ Upper( cFile ) .OR. ;       
         ".PNG" $ Upper( cFile )

        /*
         * File contains a bitmap. Limit the window size to a range
         * between 16x16 pixel and the screen resolution
         */
         oImage   := XbpBitmap():new():create( oPS )
         oImage:loadFile( cFile )

         IF oImage:transparentClr <> GRA_CLR_INVALID
            lBGClr := XBPSYSCLR_DIALOGBACKGROUND
         ENDIF

         aSize    := { oImage:xSize, oImage:ySize }
       	 aSize[1] := Max( 16, Min( aSize[1], AppDeskTop():currentSize()[1]-20 ) )
       	 aSize[2] := Max( 16, Min( aSize[2], AppDeskTop():currentSize()[2]-30 ) )
         aSize    := oDlg:calcFrameRect( {0,0, aSize[1], aSize[2]} )

         oDlg:setSize( {aSize[3], aSize[4]} )

         /*
          * The window must react to xbeP_Paint to redraw the bitmap
          */
         oDlg:drawingarea:paint := {|x,y,obj| x:=obj:currentSize(), ;
                                     oImage:draw( oPS, {0, 0, x[1], x[2]}, ;
                                     {0, 0, oImage:xSize, oImage:ySize},,;
                                     GRA_BLT_BBO_IGNORE), Sleep(0.1) }
      ELSE
        /*
         * Display a meta file. It has no size definition for the image
         */
         oImage := XbpMetafile():new():create()
         oImage:load( cFile )
         aSize := { 600, 400 }
         oDlg:setSize( aSize )
         oDlg:drawingarea:paint := {|x,y,obj| x:=obj:currentSize(), ;
                                              oImage:draw( oPS, {0, 0, x[1], x[2]}),;
                                              Sleep(0.1) }
         lBGClr := XBPSYSCLR_DIALOGBACKGROUND
      ENDIF

     /*
      * Set the background color for the dialog's drawingarea.
      * Per default, the transparent color is used to avoid 
      * flicker during refreshs. For transparent images and 
      * metafiles, however, color gray is set instead, see above. 
      * This is done to prevent bits of the desktop from being
      * visible in transparent areas of the bitmap/metafile image.
      * Alternatively, transparency could be explicitly switched
      * off for bitmapped images.
      */
      oDlg:drawingArea:SetColorBG( lBGClr )

     /*
      * Display the window centered on the desktop
      */
      aPos:= CenterPos( oDlg:currentSize(), AppDesktop():currentSize() )
      oDlg:setPos( aPos )
      oDlg:show()
      SetAppFocus( oDlg )
   ENDIF
RETURN



/*
 * Class for displaying bitmaps or meta files
 */
CLASS ImageView FROM XbpStatic
   PROTECTED:
   VAR oFrame
   VAR oCanvas
   VAR oPS
   VAR oBitmap
   VAR oMetafile
   VAR nMode

   EXPORTED:
   VAR autoScale
   VAR file
   METHOD init, create, load, display
ENDCLASS



/*
 * Initialize the object. Self is the parent for two additional XbpStatic objects
 */
METHOD ImageView:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   ::xbpStatic:type := XBPSTATIC_TYPE_RAISEDRECT

   ::oFrame         := XbpStatic():new( self )
   ::oFrame:type    := XBPSTATIC_TYPE_RECESSEDRECT

   ::oCanvas        := XbpStatic():new( self )
   ::autoScale      := .T.
   ::nMode          := 0
   ::oPS            := XbpPresSpace():new()
RETURN self



/*
 * Request system resources. Self is the outer frame, ::oFrame is the
 * inner frame and ::oCanvas is used for displaying the image
 */
METHOD ImageView:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   LOCAL aAttr[ GRA_AA_COUNT ]

   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   aSize    := ::currentSize()
   aSize[1] -= 8
   aSize[2] -= 8

   ::oFrame:create(self,, {4,4}, aSize )

   aSize[1] -= 2
   aSize[2] -= 2

   ::oCanvas:create(::oFrame,, {1,1}, aSize )

  /*
   * Connect presentation space with device context of XbpStatic object
   */
   ::oPS:create( ::oCanvas:winDevice() )
   aAttr[ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   ::oPS:setAttrArea( aAttr )
   ::oCanvas:paint := {| aClip | ::display( aClip ) }

RETURN self



/*
 * Load an image file
 */
METHOD ImageView:load( cFile )
   LOCAL lSuccess := .F.

   IF Valtype( cFile ) <> "C" .OR. ! File( cFile )
      RETURN lSuccess
   ENDIF

   ::nMode := 0
   ::file  := ""

   IF ".BMP" $ Upper( cFile ) .OR. ;
      ".GIF" $ Upper( cFile ) .OR. ;
      ".JPG" $ Upper( cFile ) .OR. ;       
      ".PNG" $ Upper( cFile )

      IF ::oBitmap == NIL
         ::oBitmap := XbpBitmap():New():create( ::oPS )
      ENDIF

      IF ( lSuccess := ::oBitmap:loadFile( cFile ) )
         ::nMode := 1
         ::file := cFile
      ENDIF

   ELSEIF ".EMF" $ Upper( cFile ) .OR. ".MET" $ Upper( cFile )
      IF ::oMetafile == NIL
         ::oMetafile := XbpMetafile():New():create()
      ENDIF

      IF ( lSuccess := ::oMetafile:load( cFile ) )
         ::nMode := 2
         ::file := cFile
      ENDIF
   ENDIF

RETURN lSuccess



/*
 * Display the image
 */
METHOD ImageView:display( aClip )
   LOCAL lSuccess := .F.
   LOCAL aSize    := ::oCanvas:currentSize()
   LOCAL aTarget, aSource, nAspect

   /*
    * Prepare clipping path
    */
   DEFAULT aClip TO { 1, 1, aSize[1]-1, aSize[2]-1 }
   GraPathBegin( ::oPS )
   GraBox( ::oPS, { aClip[1]-1, aClip[2]-1 }, { aClip[3]+1, aClip[4]+1 }, GRA_OUTLINE )
   GraPathEnd( ::oPS )
   GraPathClip( ::oPS, .T. )

   GraBox( ::oPS, {0,0}, aSize, GRA_FILL )

   DO CASE
   CASE ::nMode == 1
     /*
      * A bitmap file is loaded
      */
      aSource := {0,0,::oBitmap:xSize,::oBitmap:ySize}
      aTarget := {1,1,aSize[1]-2,aSize[2]-2}

      IF ::autoScale
        /*
         * Bitmap is scaled to the size of ::oCanvas
         */
         nAspect    := aSource[3] / aSource[4]
         IF nAspect > 1
            aTarget[4] := aTarget[3] / nAspect
         ELSE
            aTarget[3] := aTarget[4] * nAspect
         ENDIF
      ELSE
         aTarget[3] := aSource[3]
         aTarget[4] := aSource[4]
      ENDIF


     /*
      * Center bitmap horizontally or vertically in ::oCanvas
      */
      IF aTarget[3] < aSize[1]-2
         nAspect := ( aSize[1]-2-aTarget[3] ) / 2
         aTarget[1] += nAspect
         aTarget[3] += nAspect
      ENDIF

      IF aTarget[4] < aSize[2]-2
         nAspect := ( aSize[2]-2-aTarget[4] ) / 2
         aTarget[2] += nAspect
         aTarget[4] += nAspect
      ENDIF

      ::oBitmap:draw( ::oPS, aTarget, aSource, , GRA_BLT_BBO_IGNORE )
      Sleep(1)

   CASE ::nMode == 2
     /*
      * A meta file is loaded
      */
      IF ::autoScale
         ::oMetafile:draw( ::oPS, XBPMETA_DRAW_SCALE )
      ELSE
         ::oMetafile:draw( ::oPS, XBPMETA_DRAW_DEFAULT )
      ENDIF

   ENDCASE

   GraPathClip( ::oPS, .F. )

RETURN lSuccess




/*
 * Class for selecting drives and directories
 */
CLASS DirSelector FROM XbpStatic
   PROTECTED:
   VAR aDrives
   VAR oItem

   EXPORTED:
   VAR startDir
   VAR dirTree
   VAR driveList

   METHOD init, create, fillTree, curDir
ENDCLASS



/*
 * Initialize the object
 */
METHOD DirSelector:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   ::xbpStatic:type    := XBPSTATIC_TYPE_RAISEDRECT

   ::dirTree           := XbpTreeView():new( self )
   ::dirTree:tabStop   := .T.
   ::dirTree:hasLines  := .T.
   ::dirTree:hasButtons:= .T.

   ::driveList         := XbpComboBox():new( self )
   ::driveList:tabStop := .T.
   ::driveList:type    := XBPCOMBO_DROPDOWNLIST

   ::startDir          := CurDrive() + ":\" + CurDir()
RETURN self



/*
 * Request system resources and fill tree view with
 * data from current directory
 */
METHOD DirSelector:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

   LOCAL nDrive

   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

   aSize := ::currentSize()

  /*
   * The tree view creates a user-defined event with the selected
   * directory attached as the first message parameter
   */
   ::dirTree:create  (,, {4, 30}, {aSize[1]-8, aSize[2]-30-4 } )
   ::dirTree:itemSelected := {|oItem| ::fillTree( oItem ) }
   ::dirTree:itemMarked   := {|oItem| PostAppEvent( xbeDS_DirChanged, oItem:getData() ) }


   IF At(";", ::startDir) <> 0
      ::startDir := Left( ::startDir, At(";", ::startDir) -1 )
   ENDIF


   ::dirTree:rootItem:setData( ::startDir )

   ::driveList:create(,, {4, 30-4-24-96}, {aSize[1]-8, 24+96 } )

   ::aDrives := AvailDrives()
   AEval( ::aDrives, {|c| ::driveList:xbpListBox:addItem( c+":" ) } )

   IF At( ":", ::startDir ) == 2
      nDrive := AScan( ::aDrives, Left(::startDir,1) )
   ELSE
      nDrive := AScan( ::aDrives, CurDrive() )
   ENDIF

   IF nDrive == 0
      MsgBox( "The drive specified for the image directory does not "  +;
              "exist! Please check your installation and ensure that " +;
              "XPPRESOURCE contains a valid path." )

      ::startDir := CurDrive() + ":\" + CurDir()
      ::dirTree:rootItem:setData( ::startDir )
      nDrive     := AScan( ::aDrives, CurDrive() )
   ENDIF
      
   ::driveList:xbpListBox:setData( { nDrive } )
   ::driveList:xbpListBox:itemSelected := {|| ::fillTree() }

   ::fillTree( ::dirTree:rootItem )
RETURN self



/*
 * Rebuild the tree view with data from the selected directory. Each
 * XbpTreeViewItem contains a character string of the directory
 * which is displayed by the item
 */
METHOD DirSelector:fillTree( oItem )
   LOCAL aItems    := ::dirTree:rootItem:getChildItems()
   LOCAL cDrive    := AllTrim( ::driveList:xbpSLE:getData() )
   LOCAL cDir, aDir, i, imax, nLen, oParent, oTemp, cTemp, oNewParent

   IF oItem <> NIL .AND. ::isVisible() .AND. Len( Directory( oItem:getData() + "\*.", "D" ) ) == 2
      Tone(1000)
      RETURN self
   ENDIF

   ::dirTree:LockUpdate( .T. )

   AEval( aItems, {|obj| ::dirTree:rootItem:delItem(obj) } )
   cDir    := cDrive

  /*
   * The tree view begins with the drive letter
   */
   oParent := ::dirTree:rootItem:addItem( cDrive, ICON_DRIVE, ICON_DRIVE, ICON_DRIVE,, cDrive )

   IF oItem == NIL
      oItem := oParent
   ENDIF

   ::oItem := oItem

  /*
   * An XbpTreeviewItem is created for each sub-directory
   */
   DO WHILE Upper(cDir) <> Upper(oItem:getData())
      cTemp := SubStr( oItem:getData(), Len(cDir)+2 )
      nLen := At( "\", cTemp ) - 1
      IF nLen < 0
          nLen := len(cTemp)
      ENDIF
      cTemp := SubStr( cTemp, 1, nLen)

      aDir := Directory( cDir + "\", "D" )
      imax := Len( aDir )
      FOR i:=1 TO imax
         IF "D" $ aDir[i,F_ATTR]        .AND. ;
            .NOT. aDir[i,F_NAME] == "." .AND. ;
            .NOT. aDir[i,F_NAME] == ".."
            oTemp := oParent:addItem( aDir[i,F_NAME]   , ;
                                      ICON_CLOSEDFOLDER, ;
                                      ICON_CLOSEDFOLDER, ;
                                      ICON_OPENFOLDER  , ;
                                      NIL              , ;
                                      cDir + "\" + aDir[i,F_NAME] )
            IF Upper( cDir + "\" + aDir[i,F_NAME] ) == Upper( SubStr( oItem:getData(), 1, Len(cDir + "\" + cTemp) ) )
               oNewParent := oTemp
            ENDIF
         ENDIF
      NEXT
      cDir += "\" + cTemp
      oParent:expand(.T.)
      IF oNewParent <> NIL
         oParent := oNewParent
      ENDIF
   ENDDO


  /*
   * Create XbpTreeViewItems for the files in the lowest subdirectory
   */
   aDir := Directory( cDir + "\", "D" )

   imax := Len( aDir )
   FOR i:=1 TO imax
      IF "D" $ aDir[i,F_ATTR]        .AND. ;
         .NOT. aDir[i,F_NAME] == "." .AND. ;
         .NOT. aDir[i,F_NAME] == ".."

         oParent:addItem( aDir[i,F_NAME]   , ;
                          ICON_CLOSEDFOLDER, ;
                          ICON_CLOSEDFOLDER, ;
                          ICON_OPENFOLDER  , ;
                          NIL              , ;
                          cDir + "\" + aDir[i,F_NAME] )
      ENDIF
   NEXT

   oParent:expand( .T. )
   ::dirTree:SetData( oParent )

   ::dirTree:LockUpdate( .F. )
   ::dirTree:InvalidateRect()

   PostAppEvent( xbeDS_DirChanged, oParent:getData() )   
RETURN self   



/*
 * Return the currently selected directory
 */
METHOD DirSelector:curDir
RETURN ::oItem:getData()



/*
 * Class for selecting files with a specific extension
 */
CLASS FileSelector FROM XbpStatic
   PROTECTED:
   VAR aFiles
   VAR aExtensions
   VAR oItem
   VAR cDir

   EXPORTED:
   VAR fileList
   VAR extList

   METHOD init, create, setFilesTo, fillFileList, fileMarked, fileSelected
ENDCLASS



/*
 * Initialize the object
 */
METHOD FileSelector:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   ::xbpStatic:type := XBPSTATIC_TYPE_RAISEDRECT

   ::fileList         := XbpListBox():new( self )
   ::fileList:tabStop := .T.
   ::extList          := XbpComboBox():new( self )
   ::extList:tabStop  := .T.
   ::extList:type     := XBPCOMBO_DROPDOWNLIST

RETURN self



/*
 * Request system resources
 */
METHOD FileSelector:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

   aSize := ::currentSize()
   ::fileList:create  (,, {4, 30}        , {aSize[1]-8, aSize[2]-30-4 } )
   ::fileList:itemMarked   := {|| ::fileMarked() }
   ::fileList:itemSelected := {|| ::fileSelected() }
   ::extList:create(,, {4, 30-4-24-96}, {aSize[1]-8, 24+96 } )

   ::aExtensions := { "*.*" }
   AEval( ::aExtensions, {|c| ::extList:xbpListBox:addItem( c ) } )

   ::extList:xbpListBox:setData( { 1 } )
   ::extList:xbpListBox:itemSelected := {|| ::fillFileList() }

RETURN self



/*
 * Define the directory and file extensiond for the file list
 */
METHOD FileSelector:setFilesTo( cDir, aExtensions )

   IF Valtype( aExtensions ) == "A"
      ::extList:xbpListBox:clear()
      AEval( aExtensions, {|c| ::extList:xbpListBox:addItem( c ) } )
      ::extList:xbpListBox:setData( { 1 } )
   ENDIF

   IF Valtype( cDir ) == "C"
      ::cDir := cDir
   ENDIF

   ::fillFileList()
RETURN self



/*
 * Fill listbox with names of matching files
 */
METHOD FileSelector:fillFileList()
   LOCAL i, imax, j, jmax, aExtensions := {}
   LOCAL nOffset, cExtension


  /*
   * Get the list of slected extensions from the combobox
   * A blank space is used as delimiter
   */
   cExtension := AllTrim( ::extList:xbpSLE:getData() )

   DO WHILE ! Empty( cExtension )
      i := At( " ", cExtension )
      IF i > 0
         AAdd( aExtensions, AllTrim( SubStr( cExtension, 1, i-1 ) ) )
         cExtension := AllTrim( SubStr( cExtension, i+1 ) )
      ELSE
         AAdd( aExtensions, cExtension )
         cExtension := ""
      ENDIF
   ENDDO

   jmax := Len( aExtensions )

   ::fileList:clear()
   ::aFiles  := {}


  /*
   * Fill the listbox with file names
   */
   FOR j:=1 TO jmax
      aDir    := Directory( ::cDir + "\" + aExtensions[j] )
      nOffset := Len( ::aFiles )
      imax    := Len( aDir )

      ASize( ::aFiles, nOffset + imax )

      FOR i:=1 TO imax
         ::aFiles[nOffset+i] := aDir[i,F_NAME]
         ::fileList:addItem( aDir[i,F_NAME] )
      NEXT
   NEXT

RETURN self



/*
 * Post a user-defined event along with the file name when a file is marked
 */
METHOD FileSelector:fileMarked
   LOCAL aItems := ::fileList:getData()

   IF ! Empty( aItems ) .AND. ! Empty( ::aFiles )
      PostAppEvent( xbeFS_FileMarked, ::cDir + "\" + ::aFiles[ aItems[1] ] )
   ENDIF
RETURN self



/*
 * Post a user-defined event along with the file name when a file is selected
 */
METHOD FileSelector:fileSelected
   LOCAL aItems := ::fileList:getData()

   IF ! Empty( aItems ) .AND. ! Empty( ::aFiles )
      PostAppEvent( xbeFS_FileSelected, ::cDir + "\" + ::aFiles[ aItems[1] ] )
   ENDIF

RETURN self



/*
 * This function returns an array filled with letters of all available drives
 * except floppy drives
 */
#define DRIVE_NOT_READY  21

FUNCTION AvailDrives()
   LOCAL cDrive := CurDrive()
   LOCAL aDrives:= { cDrive }
   LOCAL bError := ErrorBlock( {|oErr| DriveError( oErr, aDrives ) } )

   FOR i:=3 TO 26
      BEGIN SEQUENCE
        Curdrive( Chr(64+i) ) 
        CurDir()
        IF AScan( aDrives, CurDrive() ) == 0
           AAdd( aDrives, Curdrive() )
        ENDIF
      ENDSEQUENCE
   NEXT

   CurDrive( cDrive )

   ErrorBlock( bError )   
RETURN ASort( aDrives )



/*
 * Handle runtime error when a drive is not ready
 */
STATIC PROCEDURE DriveError( oError, aDrives )
   IF oError:osCode == DRIVE_NOT_READY
      AAdd( aDrives, oError:args[1] )
   ENDIF
   BREAK
RETURN
