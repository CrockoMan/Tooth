//////////////////////////////////////////////////////////////////////
//
//  QBROWSE2.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2003. All rights reserved.
//
//  Contents:
//      The sample demonstrates how to use the XbpQuickBrowse Object
//      for an incremental search within a database.
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Directry.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Font.ch"
#include "Xbp.ch"
#include "Resource.ch"




Function ColorBrowse(oDlg)
   LOCAL nEvent, mp1, mp2, oXbp, i, oBrowse, oSLE, oDialog, aPrevSize[2]


   LOCAL aFields :={ ;
         "PACIENT->Family" , ;
         "PACIENT->Name"   , ;
         "PACIENT->Otch"   , ;
         "PACIENT->Phone"  , ;
         "PACIENT->Fax"  , ;
         "PACIENT->Adres"         }

   LOCAL aHeader  := { ;
         "Фамилия" , ;
         "Имя"     , ;
         "Отчество", ;
         "Телефон" , ;
         "Телефон/Факс" , ;
         "Адрес"    }
//-----------------------------------------------------------------------------------------------------
   LOCAL  oStatic, drawingArea
   LOCAL aControls:={}, bKeyhandler
   LOCAL aSize[2], aPos[2], oWinMenu

   FIELD CUSTNO, MR_MRS, LASTNAME, FIRSTNAME, STREET, CITY, ZIP , ;
         PHONE , FAX   , NOTES   , BLOCKED  , TOTALSALES

   PRIVATE oBrowseWin

   Select Pacient
   
//   If RecCount()==0
//   		Append Blank
// 	 ENDIF
         
   If RecCount()>25
   		IF IsCorrectCopy()==.F.
   			 MsgBox( cLicenseText )
   			 Return NIL
   		ENDIF
   EndIf


//----------------------------------------------------------------------------------------------------------------------------------------------------
   oParent    := ;
   oOwner     := oDlg:drawingArea

   aSize[1]   = 795                // Ширина окна
   aSize[2]   = 500                // Высота окна
	 aPrevSize:=AppDesktop():currentSize()
	 
   oWinMenu   := oDlg:childFromName( WindowMenu():ID )
   aPos       := oWinMenu:nextWinPos( aSize )

   oDlg               := XbpDialog():new( oParent, oOwner, aPos, aSize,, .T. )
   oDlg:title         := "Пациенты"
   oDlg:icon  := ICON_APPLICATION
   oDlg:taskList      := ( oParent == AppDeskTop() )
   oDlg:moveWithOwner := ( oOwner <> oParent .AND. .NOT. lModal )
   oDlg:clipSiblings  := .T.
   oDlg:cargo         := {}
   oDlg:MinSize				:= aSize
   oDlg:MaxSize				:= {1300,aPrevSize[2]}
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( "8.Helv" )
   oBrowseWin = oDlg
   

   oDlg:show()
//   oDlg:close := {|mp1,mp2,obj| CloseChildWindow( oDlg ) }
   oDlg:close := {|mp1,mp2,obj| CloseWindow( oDlg ) }
//   oDlg:close := {|mp1,mp2,obj| oDlg:destroy() }

   SetAppFocus( oDlg )
   If aDlg[2]==NIL
                   aDlg[2]=oDlg
   EndIf

//----------------------------------------------------------------------------------------------------------------------------------------------------


   drawingArea := oDlg:drawingArea
//-----------------------------------------------------------------------------------------------------


//   SET DEFAULT TO ..\..\DATA\

//   USE CUSTOMER  New

   /*
    * Create application window, browser and SLE
    */
   oDialog            := oDlg
//   oDialog            := GuiStdDialog( "Address Data" )
//   SetAppWindow( oDialog )


   /*
    * The SLE is required as entry field for the incremental
    * search. It is related to the column of the browser in
    * which the cursor is positioned.
    * Whenever a key is pressed, the procedure IncrementalSearch()
    * is called.
    */
   oSLE               := XbpSLE():new( oDialog:drawingArea,,{0,aSize[2]-60},{aSize[1]-20,20}, , .t. )
   oSLE:keyboard      := {|nKey,mp2,obj| IncrementalSearch( nKey, obj, ;
                                                            oBrowse, aFields ) }
   oSLE:create()


//   aSize[1]   *= 0.98                // Ширина окна
//   aSize[2]=aSize[2]-70                // Высота окна

 	 oBrowse            := XbpColoredQuickBrowse():new( oDialog:drawingArea,,{0,50},{aSize[1]-20,aSize[2]-110}, , .F. )
   oBrowse:style      := XBP_STYLE_SYSTEMDEFAULT
   oBrowse:cursorMode := XBPBRW_CURSOR_CELL
   oBrowse:dataLink   := DacPagedDataStore():new( Alias(), aFields )
   oBrowse:create()


   Resize( oSLE, oBrowse, oDialog:drawingArea:currentSize() )

   /*
    * Copy strings to header of columns.
    */
   oBrowse:setHeader( aHeader )

   /*
    * Respecting the SLE, the browser fills the entire window. The cache
    * size of DacPagedDataStore must be set according to the rows of the
    * browser.
    */
   oDialog:drawingArea:resize := {|mp1,mp2,obj| Resize( oSLE, oBrowse, mp2 ) }


      oOwner     := oDlg:drawingArea

      oXbp := XbpPushButton():new( drawingArea,, {15,10}, {90,30} )
      oXbp:caption := "Назад"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:activate := {|| oBrowse:up(.T.) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {105,10}, {90,30} )
      oXbp:caption := "Вперед"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:activate :=  {|| oBrowse:down(.T.)  }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {255,10}, {90,30} )
      oXbp:caption := "Изменить"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate := {|| PacientEdit(oDlg, oBrowse) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {345,10}, {90,30} )
      oXbp:caption := "Добавить"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate := {|| PacientEdit(oDlg, oBrowse, .T.) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {435,10}, {90,30} )
      oXbp:caption := "Удалить"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate :=  {|| DeletePacientRecord(oDlg, oBrowse) }
      AAdd( aControls, oXbp )


      oXbp := XbpPushButton():new( drawingArea,, {675,10}, {90,30} )
      oXbp:caption := "История"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate :=  {|| Mouth(oDlg, oBrowse, oBrowse:getData() )}
      AAdd( aControls, oXbp )


      bKeyHandler := {|nKey,x,obj| DlgKeyhandler( nKey, obj, aControls, oDlg ) }
      AEval( aControls, {|o| o:keyBoard := bKeyHandler } )


   oSLE:show()
   oBrowse:show()
   oDialog:show()
   SetAppFocus( oSLE )

Return Nil


PROCEDURE DeletePacientRecord(oPrevDlg, oBrowse)
LOCAL RecNo

	RefreshPacientWin(oBrowse)
	Select Pacient
	Go oBrowse:getData()
  RefreshPacientWin(oBrowse)

  If ConfirmBox( oPrevDlg, "Удалить эту запись ?"+Chr(13)+Chr(10)+LTrim(RTrim(Family))+" "+LTrim(RTrim(Name))+" "+LTrim(RTrim(Otch))+" ", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES



				Select History
				Seek Pacient->Code
				Do While Code==Pacient->Code
	 					DeleteFile(cImageDir+Foto1)
	 					DeleteFile(cImageDir+Foto2)
	 					DeleteFile(cImageDir+Foto3)
	 					DeleteFile(cImageDir+Foto4)
	 					DeleteFile(cImageDir+Foto5)
	 					DeleteFile(cImageDir+Foto6)
	 					RecLock()
	 				 	DbDelete()
	 				 	DbUnlock()
					 	Skip
				EndDo
				Select Pacient
				RecLock()
	 			DbDelete()
	 			DbUnlock()
				oBrowse:up()
				RefreshPacientWin(oBrowse)

  			MsgBox( If(Deleted(),"Вся история по пациенту успешно удалена","Ошибка удаления истории") , "" ) 
//				Set Order to 0
//				Set Order to 1
	ENDIF
Return


/*
 * The procedure uses DbLocate()/DbContinue() and the
 * edit buffer of the SLE to perform an incremental search
 * in the database.
 * Pressing the RETURN key continues the search.
 */
PROCEDURE IncrementalSearch( nKey, oSle, oBrowse, aFields )
   LOCAL cValue  := Upper( AllTrim( oSle:editBuffer() ) )
   LOCAL cField  := aFields[oBrowse:colPos]
   LOCAL cSearch := '{||Upper(Left(%1,%2))=="%3"}'
   LOCAL nRecno, bGoto

   IF nKey == xbeK_RETURN
      DbContinue()
   ELSE
      // The first column contains the right adjustet customer number.
/*      IF oBrowse:colPos == 1
         cValue := Right(Space(6) + cValue, 6)
      ENDIF
*/
      cSearch := StrTran( cSearch, "%1", cField )
      cSearch := StrTran( cSearch, "%2", LTrim( Str(Len(cValue)) ) )
      cSearch := StrTran( cSearch, "%3", cValue )
      DbLocate( &(cSearch) )
   ENDIF

   IF Found()
      // Position the browser cursor on the found
      // record.
      oBrowse:gotoRecord( Recno() )
   ELSE
      Tone(1000)
   ENDIF
RETURN



/*
 * Resize of the SLE and the Browser to fit the window size.
 */
PROCEDURE Resize( oSLE, oBrowse, aSize )
   oSLE:setPos(  { 0, aSize[2]-24 } )
   oSLE:setSize( { aSize[1], 24} )

//   oBrowse:setSize( {aSize[1], aSize[2]-24} )
   oBrowse:setSize( {aSize[1], aSize[2]-70} )
   oBrowse:dataLink:setAbsolutePageSize( oBrowse:rowCount )
RETURN



/*
 * Create invisible standard dialog
 */
FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg
   LOCAL aSize := {600,400}
   LOCAL aPos  := CenterPos( aSize, AppDesktop():currentSize() )

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( ,, aPos, aSize,, .F. )
   oDlg:icon     := 1
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )

RETURN oDlg


FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }


/*
 * This derived class paints the even records in a different
 * color than the odd rows.
 */
CLASS XbpColoredQuickBrowse FROM XbpQuickBrowse
EXPORTED:
   VAR EvenRow
   INLINE METHOD Init( p1, p2, p3, p4, p5, p6 )
      ::XbpQuickbrowse:Init( p1, p2, p3, p4, p5, p6 )
      ::EvenRow := .T.
   RETURN self

   INLINE METHOD DrawRow( nRow, nCol, aValues, nCols, lRedraw )
      LOCAL aValue, j
      LOCAL lRGBLightBlue := {201,222,245}, lHilite := .F.

      /*
       * Draw the row
       */
      ::XbpQuickBrowse:DrawRow( nRow, nCol, aValues, nCols, lRedraw )
      aValue := ::dataLink:GetRowData( nRow )

      /*
       * Give the row a color in case it has a valid record id,
       * and depending whether we colour odd or even rows.
       */
      IF ::EvenRow
         IF nRow % 2 == 0
            lHilite := .T.
         ENDIF
      ELSE
         IF nRow % 2 == 1
            lHilite := .T.
         ENDIF
      ENDIF
      IF lHilite .AND. ValType(aValues[1]) != "U"
         FOR j := 1 TO ::ColCount
            ::dataArea:SetCellColor( nRow, j, NIL, GraMakeRGBColor( lRGBLightBlue ), lRedraw )
         NEXT
      ENDIF
   RETURN .T.

   INLINE METHOD ScrollDown( nScroll )
      IF nScroll % 2 == 1
         ::EvenRow := !::EvenRow
      ENDIF
   RETURN ::XbpQuickbrowse:ScrollDown( nScroll )

   INLINE METHOD ScrollUp( nScroll )
      IF nScroll % 2 == 1
         ::EvenRow := !::EvenRow
      ENDIF
   RETURN ::XbpQuickbrowse:ScrollUp( nScroll )

   INLINE METHOD RefreshAll( p1, p2 )
      ::EvenRow := .T.
   RETURN ::XbpQuickbrowse:RefreshAll( p1, p2 )

   INLINE METHOD RefreshRows( p1, p2 )
      ::EvenRow := .T.
   RETURN ::XbpQuickbrowse:RefreshRows( p1, p2 )

   INLINE METHOD GoTop()
      ::EvenRow := .T.
      ::XbpQuickbrowse:GoTop()
   RETURN ::ForceStable()

   INLINE METHOD GoBottom()
      ::EvenRow := .T.
      ::XbpQuickbrowse:GoBottom()
   RETURN ::ForceStable()


ENDCLASS