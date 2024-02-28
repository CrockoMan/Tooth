// DbRegisterClient()  - Registers an object to receive messages from a work area. 
///////////////////////////////////////////////////////////////////////////////
//
//  Function-oriented code created by the Xbase++ FormDesigner
//    Creation date: 14.04.2009 Time: 16:25:05
//
///////////////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ASCOM10.LIB" )




Function RefreshPacientWin(oBrowse)
LOCAL nRec:=RecNo()
//DbSetOrder(1)
//oBrowse:gotoRecord( nRec )
oBrowse:datalink:refresh()
Return NIL



PROCEDURE PacientEdit(oPrevDlg, oBrowse, lAppend)
   LOCAL nEvent, mp1, mp2, aSize, oWinMenu, aPos, oParent, oOwner
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}
	
	 lAppend=If(lAppend==NIL,.F.,lAppend)

	 aSize:= {693,371}
	 oDlg := CreateModalWindow( oPrevDlg, , aSize, "Информация о пациенте"  )


	 Select Pacient

//	 MsgBox( "RecNo = "+Str(oBrowse:getData()) )

	IF lAppend==.T.
		IF .not.NetAppend()
			oDlg:destroy()
			Return
		ENDIF
//		DbAppend()
		Replace Code with Val(LTrim(Str(Year(Date()),4,0))+""+LTrim(Str(Month(Date()),2,0))+""+LTrim(Str(Day(Date()),2,0))+""+LTrim(Str(Seconds(),9,0)))
		Go RecNo()
	Else
	  Go oBrowse:getData()
	  If .not.RecLock()
	  		oDlg:destroy()
	  		Return
		ENDIF
	ENDIF
	 


   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp := XbpStatic():new( drawingArea, , {12,48}, {660,288} )
   oXbp:caption := "Пациент"
   oXbp:clipSiblings := .T.
   oXbp:type := XBPSTATIC_TYPE_GROUPBOX
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {24,288}, {60,24} )
   oXbp:caption := "Фамилия"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,288}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 20
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( Pacient->Family ), Pacient->Family := x ) }
   oXbp:create():setData()
//   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), MsgBox( Pacient->Family ) } 
//   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), oBrowse:Stabilize() } 
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,252}, {60,24} )
   oXbp:caption := "Имя:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,252}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 20
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->NAME ), PACIENT->NAME := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,216}, {60,24} )
   oXbp:caption := "Отчество:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,216}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 20
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->OTCH ), PACIENT->OTCH := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,180}, {60,24} )
   oXbp:caption := "Адрес:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,180}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 40
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->ADRES ), PACIENT->ADRES := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,144}, {60,24} )
   oXbp:caption := "Город:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,144}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 20
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->TOWN ), PACIENT->TOWN := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,108}, {60,24} )
   oXbp:caption := "Телефон:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,108}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 30
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->PHONE ), PACIENT->PHONE := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( drawingArea, , {24,72}, {60,24} )
   oXbp:caption := "Телефон:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,72}, {288,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 30
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( PACIENT->FAX ), PACIENT->FAX := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpMLE():new( drawingArea, , {408,60}, {252,252}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, PACIENT->cNOTE, PACIENT->cNOTE := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshPacientWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {564,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Закрыть"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| oDlg:destroy(), (Pacient->( DbUnlock() ))  }
   AAdd ( aEditControls, oXbp )

//   oDlg:show()
   

RETURN