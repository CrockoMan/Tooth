#include "Appevent.ch"
#include "Directry.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Font.ch"
#include "Xbp.ch"

#PRAGMA LIBRARY( "ASCOM10.LIB" )


Function ToothHistory(oPrevDlg, nPacientRec, nTooth, oPrevXbp)
   LOCAL nEvent, mp1, mp2, oXbp, i, oBrowse, oSLE, oDialog, oDlg, HistoryRec



   LOCAL aFields := { "Date", "Summa" }

   LOCAL aHeader := { "Дата", "Стоимость" }
//-----------------------------------------------------------------------------------------------------
   LOCAL  oStatic
   LOCAL  bKeyhandler
   LOCAL aSize[2], aPos[2], oWinMenu
	 Local   drawingArea, aControls:={}

   aSize[1]   = 350                // Ширина окна
   aSize[2]   = 500                // Высота окна

//   PRIVATE oBrowseWin

	 oDlg := CreateModalWindow( oPrevDlg, {0,0}, aSize, "История зуба"  )

	 Select Pacient
   Go nPacientRec

   Select tHistory
   Zap
   
   GO TOP
   Select History
   Seek Pacient->Code
   Do While Code==Pacient->Code
   	
   		IF NumTooth==nTooth
   		
   				HistoryRec=RecNo()
   				Select tHistory
   				Append Blank
   		
   				Replace tHistory->Rec        with HistoryRec
   				Replace tHistory->Code       with History->Code
   				Replace tHistory->Date       with History->Date
   				Replace tHistory->Numtooth   with History->Numtooth
   				Replace tHistory->ToothArray with History->ToothArray
   				Replace tHistory->Summa			 with History->Summa
   				Replace tHistory->Foto1	 		 with History->Foto1
   				Replace tHistory->Foto2	 		 with History->Foto2
   				Replace tHistory->Foto3	 		 with History->Foto3
   				Replace tHistory->Foto4	 		 with History->Foto4
   				Replace tHistory->Foto5	 		 with History->Foto5
   				Replace tHistory->Foto6	 		 with History->Foto6
   				Replace tHistory->cWork			 with History->cWork
   		
   				Select History
   				
   		ENDIF
   		
   		Skip
 	 EndDo

// DrawCircle( oPrevXbp, GRA_CLR_RED )
 	 
 	 Select tHistory
 	 Go Top

   If RecCount()>2
   		IF IsCorrectCopy()==.F.
   			 Zap
   			 MsgBox( cLicenseText )
   			 oDlg:destroy()
   			 Return NIL
   		ENDIF
   EndIf


//----------------------------------------------------------------------------------------------------------------------------------------------------

//	 oDlg := CreateModalWindow( oPrevDlg, {0,0}, aSize, "История зуба"  )
   drawingArea := oDlg:drawingArea
//-----------------------------------------------------------------------------------------------------


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
//   oSLE               := XbpSLE():new( oDialog:drawingArea,,{0,aSize[2]-60},{aSize[1]-20,20}, , .t. )
//   oSLE:keyboard      := {|nKey,mp2,obj| IncrementalSearch( nKey, obj, ;
//                                                            oBrowse, aFields ) }
//   oSLE:create()


//   aSize[1]   *= 0.98                // Ширина окна
//   aSize[2]=aSize[2]-70                // Высота окна

// 	 oBrowse            := XbpColoredQuickBrowse():new( oDialog:drawingArea,,{0,50},{195,aSize[2]-110}, , .F. )
 	 oBrowse            := XbpColoredQuickBrowse():new( oDialog:drawingArea,,{0,0},{195,aSize[2]-35}, , .F. )
   oBrowse:style      := XBP_STYLE_SYSTEMDEFAULT
   oBrowse:cursorMode := XBPBRW_CURSOR_CELL
   oBrowse:dataLink   := DacPagedDataStore():new( Alias(), aFields )
   oBrowse:create()


//   Resize( oSLE, oBrowse, oDialog:drawingArea:currentSize() )

   /*
    * Copy strings to header of columns.
    */
   oBrowse:setHeader( aHeader )

   /*
    * Respecting the SLE, the browser fills the entire window. The cache
    * size of DacPagedDataStore must be set according to the rows of the
    * browser.
    */
//   oDialog:drawingArea:resize := {|mp1,mp2,obj| Resize( oSLE, oBrowse, mp2 ) }


      oOwner     := oDlg:drawingArea
/*
      oXbp := XbpPushButton():new( drawingArea,, {15,10}, {90,30} )
      oXbp:caption := "Показать"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:activate := {|| History(oBrowse, oDlg, 1,nPacientRec, nTooth) }
      AAdd( aControls, oXbp )
*/

      oXbp := XbpPushButton():new( drawingArea,, {220,400}, {90,30} )
      oXbp:caption := "Изменить"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:activate :=  {|| History(oBrowse, oDlg, 2,nPacientRec, nTooth)  }
//      oXbp:activate :=  {|| oBrowse:datalink:refresh()  }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {220,300}, {90,30} )
      oXbp:caption := "Добавить"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate := {|| History(oBrowse, oDlg, 3, nPacientRec, nTooth) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {220,200}, {90,30} )
      oXbp:caption := "Удалить"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:activate := {|| DeleteHistoryRecord(oDlg, oBrowse) }
      AAdd( aControls, oXbp )


      oXbp := XbpPushButton():new( drawingArea,, {220,10}, {90,30} )
      oXbp:caption := "Закрыть"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
//      oXbp:setName( ID_BTN_NEXT )
//      oXbp:activate :=  {|| RefreshPacientWin(oBrowse)}
      oXbp:activate :=  {|| CloseToothHistory(oDlg, oPrevXbp)}
//      oXbp:activate :=  {|| PacientHistory()}
      AAdd( aControls, oXbp )


      bKeyHandler := {|nKey,x,obj| DlgKeyhandler( nKey, obj, aControls, oDlg ) }
      AEval( aControls, {|o| o:keyBoard := bKeyHandler } )


//   oSLE:show()
   oBrowse:show()
//   oDialog:show()
   SetAppFocus( oBrowse )

Return Nil


PROCEDURE DeleteHistoryRecord(oPrevDlg, oBrowse)
LOCAL RecNo

	Select tHistory
	Go oBrowse:getData()
  RefreshHistorytWin(oBrowse)

//  MsgBox( Alias()+" "+Str(oBrowse:getData()) , "RecNo" ) 

  If ConfirmBox( oPrevDlg, "Удалить эту запись от "+DtoC(tHistory->Date)+" "+LTrim(Str(Summa))+"руб.", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES


				
	 			DeleteFile(cImageDir+Foto1)
	 			DeleteFile(cImageDir+Foto2)
	 			DeleteFile(cImageDir+Foto3)
	 			DeleteFile(cImageDir+Foto4)
	 			DeleteFile(cImageDir+Foto5)
	 			DeleteFile(cImageDir+Foto6)
	 			DbDelete()

  			MsgBox( If(Deleted(),"Запись успешно удалена","Ошибка удаления") ,   "" ) 
//			  Skip
//				oBrowse:up()
				RefreshHistorytWin(oBrowse)
	ENDIF
Return



Function CloseToothHistory(oDlg, oXbp)
LOCAL lDeleted, nRec:=1

	 Select tHistory
   GO nRec
   Do While !EOF()

   		IF tHistory->Code!=0

					lDeleted:=Deleted()
					Select History

   				IF tHistory->Rec!=0
							Go tHistory->Rec
							RecLock()
   				ELSE
   						IF lDeleted==.F.
   							 NetAppend()
//		   						Append Blank
		   				ENDIF
   				ENDIF

   				IF  lDeleted==.F.

							Replace History->Code       with tHistory->Code
							Replace History->Date       with tHistory->Date
							Replace History->Numtooth   with tHistory->Numtooth
							Replace History->ToothArray with tHistory->ToothArray
							Replace History->Summa			with tHistory->Summa
							Replace History->Foto1  	  with tHistory->Foto1
							Replace History->Foto2  	  with tHistory->Foto2
							Replace History->Foto3  	  with tHistory->Foto3
							Replace History->Foto4  	  with tHistory->Foto4
							Replace History->Foto5  	  with tHistory->Foto5
							Replace History->Foto6  	  with tHistory->Foto6
							Replace History->cWork			with tHistory->cWork

					ELSE


	 					DeleteFile(cImageDir+tHistory->Foto1)
	 					DeleteFile(cImageDir+tHistory->Foto2)
	 					DeleteFile(cImageDir+tHistory->Foto3)
	 					DeleteFile(cImageDir+tHistory->Foto4)
	 					DeleteFile(cImageDir+tHistory->Foto5)
	 					DeleteFile(cImageDir+tHistory->Foto6)

						IF tHistory->Rec!=0
								DbDelete()
						ENDIF

   				ENDIF
   				DbUnLock()

   		ENDIF
   		

   		Select tHistory
   		nRec:=nRec+1
   		Go nRec
//   		Skip
   EndDo
   Zap
   Select History

If oDlg!=NIL   
	 oDlg:destroy()
EndIf
Return NIL



Function ReplaceToothArray(nPos, cVal)
If nPos==1
			THISTORY->ToothArray := cVal+SubStr(THISTORY->ToothArray,2,29)
ELSE
	If nPos==30
			THISTORY->ToothArray := SubStr(THISTORY->ToothArray,1,29)+cVal
	ELSE
			THISTORY->ToothArray := SubStr(THISTORY->ToothArray,1,nPos-1)+cVal+SubStr(THISTORY->ToothArray,nPos+1,29-nPos)
	ENDIF
ENDIF			
Return NIL

Function TestToothArray(nPos)
LOCAL lValue:=.F.
If Val(SubStr(THISTORY->ToothArray,nPos,1))==1
		lValue:=.T.
EndIf
Return lValue



Function RefreshHistorytWin(oBrowse)
LOCAL nRec
Select tHistory
oBrowse:datalink:refresh()
Return NIL



Function History(oBrowse, oPrevDlg, nMode, nPacientRec, nTooth, oPrevXbp)
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}
   LOCAL oView1,oView2,oView3,oView4


//	 oDlg := CreateModalWindow( oPrevDlg, {450,255}, {1016,480}, "Выполненная работа"  )
//	 oDlg := CreateModalWindow( oPrevDlg, , {1016,480}, "Выполненная работа"  )
	 oDlg := CreateModalWindow( oPrevDlg, , {990,480}, "Выполненная работа"  )

   If nMode==1 .or. nMode==2
   		Select tHistory
   		Go oBrowse:getData()
	 EndIf
	 
	 
   If nMode==3					// Режим добавления записи
   		Select Pacient
   		Go nPacientRec
   		
   		Select tHistory
   		DbAppend()
   		
   		Replace THISTORY->Code  		 with Pacient->Code
   		Replace THISTORY->NumTooth 	 with nTooth
   		Replace THISTORY->ToothArray with Replicate("0",30)
   		Replace THISTORY->Date  		 with Date()
   EndIf
   


   If Len(THISTORY->ToothArray)!=30
   		THISTORY->ToothArray=Replicate("0",30)
   ENDIF

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )
   oDlg:show()
   oDlg:close := {|mp1,mp2,obj| oDlg:destroy() }

   oXbp := XbpStatic():new( drawingArea, , {168,396}, {36,24} )
   oXbp:caption := "Дата:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()


   oXbp := XbpSLE():new( drawingArea, , {216,396}, {72,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 10
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, DtoC( THISTORY->DATE ), THISTORY->DATE := CtoD(x) ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshHistorytWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )
   SetAppFocus( oXbp )

   oXbp := XbpStatic():new( drawingArea, , {300,396}, {60,24} )
   oXbp:caption := "Сумма:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {372,396}, {120,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 10
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, LTrim(Transform( THISTORY->SUMMA, '@N' )), THISTORY->SUMMA := Val(x) ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshHistorytWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )

   oXbp := XbpMLE():new( drawingArea, , {180,72}, {312,300}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:ignoreTab := .T.
   oXbp:tabStop := .T.
//   oXbp:dataLink := {|x| IIf( PCOUNT()==0, THISTORY->CWORK, THISTORY->CWORK := x ) }
   oXbp:dataLink := {|x| IIf( x==NIL, THISTORY->CWORK, THISTORY->CWORK := x ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData() } 
   AAdd ( aEditControls, oXbp )


   oXbp := XbpCheckBox():new( drawingArea, , {36,396}, {96,24} )
   oXbp:caption := "Анастезия"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(1)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(1, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,372}, {96,24} )
   oXbp:caption := "Матрица"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(2)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(2, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,348}, {96,24} )
   oXbp:caption := "Пломба"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(3)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(3, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,324}, {108,24} )
   oXbp:caption := "Ретракция"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(4)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(4, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,300}, {96,24} )
   oXbp:caption := "Канал №1"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(5)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(5, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,276}, {96,24} )
   oXbp:caption := "Канал №2"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(6)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(6, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,252}, {96,24} )
   oXbp:caption := "Канал №3"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(7)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(7, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,228}, {96,24} )
   oXbp:caption := "Прокладка"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(8)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(8, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,204}, {96,24} )
   oXbp:caption := "Штифт"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(9)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(9, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,180}, {96,24} )
   oXbp:caption := "Наращивание"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(10)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(10, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,156}, {96,24} )
   oXbp:caption := "Культя"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(11)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(11, IIf( oChk:getData(), "1", "0" )) }

   oXbp := XbpCheckBox():new( drawingArea, , {36,132}, {96,24} )
   oXbp:caption := "УЗ Чистка"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(12)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(12, IIf( oChk:getData(), "1", "0" )) }


   oXbp := XbpCheckBox():new( drawingArea, , {36,108}, {96,24} )
   oXbp:caption := "Протез"
   oXbp:tabStop := .T.
   oXbp:selection:= TestToothArray(13)
   oXbp:create()
   oXbp:selected := {| mp1, mp2, oChk| ReplaceToothArray(13, IIf( oChk:getData(), "1", "0" )) }

	 		oView1:= ImageView():new( drawingArea, , {500,280}, {210,150} ):create()
 	 		oView1:autoScale := .T.
	 		oView1:Load(cImageDir+Foto1)
	 		oView1:display()

	 		oView2:= ImageView():new( drawingArea, , {750,280}, {210,150} ):create()
 	 		oView2:autoScale := .T.
	 		oView2:Load(cImageDir+Foto2)
	 		oView2:display()

	 		oView3:= ImageView():new( drawingArea, , {500,100}, {210,150} ):create()
 	 		oView3:autoScale := .T.
	 		oView3:Load(cImageDir+Foto3)
	 		oView3:display()

	 		oView4:= ImageView():new( drawingArea, , {750,100}, {210,150} ):create()
 	 		oView4:autoScale := .T.
	 		oView4:Load(cImageDir+Foto4)
	 		oView4:display()


   oXbp := XbpPushButton():new( drawingArea, , {420,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Закрыть"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {||  oDlg:destroy() }
   
   oXbp := XbpPushButton():new( drawingArea, , {605,258}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Фото №1"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {||  OpenFileDialog(1, oView1) }

   oXbp := XbpPushButton():new( drawingArea, , {500,258}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Полный экран"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| FullView( cImageDir+Foto1, oDlg )}

   oXbp := XbpPushButton():new( drawingArea, , {855,258}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Фото №2"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {||  OpenFileDialog(2, oView2) }

   oXbp := XbpPushButton():new( drawingArea, , {750,258}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Полный экран"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| FullView( cImageDir+Foto2 )}
   
   oXbp := XbpPushButton():new( drawingArea, , {605,78}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Фото №3"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {||  OpenFileDialog(3, oView3)}

   oXbp := XbpPushButton():new( drawingArea, , {500,78}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Полный экран"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| FullView( cImageDir+Foto3 )}

   oXbp := XbpPushButton():new( drawingArea, , {750,78}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Полный экран"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| FullView( cImageDir+Foto4 )}

   oXbp := XbpPushButton():new( drawingArea, , {855,78}, {105,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Фото №4"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {||  OpenFileDialog(4, oView4)}
Return NIL




Function OpenFileDialog(nFotoNum, oPreView)
   LOCAL oDlg := XbpFiledialog():New(), cFile, cFoto:="Foto", Sel:=Select(), cPrevFile, cRndName

	 Select tHistory
	 
	 cFoto:="tHistory->"+cFoto+AllTrim(Str(nFotoNum))
   oDlg:Title := "Фото"
   oDlg:fileFilters	 :=  { {"Фотографии Jpg Bmp Gif Png", "*.JPG;*.BMP;*.GIF;*.PNG"} }
   oDlg:Create()
//   cFile := oDlg:Open(&cFoto)
   cFile := oDlg:Open()
//   cFile:=If(cFile==NIL,"", MyUpper(cFile))
   cFile:=If(cFile==NIL,"", cFile)
//	 MsgBox( cImageDir+" "+cFile+" "+Str(AtNum(MyUpper(cImageDir),cFile)))
	 
//		oPreView:Load(cFile)
//		oPreView:display()

//   If !empty(cFile) .and. (AtNum(cImageDir, cFile))==0 
		IF !File(cImageDir+SubStr(cFile,atNum("\",cFile)))
   		SmallCopy(cFile, cImageDir+SubStr(cFile,atNum("\",cFile)+1) )
		  cFile:=SubStr(cFile,atNum("\",cFile)+1)
   	ELSE
//   		SmallCopy(cFile, cImageDir+SubStr(AllTrim(Str(Month(Date())))+AllTrim(Str(Day(Date())))+AllTrim(Str(Seconds())),1,8)+SubStr(cFile,atNum(".",cFile)) )
		  cRndName:=SubStr(AllTrim(Str(Month(Date())))+AllTrim(Str(Day(Date())))+AllTrim(Str(Seconds())),1,8)
		  Do While File(cImageDir+cRndName+SubStr(cFile,atNum(".",cFile))) == .T.
		  		cRndName:=SubStr(AllTrim(Str(Val(cRndName)+1)),1,8)
		  ENDDO
   		SmallCopy(cFile, cImageDir+cRndName+SubStr(cFile,atNum(".",cFile)) )
   		cFile:=cRndName+SubStr(cFile,atNum(".",cFile))
//   		MsgBox( cRndName+SubStr(cFile,atNum(".",cFile)) )
//		  cFile:=SubStr(cFile,atNum("\",cFile)+1)
   	ENDIF
/*   		
		IF File(cFile) .and. Upper(cFile)!=Upper( cImageDir+SubStr(cFile,atNum("\",cFile)+1) )
   		Copy File (cFile) to (cImageDir+SubStr(cFile,atNum("\",cFile)+1) )
   	ENDIF
*/
   		
//   		MsgBox( cFile+Chr(13)+Chr(10)+cImageDir+SubStr(cFile,atNum("\",cFile)+1) )
//		  cFile:=MyUpper(cImageDir+SubStr(cFile,atNum("\",cFile)+1))
//		  cFile:=MyUpper(SubStr(cFile,atNum("\",cFile)+1))
//		  cFile:=SubStr(cFile,atNum("\",cFile)+1)
// 	 EndIf

	 		DO Case
	 				Case nFotoNum==1
	 						Replace Foto1 with cFile

	 				Case nFotoNum==2
	 						Replace Foto2 with cFile

	 				Case nFotoNum==3
	 						Replace Foto3 with cFile

	 				Case nFotoNum==4
	 						Replace Foto4 with cFile
	 		ENDCASE

		oPreView:Load(cImageDir+cFile)
		oPreView:display()

	Select(Sel)

Return NIL



function smallcopy(source,dest,row,col,pos)
LOCAL TMPFILE,Desc,Remain:=0,Size,Total,Buffer,Kol_Kl:=0
TmpFile:=Fopen(Source,0)
Total := FSEEK(TmpFile, 0, 2)
FSEEK(TmpFile, 0,0)
Desc:=FCreate(dest)
DO While .T.
   IF (Remain+(512*20))<TOTAL
      SIZE:=512*20
   ELSE
      SIZE:=Total-Remain
      Buffer:=Space(SIZE)
      FRead(TmpFile, @buffer, SIZE)
      FWrite(Desc,buffer)
      exit
   ENDIF
   buffer = space(SIZE)
   Remain:=Remain+SIZE
   FRead(TmpFile, @buffer, SIZE)
   FWrite(Desc,buffer)
EndDo
FClose(Desc)
FClose(TmpFile)
RETURN NIL



FUNCTION MyUPPER(cStr)
Local i,cNewStr:=""
For i=1 To Len(cStr)
    DO CASE
       CASE ASC(Substr(cStr,i,1))>=160.and.ASC(Substr(cStr,i,1))<176
            cNewStr=cNewStr+Chr(ASC(Substr(cStr,i,1))-32)
       CASE ASC(Substr(cStr,i,1))>=224.and.ASC(Substr(cStr,i,1))<240
            cNewStr=cNewStr+Chr(ASC(Substr(cStr,i,1))-80)
       CASE ASC(Substr(cStr,i,1))>=97.and.ASC(Substr(cStr,i,1))<123
            cNewStr=cNewStr+Chr(ASC(Substr(cStr,i,1))-32)
       OTHERWISE
            cNewStr=cNewStr+Substr(cStr,i,1)
    ENDCASE
Next
Return cNewStr





Function GetPassword(oPrevDlg, lShowString)
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}

	 lShowString:=If(lShowString==NIL, ".T.", lShowString)
	 oDlg := CreateModalWindow( oPrevDlg, , {327,139}, "Введите пароль"  )

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )
   oDlg:show()
   oDlg:close := {|mp1,mp2,obj| oDlg:destroy() }

//   oDlg := XbpDialog():new( AppDesktop(), , {600,669}, {327,139}, , .F.)
//   oDlg:title := "Введите пароль"
//   oDlg:create()
//   drawingArea := oDlg:drawingArea
//   drawingArea:setColorFG( GRA_CLR_BACKGROUND )
//   drawingArea:setFontCompoundName( "12.Courier" )

/*
   oXbp := XbpSLE():new( drawingArea, , {216,396}, {72,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 10
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, DtoC( THISTORY->DATE ), THISTORY->DATE := CtoD(x) ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData(), RefreshHistorytWin(oBrowse) } 
   AAdd ( aEditControls, oXbp )
*/

   oXbp := XbpSLE():new( drawingArea, , {108,60}, {168,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp:bufferLength := 20
   oXbp:tabStop := .T.
   oXbp:unReadable := lShowString
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, cPassWord, cPassWord := LTrim(RTrim(x)) ) }
   oXbp:create():setData()
   oXbp:killInputFocus :=  { |x,y,oSLE| oSLE:getData() } 
   AAdd ( aEditControls, oXbp )
   SetAppFocus(oXbp)

   oXbp := XbpStatic():new( drawingArea, , {12,60}, {84,24}, { { XBP_PP_FGCLR, GRA_CLR_BLACK } } )
   oXbp:caption := "Пароль"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpPushButton():new( drawingArea, , {108,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Готово"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| NIL }
   oXbp:activate := {||  oDlg:destroy(), 	 MsgBox( "Пароль ="+cPassWord)  }
   AAdd ( aEditControls, oXbp )

//   oDlg:show()
Return NIL