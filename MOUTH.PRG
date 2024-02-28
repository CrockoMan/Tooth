///////////////////////////////////////////////////////////////////////////////
//
//  Function-oriented code created by the Xbase++ FormDesigner
//    Creation date: 15.04.2009 Time: 11:53:44
//
///////////////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ASCOM10.LIB" )






PROCEDURE Mouth(oPrevDlg, oBrowse, nRec)
   LOCAL nEvent, mp1, mp2, aPos, aSize, nPos, Sel:=Select(), Rec:=RecNo(), RecHis, nTotalTooth:=0
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}, aXbp[32]
	 LOCAL oParent, oPs, nSegId := 0, aTooth:={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}



	 aPos :={400,450}
	 aSize:={465,252}

	 oDlg := CreateModalWindow( oPrevDlg, , aSize, "Ткни в зуб"  )
	 
	 Select Pacient
	 Go oBrowse:GetData()

	 Select History
	 RecHis:=RecNo()
	 Seek Pacient->CODE
	 If Found()
	 		Do While Code==Pacient->CODE
	 	  		aTooth[NumTooth]=1
	 	  		nTotalTooth=nTotalTooth+1
	 				Skip
	 		EndDo
	 EndIf
	 Go RecHis
	 Select(Sel)
	 Go Rec
	 
   If nTotalTooth>10
   		IF IsCorrectCopy()==.F.
   			 oDlg:destroy()
   			 MsgBox( cLicenseText )
   			 Return
   		ENDIF
   EndIf
	 

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )


   oXbp := XbpPushButton():new( drawingArea, , {24,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "8"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 1) }
   If aTooth[1]==1
	   	aXbp[1]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 1, aXbp[1])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 1) }
   ENDIF
   AAdd ( aEditControls, oXbp )


   oXbp := XbpPushButton():new( drawingArea, , {48,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "7"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 2) }
   If aTooth[2]==1
	   	aXbp[2]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 2, aXbp[2])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 2) }
   ENDIF
	 AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {72,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "6"
   oXbp:tabStop := .T.
   oXbp:create()
   //oXbp:activate := {|| ToothHistory(oDlg, nRec, 3) }
   If aTooth[3]==1
	   	aXbp[3]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 3, aXbp[3])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 3) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {96,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "5"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 4) }
   If aTooth[4]==1
	   	aXbp[4]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 4, aXbp[4])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 4) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {120,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "4"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 5) }
   If aTooth[5]==1
	   	aXbp[5]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 5, aXbp[4])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 5) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {144,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "3"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 6) }
   If aTooth[6]==1
	   	aXbp[6]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 6, aXbp[6])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 6) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {168,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "2"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 7) }
   If aTooth[7]==1
	   	aXbp[7]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 7, aXbp[7])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 7) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {192,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "1"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 8) }
   If aTooth[8]==1
	   	aXbp[8]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 8, aXbp[8])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 8) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {228,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "1"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 9) }
   If aTooth[9]==1
	   	aXbp[9]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 9, aXbp[9])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 9) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {252,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "2"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| ToothHistory(oDlg, nRec, 10) }
   If aTooth[10]==1
	   	aXbp[10]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 10, aXbp[10])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 10) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {276,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "3"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 11) }
   If aTooth[11]==1
	   	aXbp[11]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 11, aXbp[11])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 11) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {300,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "4"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 12) }
   If aTooth[12]==1
	   	aXbp[12]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 12, aXbp[12])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 12) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {324,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "5"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 13) }
   If aTooth[13]==1
	   	aXbp[13]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 13, aXbp[13])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 13) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {348,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "6"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 14) }
   If aTooth[14]==1
	   	aXbp[14]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 14, aXbp[14])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 14) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {372,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "7"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 15) }
   If aTooth[15]==1
	   	aXbp[15]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 15, aXbp[15])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 15) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {396,144}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "8"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 16) }
   If aTooth[16]==1
	   	aXbp[16]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 16, aXbp[16])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 16) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {24,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "8"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 17) }
   If aTooth[17]==1
	   	aXbp[17]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 17, aXbp[17])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 17) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {48,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "7"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 18) }
   If aTooth[18]==1
	   	aXbp[18]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 18, aXbp[18])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 18) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {72,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "6"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 19) }
   If aTooth[19]==1
	   	aXbp[19]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 19, aXbp[19])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 19) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {96,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "5"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 20) }
   If aTooth[20]==1
	   	aXbp[20]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 20, aXbp[20])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 20) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {120,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "4"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 21) }
   If aTooth[21]==1
	   	aXbp[21]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 21, aXbp[21])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 21) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {144,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "3"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 22) }
   If aTooth[22]==1
	   	aXbp[22]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 22, aXbp[22])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 22) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {168,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "2"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 23) }
   If aTooth[23]==1
	   	aXbp[23]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 23, aXbp[23])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 23) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {192,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "1"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 24) }
   If aTooth[24]==1
	   	aXbp[24]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 24, aXbp[24])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 24) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {228,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "1"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 25) }
   If aTooth[25]==1
	   	aXbp[25]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 25, aXbp[25])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 25) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {252,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "2"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 26) }
   If aTooth[26]==1
	   	aXbp[26]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 26, aXbp[26])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 26) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {276,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "3"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 27) }
   If aTooth[27]==1
	   	aXbp[27]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 27, aXbp[27])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 27) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {300,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "4"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 28) }
   If aTooth[28]==1
	   	aXbp[28]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 28, aXbp[28])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 28) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {324,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "5"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 29) }
   If aTooth[29]==1
	   	aXbp[29]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 29, aXbp[29])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 29) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {348,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "6"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 30) }
   If aTooth[30]==1
	   	aXbp[30]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 30, aXbp[30])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 30) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {372,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "7"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 31) }
   If aTooth[31]==1
	   	aXbp[31]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 31, aXbp[31])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 31) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {396,72}, {24,48}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "8"
   oXbp:tabStop := .T.
   oXbp:create()
//   oXbp:activate := {|| ToothHistory(oDlg, nRec, 32) }
   If aTooth[32]==1
	   	aXbp[32]:=oXbp
   		oXbp:cargo    := .F.
   		oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, DrawCircle( obj ), ToothHistory(oDlg, nRec, 32, aXbp[32])  }
   		oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   		oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }
   		DrawCircle( oXbp )
   Else
   		oXbp:activate := {|| ToothHistory(oDlg, nRec, 32) }
   ENDIF
   AAdd ( aEditControls, oXbp )

   oXbp := XbpPushButton():new( drawingArea, , {168,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   oXbp:caption := "Закрыть"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| oDlg:destroy()}
   AAdd ( aEditControls, oXbp )


//   oDlg:show()
//   oDlg:showModal()


   SetAppFocus( oXbp )

Return




/*
 * Display circle in push button with changing colors (red/green)
 */
//STATIC PROCEDURE DrawCircle( oXbp )
Procedure DrawCircle( oXbp, nClr )
   LOCAL oPS, aSize, nX, nY, nRadius, aAttr[ GRA_AA_COUNT ], aOld
   
  /*
   * Calculate center point and radius of the circle from the 
   * size of the pushbutton
   */
   aSize   := oXbp:currentSize()
   nX      := aSize[1] / 2
   nY      := aSize[2] / 6
   nRadius := Min( nX, nY ) * 0.8

  /*
   * Fill color for circle
   */
//   aAttr [ GRA_AA_COLOR ] := IIf( oXbp:cargo, GRA_CLR_GREEN, GRA_CLR_RED )
//	 If nClr==NIL
//   		aAttr [ GRA_AA_COLOR ] := IIf( oXbp:cargo,  GRA_CLR_RED, GRA_CLR_BACKGROUND)
   		aAttr [ GRA_AA_COLOR ] := GRA_CLR_RED
//	 Else
//	 		aAttr [ GRA_AA_COLOR ] := nClr
//	 EndIf

  /*
   * Get MicroPS, set color attributes, draw circle and clean up
   */
   oPS  := oXbp:lockPS()
   aOld := oPS:setAttrArea( aAttr )

   GraArc( oPS, {nX , nY}, nRadius,,,, GRA_OUTLINEFILL )

   oPS:setAttrArea( aOld )
   oXbp:unlockPS( oPS )

RETURN
