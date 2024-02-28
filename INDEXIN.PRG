STATIC first_time:=.T., save_curs, NULLC, save_screen, color, rowstart
STATIC valperstep:=0, pospaint:=0

#include "Appevent.ch"
#include "Gra.ch"
#include "Xbp.ch"


#include "fileio.ch"
#include "setcurs.ch"
#define NTX_EXPR_SIZE 256
#define NTX_KEY_START 22







function make_ind(oblast,database,ind1,pole1,ind2,pole2,monopol,Is_Close)
LOCAL old_scr,oldscr,retlogval:=.T.	//,win:=win_save(),clr:=setcolor()
LOCAL Pcount:=0
//work(maxrow(),0)
Monopol:=IF(Monopol==NIL,.F.,Monopol)
Is_Close:=If(Is_Close==NIL,.T.,Is_Close)
pcount=if(oblast!=NIL,pcount+1,pcount)
pcount=if(database!=NIL,pcount+1,pcount)
pcount=if(ind1!=NIL,pcount+1,pcount)
pcount=if(pole1!=NIL,pcount+1,pcount)
pcount=if(ind2!=NIL,pcount+1,pcount)
pcount=if(pole2!=NIL,pcount+1,pcount)
IF FILE(DataBase)
   DO CASE
      CASE pcount=6
           select &oblast
           IF NetUse(database,Monopol,0.1,,.f.)
              IF !NetErr()
                 indexing(pole1,ind1,database)
                 indexing(pole2,ind2,database)
                 IF Is_Close
                    Use
                 ENDIF
              ELSE
                 IF NetUse(database,,0.1,,.f.)
                    CLOSE
//                    inkey(0.1)
                 ELSE
                    DO WHILE NetUse(database,,0.1,,.f.)
                       CLOSE
                    ENDDO
                 ENDIF
                 retlogval:=.F.
              ENDIF
           ENDIF
      CASE pcount=4
           select &oblast
           IF NetUse(database,Monopol,0.1,,.f.)
              IF !NetErr()
                 indexing(pole1,ind1,database)
                 IF Is_Close
                    Use
                 ENDIF
              ELSE
                 IF NetUse(database,,0.1,,.f.)
                    CLOSE
//                    inkey(0.1)
                 ELSE
                    DO WHILE NetUse(database,,0.1,,.f.)
                       CLOSE
                    ENDDO
                 ENDIF
                 retlogval:=.F.
              ENDIF
           ENDIF
   ENDCASE
   IF Is_Close
      IF retlogval:=.T.
         close
      ENDIF
   ENDIF
ELSE
//   al_box({'"'+MYupper(database)+'" ionoonaoao. Ainnoaiiaeoa oaeeu n aenea'})
ENDIF
//setcolor(clr)
//win_rest(win)
return retlogval






FUNCTION Indexing(fieldname,filename,database)
Local aSizeDesktop,oProgress,oDlg,aPos, oMainWindow:=SetAppWindow()
private Myfield:=fieldname,file:=filename,dbffile:=database
IF fieldname#NIL.and.filename#NIL
        do case
                case valtype(&Myfield)='C'
                        NULLC:=""
//                        IF Upper(AllTrim(Prot))=="NODISP"
//                           index on &Myfield to (filename)
//                        ELSE
      											oDlg          := XbpDialog():new(AppDesktop(),SetAppWindow(),,,,.F.)
      											aSizeDesktop    := oMainWindow:currentSize()
//      											aSizeDesktop    := SetAppWindow():currentSize()
      											oDlg:create(oMainWindow ,, {100,50}, {aSizeDeskTop[1]-200,90} ) 
//      											oDlg:create(SetAppWindow() ,, {100,50}, {aSizeDeskTop[1]-200,90} ) 
      											oDlg:title    := "Обработано" 
   													oDlg:SysMenu	 := .F.
   													oDlg:Configure()
      											oDlg:Show()
      											aSizeDesktop    := oDlg:currentSize()
      											aPos						:= oDlg:CurrentPos()
  													oProgress := ProgressBar():new(oDlg ,, {5,10}, {aSizeDeskTop[1]-18,30},, .F. )	// Progress Bar Create
   												 oProgress:create()
   												 oProgress:minimum := 1
   												 oProgress:maximum := RecCount()
   												 	
                           DbCreateIndex(FileName,Myfield,{|| oProgress:increment(),&MyField})
   												 oProgress:destroy()																							// Progress Bar Destroy
   												 oDlg:Destroy()
//                        ENDIF
                        close indexes
                case valtype(&Myfield)='D'
                        index on &Myfield to (filename)
                        close indexes
                case valtype(&Myfield)='L'
                        index on &Myfield to (filename)
                        close indexes
                case valtype(&Myfield)='N'
                        NULLC:=0
//                        IF Upper(AllTrim(Prot))=="NODISP"
//                           index on &Myfield to (filename)
//                        ELSE
      											oDlg          := XbpDialog():new(AppDesktop(),SetAppWindow(),,,,.F.)
      											aSizeDesktop    := oMainWindow:currentSize()
      											oDlg:create(oMainWindow ,, {100,50}, {aSizeDeskTop[1]-200,90} ) 
      											oDlg:title    := "Обработано" 
   													oDlg:SysMenu	 := .F.
   													oDlg:Configure()
      											oDlg:Show()
      											aSizeDesktop    := oDlg:currentSize()
      											aPos						:= oDlg:CurrentPos()
  													oProgress := ProgressBar():new(oDlg ,, {5,10}, {aSizeDeskTop[1]-18,30},, .F. )	// Progress Bar Create
  														
   												 oProgress:create()
   												 oProgress:minimum := 1
   												 oProgress:maximum := RecCount()
                           DbCreateIndex(FileName,Myfield,{|| oProgress:increment(),&MyField})
   												 oProgress:destroy()																							// Progress Bar Destroy
   												 oDlg:Destroy()

//                        ENDIF
                        close indexes
        endcase
ENDIF
RETURN NIL






/*
FUNCTION ip_start
         rowstart:=if(maxrow()<25,16,int(maxrow()/2))
         save_curs = set(_SET_CURSOR, SC_NONE)
         save_screen:=win_save(rowstart,12,rowstart+6,maxcol())
         color:=setcolor()
         first_time = .T.
//         @ 10, 10 SAY "Indexing     % complete"
         obrabot('Сортировка файла "'+MYupper(substr(dbffile,rat("\",dbffile)+1))+'"', rowstart)
         valperstep=39/reccount()
         pospaint=0
RETURN NIL


FUNCTION ip_disp()
//Do Case
//   Case Type=="C"
//        Nal:=""
//   Case Type=="N"
//        Nal:=0
//EndCase
         IF first_time
            first_time = .F.
         ELSE
//            @ 10, 19 SAY str(recno() / reccount() * 100, 4)
//            inkey(0)
                pospaint=pospaint+valperstep
                IF Int(PosPaint)==Round(PosPaint,1)
                   colorwin(rowstart+2,21,rowstart+2,21+pospaint,'gr+/gr+')
                ENDIF
         ENDIF
RETURN NULLC


FUNCTION ip_end(f_name)
LOCAL buff, ntx_expr, ntx_handle, where_at
         set(_SET_CURSOR, save_curs)
         ntx_expr = space(NTX_EXPR_SIZE)
//         ntx_handle = fopen(path_copy+f_name, FO_READWRITE)
         ntx_handle = fopen(f_name, FO_READWRITE)
         if ntx_handle=-1
            al_box({"Ошибка открытия файла "+path_copy+f_name})
         endif
         * Ищем начало индексного выражения...
         fseek(ntx_handle, NTX_KEY_START, FS_SET)
         fread(ntx_handle, @ntx_expr, NTX_EXPR_SIZE)
         * Удаляем заключительные пробелы
         ntx_expr = trim(ntx_expr)
         * Ищем + ip_disp()
         where_at = at("+ip_disp()", ntx_expr)
         * Записываем его обратно без + ip_disp(), заменяя его
         * на chr(0) для терминирования выражения...
         * Ищем начало индексного выражения
         fseek(ntx_handle, NTX_KEY_START, FS_SET)
         fwrite(ntx_handle, pad(substr(ntx_expr, 1, where_at - 1) +;
                                chr(0), NTX_EXPR_SIZE))
         fclose(ntx_handle)
         win_rest(save_screen)
         setcolor(color)
RETURN NIL
*/








/*
 * Class for visualizing a progressing process
 */
CLASS ProgressBar FROM XbpStatic, Signal, Thread
   PROTECTED:
   VAR    squares, every, _current
   METHOD displayHoriz, displayVert

   EXPORTED:
   VAR           maxWait, color 
   VAR           minimum, current, maximum
   ASSIGN METHOD minimum, current, maximum 

   METHOD init   , create , destroy , setSize
   METHOD display, execute, increment
ENDCLASS



/*
 * Initialize the object and set Thread:interval() to zero. This way,
 * method :execute() is automatically repeated.
 */
METHOD ProgressBar:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::Thread:init()
   ::Thread:setInterval( 0 )
   ::Thread:atStart := {|| ::xbpStatic:show() }

   ::Signal:init()

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpStatic:type  := XBPSTATIC_TYPE_RAISEDBOX
   ::xbpStatic:paint := {|| ::display() }

   ::color   := GRA_CLR_BLUE
   ::squares := 1
   ::current := 0
   ::every   := 1
   ::maxWait := 100
   ::minimum := 0
   ::maximum := 100
RETURN



/*
 * Request system resources; calculate the number or squares which
 * fit into the progress bar and start the thread.
 */
METHOD ProgressBar:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   aSize     := ::currentSize()
   ::squares := Int( aSize[1] / (aSize[2]+1) )
   ::start()

RETURN



/*
 * Stop the thread of ProgressBar and release system resources
 */
METHOD ProgressBar:destroy

  /*
   * Turn off automatic repetition of :execute().
   */
   ::thread:setInterVal( NIL )


   IF ::thread:active
     /*
      * Thread is still active.
      * Signal thread to leave its :wait() state
      */
      ::signal()
   ENDIF


   IF ThreadObject() <> ::thread
     /*
      * The current thread is not the thread of ProgressBar (self).
      * Therefore, the current thread must wait for the end of self:thread
      */
      ::thread:synchronize(0)
   ENDIF


  /*
   * System resources are released when self:thread has terminated
   */
   ::xbpStatic:destroy()

RETURN self



/*
 * Change the size of ProgressBar. Before the size is changed,
 * everything is overpainted with the background color.
 */
METHOD ProgressBar:setSize( aSize )
   LOCAL oPS, aAttr[ GRA_AA_COUNT ], _aSize

   oPS       := ::lockPS()
   _aSize    := ::currentSize()
   _aSize[1] -= 2
   _aSize[2] -= 2
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1,1}, _aSize, GRA_FILL )
   ::unlockPS( oPS )
   ::xbpStatic:setSize( aSize )

RETURN self



/*
 * ASSIGN method for :minimum
 */
METHOD ProgressBar:minimum( nMinimum )

   IF ::maximum <> NIL .AND. nMinimum > ::maximum
      ::minimum := ::maximum
      ::maximum := nMinimum
   ELSE
      ::minimum := nMinimum  
   ENDIF

   ::current := ::minimum 
RETURN self



/*
 * ASSIGN method for :current
 */
METHOD ProgressBar:current( nCurrent )

   IF Valtype( nCurrent ) == "N"
      ::current := nCurrent

      IF Valtype( ::maximum ) + Valtype( ::minimum ) == "NN"
         ::every    := Int( ( ::maximum - ::minimum ) / ::squares )
         ::_current := ::current
      ENDIF
   ENDIF

RETURN ::current



/*
 * ASSIGN method for :maximum
 */
METHOD ProgressBar:maximum( nMaximum )

   IF ::minimum <> NIL .AND. nMaximum < ::minimum
      ::maximum := ::minimum
      ::minimum := nMaximum
   ELSE
      ::maximum := nMaximum  
   ENDIF

   ::current := ::minimum 

RETURN self



/*
 * Increment the current value and refresh display if necessary
 */
METHOD ProgressBar:increment( nIncrement )

   IF Valtype( nIncrement ) <> "N"
      nIncrement := 1
   ENDIF

  /*
   * While a progress is displayed, PROTECTED VAR :_current is incremented
   * to avoid the overhead of the ASSIGN method :current()
   */
   ::_current += nIncrement
   

   IF Int( ::_current % ::every ) == 0
     /*
      * This interrupts the ::wait( ::maxWait ) method in :execute().
      * The progress bar is then refreshed immediately in its own thread.
      * Since the display occurs in a separate thread, it does not
      * slow down the actual process whose progress is visualized.
      * Index creation, for example, does not update the display,
      * but only signals self:thread
      */
      ::signal()
   ENDIF
RETURN



/*
 * Refresh progress bar automatically every ::maxWait / 100 seconds
 * This method runs in self:thread and is automatically restarted
 * due to :setInterval(0)
 */
METHOD ProgressBar:execute

   ::wait( ::maxWait )
   ::display()

RETURN self



/*
 * Visualize the current state of a process
 */
METHOD ProgressBar:display
   LOCAL oPS   := ::lockPS()
   LOCAL aSize := ::currentSize()
   LOCAL aAttr [ GRA_AA_COUNT ]

   aSize[1] -= 2    
   aSize[2] -= 2

   IF aSize[1] > aSize[2]
      ::displayHoriz( oPS, aSize, aAttr )
   ELSE
      ::displayVert ( oPS, aSize, aAttr )
   ENDIF

   ::unlockPS( oPS )
RETURN self



/*
 * Display squares from left to right (horizontal display)
 */
METHOD ProgressBar:displayHoriz( oPS, aSize, aAttr )
   LOCAL nX, aPos1, aPos2, nCenter

  /*
   * Max. x coordinate for squares
   */
   nX := aSize[1] * ::_current / ( ::maximum - ::minimum )
   nX := Min( nX, aSize[1] )

  /*
   * Fill the area to the right of the squares with background color
   */
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1+nX,1}, {aSize[1],aSize[2]}, GRA_FILL )

  /*
   * Define fill color for squares
   */
   aAttr [ GRA_AA_COLOR ] := ::color
   GraSetAttrArea( oPS, aAttr )

  /*
   * Calculate position for leftmost square (starting position)
   */
   aPos1     := { 2, 2 }   
   ::squares := Int( aSize[1] / (aSize[2]+1) )
   nCenter   := 2 + ( aSize[1] - (::squares * (aSize[2]+1)) ) / 2
   aPos1[1]  := Max( 2, nCenter )
   aPos2     := { aPos1[1]+aSize[2]-2 , aSize[2]-1 }   

  /*
   * Draw the squares
   */
   DO WHILE aPos2[1] < nX
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
      aPos1[1] += aSize[2]+1
      aPos2[1] += aSize[2]+1
   ENDDO

   IF aPos2[1] < aSize[1]
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
   ENDIF

RETURN self



/*
 * Display squares from bottom to top (vertical display)
 */
METHOD ProgressBar:displayVert( oPS, aSize, aAttr )
   LOCAL nY, aPos1, aPos2, nCenter

  /*
   * Max. y coordinate for squares
   */
   nY := aSize[2] * ::_current / ( ::maximum - ::minimum )
   nY := Min( nY, aSize[2] )

  /*
   * Fill the area above the squares with background color
   */
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1,nY}, {aSize[1],aSize[2]}, GRA_FILL )

  /*
   * Define fill color for squares
   */
   aAttr [ GRA_AA_COLOR ] := ::color
   GraSetAttrArea( oPS, aAttr )

  /*
   * Calculate position for lowest square (starting position)
   */
   aPos1     := { 2, 2 }   
   ::squares := Int( aSize[2] / (aSize[1]+1) )
   nCenter   := 2 + (aSize[2] - (::squares * (aSize[1]+1)) ) / 2
   aPos1[2]  := Max( 2, nCenter )
   aPos2     := { aSize[1]-1, aPos1[2]+aSize[1]-2 }   

  /*
   * Draw the squares
   */
   DO WHILE aPos2[2] < nY
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
      aPos1[2] += aSize[1]+1
      aPos2[2] += aSize[1]+1
   ENDDO

   IF aPos2[2] < aSize[2]
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
   ENDIF

RETURN self


