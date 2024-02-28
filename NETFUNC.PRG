// Модуль с сетевыми функциями

#include "Xbp.ch"

#define NET_WAIT     0.5   // Seconds to wait between between retries
#define NET_SECS     2     // Number of seconds to continue retry

// Добавление записи в сети
FUNCTION NetAppend( nWaitSeconds )
   LOCAL lForever           // Бесконечные попытки ?
//   LOCAL win:=win_save(maxrow(),1,maxrow(),30),clr:=setcolor()
   nWaitSeconds:=IF(nWaitSeconds==NIL,30,nWaitSeconds)
//   set color to g*/r+
//   @ maxrow(),1 say "Добавление "
   nRetry:=nWaitSeconds
   APPEND BLANK
   IF !NETERR()
//      win_rest(win); setcolor(clr)
      RETURN ( .T. )        // Запись добавлена успешно !!!
   ENDIF
   lForever:=(nWaitSeconds==0)
//------------------------- Ожидаю снятие блокировки для добавления записи...
   DO WHILE ( lForever .OR. ( nWaitSeconds > 0 ) )
//   		@ maxrow(),1 say "Добаление "
      APPEND BLANK
      IF !NETERR()
//         win_rest(win); setcolor(clr)
         RETURN ( .T. )          // Дождались !!!
      ENDIF
      INKEY( NET_WAIT )          // Жду NET_WAIT секунд для следующей попытки
      nWaitSeconds  -= NET_WAIT
      IF .not.(nWaitSeconds>0)
      	
  			IF ConfirmBox( SetAppWindow(), "База данных занята другим пользователем."+Chr(13)+Chr(10)+"Ожидать освобождения базы", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES
                 nWaitSeconds:=nRetry
        ELSE
//           win_rest(win); setcolor(clr)
           return .F. // Не дождались...
        ENDIF
      ENDIF
   ENDDO
//   win_rest(win); setcolor(clr)
RETURN ( .F. )       // Не дождались :-(



// Блокирование записи в сети
FUNCTION RecLock( nSeconds )
   LOCAL lForever          // Бесконечные попытки?
   LOCAL nRetry
//   LOCAL win:=win_save(maxrow(),1,maxrow(),30),clr:=setcolor()
   nSeconds:=if(nSeconds==NIL,30,nSeconds)
   nRetry:=nSeconds
//   set color to g*/r+
//   @ maxrow(),1 say "Блокирование "
   IF RLOCK()
//      win_rest(win); setcolor(clr)
      RETURN ( .T. )       // Заблокировано.
   ENDIF
   lForever := ( nSeconds == 0 )
//------------------------- Ожидаю снятие блокировки для добавления записи...
   DO WHILE ( lForever .OR. ( nSeconds > 0 ) )
      IF RLOCK()
//         win_rest(win); setcolor(clr)
         RETURN ( .T. )    // Дождались
      ENDIF
//   		@ maxrow(),1 say "Блокировка "
      INKEY( NET_WAIT )    // Жду NET_WAT секунд...
      nSeconds -= NET_WAIT
      if .not.(nSeconds>0)
  			IF ConfirmBox( SetAppWindow(), "База данных занята другим пользователем."+Chr(13)+Chr(10)+"Ожидать освобождения базы", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES
                 nSeconds:=nRetry
        ELSE
//           win_rest(win); setcolor(clr)
           return .F.     // Не дождались...
        ENDIF
      endif
   ENDDO
//   win_rest(win); setcolor(clr)
RETURN ( .F. )          // Не дождались



********************************************************************
* LOGICAL netuse(cdatabase, lopenmode, nseconds)
* CHARACTER cdatabase - Имя базы данных
* LOGICAL lopenmode   - Режим открытия. .T. монопольный, .F. разделяемый
* NUMERIC nseconds    - Число секунд ожидания (0 = ждать до упора)
* ВОЗВРАТ - .T., если успешна, .F., если нет
* Nantucket функция netuse со взаимодействием с пользователем, если USE
* завершилась аварийно
* Образец вызова:  IF netuse("calls", .F., 5)
*
FUNCTION netuse(cDatabase, lOpenMode, nSeconds, cAlias,lAsk)
LOCAL forever, restart, Wait_Time
//LOCAL win:=win_save(maxrow(),1,maxrow(),MaxCol()),clr:=setcolor()
    lOpenMode:=if(lOpenMode==NIL,.F.,lOpenMode)
    nSeconds:=if(nSeconds==NIL,30,nSeconds)
    cAlias:=if(cAlias==NIL,"",cAlias)
    lAsk:=if(lAsk==NIL,.T.,lAsk)
    restart = .T.
    forever = (nseconds = 0)
    DO WHILE restart
        Wait_Time = nSeconds
        DO WHILE (forever .OR. Wait_Time > 0)
//   				 @ maxrow(),1 say "Открытие "+cDatabase
           IF !Empty(cAlias)
            IF lOpenMode
                USE (cdatabase) EXCLUSIVE Alias &cAlias
            ELSE
                USE (cdatabase) SHARED Alias &cAlias
            ENDIF
           ELSE
            IF lOpenMode
                USE (cdatabase) EXCLUSIVE
            ELSE
                USE (cdatabase) SHARED
            ENDIF
           ENDIF
           IF !neterr()
//           		 win_rest(win); setcolor(clr)
               RETURN .T.
           ENDIF
           inkey(0.3)
           Wait_Time --
        ENDDO
        * Блокирование не удалось, спросим пользователя, продолжать ли
        IF lAsk
  				IF ConfirmBox( SetAppWindow(), "База данных занята другим пользователем."+Chr(13)+Chr(10)+"Ожидать освобождения базы", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES

          		restart = .T.
          Else
          		restart = .F.
        	ENDIF
        ELSE
           Restart:=.F.
        ENDIF
    ENDDO
//    win_rest(win); setcolor(clr)
RETURN .F.



********************************************************************
* LOGICAL fillock(nseconds)
* NUMERIC nseconds
* ВОЗВРАТ - .T., если успешна, .F., если нет
* Процедура блокирования файла со взаимодействием с пользователем, если
* блокирование не удалось
* Образец вызова:
* IF fillock(5)
*
FUNCTION FileLock(nseconds)
LOCAL lforever, restart, wait_time
//LOCAL win:=win_save(maxrow(),1,maxrow(),MaxCol()),clr:=setcolor()
    nSeconds:=IF(nSeconds=NIL,0,nSeconds)
//    set color to g*/r+
//    @ maxrow(),1 say "Блокирование "
//    sound(700,2)
    IF flock()
//       win_rest(win); setcolor(clr)
       RETURN (.T.)            && Заблокирован
    ENDIF
    Wait_Time:=nSeconds
    restart = .T.
    lforever = IF(nSeconds=0,.T.,.F.)
    DO WHILE restart
        wait_time = nseconds
        DO WHILE (lforever .OR. wait_time > 0)
            inkey(0.5)            // ждать 1/2 секунды
            wait_time = wait_time - .5
            IF flock()
//               win_rest(win); setcolor(clr)
               RETURN (.T.)     && Заблокирован
            ENDIF
        ENDDO
  				IF ConfirmBox( SetAppWindow(), "База данных занята другим пользователем."+Chr(13)+Chr(10)+"Ожидать освобождения базы", ; 
                           "Внимание", ; 
                            XBPMB_YESNO , ; 
                            XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE ) == XBPMB_RET_YES

          		restart = .T.
          Else
          		restart = .F.
        	ENDIF
    ENDDO
//   win_rest(win); setcolor(clr)
RETURN (.F.)                     // Не заблокирован