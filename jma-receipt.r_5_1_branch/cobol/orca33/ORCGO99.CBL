      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGO99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/23    NACL−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.07.10  印刷指示画面からの遷移を考慮
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      * 01  SPA-RECE07-REC. 
      *     COPY    "CPRCF007.INC"  REPLACING  //RECE07//
      *                             BY         //SPA-RECE07//.
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "O01COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-RUN             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-TEIKIPRT      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-UPDCNT          PIC 9(06).
           03  CNT-BTPARA          PIC 9(06).
           03  CNT-STOP            PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SHELLID         PIC X(07).
           03  WRK-JOBID           PIC 9(07).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
      *
       01  KBT-AREA.
           03  KBT-OCC             OCCURS   3.
               05  KBT-KBT         PIC 9(01).
               05  KBT-STOP        PIC 9(01).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  WRK-DOT1        PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  WRK-DOT2        PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  CONST-AREA.
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID       PIC X(08)   VALUE   "TEIKI1".
           03  CONST-SHELLID2      PIC X(08)   VALUE   "TEIKI2".
           03  CONST-JOBID2        PIC 9(07)   VALUE   1.
           03  CONST-SHELLID3      PIC X(08)   VALUE   "TEIKI3".
           03  CONST-JOBID3        PIC 9(07)   VALUE   1.
           03  CONST-3003-KBNCD    PIC X(08)   VALUE   "033".
           03  CONST-KBT-VAL.
               05                  PIC 9(01)   VALUE   1.
               05                  PIC X(04)   VALUE   "１期".
               05                  PIC 9(01)   VALUE   2.
               05                  PIC X(04)   VALUE   "２期".
               05                  PIC 9(01)   VALUE   3.
               05                  PIC X(04)   VALUE   "３期".
           03  CONST-KBT-R         REDEFINES   CONST-KBT-VAL.
               05  CONST-KBT-OCC OCCURS   3.
                   07  CONST-KBTL      PIC 9(01).
                   07  CONST-KBTMSGL   PIC X(04).
      *
           COPY    "CPTEIKI1BTPARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK3003.INC".
      *
      *    定期印刷マスタ
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "O01.INC".
           COPY    "O02.INC".
           COPY    "O03.INC".
           COPY    "O04.INC".
           COPY    "O05.INC".
           COPY    "O99.INC".
           COPY    "OERR.INC".
           COPY    "OID1.INC".
           COPY    "OID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
           PERFORM 490-KAKUNIN-SEC
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "O99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *???
           DISPLAY "MCP-WIDGET=" MCP-WIDGET "##"
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    処理状態
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-KAKUNIN-SEC
      *    印刷画面へ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4501-NEXT-GMN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SPA-O99-FLG
      *     
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "O99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE        SPACE      TO  SPA-O99-MSG
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "O03"
               PERFORM 4901-SIN-KAKUNIN-SEC
           WHEN    "O05"
               PERFORM 4901-SIN-KAKUNIN-SEC
           WHEN    OTHER
               PERFORM 4901-MUL-KAKUNIN-SEC
           END-EVALUATE
      *
           IF    ( SPA-MOTOPG      =   "O03" )
               MOVE    "F12:プレビュー"    TO  O99-B12-LABEL
           ELSE
               MOVE    "F12:印　刷"        TO  O99-B12-LABEL
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "O99    "       TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
           END-IF
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       4901-SIN-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      O99
      *
           MOVE    ZERO                TO  O99-COUNT
           MOVE    SPACE               TO  WRK-SHELLID
      *
           MOVE    ZERO                TO  CNT-UPDCNT
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
      *
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           EVALUATE    SPA-MOTOPG
           WHEN    "O03"
               MOVE    CONST-JOBID2    TO  JOB-JOBID
               MOVE    CONST-SHELLID2  TO  JOB-SHELLID
           WHEN    "O05"
               MOVE    CONST-JOBID3    TO  JOB-JOBID
               MOVE    CONST-SHELLID3  TO  JOB-SHELLID
           WHEN    OTHER
               MOVE    1               TO  JOB-JOBID
               MOVE    CONST-SHELLID   TO  JOB-SHELLID
           END-EVALUATE
      *
           MOVE    JOB-SHELLID         TO  WRK-SHELLID
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                    
               MOVE    JOB-JOBID       TO  O99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  O99-SHORINM  (IDX)
               MOVE    "."             TO  WRK-DOT1
                                           WRK-DOT2
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  O99-STARTTIME(IDX)
               MOVE    "."             TO  WRK-DOT1
                                           WRK-DOT2
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  O99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  O99-ERRMSG   (IDX)
               COMPUTE O99-PROGRESSBAR-VALUE  =
                       JOB-STEPCNT  /  1 *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  O99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       MOVE   SPACE        TO  O99-MSG
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  O99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    JOB-UPDCNT              TO  CNT-UPDCNT
               MOVE    IDX                     TO  O99-COUNT    
           END-IF
      *
           IF    ( SPA-MOTOPG      =   "O03" )
               PERFORM 4901-EDIT-PRV-MSG-SEC
           ELSE
               PERFORM 4901-EDIT-SIN-MSG-SEC
           END-IF
      *
           .
       4901-SIN-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       4901-EDIT-PRV-MSG-SEC           SECTION.
      *
           MOVE    WIDGET-INSENSITIVE   TO  O99-B12-STATE
           IF    ( O99-MSG              =   SPACE )
               IF      O99-COUNT            >  ZERO    
                   MOVE    "処理は正常に終了しました"
                                        TO  O99-MSG
                   MOVE   100           TO  O99-PROGRESSBAR-VALUE
                   MOVE    "B01"        TO  MCP-WIDGET
                   IF    ( SPA-OID1-FLG =   "PV" )
                    AND  ( CNT-UPDCNT   >   ZERO )
                       MOVE    WIDGET-NORMAL   TO  O99-B12-STATE
                       IF    ( SPA-O01-CHGWINDOWKBN    =   "1" )
                           PERFORM 4501-NEXT-GMN-SEC
                       END-IF
                   END-IF
               ELSE
                   MOVE    "B01"        TO  MCP-WIDGET
                   MOVE    "該当する処理はありません"
                                        TO  O99-MSG
               END-IF
           ELSE
               MOVE    "B11"            TO  MCP-WIDGET
           END-IF
      *
           .
       4901-EDIT-SIN-MSG-EXT.
           EXIT.
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       4901-EDIT-SIN-MSG-SEC           SECTION.
      *
           MOVE    WIDGET-INSENSITIVE   TO  O99-B12-STATE
           IF      O99-MSG              =   SPACE
               IF      O99-COUNT            >  ZERO    
                   MOVE    "処理は正常に終了しました"
                                               TO  O99-MSG
                   MOVE   100                  TO
                                               O99-PROGRESSBAR-VALUE
                   MOVE    "B01"               TO  MCP-WIDGET
                   IF    ( WRK-SHELLID  =   CONST-SHELLID )
                       PERFORM 900-TEIKIPRT-ALL-SEL-SEC
                       IF    ( FLG-TEIKIPRT    =   ZERO )
                           MOVE    WIDGET-NORMAL   TO  O99-B12-STATE
                       END-IF
                   END-IF
               ELSE
                   MOVE    "B01"                TO  MCP-WIDGET
                   MOVE    "該当する処理はありません"
                                                TO  O99-MSG
               END-IF                                 
           ELSE                                        
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           .
       4901-EDIT-SIN-MSG-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理（複数ジョブチェック）
      *****************************************************************
       4901-MUL-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      O99
                                           KBT-AREA
      *
           MOVE    ZERO                TO  O99-COUNT
                                           O99-PROGRESSBAR-VALUE
      *
           MOVE    ZERO                TO  FLG-RUN
                                           CNT-STOP
                                           CNT-UPDCNT
                                           CNT-BTPARA
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   SYS-3003-JOBID )
      *
                   MOVE    IDX4        TO  WRK-JOBID
                   PERFORM 900-BTPARA-KEY5-SEL-SEC
                   IF    ( FLG-BTPARA  =   ZERO )
      *
                       COMPUTE CNT-BTPARA  =   CNT-BTPARA  +   1
      *
                       MOVE    BTPARA-INFO-PARA
                                       TO  TEIKI1BTPARA
                       MOVE    CONST-KBTL(TEIKI1BT-KBT)
                                       TO  KBT-KBT (IDX4)
                   END-IF
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   MOVE    IDX4            TO  JOB-JOBID
                   MOVE    CONST-SHELLID   TO  JOB-SHELLID
      *
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       MOVE    JOB-JOBID       TO  O99-RENNUM   (IDX4)
                       MOVE    JOB-SHELLMSG    TO  O99-SHORINM  (IDX4)
                       MOVE    "."             TO  WRK-DOT1
                                                   WRK-DOT2
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  O99-STARTTIME(IDX4)
                       MOVE    "."             TO  WRK-DOT1
                                                   WRK-DOT2
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  O99-ENDTIME  (IDX4)
                       MOVE    JOB-YOBI        TO  O99-ERRMSG   (IDX4)
                       IF    ( JOB-STEPCNT     >   ZERO )
                           COMPUTE O99-PROGRESSBAR-VALUE
                                  = (( IDX4 - 1) * 4   +   JOB-STEPCNT ) 
                                  / ( SYS-3003-JOBID   *   4 )
                                  *    100
                       END-IF
      *
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  O99-MSG
                           MOVE    1           TO  FLG-RUN
                       ELSE
      *
                           IF    ( JOB-STOPFLG =   2 )
                               MOVE    1       TO  KBT-STOP (IDX4)
                               COMPUTE CNT-STOP    =   CNT-STOP    +   1
                           END-IF
      *
                           COMPUTE  CNT-UPDCNT
                               =    CNT-UPDCNT +   JOB-UPDCNT
      *
                           IF    (  JOB-ERRCD      =   SPACE )
                               CONTINUE
                           ELSE
                               IF    ( CNT-STOP    <=  1 )
                                   MOVE   SPACE        TO  O99-MSG
                                   STRING  JOB-YOBI
                                                   DELIMITED  BY  SPACE
                                           "【"    DELIMITED  BY  SIZE
                                          JOB-SHELLMSG
                                                   DELIMITED  BY  SPACE
                                          "】"     DELIMITED  BY  SIZE
                                   INTO  O99-MSG
                                   END-STRING
                               END-IF
                           END-IF
      *
                       END-IF
                   END-IF
                   MOVE    IDX4             TO  O99-COUNT
               END-PERFORM
           END-IF
      *
      *    複数起動対応前後で編集するメッセージを変える
           IF    ( CNT-BTPARA      =   ZERO )
               PERFORM 4901-SIN-KAKUNIN-SEC
           ELSE
               PERFORM 4901-EDIT-MUL-MSG-SEC
           END-IF
      *
           .
       4901-MUL-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       4901-EDIT-MUL-MSG-SEC           SECTION.
      *
           MOVE    WIDGET-INSENSITIVE       TO  O99-B12-STATE
           IF    ( O99-COUNT        >       ZERO  )
               MOVE    "B11"                TO  MCP-WIDGET
               IF    ( FLG-RUN              =   ZERO )
                   IF    ( CNT-STOP         =   ZERO )
                       IF    ( CNT-UPDCNT   >   ZERO )
                           MOVE    "B12"           TO  MCP-WIDGET
                           MOVE    "処理は正常に終了しました"
                                                   TO  O99-MSG
                           PERFORM 900-TEIKIPRT-ALL-SEL-SEC
                           IF    ( FLG-TEIKIPRT    =   ZERO )
                               MOVE    WIDGET-NORMAL   TO  O99-B12-STATE
                               IF    ( SPA-O01-CHGWINDOWKBN    =   "1" )
                                   PERFORM 4501-NEXT-GMN-SEC
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "処理対象のデータはありませんでした"
                                                   TO  O99-MSG
                       END-IF
                   ELSE
                       IF    ( CNT-UPDCNT   >   ZERO )
                           EVALUATE    TRUE
                           WHEN  ( KBT-STOP (1)    =   1 )
                               CONTINUE
                           WHEN  ( KBT-STOP (2)    =   1 )
                               MOVE    KBT-KBT (2) TO  IDX1
                               MOVE    KBT-KBT (1) TO  IDX2
                           WHEN  ( KBT-STOP (3)    =   1 )
                               MOVE    KBT-KBT (3) TO  IDX1
                               MOVE    KBT-KBT (2) TO  IDX2
                           END-EVALUATE
      *
                           IF   (( IDX1    >=  1 )
                            AND  ( IDX1    <=  3 ))
                            AND (( IDX2    >=  1 )
                            AND  ( IDX2    <=  3 ))
                               MOVE    SPACE       TO  O99-MSG
                               STRING  CONST-KBTMSGL (IDX1)
                                                   DELIMITED BY  SIZE
                                       "の処理が異常終了しています。"
                                                   DELIMITED BY  SIZE
                                       "請求データは"
                                                   DELIMITED BY  SIZE
                                       CONST-KBTMSGL (IDX2)
                                                   DELIMITED BY  SIZE
                                       "まで作成されています"
                                                   DELIMITED BY  SIZE
                               INTO    O99-MSG
                               END-STRING
                               MOVE    O99-MSG         TO  SPA-O99-MSG
                               MOVE    "B12"           TO  MCP-WIDGET
                               PERFORM 900-TEIKIPRT-ALL-SEL-SEC
                               IF    ( FLG-TEIKIPRT    =   ZERO )
                                   MOVE    WIDGET-NORMAL
                                                       TO  O99-B12-STATE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    "B01"                TO  MCP-WIDGET
               MOVE    "該当する処理はありません"
                                            TO  O99-MSG
           END-IF
      *
           .
       4901-EDIT-MUL-MSG-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
           MOVE    "OK"                TO  SPA-O99-FLG
      *     
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "O99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター検索（KEY10）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索（KEY5）
      *****************************************************************
       900-BTPARA-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-SHELLID       TO  BTPARA-SHELLID
           MOVE    WRK-JOBID           TO  BTPARA-JOBID
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  BTPARA-REC
           ELSE
               MOVE    1               TO  FLG-BTPARA
               MOVE    MCPDATA-REC     TO  BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-BTPARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期印刷検索処理（ALL）
      *****************************************************************
       900-TEIKIPRT-ALL-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIPRT
      *
           INITIALIZE                      TEIKIPRT-REC
           MOVE    SPA-HOSPNUM         TO  TEIKIPRT-HOSPNUM
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIPRT-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIPRT
               INITIALIZE                  TEIKIPRT-REC
           END-IF
      *
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIPRT-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   LOW-VALUE        TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
