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
       PROGRAM-ID.                 ORCGR99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.08.30  タイマー機能の追加
      *
      *  01.01.01    NACL-藤原    02/09/24  ２００２．１０改正対応
      *　入院機能追加　
      *  01.02.01    NACL-藤原    02/12/13  入外区分追加 
      *
      *  ２００３．４改正対応
      *  01.03.01    NACL-藤原    03/04/17  対象がないときエラーを表示する
      *  01.03.02    NACL-藤原    04/07/09  印刷画面への自動遷移
      *  01.03.03    NACL-藤原    04/08/03  プレビューの該当が無いときの
      *                                     カーソルとボタンの設定
      *  中止処理追加
      *  01.04.01    NACL-藤原    04/09/28 中止ボタンを追加
      *  01.04.02    NACL-藤原    04/09/28 印刷中止機能を追加
      *
      *  02.06.01    NACL-藤原    05/06/22 自賠責レセプトの様式追加
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *
      *  03.04.01    NACL-藤原    06/12/01  主科未設定分があるときは印刷
      *                                     指示画面への自動遷移をしない
      *  03.04.02    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     不明分ありのとき赤字でエラー
      *                                     表示を行う
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.02    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.03    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.04    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.05    NACL-藤原    14/04/22  ＥＦファイル作成対応
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
       WORKING-STORAGE             SECTION.
      *
       01  SPA-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //SPA-RECE07//.
           03  SPA-RECE07-SRYKA-KOKUHO
                                   PIC X(02).
           03  SPA-RECE07-SRYKA-KOUIKI
                                   PIC X(02).
           03  SPA-RECE07-RECE-PUTH-T   OCCURS 2.
               05  SPA-RECE07-ZAITAKU-KBN-T
                                   PIC  X(01).
               05  SPA-RECE07-CHOUKI-KBN-T
                                   PIC  X(01).
               05  SPA-RECE07-SRT-LAST-T
                                   PIC  X(01).
               05  SPA-RECE07-KOUGAKU-TEN-T
                                   PIC  9(07).
           03  SPA-RECE08-DRCD-SRYKA
                                   PIC X(02).
           03  SPA-RECE08-SORT-1   PIC X(10).
           03  SPA-RECE08-SORT-2   PIC X(50).
           03  SPA-RECE08-RECESYUBETU-SRT
                                   PIC X(03).
           03  SPA-RECE07-FUMEI-PAGE
                                   PIC 9(05).
           03  SPA-RECE07-HKNJANUM-SYAHO-T.
               05  SPA-RECE07-HKNJANUM-SRT-SYAHO
                                   PIC X(01).
               05  SPA-RECE07-HKNJANUM-SYAHO-MAX
                                   PIC  9(02).
               05  SPA-RECE07-HKNJANUM-SYAHO
                                   PIC X(08)   OCCURS  10.
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-PAGE            PIC ZZZZ9.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-RECEERR         PIC X(200).
      *     
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値（作成処理）  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *    ジョブ管理ＤＢのキー値（印刷処理）  
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   1.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           COPY    "ORCA42SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
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
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    中止
               WHEN    "CLICKED"       ALSO    "B21"
                   PERFORM 490-CANCEL-SEC
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
           MOVE    SPACE               TO  SPA-R99-FLG
      *     
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "R99"               TO  SPA-MOTOPG
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
      *    中止処理
      *****************************************************************
       490-CANCEL-SEC            SECTION.
      *
           MOVE    "CAN"           TO  SPA-R99-CANCEL
           MOVE    1               TO  SPA-R01-STOPFLG
      *
      *    中止フラグのセット
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
               MOVE    WRK-CONS-JOB-JOBID1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID1
                                       TO  JOB-SHELLID
           ELSE
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           END-IF
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
             IF    ( SPA-R01-GMNKBN  =   "R02"  OR  "R04"           )
             OR    ((SPA-R01-GMNKBN  NOT =   "R02"  OR  "R04" ) AND
                    ( JOB-ENDYMD         =   SPACE            )     )
      *        ジョブ管理終了処理
               MOVE    "STP"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                   MOVE    WRK-CONS-JOB-JOBID1
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  JOB-SHELLID
               ELSE
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
               END-IF
               MOVE    1               TO  JOB-STOPFLG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        ジョブ管理終了処理
               MOVE    "STP"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                   MOVE    WRK-CONS-JOB-JOBID1
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  JOB-SHELLID
               ELSE
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
               END-IF
               MOVE    1               TO  JOB-STOPFLG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        
               IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                   MOVE    SPACE           TO  SHELLTBL
                   MOVE    "print_stop.sh"
                                           TO  SHELLTBL-NAME
      *     
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  SHELLTBL-ARG1
                   MOVE    SPA-R01-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG2
      *     
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "allways"       TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
             END-IF
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO  R99-B12-STATE
                                           R99-B21-STATE
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  R99
           INITIALIZE                      R99
                         
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
           MOVE    "black"             TO  R99-MSG-STYLE
      *
           IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
               IF      SPA-RID1-FLG    =   "PV"
                   MOVE    "F12:プレビュー"    TO  R99-B12-LABEL
               END-IF
           ELSE
               IF    ( SPA-R01-GMNKBN  =   "R97"       )
               OR    ( SPA-R01-PRTKBN  =   "9" OR  "8" )
                   CONTINUE
               ELSE
                   MOVE    "F12:印    刷"      TO  R99-B12-LABEL
               END-IF
           END-IF
      *
      *    中止ボタン状態フラグによる中止フラグのセット
           IF      SPA-R01-STOPFLG =   1
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                   MOVE    WRK-CONS-JOB-JOBID1
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  JOB-SHELLID
               ELSE
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
               END-IF
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
               AND   ( JOB-STOPFLG         =   ZERO )
                   IF    ( SPA-R01-GMNKBN  =   "R02"  OR  "R04"        )
                   OR    ((SPA-R01-GMNKBN  NOT =   "R02"  OR  "R04" )
                                                                   AND
                         ( JOB-ENDYMD          =   SPACE            )  )
      *                ジョブ管理終了処理
                       MOVE    "STP"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                           MOVE    WRK-CONS-JOB-JOBID1
                                                   TO  JOB-JOBID
                           MOVE    WRK-CONS-JOB-SHELLID1
                                                   TO  JOB-SHELLID
                       ELSE
                           MOVE    WRK-CONS-JOB-JOBID
                                                   TO  JOB-JOBID
                           MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                       END-IF
                       MOVE    1               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
               END-IF
           END-IF    
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
               MOVE    WRK-CONS-JOB-JOBID1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID1
                                       TO  JOB-SHELLID
           ELSE
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           END-IF
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                    
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               IF      SPA-R01-GMNKBN  =   "R02"  OR  "R04"
                   COMPUTE R99-PROGRESSBAR-VALUE  =
                           JOB-STEPCNT  /   SPA-R01-JOBCNT *   100
               ELSE
                   COMPUTE R99-PROGRESSBAR-VALUE  =
                           JOB-STEPCNT  /   13     *   100
               END-IF
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       IF      SPA-R01-GMNKBN  =   "R02"   OR  "R04"
                                               OR  "R97"
                           CONTINUE
                       ELSE
                           IF      JOB-UPDCNT      =   ZERO    
                               MOVE    "対象データがありませんでした"
                                                       TO  R99-MSG
                           ELSE
                             IF      SPA-R01-PRTKBN  =   "9" OR  "8"
                               CONTINUE
                             ELSE
                               PERFORM 4501-REPRINT-CHK-SEC
                               IF      SPA-RECE07-PRTID    =   SPACE
                                   MOVE    
                                   "対象のレセプトが作成されていません"
                                               TO  R99-MSG
                               ELSE                    
                                   MOVE    SPA-RECE07-KEY
                                           TO  SPA-R01-RECE07-KEY
                                   MOVE    SPA-RECE07-RECEKBN
                                           TO  SPA-R01-RECE07-RECEKBN
                                   MOVE    SPA-RECE07-SRYYM
                                           TO  SPA-R01-RECE07-SRYYM
                                   MOVE    SPA-RECE07-NYUGAIKBN
                                           TO  SPA-R01-RECE07-NYUGAIKBN
                                   MOVE    SPA-RECE07-SYORISBT
                                           TO  SPA-R01-RECE07-SYORISBT
                                   MOVE    SPA-RECE07-STYMD
                                           TO  SPA-R01-RECE07-STYMD
                                   MOVE    SPA-RECE07-EDYMD
                                           TO  SPA-R01-RECE07-EDYMD
                                   MOVE    SPA-RECE07-HSEQNUM
                                          TO  SPA-R01-RECE07-HSEQNUM
                                   MOVE    SPA-RECE07-HKNJANUM
                                          TO  SPA-R01-RECE07-HKNJANUM
                                   MOVE    SPA-RECE07-SRYKA-KOKUHO
                                          TO SPA-R01-RECE07-SRYKA-KOKUHO
                                   MOVE    SPA-RECE07-SRYKA-KOUIKI
                                          TO SPA-R01-RECE07-SRYKA-KOUIKI
                                   MOVE    SPA-RECE07-RECE-PUTH-T (1)
                                          TO SPA-R01-RECE07-RECE-PUTH(1)
                                   MOVE    SPA-RECE07-RECE-PUTH-T (2)
                                          TO SPA-R01-RECE07-RECE-PUTH(2)
                                   MOVE    SPA-RECE08-DRCD-SRYKA
                                           TO SPA-R01-RECE08-DRCD-SRYKA
                                   MOVE    SPA-RECE08-SORT-1
                                           TO SPA-R01-RECE08-SORT-1
                                   MOVE    SPA-RECE08-SORT-2
                                           TO SPA-R01-RECE08-SORT-2
                                   MOVE    SPA-RECE08-RECESYUBETU-SRT
                                           TO
                                       SPA-R01-RECE08-RECESYUBETU-SRT 
                                   MOVE    SPA-RECE07-FUMEI-PAGE
                                           TO  SPA-R01-RECE07-FUMEI-PAGE
                                   MOVE    SPA-RECE07-HKNJANUM-SYAHO-T
                                           TO
                                       SPA-R01-RECE07-HKNJANUM-SYAHO-T
                               END-IF
                             END-IF
                           END-IF   
                       END-IF
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *     
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           MOVE    WIDGET-INSENSITIVE   TO  R99-B21-STATE
      *
           IF      R99-MSG              =   SPACE
               IF      R99-COUNT            >   ZERO    
                   IF      SPA-R01-GMNKBN  =   "R02"   OR  "R04"
                       IF      JOB-STOPFLG      NOT =   ZERO
                           MOVE    "印刷処理が中止されました"
                                                        TO  R99-MSG
                       ELSE
                           IF      JOB-UPDCNT           =   ZERO
                               MOVE
                                   "該当のレセプトはありませんでした"
                                                            TO  R99-MSG
                           ELSE
                               MOVE    JOB-UPDCNT           TO  WRK-PAGE
                               STRING
                               "処理は正常に終了しました　処理枚数は"
                                                    DELIMITED  BY  SIZE
                                   WRK-PAGE         DELIMITED  BY  SIZE
                                   "枚です"         DELIMITED  BY  SIZE
                                                    INTO  R99-MSG
                               END-STRING
                               IF      JOB-STOPFLG     =   ZERO
                                   MOVE    WIDGET-NORMAL
                                                       TO  R99-B21-STATE
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF    ( SPA-R01-GMNKBN  =   "R97"       )
                       OR    ( SPA-R01-PRTKBN  =   "9" OR  "8" )
                           MOVE    "処理は正常に終了しました"
                                                TO  R99-MSG
                       ELSE
                           IF    ( SPA-R01-RECE07-SRYKA    =   "XX" )
                           OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "XX" )
                           OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                           =   "XX" )
                           OR    ( SPA-R01-RECE07-FUMEI-PAGE
                                                           >   ZERO )
                               STRING
                                   "主科未設定分又は種別不明分がありま"
                                                   DELIMITED   BY  SIZE
                                   "すので印刷画面で確認して下さい"
                                                   DELIMITED   BY  SIZE
                                                   INTO    R99-MSG
                               END-STRING
                               MOVE    "red"        TO  R99-MSG-STYLE 
                           ELSE
                               MOVE    "処理は正常に終了しました"
                                                    TO  R99-MSG
                           END-IF
                       END-IF
                   END-IF
                   MOVE   100                   TO
                                            R99-PROGRESSBAR-VALUE
                   IF      SPA-R01-GMNKBN  =   "R02"   OR  "R04"
                       IF    ( SPA-RID1-FLG    =   "PV" )
                       AND   ( JOB-UPDCNT      >   ZERO )
                       AND   ( JOB-STOPFLG     =   ZERO )
                           MOVE    WIDGET-NORMAL   TO  R99-B12-STATE
                           MOVE    "B12"           TO  MCP-WIDGET
                           IF      SPA-R01-AUTODSPKBN   =   "1"
      *                        プレビュー画面への自動遷移
                               PERFORM 4501-NEXT-GMN-SEC
                               GO  TO  490-KAKUNIN-EXT
                           END-IF
                       ELSE
                           MOVE    "B01"            TO  MCP-WIDGET
                       END-IF
                   ELSE
                     IF    ( SPA-R01-GMNKBN  =   "R97"       )
                     OR    ( SPA-R01-PRTKBN  =   "9" OR  "8" )
                         MOVE    "B01"                TO  MCP-WIDGET
                     ELSE
                       MOVE    WIDGET-NORMAL        TO  R99-B12-STATE
                       MOVE    "B12"                TO  MCP-WIDGET
                       IF      SPA-R01-AUTODSPKBN   =   "1"
                           IF    ( SPA-R01-RECE07-SRYKA    =   "XX" )
                           OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "XX" )
                           OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                           =   "XX" )
                           OR    ( SPA-R01-RECE07-FUMEI-PAGE
                                                           >   ZERO )
                               CONTINUE
                           ELSE
      *                        印刷画面への自動遷移（主科未設定分がないとき）
                               PERFORM 4501-NEXT-GMN-SEC
                               GO  TO  490-KAKUNIN-EXT
                           END-IF
                       END-IF
                     END-IF 
                   END-IF 
               ELSE
                   MOVE    "B01"                TO  MCP-WIDGET
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE
               IF    ( JOB-STOPFLG          =   ZERO  )
               AND   ( SPA-R01-GMNKBN   NOT =   "R97" ) 
                   MOVE    WIDGET-NORMAL        TO  R99-B21-STATE
               END-IF
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷チェック処理
      *****************************************************************
       4501-REPRINT-CHK-SEC         SECTION.
      *
           INITIALIZE              SPA-RECE07-REC
      *
      *
           INITIALIZE                  RECE07-REC
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
               MOVE    "HC06"              TO  RECE07-PRTID
           ELSE
               IF      SPA-R01-RECEKBN     =   "3"
                   MOVE    "HC21"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "4" OR  "5" OR  "8"
                   MOVE    "HC23"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "6"
                   MOVE    "HC25"              TO  RECE07-PRTID
               END-IF
           END-IF            
grpsys     MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
           IF      SPA-R01-FILEKBN         =   "1"
               MOVE    SPA-R01-SRYYM       TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    SPA-R01-RECEKBN     TO  RECE07-RECEKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "2"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    RECE07-REC          TO  MCPDATA-REC
           PERFORM 900-RECE07-INV-SEC
           IF      FLG-RECE07          =   ZERO
               MOVE    RECE07-REC      TO  SPA-RECE07-REC
               MOVE    "99999999"      TO  SPA-RECE07-HKNJANUM
               MOVE    "00"            TO  SPA-RECE07-SRYKA 
                                           SPA-RECE07-SRYKA-KOKUHO
                                           SPA-RECE07-SRYKA-KOUIKI
               INITIALIZE                  SPA-RECE07-RECE-PUTH-T(1)
                                           SPA-RECE07-RECE-PUTH-T(2)
               MOVE    SPACE           TO  SPA-RECE08-DRCD-SRYKA
                                           SPA-RECE08-SORT-1
                                           SPA-RECE08-SORT-2
                                           SPA-RECE08-RECESYUBETU-SRT
               MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               INITIALIZE                  SPA-RECE07-HKNJANUM-SYAHO-T
               MOVE    "%%%%%%%%"      TO  SPA-RECE07-HKNJANUM-SYAHO (1)
               MOVE    1               TO  SPA-RECE07-HKNJANUM-SYAHO-MAX
               MOVE    "9"             TO  SPA-RECE07-HKNJANUM-SRT-SYAHO
      *
      *        一括作成で社保処理のとき編綴順設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "1" )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key6"              TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-RECESYUBETU-SRT
                                                   TO
                                           SPA-RECE08-RECESYUBETU-SRT
                   END-IF
               END-IF  
      *
      *        一括作成で国保処理のとき編綴順設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "2" )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key6"              TO  WRK-MCP-PATHNAME
                   MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-HKNJANUM TO  SPA-RECE07-HKNJANUM
                       MOVE    RECE07-HSEQNUM  TO  SPA-RECE07-HSEQNUM
      *                編綴順設定のとき詳細情報を取得
                       IF    ( SPA-RECE07-HKNJANUM
                                               NOT =   "99999999" )
                       AND   ( SPA-RECE07-HSEQNUM  =   "99999999" )
                           MOVE    "tbl_recekanri" TO  WRK-MCP-TABLE
                           MOVE    "key12"         TO  WRK-MCP-PATHNAME
                           MOVE    "1"             TO  RECE07-PREFKBN
                           MOVE    "2"             TO
                                                   RECE07-TEISYUTUSAKI
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   ZERO
                               EVALUATE    RECE07-PREFKBN-SRT (1:1)
                                   WHEN    "1"
                                       MOVE    1           TO  IDXX
                                   WHEN    "2"
                                       MOVE    2           TO  IDXX
                               END-EVALUATE    
                               MOVE    RECE07-ZAITAKU-KBN  TO
                                         SPA-RECE07-ZAITAKU-KBN-T (IDXX)
                               MOVE    RECE07-CHOUKI-KBN   TO
                                         SPA-RECE07-CHOUKI-KBN-T  (IDXX)
                               MOVE    RECE07-SRT-LAST     TO
                                         SPA-RECE07-SRT-LAST-T    (IDXX)
                               MOVE    RECE07-KOUGAKU-TEN  TO
                                         SPA-RECE07-KOUGAKU-TEN-T (IDXX)
                           END-IF
                           MOVE    "tbl_recekanri" TO  WRK-MCP-TABLE
                           MOVE    "key12"         TO  WRK-MCP-PATHNAME
                           MOVE    "2"             TO  RECE07-PREFKBN
                           MOVE    "2"             TO
                                                   RECE07-TEISYUTUSAKI
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   ZERO
                               EVALUATE    RECE07-PREFKBN-SRT (1:1)
                                   WHEN    "1"
                                       MOVE    1           TO  IDXX
                                   WHEN    "2"
                                       MOVE    2           TO  IDXX
                               END-EVALUATE    
                               MOVE    RECE07-ZAITAKU-KBN  TO
                                         SPA-RECE07-ZAITAKU-KBN-T (IDXX)
                               MOVE    RECE07-CHOUKI-KBN   TO
                                         SPA-RECE07-CHOUKI-KBN-T  (IDXX)
                               MOVE    RECE07-SRT-LAST     TO
                                         SPA-RECE07-SRT-LAST-T    (IDXX)
                               MOVE    RECE07-KOUGAKU-TEN  TO
                                         SPA-RECE07-KOUGAKU-TEN-T (IDXX)
                           END-IF
                       END-IF
                   END-IF
               END-IF  
      *
      *        一括作成のとき主科設定の有無チェック
               IF    ( SPA-SYUKA-KBN           =   "1"         )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO  SPA-RECE07-SRYKA
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOKUHO
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "6"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOUIKI
                   END-IF
               END-IF  
      *
      *        一括作成のとき医師別設定、病棟別設定、
      *                      ターミナルデジット設定の有無チェック
               IF    ( SPA-R01-FILEKBN         =   "1"         )
                   INITIALIZE                      RECE08-REC
                   MOVE    RECE07-HOSPNUM      TO  RECE08-HOSPNUM
                   MOVE    RECE07-PRTID        TO  RECE08-PRTID
                   MOVE    RECE07-SRYYM        TO  RECE08-RECEYM
                   MOVE    RECE07-CREYMD       TO  RECE08-CREYMD
                   MOVE    RECE07-CREHMS       TO  RECE08-CREHMS
                   MOVE    RECE07-NYUGAIKBN    TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE08-INV-SEC
                   IF      FLG-RECE08          =   ZERO
                       MOVE    RECE08-DRCD-SRYKA   TO
                                               SPA-RECE08-DRCD-SRYKA
                       MOVE    RECE08-SORT-1       TO
                                               SPA-RECE08-SORT-1
                       MOVE    RECE08-SORT-2       TO
                                               SPA-RECE08-SORT-2
                   END-IF
               END-IF
      *
      *        一括作成で社保処理のとき直接請求する保険者のチェックと取得
               IF    ( SPA-R01-FILEKBN         =   "1"         )
               AND   ( SPA-R01-RECEKBN         =   "0" OR  "1" )
                   MOVE    "99999999"          TO
                                           SPA-RECE07-HKNJANUM-SYAHO (1)
                   MOVE    "9"                 TO
                                           SPA-RECE07-HKNJANUM-SRT-SYAHO
                   MOVE    1                   TO  IDXX
      *
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key14"             TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO
                                               SPA-RECE07-TEISYUTUSAKI
                   MOVE    SPA-RECE07-REC      TO  MCPDATA-REC
                   PERFORM 900-RECE07-SELECT-SEC
                   PERFORM             UNTIL   FLG-RECE07  =   1
                                       OR      IDXX        =   10
                                       OR      RECE07-HKNJANUM-SRT
                                                           =   "9"
                       IF      IDXX            =   1
                           MOVE    RECE07-HKNJANUM-SRT TO
                                       SPA-RECE07-HKNJANUM-SRT-SYAHO
                       END-IF                                        
                       IF      RECE07-HKNJANUM NOT =   "99999999"
                           ADD     1                   TO  IDXX
                           MOVE    RECE07-HKNJANUM     TO
                                       SPA-RECE07-HKNJANUM-SYAHO (IDXX)
                       END-IF
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-RECE07-FETCH-SEC
                   END-PERFORM
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      IDXX                =   1
                       MOVE    "%%%%%%%%"          TO  
                                           SPA-RECE07-HKNJANUM-SYAHO (1)
                   END-IF
      *
                   MOVE    IDXX                TO
                                           SPA-RECE07-HKNJANUM-SYAHO-MAX
               END-IF  
      *
      *        不明分の有無チェック
               MOVE    SPA-RECE07-KEY      TO  RECE07-KEY
               MOVE    "%"                 TO  RECE07-TEISYUTUSAKI
               MOVE    "0000"              TO  RECE07-RECESYUBETU
               MOVE    "%%"                TO  RECE07-SRYKA 
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-PAGE         TO  SPA-RECE07-FUMEI-PAGE
               ELSE
                   MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               END-IF
           END-IF
      *
           .
       4501-REPRINT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
           MOVE    "OK"                TO  SPA-R99-FLG
      *     
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "R99"               TO  SPA-MOTOPG
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
      *    レセプト管理読込（キー）
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理（ＳＥＬＥＣＴ）
      *****************************************************************
       900-RECE07-SELECT-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECE07-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理（ＦＥＴＣＨ）
      *****************************************************************
       900-RECE07-FETCH-SEC            SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE07
               MOVE    MCPDATA-REC         TO  RECE07-REC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-INV-SEC           SECTION.
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
                   MOVE    MCPDATA-REC         TO  RECE08-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE08-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
