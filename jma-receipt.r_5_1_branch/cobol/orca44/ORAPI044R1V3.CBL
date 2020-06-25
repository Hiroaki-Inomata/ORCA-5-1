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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI044R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    :  API連携用モジュール
      *  コンポーネント名  : レセ電データ作成(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/08/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.02    NACL-藤原    20/03/27  平成３２年４月改正未対応メッ
      *                                     セージの返却
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
      *    エラーファイル名称領域 
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
           COPY    "T06SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-HENYMDG             PIC X(09).
           03  WRK-HENYMDG3            PIC X(22).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WARNING-INFO.
               05  WRK-WARNING-OCC         OCCURS 5.
                   07  WRK-WARNING     PIC X(03).
                   07  WRK-WARNING-MSG PIC X(100).
               05  WRK-WARNING-MAX     PIC 9(02).
      *
           03  WRK-CHK-AREA.
               05  WRK-CHK-PRTKBN      PIC X(01).
               05  WRK-CHK-NYUGAIKBN   PIC X(01).
               05  WRK-CHK-HKNJANUM    PIC X(08).
               05  WRK-CHK-SRYYMKBN    PIC X(02).
               05  WRK-CHK-DIRECT-HKNJANUM
                                       PIC X(08). 
               05  WRK-START-MONTH     PIC X(06).
               05  WRK-END-MONTH       PIC X(06).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-9                   PIC 9(10).
           03  WRK-X                   REDEFINES   WRK-9.
               05  WRK-X1              PIC X(05).
               05  WRK-X2              PIC X(05).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-SPA-TERMID          PIC X(64).
      *  
           03  WRK-STPAGE              PIC 9(05).
           03  WRK-EDPAGE              PIC 9(05).
      *
           03  WRK-MONBLOB-PRINT-ID    PIC X(37).
           03  WRK-MONBLOB-DATA-ID     PIC X(37).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      * 
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理マスタ
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"
                                   REPLACING  //JOB//
                                   BY         //WRK-JOB//.
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    パラメタＤＢのキー値（レセ電作成）  
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(10)   VALUE   "recept4.sh".
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID.
               05  FILLER          PIC X(08)   VALUE   "0100007F". 
               05  FILLER          PIC X(56)   VALUE   ALL "Z".
      *     
      *    ジョブ管理ＤＢのキー値（レセプト）  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPTDATA-MAKEV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPTDATA-MAKEV3RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
           COPY    "CPORCSTEMPDIR.INC".
      *
      **** COPY    "MONBLOB".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK3003.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "**************************"
           DISPLAY   "* receiptdata_Make start *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *****INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
                                           SPA-T01FREE
                                           SPA-T01KYOUTU
                                           SPA-T06
      *
           MOVE    MCPAREA             TO  TAIHI-MCPAREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**************************"
           DISPLAY   "*  receiptdata_Make end  *"
           DISPLAY   "**************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-RECEIPTDATA-MAKERES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  RECEDMAKES-RESKEY
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO
                                       RECEDMAKES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO
                                       RECEDMAKES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      * 
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"            TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  RECEDMAKES-API-RESULT
           INITIALIZE                      WRK-ERRCD
                                           WRK-ERRMSG
                                           WRK-WARNING-INFO
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    RECEDMAKEQ-REQUEST-NUMBER
               WHEN    "00"
      *            情報取得
                   MOVE    9                   TO  FLG-SYORI
               WHEN    "01"
      *            処理実施
                   MOVE    1                   TO  FLG-SYORI
               WHEN    "02"
      *            処理確認
                   MOVE    2                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "E91"               TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *??
           DISPLAY "2001=" WRK-ERRCD "#" SPA-ERRCD "#"
      *??
           IF    ( WRK-ERRCD           =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    RECEDMAKEQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理実施処理
                   PERFORM 2002-RECEIPTDATA-MAKE-SEC
               WHEN    "02"
      *            処理確認処理
                   PERFORM 2003-RECEIPTDATA-CHECK-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-RECEIPTDATA-MAKEREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_receiptdata_makev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "receiptdata_makev3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-RECEIPTDATA-MAKEREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "01"        TO  RECEDMAKES-RESPONS-NUMBER
                   WHEN    OTHER
                       MOVE    "02"        TO  RECEDMAKES-RESPONS-NUMBER
               END-EVALUATE
       *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  RECEDMAKES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  RECEDMAKES-API-RESULT-MSG
                   WHEN    9
                       MOVE    "情報取得終了"
                                           TO  RECEDMAKES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "00"        TO  RECEDMAKES-RESPONS-NUMBER
                   WHEN    OTHER
                       MOVE    "01"        TO  RECEDMAKES-RESPONS-NUMBER
               END-EVALUATE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  RECEDMAKES-API-RESULT
               MOVE    WRK-ERRMSG      TO  RECEDMAKES-API-RESULT-MSG
           END-IF
      *
      *    警告メッセージ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      WRK-WARNING (IDX)   =   SPACE
               MOVE    WRK-WARNING     (IDX)   TO
                                       RECEDMAKES-API-WARNING     (IDX)
               MOVE    WRK-WARNING-MSG (IDX)   TO
                                       RECEDMAKES-API-WARNING-MSG (IDX)
           END-PERFORM
      *
           MOVE    RECEDMAKEQ-KARTE-UID TO  RECEDMAKES-KARTE-UID
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI           =   1
                   IF    ( WRK-ERRCD           =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       MOVE    RECEDMAKEQ-ORCA-UID
                                               TO  RECEDMAKES-ORCA-UID
                   END-IF
               ELSE
                   MOVE    RECEDMAKEQ-ORCA-UID  TO  RECEDMAKES-ORCA-UID
               END-IF
           END-IF
           MOVE    RECEDMAKEQ-REQUEST-NUMBER
                                       TO  RECEDMAKES-REQUEST-NUMBER
           MOVE    RECEDMAKEQ-SUBMISSION-MODE
                                       TO  RECEDMAKES-SUBMISSION-MODE
           MOVE    RECEDMAKEQ-PERFORM-MONTH
                                       TO  RECEDMAKES-PERFORM-MONTH
           MOVE    RECEDMAKEQ-START-MONTH
                                       TO  RECEDMAKES-START-MONTH
           MOVE    RECEDMAKEQ-END-MONTH
                                       TO  RECEDMAKES-END-MONTH
           MOVE    RECEDMAKEQ-HKNJANUM TO  RECEDMAKES-HKNJANUM
      **** MOVE    RECEDMAKEQ-AC-DATE  TO  RECEDMAKES-AC-DATE
      **** MOVE    RECEDMAKEQ-INOUT    TO  RECEDMAKES-INOUT
      **** MOVE    RECEDMAKEQ-CREATE-MODE
      ****                             TO  RECEDMAKES-CREATE-MODE
      **** MOVE    RECEDMAKEQ-CHECK-MODE
      ****                             TO  RECEDMAKES-CHECK-MODE
      *
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "01"
               MOVE    SPA-T01-MONBLOB-PRINT-ID
                                           TO  RECEDMAKES-PRINT-ID (1)
               MOVE    SPA-T01-MONBLOB-DATA-ID
                                           TO  RECEDMAKES-DATA-ID  (1)
           END-IF
      *
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_receiptdata_makev3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  RECEDMAKES-MODE
           MOVE    "receiptdata_makev3res"
                                       TO  RECEDMAKES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-RECEIPTDATA-MAKERES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  RECEDMAKES-OBJECT      TO  APILST-BODY
               MOVE  "application/xml"     TO  APILST-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    カルテUID
           IF      RECEDMAKEQ-KARTE-UID =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   オルカUID
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "01"    OR  "00"
               CONTINUE
           ELSE
               IF      RECEDMAKEQ-ORCA-UID  =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    処理区分
           IF      RECEDMAKEQ-RECEIPT-MODE
                                       =   "02"
               CONTINUE
           ELSE
               MOVE    "E15"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    診療年月
           IF      RECEDMAKEQ-PERFORM-MONTH
                                       =   SPACE
               MOVE    "E14"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           ELSE
               MOVE    RECEDMAKEQ-PERFORM-MONTH
                                           TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
      *            診療年月をシステム日付として設定し期間情報取得
      **           IF      RECEDMAKEQ-REQUEST-NUMBER
      **                                       =   "00"
                       MOVE    WRK-SYMD (1:6)      TO  SPA-SYSYMD (1:6)
                       MOVE    "15"                TO  SPA-SYSYMD (7:2)
                       MOVE    SPA-SYSYMD          TO  WRK-SYMD
                       PERFORM 803-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-SYSYMDWH
      **           END-IF
               ELSE
                   MOVE    "E11"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    提出先
           EVALUATE    RECEDMAKEQ-SUBMISSION-MODE
               WHEN    "02"
               WHEN    "03"
               WHEN    "04"
               WHEN    "05"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "E13"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
           END-EVALUATE
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                      SPA-T01KYOUTU
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGT01-CALL-SEC
      *
      *    改正対応メッセージ
           IF      T01-MSG-VALUE   NOT =   SPACE
               ADD     1                   TO  WRK-WARNING-MAX
               MOVE    T01-MSG-VALUE       TO  WRK-WARNING-MSG
                                                   (WRK-WARNING-MAX)
               MOVE    "W99"               TO  WRK-WARNING
                                                   (WRK-WARNING-MAX)
           END-IF
      *
      *    ０点公費記載ありの点検用レセあり
           IF      SPA-GMN-TITLE   NOT =   SPACE
               ADD     1                   TO  WRK-WARNING-MAX
               MOVE    SPA-GMN-TITLE       TO  WRK-WARNING-MSG
                                                   (WRK-WARNING-MAX)
               MOVE    "W01"               TO  WRK-WARNING
                                                   (WRK-WARNING-MAX)
           END-IF
      *    未実施の未請求解除データあり
           IF      SPA-GMN-TITLE1  NOT =   SPACE
               ADD     1                   TO  WRK-WARNING-MAX
               MOVE    SPA-GMN-TITLE1      TO  WRK-WARNING-MSG
                                                   (WRK-WARNING-MAX)
               MOVE    "W02"               TO  WRK-WARNING
                                                   (WRK-WARNING-MAX)
           END-IF
      *
      *    月途中変更の診療年月のとき
           IF      SPA-GMN-TITLE2  NOT =   SPACE
               MOVE    SPA-GMN-TITLE2      TO  WRK-ERRMSG
               MOVE    "E50"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    レセ電データ作成の設定がシステム管理で設定されているか
           IF      RECEDMAKEQ-RECEIPT-MODE
                                       =   "02"
               IF    ( SPA-B06-STATE       =   WIDGET-INSENSITIVE )
               AND   ( SPA-B07-STATE       =   WIDGET-INSENSITIVE )
               AND   ( SPA-B08-STATE       =   WIDGET-INSENSITIVE )
               AND   ( SPA-B17-STATE       =   WIDGET-INSENSITIVE )
                   MOVE    "E18"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    情報取得のとき期間、直接請求する保険者番号の情報取得（医保分のみ）
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00" )
           AND   ( RECEDMAKEQ-SUBMISSION-MODE
                                   NOT =   "05" )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SRYYMKBN-MAX
                   MOVE    SPA-NAI-SRYYMKBN-ST (IDX)
                                           TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO
                                           RECEDMAKES-START-MONTH-O(IDX)
                   MOVE    SPA-NAI-SRYYMKBN-ED (IDX)
                                           TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO
                                           RECEDMAKES-END-MONTH-O  (IDX)
                   IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                           =   "1"
                       MOVE    "Ng"            TO
                                           RECEDMAKES-SELECT-MODE-O(IDX)
                   END-IF
               END-PERFORM
      *        社保のとき直接請求する保険者番号を取得
               IF      RECEDMAKEQ-SUBMISSION-MODE
                                           =   "02"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >   SPA-GMN-DIRECT-HKNJANUM-MAX
                       MOVE    SPA-GMN-DIRECT-HKNJANUML (IDX)
                                           TO  RECEDMAKES-HKNJANUM-O
                                                                   (IDX)
                       MOVE    SPA-GMN-DIRECT-HKNJANAMEL (IDX)
                                           TO  RECEDMAKES-HKNJANAME-O
                                                                   (IDX)
                   END-PERFORM
               END-IF
           END-IF
      *    情報取得時、返却情報がない場合は警告とする
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00"
               IF      ( RECEDMAKES-START-MONTH-O (1)
                                           =   SPACE )
               AND     ( RECEDMAKES-HKNJANUM-O    (1)
                                           =   SPACE )
                   ADD     1                   TO  WRK-WARNING-MAX
                   MOVE    "返却情報はありません"
                                               TO  WRK-WARNING-MSG
                                                       (WRK-WARNING-MAX)
                   MOVE    "W04"               TO  WRK-WARNING
                                                       (WRK-WARNING-MAX)
               END-IF
           END-IF
      *
      *    提出先
      *****IF    ( RECEDMAKEQ-REQUEST-NUMBER
      *****                            =   "00"  )
      *****AND   ( RECEDMAKEQ-CREATE-MODE
      *****                            =   SPACE )
      *****    CONTINUE
      *****ELSE
               EVALUATE    RECEDMAKEQ-SUBMISSION-MODE
                   WHEN    "02"
                       IF      SPA-B06-STATE   =   WIDGET-INSENSITIVE
                           MOVE    "E19"           TO  WRK-ERRCD
                       END-IF
                   WHEN    "03"
                       IF      SPA-B07-STATE   =   WIDGET-INSENSITIVE
                           MOVE    "E19"           TO  WRK-ERRCD
                       END-IF
                   WHEN    "04"
                       IF      SPA-B08-STATE   =   WIDGET-INSENSITIVE
                           MOVE    "E19"           TO  WRK-ERRCD
                       END-IF
                   WHEN    "05"
                       IF      SPA-B17-STATE   =   WIDGET-INSENSITIVE
                           MOVE    "E19"           TO  WRK-ERRCD
                       END-IF
                   WHEN    OTHER
                       MOVE    "E13"           TO  WRK-ERRCD
               END-EVALUATE
      *****END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    請求年月日
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00"  )
           AND   ( RECEDMAKEQ-AC-DATE  =   SPACE )
               CONTINUE
           ELSE
               IF      RECEDMAKEQ-AC-DATE  =   SPACE
                   MOVE    "E16"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               ELSE
                   MOVE    RECEDMAKEQ-AC-DATE  TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-SYMD            TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                                   ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    WRK-SYMD            TO  T01-SKYYMD
                   ELSE
                       MOVE    "E20"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    作成区分
      *****IF    ( RECEDMAKEQ-REQUEST-NUMBER
      *****                            =   "00"  )
      *****AND   ( RECEDMAKEQ-CREATE-MODE
      *****                            =   SPACE )
      *****    CONTINUE
      *****ELSE
               EVALUATE    RECEDMAKEQ-CREATE-MODE
                   WHEN    "Check"
                       MOVE    "2"             TO  WRK-CHK-PRTKBN
                   WHEN    OTHER
                       MOVE    "1"             TO  WRK-CHK-PRTKBN
               END-EVALUATE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                   IF        WRK-CHK-PRTKBN
                                   =   SPA-GMN-PRTKBNL (IDX)
                       MOVE    WRK-CHK-PRTKBN  TO  T01-PRTKBN
                       MOVE    SPACE           TO  WRK-CHK-PRTKBN
                       MOVE    SPA-GMN-PRTKBN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-CHK-PRTKBN  NOT =   SPACE
                   MOVE    "E21"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
      *****END-IF
      *
      *    入外区分
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00"  )
           AND   ( RECEDMAKEQ-INOUT    =   SPACE )
               CONTINUE
           ELSE
               EVALUATE    RECEDMAKEQ-INOUT
                   WHEN    "I"
                       MOVE    "1"             TO  WRK-CHK-NYUGAIKBN
                   WHEN    "O"
                       MOVE    "2"             TO  WRK-CHK-NYUGAIKBN
                   WHEN    "IO"
                   WHEN    "OI"
                       MOVE    "0"             TO  WRK-CHK-NYUGAIKBN
                  WHEN    OTHER
                       MOVE    "E12"           TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
               END-EVALUATE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
                   IF        WRK-CHK-NYUGAIKBN
                                   =   SPA-GMN-NYUGAIKBNL (IDX)
                       MOVE    WRK-CHK-NYUGAIKBN
                                               TO  T01-NYUGAIKBN
                       MOVE    SPACE           TO  WRK-CHK-NYUGAIKBN
                       MOVE    SPA-GMN-NYUGAIKBN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-CHK-NYUGAIKBN   NOT =   SPACE
                   MOVE    "E12"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    チェック区分
      *****IF    ( RECEDMAKEQ-REQUEST-NUMBER
      *****                            =   "00"  )
      *****AND   ( RECEDMAKEQ-CHECK-MODE
      *****                            =   SPACE )
      *****    CONTINUE
      *****ELSE
               EVALUATE    RECEDMAKEQ-CHECK-MODE
                   WHEN    "Yes"
                       MOVE    "1"             TO  T01-RECEDEN-CHK
                   WHEN    OTHER
                       MOVE    "0"             TO  T01-RECEDEN-CHK
               END-EVALUATE
      *****END-IF
      *
      *    期間指定（医保分のみ）
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00" )
           OR    ( RECEDMAKEQ-SUBMISSION-MODE
                                       =   "05" )
               CONTINUE
           ELSE
               IF    ( RECEDMAKEQ-START-MONTH  =   SPACE )
               AND   ( RECEDMAKEQ-END-MONTH    =   SPACE )
                   IF      SPA-GMN-SRYYMKBN-MAX    >   ZERO
                       MOVE    "E22"           TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   IF      SPA-GMN-SRYYMKBN-MAX    =   ZERO
                       MOVE    "E23"           TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   ELSE
                       MOVE    RECEDMAKEQ-START-MONTH
                                               TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       IF      WRK-SYMD (1:6)  =   "999999"   OR
                                                   "000000"
                           MOVE    WRK-SYMD (1:6)  TO  WRK-START-MONTH
                       ELSE
                           INITIALIZE                  ORCSGDAYAREA
                           MOVE    "1"             TO  SGDAY-INTYPE
                           MOVE    WRK-SYMD (1:6)  TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC        =   ZERO
                               MOVE    WRK-SYMD (1:6)  TO
                                                       WRK-START-MONTH
                           ELSE
                               MOVE    "E25"           TO  WRK-ERRCD
                               GO  TO  2001-INPUT-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    RECEDMAKEQ-END-MONTH
                                                   TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       IF      WRK-SYMD (1:6)  =   "999999"   OR
                                                   "000000"
                           MOVE    WRK-SYMD (1:6)  TO  WRK-END-MONTH
                       ELSE 
                           INITIALIZE                  ORCSGDAYAREA
                           MOVE    "1"             TO  SGDAY-INTYPE
                           MOVE    WRK-SYMD (1:6)  TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC        =   ZERO
                               MOVE    WRK-SYMD (1:6)  TO  WRK-END-MONTH
                           ELSE
                               MOVE    "E25"           TO  WRK-ERRCD
                               GO  TO  2001-INPUT-CHK-EXT
                           END-IF
                       END-IF
      *
                       MOVE    SPACE           TO  WRK-CHK-SRYYMKBN
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   SPA-GMN-SRYYMKBN-MAX
                           IF    ( WRK-START-MONTH
                                       =   SPA-NAI-SRYYMKBN-ST (IDX) )
                           AND   (  WRK-END-MONTH
                                       =   SPA-NAI-SRYYMKBN-ED (IDX) )
                               MOVE    SPA-GMN-SRYYMKBNL (IDX)
                                                   TO  WRK-CHK-SRYYMKBN
                               MOVE    SPA-GMN-SRYYMKBN-MAX
                                                   TO  IDX
                           END-IF
                       END-PERFORM
                       IF      WRK-CHK-SRYYMKBN    =   SPACE
                           MOVE    "E24"           TO  WRK-ERRCD
                           GO  TO  2001-INPUT-CHK-EXT
                       ELSE
                           MOVE    WRK-CHK-SRYYMKBN
                                                   TO  T01-SRYYMKBN
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    直接請求する保険者（社保のみ）
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00" )
           OR    ( RECEDMAKEQ-SUBMISSION-MODE
                                   NOT =   "02" )
               CONTINUE
           ELSE
               EVALUATE    RECEDMAKEQ-HKNJANUM
                   WHEN    SPACE
                       IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                                   >   ZERO
                           MOVE    "E26"           TO  WRK-ERRCD
                           GO  TO  2001-INPUT-CHK-EXT
                       END-IF
                   WHEN    OTHER
                       IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                                   =   ZERO
                           MOVE    "E27"           TO  WRK-ERRCD
                           GO  TO  2001-INPUT-CHK-EXT
                       ELSE
                           MOVE    RECEDMAKEQ-HKNJANUM
                                                   TO
                                               WRK-CHK-DIRECT-HKNJANUM
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >
                                           SPA-GMN-DIRECT-HKNJANUM-MAX
                               IF        WRK-CHK-DIRECT-HKNJANUM
                                       =   SPA-GMN-DIRECT-HKNJANUML(IDX)
                                   MOVE    WRK-CHK-DIRECT-HKNJANUM
                                                   TO
                                                   T01-DIRECT-HKNJANUM
                                   MOVE    SPACE   TO
                                               WRK-CHK-DIRECT-HKNJANUM
                                   MOVE    SPA-GMN-DIRECT-HKNJANUM-MAX
                                                   TO  IDX
                               END-IF
                           END-PERFORM
                           IF      WRK-CHK-DIRECT-HKNJANUM
                                                   NOT =   SPACE
                               MOVE    "E17"           TO  WRK-ERRCD
                               GO  TO  2001-INPUT-CHK-EXT
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    更新前チェック
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-T01KYOUTU
      *
           MOVE    RECEDMAKEQ-KARTE-UID
                                   TO  SPA-T01-JOB-KARTE-UID
           MOVE    RECEDMAKEQ-ORCA-UID
                                   TO  SPA-T01-JOB-ORCA-UID
      *
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
           IF    ( RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00"  )
           AND   ( RECEDMAKEQ-SUBMISSION-MODE
                                       =   SPACE )
               CONTINUE
           ELSE
               EVALUATE    RECEDMAKEQ-SUBMISSION-MODE
                   WHEN    "02"
                       MOVE    "B06"           TO  MCP-WIDGET
                   WHEN    "03"
                       MOVE    "B07"           TO  MCP-WIDGET
                   WHEN    "04"
                       MOVE    "B08"           TO  MCP-WIDGET
                   WHEN    "05"
                       MOVE    "B17"           TO  MCP-WIDGET
               END-EVALUATE
           END-IF
      *
           PERFORM 800-ORCGT01-CALL-SEC
      *??
           DISPLAY "CHK SPA-ERRCD=" SPA-ERRCD "#" SPA-TID1-AREA "#"
      *??
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "0003"
                       MOVE    "E11"               TO  WRK-ERRCD
                   WHEN    "0004"
                       MOVE    "E20"               TO  WRK-ERRCD
      *************WHEN    "0005"
      *************    MOVE    "E17"               TO  WRK-ERRCD
                   WHEN    "7777"
                       MOVE    "E70"               TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                       IF      RECEDMAKEQ-REQUEST-NUMBER
                                                   =   "02"
      *                    ジョブ管理チェック処理
                           MOVE    SPACE           TO  SYS-3003-REC 
                           INITIALIZE                  SYS-3003-REC
                           MOVE    SPA-HOSPNUM     TO  SYS-3003-HOSPNUM
                           MOVE    "3003"          TO  SYS-3003-KANRICD
                           MOVE    WRK-CONS-3003-KBNCD
                                                   TO  SYS-3003-KBNCD
                           MOVE    SPA-NAI-SKYYM   TO  SYS-3003-STYUKYMD
                                                                   (1:6)
                           MOVE    "01"            TO  SYS-3003-STYUKYMD
                                                                   (7:2)
                           MOVE    SYS-3003-STYUKYMD
                                                   TO  SYS-3003-EDYUKYMD 
                           MOVE    SYS-3003-REC    TO  MCPDATA-REC
                           PERFORM 900-SYSKANRI-READ-SEC
                           IF      FLG-SYSKANRI    =   ZERO
                               MOVE    MCPDATA-REC TO  SYS-3003-REC 
                               PERFORM VARYING IDX FROM    1   BY  1
                                       UNTIL   IDX >   SYS-3003-JOBID
                                   MOVE    "CHK"       TO
                                                       SJOBKANRI-MODE
                                   INITIALIZE          JOBKANRI-REC
                                   MOVE    IDX         TO  JOB-JOBID
                                   MOVE    WRK-CONS-JOB-SHELLID
                                                       TO  JOB-SHELLID
                                   PERFORM 900-CALL-ORCSJOB-SEC
                                   IF      SJOBKANRI-RETURN
                                                       =   ZERO
                                       IF    ( RECEDMAKEQ-KARTE-UID
                                               NOT =   JOB-KARTE-UID )
                                       OR    ( RECEDMAKEQ-ORCA-UID
                                               NOT =   JOB-ORCA-UID  )
                                           MOVE    "E41"   TO  WRK-ERRCD
                                           GO  TO  2001-INPUT-CHK-EXT
                                       END-IF
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-IF
                   WHEN    "0006"
                   WHEN    "0050"
                       IF      RECEDMAKEQ-REQUEST-NUMBER
                                                   =   "00"
                           CONTINUE
                       ELSE
                           MOVE    "E60"               TO  WRK-ERRCD
                       END-IF
                   WHEN    "0051"
                       IF      RECEDMAKEQ-SUBMISSION-MODE
                                                   =   "05"
                           CONTINUE
                       ELSE
                           MOVE    "E60"               TO  WRK-ERRCD
                       END-IF
                   WHEN    OTHER
                       MOVE    "E60"               TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ作成処理
      *****************************************************************
       2002-RECEIPTDATA-MAKE-SEC       SECTION.
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  RECEDMAKEQ-ORCA-UID
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-RECEIPTDATA-MAKE-EXT
           END-IF
      *??
           DISPLAY "MAKE SPA-ERRCD=" SPA-ERRCD "#" SPA-TID1-AREA "#"
      *??
      *
           MOVE    RECEDMAKEQ-KARTE-UID
                                   TO  SPA-T01-JOB-KARTE-UID
           MOVE    RECEDMAKEQ-ORCA-UID
                                   TO  SPA-T01-JOB-ORCA-UID
      *
           PERFORM 800-TEMPDIR-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2002-RECEIPTDATA-MAKE-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-TID1-AREA
      *
           MOVE    "TID2"          TO  SPA-MOTOPG
           MOVE    "NO"            TO  SPA-TID1-FLG
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           PERFORM 800-ORCGT01-CALL-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD           =   "E88"
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               ELSE
                   MOVE    "E62"               TO  WRK-ERRCD
               END-IF
               GO  TO  2002-RECEIPTDATA-MAKE-EXT
           END-IF
      *
      *    レセ電処理後の印刷処理（ORCGT99の4903-PRINT-SECにあわせる）
           PERFORM 20022-PRINT-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "E62"               TO  WRK-ERRCD
               GO  TO  2002-RECEIPTDATA-MAKE-EXT
           END-IF
      *
      *    monblobapi処理
           PERFORM 20023-MONBLOBAPI-SEC
      *
           .
       2002-RECEIPTDATA-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電処理後の印刷処理（ORCGT99 4903-PRINT-SECにあわせる）
      *****************************************************************
       20022-PRINT-SEC            SECTION.
      *
      *    ジョブ開始セット処理
           ADD     1               TO  SPA-T01-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-T01-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-T01-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-T01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-T01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
           PERFORM 900-CALL-ORCSJOB-SEC
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *      
           MOVE    SPA-LNK-PRTKANRI-AREA
                                   TO  LNK-PRTKANRI-AREA
      *
      *    印刷処理
           PERFORM 49031-PRINT-SYORI-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 49032-JIBID-UPD-SEC
      *
      *    印刷データ削除処理
           MOVE    SPACE               TO  SHELLTBL
      *
           MOVE    "prtdbdel.sh"       TO  SHELLTBL-NAME
           MOVE    SPA-T01-JOB-JOBID   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG3
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "monthly"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    9               TO  SPA-T01-SYORIFLG
      *
           .
       20022-PRINT-EXT.
           EXIT. 
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       49031-PRINT-SYORI-SEC           SECTION.
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    99999               TO  WRK-EDPAGE
      *             
           MOVE    SPACE               TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    SPA-T01-TBL-GROUP   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    SPA-NAI-SKYYM       TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-CONS-TERMID     TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *             
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
           MOVE    SPA-T01-JOB-JOBID   TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
           MOVE    "7"                 TO  SHELLTBL-ARG17
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           .
       49031-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       49032-JIBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
           ELSE       
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           END-IF    
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    SPA-T01-JOB-JOBID   TO  SYS-3003-JOBID                                      
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF         
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       49032-JIBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       20021-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       20021-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBLOBAPI処理
      *****************************************************************
       20023-MONBLOBAPI-SEC        SECTION.
      *
      *    ジョブ開始セット処理
           ADD     1               TO  SPA-T01-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-T01-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-T01-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-T01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-T01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
           PERFORM 900-CALL-ORCSJOB-SEC
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           MOVE    SPA-LNK-PRTKANRI-AREA
                                   TO  LNK-PRTKANRI-AREA
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "haori_monblobapi.sh"
                                       TO  SHELLTBL-NAME
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    SPA-T01-TBL-GROUP   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    SPA-NAI-SKYYM       TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-NAI-SKYYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "haori_monblobapi.sh"
                                       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-CONS-TERMID     TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
           MOVE    SPA-T01-JOB-JOBID   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                       TO  SHELLTBL-ARG15
           MOVE    SPA-T01-MONBLOB-PRINT-ID
                                       TO  SHELLTBL-ARG16
           MOVE    "receiptdata_make"  TO  SHELLTBL-ARG17
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 49032-JIBID-UPD-SEC
      *
           .
       20023-MONBLOBAPI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ作成確認処理
      *****************************************************************
       2003-RECEIPTDATA-CHECK-SEC      SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF    ( RECEDMAKEQ-KARTE-UID
                                           NOT =   JOB-KARTE-UID )
                       OR    ( RECEDMAKEQ-ORCA-UID
                                           NOT =   JOB-ORCA-UID  )
                           MOVE    "E41"           TO  WRK-ERRCD
                           GO  TO  2003-RECEIPTDATA-CHECK-EXT
                       END-IF
      *
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "E70"           TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMSG
                           STRING  "処理中です【"  DELIMITED   BY  SIZE
                                   JOB-SHELLMSG    DELIMITED   BY  SPACE
                                   "】"            DELIMITED   BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                           GO  TO  2003-RECEIPTDATA-CHECK-EXT
                       END-IF
                   ELSE
                       MOVE    "E40"           TO  WRK-ERRCD
                       GO  TO  2003-RECEIPTDATA-CHECK-EXT
                   END-IF
      *
                   IF      IDX             =   1
                       MOVE    JOBKANRI-REC    TO  WRK-JOBKANRI-REC
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-JOB-UPDCNT2     >   ZERO
      *        対象レセプトあり
               MOVE    WRK-JOB-UPDCNT2 TO  WRK-9
               MOVE    WRK-X2          TO  RECEDMAKES-ALL-COUNT
               MOVE    WRK-JOB-UPDCNT  TO  WRK-9
               MOVE    WRK-X2          TO
                                       RECEDMAKES-ALL-NUMBER-OF-SHEETS
               IF      WRK-JOB-UPDCNT1 >   ZERO
      *            チェックリストあり
                   ADD     1                   TO  WRK-WARNING-MAX
                   MOVE    "【チェックリストが作成されました】"
                                               TO  WRK-WARNING-MSG
                                                       (WRK-WARNING-MAX)
                   MOVE    "W03"           TO  WRK-WARNING
                                                       (WRK-WARNING-MAX)
               END-IF
           ELSE
               MOVE    "E31"           TO  WRK-ERRCD
               MOVE    WRK-JOB-YOBI    TO  WRK-ERRMSG
           END-IF
      *
           .
       2003-RECEIPTDATA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "E70"           TO  WRK-ERRCD
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
           IF      WRK-ERRCD       =   SPACE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                       TO  JOB-SHELLID
               PERFORM 900-CALL-ORCSJOB-SEC
               IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "E70"           TO  WRK-ERRCD
                       STRING  "レセプト作成処理中です【"
                                               DELIMITED  BY  SIZE
                               JOB-SHELLMSG    DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ作成処理
      *****************************************************************
       800-ORCGT01-CALL-SEC                SECTION.
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
      *
           CALL    "ORCGT01"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGT01-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ディレクトリ作成処理
      *****************************************************************
       800-TEMPDIR-SEC             SECTION.
      *
           MOVE    SPA-TERMID      TO  WRK-SPA-TERMID
      *
      *    一時ディレクトリ作成
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"   USING
                                       STEMPDIR-AREA
      *
           MOVE    WRK-SPA-TERMID  TO  SPA-TERMID
           .
       800-TEMPDIR-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換処理
      *****************************************************************
       803-HIZUKE-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG3
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE OR
                                           ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
               EVALUATE    WRK-SYMD (1:6)
                   WHEN    "000000"
                       MOVE    "００００００００"  TO  WRK-HENYMDG3
                   WHEN    "999999"
                       MOVE    "９９９９９９９９"  TO  WRK-HENYMDG3
               END-EVALUATE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-IF
      *
           .
       803-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E50"
      *            月途中変更の診療年月
               WHEN    "E70"
      *            レセプト作成、定期請求の処理中
                   GO  TO  890-ERRCD-MSG-EXT
               WHEN    "E30"
               WHEN    "E31"
               WHEN    "E32"
      *            レセプト作成結果
                   GO  TO  890-ERRCD-MSG-EXT
               WHEN    "E63"
      *            個別対象患者エラー
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      * 
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E01"
                   MOVE    "患者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E04"
                   MOVE    "電子カルテＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "オルカＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E11"
                   MOVE    "診療年月が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "入外区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "提出先の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E14"
                   MOVE    "診療年月が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "処理区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "請求年月日が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "保険者番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E18"
                   MOVE    "レセ電データ作成の設定が行われていません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E19"
                   MOVE    "レセ電データ作成の設定が行われていません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "請求年月日が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                   MOVE    "作成区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE    "期間指定が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E23"
               WHEN    "E24"
                   MOVE    "期間指定の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E25"
                   MOVE    "期間指定が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E26"
                   MOVE    "保険者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E27"
                   MOVE    "保険者番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
       *
               WHEN    "E40"
                   MOVE    "対象のジョブ管理がありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E41"
                   MOVE    "対象の処理はありません。"
                                                   TO  WRK-ERRMSG
      *
               WHEN    "E60"
               WHEN    "E61"
               WHEN    "E62"
      *            画面エラー
               WHEN    "E88"
      *            ＭＯＮＢＬＯＢ取得エラー
                   MOVE    SPA-ERRMSG              TO  WRK-ERRMSG
      *
               WHEN    "E80"
                   MOVE    "一時ディレクトリが取得できません"
                                               TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
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
