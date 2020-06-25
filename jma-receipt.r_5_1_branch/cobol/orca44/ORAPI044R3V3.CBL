     ******************************************************************
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
       PROGRAM-ID.         ORAPI044R3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 個別レセ電データ作成(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/06/27    NaCl−藤原    新規作成
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
           COPY    "T01SCR-SPA"    REPLACING  //SPA-NAI-200501-//
                                   BY         //SPA-NAI-25-//.
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-T1-LK-//.
      *
           COPY    "T06SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-PATIENT-ERR         PIC 9(01).
           03  FLG-PATIENT-SEL         PIC 9(01).
           03  FLG-DETAIL-CHK          PIC 9(01).
           03  FLG-RECEIPT-CHK         PIC 9(01).
           03  FLG-SELECT-ERR          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDVV                    PIC 9(04).
           03  IDWW                    PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
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
      *
           03 WRK-SYSYM-2              PIC 9(06).
      *
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WARNING-INFO.
               05  WRK-WARNING-OCC         OCCURS 5.
                   07  WRK-WARNING     PIC X(03).
                   07  WRK-WARNING-MSG PIC X(100).
               05  WRK-WARNING-MAX     PIC 9(02).
      *
           03  WRK-ERR-PATIENT-MAX     PIC 9(03).
           03  WRK-S-PATIENT-MAX       PIC 9(03).
           03  WRK-S-DETAIL-MAX        PIC 9(03).
           03  WRK-S-RECEIPT-MAX       PIC 9(03).
      *
      *    ワーニング患者一覧
           03  WRK-WARNING-INFORMATION.
               05  WRK-WARNING-PATIENT-MAX PIC 9(03).
               05  WRK-WARNING-INFORMATION-OCC OCCURS  100.
                   07  WRK-WARNING-PATIENT-ID  PIC X(20).
                   07  WRK-WARNING-PERFORM-DATE
                                               PIC X(7).
                   07  WRK-WARNING-ERROR-CODE  PIC X(3).
                   07  WRK-WARNING-ERROR-MSG   PIC X(100).
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
      ***  03  WRK-TIDCD               PIC X(04).
      *
           03  ST-IDX                  PIC 9(02).
           03  ED-IDX                  PIC 9(02).
           03  WRK-T09-SELNUM          PIC 9(02).
           03  WRK-SELNUM              PIC 9(02).
           03  WRK-DETAIL-ERR          PIC 9(02).
           03  WRK-DETAIL-IDX          PIC 9(02).
           03  WRK-DETAIL-TBL.
               05  WRK-DETAIL-MAX      PIC 9(02).
               05  WRK-DETAIL-OCC      OCCURS  5.
                   07  WRK-DETAIL-RECEIPT-PERFORM-FLAG
                                           PIC X(04).
                   07  WRK-DETAIL-NYUGAIKBN
                                           PIC X(01).
                   07  WRK-DETAIL-RECESYUBETU
                                           PIC X(04).
                   07  WRK-DETAIL-HKNID    PIC 9(10).
                   07  WRK-DETAIL-HKNNUM   PIC X(03).
                   07  WRK-DETAIL-HKNJANUM PIC X(08).
                   07  WRK-DETAIL-HOJOKBN  PIC X(01).
                   07  WRK-DETAIL-KIGO     PIC X(40).
                   07  WRK-DETAIL-NUM      PIC X(40).
                   07  WRK-DETAIL-DISASTER PIC X(04).
                   07    WRK-DETAIL-INCARE-CLASS
                                           PIC X(04).
                   07  WRK-DETAIL-KOH-OCC  OCCURS  4.
                       09  WRK-DETAIL-KOHNUM
                                           PIC X(03).
                       09  WRK-DETAIL-KOHID
                                           PIC 9(10).
                       09  WRK-DETAIL-FTAJANUM
                                           PIC X(08).
                       09  WRK-DETAIL-JKYSNUM
                                           PIC X(20).
                   07  WRK-DETAIL-HKNKBN   PIC X(01).
                   07  WRK-DETAIL-SAIGAIKBN
                                           PIC X(01).
                   07  WRK-DETAIL-KOFUNUM  PIC X(14).
      *
           03  WRK-T09-TBL.
      *        対象となるレセプト
               05  WRK-T09-SEL-OCC     OCCURS  20.
                   07  WRK-T09-CHK         PIC 9(02).
                   07  WRK-T09-FLAG        PIC X(04).
                   07  WRK-T09-DET-IDX     PIC 9(02).
      *        対象となるレセプト件数
               05  WRK-T09-DET-OCC     OCCURS  30.
                   07  WRK-T09-CNT         PIC 9(02).
                   07  WRK-T09-IDX-OCC.
                       09  WRK-T09-IDX     PIC 9(02)   OCCURS  20.
      *
           03  WRK-NUM-X.
               05  WRK-NUM             PIC 9(03).
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
      *    個別対象患者一覧
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *    個別対象−レセプト情報
           03  WRK-CONS-DETAIL-MAX PIC 9(03)   VALUE   5.
      *
      *    エラー一覧、選択一覧の最大値
           03  WRK-CONS-OVERFLOW   PIC 9(03)   VALUE   100.
           03  WRK-CONS-DETAIL-OVERFLOW
                                   PIC 9(03)   VALUE   20.
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
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPTDATA-PATIENT-MAKEV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPTDATA-PATIENT-MAKEV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
           COPY    "CPORCSTEMPDIR.INC".
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
           DISPLAY   "**********************************"
           DISPLAY   "* receiptdata_patient_Make start *"
           DISPLAY   "**********************************"
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
               MOVE   404                  TO  APILST03-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************************"
           DISPLAY   "*  receiptdata_patient_Make end  *"
           DISPLAY   "**********************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-RECEIPTDATA-PATIENT-MAKERES
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
      *
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  RECEDMAKES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  RECEDMAKES-INFORMATION-TIME
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
               MOVE   "E89"                TO  WRK-ERRCD
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
                   PERFORM 2002-RECEIPTDATA-CHECK-MAKE-SEC
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
           INITIALIZE                  XML-APIREQ
           INITIALIZE                  XML-RECEIPTDATA-PATIENT-MAKEREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_receiptdata_patient_makev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "receiptdata_patient_makev3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO
                                       XML-RECEIPTDATA-PATIENT-MAKEREQ
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
           IF      APILST03-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_receiptdata_patient_makev3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  RECEDMAKES-MODE
           MOVE    "receiptdata_patient_makev3res"
                                       TO  RECEDMAKES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-RECEIPTDATA-PATIENT-MAKERES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  RECEDMAKES-OBJECT     TO  APILST03-BODY
               MOVE  "application/xml"     TO  APILST03-CONTENT-TYPE
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
      *    システム日付−２ヵ月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE  "61"                  TO  LNK-DAY6-IRAI
           MOVE  SPA-SYSYMD            TO  LNK-DAY6-KIJUN
           MOVE  "2"                   TO  LNK-DAY6-ZOGENPTN
           MOVE  -2                    TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  WRK-SYSYM-2
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
      *
      *    システム日付の二ヶ月前以前は処理不可 
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "01"    OR  "00"
               IF      WRK-SYSYM-2         >=  SPA-NAI-SKYYM
                   MOVE    "E30"               TO  WRK-ERRCD
                  GO  TO  2001-INPUT-CHK-EXT
               END-IF
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
           IF    ( SPA-B16-STATE       =   WIDGET-INSENSITIVE )
               MOVE    "E18"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    提出先
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
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
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
      *    チェック区分
           EVALUATE    RECEDMAKEQ-CHECK-MODE
               WHEN    "Yes"
                   MOVE    "1"             TO  T01-RECEDEN-CHK
               WHEN    OTHER
                   MOVE    "0"             TO  T01-RECEDEN-CHK
           END-EVALUATE
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
      *
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "00"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
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
      *    画面遷移前チェック
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
           MOVE    "B16"           TO  MCP-WIDGET
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
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      RECEDMAKEQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    個別作成チェック
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                      SPA-T01KYOUTU-H
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGT06-CALL-SEC
      *
           MOVE    SPA-GMN-SRYYMKBN-G  TO  SPA-GMN-T06-SRYYMKBN-G
           MOVE    SPA-NAI-SRYYMKBN-SELNUM
                                       TO  SPA-NAI-T06-SRYYMKBN-SELNUM
           MOVE    SPA-GMN-SKYYMD      TO  SPA-GMN-T06-SRYYM-H
           MOVE    SPA-NAI-SKYYMD      TO  SPA-NAI-T06-SKYYMD-H
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI1
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI2
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI3
           MOVE    "F"                 TO  SPA-GMN-T06-TEISYUTUSAKI4
           EVALUATE    RECEDMAKEQ-SUBMISSION-MODE
               WHEN    "02"
                   MOVE    "T"             TO  SPA-GMN-T06-TEISYUTUSAKI1
               WHEN    "03"
                   MOVE    "T"             TO  SPA-GMN-T06-TEISYUTUSAKI2
               WHEN    "04"
                   MOVE    "T"             TO  SPA-GMN-T06-TEISYUTUSAKI3
               WHEN    "05"
                   MOVE    "T"             TO  SPA-GMN-T06-TEISYUTUSAKI4
           END-EVALUATE
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       WRK-CONS-LINE-MAX
                   OR (    RECEDMAKEQ-PATIENT-ID    (IDZZ)
                                   =   SPACE
                       AND RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                   =   SPACE
                       AND RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                   =   SPACE )
               INITIALIZE              SPA-T01KYOUTU-H
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
                                           SPA-ERRCD
                                           SPA-ERRMSG
               MOVE    ZERO            TO  WRK-T09-SELNUM
                                           FLG-PATIENT-ERR
                                           FLG-PATIENT-SEL
                                           WRK-DETAIL-ERR
                                           WRK-SELNUM
                                           WRK-S-DETAIL-MAX
                                           WRK-S-RECEIPT-MAX
               INITIALIZE                  WRK-DETAIL-TBL
                                           WRK-T09-TBL
               INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-RECE-KOUMOKU
                                           SPA-NAI-T06-BODY-SELNUM
                                           SPA-T09-AREA
      *??
      *        display "IDZZ=" idzz "#"
      *??
      *
      *        個別指定の重複チェック
               IF      IDZZ            >   1
                   COMPUTE IDZ =   IDZZ    -   1
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       IDZ
                       IF    ( RECEDMAKEQ-PATIENT-ID    (IDZZ)
                                   =   RECEDMAKEQ-PATIENT-ID    (IDYY) )
                       AND   ( RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                   =   RECEDMAKEQ-PATIENT-INOUT (IDYY) )
                       AND   ( RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                   =   RECEDMAKEQ-PATIENT-MONTH (IDYY) )
                           MOVE    "E029"              TO  SPA-ERRCD
                           MOVE    "同じ内容で設定してあります。"
                                                       TO  SPA-ERRMSG
                           MOVE    WRK-CONS-LINE-MAX   TO  IDYY
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
      *            患者番号
                   IF      RECEDMAKEQ-PATIENT-ID (IDZZ)
                                               =   SPACE
                       MOVE    "E001"              TO  SPA-ERRCD
                       MOVE    "患者番号が未設定です"
                                                   TO  SPA-ERRMSG
                   ELSE
                       MOVE    RECEDMAKEQ-PATIENT-ID (IDZZ)
                                                   TO  T06-PTNUM
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
      *            入外区分
                   IF    ( RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                           =   SPACE )
                   AND   ( SPA-BEDSU       =   ZERO  )
      *                病床数０で設定無しのとき、入院外を設定
                       MOVE    "O"             TO
                                       RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                   END-IF
                   IF      RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                           =   SPACE
                       MOVE    "E028"          TO  SPA-ERRCD
                       MOVE    "入外区分が未設定です"
                                               TO  SPA-ERRMSG
                   ELSE
      *                病床数０で設定無しのとき、入院を設定
                       IF      SPA-BEDSU       =   ZERO
                           IF      RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                                   =   "O"
                               CONTINUE
                           ELSE
                               MOVE    "0001"          TO  SPA-ERRCD
                               MOVE
                                   "入外区分の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           END-IF
                       END-IF
                       IF      SPA-ERRCD       =   SPACE
                           EVALUATE    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                               WHEN    "IO"
                               WHEN    "OI"
                                   MOVE    SPACE       TO  T06-NYUGAIKBN
                               WHEN    "I"
                                   MOVE    "1"         TO  T06-NYUGAIKBN
                               WHEN    "O"
                                   MOVE    "2"         TO  T06-NYUGAIKBN
                               WHEN    OTHER
                                   MOVE    "0001"      TO  SPA-ERRCD
                                   MOVE
                                   "入外区分の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
      *            診療年月
                   IF      RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                               =   SPACE
                       MOVE    "E032"              TO  SPA-ERRCD
                       MOVE    "診療年月が未設定です"
                                                   TO  SPA-ERRMSG
                   ELSE
                       MOVE    RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                                   TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       MOVE    WRK-SYMD (1:6)      TO  T06-SRYYM
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   MOVE    SPACE           TO  MCP-STATUS
                   MOVE    "ACTIVATE"      TO  MCP-EVENT
                   MOVE    SPACE           TO  MCP-WIDGET
      *
                   PERFORM 800-ORCGT06-CALL-SEC
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
      *            個別対象−レセプト情報チェック処理
                   PERFORM 20011-PATIENT-DETAIL-CHK-SEC
               ELSE
                   IF      SPA-GMN-T09-MAX >   ZERO
                       MOVE    1           TO  WRK-T09-SELNUM
                   END-IF
                   PERFORM 20012-PATIENT-ERR-SEC
               END-IF
      *
      *??
      *   DISPLAY "2001 SPA-TIDCD=" IDZZ "#" SPA-ERRCD "#" SPA-TIDCD "#"
      *                 SPA-GMN-T09-MAX "#" WRK-DETAIL-MAX "#"
      *??
               IF      SPA-ERRCD       =   SPACE
                   EVALUATE    SPA-TIDCD
                       WHEN    "1002"
                       WHEN    "1006"
                       WHEN    "1011"
                       WHEN    "1012"
      *                    警告時の対象処理
                           IF      WRK-DETAIL-MAX  =   ZERO
                               MOVE    "E56"       TO  SPA-ERRCD
                               PERFORM 20012-PATIENT-ERR-SEC
      *                        個別対象患者選択一覧処理
                               MOVE    1           TO  WRK-T09-SELNUM
                               PERFORM 20013-PATIENT-SELECT-SEC
                           ELSE
      *                        個別対象一覧に存在するか
      *************************INITIALIZE          SPA-T01KYOUTU-H
                               MOVE    SPACE       TO  LINKAREA
                                                       SPA-MOTOPG
                                                       SPA-ERRCD
                                                       SPA-ERRMSG
                               IF    ( WRK-T09-CHK  (1)    =   1      )
                               AND   ( WRK-T09-FLAG (1)    =   "True" )
                                   MOVE    "OK"        TO  SPA-TID1-FLG
                               ELSE
                                   MOVE    "NO"        TO  SPA-TID1-FLG
                               END-IF
      *
                               MOVE    "TID1"      TO  SPA-MOTOPG
      *
                               MOVE    "LINK"      TO  MCP-STATUS
                               MOVE    SPACE       TO  MCP-EVENT
      *
                               PERFORM 800-ORCGT06-CALL-SEC
                           END-IF
                       WHEN    OTHER
                           IF      SPA-GMN-T09-MAX >   1
                               IF      WRK-DETAIL-MAX       =   ZERO
                                   MOVE    "E55"   TO  SPA-ERRCD
                                   MOVE    ZERO    TO  WRK-T09-SELNUM
                                   PERFORM 20012-PATIENT-ERR-SEC
                               END-IF
      *                        個別チェック
                               PERFORM VARYING IDZ FROM    1   BY  1
                                       UNTIL   IDZ >   SPA-GMN-T09-MAX
      *                          個別対象一覧に存在するか個別対象一覧の設定がないとき
                                 IF    ( WRK-T09-CHK (IDZ)    =   1    )
                                 OR    ( WRK-DETAIL-MAX       =   ZERO )
                                   INITIALIZE          SPA-T01KYOUTU-H
                                   MOVE    SPACE       TO  SPA-ERRCD
                                                           SPA-ERRMSG
                                   COMPUTE IDYY    =   WRK-SELNUM  +   1
                                   MOVE    IDZ         TO  T09-SELNUM
                                                               (IDYY)
                                   MOVE    SPACE       TO  MCP-STATUS
                                   MOVE    "ACTIVATE"  TO  MCP-EVENT
                                   EVALUATE    IDYY
                                       WHEN    1
                                           MOVE    "SELNUM01"
                                                          TO  MCP-WIDGET
                                       WHEN    2
                                           MOVE    "SELNUM02"
                                                          TO  MCP-WIDGET
                                       WHEN    3
                                           MOVE    "SELNUM03"
                                                          TO  MCP-WIDGET
                                       WHEN    4
                                           MOVE    "SELNUM04"
                                                          TO  MCP-WIDGET
                                       WHEN    5
                                           MOVE    "SELNUM05"
                                                          TO  MCP-WIDGET
                                   END-EVALUATE
      *
                                   PERFORM 800-ORCGT09-CALL-SEC
                                   IF      SPA-ERRCD   NOT =   SPACE
                                       MOVE    IDZ     TO
                                                       WRK-T09-SELNUM
                                       IF      WRK-T09-CHK (IDZ)   =   1
                                           MOVE    WRK-T09-DET-IDX (IDZ)
                                                   TO  WRK-DETAIL-ERR
                                       END-IF
                                       PERFORM 20012-PATIENT-ERR-SEC
                                   ELSE
                                       EVALUATE    SPA-TIDCD
                                           WHEN    "1002"
                                           WHEN    "1006"
                                           WHEN    "1011"
                                           WHEN    "1012"
                                               IF      WRK-T09-FLAG(IDZ)
                                                           =   "True"
      *                                            警告分を対象とする
                                                   MOVE    SPACE
                                                       TO  SPA-ERRCD
                                                           SPA-ERRMSG
                                                   MOVE    "OK"
                                                       TO  SPA-TID1-FLG
                                                   MOVE    "TID1"
                                                       TO  SPA-MOTOPG
      *
                                                   MOVE    "LINK"
                                                       TO  MCP-STATUS
                                                   MOVE    SPACE
                                                      TO  MCP-EVENT
      *
                                                   PERFORM
                                                   800-ORCGT09-CALL-SEC
                                                   MOVE    IDYY    TO
                                                           WRK-SELNUM
                                               ELSE
                                                   IF  WRK-DETAIL-MAX
                                                               =   ZERO
                                                       MOVE    ZERO  TO
                                                          FLG-SELECT-ERR
                                                   ELSE
                                                       MOVE    2     TO
                                                          FLG-SELECT-ERR
                                                   END-IF
                                                   MOVE    IDZ TO
                                                          WRK-T09-SELNUM
      *                                            個別対象患者選択一覧処理
                                                   PERFORM
                                               20013-PATIENT-SELECT-SEC
                                               END-IF
                                           WHEN    OTHER
                                               IF      WRK-DETAIL-MAX
                                                               =   ZERO
                                                   MOVE    ZERO    TO
                                                          FLG-SELECT-ERR
                                                   MOVE    IDZ     TO
                                                          WRK-T09-SELNUM
      *                                            個別対象患者選択一覧処理
                                                   PERFORM
                                               20013-PATIENT-SELECT-SEC
                                               ELSE
                                                   MOVE    IDYY    TO
                                                              WRK-SELNUM
                                               END-IF
                                       END-EVALUATE
                                   END-IF
                                 END-IF
                               END-PERFORM
      *
      *                        選択がある場合対象とする
                               IF      WRK-SELNUM    >   ZERO
                                 INITIALIZE          SPA-T01KYOUTU-H
                                 MOVE    SPACE       TO  LINKAREA
                                                         SPA-MOTOPG
                                                         SPA-ERRCD
                                                         SPA-ERRMSG
      *
                                 MOVE    "OK"        TO  SPA-TID1-FLG
                                 MOVE    "T09"       TO  SPA-MOTOPG
      *
                                 MOVE    "LINK"      TO  MCP-STATUS
                                 MOVE    SPACE       TO  MCP-EVENT
      *
                                 PERFORM 800-ORCGT06-CALL-SEC
                               END-IF
                           END-IF
                   END-EVALUATE
               END-IF
      *??
      *    DISPLAY "SPA-GMN-T06-RECE-MAX=" SPA-GMN-T06-RECE-MAX "#" 
      *??
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF    ( WRK-ERRCD       NOT =   SPACE )
           OR    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    オンラインの変わりに追加処理後のクリア処理を行う
           INITIALIZE                  SPA-GMN-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-RECE-KOUMOKU
                                       SPA-NAI-T06-BODY-SELNUM
                                       SPA-T09-AREA
      *    レセ電データ作成前チェック
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-ERRCD
           INITIALIZE                  SPA-T01KYOUTU-H
      *
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGT06-CALL-SEC
      *
           IF      SPA-ERRCD   NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "7777"
                       MOVE    "E70"           TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                   WHEN    OTHER
                       MOVE    "E61"           TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別対象−レセプト情報チェック処理
      *****************************************************************
       20011-PATIENT-DETAIL-CHK-SEC     SECTION.
      *
           MOVE    ZERO            TO  WRK-DETAIL-IDX
      *
           IF      RECEDMAKEQ-DETAIL-INFORMATION-G  (IDZZ)
                                   =   SPACE
               GO  TO  20011-PATIENT-DETAIL-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       WRK-CONS-DETAIL-MAX
                   OR      RECEDMAKEQ-DETAIL-INFORMATION  (IDZZ IDYY)
                                   =   SPACE
               MOVE    SPACE           TO  SPA-ERRCD
      *??
      *         DISPLAY "IDYY=" IDYY "#" 
      *??
               ADD     1               TO  WRK-DETAIL-MAX
      *        個別指定の重複チェック
               IF      IDYY            >   1
                   COMPUTE IDY =   IDYY    -   1
                   PERFORM VARYING IDVV    FROM    1   BY  1
                           UNTIL   IDVV    >       IDY
                       IF    ( RECEDMAKEQ-DETAIL-INFORMATION (IDZZ IDVV)
                                   =   RECEDMAKEQ-DETAIL-INFORMATION
                                                          (IDZZ IDYY) )
                           MOVE    "E029"              TO  SPA-ERRCD
                           MOVE    "同じ内容で設定してあります。"
                                                       TO  SPA-ERRMSG
                           MOVE    WRK-CONS-DETAIL-MAX TO  IDVV
                       END-IF
                   END-PERFORM
               END-IF
      *        入外区分
               IF      SPA-ERRCD       =   SPACE
                   IF      RECEDMAKEQ-DETAIL-INOUT (IDZZ IDYY)
                                           =   SPACE
                       EVALUATE    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                           WHEN    "I"
                           WHEN    "O"
                               MOVE    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                           TO
                                   RECEDMAKEQ-DETAIL-INOUT (IDZZ IDYY)
                       END-EVALUATE
                   END-IF
                   EVALUATE    RECEDMAKEQ-DETAIL-INOUT (IDZZ IDYY)
                               ALSO    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                       WHEN    "I"     ALSO    "IO"
                       WHEN    "I"     ALSO    "IO"
                       WHEN    "I"     ALSO    "I"
                       WHEN    "O"     ALSO    "IO"
                       WHEN    "O"     ALSO    "OI"
                       WHEN    "O"     ALSO    "O"
                           IF      RECEDMAKEQ-DETAIL-INOUT (IDZZ IDYY)
                                               =   "I"
                               MOVE    "1"         TO
                                           WRK-DETAIL-NYUGAIKBN
                                                       (WRK-DETAIL-MAX)
                           ELSE
                               MOVE    "2"         TO
                                           WRK-DETAIL-NYUGAIKBN
                                                       (WRK-DETAIL-MAX)
                           END-IF
                       WHEN    SPACE   ALSO    ANY
                           MOVE    "E3o"          TO  SPA-ERRCD
                       WHEN    OTHER
                           MOVE    "E3n"          TO  SPA-ERRCD
                   END-EVALUATE
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   IF      RECEDMAKEQ-SUBMISSION-MODE
                                           =   "05"
      *                医保情報の入力の有無
                       IF    ( RECEDMAKEQ-DETAIL-CODE      (IDZZ IDYY)
                                           NOT =   SPACE )
                       OR    ( RECEDMAKEQ-MAIN-INSURANCE-1 (IDZZ IDYY)
                                           NOT =   SPACE )
                       OR    ( RECEDMAKEQ-PUBLIC-INSURANCE (IDZZ IDYY)
                                           NOT =   SPACE )
                       OR    ( RECEDMAKEQ-ACCIDENT-TARGET  (IDZZ IDYY)
                                           NOT =   SPACE )
                       OR    ( RECEDMAKEQ-INCARE-CLASS     (IDZZ IDYY)
                                           NOT =   SPACE )
                           MOVE    "E33"           TO  SPA-ERRCD
                       END-IF
      *                保険ＩＤ
                       IF      RECEDMAKEQ-HKNID (IDZZ IDYY)
                                           NOT =   SPACE
                           INITIALIZE              ORCSNUMAREA
                           MOVE    RECEDMAKEQ-HKNID (IDZZ IDYY)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"       USING
                                                   ORCSNUMAREA
                           IF      SNUM-RC     =   ZERO
                               MOVE    SNUM-OUTNUM TO
                                       WRK-DETAIL-HKNID (WRK-DETAIL-MAX)
                           ELSE
                               MOVE    "E3l"       TO  SPA-ERRCD
                           END-IF
                       END-IF
      *                保険番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-INSURANCE-CLASS
                                                             (IDZZ IDYY)
                                               NOT =   SPACE
                               INITIALIZE          ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI 
                               MOVE    3           TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-INSURANCE-CLASS
                                                           (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   1    )
                               AND   ( KANACHK-MAX     =   3    )
                                   MOVE    RECEDMAKEQ-INSURANCE-CLASS
                                                           (IDZZ IDYY)
                                                           TO
                                   WRK-DETAIL-HKNNUM (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E3j"           TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                労災自賠保険区分
                       IF      SPA-ERRCD       =   SPACE
                           EVALUATE    RECEDMAKEQ-ACCIDENT (IDZZ IDYY)
                               WHEN    "1"
                               WHEN    "2"
                               WHEN    SPACE
                                   MOVE    RECEDMAKEQ-ACCIDENT
                                                          (IDZZ IDYY)
                                                       TO
                                                   WRK-DETAIL-HKNKBN
                                                       (WRK-DETAIL-MAX)
                               WHEN    OTHER
                                   MOVE    "E34"       TO  SPA-ERRCD
                           END-EVALUATE
                       END-IF
      *
                       IF      SPA-ERRCD       =   SPACE
                           IF    ( RECEDMAKEQ-ACCIDENT-NUMBER
                                                           (IDZZ IDYY)
                                               NOT =   SPACE )
                           AND   ( RECEDMAKEQ-PENSIONCERTIFICATE
                                                           (IDZZ IDYY)
                                               NOT =   SPACE )
      *                        労働保険番号と年金証書番号の重複
                               MOVE    "E3m"       TO  SPA-ERRCD
                           END-IF
                       END-IF
      *                労働保険番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-ACCIDENT-NUMBER
                                                           (IDZZ IDYY)
                                           NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI 
                               MOVE    14          TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-ACCIDENT-NUMBER
                                                           (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO       )
                               AND   ( KANACHK-RC2     =   ZERO       )
                               AND   ( KANACHK-SYUBETU =   1          )
      *************************AND   ( KANACHK-MAX     =   14  OR  11 )
                                   MOVE    RECEDMAKEQ-ACCIDENT-NUMBER
                                                          (IDZZ IDYY)
                                                       TO
                                                   WRK-DETAIL-KOFUNUM
                                                       (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E35"       TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                年金証書番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-PENSIONCERTIFICATE
                                                           (IDZZ IDYY)
                                           NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI 
                               MOVE    9           TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-PENSIONCERTIFICATE
                                                           (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   1    )
      *************************AND   ( KANACHK-MAX     =   9    )
                                   MOVE    RECEDMAKEQ-PENSIONCERTIFICATE
                                                          (IDZZ IDYY)
                                                       TO
                                                   WRK-DETAIL-KOFUNUM
                                                       (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E36"       TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                災害区分
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-ACCIDENT-CLASS (IDZZ IDYY)
                                           NOT =   SPACE
                               EVALUATE    RECEDMAKEQ-ACCIDENT-CLASS
                                                           (IDZZ IDYY)
                                   WHEN    SPACE
                                   WHEN    "1"
                                   WHEN    "2"
                                       MOVE    RECEDMAKEQ-ACCIDENT-CLASS
                                                          (IDZZ IDYY)
                                                       TO
                                                   WRK-DETAIL-SAIGAIKBN
                                                       (WRK-DETAIL-MAX)
                                   WHEN    OTHER
                                       MOVE    "E37"   TO  SPA-ERRCD
                               END-EVALUATE
                           END-IF
                       END-IF
                   ELSE
      *                労災情報の入力の有無
                       IF      RECEDMAKEQ-ACCIDENT-INSURANCE (IDZZ IDYY)
                                       NOT =   SPACE
                           MOVE    "E33"           TO  SPA-ERRCD
                       END-IF
      *                種別コード
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-DETAIL-CODE (IDZZ IDYY)
                                           NOT =   SPACE
                               INITIALIZE                  ORCSNUMAREA
                               MOVE    RECEDMAKEQ-DETAIL-CODE
                                                           (IDZZ IDYY)
                                                       TO  SNUM-INX
                               CALL    "ORCSNUM"           USING
                                                           ORCSNUMAREA
                               IF    ( SNUM-RC         =   ZERO )
                               AND   ( SNUM-SEISUU     =   4    )
                               AND   ( SNUM-SYOSUU     =   0    ) 
                                   MOVE    RECEDMAKEQ-DETAIL-CODE
                                                          (IDZZ IDYY)
                                                       TO
                                               WRK-DETAIL-RECESYUBETU
                                                       (WRK-DETAIL-MAX)
                               ELSE
                                  MOVE    "E38"            TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                保険ＩＤ
                       IF      RECEDMAKEQ-HKNID (IDZZ IDYY)
                                           NOT =   SPACE
                           INITIALIZE              ORCSNUMAREA
                           MOVE    RECEDMAKEQ-HKNID (IDZZ IDYY)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"       USING
                                                   ORCSNUMAREA
                           IF      SNUM-RC     =   ZERO
                               MOVE    SNUM-OUTNUM TO
                                       WRK-DETAIL-HKNID (WRK-DETAIL-MAX)
                           ELSE
                               MOVE    "E3l"       TO  SPA-ERRCD
                           END-IF
                       END-IF
      *                保険番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-INSURANCE-CLASS
                                                             (IDZZ IDYY)
                                               NOT =   SPACE
                               INITIALIZE          ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI 
                               MOVE    3           TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-INSURANCE-CLASS
                                                           (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   1    )
                               AND   ( KANACHK-MAX     =   3    )
                                   MOVE    RECEDMAKEQ-INSURANCE-CLASS
                                                           (IDZZ IDYY)
                                                           TO
                                   WRK-DETAIL-HKNNUM (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E3j"           TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                保険者番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-INSURANCE-NUMBER
                                                             (IDZZ IDYY)
                                               NOT =   SPACE
                               INITIALIZE          ORCSNUMAREA
                               MOVE    RECEDMAKEQ-INSURANCE-NUMBER
                                                           (IDZZ IDYY)
                                                   TO  SNUM-INX
                               CALL    "ORCSNUM"       USING
                                                       ORCSNUMAREA
                               IF      SNUM-RC         =   ZERO
                                   MOVE    RECEDMAKEQ-INSURANCE-NUMBER
                                                           (IDZZ IDYY)
                                                           TO
                                   WRK-DETAIL-HKNJANUM (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E39"           TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                記号
                       IF      SPA-ERRCD       =   SPACE
                            IF      RECEDMAKEQ-MAIN-MARK (IDZZ IDYY)
                                               NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI 
                               MOVE    38          TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-MAIN-MARK (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   2    )
                                   MOVE    RECEDMAKEQ-MAIN-MARK
                                                            (IDZZ IDYY)
                                                  TO
                                       WRK-DETAIL-KIGO (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E3a"           TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                番号
                       IF      SPA-ERRCD       =   SPACE
                           IF      RECEDMAKEQ-MAIN-NUMBER (IDZZ IDYY)
                                               NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"             TO  KANACHK-SYORI 
                               MOVE    38          TO  KANACHK-MAX-CNT
                               MOVE    RECEDMAKEQ-MAIN-NUMBER
                                                           (IDZZ IDYY)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   2    )
                                   MOVE    RECEDMAKEQ-MAIN-NUMBER
                                                           (IDZZ IDYY)
                                                           TO
                                       WRK-DETAIL-NUM (WRK-DETAIL-MAX)
                               ELSE
                                   MOVE    "E3b"           TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
      *                補助区分
                       IF      SPA-ERRCD       =   SPACE
      *??                  IF      RECEDMAKEQ-MAIN-ASSISTANCE(IDZZ IDYY)
      *??                                      NOT =   SPACE
      *??                      MOVE    "E3c"           TO  SPA-ERRCD
      *??                  END-IF
                           MOVE    RECEDMAKEQ-MAIN-ASSISTANCE
                                                       (IDZZ IDYY)
                                                   TO
                                   WRK-DETAIL-HOJOKBN (WRK-DETAIL-MAX)
                       END-IF
      *                公費情報
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       4
                               OR      RECEDMAKEQ-PUBLIC-INSURANCE-O
                                                       (IDZZ IDYY IDXX)
                                               =   SPACE
                               OR      SPA-ERRCD
                                           NOT =   SPACE
      *                    公費ＩＤ
                           IF      RECEDMAKEQ-KOHID (IDZZ IDYY IDXX)
                                               NOT =   SPACE
                               INITIALIZE              ORCSNUMAREA
                               MOVE    RECEDMAKEQ-KOHID
                                                       (IDZZ IDYY IDXX)
                                                   TO  SNUM-INX
                               CALL    "ORCSNUM"       USING
                                                       ORCSNUMAREA
                               IF      SNUM-RC     =   ZERO
                                   MOVE    SNUM-OUTNUM
                                               TO  WRK-DETAIL-KOHID
                                                   (WRK-DETAIL-MAX IDXX)
                               ELSE
                                   MOVE    "E3d"       TO  SPA-ERRCD
                               END-IF
                           END-IF
      *                    公費番号
                           IF      SPA-ERRCD       =   SPACE
                               IF      RECEDMAKEQ-KOHNUM
                                                       (IDZZ IDYY IDXX)
                                               NOT =   SPACE
                                   INITIALIZE      ORCSKANACHKAREA
                                   MOVE    "1"     TO  KANACHK-SYORI 
                                   MOVE    3       TO  KANACHK-MAX-CNT
                                   MOVE    RECEDMAKEQ-KOHNUM
                                                        (IDZZ IDYY IDXX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                                   IF    ( KANACHK-RC      =   ZERO )
                                   AND   ( KANACHK-RC2     =   ZERO )
                                   AND   ( KANACHK-SYUBETU =   1    )
                                   AND   ( KANACHK-MAX     =   3    )
                                       MOVE    RECEDMAKEQ-KOHNUM
                                                       (IDZZ IDYY IDXX)
                                               TO  WRK-DETAIL-KOHNUM
                                                   (WRK-DETAIL-MAX IDXX)
                                   ELSE
                                       MOVE    "E3k"       TO  SPA-ERRCD
                                   END-IF
                               END-IF
                           END-IF
      *                    負担者番号
                           IF      SPA-ERRCD       =   SPACE
                               IF      RECEDMAKEQ-PROVIDER-NUMBER
                                                       (IDZZ IDYY IDXX)
                                                   NOT =   SPACE
                                   INITIALIZE      ORCSKANACHKAREA
                                   MOVE    "1"     TO  KANACHK-SYORI 
                                   MOVE    8       TO
                                                       KANACHK-MAX-CNT
                                   MOVE
                                           RECEDMAKEQ-PROVIDER-NUMBER
                                                       (IDZZ IDYY IDXX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                                   IF    ( KANACHK-RC      =   ZERO )
                                   AND   ( KANACHK-RC2     =   ZERO )
                                   AND   ( KANACHK-SYUBETU =   1    )
      **************************** AND   ( KANACHK-MAX     =   8    )
                                       MOVE   RECEDMAKEQ-PROVIDER-NUMBER
                                                      (IDZZ IDYY IDXX)
                                               TO  WRK-DETAIL-FTAJANUM
                                                   (WRK-DETAIL-MAX IDXX)
                                   ELSE
                                       MOVE    "E3e"       TO  SPA-ERRCD
                                   END-IF
                               END-IF
                           END-IF
      *                    受給者番号
                           IF      SPA-ERRCD       =   SPACE
                               IF      RECEDMAKEQ-RECIPIENT-NUMBER
                                                       (IDZZ IDYY IDXX)
                                                       NOT =   SPACE
                                   INITIALIZE      ORCSKANACHKAREA
                                   MOVE    "1"     TO  KANACHK-SYORI 
                                   MOVE    7       TO
                                                       KANACHK-MAX-CNT
                                   MOVE
                                           RECEDMAKEQ-RECIPIENT-NUMBER
                                                       (IDZZ IDYY IDXX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                                   IF    ( KANACHK-RC      =   ZERO )
                                   AND   ( KANACHK-RC2     =   ZERO )
                                   AND   ( KANACHK-SYUBETU =   1    )
      **************************** AND   ( KANACHK-MAX     =   7    )
                                       MOVE
                                           RECEDMAKEQ-RECIPIENT-NUMBER
                                                      (IDZZ IDYY IDXX)
                                               TO  WRK-DETAIL-JKYSNUM
                                                   (WRK-DETAIL-MAX IDXX)
                                  ELSE
                                       MOVE    "E3f"       TO  SPA-ERRCD
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
      *                災害該当の有無
                       IF      SPA-ERRCD       =   SPACE
                           EVALUATE   RECEDMAKEQ-ACCIDENT-TARGET
                                                           (IDZZ IDYY)
                               WHEN    SPACE
                               WHEN    "True"
                                   MOVE    RECEDMAKEQ-ACCIDENT-TARGET
                                                           (IDZZ IDYY)
                                           TO  WRK-DETAIL-DISASTER
                                                       (WRK-DETAIL-MAX)
                               WHEN    OTHER
                                   MOVE    "E3g"       TO  SPA-ERRCD
                           END-EVALUATE
                       END-IF
      *                入所中の有無
                       IF      SPA-ERRCD       =   SPACE
                           EVALUATE   RECEDMAKEQ-INCARE-CLASS
                                                           (IDZZ IDYY)
                               WHEN    SPACE
                               WHEN    "True"
                                   MOVE    RECEDMAKEQ-INCARE-CLASS
                                                           (IDZZ IDYY)
                                           TO    WRK-DETAIL-INCARE-CLASS
                                                       (WRK-DETAIL-MAX)
                               WHEN    OTHER
                                   MOVE    "E3h"       TO  SPA-ERRCD
                           END-EVALUATE
                       END-IF
                   END-IF
      *            実施確認指示（警告）
                   IF      SPA-ERRCD       =   SPACE
                       EVALUATE   RECEDMAKEQ-RECEIPT-PERFORM-FLAG
                                                           (IDZZ IDYY)
                           WHEN    SPACE
                           WHEN    "True"
                               MOVE    RECEDMAKEQ-RECEIPT-PERFORM-FLAG
                                                           (IDZZ IDYY)
                                          TO
                                       WRK-DETAIL-RECEIPT-PERFORM-FLAG
                                                       (WRK-DETAIL-MAX)
                           WHEN    OTHER
                               MOVE    "E3i"       TO  SPA-ERRCD
                       END-EVALUATE
                   END-IF
               END-IF
      *
      *??
      *?? DISPLAY "MAX       =" WRK-DETAIL-MAX 
      *?? DISPLAY "FLAG      =" WRK-DETAIL-RECEIPT-PERFORM-FLAG
      *??                                      (WRK-DETAIL-MAX)
      *?? DISPLAY "SYUBETU   =" WRK-DETAIL-RECESYUBETU (WRK-DETAIL-MAX)
      *?? DISPLAY "NYUGAIKBN =" WRK-DETAIL-NYUGAIKBN (WRK-DETAIL-MAX)
      *?? DISPLAY "HKNID     =" WRK-DETAIL-HKNID (WRK-DETAIL-MAX)
      *?? DISPLAY "HKNNUM    =" WRK-DETAIL-HKNNUM (WRK-DETAIL-MAX)
      *?? DISPLAY "HKNJANUM  =" WRK-DETAIL-HKNJANUM (WRK-DETAIL-MAX)
      *?? DISPLAY "HOJOKBN   =" WRK-DETAIL-HOJOKBN (WRK-DETAIL-MAX) 
      *?? DISPLAY "KIGO      =" WRK-DETAIL-KIGO (WRK-DETAIL-MAX)
      *?? DISPLAY "NUM       =" WRK-DETAIL-NUM (WRK-DETAIL-MAX)
      *?? DISPLAY "DISASTER  =" WRK-DETAIL-DISASTER (WRK-DETAIL-MAX)
      *?? DISPLAY "INCARE-CLASS="   WRK-DETAIL-INCARE-CLASS(WRK-DETAIL-MAX)
      *?? DISPLAY "KOH-OCC(1)=" WRK-DETAIL-KOH-OCC (WRK-DETAIL-MAX 1)
      *?? DISPLAY "KOH-OCC(2)=" WRK-DETAIL-KOH-OCC (WRK-DETAIL-MAX 2)
      *?? DISPLAY "KOH-OCC(3)=" WRK-DETAIL-KOH-OCC (WRK-DETAIL-MAX 3)
      *?? DISPLAY "KOH-OCC(4)=" WRK-DETAIL-KOH-OCC (WRK-DETAIL-MAX 4)
      *?? DISPLAY "HKNKBN   =" WRK-DETAIL-HKNKBN (WRK-DETAIL-MAX)
      *?? DISPLAY "SAIGAIKBN=" WRK-DETAIL-SAIGAIKBN (WRK-DETAIL-MAX)
      *?? display "KOFUNUM  =" WRK-DETAIL-KOFUNUM  (WRK-DETAIL-MAX)
      *?? DISPLAY "SPA-ERRCD=" SPA-ERRCD "#"
      *?? display " "
      *??
               IF      SPA-ERRCD       =   SPACE
                   CONTINUE
               ELSE
                   MOVE    IDYY            TO  WRK-DETAIL-ERR
                   PERFORM 20012-PATIENT-ERR-SEC
               END-IF
           END-PERFORM
      *
           IF      WRK-DETAIL-IDX      >   ZERO
               INITIALIZE                  WRK-DETAIL-TBL
                                           WRK-T09-TBL
               MOVE    "E52"               TO  SPA-ERRCD
               GO  TO  20011-PATIENT-DETAIL-CHK-EXT
           END-IF
      *
      *    レセプト情報と請求管理のチェック
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       WRK-DETAIL-MAX
               MOVE    ZERO            TO  FLG-RECEIPT-CHK
      *
               PERFORM VARYING IDVV    FROM    1   BY  1
                       UNTIL   IDVV    >       SPA-GMN-T09-MAX
                   MOVE    ZERO            TO  FLG-DETAIL-CHK
      *
                   IF      WRK-DETAIL-NYUGAIKBN (IDYY)
                                       NOT =   SPACE
                       IF      WRK-DETAIL-NYUGAIKBN (IDYY)
                               =   SPA-NAI-T09-RECE10-TNYUGAIKBN (IDVV)
                           CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-DETAIL-CHK
                       END-IF
                   END-IF
                   IF      WRK-DETAIL-HKNID (IDYY)
                                       >   ZERO
                       IF      WRK-DETAIL-HKNID (IDYY)
                                   =    SPA-NAI-T09-RECE10-THKNID (IDVV)
                           CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-DETAIL-CHK
                       END-IF
                   END-IF
                   IF      WRK-DETAIL-HKNNUM (IDYY)
                                       NOT =   SPACE
                       IF      WRK-DETAIL-HKNNUM (IDYY)
                                   =   SPA-NAI-T09-RECE10-THKNNUM (IDVV)
                               CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-DETAIL-CHK
                       END-IF
                   END-IF
      *
                   IF      RECEDMAKEQ-SUBMISSION-MODE
                                           =   "05"
                       IF      WRK-DETAIL-HKNKBN (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-HKNKBN (IDYY)
                                   =   SPA-NAI-T09-RECE10-THKNKBN
                                                               (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-SAIGAIKBN (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-SAIGAIKBN (IDYY)
                                   =   SPA-NAI-T09-RECE10-TSAIGAIKBN
                                                               (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-KOFUNUM (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-KOFUNUM (IDYY)
                                   =   SPA-NAI-T09-RECE10-TKIGO(IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-DETAIL-RECESYUBETU (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-RECESYUBETU (IDYY)
                                   =   SPA-NAI-T09-RECE10-TRECESYUBETU
                                                               (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-HKNJANUM (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-HKNJANUM (IDYY)
                                   =   SPA-NAI-T09-RECE10-THKNJANUM
                                                               (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-HOJOKBN (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-HOJOKBN (IDYY)
                                   =   SPA-NAI-T09-THOJOKBN    (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-KIGO (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-KIGO (IDYY)
                                   =   SPA-NAI-T09-RECE10-TKIGO  (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       IF      WRK-DETAIL-NUM (IDYY)
                                           NOT =   SPACE
                           IF      WRK-DETAIL-NUM (IDYY)
                                   =   SPA-NAI-T09-RECE10-TNUM  (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                       END-IF
                       EVALUATE    WRK-DETAIL-DISASTER (IDYY)
                           WHEN    "True"
                               IF      SPA-NAI-T09-TDISASTER  (IDVV)
                                                   >   ZERO
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           WHEN    SPACE
                               IF      SPA-NAI-T09-TDISASTER  (IDVV)
                                                   =   ZERO
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                       END-EVALUATE
                       EVALUATE    WRK-DETAIL-INCARE-CLASS (IDYY)
                           WHEN    "True"
                               EVALUATE
                                   SPA-NAI-T09-RECE10-TRECEIPT-KBN
                                                           (IDVV) (1:1)
                                   WHEN    "1"
                                   WHEN    "2"
                                   WHEN    "3"
                                   WHEN    "4"
                                       CONTINUE
                                   WHEN    OTHER
                                       MOVE    1   TO  FLG-DETAIL-CHK
                               END-EVALUATE
                           WHEN    SPACE
                               EVALUATE
                                   SPA-NAI-T09-RECE10-TRECEIPT-KBN
                                                           (IDVV) (1:1)
                                   WHEN    "1"
                                   WHEN    "2"
                                   WHEN    "3"
                                   WHEN    "4"
                                       MOVE    1   TO  FLG-DETAIL-CHK
                               END-EVALUATE
                       END-EVALUATE
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       4
                               OR     FLG-DETAIL-CHK  =       1
                           IF      WRK-DETAIL-KOHNUM (IDYY IDY)
                                               NOT =   SPACE
                               IF      WRK-DETAIL-KOHNUM (IDYY IDY)
                                   =   SPA-NAI-RECE10-T09-KOHNUM
                                                           (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-DETAIL-KOHID (IDYY IDY)
                                                   >   ZERO
                               IF      WRK-DETAIL-KOHID (IDYY IDY)
                                   =   SPA-NAI-RECE10-T09-KOHID
                                                           (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-DETAIL-FTAJANUM (IDYY IDY)
                                               NOT =   SPACE
                               IF      WRK-DETAIL-FTAJANUM (IDYY IDY)
                                   =   SPA-NAI-T09-FTNJANUM (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-DETAIL-JKYSNUM (IDYY IDY)
                                               NOT =   SPACE
                               IF      WRK-DETAIL-JKYSNUM (IDYY IDY)
                                   =   SPA-NAI-T09-JKYSNUM (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
      *
                   IF      FLG-DETAIL-CHK         =   ZERO
      *                対象のレセプトに対する件数
                       ADD     1               TO  WRK-T09-CHK    (IDVV)
                       MOVE    IDYY            TO  WRK-T09-DET-IDX(IDVV)
                       MOVE    WRK-DETAIL-RECEIPT-PERFORM-FLAG (IDYY)
                                               TO  WRK-T09-FLAG   (IDVV)
      *                リクエストに対する件数
                       ADD     1               TO  WRK-T09-CNT    (IDYY)
                       MOVE    1               TO  WRK-T09-IDX 
                                                             (IDYY IDVV)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO            TO  FLG-SELECT-ERR
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       WRK-DETAIL-MAX
      *        個別対象−レセプト情報と一致するレセプトがないとき
               IF      WRK-T09-CNT (IDYY)  =   ZERO
                   MOVE    "E53"              TO  SPA-ERRCD
                   MOVE    IDYY            TO  WRK-DETAIL-ERR
                   PERFORM 20012-PATIENT-ERR-SEC
               ELSE
      *        複数の個別対象−レセプト情報と一致するとき
                   IF      WRK-T09-CNT (IDYY)  >   1
                       MOVE    1                   TO  FLG-SELECT-ERR
                       MOVE    "E54"               TO  SPA-ERRCD
                       MOVE    IDYY                TO  WRK-DETAIL-ERR
                       PERFORM 20012-PATIENT-ERR-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        複数の個別対象−レセプト情報と一致するとき
               IF      FLG-SELECT-ERR      =   1
                   IF      FLG-PATIENT-SEL     =   ZERO
                       MOVE    ZERO            TO  WRK-S-DETAIL-MAX
                                                   IDVV
                   END-IF
      *
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       WRK-DETAIL-MAX
                       IF      WRK-T09-CNT (IDYY)  >   1
                           ADD     1       TO  WRK-S-DETAIL-MAX
                                               IDVV
                           PERFORM VARYING IDW FROM    1   BY  1
                                   UNTIL   IDW >       SPA-GMN-T09-MAX
                               IF      WRK-T09-IDX (IDYY IDW)
                                                   >   ZERO
                                   MOVE    IDW     TO  WRK-T09-SELNUM
      *                           個別対象患者選択一覧処理
                                  PERFORM 20013-PATIENT-SELECT-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        ひとつの個別対象−レセプト情報と一致するとき
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-DETAIL-MAX
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       SPA-GMN-T09-MAX
                       IF      WRK-T09-CHK (IDW)   >   1
                           IF      WRK-T09-IDX (IDYY IDW)
                                                   >   ZERO
                               MOVE    "E57"       TO  SPA-ERRCD
                               MOVE    IDYY        TO  WRK-DETAIL-ERR
                               PERFORM 20012-PATIENT-ERR-SEC
      *                        個別対象患者選択一覧処理
                               MOVE    1           TO  FLG-SELECT-ERR
                               ADD     1           TO  WRK-S-DETAIL-MAX
                               MOVE    IDW         TO  WRK-T09-SELNUM
                               PERFORM 20013-PATIENT-SELECT-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               INITIALIZE                  WRK-DETAIL-TBL
                                           WRK-T09-TBL
               MOVE    ZERO            TO  FLG-SELECT-ERR
           END-IF
           .
       20011-PATIENT-DETAIL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別対象患者エラー処理
      *****************************************************************
       20012-PATIENT-ERR-SEC     SECTION.
      *
           IF      FLG-PATIENT-ERR     =   ZERO
               ADD     1               TO  WRK-ERR-PATIENT-MAX
      *
               MOVE    1               TO  FLG-PATIENT-ERR
               MOVE    ZERO            TO  WRK-DETAIL-IDX
      *
      ***     IF      WRK-ERR-PATIENT-MAX >   WRK-CONS-OVERFLOW
      *??          100件以上あり
      ***          MOVE    "True"          TO  RECEDMAKES-ERR-OVERFLOW
      *
      ***          MOVE    ZERO            TO  WRK-T09-SELNUM
      ***                                      WRK-DETAIL-ERR
      ***          GO  TO  20012-PATIENT-ERR-EXT
      ***      END-IF
      *
               MOVE    IDZZ            TO  WRK-NUM
               MOVE    WRK-NUM-X       TO  RECEDMAKES-ITEM-POSITION
                                                   (WRK-ERR-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-ID (IDZZ)
                                       TO  RECEDMAKES-PATIENT-ID
                                                   (WRK-ERR-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                       TO  RECEDMAKES-PATIENT-MONTH
                                                   (WRK-ERR-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                       TO  RECEDMAKES-PATIENT-INOUT
                                                   (WRK-ERR-PATIENT-MAX)
           END-IF
      *
      *??
      *   DISPLAY "20012 ERR=" WRK-DETAIL-ERR "#" WRK-T09-SELNUM "#"
      *          SPA-ERRCD "#"
      *??
           IF      WRK-DETAIL-ERR      =   ZERO
               IF      WRK-T09-SELNUM      =   ZERO
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    SPA-ERRCD
                           WHEN    "0001"
                               MOVE    "E12"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "0003"
                               MOVE    "E10"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "0009"
                               MOVE    "E31"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E001"
                               MOVE    "E01"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E028"
                               MOVE    "E28"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E029"
                               MOVE    "E29"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E032"
                               MOVE    "E32"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E52"
                               MOVE    "レセプト情報に誤りがあります。"
                                               TO  SPA-ERRMSG
                               MOVE    "E52"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "0008"
                               MOVE
                               "同じレセプトが既に対象になっています。"
                                               TO  SPA-ERRMSG
                               MOVE    "E51"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E55"
                               MOVE
                                   "対象のレセプトが複数存在します。"
                                               TO  SPA-ERRMSG
                               MOVE    "E55"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    "E56"
                               MOVE
                      "レセプト情報の設定が必要なレセプトが存在します。"
                                               TO  SPA-ERRMSG
                               MOVE    "E56"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                           WHEN    OTHER
                               MOVE    "E51"   TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                       END-EVALUATE
                       MOVE    SPA-ERRMSG      TO  RECEDMAKES-ERROR-MSG
                                                   (WRK-ERR-PATIENT-MAX)
      *
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    "E63"           TO  WRK-ERRCD
                       END-IF
                   END-IF
               ELSE
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    SPA-ERRCD
                           WHEN    "0008"
                               MOVE
                               "同じレセプトが既に対象になっています。"
                                               TO  SPA-ERRMSG
                       END-EVALUATE
                       MOVE    "E51"           TO
                                    RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-T09-SELNUM)
                       MOVE    SPA-ERRMSG      TO
                                   RECEDMAKES-DETAIL-ERROR-MSG
                                   (WRK-ERR-PATIENT-MAX WRK-T09-SELNUM)
      *
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    "E63"           TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           ELSE
      *        個別対象−レセプト情報がエラーのとき
               ADD     1                   TO  WRK-DETAIL-IDX
      *
      ***      IF      WRK-DETAIL-IDX  >   WRK-CONS-DETAIL-OVERFLOW
      ***          20件以上あり
      ***          MOVE    "True"          TO  RECEDMAKES-ERR-OVERFLOW
      *
      ***          MOVE    ZERO            TO  WRK-T09-SELNUM
      ***                                      WRK-DETAIL-ERR
      ***          GO  TO  20012-PATIENT-ERR-EXT
      ***      END-IF
      *
               MOVE    WRK-DETAIL-ERR      TO  WRK-NUM
               MOVE    WRK-NUM-X (2:2)     TO
                                   RECEDMAKES-DETAIL-ITEM-POSITION
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
               MOVE    RECEDMAKEQ-DETAIL-INFORMATION
                                               (IDZZ WRK-DETAIL-ERR)
                                           TO
                                   RECEDMAKES-DETAIL-INFORMATION-G
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
      *
               EVALUATE    SPA-ERRCD
                   WHEN    "0001"
                       MOVE    "E12"       TO
                                   RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                   WHEN    "E028"
                       MOVE    "E28"       TO
                                   RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                   WHEN    "E029"
                       MOVE    "E29"       TO
                                   RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                   WHEN    "0008"
                       MOVE    "同じレセプトが既に対象になっています。"
                                               TO  SPA-ERRMSG
                       MOVE    "E51"       TO
                                   RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                   WHEN    OTHER
                       MOVE    SPA-ERRCD       TO
                                   RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                       EVALUATE    SPA-ERRCD
                           WHEN    "E33"
                               MOVE    "不要な設定があります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E34"
                               MOVE
                              "労災自賠保険区分の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E35"
                               MOVE
                               "労働保険番号の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E36"
                               MOVE
                               "年金証書番号の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E37"
                               MOVE
                                   "災害区分の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E38"
                               MOVE
                                   "種別コードの設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E39"
                               MOVE
                                   "保険者番号の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3a"
                               MOVE    "記号の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3b"
                               MOVE    "番号の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
      ***                  WHEN    "E3c"
      ***                      MOVE
      ***                          "補助区分の設定に誤りがあります。"
      ***                                              TO  SPA-ERRMSG
                           WHEN    "E3d"
                               MOVE
                                   "公費ＩＤの設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3e"
                               MOVE
                                   "負担者番号の設定に誤りがあります。"
                                                       TO SPA-ERRMSG
                           WHEN    "E3f"
                               MOVE
                                   "受給者番号の設定に誤りがあります。"
                                                       TO SPA-ERRMSG
                           WHEN    "E3g"
                               MOVE
                               "災害該当の有無の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3h"
                               MOVE
                                  "入所中の有無の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3i"
                               MOVE
                                  "実施確認指示の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3j"
                               MOVE
                                  "保険の種類の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3k"
                               MOVE
                                  "公費の種類の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3l"
                               MOVE
                                   "保険ＩＤの設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3m"
                               STRING  "労働保険番号、年金証書番号の"
                                                   DELIMITED   BY  SIZE
                                       "の両方に設定してあります。"
                                                   DELIMITED   BY  SIZE
                                                   INTO    SPA-ERRMSG
                               END-STRING
                           WHEN    "E3n"
                               MOVE
                                   "入外区分の設定に誤りがあります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E3o"
                               MOVE    "入外区分が未設定です。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E53"
                               MOVE
                        "レセプト情報の対象となるレセプトがありません。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E54"
                               MOVE
                     "レセプト情報から複数のレセプトが対象になります。"
                                                       TO  SPA-ERRMSG
                           WHEN    "E57"
                               MOVE
                               "複数のレセプト情報から対象となります。"
                                                       TO  SPA-ERRMSG
                       END-EVALUATE
               END-EVALUATE
               MOVE    SPA-ERRMSG          TO
                                   RECEDMAKES-DETAIL-ERROR-MSG
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
      *
               IF       RECEDMAKES-ERROR-CODE (WRK-ERR-PATIENT-MAX)
                                           =   SPACE
                   MOVE    RECEDMAKES-DETAIL-ERROR-CODE
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                                           TO  RECEDMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   MOVE    RECEDMAKES-DETAIL-ERROR-MSG
                                   (WRK-ERR-PATIENT-MAX WRK-DETAIL-IDX)
                                           TO  RECEDMAKES-ERROR-MSG
                                                   (WRK-ERR-PATIENT-MAX)
               END-IF
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E63"           TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    ZERO           TO  WRK-T09-SELNUM
                                      WRK-DETAIL-ERR
           .
       20012-PATIENT-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別対象患者選択一覧処理
      *****************************************************************
       20013-PATIENT-SELECT-SEC     SECTION.
      *
           IF      FLG-PATIENT-SEL  =   ZERO
               ADD     1               TO  WRK-S-PATIENT-MAX
      *
               MOVE    1               TO  FLG-PATIENT-SEL
               MOVE    ZERO            TO  WRK-S-RECEIPT-MAX
      *
      ***      IF      WRK-S-PATIENT-MAX
      ***                              >   WRK-CONS-OVERFLOW
      *            100件以上あり
      ***          MOVE    "True"          TO
      ***                                  RECEDMAKES-SELECT-OVERFLOW
      ***          GO  TO  20013-PATIENT-SELECT-EXT
      ***      END-IF
      *
               MOVE    IDZZ            TO  WRK-NUM
               MOVE    WRK-NUM-X       TO  RECEDMAKES-S-ITEM-POSITION
                                                   (WRK-S-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-ID (IDZZ)
                                       TO  RECEDMAKES-S-PATIENT-ID
                                                   (WRK-S-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-MONTH (IDZZ)
                                       TO  RECEDMAKES-S-PATIENT-MONTH
                                                   (WRK-S-PATIENT-MAX)
               MOVE    RECEDMAKEQ-PATIENT-INOUT (IDZZ)
                                       TO  RECEDMAKES-S-PATIENT-INOUT
                                                   (WRK-S-PATIENT-MAX)
           END-IF
      *
           IF      WRK-T09-SELNUM  =   ZERO
               MOVE    1               TO  ST-IDX
               MOVE    SPA-GMN-T09-MAX TO  ED-IDX
           ELSE
               MOVE    WRK-T09-SELNUM  TO  ST-IDX
                                           ED-IDX
           END-IF
      *
           IF      FLG-SELECT-ERR      =   ZERO
      *        詳細設定なしのとき
               MOVE    1                   TO  IDVV
                                               WRK-S-DETAIL-MAX
               MOVE    ZERO                TO  WRK-NUM
           ELSE
               IF      FLG-SELECT-ERR      =   2
      *            警告ありのとき
                   ADD     1                   TO  WRK-S-DETAIL-MAX
                   MOVE    WRK-T09-DET-IDX (WRK-T09-SELNUM)
                                               TO  WRK-NUM
               ELSE
                   MOVE    IDYY                TO  WRK-NUM
               END-IF
           END-IF
      *
      ***  IF      WRK-S-DETAIL-MAX
      ***                          >   WRK-CONS-DETAIL-OVERFLOW
      *        20件以上あり
      ***      MOVE    "True"          TO  RECEDMAKES-SELECT-OVERFLOW
      ***      GO  TO  20013-PATIENT-SELECT-EXT
      ***  END-IF
      *
           MOVE    WRK-NUM-X (2:2)     TO  RECEDMAKES-S-DETAIL-POSITION
                                   (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX)
      *
           PERFORM VARYING IDXX    FROM    ST-IDX  BY  1
                   UNTIL   IDXX    >       ED-IDX
               ADD     1                   TO  WRK-S-RECEIPT-MAX
      *
      ***      IF      WRK-S-RECEIPT-MAX
      ***                              >   WRK-CONS-DETAIL-OVERFLOW
      *        20件以上あり
      ***      MOVE    "True"          TO  RECEDMAKES-SELECT-OVERFLOW
      ***      GO  TO  20013-PATIENT-SELECT-EXT
      ***  END-IF
      *
               EVALUATE    SPA-NAI-T09-RECE10-TNYUGAIKBN (IDXX)
                   WHEN    "1"
                       MOVE    "I"         TO  RECEDMAKES-S-DETAIL-INOUT
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   WHEN    "2"
                       MOVE    "O"         TO  RECEDMAKES-S-DETAIL-INOUT
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
               END-EVALUATE
      *
               IF      SPA-NAI-T09-RECE10-TTEISYUTUSA (IDXX)
                                               =   "3"
                   IF      SPA-NAI-T09-RECE10-THKNNUM  (IDXX)
                                               NOT =   SPACE
                       MOVE    SPA-NAI-T09-RECE10-THKNID   (IDXX)
                                       TO  RECEDMAKES-S-HKNID
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-NAI-T09-RECE10-THKNNUM   (IDXX)
                                       TO  RECEDMAKES-S-INSURANCE-CLASS
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-IF
                   MOVE    SPA-NAI-T09-RECE10-THKNKBN   (IDXX)
                                       TO  RECEDMAKES-S-ACCIDENT
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   MOVE    SPA-NAI-T09-RECE10-TSAIGAIKBN (IDXX)
                                       TO  RECEDMAKES-S-ACCIDENT-CLASS
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   EVALUATE    SPA-NAI-T09-RECE10-TRECESYUBETU
                                                       (IDXX)
                       WHEN    "7101"
                       WHEN    "7091"
                           MOVE    "傷病・業務"        TO
                                           RECEDMAKES-S-DETAIL-TYPE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "7102"
                       WHEN    "7092"
                           MOVE    "傷病・通勤"        TO
                                           RECEDMAKES-S-DETAIL-TYPE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "7071"
                       WHEN    "7081"
                           MOVE    "短期・業務"        TO
                                           RECEDMAKES-S-DETAIL-TYPE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "7072"
                       WHEN    "7082"
                           MOVE    "短期・通勤"        TO
                                           RECEDMAKES-S-DETAIL-TYPE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    OTHER
                           MOVE    SPA-NAI-T09-RECE10-TRECESYUBETU
                                                       (IDXX)
                                                       TO
                                           RECEDMAKES-S-DETAIL-TYPE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-EVALUATE
                   EVALUATE    SPA-NAI-T09-RECE10-TRECESYUBETU (IDXX)
                       WHEN    "7101"
                       WHEN    "7091"
                       WHEN    "7102"
                       WHEN    "7092"
      *                    年金証書番号
                           MOVE    SPA-NAI-T09-RECE10-TKIGO
                                                       (IDXX)
                                   TO  RECEDMAKES-S-PENSIONCERTIFICATE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "7071"
                       WHEN    "7081"
                       WHEN    "7072"
                       WHEN    "7082"
      *                    労働保険番号
                           MOVE    SPA-NAI-T09-RECE10-TKIGO
                                                       (IDXX)
                                       TO  RECEDMAKES-S-ACCIDENT-NUMBER
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-EVALUATE
               ELSE
                   MOVE    SPA-NAI-T09-RECE10-TRECESYUBETU
                                                       (IDXX)
                                       TO  RECEDMAKES-S-DETAIL-CODE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   IF      SPA-NAI-T09-RECE10-THKNNUM  (IDXX)
                                               NOT =   SPACE
                       MOVE    SPA-NAI-T09-RECE10-THKNID   (IDXX)
                                       TO  RECEDMAKES-S-HKNID
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-NAI-T09-RECE10-THKNNUM   (IDXX)
                                       TO  RECEDMAKES-S-INSURANCE-CLASS
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-GMN-T09-THKNJANUM   (IDXX)
                                       TO  RECEDMAKES-S-INSURANCE-NUMBER
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-NAI-T09-RECE10-TKIGO(IDXX)
                                       TO  RECEDMAKES-S-MAIN-MARK
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-NAI-T09-RECE10-TNUM (IDXX)
                                       TO  RECEDMAKES-S-MAIN-NUMBER
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       MOVE    SPA-NAI-T09-RECE10-THOJOKBN (IDXX)
                                       TO  RECEDMAKES-S-MAIN-ASSISTANCE
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-IF
                   IF      SPA-NAI-RECE10-T09-KOHNUM (IDXX 1)
                                               NOT =   SPACE
                       PERFORM VARYING  IDWW    FROM    1   BY  1
                               UNTIL    IDWW    >       4
                               OR       SPA-NAI-RECE10-T09-KOHNUM
                                                            (IDXX IDWW)
                                                =       SPACE
                           MOVE    SPA-NAI-RECE10-T09-KOHNUM (IDXX IDWW)
                                    TO  RECEDMAKES-S-KOHNUM
             (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX IDWW)
                           MOVE    SPA-NAI-RECE10-T09-KOHID  (IDXX IDWW)
                                    TO  RECEDMAKES-S-KOHID
             (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX IDWW)
                           MOVE    SPA-NAI-T09-FTNJANUM      (IDXX IDWW)
                                       TO  RECEDMAKES-S-PROVIDER-NUMBER
             (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX IDWW)
                           MOVE    SPA-NAI-T09-JKYSNUM       (IDXX IDWW)
                                       TO  RECEDMAKES-S-RECIPIENT-NUMBER
             (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX IDWW)
                       END-PERFORM
                   END-IF
                   EVALUATE    SPA-NAI-T09-RECE10-TRECEIPT-KBN
                                                           (IDXX) (1:1)
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                           MOVE    "True"  TO  RECEDMAKES-S-INCARE-CLASS
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-EVALUATE
                   IF      SPA-NAI-T09-TDISASTER   (IDXX)
                                           >   ZERO
                           MOVE    "True"  TO
                                           RECEDMAKES-S-ACCIDENT-TARGET
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                   END-IF
               END-IF
      *
               IF      WRK-T09-SELNUM      >   ZERO
                   EVALUATE    SPA-TIDCD
                       WHEN    SPACE
                           CONTINUE
                       WHEN    "1002"
                           MOVE    "W13"   TO
                                       RECEDMAKES-S-DETAIL-WARNING
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                           MOVE    "既に請求済です"
                                           TO
                                       RECEDMAKES-S-DETAIL-WARNING-MSG
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "1006"
                           MOVE    "W14"   TO
                                       RECEDMAKES-S-DETAIL-WARNING
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                           MOVE
                           "請求データの作成日がレセ送信日以前です"
                                           TO
                                       RECEDMAKES-S-DETAIL-WARNING-MSG
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "1011"
                           MOVE    "W11"   TO
                                       RECEDMAKES-S-DETAIL-WARNING
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                           MOVE
                               "返戻データが存在します（返戻設定無し）"
                                           TO
                                       RECEDMAKES-S-DETAIL-WARNING-MSG
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                       WHEN    "1012"
                           MOVE    "W12"   TO
                                       RECEDMAKES-S-DETAIL-WARNING
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                           STRING  "保険情報が一致する返戻データが"
                                               DELIMITED  BY  SIZE
                                   "存在します（返戻設定無し）"
                                               DELIMITED  BY  SIZE
                                   INTO
                                       RECEDMAKES-S-DETAIL-WARNING-MSG
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
                           END-STRING
                       WHEN    OTHER
                           MOVE    "W05"   TO
                                       RECEDMAKES-S-DETAIL-WARNING
                  (WRK-S-PATIENT-MAX WRK-S-DETAIL-MAX WRK-S-RECEIPT-MAX)
      *??
                           DISPLAY "W05 SPA-TIDCD=" SPA-TIDCD "#" 
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           .
       20013-PATIENT-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    レセ電データ作成処理
      *****************************************************************
       2002-RECEIPTDATA-CHECK-MAKE-SEC       SECTION.
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  RECEDMAKEQ-ORCA-UID
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-RECEIPTDATA-CHECK-MAKE-EXT
           END-IF
      *
           MOVE    RECEDMAKEQ-KARTE-UID
                                   TO  SPA-T01-JOB-KARTE-UID
           MOVE    RECEDMAKEQ-ORCA-UID
                                   TO  SPA-T01-JOB-ORCA-UID
           MOVE    "9"             TO  SPA-T01-JOB-DATAKBN
      *
           PERFORM 800-TEMPDIR-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2002-RECEIPTDATA-CHECK-MAKE-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-TID1-AREA
      *
           MOVE    "T98"           TO  SPA-MOTOPG
           MOVE    "NO"            TO  SPA-TID1-FLG
           MOVE    SPA-T01-PRTKBN  TO  SPA-GMN-T98-PRTKBN
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           PERFORM 800-ORCGT06-CALL-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD           =   "E88"
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               ELSE
                   MOVE    "E62"               TO  WRK-ERRCD
               END-IF
               GO  TO  2002-RECEIPTDATA-CHECK-MAKE-EXT
           END-IF
      *
      *    レセ電処理後の印刷処理（ORCGT99の4903-PRINT-SECにあわせる）
           PERFORM 20022-PRINT-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "E62"               TO  WRK-ERRCD
               GO  TO  2002-RECEIPTDATA-CHECK-MAKE-EXT
           END-IF
      *
      *    monblobapi処理
           PERFORM 20023-MONBLOBAPI-SEC
      *
           .
       2002-RECEIPTDATA-CHECK-MAKE-EXT.
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
           MOVE    "receiptdata_patient_make"
                                       TO  SHELLTBL-ARG17
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
               MOVE    "E3z"           TO  WRK-ERRCD
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
      *    個別レセ電データ作成処理
      *****************************************************************
       800-ORCGT06-CALL-SEC                SECTION.
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T06         TO  SPA-FREE
      *
           CALL    "ORCGT06"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T06
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGT06-CALL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    個別レセ電データ作成（レセ電対象患者一覧）処理
      *****************************************************************
       800-ORCGT09-CALL-SEC                SECTION.
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T06         TO  SPA-FREE
      *
           CALL    "ORCGT09"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T06
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGT09-CALL-EXT.
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
      *            レセプト作成の処理中
               WHEN    "E3z"
      *********WHEN    "E36"
      *            レセプト作成結果
      ***      WHEN    "E63"
      ***          個別対象患者エラー
      ***       WHEN    "E55"
      ***          選択一覧あり
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
                   MOVE    "直接請求する保険者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E27"
                   MOVE
                       "直接請求する保険者番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E30"
                   MOVE
                   "システム日付の前月の診療年月まで処理が可能です。"
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
               WHEN    "E63"
      *            個別対象患者エラー
                   MOVE    "設定内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E55"
      *            レセプト複数対象個別対象患者エラー
                   MOVE    "対象のレセプトが複数存在します。"
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
