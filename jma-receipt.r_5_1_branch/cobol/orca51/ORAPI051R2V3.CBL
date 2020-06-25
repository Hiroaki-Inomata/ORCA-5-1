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
       PROGRAM-ID.         ORAPI051R2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 日次月次統計一覧取得  (xml2)
      *  管理者            :
      *  作成日付    作業者      記述
      *  18/05/10    NACL−藤原  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.02    NACL-藤原    19/06/05  システム管理（３００３）の
      *                                     キー値を変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE         SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "L01COMMON-SPA".
      *
      *    日次統計画面用ＳＰＡ
           COPY    "L01SCR-SPA".
      *
      *    月次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    月次統計画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ENABLED             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDWW                    PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
           03  IDAA                    PIC 9(04).
           03  IDBB                    PIC 9(04).
           03  IDCC                    PIC 9(04).
      *
           03  IDXI                    PIC 9(04).
           03  IDXT                    PIC 9(04).
           03  IDXU                    PIC 9(04).
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
           03  WRK-ERR-1               PIC X(100).
           03  WRK-ERR-2               PIC X(100).
      *
           03  WRK-PROCESSING-ERRCD    PIC X(03).
           03  WRK-ERR-PROCESSING-MAX  PIC 9(03).
           03  WRK-ERR-PARAMETER-IDX   PIC 9(03).
      *
           03  WRK-NUM-X.
               05  WRK-NUM             PIC 9(03).
           03  WRK-NUM-2-X.
               05  WRK-NUM-2           PIC 9(02).
      *
           03  ST-IDX                  PIC 9(04).
           03  ED-IDX                  PIC 9(04).
      *
      *    パラメタ値情報
           03  WRK-PARM-G.
               05  WRK-PARM-FLG-OCC    OCCURS  10.
                   07  WRK-PARM-PAGE   PIC 9(02).
                   07  WRK-PARM-LINE   PIC 9(02).
                   07  WRK-PARM-INDEX  PIC 9(02).
      *
                   07  WRK-PARM-PROGRAM-NO
                                       PIC 9(3).
                   07  WRK-PARM-PARM-NO
                                       PIC 9(2)    OCCURS  10.
      *    リクエスト情報 
           03  WRK-LSTREQ-G.
               05  WRK-LSTREQ-PROGRAM-NO
                                       PIC 9(3)    OCCURS  100.
      *    頁情報
           03  WRK-PAGE-G.
               05  WRK-PAGE-MAX        PIC 9(02).
               05  WRK-PAGE-OCC        OCCURS  10.
                   07  WRK-PAGE-FLG    PIC 9(01).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-SPA-TERMID          PIC X(64).
      *
           03  WRK-UPDCNT-X.
               05  WRK-UPDCNT          PIC 9(10).
      *
      *    ジョブ管理マスタ
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"
                                   REPLACING  //JOB//
                                   BY         //WRK-JOB//.
      * 
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
       01  WRK-CONS-AREA.
      *    プログラム最大個数
           03  WRK-CONS-PROCESSING-MAX PIC 9(02)   VALUE   10.
      *
      *    パラメータ最大個数
           03  WRK-CONS-PARA-MAX       PIC 9(05)   VALUE   10.
           03  WRK-CONS-PARA-L01-MAX   PIC 9(05)   VALUE   8.
           03  WRK-CONS-PARA-G01-MAX   PIC 9(05)   VALUE   10.
      *    最大明細数
           03  WRK-CONS-LINE-MAX       PIC 9(05)   VALUE   100.
      *    １画面あたりの最大明細数
           03  WRK-CONS-SCR-MAX        PIC 9(05)   VALUE   10.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  WRK-CONS-3003-KBNCD     PIC X(08)   VALUE   "051".
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID    PIC X(08)   VALUE   "DAILY".
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  WRK-CONS-52-3003-KBNCD  PIC X(08)   VALUE   "052G01".
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-52-JOB-SHELLID PIC X(08)   VALUE   "MONTHLY".
           03  WRK-CONS-52-JOB-JOBID   PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPSTATISTICSFORMV3REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPSTATISTICSFORMV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
           COPY    "MONBLOB".
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
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （医療機関情報）
      *    COPY    "CPSK1001.INC".
      *
           COPY    "CPSK3003.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
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
           DISPLAY   "*  statisticsform start  *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           INITIALIZE                      SPA-L01FREE
                                           SPA-L01KYOUTU
           INITIALIZE                      SPA-G01FREE
                                           SPA-G01KYOUTU
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
           IF      WRK-ERRCD           =   "E99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST10-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**************************"
           DISPLAY   "*  statisticsform   end  *"
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
           INITIALIZE                      XML-LSTRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "statistics form"    TO  LSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  LSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  LSTRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E99"            TO  WRK-ERRCD
               GO  TO 100-INIT-EXT
           END-IF
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
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE   LSTREQ-REQUEST-NUMBER
               WHEN    "00"
      *            情報取得
                   MOVE    9                   TO  FLG-SYORI
               WHEN    "01"
                   MOVE    1                   TO  FLG-SYORI
               WHEN    "02"
                   MOVE    2                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "E91"               TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE   LSTREQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理実施
                   PERFORM 2002-SYORI-01-SEC
               WHEN    "02"
      *            結果情報返却
                   PERFORM 2003-SYORI-02-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST10-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-LSTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_statistics_formv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST10-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "statistics_formv3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-LSTREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "01"        TO  LSTRES-RESPONSE-NUMBER
                   WHEN    OTHER
                       MOVE    "02"        TO  LSTRES-RESPONSE-NUMBER
               END-EVALUATE
       *
               MOVE    ZERO                TO  LSTRES-API-RESULT
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    9
                       MOVE    "情報取得終了"
                                           TO  LSTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "00"        TO  LSTRES-RESPONSE-NUMBER
                   WHEN    OTHER
                       MOVE    "01"        TO  LSTRES-RESPONSE-NUMBER
               END-EVALUATE
               IF      WRK-ERRMSG          =   SPACE
      *            エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  LSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  LSTRES-API-RESULT-MSG
      *
      *        エラー時は処理一覧をクリア
               IF    ( FLG-SYORI          =   2     )
               AND   ( WRK-ERRCD          =   "E30" )
                   CONTINUE
               ELSE
                   INITIALIZE              LSTRES-PROCESSING-G
               END-IF
           END-IF
      *
           MOVE   LSTREQ-STATISTICS-MODE
                                       TO  LSTRES-STATISTICS-MODE
           MOVE   LSTREQ-KARTE-UID     TO  LSTRES-KARTE-UID
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI           =   1
                   IF      WRK-ERRCD           =   SPACE
                       MOVE   LSTREQ-ORCA-UID      TO  LSTRES-ORCA-UID
                   END-IF
                   EVALUATE    LSTREQ-STATISTICS-MODE
                       WHEN    "Daily"
                           MOVE    SPA-L01-MONBLOB-PRINT-ID
                                               TO  LSTRES-PRINT-ID (1)
                       WHEN    "Monthly"
                           MOVE    SPA-G01-MONBLOB-PRINT-ID
                                               TO  LSTRES-PRINT-ID (1)
                   END-EVALUATE
               ELSE
                   MOVE   LSTREQ-ORCA-UID  TO  LSTRES-ORCA-UID
               END-IF
           END-IF
      *
           PERFORM 3001-LSTRES-SEC
           .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-LSTRES-SEC        SECTION.
      *
           IF      APILST10-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_statistics_formv3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  LSTRES-MODE
           MOVE    "statistics_formv3res"
                                       TO  LSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-LSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    LSTRES-OBJECT       TO  APILST10-BODY 
               MOVE    "application/xml"   TO  APILST10-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-LSTRES-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           MOVE   LSTREQ-REQUEST-NUMBER
                                       TO  LSTRES-REQUEST-NUMBER
      *
           MOVE    ZERO                TO  WRK-ERR-PARAMETER-IDX
      *
      *    カルテUID
           IF     LSTREQ-KARTE-UID     =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    オルカUID
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "00"    OR  "01"
               CONTINUE
           ELSE
               IF     LSTREQ-ORCA-UID      =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    指示区分
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
               WHEN    "Monthly"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "E15"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
           END-EVALUATE
      *
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    処理実施のとき処理一覧チェック
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "01"
               PERFORM 20011-PROCESSING-CHECK-SEC
      *        エラーがあれば処理終了
               IF    ( WRK-ERRCD       NOT =   SPACE )
               OR    ( WRK-ERR-PROCESSING-MAX
                                           >   ZERO  )
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   PERFORM 20012-DAILY-CHECK-SEC
               WHEN    "Monthly"
                   PERFORM 20013-MONTHLY-CHECK-SEC
           END-EVALUATE
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理一覧チェック処理
      *****************************************************************
       20011-PROCESSING-CHECK-SEC      SECTION.
      *
           INITIALIZE                  WRK-PARM-G
                                       WRK-PAGE-G
      *
      *    パラメタチェック
           IF      LSTREQ-PROCESSING-G =   SPACE
               MOVE    "E17"               TO  WRK-ERRCD
               GO  TO  20011-PROCESSING-CHECK-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
      *
              IF      LSTREQ-PROGRAM-NO (IDX)
                                       =   SPACE
                  CONTINUE
               ELSE
                   INITIALIZE                  ORCSNUMAREA
                   MOVE   LSTREQ-PROGRAM-NO (IDX)
                                           TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF    ( SNUM-RC     NOT =   ZERO  )
                   OR    ( SNUM-SYOKBN     =   "1"   )
                   OR    ( SNUM-MINKBN     =   "1"   )
                       MOVE    "E14"       TO  WRK-PROCESSING-ERRCD
                   ELSE
                       EVALUATE    SNUM-OUTNUM
                           WHEN    1   THRU    100
                               MOVE    SNUM-OUTNUM
                                               TO  WRK-PARM-PROGRAM-NO
                                                                   (IDX)
                           WHEN    OTHER
                               MOVE    "E14"   TO  WRK-PROCESSING-ERRCD
                       END-EVALUATE
                   END-IF
               END-IF
      *        プログラム名のチェック
               IF       LSTREQ-PROGRAM-NAME (IDX)
                                       =   SPACE
                   MOVE    "E19"           TO  WRK-PROCESSING-ERRCD
               ELSE
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    LSTREQ-PROGRAM-NAME (IDX)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-RC2     =   ZERO )
                   AND   ( KANACHK-SYUBETU =   1    )
                       CONTINUE
                   ELSE
                        MOVE    "E20"           TO  WRK-PROCESSING-ERRCD
                  END-IF
               END-IF
      *        パラメタのチェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-PARA-MAX
                       OR      LSTREQ-PARAMETER-INFO (IDX IDY)
                                   =    SPACE
                       OR      WRK-PROCESSING-ERRCD
                               NOT =   SPACE
                   IF      LSTREQ-PARM-NO  (IDX IDY)
                                           =   SPACE
                       MOVE    "E21"           TO  WRK-PROCESSING-ERRCD
                   ELSE
                       INITIALIZE                  ORCSNUMAREA
                       MOVE   LSTREQ-PARM-NO  (IDX IDY)
                                               TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF    ( SNUM-RC     NOT =   ZERO  )
                       OR    ( SNUM-SYOKBN     =   "1"   )
                       OR    ( SNUM-MINKBN     =   "1"   )
                           MOVE    "E22"       TO  WRK-PROCESSING-ERRCD
                       ELSE
                           EVALUATE    LSTREQ-STATISTICS-MODE
                               WHEN    "Daily"
                                   EVALUATE    SNUM-OUTNUM
                                       WHEN    1   THRU    8
                                           MOVE    SNUM-OUTNUM TO
                                                       WRK-PARM-PARM-NO
                                                               (IDX IDY)
                                       WHEN    OTHER
                                           MOVE    "E22"       TO
                                                   WRK-PROCESSING-ERRCD
                                   END-EVALUATE
                               WHEN    "Monthly"
                                   EVALUATE    SNUM-OUTNUM
                                       WHEN    1   THRU    10
                                           MOVE    SNUM-OUTNUM TO
                                                       WRK-PARM-PARM-NO
                                                               (IDX IDY)
                                       WHEN    OTHER
                                           MOVE    "E22"       TO
                                                   WRK-PROCESSING-ERRCD
                                   END-EVALUATE
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   IF      WRK-PROCESSING-ERRCD    =   SPACE
                       IF      LSTREQ-PARM-CLASS (IDX IDY)
                                               =   SPACE
                           MOVE    "E23"       TO  WRK-PROCESSING-ERRCD
                       ELSE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI 
                           MOVE    LSTREQ-PARM-CLASS (IDX IDY)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                               CONTINUE
                           ELSE
                               MOVE    "E24"           TO
                                                   WRK-PROCESSING-ERRCD
                           END-IF
                       END-IF
                   END-IF 
      *
                   IF      WRK-PROCESSING-ERRCD
                                       NOT =   SPACE
                       MOVE    IDY         TO  WRK-ERR-PARAMETER-IDX
                   END-IF
               END-PERFORM
      *
               IF      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   PERFORM 20019-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  20011-PROCESSING-CHECK-EXT
           END-IF
      *
      *    重複チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
      *        プログラム名の重複チェック
               COMPUTE IDY =   IDX +   1
               PERFORM VARYING IDYY    FROM    IDY  BY  1
                       UNTIL   IDYY    >       WRK-CONS-PROCESSING-MAX
                       OR      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   IF       LSTREQ-PROGRAM-NAME (IDX)
                                       =   LSTREQ-PROGRAM-NAME (IDYY)
                       MOVE    "E27"       TO  WRK-PROCESSING-ERRCD
                   END-IF
               END-PERFORM
      *        パラメタ番号の重複チェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-PARA-MAX
                       OR      LSTREQ-PARAMETER-INFO (IDX IDY)
                                   =       SPACE
                       OR      WRK-PROCESSING-ERRCD
                               NOT =   SPACE
                   COMPUTE IDXX    =   IDY +   1
                   PERFORM VARYING IDYY    FROM    IDXX    BY  1
                           UNTIL   IDYY    >       WRK-CONS-PARA-MAX
                           OR      LSTREQ-PARAMETER-INFO (IDX IDYY)
                                           =       SPACE
                           OR      WRK-PROCESSING-ERRCD
                                       NOT =   SPACE
                       IF       LSTREQ-PARM-NO  (IDX IDY)
                                           =   LSTREQ-PARM-NO(IDX IDYY)
                           MOVE    "E28"       TO  WRK-PROCESSING-ERRCD
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               IF      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   PERFORM 20019-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
           .
       20011-PROCESSING-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日次統計処理チェック
      *****************************************************************
       20012-DAILY-CHECK-SEC      SECTION.
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                  SPA-L01KYOUTU
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGL01-CALL-SEC
      *
      *    情報取得のとき処理一覧を取得
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "00"
               MOVE    ZERO            TO  IDXX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                   IF    ( SPA-GMN-L01-PRGNAME (IDX)
                                       NOT =   SPACE )
                   AND   ( SPA-GMN-L01-HCNAME  (IDX)
                                       NOT =   SPACE )
                       ADD     1           TO  IDXX
      *
                       MOVE    IDX         TO  WRK-NUM
                       MOVE    WRK-NUM-X   TO  LSTRES-PROGRAM-NO
                                                               (IDXX)
                       MOVE    SPA-GMN-L01-PRGNAME (IDX)
                                           TO  LSTRES-PROGRAM-NAME
                                                               (IDXX)
                       MOVE    SPA-GMN-L01-HCNAME  (IDX)
                                           TO  LSTRES-PROGRAM-LABEL
                                                               (IDXX)
                       IF      SPA-MW      =   SPA-GINBEE
                           MOVE    IDX         TO  IDXI
                           PERFORM 800-IS-ENABLED-SEC
                           IF      FLG-ENABLED =   ZERO
                               MOVE    "False" TO  LSTRES-PERFORM-CLASS
                                                               (IDXX)
                           ELSE
                               MOVE    "True"  TO  LSTRES-PERFORM-CLASS
                                                               (IDXX)
                           END-IF
                       END-IF
                       MOVE    ZERO        TO  IDYY
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-CONS-PARA-L01-MAX
                           IF     SPA-NAI-L01-PARA (IDX IDY)
                                           NOT =   SPACE
                               ADD     1       TO  IDYY
      *
                               MOVE    IDY     TO  WRK-NUM-2
                               MOVE    WRK-NUM-2-X
                                               TO   LSTRES-PARM-NO
                                                           (IDXX IDYY)
                               MOVE    SPA-GMN-L01-PARAHYOJI (IDX IDY)
                                               TO   LSTRES-PARM-LABEL
                                                          (IDXX IDYY)
                               MOVE    SPA-NAI-L01-PARA      (IDX IDY)
                                               TO   LSTRES-PARM-CLASS
                                                          (IDXX IDYY)
                               IF      SPA-NAI-L01-HISSU-KBN (IDX IDY)
                                               =   "1"
                                   MOVE    "True"  TO
                                               LSTRES-PARM-REQUIRED-ITEM
                                                          (IDXX IDYY)
                               ELSE
                                   MOVE    "False" TO
                                               LSTRES-PARM-REQUIRED-ITEM
                                                          (IDXX IDYY)
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *        システム管理に登録処理なし
               IF      IDXX                =   ZERO
                   MOVE    "E16"               TO  WRK-ERRCD
               END-IF
               GO  TO  20012-DAILY-CHECK-EXT
           END-IF
      *
      *    システム管理の登録内容とのチェック
      *****INITIALIZE                  WRK-PARM-G
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
               MOVE    ZERO            TO  FLG-CHK
      *
      *        プログラム番号設定時は指定番号でチェック
               IF      LSTREQ-PROGRAM-NO (IDX)
                                   NOT =   SPACE
                   MOVE    WRK-PARM-PROGRAM-NO (IDX)
                                           TO  ST-IDX
                                               ED-IDX
               ELSE
                   MOVE    1               TO  ST-IDX
                   MOVE    WRK-CONS-LINE-MAX
                                           TO  ED-IDX
               END-IF
      *
               PERFORM VARYING IDXX    FROM    ST-IDX  BY  1
                       UNTIL   IDXX    >       ED-IDX
                       OR      FLG-CHK >       ZERO
                       OR      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
      *            プログラム番号設定時のプログラム名の存在チェック
                   IF      LSTREQ-PROGRAM-NO (IDX)
                                   NOT =   SPACE
                       IF     SPA-GMN-L01-PRGNAME (IDXX)
                                           =   SPACE
                           MOVE    9               TO  FLG-CHK
                       END-IF
                   END-IF
      *
      *            プログラム名のチェック
                   IF     LSTREQ-PROGRAM-NAME (IDX)
                                       =   SPA-GMN-L01-PRGNAME (IDXX)
                     MOVE    IDXX          TO  IDXI
                     PERFORM 800-IS-ENABLED-SEC
                     IF      FLG-ENABLED     =   ZERO
                       MOVE    8               TO  FLG-CHK
                     ELSE
                       MOVE    1               TO  FLG-CHK
      *
                       COMPUTE IDZ     = ( IDXX
                                       +   WRK-CONS-SCR-MAX   -   1 )
                                       /   WRK-CONS-SCR-MAX
                       COMPUTE IDZZ    =   IDXX
                                       -   ( ( IDZ -   1 )
                                               *   WRK-CONS-SCR-MAX )
                       MOVE    IDZ             TO  WRK-PARM-PAGE (IDX)
                       MOVE    IDZZ            TO  WRK-PARM-LINE (IDX)
                       MOVE    IDXX            TO  WRK-PARM-INDEX(IDX)
                       MOVE    1               TO  WRK-PAGE-FLG  (IDZ)
                       IF      WRK-PAGE-MAX    <   IDZ
                           MOVE    IDZ             TO  WRK-PAGE-MAX
                       END-IF
      *
      *                パラメタ種別のチェック
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-CONS-PARA-L01-MAX
                               OR      LSTREQ-PARM-NO (IDX IDY)
                                           =   SPACE
                               OR      WRK-PROCESSING-ERRCD
                                       NOT =   SPACE
                           MOVE    WRK-PARM-PARM-NO (IDX IDY)
                                                   TO  IDYY
                           IF     SPA-NAI-L01-PARA (IDXX IDYY)
                                               =   SPACE
                                   MOVE    "E25"       TO
                                                   WRK-PROCESSING-ERRCD
                           ELSE
                               IF      LSTREQ-PARM-CLASS (IDX IDY)
                                                   =   SPA-NAI-L01-PARA
                                                          (IDXX IDYY)
                                   CONTINUE
                               ELSE
                                   MOVE    "E29"       TO
                                                   WRK-PROCESSING-ERRCD
                               END-IF
                           END-IF
                           IF      WRK-PROCESSING-ERRCD
                                           NOT =   SPACE
                               MOVE    IDY         TO
                                                   WRK-ERR-PARAMETER-IDX
                           END-IF
                       END-PERFORM
      *
                       MOVE    IDX         TO  WRK-LSTREQ-PROGRAM-NO
                                                                  (IDXX)
      *
      *                処理一覧に返却
                       MOVE    IDXX        TO  WRK-NUM
                       MOVE    WRK-NUM-X   TO  LSTRES-PROGRAM-NO   (IDX)
                       MOVE    SPA-GMN-L01-PRGNAME (IDXX)
                                           TO  LSTRES-PROGRAM-NAME (IDX)
                       MOVE    SPA-GMN-L01-HCNAME  (IDXX)
                                           TO  LSTRES-PROGRAM-LABEL(IDX)
                     END-IF
                   END-IF
               END-PERFORM
      *
               IF      FLG-CHK         =   ZERO
                   IF      LSTREQ-PROGRAM-NO (IDX)
                                           =   SPACE
                       MOVE    "E18"           TO  WRK-PROCESSING-ERRCD
                   ELSE
                       MOVE    "E31"           TO  WRK-PROCESSING-ERRCD
                   END-IF
               ELSE
                   IF      FLG-CHK             =   8
                       MOVE    "E33"           TO  WRK-PROCESSING-ERRCD
                   END-IF
                   IF      FLG-CHK             =   9
                       MOVE    "E32"           TO  WRK-PROCESSING-ERRCD
                   END-IF
               END-IF
      *
               IF      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   PERFORM 20019-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  20012-DAILY-CHECK-EXT
           END-IF
      *
      *    パラメタ値の入力チェック
           MOVE    ZERO               TO  IDZZ
           PERFORM         UNTIL   IDZZ    >=  WRK-PAGE-MAX
               MOVE    SPA-GMN-L01-PAGE    TO  IDZZ
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                       OR      LSTREQ-PROCESSING-INFO (IDX)
                                   =       SPACE
      *            該当頁に設定された処理があるときパラメタ値を設定
                   IF     IDZZ     =    WRK-PARM-PAGE (IDX)
                       MOVE    WRK-PARM-INDEX (IDX)
                                                   TO  IDY
                       MOVE    "T"                 TO
                                           SPA-GMN-L01-CHK-VALUE (IDY)
                       MOVE    WRK-PARM-LINE (IDX) TO  IDY
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >   WRK-CONS-PARA-L01-MAX
                               OR      WRK-PARM-PARM-NO (IDX IDXX)
                                               =   ZERO
                           MOVE    WRK-PARM-PARM-NO (IDX IDXX)
                                                   TO  IDWW
                           EVALUATE    LSTREQ-PARM-CLASS (IDX IDXX)
                               WHEN    "YMD"
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO  WRK-HEN-DATE
                                   PERFORM 802-DAYHEN02-SEC
                                   MOVE    WRK-SYMD    TO
                                                   L01-PARA (IDY IDWW)
                               WHEN    "YM"
                               WHEN    "KKNYM"
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO  WRK-HEN-DATE
                                   PERFORM 802-DAYHEN02-SEC
                                   MOVE    WRK-SYMD    TO
                                                   L01-PARA (IDY IDWW)
                               WHEN    OTHER
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO
                                                   L01-PARA (IDY IDWW)
                           END-EVALUATE
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
               INITIALIZE                  SPA-LIDCD
                                           SPA-LID1-AREA
               MOVE    LSTREQ-KARTE-UID
                                       TO  SPA-L01-JOB-KARTE-UID
               MOVE    LSTREQ-ORCA-UID TO  SPA-L01-JOB-ORCA-UID
               MOVE    "PUTG"          TO  MCP-STATUS
               MOVE    "CLICKED"       TO  MCP-EVENT
               IF      SPA-GMN-L01-PAGE
                                       =   WRK-PAGE-MAX
      *            更新前チェック
                   MOVE    "B12"           TO  MCP-WIDGET
               ELSE
      *            次頁チェック
                   MOVE    "B07"           TO  MCP-WIDGET
               END-IF
      *
               PERFORM 800-ORCGL01-CALL-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   EVALUATE    SPA-ERRCD
                       WHEN    "0002"
                       WHEN    "0103"
                       WHEN    "0004"
                           MOVE    "E61"           TO
                                                   WRK-PROCESSING-ERRCD
                           PERFORM 20019-PROCESSING-ERR-SEC
                       WHEN    "7777"
                           MOVE    "E70"               TO  WRK-ERRCD
                           MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                       WHEN    OTHER
                           MOVE    "E60"           TO  WRK-ERRCD
                   END-EVALUATE
                   GO  TO  20012-DAILY-CHECK-EXT
               END-IF
           END-PERFORM
      *
           .
       20012-DAILY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    月次統計処理チェック
      *****************************************************************
       20013-MONTHLY-CHECK-SEC      SECTION.
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                  SPA-G01KYOUTU
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGL01-CALL-SEC
      *
      *    情報取得のとき処理一覧を取得
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "00"
               MOVE    ZERO            TO  IDXX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                   IF    ( SPA-GMN-G01-PRGNAME (IDX)
                                       NOT =   SPACE )
                   AND   ( SPA-GMN-G01-HCNAME  (IDX)
                                       NOT =   SPACE )
                       ADD     1           TO  IDXX
      *
                       MOVE    IDX         TO  WRK-NUM
                       MOVE    WRK-NUM-X   TO   LSTRES-PROGRAM-NO
                                                               (IDXX)
                       MOVE    SPA-GMN-G01-PRGNAME (IDX)
                                           TO   LSTRES-PROGRAM-NAME
                                                               (IDXX)
                       MOVE    SPA-GMN-G01-HCNAME  (IDX)
                                           TO   LSTRES-PROGRAM-LABEL
                                                               (IDXX)
                       IF      SPA-MW      =   SPA-GINBEE
                           MOVE    IDX         TO  IDXI
                           PERFORM 800-IS-ENABLED-SEC
                           IF      FLG-ENABLED =   ZERO
                               MOVE    "False" TO  LSTRES-PERFORM-CLASS
                                                               (IDXX)
                           ELSE
                               MOVE    "True"  TO  LSTRES-PERFORM-CLASS
                                                               (IDXX)
                           END-IF
                       END-IF
                       MOVE    ZERO        TO  IDYY
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-CONS-PARA-G01-MAX
                           IF     SPA-NAI-G01-PARA (IDX IDY)
                                           NOT =   SPACE
                               ADD     1       TO  IDYY
      *
                               MOVE    IDY     TO  WRK-NUM-2
                               MOVE    WRK-NUM-2-X
                                               TO  LSTRES-PARM-NO
                                                           (IDXX IDYY)
                               MOVE    SPA-GMN-G01-PARAHYOJI (IDX IDY)
                                               TO  LSTRES-PARM-LABEL
                                                          (IDXX IDYY)
                               MOVE    SPA-NAI-G01-PARA      (IDX IDY)
                                               TO  LSTRES-PARM-CLASS
                                                          (IDXX IDYY)
                               IF      SPA-NAI-G01-HISSU-KBN (IDX IDY)
                                               =   "1"
                                   MOVE    "True"  TO
                                               LSTRES-PARM-REQUIRED-ITEM
                                                          (IDXX IDYY)
                               ELSE
                                   MOVE    "False" TO
                                               LSTRES-PARM-REQUIRED-ITEM
                                                          (IDXX IDYY)
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *        システム管理に登録処理なし
               IF      IDXX                =   ZERO
                   MOVE    "E16"               TO  WRK-ERRCD
               END-IF
               GO  TO  20013-MONTHLY-CHECK-EXT
           END-IF
      *
      *    システム管理の登録内容とのチェック
      *****INITIALIZE                  WRK-PARM-G
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
               MOVE    ZERO            TO  FLG-CHK
      *
      *        プログラム番号設定時は指定番号でチェック
               IF      LSTREQ-PROGRAM-NO (IDX)
                                   NOT =   SPACE
                   MOVE    WRK-PARM-PROGRAM-NO (IDX)
                                           TO  ST-IDX
                                               ED-IDX
               ELSE
                   MOVE    1               TO  ST-IDX
                   MOVE    WRK-CONS-LINE-MAX
                                           TO  ED-IDX
               END-IF
      *
               PERFORM VARYING IDXX    FROM    ST-IDX  BY  1
                       UNTIL   IDXX    >       ED-IDX
                       OR      FLG-CHK >       ZERO
                       OR      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
      *            プログラム番号設定時のプログラム名の存在チェック
                   IF      LSTREQ-PROGRAM-NO (IDX)
                                   NOT =   SPACE
                       IF     SPA-GMN-G01-PRGNAME (IDXX)
                                           =   SPACE
                           MOVE    9               TO  FLG-CHK
                       END-IF
                   END-IF
      *
      *            プログラム名のチェック
                   IF     LSTREQ-PROGRAM-NAME (IDX)
                                       =   SPA-GMN-G01-PRGNAME (IDXX)
                     MOVE    IDXX          TO  IDXI
                     PERFORM 800-IS-ENABLED-SEC
                     IF      FLG-ENABLED     =   ZERO
                       MOVE    8               TO  FLG-CHK
                     ELSE
                       MOVE    1               TO  FLG-CHK
      *
                       COMPUTE IDZ     = ( IDXX
                                       +   WRK-CONS-SCR-MAX   -   1 )
                                       /   WRK-CONS-SCR-MAX
                       COMPUTE IDZZ    =   IDXX
                                       -   ( ( IDZ -   1 )
                                               *   WRK-CONS-SCR-MAX )
                       MOVE    IDZ             TO  WRK-PARM-PAGE (IDX)
                       MOVE    IDZZ            TO  WRK-PARM-LINE (IDX)
                       MOVE    IDXX            TO  WRK-PARM-INDEX(IDX)
                       MOVE    1               TO  WRK-PAGE-FLG  (IDZ)
                       IF      WRK-PAGE-MAX    <   IDZ
                           MOVE    IDZ             TO  WRK-PAGE-MAX
                       END-IF
      *
      *                パラメタ種別のチェック
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-CONS-PARA-G01-MAX
                               OR      LSTREQ-PARM-NO (IDX IDY)
                                           =   SPACE
                               OR      WRK-PROCESSING-ERRCD
                                       NOT =   SPACE
                           MOVE    WRK-PARM-PARM-NO (IDX IDY)
                                                   TO  IDYY
                           IF     SPA-NAI-G01-PARA (IDXX IDYY)
                                               =   SPACE
                                   MOVE    "E25"       TO
                                                   WRK-PROCESSING-ERRCD
                           ELSE
                               IF      LSTREQ-PARM-CLASS (IDX IDY)
                                                   =   SPA-NAI-G01-PARA
                                                          (IDXX IDYY)
                                   CONTINUE
                               ELSE
                                   MOVE    "E29"       TO
                                                   WRK-PROCESSING-ERRCD
                               END-IF
                           END-IF
                           IF      WRK-PROCESSING-ERRCD
                                           NOT =   SPACE
                               MOVE    IDY         TO
                                                   WRK-ERR-PARAMETER-IDX
                           END-IF
                       END-PERFORM
      *
                       MOVE    IDX         TO  WRK-LSTREQ-PROGRAM-NO
                                                                  (IDXX)
      *
      *                処理一覧に返却
                       MOVE    IDXX        TO  WRK-NUM
                       MOVE    WRK-NUM-X   TO  LSTRES-PROGRAM-NO   (IDX)
                       MOVE    SPA-GMN-G01-PRGNAME (IDXX)
                                           TO  LSTRES-PROGRAM-NAME (IDX)
                       MOVE    SPA-GMN-G01-HCNAME  (IDXX)
                                           TO  LSTRES-PROGRAM-LABEL(IDX)
                     END-IF
                   END-IF
               END-PERFORM
      *
               IF      FLG-CHK         =   ZERO
                   IF      LSTREQ-PROGRAM-NO (IDX)
                                           =   SPACE
                       MOVE    "E18"           TO  WRK-PROCESSING-ERRCD
                   ELSE
                       MOVE    "E31"           TO  WRK-PROCESSING-ERRCD
                   END-IF
               ELSE
                   IF      FLG-CHK             =   8
                       MOVE    "E33"           TO  WRK-PROCESSING-ERRCD
                   END-IF
                   IF      FLG-CHK             =   9
                       MOVE    "E32"           TO  WRK-PROCESSING-ERRCD
                   END-IF
               END-IF
      *
               IF      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   PERFORM 20019-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  20013-MONTHLY-CHECK-EXT
           END-IF
      *
      *    パラメタ値の入力チェック
           MOVE    ZERO               TO  IDZZ
           PERFORM         UNTIL   IDZZ    >=  WRK-PAGE-MAX
               MOVE    SPA-GMN-G01-PAGE    TO  IDZZ
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                       OR      LSTREQ-PROCESSING-INFO (IDX)
                                   =       SPACE
      *            該当頁に設定された処理があるときパラメタ値を設定
                   IF     IDZZ     =    WRK-PARM-PAGE (IDX)
                       MOVE    WRK-PARM-INDEX (IDX)
                                                   TO  IDY
                       MOVE    "T"                 TO
                                           SPA-GMN-G01-CHK-VALUE (IDY)
                       MOVE    WRK-PARM-LINE (IDX) TO  IDY
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >   WRK-CONS-PARA-G01-MAX
                               OR      WRK-PARM-PARM-NO (IDX IDXX)
                                               =   ZERO
                           MOVE    WRK-PARM-PARM-NO (IDX IDXX)
                                                   TO  IDWW
                           EVALUATE    LSTREQ-PARM-CLASS (IDX IDXX)
                               WHEN    "YMD"
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO  WRK-HEN-DATE
                                   PERFORM 802-DAYHEN02-SEC
                                   MOVE    WRK-SYMD    TO
                                                   G01-PARA (IDY IDWW)
                               WHEN    "YM"
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO  WRK-HEN-DATE
                                   PERFORM 802-DAYHEN02-SEC
                                   MOVE    WRK-SYMD    TO
                                                   G01-PARA (IDY IDWW)
                               WHEN    "KKNYM"
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO  WRK-HEN-DATE
                                   PERFORM 802-DAYHEN02-SEC
                                   MOVE    WRK-SYMD    TO
                                                   G01-PARA (IDY IDWW)
                               WHEN    OTHER
                                   MOVE    LSTREQ-PARM-VALUE  (IDX IDXX)
                                                       TO
                                                   G01-PARA (IDY IDWW)
                           END-EVALUATE
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
               INITIALIZE                  SPA-GIDCD
                                           SPA-GID1-AREA
               MOVE    LSTREQ-KARTE-UID
                                       TO  SPA-G01-JOB-KARTE-UID
               MOVE    LSTREQ-ORCA-UID TO  SPA-G01-JOB-ORCA-UID
               MOVE    "PUTG"          TO  MCP-STATUS
               MOVE    "CLICKED"       TO  MCP-EVENT
               IF      SPA-GMN-G01-PAGE
                                       =   WRK-PAGE-MAX
      *            更新前チェック
                   MOVE    "B12"           TO  MCP-WIDGET
               ELSE
      *            次頁チェック
                   MOVE    "B07"           TO  MCP-WIDGET
               END-IF
      *
               PERFORM 800-ORCGL01-CALL-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   EVALUATE    SPA-ERRCD
                       WHEN    "0002"
                       WHEN    "0103"
                       WHEN    "0004"
                           MOVE    "E61"           TO
                                                   WRK-PROCESSING-ERRCD
                           PERFORM 20019-PROCESSING-ERR-SEC
                       WHEN    "7777"
                           MOVE    "E70"               TO  WRK-ERRCD
                           MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                       WHEN    OTHER
                           MOVE    "E60"           TO  WRK-ERRCD
                   END-EVALUATE
                   GO  TO  20013-MONTHLY-CHECK-EXT
               END-IF
           END-PERFORM
      *
           .
       20013-MONTHLY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理別エラー処理
      *****************************************************************
       20019-PROCESSING-ERR-SEC     SECTION.
      *
           ADD     1               TO  WRK-ERR-PROCESSING-MAX
           IF      SPA-ERRCD       =   SPACE
               MOVE    WRK-PROCESSING-ERRCD
                                       TO  LSTRES-ERROR-CODE
                                               (WRK-ERR-PROCESSING-MAX)
               MOVE    LSTREQ-PROGRAM-NO   (IDX)
                                       TO  LSTRES-ERR-PROGRAM-NO
                                               (WRK-ERR-PROCESSING-MAX)
               MOVE    LSTREQ-PROGRAM-NAME (IDX)
                                       TO  LSTRES-ERR-PROGRAM-NAME
                                               (WRK-ERR-PROCESSING-MAX)
               EVALUATE    WRK-PROCESSING-ERRCD
                   WHEN    "E14"
                       MOVE    "プログラム番号の設定に誤りがあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E18"
                       MOVE    "設定したプログラム名が未登録です。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E19"
                       MOVE    "プログラム名が未設定です。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E20"
                       MOVE    "プログラム名の設定に誤りがあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E21"
                       MOVE    "パラメタ番号が未設定です。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E22"
                       MOVE    "パラメタ番号の設定に誤りがあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E23"
                       MOVE    "パラメタ種別が未設定です。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E24"
                       MOVE    "パラメタ種別の設定に誤りがあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E25"
                       MOVE
                   "設定したパラメタ番号にパラメタの登録がありません。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E26"
                       MOVE    "パラメタ値の設定に誤りがあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E27"
                       MOVE
                           "プログラム名が重複して設定してあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E28"
                       MOVE
                           "パラメタ番号が重複して設定してあります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E29"
                       MOVE    "登録してあるパラメタ種別と異なります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E31"
                       MOVE    "登録してあるプログラム名と異なります。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E32"
                       MOVE
                      "設定したプログラム番号に処理の登録がありません。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                   WHEN    "E33"
                       MOVE   "設定したプログラムは処理できません。"
                                               TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E63"           TO  WRK-ERRCD
                   MOVE    LSTRES-ERROR-MSG (WRK-ERR-PROCESSING-MAX)
                                           TO  WRK-ERRMSG
               END-IF
           ELSE
               EVALUATE    LSTREQ-STATISTICS-MODE
                   WHEN    "Daily"
                       EVALUATE    WRK-PROCESSING-ERRCD
                           WHEN    "E61"
      *                        IDBB PARA INDEX
      *                        IDCC SPA LINE
                               MOVE    SPA-L01-PARAHYOJI-1 TO  IDCC
                               MOVE    SPA-L01-PARAHYOJI-2 TO  IDBB
      *
                               MOVE    WRK-PROCESSING-ERRCD
                                           TO  LSTRES-ERROR-CODE
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-L01-NUM     (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-NO
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-L01-PRGNAME (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-NAME
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-L01-HCNAME  (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-LABEL
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    IDBB
                                           TO  WRK-ERR-PARAMETER-IDX
      *
                               MOVE    WRK-LSTREQ-PROGRAM-NO (IDCC)
                                           TO  IDX
                       END-EVALUATE
      *
                       EVALUATE    SPA-ERRCD
                           WHEN    "0002"
                           WHEN    "0004"
                           WHEN    "0103"
                               MOVE    SPACE       TO  WRK-ERR-1
                                                       WRK-ERR-2
                               UNSTRING    SPA-ERRMSG
                                                   DELIMITED  BY  "【"
                                                   INTO    WRK-ERR-1
                                                           WRK-ERR-2
                               END-UNSTRING
                               STRING  WRK-ERR-1   DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                       SPA-GMN-L01-PARAHYOJI (IDCC IDBB)
                                                   DELIMITED  BY  SPACE
                                       "】"        DELIMITED  BY  SIZE
                                           INTO    LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                               END-STRING
                           WHEN    OTHER
                               MOVE    SPA-ERRMSG  TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                       END-EVALUATE
                   WHEN    "Monthly"
                       EVALUATE    WRK-PROCESSING-ERRCD
                           WHEN    "E61"
      *                        IDBB PARA INDEX
      *                        IDCC SPA LINE
                               MOVE    SPA-G01-PARAHYOJI-1 TO  IDCC
                               MOVE    SPA-G01-PARAHYOJI-2 TO  IDBB
       *
                               MOVE    WRK-PROCESSING-ERRCD
                                           TO  LSTRES-ERROR-CODE
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-G01-NUM     (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-NO
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-G01-PRGNAME (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-NAME
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    SPA-GMN-G01-HCNAME  (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-LABEL
                                               (WRK-ERR-PROCESSING-MAX)
                               MOVE    IDBB
                                           TO  WRK-ERR-PARAMETER-IDX
      *
                               MOVE    WRK-LSTREQ-PROGRAM-NO (IDCC)
                                           TO  IDX
                       END-EVALUATE
      *
                       EVALUATE    SPA-ERRCD
                           WHEN    "0002"
                           WHEN    "0004"
                           WHEN    "0103"
                               MOVE    SPACE       TO  WRK-ERR-1
                                                       WRK-ERR-2
                               UNSTRING    SPA-ERRMSG
                                                   DELIMITED  BY  "【"
                                                   INTO    WRK-ERR-1
                                                           WRK-ERR-2
                               END-UNSTRING
                               STRING  WRK-ERR-1   DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                       SPA-GMN-G01-PARAHYOJI (IDCC IDBB)
                                                   DELIMITED  BY  SPACE
                                       "】"        DELIMITED  BY  SIZE
                                           INTO    LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                               END-STRING
                           WHEN    OTHER
                               MOVE    SPA-ERRMSG  TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                       END-EVALUATE
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E63"           TO  WRK-ERRCD
                   MOVE    LSTRES-ERROR-MSG  (WRK-ERR-PROCESSING-MAX)
                                           TO  WRK-ERRMSG
               END-IF
           END-IF
      *
      *    パラメタ値返却
           IF      WRK-ERR-PARAMETER-IDX
                                   =   ZERO
               MOVE    LSTREQ-PARAMETER-G  (IDX)
                                       TO  LSTRES-ERR-PARAMETER-G
                                               (WRK-ERR-PROCESSING-MAX)
           ELSE
               MOVE    LSTREQ-PARAMETER-INFO (IDX WRK-ERR-PARAMETER-IDX)
                                       TO  LSTRES-ERR-PARAMETER-INFO
                                              (WRK-ERR-PROCESSING-MAX 1)
           END-IF
      *
           MOVE    ZERO            TO  WRK-ERR-PARAMETER-IDX
           .
       20019-PROCESSING-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実施処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  LSTREQ-ORCA-UID
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   MOVE    LSTREQ-KARTE-UID
                                           TO  SPA-L01-JOB-KARTE-UID
                   MOVE    LSTREQ-ORCA-UID TO  SPA-L01-JOB-ORCA-UID
                   MOVE    "statistics_form(Daily)"
                                           TO  SPA-L01-PUSHBA01-EVENT
      *
                   PERFORM 800-TEMPDIR-SEC
                   IF      WRK-ERRCD   NOT =   SPACE
                       GO  TO  2002-SYORI-01-EXT
                   END-IF
      *
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                                               SPA-LID1-AREA
      *
                   MOVE    "LID2"          TO  SPA-MOTOPG
                   MOVE    "OK"            TO  SPA-LID1-FLG
               WHEN    "Monthly"
                   MOVE    LSTREQ-KARTE-UID
                                           TO  SPA-G01-JOB-KARTE-UID
                   MOVE    LSTREQ-ORCA-UID TO  SPA-G01-JOB-ORCA-UID
                   MOVE    "statistics_form(Monthly)"
                                           TO  SPA-G01-PUSHBA01-EVENT
      *
                   PERFORM 800-TEMPDIR-SEC
                   IF      WRK-ERRCD   NOT =   SPACE
                       GO  TO  2002-SYORI-01-EXT
                   END-IF
      *
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                                               SPA-GID1-AREA
      *
                   MOVE    "GID2"          TO  SPA-MOTOPG
                   MOVE    "OK"            TO  SPA-GID1-FLG
           END-EVALUATE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           PERFORM 800-ORCGL01-CALL-SEC
      *
           IF      SPA-ERRCD   NOT =   SPACE
               IF      SPA-ERRCD       =   "E88"
                   MOVE    SPA-ERRCD       TO  WRK-ERRCD
               ELSE
                  MOVE    "E60"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       20021-UID-GET-SEC           SECTION.
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
      *****************************************************************
      *    結果情報返却処理
      *****************************************************************
       2003-SYORI-02-SEC           SECTION.
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               WHEN    "Monthly"
                   MOVE    WRK-CONS-52-3003-KBNCD
                                               TO  SYS-3003-KBNCD
           END-EVALUATE
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   EVALUATE    LSTREQ-STATISTICS-MODE
                       WHEN    "Daily"
                           MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                       WHEN    "Monthly"
                           MOVE    WRK-CONS-52-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   END-EVALUATE
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF    ( LSTREQ-KARTE-UID
                                           NOT =   JOB-KARTE-UID )
                       OR    ( LSTREQ-ORCA-UID
                                           NOT =   JOB-ORCA-UID  )
                           MOVE    "E41"           TO  WRK-ERRCD
                           GO  TO  2003-SYORI-02-EXT
                       END-IF
      *
      *                処理一覧に返却
                       IF      JOB-PGID        =   "ORCBPRT1"
                                               OR  "ORCBPUSH"
                           CONTINUE
                       ELSE
                           MOVE    JOB-PGID
                                           TO  LSTRES-PROGRAM-NAME (IDX)
                           MOVE    JOB-SHELLMSG
                                           TO  LSTRES-PROGRAM-LABEL(IDX)
                           MOVE    JOB-YOBI
                                           TO  LSTRES-PROGRAM-MSG  (IDX)
                           MOVE    JOB-UPDCNT
                                           TO  WRK-UPDCNT
                           MOVE    WRK-UPDCNT-X
                                           TO
                                       LSTRES-PROGRAM-OUTPUT-COUNT (IDX)
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
                           GO  TO  2003-SYORI-02-EXT
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE               )
      *********************OR    ( JOB-YOBI    =
      *********************        "処理対象のデータがありませんでした")
                               CONTINUE
                           ELSE
                               MOVE    "E30"       TO  WRK-ERRCD
                               MOVE    SPACE       TO  WRK-ERRMSG
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                       JOB-SHELLMSG
                                                   DELIMITED  BY  SPACE
                                       "】"        DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                               END-STRING
      *************************GO  TO  2003-SYORI-02-EXT
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "E40"           TO  WRK-ERRCD
                       GO  TO  2003-SYORI-02-EXT
                   END-IF
      *
                   IF      JOB-PGID        =   "ORCBPRT1"
                       MOVE    JOBKANRI-REC    TO  WRK-JOBKANRI-REC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ページ数返却
           MOVE    WRK-JOB-UPDCNT      TO  WRK-UPDCNT
           MOVE    WRK-UPDCNT-X        TO  LSTRES-OUTPUT-COUNT
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               WHEN    "Monthly"
                   MOVE    WRK-CONS-52-3003-KBNCD
                                               TO  SYS-3003-KBNCD
           END-EVALUATE
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   EVALUATE    LSTREQ-STATISTICS-MODE
                       WHEN    "Daily"
                           MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                       WHEN    "Monthly"
                           MOVE    WRK-CONS-52-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   END-EVALUATE
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "E70"           TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMSG
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                           GO  TO  800-JOBKANRI-CHK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日次月次統計処理
      *****************************************************************
       800-ORCGL01-CALL-SEC                SECTION.
      *
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   MOVE    SPA-L01KYOUTU   TO  SPA-WORK
                   MOVE    SPA-AREA        TO  SPA-COMMON
                   MOVE    SPA-L01FREE     TO  SPA-FREE
      *
                   CALL    "ORCGL01"           USING
                                                   MCPAREA
                                                   SPAAREA
                                                  LINKAREA
                                                  SCRAREA
      *
                   MOVE    SPA-COMMON      TO  SPA-AREA
                   MOVE    SPA-FREE        TO  SPA-L01FREE
                   MOVE    SPA-WORK        TO  SPA-L01KYOUTU
               WHEN    "Monthly"
                   MOVE    SPA-G01KYOUTU   TO  SPA-WORK
                   MOVE    SPA-AREA        TO  SPA-COMMON
                   MOVE    SPA-G01FREE     TO  SPA-FREE
      *
                   CALL    "ORCGG01"           USING
                                                   MCPAREA
                                                   SPAAREA
                                                  LINKAREA
                                                  SCRAREA
      *
                   MOVE    SPA-COMMON      TO  SPA-AREA
                   MOVE    SPA-FREE        TO  SPA-G01FREE
                   MOVE    SPA-WORK        TO  SPA-G01KYOUTU
           END-EVALUATE
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGL01-CALL-EXT.
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
      *    使用可否チェック処理
      *****************************************************************
       800-IS-ENABLED-SEC        SECTION.
      *
           MOVE    1               TO  FLG-ENABLED
      *
           EVALUATE    LSTREQ-STATISTICS-MODE
               WHEN    "Daily"
                   IF    ( SPA-MW                      =   SPA-GINBEE )
                   AND   ( SPA-GMN-L01-REPOS-NAME (IDXI)
                                                   NOT =   SPACE )
                   AND   ( SPA-L01-CUSTOM-ENABLED  NOT =   "1"   )
                       MOVE    ZERO        TO  FLG-ENABLED
                   END-IF
      *
                   IF    ( SPA-MW                       =   SPA-GINBEE )
                   AND ( SPA-GMN-L01-REPOS-NAME (IDXI)
                                                        =   SPACE )
                       MOVE    ZERO        TO  IDXU
                       PERFORM VARYING IDXT    FROM    1   BY  1
                               UNTIL ( IDXT    >   SPA-L01-PGLST-MAX )
                               OR    ( IDXU    >   ZERO )
                           IF    ( SPA-GMN-L01-PRGNAME (IDXI)
                                               =   SPA-L01-TPGID (IDXT))
                               MOVE    IDXT    TO  IDXU
                           END-IF
                       END-PERFORM
      *
                       IF    ( IDXU            =   ZERO )
                           MOVE    ZERO        TO  FLG-ENABLED
                       END-IF
                   END-IF
               WHEN    "Monthly"
                   IF    ( SPA-MW                      =   SPA-GINBEE )
                   AND   ( SPA-GMN-G01-REPOS-NAME (IDXI)
                                                   NOT =   SPACE )
                   AND   ( SPA-G01-CUSTOM-ENABLED  NOT =   "1" )
                       MOVE    ZERO        TO  FLG-ENABLED
                   END-IF
      *
                   IF    ( SPA-MW                      =   SPA-GINBEE )
                   AND   ( SPA-GMN-G01-REPOS-NAME (IDXI)
                                                       =   SPACE )
                       MOVE    ZERO        TO  IDXU
                       PERFORM VARYING IDXT    FROM    1   BY  1
                               UNTIL ( IDXT    >   SPA-G01-PGLST-MAX )
                               OR    ( IDXU    >   ZERO )
                           IF    ( SPA-GMN-G01-PRGNAME (IDXI)
                                               =   SPA-G01-TPGID (IDXT))
                               MOVE    IDXT    TO  IDXU
                           END-IF
                       END-PERFORM
      *
                       IF    ( IDXU            =   ZERO )
                           MOVE    ZERO        TO  FLG-ENABLED
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       800-IS-ENABLED-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
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
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
      *
      *    "9999-99"はエラーとする
           IF      WRK-SYMD (1:6)      =   "999999"
               MOVE    "12"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           ELSE
               IF    ( WRK-SYMD            =   "99991231" )
               OR    ( WRK-SYMD (1:6)      =   "999912"   )
                   MOVE    "99"                TO  WRK-SMM
                   MOVE    "99"                TO  WRK-SDD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E30"
               WHEN    "E70"
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "E04"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "E05"
               MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "E15"
               MOVE    "指示区分の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "E16"
               MOVE    "システム管理に処理が登録してありません。"
                                               TO  WRK-ERRMSG
           WHEN    "E17"
               MOVE    "対象の処理が未設定です。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "E40"
               MOVE    "対象のジョブ管理がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "E41"
               MOVE    "対象の処理はありません。"
                                               TO  WRK-ERRMSG
      *        画面エラー
           WHEN    "E60"
           WHEN    "E61"
           WHEN    "E88"
               MOVE    SPA-ERRMSG              TO  WRK-ERRMSG
      *
           WHEN    "E80"
               MOVE    "一時ディレクトリが取得できません。"
                                               TO  WRK-ERRMSG
      *共通エラー
           WHEN    "E89"
      *        ORCSCOMMON のエラー
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
                   MOVE    "他端末で使用中です。"
                                               TO  WRK-ERRMSG
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
               INITIALIZE                      SYSKANRI-REC
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
      *
           .
       910-DBSELECT-EXT.
           EXIT.
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
      *
           .
       910-DBFETCH-EXT.
           EXIT.
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
