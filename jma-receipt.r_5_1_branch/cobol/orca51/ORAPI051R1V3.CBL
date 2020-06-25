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
       PROGRAM-ID.         ORAPI051R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 中途データ一括登録  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/12/28    NACL−藤原　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "L01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "L01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
           03  IDAA                    PIC 9(04).
           03  IDBB                    PIC 9(04).
           03  IDCC                    PIC 9(04).
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
               05  WRK-NUM             PIC 9(01).
           03  WRK-NUM-2-X.
               05  WRK-NUM-2           PIC 9(02).
      *
           03  WRK-PARM-G.
               05  WRK-PARM-FLG-OCC    OCCURS  10.
                   07  WRK-PARM-PAGE   PIC 9(02).
                   07  WRK-PARM-LINE   PIC 9(02).
                   07  WRK-PARM-INDEX  PIC 9(02).
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
           03  WRK-CONS-PROCESSING-MAX         PIC 9(02) VALUE 1.
      *
      *    パラメータ最大個数
           03  WRK-CONS-PARA-MAX               PIC 9(05) VALUE 8.
      *    最大明細数
           03  WRK-CONS-LINE-MAX               PIC 9(05) VALUE 100.
      *    １画面あたりの最大明細数
           03  WRK-CONS-SCR-MAX                PIC 9(05) VALUE 10.
      *    システム管理ＤＢ（３００３）のキー値
           03  WRK-CONS-3003-KBNCD             PIC X(08) VALUE "051".
      *    印刷管理ＤＢのキー値
           03  WRK-CONS-PRTKANRI-TBL-KEY       PIC X(08) VALUE "DAILY".
           03  WRK-CONS-PRTKANRI-TBL-GROUP     PIC X(14) VALUE SPACE.
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID            PIC X(08) VALUE "DAILY".
           03  WRK-CONS-JOB-JOBID              PIC 9(07) VALUE ZERO.
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
           COPY    "CPTEMPMEDICALADDV3REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPTEMPMEDICALADDV3RES.INC".
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
           DISPLAY   "* tempmedicaladd start *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           INITIALIZE                      SPA-L01FREE
                                           SPA-L01KYOUTU
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
               MOVE   404                  TO  APILST09-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*************************"
           DISPLAY   "* tempmedicaladd end  *"
           DISPLAY   "*************************"
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
           MOVE   "temp medical add"   TO  LSTRES-RESKEY
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
           IF      APILST09-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-LSTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_temp_medical_addv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST09-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "temp_medical_addv3req"
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
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    "02"                TO  LSTRES-RESPONSE-NUMBER
               MOVE    ZERO                TO  LSTRES-API-RESULT
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  LSTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               MOVE    "01"                TO  LSTRES-RESPONSE-NUMBER
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  LSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  LSTRES-API-RESULT-MSG
      *
               INITIALIZE                      LSTRES-PROCESSING-G
           END-IF
      *
           MOVE   LSTREQ-KARTE-UID     TO  LSTRES-KARTE-UID
           IF      FLG-SYORI           =   1
               IF      WRK-ERRCD           =   SPACE
                   MOVE   LSTREQ-ORCA-UID      TO  LSTRES-ORCA-UID
               END-IF
               MOVE    SPA-L01-MONBLOB-PRINT-ID
                                               TO  LSTRES-PRINT-ID (1)
          ELSE
               MOVE   LSTREQ-ORCA-UID  TO  LSTRES-ORCA-UID
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
           IF      APILST09-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_temp_medical_addv3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  LSTRES-MODE
           MOVE    "temp_medical_addv3res"
                                       TO  LSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-LSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    LSTRES-OBJECT       TO  APILST09-BODY 
               MOVE    "application/xml"   TO  APILST09-CONTENT-TYPE
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
                                       =   "01"
               CONTINUE
           ELSE
               IF     LSTREQ-ORCA-UID      =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    パラメタチェック
           IF      LSTREQ-PROCESSING-G =   SPACE
               MOVE    "E17"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
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
      *
                  IF      LSTREQ-PROGRAM-NAME (IDX)
                                           =   "ORCBNYUALL"
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
                       OR    ( SNUM-SEISUU     >   1     )
                           MOVE    "E22"       TO  WRK-PROCESSING-ERRCD
                       ELSE
                           EVALUATE    SNUM-OUTNUM
                               WHEN    1   THRU    8
                                   CONTINUE
                               WHEN    OTHER
                                   MOVE    "E22"   TO
                                                   WRK-PROCESSING-ERRCD
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
                   IF      WRK-PROCESSING-ERRCD    =   SPACE
                       IF      LSTREQ-PARM-VALUE (IDX IDY)
                                               =   SPACE
      *                    必須以外もあるのでエラーなし
                           CONTINUE
                       ELSE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI 
                           MOVE    LSTREQ-PARM-VALUE (IDX IDY)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                               CONTINUE
                           ELSE
                               MOVE    "E26"       TO
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
                   PERFORM 20012-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   重複チェック
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
      *        番号の重複チェック
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-PARA-MAX
                       OR      LSTREQ-PARAMETER-INFO (IDX IDY)
                                   =       SPACE
                       OR      WRK-PROCESSING-ERRCD
                               NOT =   SPACE
                   COMPUTE IDXX =   IDY +   1
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
                   PERFORM 20012-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
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
      *    システム管理の登録内容とのチェック
           INITIALIZE                  WRK-PARM-G
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               MOVE    SPACE           TO  WRK-PROCESSING-ERRCD
               MOVE    ZERO            TO  FLG-CHK
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       WRK-CONS-LINE-MAX
                       OR      FLG-CHK =       1
                       OR      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
      *            プログラム名のチェック
                   IF     LSTREQ-PROGRAM-NAME (IDX)
                                       =   SPA-GMN-L01-PRGNAME (IDXX)
                       MOVE    1               TO  FLG-CHK
      *
                       COMPUTE IDZ     = ( IDXX
                                       +   WRK-CONS-SCR-MAX   -   1 )
                                       /   WRK-CONS-SCR-MAX
                       COMPUTE IDZZ    =   IDXX
                                       - ( ( IDZ -   1 )
                                             *   WRK-CONS-SCR-MAX )
                       MOVE    IDZ           TO  WRK-PARM-PAGE (IDX)
                       MOVE    IDZZ          TO  WRK-PARM-LINE (IDX)
                       MOVE    IDXX          TO  WRK-PARM-INDEX(IDX)
      *
      *                パラメタ種別のチェック
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       WRK-CONS-PARA-MAX
                               OR      LSTREQ-PARM-NO (IDX IDY)
                                           =   SPACE
                               OR      WRK-PROCESSING-ERRCD
                                       NOT =   SPACE
                           MOVE    LSTREQ-PARM-NO (IDX IDY)
                                                   TO  WRK-NUM-X
                           MOVE    WRK-NUM         TO  IDYY
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
                       MOVE    LSTREQ-PROGRAM-NAME (IDX)
                                           TO  LSTRES-PROGRAM-NAME (IDX)
                       MOVE    SPA-GMN-L01-HCNAME  (IDXX)
                                           TO  LSTRES-PROGRAM-LABEL(IDX)
                   END-IF
               END-PERFORM
      *
               IF      FLG-CHK         =   ZERO
                   MOVE    "E18"           TO  WRK-PROCESSING-ERRCD
               END-IF 
      *
               IF      WRK-PROCESSING-ERRCD
                                   NOT =   SPACE
                   PERFORM 20012-PROCESSING-ERR-SEC
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    パラメタ値の入力チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-PROCESSING-MAX
                   OR      LSTREQ-PROCESSING-INFO (IDX)
                               =       SPACE
               IF      WRK-PARM-PAGE (IDX)
                                       =   SPA-GMN-L01-PAGE
                   CONTINUE
               ELSE
      *            改頁処理
                   COMPUTE IDY =   WRK-PARM-PAGE (IDX) -   1
                   MOVE    IDY             TO  SPA-GMN-L01-PAGE
      *
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                   INITIALIZE                  SPA-L01KYOUTU
      *
                   MOVE    "PUTG"          TO  MCP-STATUS
                   MOVE    "CLICKED"       TO  MCP-EVENT
                   MOVE    "B07"           TO  MCP-WIDGET
      *
                   PERFORM 800-ORCGL01-CALL-SEC
                   IF      SPA-ERRCD   NOT =   SPACE
                      MOVE    "E60"            TO  WRK-ERRCD
                      GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               END-IF
      *
               MOVE    WRK-PARM-INDEX (IDX)
                                       TO  IDY
               MOVE    "T"             TO  SPA-GMN-L01-CHK-VALUE (IDY)
               MOVE    WRK-PARM-LINE (IDX)
                                       TO  IDY
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                   EVALUATE    LSTREQ-PARM-CLASS (IDX IDXX)
                       WHEN    "YMD"
                           MOVE    LSTREQ-PARM-VALUE (IDX IDXX)
                                                  TO  WRK-HEN-DATE
                           PERFORM 802-DAYHEN02-SEC
                           MOVE    WRK-SYMD       TO  L01-PARA(IDY IDXX)
                       WHEN    "YM"
                           MOVE    LSTREQ-PARM-VALUE (IDX IDXX)
                                                  TO  WRK-HEN-DATE
                           PERFORM 802-DAYHEN02-SEC
                           MOVE    WRK-SYMD (1:6) TO  L01-PARA(IDY IDXX)
                       WHEN    OTHER
                       MOVE    LSTREQ-PARM-VALUE (IDX IDXX)
                                               TO  L01-PARA (IDY IDXX)
                   END-EVALUATE
               END-PERFORM
      *        入力チェック
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
               INITIALIZE                  SPA-L01KYOUTU
      *
               MOVE    SPACE           TO  MCP-STATUS
               MOVE    "ACTIVATE"      TO  MCP-EVENT
               MOVE    IDY             TO  WRK-NUM-2
               STRING  "PARA"              DELIMITED  BY SIZE
                       WRK-NUM-2-X         DELIMITED  BY SIZE
                       "1"                 DELIMITED  BY SIZE
                                           INTO  MCP-WIDGET
               END-STRING
      *
               PERFORM 800-ORCGL01-CALL-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   EVALUATE    SPA-ERRCD
                       WHEN    "0002"
                       WHEN    "0004"
                       WHEN    "0103"
                           MOVE    "E61"           TO
                                                   WRK-PROCESSING-ERRCD
                           PERFORM 20012-PROCESSING-ERR-SEC
                       WHEN    OTHER
                           MOVE    "E60"           TO  WRK-ERRCD
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    エラーがあれば処理終了
           IF      WRK-ERR-PROCESSING-MAX
                                       >   ZERO
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    更新前チェック
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-L01KYOUTU
      *
           MOVE    LSTREQ-KARTE-UID
                                   TO  SPA-L01-JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID
                                   TO  SPA-L01-JOB-ORCA-UID
      *
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGL01-CALL-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "0002"
                   WHEN    "0004"
                   WHEN    "0103"
                       MOVE    "E61"           TO  WRK-PROCESSING-ERRCD
                       PERFORM 20012-PROCESSING-ERR-SEC
                   WHEN    "7777"
                       MOVE    "E70"               TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                       IF      LSTREQ-REQUEST-NUMBER
                                                   =   "02"
      *                    ジョブ管理チェック処理
                           MOVE    SPACE           TO  SYS-3003-REC 
                           INITIALIZE                  SYS-3003-REC
                           MOVE    SPA-HOSPNUM     TO  SYS-3003-HOSPNUM
                           MOVE    "3003"          TO  SYS-3003-KANRICD
                           MOVE    WRK-CONS-3003-KBNCD
                                                   TO  SYS-3003-KBNCD
                           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                           MOVE    SYS-3003-STYUKYMD
                                                   TO  SYS-3003-EDYUKYMD 
                           MOVE    SYS-3003-REC    TO  MCPDATA-REC
                           PERFORM 900-SYSKANRI-READ-SEC
                           IF      FLG-SYSKANRI    =   ZERO
                               MOVE    MCPDATA-REC TO  SYS-3003-REC 
                               PERFORM VARYING IDX FROM    1   BY  1
                                       UNTIL   IDX >   SYS-3003-JOBID
                                   MOVE    "CH2"       TO
                                                       SJOBKANRI-MODE
                                   INITIALIZE          JOBKANRI-REC
                                   MOVE    IDX         TO  JOB-JOBID
                                   MOVE    WRK-CONS-JOB-SHELLID
                                                       TO  JOB-SHELLID
                                   PERFORM 900-CALL-ORCSJOB-SEC
                                   IF      SJOBKANRI-RETURN
                                                       =   ZERO
                                       IF    ( LSTREQ-KARTE-UID
                                               NOT =   JOB-KARTE-UID )
                                       OR    ( LSTREQ-ORCA-UID
                                               NOT =   JOB-ORCA-UID  )
                                           MOVE    "E41"   TO  WRK-ERRCD
                                           GO  TO  2001-INPUT-CHK-EXT
                                       END-IF
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-IF
                   WHEN    OTHER
                       MOVE    "E60"           TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理別エラー処理
      *****************************************************************
       20012-PROCESSING-ERR-SEC     SECTION.
      *
           ADD     1               TO  WRK-ERR-PROCESSING-MAX
      *
           MOVE    LSTREQ-PROGRAM-NAME (IDX)
                                   TO  LSTRES-ERR-PROGRAM-NAME
                                               (WRK-ERR-PROCESSING-MAX)
           MOVE    WRK-PROCESSING-ERRCD
                                   TO  LSTRES-ERROR-CODE
                                               (WRK-ERR-PROCESSING-MAX)
           IF      SPA-ERRCD       =   SPACE
               EVALUATE    WRK-PROCESSING-ERRCD
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
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
      *****        MOVE    "E63"           TO  WRK-ERRCD
                   MOVE    WRK-PROCESSING-ERRCD
                                           TO  WRK-ERRCD
                   MOVE    LSTRES-ERROR-MSG (WRK-ERR-PROCESSING-MAX)
                                           TO  WRK-ERRMSG
               END-IF
           ELSE
               EVALUATE    WRK-PROCESSING-ERRCD
                   WHEN    "E61"
                       EVALUATE    SPA-ERRCD
                           WHEN    "0002"
                           WHEN    "0103"
      *                        IDBB PARA INDEX
      *                        IDAA PAGE LINE
      *                        IDCC SPA LINE
                               COMPUTE SPA-GMN-L01-CUR
                                            =   SPA-GMN-L01-CUR -   1
                               DIVIDE  9   INTO        SPA-GMN-L01-CUR
                                           GIVING      IDAA
                                           REMAINDER   IDBB
                               END-DIVIDE
                               COMPUTE IDCC    = ( SPA-GMN-L01-PAGE
                                                           -   1 )
                                               *   WRK-CONS-SCR-MAX
                                               +   IDAA    +   1
                           WHEN    "0004"
                               MOVE    SPA-L01-PARAHYOJI-1
                                                   TO  IDCC
                               MOVE    SPA-L01-PARAHYOJI-2
                                                   TO  IDBB
                       END-EVALUATE
       *
                       MOVE    SPA-GMN-L01-HCNAME  (IDCC)
                                           TO  LSTRES-ERR-PROGRAM-LABEL
                                               (WRK-ERR-PROCESSING-MAX)
                       MOVE    IDBB        TO  WRK-ERR-PARAMETER-IDX
               END-EVALUATE
      *
               EVALUATE    SPA-ERRCD
                   WHEN    "0002"
                   WHEN    "0004"
                   WHEN    "0103"
                       MOVE    SPACE           TO  WRK-ERR-1
                                                   WRK-ERR-2
                       UNSTRING    SPA-ERRMSG  DELIMITED  BY  "【"
                                               INTO    WRK-ERR-1
                                                       WRK-ERR-2
                       END-UNSTRING
                       STRING  WRK-ERR-1       DELIMITED  BY  SPACE
                               "【"            DELIMITED  BY  SIZE
                               SPA-GMN-L01-PARAHYOJI (IDCC IDBB)
                                               DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                                               INTO    LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
                       END-STRING
                   WHEN    OTHER
                       MOVE    SPA-ERRMSG      TO  LSTRES-ERROR-MSG
                                               (WRK-ERR-PROCESSING-MAX)
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
      *****        MOVE    "E63"           TO  WRK-ERRCD
                   MOVE    WRK-PROCESSING-ERRCD
                                           TO  WRK-ERRCD
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
       20012-PROCESSING-ERR-EXT.
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
           MOVE    LSTREQ-KARTE-UID
                                   TO  SPA-L01-JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID
                                   TO  SPA-L01-JOB-ORCA-UID
           MOVE    "temp_medical_add"
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
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
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
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
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
                           OR    ( JOB-YOBI    =
                                   "処理対象のデータがありませんでした")
                               CONTINUE
                           ELSE
                               MOVE    "E30"       TO  WRK-ERRCD
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                       JOB-SHELLMSG
                                                   DELIMITED  BY  SPACE
                                       "】"        DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                               END-STRING
                               GO  TO  2003-SYORI-02-EXT
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "E40"           TO  WRK-ERRCD
                       GO  TO  2003-SYORI-02-EXT
                   END-IF
      *
                   IF      IDX             =   2
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
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
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
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
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
      *    レセ電データ作成処理
      *****************************************************************
       800-ORCGL01-CALL-SEC                SECTION.
      *
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
