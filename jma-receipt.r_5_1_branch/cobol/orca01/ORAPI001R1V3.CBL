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
       PROGRAM-ID.         ORAPI001R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  :  再印刷取得  (xml2)
      *  管理者            :
      *  作成日付    作業者      記述
      *  19/08/22    NACL−藤原  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    再印刷処理共通パラメタ
           COPY    "XA01COMMON-SPA".
      *
      *    再印刷画面用ＳＰＡ
           COPY    "XA01SCR-SPA".
      *
      *    再印刷処理共通パラメタ
           COPY    "XA02COMMON-SPA"
                                   REPLACING  //SPA-XAID//
                                   BY         //SPA-XA02-XAID//
                                              //SPA-XA99//
                                   BY         //SPA-XA02-XA99//.
      *
           COPY    "XA02SCR-SPA"   REPLACING  //SPA-//
                                   BY         //SPA-XA02-//.
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
      *
           03  WRK-HENYMDG             PIC X(09).
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
      *
           03  WRK-NUM                 PIC 9(05).
           03  WRK-NUM-X               REDEFINES   WRK-NUM.
               05  FILLER              PIC X(01).
               05  WRK-NUM-4-X         PIC X(04).
      *
      *    パラメタ値情報
           03  WRK-PARM-G.
               05  WRK-GROUP           PIC X(14).
               05  WRK-PTNUM           PIC X(20).
               05  WRK-RENNUM          PIC 9(4).
               05  WRK-SHORI-RENNUM    PIC 9(4).
               05  WRK-STPAGE          PIC 9(5).
               05  WRK-EDPAGE          PIC 9(5).
               05  WRK-CREYMD          PIC X(8).
               05 WRK-SRYYMDH          PIC X(9).
      *
           03  WRK-SELNUM              PIC 9(03).
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
           COPY    "CMJOBNAME.INC".
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "001".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "REPRINT".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "REPRINT".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
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
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPREPRINTV3REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPREPRINTV3RES.INC".
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
           COPY    "ORCA01SCRAREA.INC".
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
           DISPLAY   "*     reprint  start     *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           INITIALIZE                      SPA-XA01
                                           SPA-XA01KYOUTU
           INITIALIZE                      SPA-XA02-XA02-AREA
                                           SPA-XA02KYOUTU
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
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**************************"
           DISPLAY   "*     reprint    end     *"
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
           MOVE   "reprint"        TO  LSTRES-RESKEY
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
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-LSTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_reprintv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "reprintv3req"      TO  APIREQ-RECORDNAME
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
                   CONTINUE
               ELSE
                   INITIALIZE              LSTRES-REPRINT-INFORMATION
               END-IF
           END-IF
      *
           MOVE   LSTREQ-REPRINT-MODE  TO  LSTRES-REPRINT-MODE
           MOVE   LSTREQ-KARTE-UID     TO  LSTRES-KARTE-UID
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI           =   1
                   MOVE    LSTREQ-GROUP        TO  LSTRES-GROUP
                   MOVE    LSTREQ-RENNUM       TO  LSTRES-RENNUM
                   MOVE    LSTREQ-SHORI-RENNUM TO  LSTRES-SHORI-RENNUM
                   MOVE    LSTREQ-STPAGE       TO  LSTRES-STPAGE
                   MOVE    LSTREQ-EDPAGE       TO  LSTRES-EDPAGE
                   MOVE    LSTREQ-PTNUM        TO  LSTRES-PTNUM
                   MOVE    LSTREQ-UUID         TO  LSTRES-UUID
                   IF      WRK-ERRCD           =   SPACE
                       MOVE   LSTREQ-ORCA-UID      TO  LSTRES-ORCA-UID
                   END-IF
                   EVALUATE    LSTREQ-REPRINT-MODE
                       WHEN    "Daily"
                           MOVE    SPA-XA02-MONBLOB-PRINT-ID (1)
                                               TO  LSTRES-PRINT-ID (1)
                       WHEN    SPACE
                           MOVE    SPA-XA01-MONBLOB-PRINT-ID (1)
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
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_reprintv3res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  LSTRES-MODE
           MOVE    "reprintv3res"      TO  LSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-LSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    LSTRES-OBJECT       TO  APILST-BODY 
               MOVE    "application/xml"   TO  APILST-CONTENT-TYPE
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
           MOVE   LSTREQ-SELECT        TO  LSTRES-SELECT
           MOVE   LSTREQ-CREYMD        TO  LSTRES-CREYMD
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
      *    指示区分、処理選択キー
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    "Daily"
                   IF      LSTREQ-SELECT       =   SPACE   OR  "00"
                       MOVE    "E15"           TO  WRK-ERRCD
                   ELSE
      *                初画面情報
                       MOVE    SPACE           TO  LINKAREA
                       MOVE    "M01"           TO  SPA-MOTOPG
                       INITIALIZE                  SPA-XA02KYOUTU
                       MOVE    "ONLINE"        TO  SPA-PRTKANRI-TBL-KEY
      *
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    SPACE               TO  MCP-EVENT
      *
                       PERFORM 800-ORCGXA01-CALL-SEC
      *
                       MOVE    SPACE           TO  LINKAREA
                                                   SPA-XA02-XA02-MOTOPG
                       INITIALIZE                  SPA-XA02-XAIDCD
                                                   SPA-XA02-XAIDCD2
                                                   SPA-XA02-XAID1-AREA
                       MOVE    LSTREQ-SELECT   TO  XA02-G-VALUE
                       MOVE    "ACTIVATE"      TO  MCP-EVENT
                       MOVE    "GYOMUCD"       TO  MCP-WIDGET
                       MOVE    SPACE           TO  MCP-STATUS
      *
                       PERFORM 800-ORCGXA01-CALL-SEC
      *
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    "E15"           TO  WRK-ERRCD
                       END-IF
      *
                       MOVE     SPA-XA02-GMN-GYOMU-NAME
                                               TO  LSTRES-SELECT-NAME
      *
      *               作成日
                       IF      LSTREQ-CREYMD       =   SPACE
                           MOVE    "E32"               TO  WRK-ERRCD
                           GO  TO  2001-INPUT-CHK-EXT
                       ELSE
                           MOVE    LSTREQ-CREYMD      TO  WRK-HEN-DATE
                           PERFORM 802-DAYHEN02-SEC
      *
                           MOVE    SPACE       TO  LINKAREA
                                                   SPA-XA02-XA02-MOTOPG
                           INITIALIZE                  SPA-XA02-XAIDCD
                                                   SPA-XA02-XAIDCD2
                                                   SPA-XA02-XAID1-AREA
                           MOVE    WRK-SYMD        TO  XA02-CREYMD
                           MOVE    "ACTIVATE"      TO  MCP-EVENT
                           MOVE    "CREYMD"        TO  MCP-WIDGET
                           MOVE    SPACE           TO  MCP-STATUS
      *
                           PERFORM 800-ORCGXA01-CALL-SEC
      *
                           IF      SPA-ERRCD       NOT =   SPACE
                               MOVE    "E32"               TO  WRK-ERRCD
                          END-IF
                       END-IF
                   END-IF
               WHEN    SPACE
                   IF      LSTREQ-SELECT       =   SPACE
                       MOVE    "E15"           TO  WRK-ERRCD
                   ELSE
                     EVALUATE    LSTREQ-SELECT
      *                データチェック
                       WHEN    "ORCBSD1"
      *                レセプト印刷
                       WHEN    "RECEPT2"
      *                日次統計
                       WHEN    "DAILY"
      *                月次統計
                       WHEN    "MONTHLY"
      *                総括・公費請求書
                       WHEN    "SOKATU"
      *                支払証明書
                       WHEN    "ORCBS01"
      *                マスタ更新
                       WHEN    "MSTMNT"
      *                点数マスタ一覧
                       WHEN    "TENSU"
      *                経過措置医薬品置換マスタ一覧
                       WHEN    "TENSU2"
      *                定期請求−請求書印刷
                       WHEN    "TEIKI2"
      *                チェックマスタ一覧
                       WHEN    "CHKMST"
      *                薬剤情報マスタ
                       WHEN     "YKJMST"
                           MOVE    ZERO                TO  FLG-CHK
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   MNAME-MAX
                                   OR      FLG-CHK     =   1
                               IF      LSTREQ-SELECT
                                              =   MNAME-SHELLID (IDX)
                                   MOVE     MNAME-JOBNAME (IDX)
                                       TO  LSTRES-SELECT-NAME
                                   MOVE   1            TO  FLG-CHK
                               END-IF
                           END-PERFORM
                           IF      FLG-CHK             =   ZERO
                               MOVE    "E15"               TO  WRK-ERRCD
                           END-IF
                       WHEN    OTHER
                           MOVE    "E15"           TO  WRK-ERRCD
                     END-EVALUATE
                   END-IF
               WHEN    OTHER
                   MOVE    "E14"               TO  WRK-ERRCD
           END-EVALUATE
      *
           IF     WRK-ERRCD        NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    処理実施のとき再印刷情報チェック
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "01"
               PERFORM 20011-PROCESSING-CHECK-SEC
      *        エラーがあれば処理終了
               IF    ( WRK-ERRCD       NOT =   SPACE )
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    SPACE
                   PERFORM 20012-XA01-CHECK-SEC
               WHEN    "Daily"
                   PERFORM 20013-XA02-CHECK-SEC
           END-EVALUATE
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷情報チェック処理
      *****************************************************************
       20011-PROCESSING-CHECK-SEC      SECTION.
      *
           INITIALIZE                      WRK-PARM-G
      *
           IF      LSTREQ-REPRINT-INFORMATION
                                       =   SPACE
               MOVE    "E17"               TO  WRK-ERRCD
               GO  TO  20011-PROCESSING-CHECK-EXT
           END-IF
      *
      *    未入力チェック 
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    "Daily"
                  IF     LSTREQ-GROUP     =   SPACE
                       MOVE    "E18"           TO  WRK-ERRCD
                   ELSE
                       IF     LSTREQ-UUID     =   SPACE
                           MOVE    "E33"           TO  WRK-ERRCD
                       ELSE
                           IF     LSTREQ-PTNUM     =   SPACE
                               MOVE    "E34"           TO  WRK-ERRCD
                           END-IF
                       END-IF
                   END-IF
                   IF      WRK-ERRCD       =   SPACE
                       IF    ( LSTREQ-STPAGE   NOT =   SPACE )
                       OR    ( LSTREQ-EDPAGE   NOT =   SPACE )
                           IF      LSTREQ-SHORI-RENNUM =   SPACE
                               MOVE    "E19"           TO  WRK-ERRCD
                           ELSE
                               IF      LSTREQ-RENNUM       =   SPACE
                                   MOVE    "E20"           TO  WRK-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               WHEN    SPACE
                   IF     LSTREQ-GROUP     =   SPACE
                       MOVE    "E18"           TO  WRK-ERRCD
                   ELSE
                       IF    ( LSTREQ-STPAGE   NOT =   SPACE )
                       OR    ( LSTREQ-EDPAGE   NOT =   SPACE )
                           IF      LSTREQ-SHORI-RENNUM =   SPACE
                               MOVE    "E19"           TO  WRK-ERRCD
                           ELSE
                               IF      LSTREQ-RENNUM       =   SPACE
                                   MOVE    "E20"           TO  WRK-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  20011-PROCESSING-CHECK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI 
           MOVE    LSTREQ-GROUP    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
           AND   ( KANACHK-RC2     =   ZERO )
           AND   ( KANACHK-SYUBETU =   1    )
           AND   ( KANACHK-MAX     =   14   )
               CONTINUE
           ELSE
               MOVE    "E21"           TO  WRK-ERRCD
               GO  TO  20011-PROCESSING-CHECK-EXT
           END-IF
      *
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    "Daily"
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    LSTREQ-UUID     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-RC2     =   ZERO )
                   AND   ( KANACHK-SYUBETU =   1    )
                       CONTINUE
                   ELSE
                        MOVE    "E35"           TO  WRK-ERRCD
                       GO  TO  20011-PROCESSING-CHECK-EXT
                   END-IF
      *
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    LSTREQ-PTNUM    TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-RC2     =   ZERO )
                   AND   ( KANACHK-SYUBETU =   1    )
                       CONTINUE
                   ELSE
                        MOVE    "E23"           TO  WRK-ERRCD
                       GO  TO  20011-PROCESSING-CHECK-EXT
                   END-IF
           END-EVALUATE 
      *
           IF     LSTREQ-RENNUM    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE   LSTREQ-RENNUM    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC     NOT =   ZERO  )
               OR    ( SNUM-SYOKBN     =   "1"   )
               OR    ( SNUM-MINKBN     =   "1"   )
                   MOVE    "E22"         TO  WRK-ERRCD
                   GO  TO  20011-PROCESSING-CHECK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-RENNUM
               END-IF
           END-IF
      *
           IF     LSTREQ-SHORI-RENNUM
                                   =   SPACE
              CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE   LSTREQ-SHORI-RENNUM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC     NOT =   ZERO  )
               OR    ( SNUM-SYOKBN     =   "1"   )
               OR    ( SNUM-MINKBN     =   "1"   )
                   MOVE    "E24"       TO  WRK-ERRCD
                   GO  TO  20011-PROCESSING-CHECK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-SHORI-RENNUM
               END-IF
           END-IF
      *
           IF     LSTREQ-SHORI-RENNUM
                               NOT =   SPACE
               IF   ( LSTREQ-STPAGE    =   SPACE )
               OR   ( LSTREQ-EDPAGE    =   SPACE )
                   MOVE    "E31"       TO  WRK-ERRCD
                   GO  TO  20011-PROCESSING-CHECK-EXT
               END-IF
           END-IF
      *
           IF     LSTREQ-STPAGE    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE   LSTREQ-STPAGE   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC     NOT =   ZERO  )
               OR    ( SNUM-SYOKBN     =   "1"   )
               OR    ( SNUM-MINKBN     =   "1"   )
                   MOVE    "E25"       TO  WRK-ERRCD
                   GO  TO  20011-PROCESSING-CHECK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-STPAGE
               END-IF
           END-IF
      *
           IF     LSTREQ-EDPAGE    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE   LSTREQ-EDPAGE   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC     NOT =   ZERO  )
               OR    ( SNUM-SYOKBN     =   "1"   )
               OR    ( SNUM-MINKBN     =   "1"   )
                   MOVE    "E26"       TO  WRK-ERRCD
                   GO  TO  20011-PROCESSING-CHECK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-EDPAGE
               END-IF
           END-IF
           .
       20011-PROCESSING-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    月次業務再印刷チェック
      *****************************************************************
       20012-XA01-CHECK-SEC      SECTION.
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                      SPA-XA01KYOUTU
           MOVE    LSTREQ-SELECT       TO  SPA-PRTKANRI-TBL-KEY
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGXA01-CALL-SEC
      *
      *    情報取得のとき再印刷一覧を取得
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "00"
               IF      SPA-NAI-NEXT        =   1
                   MOVE    "True"              TO
                                       LSTRES-REPRINT-INFORMATION-OVER
               END-IF
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                       OR      SPA-GMN-TNO (IDX)
                                   =       ZERO
                   MOVE    SPA-NAI-TTBL-GROUP (IDX)
                                           TO  LSTRES-TGROUP    (IDX)
                   MOVE    SPA-GMN-TRENNUM (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-4-X     TO  LSTRES-TRENNUM   (IDX)
                   MOVE    SPA-GMN-TSHORI-RENNUM  (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-4-X     TO  LSTRES-TSHORI-RENNUM
                                                               (IDX)
                   MOVE    SPA-GMN-TPAGE  (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-X       TO  LSTRES-TPAGE     (IDX)
                   MOVE    SPA-GMN-TPRT-TITLE (IDX)
                                           TO  LSTRES-TTITLE    (IDX)
                   MOVE    SPA-NAI-TSRYYM (IDX)
                                           TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO  LSTRES-TSRYYM    (IDX)
                   MOVE    SPA-NAI-TSKYYMD (IDX)
                                           TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  LSTRES-TSKYYMD   (IDX)
                   MOVE    SPA-NAI-TCREYMD (IDX)
                                           TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  LSTRES-TCREYMD   (IDX)
                   IF      SPA-NAI-TPRTFLG (IDX)
                                           =   SPACE
                       MOVE    "True"          TO  LSTRES-TFLG  (IDX)
                   END-IF
               END-PERFORM
      *        対象帳票なし
               IF      SPA-GMN-MAX             =   ZERO
                   MOVE    "E16"               TO  WRK-ERRCD
               END-IF
               GO  TO  20012-XA01-CHECK-EXT
           END-IF
      *
      *    該当帳票チェック
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-MAX
                   OR      LSTREQ-GROUP
                                   >       SPA-NAI-TTBL-GROUP (IDX)
               IF     LSTREQ-GROUP     =   SPA-NAI-TTBL-GROUP (IDX)
                   MOVE    LSTREQ-GROUP    TO  SPA-GMN-TBL-GROUP
                   IF      WRK-RENNUM      =   ZERO
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   ELSE
                       IF     WRK-RENNUM       =   SPA-GMN-TRENNUM (IDX)
                           MOVE    LSTREQ-RENNUM   TO  SPA-GMN-RENNUM
                           IF      WRK-SHORI-RENNUM
                                                   =   ZERO
                               MOVE    IDX             TO  WRK-SELNUM
                               MOVE    SPA-GMN-MAX     TO  IDX
                           ELSE
                               IF      WRK-SHORI-RENNUM
                                       =   SPA-GMN-TSHORI-RENNUM (IDX)
                                   MOVE    LSTREQ-SHORI-RENNUM TO
                                                   SPA-GMN-SHORI-RENNUM
                                   IF    ( WRK-STPAGE  >
                                                   SPA-GMN-TPAGE (IDX) )
                                   OR    ( WRK-EDPAGE  >
                                                   SPA-GMN-TPAGE (IDX) )
                                       MOVE    "E28"       TO  WRK-ERRCD
                                       GO  TO  20012-XA01-CHECK-EXT
                                   END-IF
                                   IF      WRK-STPAGE  >   WRK-EDPAGE
                                       MOVE    "E27"       TO  WRK-ERRCD
                                       GO  TO  20012-XA01-CHECK-EXT
                                   END-IF
      *
                                   MOVE    IDX         TO  WRK-SELNUM
                                   MOVE    SPA-GMN-MAX TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          =   ZERO
               MOVE    "E29"               TO  WRK-ERRCD
               GO  TO  20012-XA01-CHECK-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-XAIDCD
                                       SPA-XAIDCD2
                                       SPA-XAID1-AREA
           MOVE    WRK-SELNUM      TO  XA01-SELNUM
           MOVE    "ACTIVATE"      TO  MCP-EVENT
           MOVE    "SELNUM"        TO  MCP-WIDGET
           MOVE    SPACE           TO  MCP-STATUS
      *
           PERFORM 800-ORCGXA01-CALL-SEC
      *
      *??
           DISPLAY "SELNUM SPA-ERRCD=" SPA-ERRCD "#"
      *??
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "E60"           TO  WRK-ERRCD
               GO  TO  20012-XA01-CHECK-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-XAIDCD
                                       SPA-XAIDCD2
                                       SPA-XAID1-AREA
           MOVE    WRK-RENNUM      TO  XA01-RENNUM
           MOVE    WRK-SHORI-RENNUM
                                   TO  XA01-SHORI-RENNUM
           MOVE    WRK-STPAGE      TO  XA01-STPAGE
           MOVE    WRK-EDPAGE      TO  XA01-EDPAGE
           MOVE    LSTREQ-KARTE-UID
                                   TO  SPA-XA01-JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID TO  SPA-XA01-JOB-ORCA-UID
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGXA01-CALL-SEC
      *??
           DISPLAY "B12 SPA-ERRCD=" SPA-ERRCD "#"
      *??
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "7777"
                       MOVE    "E70"           TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                   WHEN    OTHER
                       MOVE    "E60"           TO  WRK-ERRCD
                END-EVALUATE
           END-IF
      *
           .
       20012-XA01-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン帳票再印刷処理チェック
      *****************************************************************
       20013-XA02-CHECK-SEC      SECTION.
      *
      *    情報取得のとき再印刷一覧を取得
           IF      LSTREQ-REQUEST-NUMBER
                                       =   "00"
               IF      SPA-XA02-GMN-TBL-NEXT   =   1
                   MOVE    "True"              TO
                                       LSTRES-REPRINT-INFORMATION-OVER
               END-IF
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-XA02-GMN-MAX
                       OR      SPA-XA02-GMN-TNO (IDX)
                                   =       ZERO
                   MOVE    SPA-XA02-NAI-TTBL-GROUP (IDX)
                                           TO  LSTRES-TGROUP    (IDX)
                   MOVE    SPA-XA02-GMN-TRENNUM (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-4-X     TO  LSTRES-TRENNUM   (IDX)
                   MOVE    SPA-XA02-GMN-TSHORI-RENNUM  (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-4-X     TO  LSTRES-TSHORI-RENNUM
                                                                (IDX)
                   MOVE    SPA-XA02-GMN-TPAGE      (IDX)
                                           TO  WRK-NUM
                   MOVE    WRK-NUM-X       TO  LSTRES-TPAGE     (IDX)
                   MOVE    SPA-XA02-GMN-TPRT-TITLE (IDX)
                                           TO  LSTRES-TTITLE    (IDX)
                   MOVE    SPA-XA02-NAI-TTBL-UUID  (IDX)
                                           TO  LSTRES-TUUID     (IDX)
                   MOVE    SPA-XA02-GMN-TLPTNUM    (IDX)
                                           TO  LSTRES-TPTNUM    (IDX)
                   MOVE    SPA-XA02-GMN-TLPTNAME   (IDX)
                                           TO  LSTRES-TNAME     (IDX)
                   IF      SPA-XA02-NAI-TSKYYMD-9 (IDX)
                                       NOT =   SPACE   AND ZERO
                       MOVE    SPA-XA02-NAI-TSKYYMD-9  (IDX)
                                               TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO  LSTRES-TSRYYMD (IDX)
                   END-IF
               END-PERFORM
      *        対象帳票なし
               IF      SPA-XA02-GMN-MAX             =   ZERO
                   MOVE    "E16"               TO  WRK-ERRCD
               END-IF
               GO  TO  20013-XA02-CHECK-EXT
           END-IF
      *
      *    該当帳票チェック
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-XA02-GMN-MAX
                   OR      LSTREQ-GROUP
                                   >       SPA-XA02-NAI-TTBL-GROUP (IDX)
               IF   ( LSTREQ-GROUP =   SPA-XA02-NAI-TTBL-GROUP (IDX) )
               AND  ( LSTREQ-UUID  =   SPA-XA02-NAI-TTBL-UUID  (IDX) )
               AND  ( LSTREQ-PTNUM =   SPA-XA02-GMN-TLPTNUM    (IDX) )
                   MOVE    LSTREQ-GROUP    TO  SPA-XA02-NAI-TBL-GROUP
                   IF      WRK-RENNUM      =   ZERO
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-XA02-GMN-MAX     TO  IDX
                   ELSE
                       IF     WRK-RENNUM       =
                                           SPA-XA02-GMN-TRENNUM (IDX)
                           MOVE    LSTREQ-RENNUM   TO
                                                   SPA-XA02-GMN-RENNUM
                           IF      WRK-SHORI-RENNUM
                                                   =   ZERO
                               MOVE    IDX             TO  WRK-SELNUM
                               MOVE    SPA-XA02-GMN-MAX
                                                       TO  IDX
                           ELSE
                               IF      WRK-SHORI-RENNUM
                                   =   SPA-XA02-GMN-TSHORI-RENNUM (IDX)
                                   MOVE    LSTREQ-SHORI-RENNUM TO
                                               SPA-XA02-GMN-SHORI-RENNUM
                                   IF    ( WRK-STPAGE  >
                                              SPA-XA02-GMN-TPAGE (IDX) )
                                   OR    ( WRK-EDPAGE  >
                                              SPA-XA02-GMN-TPAGE (IDX) )
                                       MOVE    "E28"       TO  WRK-ERRCD
                                       GO  TO  20013-XA02-CHECK-EXT
                                   END-IF
                                   IF      WRK-STPAGE  >   WRK-EDPAGE
                                       MOVE    "E27"       TO  WRK-ERRCD
                                       GO  TO  20013-XA02-CHECK-EXT
                                   END-IF
      *
                                   MOVE    IDX         TO  WRK-SELNUM
                                   MOVE    SPA-XA02-GMN-MAX
                                                       TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *??
           DISPLAY "SELNUM=" WRK-SELNUM "#"
      *??
           IF      WRK-SELNUM          =   ZERO
               MOVE    "E30"               TO  WRK-ERRCD
               GO  TO  20013-XA02-CHECK-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-XA02-XAIDCD
                                       SPA-XA02-XAIDCD2
                                       SPA-XA02-XAID1-AREA
           MOVE    WRK-SELNUM      TO  XA02-SELNUM
           MOVE    "ACTIVATE"      TO  MCP-EVENT
           MOVE    "SELNUM"        TO  MCP-WIDGET
           MOVE    SPACE           TO  MCP-STATUS
      *
           PERFORM 800-ORCGXA01-CALL-SEC
      *
      *??
           DISPLAY "SELNUM SPA-ERRCD=" SPA-ERRCD "#"
      *??
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "E60"           TO  WRK-ERRCD
               GO  TO  20013-XA02-CHECK-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-XA02-XAIDCD
                                       SPA-XA02-XAIDCD2
                                       SPA-XA02-XAID1-AREA
           MOVE    WRK-RENNUM      TO  XA02-RENNUM
           MOVE    WRK-SHORI-RENNUM
                                   TO  XA02-SHORI-RENNUM
           MOVE    WRK-STPAGE      TO  XA02-STPAGE
           MOVE    WRK-EDPAGE      TO  XA02-EDPAGE
           MOVE    LSTREQ-KARTE-UID
                                   TO  SPA-XA02-JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID TO  SPA-XA02-JOB-ORCA-UID
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGXA01-CALL-SEC
      *??
           DISPLAY "B12 SPA-XA02-ERRCD=" SPA-ERRCD "#"
      *??
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "7777"
                       MOVE    "E70"           TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                   WHEN    OTHER
                       MOVE    "E60"           TO  WRK-ERRCD
                END-EVALUATE
           END-IF
      *
           .
       20013-XA02-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実施処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *??     ジョブ管理チェック処理
      *??     PERFORM 800-JOBKANRI-CHK-SEC
      *??     IF      WRK-ERRCD       NOT =   SPACE
      *??         GO  TO  2002-SYORI-01-EXT
      *??     END-IF
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  LSTREQ-ORCA-UID
      *
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    SPACE
                   MOVE    LSTREQ-KARTE-UID
                                           TO  SPA-XA01-JOB-KARTE-UID
                   MOVE    LSTREQ-ORCA-UID TO  SPA-XA01-JOB-ORCA-UID
                   MOVE    "reprint"       TO  SPA-XA01-PUSHBA01-EVENT
      *
                   PERFORM 800-TEMPDIR-SEC
                   IF      WRK-ERRCD   NOT =   SPACE
                       GO  TO  2002-SYORI-01-EXT
                   END-IF
      *
                   MOVE    SPACE           TO  LINKAREA
                   MOVE    "XAID2"         TO  SPA-MOTOPG
                   MOVE    "OK"            TO  SPA-XAID1-FLG
                   MOVE    "1001"          TO  SPA-XAIDCD2
               WHEN    "Daily"
                   MOVE    LSTREQ-KARTE-UID
                                           TO  SPA-XA02-JOB-KARTE-UID
                   MOVE    LSTREQ-ORCA-UID TO  SPA-XA02-JOB-ORCA-UID
                   MOVE    "reprint"       TO  SPA-XA02-PUSHBA01-EVENT
      *
                   PERFORM 800-TEMPDIR-SEC
                   IF      WRK-ERRCD   NOT =   SPACE
                       GO  TO  2002-SYORI-01-EXT
                   END-IF
      *
                   MOVE    SPACE           TO  LINKAREA
                   MOVE    "XAID2"         TO  SPA-MOTOPG
                   MOVE    "OK"            TO  SPA-XA02-XAID1-FLG
                   MOVE    "1001"          TO  SPA-XA02-XAIDCD2
           END-EVALUATE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           PERFORM 800-ORCGXA01-CALL-SEC
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
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
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
                           IF      JOB-ERRCD   =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    "E39"       TO  WRK-ERRCD
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
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
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
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
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
       800-ORCGXA01-CALL-SEC                SECTION.
      *
           EVALUATE    LSTREQ-REPRINT-MODE
               WHEN    "Daily"
                   MOVE    SPA-XA02KYOUTU
                                           TO  SPA-WORK
                   MOVE    SPA-AREA        TO  SPA-COMMON
                   MOVE    SPA-XA02-XA02FREE
                                           TO  SPA-FREE
      *
                   CALL    "ORCGXA02"           USING
                                                MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
      *
                   MOVE    SPA-COMMON      TO  SPA-AREA
                   MOVE    SPA-FREE        TO  SPA-XA02-XA02FREE
                   MOVE    SPA-WORK        TO  SPA-XA02KYOUTU
               WHEN    SPACE
                   MOVE    SPA-XA01KYOUTU  TO  SPA-WORK
                   MOVE    SPA-AREA        TO  SPA-COMMON
                   MOVE    SPA-XA01        TO  SPA-FREE
      *
                   CALL    "ORCGXA01"           USING
                                                MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
      *
                   MOVE    SPA-COMMON      TO  SPA-AREA
                   MOVE    SPA-FREE        TO  SPA-XA01
                   MOVE    SPA-WORK        TO  SPA-XA01KYOUTU 
           END-EVALUATE
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGXA01-CALL-EXT.
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
               WHEN    "E14"
                   MOVE    "指示区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "処理選択キーの設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE
                   "処理選択キーの設定に該当する帳票が存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "対象の帳票の条件が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E18"
                   MOVE    "グループ番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E19"
                   MOVE    "連番が未設定です。"    TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "処理番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                   MOVE    "グループ番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE    "処理番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E23"
                   MOVE    "患者番号の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E24"
                   MOVE    "連番の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E25"
                   MOVE    "開始頁の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E26"
                   MOVE    "終了頁の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E27"
               WHEN    "E28"
                   MOVE    "頁の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E29"
               WHEN    "E30"
                   MOVE    "指定された条件に該当する帳票がありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E31"
                   MOVE    "開始頁または終了頁が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E32"
                   MOVE    "作成日の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E33"
                   MOVE    "ＵＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E34"
                   MOVE    "患者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E35"
                   MOVE    "ＵＵＩＤの設定に誤りがあります。"
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
               WHEN    "E88"
                   EVALUATE    SPA-ERRCD
                       WHEN    "0001"
                           MOVE
                           "指定された条件に該当する帳票がありません。"
                                                       TO  WRK-ERRMSG
                       WHEN    OTHER
                           MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                   END-EVALUATE
      *
               WHEN    "E80"
                   MOVE    "一時ディレクトリが取得できません。"
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
