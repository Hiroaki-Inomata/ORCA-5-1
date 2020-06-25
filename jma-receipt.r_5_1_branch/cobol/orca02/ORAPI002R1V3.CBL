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
       PROGRAM-ID.         ORAPI002R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : ジョブ一覧返却(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  19/06/20    NACL−藤原　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ジョブ一覧共通パラメタ
           COPY    "XB01COMMON-SPA".
      *
           COPY    "XB01SCR-SPA".
      *
           COPY    "CMJOBNAME.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
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
           03  WRK-WARNING             PIC X(03).
           03  WRK-WARNING-MSG         PIC X(100).
      *
           03  WRK-SELNUM              PIC 9(04).
           03 WRK-JOBMANAGE-STARTDATE  PIC X(08).
      *
           03  WRK-JOB-STARTYMD        PIC X(08).
           03  WRK-JOB-STARTTIME.
                05  WRK-JOB-START-HH   PIC 9(02).
                05  WRK-JOB-START-MM   PIC 9(02).
                05  FILLER             PIC X(04).
      * 
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値 
           03  WRK-CONS-JOB-SHELLID.
               05  FILLER          PIC X(05)   VALUE   "HAORI".
               05  WRK-CONS-JOB-SHELLID-LD
                                   PIC X(02).
               05  WRK-CONS-JOB-SHELLID-SEQ
                                   PIC 9(01).
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPJOBMANAGEV3REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPJOBMANAGEV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
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
           COPY    "MCPDATA.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
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
           COPY    "ORCA02SCRAREA.INC".
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
           DISPLAY   "*    jobmanage start     *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
                                           SPA-XB01
                                           SPA-XB01KYOUTU
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
           DISPLAY   "*************************"
           DISPLAY   "*     jobmanage end     *"
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
           MOVE   "job manage"         TO  LSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
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
           MOVE    ZERO                TO  LSTRES-API-RESULT
           INITIALIZE                      WRK-ERRCD
                                           WRK-ERRMSG
                                           WRK-WARNING
                                           WRK-WARNING-MSG
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE   LSTREQ-REQUEST-NUMBER
               WHEN    "00"
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
      *
      *??
           DISPLAY "2001=" WRK-ERRCD "#" SPA-ERRCD "#"
      *??
           IF      WRK-ERRCD           =   SPACE
               CONTINUE
           ELSE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE   LSTREQ-REQUEST-NUMBER
               WHEN    "00"
      *            情報取得
                   PERFORM 2002-SYORI-00-SEC
               WHEN    "01"
      *            情報確認
                   PERFORM 2002-SYORI-01-SEC
               WHEN    "02"
      *            情報削除
                   PERFORM 2002-SYORI-02-SEC
           END-EVALUATE
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
           INITIALIZE                      XML-LSTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_job_managev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "job_managev3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-LSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "01"        TO  LSTRES-RESPONS-NUMBER
                   WHEN    OTHER
                       MOVE    "02"        TO  LSTRES-RESPONS-NUMBER
               END-EVALUATE
       *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理確認終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理実施終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    9
                       MOVE    "情報取得終了"
                                           TO  LSTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "00"        TO  LSTRES-RESPONS-NUMBER
                   WHEN    1
                       MOVE    "01"        TO  LSTRES-RESPONS-NUMBER
                   WHEN    2
                       MOVE    "02"        TO  LSTRES-RESPONS-NUMBER
               END-EVALUATE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  LSTRES-API-RESULT
               MOVE    WRK-ERRMSG      TO  LSTRES-API-RESULT-MSG
           END-IF
      *
      *    警告メッセージ
           IF      WRK-WARNING     NOT =   SPACE
               MOVE    WRK-WARNING         TO  LSTRES-API-WARNING
               MOVE    WRK-WARNING-MSG     TO  LSTRES-API-WARNING-MSG
           END-IF
      *
           MOVE   LSTREQ-KARTE-UID     TO  LSTRES-KARTE-UID
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI           =   1
                   IF    ( WRK-ERRCD           =   SPACE )
                   AND   ( SPA-ERRCD           =   SPACE )
                       MOVE   LSTREQ-ORCA-UID  TO  LSTRES-ORCA-UID
                   END-IF
               ELSE
                   MOVE   LSTREQ-ORCA-UID  TO  LSTRES-ORCA-UID
               END-IF
           END-IF
           MOVE   LSTREQ-REQUEST-NUMBER
                                       TO  LSTRES-REQUEST-NUMBER
           MOVE   LSTREQ-JOBMANAGE-SELECT
                                       TO  LSTRES-JOBMANAGE-SELECT
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               MOVE   LSTREQ-JOBMANAGE-STARTTIME
                                       TO  LSTRES-JOBMANAGE-STARTTIME
               MOVE   LSTREQ-JOBMANAGE-STARTDATE
                                       TO  LSTRES-JOBMANAGE-STARTDATE
           END-IF
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_job_managev3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  LSTRES-MODE
           MOVE    "job_managev3res"
                                       TO  LSTRES-RECORDNAME
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
           .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    カルテUID
           IF     LSTREQ-KARTE-UID     =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    オルカUID
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "01"    OR  "00"
               CONTINUE
           ELSE
               IF     LSTREQ-ORCA-UID      =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    処理選択キーチェック
           EVALUATE    LSTREQ-JOBMANAGE-SELECT
               WHEN    "ORCBSD1"
                   MOVE    "41"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    1               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "RECEPT1"
                   MOVE    "42"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    1               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "RECEPT2"
                   MOVE    "42"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    2               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "DAILY"
                   MOVE    "51"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    1               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "MONTHLY"
                   MOVE    "52"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    1               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "STOCK"
                   MOVE    "52"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    2               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    "SOKATU"
                   MOVE    "44"            TO  WRK-CONS-JOB-SHELLID-LD
                   MOVE    1               TO  WRK-CONS-JOB-SHELLID-SEQ
               WHEN    OTHER
                   MOVE    "E15"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-CHK
           IF      LSTREQ-JOBMANAGE-SELECT
                                       =   "STOCK"
               MOVE    "在庫管理"      TO  LSTRES-JOBMANAGE-SELECT-NAME
               MOVE   1                TO  FLG-CHK
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   MNAME-MAX
                       OR      FLG-CHK     =   1
                   IF      LSTREQ-JOBMANAGE-SELECT
                                               =   MNAME-SHELLID (IDX)
                       MOVE     MNAME-JOBNAME (IDX)
                                           TO
                                           LSTRES-JOBMANAGE-SELECT-NAME
                       MOVE   1            TO  FLG-CHK
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-CHK             =   ZERO
               MOVE    "E15"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    処理日付
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "00"
               CONTINUE
           ELSE
               IF     LSTREQ-JOBMANAGE-STARTDATE
                                           =   SPACE
                   MOVE    "E24"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               ELSE
                   MOVE    LSTREQ-JOBMANAGE-STARTDATE
                                               TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-SYMD            TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                                   ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    WRK-SYMD            TO
                                               WRK-JOBMANAGE-STARTDATE
                   ELSE
                       MOVE    "E25"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    処理時間
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "00"
               CONTINUE
           ELSE
               IF     LSTREQ-JOBMANAGE-STARTTIME
                                           =   SPACE
                   MOVE    "E16"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               ELSE
                   IF   ( LSTREQ-JOBMANAGE-STARTTIME (3:1)
                                           =   ":" )
                   AND  ( LSTREQ-JOBMANAGE-STARTTIME (6:1)
                                           =   ":" )
                   AND  ( LSTREQ-JOBMANAGE-STARTTIME (1:2)
                                           NUMERIC )
                   AND  ( LSTREQ-JOBMANAGE-STARTTIME (4:2)
                                           NUMERIC )
                   AND  ( LSTREQ-JOBMANAGE-STARTTIME (7:2)
                                           NUMERIC )
                       CONTINUE
                   ELSE 
                       MOVE    "E17"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                      SPA-XB01KYOUTU
      *
           MOVE     LSTREQ-JOBMANAGE-SELECT
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGXB01-CALL-SEC
      *
      *    対象がない場合（指定された条件の処理が存在しません）
           IF      XB01-LBLMSG01   NOT =   SPACE
               MOVE    XB01-LBLMSG01       TO  SPA-ERRMSG
               MOVE    "E18"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "00"
               CONTINUE
           ELSE
               MOVE    ZERO            TO  WRK-SELNUM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-JOBLIST-MAX
                   IF    ( WRK-JOBMANAGE-STARTDATE
                                           =   SPA-NAI-TSTYMD (IDX) )
                   AND   ( LSTREQ-JOBMANAGE-STARTTIME
                                           =   SPA-GMN-TSTHMS (IDX) )
                       MOVE    SPA-GMN-TNO (IDX)
                                               TO  WRK-SELNUM
                       MOVE    SPA-GMN-JOBLIST-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
      *
               IF  WRK-SELNUM          =   ZERO
                   MOVE    "E19"           TO  WRK-ERRCD
               ELSE
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                   INITIALIZE                  SPA-XB01KYOUTU
      *
                   MOVE    WRK-SELNUM      TO  XB01-SELNUM
      *
                   MOVE    SPACE           TO  MCP-STATUS
                   MOVE    "ACTIVATE"      TO  MCP-EVENT
                   MOVE    "SELNUM"        TO  MCP-WIDGET
      *
                   PERFORM 800-ORCGXB01-CALL-SEC
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    SPA-ERRCD
                           WHEN    "0001"
                               MOVE    "E19"           TO  WRK-ERRCD
                           WHEN    OTHER
                               MOVE    "E70"           TO  WRK-ERRCD
                               MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報取得処理
      *****************************************************************
       2002-SYORI-00-SEC           SECTION.
      *
      *    処理一覧返却処理
           PERFORM 20021-JOBMANAGE-INFO-SEC
           .
      *
       2002-SYORI-00-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    情報確認処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      LSTREQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID
      *            ５分以上のチェック
                   MOVE    JOB-STARTTIME   TO  WRK-JOB-STARTTIME
                   MOVE    JOB-STARTYMD    TO  WRK-JOB-STARTYMD
                   IF      WRK-JOB-START-MM    <   55
                       COMPUTE WRK-JOB-START-MM    =   WRK-JOB-START-MM
                                                   +   5
                   ELSE
                       COMPUTE WRK-JOB-START-MM    =   WRK-JOB-START-MM
                                                   -   55
                       IF      WRK-JOB-START-HH    <   23
                           COMPUTE WRK-JOB-START-HH
                                                   =   WRK-JOB-START-HH
                                                   +   1
                       ELSE
                           MOVE    0               TO  WRK-JOB-START-HH
                           INITIALIZE                  STS-AREA-DAY
                           INITIALIZE                  LNK-DAY6-AREA
                           MOVE    "61"            TO  LNK-DAY6-IRAI
                           MOVE    JOB-STARTYMD    TO  LNK-DAY6-KIJUN 
                           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                           MOVE    1               TO  LNK-DAY6-ZOGEN
                           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                           MOVE    LNK-DAY6-KEISAN TO  WRK-JOB-STARTYMD
                       END-IF
                   END-IF
      *
                   IF      SMCNDATE-YMD    >   WRK-JOB-STARTYMD
                       CONTINUE
                   ELSE
                       IF      SMCNDATE-YMD    =   WRK-JOB-STARTYMD
                           IF      SMCNDATE-HMS    <=  WRK-JOB-STARTTIME
                               MOVE    "E71"           TO  WRK-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  LSTREQ-ORCA-UID
      *
      *    ジョブ管理設定処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "E60"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    LSTREQ-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID TO  JOB-ORCA-UID
      *
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "E61"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    処理一覧返却処理
           PERFORM 20021-JOBMANAGE-INFO-SEC
           .
      *
       2002-SYORI-01-EXT.
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
      *    処理一覧返却処理
      *****************************************************************
       20021-JOBMANAGE-INFO-SEC    SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-JOBLIST-MAX
               IF   ( LSTREQ-REQUEST-NUMBER
                                           =   "01"              )
               AND  ( WRK-SELNUM       NOT =   SPA-GMN-TNO (IDX) )
                   CONTINUE
               ELSE
                   IF      LSTREQ-JOBMANAGE-SELECT
                                           =   SPA-GMN-TSHELLID (IDX)
                       MOVE    SPA-GMN-TNO (IDX)   TO
                                        LSTRES-JOBMANAGE-TNUM      (IDX)
                       MOVE    SPA-GMN-TJOBNAME (IDX)
                                                   TO
                                        LSTRES-JOBMANAGE-TSHELLMSG (IDX)
                       MOVE    SPA-GMN-TJOBID  (IDX)
                                                   TO
                                        LSTRES-JOBMANAGE-TJOBID    (IDX)
                       MOVE    SPA-NAI-TSTYMD (IDX)
                                                   TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE        TO
                                        LSTRES-JOBMANAGE-TSTARTYMD (IDX)
                       MOVE    SPA-GMN-TSTHMS (IDX)
                                                   TO
                                        LSTRES-JOBMANAGE-TSTARTTIME(IDX)
                       MOVE    SPA-NAI-TEDYMD (IDX)
                                                   TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE        TO
                                        LSTRES-JOBMANAGE-TENDYMD   (IDX)
                       MOVE    SPA-GMN-TEDHMS (IDX)
                                                   TO
                                        LSTRES-JOBMANAGE-TENDTIME  (IDX)
                       MOVE    SPA-GMN-TSTEPSTHMS (IDX)
                                                   TO
                                    LSTRES-JOBMANAGE-TSTEPSTARTTIME(IDX)
                       MOVE    SPA-GMN-TSTEPEDHMS (IDX)
                                                   TO
                                    LSTRES-JOBMANAGE-TSTEPENDTIME  (IDX)
                       MOVE    SPA-GMN-TERRCD (IDX)
                                                   TO
                                        LSTRES-JOBMANAGE-TERRCD    (IDX)
                       MOVE    SPA-NAI-TYOBI (IDX) TO
                                        LSTRES-JOBMANAGE-TYOBI     (IDX)
                       IF    ( SPA-NAI-TEDYMD (IDX)
                                               =   SPACE )
                       OR    ( SPA-NAI-TMONBATCH (IDX)
                                               =   2     )
                           MOVE    "W01"           TO  WRK-WARNING
                           MOVE    "処理中"        TO  WRK-WARNING-MSG
                           IF       SPA-NAI-TMONBATCH (IDX)
                                                   =   2
      *                        monbatch のとき
                               MOVE    "True"          TO
                                   LSTRES-JOBMANAGE-TSELECT-MODE-O (IDX)
                           ELSE
      *                        jobkanri のとき
                               IF      SPA-NAI-TEDYMD (IDX)
                                                       =   SPACE
                                   IF      SPA-GMN-TSTEPSTHMS (IDX)
                                                          =   SPACE
                                     MOVE    "False"         TO
                                       LSTRES-JOBMANAGE-TSELECT-MODE-O
                                                                   (IDX)
                                   ELSE
                                     MOVE    "True"          TO
                                       LSTRES-JOBMANAGE-TSELECT-MODE-O
                                                                   (IDX)
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       20021-JOBMANAGE-INFO-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除処理
      *****************************************************************
       2002-SYORI-02-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-SYORI-02-EXT
           END-IF
      *
      *    削除処理
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
           INITIALIZE                  SPA-XB01KYOUTU
      *
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B02"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGXB01-CALL-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "1101"
                       MOVE    "E20"           TO  WRK-ERRCD
                   WHEN    "1105"
                       MOVE    "E23"           TO  WRK-ERRCD
                   WHEN    OTHER
                       MOVE    "E70"           TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
               END-EVALUATE
               GO  TO  2002-SYORI-02-EXT
           END-IF
      *
           IF      SPA-XBIDCD      =   "3001"
      *        確認画面のときの削除処理
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
                                           SPA-XBID-AREA
      *
               MOVE    "XBID"          TO  SPA-MOTOPG
               MOVE    "OK"            TO  SPA-XBID-FLG
               MOVE    "3001"          TO  SPA-XBIDCD
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               PERFORM 800-ORCGXB01-CALL-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   EVALUATE    SPA-ERRCD
                       WHEN    "1102"
                           MOVE    "E21"           TO  WRK-ERRCD
                       WHEN    "1103"
                       WHEN    "1104"
                           MOVE    "E22"           TO  WRK-ERRCD
                       WHEN    OTHER
                           MOVE    "E70"           TO  WRK-ERRCD
                           MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "E60"               TO  WRK-ERRCD
           END-IF
           .
       2002-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ一覧処理
      *****************************************************************
       800-ORCGXB01-CALL-SEC       SECTION.
      *
           MOVE    SPA-XB01KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XB01        TO  SPA-FREE
      *
           CALL    "ORCGXB01"          USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XB01
           MOVE    SPA-WORK        TO  SPA-XB01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGXB01-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
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
                   GO  TO  800-JOBKANRI-CHK-EXT
               END-IF
           ELSE
               MOVE    "E40"           TO  WRK-ERRCD
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
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
               WHEN    "E70"
      *            想定外のエラー
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E04"
                   MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "処理選択キーの設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "処理開始時間が未設定です。"
                                               TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "処理開始時間の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E18"
      *            対象なし
                   MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
               WHEN    "E19"
               WHEN    "E20"
                   MOVE
                   "処理開始日付、時間の対象となる処理が存在しません。"
                                               TO  WRK-ERRMSG
               WHEN    "E21"
               WHEN    "E22"
               WHEN    "E23"
      *            削除エラー
                   MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
               WHEN    "E24"
                   MOVE    "処理開始日付が未設定です。"
                                               TO  WRK-ERRMSG
               WHEN    "E25"
                   MOVE    "処理開始日付の設定に誤りがあります。"
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
                  MOVE    "ジョブ管理が削除できませんでした。"
                                               TO  WRK-ERRMSG
               WHEN    "E61"
               WHEN    "E62"
                   MOVE    "ジョブ管理が更新できませんでした。"
                                               TO  WRK-ERRMSG
               WHEN    "E71"
                   MOVE    "他からアクセス中です。"
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
