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
       PROGRAM-ID.         ORAPI042R2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    :  API連携用モジュール
      *  コンポーネント名  : レセプト印刷(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/09/15    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "R01SCR-SPA".
      *
      *    レセプト作成−印刷指示画面ＳＰＡ
           COPY    "R02SCR-SPA".
      *
       01  SPA-SCR-REC.
           COPY    "CPR02SPASCR.INC".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
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
           03  WRK-KENSU               PIC 9(05).
           03  WRK-PAGE                PIC 9(05).
           03  WRK-UPDCNT-X.
               05  WRK-UPDCNT          PIC 9(05).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-X.
               05  WRK-9               PIC 9(05).
      *
           03  WRK-ERR-SUBMISSION-MAX  PIC 9(03).
           03  WRK-ERR-DETAIL-MAX      PIC 9(03).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-SPA-TERMID          PIC X(64).
      *
      *    チェック用
           03  WRK-CHK-TABLE-AREA.
               05  WRK-CHK-R02-TBL.
                   07  WRK-CHK-R02-TBL-OCC     OCCURS  19.
                       09  WRK-CHK-R02-TBL-G   OCCURS  36.
                           11  WRK-CHK-R02-CHK
                                       PIC X(01).
      *
               05  WRK-CHK-R04-TBL.
                   07  WRK-CHK-R04-TBL-G       OCCURS  25.
                       09  WRK-CHK-R04-CHK
                                       PIC X(01).
      * 
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
       01  WRK-CONS-AREA.
      *    １頁の最大行数
           03  WRK-CONS-LINE-MAX   PIC 9(02)   VALUE   36.   
      *    最大頁数
           03  WRK-CONS-PAGE-MAX   PIC 9(02)   VALUE   19.   
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "RECEPT2".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPT-PRINTV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPT-PRINTV3RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA2.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *****************************************************************
      *    連絡領域
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
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***********************"
           DISPLAY   "* receipt_Print start *"
           DISPLAY   "***********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *****INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
                                           SPA-R01KYOUTU
                                           SPA-R02
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
           DISPLAY   "***********************"
           DISPLAY   "* receipt_Print end   *"
           DISPLAY   "***********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      ***  INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-RECEIPT-PRINTRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  RECEPRINTS-RESKEY
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
                                       RECEPRINTS-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO
                                       RECEPRINTS-INFORMATION-TIME
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
           MOVE    ZERO                TO  RECEPRINTS-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
                                           WRK-WARNING
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    RECEPRINTQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理実施
                   MOVE    1                   TO  FLG-SYORI
               WHEN    "02"
      *            処理確認
                   MOVE    2                   TO  FLG-SYORI
      ****     WHEN    "03"
      ****        キャンセル
      ****         MOVE    3                   TO  FLG-SYORI
      *
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
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    RECEPRINTQ-REQUEST-NUMBER
           WHEN    "01"
      *        処理実施処理
               PERFORM 2002-RECEIPT-PRINT-SEC
           WHEN    "02"
      *        処理確認処理
               PERFORM 2003-RECEIPT-CHECK-SEC
           WHEN    "03"
      *        キャンセル処理
               PERFORM 2004-RECEIPT-CANCEL-SEC
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
           INITIALIZE                      XML-RECEIPT-PRINTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_receipt_printv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "receipt_printv3req" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-RECEIPT-PRINTREQ
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
           IF    ( WRK-ERRCD           =   SPACE )
      *        正常終了
               MOVE    "02"            TO  RECEPRINTS-RESPONS-NUMBER
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  RECEPRINTS-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  RECEPRINTS-API-RESULT-MSG
                   WHEN    3
                       MOVE    "処理キャンセル終了"
                                           TO  RECEPRINTS-API-RESULT-MSG
               END-EVALUATE
           ELSE
               MOVE    "01"            TO  RECEPRINTS-RESPONS-NUMBER
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  RECEPRINTS-API-RESULT
               MOVE    WRK-ERRMSG      TO  RECEPRINTS-API-RESULT-MSG
           END-IF
      *    警告メッセージ
           MOVE    WRK-WARNING         TO  RECEPRINTS-API-WARNING
           MOVE    WRK-WARNING-MSG     TO  RECEPRINTS-API-WARNING-MSG
      *
           MOVE    RECEPRINTQ-KARTE-UID TO  RECEPRINTS-KARTE-UID
           MOVE    RECEPRINTQ-ORCA-UID  TO  RECEPRINTS-ORCA-UID
           MOVE    RECEPRINTQ-REQUEST-NUMBER
                                       TO  RECEPRINTS-REQUEST-NUMBER
           MOVE    RECEPRINTQ-PERFORM-MONTH
                                       TO  RECEPRINTS-PERFORM-MONTH
      *
           IF      RECEPRINTQ-REQUEST-NUMBER
                                       =   "01"
               MOVE    SPA-R01-MONBLOB-ID   TO  RECEPRINTS-PRINT-ID (1)
           END-IF
      *
           IF      APILST03-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_receipt_printv3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  RECEPRINTS-MODE
           MOVE    "receipt_printv3res" TO  RECEPRINTS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-RECEIPT-PRINTRES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  RECEPRINTS-OBJECT     TO  APILST03-BODY
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
      *    カルテUID
           IF      RECEPRINTQ-KARTE-UID
                                       =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   オルカUID
           IF      RECEPRINTQ-ORCA-UID =   SPACE
               MOVE    "E05"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ERR-SUBMISSION-MAX
                                           WRK-ERR-DETAIL-MAX
      *
      *    処理区分
           EVALUATE    RECEPRINTQ-RECEIPT-MODE
               WHEN    "All"
                   MOVE    "1"         TO  SPA-R01-FILEKBN
               WHEN    OTHER
                   MOVE    "2"         TO  SPA-R01-FILEKBN
           END-EVALUATE
      *
      *    入外区分
           EVALUATE    RECEPRINTQ-INOUT
               WHEN    "I"
                   MOVE    "1"             TO  SPA-R01-NYUGAIKBN
               WHEN    "O"
                   MOVE    "2"             TO  SPA-R01-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "E12"           TO  WRK-ERRCD
           END-EVALUATE
      *
      *    提出先
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    RECEPRINTQ-SUBMISSION-MODE
                   WHEN    "01"
                       MOVE    "0"         TO  SPA-R01-RECEKBN
                   WHEN    "02"
                       MOVE    "1"         TO  SPA-R01-RECEKBN
                   WHEN    "03"
                       MOVE    "2"         TO  SPA-R01-RECEKBN
                   WHEN    "04"
                       MOVE    "7"         TO  SPA-R01-RECEKBN
                   WHEN    "05"
                       MOVE    "3"         TO  SPA-R01-RECEKBN
                   WHEN    "06"
                       MOVE    "4"         TO  SPA-R01-RECEKBN
                   WHEN    "07"
                       MOVE    "5"         TO  SPA-R01-RECEKBN
                   WHEN    "08"
                       MOVE    "8"         TO  SPA-R01-RECEKBN
                   WHEN    "09"
                       MOVE    "6"         TO  SPA-R01-RECEKBN
                   WHEN    OTHER
                       MOVE    "E13"       TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
      *            診療年月
                   IF      RECEPRINTQ-PERFORM-MONTH
                                               =   SPACE
                       MOVE    "E14"              TO  WRK-ERRCD  
                       GO  TO  2001-INPUT-CHK-EXT
                   ELSE
                       MOVE    RECEPRINTQ-PERFORM-MONTH
                                                   TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       MOVE    WRK-SYMD (1:6)      TO  SPA-R01-SRYYM
      *
                       INITIALIZE                  ORCSGDAYAREA
                       MOVE    "1"             TO  SGDAY-INTYPE
                       MOVE    SPA-R01-SRYYM   TO  SGDAY-INDAY
                       CALL    "ORCSGDAY"      USING   ORCSGDAYAREA
                       IF      SGDAY-RC        =   ZERO
                           CONTINUE
                       ELSE
                           MOVE    "E11"            TO  WRK-ERRCD
                       END-IF
                   END-IF
               WHEN    "2"
                   MOVE    ZERO            TO  SPA-R01-SRYYM
           END-EVALUATE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *
      *    レセプト作成結果取得処理
           PERFORM 20013-RECEIPT-CHK-SEC
      *??
      *     DISPLAY "SPA-R01-RECEKBN  =" SPA-R01-RECEKBN "#" 
      *     DISPLAY "SPA-R01-FILEKBN  =" SPA-R01-FILEKBN "#" 
      *     DISPLAY "SPA-R01-NYUGAIKBN=" SPA-R01-NYUGAIKBN "#" 
      *     DISPLAY "SPA-R01-SRYYM    =" SPA-R01-SRYYM "#" 
      *     DISPLAY "WRK-ERRCD        =" WRK-ERRCD "#" 
      *     DISPLAY "RECEPRINTQ-PRINT-MODE=" RECEPRINTQ-PRINT-MODE "#"
      *??
      *
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      RECEPRINTQ-REQUEST-NUMBER
                                   =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    印刷指示
           EVALUATE    RECEPRINTQ-PRINT-MODE
               WHEN    "All"
                   IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDZZ    FROM    1   BY  1
                               UNTIL   IDZZ    >       25
                           IF      SPA-GMN-R04-TPAGE   (IDZZ)
                                           >   ZERO
                               MOVE    "T"             TO
                                          SPA-GMN-R04-TCHK-VALUE (IDZZ)
                           END-IF
                       END-PERFORM
                   END-IF
                   GO  TO  2001-INPUT-CHK-EXT
               WHEN    OTHER
                   IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
      *                画面ＳＰＡ書込み
                       PERFORM 000-R02SPA-OUT-SEC
      *
                       MOVE    "PUTG"          TO  MCP-STATUS
                       MOVE    "CLICKED"       TO  MCP-EVENT
                       MOVE    "SYOKBN"        TO  MCP-WIDGET
      *
                       MOVE    SPACE           TO  SPA-MOTOPG
                                                   LINKAREA
                       PERFORM 800-ORCGR02-CALL-SEC
                   END-IF
           END-EVALUATE
      *
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           EVALUATE    RECEPRINTQ-PRINT-MODE
               WHEN    "All"
                   CONTINUE
               WHEN    OTHER
                   PERFORM 20011-SUBMISSION-CHECK-SEC
           END-EVALUATE
      *
      *    レセプトが選択されているか
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                    TO  FLG-CHK
               IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       SPA-GMN-PAGE-MAX
                       PERFORM VARYING IDZZ    FROM    1   BY  1
                               UNTIL   IDZZ    >       WRK-CONS-LINE-MAX
                           IF      SPA-GMN-TCHK-VALUE (IDYY IDZZ)
                                                   =   "T"
                               MOVE    1                   TO  FLG-CHK
                               MOVE    SPA-GMN-PAGE-MAX    TO  IDYY
                               MOVE    WRK-CONS-LINE-MAX   TO  IDZZ
                           END-IF
                       END-PERFORM
                   END-PERFORM
               ELSE
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       25
                       IF      SPA-GMN-R04-TCHK-VALUE (IDZZ)   =   "T"
                           MOVE    1               TO  FLG-CHK
                           MOVE    25              TO  IDZZ
                       END-IF
                   END-PERFORM
               END-IF    
               IF      FLG-CHK         =   ZERO
                   MOVE    "E28"           TO  WRK-ERRCD
               END-IF
           END-IF
       *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       20011-SUBMISSION-CHECK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CHK-TABLE-AREA
      *
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
               IF      SPA-GMN-PRTJYUN    =   "05"    OR  "13"
                   MOVE    "E15"              TO  WRK-ERRCD
                   GO  TO  20011-SUBMISSION-CHECK-EXT
               END-IF
           END-IF
      *
      *    個別の項目チェック
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       19
                   OR      RECEPRINTQ-SUBMISSION-POSITION (IDYY)
                                   =   SPACE
               MOVE    SPACE           TO  WRK-ERRCD
               MOVE    ZERO            TO  FLG-ERR
      *
      *        連番（頁）
               INITIALIZE              ORCSNUMAREA
               MOVE    RECEPRINTQ-SUBMISSION-POSITION (IDYY)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC             =   ZERO )
               AND   ( SNUM-SEISUU         <   6    )
               AND   ( SNUM-SYOSUU         =   0    )
                   MOVE    SNUM-OUTNUM          TO  IDY
                   IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                       IF    ( SNUM-OUTNUM     >   SPA-GMN-PAGE-MAX )
                       OR    ( SNUM-OUTNUM     =   ZERO             )
                           MOVE    "E20"               TO  WRK-ERRCD
                       END-IF
                   ELSE
                       IF    ( SNUM-OUTNUM     >   1    )
                       OR    ( SNUM-OUTNUM     =   ZERO )
                           MOVE    "E20"               TO  WRK-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "E20"               TO  WRK-ERRCD
               END-IF
      *
               IF      WRK-ERRCD       =   SPACE
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       36
                           OR      RECEPRINTQ-DETAIL-POSITION
                                                       (IDYY IDZZ)
                                                   =   SPACE
      *                関連チェック処理
                       PERFORM 200111-CHECK-SEC
      *
                       IF      WRK-ERRCD       =   SPACE
      *
                           IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                               MOVE    "1"         TO
                                           WRK-CHK-R02-CHK  (IDY IDZ)
                           ELSE
                               MOVE    "1"         TO
                                           WRK-CHK-R04-CHK  (IDZ)
                           END-IF
      *
                           IF      RECEPRINTQ-PRINT-INSTRUCT (IDYY IDZZ)
                                                   =   "Yes"
                               IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                                   MOVE    "T"         TO
                                           SPA-GMN-TCHK-VALUE  (IDY IDZ)
                               ELSE
                                   MOVE    "T"         TO
                                           SPA-GMN-R04-TCHK-VALUE (IDZ)
                               END-IF
                           END-IF
                       ELSE
                           PERFORM 200112-RECEIPT-ERR-SEC
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       36
                           OR      RECEPRINTQ-DETAIL-POSITION
                                                       (IDYY IDZZ)
                                                   =   SPACE
                       PERFORM 200112-RECEIPT-ERR-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    明細一覧にエラーがあるとき
           IF      WRK-ERR-SUBMISSION-MAX
                                       >   ZERO
               MOVE    "E50"               TO  WRK-ERRCD
               GO  TO  20011-SUBMISSION-CHECK-EXT
           END-IF
      *
      *    明細一覧が全て設定されているか
           MOVE    ZERO                    TO  FLG-CHK
           IF      SPA-R01-RECEKBN
                                   =   "0" OR  "1" OR  "2" OR  "7"
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       SPA-GMN-PAGE-MAX
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       WRK-CONS-LINE-MAX
                       IF    ( SPA-NAI-PRTJYUNI (IDYY IDZZ)
                                           NOT =   SPACE )
                       AND   ( WRK-CHK-R02-CHK  (IDYY IDZZ)
                                               =   SPACE )
                           MOVE    1                   TO  FLG-CHK
                           MOVE    SPA-GMN-PAGE-MAX    TO  IDYY
                           MOVE    WRK-CONS-LINE-MAX   TO  IDZZ
                       END-IF
                   END-PERFORM
               END-PERFORM
           ELSE
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       25
                   IF    ( SPA-NAI-R04-PRTJYUNI (IDZZ)
                                           NOT =   SPACE )
                   AND   ( WRK-CHK-R04-CHK  (IDZZ)
                                               =   SPACE )
                       MOVE    1               TO  FLG-CHK
                       MOVE    25              TO  IDZZ
                   END-IF
               END-PERFORM
           END-IF    
           IF      FLG-CHK         =   1
               MOVE    "E27"           TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SUBMISSION-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       200111-CHECK-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRCD
      *
      *    連番（行）
           INITIALIZE                  ORCSNUMAREA
           MOVE    RECEPRINTQ-DETAIL-POSITION
                                       (IDYY IDZZ)
                                   TO  SNUM-INX
           CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
           AND   ( SNUM-SEISUU     <   6    )
           AND   ( SNUM-SYOSUU     =   0    )
               MOVE    SNUM-OUTNUM     TO  IDZ
               IF      SPA-R01-RECEKBN
                                   =   "0" OR  "1" OR  "2" OR  "7"
                   IF    ( SNUM-OUTNUM     >   36   )
                   OR    ( SNUM-OUTNUM     =   ZERO )
                       MOVE    "E21"           TO  WRK-ERRCD
                   ELSE
      *                印刷対象があるか
                       IF      SPA-NAI-PRTJYUNI (IDY IDZ)
                                               =   SPACE
                           MOVE    "E24"           TO  WRK-ERRCD
                       END-IF
                   END-IF
               ELSE
                   IF    ( SNUM-OUTNUM     >   25   )
                   OR    ( SNUM-OUTNUM     =   ZERO )
                       MOVE    "E21"           TO  WRK-ERRCD
                   ELSE
      *                印刷対象があるか
                       IF      SPA-NAI-R04-PRTJYUNI (IDZ)
                                               =   SPACE
                           MOVE    "E24"           TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    "E21"           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD    NOT =   SPACE
               GO  TO  200111-CHECK-EXT
           END-IF
      *
      *    件数
           INITIALIZE              ORCSNUMAREA
           MOVE    RECEPRINTQ-COUNT (IDYY IDZZ)
                                   TO  SNUM-INX
           CALL    "ORCSNUM"       USING
                                   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
           AND   ( SNUM-SEISUU     <   6    )
           AND   ( SNUM-SYOSUU     =   0    )
               MOVE    SNUM-OUTNUM     TO  WRK-KENSU
           ELSE
               MOVE    "E22"           TO  WRK-ERRCD
               GO  TO  200111-CHECK-EXT
           END-IF
      *
      *    枚数
           INITIALIZE              ORCSNUMAREA
           MOVE    RECEPRINTQ-NUMBER-OF-SHEETS (IDYY IDZZ)
                                   TO  SNUM-INX
           CALL    "ORCSNUM"       USING
                                   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
           AND   ( SNUM-SEISUU     <   6    )
           AND   ( SNUM-SYOSUU     =   0    )
               MOVE    SNUM-OUTNUM     TO  WRK-PAGE
           ELSE
               MOVE    "E23"           TO  WRK-ERRCD
               GO  TO  200111-CHECK-EXT
           END-IF
      *
      *    設定内容が同じか
           IF      SPA-R01-RECEKBN
                               =   "0" OR  "1" OR  "2" OR  "7"
               IF    ( SPA-GMN-TPAGE   (IDY IDZ)
                                       =   WRK-PAGE    )
               AND   ( SPA-GMN-TKENSU  (IDY IDZ)
                                       =   WRK-KENSU   )
               AND   ( SPA-NAI-SYUBETU (IDY IDZ 1)
                                       =   RECEPRINTQ-SUBMISSION-CODE
                                           (IDYY IDZZ) )
                   CONTINUE
               ELSE
                   MOVE    "E25"           TO  WRK-ERRCD
                   GO  TO  200111-CHECK-EXT
               END-IF
           ELSE
               IF    ( SPA-GMN-R04-TPAGE   (IDZ)
                                       =   WRK-PAGE    )
               AND   ( SPA-GMN-R04-TKENSU  (IDZ)
                                       =   WRK-KENSU   )
               AND   ( SPA-NAI-R04-SYUBETU (IDZ 1)
                                       =   RECEPRINTQ-SUBMISSION-CODE
                                           (IDYY IDZZ) )
                   CONTINUE
               ELSE
                   MOVE    "E25"           TO  WRK-ERRCD
                   GO  TO  200111-CHECK-EXT
               END-IF
           END-IF
      *
      *    印刷したいレセプトが０ページのとき
           IF      RECEPRINTQ-PRINT-INSTRUCT (IDYY IDZZ)
                                   =   "Yes"
               IF      SPA-R01-RECEKBN
                                       =   "0" OR  "1" OR  "2" OR  "7"
                   IF      SPA-GMN-TPAGE   (IDY IDZ)
                                           =   ZERO
                       MOVE    "E26"           TO  WRK-ERRCD
                   END-IF
               ELSE
                   IF      SPA-GMN-R04-TPAGE   (IDZ)
                                           =   ZERO
                       MOVE    "E26"           TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF    
      *
           .
       200111-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別指示エラー明細一覧編集処理
      *****************************************************************
       200112-RECEIPT-ERR-SEC     SECTION.
      *
           IF      FLG-ERR         =   ZERO
               MOVE    1               TO  FLG-ERR
               ADD     1               TO  WRK-ERR-SUBMISSION-MAX
               MOVE    ZERO            TO  WRK-ERR-DETAIL-MAX
      *
               MOVE    RECEPRINTQ-SUBMISSION-POSITION (IDYY)
                                       TO
                                       RECEPRINTS-SUBMISSION-POSITION
                                               (WRK-ERR-SUBMISSION-MAX)
               IF      WRK-ERRCD       =   "E20"
                   MOVE    WRK-ERRCD       TO  RECEPRINTS-ERROR-CODE
                                               (WRK-ERR-SUBMISSION-MAX)
                   MOVE    SPACE           TO  WRK-ERRCD
               END-IF
           END-IF
      *
           ADD     1               TO  WRK-ERR-DETAIL-MAX
      *??
      *     DISPLAY "MAX=" WRK-ERR-SUBMISSION-MAX "#" 
      *                    WRK-ERR-DETAIL-MAX "#"
      *??
           MOVE    RECEPRINTQ-DETAIL-POSITION     (IDYY IDZZ)
                                   TO  RECEPRINTS-DETAIL-POSITION
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-PRINT-INSTRUCT     (IDYY IDZZ)
                                   TO  RECEPRINTS-PRINT-INSTRUCT
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-SUBMISSION-IDENT   (IDYY IDZZ)
                                   TO  RECEPRINTS-SUBMISSION-IDENT
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-SUBMISSION-CODE      (IDYY IDZZ)
                                   TO  RECEPRINTS-SUBMISSION-CODE
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-SUBMISSION-TYPE      (IDYY IDZZ)
                                   TO  RECEPRINTS-SUBMISSION-TYPE
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-COUNT                (IDYY IDZZ)
                                   TO  RECEPRINTS-COUNT
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    RECEPRINTQ-NUMBER-OF-SHEETS     (IDYY IDZZ)
                                   TO  RECEPRINTS-NUMBER-OF-SHEETS
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           MOVE    WRK-ERRCD       TO  RECEPRINTS-DETAIL-ERROR-CODE
                           (WRK-ERR-SUBMISSION-MAX WRK-ERR-DETAIL-MAX)
           .
       200112-RECEIPT-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷確認処理
      *****************************************************************
       20013-RECEIPT-CHK-SEC     SECTION.
      *
      *    ジョブ管理チェック処理（レセプト作成）
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF    ( RECEPRINTQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID )
               OR    ( RECEPRINTQ-ORCA-UID
                                   NOT =   JOB-ORCA-UID  )
               OR    ( SPA-R01-NYUGAIKBN
                                   NOT =   JOB-NYUGAIKBN )
               OR    ( SPA-R01-FILEKBN
                                   NOT =   JOB-SYOKBN    )
               OR    ( SPA-R01-SRYYM
                                   NOT =   JOB-SRYYM     )
                   MOVE    "E42"           TO  WRK-ERRCD
                   GO  TO  20013-RECEIPT-CHK-EXT
               END-IF
      *
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "E70"           TO  WRK-ERRCD
                   STRING  "レセプト作成処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE
                                             INTO  WRK-ERRMSG
                   END-STRING
                  GO  TO  20013-RECEIPT-CHK-EXT
               END-IF
      *
               IF      RECEPRINTQ-REQUEST-NUMBER
                                       =   "02"
                  GO  TO  20013-RECEIPT-CHK-EXT
               END-IF
      *
               MOVE    "PUTG"          TO  MCP-STATUS
               MOVE    "CLICKED"       TO  MCP-EVENT
               MOVE    "B11"           TO  MCP-WIDGET
      *
               MOVE    SPACE           TO  LINKAREA
               MOVE    "R03"           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-R01-GMNKBN
      *
               MOVE    SPA-R01KYOUTU   TO  SPA-WORK
               MOVE    SPA-AREA        TO  SPA-COMMON
      *
               CALL    "ORCGR99"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
               MOVE    TAIHI-MCPAREA   TO  MCPAREA
      *
               EVALUATE    R99-MSG
                   WHEN    "処理は正常に終了しました"
                       CONTINUE
                   WHEN    OTHER
                       IF      R99-B12-STATE   =   WIDGET-NORMAL
      ****                 MOVE    R99-MSG         TO  WRK-WARNING-MSG
      ****                 MOVE    "W01"           TO  WRK-WARNING
                           CONTINUE
                       ELSE
                           MOVE    "E31"           TO  WRK-ERRCD
                           MOVE    R99-MSG         TO  WRK-ERRMSG
                       END-IF
               END-EVALUATE
      *
               IF      R99-B12-STATE   =   WIDGET-NORMAL
      *            頁・件数の編集処理
                   PERFORM 3101-HENSYU-SEC
               END-IF
           ELSE
               MOVE    "E40"           TO  WRK-ERRCD
           END-IF
           .
       20013-RECEIPT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       3101-HENSYU-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-MOTOPG
                                       LINKAREA
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
               PERFORM 800-ORCGR02-CALL-SEC
           ELSE
               PERFORM 800-ORCGR04-CALL-SEC
           END-IF
           .
       3101-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷処理
      *****************************************************************
       2002-RECEIPT-PRINT-SEC     SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-RECEIPT-PRINT-EXT
           END-IF
      *
           MOVE    RECEPRINTQ-KARTE-UID
                                   TO  SPA-R01-JOB-KARTE-UID
           MOVE    RECEPRINTQ-ORCA-UID
                                   TO  SPA-R01-JOB-ORCA-UID
      *
           PERFORM 800-TEMPDIR-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2002-RECEIPT-PRINT-EXT
           END-IF
      *
           MOVE    "RID3"          TO  SPA-MOTOPG
           MOVE    "NO"            TO  SPA-RID1-FLG
           MOVE    "1001"          TO  SPA-RIDCD3
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
      *        画面ＳＰＡ書込み
               PERFORM 000-R02SPA-OUT-SEC
      *
               PERFORM 800-ORCGR02-CALL-SEC
           ELSE
               PERFORM 800-ORCGR04-CALL-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD           =   "E88"
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               ELSE
                   MOVE    "E30"               TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2002-RECEIPT-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷確認処理
      *****************************************************************
       2003-RECEIPT-CHECK-SEC     SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID1
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF    ( RECEPRINTQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID )
               OR    ( RECEPRINTQ-ORCA-UID
                                   NOT =   JOB-ORCA-UID  )
                   MOVE    "E43"           TO  WRK-ERRCD
                   GO  TO  2003-RECEIPT-CHECK-EXT
               END-IF
      *
      *??
      *         DISPLAY "JOB-UPDCNT=" JOB-UPDCNT "#" JOB-ERRCD
      *??
               IF      JOB-UPDCNT      >   ZERO
                   MOVE    JOB-UPDCNT      TO  WRK-UPDCNT
                   MOVE    WRK-UPDCNT-X    TO
                                       RECEPRINTS-ALL-NUMBER-OF-SHEETS
                   IF      JOB-ERRCD   NOT =   SPACE
                       MOVE    "E31"           TO  WRK-ERRCD
                       MOVE    JOB-YOBI        TO  WRK-ERRMSG
                   END-IF
               ELSE
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "E70"           TO  WRK-ERRCD
                       STRING  "レセプト印刷処理中です【"
                                                 DELIMITED  BY  SIZE
                               JOB-SHELLMSG      DELIMITED  BY  SPACE
                               "】"              DELIMITED  BY  SIZE
                                                 INTO  WRK-ERRMSG
                       END-STRING
                   ELSE
                       MOVE    "E31"           TO  WRK-ERRCD
                       MOVE    "該当のレセプトはありませんでした"
                                               TO  WRK-ERRMSG
                   END-IF
               END-IF
           ELSE
               MOVE    "E41"           TO  WRK-ERRCD
           END-IF
           .
       2003-RECEIPT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト（医保）印刷処理
      *****************************************************************
       800-ORCGR02-CALL-SEC                SECTION.
      *??
      *     DISPLAY "800 R02 IN =" SPA-TERMID "#" 
      *??
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           CALL    "ORCGR02"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *??
      *     DISPLAY "800 R02 OUT=" SPA-TERMID "#" 
      *??
      *    画面ＳＰＡ読込み
           PERFORM 000-R02SPA-SET-SEC
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGR02-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト（医保以外）印刷処理
      *****************************************************************
       800-ORCGR04-CALL-SEC                SECTION.
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           CALL    "ORCGR04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGR04-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-R02SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  FLG-SPASCR
      *??
      *     DISPLAY "800 SPA IN =" SPA-TERMID "#" 
      *??
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SPATMP-READ-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO   SPA-SCR-REC
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBCLOSE-SEC
      *
      *    一時保存テーブル削除処理
           PERFORM 900-SPATMP-DBDELETE-SEC
           .
       000-R02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-R02SPA-OUT-SEC             SECTION.
      *??
      *     DISPLAY "800 SPA OUT=" SPA-TERMID "#" 
      *??
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-SCR-REC         TO  SPATMP-DATA-REC
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "R02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       000-R02SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成キャンセル処理
      *****************************************************************
       2004-RECEIPT-CANCEL-SEC     SECTION.
           .
       2004-RECEIPT-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID1
                                   TO  JOB-SHELLID
          PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "E70"           TO  WRK-ERRCD
                   STRING  "レセプト印刷処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE
                                             INTO  WRK-ERRMSG
                   END-STRING
               END-IF
           END-IF    
           .
       800-JOBKANRI-CHK-EXT.
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
      *
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E70"
      *            レセプト印刷処理中
                   GO  TO  890-ERRCD-MSG-EXT
               WHEN    "E30"
               WHEN    "E31"
               WHEN    "E32"
      *            レセプト作成結果
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
                   STRING  "印刷順が入力順、ターミナルデジットのときは"
                                           DELIMITED   BY  SIZE
                           "個別指示での印刷はできません。"
                                           DELIMITED   BY  SIZE
                                           INTO        WRK-ERRMSG
                   END-STRING
               WHEN    "E20"
                   MOVE    "連番の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E21"
                   MOVE    "連番の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE    "件数の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E23"
                   MOVE    "枚数の設定に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "E24"
                   MOVE    "対象のレセプトがありません。"
                                               TO  WRK-ERRMSG
               WHEN    "E25"
                   MOVE    "設定内容が違います。"  TO  WRK-ERRMSG
               WHEN    "E26"
                   MOVE    "印刷するレセプトがありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E27"
                   MOVE    "明細一覧に設定されていない種別があります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E28"
                   MOVE    "印刷するレセプトが選択してありません。"
                                                   TO  WRK-ERRMSG
      *
               WHEN    "E40"
               WHEN    "E41"
                   MOVE    "対象のジョブ管理がありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E42"
               WHEN    "E43"
                   MOVE    "該当する処理はありません。"
                                                   TO  WRK-ERRMSG
      *
               WHEN    "E50"
                   MOVE    "明細の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
      *
               WHEN    "E80"
                   MOVE    "一時ディレクトリが取得できません"
                                                   TO  WRK-ERRMSG
               WHEN    "E88"
      *            ＭＯＮＢＬＯＢ取得エラー
                   MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
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
      *    一時保存テーブルＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       900-SPATMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルクローズ処理
      *****************************************************************
       900-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル削除処理
      *****************************************************************
       900-SPATMP-DBDELETE-SEC         SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       900-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       900-SPATMP-CALL-EXT.
           EXIT.
      *
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
