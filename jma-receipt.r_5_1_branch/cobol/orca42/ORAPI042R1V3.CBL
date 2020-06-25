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
       PROGRAM-ID.         ORAPI042R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    :  API連携用モジュール
      *  コンポーネント名  : レセプト作成(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/09/15    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    20/03/27  平成３２年４月改正未対応メッ
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "R01SCR-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "R03SCR-SPA"    REPLACING  //SPA-GMN-//
                                   BY         //SPA-GMN-R03-//.
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
           03  WRK-WARNING-MSG         PIC X(140).
      *
           03  WRK-ERR-PATIENT-MAX     PIC 9(03).
           03  WRK-ERR-PATIENT-MAX1    PIC 9(03).
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
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-X.
               05  WRK-9               PIC 9(05).
      *
           03  WRK-FILEMEI             PIC X(20).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-SPA-TERMID          PIC X(64).
      * 
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "TEIKI1".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   1.
           03  WRK-CONS-JOB-SHELLID2
                                   PIC X(08)   VALUE   "RECEPT2".
      *
      *    個別対象患者一覧
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPT-MAKEV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPRECEIPT-MAKEV3RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "MCPDATA2.INC".
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
      *    定期請求ジョブチェックサブ
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
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
           DISPLAY   "**********************"
           DISPLAY   "* receipt_Make start *"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *****INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
                                           SPA-R01KYOUTU
                                           SPA-R01FREE
                                           SPA-R02
                                           SPA-R03
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
           DISPLAY   "**********************"
           DISPLAY   "* receipt_Make end   *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-RECEIPT-MAKERES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  RECEMAKES-RESKEY
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
                                       RECEMAKES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO
                                       RECEMAKES-INFORMATION-TIME
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
           MOVE    ZERO                TO  RECEMAKES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
                                           WRK-WARNING
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    RECEMAKEQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理実施
                   MOVE    1                   TO  FLG-SYORI
               WHEN    "02"
      *            処理確認
                   MOVE    2                   TO  FLG-SYORI
      *****    WHEN    "03"
      *****       キャンセル
      *****        MOVE    3                   TO  FLG-SYORI
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
           IF    ( WRK-ERRCD           =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    RECEMAKEQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理実施処理
                   PERFORM 2002-RECEIPT-MAKE-SEC
               WHEN    "02"
      *            処理確認処理
                   PERFORM 2003-RECEIPT-CHECK-SEC
               WHEN    "03"
      *            キャンセル処理
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
           INITIALIZE                      XML-RECEIPT-MAKEREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_receipt_makev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "receipt_makev3req" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-RECEIPT-MAKEREQ
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
               MOVE    "02"            TO  RECEMAKES-RESPONS-NUMBER
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  RECEMAKES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  RECEMAKES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "処理キャンセル終了"
                                           TO  RECEMAKES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               MOVE    "01"            TO  RECEMAKES-RESPONS-NUMBER
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  RECEMAKES-API-RESULT
               MOVE    WRK-ERRMSG      TO  RECEMAKES-API-RESULT-MSG
           END-IF
      *
      *    エラーがなければワーニング分も返却
           IF    ( WRK-ERR-PATIENT-MAX1
                                       =   ZERO )
           AND   ( WRK-WARNING-PATIENT-MAX
                                       >   ZERO )
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       WRK-WARNING-PATIENT-MAX
                   ADD     1               TO  WRK-ERR-PATIENT-MAX
                   MOVE    WRK-WARNING-PATIENT-ID
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  RECEMAKES-PATIENT-ID
                                                   (WRK-ERR-PATIENT-MAX)
                   MOVE    WRK-WARNING-PERFORM-DATE
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  RECEMAKES-PERFORM-DATE
                                                   (WRK-ERR-PATIENT-MAX)
                   MOVE    WRK-WARNING-ERROR-CODE
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   MOVE    WRK-WARNING-ERROR-MSG
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  RECEMAKES-ERROR-MSG
                                                   (WRK-ERR-PATIENT-MAX)
                   IF      WRK-WARNING     =   SPACE
                       MOVE    WRK-WARNING-ERROR-CODE
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  WRK-WARNING
                       MOVE    WRK-WARNING-ERROR-MSG
                                               (WRK-WARNING-PATIENT-MAX)
                                           TO  WRK-WARNING-MSG
                   END-IF
               END-PERFORM
           END-IF
      *    警告メッセージ
           MOVE    WRK-WARNING     TO  RECEMAKES-API-WARNING
           MOVE    WRK-WARNING-MSG TO  RECEMAKES-API-WARNING-MSG
      *
           MOVE    RECEMAKEQ-KARTE-UID TO  RECEMAKES-KARTE-UID
           IF      FLG-SYORI           =   1
               IF    ( WRK-ERRCD           =   SPACE )
               AND   ( SPA-ERRCD           =   SPACE )
                   MOVE    RECEMAKEQ-ORCA-UID  TO  RECEMAKES-ORCA-UID
               END-IF
           ELSE
               MOVE    RECEMAKEQ-ORCA-UID  TO  RECEMAKES-ORCA-UID
           END-IF
           MOVE    RECEMAKEQ-REQUEST-NUMBER
                                       TO  RECEMAKES-REQUEST-NUMBER
           EVALUATE    RECEMAKEQ-RECEIPT-MODE
               WHEN    "All"
                   MOVE    RECEMAKEQ-PERFORM-MONTH
                                           TO  RECEMAKES-PERFORM-MONTH
           END-EVALUATE
      *
           IF      APILST-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_receipt_makev3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  RECEMAKES-MODE
           MOVE    "receipt_makev3res" TO  RECEMAKES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-RECEIPT-MAKERES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  RECEMAKES-OBJECT      TO  APILST-BODY
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
           IF      RECEMAKEQ-KARTE-UID =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   オルカUID
           IF      RECEMAKEQ-REQUEST-NUMBER
                                       =   "01"
               CONTINUE
           ELSE
               IF      RECEMAKEQ-ORCA-UID  =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    初画面情報
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-RIDCD-AREA
                                       SPA-RID1-AREA
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           PERFORM 800-ORCGR03-CALL-SEC
      *
      *    改正対応メッセージ
           IF      R03-MSG-LABEL   NOT =   SPACE
               MOVE    "W99"           TO  WRK-WARNING
               MOVE    R03-MSG-LABEL   TO  WRK-WARNING-MSG
           END-IF
      *
      *    処理区分
           EVALUATE    RECEMAKEQ-RECEIPT-MODE
               WHEN    "All"
                   MOVE    "T"             TO  SPA-GMN-R03-IKKATU
                   MOVE    "F"             TO  SPA-GMN-R03-KOBETU
               WHEN    OTHER
                   MOVE    "T"             TO  SPA-GMN-R03-KOBETU
                   MOVE    "F"             TO  SPA-GMN-R03-IKKATU
           END-EVALUATE
      *
      *    レセプト作成区分
           EVALUATE    RECEMAKEQ-PRINT-MODE
               WHEN    "Check"
                       MOVE    "2"         TO  R03-PRTKBN
                   WHEN    OTHER
                       MOVE    "1"         TO  R03-PRTKBN
           END-EVALUATE
      *
      *    入外区分
           EVALUATE    RECEMAKEQ-INOUT
               WHEN    "I"
                   MOVE    "1"             TO  R03-NYUGAIKBN
               WHEN    "O"
                   MOVE    "2"             TO  R03-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "E12"           TO  WRK-ERRCD
           END-EVALUATE
      *
      *    提出先
           IF      WRK-ERRCD           =   SPACE
               MOVE    "F"                 TO  SPA-DATAKBN1-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN2-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN3-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN4-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN5-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN11-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN21-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN22-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN23-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN24-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN25-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN26-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN31-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN32-VALUE
               MOVE    "F"                 TO  SPA-DATAKBN33-VALUE
               EVALUATE    SPA-GMN-R03-IKKATU
                   WHEN    "T"
                       EVALUATE    RECEMAKEQ-SUBMISSION-MODE
                           WHEN    "01"
                               MOVE    "T"     TO  SPA-DATAKBN1-VALUE
                           WHEN    "02"
                               MOVE    "T"     TO  SPA-DATAKBN2-VALUE
                           WHEN    "03"
                               MOVE    "T"     TO  SPA-DATAKBN3-VALUE
                           WHEN    "04"
                               MOVE    "T"     TO  SPA-DATAKBN5-VALUE
                           WHEN    "05"
                               MOVE    "T"     TO  SPA-DATAKBN11-VALUE
                           WHEN    "06"
                               MOVE    "T"     TO  SPA-DATAKBN31-VALUE
                           WHEN    "07"
                               MOVE    "T"     TO  SPA-DATAKBN32-VALUE
                           WHEN    "08"
                               MOVE    "T"     TO  SPA-DATAKBN33-VALUE
                           WHEN    "09"
                               MOVE    "T"     TO  SPA-DATAKBN4-VALUE
                           WHEN    OTHER
                               MOVE    "E13"   TO  WRK-ERRCD
                       END-EVALUATE
                   WHEN    "F"
                       EVALUATE    RECEMAKEQ-SUBMISSION-MODE
                           WHEN    "01"
                               MOVE    "T"     TO  SPA-DATAKBN21-VALUE
                           WHEN    "05"
                               MOVE    "T"     TO  SPA-DATAKBN22-VALUE
                           WHEN    "06"
                               MOVE    "T"     TO  SPA-DATAKBN23-VALUE
                           WHEN    "07"
                               MOVE    "T"     TO  SPA-DATAKBN24-VALUE
                           WHEN    "08"
                               MOVE    "T"     TO  SPA-DATAKBN26-VALUE
                           WHEN    "09"
                               MOVE    "T"     TO  SPA-DATAKBN25-VALUE
                           WHEN    OTHER
                               MOVE    "E13"   TO  WRK-ERRCD
                       END-EVALUATE
               END-EVALUATE
           END-IF
     *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-R03-IKKATU
               WHEN    "T"
      *            診療年月
                   IF      RECEMAKEQ-PERFORM-MONTH
                                               =   SPACE
                       MOVE    "E14"              TO  WRK-ERRCD  
                       GO  TO  2001-INPUT-CHK-EXT
                   ELSE
                       MOVE    RECEMAKEQ-PERFORM-MONTH
                                                  TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       MOVE    WRK-SYMD (1:6)      TO  R03-SRYYM
                   END-IF
           END-EVALUATE
      *
           MOVE    RECEMAKEQ-KARTE-UID
                                   TO  SPA-R01-JOB-KARTE-UID
           MOVE    RECEMAKEQ-ORCA-UID
                                   TO  SPA-R01-JOB-ORCA-UID
      *
      *    更新前チェック
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-RIDCD-AREA
                                       SPA-RID1-AREA
      *
           MOVE    "PUTG"          TO  MCP-STATUS
           MOVE    "CLICKED"       TO  MCP-EVENT
           MOVE    "B12"           TO  MCP-WIDGET
      *
           PERFORM 800-ORCGR03-CALL-SEC
      *??
      *     DISPLAY "CHK SPA-ERRCD=" SPA-ERRCD "#" SPA-RIDCD-AREA "#"
      *     DISPLAY "CHK SPA-R01-FILEKBN=" SPA-R01-FILEKBN
      *     DISPLAY "CHK SPA-R01-RECEKBN=" SPA-R01-RECEKBN
      *     DISPLAY "CHK SPA-R01-PRTKBN =" SPA-R01-PRTKBN
      *     DISPLAY "CHK SPA-R01-NYUGAIKBN =" SPA-R01-NYUGAIKBN
      *??
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "0001"
                       MOVE    "E11"               TO  WRK-ERRCD
                   WHEN    "7777"
                       MOVE    "E70"               TO  WRK-ERRCD
                       MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
                       IF      RECEMAKEQ-REQUEST-NUMBER
                                                   =   "02"
      *                    ジョブ管理チェック処理
                           MOVE    "CHK"           TO  SJOBKANRI-MODE
                           INITIALIZE                  JOBKANRI-REC
                           MOVE    WRK-CONS-JOB-JOBID
                                                   TO  JOB-JOBID
                           MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                           PERFORM 900-CALL-ORCSJOB-SEC
                           IF      SJOBKANRI-RETURN
                                                   =   ZERO
      *
                               IF    ( RECEMAKEQ-KARTE-UID
                                               NOT =   JOB-KARTE-UID )
                               OR    ( RECEMAKEQ-ORCA-UID
                                               NOT =   JOB-ORCA-UID  )
                               OR    ( SPA-R01-NYUGAIKBN
                                               NOT =   JOB-NYUGAIKBN )
                               OR    ( SPA-R01-FILEKBN
                                               NOT =   JOB-SYOKBN    )
                               OR    ( SPA-R01-SRYYM
                                               NOT =   JOB-SRYYM     )
                                   MOVE    "E41"           TO  WRK-ERRCD
                                   GO  TO  2001-INPUT-CHK-EXT
                               END-IF
                           END-IF
                       END-IF
                   WHEN    OTHER
                       MOVE    "E60"               TO  WRK-ERRCD
               END-EVALUATE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    個別作成チェック
           IF    ( SPA-R01-FILEKBN =   "2"  )
           AND   ( RECEMAKEQ-REQUEST-NUMBER
                                   =   "01" )
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
                                           SPA-RIDCD-AREA
                                           SPA-RID1-AREA
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               PERFORM 800-ORCGR01-CALL-SEC
      *
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       WRK-CONS-LINE-MAX
                       OR (    RECEMAKEQ-PATIENT-ID (IDZZ)
                                       =   SPACE
                           AND RECEMAKEQ-PATIENT-PERFORM-DATE (IDZZ)
                                       =   SPACE )
      *
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                                               SPA-RIDCD-AREA
                                               SPA-RID1-AREA
                                               SPA-ERRCD
                                               SPA-ERRMSG
      *            患者番号
                   IF      RECEMAKEQ-PATIENT-ID (IDZZ)
                                               =   SPACE
                       MOVE    "E001"              TO  SPA-ERRCD
                       MOVE    "患者番号が未設定です"
                                                   TO  SPA-ERRMSG
                   ELSE
                       MOVE    RECEMAKEQ-PATIENT-ID (IDZZ)
                                                   TO  R01-PTNUM
                   END-IF
      *            診療年月
                   IF      RECEMAKEQ-PATIENT-PERFORM-DATE (IDZZ)
                                               =   SPACE
                       MOVE    "E014"              TO  SPA-ERRCD
                       MOVE    "診療年月が未設定です"
                                                   TO  SPA-ERRMSG
                   ELSE
                       MOVE    RECEMAKEQ-PATIENT-PERFORM-DATE (IDZZ)
                                                   TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       MOVE    WRK-SYMD (1:6)      TO  R01-SRYYM
                   END-IF
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE           TO  MCP-STATUS
                       MOVE    "ACTIVATE"      TO  MCP-EVENT
                       MOVE    SPACE           TO  MCP-WIDGET
      *
                       PERFORM 800-ORCGR01-CALL-SEC
      *??
      *     DISPLAY "R01 SPA-ERRCD=" IDZZ "#"
      *                 RECEMAKEQ-PATIENT-ID (IDZZ) "#"
      *               SPA-ERRCD "#" SPA-RIDCD "#" SPA-GMN-R01-MAX "#"
      *??
                   END-IF
      *
                   EVALUATE    SPA-RIDCD
                       WHEN    "1008"
                       WHEN    "1009"
                       WHEN    "1010"
      *                    診療なしの請求管理削除処理
                           MOVE    SPACE       TO  LINKAREA
                                                   SPA-MOTOPG
                                                   SPA-RIDCD-AREA
                                                   SPA-RID1-AREA
      *
                           MOVE    "OK"        TO  SPA-RID1-FLG
                           MOVE    "RID1"      TO  SPA-MOTOPG
                           MOVE    "1008"      TO  SPA-RIDCD
      *
                           MOVE    "LINK"      TO  MCP-STATUS
                           MOVE    SPACE       TO  MCP-EVENT
      *
                           PERFORM 800-ORCGR01-CALL-SEC
      *
                           MOVE    "1008"      TO  SPA-RIDCD
                           PERFORM 20012-PATIENT-ERR-SEC
                       WHEN    OTHER
                           IF      SPA-ERRCD   =   SPACE
                               IF      SPA-NAI-R01-TCHK
                                                   (SPA-GMN-R01-MAX)
                                                   =   1
                                   PERFORM 20013-PATIENT-WARNING-SEC
                               END-IF
                           ELSE
                               PERFORM 20012-PATIENT-ERR-SEC
                           END-IF
                   END-EVALUATE
               END-PERFORM
      *
      *        エラーがあれば処理終了
               IF      WRK-ERR-PATIENT-MAX1    >   ZERO
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
      *
      *        更新前チェック
               MOVE    SPACE           TO  LINKAREA
                                           SPA-MOTOPG
                                           SPA-RIDCD-AREA
                                           SPA-RID1-AREA
      *
               MOVE    "PUTG"          TO  MCP-STATUS
               MOVE    "CLICKED"       TO  MCP-EVENT
               MOVE    "B12"           TO  MCP-WIDGET
      *
               PERFORM 800-ORCGR01-CALL-SEC
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
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別対象患者エラー処理
      *****************************************************************
       20012-PATIENT-ERR-SEC     SECTION.
      *
           ADD     1               TO  WRK-ERR-PATIENT-MAX
           MOVE    RECEMAKEQ-PATIENT-ID (IDZZ)
                                   TO  RECEMAKES-PATIENT-ID
                                                   (WRK-ERR-PATIENT-MAX)
           MOVE    RECEMAKEQ-PATIENT-PERFORM-DATE (IDZZ)
                                   TO  RECEMAKES-PERFORM-DATE
                                                   (WRK-ERR-PATIENT-MAX)
           IF      SPA-ERRCD       =   SPACE
               IF       SPA-RIDCD      =   "1008"
                   MOVE    "W20"               TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   MOVE    "診療がないので請求管理を削除しました。"
                                               TO  RECEMAKES-ERROR-MSG
                                                   (WRK-ERR-PATIENT-MAX)
      *
                   IF      WRK-WARNING     =   SPACE
                       MOVE    "W20"           TO  WRK-WARNING
                       MOVE    RECEMAKES-ERROR-MSG (WRK-ERR-PATIENT-MAX)
                                               TO  WRK-WARNING-MSG
                   END-IF
               END-IF
           ELSE
               ADD     1               TO  WRK-ERR-PATIENT-MAX1
               EVALUATE    SPA-ERRCD
                   WHEN    "0003"
                       MOVE    "E10"       TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   WHEN    "0009"
                       MOVE    "E11"       TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   WHEN    "E001"
                       MOVE    "E01"       TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   WHEN    "E014"
                       MOVE    "E14"       TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
                   WHEN    OTHER
                       MOVE    "E50"       TO  RECEMAKES-ERROR-CODE
                                                   (WRK-ERR-PATIENT-MAX)
               END-EVALUATE
               MOVE    SPA-ERRMSG      TO  RECEMAKES-ERROR-MSG
                                                   (WRK-ERR-PATIENT-MAX)
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E63"           TO  WRK-ERRCD
                   MOVE    RECEMAKES-ERROR-MSG (WRK-ERR-PATIENT-MAX)
                                           TO  WRK-ERRMSG
               END-IF
           END-IF
            .
       20012-PATIENT-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別対象患者ワーニング処理
      *****************************************************************
       20013-PATIENT-WARNING-SEC   SECTION.
      *
           ADD     1               TO  WRK-WARNING-PATIENT-MAX
           MOVE    RECEMAKEQ-PATIENT-ID (IDZZ)
                                   TO  WRK-WARNING-PATIENT-ID
                                               (WRK-WARNING-PATIENT-MAX)
           MOVE    RECEMAKEQ-PATIENT-PERFORM-DATE (IDZZ)
                                   TO  WRK-WARNING-PERFORM-DATE
                                               (WRK-WARNING-PATIENT-MAX)
           MOVE    "W21"           TO  WRK-WARNING-ERROR-CODE
                                               (WRK-WARNING-PATIENT-MAX)
           MOVE    "請求しない設定の請求管理を削除しました。"
                                   TO  WRK-WARNING-ERROR-MSG
                                               (WRK-WARNING-PATIENT-MAX)
      *
          .
       20013-PATIENT-WARNING-EXT.
           EXIT.
      *****************************************************************
      *    レセプト作成処理
      *****************************************************************
       2002-RECEIPT-MAKE-SEC       SECTION.
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  RECEMAKEQ-ORCA-UID
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2002-RECEIPT-MAKE-EXT
           END-IF
      *??
      *     DISPLAY "MAKE SPA-ERRCD=" SPA-ERRCD "#" SPA-RIDCD-AREA "#"
      *     DISPLAY "MAKE SPA-R01-SYOKBN=" SPA-R01-SYOKBN "#"
      *     DISPLAY "MAKE SPA-R01-RECEKBN=" SPA-R01-RECEKBN 
      *     DISPLAY "MAKE SPA-R01-FILEKBN=" SPA-R01-FILEKBN
      *??
      *
           MOVE    RECEMAKEQ-KARTE-UID
                                   TO  SPA-R01-JOB-KARTE-UID
           MOVE    RECEMAKEQ-ORCA-UID
                                   TO  SPA-R01-JOB-ORCA-UID
      *
           PERFORM 800-TEMPDIR-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  2002-RECEIPT-MAKE-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
                                       SPA-MOTOPG
                                       SPA-RID1-AREA
      *
           IF     SPA-NAI-R01-SEIKYU-CHK   =   1
               MOVE    "RID4"          TO  SPA-MOTOPG
               MOVE    "CHG"           TO  SPA-RID4-FLG
           ELSE
               MOVE    "RID1"          TO  SPA-MOTOPG
               MOVE    "OK"            TO  SPA-RID1-FLG
           END-IF
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
                   PERFORM 800-ORCGR03-CALL-SEC
               WHEN    "2"
                   PERFORM 800-ORCGR01-CALL-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "E62"               TO  WRK-ERRCD
           END-IF
      *
           .
       2002-RECEIPT-MAKE-EXT.
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
      *    レセプト作成確認処理
      *****************************************************************
       2003-RECEIPT-CHECK-SEC     SECTION.
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
               IF    ( RECEMAKEQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID )
               OR    ( RECEMAKEQ-ORCA-UID
                                   NOT =   JOB-ORCA-UID  )
               OR    ( SPA-R01-NYUGAIKBN
                                   NOT =   JOB-NYUGAIKBN )
               OR    ( SPA-R01-FILEKBN
                                   NOT =   JOB-SYOKBN    )
               OR    ( SPA-R01-SRYYM
                                   NOT =   JOB-SRYYM     )
                   MOVE    "E41"           TO  WRK-ERRCD
                   GO  TO  2003-RECEIPT-CHECK-EXT
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
                   GO  TO  2003-RECEIPT-CHECK-EXT
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
                           MOVE    R99-MSG         TO  WRK-WARNING-MSG
                           MOVE    "W01"           TO  WRK-WARNING
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
      *
           .
       2003-RECEIPT-CHECK-EXT.
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
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2" OR  "7"
               CALL    "ORCGR02"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *        画面ＳＰＡ読込み
               PERFORM 000-R02SPA-SET-SEC
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-FREE        TO  SPA-R02
               MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
               MOVE    TAIHI-MCPAREA   TO  MCPAREA
      *
               PERFORM VARYING     IDW     FROM    1   BY  1
                       UNTIL       IDW     >       SPA-GMN-PAGE-MAX
                   MOVE    IDW             TO  WRK-9
                   MOVE    WRK-X           TO
                                   RECEMAKES-SUBMISSION-POSITION (IDW)
                   IF      SPA-GMN-THKNJANAME-SYAHO (IDW)
                                       NOT =   SPACE
                       MOVE    SPA-GMN-THKNJANAME-SYAHO (IDW)
                                           TO  
                                   RECEMAKES-SUBMISSION-TITLE (IDW)
                   END-IF
      *
                   MOVE    ZERO            TO  IDZ
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >       36
                       IF      SPA-NAI-PRTJYUNI (IDW IDX)  =   SPACE
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDZ
                           MOVE    IDX             TO  WRK-9
                           MOVE    WRK-X           TO
                                    RECEMAKES-DETAIL-POSITION(IDW IDZ)
                           MOVE    SPA-GMN-TKENSU      (IDW IDX)
                                                   TO
                                   RECEMAKES-COUNT           (IDW IDZ)
                           MOVE    SPA-GMN-TPAGE       (IDW IDX)
                                                   TO
                                   RECEMAKES-NUMBER-OF-SHEETS(IDW IDZ)
                           MOVE    SPA-NAI-SYUBETU    (IDW IDX 1)
                                                   TO
                                   RECEMAKES-SUBMISSION-CODE (IDW IDZ)
                           MOVE    SPA-GMN-TSYURUINAME (IDW IDX)
                                                   TO
                                   RECEMAKES-SUBMISSION-TYPE (IDW IDZ)
                           EVALUATE    SPA-NAI-SYUBETU    (IDW IDX 1)
                               WHEN    "0000"
                               WHEN    "8888"
                                   MOVE    "99"    TO
                                       RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                               WHEN    "6666"
                                   IF      IDX         =   33
                                       MOVE    "04"    TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                   ELSE
                                       MOVE    "03"    TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                   END-IF
                               WHEN    OTHER
                                   IF      SPA-GMN-PRTJYUN =
                                                   "07"    OR  "08"  OR
                                                   "11"    OR  "12"  OR
                                                   "13"
                                     MOVE    "99"    TO
                                             RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                   ELSE
                                       EVALUATE  SPA-NAI-TEISYUTUSAKI
                                                           (IDW IDX)
                                           WHEN    "1"
                                             MOVE    "02"    TO
                                             RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                            WHEN    "2"
                                              MOVE    "03"    TO
                                              RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                           WHEN    "6"
                                              MOVE    "04"    TO
                                              RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                           WHEN    OTHER
                                              MOVE    "99"    TO
                                              RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                                       END-EVALUATE
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               MOVE    SPA-GMN-PRTJYUNMEI  TO
                                       RECEMAKES-RECEIPT-PRINT-ORDER (1)
               MOVE    SPA-GMN-PRTJYUN1MEI TO
                                       RECEMAKES-RECEIPT-PRINT-ORDER (2)
               MOVE    SPA-GMN-PRTJYUN2MEI TO
                                       RECEMAKES-RECEIPT-PRINT-ORDER (3)
               MOVE    SPA-GMN-PAGEH       TO  RECEMAKES-ALL-COUNT
               MOVE    SPA-GMN-KENSUH      TO
                                       RECEMAKES-ALL-NUMBER-OF-SHEETS
               MOVE    SPA-GMN-FILEKBN     TO  RECEMAKES-PERFORM-TITLE
           ELSE
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
      *
               MOVE    1               TO  IDW
               MOVE    IDW             TO  WRK-9
               MOVE    WRK-X           TO
                                   RECEMAKES-SUBMISSION-POSITION (IDW)
      *
               MOVE    ZERO            TO  IDZ
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >       25
                   IF      SPA-NAI-R04-PRTJYUNI (IDX)  =   SPACE
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDZ
                       MOVE    IDX             TO  WRK-9
                       MOVE    WRK-X           TO
                                    RECEMAKES-DETAIL-POSITION(IDW IDZ)
                       MOVE    SPA-GMN-R04-TKENSU      (IDX)
                                               TO
                                   RECEMAKES-COUNT           (IDW IDZ)
                       MOVE    SPA-GMN-R04-TPAGE       (IDX)
                                               TO
                                   RECEMAKES-NUMBER-OF-SHEETS(IDW IDZ)
                       MOVE    SPA-NAI-R04-SYUBETU     (IDX 1)
                                               TO
                                   RECEMAKES-SUBMISSION-CODE (IDW IDZ)
                       MOVE    SPA-GMN-R04-TSYURUINAME (IDX)
                                               TO
                                   RECEMAKES-SUBMISSION-TYPE (IDW IDZ)
                       IF      SPA-NAI-R04-SYUBETU (IDX 1)
                                               =   "0000"
                           MOVE    "99"        TO
                                       RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                       ELSE
                           EVALUATE    SPA-R01-RECEKBN
                               WHEN    "3"
                                   MOVE    "05"        TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                               WHEN    "4"
                                   MOVE    "06"        TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                               WHEN    "5"
                                   MOVE    "07"        TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                               WHEN    "8"
                                   MOVE    "08"        TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                               WHEN    "6"
                                   MOVE    "09"        TO
                                           RECEMAKES-SUBMISSION-IDENT
                                                             (IDW IDZ)
                           END-EVALUATE
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    SPA-GMN-R04-PRTJYUNMEI
                                           TO
                                       RECEMAKES-RECEIPT-PRINT-ORDER (1)
               MOVE    SPA-GMN-R04-PAGEH   TO
                                       RECEMAKES-ALL-COUNT
               MOVE    SPA-GMN-R04-KENSUH  TO
                                       RECEMAKES-ALL-NUMBER-OF-SHEETS
               MOVE    SPA-GMN-R04-FILEKBN TO  RECEMAKES-PERFORM-TITLE
           END-IF
           .
       3101-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成キャンセル処理
      *****************************************************************
       2004-RECEIPT-CANCEL-SEC     SECTION.
      *
      *    中止フラグのセット
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
      *        パラメータとカルテUID+オルカUID をチェック
               IF    ( RECEMAKEQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID )
               OR    ( RECEMAKEQ-ORCA-UID
                                   NOT =   JOB-ORCA-UID  )
                   MOVE    "E41"           TO  WRK-ERRCD
                   GO  TO  2004-RECEIPT-CANCEL-EXT
               END-IF
      *
               IF      JOB-ENDYMD      =   SPACE
      *            ジョブ管理終了処理
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    1               TO  JOB-STOPFLG
                   PERFORM 900-CALL-ORCSJOB-SEC
      *            ジョブ管理終了処理
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    1               TO  JOB-STOPFLG
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
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
           MOVE    WRK-CONS-JOB-SHELLID
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
      *
      *    入院の処理のとき定期請求のチェックをする
           IF    ( WRK-ERRCD       =   SPACE   )
           AND   ( SPA-R01-NYUGAIKBN   =   "1" )
               INITIALIZE                  STEIKIJOB-AREA
               MOVE    "1"             TO  STEIKIJOB-RECEKBN
               MOVE    SPA-R01-NYUGAIKBN
                                       TO  STEIKIJOB-NYUGAIKBN
               IF    ( SPA-R01-FILEKBN     =   "1" )
                   MOVE    "1"         TO  STEIKIJOB-SYOKBN
                   MOVE    SPA-R01-SRYYM   TO  STEIKIJOB-SRYYM
               ELSE
                   MOVE    "2"         TO  STEIKIJOB-SYOKBN
               END-IF
      *
               CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                                   SPA-AREA
               IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    "E70"               TO  WRK-ERRCD
                   MOVE    STEIKIJOB-ERRMSG    TO  WRK-ERRMSG
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       =   SPACE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID2
                                       TO  JOB-SHELLID
               PERFORM 900-CALL-ORCSJOB-SEC
               IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "E70"           TO  WRK-ERRCD
                       STRING  "レセプト印刷処理中です【"
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
      *    レセプト一括作成処理
      *****************************************************************
       800-ORCGR03-CALL-SEC                SECTION.
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R03         TO  SPA-FREE
      *
           CALL    "ORCGR03"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R03
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGR03-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト個別作成処理
      *****************************************************************
       800-ORCGR01-CALL-SEC                SECTION.
     *
               MOVE    SPA-R01KYOUTU   TO  SPA-WORK
               MOVE    SPA-AREA        TO  SPA-COMMON
      *
      *        画面ＳＰＡ書込み
               PERFORM 000-R01SPA-OUT-SEC
      *
               CALL    "ORCGR01"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
      *        画面ＳＰＡ読込み
               PERFORM 000-R01SPA-SET-SEC
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGR01-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-R01SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-R01-AREA
      *
           MOVE    "SPAGMN"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-SEL-SEC
      *
           MOVE    "SPANAI"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-SEL-SEC
      *
      *    一時保存テーブル削除処理
           INITIALIZE                      SPATMP-REC
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           PERFORM 900-SPATMP-DBDELETE-SEC
           .
       000-R01SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-SPATMP-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SPASCR
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI         TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SPATMP-READ-SEC
               IF      FLG-SPATMP          =   ZERO
                   IF      WRK-FILEMEI         =   "SPAGMN"
                       MOVE    SPATMP-DATA-REC     TO   SPA-R01-GMN
                   ELSE
                       MOVE    SPATMP-DATA-REC     TO   SPA-R01-NAI
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBCLOSE-SEC
           .
       000-SPATMP-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-R01SPA-OUT-SEC             SECTION.
      *
           MOVE    "SPAGMN"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-OUT-SEC
      *
           MOVE    "SPANAI"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-OUT-SEC
      *
           .
       000-R01SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-SPATMP-OUT-SEC                SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI         TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           IF      WRK-FILEMEI         =   "SPAGMN"
               MOVE    SPA-R01-GMN         TO  SPATMP-DATA-REC
           ELSE
               MOVE    SPA-R01-NAI         TO  SPATMP-DATA-REC
           END-IF
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "R01 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       000-SPATMP-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-R02SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  FLG-SPASCR
      *??  MOVE    ZERO                TO  IDX
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
           INITIALIZE                      SPATMP-REC
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           PERFORM 900-SPATMP-DBDELETE-SEC
           .
       000-R02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ディレクトリ作成処理
      *****************************************************************
       800-TEMPDIR-SEC             SECTION.
      *
           MOVE    SPA-TERMID      TO  WRK-SPA-TERMID
      *
      *    一時ディレクトリのルートディレクトリ取得
           ACCEPT  WRK-TEMPDIR-ROOT
                               FROM    ENVIRONMENT "MCP_TEMPDIR_ROOT"
      *    一時ディレクトリ名組み立て
           STRING  WRK-TEMPDIR-ROOT    DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   SPA-TERMID          DELIMITED   BY  SPACE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO        MCP-TEMPDIR
           END-STRING
      *
      *    環境変数［MCP_TEMPDIR］を生成（MCP-TEMPDIRの値を設定）
           SET ENVIRONMENT "MCP_TEMPDIR"
                                   TO  MCP-TEMPDIR
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
               WHEN    "E10"
                   MOVE    "患者番号に該当する患者が存在しません。"
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
