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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORAPIRELR1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者お薬手帳情報 (xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/07/07    NACL−伊藤　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *  お薬手帳データ　日レセ　→　スマホ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-MNOTE-H             PIC 9(01).
           03  FLG-MNOTE-B             PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
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
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-XML                 PIC X(30000).
           03  WRK-XML-LEN             PIC 9(05).
           03  WRK-XML-P               PIC 9(05).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPOKUSURIGETV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPOKUSURIGETV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  MNOTE-H-REC.
           COPY    "CPMNOTE-H.INC".
      *
       01  MNOTE-B-REC.
           COPY    "CPMNOTE-B.INC".
      *
           COPY    "MCPDATA.INC".
      *
      **   COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
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
           DISPLAY   "* okusuriget start   *"
           DISPLAY   "**********************"
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST28-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-MNOTEINFMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "* okusuriget end     *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  WRK-XML-AREA
           INITIALIZE  XML-OKUSURIGETRES
      *    更新日取得
           INITIALIZE  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO
                                       OKUSURIGETRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO
                                       OKUSURIGETRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"   USING   MCPAREA
                                           SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "99"                TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE  SYS-1010-REC
           INITIALIZE  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"    USING   MCPAREA
                                           SPA-AREA
                                           ORCSCOMMONAREA
                                           SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "89"                TO  WRK-ERRCD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-MNOTEINFREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  OKUSURIGETRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST28-CLASS
               WHEN    "01"
               WHEN    "02"
      *        お薬手帳
                   MOVE    1                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    APILST28-CLASS
               WHEN    "01"
      *            お薬手帳管理情報処理
                   PERFORM 2002-MNOTE-KANRI-SEC
               WHEN    "02"
      *            お薬手帳情報処理
                   PERFORM 2002-MNOTE-DATA-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-MNOTEINFREAD-SEC       SECTION.
      *
           IF      APILST28-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_okusurigetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST28-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "okusurigetv2req"   TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           XML-APIREQ
                                           SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1)
               MOVE    XML-APIREQ          TO  XML-OKUSURIGETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE    "98"                TO  WRK-ERRCD
           END-IF
           .
       1000-MNOTEINFREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-MNOTEINFMAKE-SEC        SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                       TO  OKUSURIGETRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  OKUSURIGETRES-API-RESULT
               MOVE    WRK-ERRMSG      TO  OKUSURIGETRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST28-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_okusurigetv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  OKUSURIGETRES-MODE
           MOVE    "okusurigetv2res"   TO  OKUSURIGETRES-RECORDNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           XML-OKUSURIGETRES
                                           SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1)
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    OKUSURIGETRES-OBJECT
                                           TO  APILST28-BODY 
               MOVE    "application/xml"   TO  APILST28-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
               DISPLAY "okusuriget xml write failure "
           END-IF
           .
       300-MNOTEINFMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      OKUSURIGETREQ-PATIENTID
                                       =   LOW-VALUE
               MOVE    SPACE               TO  OKUSURIGETREQ-PATIENTID
           END-IF
           IF      OKUSURIGETREQ-RENNUM
                                       =   LOW-VALUE
               MOVE    SPACE               TO  OKUSURIGETREQ-RENNUM
           END-IF
           IF      OKUSURIGETREQ-TBL-GROUP
                                       =   LOW-VALUE
               MOVE    SPACE               TO  OKUSURIGETREQ-TBL-GROUP
           END-IF
           IF      OKUSURIGETREQ-TBL-UUID
                                       =   LOW-VALUE
               MOVE    SPACE               TO  OKUSURIGETREQ-TBL-UUID
           END-IF
           IF      OKUSURIGETREQ-SHORI-RENNUM
                                       =   LOW-VALUE
               MOVE    SPACE               TO
                                           OKUSURIGETREQ-SHORI-RENNUM
           END-IF
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC          SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC         SECTION.
      *
      *    患者番号
           MOVE    OKUSURIGETREQ-PATIENTID
                                       TO  WRK-PTNUM
      *
      *    必須入力チェック
      *    患者番号
           IF     (WRK-PTNUM           =   SPACE)
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC         SECTION.
      *
      *    患者番号取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO  TO  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           ELSE
      *        患者情報検索
               PERFORM 200212-PTINF-HEN-SEC
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳管理情報編集処理
      *****************************************************************
       2002-MNOTE-KANRI-SEC        SECTION.
      *
           PERFORM 900-MNOTE-H-KEY8-SEL-SEC
           PERFORM 900-MNOTE-H-KEY8-FET-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (FLG-MNOTE-H NOT =   ZERO)   OR
                          (IDX     >   10)
      *
               MOVE    MNOTE-H-RENNUM
                                       TO  OKUSURIGETRES-RENNUM (IDX)
               MOVE    MNOTE-H-TBL-GROUP
                                       TO  OKUSURIGETRES-TBL-GROUP (IDX)
               MOVE    MNOTE-H-TBL-UUID
                                       TO  OKUSURIGETRES-TBL-UUID (IDX)
               MOVE    MNOTE-H-SHORI-RENNUM
                                   TO  OKUSURIGETRES-SHORI-RENNUM (IDX)
               MOVE    MNOTE-H-SRYYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE
                                   TO  OKUSURIGETRES-PERFORM-DATE (IDX)
      *
               PERFORM 900-MNOTE-H-KEY8-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2002-MNOTE-KANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳情報編集処理
      *****************************************************************
       2002-MNOTE-DATA-SEC         SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-XML
           MOVE    ZERO                TO  WRK-XML-LEN
           MOVE    1                   TO  WRK-XML-P
      *
           PERFORM 900-MNOTE-H-KEY-SEL-SEC
           PERFORM 900-MNOTE-B-KEY2-SEL-SEC
           PERFORM 900-MNOTE-B-KEY2-FET-SEC
      *
           PERFORM UNTIL ( FLG-MNOTE-B NOT =   ZERO )
      *
               MOVE    FUNCTION    BYTE-LENGTH (
                       FUNCTION    TRIM ( MNOTE-B-CSVDATA ))
                                           TO  WRK-XML-LEN
               MOVE    MNOTE-B-CSVDATA     TO  WRK-XML (WRK-XML-P:)
               COMPUTE WRK-XML-P           =   WRK-XML-P
                                           +   WRK-XML-LEN
      *
               PERFORM 900-MNOTE-B-KEY2-FET-SEC
      *
               IF      FLG-MNOTE-B         =   ZERO
                   MOVE    X"0d0a"             TO  WRK-XML (WRK-XML-P:)
                   COMPUTE WRK-XML-P       =   WRK-XML-P
                                           +   2
               ELSE
                   MOVE    X"0a"           TO  WRK-XML (WRK-XML-P:)
               END-IF
           END-PERFORM
      *
           MOVE    WRK-XML             TO  OKUSURIGETRES-CSV-DATA
      *
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2002-MNOTE-DATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       200212-PTINF-HEN-SEC        SECTION.
      *
           MOVE    WRK-PTNUM           TO  OKUSURIGETRES-PTNUM
           MOVE    PTINF-NAME          TO  OKUSURIGETRES-NAME
           MOVE    PTINF-KANANAME      TO  OKUSURIGETRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  OKUSURIGETRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  OKUSURIGETRES-SEX
           .
       200212-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC            SECTION.
      *
           INITIALIZE                      WRK-HEN-DATE
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
           IF      WRK-TSS             =   SPACE
               MOVE    ZERO                TO  WRK-TSS
           END-IF
           INITIALIZE                      WRK-HEN-TIME
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
       802-DAYHEN02-SEC            SECTION.
      *
           INITIALIZE                      WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                      WRK-THMS
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
       890-ERRCD-MSG-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
               WHEN    "01"
                   MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
               WHEN    "10"
                   MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
      *共通エラー
               WHEN    "89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                                   "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "90"
                   MOVE    "他端末使用中"      TO  WRK-ERRMSG
               WHEN    "91"
                   MOVE    "処理区分未設定"    TO  WRK-ERRMSG
               WHEN    "97"
                   MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
               WHEN    "98"
                   MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
               WHEN    "99"
                   MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳管理検索処理
      *****************************************************************
       900-MNOTE-H-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE  MNOTE-H-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    "MNOTE"             TO  MNOTE-H-TBL-KEY
           MOVE    OKUSURIGETREQ-RENNUM
                                       TO  MNOTE-H-RENNUM
           MOVE    OKUSURIGETREQ-TBL-GROUP
                                       TO  MNOTE-H-TBL-GROUP
           MOVE    OKUSURIGETREQ-TBL-UUID
                                       TO  MNOTE-H-TBL-UUID
           MOVE    OKUSURIGETREQ-SHORI-RENNUM
                                       TO  MNOTE-H-SHORI-RENNUM
      *
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MNOTE-H-REC
           ELSE
               MOVE    1                   TO  FLG-MNOTE-H
               INITIALIZE  MNOTE-H-REC
           END-IF
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-MNOTE-H-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳管理検索処理
      *****************************************************************
       900-MNOTE-H-KEY8-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE  MNOTE-H-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    "MNOTE"             TO  MNOTE-H-TBL-KEY
           MOVE    SPA-PTID            TO  MNOTE-H-PTID
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MNOTE-H-REC
           ELSE
               MOVE    1                   TO  FLG-MNOTE-H
               INITIALIZE  MNOTE-H-REC
           END-IF
           .
       900-MNOTE-H-KEY8-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳管理FETCH処理
      *****************************************************************
       900-MNOTE-H-KEY8-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MNOTE-H-REC
           ELSE
               MOVE    1                   TO  FLG-MNOTE-H
               INITIALIZE                      MNOTE-H-REC
           END-IF
           .
       900-MNOTE-H-KEY8-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳データ検索処理
      *****************************************************************
       900-MNOTE-B-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-B
      *
           INITIALIZE  MNOTE-B-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
           MOVE    MNOTE-H-TBL-KEY     TO  MNOTE-B-TBL-KEY
           MOVE    MNOTE-H-RENNUM      TO  MNOTE-B-RENNUM
           MOVE    MNOTE-H-TBL-GROUP   TO  MNOTE-B-TBL-GROUP
           MOVE    MNOTE-H-TBL-UUID    TO  MNOTE-B-TBL-UUID
           MOVE    MNOTE-H-SHORI-RENNUM
                                       TO  MNOTE-B-SHORI-RENNUM
           MOVE    MNOTE-B-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MNOTE-B-REC
           ELSE
               MOVE    1                   TO  FLG-MNOTE-B
               INITIALIZE  MNOTE-B-REC
           END-IF
           .
       900-MNOTE-B-KEY2-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳データFETCH処理
      *****************************************************************
       900-MNOTE-B-KEY2-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-B
      *
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MNOTE-B-REC
           ELSE
               MOVE    1                   TO  FLG-MNOTE-B
               INITIALIZE  MNOTE-B-REC
           END-IF
           .
       900-MNOTE-B-KEY2-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
