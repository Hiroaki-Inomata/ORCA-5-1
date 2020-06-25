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
       PROGRAM-ID.                 ORAPIRELR2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者お薬手帳情報 (xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/08/11    NACL−伊藤　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *  お薬手帳データ　スマホ　→　日レセ
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
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CUT                 PIC 9(01).
           03  FLG-DATA-KEY            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IX-D                    PIC 9(06).
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
      *    お薬手帳データ編集
           03  WRK-CSV-DATA            PIC X(30000).
           03  WRK-CSV-DATA-R          REDEFINES   WRK-CSV-DATA.
               05  WRK-CSV-X           PIC X(01)   OCCURS 30000.
           03  WRK-ST                  PIC 9(06).
           03  WRK-LEN                 PIC 9(06).
           03  WRK-REC                 PIC X(1000).
           03  WRK-STR                 PIC X(10).
           03  WRK-ELE-C               PIC 9(02).
           03  WRK-RP                  PIC 9(03).
           03  WRK-RP-SEQ              PIC 9(03).
           03  WRK-DATA-KEY            PIC 9(16).
           03  WRK-DATA-KEY-R          REDEFINES   WRK-DATA-KEY.
               05  WRK-DATA-KEY-1      PIC X(16).
           03  WRK-RENNUM              PIC 9(03).
      *
      *01  CNT-AREA.
      *    03  CNT-STR                 PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-DATA-MAX          PIC 9(06)  VALUE  30000.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    CSV分割サブ
           COPY    "CPORCSDIVCSV.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPOKUSURIPUTV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPOKUSURIPUTV2RES.INC".
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
      *    お薬手帳データ登録
       01  OKUSURI-KANRI-REC.
           COPY    "CPOKUSURIKANRI.INC".
       01  OKUSURI-DATA-REC.
           COPY    "CPOKUSURIDATA.INC".
       01  OKUSURI-KANRI-SAV-REC.
           COPY    "CPOKUSURIKANRI.INC"    REPLACING  //OKUKAN-//
                                           BY         //OKUSAV-//.
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
           DISPLAY   "* okusuriput start   *"
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
               MOVE   404                  TO  APILST29-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-OKUSURIPUTMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "* okusuriput end     *"
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
           INITIALIZE  XML-OKUSURIPUTRES
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
                                       OKUSURIPUTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO
                                       OKUSURIPUTRES-INFORMATION-TIME
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
           PERFORM 2001-OKUSURIPUTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  OKUSURIPUTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST29-CLASS
               WHEN    "01"
      *        新規登録
                   MOVE    1                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    APILST29-CLASS
               WHEN    "01"
      *            お薬手帳情報登録処理
                   PERFORM 400-OKUSURI-TOROKU-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       2001-OKUSURIPUTREAD-SEC     SECTION.
      *
           IF      APILST29-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_okusuriputv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST29-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "okusuriputv2req"   TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           XML-APIREQ
                                           SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1)
               MOVE    XML-APIREQ          TO  XML-OKUSURIPUTREQ
               PERFORM 20011-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE    "98"                TO  WRK-ERRCD
           END-IF
           .
       2001-OKUSURIPUTREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       20011-XML-REMAKE-SEC        SECTION.
      *
           IF      OKUSURIPUTREQ-CSV-DATA
                                       =   LOW-VALUE
               MOVE    SPACE               TO  OKUSURIPUTREQ-CSV-DATA
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20011-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-OKUSURIPUTMAKE-SEC      SECTION.
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
                                       TO  OKUSURIPUTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  OKUSURIPUTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO
                                           OKUSURIPUTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST29-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_okusuriputv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  OKUSURIPUTRES-MODE
           MOVE    "okusuriputv2res"   TO  OKUSURIPUTRES-RECORDNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           XML-OKUSURIPUTRES
                                           SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1)
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    OKUSURIPUTRES-OBJECT
                                           TO  APILST29-BODY 
               MOVE    "application/xml"   TO  APILST29-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
               DISPLAY "okusuriput xml write failure "
           END-IF
           .
       300-OKUSURIPUTMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳情報登録処理
      *****************************************************************
       400-OKUSURI-TOROKU-SEC      SECTION.
      *
           IF      OKUSURIPUTREQ-CSV-DATA
                                   NOT =   SPACE
               MOVE    OKUSURIPUTREQ-CSV-DATA
                                           TO  WRK-CSV-DATA
               PERFORM 4001-OKUSURI-KANRI-INS-SEC
           END-IF
           .
       400-OKUSURI-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳管理追加処理
      *****************************************************************
       4001-OKUSURI-KANRI-INS-SEC  SECTION.
      *
           PERFORM 40011-DATA-KEY-SEC
           PERFORM 4002-CSV-RECORD-SEC
      *
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    OKUSURI-KANRI-REC   TO  OKUSURI-KANRI-SAV-REC
      *
      *    RENNUM決定
           PERFORM 40012-RENNUM-SEC
      *
           MOVE    OKUSURI-KANRI-SAV-REC
                                       TO  OKUSURI-KANRI-REC
      *
           MOVE    WRK-RENNUM          TO  OKUKAN-RENNUM
           MOVE    WRK-DATA-KEY        TO  OKUKAN-DATA-KEY
      *
           MOVE    WRK-SYMD            TO  OKUKAN-CREYMD
           MOVE    WRK-SYMD            TO  OKUKAN-UPYMD
           MOVE    WRK-THMS            TO  OKUKAN-UPHMS
      *
           MOVE    OKUSURI-KANRI-REC   TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API OKUSURI-KANRI INS ERR:" MCP-RC
                           ",KEY:"   OKUKAN-KEY    "."
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
           .
       4001-OKUSURI-KANRI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    データキー決定処理
      *****************************************************************
       40011-DATA-KEY-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-DATA-KEY
      *
           PERFORM UNTIL   FLG-DATA-KEY    =   1
               MOVE    FUNCTION CURRENT-DATE
                                           TO  WRK-DATA-KEY-1
               INITIALIZE  OKUSURI-DATA-REC
               MOVE    WRK-DATA-KEY        TO  OKUDAT-KEY
               MOVE    OKUSURI-DATA-REC    TO  MCPDATA-REC
               MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC         TO  OKUSURI-DATA-REC
                       IF      OKUDAT-DATA-KEY NOT =   WRK-DATA-KEY
                           MOVE    1                   TO  FLG-DATA-KEY
                       ELSE
                           CONTINUE
      *                    ここで遅延を入れるか(sleep)
                       END-IF
                   ELSE
                       MOVE    1                   TO  FLG-DATA-KEY
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-DATA-KEY
               END-IF
               MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
           .
       40011-DATA-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    連番決定処理
      *****************************************************************
       40012-RENNUM-SEC            SECTION.
      *
           MOVE    OKUSURI-KANRI-REC   TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  OKUSURI-KANRI-REC
                   COMPUTE WRK-RENNUM          =   OKUKAN-RENNUM
                                               +   1
               ELSE
                   MOVE    1                   TO  WRK-RENNUM
               END-IF
           ELSE
               MOVE    1                   TO  WRK-RENNUM
           END-IF
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40012-RENNUM-EXT.
           EXIT.
      *****************************************************************
      *    CSV-DATA 分割処理
      *****************************************************************
       4002-CSV-RECORD-SEC         SECTION.
      *
      *    レコード数を求める
      *     MOVE    ZERO                TO  CNT-STR
      *     INSPECT WRK-CSV-DATA    TALLYING   CNT-STR
      *             FOR     ALL X"0A"
      *     IF    ( CNT-STR     >   ZERO )
      *         DISPLAY "CNT-STR [ " CNT-STR " ]"
      *         COMPUTE CNT-STR             =   CNT-STR +   1
      *     END-IF
      *
      *    改行文字までのデータを切り出し
           MOVE    ZERO                TO  FLG-CUT
           MOVE    1                   TO  WRK-ST
           PERFORM UNTIL   FLG-CUT =   1
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-LEN
               PERFORM VARYING IX-D
                               FROM    WRK-ST
                               BY      1
                       UNTIL  (IX-D        >   CONST-DATA-MAX)
                           OR (WRK-CSV-X (IX-D)
                                           =   X"0A"
                                           OR  LOW-VALUE)
                   COMPUTE WRK-LEN             =   WRK-LEN +   1
               END-PERFORM
               MOVE    WRK-CSV-DATA (WRK-ST:WRK-LEN)
                                       TO  WRK-REC
               IF      WRK-REC             =   "JAHISTC01"
                   CONTINUE
               ELSE
                   PERFORM 40021-REC-DIVIDE-SEC
               END-IF
      *
               COMPUTE WRK-ST              =   IX-D    +   1
               IF      WRK-CSV-X (WRK-ST)  =   LOW-VALUE
                                           OR  SPACE
                   MOVE    1                   TO  FLG-CUT
               END-IF
           END-PERFORM
           .
       4002-CSV-RECORD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV-DATA 分割処理
      *****************************************************************
       40021-REC-DIVIDE-SEC        SECTION.
      *
      *    要素数を求める
           STRING  WRK-REC         DELIMITED   BY  ","
               INTO    WRK-STR
           END-STRING
           EVALUATE    WRK-STR
               WHEN    "1"
                   MOVE    4                   TO  WRK-ELE-C
               WHEN    "5"
               WHEN    "15"
                   MOVE    2                   TO  WRK-ELE-C
               WHEN    "11"
                   MOVE    5                   TO  WRK-ELE-C
               WHEN    "201"
                   MOVE    7                   TO  WRK-ELE-C
               WHEN    "281"
                   MOVE    3                   TO  WRK-ELE-C
               WHEN    "301"
                   MOVE    8                   TO  WRK-ELE-C
               WHEN    OTHER
                   MOVE    10                  TO  WRK-ELE-C
           END-EVALUATE
      *
           INITIALIZE  ORCSDIVCSV-AREA
           MOVE    WRK-REC             TO  ORCSDIVCSV-REC
           MOVE    WRK-ELE-C           TO  ORCSDIVCSV-ELE-C
           MOVE    ","                 TO  ORCSDIVCSV-DELI
           MOVE    ZERO                TO  ORCSDIVCSV-DQ
           CALL    "ORCSDIVCSV"     USING  ORCSDIVCSV-AREA
           IF      ORCSDIVCSV-RC       =   ZERO
                PERFORM 40022-SET-VALUE-SEC
           END-IF
           .
       40021-REC-DIVIDE-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV-DATA 分割処理
      *****************************************************************
       40022-SET-VALUE-SEC         SECTION.
      *
           EVALUATE    ORCSDIVCSV-D (1)
               WHEN    "1"
                   MOVE    ORCSDIVCSV-D (2)    TO  OKUKAN-NAME
                   MOVE    ORCSDIVCSV-D (3)    TO  OKUKAN-SEX
                   MOVE    ORCSDIVCSV-D (4)    TO  OKUKAN-BIRTHDAY
               WHEN    "5"
                   MOVE    ORCSDIVCSV-D (2)    TO  OKUKAN-SRYYMD
      *         WHEN    "15"
      *             MOVE    2                   TO  WRK-ELE-C
               WHEN    "11"
                   MOVE    ORCSDIVCSV-D (2)    TO  OKUKAN-HOSPNAME
                   MOVE    ORCSDIVCSV-D (5)    TO  OKUKAN-HOSPCD
               WHEN    "201"
                   PERFORM 4003-OKUSURI-DATA-INS-SEC
      *         WHEN    "281"
      *             MOVE    3                   TO  WRK-ELE-C
               WHEN    "301"
                   PERFORM 4003-OKUSURI-DATA-INS-SEC
      *         WHEN    OTHER
      *             MOVE    10                  TO  WRK-ELE-C
           END-EVALUATE
           .
       40022-SET-VALUE-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳データ追加処理
      *****************************************************************
       4003-OKUSURI-DATA-INS-SEC   SECTION.
      *
           INITIALIZE  OKUSURI-DATA-REC
      *
           MOVE    WRK-DATA-KEY        TO  OKUDAT-KEY
      *
           INITIALIZE  ORCSNUMAREA
           MOVE    ORCSDIVCSV-D (2)    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               CONTINUE
           ELSE
               DISPLAY "RP error!"
           END-IF
           IF      WRK-RP          NOT =   SNUM-OUTNUM
               MOVE    SNUM-OUTNUM         TO  WRK-RP
               MOVE    ZERO                TO  WRK-RP-SEQ
           END-IF
           MOVE    WRK-RP              TO  OKUDAT-RP
           COMPUTE WRK-RP-SEQ          =   WRK-RP-SEQ  +   1
           MOVE    WRK-RP-SEQ          TO  OKUDAT-RP-SEQ
      *
           INITIALIZE  ORCSNUMAREA
           MOVE    ORCSDIVCSV-D (1)    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               CONTINUE
           ELSE
               DISPLAY "RP error!"
           END-IF
           MOVE    SNUM-OUTNUM         TO  OKUDAT-DATA-CLASS
      *
           EVALUATE    ORCSDIVCSV-D (1)
               WHEN    "201"
               WHEN    "301"
                   MOVE    ORCSDIVCSV-D (3)    TO  OKUDAT-NAME
                   INITIALIZE  ORCSNUMAREA
                   MOVE    ORCSDIVCSV-D (4)    TO  SNUM-INX
                   CALL    "ORCSNUM"       USING   ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                       CONTINUE
                   ELSE
                       DISPLAY "RP error!"
                   END-IF
                   MOVE    SNUM-OUTNUM         TO  OKUDAT-SURYO
                   MOVE    ORCSDIVCSV-D (5)    TO  OKUDAT-TANINAME
                   INITIALIZE  ORCSNUMAREA
                   MOVE    ORCSDIVCSV-D (6)    TO  SNUM-INX
                   CALL    "ORCSNUM"       USING   ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                       CONTINUE
                   ELSE
                       DISPLAY "RP error!"
                   END-IF
                   MOVE    SNUM-OUTNUM         TO  OKUDAT-CODE-CLASS
                   MOVE    ORCSDIVCSV-D (7)    TO  OKUDAT-CODE
      *         WHEN    "281"
               WHEN    OTHER
                   GO  TO  4003-OKUSURI-DATA-INS-EXT
           END-EVALUATE
      *
           MOVE    WRK-SYMD            TO  OKUDAT-CREYMD
           MOVE    WRK-SYMD            TO  OKUDAT-UPYMD
           MOVE    WRK-THMS            TO  OKUDAT-UPHMS
      *
           MOVE    OKUSURI-DATA-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API OKUSURI-DATA INS ERR:" MCP-RC
                           ",KEY:"   OKUDAT-KEY    "."
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
           .
       4003-OKUSURI-DATA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC            SECTION.
      *
           INITIALIZE  WRK-HEN-DATE
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
           INITIALIZE  WRK-HEN-TIME
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
               WHEN    "01"
                   MOVE    "お薬手帳データがありません"
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
