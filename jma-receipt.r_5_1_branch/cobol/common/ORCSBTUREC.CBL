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
       PROGRAM-ID.                 ORCSBTUREC.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : レセ電用病床機能報告データ取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/05/24    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TENSUWRK                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-SET                     PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-SRYKBN90                PIC 9(01).
           03  FLG-SRYKBN92                PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  IDX-SYS                     PIC 9(04).
           03  IDX-ST                      PIC 9(04).
           03  IDX-ED                      PIC 9(04).
           03  IDX-SYUHKN                  PIC 9(04).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    入院診療計画未実施減算
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    院内感染防止対策未実施減算
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    医療安全管理体制未整備減算
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    褥瘡対策未実施減算
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    日常生活障害加算
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    認知症加算
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    褥瘡患者管理加算
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *    特定抗精神病薬治療管理加算
           03  CONS-TOKUSEIYAKU-KSN        PIC X(09) VALUE "190117570".
      *    その他診療行為で入力した入院料加算
           03  CONS-ETCNYUIN-TBL.
             05  CONS-ETCNYUIN-KSN         PIC X(09)  OCCURS 10.
      *    点数マスタ読み込みワーク用
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
           03  WRK-NYUKHNKBN-XXX           PIC 9(03).
      *
      *
      *
      *    同日再入院処理用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    システム管理病棟情報ワーク
           03  WRK-SYSBTU-TABLE.
               05  WRK-SYSBTU-TBL          OCCURS  100.
                   07  WRK-SYSBTU-KBNCD    PIC X(02).
                   07  WRK-SYSBTU-SRYCD    PIC X(09).
      *    システム管理病棟情報ワーク（特定入院料）
           03  WRK-SYSBTU-TOKU-TABLE.
               05  WRK-SYSBTU-TOKU-TBL     OCCURS  100.
                   07  WRK-SYSBTU-TKBNCD   PIC X(02).
                   07  WRK-SYSBTU-TSRYCD   PIC X(09).
      *    システム管理病室情報ワーク（特定入院料）
           03  WRK-SYSBRM-TABLE.
               05  WRK-SYSBRM-TBL          OCCURS  500.
                   07  WRK-SYSBRM-KBNCD    PIC X(08).
                   07  WRK-SYSBRM-TSRYCD   PIC X(09).
      *
      *    保険組合せ退避領域
           03  WRK-SYUHKN-TBL.
             04  WRK-SYUHKN-OCC            OCCURS  31.
               05  WRK-SYUHKN              PIC 9(01).
               05  WRK-SYUHKN-HKNCOMBI     PIC 9(03).
      *
      *    返却項目編集ワーク
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-HKNSYU-DAY      PIC 9(01).
                   07  WRK-BTUNUM          PIC X(02).
                   07  WRK-BRMNUM          PIC X(08).
      *            特定入院料区分（１：医療機関／２：病棟／３：病室）
                   07  WRK-TOKUTEI-KBN     PIC X(01).
      *            特定入院料
                   07  WRK-TOKUTEI-NYUIN   PIC X(02).
      *            算定要件外区分
                   07  WRK-TOKU-SANTEIYOUKENKBN
                                           PIC X(01).
      *            入院料算定（１：入院基本料／２：特定入院料）
                   07  WRK-SANTEI-KBN      PIC X(01).
                   07  WRK-SRYSKB          PIC X(02).
                   07  WRK-BTUCD           PIC X(09).
      *
      *
      *    診療コード計集計用
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *    開始日　終了日取得用
           03  WRK-YMDX                    PIC X(08).
           03  WRK-YMD9    REDEFINES       WRK-YMDX.
               05   WRK-YY9                PIC 9(04).
               05   WRK-MM9                PIC 9(02).
               05   WRK-DD9                PIC 9(02).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報マスタ−
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者保険情報マスタ−
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    入院会計作成用パラメタ
           COPY    "CPORCSBTUREC.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSBTUREC-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-BTUREC-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-BTUREC-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *
      *    主処理
           IF      LNK-BTUREC-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *     DISPLAY  "LNK-BTUREC-RC = " LNK-BTUREC-RC
      *     IF      LNK-BTUREC-RC    =   ZERO
      *         PERFORM    VARYING  IDX   FROM 1  BY  1
      *           UNTIL    IDX  >  31
      *              DISPLAY "IDX = " IDX
      *              DISPLAY "LNK-BTUREC-HKNCOMBI="
      *                       LNK-BTUREC-HKNCOMBI(IDX)
      *              DISPLAY "LNK-BTUREC-SRYSKB="
      *                       LNK-BTUREC-SRYSKB(IDX)
      *              DISPLAY "LNK-BTUREC-BTUCD="
      *                       LNK-BTUREC-BTUCD(IDX)
      *         END-PERFORM
      *         DISPLAY   "LNK-BTUREC-SYU-HKNCOMBI-MEISAI = "
      *                    LNK-BTUREC-SYU-HKNCOMBI-MEISAI
      *         PERFORM    VARYING  IDX   FROM 1  BY  1
      *           UNTIL    IDX  >  31
      *              DISPLAY "IDX = " IDX
      *              DISPLAY "LNK-BTUREC-SYU-HKNCOMBI(1)="
      *                       LNK-BTUREC-SYU-HKNCOMBI(1 IDX)
      *         END-PERFORM
      *         PERFORM    VARYING  IDX   FROM 1  BY  1
      *           UNTIL    IDX  >  31
      *              DISPLAY "IDX = " IDX
      *              DISPLAY "LNK-BTUREC-SYU-HKNCOMBI(2)="
      *                       LNK-BTUREC-SYU-HKNCOMBI(2 IDX)
      *         END-PERFORM
      *         PERFORM    VARYING  IDX   FROM 1  BY  1
      *           UNTIL    IDX  >  31
      *              DISPLAY "IDX = " IDX
      *              DISPLAY "LNK-BTUREC-SYU-HKNCOMBI(3)="
      *                       LNK-BTUREC-SYU-HKNCOMBI(3 IDX)
      *         END-PERFORM
      *     END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF    ( LNK-BTUREC-SANTEIYM(1:4)  >=  "2016")  AND
                 ( LNK-BTUREC-SANTEIYM(5:2)  =   "06")
               CONTINUE
           ELSE
               MOVE    1               TO  LNK-BTUREC-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-BTUREC-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-BTUREC-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスターの保険組合せ検索）
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-BTUREC-HOSPNUM      TO  NACCT-HOSPNUM
           MOVE    LNK-BTUREC-PTID         TO  NACCT-PTID
           MOVE    LNK-BTUREC-SANTEIYM     TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  LNK-BTUREC-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    3              TO  LNK-BTUREC-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        MOVE   NACCT-DAY (IDX1) TO 
                                            WRK-HKNCOMBI-DAY(IDX1)
                    END-PERFORM
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    医療機関情報の取得（システム管理テーブルより）
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-BTUREC-SANTEIYM TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-BTUREC-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   0
                   MOVE   SYS-1001-HOSPSBT
                                       TO  SPA-HOSPSBT
                   IF     SYS-1001-HOSPSBT   NOT =    1
                       MOVE    5       TO  LNK-BTUREC-RC
                   END-IF
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得（病床機能報告の返却エリア編集）
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    事前に病棟情報（システム管理）から病棟コードを取得しておく
           MOVE    ZERO                TO  IDX-SYS
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "31"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL  (FLG-SYSKANRI   =   1)  OR
                               (IDX-SYS        =   100)
                 IF   SYS-5001-SRYCD   NOT =   SPACE
                    ADD    1           TO  IDX-SYS
                    MOVE   SYS-5001-KBNCD
                                       TO  WRK-SYSBTU-KBNCD(IDX-SYS)
                    MOVE   SYS-5001-SRYCD
                                       TO  WRK-SYSBTU-SRYCD(IDX-SYS)
      **              DISPLAY "IDX-SYS ="  IDX-SYS
      **              DISPLAY "WRK-SYSBTU-KBNCD(IDX-SYS) = "
      **                       WRK-SYSBTU-KBNCD(IDX-SYS)
      **              DISPLAY "WRK-SYSBTU-SRYCD(IDX-SYS) = "
      **                       WRK-SYSBTU-SRYCD(IDX-SYS)
      **              DISPLAY "WRK-SYSBTU-TSRYCD(IDX-SYS) = "
      **                       WRK-SYSBTU-TSRYCD(IDX-SYS)
                 END-IF
                 MOVE    "tbl_syskanri" TO  MCP-TABLE
                 MOVE    "key2"         TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    事前に病棟情報（システム管理）から特定入院料の病棟コードを
      *    取得しておく
           MOVE    ZERO                TO  IDX-SYS
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "31"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL  (FLG-SYSKANRI   =   1)  OR
                               (IDX-SYS        =   100)
                 IF   SYS-5001-TSRYCD   NOT =   SPACE
                    ADD    1           TO  IDX-SYS
                    MOVE   SYS-5001-KBNCD
                                       TO  WRK-SYSBTU-TKBNCD(IDX-SYS)
                    MOVE   SYS-5001-TSRYCD
                                       TO  WRK-SYSBTU-TSRYCD(IDX-SYS)
      **              DISPLAY "IDX-SYS ="  IDX-SYS
      **              DISPLAY "WRK-SYSBTU-TKBNCD(IDX-SYS) = "
      **                       WRK-SYSBTU-TKBNCD(IDX-SYS)
      **              DISPLAY "WRK-SYSBTU-TSRYCD(IDX-SYS) = "
      **                       WRK-SYSBTU-TSRYCD(IDX-SYS)
                 END-IF
                 MOVE    "tbl_syskanri" TO  MCP-TABLE
                 MOVE    "key2"         TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    事前に病室情報（システム管理）から病室コードを取得しておく
           MOVE    ZERO                TO  IDX-SYS
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "31"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL  (FLG-SYSKANRI   =   1)  OR
                               (IDX-SYS        =   500)
                 IF   SYS-5002-TSRYCD  NOT =   SPACE
                    ADD    1           TO  IDX-SYS
                    MOVE   SYS-5002-KBNCD
                                       TO  WRK-SYSBRM-KBNCD(IDX-SYS)
                    MOVE   SYS-5002-TSRYCD
                                       TO  WRK-SYSBRM-TSRYCD(IDX-SYS)
      **              DISPLAY "IDX-SYS ="  IDX-SYS
      **              DISPLAY "WRK-SYSBRM-KBNCD(IDX-SYS) = "
      **                       WRK-SYSBRM-KBNCD(IDX-SYS)
      **              DISPLAY "WRK-SYSBRM-TSRYCD(IDX-SYS) = "
      **                       WRK-SYSBRM-TSRYCD(IDX-SYS)
                 END-IF
                 MOVE    "tbl_syskanri" TO  MCP-TABLE
                 MOVE    "key2"         TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    月末日を取得
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-BTUREC-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
      *
      *    患者がもつ保険組合せから社保、国保、後期の期間を取得
           MOVE    ZERO                TO  IDX-SYUHKN
           INITIALIZE                      HKNCOMBI-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    LNK-BTUREC-PTID     TO  COMB-PTID
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  COMB-TEKSTYMD(1:6)
           MOVE    "31"                TO  COMB-TEKSTYMD(7:2)
           MOVE    LNK-BTUREC-SANTEIYM(1:6)
                                       TO  COMB-TEKEDYMD(1:6)
           MOVE    "01"                TO  COMB-TEKEDYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM   910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =   ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM  910-HKNCOMBI-READ-SEC
               PERFORM  UNTIL  (FLG-HKNCOMBI   =   1)
                IF   COMB-DELKBN       =    SPACE
                  MOVE     COMB-TEKSTYMD   TO    WRK-YMDX
                  MOVE     WRK-DD9         TO  IDX-ST
                  MOVE     COMB-TEKEDYMD   TO  WRK-YMDX
                  MOVE     WRK-DD9         TO  IDX-ED
      *           国保
                  IF     COMB-HKNNUM     = "060" OR "067" OR
                                           "068" OR "069"
                     ADD        1       TO  IDX-SYUHKN
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                  END-IF
      *           後期高齢者
                  IF     COMB-HKNNUM     = "039" OR "040"
                     ADD        1       TO  IDX-SYUHKN
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                  END-IF
      *           社保
                  IF    (COMB-HKNNUM   NOT = "060" AND "067" AND
                                             "068" AND "069" AND
                                             "039" AND "040" AND
                                             "971" AND "973" AND
                                             "975" )   AND
                        (COMB-HKNNUM(1:2)  NOT =    "98"  AND
                                                    "90"  AND
                                                    "91")
                     ADD        1       TO  IDX-SYUHKN
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) = LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  1  BY  1
                           UNTIL    IDX    >   IDX-ED
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                     IF (COMB-TEKSTYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                        (COMB-TEKEDYMD(1:6) > LNK-BTUREC-SANTEIYM)
                         PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                           UNTIL    IDX    >   WRK-GETSUMATSU-DD
                             MOVE   COMB-HKNCOMBINUM
                                          TO   LNK-BTUREC-SYU-HKNCOMBI
                                                       (IDX-SYUHKN IDX)
                         END-PERFORM
                     END-IF
                  END-IF
                END-IF
      *
                MOVE    "tbl_hkncombi" TO  MCP-TABLE
                MOVE    "key6"         TO  MCP-PATHNAME
                PERFORM  910-HKNCOMBI-READ-SEC
               END-PERFORM
           END-IF
      *
      *    保険組合せ管理クローズ
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    患者保険組合せ返却テーブル明細数のセット
           MOVE     IDX-SYUHKN     TO  LNK-BTUREC-SYU-HKNCOMBI-MEISAI
      *
      **     PERFORM   VARYING   IDX   FROM  1  BY  1
      **      UNTIL   (IDX     >   31)
      **         DISPLAY "IDX = " IDX
      **         DISPLAY "WRK-SYUHKN(IDX) = " WRK-SYUHKN(IDX)
      **     END-PERFORM
      *
      *    入院会計の保険組合せ番号から保険種類の取得
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-BTUREC-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-BTUREC-PTID     TO  COMB-PTID
                   MOVE    WRK-HKNCOMBI-DAY(IDX)
                                               TO  COMB-HKNCOMBINUM
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-SELECT-SEC
                   IF      FLG-HKNCOMBI            =   ZERO
                       MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM   910-HKNCOMBI-READ-SEC
                       IF   FLG-HKNCOMBI         =  ZERO
      *                     国保
                            IF     COMB-HKNNUM     = "060" OR "067" OR
                                                     "068" OR "069"
                                 MOVE    2      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
      *                     後期高齢者
                            IF     COMB-HKNNUM     = "039" OR "040"
                                 MOVE    3      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
      *                     労災、自賠責、公害
                            IF     COMB-HKNNUM     = "971" OR "973" OR
                                                     "975"
                                 MOVE    4      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
      *                     自費
                            IF     COMB-HKNNUM(1:2)  =    "98"
                                 MOVE    5      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
      *                     治験
                            IF     COMB-HKNNUM(1:2)  =    "90"  OR  "91"
                                 MOVE    6      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
      *                     社保
                            IF     WRK-HKNSYU-DAY(IDX)  =    ZERO
                                 MOVE    1      TO   WRK-HKNSYU-DAY(IDX)
                            END-IF
                       ELSE
                           MOVE    4             TO  LNK-BTUREC-RC
                       END-IF
      *                保険組合せクローズ
                       MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                       MOVE    "key"             TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                   ELSE
      *                保険組合せクローズ
                       MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                       MOVE    "key"             TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                       MOVE    4                 TO  LNK-BTUREC-RC
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *    入院履歴から対象月の入院履歴を取得
           INITIALIZE                        PTNYUINRRK-REC
           MOVE    LNK-BTUREC-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-BTUREC-PTID       TO  PTNYUINRRK-PTID
           MOVE    LNK-BTUREC-SANTEIYM   TO  PTNYUINRRK-NYUINYMD(1:6)
           MOVE    "31"                  TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    LNK-BTUREC-SANTEIYM   TO  PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "01"                  TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key58"               TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
              MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
              MOVE    "key58"            TO  MCP-PATHNAME
              PERFORM   910-PTNYUINRRK-READ-SEC
              PERFORM   UNTIL   FLG-PTNYUINRRK   =   1
      **          DISPLAY "PTNYUINRRK-RRKNUM = "
      **                   PTNYUINRRK-RRKNUM
      **          DISPLAY "PTNYUINRRK-TENNYUYMD = "
      **                   PTNYUINRRK-TENNYUYMD
      **          DISPLAY "PTNYUINRRK-TENSTUYMD = "
      **                   PTNYUINRRK-TENSTUYMD
                MOVE     PTNYUINRRK-TENNYUYMD TO  WRK-YMDX
                MOVE     WRK-DD9              TO  IDX-ST
                MOVE     PTNYUINRRK-TENSTUYMD TO  WRK-YMDX
                MOVE     WRK-DD9              TO  IDX-ED
                IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                   (PTNYUINRRK-TENSTUYMD(1:6) = LNK-BTUREC-SANTEIYM)
                    PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                      UNTIL    IDX    >   IDX-ED
                      IF   WRK-HKNSYU-DAY(IDX)   NOT =  ZERO
                        PERFORM   210-WRK-SET-SEC
                      END-IF
                    END-PERFORM
                END-IF
                IF (PTNYUINRRK-TENNYUYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                   (PTNYUINRRK-TENSTUYMD(1:6) > LNK-BTUREC-SANTEIYM)
                    PERFORM    VARYING   IDX   FROM  1  BY  1
                      UNTIL    IDX    >   31
                      IF   WRK-HKNSYU-DAY(IDX)   NOT =  ZERO
                        PERFORM   210-WRK-SET-SEC
                      END-IF
                    END-PERFORM
                END-IF
                IF (PTNYUINRRK-TENNYUYMD(1:6) < LNK-BTUREC-SANTEIYM) AND
                   (PTNYUINRRK-TENSTUYMD(1:6) = LNK-BTUREC-SANTEIYM)
                    PERFORM    VARYING   IDX   FROM  1  BY  1
                      UNTIL    IDX    >   IDX-ED
                      IF   WRK-HKNSYU-DAY(IDX)   NOT =  ZERO
                        PERFORM   210-WRK-SET-SEC
                      END-IF
                    END-PERFORM
                END-IF
                IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-BTUREC-SANTEIYM) AND
                   (PTNYUINRRK-TENSTUYMD(1:6) > LNK-BTUREC-SANTEIYM)
                    PERFORM    VARYING   IDX   FROM  IDX-ST  BY  1
                      UNTIL    IDX    >   31
                      IF   WRK-HKNSYU-DAY(IDX)   NOT =  ZERO
                        PERFORM   210-WRK-SET-SEC
                      END-IF
                    END-PERFORM
                END-IF
      *
                MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
                MOVE    "key58"            TO  MCP-PATHNAME
                PERFORM 910-PTNYUINRRK-READ-SEC
              END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key58"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      **     PERFORM   VARYING   IDX   FROM  1  BY  1
      **      UNTIL   (IDX     >   31)
      **         DISPLAY "IDX = " IDX
      **         DISPLAY "WRK-BTUNUM(IDX) = " WRK-BTUNUM(IDX)
      **     END-PERFORM
      *
      *
      *    入院料の取得
           MOVE    ZERO                TO  FLG-SRYKBN90
           MOVE    ZERO                TO  FLG-SRYKBN92
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-BTUREC-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-BTUREC-PTID     TO  NACCT-PTID
           MOVE    LNK-BTUREC-SANTEIYM TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF   NACCT-SRYKBN  =   "90"
                        MOVE   1           TO  FLG-SRYKBN90
                   END-IF
                   IF   NACCT-SRYKBN  =   "92"
                        MOVE   1           TO  FLG-SRYKBN92
                   END-IF
                   PERFORM    VARYING   IDX   FROM  1  BY  1
                     UNTIL    IDX       >    31
                         IF    NACCT-DAY(IDX)  =   1
                             MOVE   NACCT-SRYKBN   TO  WRK-SRYSKB(IDX)
                             EVALUATE   NACCT-SRYKBN
                                 WHEN   "90"
                                     MOVE  "1"  TO  WRK-SANTEI-KBN(IDX)
                                 WHEN   "92"
                                     MOVE  "2"  TO  WRK-SANTEI-KBN(IDX)
                             END-EVALUATE
                         END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院会計のカレンダーがゼロの場合の対処
           PERFORM   VARYING   IDX   FROM  1  BY  1
            UNTIL   (IDX     >   31)
                IF   (WRK-SANTEI-KBN(IDX)     =       SPACE)   AND
                     (WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO)
                    IF  (FLG-SRYKBN90    =   1)     AND
                        (FLG-SRYKBN92    =   ZERO)
                          MOVE   "90"       TO  WRK-SRYSKB(IDX)
                          MOVE   "1"        TO  WRK-SANTEI-KBN(IDX)
                    END-IF
                    IF  (FLG-SRYKBN90    =   ZERO)     AND
                        (FLG-SRYKBN92    =   1)
                          MOVE   "92"       TO  WRK-SRYSKB(IDX)
                          MOVE   "2"        TO  WRK-SANTEI-KBN(IDX)
                    END-IF
                    IF  (FLG-SRYKBN90    =   1)     AND
                        (FLG-SRYKBN92    =   1)
                          MOVE   "90"       TO  WRK-SRYSKB(IDX)
                          MOVE   "1"        TO  WRK-SANTEI-KBN(IDX)
                    END-IF
                END-IF
           END-PERFORM
      *
      *    短期滞在手術基本料２と３の入院料未算定日について取得
           INITIALIZE                             ORCSNYUINDAY-AREA
           MOVE    LNK-BTUREC-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-BTUREC-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-BTUREC-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
                                                  SPA-AREA
           PERFORM   VARYING   IDX   FROM  1  BY  1
            UNTIL   (IDX     >   31)
               IF   LNK-NYUINDAY-TANKI-KBN(IDX)  =  "1"
                  MOVE   "92"             TO  WRK-SRYSKB(IDX)
                  MOVE   "1"              TO  WRK-SANTEI-KBN(IDX)
               END-IF
           END-PERFORM
      *
      **     PERFORM   VARYING   IDX   FROM  1  BY  1
      **      UNTIL   (IDX     >   31)
      **         DISPLAY "IDX = " IDX
      **         DISPLAY "WRK-BTUNUM(IDX) = " WRK-BTUNUM(IDX)
      **         DISPLAY "WRK-SRYSKB(IDX) = " WRK-SRYSKB(IDX)
      **         DISPLAY "WRK-SANTEI-KBN(IDX) = " WRK-SANTEI-KBN(IDX)
      **     END-PERFORM
      *
      *    病棟情報コードのセット
           PERFORM   VARYING   IDX   FROM  1  BY  1
            UNTIL   (IDX     >   31)
              IF   WRK-BTUNUM(IDX)  NOT =   SPACE
      *          入院基本料（特定入院料算定時も病棟、病室に特定入院料
      *                      算定時の病棟情報コード未設定を考慮してセット
      *                      しておく）
                 IF  WRK-SANTEI-KBN(IDX)    =   "1"  OR  "2"
                     MOVE   ZERO     TO    FLG-SET
                     PERFORM   VARYING   IDX-SYS   FROM  1  BY  1
                       UNTIL  (IDX-SYS   >   100 )  OR
                              (FLG-SET   =   1)
                       IF  WRK-BTUNUM(IDX)  =  WRK-SYSBTU-KBNCD(IDX-SYS)
                           MOVE   WRK-SYSBTU-SRYCD(IDX-SYS)
                                             TO  WRK-BTUCD(IDX)
                           MOVE   1          TO  FLG-SET
                       END-IF
                     END-PERFORM
                 END-IF
      *          病棟の特定入院料
                 IF (WRK-SANTEI-KBN(IDX)    =   "2")   AND
                    (WRK-TOKUTEI-KBN(IDX)   =   "2")
                     MOVE   ZERO     TO    FLG-SET
                     PERFORM   VARYING   IDX-SYS   FROM  1  BY  1
                       UNTIL  (IDX-SYS   >   100 )  OR
                              (FLG-SET   =   1)
                       IF  WRK-BTUNUM(IDX)  = WRK-SYSBTU-TKBNCD(IDX-SYS)
                           MOVE   WRK-SYSBTU-TSRYCD(IDX-SYS)
                                             TO  WRK-BTUCD(IDX)
                           MOVE   1          TO  FLG-SET
                       END-IF
                     END-PERFORM
                 END-IF
      *          病室の特定入院料
                 IF (WRK-SANTEI-KBN(IDX)    =   "2")   AND
                    (WRK-TOKUTEI-KBN(IDX)   =   "3")
                     MOVE   ZERO     TO    FLG-SET
                     PERFORM   VARYING   IDX-SYS   FROM  1  BY  1
                       UNTIL  (IDX-SYS   >   500 )  OR
                              (FLG-SET   =   1)
                       IF  WRK-BRMNUM(IDX)  =  WRK-SYSBRM-KBNCD(IDX-SYS)
                           MOVE   WRK-SYSBRM-TSRYCD(IDX-SYS)
                                             TO  WRK-BTUCD(IDX)
                           MOVE   1          TO  FLG-SET
                       END-IF
                     END-PERFORM
                 END-IF
              END-IF
           END-PERFORM
      *
      **     PERFORM   VARYING   IDX   FROM  1  BY  1
      **      UNTIL   (IDX     >   31)
      **         DISPLAY "IDX = " IDX
      **         DISPLAY "WRK-BTUCD(IDX) = " WRK-BTUCD(IDX)
      **     END-PERFORM
      *
      *
      *    返却エリアのセット
           PERFORM   VARYING   IDX   FROM  1  BY  1
            UNTIL   (IDX     >   31)
                 IF  (WRK-BTUNUM(IDX)  NOT =   SPACE)   AND
                     (WRK-BTUCD(IDX)   NOT =   SPACE)   AND
                     (WRK-HKNCOMBI-DAY(IDX)  NOT =  ZERO)
                     MOVE   WRK-HKNCOMBI-DAY(IDX)
                                           TO  LNK-BTUREC-HKNCOMBI(IDX)
                     MOVE   WRK-SRYSKB(IDX)
                                           TO  LNK-BTUREC-SRYSKB(IDX)
                     MOVE   WRK-BTUCD(IDX)
                                           TO  LNK-BTUREC-BTUCD(IDX)
                 END-IF
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴からワーク領域にセット
      *****************************************************************
       210-WRK-SET-SEC            SECTION.
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  WRK-BTUNUM(IDX)
           MOVE   PTNYUINRRK-BRMNUM   TO  WRK-BRMNUM(IDX)
           MOVE   PTNYUINRRK-TOKUTEI-KBN
                                      TO  WRK-TOKUTEI-KBN(IDX)
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                                      TO  WRK-TOKUTEI-NYUIN(IDX)
           MOVE   PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                      TO  WRK-TOKU-SANTEIYOUKENKBN(IDX)
      **     IF   PTNYUINRRK-TOKU-SANTEIYOUKENKBN  =  "1"
      **         MOVE  SPACE            TO  WRK-TOKUTEI-KBN(IDX)
      **         MOVE  SPACE            TO  WRK-TOKUTEI-NYUIN(IDX)
      **     END-IF
      *
           .
       210-WRK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC     TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター検索
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター検索
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報検索
      *****************************************************************
       910-PTHKNINF-SELECT-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       910-PTHKNINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTHKNINF-REC
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                     PTHKNINF-REC
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報検索
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTKOHINF-REC
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                     PTKOHINF-REC
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
                                               SYS-5000-REC
                                               SYS-5001-REC
                                               SYS-5002-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
                                               SYS-5000-REC
                                               SYS-5001-REC
                                               SYS-5002-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
