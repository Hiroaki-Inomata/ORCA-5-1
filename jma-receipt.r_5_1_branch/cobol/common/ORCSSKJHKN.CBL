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
       PROGRAM-ID.                 ORCSSKJHKN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 食事保険取得（平成２９年１０月以降）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/10/04    NACL−小豆沢  新規作成
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TANKI2                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-TK3                     PIC 9(02).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    食事保険組合せ指定
           03   CONS-HKN-SITEI             PIC X(09) VALUE "099999930".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNSYU                  PIC X(01).
      *
           03  WRK-NACCT-DAY9              PIC 9(04)   OCCURS 31.
           03  WRK-NACCT-DAY               PIC X(04)   OCCURS 31.
      *
      *    同日再入院日の翌日取得用
           03  WRK-DOUJITSU-YMD            PIC X(08).
           03  WRK-DOUJITSU-YMDR    REDEFINES   WRK-DOUJITSU-YMD.
               05  WRK-DOUJITSU-YYMM       PIC 9(06).
               05  WRK-DOUJITSU-DD         PIC 9(02).
      *
      *    同日再入院日の翌日取得用
           03  WRK-YOKUJITSU-YMD          PIC 9(08).
           03  WRK-YOKUJITSU-YMDR   REDEFINES   WRK-YOKUJITSU-YMD.
               05  WRK-YOKUJITSU-YYMM      PIC 9(06).
               05  WRK-YOKUJITSU-DD        PIC 9(02).
      *
      *    月末日取得用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    短期滞在以外で入院料を算定している日
           03  WRK-TANKI-IGAI              PIC X(01)  OCCURS 31.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
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
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKJHKN.INC".
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSSKJHKN-AREA
           ORCSNYUINDAY-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-SKJHKN-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-SKJHKN-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SKJHKN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-SKJHKN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     PERFORM    VARYING IDX   FROM   1   BY  1
      *                UNTIL IDX   >   31
      *       DISPLAY "IDX = " IDX
      *               " NYUIN-HKNCOMBI = "
      *                LNK-SKJHKN-HKNCOMBINUM(IDX 1)
      *               " HKNSYU = "
      *                LNK-SKJHKN-HKNSYU(IDX 1)
      *               " DOUJITSU-HKNCOMBI = "
      *                LNK-SKJHKN-HKNCOMBINUM(IDX 2)
      *               " HKNSYU = "
      *                LNK-SKJHKN-HKNSYU(IDX 2)
      *     END-PERFORM
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
           IF     (LNK-SKJHKN-SANTEIYM     =  SPACE)      OR
                  (LNK-SKJHKN-SANTEIYM    <=  "201709")
               MOVE    1                  TO  LNK-SKJHKN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                         PTINF-REC
           MOVE    LNK-SKJHKN-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    LNK-SKJHKN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    2                  TO  LNK-SKJHKN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                         NYUINACCT-REC
           MOVE    LNK-SKJHKN-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    LNK-SKJHKN-PTID        TO  NACCT-PTID
           MOVE    LNK-SKJHKN-SANTEIYM    TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          NOT =   ZERO
               MOVE    3                  TO  LNK-SKJHKN-RC
           ELSE
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key17"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT         NOT =   ZERO
                    MOVE    3             TO  LNK-SKJHKN-RC
               ELSE
                  PERFORM    VARYING IDX   FROM   1   BY  1
                    UNTIL    IDX   >   31
                      MOVE   NACCT-DAY(IDX)      TO  WRK-NACCT-DAY9(IDX)
                      MOVE   WRK-NACCT-DAY9(IDX) TO  WRK-NACCT-DAY(IDX)
                  END-PERFORM
               END-IF
           END-IF
      **     PERFORM    VARYING IDX   FROM   1   BY  1
      **                UNTIL IDX   >   31
      **       DISPLAY "IDX = " IDX
      **               " WRK-NACCT-DAY = "
      **                WRK-NACCT-DAY(IDX)
      **     END-PERFORM
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    月末日を取得
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-SKJHKN-SANTEIYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                              LNK-DAY7-AREA
           IF      STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
           END-IF
      *
      *    同日再入院日の保険組合せ取得
      **     INITIALIZE                         ORCSNYUINDAY-AREA
      **     MOVE    LNK-SKJHKN-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
      **     MOVE    LNK-SKJHKN-PTID        TO  LNK-NYUINDAY-PTID
      **     MOVE    LNK-SKJHKN-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
      **     CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
      **                                            SPA-AREA
      **     PERFORM    VARYING IDX   FROM   1   BY  1
      **                UNTIL IDX   >   31
      **       DISPLAY "IDX = " IDX
      **               " DOUJITSU = "
      **                LNK-NYUINDAY-DOUJITSU-KBN(IDX)
      **               " TAI = "
      **                LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
      **               " SAINYU = "
      **                LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
      **     END-PERFORM
      *
      *    システム予約の入力を取得
           INITIALIZE                          SRYACT-REC
           MOVE    LNK-SKJHKN-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    "1"                     TO  SRY-NYUGAIKBN
           MOVE    LNK-SKJHKN-PTID         TO  SRY-PTID
           MOVE    LNK-SKJHKN-SANTEIYM     TO  SRY-SRYYM
           MOVE    CONS-HKN-SITEI          TO  SRY-SRYCD(1)
                                               SRY-SRYCD(2)
                                               SRY-SRYCD(3)
                                               SRY-SRYCD(4)
                                               SRY-SRYCD(5)
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT             =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACT  =   1)
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE    SRY-PTID        TO  ACCT-PTID
                   MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                   MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                   MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-SELECT-SEC
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                   IF    FLG-SRYACCT       =   0
                       PERFORM    VARYING   IDX   FROM   1   BY   1
                         UNTIL     IDX   >   31
                           IF   ACCT-DAY(1 IDX)   NOT =   ZERO
                             IF   ACCT-DAY(2 IDX)   NOT =   ZERO
                                 IF   ACCT-HKNCOMBI  NOT =
                                                WRK-NACCT-DAY9(IDX)
                                     IF   LNK-SKJHKN-HKNCOMBINUM(IDX 1)
                                                      =   SPACE
                                         MOVE   ACCT-HKNCOMBI
                                           TO   LNK-SKJHKN-HKNCOMBINUM
                                                                (IDX 1)
                                     ELSE
                                         MOVE   5  TO   LNK-SKJHKN-RC
                                     END-IF
                                 END-IF
                             END-IF
      *                      同日再入院分
                             IF   ACCT-DAY(3 IDX)   NOT =   ZERO
      **                           DISPLAY "IDX = " IDX
      **                                   "ACCT-HKMCOMBI = "
      **                                    ACCT-HKNCOMBI
                                 IF   ACCT-HKNCOMBI  NOT =
                                     LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                     IF   LNK-SKJHKN-HKNCOMBINUM(IDX 2)
                                                      =   SPACE
                                         MOVE   ACCT-HKNCOMBI
                                           TO   LNK-SKJHKN-HKNCOMBINUM
                                                                (IDX 2)
                                     ELSE
                                         MOVE   5  TO   LNK-SKJHKN-RC
                                     END-IF
                                 END-IF
                             END-IF
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            診療会計クローズ
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key31"         TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    診療行為クローズ
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    保険種類のセット
           PERFORM    VARYING IDX   FROM   1   BY  1
                      UNTIL IDX   >   31
              IF  LNK-SKJHKN-HKNCOMBINUM (IDX 1)  NOT = SPACE
                 MOVE    LNK-SKJHKN-HKNCOMBINUM (IDX 1)
                                           TO  WRK-HKNCOMBINUM
                 PERFORM   210-HKNSYURUI-GET-SEC
                 MOVE    WRK-HKNSYU        TO  LNK-SKJHKN-HKNSYU(IDX 1)
              END-IF
              IF  LNK-SKJHKN-HKNCOMBINUM (IDX 2)  NOT = SPACE
                 MOVE    LNK-SKJHKN-HKNCOMBINUM (IDX 2)
                                           TO  WRK-HKNCOMBINUM
                 PERFORM   210-HKNSYURUI-GET-SEC
                 MOVE    WRK-HKNSYU        TO  LNK-SKJHKN-HKNSYU(IDX 2)
              END-IF
           END-PERFORM
      *
           IF       LNK-SKJHKN-TBL     =    SPACE
               MOVE     4              TO   LNK-SKJHKN-RC
           END-IF
           DISPLAY  "LNK-SKJHKN-RC = " LNK-SKJHKN-RC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院（入院分）の保険種類取得
      *****************************************************************
       210-HKNSYURUI-GET-SEC            SECTION.
      *
           MOVE    SPACE                TO   WRK-HKNSYU
           MOVE    "1"                  TO   WRK-HKNSYU
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-SKJHKN-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    LNK-SKJHKN-PTID         TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            国保
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO  WRK-HKNSYU
                   END-IF
      *            後期高齢者
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO  WRK-HKNSYU
                   END-IF
      *            労災保険
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO  WRK-HKNSYU
                   END-IF
      *            自賠責保険
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO  WRK-HKNSYU
                   END-IF
      *            公害保険
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO  WRK-HKNSYU
                   END-IF
      *            自費
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO  WRK-HKNSYU
                   END-IF
      *            治験
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO  WRK-HKNSYU
                   END-IF
               END-IF
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET-EXT.
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
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
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
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
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
               MOVE    ZERO               TO  FLG-SRYACT
           ELSE
               MOVE    1                  TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
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
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   MOVE    ZERO        TO  FLG-TENSU
               ELSE
                   MOVE    1           TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
