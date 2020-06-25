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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSLASTYMD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 最終来院日設定処理
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03.05.00    NACL-多々納   新規作成
      *****************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
           03  FLG-NYUIN               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX2                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-LASTYMD             PIC X(08).
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-HKNCOMBI-MEI        PIC X(10).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HONKZKKBN-MEI       PIC X(02).
      *
          03  WRK-NYUINYMD             PIC X(08).
          03  WRK-BRMNUM.
               05  WRK-BRMNUM1             PIC X(02).
               05  WRK-BRMNUM-H            PIC X(01).
               05  WRK-BRMNUM2             PIC X(06).
           03  WRK-Z6-X.
               05  WRK-Z6              PIC ZZZZZZ.
           03  WRK-BRMNUM-9            PIC 9(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "CPSK1005.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSLASTYMD.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSLASTYMDAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    メイン処理
           PERFORM 200-MAIN-SEC
      *    終了理
           PERFORM 300-END-SEC
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  ORCSLASTYMD-OUT
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      SPA-BEDFLG          >   ZERO
      *        病床がある場合、入院中の判定
               PERFORM 2001-NYUIN-CHK-SEC
           END-IF
      *    入院中以外
           PERFORM 2002-JYURRK-CHK-SEC
      *    診療科履歴
           IF     (FLG-NYUIN           =   ZERO ) AND
                  (WRK-LASTYMD         =   SPACE)
               PERFORM 2003-SRYKARRK-CHK-SEC
           END-IF
      *
      *    出力内容編集
           MOVE    WRK-LASTYMD         TO  ORCSLASTYMD-LASTYMD
           IF      WRK-LASTYMD     NOT =   SPACE
               MOVE    WRK-LASTYMD         TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  ORCSLASTYMD-LASTYMDW
           END-IF
           MOVE    WRK-SRYKA           TO  ORCSLASTYMD-SRYKA
           MOVE    WRK-DRCD            TO  ORCSLASTYMD-DRCD
           MOVE    WRK-BRMNUM          TO  ORCSLASTYMD-BRMNUM
           MOVE    WRK-NYUINYMD        TO  ORCSLASTYMD-NYUINYMD
           MOVE    WRK-TAIINYMD        TO  ORCSLASTYMD-TAIINYMD
      *    保険組合せ名称編集
           PERFORM 2004-HKNCOMBI-HEN-SEC
      *    診療科名称
           IF     (WRK-SRYKA       NOT =   SPACE )  AND
                  (ORCSLASTYMD-SRYKAFLG    =   "1" )
               PERFORM 2005-SRYKAMEI-HEN-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴チェック編集処理
      *****************************************************************
       2001-NYUIN-CHK-SEC                SECTION.
      *
           MOVE    SPACE                   TO  WRK-BRMNUM
           MOVE    SPACE                   TO  WRK-SRYKA
           MOVE    SPACE                   TO  WRK-HKNCOMBI
           MOVE    SPACE                   TO  WRK-LASTYMD
           MOVE    SPACE                   TO  WRK-TAIINYMD
           MOVE    SPACE                   TO  WRK-DRCD
           MOVE    ZERO                    TO  FLG-NYUIN
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-TAIINYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           PERFORM UNTIL    FLG-PTNYUINRRK =   1
      *
               IF     (PTNYUINRRK-NYUINYMD <=  SPA-SYSYMD) AND
                      (PTNYUINRRK-TAIINYMD >=  SPA-SYSYMD)
                   PERFORM 20011-NYUIN-HENSYU-SEC
                   MOVE    1                   TO  FLG-PTNYUINRRK
               ELSE
      *            最終入院日・退院日
                   IF      WRK-NYUINYMD        =   SPACE
                       MOVE    PTNYUINRRK-TAIINYMD TO  WRK-TAIINYMD
                       MOVE    PTNYUINRRK-NYUINYMD TO  WRK-NYUINYMD
                   END-IF
               END-IF
      *        システム日以前に退院
               IF     (PTNYUINRRK-TAIINYMD <   SPA-SYSYMD)
                   MOVE    1                   TO  FLG-PTNYUINRRK
               END-IF
               IF      FLG-PTNYUINRRK          =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key29"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       20011-NYUIN-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-BRMNUM
           MOVE    PTNYUINRRK-BRMNUM(1:2)      TO  WRK-BRMNUM1
           MOVE    "-"                         TO  WRK-BRMNUM-H
           IF      PTNYUINRRK-BRMNUM(3:6)  NUMERIC
               MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM-9
               MOVE    WRK-BRMNUM-9            TO  WRK-Z6
               MOVE    ZERO                    TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   6
                   IF      WRK-Z6-X(IDX:1)     NOT =   SPACE
                       ADD     1               TO  IDX2
                       MOVE    WRK-Z6-X(IDX:1) TO  WRK-BRMNUM2
                                                            (IDX2:1)
                   END-IF
               END-PERFORM
           ELSE
               MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM2
           END-IF
           MOVE    PTNYUINRRK-NYUINKA      TO  WRK-SRYKA
           MOVE    PTNYUINRRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
           MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-NYUINYMD
      *    前回ドクター
           MOVE    PTNYUINRRK-DRCD1        TO  WRK-DRCD
           MOVE    1                       TO  FLG-NYUIN
      *    当日退院の時
           MOVE    SPACE                   TO  WRK-TAIINYMD
           IF      PTNYUINRRK-TAIINYMD     =   SPA-SYSYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-LASTYMD
           END-IF
      *
           .
       20011-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2002-JYURRK-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  JYURRK-PTID
      **** MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
           MOVE    "99999999"          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      FLG-NYUIN           =   ZERO
                   MOVE    JYURRK-SRYYMD       TO  WRK-LASTYMD
                   MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
                   MOVE    JYURRK-DRCD         TO  WRK-DRCD
               END-IF
           END-IF
      *
      *    退院日＞受診履歴最終日は、退院日
           IF      (FLG-NYUIN          =   ZERO  )  AND
                   (WRK-LASTYMD    NOT =   SPACE )  AND
                   (WRK-TAIINYMD   NOT =   SPACE )
               IF      WRK-LASTYMD     <   WRK-TAIINYMD
                   MOVE    WRK-TAIINYMD    TO  WRK-LASTYMD
              END-IF
           END-IF
      *
           .
       2002-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       2003-SRYKARRK-CHK-SEC                SECTION.
      *
      *    診療科履歴読込
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           PERFORM UNTIL   FLG-SRYKARRK    =   1
               IF     (KARRK-LASTYMD   NOT =   SPACE )  AND
                      (KARRK-SRYKA     NOT =   SPACE )
                   MOVE    KARRK-LASTYMD       TO  WRK-LASTYMD
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA
                   MOVE    1                   TO  FLG-SRYKARRK
               ELSE
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-SRYKARRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2003-SRYKARRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       2004-HKNCOMBI-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-HONKZKKBN-MEI
      *
           IF      WRK-HKNCOMBI    NOT =   SPACE
               PERFORM 20041-HKNCOMBI-CHK1-SEC
           END-IF
           IF      WRK-HKNCOMBI-MEI    =   SPACE
               PERFORM 20042-HKNCOMBI-CHK2-SEC
           END-IF
      *
           MOVE    WRK-HKNCOMBI        TO  ORCSLASTYMD-HKNCOMBI
           MOVE    WRK-HKNNUM          TO  ORCSLASTYMD-HKNNUM
           MOVE    WRK-HKNCOMBI-MEI    TO  ORCSLASTYMD-HKNCOMBI-MEI
           MOVE    WRK-HONKZKKBN-MEI   TO  ORCSLASTYMD-HONKZKKBN
           .
       2004-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       20041-HKNCOMBI-CHK1-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-HKNCOMBI        =   ZERO )  AND
                  (COMB-DELKBN     NOT =   "1"  )  AND
                 ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                  (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
               PERFORM 200411-HKNCOMBI-MEI-SEC
           END-IF
      *
           .
       20041-HKNCOMBI-CHK1-EXT.
           EXIT.
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       20042-HKNCOMBI-CHK2-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCSLASTYMD-PTID    TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           PERFORM
               UNTIL       FLG-HKNCOMBI        =   1
               IF     (COMB-DELKBN     NOT =   "1"  )  AND
                     ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                      (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
                   IF     (WRK-HKNNUM      =   SPACE )  OR
                          (COMB-HKNNUM     =   "971"  OR  "973")
                       PERFORM 200411-HKNCOMBI-MEI-SEC
                   END-IF
               END-IF
               IF      FLG-HKNCOMBI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20042-HKNCOMBI-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       200411-HKNCOMBI-MEI-SEC               SECTION.
      *
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    COMB-KOH1-TANSEIDONAME  TO  WRK-HKNCOMBI-MEI
               MOVE    COMB-KOH1HKNNUM         TO  WRK-HKNNUM
           ELSE
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNCOMBI-MEI
               MOVE    COMB-HKNNUM             TO  WRK-HKNNUM
           END-IF
      *    本人家族
           EVALUATE    COMB-HONKZKKBN
               WHEN    "1"
                   MOVE    "本"            TO  WRK-HONKZKKBN-MEI
               WHEN    "2"
                   MOVE    "家"            TO  WRK-HONKZKKBN-MEI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-HONKZKKBN-MEI
           END-EVALUATE
           .
       200411-HKNCOMBI-MEI-EXT.
           EXIT.
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       2005-SRYKAMEI-HEN-SEC             SECTION.
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME3 TO  ORCSLASTYMD-SRYKA-MEI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2005-SRYKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       900-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       900-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       920-JYURRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       920-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    保険組合せ読込み処理
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
