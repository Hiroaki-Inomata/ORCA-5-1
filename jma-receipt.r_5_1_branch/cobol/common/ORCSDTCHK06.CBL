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
       PROGRAM-ID.             ORCSDTCHK06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェックマスタ−患者病名チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/02    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-QDTCHK                  PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-MATCH                   PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-LOOP-END                PIC 9(01).
           03  FLG-TAISYO-HKN              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-LEN                     PIC 9(05).
           03  IDX-ST                      PIC 9(05).
           03  IDX-COMB                    PIC 9(05).
           03  IDX-DAY                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                    PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-PATHNAME                PIC X(64).
           03  WRK-STR-LEN                 PIC 9(05).
           03  WRK-STR                     PIC X(500).
           03  WRK-BYOMEICD-G.
               05  WRK-BYOMEICD-OCC        OCCURS  21.
                   07  WRK-BYOMEICD        PIC X(07).
                   07  WRK-BYOMEICD-SEP    PIC X(01).
           03  WRK-CHKMST-KEY              PIC X(200).
           03  WRK-PTBYOMEI-KEY            PIC X(200).
           03  WRK-HKNCOMBI                PIC 9(04).
           03  WRK-ZAIHKNNUM               PIC X(03).
           03  WRK-BYOEDYMD                PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
      *
       01  LEN-AREA.
           03  LEN-CHKMST                  PIC 9(05).
           03  LEN-PTBYOMEI                PIC 9(05).
      *
      *    病名退避ワーク
       01  TBYOMEI-AREA.
           03  TBYOMEI-IDX1                PIC 9(05).
           03  TBYOMEI-IDX2                PIC 9(05).
           03  TBYOMEI-STIDX               PIC 9(05).
           03  TBYOMEI-MAX                 PIC 9(05).
           03  TBYOMEI-OCC                 OCCURS 101.
               05  TBYOMEI-ID              PIC 9(09).
               05  TBYOMEI-PTBYOMEI        PIC X(160).
               05  TBYOMEI-BYODAY-G.
                   07  TBYOMEI-BYODAY      PIC 9(01)   OCCURS  31.
               05  TBYOMEI-KEY.
                   07  TBYOMEI-SRYKA       PIC X(02).
                   07  TBYOMEI-SRYYMD      PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-TBYOMEI-MAX           PIC 9(05)   VALUE   100.
           03  CONST-STRLEN-MAX            PIC 9(05)   VALUE   200.
           03  CONST-MICODE                PIC X(07)   VALUE "0000999".
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
           03  CONST-SYAHO                 PIC X(01)
                                           VALUE   "1".
           03  CONST-KOKUHO                PIC X(01)
                                           VALUE   "2".
           03  CONST-ROSAI                 PIC X(01)
                                           VALUE   "3".
           03  CONST-JIBAI                 PIC X(01)
                                           VALUE   "4".
           03  CONST-JIHI                  PIC X(01)
                                           VALUE   "5".
           03  CONST-KOUGAI                PIC X(01)
                                           VALUE   "6".
           03  CONST-KOUKI                 PIC X(01)
                                           VALUE   "7".
           03  CONST-CHIKEN                PIC X(01)
                                           VALUE   "8".
           03  CONST-DAISAN                PIC X(01)
                                           VALUE   "9".
           03  CONST-HOUKATU               PIC X(01)
                                           VALUE   "H".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    データチェッククエリー
       01  QDTCHK-REC.
           COPY    "CPQDTCHK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSDTCHK06.INC".
           COPY    "CPSK1101.INC".
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
      *
           COPY    "COMMON-SPA".
           COPY    "CPDTCHKCOMMON.INC".
      *
       PROCEDURE                    DIVISION    USING
       SDTCHK06AREA
       SYS-1101-REC
       SDTCHK02AREA
       LNK-HKNCOMBI-AREA
       SPA-AREA
       DTCHKCOMMON-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       LEN-AREA
                                       TBYOMEI-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    "key6"          TO  WRK-PATHNAME
           IF    ( SDTCHK06-CHKPTN (1)     =   "1" )
               IF    ( SDTCHK06-CHKPTN (2)     =   " " )
                   IF    ( SDTCHK06-CHKPTN (3) =   " " )
                     AND ( SDTCHK06-CHKPTN (4) =   " " )
                     AND ( SDTCHK06-CHKPTN (5) =   " " )
      *
                       MOVE    "key7"  TO  WRK-PATHNAME
      *
                   ELSE
                       MOVE    "key6"  TO  WRK-PATHNAME
                   END-IF
               ELSE
                       MOVE    "key5"  TO  WRK-PATHNAME
               END-IF
           ELSE
               MOVE    "key8"          TO  WRK-PATHNAME
           END-IF
      *
           PERFORM 900-QDTCHK-KEY-SEL-SEC
           IF    ( FLG-QDTCHK      =   ZERO )
               PERFORM 900-QDTCHK-KEY-FET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-QDTCHK      NOT =   ZERO )
      *
               PERFORM 2001-BYOMEI-CHK-SEC
               IF    ( FLG-MATCH    =  1 )
                   PERFORM 2001-TAISYO-CHK-SEC
                   IF    ( FLG-TAISYO  =   1 )
                       PERFORM 2001-EDIT-TBYOMEI-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-QDTCHK-KEY-FET-SEC
      *
           END-PERFORM
      *
      *
           MOVE    "query_dtchk"       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATH
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       2001-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-MATCH
      *
           IF    ( QDTCHK-CD               NOT =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)     NOT =   CONST-MICODE )
      *
                IF   ( SDTCHK06-CHKPTN (1)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
      *
                       PERFORM 20011-BYOMEICDKEY-HEN-SEC
                       MOVE    QDTCHK-CD       TO  WRK-CHKMST-KEY
                       MOVE    WRK-BYOMEICD-G  TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
                IF   ( SDTCHK06-CHKPTN (2)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD               NOT =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)         =   CONST-MICODE )
      *
                IF   ( SDTCHK06-CHKPTN (3)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD                   =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)     NOT =   CONST-MICODE )
      *
                IF   ( SDTCHK06-CHKPTN (4)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD                   =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)         =   CONST-MICODE )
      *
                IF   ( SDTCHK06-CHKPTN (5)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           .
       2001-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードKEY編集処理
      *****************************************************************
       20011-BYOMEICDKEY-HEN-SEC      SECTION.
      *
           INITIALIZE                  WRK-BYOMEICD-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                    OR   ( QDTCHK-BYOMEICD (IDX0)  =   SPACE )
      *
               MOVE    QDTCHK-BYOMEICD (IDX0)
                                   TO  WRK-BYOMEICD (IDX0)
      *
               IF    ( IDX0        >   1 )
                   MOVE    "-"     TO  WRK-BYOMEICD-SEP (IDX0 - 1)
               END-IF
      *
           END-PERFORM
      *
           .
       20011-BYOMEICDKEY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    突合処理
      *****************************************************************
       20011-TOTUGO-SEC                SECTION.
      *
           EVALUATE    QDTCHK-COMPKBN
           WHEN    "2"
               IF    ( WRK-CHKMST-KEY  =   WRK-PTBYOMEI-KEY )
                       MOVE    1   TO  FLG-MATCH
               END-IF
           WHEN    "3"
           WHEN    "4"
      *
               MOVE    WRK-CHKMST-KEY      TO  WRK-STR
               PERFORM 800-STRLEN-SEC
               MOVE    WRK-STR-LEN         TO  LEN-CHKMST
      *
               MOVE    WRK-PTBYOMEI-KEY    TO  WRK-STR
               PERFORM 800-STRLEN-SEC
               MOVE    WRK-STR-LEN         TO  LEN-PTBYOMEI
      *
               IF    ( LEN-PTBYOMEI   >=   LEN-CHKMST )
                   IF    ( QDTCHK-COMPKBN  =   "3" )
                       IF    ( WRK-PTBYOMEI-KEY (1:LEN-CHKMST)
                                           =   WRK-CHKMST-KEY )
                           MOVE    1   TO  FLG-MATCH
                       END-IF
                   ELSE
                       COMPUTE IDX-ST
                           =   LEN-PTBYOMEI
                           -   LEN-CHKMST
                           +   1
                       IF    ( WRK-PTBYOMEI-KEY (IDX-ST :)
                                           =   WRK-CHKMST-KEY )
                           MOVE    1   TO  FLG-MATCH
                       END-IF
                   END-IF
               END-IF
           WHEN    OTHER
               MOVE    1           TO  FLG-MATCH
           END-EVALUATE
      *
           .
       20011-TOTUGO-EXT.
           EXIT.
      *****************************************************************
      *    病名対象チェック
      *****************************************************************
       2001-TAISYO-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-TAISYO
      *
           IF    ( QDTCHK-SRYYMD (1:6)        <=   SDTCHK06-KJNYMD(1:6))
               IF    ( QDTCHK-TENKIKBN         =   SPACE )
                   IF    ( QDTCHK-NYUGAIKBN    =   SPACE )
                    OR   ( QDTCHK-NYUGAIKBN    =   SDTCHK06-NYUGAIKBN )
                           MOVE    1       TO  FLG-TAISYO
                   END-IF
               ELSE
                   IF    ( QDTCHK-TENKIYMD (1:6)
                                              >=   SDTCHK06-KJNYMD(1:6))
                       IF    ( QDTCHK-NYUGAIKBN
                                               =   SPACE )
                        OR   ( QDTCHK-NYUGAIKBN
                                               =   SDTCHK06-NYUGAIKBN )
                           MOVE    1       TO  FLG-TAISYO
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    レセプト表示期間を過ぎていないかチェック
           IF    ( QDTCHK-REZEMM       >   ZERO )
      *
               MOVE    QDTCHK-SRYYMD   TO  WRK-SYMD
      *
               IF    ( QDTCHK-REZEMM   >   1 )
                   MOVE    "2"     TO  WRK-ZOGENPTN
                   COMPUTE WRK-ZOGEN   =   QDTCHK-REZEMM
                                       -   1
                   PERFORM 800-CALENDAR-SEC
               END-IF
      *
               IF    ( SDTCHK06-KJNYMD(1:6)    >   WRK-SYMD(1:6))
                   MOVE    ZERO    TO  FLG-TAISYO
               END-IF
      *
           END-IF
      *
           .
       2001-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名退避エリア編集処理
      *****************************************************************
       2001-EDIT-TBYOMEI-SEC           SECTION.
      *
           IF    ( CONST-TBYOMEI-MAX   >   TBYOMEI-MAX )
      *
               COMPUTE TBYOMEI-MAX
                   =   TBYOMEI-MAX     +   1
      *
               MOVE    QDTCHK-ID       TO  TBYOMEI-ID     (TBYOMEI-MAX)
               MOVE    QDTCHK-SRYKA    TO  TBYOMEI-SRYKA  (TBYOMEI-MAX)
               MOVE    QDTCHK-SRYYMD   TO  TBYOMEI-SRYYMD (TBYOMEI-MAX)
               MOVE    QDTCHK-PTBYOMEI TO  TBYOMEI-PTBYOMEI(TBYOMEI-MAX)
      *
               MOVE    SDTCHK06-KJNYMD TO  WRK-SRYYMD
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   MOVE    IDX-DAY     TO  WRK-SRYDD
                   IF    ( QDTCHK-SRYYMD   <=  WRK-SRYYMD )
                       IF    ( QDTCHK-TENKIKBN =   SPACE )
                               MOVE    1   TO  TBYOMEI-BYODAY
                                                   (TBYOMEI-MAX IDX-DAY)
                       ELSE
                           IF    ( QDTCHK-TENKIYMD >=  WRK-SRYYMD )
                               MOVE    1   TO  TBYOMEI-BYODAY
                                                   (TBYOMEI-MAX IDX-DAY)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
           END-IF
      *
           .
       2001-EDIT-TBYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       220-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   SDTCHK02-O-MAX )
                    OR   ( COMB-HOSPNUM    NOT =   ZERO )
      *
               IF    ( LNK-COMB-HKNCOMBINUM (IDX-COMB)
                                  =    WRK-HKNCOMBI )
                   MOVE    LNK-HKNCOMBI-OCC (IDX-COMB)
                                  TO   HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       220-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    対象保険チェック処理
      *****************************************************************
       220-TAISYO-HKN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TAISYO-HKN
      *
           EVALUATE    TRUE
           WHEN      ( WRK-ZAIHKNNUM   =   "060" )
           WHEN      ( WRK-ZAIHKNNUM   =   "067" )
           WHEN      ( WRK-ZAIHKNNUM   =   "068" )
           WHEN      ( WRK-ZAIHKNNUM   =   "069" )
               IF    ( SDTCHK06-HKNKBN =   CONST-KOKUHO )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "971" )
               IF    ( SDTCHK06-HKNKBN =   CONST-ROSAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "973" )
               IF    ( SDTCHK06-HKNKBN =   CONST-JIBAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "970" )
               IF    ( SDTCHK06-HKNKBN =   CONST-DAISAN )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "975" )
               IF    ( SDTCHK06-HKNKBN =   CONST-KOUGAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "040" )
           WHEN      ( WRK-ZAIHKNNUM   =   "039" )
               IF    ( SDTCHK06-HKNKBN =   CONST-KOUKI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "98" )
               IF    ( SDTCHK06-HKNKBN =   CONST-JIHI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "90" OR "91" )
               IF    ( SDTCHK06-HKNKBN =   CONST-CHIKEN )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( SDTCHK06-HKNKBN =   CONST-HOUKATU )
                   MOVE    1           TO  FLG-TAISYO-HKN
      *
           WHEN      OTHER
               IF    ( SDTCHK06-HKNKBN =   CONST-SYAHO )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           END-EVALUATE
      *
           .
       220-TAISYO-HKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           INITIALIZE                      SDTCHK06-OT
      *
           PERFORM 3001-TBYOMEI-SORT-SEC
      *
           MOVE    TBYOMEI-MAX         TO  SDTCHK06-O-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TBYOMEI-MAX )
      *
               MOVE    TBYOMEI-ID (IDX0)
                                       TO  SDTCHK06-O-ID (IDX0)
               MOVE    TBYOMEI-SRYKA  (IDX0)
                                       TO  SDTCHK06-O-SRYKA  (IDX0)
               MOVE    TBYOMEI-SRYYMD (IDX0)
                                       TO  SDTCHK06-O-SRYYMD (IDX0)
               MOVE    TBYOMEI-PTBYOMEI(IDX0)
                                       TO  SDTCHK06-O-PTBYOMEI(IDX0)
               MOVE    TBYOMEI-BYODAY-G(IDX0)
                                       TO  SDTCHK06-O-BYODAY-G(IDX0)
      *
           END-PERFORM
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ＳＯＲＴ処理
      *****************************************************************
       3001-TBYOMEI-SORT-SEC           SECTION.
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING TBYOMEI-IDX1    FROM    2   BY  1
                   UNTIL ( TBYOMEI-IDX1    >   TBYOMEI-MAX )
               COMPUTE TBYOMEI-STIDX       =   TBYOMEI-IDX1    -   1
               MOVE    TBYOMEI-OCC (TBYOMEI-IDX1)
                                   TO  TBYOMEI-OCC
                                            (CONST-TBYOMEI-MAX + 1)
               PERFORM VARYING TBYOMEI-IDX2
                       FROM    TBYOMEI-STIDX   BY  -1
                       UNTIL ( TBYOMEI-IDX2    <   1 )
                          OR ( TBYOMEI-KEY (CONST-TBYOMEI-MAX + 1)
                                   <=  TBYOMEI-KEY (TBYOMEI-IDX2) )
                       MOVE    TBYOMEI-OCC (TBYOMEI-IDX2)
                           TO  TBYOMEI-OCC (TBYOMEI-IDX2 + 1)
               END-PERFORM
               MOVE    TBYOMEI-OCC (CONST-TBYOMEI-MAX + 1)
                           TO  TBYOMEI-OCC (TBYOMEI-IDX2 + 1)
           END-PERFORM
      *
           .
       3001-TBYOMEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       800-STRLEN-SEC                  SECTION.
      *
           MOVE    ZERO    TO          WRK-STR-LEN
      *
           PERFORM VARYING IDX-LEN FROM    1   BY  1
                   UNTIL ( IDX-LEN             >   CONST-STRLEN-MAX )
                    OR   ( WRK-STR (IDX-LEN :) =   SPACE )
      *
               COMPUTE    WRK-STR-LEN  =   WRK-STR-LEN +   1
      *
           END-PERFORM
      *
           .
       800-STRLEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    データチェッククエリ検索処理(KEY)
      *****************************************************************
       900-QDTCHK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-QDTCHK
      *
           INITIALIZE                      QDTCHK-REC
      *
           MOVE    SDTCHK06-HOSPNUM     TO  QDTCHK-SELHOSPNUM
           MOVE    SDTCHK06-PTID       TO  QDTCHK-SELPTID
      *    IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
      *        MOVE    SDTCHK06-SRYKA  TO  QDTCHK-SELSRYKA
      *    ELSE
               MOVE    "%"             TO  QDTCHK-SELSRYKA
      *    END-IF
           MOVE    SDTCHK06-CHKKBN     TO  QDTCHK-SELCHKKBN
           MOVE    SDTCHK06-SRYCD      TO  QDTCHK-SELSRYCD
           MOVE    SDTCHK06-KJNYMD     TO  QDTCHK-SELYUKOSTYMD
                                           QDTCHK-SELYUKOEDYMD
           MOVE    SDTCHK06-CDKBN      TO  QDTCHK-SELCDKBN
      *
           MOVE    QDTCHK-REC          TO  MCPDATA-REC
           MOVE    "query_dtchk"       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  QDTCHK-REC
           ELSE
               INITIALIZE                  QDTCHK-REC
               MOVE    1               TO  FLG-QDTCHK
           END-IF
      *
           .
       900-QDTCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェッククエリFETCH処理(KEY)
      *****************************************************************
       900-QDTCHK-KEY-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-QDTCHK      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "query_dtchk"       TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  QDTCHK-REC
      *
                   MOVE    1               TO  FLG-LOOP-END
      *
                   CALL    "ORCSDTCHK07"   USING   QDTCHK-REC
      *
                   IF    ( SYS-1101-YBUTAGAIKBN    =   "2" )
                       IF    ( SDTCHK06-CHKKBN     =   "8" )
                        AND  ( QDTCHK-UTAGAIFLG    =   "1"  OR "3" )
                           MOVE    ZERO        TO  FLG-LOOP-END
                       END-IF
                   END-IF
      *
                   IF    ( QDTCHK-REZEFLG  =   "1" )
                       MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( QDTCHK-TENKIKBN =   "8" )
                    AND  ( QDTCHK-TENKIYMD (1:6)
                                          <=   SDTCHK06-KJNYMD (1:6))
                       MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( QDTCHK-HKNCOMBI  NOT =   ZERO )
                    OR   ( QDTCHK-KOHID     NOT =   ZERO )
      *
                       MOVE    "99999999"  TO  WRK-BYOEDYMD
                       IF    ( QDTCHK-TENKIYMD  NOT =   SPACE )
                           MOVE    QDTCHK-TENKIYMD
                                           TO  WRK-BYOEDYMD
                       END-IF
      *
                       IF    ( QDTCHK-HKNCOMBI  NOT =   ZERO )
                           MOVE    QDTCHK-HKNCOMBI     TO  WRK-HKNCOMBI
                           PERFORM 220-HKNCOMBI-GET-SEC
                           MOVE    COMB-HKNNUM         TO  WRK-ZAIHKNNUM
                           PERFORM 220-TAISYO-HKN-CHK-SEC
                           IF    ( FLG-TAISYO-HKN  =   ZERO )
                               MOVE    ZERO            TO  FLG-LOOP-END
                           END-IF
      *
                           MOVE    ZERO            TO  WRK-HKNCOMBI
                           PERFORM VARYING IDX-COMB  FROM  1   BY  1
                                   UNTIL ( IDX-COMB  >   THKNCOMBI-MAX )
                                    OR   ( WRK-HKNCOMBI  NOT = ZERO )
      *
                               IF    ( COMB-HKNCOMBINUM
                                               =   THKNCOMBI-HKNCOMBI
                                                        (IDX-COMB))
                                   MOVE    THKNCOMBI-HKNCOMBI (IDX-COMB)
                                                   TO  WRK-HKNCOMBI
      *
                                   IF    ( QDTCHK-SRYYMD
                                               <=  COMB-TEKEDYMD )
                                    AND  ( WRK-BYOEDYMD
                                               >=  COMB-TEKSTYMD )
                                       CONTINUE
                                   ELSE
                                       MOVE    ZERO    TO  FLG-LOOP-END
                                   END-IF
      *
                               END-IF
      *
                           END-PERFORM
      *
                           IF    ( WRK-HKNCOMBI    =   ZERO )
                                   MOVE    ZERO    TO  FLG-LOOP-END
                           END-IF
                       END-IF
                       IF    ( QDTCHK-KOHID    NOT =   ZERO )
                           MOVE    "970"           TO  WRK-ZAIHKNNUM
                           PERFORM 220-TAISYO-HKN-CHK-SEC
                           IF    ( FLG-TAISYO-HKN  =   ZERO )
                               MOVE    ZERO        TO  FLG-LOOP-END
                           ELSE
                               PERFORM 900-PTKOHINF-KEY-SEL-SEC
                               IF    ( QDTCHK-SRYYMD <= PTKOH-TEKEDYMD )
                                AND  ( WRK-BYOEDYMD  >= PTKOH-TEKSTYMD )
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-LOOP-END
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF    ( SDTCHK06-HKNKBN      =  CONST-ROSAI
                                                    OR CONST-JIBAI
                                                    OR CONST-KOUGAI
                                                    OR CONST-DAISAN
                                                                   )
                           MOVE    ZERO            TO  FLG-LOOP-END
                       END-IF
                   END-IF
      *
               ELSE
                   INITIALIZE                  QDTCHK-REC
                   MOVE    1               TO  FLG-QDTCHK
               END-IF
      *
           END-PERFORM
      *
           .
       900-QDTCHK-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                      PTKOHINF-REC
           MOVE    SDTCHK06-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    SDTCHK06-PTID       TO  PTKOH-PTID
           MOVE    QDTCHK-KOHID        TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
