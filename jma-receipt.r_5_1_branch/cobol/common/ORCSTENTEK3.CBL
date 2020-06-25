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
       PROGRAM-ID.             ORCSTENTEK3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 入院点滴集計サブ（会計カード用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-HKNCOMBI                  PIC 9(01).
           03  FLG-SRYACCT                   PIC 9(01).
           03  FLG-SRYACT                    PIC 9(01).
           03  FLG-ARI                       PIC 9(01).
           03  FLG-OK                        PIC 9(01).
           03  FLG-NG                        PIC 9(01).
           03  FLG-NASHI                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDM                           PIC 9(03).
           03  IDX                           PIC 9(03).
           03  IDX5                          PIC 9(03).
           03  IDX6                          PIC 9(03).
           03  IDX9                          PIC 9(03).
           03  IDX13                         PIC 9(03).
           03  IDY                           PIC 9(03).
           03  IDY2                          PIC 9(03).
           03  IDY3                          PIC 9(03).
           03  IDY4                          PIC 9(03).
           03  IDY5                          PIC 9(03).
           03  IDY6                          PIC 9(03).
           03  IDY7                          PIC 9(03).
           03  FUKA-IDX                      PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD2-X.
             05  WRK-SRYCD2                  PIC 9(09).
           03  WRK-FUKA-KINGAKU              PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                PIC 9(01).
           03  WRK-ZAINUM                    PIC 9(08).
      *
       01  WRK-TENTEKI2-TABLE.
           03  WRK-TENTEKI2-ZAINUM                 PIC 9(08)
                                                   OCCURS   40.
      *
           03  WRK-TENTEKI2-T-AREA                 OCCURS   31.
             05  WRK-TENTEKI2-T-SRYKA              PIC X(02).
             05  WRK-TENTEKI2-T-MEISAISU           PIC 9(03).
             05  WRK-TENTEKI2-T-ZAITEN             PIC 9(07).
             05  WRK-TENTEKI2-T-SRYCDTOTAL         PIC 9(13).
             05  WRK-TENTEKI2-T-SRYCD-X.
               07  WRK-TENTEKI2-T-SRYCD-X-OCC      OCCURS    5.
                 08  WRK-TENTEKI2-T-SRYCD            PIC X(09).
                 08  WRK-TENTEKI2-T-COM-MEISAISU     PIC 9(03).
                 08  WRK-TENTEKI2-T-COM-AREA       OCCURS    5.
                   09  WRK-TENTEKI2-T-COM-FLG        PIC X(01).
                   09  WRK-TENTEKI2-T-COM-SRYCD      PIC X(09).
                   09  WRK-TENTEKI2-T-COM-MOJI       PIC X(100).
           03  WRK-TENTEKI2-T-ERRCD                PIC 9(01).
      *
           03  WRK-TENTEKI2-C-AREA                 OCCURS   31.
             05  WRK-TENTEKI2-C-SRYKA              PIC X(02).
             05  WRK-TENTEKI2-C-MEISAISU           PIC 9(03).
             05  WRK-TENTEKI2-C-ZAITEN             PIC 9(07).
             05  WRK-TENTEKI2-C-SRYCDTOTAL         PIC 9(13).
             05  WRK-TENTEKI2-C-SRYCD-X.
               07  WRK-TENTEKI2-C-SRYCD-X-OCC      OCCURS    5.
                 08  WRK-TENTEKI2-C-SRYCD            PIC X(09).
                 08  WRK-TENTEKI2-C-COM-MEISAISU     PIC 9(03).
                 08  WRK-TENTEKI2-C-COM-AREA       OCCURS    5.
                   09  WRK-TENTEKI2-C-COM-FLG        PIC X(01).
                   09  WRK-TENTEKI2-C-COM-SRYCD      PIC X(09).
                   09  WRK-TENTEKI2-C-COM-MOJI       PIC X(100).
           03  WRK-TENTEKI2-C-ERRCD                PIC 9(01).
      *
       01  WRK-COM-TABLE.
           03  WRK-COM-OCC                   OCCURS    350.
             05  WRK-COM-FLG                 PIC 9(01).
             05  WRK-COM                     PIC X(200).
       01  WRK-COM2-TABLE.
           03  WRK-COM2-OCC                  OCCURS    350.
             05  WRK-COM2-FLG                PIC 9(01).
             05  WRK-COM2                    PIC X(200).
       01  WRK-IDX-TABLE.
           03  WRK-IDX-OCC                   OCCURS    350.
             05  WRK-IDX                     PIC 9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    入院点滴手技加算まとめサブ
           COPY    "CPORCSTENTEK2.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSTENTEK3.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSTENTEK3AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-ORCSTEN3-RC
           INITIALIZE                      LNK-ORCSTEN3-OUT-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-TENTEKI2-TABLE
      *
      *    パラメタチェック
           PERFORM     100-PRM-CHK-SEC
           IF    LNK-ORCSTEN3-RC  =  ZERO
             PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    患者ＩＤチェック
           IF      LNK-ORCSTEN3-PTID       =  ZERO
               MOVE    1                 TO  LNK-ORCSTEN3-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    診療年月チェック
           IF      LNK-ORCSTEN3-SRYYM      =  SPACE
           OR      LNK-ORCSTEN3-SRYYM      < "200804"
               MOVE    2                 TO  LNK-ORCSTEN3-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    保険組合せチェック
           IF      LNK-ORCSTEN3-HKNCOMBI   =  ZERO
               MOVE    3                 TO  LNK-ORCSTEN3-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           INITIALIZE                        HKNCOMBI-REC
           MOVE   LNK-ORCSTEN3-HOSPNUM   TO  COMB-HOSPNUM
           MOVE   LNK-ORCSTEN3-PTID      TO  COMB-PTID
           MOVE   LNK-ORCSTEN3-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE   HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE   "tbl_hkncombi"         TO  MCP-TABLE
           MOVE   "key"                  TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE   "tbl_hkncombi"       TO  MCP-TABLE
             MOVE   "key"                TO  MCP-PATHNAME
             PERFORM 900-HKNCOMBI-READ-SEC
             IF      FLG-HKNCOMBI  =  ZERO
               CONTINUE
             ELSE
               MOVE    3                 TO  LNK-ORCSTEN3-RC
             END-IF
           ELSE
               MOVE    3                 TO  LNK-ORCSTEN3-RC
           END-IF
           MOVE   "tbl_hkncombi"         TO  MCP-TABLE
           MOVE   "key"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    入院点滴の手技及び加算編集
           PERFORM     210-TENTEKI-SYUGI-HENSYU-SEC
      *
      *    入院点滴の薬剤及び器材編集
           PERFORM     220-TENTEKI-YAK-HENSYU-SEC
      *
      *    まとめ対象剤番号編集
           PERFORM     230-TENTEKI-ZAINUM-HENSYU-SEC
      *
      *    剤まとめ編集
           PERFORM     240-TENTEKI-ZAI-MATOME-SEC
      *
      *    入院点滴の全体まとめ編集
           PERFORM     250-TENTEKI-ALL-HENSYU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点滴の手技及び加算編集
      *****************************************************************
       210-TENTEKI-SYUGI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                         ORCSTENTEK2AREA
           MOVE    LNK-ORCSTEN3-HOSPNUM   TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    LNK-ORCSTEN3-PTID      TO  LNK-ORCSTEN2-PTID
           MOVE    LNK-ORCSTEN3-SRYYM     TO  LNK-ORCSTEN2-SRYYM
           MOVE    LNK-ORCSTEN3-HKNCOMBI  TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"          USING
                                          ORCSTENTEK2AREA
                                          SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
             IF   (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
             OR   (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
               MOVE  LNK-ORCSTEN2-OUT-AREA
                                          TO  WRK-TENTEKI2-TABLE
             END-IF
           END-IF
      *
      *    点滴注射
           IF   WRK-TENTEKI2-T-ERRCD  =  ZERO
             PERFORM  VARYING  IDY    FROM    1   BY  1
                       UNTIL   IDY     >     31
               IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                 MOVE    ZERO   TO   IDX6
                 PERFORM VARYING IDX5 FROM 1 BY 1
                          UNTIL  IDX5                           >  5
                            OR   WRK-TENTEKI2-T-SRYCD(IDY IDX5) = SPACE
                   ADD   1      TO   IDX6
                   MOVE  WRK-TENTEKI2-T-SRYCD(IDY IDX5)
                                TO   LNK-ORCSTEN3-SYU-SRYCD(IDY IDX6)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU-SURYO(IDY IDX6)
                   PERFORM 2101-TENTEKI-SYUGI-TOTAL-SEC
                   PERFORM VARYING IDX9 FROM 1 BY 1
                            UNTIL  IDX9                       >   5
                              OR   WRK-TENTEKI2-T-COM-SRYCD
                                              (IDY IDX5 IDX9) =  SPACE
                      ADD   1   TO   IDX6
                      MOVE  "810000001"
                                TO   LNK-ORCSTEN3-SYU-SRYCD(IDY IDX6)
                      MOVE  1   TO   LNK-ORCSTEN3-SYU-SURYO(IDY IDX6)
                      MOVE  WRK-TENTEKI2-T-COM-MOJI(IDY IDX5 IDX9)
                                TO   LNK-ORCSTEN3-SYU-COM(IDY IDX6)
                      PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                 UNTIL    IDX13    <    1
                        IF  LNK-ORCSTEN3-SYU-COM(IDY IDX6)(IDX13:1)
                                                         NOT =  SPACE
                          COMPUTE LNK-ORCSTEN3-SYU-YUKOKETA(IDY IDX6) =
                                  IDX13    /    2
                          MOVE    1      TO  IDX13
                        END-IF
                      END-PERFORM
                      ADD   1   TO   LNK-ORCSTEN3-SYU-COMCNT(IDY)
                      PERFORM 2101-TENTEKI-SYUGI-TOTAL-SEC
                   END-PERFORM
                   MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                TO   LNK-ORCSTEN3-SYU-ZAITEN(IDY)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU-ZAIKAISU(IDY)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU-DAY(IDY IDY)
                 END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *
      *    中心静脈注射
           IF   WRK-TENTEKI2-C-ERRCD  =  ZERO
             PERFORM  VARYING  IDY    FROM    1   BY  1
                       UNTIL   IDY     >     31
               IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                 MOVE    ZERO   TO   IDX6
                 PERFORM VARYING IDX5 FROM 1 BY 1
                          UNTIL  IDX5                           >  5
                            OR   WRK-TENTEKI2-C-SRYCD(IDY IDX5) = SPACE
                   ADD   1      TO   IDX6
                   MOVE  WRK-TENTEKI2-C-SRYCD(IDY IDX5)
                                TO   LNK-ORCSTEN3-SYU2-SRYCD(IDY IDX6)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU2-SURYO(IDY IDX6)
                   PERFORM 2102-TENTEKI-SYUGI-TOTAL2-SEC
                   PERFORM VARYING IDX9 FROM 1 BY 1
                            UNTIL  IDX9                       >   5
                              OR   WRK-TENTEKI2-C-COM-SRYCD
                                              (IDY IDX5 IDX9) =  SPACE
                      ADD   1   TO   IDX6
                      MOVE  "810000001"
                                TO   LNK-ORCSTEN3-SYU2-SRYCD(IDY IDX6)
                      MOVE  1   TO   LNK-ORCSTEN3-SYU2-SURYO(IDY IDX6)
                      MOVE  WRK-TENTEKI2-C-COM-MOJI(IDY IDX5 IDX9)
                                TO   LNK-ORCSTEN3-SYU2-COM(IDY IDX6)
                      PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                 UNTIL    IDX13    <    1
                        IF  LNK-ORCSTEN3-SYU2-COM(IDY IDX6)(IDX13:1)
                                                         NOT =  SPACE
                          COMPUTE LNK-ORCSTEN3-SYU2-YUKOKETA(IDY IDX6) =
                                  IDX13    /    2
                          MOVE    1      TO  IDX13
                        END-IF
                      END-PERFORM
                      ADD   1   TO   LNK-ORCSTEN3-SYU2-COMCNT(IDY)
                      PERFORM 2102-TENTEKI-SYUGI-TOTAL2-SEC
                   END-PERFORM
                   MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                TO   LNK-ORCSTEN3-SYU2-ZAITEN(IDY)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU2-ZAIKAISU(IDY)
                   MOVE  1      TO   LNK-ORCSTEN3-SYU2-DAY(IDY IDY)
                 END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *
      *    まとめ対象剤番号編集
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY                       >   40
                       OR    WRK-TENTEKI2-ZAINUM(IDY)  =  ZERO
               MOVE   WRK-TENTEKI2-ZAINUM(IDY)
                                TO   LNK-ORCSTEN3-ZAINUM(IDY)
           END-PERFORM
      *
           .
       210-TENTEKI-SYUGI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射のトータル編集
      *****************************************************************
       2101-TENTEKI-SYUGI-TOTAL-SEC          SECTION.
      *
           MOVE  LNK-ORCSTEN3-SYU-SRYCD(IDY IDX6)
                        TO   WRK-SRYCD2-X
      *
           ADD   1      TO   LNK-ORCSTEN3-SYU-MEISAISU(IDY)
           COMPUTE  LNK-ORCSTEN3-SYU-SRYCDTOTAL(IDY)  =
                    LNK-ORCSTEN3-SYU-SRYCDTOTAL(IDY)  +
                    WRK-SRYCD2
           COMPUTE  LNK-ORCSTEN3-SYU-SURYOUTOTAL(IDY) =
                    LNK-ORCSTEN3-SYU-SURYOUTOTAL(IDY) +
                    LNK-ORCSTEN3-SYU-SURYO(IDY IDX6)
      *
           MOVE     ZERO    TO    WRK-FUKA-KINGAKU
           PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                     UNTIL   FUKA-IDX   >    9
             MOVE   WRK-SRYCD2-X(FUKA-IDX:1)
                            TO    WRK-FUKA-VALUE
             IF  WRK-FUKA-VALUE  =  ZERO
               CONTINUE
             ELSE
               COMPUTE  LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY) =
                        LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY) +
                       (WRK-FUKA-VALUE * WRK-SRYCD2)
               COMPUTE  WRK-FUKA-KINGAKU     =
                        WRK-FUKA-KINGAKU     +
                       ((WRK-FUKA-VALUE^4) *
                         FUKA-IDX          *
                         LNK-ORCSTEN3-SYU-SURYO(IDY IDX6))
             END-IF
           END-PERFORM
           COMPUTE  LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY) =
                    LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY) +
                    WRK-FUKA-KINGAKU
      *
           .
       2101-TENTEKI-SYUGI-TOTAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    中心静脈注射のトータル編集
      *****************************************************************
       2102-TENTEKI-SYUGI-TOTAL2-SEC          SECTION.
      *
           MOVE  LNK-ORCSTEN3-SYU2-SRYCD(IDY IDX6)
                        TO   WRK-SRYCD2-X
      *
           ADD   1      TO   LNK-ORCSTEN3-SYU2-MEISAISU(IDY)
           COMPUTE  LNK-ORCSTEN3-SYU2-SRYCDTOTAL(IDY)  =
                    LNK-ORCSTEN3-SYU2-SRYCDTOTAL(IDY)  +
                    WRK-SRYCD2
           COMPUTE  LNK-ORCSTEN3-SYU2-SURYOUTOTAL(IDY) =
                    LNK-ORCSTEN3-SYU2-SURYOUTOTAL(IDY) +
                    LNK-ORCSTEN3-SYU2-SURYO(IDY IDX6)
      *
           MOVE     ZERO    TO    WRK-FUKA-KINGAKU
           PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                     UNTIL   FUKA-IDX   >    9
             MOVE   WRK-SRYCD2-X(FUKA-IDX:1)
                            TO    WRK-FUKA-VALUE
             IF  WRK-FUKA-VALUE  =  ZERO
               CONTINUE
             ELSE
               COMPUTE  LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY) =
                        LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY) +
                       (WRK-FUKA-VALUE * WRK-SRYCD2)
               COMPUTE  WRK-FUKA-KINGAKU     =
                        WRK-FUKA-KINGAKU     +
                       ((WRK-FUKA-VALUE^4) *
                         FUKA-IDX          *
                         LNK-ORCSTEN3-SYU2-SURYO(IDY IDX6))
             END-IF
           END-PERFORM
           COMPUTE  LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY) =
                    LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY) +
                    WRK-FUKA-KINGAKU
      *
           .
       2102-TENTEKI-SYUGI-TOTAL2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点滴の薬剤及び器材編集
      *****************************************************************
       220-TENTEKI-YAK-HENSYU-SEC          SECTION.
      *
           INITIALIZE                         ORCSTENTEKAREA
           MOVE    "2"                    TO  ORCSTEN-KBN
           MOVE    LNK-ORCSTEN3-PTID      TO  ORCSTEN-PTID
           MOVE    LNK-ORCSTEN3-SRYYM     TO  ORCSTEN-SRYYM
           MOVE    LNK-ORCSTEN3-HKNCOMBI  TO  ORCSTEN-HKNCOMBI
           CALL    "ORCSTENTEK"           USING
                                          SPA-AREA
                                          ORCSTENTEKAREA
      *
      *    薬剤
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY    >  31
             IF  ORCSTEN-ZAITEN(IDY)  NOT =  ZERO
               MOVE     ZERO  TO   IDX6
               PERFORM  VARYING  IDX5    FROM    1   BY  1
                         UNTIL   IDX5                    >  50
                           OR    ORCSTEN-SRYCD(IDY IDX5) = SPACE
                 ADD   1      TO   IDX6
                 MOVE  ORCSTEN-SRYCD(IDY IDX5)
                              TO   LNK-ORCSTEN3-YAK-SRYCD(IDY IDX6)
                 MOVE  ORCSTEN-SURYO(IDY IDX5)
                              TO   LNK-ORCSTEN3-YAK-SURYO(IDY IDX6)
                 PERFORM 2201-TENTEKI-YAK-TOTAL-SEC
                 IF  ORCSTEN-HAIK-SRYCD(IDY IDX5) NOT = SPACE
                    ADD   1   TO   IDX6
                    MOVE  ORCSTEN-HAIK-SRYCD(IDY IDX5)
                              TO   LNK-ORCSTEN3-YAK-SRYCD(IDY IDX6)
                    MOVE  ORCSTEN-HAIK-SURYO(IDY IDX5)
                              TO   LNK-ORCSTEN3-YAK-SURYO(IDY IDX6)
                    PERFORM 2201-TENTEKI-YAK-TOTAL-SEC
                 END-IF
                 PERFORM  VARYING  IDX9  FROM  1  BY  1
                           UNTIL   IDX9    >   5
                             OR    ORCSTEN-COM-SRYCD(IDY IDX5 IDX9)
                                           =  SPACE
                    ADD   1   TO   IDX6
                    MOVE  "810000001"
                              TO   LNK-ORCSTEN3-YAK-SRYCD(IDY IDX6)
                    MOVE  1   TO   LNK-ORCSTEN3-YAK-SURYO(IDY IDX6)
                    MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX9)
                              TO   LNK-ORCSTEN3-YAK-COM(IDY IDX6)
                    PERFORM   VARYING   IDX13   FROM   200   BY  -1
                               UNTIL    IDX13    <    1
                      IF  LNK-ORCSTEN3-YAK-COM(IDY IDX6)(IDX13:1)
                                                       NOT =  SPACE
                        COMPUTE  LNK-ORCSTEN3-YAK-YUKOKETA(IDY IDX6) =
                                 IDX13    /    2
                        MOVE     1      TO  IDX13
                      END-IF
                    END-PERFORM
                    ADD   1   TO   LNK-ORCSTEN3-YAK-COMCNT(IDY)
                    PERFORM 2201-TENTEKI-YAK-TOTAL-SEC
                 END-PERFORM
                 MOVE  ORCSTEN-ZAITEN(IDY)
                              TO   LNK-ORCSTEN3-YAK-ZAITEN(IDY)
                 MOVE  1      TO   LNK-ORCSTEN3-YAK-ZAIKAISU(IDY)
                 MOVE  1      TO   LNK-ORCSTEN3-YAK-DAY(IDY IDY)
               END-PERFORM
             END-IF
           END-PERFORM
      *
      *    器材
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY                      >   50
                       OR    ORCSTEN-KIZ-ZAITEN(IDY)  =  ZERO
             MOVE     ZERO  TO   IDX6
             PERFORM  VARYING  IDX5    FROM    1   BY  1
                       UNTIL   IDX5                        >  10
                         OR    ORCSTEN-KIZ-SRYCD(IDY IDX5) = SPACE
               ADD   1      TO   IDX6
               MOVE  ORCSTEN-KIZ-SRYCD(IDY IDX5)
                            TO   LNK-ORCSTEN3-KIZ-SRYCD(IDY IDX6)
               MOVE  ORCSTEN-KIZ-SURYO(IDY IDX5)
                            TO   LNK-ORCSTEN3-KIZ-SURYO(IDY IDX6)
               PERFORM  VARYING  IDX9  FROM  1  BY  1
                         UNTIL   IDX9    >   5
                           OR    ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX9)
                                         =  SPACE
                  ADD   1   TO   IDX6
                  MOVE  "810000001"
                            TO   LNK-ORCSTEN3-KIZ-SRYCD(IDY IDX6)
                  MOVE  1   TO   LNK-ORCSTEN3-KIZ-SURYO(IDY IDX6)
                  MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX9)
                            TO   LNK-ORCSTEN3-KIZ-COM(IDY IDX6)
                  PERFORM   VARYING   IDX13   FROM   200   BY  -1
                             UNTIL    IDX13    <    1
                    IF  LNK-ORCSTEN3-KIZ-COM(IDY IDX6)(IDX13:1)
                                                     NOT =  SPACE
                      COMPUTE  LNK-ORCSTEN3-KIZ-YUKOKETA(IDY IDX6) =
                               IDX13    /    2
                      MOVE     1      TO  IDX13
                    END-IF
                  END-PERFORM
               END-PERFORM
             END-PERFORM
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                            TO   LNK-ORCSTEN3-KIZ-ZAITEN(IDY)
             PERFORM  VARYING  IDX5    FROM    1   BY  1
                       UNTIL   IDX5      >    31
               ADD   ORCSTEN-KIZ-DAY(IDY IDX5)
                            TO   LNK-ORCSTEN3-KIZ-ZAIKAISU(IDY)
               MOVE  ORCSTEN-KIZ-DAY(IDY IDX5)
                            TO   LNK-ORCSTEN3-KIZ-DAY(IDY IDX5)
             END-PERFORM
           END-PERFORM
      *
           .
       220-TENTEKI-YAK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点滴薬剤のトータル編集
      *****************************************************************
       2201-TENTEKI-YAK-TOTAL-SEC          SECTION.
      *
           MOVE  LNK-ORCSTEN3-YAK-SRYCD(IDY IDX6)
                        TO   WRK-SRYCD2-X
      *
           ADD   1      TO   LNK-ORCSTEN3-YAK-MEISAISU(IDY)
           COMPUTE  LNK-ORCSTEN3-YAK-SRYCDTOTAL(IDY)  =
                    LNK-ORCSTEN3-YAK-SRYCDTOTAL(IDY)  +
                    WRK-SRYCD2
           COMPUTE  LNK-ORCSTEN3-YAK-SURYOUTOTAL(IDY) =
                    LNK-ORCSTEN3-YAK-SURYOUTOTAL(IDY) +
                    LNK-ORCSTEN3-YAK-SURYO(IDY IDX6)
      *
           MOVE     ZERO    TO    WRK-FUKA-KINGAKU
           PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                     UNTIL   FUKA-IDX   >    9
             MOVE   WRK-SRYCD2-X(FUKA-IDX:1)
                            TO    WRK-FUKA-VALUE
             IF  WRK-FUKA-VALUE  =  ZERO
               CONTINUE
             ELSE
               COMPUTE  LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY) =
                        LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY) +
                       (WRK-FUKA-VALUE * WRK-SRYCD2)
               COMPUTE  WRK-FUKA-KINGAKU     =
                        WRK-FUKA-KINGAKU     +
                       ((WRK-FUKA-VALUE^4) *
                         FUKA-IDX          *
                         LNK-ORCSTEN3-YAK-SURYO(IDY IDX6))
             END-IF
           END-PERFORM
           COMPUTE  LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY) =
                    LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY) +
                    WRK-FUKA-KINGAKU
      *
           .
       2201-TENTEKI-YAK-TOTAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    まとめ対象剤番号編集
      *****************************************************************
       230-TENTEKI-ZAINUM-HENSYU-SEC          SECTION.
      *
           INITIALIZE                             SRYACCT-REC
           MOVE    LNK-ORCSTEN3-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    LNK-ORCSTEN3-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN3-PTID          TO  ACCT-PTID
           MOVE    LNK-ORCSTEN3-SRYYM         TO  ACCT-SRYYM
           MOVE    LNK-ORCSTEN3-HKNCOMBI      TO  ACCT-HKNCOMBI
           MOVE    "33"                       TO  ACCT-SRYKBN
           MOVE    SRYACCT-REC                TO  MCPDATA-REC
           MOVE    "tbl_sryacct"              TO  MCP-TABLE
           MOVE    "key65"                    TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
              MOVE    "tbl_sryacct"           TO  MCP-TABLE
              MOVE    "key65"                 TO  MCP-PATHNAME
              PERFORM  900-SRYACCT-READ-SEC
              PERFORM  UNTIL  FLG-SRYACCT =  1
      *
                PERFORM  2301-TENTEKI-ZAINUM-GET-SEC
      *
                MOVE    "tbl_sryacct"         TO  MCP-TABLE
                MOVE    "key65"               TO  MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_sryacct"              TO  MCP-TABLE
           MOVE    "key65"                    TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       230-TENTEKI-ZAINUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    まとめ対象剤番号編集
      *****************************************************************
       2301-TENTEKI-ZAINUM-GET-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-OK
           MOVE    ZERO                  TO  WRK-ZAINUM
      *
           INITIALIZE                        SRYACT-REC
           MOVE    ACCT-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN        TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID             TO  SRY-PTID
           MOVE    ACCT-SRYYM            TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM           TO  SRY-ZAINUM
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key9"                TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
              MOVE    "tbl_sryact"       TO  MCP-TABLE
              MOVE    "key9"             TO  MCP-PATHNAME
              PERFORM  900-SRYACT-READ-SEC
              PERFORM  UNTIL  FLG-SRYACT =  1
                         OR   FLG-OK     =  1
      *
                PERFORM  23011-TENTEKI-ZAINUM-GET2-SEC
      *
                MOVE    "tbl_sryact"     TO  MCP-TABLE
                MOVE    "key9"           TO  MCP-PATHNAME
                PERFORM  900-SRYACT-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key9"                TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           IF   FLG-OK  =  1
              MOVE     ZERO         TO  FLG-ARI
              PERFORM  VARYING  IDY  FROM  1  BY  1
                        UNTIL  (IDY                      > 100 )
                          OR   (LNK-ORCSTEN3-ZAINUM(IDY) = ZERO)
                          OR   (FLG-ARI                  =  1  )
                IF   WRK-ZAINUM  =  LNK-ORCSTEN3-ZAINUM(IDY)
                   MOVE   1         TO  FLG-ARI
                END-IF
              END-PERFORM
              IF    FLG-ARI  =  ZERO
              AND   IDY     <=  100
                MOVE   WRK-ZAINUM   TO  LNK-ORCSTEN3-ZAINUM(IDY)
              END-IF
           END-IF
      *
           .
       2301-TENTEKI-ZAINUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    まとめ対象剤番号編集
      *****************************************************************
       23011-TENTEKI-ZAINUM-GET2-SEC          SECTION.
      *
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                     UNTIL   IDX5            >   5
                       OR    SRY-SRYCD(IDX5) =  SPACE
                       OR    FLG-OK          =   1
      *
             IF  ( SRY-SRYSYUKBN  =
                   "330" OR "331" OR "332" OR "350" OR "352" )
             AND ((SRY-SRYCD(IDX5)(1:3)  = "059"             ) OR
                  (SRY-SRYCD(IDX5)(1:1)  = "7"               ) OR
                  (SRY-SRYCD(IDX5)       = "099309901"       ) OR
                  (SRY-SRYCD(IDX5)(1:1)  = "6"               ))
             AND ( ACCT-ZAITEN      NOT =  ZERO              )
      *******AND ( ACCT-SYUTEN1         =  ZERO              )
             AND ( ACCT-ZAIKAISU    NOT =  ZERO              )
             AND ( ACCT-JIHOKBN         =  SPACE             )
      *
                MOVE    1             TO   FLG-OK
                MOVE    ACCT-ZAINUM   TO   WRK-ZAINUM
      *
             END-IF
      *
           END-PERFORM
      *
           .
       23011-TENTEKI-ZAINUM-GET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤まとめ編集
      *****************************************************************
       240-TENTEKI-ZAI-MATOME-SEC          SECTION.
      *
      *    点滴注射
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY    >  30
      *
             IF   LNK-ORCSTEN3-SYU-ZAITEN(IDY)  NOT =  ZERO
               COMPUTE  IDY2  =  IDY  +  1
               PERFORM  VARYING  IDY3  FROM  IDY2  BY  1
                         UNTIL   IDY3    >    31
                 MOVE    ZERO    TO   FLG-OK
                 IF  (LNK-ORCSTEN3-SYU-ZAITEN(IDY)      =
                                 LNK-ORCSTEN3-SYU-ZAITEN(IDY3)     )
                 AND (LNK-ORCSTEN3-SYU-MEISAISU(IDY)    =
                                 LNK-ORCSTEN3-SYU-MEISAISU(IDY3)   )
                 AND (LNK-ORCSTEN3-SYU-SRYCDTOTAL(IDY)  =
                                 LNK-ORCSTEN3-SYU-SRYCDTOTAL(IDY3) )
                 AND (LNK-ORCSTEN3-SYU-SURYOUTOTAL(IDY) =
                                 LNK-ORCSTEN3-SYU-SURYOUTOTAL(IDY3))
                 AND (LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY) =
                                 LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY3))
                    IF   (LNK-ORCSTEN3-SYU-COMCNT(IDY)  =  ZERO)
                    AND  (LNK-ORCSTEN3-SYU-COMCNT(IDY3) =  ZERO)
                         MOVE    1     TO   FLG-OK
                    ELSE
                      IF  LNK-ORCSTEN3-SYU-COMCNT(IDY)  =
                                   LNK-ORCSTEN3-SYU-COMCNT(IDY3)
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  30
                           IF  LNK-ORCSTEN3-SYU-SRYCD(IDY IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-SYU-COM(IDY IDY4)
                                       TO  WRK-COM(IDX)
                           END-IF
                         END-PERFORM
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM2-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  30
                           IF  LNK-ORCSTEN3-SYU-SRYCD(IDY3 IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM2-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-SYU-COM(IDY3 IDY4)
                                       TO  WRK-COM2(IDX)
                           END-IF
                         END-PERFORM
                         PERFORM  2401-TENTEKI-COM-CHECK-SEC
                      END-IF
                    END-IF
                    IF   FLG-OK  =  1
                      ADD  LNK-ORCSTEN3-SYU-ZAIKAISU(IDY3)
                               TO  LNK-ORCSTEN3-SYU-ZAIKAISU(IDY)
                      ADD  LNK-ORCSTEN3-SYU-DAY(IDY3 IDY3)
                               TO  LNK-ORCSTEN3-SYU-DAY(IDY IDY3)
                      INITIALIZE   LNK-ORCSTEN3-SYU-OCC(IDY3)
                    END-IF
                 END-IF
               END-PERFORM
             END-IF
      *
           END-PERFORM
      *
      *    中心静脈注射
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY    >  30
      *
             IF   LNK-ORCSTEN3-SYU2-ZAITEN(IDY)  NOT =  ZERO
               COMPUTE  IDY2  =  IDY  +  1
               PERFORM  VARYING  IDY3  FROM  IDY2  BY  1
                         UNTIL   IDY3    >    31
                 MOVE    ZERO    TO   FLG-OK
                 IF  (LNK-ORCSTEN3-SYU2-ZAITEN(IDY)      =
                                 LNK-ORCSTEN3-SYU2-ZAITEN(IDY3)     )
                 AND (LNK-ORCSTEN3-SYU2-MEISAISU(IDY)    =
                                 LNK-ORCSTEN3-SYU2-MEISAISU(IDY3)   )
                 AND (LNK-ORCSTEN3-SYU2-SRYCDTOTAL(IDY)  =
                                 LNK-ORCSTEN3-SYU2-SRYCDTOTAL(IDY3) )
                 AND (LNK-ORCSTEN3-SYU2-SURYOUTOTAL(IDY) =
                                 LNK-ORCSTEN3-SYU2-SURYOUTOTAL(IDY3))
                 AND (LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY) =
                                 LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY3))
                    IF   (LNK-ORCSTEN3-SYU2-COMCNT(IDY)  =  ZERO)
                    AND  (LNK-ORCSTEN3-SYU2-COMCNT(IDY3) =  ZERO)
                         MOVE    1     TO   FLG-OK
                    ELSE
                      IF  LNK-ORCSTEN3-SYU2-COMCNT(IDY)  =
                                   LNK-ORCSTEN3-SYU2-COMCNT(IDY3)
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  30
                           IF  LNK-ORCSTEN3-SYU2-SRYCD(IDY IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-SYU2-COM(IDY IDY4)
                                       TO  WRK-COM(IDX)
                           END-IF
                         END-PERFORM
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM2-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  30
                           IF  LNK-ORCSTEN3-SYU2-SRYCD(IDY3 IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM2-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-SYU2-COM(IDY3 IDY4)
                                       TO  WRK-COM2(IDX)
                           END-IF
                         END-PERFORM
                         PERFORM  2401-TENTEKI-COM-CHECK-SEC
                      END-IF
                    END-IF
                    IF   FLG-OK  =  1
                      ADD  LNK-ORCSTEN3-SYU2-ZAIKAISU(IDY3)
                               TO  LNK-ORCSTEN3-SYU2-ZAIKAISU(IDY)
                      ADD  LNK-ORCSTEN3-SYU2-DAY(IDY3 IDY3)
                               TO  LNK-ORCSTEN3-SYU2-DAY(IDY IDY3)
                      INITIALIZE   LNK-ORCSTEN3-SYU2-OCC(IDY3)
                    END-IF
                 END-IF
               END-PERFORM
             END-IF
      *
           END-PERFORM
      *
      *    薬剤
           PERFORM  VARYING  IDY  FROM  1  BY  1
                     UNTIL   IDY    >  30
      *
             IF   LNK-ORCSTEN3-YAK-ZAITEN(IDY)  NOT =  ZERO
               COMPUTE  IDY2  =  IDY  +  1
               PERFORM  VARYING  IDY3  FROM  IDY2  BY  1
                         UNTIL   IDY3    >    31
                 MOVE    ZERO    TO   FLG-OK
                 IF  (LNK-ORCSTEN3-YAK-ZAITEN(IDY)      =
                                 LNK-ORCSTEN3-YAK-ZAITEN(IDY3)     )
                 AND (LNK-ORCSTEN3-YAK-MEISAISU(IDY)    =
                                 LNK-ORCSTEN3-YAK-MEISAISU(IDY3)   )
                 AND (LNK-ORCSTEN3-YAK-SRYCDTOTAL(IDY)  =
                                 LNK-ORCSTEN3-YAK-SRYCDTOTAL(IDY3) )
                 AND (LNK-ORCSTEN3-YAK-SURYOUTOTAL(IDY) =
                                 LNK-ORCSTEN3-YAK-SURYOUTOTAL(IDY3))
                 AND (LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY) =
                                 LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY3))
                    IF   (LNK-ORCSTEN3-YAK-COMCNT(IDY)  =  ZERO)
                    AND  (LNK-ORCSTEN3-YAK-COMCNT(IDY3) =  ZERO)
                         MOVE    1     TO   FLG-OK
                    ELSE
                      IF  LNK-ORCSTEN3-YAK-COMCNT(IDY)  =
                                   LNK-ORCSTEN3-YAK-COMCNT(IDY3)
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  350
                           IF  LNK-ORCSTEN3-YAK-SRYCD(IDY IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-YAK-COM(IDY IDY4)
                                       TO  WRK-COM(IDX)
                           END-IF
                         END-PERFORM
                         MOVE    ZERO  TO   IDX
                         INITIALIZE    WRK-COM2-TABLE
                         PERFORM  VARYING  IDY4  FROM  1  BY  1
                                   UNTIL   IDY4    >  350
                           IF  LNK-ORCSTEN3-YAK-SRYCD(IDY3 IDY4)
                                                   =  "810000001"
                              ADD   1  TO  IDX
                              MOVE  1  TO  WRK-COM2-FLG(IDX)
                              MOVE  LNK-ORCSTEN3-YAK-COM(IDY3 IDY4)
                                       TO  WRK-COM2(IDX)
                           END-IF
                         END-PERFORM
                         PERFORM  2401-TENTEKI-COM-CHECK-SEC
                      END-IF
                    END-IF
                    IF   FLG-OK  =  1
                      ADD  LNK-ORCSTEN3-YAK-ZAIKAISU(IDY3)
                               TO  LNK-ORCSTEN3-YAK-ZAIKAISU(IDY)
                      ADD  LNK-ORCSTEN3-YAK-DAY(IDY3 IDY3)
                               TO  LNK-ORCSTEN3-YAK-DAY(IDY IDY3)
                      INITIALIZE   LNK-ORCSTEN3-YAK-OCC(IDY3)
                    END-IF
                 END-IF
               END-PERFORM
             END-IF
      *
           END-PERFORM
      *
           .
       240-TENTEKI-ZAI-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点滴の全体まとめ編集
      *****************************************************************
       250-TENTEKI-ALL-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDM
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   31
      *
             IF  ( LNK-ORCSTEN3-SYU-ZAITEN (IDY)   NOT =   ZERO )
               COMPUTE IDM = IDM + 1
               MOVE    "1"     TO  LNK-ORCSTEN3-SBT (IDM)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL ( IDY2    >   30 )
                        OR   ( LNK-ORCSTEN3-SYU-SRYCD (IDY IDY2)
                                       =   SPACE )
      *
                   COMPUTE LNK-ORCSTEN3-OCC2-MAX (IDM)
                       =   LNK-ORCSTEN3-OCC2-MAX (IDM) +   1
      *
                   MOVE  LNK-ORCSTEN3-SYU-SRYCD    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SRYCD      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU-SURYO    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SURYO      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU-YUKOKETA (IDY IDY2)
                               TO  LNK-ORCSTEN3-YUKOKETA   (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU-COM      (IDY IDY2)  
                               TO  LNK-ORCSTEN3-COM        (IDM IDY2)
               END-PERFORM
               MOVE    LNK-ORCSTEN3-SYU-ZAITEN     (IDY)
                               TO  LNK-ORCSTEN3-ZAITEN     (IDM)
               MOVE    LNK-ORCSTEN3-SYU-ZAIKAISU   (IDY)
                               TO  LNK-ORCSTEN3-ZAIKAISU   (IDM)
               MOVE    LNK-ORCSTEN3-SYU-DAY-G      (IDY)
                               TO  LNK-ORCSTEN3-DAY-G      (IDM)
               MOVE    LNK-ORCSTEN3-SYU-MEISAISU   (IDY)
                               TO  LNK-ORCSTEN3-MEISAISU   (IDM)
               MOVE    LNK-ORCSTEN3-SYU-SRYCDTOTAL (IDY)
                               TO  LNK-ORCSTEN3-SRYCDTOTAL (IDM)
               MOVE    LNK-ORCSTEN3-SYU-SURYOUTOTAL(IDY)
                               TO  LNK-ORCSTEN3-SURYOUTOTAL(IDM)
               MOVE    LNK-ORCSTEN3-SYU-RUIKEITOTAL(IDY)
                               TO  LNK-ORCSTEN3-RUIKEITOTAL(IDM)
               MOVE    LNK-ORCSTEN3-SYU-COMCNT     (IDY)
                               TO  LNK-ORCSTEN3-COMCNT     (IDM)
      *
             END-IF
           END-PERFORM
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   31
      *
             IF  ( LNK-ORCSTEN3-SYU2-ZAITEN (IDY)   NOT =   ZERO )
               COMPUTE IDM = IDM + 1
               MOVE    "2"     TO  LNK-ORCSTEN3-SBT (IDM)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL ( IDY2    >   30 )
                        OR   ( LNK-ORCSTEN3-SYU2-SRYCD (IDY IDY2)
                                       =   SPACE )
      *
                   COMPUTE LNK-ORCSTEN3-OCC2-MAX (IDM)
                       =   LNK-ORCSTEN3-OCC2-MAX (IDM) +   1
      *
                   MOVE  LNK-ORCSTEN3-SYU2-SRYCD    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SRYCD      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU2-SURYO    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SURYO      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU2-YUKOKETA (IDY IDY2)
                               TO  LNK-ORCSTEN3-YUKOKETA   (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-SYU2-COM      (IDY IDY2)  
                               TO  LNK-ORCSTEN3-COM        (IDM IDY2)
               END-PERFORM
               MOVE    LNK-ORCSTEN3-SYU2-ZAITEN     (IDY)
                               TO  LNK-ORCSTEN3-ZAITEN     (IDM)
               MOVE    LNK-ORCSTEN3-SYU2-ZAIKAISU   (IDY)
                               TO  LNK-ORCSTEN3-ZAIKAISU   (IDM)
               MOVE    LNK-ORCSTEN3-SYU2-DAY-G      (IDY)
                               TO  LNK-ORCSTEN3-DAY-G      (IDM)
               MOVE    LNK-ORCSTEN3-SYU2-MEISAISU   (IDY)
                               TO  LNK-ORCSTEN3-MEISAISU   (IDM)
               MOVE    LNK-ORCSTEN3-SYU2-SRYCDTOTAL (IDY)
                               TO  LNK-ORCSTEN3-SRYCDTOTAL (IDM)
               MOVE    LNK-ORCSTEN3-SYU2-SURYOUTOTAL(IDY)
                               TO  LNK-ORCSTEN3-SURYOUTOTAL(IDM)
               MOVE    LNK-ORCSTEN3-SYU2-RUIKEITOTAL(IDY)
                               TO  LNK-ORCSTEN3-RUIKEITOTAL(IDM)
               MOVE    LNK-ORCSTEN3-SYU2-COMCNT     (IDY)
                               TO  LNK-ORCSTEN3-COMCNT     (IDM)
      *
             END-IF
           END-PERFORM
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   31
      *
             IF  ( LNK-ORCSTEN3-YAK-ZAITEN (IDY)   NOT =   ZERO )
               COMPUTE IDM = IDM + 1
               MOVE    "3"     TO  LNK-ORCSTEN3-SBT (IDM)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL ( IDY2    >   350 )
                        OR   ( LNK-ORCSTEN3-YAK-SRYCD (IDY IDY2)
                                       =   SPACE )
      *
                   COMPUTE LNK-ORCSTEN3-OCC2-MAX (IDM)
                       =   LNK-ORCSTEN3-OCC2-MAX (IDM) +   1
      *
                   MOVE  LNK-ORCSTEN3-YAK-SRYCD    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SRYCD      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-YAK-SURYO    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SURYO      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-YAK-YUKOKETA (IDY IDY2)
                               TO  LNK-ORCSTEN3-YUKOKETA   (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-YAK-COM      (IDY IDY2)  
                               TO  LNK-ORCSTEN3-COM        (IDM IDY2)
               END-PERFORM
               MOVE    LNK-ORCSTEN3-YAK-ZAITEN     (IDY)
                               TO  LNK-ORCSTEN3-ZAITEN     (IDM)
               MOVE    LNK-ORCSTEN3-YAK-ZAIKAISU   (IDY)
                               TO  LNK-ORCSTEN3-ZAIKAISU   (IDM)
               MOVE    LNK-ORCSTEN3-YAK-DAY-G      (IDY)
                               TO  LNK-ORCSTEN3-DAY-G      (IDM)
               MOVE    LNK-ORCSTEN3-YAK-MEISAISU   (IDY)
                               TO  LNK-ORCSTEN3-MEISAISU   (IDM)
               MOVE    LNK-ORCSTEN3-YAK-SRYCDTOTAL (IDY)
                               TO  LNK-ORCSTEN3-SRYCDTOTAL (IDM)
               MOVE    LNK-ORCSTEN3-YAK-SURYOUTOTAL(IDY)
                               TO  LNK-ORCSTEN3-SURYOUTOTAL(IDM)
               MOVE    LNK-ORCSTEN3-YAK-RUIKEITOTAL(IDY)
                               TO  LNK-ORCSTEN3-RUIKEITOTAL(IDM)
               MOVE    LNK-ORCSTEN3-YAK-COMCNT     (IDY)
                               TO  LNK-ORCSTEN3-COMCNT     (IDM)
      *
             END-IF
           END-PERFORM
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >   50
      *
             IF  ( LNK-ORCSTEN3-KIZ-ZAITEN (IDY)   NOT =   ZERO )
               COMPUTE IDM = IDM + 1
               MOVE    "4"     TO  LNK-ORCSTEN3-SBT (IDM)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL ( IDY2    >   60 )
                        OR   ( LNK-ORCSTEN3-KIZ-SRYCD (IDY IDY2)
                                       =   SPACE )
      *
                   COMPUTE LNK-ORCSTEN3-OCC2-MAX (IDM)
                       =   LNK-ORCSTEN3-OCC2-MAX (IDM) +   1
      *
                   MOVE  LNK-ORCSTEN3-KIZ-SRYCD    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SRYCD      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-KIZ-SURYO    (IDY IDY2)
                               TO  LNK-ORCSTEN3-SURYO      (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-KIZ-YUKOKETA (IDY IDY2)
                               TO  LNK-ORCSTEN3-YUKOKETA   (IDM IDY2)
                   MOVE  LNK-ORCSTEN3-KIZ-COM      (IDY IDY2)  
                               TO  LNK-ORCSTEN3-COM        (IDM IDY2)
               END-PERFORM
               MOVE    LNK-ORCSTEN3-KIZ-ZAITEN     (IDY)
                               TO  LNK-ORCSTEN3-ZAITEN     (IDM)
               MOVE    LNK-ORCSTEN3-KIZ-ZAIKAISU   (IDY)
                               TO  LNK-ORCSTEN3-ZAIKAISU   (IDM)
               MOVE    LNK-ORCSTEN3-KIZ-DAY-G      (IDY)
                               TO  LNK-ORCSTEN3-DAY-G      (IDM)
      *
             END-IF
           END-PERFORM
      *
           MOVE    IDM         TO  LNK-ORCSTEN3-MAX
      *
           .
       250-TENTEKI-ALL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    剤まとめコメントチェック
      *****************************************************************
       2401-TENTEKI-COM-CHECK-SEC          SECTION.
      *
           MOVE     ZERO        TO    FLG-NASHI
           INITIALIZE                 WRK-IDX-TABLE
      *
           PERFORM  VARYING   IDY5   FROM    1    BY   1
                     UNTIL   (IDY5                >  350 )
                       OR    (WRK-COM2-FLG(IDY5)  =  ZERO)
      *
              MOVE    ZERO      TO    FLG-ARI
              PERFORM VARYING IDY6   FROM    1    BY   1
                       UNTIL (IDY6                >  350 )
                         OR  (WRK-COM-FLG(IDY6)   =  ZERO)
                 IF  WRK-COM2(IDY5)  =  WRK-COM(IDY6)
                     MOVE    ZERO      TO   FLG-NG
                     PERFORM VARYING IDY7  FROM  1  BY  1
                              UNTIL (IDY7          >  350 )
                                OR  (WRK-IDX(IDY7) =  ZERO)
                                OR  (FLG-NG        =  1   )
                        IF  IDY6 = WRK-IDX(IDY7)
                          MOVE  1      TO   FLG-NG
                        END-IF
                     END-PERFORM
                     IF   FLG-NG = ZERO
                        MOVE    1      TO   FLG-ARI
                        MOVE    IDY6   TO   WRK-IDX(IDY7)
                        MOVE    350    TO   IDY6
                     END-IF
                 END-IF
              END-PERFORM
              IF     FLG-ARI  =  ZERO
                 MOVE     1     TO     FLG-NASHI
                 MOVE     350   TO     IDY5
              END-IF
      *
           END-PERFORM
      *
           IF    FLG-NASHI    =  ZERO
                 MOVE     1     TO    FLG-OK
           END-IF
      *
           .
       2401-TENTEKI-COM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読み込み
      *****************************************************************
       900-HKNCOMBI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       900-SRYACCT-READ-SEC            SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ読み込み
      *****************************************************************
       900-SRYACT-READ-SEC             SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table="  MCP-TABLE
                       " pathname="         MCP-PATHNAME
           END-IF
           .
      *
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-CLOSE-SEC                    SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
