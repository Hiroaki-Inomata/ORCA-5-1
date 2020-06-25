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
       PROGRAM-ID.             ORCSSYOTOKU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 患者所得情報編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-門脇    09/07/28  所得者情報の機能追加
      *                                     （標準負担額のみの減額）
      *
      *  04.08.01    NACL-門脇    15/10/21  現物給付対象外の低所得（食事）対応
      *  04.08.02    NACL-門脇    17/08/01  平成２９年１０月改正
      *                                     （入院時生活療養費の見直し）
      *  04.08.03    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
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
           03  FLG-TNKRRK            PIC 9(01).
           03  FLG-TSYRRK            PIC 9(01).
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-PTINF             PIC 9(01).
           03  FLG-NYUINACCT         PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-CHECK             PIC 9(01).
           03  FLG-CHECK2            PIC 9(01).
           03  FLG-ARI               PIC 9(01).
           03  FLG-ARI2              PIC 9(01).
           03  FLG-OK                PIC 9(01).
           03  FLG-PARA              PIC 9(01).
           03  FLG-OKIKAE            PIC 9(01).
           03  FLG-SRYACCT           PIC 9(01).
           03  FLG-SRYACT            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(02).
           03  IDX2                  PIC 9(02).
           03  IDX3                  PIC 9(02).
           03  IDX4                  PIC 9(02).
           03  IDX5                  PIC 9(03).
           03  IDX-ST                PIC 9(02).
           03  IDX-ED                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DAY-TABLE.
             05  WRK-DAY-T             OCCURS   31.
               07  WRK-DAY-SYU-TNK     PIC X(01).
               07  WRK-DAY-SYU-TSY     PIC X(01).
               07  WRK-DAY-CHI-TNK     PIC X(01).
               07  WRK-DAY-CHI-TSY     PIC X(01).
               07  WRK-DAY-SKJ-TNK     PIC X(01).
               07  WRK-DAY-SKJ-TSY     PIC X(01).
               07  WRK-DAY-KKS-TNK     PIC X(01).
               07  WRK-DAY-KKS-TSY     PIC X(01).
      *
               07  WRK-DAY-KOH966      PIC X(01).
               07  WRK-DAY-KOH967      PIC X(01).
               07  WRK-DAY-KOH956      PIC X(01).
               07  WRK-DAY-KOH957      PIC X(01).
      *
               07  WRK-DAY-KOH968      PIC X(01).
               07  WRK-DAY-KOH976      PIC X(01).
      *
               07  WRK-DAY-HKNCOMBI              PIC 9(04).
               07  WRK-DAY-HKNCOMBIINF.
                 09  WRK-DAY-HKNNUM              PIC X(03).
                 09  WRK-DAY-KOH1HKNNUM          PIC X(03).
                 09  WRK-DAY-HKNKOHSBTKBN        PIC X(01).
                 09  WRK-DAY-HOJOKBN             PIC X(01).
                 09  WRK-DAY-SETFLG              PIC 9(01).
      *
               07  WRK-DAY-NENREIFLG             PIC X(01).
      *
               07  WRK-DAY-HKNCOMBI-DOU          PIC 9(04).
               07  WRK-DAY-HKNCOMBI-SKJHKN       PIC 9(04).
               07  WRK-DAY-HKNCOMBI-SKJHKN-DOU   PIC 9(04).
      *
           03  WRK-SYOTOKU-YMD.
               05  WRK-SYOTOKU-YM      PIC 9(06).
               05  WRK-SYOTOKU-DD      PIC 9(02).
      *
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-NENREI              PIC 9(03).
      *
           03  WRK-FIRST-DAY           PIC 9(02).
           03  WRK-FIRST-SKJKBN        PIC X(01).
      *
           03  WRK-CHECK-TABLE.
             05  WRK-CHECK-T           OCCURS   31.
               07  WRK-CHECK-HKNCOMBI  PIC 9(04).
               07  WRK-CHECK-FLG       PIC 9(01).
      *
           03  WRK-TAIHI-HKNCOMBI      PIC 9(04).
      *
           03  WRK-CHECK2-TABLE.
             05  WRK-CHECK2-T          OCCURS  100.
               07  WRK-CHECK2-HKNCOMBI           PIC 9(04).
               07  WRK-CHECK2-HKNCOMBIINF.
                 09  WRK-CHECK2-HKNNUM           PIC X(03).
                 09  WRK-CHECK2-KOH1HKNNUM       PIC X(03).
                 09  WRK-CHECK2-HKNKOHSBTKBN     PIC X(01).
                 09  WRK-CHECK2-HOJOKBN          PIC X(01).
                 09  WRK-CHECK2-SETFLG           PIC 9(01).
      *
      *    固定値
       01  WRK-CONS-AREA.
      *
           03  WRK-CONS-SRYYM-200610   PIC 9(06)   VALUE   200610.
           03  WRK-CONS-SRYYM-200604   PIC 9(06)   VALUE   200604.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    低１
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    低２
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    特定疾患等算定判定サブ
           COPY    "CPORCSSRHKOH.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSYOTOKU.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSSYOTOKUAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-SYOTOKU-RC
           INITIALIZE                      LNK-SYOTOKU-HENKYAKU-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメタチェック
           PERFORM   100-PRM-CHK-SEC
           IF    LNK-SYOTOKU-RC   NOT =   ZERO
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-SYOTOKU-RC       =   ZERO
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
      *    処理年月チェック
           IF      LNK-SYOTOKU-SRYYM    =   SPACE
               MOVE    1                 TO  LNK-SYOTOKU-RC
           END-IF
      *
      *    患者ＩＤチェック
           IF  LNK-SYOTOKU-RC  =  ZERO
             IF    LNK-SYOTOKU-PTID     =   ZERO
               MOVE    2                 TO  LNK-SYOTOKU-RC
             ELSE
               MOVE    SPACE                TO  WRK-BIRTHDAY
               INITIALIZE                       PTINF-REC
               MOVE    LNK-SYOTOKU-HOSPNUM  TO  PTINF-HOSPNUM
               MOVE    LNK-SYOTOKU-PTID     TO  PTINF-PTID
               MOVE    PTINF-REC            TO  MCPDATA-REC
               MOVE    "DBSELECT"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC  =  ZERO
                  MOVE      "tbl_ptinf"     TO  MCP-TABLE
                  MOVE      "key"           TO  MCP-PATHNAME
                  PERFORM    910-PTINF-READ-SEC
                  IF   FLG-PTINF  =  ZERO
                     MOVE    PTINF-BIRTHDAY TO  WRK-BIRTHDAY
                  ELSE
                     MOVE    2              TO  LNK-SYOTOKU-RC
                  END-IF
               ELSE
                     MOVE    2              TO  LNK-SYOTOKU-RC
               END-IF
               MOVE    "tbl_ptinf"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
             END-IF
           END-IF
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
      *    低１
           INITIALIZE                        TNKRRK-REC
           MOVE    LNK-SYOTOKU-HOSPNUM   TO  TNKRRK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID      TO  TNKRRK-PTID
           MOVE    TNKRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"          TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING   MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC                =   ZERO
              MOVE   "tbl_tnkrrk"        TO  MCP-TABLE
              MOVE   "key3"              TO  MCP-PATHNAME
              PERFORM 910-TNKRRK-READ-SEC
              PERFORM   UNTIL
                     (FLG-TNKRRK         =      1             ) OR
                     (TNKRRK-EDYMD(1:6)  <   LNK-SYOTOKU-SRYYM)
                 IF  (TNKRRK-STYMD(1:6)  <=  LNK-SYOTOKU-SRYYM)
                 AND (LNK-SYOTOKU-SRYYM  <=  TNKRRK-EDYMD(1:6))
                   IF LNK-SYOTOKU-SRYYM  >=  WRK-CONS-SRYYM-200604
                      PERFORM  210-TNK-HENSYU-SEC
                   ELSE
                      PERFORM  211-TNK-HENSYU2-SEC
                   END-IF
                 END-IF
                 MOVE   "tbl_tnkrrk"     TO  MCP-TABLE
                 MOVE   "key3"           TO  MCP-PATHNAME
                 PERFORM 910-TNKRRK-READ-SEC
              END-PERFORM
           ELSE
              MOVE    3                  TO  LNK-SYOTOKU-RC
           END-IF
           MOVE     "tbl_tnkrrk"         TO  MCP-TABLE
           MOVE     "key3"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    低２
           INITIALIZE                        TSYRRK-REC
           MOVE    LNK-SYOTOKU-HOSPNUM   TO  TSYRRK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID      TO  TSYRRK-PTID
           MOVE    TSYRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"          TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING   MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC              =   ZERO
              MOVE   "tbl_tsyrrk"        TO  MCP-TABLE
              MOVE   "key3"              TO  MCP-PATHNAME
              PERFORM 910-TSYRRK-READ-SEC
              PERFORM   UNTIL
                     (FLG-TSYRRK            =      1             ) OR
                     (TSYRRK-NINEDYMD(1:6)  <   LNK-SYOTOKU-SRYYM)
                 IF  (TSYRRK-NINSTYMD(1:6)  <=  LNK-SYOTOKU-SRYYM   )
                 AND (LNK-SYOTOKU-SRYYM     <=  TSYRRK-NINEDYMD(1:6))
                   IF LNK-SYOTOKU-SRYYM     >=  WRK-CONS-SRYYM-200604
                      PERFORM  220-TSY-HENSYU-SEC
                   ELSE
                      PERFORM  221-TSY-HENSYU2-SEC
                   END-IF
                 END-IF
                 MOVE   "tbl_tsyrrk"     TO  MCP-TABLE
                 MOVE   "key3"           TO  MCP-PATHNAME
                 PERFORM 910-TSYRRK-READ-SEC
              END-PERFORM
           ELSE
              MOVE    4                  TO  LNK-SYOTOKU-RC
           END-IF
           MOVE     "tbl_tsyrrk"         TO  MCP-TABLE
           MOVE     "key3"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    結果編集
           IF        LNK-SYOTOKU-RC      =   ZERO
      *
             PERFORM   VARYING   IDX     FROM    1   BY  1
                        UNTIL    IDX      >     31
      *
      *       主保険・全国公費区分
              EVALUATE WRK-DAY-SYU-TSY(IDX) ALSO WRK-DAY-SYU-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    SPACE              ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                       MOVE   "2"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SYUKBN(IDX)
              END-EVALUATE
      *
      *       地方公費区分
              EVALUATE WRK-DAY-CHI-TSY(IDX) ALSO WRK-DAY-CHI-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                       MOVE   "2"     TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-CHIKBN(IDX)
              END-EVALUATE
      *
      *       食事区分
              EVALUATE WRK-DAY-SKJ-TSY(IDX) ALSO WRK-DAY-SKJ-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    SPACE              ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                       MOVE   "2"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "2"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "3"                ALSO    SPACE
                       MOVE   "3"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "3"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "3"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
              END-EVALUATE
      *
      *       【食事用】境界層区分
              EVALUATE WRK-DAY-KKS-TSY(IDX) ALSO WRK-DAY-KKS-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-KKSKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                   IF  LNK-SYOTOKU-SKJKBN(IDX) = "1" OR "4"
                       MOVE   "1"     TO    LNK-SYOTOKU-KKSKBN(IDX)
                   END-IF
                 WHEN    "2"                ALSO    SPACE
                   IF  LNK-SYOTOKU-SKJKBN(IDX) = "2" OR "3"
                       MOVE   "1"     TO    LNK-SYOTOKU-KKSKBN(IDX)
                   END-IF
                 WHEN    "2"                ALSO    "1"
                   IF  LNK-SYOTOKU-SKJKBN(IDX) =
                                   "1" OR "4" OR "2" OR "3"
                       MOVE   "1"     TO    LNK-SYOTOKU-KKSKBN(IDX)
                   END-IF
              END-EVALUATE
      *
             END-PERFORM
      *
      *      食事区分チェック＋【食事用】返却項目編集
             IF  (LNK-SYOTOKU-SRYYM      >=  "201501")
             AND (LNK-SYOTOKU-SKJCHKFLG   =  "1"     )
               PERFORM  230-SKJKBN-CHECK-SEC
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   LNK-SYOTOKU-KKSKBN(IDX) NOT = SPACE
                   IF   LNK-SYOTOKU-SRYYM  >=  "201710"
                     IF   LNK-SYOTOKU-AGEKBN(IDX)   = "1"
                       IF   LNK-SYOTOKU-SKJKBN(IDX) = "1"
                         CONTINUE
                       ELSE
                         MOVE  SPACE  TO  LNK-SYOTOKU-KKSKBN(IDX)
                       END-IF
                     END-IF
                     IF   LNK-SYOTOKU-AGEKBN(IDX)   = "2"
                       IF   LNK-SYOTOKU-SKJKBN(IDX) = "2" OR "3"
                         CONTINUE
                       ELSE
                         MOVE  SPACE  TO  LNK-SYOTOKU-KKSKBN(IDX)
                       END-IF
                     END-IF
                   ELSE
                         MOVE  SPACE  TO  LNK-SYOTOKU-KKSKBN(IDX)
                   END-IF
                 END-IF
               END-PERFORM
             ELSE
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   LNK-SYOTOKU-KKSKBN(IDX) NOT = SPACE
                   MOVE  SPACE  TO  LNK-SYOTOKU-KKSKBN(IDX)
                 END-IF
               END-PERFORM
             END-IF
      *
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低１編集
      *****************************************************************
       210-TNK-HENSYU-SEC            SECTION.
      *
           IF   TNKRRK-STYMD(1:6)  <  LNK-SYOTOKU-SRYYM
               MOVE    1                   TO   IDX-ST
           ELSE
               MOVE    TNKRRK-STYMD(7:2)   TO   IDX-ST
           END-IF
           IF   LNK-SYOTOKU-SRYYM  <  TNKRRK-EDYMD(1:6)
               MOVE    31                  TO   IDX-ED
           ELSE
               MOVE    TNKRRK-EDYMD(7:2)   TO   IDX-ED
           END-IF
      *
           PERFORM   VARYING   IDX     FROM   IDX-ST   BY  1
                      UNTIL    IDX      >     IDX-ED
      *
              MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
              MOVE    IDX                  TO  WRK-SYOTOKU-DD
      *
              IF    TNKRRK-HKNTEKKBN  = "0" OR "1"
                 MOVE    "1"     TO    WRK-DAY-SYU-TNK(IDX)
                 IF  (LNK-SYOTOKU-SRYYM    >=  WRK-CONS-SRYYM-200610 )
                 AND (TNKRRK-RRI-FUKUSHI-KBN  =   "1"                )
                   MOVE  "4"     TO    WRK-DAY-SYU-TNK(IDX)
                 END-IF
              END-IF
      *
              IF    TNKRRK-HKNTEKKBN  = "0" OR "2" OR "4"
                 MOVE    "1"     TO    WRK-DAY-CHI-TNK(IDX)
              END-IF
      *
              IF  (TNKRRK-HKNTEKKBN = "0" OR "1" OR "3" OR "4")
              AND (TNKRRK-SKJNINSTYMD   NOT =  SPACE          )
              AND (TNKRRK-SKJNINSTYMD      <=  WRK-SYOTOKU-YMD)
              AND (TNKRRK-EDYMD            >=  WRK-SYOTOKU-YMD)
                 MOVE    "1"     TO    WRK-DAY-SKJ-TNK(IDX)
                 IF  (LNK-SYOTOKU-SRYYM    >=  WRK-CONS-SRYYM-200610 )
                 AND (TNKRRK-RRI-FUKUSHI-KBN  =   "1"                )
                   MOVE  "4"     TO    WRK-DAY-SKJ-TNK(IDX)
                 END-IF
              END-IF
      *
              IF   TNKRRK-KYOUKAISOUKBN  =  "1"
                   MOVE  "1"     TO    WRK-DAY-KKS-TNK(IDX)
              END-IF
      *
           END-PERFORM
      *
           .
       210-TNK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    低２編集
      *****************************************************************
       220-TSY-HENSYU-SEC            SECTION.
      *
           IF   TSYRRK-NINSTYMD(1:6)  <  LNK-SYOTOKU-SRYYM
               MOVE    1                      TO   IDX-ST
           ELSE
               MOVE    TSYRRK-NINSTYMD(7:2)   TO   IDX-ST
           END-IF
           IF   LNK-SYOTOKU-SRYYM     <  TSYRRK-NINEDYMD(1:6)
               MOVE    31                     TO   IDX-ED
           ELSE
               MOVE    TSYRRK-NINEDYMD(7:2)   TO   IDX-ED
           END-IF
      *
           PERFORM   VARYING   IDX     FROM   IDX-ST   BY  1
                      UNTIL    IDX      >     IDX-ED
      *
              MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
              MOVE    IDX                  TO  WRK-SYOTOKU-DD
      *
              IF    TSYRRK-HKNTEKKBN  = "0" OR "1"
                 MOVE    "2"     TO    WRK-DAY-SYU-TSY(IDX)
              END-IF
      *
              IF    TSYRRK-HKNTEKKBN  = "0" OR "2" OR "4"
                 MOVE    "2"     TO    WRK-DAY-CHI-TSY(IDX)
              END-IF
      *
              IF  (TSYRRK-HKNTEKKBN = "0" OR "1" OR "3" OR "4")
              AND (TSYRRK-SKJNINSTYMD   NOT =  SPACE          )
              AND (TSYRRK-SKJNINSTYMD      <=  WRK-SYOTOKU-YMD)
              AND (TSYRRK-NINEDYMD         >=  WRK-SYOTOKU-YMD)
                 MOVE    "2"     TO    WRK-DAY-SKJ-TSY(IDX)
                 IF  (TSYRRK-SKJGNSTYMD NOT =  SPACE          )
                 AND (TSYRRK-SKJGNSTYMD    <=  WRK-SYOTOKU-YMD)
                 AND (TSYRRK-NINEDYMD      >=  WRK-SYOTOKU-YMD)
                   MOVE  "3"     TO    WRK-DAY-SKJ-TSY(IDX)
                 END-IF
              END-IF
      *
              IF   TSYRRK-KYOUKAISOUKBN  =  "1"
                   MOVE  "2"     TO    WRK-DAY-KKS-TSY(IDX)
              END-IF
      *
           END-PERFORM
      *
           .
       220-TSY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    低１編集（２００６年３月まで）
      *****************************************************************
       211-TNK-HENSYU2-SEC            SECTION.
      *
           MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
           MOVE    01                   TO  WRK-SYOTOKU-DD
      *
           IF  (TNKRRK-STYMD     <=  WRK-SYOTOKU-YMD)
           AND (WRK-SYOTOKU-YMD  <=  TNKRRK-EDYMD   )
      *
              PERFORM   VARYING   IDX     FROM    1   BY  1
                         UNTIL    IDX      >     31
                  MOVE    "1"     TO    WRK-DAY-SYU-TNK(IDX)
                  MOVE    "1"     TO    WRK-DAY-CHI-TNK(IDX)
                  MOVE    "1"     TO    WRK-DAY-SKJ-TNK(IDX)
              END-PERFORM
      *
           END-IF
      *
           .
       211-TNK-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    低２編集（２００６年３月まで）
      *****************************************************************
       221-TSY-HENSYU2-SEC            SECTION.
      *
           MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
           MOVE    01                   TO  WRK-SYOTOKU-DD
      *
           IF  (TSYRRK-NINSTYMD  <=  WRK-SYOTOKU-YMD)
           AND (WRK-SYOTOKU-YMD  <=  TSYRRK-NINEDYMD)
      *
              PERFORM   VARYING   IDX     FROM    1   BY  1
                         UNTIL    IDX      >     31
      *
                  MOVE    "2"     TO    WRK-DAY-SYU-TSY(IDX)
                  MOVE    "2"     TO    WRK-DAY-CHI-TSY(IDX)
      *
                  MOVE    "2"     TO    WRK-DAY-SKJ-TSY(IDX)
                  IF  (TSYRRK-SKJGNSTYMD NOT =  SPACE          )
                  AND (TSYRRK-SKJGNSTYMD    <=  WRK-SYOTOKU-YMD)
                  AND (TSYRRK-NINEDYMD      >=  WRK-SYOTOKU-YMD)
                    MOVE  "3"     TO    WRK-DAY-SKJ-TSY(IDX)
                  END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       221-TSY-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事区分チェック＋【食事用】返却項目編集
      *****************************************************************
       230-SKJKBN-CHECK-SEC            SECTION.
      *
      *    患者年齢
           MOVE    ZERO                   TO  WRK-NENREI
           INITIALIZE                         ORCSAGECHKAREA
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  AGECHK-PTID
           MOVE    LNK-SYOTOKU-SRYYM      TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                   TO  AGECHK-SRYYMD(7:2)
           MOVE    WRK-BIRTHDAY           TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"           USING  ORCSAGECHKAREA
                                                 SPA-AREA
           IF      AGECHK-RC  =  ZERO
              MOVE    AGECHK-NENREI       TO  WRK-NENREI
           END-IF
      *
      *    患者公費情報取得（保険番号968・976）
           PERFORM   2301-KOHINF-GET1-SEC
      *
      *    入院会計の保険組み合わせ
           INITIALIZE                         NYUINACCT-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  NACCT-PTID
           MOVE    LNK-SYOTOKU-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_nyuinacct"   TO  MCP-TABLE
              MOVE      "key17"           TO  MCP-PATHNAME
              PERFORM    910-NYUINACCT-READ-SEC
              IF   FLG-NYUINACCT  =  ZERO
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                   MOVE  NACCT-DAY(IDX2)  TO  WRK-DAY-HKNCOMBI(IDX2)
                END-PERFORM
              END-IF
           END-IF
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    保険組み合わせ情報編集
           PERFORM   VARYING   IDX2   FROM   1   BY   1
                      UNTIL    IDX2     >   31
              IF   WRK-DAY-HKNCOMBI(IDX2)  NOT =  ZERO
              AND  WRK-DAY-SETFLG(IDX2)        =  ZERO
                MOVE    1      TO  FLG-PARA
                MOVE    WRK-DAY-HKNCOMBI(IDX2)
                               TO  WRK-TAIHI-HKNCOMBI
                PERFORM 900-HKNCOMBI-KENSAKU-SEC
      *
                COMPUTE   IDX4 = IDX2 + 1
                PERFORM   VARYING   IDX3   FROM   IDX4   BY   1
                           UNTIL    IDX3     >     31
                   IF   WRK-DAY-HKNCOMBI(IDX2) = WRK-DAY-HKNCOMBI(IDX3)
                     MOVE  WRK-DAY-HKNCOMBIINF(IDX2)
                                       TO WRK-DAY-HKNCOMBIINF(IDX3)
                   END-IF
                END-PERFORM
              END-IF
           END-PERFORM
      *
      *    保険組み合わせ情報編集（置き換え）
           INITIALIZE     WRK-CHECK2-TABLE
           PERFORM   2304-HKNCOMBI-OKIKAE-SEC
      *
      *    年齢区分編集
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
              IF   WRK-DAY-HKNCOMBI(IDX2)  NOT =  ZERO
                 IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
                 OR  (WRK-DAY-HKNNUM(IDX2)       =
                                "971" OR "973" OR "975" OR
                                "068" OR "069" OR "040"       )
                     MOVE   "9"           TO  WRK-DAY-NENREIFLG(IDX2)
                     IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
                       MOVE   SPACE       TO  LNK-SYOTOKU-SKJKBN(IDX2)
                     END-IF
                     IF  LNK-SYOTOKU-KKSKBN(IDX2) NOT = SPACE
                       MOVE   SPACE       TO  LNK-SYOTOKU-KKSKBN(IDX2)
                     END-IF
                 ELSE
                   IF  ( WRK-NENREI                  >=  70   )
                   OR  ((WRK-DAY-HKNNUM(IDX2)         = SPACE) AND
                        (WRK-DAY-KOH1HKNNUM(IDX2) NOT = SPACE) AND
                        (WRK-DAY-KOH968(IDX2)         = "1"  ))
                   OR  ( WRK-DAY-HKNNUM(IDX2)         = "039" )
                     MOVE   "1"           TO  WRK-DAY-NENREIFLG(IDX2)
                   ELSE
                     MOVE   "2"           TO  WRK-DAY-NENREIFLG(IDX2)
                   END-IF
                 END-IF
      *
                 IF      WRK-DAY-NENREIFLG(IDX2)  =  "1"
                   IF    WRK-DAY-HKNNUM(IDX2)  =  SPACE OR "039"
                     CONTINUE
                   ELSE
                     IF  ( WRK-DAY-HKNNUM(IDX2)     =
                                                   "060"           )
                     OR  ((WRK-DAY-HKNNUM(IDX2) NOT =
                                                   "060"          ) AND
                          (WRK-DAY-HOJOKBN(IDX2)    =  SPACE      ))
                       IF  WRK-DAY-KOH976(IDX2)     = "1"
                          MOVE   "2"      TO  WRK-DAY-NENREIFLG(IDX2)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
              ELSE
                 IF    LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
                   MOVE     SPACE         TO  LNK-SYOTOKU-SKJKBN(IDX2)
                 END-IF
                 IF    LNK-SYOTOKU-KKSKBN(IDX2) NOT = SPACE
                   MOVE     SPACE         TO  LNK-SYOTOKU-KKSKBN(IDX2)
                 END-IF
              END-IF
           END-PERFORM
      *
           MOVE    ZERO         TO    FLG-CHECK
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
              IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
                   MOVE    1    TO    FLG-CHECK
                   MOVE   31    TO    IDX2
              END-IF
           END-PERFORM
      *
      *    特定疾病給付対象療養の診療チェック
           INITIALIZE     WRK-CHECK-TABLE
           IF    FLG-CHECK  =  1
      *      患者公費情報取得（保険番号966・967・956・957）
             PERFORM   2302-KOHINF-GET2-SEC
             PERFORM   VARYING   IDX2    FROM    1   BY  1
                        UNTIL    IDX2     >     31
               IF      WRK-DAY-NENREIFLG(IDX2)  =  "2"
                 IF    WRK-DAY-HKNNUM(IDX2)  =  SPACE
                   CONTINUE
                 ELSE
                   PERFORM   2303-TOKUSITU-CHECK-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *    【食事用】食事区分インデックス（レセプト用）
           MOVE      ZERO       TO    WRK-FIRST-DAY
           MOVE      SPACE      TO    WRK-FIRST-SKJKBN
           IF  LNK-SYOTOKU-HKNCOMBI(1)  NOT =  ZERO
             PERFORM   VARYING   IDX2    FROM    1   BY  1
                        UNTIL    IDX2     >     31
               PERFORM VARYING   IDX3    FROM    1   BY  1
                        UNTIL    IDX3     >     10
                 IF  (WRK-DAY-HKNCOMBI(IDX2)  NOT =  ZERO)
                 AND (WRK-DAY-HKNCOMBI(IDX2)      =
                               LNK-SYOTOKU-HKNCOMBI(IDX3))
                     IF ( WRK-FIRST-DAY                =  ZERO  )
                     OR ((WRK-FIRST-DAY            NOT =  ZERO ) AND
                         (WRK-FIRST-SKJKBN             =  SPACE) AND
                         (LNK-SYOTOKU-SKJKBN(IDX2) NOT =  SPACE))
                     OR ((WRK-FIRST-DAY            NOT =  ZERO ) AND
                         (WRK-FIRST-SKJKBN             =  "2"  ) AND
                         (LNK-SYOTOKU-SKJKBN(IDX2)     =  "3"  ))
                        MOVE   IDX2      TO  LNK-SYOTOKU-SKJIDX
                                             WRK-FIRST-DAY
                        MOVE   LNK-SYOTOKU-SKJKBN(IDX2)
                                         TO  WRK-FIRST-SKJKBN
                     END-IF
                     MOVE      10        TO  IDX3
                 END-IF
               END-PERFORM
             END-PERFORM
           END-IF
      *
      *    【食事用】年齢区分
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
                IF   WRK-DAY-NENREIFLG(IDX2)  =  "1" OR "2"
                  MOVE   WRK-DAY-NENREIFLG(IDX2)
                                 TO  LNK-SYOTOKU-AGEKBN(IDX2)
                END-IF
           END-PERFORM
      *
           .
       230-SKJKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報取得（保険番号968・976）
      *****************************************************************
       2301-KOHINF-GET1-SEC            SECTION.
      *
      *    公費単独の老人保険入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "968"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH968(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    高齢非該当入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "976"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH976(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
       2301-KOHINF-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報取得（保険番号966・967・956・957）
      *****************************************************************
       2302-KOHINF-GET2-SEC            SECTION.
      *
      *    認定証入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "966"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH966(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "967"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH967(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "956"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH956(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "957"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH957(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
       2302-KOHINF-GET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾病給付対象療養の診療チェック
      *****************************************************************
       2303-TOKUSITU-CHECK-SEC            SECTION.
      *
           MOVE      ZERO       TO   FLG-ARI
      *
           IF   (WRK-DAY-KOH966(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
           IF   (WRK-DAY-KOH967(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
           IF   (WRK-DAY-KOH956(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
      *
           IF   (WRK-DAY-KOH957(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE     ZERO      TO   FLG-OK
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                       UNTIL  (IDX3                     >  31 )
                        OR    (WRK-CHECK-HKNCOMBI(IDX3) = ZERO)
                        OR    (FLG-OK                   =   1 )
               IF  WRK-DAY-HKNCOMBI(IDX2) = WRK-CHECK-HKNCOMBI(IDX3)
                 IF   WRK-CHECK-FLG(IDX3) = 1
                    CONTINUE
                 ELSE
                    IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
                      MOVE   SPACE   TO   LNK-SYOTOKU-SKJKBN(IDX2)
                    END-IF
                    IF  LNK-SYOTOKU-KKSKBN(IDX2) NOT = SPACE
                      MOVE   SPACE   TO   LNK-SYOTOKU-KKSKBN(IDX2)
                    END-IF
                 END-IF
                 MOVE    1      TO   FLG-OK
               END-IF
             END-PERFORM
             IF    FLG-OK  =  ZERO
             AND   IDX3   <=   31
               INITIALIZE                         SSRHKOH-AREA
               MOVE   LNK-SYOTOKU-PTID        TO  SSRHKOH-PTID
               MOVE   "1"                     TO  SSRHKOH-NYUGAIKBN
               MOVE   LNK-SYOTOKU-SRYYM       TO  SSRHKOH-SRYYM
               MOVE   WRK-DAY-HKNCOMBI(IDX2)  TO  SSRHKOH-HKNCOMBI
               CALL   "ORCSSRHKOH"            USING  SSRHKOH-AREA
                                                     SPA-AREA
               IF  SSRHKOH-RC  =  1
                 CONTINUE
               ELSE
                 IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
                   MOVE  SPACE  TO   LNK-SYOTOKU-SKJKBN(IDX2)
                 END-IF
                 IF  LNK-SYOTOKU-KKSKBN(IDX2) NOT = SPACE
                   MOVE  SPACE  TO   LNK-SYOTOKU-KKSKBN(IDX2)
                 END-IF
               END-IF
               MOVE  WRK-DAY-HKNCOMBI(IDX2)
                                TO   WRK-CHECK-HKNCOMBI(IDX3)
               IF  SSRHKOH-RC  =  1
                 MOVE    1      TO   WRK-CHECK-FLG(IDX3)
               ELSE
                 MOVE    ZERO   TO   WRK-CHECK-FLG(IDX3)
               END-IF
             END-IF
             MOVE    1          TO   FLG-ARI
           END-IF
      *
      ***> IF    FLG-ARI  =  ZERO
      ***>     IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***>       MOVE   SPACE   TO   LNK-SYOTOKU-SKJKBN(IDX2)
      ***>     END-IF
      ***>     IF  LNK-SYOTOKU-KKSKBN(IDX2) NOT = SPACE
      ***>       MOVE   SPACE   TO   LNK-SYOTOKU-KKSKBN(IDX2)
      ***>     END-IF
      ***> END-IF
      *
           .
       2303-TOKUSITU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ情報編集（置き換え）
      *****************************************************************
       2304-HKNCOMBI-OKIKAE-SEC            SECTION.
      *
           MOVE    ZERO       TO    FLG-CHECK
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
             IF   WRK-DAY-HKNCOMBI(IDX2)      NOT =  ZERO
               IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
               OR  (WRK-DAY-HKNNUM(IDX2)       =
                              "971" OR "973" OR "975" OR
                              "068" OR "069" OR "040"       )
                 MOVE    1    TO    FLG-CHECK
                 MOVE   31    TO    IDX2
               END-IF
             END-IF
           END-PERFORM
      *
           IF    FLG-CHECK  =  1
      *      同日再入院の保険組み合わせ取得
             INITIALIZE                        ORCSNYUINDAY-AREA
             MOVE     LNK-SYOTOKU-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
             MOVE     LNK-SYOTOKU-PTID     TO  LNK-NYUINDAY-PTID
             MOVE     LNK-SYOTOKU-SRYYM    TO  LNK-NYUINDAY-SANTEIYM
             CALL     "ORCSNYUINDAY"       USING   ORCSNYUINDAY-AREA
                                                   SPA-AREA
             IF       LNK-NYUINDAY-RC  =  ZERO
               PERFORM   VARYING   IDX2    FROM    1   BY  1
                          UNTIL    IDX2     >     31
                 MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX2)
                                 TO  WRK-DAY-HKNCOMBI-DOU(IDX2)
               END-PERFORM
             END-IF
      *      同日再入院の保険組み合わせ置き換え編集
             PERFORM   VARYING   IDX2    FROM    1   BY  1
                        UNTIL    IDX2     >     31
               IF   WRK-DAY-HKNCOMBI(IDX2)      NOT =  ZERO
                 IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
                 OR  (WRK-DAY-HKNNUM(IDX2)       =
                                "971" OR "973" OR "975" OR
                                "068" OR "069" OR "040"       )
                   IF   WRK-DAY-HKNCOMBI-DOU(IDX2) NOT = ZERO
                     MOVE    ZERO   TO  FLG-ARI2
                     MOVE    2      TO  FLG-PARA
                     MOVE    WRK-DAY-HKNCOMBI-DOU(IDX2)
                                    TO  WRK-TAIHI-HKNCOMBI
                     PERFORM 2306-HKNCOMBI-OKIKAE3-SEC
                     IF  FLG-ARI2  =  ZERO
                       PERFORM 900-HKNCOMBI-KENSAKU-SEC
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           MOVE    ZERO       TO    FLG-CHECK
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
             IF   WRK-DAY-HKNCOMBI(IDX2)      NOT =  ZERO
               IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
               OR  (WRK-DAY-HKNNUM(IDX2)       =
                              "971" OR "973" OR "975" OR
                              "068" OR "069" OR "040"       )
                 MOVE    1    TO    FLG-CHECK
                 MOVE   31    TO    IDX2
               END-IF
             END-IF
           END-PERFORM
      *
           IF   (FLG-CHECK           =   1      )
           AND  (LNK-SYOTOKU-SRYYM  >=  "201710")
             PERFORM 2305-HKNCOMBI-OKIKAE2-SEC
           END-IF
      *
           .
       2304-HKNCOMBI-OKIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ情報編集（置き換え）
      *****************************************************************
       2305-HKNCOMBI-OKIKAE2-SEC            SECTION.
      *
           MOVE    ZERO     TO    FLG-CHECK2
      *
           INITIALIZE                       SRYACT-REC
           MOVE    LNK-SYOTOKU-HOSPNUM  TO  SRY-HOSPNUM
           MOVE    "1"                  TO  SRY-NYUGAIKBN
           MOVE    LNK-SYOTOKU-PTID     TO  SRY-PTID
           MOVE    LNK-SYOTOKU-SRYYM    TO  SRY-SRYYM
           MOVE    "099999930"          TO  SRY-SRYCD(1)
           MOVE    "099999930"          TO  SRY-SRYCD(2)
           MOVE    "099999930"          TO  SRY-SRYCD(3)
           MOVE    "099999930"          TO  SRY-SRYCD(4)
           MOVE    "099999930"          TO  SRY-SRYCD(5)
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_sryact"       TO  MCP-TABLE
             MOVE    "key7"             TO  MCP-PATHNAME
             PERFORM  910-SRYACT-READ-SEC
             PERFORM  UNTIL (FLG-SRYACT  =  1)
                       OR   (FLG-CHECK2  =  1)
               INITIALIZE                   SRYACCT-REC
               MOVE    SRY-HOSPNUM      TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
               MOVE    SRY-SRYKA        TO  ACCT-SRYKA
               MOVE    SRY-PTID         TO  ACCT-PTID
               MOVE    SRY-SRYYM        TO  ACCT-SRYYM
               MOVE    SRY-ZAINUM       TO  ACCT-ZAINUM
               MOVE    SRYACCT-REC      TO  MCPDATA-REC
               MOVE    "DBSELECT"       TO  MCP-FUNC
               MOVE    "tbl_sryacct"    TO  MCP-TABLE
               MOVE    "key31"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC  =  ZERO
                 MOVE    "tbl_sryacct"  TO  MCP-TABLE
                 MOVE    "key31"        TO  MCP-PATHNAME
                 PERFORM  910-SRYACCT-READ-SEC
                 IF    FLG-SRYACCT  =  ZERO
                   IF  ACCT-ZAIKAISU  NOT =  ZERO
                     MOVE   1   TO   FLG-CHECK2
                   END-IF
                 END-IF
               END-IF
               MOVE    "tbl_sryacct"    TO  MCP-TABLE
               MOVE    "key31"          TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
      *
               MOVE    "tbl_sryact"     TO  MCP-TABLE
               MOVE    "key7"           TO  MCP-PATHNAME
               PERFORM  910-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           IF    FLG-CHECK2  =  1
      *      入院料と異なる食事算定保険組み合わせ取得
             INITIALIZE                      ORCSSKJHKN-AREA
             MOVE   LNK-SYOTOKU-HOSPNUM  TO  LNK-SKJHKN-HOSPNUM
             MOVE   LNK-SYOTOKU-PTID     TO  LNK-SKJHKN-PTID
             MOVE   LNK-SYOTOKU-SRYYM    TO  LNK-SKJHKN-SANTEIYM
             CALL   "ORCSSKJHKN"         USING   ORCSSKJHKN-AREA
                                                 ORCSNYUINDAY-AREA
                                                 SPA-AREA
             IF     LNK-SKJHKN-RC  =  ZERO
               PERFORM    VARYING   IDX2   FROM   1   BY   1
                           UNTIL    IDX2    >    31
                 IF   LNK-SKJHKN-HKNCOMBINUM(IDX2 1) NOT = SPACE
                   MOVE   LNK-SKJHKN-HKNCOMBINUM(IDX2 1)
                               TO  WRK-DAY-HKNCOMBI-SKJHKN(IDX2)
                 END-IF
                 IF   LNK-SKJHKN-HKNCOMBINUM(IDX2 2) NOT = SPACE
                   MOVE   LNK-SKJHKN-HKNCOMBINUM(IDX2 2)
                               TO  WRK-DAY-HKNCOMBI-SKJHKN-DOU(IDX2)
                 END-IF
               END-PERFORM
             END-IF
      *      入院料と異なる食事算定保険組み合わせ置き換え編集
             PERFORM   VARYING   IDX2    FROM    1   BY  1
                        UNTIL    IDX2     >     31
               IF   WRK-DAY-HKNCOMBI(IDX2)      NOT =  ZERO
                 IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
                 OR  (WRK-DAY-HKNNUM(IDX2)       =
                                "971" OR "973" OR "975" OR
                                "068" OR "069" OR "040"       )
                   MOVE    ZERO     TO  FLG-OKIKAE
                   IF  (WRK-DAY-HKNCOMBI-SKJHKN(IDX2) NOT = ZERO)
                   AND (FLG-OKIKAE                        = ZERO)
                     MOVE    ZERO   TO  FLG-ARI2
                     MOVE    2      TO  FLG-PARA
                     MOVE    WRK-DAY-HKNCOMBI-SKJHKN(IDX2)
                                    TO  WRK-TAIHI-HKNCOMBI
                     PERFORM 2306-HKNCOMBI-OKIKAE3-SEC
                     IF  FLG-ARI2  =  ZERO
                       PERFORM 900-HKNCOMBI-KENSAKU-SEC
                     END-IF
                   END-IF
                   IF  (WRK-DAY-HKNCOMBI-SKJHKN-DOU(IDX2) NOT = ZERO)
                   AND (FLG-OKIKAE                            = ZERO)
                     MOVE    ZERO   TO  FLG-ARI2
                     MOVE    2      TO  FLG-PARA
                     MOVE    WRK-DAY-HKNCOMBI-SKJHKN-DOU(IDX2)
                                    TO  WRK-TAIHI-HKNCOMBI
                     PERFORM 2306-HKNCOMBI-OKIKAE3-SEC
                     IF  FLG-ARI2  =  ZERO
                       PERFORM 900-HKNCOMBI-KENSAKU-SEC
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       2305-HKNCOMBI-OKIKAE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ情報編集（置き換え）
      *****************************************************************
       2306-HKNCOMBI-OKIKAE3-SEC            SECTION.
      *
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                     UNTIL  (IDX5                      >  100)
                      OR    (WRK-CHECK2-HKNCOMBI(IDX5) = ZERO)
                      OR    (FLG-ARI2                  =  1  )
             IF     WRK-TAIHI-HKNCOMBI = WRK-CHECK2-HKNCOMBI(IDX5)
               IF  (WRK-CHECK2-HKNKOHSBTKBN(IDX5)  =  "8" OR "9")
               OR  (WRK-CHECK2-HKNNUM(IDX5)        =
                                   "971" OR "973" OR "975" OR
                                   "068" OR "069" OR "040"      )
                 CONTINUE
               ELSE
                 MOVE  WRK-CHECK2-HKNCOMBI(IDX5)
                                TO  WRK-DAY-HKNCOMBI(IDX2)
                 MOVE  WRK-CHECK2-HKNCOMBIINF(IDX5)
                                TO  WRK-DAY-HKNCOMBIINF(IDX2)
                 MOVE  1        TO  FLG-OKIKAE
               END-IF
               MOVE    1        TO  FLG-ARI2
             END-IF
           END-PERFORM
      *
           .
       2306-HKNCOMBI-OKIKAE3-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ検索
      *****************************************************************
       900-HKNCOMBI-KENSAKU-SEC            SECTION.
      *
           INITIALIZE                         HKNCOMBI-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  COMB-PTID
           MOVE    WRK-TAIHI-HKNCOMBI     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_hkncombi"     TO  MCP-TABLE
             MOVE      "key"              TO  MCP-PATHNAME
             PERFORM    910-HKNCOMBI-READ-SEC
             IF    FLG-HKNCOMBI  =  ZERO
               EVALUATE  FLG-PARA
                 WHEN       1
                   MOVE COMB-HKNNUM     TO WRK-DAY-HKNNUM(IDX2)
                   MOVE COMB-KOH1HKNNUM TO WRK-DAY-KOH1HKNNUM(IDX2)
                   MOVE COMB-SYU-HKNKOHSBTKBN
                                        TO WRK-DAY-HKNKOHSBTKBN(IDX2)
                   MOVE COMB-HOJOKBN    TO WRK-DAY-HOJOKBN(IDX2)
                   MOVE 1               TO WRK-DAY-SETFLG(IDX2)
                 WHEN       2
                   MOVE WRK-TAIHI-HKNCOMBI
                                        TO WRK-CHECK2-HKNCOMBI(IDX5)
                   MOVE COMB-HKNNUM     TO WRK-CHECK2-HKNNUM(IDX5)
                   MOVE COMB-KOH1HKNNUM TO WRK-CHECK2-KOH1HKNNUM(IDX5)
                   MOVE COMB-SYU-HKNKOHSBTKBN
                                        TO WRK-CHECK2-HKNKOHSBTKBN(IDX5)
                   MOVE COMB-HOJOKBN    TO WRK-CHECK2-HOJOKBN(IDX5)
                   MOVE 1               TO WRK-CHECK2-SETFLG(IDX5)
      *
                   IF  (COMB-SYU-HKNKOHSBTKBN =  "8" OR "9")
                   OR  (COMB-HKNNUM           =
                             "971" OR "973" OR "975" OR
                             "068" OR "069" OR "040"       )
                     CONTINUE
                   ELSE
                     MOVE WRK-TAIHI-HKNCOMBI
                                        TO WRK-DAY-HKNCOMBI(IDX2)
                     MOVE COMB-HKNNUM   TO WRK-DAY-HKNNUM(IDX2)
                     MOVE COMB-KOH1HKNNUM
                                        TO WRK-DAY-KOH1HKNNUM(IDX2)
                     MOVE COMB-SYU-HKNKOHSBTKBN
                                        TO WRK-DAY-HKNKOHSBTKBN(IDX2)
                     MOVE COMB-HOJOKBN  TO WRK-DAY-HOJOKBN(IDX2)
                     MOVE 1             TO WRK-DAY-SETFLG(IDX2)
                     MOVE 1             TO FLG-OKIKAE
                   END-IF
               END-EVALUATE
             END-IF
           END-IF
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
       900-HKNCOMBI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得１マスタ読み込み
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得２マスタ読み込み
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       910-PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計読込
      *****************************************************************
       910-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       910-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
