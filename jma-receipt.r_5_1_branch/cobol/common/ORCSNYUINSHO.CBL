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
       PROGRAM-ID.                 ORCSNYUINSHO.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院退院
      *  コンポーネント名  : 入院院外処方せん印刷処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/25    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  10/01/XX  治験対応
      *  04.07.00    NACL-多々納  11/11/XX  同日再入院対応
      *  04.08.00    NACL-多々納  16/08/XX  持参薬対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
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
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-YAKU-ARI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDXA                PIC 9(04).
           03  IDXA2               PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-T2              PIC 9(04).
           03  IDX-T3              PIC 9(04).
      *
           03  IDX-T               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDX-YAK             PIC 9(04).
           03  IDX-C1              PIC 9(04).
           03  IDX-C               PIC 9(04).
      *
           03  IDX-S               PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-STDD                PIC 9(02).
           03  WRK-ENDD                PIC 9(02).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-DRCD                PIC X(05).
           03  WRK-INPUTCOMENT         PIC X(80).
      *
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC  X(08)  OCCURS  5.
      *
           03  WRK-ZAIKAISU            PIC 9(05).
      *    同日再入院日
           03  WRK-DDAY                PIC 9(02).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   5.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-DRCD            PIC X(05).
               05  TBL-HKNCOMBI        PIC X(04)
                                           OCCURS   10.
      *
       01  TBL-SHOHOU-AREA.
           03  TBL-SHOHOU-ZAI-G         OCCURS   100.
      *        剤毎に編集する
               05  TBL-SHO-SRYKA           PIC X(02).
               05  TBL-SHO-HKNCOMBI        PIC X(04).
               05  TBL-SHO-ZAIKAISU        PIC 9(03).
               05  TBL-SHO-SRYKBN          PIC X(02).
               05  TBL-SHO-SRYSYUKBN       PIC X(03).
               05  TBL-SHO-DATA            OCCURS   50.
                   07  TBL-SHO-SRYCD           PIC X(09).
                   07  TBL-SHO-SRYSURYO        PIC 9(05)V9(05).
                   07  TBL-SHO-SRYKAISU        PIC 9(03).
                   07  TBL-SHO-INPUTNUM        PIC 9(03).
                   07  TBL-SHO-INPUTCOMENT     PIC X(80).
      *H30.4       入力値
                   07  TBL-SHO-INPUTCHI-G.
                       09  TBL-SHO-INPUTCHI    PIC  X(08)
                                                    OCCURS  5.
           03  TBL-IDX                     PIC 9(04).
      *
      *    診療行為マスター
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS  10.
           COPY    "CPSRYACT.INC"     REPLACING
                                     //SRY-// BY //TBL-SRY-//.
      *    ＳＰＡ保存
           COPY    "COMMON-SPA"    REPLACING
                                   //SPA-// BY //HZN-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    入院院外処方せんパラメタ領域
           COPY    "CPORCSNYUINSHO.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSNYUINSHOAREA
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
      *
           EVALUATE    ORCSNYUINSHO-KBN
               WHEN    "1"
      *            診療科編集処理
                   PERFORM 100-SRYKA-HENSYU-SEC
               WHEN    "2"
               WHEN    "3"
      *            院外処方せん印刷処理
                   PERFORM 200-SHOHOU-PRINT-SEC
           END-EVALUATE
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療科編集処理処理
      *****************************************************************
       100-SRYKA-HENSYU-SEC            SECTION.
      *
      *    院外処方のある診療科を編集
           PERFORM 1001-SRYKA-TBL-SEC
      *    出力パラメタ設定
           INITIALIZE                  ORCSNYUINSHO-SRYKA-AREA
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      TBL-SRYKA (IDX) NOT =   SPACE
                   ADD     1               TO  IDX2
                   MOVE    TBL-SRYKA (IDX) TO  ORCSNYUINSHO-SRYKA
                                                            (IDX2)
                   MOVE    IDX2            TO  ORCSNYUINSHO-SRYKA-MAX
               END-IF
           END-PERFORM
           .
       100-HENSYU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       1001-SRYKA-TBL-SEC            SECTION.
      *
           INITIALIZE                 TBL-SRYKA-AREA
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCSNYUINSHO-NYUINYMD(1:6)
                                       TO  ACCT-STSRYYM
           MOVE    ORCSNYUINSHO-TAIINYMD(1:6)
                                       TO  ACCT-EDSRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key52"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1       )
               IF      ACCT-SRYYM          =   ORCSNYUINSHO-NYUINYMD
                                                     (1:6)
                   MOVE    ORCSNYUINSHO-NYUINYMD(7:2)  TO  WRK-STDD
               ELSE
                   MOVE    1                           TO  WRK-STDD
               END-IF
               IF      ACCT-SRYYM          =   ORCSNYUINSHO-TAIINYMD
                                                     (1:6)
                   MOVE    ORCSNYUINSHO-TAIINYMD(7:2)  TO  WRK-ENDD
               ELSE
                   MOVE    31                          TO  WRK-ENDD
               END-IF
      *        同日再入院区分
               MOVE    ZERO                TO  WRK-DDAY
               IF      ORCSNYUINSHO-DOUJITSUKBN    =   "1"
                   IF      ACCT-SRYYM          =   ORCSNYUINSHO-NYUINYMD
                                                            (1:6)
                       MOVE    ORCSNYUINSHO-NYUINYMD(7:2)  TO  WRK-DDAY
                   END-IF
               END-IF
      *
               MOVE    ZERO                TO  WRK-ZAIKAISU
      *        回数集計
               PERFORM VARYING     IDY2    FROM    WRK-STDD  BY  1
                       UNTIL       IDY2    >   WRK-ENDD
                   IF      ACCT-DAY (1 IDY2)   >   ZERO
                       IF      IDY2            =   WRK-DDAY
      *                    同日再入院日
                           ADD     ACCT-DAY (3 IDY2)   TO  WRK-ZAIKAISU
                       ELSE
                           ADD     ACCT-DAY (2 IDY2)   TO  WRK-ZAIKAISU
                       END-IF
                   END-IF
               END-PERFORM
      *        診療科指定時
               IF      ORCSNYUINSHO-KBN    =   "3"
                   IF      ORCSNYUINSHO-SRYKA (1)  NOT =   ACCT-SRYKA
                       MOVE    ZERO            TO  WRK-ZAIKAISU
                   END-IF
               END-IF
      *        薬評・器評対象外
               IF      ACCT-ZAIKBN         =   1
                   MOVE    ZERO            TO  WRK-ZAIKAISU
               END-IF
      *H28.9   持参薬　対象外
               IF      ACCT-ZAIKBN         =   3
                                           OR  4
                   MOVE    ZERO            TO  WRK-ZAIKAISU
               END-IF
               IF      WRK-ZAIKAISU        >   ZERO
      *            入院期間中に算定があれば、対象
                   EVALUATE    ORCSNYUINSHO-KBN
                       WHEN    "1"
                           PERFORM 1001-SRYACT-CHK-SEC
                       WHEN    "2"
                           PERFORM 2001-SRYACT-HENSYU-SEC
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key52"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-SRYKA-TBL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       1001-SRYACT-CHK-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-YAKU-ARI
           IF      FLG-SRYACT          =   ZERO
               IF      SRY-SRYSYUKBN       =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "148"
                                           OR  "149"
      *H26.4   外用（臨時）
                                           OR  "295"
                                           OR  "298"
                   MOVE    1               TO  FLG-YAKU-ARI
               END-IF
           END-IF
      *
           IF      FLG-YAKU-ARI        =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   5
                   IF      TBL-SRYKA (IDX)     =   SPACE
                       MOVE    SRY-SRYKA       TO  TBL-SRYKA (IDX)
                   END-IF
                   IF      SRY-SRYKA       =   TBL-SRYKA (IDX)
                       PERFORM VARYING     IDX2    FROM    1   BY  1
                               UNTIL       IDX2    >   10
                           IF      TBL-HKNCOMBI (IDX IDX2) =   SPACE
                               MOVE    ACCT-HKNCOMBI   TO  TBL-HKNCOMBI
                                                              (IDX IDX2)
                           END-IF
                           IF      ACCT-HKNCOMBI       =   TBL-HKNCOMBI
                                                              (IDX IDX2)
                                MOVE    10             TO  IDX2
                            END-IF
                       END-PERFORM
                       MOVE    10                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       1001-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方せん印刷処理
      *****************************************************************
       200-SHOHOU-PRINT-SEC            SECTION.
      *
           MOVE    ORCSNYUINSHO-TAIINYMD
                                       TO  SPA-SRYYMD
      *
           INITIALIZE                  TBL-SHOHOU-AREA
      *    院外処方の内容編集
           PERFORM 1001-SRYKA-TBL-SEC
      *
      *    ドクター編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5  )   OR
                              (TBL-SRYKA (IDX) =   SPACE )
               MOVE    ORCSNYUINSHO-SYUDRCD    TO  TBL-DRCD (IDX)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   5
                   IF      TBL-SRYKA (IDX) =   ORCSNYUINSHO-SRYKA
                                                              (IDX2)
                       MOVE    ORCSNYUINSHO-DRCD (IDX2)
                                               TO  TBL-DRCD (IDX)
                       MOVE    5               TO  IDX2
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    SPA-AREA            TO  HZN-SPA-AREA
           EVALUATE   SPA-SEX
               WHEN   "1"
                   MOVE   "男"             TO  SPA-SEX
               WHEN   "2"
                   MOVE   "女"             TO  SPA-SEX
           END-EVALUATE
      *    帳票プログラム取得
           INITIALIZE                      SYS-1032-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  SPA-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *    診療科・保険組み合わせ毎に印刷する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      TBL-SRYKA (IDX)     NOT =   SPACE
                   MOVE    TBL-SRYKA (IDX)     TO  WRK-SRYKA
                   MOVE    TBL-DRCD  (IDX)     TO  WRK-DRCD
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   10   )
                                  OR  (TBL-HKNCOMBI (IDX IDX2)
                                                   =   SPACE )
                       MOVE    TBL-HKNCOMBI (IDX IDX2) TO  WRK-HKNCOMBI
      *                院外処方せん印刷処理
                       PERFORM 2002-PRINT-SYORI-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    HZN-SPA-AREA        TO  SPA-AREA
           .
       200-SHOHOU-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       2001-SRYACT-HENSYU-SEC          SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           INITIALIZE                  TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-YAKU-ARI
           IF      FLG-SRYACT          =   ZERO
               IF      SRY-SRYSYUKBN       =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "148"
                                           OR  "149"
                                           OR  "982"
      *H26.4   外用（臨時）
                                           OR  "295"
                                           OR  "298"
                   MOVE    1               TO  FLG-YAKU-ARI
               END-IF
           END-IF
      *
           PERFORM UNTIL  (FLG-SRYACT      =   1   )
                   OR     (FLG-YAKU-ARI    =   ZERO )
               ADD     1                   TO  IDX-S
               MOVE    SRYACT-REC          TO  TBL-SRYACT-REC (IDX-S)
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-YAKU-ARI        =   1
      *        剤内容を編集
               PERFORM 2003-SRYACT-TBL-HEN-SEC
      *        診療科・保険毎にテーブル編集を行う
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   5
                   IF      TBL-SRYKA (IDX)     =   SPACE
                       MOVE    SRY-SRYKA       TO  TBL-SRYKA (IDX)
                   END-IF
                   IF      SRY-SRYKA       =   TBL-SRYKA (IDX)
                       PERFORM VARYING     IDX2    FROM    1   BY  1
                               UNTIL       IDX2    >   10
                           IF      TBL-HKNCOMBI (IDX IDX2) =   SPACE
                               MOVE    ACCT-HKNCOMBI    TO  TBL-HKNCOMBI
                                                              (IDX IDX2)
                           END-IF
                           IF      ACCT-HKNCOMBI        =   TBL-HKNCOMBI
                                                              (IDX IDX2)
                                MOVE    10             TO  IDX2
                            END-IF
                       END-PERFORM
                       MOVE    10                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       2001-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       2003-SRYACT-TBL-HEN-SEC            SECTION.
      *
           ADD     1                   TO  TBL-IDX
           IF      TBL-IDX             >   100
               GO      TO      2003-SRYACT-TBL-HEN-EXT
           END-IF
      *
           MOVE    ACCT-SRYKA          TO  TBL-SHO-SRYKA    (TBL-IDX)
           MOVE    ACCT-HKNCOMBI       TO  TBL-SHO-HKNCOMBI (TBL-IDX)
           MOVE    WRK-ZAIKAISU        TO  TBL-SHO-ZAIKAISU (TBL-IDX)
           MOVE    TBL-SRY-SRYKBN(1)   TO  TBL-SHO-SRYKBN    (TBL-IDX)
           MOVE    TBL-SRY-SRYSYUKBN (1)
                                       TO  TBL-SHO-SRYSYUKBN (TBL-IDX)
      *
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDX-S
               MOVE    TBL-SRYACT-REC (IDX)    TO  SRYACT-REC
               PERFORM VARYING IDX2        FROM    1   BY  1
                       UNTIL  (IDX2        >   5   )
                       OR     (SRY-SRYCD (IDX2)    =   SPACE )
                       OR     (IDX-T       >=  50  )
                   ADD     1               TO  IDX-T
                   MOVE    SRY-SRYCD (IDX2)    TO  TBL-SHO-SRYCD
                                                      (TBL-IDX IDX-T)
                   MOVE    SRY-SRYSURYO (IDX2) TO  TBL-SHO-SRYSURYO
                                                      (TBL-IDX IDX-T)
                   MOVE    SRY-SRYKAISU (IDX2) TO  TBL-SHO-SRYKAISU
                                                      (TBL-IDX IDX-T)
                   MOVE    SRY-INPUTNUM (IDX2) TO  TBL-SHO-INPUTNUM
                                                      (TBL-IDX IDX-T)
                   IF      SRY-INPUTNUM (IDX2) NOT =   ZERO
                       PERFORM 20031-PTCOM-SET-SEC
                       MOVE    WRK-INPUTCOMENT TO  TBL-SHO-INPUTCOMENT
                                                      (TBL-IDX IDX-T)
      *H30.4           入力値
                       MOVE    WRK-INPUTCHI-G  TO  TBL-SHO-INPUTCHI-G
                                                      (TBL-IDX IDX-T)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       2003-SRYACT-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       20031-PTCOM-SET-SEC              SECTION.
      *
           INITIALIZE                     PTCOM-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD   (IDX2)  TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-INPUTCOMENT
               MOVE    PTCOM-INPUTCHI-AREA TO  WRK-INPUTCHI-G
           ELSE
               MOVE    SPACE               TO  WRK-INPUTCOMENT
               MOVE    SPACE               TO  WRK-INPUTCHI-G
           END-IF
           .
       20031-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方せん印刷処理
      *****************************************************************
       2002-PRINT-SYORI-SEC         SECTION.
      *
           INITIALIZE                  ORCHC19AREA
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING TBL-IDX     FROM    1   BY  1
                   UNTIL   TBL-IDX     >   100
               IF     (TBL-SHO-SRYKA (TBL-IDX)     =   WRK-SRYKA )
                  AND (TBL-SHO-HKNCOMBI(TBL-IDX)   =   WRK-HKNCOMBI)
                   PERFORM 20021-ORCHC19-HENSYU-SEC
                END-IF
           END-PERFORM
           IF      ORCHC19-ZAI-MAX     >   ZERO
               PERFORM 20022-SYOHO-PRINT-SEC
           END-IF
           .
       2002-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん内容編集処理
      *****************************************************************
       20021-ORCHC19-HENSYU-SEC         SECTION.
      *
           ADD     1                   TO  IDX1
           MOVE    IDX1                TO  ORCHC19-ZAI-MAX
           MOVE    TBL-SHO-ZAIKAISU (TBL-IDX)
                                       TO  ORCHC19-ZAIKAIKEI (IDX1)
           MOVE    TBL-SHO-SRYSYUKBN (TBL-IDX)
                                       TO  ORCHC19-SRYSYUKBN (IDX1)
           MOVE    TBL-SHO-SRYKBN (TBL-IDX)
                                       TO  ORCHC19-SRYKBN (IDX1)
           MOVE    ZERO                TO  ORCHC19-ZAITENKEI (IDX1)
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING IDX-T       FROM    1   BY  1
                   UNTIL  (IDX-T       >   50 )
                       OR (TBL-SHO-SRYCD (TBL-IDX IDX-T)   =   SPACE)
               ADD     1                   TO  IDX3
               MOVE    TBL-SHO-SRYCD  (TBL-IDX IDX-T)
                                           TO  ORCHC19-SRYCD (IDX1 IDX3)
               MOVE    TBL-SHO-SRYSURYO (TBL-IDX IDX-T)
                                           TO  ORCHC19-SRYSURYO
                                                             (IDX1 IDX3)
               MOVE    TBL-SHO-SRYKAISU (TBL-IDX IDX-T)
                                           TO  ORCHC19-SRYKAISU
                                                             (IDX1 IDX3)
               MOVE    TBL-SHO-INPUTCOMENT (TBL-IDX IDX-T)
                                           TO  ORCHC19-INPUTCOMENT
                                                             (IDX1 IDX3)
               MOVE    TBL-SHO-INPUTCHI-G (TBL-IDX IDX-T)
                                           TO  ORCHC19-INPUTCHI-G
                                                             (IDX1 IDX3)
           END-PERFORM
           .
       20021-ORCHC19-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん内容編集処理
      *****************************************************************
       20022-SYOHO-PRINT-SEC         SECTION.
      *
           MOVE    "N"                 TO  ORCHC19-KBN
           MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
           MOVE    WRK-DRCD            TO  SPA-DRCD
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *****MOVE    SPACE               TO  SPA-PRT-FLG
           CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                               ORCHC19AREA
           .
       20022-SYOHO-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       920-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *
