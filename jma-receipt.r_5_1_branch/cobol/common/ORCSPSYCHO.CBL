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
       PROGRAM-ID.                 ORCSPSYCHO.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 向精神薬多剤投与チェック処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/06/05    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/03/XX  平成２８年４月改正対応
      *  04.08.00    NACL-多々納  18/03/XX  平成３０年４月改正対応
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
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *H28.4
           03  FLG-PSYCHOTROPIC        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-G                   PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YAKKAKJNCD.
               05  WRK-YAKCODE         PIC X(07).
               05  WRK-YAKCODE-02      PIC X(05).
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-CLASS               PIC 9(01).
           03  WRK-GROUP               PIC X(07).
      *
       01  TBL-GRP-AREA.
           03  TBL-GRP-OCC             OCCURS   4.
               05  TBL-GRP-CNT         PIC 9(02).
               05  TBL-GRP-GROUP       PIC X(07)
                                       OCCURS    50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *    向精神薬成分コードテーブル
      ***  COPY    "CMPSYCHOTROPIC.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    向精神薬成分コード
       01  PSYCHOTROPIC-REC.
           COPY    "CPPSYCHOTROPIC.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    向精神薬多剤投与チェックパラメタ
           COPY    "CPORCSPSYCHO.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSPSYCHOAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      TBL-GRP-AREA
      *
           INITIALIZE                      ORCSPSYCHO-OUT-AREA
      *
           MOVE    ORCSPSYCHO-SRYYMD   TO  WRK-SRYYMD
           IF      ORCSPSYCHO-SRYYMD   =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           END-IF
      *    主処理
           PERFORM 200-MAIN-SYORI-SEC
      *
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SYORI-SEC              SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX              >   50  )
                       OR    (ORCSPSYCHO-YAKKAKJNCD(IDX)
                                               =   SPACE )
      *        薬価基準コードの頭７桁で検索
               MOVE    ORCSPSYCHO-YAKKAKJNCD(IDX)
                                           TO  WRK-YAKKAKJNCD
      *****    PERFORM 2001-EFFECTKBN-CHEK-SEC
               PERFORM 2001-EFFECTKBN-CHEK02-SEC
      *        薬剤の効能区分
               MOVE    WRK-CLASS          TO  ORCSPSYCHO-FFECT-KBN
                                                           (IDX)
      *        種類区分
               MOVE    WRK-GROUP          TO  ORCSPSYCHO-GROUP-KBN
                                                           (IDX)
               MOVE    ZERO                TO  FLG-ERR
      *        臨時投薬対象外
               IF     (ORCSPSYCHO-SRYSYUKBN(IDX)(1:2)
                                           =   "29" )
                 AND  (WRK-CLASS           =   3
                                           OR  4    )
      *            １４日以内は対象外
                   IF      ORCSPSYCHO-KAISU (IDX)  <=  14
                       MOVE    1               TO  FLG-ERR
                       MOVE    "1"             TO  ORCSPSYCHO-RINJI-KBN
                                                                  (IDX)
                   END-IF
               END-IF
      *
               IF     (WRK-CLASS           >=  1  AND  <=  4 )
                   MOVE    1               TO  ORCSPSYCHO-FFECT-FLG
               END-IF
      *
               IF     (WRK-CLASS           >=  1  AND  <=  4 )
                  AND (FLG-ERR             =   ZERO   )
                   PERFORM 2002-CLASS-GROUP-SEC
               END-IF
           END-PERFORM
      *    効能投与結果
           PERFORM VARYING    IDX-G    FROM    1   BY  1
                   UNTIL     (IDX-G            >   4   )
               MOVE    TBL-GRP-CNT (IDX-G) TO  ORCSPSYCHO-COUNT (IDX-G)
           END-PERFORM
      *
      *    多剤投与判定
      *     抗不安薬３種類以上、睡眠薬３種類以上
      *     抗うつ薬４種類以上、抗精神病薬４種類以上
           IF     (TBL-GRP-CNT (1)     >=  3 )
              OR  (TBL-GRP-CNT (2)     >=  3 )
              OR  (TBL-GRP-CNT (3)     >=  4 )
              OR  (TBL-GRP-CNT (4)     >=  4 )
               MOVE    1                   TO  ORCSPSYCHO-TAZAI
           END-IF
      *H28.4
      *    多剤投与判定 Ｈ２８年度から
      *     抗不安薬３種類以上、睡眠薬３種類以上
      *     抗うつ薬３種類以上、抗精神病薬３種類以上
           IF     (ORCSPSYCHO-SRYYMD   >=  "20160401")
               IF     (TBL-GRP-CNT (1)     >=  3 )
                  OR  (TBL-GRP-CNT (2)     >=  3 )
                  OR  (TBL-GRP-CNT (3)     >=  3 )
                  OR  (TBL-GRP-CNT (4)     >=  3 )
                   MOVE    1                   TO  ORCSPSYCHO-TAZAI
               END-IF
           END-IF
      *
      *H30.4
      *    多剤投与判定 Ｈ３０年度から
      *     抗不安薬３種類以上、睡眠薬３種類以上
      *     抗うつ薬３種類以上、抗精神病薬３種類以上
      *     ４種類以上の抗不安薬及び睡眠薬
           IF     (ORCSPSYCHO-SRYYMD   >=  "20180401")
               IF     (TBL-GRP-CNT (1)     >=  3 )
                  OR  (TBL-GRP-CNT (2)     >=  3 )
                  OR  (TBL-GRP-CNT (3)     >=  3 )
                  OR  (TBL-GRP-CNT (4)     >=  3 )
                  OR ((TBL-GRP-CNT (1) +   TBL-GRP-CNT (2))
                                           >=  4  )
                   MOVE    1                   TO  ORCSPSYCHO-TAZAI
               END-IF
           END-IF
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助マスターチェック処理
      *****************************************************************
      *2001-EFFECTKBN-CHEK-SEC            SECTION.
      *
      *    SET     TBL-PSY             TO  1
      *    SEARCH      TBL-PSYCHO-OCC  VARYING   TBL-PSY
      *            AT  END
      *                MOVE    ZERO            TO  WRK-CLASS
      *                MOVE    SPACE           TO  WRK-GROUP
      *            WHEN   (WRK-YAKCODE         =   TBL-PSYCHO-CODE
      *                                                    (TBL-PSY))
      *            AND   ((WRK-SRYYMD          >=  TBL-PSYCHO-STYMD
      *                                                    (TBL-PSY))
      *            AND    (WRK-SRYYMD          <=  TBL-PSYCHO-EDYMD
      *                                                    (TBL-PSY)))
      *                MOVE    TBL-PSYCHO-CLASS (TBL-PSY)
      *                                         TO  WRK-CLASS
      *                MOVE    TBL-PSYCHO-GROUP (TBL-PSY)
      *                                         TO  WRK-GROUP
      *    END-SEARCH
      *    .
      *2001-EFFECTKBN-CHEK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    補助マスターチェック処理
      *****************************************************************
       2001-EFFECTKBN-CHEK02-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-CLASS
           MOVE    SPACE               TO  WRK-GROUP
      *
           MOVE    SPACE               TO  PSYCHOTROPIC-REC
           INITIALIZE                      PSYCHOTROPIC-REC
           MOVE    WRK-YAKCODE         TO  PSYCHOTROPIC-CODE
           MOVE    WRK-SRYYMD          TO  PSYCHOTROPIC-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  PSYCHOTROPIC-YUKOEDYMD
           MOVE    PSYCHOTROPIC-REC    TO  MCPDATA-REC
           MOVE    "tbl_psychotropic"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_psychotropic"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PSYCHOTROPIC-REC
      *
                   MOVE    PSYCHOTROPIC-CLASS      TO  WRK-CLASS
                   MOVE    PSYCHOTROPIC-GROUPCODE  TO  WRK-GROUP
               ELSE
                   INITIALIZE                  PSYCHOTROPIC-REC
                   MOVE    1                   TO  FLG-PSYCHOTROPIC
               END-IF
           ELSE
               INITIALIZE                  PSYCHOTROPIC-REC
               MOVE    1                   TO  FLG-PSYCHOTROPIC
           END-IF
           MOVE    "tbl_psychotropic" TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-EFFECTKBN-CHEK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    効能区分別件数集計処理
      *****************************************************************
       2002-CLASS-GROUP-SEC            SECTION.
      *
           MOVE    WRK-CLASS           TO  IDX-G
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY              >   50  )
               IF      TBL-GRP-GROUP (IDX-G IDY)
                                           =   SPACE
                   ADD     1               TO  TBL-GRP-CNT (IDX-G)
                   MOVE    WRK-GROUP       TO  TBL-GRP-GROUP
                                                           (IDX-G IDY)
                   MOVE    50              TO  IDY
               ELSE
                   IF      TBL-GRP-GROUP (IDX-G IDY)
                                           =   WRK-GROUP
                       MOVE    50              TO  IDY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2002-CLASS-GROUP-EXT.
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
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
