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
       PROGRAM-ID.                 ORCSAKYUSEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 亜急性期入院医療管理料判定サブ
      *      平成２４年３月３１日時点に亜急性期入院医療管理料１を算定
      *      している状態か判定するサブプロ
      *                      
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/03/17    NACL−小豆沢  新規作成
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
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *
      *    医療区分コメント並び替え用
           03  WRK-IDX                     PIC 9(04).
           03  WRK-RYOCOM-IRYO-SRYCD       PIC X(09).
           03  WRK-RYOCOM-IRYO-ZAINUM      PIC 9(08).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    療養レセコメント取得パラメタ
           COPY    "CPORCSAKYUSEI.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSAKYUSEI-AREA
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
           MOVE    ZERO                TO  LNK-AKYUSEI-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-AKYUSEI-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-AKYUSEI-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
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
      *    患者マスター検索
           INITIALIZE                       PTINF-REC
           MOVE    LNK-AKYUSEI-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-AKYUSEI-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    9               TO  LNK-AKYUSEI-RC
               GO  TO  100-PRM-CHK-EXT
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
      *    引数から患者入院履歴を検索し、平成２４年３月３１日時点の
      *    亜急性期入院医療管理料の算定有無を取得する
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    LNK-AKYUSEI-HOSPNUM TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-AKYUSEI-PTID    TO  PTNYUINRRK-PTID
           MOVE    LNK-AKYUSEI-RRKNUM  TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM  910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM  910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK    =   ZERO
                    PERFORM   UNTIL  FLG-PTNYUINRRK    =   1
                        IF  (PTNYUINRRK-TENNYUYMD   <=  "20120331") AND
                            (PTNYUINRRK-TENSTUYMD   >   "20120331")
                          IF   (PTNYUINRRK-TOKUTEI-KBN    =  "3")  AND
                               (PTNYUINRRK-TOKUTEI-NYUIN  =  "18")
                             MOVE    1     TO   LNK-AKYUSEI-RC
                          END-IF
                          IF   (PTNYUINRRK-TOKUTEI-KBN    =  "3")  AND
                               (PTNYUINRRK-TOKUTEI-NYUIN  =  "54")
                             MOVE    2     TO   LNK-AKYUSEI-RC
                          END-IF
                        END-IF
                        MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
                        MOVE    "key4"            TO  MCP-PATHNAME
                        PERFORM 910-PTNYUINRRK-READ-SEC
                    END-PERFORM
               ELSE
                   MOVE     9      TO  LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
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
           PERFORM   910-DB-CLOSE-SEC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"    USING MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
