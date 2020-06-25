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
       PROGRAM-ID.             ORCSPRGOPTION.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : プログラムオプションパラメタ設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/02/22    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
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
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *
           03  FLG-CHKEND           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDY                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MAX             PIC 9(05).
           03  WRK-PARA            PIC X(100).
           03  WRK-OPTION          PIC X(44000).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY  "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    共通パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSPRGOPTIONAREA
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
           INITIALIZE                  ORCSPRGOPTION-OUT-G
      *
      *    初期編集処理
           PERFORM 100-SYORI-SEC
      *    内容編集
           IF      ORCSPRGOPTION-RC    =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    パラメタ桁数
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSPRGOPTION-IN-DATA
                                       TO  KANACHK-MAE-INPUT
           MOVE    30                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-MAX
      *
      *    プログラムオプション検索
           PERFORM 1001-PRGOPTION-SET-SEC
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプション検索処理
      *****************************************************************
       1001-PRGOPTION-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    ORCSPRGOPTION-PRGID TO  PRGOPTION-PRGID
           MOVE    "*"                 TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   1
               MOVE    9               TO  ORCSPRGOPTION-RC
           END-IF
           .
       1001-PRGOPTION-SET-EXT.
           EXIT.
      *****************************************************************
      *    主 処理
      *****************************************************************
       200-MAIN-SYORI-SEC         SECTION.
      *
           MOVE    PRGOPTION-OPTION    TO  WRK-OPTION
           MOVE    SPACE               TO  WRK-PARA
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHKEND
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   44000 )
                          OR  (FLG-OK  NOT =   ZERO   )
                          OR  (FLG-CHKEND  =   1      )
      *        # 以降はコメントとする
               IF      WRK-OPTION(IDX:1)       =   "#"
                   MOVE    1                   TO  FLG-CHK
               END-IF
               IF      WRK-OPTION(IDX:1)       =   X"0A"
                   IF      FLG-CHK             =   1
                       MOVE    ZERO                TO  FLG-CHK
                   END-IF
               END-IF
      *!!!
               IF      FLG-CHK         =   ZERO
      *H28.5      空白で終了
                  IF    ( WRK-OPTION(IDX:)   =   SPACE )
                    OR  ((IDX +  WRK-MAX )   >   44000 )
                      MOVE   1                TO  FLG-CHKEND
                  END-IF
      *
                  IF     FLG-CHKEND       =   ZERO
      *
      *            # から 改行までは対象外とする
                   IF      WRK-OPTION(IDX:WRK-MAX)
                                       =   ORCSPRGOPTION-IN-DATA
                                                          (1:WRK-MAX)
                       MOVE    ZERO           TO  FLG-CHK2
      *                前が空白か改行であること
                       COMPUTE IDX4        =   (IDX   -  1 )
                       IF     (IDX4        =   ZERO )
                         OR  ((IDX4        >   ZERO )
                         AND  (WRK-OPTION(IDX4:1)   =   SPACE
                                                OR  X"0A" ))
                           CONTINUE
                       ELSE
                           MOVE    1              TO  FLG-CHK2
                       END-IF
                       IF      FLG-CHK2        =   ZERO
      *
                       COMPUTE IDX2        =   (IDX   +  WRK-MAX)
                       IF      IDX2        >=  44000
                           MOVE    2           TO  FLG-OK
                       ELSE
                           IF      WRK-OPTION(IDX2:1)      =   "="
                               PERFORM 2001-PARA-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
      *
                   END-IF
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-PARA        NOT =   SPACE
      *        パラメタ桁数
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-PARA            TO  KANACHK-MAE-INPUT
               MOVE    100                 TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
               MOVE    KANACHK-MAX         TO  ORCSPRGOPTION-OT-SIZE
      *        全角
               IF      KANACHK-SYUBETU     =   1
                   MOVE    1               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
      *        半角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    2               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
      *        混在
               IF      KANACHK-RC          =   1
                   MOVE    3               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
               MOVE    WRK-PARA            TO  ORCSPRGOPTION-OT-PARA
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションマスター読込
      *****************************************************************
       2001-PARA-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDX2
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX3    FROM   IDX2   BY  1
                   UNTIL      (IDX3        >   44000 )
                          OR  (IDY         >=  100  )
                          OR  (FLG-OK  NOT =   ZERO   )
               IF      WRK-OPTION(IDX3:1)  =   X"0A"
                                           OR  "#"
                   MOVE    1                   TO  FLG-OK
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    WRK-OPTION(IDX3:1)  TO  WRK-PARA(IDY:1)
               END-IF
           END-PERFORM
           .
       2001-PARA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションマスター読込
      *****************************************************************
       900-PRGOPTION-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRGOPTION-REC
               MOVE    ZERO                TO  FLG-PRGOPTION
           ELSE
               INITIALIZE                      PRGOPTION-REC
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
      *
           .
       900-PRGOPTION-READ-EXT.
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
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
