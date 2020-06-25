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
       PROGRAM-ID.             ORCSUSERCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ユーザプログラム起動判定サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/10/07    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.08.00    NACL-多々納  16/06/22  伝票番号追加対応
      *  05.00.00    NACL-太田    17/08/10  Ginbee対応
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
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-GYOUMU-ARI      PIC 9(01).
           03  FLG-GMN-ARI         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KANRICD         PIC X(04).
      *
           03  WRK-GYOUMU-FLG      PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9700.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9701.INC".
           COPY  "CPSK3103.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERPG.INC".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      **** COPY    "ORCA-DBPATH".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ユーザプログラム起動チェックサブパラメタ
           COPY    "CPORCSUSERCHK.INC".
grpsys*
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSUSERCHKAREA
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
           INITIALIZE                  SBC-AREA
      *
           INITIALIZE                  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           IF    ( SGETTEMP-MW     =   SPA-GINBEE )
               CALL    "ORCSBILLINGCHECK"  USING
                                           SBC-AREA
                                           SPA-AREA
           END-IF
      *
           EVALUATE    ORCSUSERCHK-SYORI
               WHEN    "1"
      *            チェックのみ
                   PERFORM 100-SYSKANRI-CHK-SEC
                   MOVE    WRK-GYOUMU-FLG  TO  ORCSUSERCHK-GYOUMU-FLG
                   MOVE    FLG-GYOUMU-ARI  TO  ORCSUSERCHK-GYOUMU-ARI
               WHEN    "2"
      *            画面表示チェック・実行
                   PERFORM 100-SYSKANRI-CHK-SEC
                   IF      FLG-GYOUMU-ARI      =   1
      *                ユーザプログラム起動
                       PERFORM 200-SYORNM-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ユーザプログラム起動チェク処理
      *****************************************************************
       100-SYSKANRI-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-9700-REC
           INITIALIZE                      SYS-9700-REC
           MOVE    "9700"              TO  SYS-9700-KANRICD
           MOVE    "1"                 TO  SYS-9700-KBNCD
           MOVE    "00000000"          TO  SYS-9700-STYUKYMD
           MOVE    "99999999"          TO  SYS-9700-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
           MOVE    SYS-9700-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPACE               TO  WRK-GYOUMU-FLG
           MOVE    ZERO                TO  FLG-GYOUMU-ARI
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9700-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   25
                   IF      SYS-9700-GYOUMUID (IDX)
                                           =   ORCSUSERCHK-GMNPG
                       MOVE    SYS-9700-GYOUMU-KANRICD (IDX)
                                               TO  WRK-KANRICD
      *                画面表示
                       MOVE    SYS-9700-GYOUMU-FLG (IDX)
                                               TO  WRK-GYOUMU-FLG
      *                明細がある時、実行
                       IF      SYS-9700-GYOUMU-CNT (IDX)
                                               >   ZERO
                            MOVE    1              TO  FLG-GYOUMU-ARI
                       END-IF
                       MOVE    25              TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SGETTEMP-MW         =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER     =   SPACE )
            AND  ( WRK-KANRICD     NOT =   SPACE )
               MOVE    ZERO            TO  FLG-GYOUMU-ARI
               PERFORM 1001-SYSKANRI-CHK-SEC
           END-IF
      *
           .
       100-SYSKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動チェク処理
      *****************************************************************
       1001-SYSKANRI-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    WRK-KANRICD         TO  SYS-9701-KANRICD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL    ( FLG-SYSKANRI     NOT =   ZERO )
                    OR      ( FLG-GYOUMU-ARI   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC         TO  SYS-9701-REC
      *
               IF    ( SYS-9701-EXECUTE-SW =   "1" )
      *
                   PERFORM 900-SYS-3103-READ-SEC
                   IF    ( SYS-3103-REPOS-NAME =   SPACE )
                       MOVE    1               TO  FLG-GYOUMU-ARI
                   END-IF
      *
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-SYSKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    業務起動処理
      *****************************************************************
       200-SYORNM-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    WRK-KANRICD         TO  SYS-9701-KANRICD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL     (FLG-SYSKANRI     =   1 )
               MOVE    MCPDATA-REC         TO  SYS-9701-REC
      *
               PERFORM 900-SYS-3103-READ-SEC
               IF    ( SPA-MW                  =   SPA-GINBEE )
                AND  ( SBC-BILLINGUSER         =   SPACE )
                AND  ( SYS-3103-REPOS-NAME NOT =   SPACE )
                   CONTINUE
               ELSE
                   IF     (SYS-9701-EXECUTE-SW     =   "1" ) AND
                          (SYS-9701-EXECUTE-YTPE   =   "2" )
                       PERFORM 2001-SHELL-SYORI-SEC
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-SYORNM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       2001-SHELL-SYORI-SEC             SECTION.
      *
           INITIALIZE                  ORCSUSERPGAREA
      *    処理ＰＧ
           MOVE    ORCSUSERCHK-GMNPG   TO  ORCSUSERPG-GMNPG
      *    処理区分
           MOVE    ORCSUSERCHK-SYORI-FLG   TO  ORCSUSERPG-SYORI-FLG
      *    システム日付西暦
           MOVE    ORCSUSERCHK-SYSYMD  TO  ORCSUSERPG-SYSYMD
      *    診療日
           MOVE    ORCSUSERCHK-SRYYMD  TO  ORCSUSERPG-SRYYMD
      *    患者ＩＤ
           MOVE    ORCSUSERCHK-PTID    TO  ORCSUSERPG-PTID
      *    患者番号
           MOVE    ORCSUSERCHK-PTNUM   TO  ORCSUSERPG-PTNUM
      *    診療科
           MOVE    ORCSUSERCHK-SRYKA   TO  ORCSUSERPG-SRYKA
      *    ドクター
           MOVE    ORCSUSERCHK-DRCD    TO  ORCSUSERPG-DRCD
      *    保険組合せ
           MOVE    ORCSUSERCHK-HKNCOMBI    TO  ORCSUSERPG-HKNCOMBI
      *    伝票番号
           MOVE    ORCSUSERCHK-DENPNUM     TO  ORCSUSERPG-DENPNUM
      *    処理・帳票名
           MOVE    SYS-9701-SYORINM    TO  ORCSUSERPG-SYORINM
      *    実行
           MOVE    SYS-9701-SHELL      TO  ORCSUSERPG-SHELL
      *    プリンタ名
           MOVE    SYS-9701-PRTNM      TO  ORCSUSERPG-PRTNM
      *    リポジトリ名
           IF    ( SPA-MW                  =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER         =   "1" )
               MOVE    SYS-3103-REPOS-NAME
                                       TO  ORCSUSERPG-REPOS-NAME
           END-IF
      *
           CALL    "ORCSUSERPG"        USING
                                       ORCSUSERPGAREA
                                       SPA-AREA
      *
           .
       2001-SHELL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-SYS-3103-READ-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           MOVE    "3103"              TO  SYS-3103-KANRICD
           MOVE    SYS-9701-KANRICD    TO  SYS-3103-KBNCD
           MOVE    "0"                 TO  SYS-3103-KBNCD(5:1)
           MOVE    SYS-9701-KBNCD(1:2) TO  SYS-3103-KBNCD(6:2)
           MOVE    SPA-SYSYMD          TO  SYS-3103-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3103-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
           MOVE    SYS-3103-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           IF  MCP-RC              =   ZERO
               PERFORM 990-SYSKANRI-READ-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-3103-REC
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYS-3103-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
