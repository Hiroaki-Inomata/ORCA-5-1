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
       PROGRAM-ID.                 ORCSHOSPCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入外共通
      *  コンポーネント名  : 医療機関コード切替え判定サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/12/21    NACL−小豆沢  新規作成
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-KSNKBN                  PIC 9(03).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
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
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    医療機関コード切替え判定パラメタ
           COPY    "CPORCSHOSPCHG.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSHOSPCHG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-HOSPCHG-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-HOSPCHG-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *
      *    主処理
           IF      LNK-HOSPCHG-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           DISPLAY "=====  ORCSHOSPCHG  ===="
           DISPLAY "LNK-HOSPCHG-CHGKBN = " LNK-HOSPCHG-CHGKBN
           DISPLAY "=="
           DISPLAY "LNK-HOSPCHG-HOSPCD(1)  = " LNK-HOSPCHG-HOSPCD(1)
           DISPLAY "LNK-HOSPCHG-HOSPCDN(1) = " LNK-HOSPCHG-HOSPCDN(1)
           DISPLAY "LNK-HOSPCHG-HOSPSBT(1) = " LNK-HOSPCHG-HOSPSBT(1)
           DISPLAY "LNK-HOSPCHG-STYUKYMD(1)= " LNK-HOSPCHG-STYUKYMD(1)
           DISPLAY "LNK-HOSPCHG-EDYUKYMD(1)= " LNK-HOSPCHG-EDYUKYMD(1)
           DISPLAY "=="
           DISPLAY "LNK-HOSPCHG-HOSPCD(2)  = " LNK-HOSPCHG-HOSPCD(2)
           DISPLAY "LNK-HOSPCHG-HOSPCDN(2) = " LNK-HOSPCHG-HOSPCDN(2)
           DISPLAY "LNK-HOSPCHG-HOSPSBT(2) = " LNK-HOSPCHG-HOSPSBT(2)
           DISPLAY "LNK-HOSPCHG-STYUKYMD(2)= " LNK-HOSPCHG-STYUKYMD(2)
           DISPLAY "LNK-HOSPCHG-EDYUKYMD(2)= " LNK-HOSPCHG-EDYUKYMD(2)
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
      *    判定年月チェック
           IF    ( LNK-HOSPCHG-HANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-HOSPCHG-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    判定月１日時点の医療機関情報の取得
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-HOSPCHG-HANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-HOSPCHG-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   0
                   MOVE   SYS-1001-HOSPCD
                                       TO  LNK-HOSPCHG-HOSPCD(1)
                   MOVE   SYS-1001-HOSPCDN
                                       TO  LNK-HOSPCHG-HOSPCDN(1)
                   MOVE   SYS-1001-HOSPSBT
                                       TO  LNK-HOSPCHG-HOSPSBT(1)
                   MOVE   SYS-1001-STYUKYMD
                                       TO  LNK-HOSPCHG-STYUKYMD(1)
                   MOVE   SYS-1001-EDYUKYMD
                                       TO  LNK-HOSPCHG-EDYUKYMD(1)
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
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
      *
           PERFORM   210-SYS-1001-CHK-SEC
      *
           IF  LNK-HOSPCHG-HOSPCD(2)  NOT =  ZERO
               MOVE    1              TO     LNK-HOSPCHG-CHGKBN
           ELSE
               MOVE    ZERO           TO     LNK-HOSPCHG-CHGKBN
               INITIALIZE                    LNK-HOSPCHG-HENKYAKU-AREA
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      ******************************************************************
      *    月内の医療期間情報取得                                      *
      ******************************************************************
       210-SYS-1001-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-HOSPCHG-HANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-HOSPCHG-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL   (FLG-SYSKANRI   =   1)  OR
                                (LNK-HOSPCHG-HOSPCD(2)  NOT =  ZERO)
      *
                 IF    SYS-1001-STYUKYMD(7:2)    =   "01"
                    CONTINUE
                 ELSE
                    IF   SYS-1001-HOSPCD  NOT = LNK-HOSPCHG-HOSPCD(1)
                       MOVE   SYS-1001-HOSPCD
                                       TO  LNK-HOSPCHG-HOSPCD(2)
                       MOVE   SYS-1001-HOSPCDN
                                       TO  LNK-HOSPCHG-HOSPCDN(2)
                       MOVE   SYS-1001-HOSPSBT
                                       TO  LNK-HOSPCHG-HOSPSBT(2)
                       MOVE   SYS-1001-STYUKYMD
                                       TO  LNK-HOSPCHG-STYUKYMD(2)
                       MOVE   "99999999"
                                       TO  LNK-HOSPCHG-EDYUKYMD(2)
                    ELSE
      *                医療機関コードに変更がない場合は有効期間の
      *                終了日だけを置き換える
                       MOVE   SYS-1001-EDYUKYMD
                                       TO  LNK-HOSPCHG-EDYUKYMD(1)
                    END-IF
                 END-IF
      *
                 MOVE    "tbl_syskanri"    TO  MCP-TABLE
                 MOVE    "key11"           TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       210-SYS-1001-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター検索
      *****************************************************************
       910-PTINF-SELECT-SEC            SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTINF-REC
               MOVE    ZERO               TO  FLG-PTINF
           ELSE
               INITIALIZE                     PTINF-REC
               MOVE    1                  TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
