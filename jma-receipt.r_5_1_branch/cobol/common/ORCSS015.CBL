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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSS015.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 請求確認システム管理項目取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/04/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  IDX-AREA.
           03  IDX1                            PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-TEXT                        PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1013.INC".
           COPY    "CPSK1041.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS015.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS015-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-SRYITEMLST-EDIT-SEC
      *
           PERFORM 2001-JIHINAME-EDIT-SEC
      *
           PERFORM 2001-NYKN-HOHO-EDIT-SEC
      *
           PERFORM 2001-SMEIJOTAI-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療内容区分編集処理
      *****************************************************************
       2001-SRYITEMLST-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SS015-SRYITEMLST-G
      *
           MOVE    16              TO  SS015-SRYITEMLST-MAX
      *
           MOVE    "A00"           TO  SS015-SRYITEML (1)
           MOVE    "初・再診料"    TO  SS015-SRYITEMMEIL (1)
      *
           MOVE    "B00"           TO  SS015-SRYITEML (2)
           MOVE    "医学管理等"    TO  SS015-SRYITEMMEIL (2)
      *
           MOVE    "C00"           TO  SS015-SRYITEML (3)
           MOVE    "在宅療養"      TO  SS015-SRYITEMMEIL (3)
      *
           MOVE    "F00"           TO  SS015-SRYITEML (4)
           MOVE    "投薬"          TO  SS015-SRYITEMMEIL (4)
      *
           MOVE    "G00"           TO  SS015-SRYITEML (5)
           MOVE    "注射"          TO  SS015-SRYITEMMEIL (5)
      *
           MOVE    "J00"           TO  SS015-SRYITEML (6)
           MOVE    "処置"          TO  SS015-SRYITEMMEIL (6)
      *
           MOVE    "K00"           TO  SS015-SRYITEML (7)
           MOVE    "手術"          TO  SS015-SRYITEMMEIL (7)
      *
           MOVE    "L00"           TO  SS015-SRYITEML (8)
           MOVE    "麻酔"          TO  SS015-SRYITEMMEIL (8)
      *
           MOVE    "D00"           TO  SS015-SRYITEML (9)
           MOVE    "検査"          TO  SS015-SRYITEMMEIL (9)
      *
           MOVE    "E00"           TO  SS015-SRYITEML (10)
           MOVE    "画像診断"      TO  SS015-SRYITEMMEIL (10)
      *
           MOVE    "H00"           TO  SS015-SRYITEML (11)
           MOVE    "リハビリ"      TO  SS015-SRYITEMMEIL (11)
      *
           MOVE    "I00"           TO  SS015-SRYITEML (12)
           MOVE    "精神科専門"    TO  SS015-SRYITEMMEIL (12)
      *
           MOVE    "M00"           TO  SS015-SRYITEML (13)
           MOVE    "放射線治療"    TO  SS015-SRYITEMMEIL (13)
      *
           MOVE    "N00"           TO  SS015-SRYITEML (14)
           MOVE    "病理診断"      TO  SS015-SRYITEMMEIL (14)
      *
           MOVE    "A10"           TO  SS015-SRYITEML (15)
           MOVE    "入院料"        TO  SS015-SRYITEMMEIL (15)
      *
           MOVE    "001"           TO  SS015-SRYITEML (16)
           MOVE    "療養担当手当"  TO  SS015-SRYITEMMEIL (16)
      *
           .
      *
       2001-SRYITEMLST-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       2001-JIHINAME-EDIT-SEC          SECTION.
      *
           INITIALIZE                  SS015-JIHINAME-G
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1013"          TO  SYS-KANRICD
           MOVE    SS015-SRYYMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1013-REC
      *
               IF    ( SYS-1013-KBNCD(2:1) IS NUMERIC )
                   MOVE    FUNCTION NUMVAL(SYS-1013-KBNCD(2:1))
                                       TO  IDX1
                   IF    ( SYS-1013-KBNCD(2:1) =   "0" )
                       MOVE    10      TO  IDX1
                   END-IF
      *
                   IF    ( SYS-1013-KBNCD(1:1) =   "1" )
                       MOVE    SYS-1013-JIHINAME
                                       TO  SS015-JIHINAME (1 IDX1)
                   ELSE
                       MOVE    SYS-1013-JIHINAME
                                       TO  SS015-JIHINAME (2 IDX1)
                   END-IF
      *
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-JIHINAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入金方法編集処理
      *****************************************************************
       2001-NYKN-HOHO-EDIT-SEC         SECTION.
      *
           INITIALIZE                  SS015-NYKN-HOHOLST-G
      *
           MOVE    ZERO                TO  IDX1
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    SS015-SRYYMD        TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDX1    >=  50 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    IDX1            TO  SS015-NYKN-HOHOLST-MAX
               MOVE    SYS-1041-KBNCD  TO  SS015-NYKN-HOHOL (IDX1)
               MOVE    SYS-1041-NYKN-TANNAME
                                       TO  SS015-NYKN-HOHOMEIL (IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-JIHINAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納明細状態編集処理
      *****************************************************************
       2001-SMEIJOTAI-EDIT-SEC         SECTION.
      *
           INITIALIZE                  SS015-SMEIJOTAILST-G
      *
           MOVE   10               TO  SS015-SMEIJOTAILST-MAX
      *
           MOVE   SPACE            TO  SS015-SMEIJOTAIL   (1)
           MOVE   "請求額なし"     TO  SS015-SMEIJOTAIMEIL (1)
      *
           MOVE   "1"              TO  SS015-SMEIJOTAIL   (2)
           MOVE   "請求"           TO  SS015-SMEIJOTAIMEIL (2)
      *
           MOVE   "2"              TO  SS015-SMEIJOTAIL   (3)
           MOVE   "入金"           TO  SS015-SMEIJOTAIMEIL (3)
      *
           MOVE   "3"              TO  SS015-SMEIJOTAIL   (4)
           MOVE   "再計算"         TO  SS015-SMEIJOTAIMEIL (4)
      *
           MOVE   "4"              TO  SS015-SMEIJOTAIL   (5)
           MOVE   "返金"           TO  SS015-SMEIJOTAIMEIL (5)
      *
           MOVE   "5"              TO  SS015-SMEIJOTAIL   (6)
           MOVE   "請求取消"       TO  SS015-SMEIJOTAIMEIL (6)
      *
           MOVE   "6"              TO  SS015-SMEIJOTAIL   (7)
           MOVE   "入金取消"       TO  SS015-SMEIJOTAIMEIL (7)
      *
           MOVE   "7"              TO  SS015-SMEIJOTAIL   (8)
           MOVE   "継続"           TO  SS015-SMEIJOTAIMEIL (8)
      *
           MOVE   "8"              TO  SS015-SMEIJOTAIL   (9)
           MOVE   "診療訂正"       TO  SS015-SMEIJOTAIMEIL (9)
      *
           MOVE   "9"              TO  SS015-SMEIJOTAIL   (10)
           MOVE   "請求"           TO  SS015-SMEIJOTAIMEIL (10)
      *
           .
       2001-SMEIJOTAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
