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
       PROGRAM-ID.                 ORCSSRCCOMMCD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : コメントコード検索サブ
      *  返却エラーコード  : 0:該当コードあり、1:該当コードなし、
      *                      2:コメント種類不正
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13.07.23    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付        内容
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
           03  FLG-TENSU           PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-STSRYCD         PIC X(09).
           03  WRK-EDSRYCD         PIC X(09).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    コメントコード検索
           COPY    "CPORCSSRCCOMMCD.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION
                                   USING
                                       ORCSSRCCOMMCD-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    0                   TO  SRCCOM-RC
           MOVE    SPACE               TO  SRCCOM-COMMENT-CODE
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  WRK-AREA
      *
           PERFORM 110-COMMENT-KENSAKU-SEC
           .
       000-PROC-EXT.
           EXIT PROGRAM.
      *****************************************************************
      *    コメントコード検索処理
      *****************************************************************
       110-COMMENT-KENSAKU-SEC     SECTION.
      *
      *    コメント種類
           EVALUATE    SRCCOM-COMMENT-KBN
      *        服用方法
               WHEN    1
                   MOVE    "001000000"         TO  WRK-STSRYCD
                   MOVE    "001999999"         TO  WRK-EDSRYCD
      *        画像診断部位
               WHEN    2
                   MOVE    "002000000"         TO  WRK-STSRYCD
                   MOVE    "002999999"         TO  WRK-EDSRYCD
      *        コメント
               WHEN    8
                   MOVE    "820000000"         TO  WRK-STSRYCD
                   MOVE    "829999999"         TO  WRK-EDSRYCD
      *        コメント種類不正
               WHEN   OTHER
                   MOVE    2                   TO  SRCCOM-RC
                   GO  TO  110-COMMENT-KENSAKU-EXT
           END-EVALUATE
      *
      *    点数マスタ読み込み処理
           PERFORM 900-TENSU-READ-SEC
      *
      *    該当データあり
           IF      FLG-TENSU           =   ZERO
               MOVE    0                   TO  SRCCOM-RC
               MOVE    TNS-SRYCD           TO  SRCCOM-COMMENT-CODE
      *    該当データなし
           ELSE
               MOVE    1                   TO  SRCCOM-RC
           END-IF
           .
       110-COMMENT-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読み込み処理
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRCCOM-MSTYMD       TO  TNS-YUKOSTYMD
           MOVE    SRCCOM-MSTYMD       TO  TNS-YUKOEDYMD
           MOVE    WRK-STSRYCD         TO  TNS-SRYCD
           MOVE    WRK-EDSRYCD         TO  TNSUP-SRYCD
           MOVE    SRCCOM-COMMENT      TO  TNS-NAME
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
