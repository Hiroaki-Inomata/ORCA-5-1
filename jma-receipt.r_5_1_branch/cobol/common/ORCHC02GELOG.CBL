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
       PROGRAM-ID.             ORCHC02GELOG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 処方箋　一般名記載ログ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/03/18    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-GENERICLOG      PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YAKKAKJNCD          PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
      *
      *    一般名例外コードテーブル
           COPY    "CMGENERICTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    一般名記載ログデータ
       01  GENERICLOG-REC.
           COPY    "CPGENERICLOG.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *    一般名記載ログ作成パラメタ
           COPY    "CPORCHC02GELOG.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC02GELOGAREA
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
           MOVE    ZERO                TO  ORCHC02GELOG-RCD
      *
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *
      *    一般名記載ログ出力
           PERFORM 200-MAIN-SYORI-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    時間重複チェック
           PERFORM UNTIL    FLG-CHK    =   1
      *        更新日取得
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
      *        一般名記載ログ
               PERFORM 1001-GENERICLOG-KEN-SEC
               IF      FLG-GENERICLOG      =   1
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載ログ出力処理
      *****************************************************************
       1001-GENERICLOG-KEN-SEC             SECTION.
      *
           INITIALIZE                      GENERICLOG-REC
           MOVE    SPA-HOSPNUM         TO  GENERICLOG-HOSPNUM
           MOVE    SPA-PTID            TO  GENERICLOG-PTID
           MOVE    SPA-SRYYMD          TO  GENERICLOG-SRYYMD
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  GENERICLOG-PRINTYMD
      *    作成時間
           MOVE    SMCNDATE-HMS        TO  GENERICLOG-PRINTHMS
      *    連番
           MOVE    1                   TO  GENERICLOG-RECIPENUM
      *
           MOVE    GENERICLOG-REC      TO  MCPDATA-REC
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_log"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICLOG-REC
                   MOVE    ZERO                TO  FLG-GENERICLOG
               ELSE
                   MOVE    1                   TO  FLG-GENERICLOG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-GENERICLOG
           END-IF
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-GENERICLOG-KEN-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載ログ出力処理
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   100 )
                         OR  (ORCHC02GELOG-SRYCD (IDX) =   SPACE)
                         OR  (ORCHC02GELOG-RCD     NOT =   ZERO )
      *        一般名記載ログ出力
               PERFORM 2001-GENERICLOG-SYORI-SEC
           END-PERFORM
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名記載ログ出力処理
      *****************************************************************
       2001-GENERICLOG-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  GENERICLOG-REC
           INITIALIZE                      GENERICLOG-REC
           MOVE    SPA-HOSPNUM         TO  GENERICLOG-HOSPNUM
           MOVE    SPA-PTID            TO  GENERICLOG-PTID
           MOVE    SPA-SRYYMD          TO  GENERICLOG-SRYYMD
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  GENERICLOG-PRINTYMD
      *    作成時間
           MOVE    SMCNDATE-HMS        TO  GENERICLOG-PRINTHMS
      *    連番
           MOVE    IDX                 TO  GENERICLOG-RECIPENUM
      *
      *    処理区分
           MOVE    ORCHC02GELOG-SYORIKBN
                                       TO  GENERICLOG-SYORIKBN
      *    伝票番号
           MOVE    ORCHC02GELOG-DENPNUM (IDX)
                                       TO  GENERICLOG-DENPNUM
      *    剤番号
           MOVE    ORCHC02GELOG-ZAINUM (IDX)
                                       TO  GENERICLOG-ZAINUM
      *    診療コード
           MOVE    ORCHC02GELOG-SRYCD (IDX)
                                       TO  GENERICLOG-SRYCD
      *    薬価基準コード（一般名コード）
      *    一般名例外コード変換
           PERFORM 20039-GENERICCD-HEN-SEC
           MOVE    WRK-YAKKAKJNCD      TO  GENERICLOG-YAKKAKJNCD
      *    一般名記載内容
           MOVE    ORCHC02GELOG-GENERICNAME (IDX)
                                       TO  GENERICLOG-GENERICNAME
      *
           MOVE    GENERICLOG-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_generic_log"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    9                   TO  ORCHC02GELOG-RCD
               DISPLAY "GENERICLOG INS ERR:" MCP-RC 
           END-IF
           .
       2001-GENERICLOG-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       20039-GENERICCD-HEN-SEC         SECTION.
      *
           SET     TBL-GEN             TO  1
           SEARCH      TBL-GENERIC-OCC     VARYING   TBL-GEN
                   AT  END
                       MOVE    SPACE            TO  WRK-YAKKAKJNCD
                       MOVE    SPACE            TO  WRK-GENERIC-STYMD
                       MOVE    SPACE            TO  WRK-GENERIC-EDYMD
                   WHEN    ORCHC02GELOG-YAKKAKJNCD(IDX)
                                               =   TBL-YAKKAKJNCD
                                                           (TBL-GEN)
                       MOVE    TBL-GENERIC-CODE (TBL-GEN)
                                                TO  WRK-YAKKAKJNCD
                       MOVE    TBL-GENERIC-STYMD (TBL-GEN)
                                                TO  WRK-GENERIC-STYMD
                       MOVE    TBL-GENERIC-EDYMD (TBL-GEN)
                                                TO  WRK-GENERIC-EDYMD
           END-SEARCH
      *
           IF      WRK-YAKKAKJNCD  NOT =   SPACE
      *        有効期間判定
               IF    (SPA-SRYYMD           >=  WRK-GENERIC-STYMD)
                 AND (SPA-SRYYMD           <=  WRK-GENERIC-EDYMD)
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-YAKKAKJNCD
               END-IF
           END-IF
      *
           IF      WRK-YAKKAKJNCD      =   SPACE
               MOVE    ORCHC02GELOG-YAKKAKJNCD(IDX)(1:9)
                                           TO  WRK-YAKKAKJNCD
           END-IF
           .
       20039-GENERICCD-HEN-EXT.
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
      *    テーブル読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
