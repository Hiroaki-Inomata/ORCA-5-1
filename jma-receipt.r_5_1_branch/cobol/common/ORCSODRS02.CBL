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
       PROGRAM-ID.                     ORCSODRS02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集サブ２（基本項目チェック）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/17    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
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
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-HIZUKE-ERR          PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-SYS-KANRICD             PIC X(04).
           03  WRK-SYS-KBNCD               PIC X(08).
           03  WRK-ORDERID                 PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-ITEMNUM-MAX               PIC 9(05)   VALUE 30.
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SODR-OT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
      *
           PERFORM 2001-HOSPNUM-CHK-SEC
      *
           IF    ( SODR-ERRCD      =   SPACE )
               PERFORM 2001-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SODR-ERRCD      =   SPACE )
               PERFORM 2001-PTNUM-CHK-SEC
           END-IF
      *
           IF    ( SODR-ERRCD      =   SPACE )
               PERFORM 2001-SRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SODR-ERRCD      =   SPACE )
               PERFORM 2001-ORDERID-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関ＩＤチェック処理
      *****************************************************************
       2001-HOSPNUM-CHK-SEC             SECTION.
      *
           MOVE    "1001"              TO  WRK-SYS-KANRICD
           MOVE    "*"                 TO  WRK-SYS-KBNCD
      *
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *
           IF    ( SYS-1001-HOSPNUM NOT =   ORDER-HOSPNUM )
               MOVE    "0001"          TO  SODR-ERRCD
           END-IF
      *
           .
       2001-HOSPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       2001-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( ORDER-NYUGAIKBN     NOT =   "1" )
               MOVE    "0006"          TO  SODR-ERRCD
           END-IF
      *
           .
       2001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       2001-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( ORDER-PTNUM     =   SPACE )
               MOVE    "0004"  TO  SODR-ERRCD
           ELSE
      *
               PERFORM 900-PTNUM-KEY2-SEL-SEC
               IF    ( FLG-PTNUM   NOT =   ZERO )
                   MOVE    "0004"  TO  SODR-ERRCD
               END-IF
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日チェック処理
      *****************************************************************
       2001-SRYYMD-CHK-SEC             SECTION.
      *
           MOVE    ORDER-SRYYMD        TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    "0007"          TO  SODR-ERRCD
           END-IF
      *
           .
       2001-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダーＩＤチェック処理
      *****************************************************************
       2001-ORDERID-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ORDERID
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   NORDERID-MAX )
                    OR   ( WRK-ORDERID NOT =   SPACE )
      *
               IF    ( NORDERID-ORCAID (IDX0) =   ORDER-ORDERID )
                   MOVE    NORDERID-ORCAID (IDX0)
                                       TO  WRK-ORDERID
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-ORDERID     =   SPACE )
               MOVE    "0013"          TO  SODR-ERRCD
           END-IF
      *
           .
       2001-ORDERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    WRK-SYMD            TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           INITIALIZE                      ORDER-REC
           MOVE    SODR-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SODR-KARTE-KEY      TO  ORDER-KARTE-KEY
           MOVE    SODR-ORDERNUM       TO  ORDER-ORDERNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ORDER-REC
           ELSE
               INITIALIZE                  ORDER-REC
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-SYS-KBNCD       TO  SYS-KBNCD
           MOVE    ORDER-SRYYMD        TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    ORDER-HOSPNUM        TO  PTNUM-HOSPNUM
           MOVE    ORDER-PTID          TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY2)
      *****************************************************************
       900-PTNUM-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    ORDER-HOSPNUM        TO  PTNUM-HOSPNUM
           STRING  ORDER-PTNUM         DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    PTNUM-PTNUM
           END-STRING
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
