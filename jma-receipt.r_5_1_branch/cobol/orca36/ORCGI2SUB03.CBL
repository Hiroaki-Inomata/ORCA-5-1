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
       PROGRAM-ID.                     ORCGI2SUB03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院患者照会
      *  コンポーネント名  : 入院患者照会サブ（コンボボックス情報取得）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDXL                            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CD99-G.
               05  WRK-CD99                    PIC 9(02).
           03  WRK-KANRICD                     PIC X(04).
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-SRYKALST-MAX              PIC 9(05)   VALUE 99.
           03  CONST-BTUNUMLST-MAX             PIC 9(05)   VALUE 50.
           03  CONST-TAIINCDLST-MAX            PIC 9(05)   VALUE 50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者状態コメント
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5013.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPI2SUB03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           I2SUB03-AREA
           SPA-AREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
                                   I2SUB03-ITEM-LIST-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療科コンボボックス初期処理
           PERFORM 2001-SRYKALST-SEC
      *
      *    病棟番号コンボボックス初期処理
           PERFORM 2001-BTUNUMLST-SEC
      *
      *    状態コンボボックス初期処理
           PERFORM 2001-JOTAILST-SEC
      *
      *    請求状態コンボボックス初期処理
           PERFORM 2001-SEIKYUJOTAILST-SEC
      *
      *    死亡区分コンボボックス初期処理
           PERFORM 2001-DEATHKBNLST-SEC
      *
      *    テスト患者コンボボックス初期処理
           PERFORM 2001-TSTPTLST-SEC
      *
      *    退院事由コンボボックス初期処理
           PERFORM 2001-TAIINCDLST-SEC
      *
      *    検索項目コンボボックス初期処理
           PERFORM 2001-SORTLST-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療科コンボボックス初期処理
      *****************************************************************
       2001-SRYKALST-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-SRYKALST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-SRYKALST-MAX
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                       TO  I2SUB03-SRYKAL (IDXL)
               MOVE    SYS-1005-SRYKANAME
                                       TO  I2SUB03-SRYKAMEIL (IDXL)
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
       2001-SRYKALST-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号コンボボックス初期処理
      *****************************************************************
       2001-BTUNUMLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-BTUNUMLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-BTUNUMLST-MAX
      *
               MOVE    SYS-5001-KBNCD (1:2)
                                       TO  I2SUB03-BTUNUML (IDXL)
               MOVE    SYS-5001-BTU-NAME
                                       TO  I2SUB03-BTUNUMMEIL (IDXL)
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
       2001-BTUNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    状態コンボボックス初期処理
      *****************************************************************
       2001-JOTAILST-SEC                   SECTION.
      *
           MOVE    4                   TO  I2SUB03-JOTAILST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-JOTAIL (1)
           MOVE    "入院中"            TO  I2SUB03-JOTAIMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-JOTAIL (2)
           MOVE    "退院済"            TO  I2SUB03-JOTAIMEIL (2)
      *
           MOVE    "3"                 TO  I2SUB03-JOTAIL (3)
           MOVE    "当月在院"          TO  I2SUB03-JOTAIMEIL (3)
      *
           MOVE    "4"                 TO  I2SUB03-JOTAIL (4)
           MOVE    "当日在院"          TO  I2SUB03-JOTAIMEIL (4)
      *
           IF    ( I2SUB03-FROM        =   "API" )
      *
               COMPUTE I2SUB03-JOTAILST-MAX
                   =   I2SUB03-JOTAILST-MAX    +   1
      *
               MOVE    "5"             TO  I2SUB03-JOTAIL (5)
               MOVE    "仮保険組合せ登録"
                                       TO  I2SUB03-JOTAIMEIL (5)
           END-IF
      *
           .
       2001-JOTAILST-EXT.
           EXIT.
      *****************************************************************
      *    請求状態コンボボックス初期処理
      *****************************************************************
       2001-SEIKYUJOTAILST-SEC             SECTION.
      *
           MOVE    2                   TO  I2SUB03-SEIKYUJOTAILST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-SEIKYUJOTAIL (1)
           MOVE    "退院請求済"        TO  I2SUB03-SEIKYUJOTAIMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-SEIKYUJOTAIL (2)
           MOVE    "直近の定期請求済"  TO  I2SUB03-SEIKYUJOTAIMEIL (2)
      *
           .
       2001-SEIKYUJOTAILST-EXT.
           EXIT.
      *****************************************************************
      *    死亡コンボ初期処理
      *****************************************************************
       2001-DEATHKBNLST-SEC                   SECTION.
      *
           MOVE    2                   TO  I2SUB03-DEATHKBNLST-MAX
      *
           MOVE    "0"                 TO  I2SUB03-DEATHKBNL    (1)
           MOVE    "未設定"            TO  I2SUB03-DEATHKBNMEIL (1)
      *
           MOVE    "1"                 TO  I2SUB03-DEATHKBNL    (2)
           MOVE    "死亡"              TO  I2SUB03-DEATHKBNMEIL (2)
      *
           .
       2001-DEATHKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者コンボ初期処理
      *****************************************************************
       2001-TSTPTLST-SEC                   SECTION.
      *
           MOVE    3                   TO  I2SUB03-TSTPTLST-MAX
      *
           MOVE    "a"                 TO  I2SUB03-TSTPTL    (1)
           MOVE    "テスト患者を除く"  TO  I2SUB03-TSTPTMEIL (1)
      *
           MOVE    "b"                 TO  I2SUB03-TSTPTL    (2)
           MOVE    "テスト患者を含める" TO I2SUB03-TSTPTMEIL (2)
      *
           MOVE    "1"                 TO  I2SUB03-TSTPTL    (3)
           MOVE    "テスト患者のみ"    TO  I2SUB03-TSTPTMEIL (3)
      *
           .
       2001-TSTPTLST-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号コンボボックス初期処理
      *****************************************************************
       2001-TAIINCDLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5013"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-TAIINCDLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-TAIINCDLST-MAX
      *
               MOVE    SYS-5013-KBNCD (1:2)
                                       TO  I2SUB03-TAIINCDL (IDXL)
               MOVE    SYS-5013-NAME
                                       TO  I2SUB03-TAIINCDMEIL (IDXL)
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
       2001-TAIINCDLST-EXT.
           EXIT.
      *****************************************************************
      *    検索項目コンボボックス初期処理
      *****************************************************************
       2001-SORTLST-SEC                    SECTION.
      *
           MOVE    7                   TO  I2SUB03-SORTLST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-SORTL (1)
           MOVE    "病棟、病室順"      TO  I2SUB03-SORTMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-SORTL (2)
           MOVE    "患者番号順"        TO  I2SUB03-SORTMEIL (2)
      *
           MOVE    "3"                 TO  I2SUB03-SORTL (3)
           MOVE    "カナ氏名順"        TO  I2SUB03-SORTMEIL (3)
      *
           MOVE    "4"                 TO  I2SUB03-SORTL (4)
           MOVE    "入院日の新しい順"  TO  I2SUB03-SORTMEIL (4)
      *
           MOVE    "5"                 TO  I2SUB03-SORTL (5)
           MOVE    "入院日の古い順"    TO  I2SUB03-SORTMEIL (5)
      *
           MOVE    "6"                 TO  I2SUB03-SORTL (6)
           MOVE    "退院日の新しい順"  TO  I2SUB03-SORTMEIL (6)
      *
           MOVE    "7"                 TO  I2SUB03-SORTL (7)
           MOVE    "退院日の古い順"    TO  I2SUB03-SORTMEIL (7)
      *
           .
       2001-SORTLST-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタFETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
