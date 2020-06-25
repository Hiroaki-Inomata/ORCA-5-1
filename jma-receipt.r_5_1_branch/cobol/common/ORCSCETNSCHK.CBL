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
       PROGRAM-ID.                 ORCSCETNSCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 電子点数表マスタチェック処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/01/20    NACL−多々納  新規作成
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
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ETENSU1             PIC 9(01).
           03  FLG-ETENSU2             PIC 9(01).
           03  FLG-ETENSU2-JMA         PIC 9(01).
           03  FLG-ETENSU2-OFF         PIC 9(01).
           03  FLG-ETENSU2-SMP         PIC 9(01).
      *
           03  FLG-ETENSU2-ARI         PIC 9(01).
           03  FLG-DATA-ARI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-GROUP               PIC X(07).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    電子点数表　補助マスター
       01  ETENSU1-REC.
           COPY    "CPETENSU1.INC".
      *
      *    包括・被包括マスター
       01  ETENSU2-REC.
           COPY    "CPETENSU2.INC".
      *
      *    包括・被包括マスター(ORCA)
       01  ETENSU2-JMA-REC.
           COPY    "CPETENSU2-JMA.INC".
      *
      *    包括・被包括マスター(ユーザーオフ）
       01  ETENSU2-OFF-REC.
           COPY    "CPETENSU2-OFF.INC".
      *
      *    検体コメントテーブル(ＯＲＣＡ）
       01  ETENSU2-SMP-REC.
           COPY    "CPETENSU2-SAMPLE.INC".
      *
      *    背反関連
       01  ETENSU3-REC.
           COPY    "CPETENSU3.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    電子点数表マスタチェックパラメタ
           COPY    "CPORCSCETNSCHK.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCETNSCHKAREA
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
           INITIALIZE                      ORCSCETNSCHK-OUTAREA
      *
           MOVE    ORCSCETNSCHK-SRYYMD     TO  WRK-SRYYMD
           IF      ORCSCETNSCHK-SRYYMD     =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           END-IF
           EVALUATE    ORCSCETNSCHK-SYORI
               WHEN    "1"
      *            包括チェック
                   PERFORM 200-HOUKATU-SYORI-SEC
           END-EVALUATE
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    包括チェック処理
      *****************************************************************
       200-HOUKATU-SYORI-SEC            SECTION.
      *
      *    補助マスターチェック
           PERFORM 100-ETNSU1-CHEK-SEC
      *
           MOVE    ZERO                TO  FLG-DATA-ARI
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX              >   3   )
                       OR    (FLG-ETENSU1      =   1   )
                       OR    (FLG-DATA-ARI     =   1   )
               IF     (ETENSU1-H-GROUP(IDX)    =   SPACE )
                  OR  (ETENSU1-H-GROUP(IDX)(1:1)   =   "0")
                   CONTINUE
               ELSE
                   MOVE    ETENSU1-H-GROUP(IDX) TO  WRK-GROUP
                   PERFORM 2001-HOUKATU-CHEK-SEC
               END-IF
           END-PERFORM
           IF      FLG-DATA-ARI        =   1
               MOVE    1               TO  ORCSCETNSCHK-CHKKBN
           END-IF
           .
       200-HOUKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助マスターチェック処理
      *****************************************************************
       100-ETNSU1-CHEK-SEC            SECTION.
      *
           MOVE    SPACE               TO  ETENSU1-REC
           INITIALIZE                  ETENSU1-REC
           MOVE    ORCSCETNSCHK-SRYCD1 TO  ETENSU1-SRYCD
           MOVE    WRK-SRYYMD          TO  ETENSU1-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  ETENSU1-YUKOEDYMD
           MOVE    ETENSU1-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_1"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU1-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU1
               INITIALIZE                  ETENSU1-REC
           END-IF
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-ETNSU1-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コードチェック処理
      *****************************************************************
       2001-HOUKATU-CHEK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU2-ARI
      *    包括・被包括（ユーザーオフ）マスタ
           PERFORM 20013-ETENSU2-OFF-CHK-SEC
      *    ユーザオフがあれば、包括なし
           IF      FLG-ETENSU2-OFF     =   ZERO
               MOVE    ZERO                TO  FLG-ETENSU2-ARI
           ELSE
      *        包括・被包括マスタチェック処理
               PERFORM 20011-ETENSU2-CHK-SEC
               IF      FLG-ETENSU2-ARI     =   ZERO
      *            包括・被包括（ＯＲＣＡ）マスタ
                   PERFORM 20012-ETENSU2-JMA-CHK-SEC
               END-IF
               IF     (FLG-ETENSU2-ARI     =   1   )
      *            検体コメントデータ編集
                   PERFORM 200121-ETENSU2-SAMPLE-CHK-SEC
               END-IF
           END-IF
           IF      FLG-ETENSU2-ARI     =   1
               MOVE    1               TO  FLG-DATA-ARI
           END-IF
           .
       2001-HOUKATU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括・被包括マスタチェック処理
      *****************************************************************
       20011-ETENSU2-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-REC
           INITIALIZE                  ETENSU2-REC
           MOVE    WRK-GROUP           TO  ETENSU2-H-GROUP
           MOVE    ORCSCETNSCHK-SRYCD2 TO  ETENSU2-SRYCD
           MOVE    WRK-SRYYMD          TO  ETENSU2-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  ETENSU2-YUKOEDYMD
           MOVE    ETENSU2-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2
               INITIALIZE                  ETENSU2-REC
           END-IF
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ETENSU2         =   ZERO
               MOVE    1                   TO  FLG-ETENSU2-ARI
           END-IF
           .
       20011-ETENSU2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括・被包括マスタチェック処理
      *****************************************************************
       20012-ETENSU2-JMA-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-JMA-REC
           INITIALIZE                  ETENSU2-JMA-REC
           MOVE    WRK-GROUP           TO  ETNSJMA-H-GROUP
           MOVE    ORCSCETNSCHK-SRYCD2 TO  ETNSJMA-SRYCD
           MOVE    WRK-SRYYMD          TO  ETNSJMA-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  ETNSJMA-YUKOEDYMD
           MOVE    ETENSU2-JMA-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-JMA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2-JMA
               INITIALIZE                  ETENSU2-JMA-REC
           END-IF
           MOVE    "tbl_etensu_2_jma"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ETENSU2-JMA     =   ZERO
               MOVE    1                   TO  FLG-ETENSU2-ARI
           END-IF
           .
       20012-ETENSU2-JMA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検体コメントデータチェック処理
      *****************************************************************
       200121-ETENSU2-SAMPLE-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-SMP-REC
           INITIALIZE                  ETENSU2-SMP-REC
           MOVE    WRK-GROUP           TO  ETNSSMP-H-GROUP
           MOVE    ORCSCETNSCHK-SRYCD2 TO  ETNSSMP-SRYCD
           MOVE    WRK-SRYYMD          TO  ETNSSMP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  ETNSSMP-YUKOEDYMD
           MOVE    ETENSU2-SMP-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2_sample"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2_sample"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-SMP-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2-SMP
               INITIALIZE                  ETENSU2-SMP-REC
           END-IF
           MOVE    ZERO                TO  IDZ
           PERFORM UNTIL      (FLG-ETENSU2-SMP =   1   )
                         OR   (IDZ             >=  10  )
               ADD     1                   TO  IDZ
               MOVE    ETNSSMP-SAMPLECD    TO  ORCSCETNSCHK-KNT-SRYCD
                                                                 (IDZ)
      *
               MOVE    "tbl_etensu_2_sample"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-SMP-READ-SEC
           END-PERFORM
           MOVE    "tbl_etensu_2_sample"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200121-ETENSU2-SAMPLE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括・被包括（ユーザオフ）チェック処理
      *****************************************************************
       20013-ETENSU2-OFF-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  ETENSU2-OFF-REC
           INITIALIZE                  ETENSU2-OFF-REC
           MOVE    SPA-HOSPNUM         TO  ETNSOFF-HOSPNUM
           MOVE    WRK-GROUP           TO  ETNSOFF-H-GROUP
           MOVE    ORCSCETNSCHK-SRYCD2 TO  ETNSOFF-SRYCD
           MOVE    WRK-SRYYMD          TO  ETNSOFF-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  ETNSOFF-YUKOEDYMD
           MOVE    ETENSU2-OFF-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-ETENSU2-OFF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU2-OFF
               INITIALIZE                  ETENSU2-OFF-REC
           END-IF
           MOVE    "tbl_etensu_2_off"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20013-ETENSU2-OFF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補助マスター読み込み
      *****************************************************************
       910-ETENSU1-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU1-REC
               MOVE    ZERO            TO  FLG-ETENSU1
           ELSE
               INITIALIZE                  ETENSU1-REC
               MOVE    1               TO  FLG-ETENSU1
           END-IF
      *
           .
       910-ETENSU1-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター読み込み
      *****************************************************************
       910-ETENSU2-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-REC
               MOVE    ZERO            TO  FLG-ETENSU2
           ELSE
               INITIALIZE                  ETENSU2-REC
               MOVE    1               TO  FLG-ETENSU2
           END-IF
      *
           .
       910-ETENSU2-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター（ＪＭＡ）読み込み
      *****************************************************************
       910-ETENSU2-JMA-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-JMA-REC
               MOVE    ZERO            TO  FLG-ETENSU2-JMA
           ELSE
               INITIALIZE                  ETENSU2-JMA-REC
               MOVE    1               TO  FLG-ETENSU2-JMA
           END-IF
      *
           .
       910-ETENSU2-JMA-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括・被包括マスター（ユーザオフ）読み込み
      *****************************************************************
       910-ETENSU2-OFF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-OFF-REC
               MOVE    ZERO            TO  FLG-ETENSU2-OFF
           ELSE
               INITIALIZE                  ETENSU2-OFF-REC
               MOVE    1               TO  FLG-ETENSU2-OFF
           END-IF
      *
           .
       910-ETENSU2-OFF-READ-EXT.
           EXIT.
      *****************************************************************
      *    検体コメントデータ読み込み
      *****************************************************************
       910-ETENSU2-SMP-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ETENSU2-SMP-REC
               MOVE    ZERO            TO  FLG-ETENSU2-SMP
           ELSE
               INITIALIZE                  ETENSU2-SMP-REC
               MOVE    1               TO  FLG-ETENSU2-SMP
           END-IF
      *
           .
       910-ETENSU2-SMP-READ-EXT.
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
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
