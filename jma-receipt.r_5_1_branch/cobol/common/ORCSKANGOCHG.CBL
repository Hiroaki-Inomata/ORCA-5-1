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
       PROGRAM-ID.                 ORCSKANGOCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 看護職員数等入院基本料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/03/02    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平成３０年４月改定対応
      *  05.00.00    NACL-小豆沢  18/03/02  療養病棟入院基本料２の経過
      *                                     措置追加（看護比率３０対１）
      *  05.00.00    NACL-小豆沢  18/03/09  療養病棟入院料２（Ｉ）
      *                                     措置追加２・通則８対応
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5001.INC".
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
      *    看護職員数等入院基本料コード取得パラメタ
           COPY    "CPORCSKANGOCHG.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKANGOCHG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-KANGOCHG-SANTEIYM       >=   "201604"
      *        システム管理検索（看護職員数等経過措置の区分取得）
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                      SYS-5001-REC
               MOVE    "5001"              TO  SYS-5001-KANRICD
               MOVE    LNK-KANGOCHG-BTU-NUM
                                           TO  SYS-5001-KBNCD
               MOVE    LNK-KANGOCHG-SANTEIYM
                                           TO  SYS-5001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
               MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
               MOVE    SYS-5001-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5001-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5001-REC
                   INITIALIZE                  SYS-5001-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        栄養管理体制減算規定該当（通則８）の対象確認
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
      *           システム管理検索
                   MOVE    SPACE           TO  SYS-5000-REC
                   INITIALIZE                  SYS-5000-REC
                   MOVE    "5000"          TO  SYS-5000-KANRICD
                   MOVE    "*"             TO  SYS-5000-KBNCD
                   MOVE    LNK-KANGOCHG-SANTEIYM
                                           TO  SYS-5000-STYUKYMD(1:6)
                   MOVE    "01"            TO  SYS-5000-STYUKYMD(7:2)
                   MOVE    SYS-5000-STYUKYMD
                                           TO  SYS-5000-EDYUKYMD
                   MOVE    SPA-HOSPNUM     TO  SYS-5000-HOSPNUM
                   MOVE    SYS-5000-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key10"             TO  MCP-PATHNAME
                       PERFORM 920-DBFETCH-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    ZERO                TO  FLG-SYSKANRI
                           MOVE    MCPDATA-REC         TO  SYS-5000-REC
                       ELSE
                           MOVE    SPACE           TO  SYS-5000-REC
                           INITIALIZE                  SYS-5000-REC
                           MOVE    1                   TO  FLG-SYSKANRI
                       END-IF
                   ELSE
                       MOVE    SPACE           TO  SYS-5000-REC
                       INITIALIZE                  SYS-5000-REC
                       MOVE    1               TO  FLG-SYSKANRI
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
      *
      *        平成２８年４月改定（看護職員数等経過措置入院基本料）
               IF  LNK-KANGOCHG-SANTEIYM       <=   "201803"
                   IF    SYS-5001-SYOKUINKBN  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    200-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        平成３０年４月改定（看護職員数等経過措置入院基本料　経過措置１）
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
                   IF    SYS-5001-SYOKUINKBN  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    201-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        平成３０年４月改定（看護比率３０対１等経過措置　経過措置２）
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
                   IF    SYS-5001-SYOKUINKBN2  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    202-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    LNK-KANGOCHG-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-KANGOCHG-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    コード置き換え（看護職員数等経過措置入院基本料）
      *    平成２８年４月から平成３０年３月まで
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）
               WHEN    "190132410"
                  MOVE "190183710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）（生活療養）
               WHEN    "190132510"
                  MOVE "190183810" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）
               WHEN    "190132610"
                  MOVE "190183910" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
               WHEN    "190132710"
                  MOVE "190184010" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）
               WHEN    "190132810"
                  MOVE "190184110" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
               WHEN    "190132910"
                  MOVE "190184210" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）
               WHEN    "190133010"
                  MOVE "190184310" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
               WHEN    "190133110"
                  MOVE "190184410" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）
               WHEN    "190133210"
                  MOVE "190184510" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
               WHEN    "190133310"
                  MOVE "190184610" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）
               WHEN    "190133410"
                  MOVE "190184710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
               WHEN    "190133510"
                  MOVE "190184810" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）
               WHEN    "190133610"
                  MOVE "190184910" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
               WHEN    "190133710"
                  MOVE "190185010" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）
               WHEN    "190133810"
                  MOVE "190185110" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
               WHEN    "190133910"
                  MOVE "190185210" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）
               WHEN    "190134010"
                  MOVE "190185310" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
               WHEN    "190134110"
                  MOVE "190185410" TO  LNK-KANGOCHG-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       200-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    コード置き換え（看護職員数等経過措置入院基本料　経過措置１）
      *    平成３０年４月から
      *****************************************************************
       201-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        療養病棟入院料２（Ａ）（経過措置１）
               WHEN    "190132410"
                  MOVE "190215110" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ａ）（生活療養）（経過措置１）
               WHEN    "190132510"
                  MOVE "190215210" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｂ）（経過措置１）
               WHEN    "190132610"
                  MOVE "190215310" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｂ）（生活療養）（経過措置１）
               WHEN    "190132710"
                  MOVE "190215410" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｃ）（経過措置１）
               WHEN    "190132810"
                  MOVE "190215510" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｃ）（生活療養）（経過措置１）
               WHEN    "190132910"
                  MOVE "190215610" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｄ）（経過措置１）
               WHEN    "190133010"
                  MOVE "190215710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｄ）（生活療養）（経過措置１）
               WHEN    "190133110"
                  MOVE "190215810" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｅ）（経過措置１）
               WHEN    "190133210"
                  MOVE "190215910" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｅ）（生活療養）（経過措置１）
               WHEN    "190133310"
                  MOVE "190216010" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｆ）（経過措置１）
               WHEN    "190133410"
                  MOVE "190216110" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｆ）（生活療養）（経過措置１）
               WHEN    "190133510"
                  MOVE "190216210" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｇ）（経過措置１）
               WHEN    "190133610"
                  MOVE "190216310" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｇ）（生活療養）（経過措置１）
               WHEN    "190133710"
                  MOVE "190216410" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｈ）（経過措置１）
               WHEN    "190133810"
                  MOVE "190216510" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｈ）（生活療養）（経過措置１）
               WHEN    "190133910"
                  MOVE "190216610" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｉ）（経過措置１）
               WHEN    "190134010"
                  MOVE "190216710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｉ）（生活療養）（経過措置１）
               WHEN    "190134110"
                  MOVE "190216810" TO  LNK-KANGOCHG-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       201-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    コード置き換え（看護比率３０対１等経過措置入院基本料　経過措置２）
      *    平成３０年４月から
      *****************************************************************
       202-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        療養病棟入院料２（Ａ）（経過措置２）
               WHEN    "190132410"
                  MOVE "190202510" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ａ）（生活療養）（経過措置２）
               WHEN    "190132510"
                  MOVE "190202610" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｂ）（経過措置２）
               WHEN    "190132610"
                  MOVE "190202710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｂ）（生活療養）（経過措置２）
               WHEN    "190132710"
                  MOVE "190202810" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｃ）（経過措置２）
               WHEN    "190132810"
                  MOVE "190202910" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｃ）（生活療養）（経過措置２）
               WHEN    "190132910"
                  MOVE "190203010" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｄ）（経過措置２）
               WHEN    "190133010"
                  MOVE "190203110" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｄ）（生活療養）（経過措置２）
               WHEN    "190133110"
                  MOVE "190203210" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｅ）（経過措置２）
               WHEN    "190133210"
                  MOVE "190203310" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｅ）（生活療養）（経過措置２）
               WHEN    "190133310"
                  MOVE "190203410" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｆ）（経過措置２）
               WHEN    "190133410"
                  MOVE "190203510" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｆ）（生活療養）（経過措置２）
               WHEN    "190133510"
                  MOVE "190203610" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｇ）（経過措置２）
               WHEN    "190133610"
                  MOVE "190203710" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｇ）（生活療養）（経過措置２）
               WHEN    "190133710"
                  MOVE "190203810" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｈ）（経過措置２）
               WHEN    "190133810"
                  MOVE "190203910" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｈ）（生活療養）（経過措置２）
               WHEN    "190133910"
                  MOVE "190204010" TO  LNK-KANGOCHG-CHGSRYCD
      *        療養病棟入院料２（Ｉ）（経過措置２）
               WHEN    "190134010"
                  MOVE "190204110" TO  LNK-KANGOCHG-CHGSRYCD
      *           通則８の時、更に置き換え
                  IF    SYS-5000-EIYOUKSNKBN   =    "1"
                       MOVE "190204310" TO  LNK-KANGOCHG-CHGSRYCD
                  END-IF
      *        療養病棟入院料２（Ｉ）（生活療養）（経過措置２）
               WHEN    "190134110"
                  MOVE "190204210" TO  LNK-KANGOCHG-CHGSRYCD
      *           通則８の時、更に置き換え
                  IF    SYS-5000-EIYOUKSNKBN   =    "1"
                       MOVE "190204410" TO  LNK-KANGOCHG-CHGSRYCD
                  END-IF
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       202-CDCHG-EXT.
           .
      *
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
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
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
