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
       PROGRAM-ID.                 ORCSYAKINTOKU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 夜勤時間特別入院基本料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/03/25    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平成３０年４月改定対応
      *  00.00.00    NACL-小豆沢  18/03/01  急性期一般入院料の新設
      *                                     地域一般入院料の新設
      *                                     療養病棟入院基本料２は平成
      *                                     ３０年３月まで
      *  00.00.00    NACL-小豆沢  18/03/02  結核病棟（重症患者割合割合
      *                                     特別入院料基本料の新設）
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
      *    夜勤時間特別入院基本料コード取得パラメタ
           COPY    "CPORCSYAKINTOKU.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSYAKINTOKU-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-YAKINTOKU-SANTEIYM       >=   "201604"
      *        システム管理検索（夜勤時間特別入院基本料の区分取得）
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                      SYS-5001-REC
               MOVE    "5001"              TO  SYS-5001-KANRICD
               MOVE    LNK-YAKINTOKU-BTU-NUM
                                           TO  SYS-5001-KBNCD
               MOVE    LNK-YAKINTOKU-SANTEIYM
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
      *        コード置き換え（夜勤時間特別入院基本料）
               IF    SYS-5001-YKNTOKUKBN   =   "1"
                   IF    LNK-YAKINTOKU-SRYCD    NOT  =  SPACE
                       IF  LNK-YAKINTOKU-SANTEIYM       >=   "201804"
                           PERFORM    201-CDCHG-SEC
                       ELSE
                           PERFORM    200-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        コード置き換え（結核病棟の重症患者割合割合特別入院料基本料）
               IF    SYS-5001-JUUSHOUKNGKBN    =   "1"
                   IF    LNK-YAKINTOKU-SRYCD    NOT  =  SPACE
                       IF  LNK-YAKINTOKU-SANTEIYM       >=   "201804"
                           PERFORM    210-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    LNK-YAKINTOKU-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-YAKINTOKU-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    コード置き換え（夜勤時間特別入院基本料）
      *    平成２８年４月から平成３０年３月まで
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        一般病棟７対１入院基本料
               WHEN    "190117710"
                  MOVE "190183310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        一般病棟１０対１入院基本料
               WHEN    "190077410"
                  MOVE "190183410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        一般病棟１３対１入院基本料
               WHEN    "190077510"
                  MOVE "190183510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        一般病棟１５対１入院基本料
               WHEN    "190077610"
                  MOVE "190183610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）
               WHEN    "190132410"
                  MOVE "190185510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ａ）（生活療養）
               WHEN    "190132510"
                  MOVE "190185610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）
               WHEN    "190132610"
                  MOVE "190185710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
               WHEN    "190132710"
                  MOVE "190185810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）
               WHEN    "190132810"
                  MOVE "190185910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
               WHEN    "190132910"
                  MOVE "190186010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）
               WHEN    "190133010"
                  MOVE "190186110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
               WHEN    "190133110"
                  MOVE "190186210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）
               WHEN    "190133210"
                  MOVE "190186310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
               WHEN    "190133310"
                  MOVE "190186410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）
               WHEN    "190133410"
                  MOVE "190186510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
               WHEN    "190133510"
                  MOVE "190186610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）
               WHEN    "190133610"
                  MOVE "190186710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
               WHEN    "190133710"
                  MOVE "190186810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）
               WHEN    "190133810"
                  MOVE "190186910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
               WHEN    "190133910"
                  MOVE "190187010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）
               WHEN    "190134010"
                  MOVE "190187110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
               WHEN    "190134110"
                  MOVE "190187210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟７対１入院基本料
               WHEN    "190117910"
                  MOVE "190187710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１０対１入院基本料
               WHEN    "190081710"
                  MOVE "190187810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１３対１入院基本料
               WHEN    "190081810"
                  MOVE "190187910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１５対１入院基本料
               WHEN    "190081910"
                  MOVE "190188110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１８対１入院基本料
               WHEN    "190082010"
                  MOVE "190188210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟２０対１入院基本料
               WHEN    "190082110"
                  MOVE "190188010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１０対１入院基本料
               WHEN    "190083610"
                  MOVE "190188310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１３対１入院基本料
               WHEN    "190134510"
                  MOVE "190188410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１５対１入院基本料
               WHEN    "190083810"
                  MOVE "190188510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１８対１入院基本料
               WHEN    "190083910"
                  MOVE "190188610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟２０対１入院基本料
               WHEN    "190084010"
                  MOVE "190188710" TO  LNK-YAKINTOKU-CHGSRYCD
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
      *    コード置き換え（夜勤時間特別入院基本料）
      *    平成３０年４月から
      *****************************************************************
       201-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        急性期一般入院料１
               WHEN    "190117710"
                  MOVE "190183310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料２
               WHEN    "190199710"
                  MOVE "190201010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料３
               WHEN    "190199810"
                  MOVE "190201110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料４
               WHEN    "190199910"
                  MOVE "190201210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料５
               WHEN    "190200010"
                  MOVE "190201310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料６
               WHEN    "190200110"
                  MOVE "190201410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        急性期一般入院料７
               WHEN    "190077410"
                  MOVE "190183410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        地域一般入院料１
               WHEN    "190200210"
                  MOVE "190201510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        地域一般入院料２
               WHEN    "190077510"
                  MOVE "190183510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        地域一般入院料３
               WHEN    "190077610"
                  MOVE "190183610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟７対１入院基本料
               WHEN    "190117910"
                  MOVE "190187710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１０対１入院基本料
               WHEN    "190081710"
                  MOVE "190187810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１３対１入院基本料
               WHEN    "190081810"
                  MOVE "190187910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１５対１入院基本料
               WHEN    "190081910"
                  MOVE "190188110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟１８対１入院基本料
               WHEN    "190082010"
                  MOVE "190188210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        結核病棟２０対１入院基本料
               WHEN    "190082110"
                  MOVE "190188010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１０対１入院基本料
               WHEN    "190083610"
                  MOVE "190188310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１３対１入院基本料
               WHEN    "190134510"
                  MOVE "190188410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１５対１入院基本料
               WHEN    "190083810"
                  MOVE "190188510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟１８対１入院基本料
               WHEN    "190083910"
                  MOVE "190188610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        精神病棟２０対１入院基本料
               WHEN    "190084010"
                  MOVE "190188710" TO  LNK-YAKINTOKU-CHGSRYCD
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
      *    コード置き換え（結核病棟の重症患者割合割合特別入院料基本料）
      *    平成３０年４月から
      *****************************************************************
       210-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        結核病棟７対１入院基本料
               WHEN    "190117910"
                  MOVE "190204610" TO  LNK-YAKINTOKU-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       210-CDCHG-EXT.
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
