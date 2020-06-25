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
       PROGRAM-ID.                 ORCSTEISU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 定数超過情報取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/09/12    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平成３０年４月改定対応
      *  05.00.00    NACL-小豆沢  18/03/08  療養病棟入院料の判定変更
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
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    定数超過入院基本料減額（１００分の１０）
           03  CONS-TEISU1                 PIC X(09) VALUE "190107690".
      *    定数超過入院基本料減額（１００分の２０）
           03  CONS-TEISU2                 PIC X(09) VALUE "190107790".
      *    定数超過入院基本料減額（１００分の１０）（特定入院料）
           03  CONS-TEISU3                 PIC X(09) VALUE "190122990".
      *    定数超過入院基本料減額（１００分の２０）（特定入院料）
           03  CONS-TEISU4                 PIC X(09) VALUE "190123090".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SANTEIYM.
             05  WRK-SANTEI-YY             PIC 9(04).
             05  WRK-SANTEI-MM             PIC 9(02).
      *
           03  WRK-TEISUKBN                PIC X(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    標欠取得用パラメタ
           COPY    "CPORCSTEISU.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
            ORCSTEISU-AREA
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
           INITIALIZE                  LNK-TEISU-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-TEISU-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-TEISU-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF    LNK-TEISU-RC      =    ZERO
               IF  LNK-TEISU-SANTEIYM    >=   "201804"
                   PERFORM    210-TEISU5-SEC
               ELSE
                   IF  LNK-TEISU-SANTEIYM    >=   "201604"
                       PERFORM    210-TEISU4-SEC
                   ELSE
                       IF  LNK-TEISU-SANTEIYM    >=   "201404"
                           PERFORM    210-TEISU3-SEC
                       ELSE
                         IF  LNK-TEISU-SANTEIYM    >=   "201204"
                             PERFORM    210-TEISU2-SEC
                         ELSE
                             PERFORM    200-TEISU1-SEC
                         END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
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
      *    算定年月チェック
           IF    ( LNK-TEISU-SANTEIYM   =   SPACE )
               MOVE    1               TO  LNK-TEISU-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    定数超過の診療コード判定取得（平成２４年３月まで）
      *****************************************************************
       200-TEISU1-SEC            SECTION.
      *
      *定数超過情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-TEISUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-TEISU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR  "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-TEISUKBN  TO  WRK-TEISUKBN
                       EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                           WHEN  "0"
                               EVALUATE  LNK-TEISU-SANTEISRYCD
      *                            療養病棟入院基本料１（１８年６月まで）
                                   WHEN  "190080610"
                                   WHEN  "190080710"
                                   WHEN  "190113910"
                                   WHEN  "190113810"
                                   WHEN  "190081310"
                                   WHEN  "190081410"
                                   WHEN  "190114510"
                                   WHEN  "190114610"
                                   WHEN  "190799810"
                                   WHEN  "190799910"
                                   WHEN  "190800510"
                                   WHEN  "190800610"
                                   WHEN  "190811410"
                                   WHEN  "190811510"
      *                            療養病棟入院基本料１（特別入院基本料）（１８年６月まで）
                                   WHEN  "190812110"
                                   WHEN  "190812210"
      *                            療養病棟入院基本料１（ＡからＩ）
                                   WHEN  "190121310"
                                   WHEN  "190121410"
                                   WHEN  "190121510"
                                   WHEN  "190121610"
                                   WHEN  "190121710"
                                   WHEN  "190131610"
                                   WHEN  "190131810"
                                   WHEN  "190132010"
                                   WHEN  "190132210"
      *                            療養病棟入院基本料１（ＡからＩ）（生活療養）
                                   WHEN  "190123710"
                                   WHEN  "190123810"
                                   WHEN  "190123910"
                                   WHEN  "190124010"
                                   WHEN  "190124110"
                                   WHEN  "190131710"
                                   WHEN  "190131910"
                                   WHEN  "190132110"
                                   WHEN  "190132310"
      *                            療養病棟入院基本料２（ＡからＩ）
                                   WHEN  "190132410"
                                   WHEN  "190132610"
                                   WHEN  "190132810"
                                   WHEN  "190133010"
                                   WHEN  "190133210"
                                   WHEN  "190133410"
                                   WHEN  "190133610"
                                   WHEN  "190133810"
                                   WHEN  "190134010"
      *                            療養病棟入院基本料２（ＡからＩ）（生活療養）
                                   WHEN  "190132510"
                                   WHEN  "190132710"
                                   WHEN  "190132910"
                                   WHEN  "190133110"
                                   WHEN  "190133310"
                                   WHEN  "190133510"
                                   WHEN  "190133710"
                                   WHEN  "190133910"
                                   WHEN  "190134110"
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN  "190121810"
                                   WHEN  "190124210"
      *                            有床診療所療養病床入院基本料１（１８年６月まで）
                                   WHEN  "190098210"
                                   WHEN  "190116110"
                                   WHEN  "190806910"
                                   WHEN  "190812910"
                                   WHEN  "190098310"
                                   WHEN  "190116210"
      *                            有床診療所療養病床入院基本料１（特別入院基本料）（１８年６月まで）
                                   WHEN  "190807010"
                                   WHEN  "190813010"
      *                            有床診療所療養病床入院基本料（ＡからＩ）
                                   WHEN  "190122010"
                                   WHEN  "190122110"
                                   WHEN  "190122210"
                                   WHEN  "190122310"
                                   WHEN  "190122410"
      *                            有床診療所療養病床入院基本料（ＡからＩ）（生活療養）
                                   WHEN  "190124310"
                                   WHEN  "190124410"
                                   WHEN  "190124510"
                                   WHEN  "190124610"
                                   WHEN  "190124710"
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN  "190122510"
                                   WHEN  "190124810"
      *                            老人特定入院基本料
                                   WHEN  "190799410"
                                   WHEN  "190799510"
                                   WHEN  "190811210"
                                   WHEN  "190811310"
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                           WHEN  "1"
                               MOVE  CONS-TEISU4   TO  
                                                LNK-TEISU-SRYCD(1)
                       END-EVALUATE
                       MOVE    LNK-TEISU-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *定数超過情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-TEISU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-TEISU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF   WRK-TEISUKBN   =  SYS-5000-TEISUKBN
                           CONTINUE
                       ELSE
                           EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                               WHEN  "0"
                                   EVALUATE  LNK-TEISU-SANTEISRYCD
      *                            療養病棟入院基本料１（１８年６月まで）
                                   WHEN  "190080610"
                                   WHEN  "190080710"
                                   WHEN  "190113910"
                                   WHEN  "190113810"
                                   WHEN  "190081310"
                                   WHEN  "190081410"
                                   WHEN  "190114510"
                                   WHEN  "190114610"
                                   WHEN  "190799810"
                                   WHEN  "190799910"
                                   WHEN  "190800510"
                                   WHEN  "190800610"
                                   WHEN  "190811410"
                                   WHEN  "190811510"
      *                            療養病棟入院基本料１（特別入院基本料）（１８年６月まで）
                                   WHEN  "190812110"
                                   WHEN  "190812210"
      *                            療養病棟入院基本料１（ＡからＩ）
                                   WHEN  "190121310"
                                   WHEN  "190121410"
                                   WHEN  "190121510"
                                   WHEN  "190121610"
                                   WHEN  "190121710"
                                   WHEN  "190131610"
                                   WHEN  "190131810"
                                   WHEN  "190132010"
                                   WHEN  "190132210"
      *                            療養病棟入院基本料１（ＡからＩ）（生活療養）
                                   WHEN  "190123710"
                                   WHEN  "190123810"
                                   WHEN  "190123910"
                                   WHEN  "190124010"
                                   WHEN  "190124110"
                                   WHEN  "190131710"
                                   WHEN  "190131910"
                                   WHEN  "190132110"
                                   WHEN  "190132310"
      *                            療養病棟入院基本料２（ＡからＩ）
                                   WHEN  "190132410"
                                   WHEN  "190132610"
                                   WHEN  "190132810"
                                   WHEN  "190133010"
                                   WHEN  "190133210"
                                   WHEN  "190133410"
                                   WHEN  "190133610"
                                   WHEN  "190133810"
                                   WHEN  "190134010"
      *                            療養病棟入院基本料２（ＡからＩ）（生活療養）
                                   WHEN  "190132510"
                                   WHEN  "190132710"
                                   WHEN  "190132910"
                                   WHEN  "190133110"
                                   WHEN  "190133310"
                                   WHEN  "190133510"
                                   WHEN  "190133710"
                                   WHEN  "190133910"
                                   WHEN  "190134110"
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN  "190121810"
                                   WHEN  "190124210"
      *                            有床診療所療養病床入院基本料１（１８年６月まで）
                                   WHEN  "190098210"
                                   WHEN  "190116110"
                                   WHEN  "190806910"
                                   WHEN  "190812910"
                                   WHEN  "190098310"
                                   WHEN  "190116210"
      *                            有床診療所療養病床入院基本料１（特別入院基本料）（１８年６月まで）
                                   WHEN  "190807010"
                                   WHEN  "190813010"
      *                            有床診療所療養病床入院基本料（ＡからＩ）
                                   WHEN  "190122010"
                                   WHEN  "190122110"
                                   WHEN  "190122210"
                                   WHEN  "190122310"
                                   WHEN  "190122410"
      *                            有床診療所療養病床入院基本料（ＡからＩ）（生活療養）
                                   WHEN  "190124310"
                                   WHEN  "190124410"
                                   WHEN  "190124510"
                                   WHEN  "190124610"
                                   WHEN  "190124710"
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN  "190122510"
                                   WHEN  "190124810"
      *                            老人特定入院基本料
                                   WHEN  "190799410"
                                   WHEN  "190799510"
                                   WHEN  "190811210"
                                   WHEN  "190811310"
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                                   END-EVALUATE
                               WHEN  "1"
                                   MOVE  CONS-TEISU4   TO  
                                                    LNK-TEISU-SRYCD(2)
                           END-EVALUATE
                           MOVE    WRK-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(2)
                           MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-TEISU-SRYCD(1)    =   SPACE)   AND
                 (LNK-TEISU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-TEISU-RC
           END-IF
      *
           .
       200-TEISU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    定数超過の診療コード判定取得（平成２４年４月から平成２６年３月まで）
      *****************************************************************
       210-TEISU2-SEC            SECTION.
      *
      *定数超過情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-TEISUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-TEISU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR  "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-TEISUKBN  TO  WRK-TEISUKBN
                       EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                           WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分からの判定に変更
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            特定入院基本料
                                   WHEN   99
      *                            特定入院基本料（特別入院基本料等算定患者）
                                   WHEN   100
      *                            一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                           WHEN  "1"
                               MOVE  CONS-TEISU4   TO  
                                                LNK-TEISU-SRYCD(1)
                       END-EVALUATE
                       MOVE    LNK-TEISU-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *定数超過情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-TEISU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-TEISU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF   WRK-TEISUKBN   =  SYS-5000-TEISUKBN
                           CONTINUE
                       ELSE
                           EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                               WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分からの判定に変更
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            特定入院基本料
                                   WHEN   99
      *                            特定入院基本料（特別入院基本料等算定患者）
                                   WHEN   100
      *                            一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                                   END-EVALUATE
                               WHEN  "1"
                                   MOVE  CONS-TEISU4   TO  
                                                    LNK-TEISU-SRYCD(2)
                           END-EVALUATE
                           MOVE    WRK-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(2)
                           MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-TEISU-SRYCD(1)    =   SPACE)   AND
                 (LNK-TEISU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-TEISU-RC
           END-IF
      *
           .
       210-TEISU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    定数超過の診療コード判定取得（平成２６年４月以降）
      *****************************************************************
       210-TEISU3-SEC            SECTION.
      *
      *定数超過情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-TEISUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-TEISU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR  "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-TEISUKBN  TO  WRK-TEISUKBN
                       EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                           WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分からの判定に変更
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間超過）
                                   WHEN   199
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間超過）
                                   WHEN   004
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間超過）
                                   WHEN   005
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間超過）
                                   WHEN   006
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間超過）
                                   WHEN   007
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間超過）
                                   WHEN   008
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間超過）
                                   WHEN   010
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間超過）
                                   WHEN   011
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間超過）
                                   WHEN   012
      *                            特定入院基本料
                                   WHEN   99
      *                            特定入院基本料（特別入院基本料等算定患者）
                                   WHEN   100
      *                            一般病棟・療養病棟入院基本料１
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                           WHEN  "1"
      *                        特定入院料の定数超過は平成２６年４月より廃止
                               CONTINUE
                       END-EVALUATE
                       MOVE    LNK-TEISU-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *定数超過情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-TEISU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-TEISU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF   WRK-TEISUKBN   =  SYS-5000-TEISUKBN
                           CONTINUE
                       ELSE
                           EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                               WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分から判定
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間超過）
                                   WHEN   199
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間超過）
                                   WHEN   004
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間超過）
                                   WHEN   005
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間超過）
                                   WHEN   006
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間超過）
                                   WHEN   007
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間超過）
                                   WHEN   008
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間超過）
                                   WHEN   010
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間超過）
                                   WHEN   011
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間超過）
                                   WHEN   012
      *                            特定入院基本料
                                   WHEN   99
      *                            特定入院基本料（特別入院基本料等算定患者）
                                   WHEN   100
      *                            一般病棟・療養病棟入院基本料１
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                                   END-EVALUATE
                               WHEN  "1"
      *                            特定入院料の定数超過は平成２６年４月より廃止
                                   CONTINUE
                           END-EVALUATE
                           MOVE    WRK-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(2)
                           MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-TEISU-SRYCD(1)    =   SPACE)   AND
                 (LNK-TEISU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-TEISU-RC
           END-IF
      *
           .
       210-TEISU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    定数超過の診療コード判定取得（平成２８年４月以降）
      *****************************************************************
       210-TEISU4-SEC            SECTION.
      *
      *定数超過情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-TEISUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-TEISU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR  "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-TEISUKBN  TO  WRK-TEISUKBN
                       EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                           WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分からの判定に変更
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間超過）
                                   WHEN   199
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間超過）
                                   WHEN   004
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間超過）
                                   WHEN   005
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間超過）
                                   WHEN   006
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間超過）
                                   WHEN   007
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間超過）
                                   WHEN   008
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間超過）
                                   WHEN   010
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間超過）
                                   WHEN   011
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間超過）
                                   WHEN   012
      *                            療養病棟入院基本料２（入院基本料Ａ）（看護職員数等経過措置）
                                   WHEN   048
      *                            療養病棟入院基本料２（入院基本料Ｂ）（看護職員数等経過措置）
                                   WHEN   049
      *                            療養病棟入院基本料２（入院基本料Ｃ）（看護職員数等経過措置）
                                   WHEN   051
      *                            療養病棟入院基本料２（入院基本料Ｄ）（看護職員数等経過措置）
                                   WHEN   052
      *                            療養病棟入院基本料２（入院基本料Ｅ）（看護職員数等経過措置）
                                   WHEN   053
      *                            療養病棟入院基本料２（入院基本料Ｆ）（看護職員数等経過措置）
                                   WHEN   054
      *                            療養病棟入院基本料２（入院基本料Ｇ）（看護職員数等経過措置）
                                   WHEN   057
      *                            療養病棟入院基本料２（入院基本料Ｈ）（看護職員数等経過措置）
                                   WHEN   058
      *                            療養病棟入院基本料２（入院基本料Ｉ）（看護職員数等経過措置）
                                   WHEN   059
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間特別入院基本料）
                                   WHEN   060
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間特別入院基本料）
                                   WHEN   061
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間特別入院基本料）
                                   WHEN   062
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間特別入院基本料）
                                   WHEN   063
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間特別入院基本料）
                                   WHEN   064
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間特別入院基本料）
                                   WHEN   065
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間特別入院基本料）
                                   WHEN   066
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間特別入院基本料）
                                   WHEN   067
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間特別入院基本料）
                                   WHEN   068
      *                            特定入院基本料（障害者施設等入院基本料）
                                   WHEN   039
      *                            特定入院基本料（障害者施設等入院基本料）（夜勤時間超過減算）
                                   WHEN   041
      *                            一般病棟・療養病棟入院基本料１
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                           WHEN  "1"
      *                        特定入院料の定数超過は平成２６年４月より廃止
                               CONTINUE
                       END-EVALUATE
                       MOVE    LNK-TEISU-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *定数超過情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-TEISU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-TEISU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF   WRK-TEISUKBN   =  SYS-5000-TEISUKBN
                           CONTINUE
                       ELSE
                           EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                               WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        入院基本料加算区分から判定
                               EVALUATE   TNS-NYUKHNKSNKBN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   152
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   153
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   154
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   155
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   156
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   157
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   158
      *                            療養病棟入院基本料１（入院基本料Ａ）
                                   WHEN   173
      *                            療養病棟入院基本料１（入院基本料Ｂ）
                                   WHEN   174
      *                            療養病棟入院基本料１（入院基本料Ｃ）
                                   WHEN   175
      *                            療養病棟入院基本料１（入院基本料Ｄ）
                                   WHEN   176
      *                            療養病棟入院基本料１（入院基本料Ｅ）
                                   WHEN   177
      *                            療養病棟入院基本料１（入院基本料Ｆ）
                                   WHEN   178
      *                            療養病棟入院基本料１（入院基本料Ｇ）
                                   WHEN   179
      *                            療養病棟入院基本料１（入院基本料Ｈ）
                                   WHEN   180
      *                            療養病棟入院基本料１（入院基本料Ｉ）
                                   WHEN   181
      *                            療養病棟入院基本料２（入院基本料Ａ）
                                   WHEN   182
      *                            療養病棟入院基本料２（入院基本料Ｂ）
                                   WHEN   183
      *                            療養病棟入院基本料２（入院基本料Ｃ）
                                   WHEN   184
      *                            療養病棟入院基本料２（入院基本料Ｄ）
                                   WHEN   185
      *                            療養病棟入院基本料２（入院基本料Ｅ）
                                   WHEN   186
      *                            療養病棟入院基本料２（入院基本料Ｆ）
                                   WHEN   187
      *                            療養病棟入院基本料２（入院基本料Ｇ）
                                   WHEN   188
      *                            療養病棟入院基本料２（入院基本料Ｈ）
                                   WHEN   189
      *                            療養病棟入院基本料２（入院基本料Ｉ）
                                   WHEN   190
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間超過）
                                   WHEN   199
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間超過）
                                   WHEN   004
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間超過）
                                   WHEN   005
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間超過）
                                   WHEN   006
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間超過）
                                   WHEN   007
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間超過）
                                   WHEN   008
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間超過）
                                   WHEN   010
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間超過）
                                   WHEN   011
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間超過）
                                   WHEN   012
      *                            療養病棟入院基本料２（入院基本料Ａ）（看護職員数等経過措置）
                                   WHEN   048
      *                            療養病棟入院基本料２（入院基本料Ｂ）（看護職員数等経過措置）
                                   WHEN   049
      *                            療養病棟入院基本料２（入院基本料Ｃ）（看護職員数等経過措置）
                                   WHEN   051
      *                            療養病棟入院基本料２（入院基本料Ｄ）（看護職員数等経過措置）
                                   WHEN   052
      *                            療養病棟入院基本料２（入院基本料Ｅ）（看護職員数等経過措置）
                                   WHEN   053
      *                            療養病棟入院基本料２（入院基本料Ｆ）（看護職員数等経過措置）
                                   WHEN   054
      *                            療養病棟入院基本料２（入院基本料Ｇ）（看護職員数等経過措置）
                                   WHEN   057
      *                            療養病棟入院基本料２（入院基本料Ｈ）（看護職員数等経過措置）
                                   WHEN   058
      *                            療養病棟入院基本料２（入院基本料Ｉ）（看護職員数等経過措置）
                                   WHEN   059
      *                            療養病棟入院基本料２（入院基本料Ａ）（夜勤時間特別入院基本料）
                                   WHEN   060
      *                            療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間特別入院基本料）
                                   WHEN   061
      *                            療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間特別入院基本料）
                                   WHEN   062
      *                            療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間特別入院基本料）
                                   WHEN   063
      *                            療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間特別入院基本料）
                                   WHEN   064
      *                            療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間特別入院基本料）
                                   WHEN   065
      *                            療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間特別入院基本料）
                                   WHEN   066
      *                            療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間特別入院基本料）
                                   WHEN   067
      *                            療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間特別入院基本料）
                                   WHEN   068
      *                            特定入院基本料（障害者施設等入院基本料）
                                   WHEN   039
      *                            特定入院基本料（障害者施設等入院基本料）（夜勤時間超過減算）
                                   WHEN   041
      *                            一般病棟・療養病棟入院基本料１
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   196
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                                   END-EVALUATE
                               WHEN  "1"
      *                            特定入院料の定数超過は平成２６年４月より廃止
                                   CONTINUE
                           END-EVALUATE
                           MOVE    WRK-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(2)
                           MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-TEISU-SRYCD(1)    =   SPACE)   AND
                 (LNK-TEISU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-TEISU-RC
           END-IF
      *
           .
       210-TEISU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    定数超過の診療コード判定取得（平成３０年４月以降）
      *****************************************************************
       210-TEISU5-SEC            SECTION.
      *
      *定数超過情報の取得（システム管理テーブルより）
           MOVE    SPACE               TO  WRK-TEISUKBN
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-TEISU-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR  "0"
                       CONTINUE
                   ELSE
                       MOVE  SYS-5000-TEISUKBN  TO  WRK-TEISUKBN
                       EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                           WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        コード表用番号からの判定に変更
                               EVALUATE TNS-CDKBN-KBNNUM    ALSO
                                        TNS-CDKBN-KOUBAN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   101   ALSO   03
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   109   ALSO   01
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   109   ALSO   02
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   109   ALSO   03
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   109   ALSO   04
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   109   ALSO   05
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   109   ALSO   06
      *                            療養病床以外・有床診療所療養病床入院
                                   WHEN   108   ALSO   13
      *                            療養病棟入院料１
                                   WHEN   101   ALSO   01
      *                            療養病棟入院料２
                                   WHEN   101   ALSO   02
      *                            療養病棟入院料２（経過措置１）
                                   WHEN   101   ALSO   10
      *                            療養病棟入院料２（経過措置２）
                                   WHEN   101   ALSO   11
      *                            特定入院基本料（障害者施設等入院基本料）
      *                            特定入院基本料（障害者施設等入院基本料）（夜勤時間超過減算）
                                   WHEN   106   ALSO   08
      *                            一般病棟・療養病棟入院基本料１
                                   WHEN   100   ALSO   14
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   317   ALSO   08
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                           WHEN  "1"
      *                        特定入院料の定数超過は平成２６年４月より廃止
                               CONTINUE
                       END-EVALUATE
                       MOVE    LNK-TEISU-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(1)
                       MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(1)
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *定数超過情報の取得（システム管理テーブルより翌月分を取得）
           MOVE    LNK-TEISU-SANTEIYM  
                                       TO  WRK-SANTEIYM
           IF      LNK-TEISU-SANTEIYM(5:2)  = "12"
               ADD    1                TO  WRK-SANTEI-YY
               MOVE   1                TO  WRK-SANTEI-MM
           ELSE
               ADD    1                TO  WRK-SANTEI-MM
           END-IF
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SANTEIYM        TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI     =      ZERO
                   IF   SYS-5000-TEISUKBN  =  SPACE OR "0"
                       CONTINUE
                   ELSE
                       IF   WRK-TEISUKBN   =  SYS-5000-TEISUKBN
                           CONTINUE
                       ELSE
                           EVALUATE  LNK-TEISU-TOKUTEINYUIN-KBN
                             WHEN  "0"
                               INITIALIZE                TNS-TENSU-REC
                               MOVE    LNK-TEISU-SANTEIYM
                                                     TO  TNS-YUKOSTYMD
                                                                 (1:6)
                               MOVE    "01"    TO  TNS-YUKOSTYMD (7:2)
                               MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                               MOVE    SPA-HOSPNUM   TO  TNS-HOSPNUM
                               MOVE    LNK-TEISU-SANTEISRYCD
                                                     TO  TNS-SRYCD
                               MOVE    TNS-TENSU-REC TO  MCPDATA-REC
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM 910-DBSELECT-SEC
                               IF      MCP-RC              =   ZERO
                                   MOVE  "tbl_tensu" TO  MCP-TABLE
                                   MOVE  "key"       TO  MCP-PATHNAME
                                   PERFORM 910-DBFETCH-SEC
                                   IF      MCP-RC     =  ZERO
                                       MOVE  MCPDATA-REC
                                                     TO  TNS-TENSU-REC
                                       MOVE  ZERO    TO  FLG-TENSU
                                   ELSE
                                       INITIALIZE        TNS-TENSU-REC
                                       MOVE    1     TO  FLG-TENSU
                                   END-IF
                               ELSE
                                   INITIALIZE            TNS-TENSU-REC
                                   MOVE    1         TO  FLG-TENSU
                               END-IF
      *
                               MOVE    "tbl_tensu"   TO  MCP-TABLE
                               MOVE    "key"         TO  MCP-PATHNAME
                               PERFORM  910-DB-CLOSE-SEC
      *
      *                        コード表用番号からの判定に変更
                               EVALUATE TNS-CDKBN-KBNNUM    ALSO
                                        TNS-CDKBN-KOUBAN
      *                            療養病棟入院基本料（特別入院基本料）
                                   WHEN   101   ALSO   03
      *                            有床診療所療養病床入院基本料Ａ
                                   WHEN   109   ALSO   01
      *                            有床診療所療養病床入院基本料Ｂ
                                   WHEN   109   ALSO   02
      *                            有床診療所療養病床入院基本料Ｃ
                                   WHEN   109   ALSO   03
      *                            有床診療所療養病床入院基本料Ｄ
                                   WHEN   109   ALSO   04
      *                            有床診療所療養病床入院基本料Ｅ
                                   WHEN   109   ALSO   05
      *                            有床診療所療養病床入院基本料（特別入院基本料）
                                   WHEN   109   ALSO   06
      *                            療養病床以外・有床診療所療養病床入院
                                   WHEN   108   ALSO   13
      *                            療養病棟入院料１
                                   WHEN   101   ALSO   01
      *                            療養病棟入院料２
                                   WHEN   101   ALSO   02
      *                            療養病棟入院料２（経過措置１）
                                   WHEN   101   ALSO   10
      *                            療養病棟入院料２（経過措置２）
                                   WHEN   101   ALSO   11
      *                            特定入院基本料（障害者施設等入院基本料）
      *                            特定入院基本料（障害者施設等入院基本料）（夜勤時間超過減算）
                                   WHEN   106   ALSO   08
      *                            一般病棟・療養病棟入院基本料１
                                   WHEN   100   ALSO   14
      *                            特定一般病棟・療養病棟入院基本料１
                                   WHEN   317   ALSO   08
                                      MOVE   CONS-TEISU1  TO
                                                LNK-TEISU-SRYCD(1)
                                   WHEN   OTHER
                                      MOVE   CONS-TEISU2  TO
                                                LNK-TEISU-SRYCD(1)
                               END-EVALUATE
                             WHEN  "1"
      *                            特定入院料の定数超過は平成２６年４月より廃止
                                   CONTINUE
                           END-EVALUATE
                           MOVE    WRK-SANTEIYM   TO
                                                LNK-TEISU-STARTYM(2)
                           MOVE    SYS-5000-EDYUKYMD(1:6)   TO
                                                LNK-TEISU-ENDYM(2)
                       END-IF
                   END-IF
               ELSE
                   MOVE  2     TO    LNK-TEISU-RC
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    (LNK-TEISU-SRYCD(1)    =   SPACE)   AND
                 (LNK-TEISU-SRYCD(2)    =   SPACE)
                 MOVE   3        TO  LNK-TEISU-RC
           END-IF
      *
           .
       210-TEISU5-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
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
      *
      *****************************************************************
      *    テーブル読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
