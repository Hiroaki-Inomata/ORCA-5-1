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
       PROGRAM-ID.             ORCGW04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 患者番号構成管理情報（Ｗ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/11/27  自由構成区分追加
      *  02.00.01    NACL-多々納  02/11/12  一括入力 
      *  02.00.02    NACL-多々納  03/03/17  連番号変更可能へ修正
      *  02.00.03    NACL-多々納  06/04/24  MONFUNC 対応
      *  03.04.00    NACL-多々納  07/01/15  年切替え月追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W04.
           03  SPA-W04-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-NEN            PIC X(04).
                   07  SPA-GMN-NENTUKI        PIC X(02).
                   07  SPA-GMN-RENNUM         PIC 9(10).
                   07  SPA-GMN-PTID           PIC 9(10).
                   07  SPA-GMN-PAGENO         PIC 9(01).
                   07  SPA-GMN-PTNUMKSIKBN    PIC X(01).
                   07  SPA-GMN-JIYKSI-AREA.
                       09  SPA-GMN-JIYKSIKBN      PIC X(01).
                       09  SPA-GMN-JIYKSIKETA     PIC 9(02).
                   07  SPA-GMN-HJNKSI-AREA.
                       09  SPA-GMN-HJNKSIKBN      PIC X(01).
                       09  SPA-GMN-HJNKSINENKBN   PIC X(01).
                       09  SPA-GMN-HJNKSIRENNUMKBN
                                                  PIC X(01).
                       09  SPA-GMN-HJNKSIRENNUMKETA
                                                  PIC 9(02). 
                   07  SPA-GMN-KKCKSI-AREA.
                       09  SPA-GMN-KKCKSIKBN      PIC X(01).
                       09  SPA-GMN-KKCKSIMAEKETA  PIC 9(02).
                       09  SPA-GMN-KKCKSIRENNUMKETA
                                                  PIC 9(02).
                       09  SPA-GMN-KKCKSIATOKETA  PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *
                   07  SPA-MAE-RENNUM         PIC 9(10).
                   07  SPA-PTNUM-HEN          PIC 9(01).
      *
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-TOTAL           PIC 9(03).
           03  WRK-WIDMSG          PIC X(80). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療内容情報
           COPY  "CPSK1009.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W04
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W04         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
               IF     (FLG-END             =   ZERO  )
                 AND  (SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W04"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               PERFORM 490-KOUSHIN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W04-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      * 
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1009-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-GMN-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN  TO  SPA-GMN-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA TO  SPA-GMN-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-GMN-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                           TO  SPA-GMN-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-GMN-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-GMN-HJNKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIKBN  TO  SPA-GMN-KKCKSIKBN 
               MOVE    SYS-1009-KKCKSIMAEKETA
                                           TO  SPA-GMN-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                           TO  SPA-GMN-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                           TO  SPA-GMN-KKCKSIATOKETA
               MOVE    SYS-1009-NEN        TO  SPA-GMN-NEN
               MOVE    SYS-1009-RENNUM     TO  SPA-GMN-RENNUM  
               MOVE    SYS-1009-PTID       TO  SPA-GMN-PTID
      *
               MOVE    SYS-1009-NENTUKI    TO  SPA-GMN-NENTUKI
      *
               MOVE    SYS-1009-RENNUM     TO  SPA-MAE-RENNUM  
           ELSE
               MOVE    SPA-SYSYMD (1:4)    TO  SPA-GMN-NEN
               MOVE    "01"                TO  SPA-GMN-NENTUKI
               MOVE    ZERO                TO  SPA-MAE-RENNUM  
           END-IF
      *    年切替え月
           IF      SPA-GMN-NENTUKI     =   SPACE
               MOVE    "01"            TO  SPA-GMN-NENTUKI
           END-IF
      *
            MOVE    1                   TO  SPA-GMN-CUR
            EVALUATE    SPA-GMN-PTNUMKSIKBN
                WHEN    "1"
                    MOVE    0               TO  SPA-GMN-PAGENO
                WHEN    "2"
                    MOVE    1               TO  SPA-GMN-PAGENO
                WHEN    "3"
                    MOVE    2               TO  SPA-GMN-PAGENO
                WHEN    OTHER
                    MOVE    0               TO  SPA-GMN-PAGENO 
           END-EVALUATE    
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    タグ
      *        WHEN    "CLICKED"       ALSO    "N01"
      *        WHEN    "CLICKED"       ALSO    "N02"
      *        WHEN    "CLICKED"       ALSO    "N03"
      **           PERFORM 440-TAG-CHANGE-SEC
      *    患者番号変更
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-PTNUM-HENKOU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *ver.4.7
      *        タグ切替え
               WHEN    "SWITCH"       ALSO    "not1"
                   PERFORM 440-TAG-CHANGE-SEC
      *
      *        患者番号構成区分
               WHEN    "ACTIVATE"      ALSO    "PTNUMKSIKBN"
                   PERFORM 4101-PTNUMKSIKBN-CHK-SEC
      *        患者番号構成区分
               WHEN    "ACTIVATE"      ALSO    "RENNUM"
                   MOVE    ZERO            TO  FLG-KOUSIN
                   PERFORM 4109-RENNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    ANY
      *            一括入力
                   MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
                   MOVE    ZERO                TO  SPA-GMN-CUR
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号構成区分チェック
      *****************************************************************
       4101-PTNUMKSIKBN-CHK-SEC       SECTION.
      *
           MOVE    1                      TO  SPA-GMN-CUR
      *
           MOVE    W04-PTNUMKSIKBN        TO  SPA-GMN-PTNUMKSIKBN
      *
           EVALUATE    SPA-GMN-PTNUMKSIKBN
               WHEN    "1"
                   MOVE    ZERO          TO  SPA-GMN-PAGENO
                   MOVE    11            TO  SPA-GMN-CUR
                   INITIALIZE            SPA-GMN-HJNKSI-AREA
                   INITIALIZE            SPA-GMN-KKCKSI-AREA
               WHEN    "2"
                   MOVE    1             TO  SPA-GMN-PAGENO
                   MOVE    03            TO  SPA-GMN-CUR
                   INITIALIZE            SPA-GMN-JIYKSI-AREA
                   INITIALIZE            SPA-GMN-KKCKSI-AREA
               WHEN    "3"
                   MOVE    2             TO  SPA-GMN-PAGENO
                   MOVE    07            TO  SPA-GMN-CUR
                   INITIALIZE            SPA-GMN-JIYKSI-AREA
                   INITIALIZE            SPA-GMN-HJNKSI-AREA
               WHEN    OTHER
                   MOVE    "0001"            TO  SPA-ERRCD
           END-EVALUATE
           .
       4101-PTNUMKSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *    自由構成
           MOVE    W04-JIYKSIKBN           TO  SPA-GMN-JIYKSIKBN
           MOVE    W04-JIYKSIKETA          TO  SPA-GMN-JIYKSIKETA
      *    標準構成
           MOVE    W04-HJNKSIKBN           TO  SPA-GMN-HJNKSIKBN
           MOVE    W04-HJNKSINENKBN        TO  SPA-GMN-HJNKSINENKBN
           MOVE    W04-HJNKSIRENNUMKBN     TO  SPA-GMN-HJNKSIRENNUMKBN
           MOVE    W04-HJNKSIRENNUMKETA    TO  SPA-GMN-HJNKSIRENNUMKETA
      *    拡張構成
           MOVE    W04-KKCKSIKBN           TO  SPA-GMN-KKCKSIKBN
           MOVE    W04-KKCKSIMAEKETA       TO  SPA-GMN-KKCKSIMAEKETA
           MOVE    W04-KKCKSIRENNUMKETA    TO  SPA-GMN-KKCKSIRENNUMKETA
           MOVE    W04-KKCKSIATOKETA       TO  SPA-GMN-KKCKSIATOKETA
      *
           MOVE    W04-NENTUKI             TO  SPA-GMN-NENTUKI
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *   年切替え月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-NENTUKI-CHK-SEC
           END-IF
           EVALUATE    SPA-GMN-PTNUMKSIKBN
               WHEN    "1"
      *            自由構成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4120-JIYKSIKBN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-JIYKSIKETA-CHK-SEC
                   END-IF
               WHEN    "2"
      *            標準構成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4112-HJNKSIKBN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4113-HJNKSINENKBN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4114-HJNKSIRENNUMKBN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4115-HJNKSIRENNUMKETA-CHK-SEC
                   END-IF
               WHEN    "3"
      *            拡張構成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4116-KKCKSIKBN-CHK-SEC  
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4117-KKCKSIMAEKETA-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4118-KKCKSIRENNUMKETA-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4119-KKCKSIATOKETA-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年切替え月チェック処理
      *****************************************************************
       4113-NENTUKI-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-NENTUKI     =   SPACE
               MOVE    "01"                TO  SPA-GMN-NENTUKI
           END-IF
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-NENTUKI     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    21                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-NENTUKI
               IF      SPA-GMN-NENTUKI     >=  "01"  AND   <=  "12"
                   CONTINUE
               ELSE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    21                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           . 
       4113-NENTUKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自由構成区分チェック
      *****************************************************************
       4120-JIYKSIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-JIYKSIKBN
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
                   CONTINUE
     ****         MOVE    2                  TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4120-JIYKSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自由構成桁数チェック
      *****************************************************************
       4100-JIYKSIKETA-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-JIYKSIKETA  >   ZERO
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       4100-JIYKSIKETA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準構成区分チェック処理
      *****************************************************************
       4112-HJNKSIKBN-CHK-SEC           SECTION.
      *
      *
           EVALUATE    SPA-GMN-HJNKSIKBN
               WHEN    "1" THRU "4"
                   CONTINUE
      ****         MOVE    4                   TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4112-HJNKSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準構成年区分チェック処理
      *****************************************************************
       4113-HJNKSINENKBN-CHK-SEC        SECTION.
      *
      *
           EVALUATE    SPA-GMN-HJNKSINENKBN
               WHEN    "1" 
               WHEN    "2"
                   CONTINUE
      ****         MOVE    5                   TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
           END-EVALUATE
           . 
       4113-HJNKSINENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    標準構成連番初期化区分チェック処理
      *****************************************************************
       4114-HJNKSIRENNUMKBN-CHK-SEC        SECTION.
      *
      *
           EVALUATE    SPA-GMN-HJNKSIRENNUMKBN
               WHEN    "1" 
               WHEN    "2"
                   CONTINUE
      *******      MOVE    6                   TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4114-HJNKSIRENNUMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準構成連番号桁数チェック処理
      *****************************************************************
       4115-HJNKSIRENNUMKETA-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-HJNKSIRENNUMKETA    >   ZERO
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
       4115-HJNKSIRENNUMKETA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    拡張構成区分チェック処理
      *****************************************************************
       4116-KKCKSIKBN-CHK-SEC        SECTION.
      *
      *
           EVALUATE    SPA-GMN-KKCKSIKBN
               WHEN    "1" 
               WHEN    "2"
               WHEN    "3"
                   CONTINUE
      ****         MOVE    8                   TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4116-KKCKSIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    拡張構成前桁数チェック処理
      *****************************************************************
       4117-KKCKSIMAEKETA-CHK-SEC        SECTION.
      *
           .
       4117-KKCKSIMAEKETA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準構成連番号桁数チェック処理
      *****************************************************************
       4118-KKCKSIRENNUMKETA-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-KKCKSIRENNUMKETA    >   ZERO
               CONTINUE
           ELSE
               MOVE    9                       TO  SPA-GMN-CUR
               MOVE    "0001"                  TO  SPA-ERRCD
           END-IF
           .
       4118-KKCKSIRENNUMKETA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    拡張構成前桁数チェック処理
      *****************************************************************
       4119-KKCKSIATOKETA-CHK-SEC        SECTION.
      *
           .
       4119-KKCKSIATOKETA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
      *ver.4.7
           MOVE    W04-PAGENO          TO   IDX
      *
      *    EVALUATE    MCP-WIDGET
      *        WHEN    "N01"
      *            MOVE   0            TO  IDX
      *        WHEN    "N02"
      *           MOVE    1            TO  IDX
      *        WHEN    "N03"
      *            MOVE   2            TO  IDX
      *****END-EVALUATE
      *
      *    タグが変わったとき関連チェック
           IF      IDX              NOT =   SPA-GMN-PAGENO
               IF      SPA-GMN-PTNUMKSIKBN  =   "1" OR  "2" OR  "3"
                   MOVE    "0005"               TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD            =   SPACE
                   PERFORM 4900-KANRENCHK-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD            =   SPACE
               MOVE    IDX                  TO  SPA-GMN-PAGENO
               EVALUATE    IDX
                   WHEN    0
                       MOVE    1            TO  SPA-GMN-CUR
                   WHEN    1
                       MOVE    20           TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    30           TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF        
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者番号変更処理
      *****************************************************************
       409-PTNUM-HENKOU-SEC                      SECTION.
      *    患者番号変更
           MOVE    1                   TO  SPA-PTNUM-HEN
           MOVE    20                  TO  SPA-GMN-CUR
      *
           .
       409-PTNUM-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者番号変更処理
      *****************************************************************
       4109-RENNUM-CHK-SEC                      SECTION.
      *    患者番号変更
           MOVE    W04-RENNUM          TO  SPA-GMN-RENNUM
      *    年度毎に連番のクリアをする時
           IF   (SPA-GMN-NEN            NOT =   SPA-SYSYMD (1:4))
            AND (SPA-GMN-NENTUKI            <=  SPA-SYSYMD (5:2))
               MOVE    SPA-SYSYMD(1:4)          TO  SYS-1009-NEN
               IF     (SPA-GMN-PTNUMKSIKBN      =   "2")   AND
                      (SPA-GMN-HJNKSIRENNUMKBN  =   "1")
                 AND  (SPA-GMN-HJNKSIKBN        =   "1"
                                                OR  "2" )
                   IF      SPA-GMN-RENNUM   NOT =   ZERO
                        MOVE    "9000"          TO  SPA-ERRCD
                        MOVE    20              TO  SPA-GMN-CUR
                        GO      TO      4109-RENNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-KOUSIN          =   1
               GO      TO      4109-RENNUM-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-RENNUM      <   SPA-MAE-RENNUM
               MOVE    "9001"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN-CUR
           ELSE
               MOVE    99              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4109-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W04"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-W01KYOUTU
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
              IF   (SPA-GMN-NEN            NOT =   SPA-SYSYMD (1:4))
               AND (SPA-GMN-NENTUKI            <=  SPA-SYSYMD (5:2))
                  IF     (SPA-GMN-PTNUMKSIKBN      =   "2")   AND
                         (SPA-GMN-HJNKSIRENNUMKBN  =   "1")
                    AND  (SPA-GMN-HJNKSIKBN        =   "1"
                                                   OR  "2" )
                      MOVE    "1011"           TO  SPA-WIDCD
                  ELSE
                      MOVE    "1010"           TO  SPA-WIDCD
                  END-IF
               END-IF  
      *        連番号の修正
               IF      SPA-PTNUM-HEN       =   1
                   MOVE    1               TO  FLG-KOUSIN
                   PERFORM 4109-RENNUM-CHK-SEC
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"                  TO  SYS-1009-KANRICD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1009-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1009-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1009-REC
               INITIALIZE                      SYS-1009-REC
               MOVE   "1009"                   TO  SYS-1009-KANRICD
               MOVE    "*"                     TO  SYS-1009-KBNCD
               MOVE    SPA-W01-STYUKYMD        TO  SYS-1009-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-1009-EDYUKYMD
               MOVE    SPA-GMN-NEN             TO  SYS-1009-NEN
           END-IF
      *    連番号更新
           IF      SPA-PTNUM-HEN       =   1
               MOVE    SPA-GMN-RENNUM      TO  SYS-1009-RENNUM
           END-IF
      *
           IF   (SPA-GMN-NEN            NOT =   SPA-SYSYMD (1:4))
            AND (SPA-GMN-NENTUKI            <=  SPA-SYSYMD (5:2))
               MOVE    SPA-SYSYMD(1:4)          TO  SYS-1009-NEN
               IF     (SPA-GMN-PTNUMKSIKBN      =   "2")   AND
                      (SPA-GMN-HJNKSIRENNUMKBN  =   "1")
                 AND  (SPA-GMN-HJNKSIKBN        =   "1"
                                                OR  "2" )
                   MOVE    ZERO                 TO  SYS-1009-RENNUM
               END-IF
           END-IF
           MOVE    SPA-GMN-NENTUKI          TO  SYS-1009-NENTUKI
      *
           MOVE    SPA-GMN-PTNUMKSIKBN      TO  SYS-1009-PTNUMKSIKBN 
           MOVE    SPA-GMN-JIYKSIKBN        TO  SYS-1009-JIYKSIKBN 
           MOVE    SPA-GMN-JIYKSIKETA       TO  SYS-1009-JIYKSIKETA 
           MOVE    SPA-GMN-HJNKSIKBN        TO  SYS-1009-HJNKSIKBN 
           MOVE    SPA-GMN-HJNKSINENKBN     TO  SYS-1009-HJNKSINENKBN
           MOVE    SPA-GMN-HJNKSIRENNUMKBN  TO
                                            SYS-1009-HJNKSIRENNUMKBN
           MOVE    SPA-GMN-HJNKSIRENNUMKETA TO
                                            SYS-1009-HJNKSIRENNUMKETA
           MOVE    SPA-GMN-KKCKSIKBN        TO  SYS-1009-KKCKSIKBN 
           MOVE    SPA-GMN-KKCKSIMAEKETA    TO  SYS-1009-KKCKSIMAEKETA
           MOVE    SPA-GMN-KKCKSIRENNUMKETA TO
                                            SYS-1009-KKCKSIRENNUMKETA
           MOVE    SPA-GMN-KKCKSIATOKETA    TO  SYS-1009-KKCKSIATOKETA
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC            TO  MCPDATA-REC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               DISPLAY "W04 SYSKANRI INV:" MCP-RC  ",KEY:" SYS-1009-KEY
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4900-KANRENCHK-EXT
           END-IF
      *
           EVALUATE    W04-PAGENO
               WHEN    0
                   PERFORM 4901-TAG1-CHK-SEC
               WHEN    1
                   PERFORM 4901-TAG2-CHK-SEC
               WHEN    2
                   PERFORM 4901-TAG3-CHK-SEC
           END-EVALUATE
      *
           IF      SPA-ERRCD             =   SPACE
               IF      WRK-TOTAL             >   20
                   MOVE    "0003"                TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       自由構成タグの関連チェック処理
      *****************************************************************
       4901-TAG1-CHK-SEC           SECTION.
      *
           IF     (SPA-GMN-JIYKSIKETA       >   ZERO   )  AND
                  (SPA-GMN-JIYKSIKBN        =   "0" OR  "1" OR  "2")
               CONTINUE
           ELSE        
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-GMN-HJNKSIKBN        =   SPACE)   AND
                  (SPA-GMN-HJNKSINENKBN     =   SPACE)   AND
                  (SPA-GMN-HJNKSIRENNUMKBN  =   SPACE)   AND
                  (SPA-GMN-HJNKSIRENNUMKETA =   ZERO)
               CONTINUE
           ELSE
               MOVE    "0002"               TO  SPA-ERRCD
           END-IF
      *  
           IF     (SPA-GMN-KKCKSIKBN        =   SPACE)   AND
                  (SPA-GMN-KKCKSIMAEKETA    =   ZERO )   AND
                  (SPA-GMN-KKCKSIRENNUMKETA =   ZERO )   AND
                  (SPA-GMN-KKCKSIATOKETA    =   ZERO)
               CONTINUE
           ELSE
               MOVE    "0002"               TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD                =   SPACE
               MOVE    SPA-GMN-JIYKSIKETA   TO  WRK-TOTAL
           END-IF
      *
           .
       4901-TAG1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       標準構成タグの関連チェック処理
      *****************************************************************
       4901-TAG2-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-HJNKSIKBN        =   "1" OR "2" OR
                                                "3" OR "4"
               CONTINUE
           ELSE
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
           IF      SPA-GMN-HJNKSINENKBN     =   "1" OR "2"
               CONTINUE
           ELSE
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
           IF      SPA-GMN-HJNKSIRENNUMKBN  =   "1" OR "2"
               CONTINUE
           ELSE
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
           IF      SPA-GMN-HJNKSIRENNUMKETA =   ZERO
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-GMN-JIYKSIKETA   NOT =   ZERO     AND 
                   SPA-GMN-JIYKSIKBN    NOT =   SPACE
               MOVE    "0002"               TO  SPA-ERRCD
           END-IF
      *  
           IF     (SPA-GMN-KKCKSIKBN        =   SPACE)   AND
                  (SPA-GMN-KKCKSIMAEKETA    =   ZERO )   AND
                  (SPA-GMN-KKCKSIRENNUMKETA =   ZERO )   AND
                  (SPA-GMN-KKCKSIATOKETA    =   ZERO)
               CONTINUE
           ELSE
               MOVE    "0002"               TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD                =   SPACE
               IF      SPA-GMN-HJNKSIKBN        =   "1"  OR  "2"
                   IF      SPA-GMN-HJNKSINENKBN     =   "1"
                       MOVE    3                    TO  WRK-TOTAL
                   ELSE
                       MOVE    4                    TO  WRK-TOTAL
                   END-IF
               END-IF
               COMPUTE WRK-TOTAL           =   WRK-TOTAL
                                           +   SPA-GMN-HJNKSIRENNUMKETA
               IF      SPA-GMN-HJNKSIKBN   =   "1"  OR  "3"
                   COMPUTE WRK-TOTAL           =   WRK-TOTAL   +   1
               END-IF
           END-IF
           .
       4901-TAG2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       拡張構成タグの関連チェック処理
      *****************************************************************
       4901-TAG3-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-KKCKSIKBN     =   "1" OR "2" OR "3"
               CONTINUE
           ELSE
               MOVE    "0001"            TO  SPA-ERRCD
           END-IF
           IF      SPA-GMN-KKCKSIRENNUMKETA   =   ZERO
               MOVE    "0001"            TO  SPA-ERRCD
           END-IF
           EVALUATE    SPA-GMN-KKCKSIKBN
               WHEN    "1"
                   IF  (SPA-GMN-KKCKSIMAEKETA  =   ZERO)  OR
                       (SPA-GMN-KKCKSIATOKETA  =   ZERO)
                       MOVE    "0001"            TO  SPA-ERRCD
                   END-IF
               WHEN    "2"
                   IF  (SPA-GMN-KKCKSIMAEKETA      =   ZERO)  OR
                       (SPA-GMN-KKCKSIATOKETA  NOT =   ZERO)
                       MOVE    "0001"            TO  SPA-ERRCD
                   END-IF
               WHEN    "3"
                   IF  (SPA-GMN-KKCKSIMAEKETA  NOT =   ZERO)  OR
                       (SPA-GMN-KKCKSIATOKETA      =   ZERO)
                       MOVE    "0001"            TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           IF      SPA-GMN-JIYKSIKETA   NOT =   ZERO     AND
                   SPA-GMN-JIYKSIKBN    NOT =   SPACE
               MOVE    "0002"            TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-GMN-HJNKSIKBN        =   SPACE)   AND
                  (SPA-GMN-HJNKSINENKBN     =   SPACE)   AND
                  (SPA-GMN-HJNKSIRENNUMKBN  =   SPACE)   AND
                  (SPA-GMN-HJNKSIRENNUMKETA =   ZERO)
               CONTINUE
           ELSE
               MOVE    "0002"               TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD                =   SPACE
               COMPUTE WRK-TOTAL        =   SPA-GMN-KKCKSIMAEKETA
                                        +   SPA-GMN-KKCKSIRENNUMKETA
                                        +   SPA-GMN-KKCKSIATOKETA
           END-IF
           .
       4901-TAG3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *
           MOVE    SPA-GMN-STYUKYMD         TO  W04-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD         TO  W04-EDYUKYMD
           MOVE    SPA-GMN-NEN              TO  W04-NEN
           MOVE    SPA-GMN-PTID             TO  W04-PTID
           MOVE    SPA-GMN-RENNUM           TO  W04-RENNUM
           MOVE    SPA-GMN-NENTUKI          TO  W04-NENTUKI
      * 
           MOVE    SPA-GMN-PTNUMKSIKBN      TO  W04-PTNUMKSIKBN
           MOVE    SPA-GMN-JIYKSIKETA       TO  W04-JIYKSIKETA
           MOVE    SPA-GMN-JIYKSIKBN        TO  W04-JIYKSIKBN
           MOVE    SPA-GMN-HJNKSIKBN        TO  W04-HJNKSIKBN
           MOVE    SPA-GMN-HJNKSINENKBN     TO  W04-HJNKSINENKBN  
           MOVE    SPA-GMN-HJNKSIRENNUMKBN  TO  W04-HJNKSIRENNUMKBN
           MOVE    SPA-GMN-HJNKSIRENNUMKETA TO  W04-HJNKSIRENNUMKETA
           MOVE    SPA-GMN-KKCKSIKBN        TO  W04-KKCKSIKBN
           MOVE    SPA-GMN-KKCKSIMAEKETA    TO  W04-KKCKSIMAEKETA
           MOVE    SPA-GMN-KKCKSIRENNUMKETA TO  W04-KKCKSIRENNUMKETA
           MOVE    SPA-GMN-KKCKSIATOKETA    TO  W04-KKCKSIATOKETA
      *
           MOVE    SPA-GMN-PAGENO           TO  W04-PAGENO
      *
      *    連番号入力のプロテクト
           IF      SPA-PTNUM-HEN       =   1
               MOVE    WIDGET-NORMAL       TO  W04-RENNUM-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  W04-RENNUM-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入力できない項目に入力があります"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "合計桁数が長すぎます"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   STRING  "年切替え月は、０１から１２で"
                           "入力して下さい。"  DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   MOVE
                       "患者番号構成区分を変更して画面を移動して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "9000"
                   MOVE    "連番号は年度変更により、ゼロに変更されます"
                                               TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE
                   "警告！連番号は前回より後の番号にして下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W04"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "患者番号構成管理情報を登録します"
                                       TO  WRK-WIDMSG
               WHEN    "1010"
      *D           MOVE    "年が変わったので変更して登録します"
      *D                               TO  WRK-WIDMSG
                   STRING  "年切替え月になりました。"
                           "年を変更して登録します。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1011"
      *D           MOVE    "連番号を初期化して登録します"
      *D                               TO  WRK-WIDMSG
                   STRING  "年切替え月になりました。"
                           "年変更と連番号の初期化をして登録します。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W04"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "PTNUMKSIKBN"      TO   MCP-WIDGET
                WHEN    2
                    MOVE  "JIYKSIKETA"       TO   MCP-WIDGET
                WHEN    3
                    MOVE  "HJNKSIKBN"        TO   MCP-WIDGET
                WHEN    4
                    MOVE  "HJNKSINENKBN"     TO   MCP-WIDGET
                WHEN    5
                    MOVE  "HJNKSIRENNUMKBN"  TO   MCP-WIDGET
                WHEN    6
                    MOVE  "HJNKSIRENNUMKETA" TO   MCP-WIDGET
                WHEN    7
                    MOVE  "KKCKSIKBN"        TO   MCP-WIDGET
                WHEN    8
                    MOVE  "KKCKSIMAEKETA"    TO   MCP-WIDGET
                WHEN    9
                    MOVE  "KKCKSIRENNUMKETA" TO   MCP-WIDGET
                WHEN    10
                    MOVE  "KKCKSIATOKETA"    TO   MCP-WIDGET
                WHEN    11
                    MOVE  "JIYKSIKBN"        TO   MCP-WIDGET
                WHEN    20
                    MOVE  "RENNUM"           TO   MCP-WIDGET
                WHEN    21
                    MOVE  "NENTUKI"          TO   MCP-WIDGET
                WHEN    99
                    MOVE  "B12"              TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "JIYKSIKBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "JIYKSIKETA"
                   MOVE    99              TO  SPA-GMN-CUR
               WHEN    "NENTUKI"
                   MOVE    99              TO  SPA-GMN-CUR
      *        標準構成
               WHEN    "HJNKSIKBN"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "HJNKSINENKBN"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "HJNKSIRENNUMKBN"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "HJNKSIRENNUMKETA"
                   MOVE    99              TO  SPA-GMN-CUR
      *    拡張構成情報タグ
               WHEN    "KKCKSIKBN"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "KKCKSIMAEKETA"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "KKCKSIRENNUMKETA"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "KKCKSIATOKETA"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " "
                                                    BY  "0"   
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
