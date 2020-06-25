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
       PROGRAM-ID.             ORCGW05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 診療科目管理情報（Ｗ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-藤原     01/09/11  削除処理の修正 
      * 01.01.01     MCC-藤原     01/09/21  JOINの修正      
      * 01.01.02     NACL-多々納  02/11/12  一括入力 
      * 01.01.03     NACL-太田    03/09/11  登録時チェック追加
      * 01.01.04     NACL-藤原    06/07/03  レセ電診療科コードのチェック
      *                                     追加
      * 01.01.05     NACL-多々納  06/07/13  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/05/02  グループ診療対応
      * 04.07.00     NACL-多々納  13/03/XX  Ｈ２６年４月改正対応
      *                                    （時間外加算等１設定追加）
      * 05.00.00     NACL-太田    18/12/04  レセ電診療科コード
      *                                     コンボボックス対応
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
       01  SPA-W05.
           03  SPA-W05-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-SRYCD          PIC X(02).
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SRYKANAME  PIC X(40).
                       09  SPA-GMN-SRYKANAME1 PIC X(10).
                       09  SPA-GMN-SRYKANAME2 PIC X(06).
                       09  SPA-GMN-SRYKANAME3 PIC X(02).
                       09  SPA-GMN-RECESRYKA-G.
                           11  SPA-GMN-RECESRYKA  PIC X(02). 
                           11  SPA-GMN-RECESRYKAFL
                                                  PIC X(01). 
                           11  SPA-GMN-RECESRYKANAME
                                                  PIC X(40).
      *                処置時間外加算等１
                       09  SPA-GMN-JIKANKSN1-40
                                              PIC X(01).
      *                手術時間外加算等１
                       09  SPA-GMN-JIKANKSN1-50
                                              PIC X(01).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
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
           03  WRK-WIDMSG          PIC X(70). 
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
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-RECESRYKANAME   PIC X(40).
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
      *
      *    レセ電診療科コードテーブル
           COPY    "CMRECEDENSRYKACD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療内容情報
           COPY  "CPSK1005.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
      *
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
           MOVE    SPA-FREE        TO  SPA-W05
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
           MOVE    SPA-W05         TO  SPA-FREE
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
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W05"                TO  MCP-WINDOW
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
               IF      SPA-WIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
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
           INITIALIZE   SPA-W05-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-W01-KBNCD       TO  SPA-GMN-SRYCD
      *
           INITIALIZE                         SPA-GMN-KOUMOKU
      * 
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"                  TO  SYS-1005-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-1005-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME      TO  SPA-GMN-SRYKANAME
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-GMN-SRYKANAME1
               MOVE    SYS-1005-SRYKANAME2     TO  SPA-GMN-SRYKANAME2
               MOVE    SYS-1005-SRYKANAME3     TO  SPA-GMN-SRYKANAME3
               MOVE    SYS-1005-RECESRYKA      TO  SPA-GMN-RECESRYKA
               PERFORM 5003-RECESRYKA-HENSYU-SEC
               MOVE    WRK-RECESRYKANAME       TO  SPA-GMN-RECESRYKANAME
      *
      *        処置時間外加算等１設定
               MOVE    SYS-1005-JIKANKSN1-40   TO  SPA-GMN-JIKANKSN1-40
               MOVE    SYS-1005-JIKANKSN1-50   TO  SPA-GMN-JIKANKSN1-50
           END-IF     
      *
      *    処置時間外加算等１初期
           IF      SPA-GMN-JIKANKSN1-40    =   SPACE
               MOVE    "0"             TO  SPA-GMN-JIKANKSN1-40
           END-IF
           IF      SPA-GMN-JIKANKSN1-50    =   SPACE
               MOVE    "0"             TO  SPA-GMN-JIKANKSN1-50
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-CUR
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
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *
      *H26.4
      *    処置時間外加算等１
               WHEN    "CLICKED"       ALSO    "JIKANKSN1_40"
                   PERFORM 4801-JIKANKSN140-SET-SEC
      *    手術時間外加算等１
               WHEN    "CLICKED"       ALSO    "JIKANKSN1_50"
                   PERFORM 4801-JIKANKSN150-SET-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処置時間外加算等１処理
      *****************************************************************
       4801-JIKANKSN140-SET-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-JIKANKSN1-40
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN-JIKANKSN1-40
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN-JIKANKSN1-40
           END-EVALUATE
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    11              TO  SPA-GMN-CUR
           END-IF
          .
       4801-JIKANKSN140-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術時間外加算等１処理
      *****************************************************************
       4801-JIKANKSN150-SET-SEC          SECTION.
      *
           EVALUATE    SPA-GMN-JIKANKSN1-50
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN-JIKANKSN1-50
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN-JIKANKSN1-50
           END-EVALUATE
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    99              TO  SPA-GMN-CUR
           END-IF
          .
       4801-JIKANKSN150-SET-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
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
      *
           MOVE    W05-SRYKANAME       TO  SPA-GMN-SRYKANAME
           MOVE    W05-SRYKANAME1      TO  SPA-GMN-SRYKANAME1
           MOVE    W05-SRYKANAME2      TO  SPA-GMN-SRYKANAME2
           MOVE    W05-SRYKANAME3      TO  SPA-GMN-SRYKANAME3
           MOVE    W05-RECESRYKA       TO  SPA-GMN-RECESRYKA-G
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
           PERFORM 4100-SRYKANAME-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-SRYKANAME1-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-SRYKANAME2-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4114-SRYKANAME3-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4115-RECESRYKA-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-SRYKANAME-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名チェック
      *****************************************************************
       4100-SRYKANAME-CHK-SEC       SECTION.
      *
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYKANAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-SRYKANAME
           END-IF
           .
       4100-SRYKANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名１チェック処理
      *****************************************************************
       4112-SRYKANAME1-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYKANAME1  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-SRYKANAME1
           END-IF
      *
           .
       4112-SRYKANAME1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名２チェック処理
      *****************************************************************
       4113-SRYKANAME2-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYKANAME2  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-SRYKANAME2
           END-IF
      *
           . 
       4113-SRYKANAME2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名３チェック処理
      *****************************************************************
       4114-SRYKANAME3-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYKANAME3  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-SRYKANAME3
           END-IF
      *
           .
       4114-SRYKANAME3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電診療科コードチェック処理
      *****************************************************************
       4115-RECESRYKA-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-RECESRYKANAME
      *
           IF      SPA-GMN-RECESRYKA   =   SPACE
               CONTINUE
           ELSE 
               PERFORM 5003-RECESRYKA-HENSYU-SEC
               IF      WRK-RECESRYKANAME =   SPACE
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-RECESRYKANAME
                                           TO  SPA-GMN-RECESRYKANAME
               END-IF
           END-IF
      *
           .
       4115-RECESRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W05"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
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
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"                  TO  SYS-1005-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-1005-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1005-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
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
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"                  TO  SYS-1005-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-1005-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1005-REC
               INITIALIZE                      SYS-1005-REC
               MOVE   "1005"                   TO  SYS-1005-KANRICD
               MOVE    SPA-W01-KBNCD           TO  SYS-1005-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1005-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1005-EDYUKYMD
           MOVE    SPA-GMN-SRYKANAME       TO  SYS-1005-SRYKANAME 
           MOVE    SPA-GMN-SRYKANAME1      TO  SYS-1005-SRYKANAME1 
           MOVE    SPA-GMN-SRYKANAME2      TO  SYS-1005-SRYKANAME2 
           MOVE    SPA-GMN-SRYKANAME3      TO  SYS-1005-SRYKANAME3 
           MOVE    SPA-GMN-RECESRYKA       TO  SYS-1005-RECESRYKA
      *
           MOVE    SPA-GMN-JIKANKSN1-40    TO  SYS-1005-JIKANKSN1-40
           MOVE    SPA-GMN-JIKANKSN1-50    TO  SYS-1005-JIKANKSN1-50
      *
           MOVE    SYS-1005-REC            TO  SYSKANRI-REC
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
           ELSE
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
           END-IF
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               DISPLAY "W05 UPDATE ERR:" SYS-KEY
                  ",MCP-RC:" MCP-RC "@"
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
           MOVE    SPACE               TO  SPA-ERRCD
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SRYCD       =   "00"  OR  "99" )
               MOVE    "0100"          TO  SPA-ERRCD
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    "W05    "           TO  MCP-WINDOW.
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
           MOVE    SPACE                    TO  W05
           INITIALIZE                           W05
      *
           MOVE    SPA-GMN-STYUKYMD         TO  W05-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD         TO  W05-YUKOEDYMD
      * 
           MOVE    SPA-GMN-SRYCD            TO  W05-SRYCD
           MOVE    SPA-GMN-SRYKANAME        TO  W05-SRYKANAME
           MOVE    SPA-GMN-SRYKANAME1       TO  W05-SRYKANAME1
           MOVE    SPA-GMN-SRYKANAME2       TO  W05-SRYKANAME2  
           MOVE    SPA-GMN-SRYKANAME3       TO  W05-SRYKANAME3
           MOVE    SPA-GMN-RECESRYKA-G      TO  W05-CMB-RECESRYKA
           MOVE    TBL-RECEDENSRYKACD-MAX   TO  W05-RECESRYKA-COUNT
           PERFORM VARYING IDX FROM    1 BY 1
                   UNTIL ( IDX > TBL-RECEDENSRYKACD-MAX )
               STRING    TBL-RECEDEN-SRYKACD (IDX)   DELIMITED BY SIZE
                         " "                         DELIMITED BY SIZE
                         TBL-RECEDEN-SRYKANAME (IDX) DELIMITED BY SPACE
               INTO      W05-RECESRYKA-ITEM (IDX)
               END-STRING
           END-PERFORM
      *H26.4
      *    時間外加算等１設定
           IF      SPA-GMN-JIKANKSN1-40    =   "1"
               MOVE    "T"                 TO  W05-JIKANKSN1-40
           ELSE
               MOVE    "F"                 TO  W05-JIKANKSN1-40
           END-IF
           IF      SPA-GMN-JIKANKSN1-50    =   "1"
               MOVE    "T"                 TO  W05-JIKANKSN1-50
           ELSE
               MOVE    "F"                 TO  W05-JIKANKSN1-50
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE      TO  W05-B03-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO    
               MOVE    WIDGET-NORMAL      TO  W05-B03-STATE
           END-IF 
           .
       51O-GSRAUTH-EXT.
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
                   MOVE    "全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE   "科コード“００”および“９９”の登録はで"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE   "きません"           TO  SPA-ERRMSG (41:)
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W05"                TO  SPA-MOTOPG
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
                   MOVE    "診療科目管理情報を登録します"
                                       TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "診療科目管理情報を削除します"
                                       TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W05"                TO  SPA-MOTOPG
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
                WHEN    2
                    MOVE  "SRYKANAME"        TO   MCP-WIDGET
                WHEN    3
                    MOVE  "SRYKANAME1"       TO   MCP-WIDGET
                WHEN    4
                    MOVE  "SRYKANAME2"       TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SRYKANAME3"       TO   MCP-WIDGET
                WHEN    6
                    MOVE  "RECESRYKA"        TO   MCP-WIDGET
                WHEN    10
                    MOVE  "JIKANKSN1_40"     TO   MCP-WIDGET
                WHEN    11
                    MOVE  "JIKANKSN1_50"     TO   MCP-WIDGET
                WHEN    99
                    MOVE  "B12"              TO   MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYKANAME"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SRYKANAME1"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "SRYKANAME2"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "SRYKANAME3"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "RECESRYKA"
      ****         MOVE    99              TO  SPA-GMN-CUR
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "JIKANKSN1_40"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "JIKANKSN1_50"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
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
      *    レセプト診療科名称編集処理
      *****************************************************************
       5003-RECESRYKA-HENSYU-SEC    SECTION.
      *
           IF      SPA-GMN-RECESRYKA   =   SPACE
               MOVE    SPACE           TO  WRK-RECESRYKANAME
           ELSE
               SET     TBL-RECEDENSRYKACD-IDX  TO  1
               SEARCH  TBL-RECEDENSRYKACD-OCC  
                                       VARYING TBL-RECEDENSRYKACD-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-RECESRYKANAME
                   WHEN    SPA-GMN-RECESRYKA   =   TBL-RECEDEN-SRYKACD
                                               (TBL-RECEDENSRYKACD-IDX)
                       MOVE    TBL-RECEDEN-SRYKANAME
                                           (TBL-RECEDENSRYKACD-IDX)
                                               TO  WRK-RECESRYKANAME
               END-SEARCH
           END-IF
      *
           .
       5003-RECESRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
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
