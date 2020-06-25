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
       PROGRAM-ID.             ORCGW97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : ユーザプログラム起動（Ｗ９７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/08/31    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/07/13  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.01.00    NACL-多々納  07/10/02  入院追加
      *  05.00.00    NACL-太田    17/08/10  Ginbee対応
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
       01  SPA-W97.
           03  SPA-W97-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(03).
      *
      *        起動画面
               05  SPA-GMN-GYOUMUMEI-G.
                   07  SPA-GMN-GYOUMUMEI          PIC X(40).
      *
      *        起動画面
               05  SPA-GMN-GYOUMU-TBL.
                   07  SPA-GMN-GYOUMU-OCC   OCCURS   25.
                       09  SPA-GMN-GYOUMUMEIL     PIC X(40).
      *
                       09  SPA-NAI-GYOUMUIDL      PIC X(10).
               05  SPA-GYOUMU-MAX      PIC 9(03).
      *        起動画面初期表示
               05  SPA-GMN-GYOUMUFLG-G.
                   07  SPA-GMN-GYOUMUFLG           PIC X(01).
                   07  SPA-GMN-GF1                 PIC X(01).
                   07  SPA-GMN-GYOUMUFLG-MEI       PIC X(18).
      *        起動画面初期表示
               05  SPA-GMN-GYOUMUFLG-LSTG.
                   07  SPA-GMN-GYOUMUFLG-LST       PIC X(20)
                                                   OCCURS   2.
      *
      *        実行スクリプト
               05  SPA-GMN-SHELL-TBL.
                   07  SPA-GMN-SHELL-OCC   OCCURS   10.
                       09  SPA-GMN-SYORINM       PIC X(30).
                       09  SPA-GMN-SHELL         PIC X(100).
                       09  SPA-GMN-REPOS-NAME    PIC X(128).
                       09  SPA-GMN-PRTNM         PIC X(16).
      *
                       09  SPA-GMN-TYPE-G.
                           11  SPA-GMN-TYPE          PIC X(01).
                           11  SPA-GMN-TYPEF1        PIC X(01).
                           11  SPA-GMN-TYPE-MEI      PIC X(18).
      *
                       09  SPA-GMN-EXECUTE       PIC X(01).
                       09  SPA-GMN-EXECUTE-MEI   PIC X(10).
                       09  SPA-GMN-CHK           PIC X(01).
                   07  SPA-TBL-MAX               PIC 9(04).
      *
               05  SPA-GMN-TYPE-LSTG.
                   07  SPA-GMN-TYPE-LST          PIC X(20)
                                                 OCCURS   2.
                   07  SPA-TYPE-MAX              PIC 9(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-CUSTOM-VISIBLE      PIC X(01).
                   07  SPA-NAI-CUSTOM-CAUTION      PIC X(01).
                   07  SPA-NAI-CUSTOM-ENABLED      PIC X(01).
                   07  SPA-NAI-GYOUMUID            PIC X(10).
                   07  SPA-NAI-KANRICD             PIC X(04).
                   07  SPA-NAI-KBNCD               PIC X(08).
                   07  SPA-NAI-GYOUMUIDX           PIC 9(04).
      *
               05  SPA-9700-TBL-AREA.
                   07  SPA-9700-TBL-OCC      OCCURS    25.
                       09  SPA-9700-GYOUMUID       PIC  X(10).
                       09  SPA-9700-GYOUMU-KANRICD PIC  X(04).
      *                指示画面初期表示
                       09  SPA-9700-GYOUMU-FLG     PIC  X(01).
      *                指示件数
                       09  SPA-9700-GYOUMU-CNT     PIC  9(02).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
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
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-SYORI-CNT       PIC 9(02).
           03  WRK-NEXT-CUR        PIC 9(03).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9700.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9701.INC".
      *    リポジトリ設定
           COPY  "CPSK3103.INC".
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
      *    カナチェック
           COPY  "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W97
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
           MOVE    SPA-W97         TO  SPA-FREE
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
               MOVE    "W97"                TO  MCP-WINDOW
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
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
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
           INITIALIZE   SPA-W97
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               INITIALIZE                          SBC-AREA
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-NAI-CUSTOM-ENABLED
                   MOVE    "1"         TO  SPA-NAI-CUSTOM-VISIBLE
               END-IF
           END-IF
      *
           MOVE    "患者登録  (P02)"   TO  SPA-GMN-GYOUMUMEIL (1)
           MOVE    "P02"               TO  SPA-NAI-GYOUMUIDL  (1)
           MOVE    "受付  (U02)"       TO  SPA-GMN-GYOUMUMEIL (2)
           MOVE    "U02"               TO  SPA-NAI-GYOUMUIDL  (2)
           MOVE    "請求確認  (K03)"   TO  SPA-GMN-GYOUMUMEIL (3)
           MOVE    "K03"               TO  SPA-NAI-GYOUMUIDL  (3)
           MOVE    "入退院登録 (I01)" 
                                       TO  SPA-GMN-GYOUMUMEIL (4)
           MOVE    "I01"               TO  SPA-NAI-GYOUMUIDL  (4)
           MOVE    "入院請求確認 (I04)" 
                                       TO  SPA-GMN-GYOUMUMEIL (5)
           MOVE    "I04"               TO  SPA-NAI-GYOUMUIDL  (5)
           MOVE    "入院診療確認 (K08)"
                                       TO  SPA-GMN-GYOUMUMEIL (6)
           MOVE    "K08"               TO  SPA-NAI-GYOUMUIDL  (6)
      *
           MOVE    "外来中途終了(K02)"
                                       TO  SPA-GMN-GYOUMUMEIL (7)
           MOVE    "K02"               TO  SPA-NAI-GYOUMUIDL  (7)
           MOVE    "入院中途終了(K02N)"
                                       TO  SPA-GMN-GYOUMUMEIL (8)
           MOVE    "K02N"              TO  SPA-NAI-GYOUMUIDL  (8)
      *
           MOVE    8                   TO  SPA-GYOUMU-MAX
      *    業務初期表示
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GYOUMU-MAX
               MOVE    SPA-NAI-GYOUMUIDL(IDX)
                                       TO  SPA-9700-GYOUMUID(IDX)
               MOVE    "0"             TO  SPA-9700-GYOUMU-FLG(IDX)
               MOVE    ZERO            TO  SPA-9700-GYOUMU-CNT(IDX)
           END-PERFORM
      *
           MOVE    "9701"          TO  SPA-9700-GYOUMU-KANRICD (1)
           MOVE    "9702"          TO  SPA-9700-GYOUMU-KANRICD (2)
           MOVE    "9703"          TO  SPA-9700-GYOUMU-KANRICD (3)
           MOVE    "9704"          TO  SPA-9700-GYOUMU-KANRICD (4)
           MOVE    "9705"          TO  SPA-9700-GYOUMU-KANRICD (5)
           MOVE    "9706"          TO  SPA-9700-GYOUMU-KANRICD (6)
           MOVE    "9707"          TO  SPA-9700-GYOUMU-KANRICD (7)
           MOVE    "9708"          TO  SPA-9700-GYOUMU-KANRICD (8)
      *
      *    業務初期表示
           MOVE    SPACE               TO  SYS-9700-REC
           INITIALIZE                      SYS-9700-REC
           MOVE    "9700"              TO  SYS-9700-KANRICD
           MOVE    "1%"                TO  SYS-9700-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9700-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9700-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
           MOVE    SYS-9700-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9700-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GYOUMU-MAX
                   IF      SYS-9700-GYOUMUID(IDX)
                                           =   SPA-9700-GYOUMUID(IDX)
                       MOVE    SYS-9700-GYOUMUID(IDX)
                                           TO  SPA-9700-GYOUMUID(IDX)
                       MOVE    SYS-9700-GYOUMU-FLG(IDX)
                                           TO  SPA-9700-GYOUMU-FLG(IDX)
                       MOVE    SYS-9700-GYOUMU-CNT(IDX)
                                           TO  SPA-9700-GYOUMU-CNT(IDX)
      *                管理コード
                       MOVE    SYS-9700-GYOUMU-KANRICD(IDX)
                                           TO  SPA-9700-GYOUMU-KANRICD
                                                                (IDX)
                   END-IF
               END-PERFORM
      **** ELSE
      *        PERFORM VARYING     IDX     FROM    1   BY  1
      *                UNTIL       IDX     >   SPA-GYOUMU-MAX
      *            MOVE    SPA-NAI-GYOUMUIDL(IDX)
      *                                    TO  SPA-9700-GYOUMUID(IDX)
      *            MOVE    "0"             TO  SPA-9700-GYOUMU-FLG(IDX)
      *            MOVE    ZERO            TO  SPA-9700-GYOUMU-CNT(IDX)
      *        END-PERFORM
      *
      *        MOVE    "9701"          TO  SPA-9700-GYOUMU-KANRICD (1)
      *        MOVE    "9702"          TO  SPA-9700-GYOUMU-KANRICD (2)
      *********MOVE    "9703"          TO  SPA-9700-GYOUMU-KANRICD (3)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    "0 表示しない"      TO  SPA-GMN-GYOUMUFLG-LST (1)
           MOVE    "1 表示する"        TO  SPA-GMN-GYOUMUFLG-LST (2)
      *
           MOVE    "1 画面指示"        TO  SPA-GMN-TYPE-LST (1)
           MOVE    "2 全部実行"        TO  SPA-GMN-TYPE-LST (2)
           MOVE    2                   TO  SPA-TYPE-MAX
      *
           MOVE    10                  TO  SPA-TBL-MAX
      *
           MOVE    SPA-GMN-GYOUMUMEIL (1)  TO  W97-GYOUMUMEI
           PERFORM 4001-GYOUMUMEI-CHK-SEC
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
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
      *    クリア
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM 420-CLREA-SEC
      *    登録
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    実行
               WHEN    "CLICKED"      ALSO    "EXECUTE01"
               WHEN    "CLICKED"      ALSO    "EXECUTE02"
               WHEN    "CLICKED"      ALSO    "EXECUTE03"
               WHEN    "CLICKED"      ALSO    "EXECUTE04"
               WHEN    "CLICKED"      ALSO    "EXECUTE05"
               WHEN    "CLICKED"      ALSO    "EXECUTE06"
               WHEN    "CLICKED"      ALSO    "EXECUTE07"
               WHEN    "CLICKED"      ALSO    "EXECUTE08"
               WHEN    "CLICKED"      ALSO    "EXECUTE09"
               WHEN    "CLICKED"      ALSO    "EXECUTE10"
                   PERFORM 410-EXECUTE-CHK-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        業務選択
               WHEN    "ACTIVATE"      ALSO    "GYOUMUMEI"
                   PERFORM 4001-GYOUMUMEI-CHK-SEC
      *        画面表示選択
               WHEN    "ACTIVATE"      ALSO    "GYOUMUFLG"
                   PERFORM 4002-GYOUMUFLG-CHK-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    業務選択チェック処理
      *****************************************************************
       4001-GYOUMUMEI-CHK-SEC       SECTION.
      *
           MOVE    W97-GYOUMUMEI       TO  SPA-GMN-GYOUMUMEI-G
      *
           MOVE    SPACE               TO  SPA-NAI-GYOUMUID
           MOVE    SPACE               TO  SPA-NAI-KANRICD
           MOVE    ZERO                TO  SPA-NAI-GYOUMUIDX
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GYOUMU-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-GYOUMUMEI   =   SPA-GMN-GYOUMUMEIL(IDX)
                   MOVE    SPA-NAI-GYOUMUIDL (IDX)
                                           TO  SPA-NAI-GYOUMUID
                   MOVE    SPA-9700-GYOUMU-KANRICD (IDX)
                                           TO  SPA-NAI-KANRICD
                   MOVE    IDX             TO  SPA-NAI-GYOUMUIDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               PERFORM 40011-GYOUMUGMN-SET-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4001-GYOUMUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    起動画面毎の画面表示処理
      *****************************************************************
       40011-GYOUMUGMN-SET-SEC       SECTION.
      *
      *    初期画面表示
           MOVE    SPA-9700-GYOUMU-FLG (SPA-NAI-GYOUMUIDX)
                                       TO  SPA-GMN-GYOUMUFLG
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   2   )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-GYOUMUFLG   =   SPA-GMN-GYOUMUFLG-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-GYOUMUFLG-LST (IDX)
                                           TO  SPA-GMN-GYOUMUFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           INITIALIZE                      SPA-GMN-SHELL-TBL
           MOVE    SPACE               TO  SPA-NAI-CUSTOM-CAUTION
           MOVE    SPA-NAI-CUSTOM-ENABLED
                                       TO  SPA-NAI-CUSTOM-VISIBLE
      *    業務初期表示
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    SPA-NAI-KANRICD     TO  SYS-9701-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9701-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     (FLG-SYSKANRI     =   1 )
                          OR (IDX              >=  10 )
               MOVE    MCPDATA-REC         TO  SYS-9701-REC
               ADD     1                   TO  IDX
               MOVE    SYS-9701-SYORINM    TO  SPA-GMN-SYORINM (IDX)
               MOVE    SYS-9701-SHELL      TO  SPA-GMN-SHELL (IDX)
               MOVE    SYS-9701-PRTNM      TO  SPA-GMN-PRTNM (IDX)
               MOVE    SYS-9701-EXECUTE-YTPE
                                           TO  SPA-GMN-TYPE  (IDX)
               MOVE    SYS-9701-EXECUTE-SW
                                           TO  SPA-GMN-EXECUTE  (IDX)
      *
               MOVE    SPACE               TO  SYS-3103-REC
               INITIALIZE                      SYS-3103-REC
               MOVE    "3103"              TO  SYS-3103-KANRICD
               MOVE    SYS-9701-KANRICD    TO  SYS-3103-KBNCD
               MOVE    "0"                 TO  SYS-3103-KBNCD(5:1)
               MOVE    SYS-9701-KBNCD(1:2) TO  SYS-3103-KBNCD(6:2)
               MOVE    SPA-SYSYMD          TO  SYS-3103-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-3103-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
               MOVE    SYS-3103-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 990-SYSKANRI-READ-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    MCPDATA-REC TO  SYS-3103-REC
                       MOVE    SYS-3103-REPOS-NAME
                                           TO  SPA-GMN-REPOS-NAME (IDX)
                       IF    ( SPA-MW      =   SPA-GINBEE )
                        AND  ( SYS-3103-REPOS-NAME
                                          NOT =   SPACE )
                           MOVE    "1" TO  SPA-NAI-CUSTOM-VISIBLE
                           IF    ( SPA-NAI-CUSTOM-ENABLED
                                              =   SPACE )
                               MOVE  "1"  TO  SPA-NAI-CUSTOM-CAUTION
                           END-IF
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    10                  TO  SPA-TBL-MAX
      *
      *    実行可否初期設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               IF      SPA-GMN-EXECUTE (IDX)   =   "1"
                   MOVE    "T"             TO  SPA-GMN-CHK (IDX)
               ELSE
                   MOVE    "F"             TO  SPA-GMN-CHK (IDX)
               END-IF
           END-PERFORM
      *    初期設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               IF      SPA-GMN-TYPE (IDX)  NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-TYPE-MAX
                       IF      SPA-GMN-TYPE (IDX)  =   SPA-GMN-TYPE-LST
                                                         (IDY) (1:1)
                            MOVE    SPA-GMN-TYPE-LST(IDY)
                                               TO  SPA-GMN-TYPE-G(IDX)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       40011-GYOUMUGMN-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面表示チェック処理
      *****************************************************************
       4002-GYOUMUFLG-CHK-SEC       SECTION.
      *
           MOVE    W97-GYOUMUFLG       TO  SPA-GMN-GYOUMUFLG-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   2   )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-GYOUMUFLG   =   SPA-GMN-GYOUMUFLG-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-GYOUMUFLG-LST (IDX)
                                           TO  SPA-GMN-GYOUMUFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    301                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       4002-GYOUMUFLG-CHK-EXT.
           EXIT.
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               MOVE    W97-SYORINM (IDX)   TO  SPA-GMN-SYORINM (IDX)
               MOVE    W97-SHELL (IDX)     TO  SPA-GMN-SHELL (IDX)
               MOVE    W97-REPOS-NAME (IDX) TO SPA-GMN-REPOS-NAME (IDX)
               MOVE    W97-PRTNM (IDX)     TO  SPA-GMN-PRTNM (IDX)
               MOVE    W97-TYPE  (IDX)     TO  SPA-GMN-TYPE-G (IDX)
           END-PERFORM
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TBL-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *        処理名が空白の時、行削除
               IF      SPA-GMN-SYORINM (IDX)   =   SPACE
                   INITIALIZE                  SPA-GMN-SHELL-OCC (IDX)
                   MOVE    "F"             TO  SPA-GMN-CHK (IDX)
               ELSE
      *            処理名
                   PERFORM 41021-SYORINM-CHK-SEC
      *            リポジトリ名
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( SPA-MW      =   SPA-GINBEE )
                        AND  ( SPA-NAI-CUSTOM-VISIBLE  =   "1" )
                           PERFORM 41021-REPOS-NAME-CHK-SEC
                       END-IF
                   END-IF
      *            タイプ名
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 41021-TYPE-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理・帳票名チェック処理
      *****************************************************************
       41021-SYORINM-CHK-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SYORINM (IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    30                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-SYORINM (IDX)
      *
           .
       41021-SYORINM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    リポジトリ名チェック処理
      *****************************************************************
       41021-REPOS-NAME-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-REPOS-NAME (IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    128                 TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-REPOS-NAME (IDX)
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   2     )
               MOVE    "0006"          TO  SPA-ERRCD
               COMPUTE SPA-GMN-CUR     =   800 +   IDX
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-REPOS-NAME (IDX)
               END-IF
           END-IF
      *
           .
       41021-REPOS-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実行形式チェック処理
      *****************************************************************
       41021-TYPE-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-TYPE-MAX ) OR
                              (FLG-OK  =   1        )
               IF      SPA-GMN-TYPE (IDX)  =   SPA-GMN-TYPE-LST
                                                         (IDY) (1:1)
                    MOVE    SPA-GMN-TYPE-LST(IDY)
                                               TO  SPA-GMN-TYPE-G(IDX)
                    MOVE    1                  TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        未選択は１
               MOVE    SPA-GMN-TYPE-LST(1)     TO  SPA-GMN-TYPE-G(IDX)
           END-IF
      *
           .
       41021-EXECUTE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    実行チェック処理
      *****************************************************************
       410-EXECUTE-CHK-SEC             SECTION.
      *
      *    他の入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           IF      MCP-WIDGET(8:2)     NUMERIC
               MOVE    MCP-WIDGET(8:2)     TO  IDX2
               MOVE    IDX2                TO  IDX
      *
               IF      SPA-GMN-SHELL (IDX) NOT =   SPACE
                   EVALUATE    SPA-GMN-CHK (IDX)
                   WHEN    "T"
                       MOVE    "F"         TO  SPA-GMN-CHK (IDX)
                   WHEN    "F"
                       MOVE    "T"         TO  SPA-GMN-CHK (IDX)
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       410-EXECUTE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W97"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
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
      *    クリア　処理
      *****************************************************************
       420-CLREA-SEC                  SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       420-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    更新前のチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-KOUSIN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       更新前のチェック処理
      *****************************************************************
       420-KOUSIN-CHK-SEC           SECTION.
      *
           IF     (SPA-NAI-GYOUMUIDX   =   ZERO )  OR
                  (SPA-NAI-KANRICD     =   SPACE ) OR
                  (SPA-NAI-GYOUMUID    =   SPACE )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *    画面表示選択
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-GYOUMUFLG-CHK-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TBL-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SYORINM(IDX)    NOT =   SPACE
                   IF      SPA-GMN-SHELL (IDX)     =   SPACE
                       MOVE    "0004"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =   300
                                               +   IDX
                   END-IF
               END-IF
           END-PERFORM
           .
       420-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *    処理・業務データ削除
           PERFORM 4900-DELETE-SEC
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  WRK-SYORI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TBL-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SYORINM(IDX)    =   SPACE
                   CONTINUE
               ELSE
      *            更新処理
                   PERFORM 4901-KOUSIN-SYORI-SEC
               END-IF
           END-PERFORM
      *    指示画面表示更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4902-KOUSIN-9700-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 40011-GYOUMUGMN-SET-SEC
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       指示画面表示更新処理
      *****************************************************************
       4902-KOUSIN-9700-SEC           SECTION.
      *
           MOVE    SPA-GMN-GYOUMUFLG   TO  SPA-9700-GYOUMU-FLG
                                                 (SPA-NAI-GYOUMUIDX)
      *    実行処理数
           MOVE    WRK-SYORI-CNT       TO  SPA-9700-GYOUMU-CNT
                                                 (SPA-NAI-GYOUMUIDX)
      *
           MOVE    SPACE               TO  SYS-9700-REC
           INITIALIZE                      SYS-9700-REC
           MOVE    "9700"              TO  SYS-9700-KANRICD
           MOVE    "1%"                TO  SYS-9700-KBNCD
           MOVE    "00000000"          TO  SYS-9700-STYUKYMD
           MOVE    "99999999"          TO  SYS-9700-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
           MOVE    SYS-9700-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9700-REC
           ELSE
               MOVE    SPACE               TO  SYS-9700-REC
               INITIALIZE                      SYS-9700-REC
               MOVE    "9700"              TO  SYS-9700-KANRICD
               MOVE    "1"                 TO  SYS-9700-KBNCD
               MOVE    "00000000"          TO  SYS-9700-STYUKYMD
               MOVE    "99999999"          TO  SYS-9700-EDYUKYMD
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GYOUMU-MAX
               MOVE    SPA-9700-GYOUMUID(IDX)
                                       TO  SYS-9700-GYOUMUID(IDX)
               MOVE    SPA-9700-GYOUMU-KANRICD (IDX)
                                       TO  SYS-9700-GYOUMU-KANRICD(IDX)
               MOVE    SPA-9700-GYOUMU-FLG(IDX)
                                       TO  SYS-9700-GYOUMU-FLG(IDX)
               MOVE    SPA-9700-GYOUMU-CNT(IDX)
                                       TO  SYS-9700-GYOUMU-CNT(IDX)
           END-PERFORM
      *
           MOVE    SMCNDATE-YMD            TO  SYS-9700-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-9700-UPHMS
      *
           IF      FLG-SYSKANRI        =   ZERO
      *        更新
               MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
               MOVE    SYS-9700-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1002"              TO  SPA-ERRCD
                   DISPLAY "W97 DB UPD ERR KEY:" SYS-9700-KEY 
               END-IF
           ELSE
      *    新規
               MOVE    SMCNDATE-YMD            TO  SYS-9700-CREYMD
               MOVE    SPA-HOSPNUM             TO  SYS-9700-HOSPNUM
               MOVE    SYS-9700-REC            TO  SYSKANRI-REC
      *
               PERFORM 4901-INSERT-SYORI-SEC
           END-IF
           .
       4902-KOUSIN-9700-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理・業務データ削除処理
      *****************************************************************
       4900-DELETE-SEC           SECTION.
      *
      *    すべてを削除する
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    SPA-NAI-KANRICD     TO  SYS-9701-KANRICD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "W97 DB DEL ERR SYSKANRI-KEY =>" SYS-9701-KEY 
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPACE               TO  SYS-3103-REC
               INITIALIZE                      SYS-3103-REC
               MOVE    "3103"              TO  SYS-3103-KANRICD
               MOVE    SPA-NAI-KANRICD     TO  SYS-3103-KBNCD
               MOVE    "%"                 TO  SYS-3103-KBNCD(5:1)
               MOVE    SPA-SYSYMD          TO  SYS-3103-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-3103-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
               MOVE    SYS-3103-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "del3"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1004"              TO  SPA-ERRCD
                   DISPLAY "W97 DB DEL ERR SYSKANRI-KEY =>" SYS-3103-KEY
               END-IF
           END-IF
      *
           .
       4900-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4901-KOUSIN-SYORI-SEC           SECTION.
      *
           IF      SPA-GMN-CHK (IDX)   =   "T"
               MOVE    "1"             TO  SPA-GMN-EXECUTE (IDX)
               ADD     1               TO  WRK-SYORI-CNT
           ELSE
               MOVE    "0"             TO  SPA-GMN-EXECUTE (IDX)
           END-IF
      *
           ADD     1                   TO  IDX2
      *
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    SPA-NAI-KANRICD     TO  SYS-9701-KANRICD
           MOVE    IDX2                TO  SYS-9701-KBNCD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-GMN-SYORINM (IDX)   TO  SYS-9701-SYORINM
           MOVE    SPA-GMN-SHELL (IDX)     TO  SYS-9701-SHELL
           MOVE    SPA-GMN-PRTNM (IDX)     TO  SYS-9701-PRTNM
           MOVE    SPA-GMN-EXECUTE(IDX)    TO  SYS-9701-EXECUTE-SW
           MOVE    SPA-GMN-TYPE   (IDX)    TO  SYS-9701-EXECUTE-YTPE
      *
           MOVE    SMCNDATE-YMD            TO  SYS-9701-CREYMD
           MOVE    SMCNDATE-YMD            TO  SYS-9701-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-9701-UPHMS
      *
           MOVE    SPA-HOSPNUM             TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC            TO  SYSKANRI-REC
      *    新規
           PERFORM 4901-INSERT-SYORI-SEC
      *
           IF    ( SPA-GMN-REPOS-NAME (IDX)    NOT =   SPACE )
               MOVE    SPACE               TO  SYS-3103-REC
               INITIALIZE                      SYS-3103-REC
               MOVE    "3103"              TO  SYS-3103-KANRICD
               MOVE    SYS-9701-KANRICD    TO  SYS-3103-KBNCD
               MOVE    "0"                 TO  SYS-3103-KBNCD(5:1)
               MOVE    SYS-9701-KBNCD(1:2) TO  SYS-3103-KBNCD(6:2)
               MOVE    "00000000"          TO  SYS-3103-STYUKYMD
               MOVE    "99999999"          TO  SYS-3103-EDYUKYMD
      *
               MOVE    SPA-GMN-REPOS-NAME (IDX)
                                           TO  SYS-3103-REPOS-NAME
      *
               MOVE    SMCNDATE-YMD        TO  SYS-3103-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-3103-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-3103-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
               MOVE    SYS-3103-REC        TO  SYSKANRI-REC
      *        新規
               PERFORM 4901-INSERT-SYORI-SEC
           END-IF
      *
           .
       4901-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *       新規登録処理
      *****************************************************************
       4901-INSERT-SYORI-SEC           SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W97 DB INS ERR KEY:" SYS-KEY
           END-IF
           .
       4901-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *       更新処理
      *****************************************************************
       4902-UPDATE-SYORI-SEC           SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W97 DB UPD ERR KEY:" SYS-9700-KEY 
           END-IF
           .
       4902-UPDATE-SYORI-EXT.
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
           MOVE    "W97    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W97
      *
           INITIALIZE                      W97
           MOVE    SPA-GMN-GYOUMUMEI-G TO  W97-GYOUMUMEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GYOUMU-MAX
               MOVE    SPA-GMN-GYOUMUMEIL(IDX)
                                           TO  W97-GYOUMUMEI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GYOUMU-MAX      TO  W97-GYOUMUMEI-COUNT
      *
           MOVE    SPA-GMN-GYOUMUFLG-G TO  W97-GYOUMUFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-GYOUMUFLG-LST(IDX)
                                           TO  W97-GYOUMUFLG-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W97-GYOUMUFLG-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               MOVE    SPA-GMN-SYORINM  (IDX)
                                           TO  W97-SYORINM  (IDX)
               MOVE    SPA-GMN-PRTNM   (IDX)
                                           TO  W97-PRTNM (IDX)
               MOVE    SPA-GMN-SHELL  (IDX)
                                           TO  W97-SHELL (IDX)
               MOVE    SPA-GMN-REPOS-NAME  (IDX)
                                           TO  W97-REPOS-NAME (IDX)
               MOVE    SPA-GMN-CHK    (IDX)
                                           TO  W97-EXECUTE-VALUE(IDX)
      *        IF      SPA-GMN-CHK    (IDX)    =   "T"
                   MOVE    "実行する"      TO  W97-EXECUTE-LABEL(IDX)
      *        ELSE
      *            MOVE    "実行しない"    TO  W97-EXECUTE-LABEL(IDX)
      *        END-IF
      *
               MOVE    SPA-GMN-TYPE-G (IDX)
                                           TO  W97-TYPE (IDX)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-TYPE-MAX
                   MOVE    SPA-GMN-TYPE-LST (IDY)
                                           TO  W97-TYPE-ITEM (IDX IDY)
               END-PERFORM
               MOVE    SPA-TYPE-MAX        TO  W97-TYPE-COUNT (IDX)
      *
               MOVE    "F"             TO  W97-REPOS-NAME-VISIBLE (IDX)
               IF    ( SPA-MW                  =   SPA-GINBEE )
                AND  ( SPA-NAI-CUSTOM-VISIBLE  =   "1" )
                       MOVE    "T"     TO  W97-REPOS-NAME-VISIBLE (IDX)
                       IF    ( IDX     =   1 )
                           MOVE    "リポジトリ名"
                                       TO  W97-LBLREPOS-NAME (IDX)
                       END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    "red"                   TO  W97-LBLMSG01-STYLE
           IF    ( SPA-MW                  =   SPA-GINBEE )
               IF    ( SPA-NAI-CUSTOM-CAUTION  =   "1" )
                   MOVE    SPACE           TO  W97-LBLMSG01
                   STRING  "リポジトリ名の設定がある処理・帳票"
                           "を実行するためには、課金が必要です"
                   INTO  W97-LBLMSG01
                   END-STRING
               END-IF
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
              IF      IDX              <=  SPA-TBL-MAX
                   MOVE    WIDGET-NORMAL       TO  W97-EXECUTE-STATE
                                                                  (IDX)
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  W97-EXECUTE-STATE
                                                                  (IDX)
               END-IF
           END-PERFORM
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "起動画面選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "指示画面初期表示エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "実行スクリプトは必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "起動画面を選択して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "リポジトリ名は半角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "登録ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
               WHEN    "1004"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W97"                TO  SPA-MOTOPG
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
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "ユーザプログラム起動情報を登録します。"
                                                     TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W97"                TO  SPA-MOTOPG
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
           IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           IF    ( SPA-GMN-CUR          >=  801 )
            AND  ( SPA-GMN-CUR          <=  810 )
               COMPUTE WRK-NEXT-CUR =   SPA-GMN-CUR - 800 + 500
               IF    ( SPA-MW           =   SPA-GINBEE )
                AND  ( SPA-NAI-CUSTOM-VISIBLE  =   "1" )
                   CONTINUE
               ELSE
                   MOVE    WRK-NEXT-CUR    TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE     "GYOUMUMEI"       TO  MCP-WIDGET
                WHEN    2
                    MOVE     "GYOUMUFLG"       TO  MCP-WIDGET
                WHEN    301  THRU  310
                    MOVE     SPA-GMN-CUR(2:2)  TO  IDX2
                    MOVE     "SYORINM"         TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(8:2)
                WHEN    401  THRU  410
                    MOVE     SPA-GMN-CUR(2:2)  TO  IDX2
                    MOVE     "SHELL"           TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(6:2)
                WHEN    501  THRU  510
                    MOVE     SPA-GMN-CUR(2:2)  TO  IDX2
                    MOVE     "PRTNM"           TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(6:2)
                WHEN    601  THRU  610
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "TYPE"            TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(5:2)
                WHEN    701  THRU  710
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "EXECUTE"         TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(8:2)
                WHEN    801  THRU  810
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "REPOS_NAME"      TO  MCP-WIDGET
                    MOVE     IDX2              TO  MCP-WIDGET(11:2)
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
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
           EVALUATE    TRUE
               WHEN    WRK-MCP-WIDGET          =   "GYOMUMEI"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    WRK-MCP-WIDGET          =   "GYOMUFLG"
                   MOVE    301                 TO  SPA-GMN-CUR
      *
               WHEN    WRK-MCP-WIDGET(1:7)     =   "SYORINM"
                   MOVE    WRK-MCP-WIDGET(8:2)     TO  IDX2
                   COMPUTE SPA-GMN-CUR         =   IDX2
                                               +   400
      *
               WHEN    WRK-MCP-WIDGET(1:5)     =   "SHELL"
                   MOVE    WRK-MCP-WIDGET(6:2)     TO  IDX2
                   COMPUTE SPA-GMN-CUR         =   IDX2
                                               +   800
      *
               WHEN    WRK-MCP-WIDGET(1:5)     =   "PRTNM"
                   MOVE    WRK-MCP-WIDGET(6:2)     TO  IDX2
                   COMPUTE SPA-GMN-CUR         =   IDX2
                                               +   600
      *
               WHEN    WRK-MCP-WIDGET(1:4)     =   "TYPE"
                   MOVE    WRK-MCP-WIDGET(5:2)     TO  IDX2
                   COMPUTE SPA-GMN-CUR         =   IDX2
                                               +   700
      *
               WHEN    WRK-MCP-WIDGET(1:10)    =   "REPOS_NAME"
                   MOVE    WRK-MCP-WIDGET(11:2)     TO  IDX2
                   COMPUTE SPA-GMN-CUR         =   IDX2
                                               +   500
      *
               WHEN    WRK-MCP-WIDGET(1:7)     =   "EXECUTE"
                   MOVE    WRK-MCP-WIDGET(8:2)     TO  IDX2
                   IF      IDX2                <   SPA-TBL-MAX
                        COMPUTE SPA-GMN-CUR    =   IDX2
                                               +   301
                   ELSE
                        MOVE    99             TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
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
       990-DBCLOSE-SEC           SECTION.
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
