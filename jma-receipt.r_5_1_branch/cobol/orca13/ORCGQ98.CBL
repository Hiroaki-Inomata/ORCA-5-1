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
       PROGRAM-ID.                     ORCGQ98.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 周辺住所画面（Ｑ９８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/30    MCC−太田　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.02     MCC-太田     02/06/26  ジョブ管理ＤＢ追加
      *                                     画面ＱＩＤ１を
      *                                     Ｑ１００に名称変更
      *                                     新たにＱＩＤ１を追加
      * 03.05.00     NACL-太田    07.05.23  グループ診療対応
      * 04.05.00     NACL-多々納  09.12.21  住所検索を郵便番号＋連番
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
      *    画面スタイル
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-EOF                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ADRS                            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SYSKANRI                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ZZZ9                            PIC ZZZ9.
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET                          PIC X(20).
      *
           03  WRK-WIDGET-R1   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R1-DAMMY1            PIC X(02).
               05  WRK-WIDGET1                     PIC 9(01).
               05  WRK-WIDGET-R1-DAMMY2            PIC X(17).
      *
           03  WRK-WIDGET-R2   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R2-DAMMY1            PIC X(03).
               05  WRK-WIDGET2                     PIC 9(01).
               05  WRK-WIDGET-R2-DAMMY2            PIC X(16).
      *
           03  WRK-WIDGET-R3   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R3-DAMMY1            PIC X(04).
               05  WRK-WIDGET3                     PIC 9(01).
               05  WRK-WIDGET-R3-DAMMY2            PIC X(15).
      *
           03  WRK-WIDGET-R4   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R4-DAMMY1            PIC X(05).
               05  WRK-WIDGET4                     PIC 9(01).
               05  WRK-WIDGET-R4-DAMMY2            PIC X(14).
      *
           03  WRK-WIDGET-R5   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R5-DAMMY1            PIC X(06).
               05  WRK-WIDGET5                     PIC 9(01).
               05  WRK-WIDGET-R5-DAMMY2            PIC X(13).
      *
           03  WRK-WIDGET-R6   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R6-DAMMY1            PIC X(07).
               05  WRK-WIDGET6                     PIC 9(01).
               05  WRK-WIDGET-R6-DAMMY2            PIC X(12).
      *
           03  WRK-WIDGET-R7   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R7-DAMMY1            PIC X(08).
               05  WRK-WIDGET7                     PIC 9(01).
               05  WRK-WIDGET-R7-DAMMY2            PIC X(11).
      *
           03  WRK-WIDGET-R8   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R8-DAMMY1            PIC X(08).
               05  WRK-WIDGET8                     PIC 9(02).
               05  WRK-WIDGET-R8-DAMMY2            PIC X(10).
      *
      *    固定値領域
      *01  CONST-AREA.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（周辺町域情報）
           COPY    "CPSK1015.INC".
      *
      *    住所マスター
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
      *    エラー画面より
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           MOVE    SPACE           TO  SPA-Q98
           INITIALIZE                  SPA-Q98
      *
           MOVE    SPACE           TO  SPA-Q01-Q98-JYUSHO
      *
      *    住所編集処理
           PERFORM 3101-ADRS-EDIT-SEC
      *
      *    カーソルキー制御項目
           MOVE   1                TO  SPA-GMN-Q98-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       3101-ADRS-EDIT-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-Q98-ADRS-TBL
                                   SPA-GMN-Q98-SYSKANRI-AREA
      *
      *    周辺住所検索処理（システム管理マスタより検索）
           INITIALIZE              SYS-1015-REC
           MOVE  "1015"            TO  SYS-1015-KANRICD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
      *    システム管理マスタ読み飛ばし処理
           PERFORM 910-SYSKANRI-READ-SKIP-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( FLG-SYSKANRI    =   1 )
                OR   ( IDX1            >   60 )
      *
               COMPUTE CNT-SYSKANRI    =   CNT-SYSKANRI    +   1
      *
               MOVE    SYS-1015-KBNCD
                                   TO SPA-GMN-Q98-TADRS (IDX1) (1:3)
               MOVE    SYS-1015-TOWNNAMEDSP
                                   TO SPA-GMN-Q98-TADRS (IDX1) (5:16)
      *
               MOVE    SYS-1015-KBNCD     TO SPA-GMN-Q98-TKBNCD (IDX1)
               MOVE    SYS-1015-LPUBCD    TO SPA-GMN-Q98-TLPUBCD(IDX1)
               MOVE    SYS-1015-POST      TO SPA-GMN-Q98-TPOST  (IDX1)
               MOVE    SYS-1015-RENNUM    TO SPA-GMN-Q98-TRENNUM(IDX1)
      *
      *        住所読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    FLG-SYSKANRI        TO  SPA-GMN-Q98-FLG-SYSKANRI
      *
           .
       3101-ADRS-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 210-CLEAR
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 210-MAE-PAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 210-TUGI-PAGE-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI
      *    周辺住所
               WHEN    "CLICKED"       ALSO    "TADRSCHK01"
               WHEN    "CLICKED"       ALSO    "TADRSCHK02"
               WHEN    "CLICKED"       ALSO    "TADRSCHK03"
               WHEN    "CLICKED"       ALSO    "TADRSCHK04"
               WHEN    "CLICKED"       ALSO    "TADRSCHK05"
               WHEN    "CLICKED"       ALSO    "TADRSCHK06"
               WHEN    "CLICKED"       ALSO    "TADRSCHK07"
               WHEN    "CLICKED"       ALSO    "TADRSCHK08"
               WHEN    "CLICKED"       ALSO    "TADRSCHK09"
               WHEN    "CLICKED"       ALSO    "TADRSCHK10"
               WHEN    "CLICKED"       ALSO    "TADRSCHK11"
               WHEN    "CLICKED"       ALSO    "TADRSCHK12"
               WHEN    "CLICKED"       ALSO    "TADRSCHK13"
               WHEN    "CLICKED"       ALSO    "TADRSCHK14"
               WHEN    "CLICKED"       ALSO    "TADRSCHK15"
               WHEN    "CLICKED"       ALSO    "TADRSCHK16"
               WHEN    "CLICKED"       ALSO    "TADRSCHK17"
               WHEN    "CLICKED"       ALSO    "TADRSCHK18"
               WHEN    "CLICKED"       ALSO    "TADRSCHK19"
               WHEN    "CLICKED"       ALSO    "TADRSCHK20"
               WHEN    "CLICKED"       ALSO    "TADRSCHK21"
               WHEN    "CLICKED"       ALSO    "TADRSCHK22"
               WHEN    "CLICKED"       ALSO    "TADRSCHK23"
               WHEN    "CLICKED"       ALSO    "TADRSCHK24"
               WHEN    "CLICKED"       ALSO    "TADRSCHK25"
               WHEN    "CLICKED"       ALSO    "TADRSCHK26"
               WHEN    "CLICKED"       ALSO    "TADRSCHK27"
               WHEN    "CLICKED"       ALSO    "TADRSCHK28"
               WHEN    "CLICKED"       ALSO    "TADRSCHK29"
               WHEN    "CLICKED"       ALSO    "TADRSCHK30"
               WHEN    "CLICKED"       ALSO    "TADRSCHK31"
               WHEN    "CLICKED"       ALSO    "TADRSCHK32"
               WHEN    "CLICKED"       ALSO    "TADRSCHK33"
               WHEN    "CLICKED"       ALSO    "TADRSCHK34"
               WHEN    "CLICKED"       ALSO    "TADRSCHK35"
               WHEN    "CLICKED"       ALSO    "TADRSCHK36"
               WHEN    "CLICKED"       ALSO    "TADRSCHK37"
               WHEN    "CLICKED"       ALSO    "TADRSCHK38"
               WHEN    "CLICKED"       ALSO    "TADRSCHK39"
               WHEN    "CLICKED"       ALSO    "TADRSCHK40"
               WHEN    "CLICKED"       ALSO    "TADRSCHK41"
               WHEN    "CLICKED"       ALSO    "TADRSCHK42"
               WHEN    "CLICKED"       ALSO    "TADRSCHK43"
               WHEN    "CLICKED"       ALSO    "TADRSCHK44"
               WHEN    "CLICKED"       ALSO    "TADRSCHK45"
               WHEN    "CLICKED"       ALSO    "TADRSCHK46"
               WHEN    "CLICKED"       ALSO    "TADRSCHK47"
               WHEN    "CLICKED"       ALSO    "TADRSCHK48"
               WHEN    "CLICKED"       ALSO    "TADRSCHK49"
               WHEN    "CLICKED"       ALSO    "TADRSCHK50"
               WHEN    "CLICKED"       ALSO    "TADRSCHK51"
               WHEN    "CLICKED"       ALSO    "TADRSCHK52"
               WHEN    "CLICKED"       ALSO    "TADRSCHK53"
               WHEN    "CLICKED"       ALSO    "TADRSCHK54"
               WHEN    "CLICKED"       ALSO    "TADRSCHK55"
               WHEN    "CLICKED"       ALSO    "TADRSCHK56"
               WHEN    "CLICKED"       ALSO    "TADRSCHK57"
               WHEN    "CLICKED"       ALSO    "TADRSCHK58"
               WHEN    "CLICKED"       ALSO    "TADRSCHK59"
               WHEN    "CLICKED"       ALSO    "TADRSCHK60"
                   MOVE    MCP-WIDGET   TO  WRK-WIDGET
                   PERFORM 490-JYUSHO-SET-SEC
               END-EVALUATE
      *
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    選択番号チェック
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   MOVE    MCP-WIDGET  TO  WRK-WIDGET
                   PERFORM 4100-SELNUM-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-Q98-CUR
      *
           MOVE    Q98-SELNUM          TO  SPA-GMN-Q98-SELNUM
      *
      *    未入力時はチェックを行わない
           IF  ( SPA-GMN-Q98-SELNUM    =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           SET     SPA-Q98-TBLIDX1     TO  1
      *
           SEARCH  SPA-GMN-Q98-SYSKANRI-OCC
                                       VARYING SPA-Q98-TBLIDX1
           AT  END
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           WHEN
               ( SPA-GMN-Q98-TKBNCD (SPA-Q98-TBLIDX1)
                                       =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           WHEN
               ( SPA-GMN-Q98-TKBNCD (SPA-Q98-TBLIDX1) 
                                       =   SPA-GMN-Q98-SELNUM )
      *
               SET    IDX1             TO   SPA-Q98-TBLIDX1
      *
      *        確定住所編集処理
               PERFORM 4100-KAKUTEI-JYUSHO-EDIT-SEC
           END-SEARCH
      *
      *
           MOVE    1                   TO  SPA-GMN-Q98-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    確定住所編集処理
      *****************************************************************
       4100-KAKUTEI-JYUSHO-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-Q98-POST
                                   SPA-GMN-Q98-ADRS
      *
      *    住所マスタ検索
           INITIALIZE                            ADRS-REC
           MOVE  SPA-HOSPNUM                 TO  ADRS-HOSPNUM
      **** MOVE  SPA-GMN-Q98-TLPUBCD(IDX1)   TO  ADRS-LPUBCD
           MOVE  SPA-GMN-Q98-TPOST  (IDX1)   TO  ADRS-POST
           MOVE  SPA-GMN-Q98-TRENNUM(IDX1)   TO  ADRS-EDANUM
      *
           PERFORM 900-ADRS-KEY-SEL-SEC
           IF  ( FLG-ADRS      =   ZERO )
               MOVE  ADRS-POST           TO  SPA-GMN-Q98-POST
               MOVE  ADRS-EDITADRS-NAME  TO  SPA-GMN-Q98-ADRS
           END-IF
      *
           .
       4100-KAKUTEI-JYUSHO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q98"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
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
       210-CLEAR                   SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-Q98-SELNUM
      *
           MOVE    SPACE               TO  SPA-GMN-Q98-POST
           MOVE    SPACE               TO  SPA-GMN-Q98-ADRS
      *
           .
       210-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       210-MAE-PAGE-SEC                SECTION.
      *
           IF  ( SPA-GMN-Q98-SKIP-KNS          >   0 )
               COMPUTE SPA-GMN-Q98-SKIP-KNS    =   SPA-GMN-Q98-SKIP-KNS
                                               -   60
               PERFORM 3101-ADRS-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q98-SELNUM
           END-IF
      *
           .
       210-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       210-TUGI-PAGE-SEC               SECTION.
      *
           IF  ( SPA-GMN-Q98-FLG-SYSKANRI      =   0 )
               COMPUTE SPA-GMN-Q98-SKIP-KNS    =   SPA-GMN-Q98-SKIP-KNS
                                               +   60
               PERFORM 3101-ADRS-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q98-SELNUM
           END-IF
      *
           .
       210-TUGI-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    １．未入力チェック
           IF  ( SPA-GMN-Q98-ADRS      =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    住所を設定し、Ｑ０１に戻る
           MOVE    SPA-GMN-Q98-POST    TO  SPA-Q01-Q98-POST
           MOVE    SPA-GMN-Q98-ADRS    TO  SPA-Q01-Q98-JYUSHO
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q98"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所設定処理
      *****************************************************************
       490-JYUSHO-SET-SEC              SECTION.
      *
           MOVE    WRK-WIDGET8     TO  IDX1
      *
           IF  ( SPA-GMN-Q98-TADRS (IDX1)  NOT =   SPACE )
      *
      *        確定住所編集処理
               PERFORM 4100-KAKUTEI-JYUSHO-EDIT-SEC
      *
               MOVE    SPA-GMN-Q98-TKBNCD (IDX1)
                                   TO  SPA-GMN-Q98-SELNUM
      *
           END-IF
      *
           .
       490-JYUSHO-SET-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q98"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       60 )
               MOVE    "whitebutton"
                                   TO  Q98-TADRSCHK-STYLE (IDX1)
      *        住所（一覧）
               MOVE    SPA-GMN-Q98-TADRS (IDX1)
                                   TO  Q98-TADRS (IDX1)
               INSPECT Q98-TADRS (IDX1)(5:16)
                       REPLACING  ALL "  "  BY  "　"
      *
           END-PERFORM
      *
      *    郵便番号
           MOVE    SPA-GMN-Q98-POST    TO  Q98-POST
      *    住所
           MOVE    SPA-GMN-Q98-ADRS    TO  Q98-ADRS
      *    選択番号
           MOVE    SPA-GMN-Q98-SELNUM  TO  Q98-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-Q98-CUR
      *
      *    選択番号
           WHEN   001
               MOVE  "SELNUM"      TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "一覧に表示されている番号を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "住所が選択されていません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q98"                TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
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
      *    システム管理マスタ検索（周辺住所）
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
                                           SYS-1015-EDYUKYMD
           MOVE    SYS-1015-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1015-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索（周辺住所）
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1015-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み飛ばし処理
      *****************************************************************
       910-SYSKANRI-READ-SKIP-SEC      SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   SPA-GMN-Q98-SKIP-KNS )
                 OR      ( FLG-SYSKANRI    =   "1" )
      *
      *        システム管理マスタ読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           .
       910-SYSKANRI-READ-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    住所マスター検索（主キー）
      *****************************************************************
       900-ADRS-KEY-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ADRS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

