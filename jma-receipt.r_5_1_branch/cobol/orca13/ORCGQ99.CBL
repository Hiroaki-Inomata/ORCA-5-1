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
       PROGRAM-ID.                     ORCGQ99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 住所検索−町域選択画面（Ｑ９９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/26    MCC−太田　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.02     MCC-太田     02/06/26  ジョブ管理ＤＢ追加
      *                                     画面ＱＩＤ１を
      *                                     Ｑ１００に名称変更
      *                                     新たにＱＩＤ１を追加
      * 01.00.03     NACL-太田    03/06/04  確定ボタン削除
      * 03.05.00     NACL-太田    07.05.23  グループ診療対応
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
           03  CNT-ADRS                            PIC 9(05).
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
      *    固定値領域
      *01  CONST-AREA.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    住所マスター
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    部分一致文字列編集
           COPY    "CPORCSADDSIGN.INC".
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
      *----(01.00.02) LINE ADD START ----------------------------------
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *----(01.00.02) LINE ADD END   ----------------------------------
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
           MOVE    "Q99"               TO  MCP-WINDOW
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
           MOVE    SPACE           TO  SPA-Q99
           INITIALIZE                  SPA-Q99
      *
           MOVE    SPACE           TO  SPA-Q01-Q99-JYUSHO
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-Q01-Q99-POST
                                   TO  ADRS-POST
      *
      *    住所検索処理
           PERFORM 900-ADRS-KEY4-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( FLG-ADRS    =   1 )
                OR   ( IDX1        >   200 )
      *
      *        住所編集処理
               PERFORM 3101-ADRS-EDIT-SEC
      *
      *        住所読込処理
               PERFORM 900-ADRS-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    カーソルキー制御項目
           MOVE   1                TO  SPA-GMN-Q99-CUR
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
           MOVE    IDX1            TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9        TO  SPA-GMN-Q99-NUM (IDX1)
           MOVE    ADRS-POST       TO  SPA-GMN-Q99-POST (IDX1)
           MOVE    ADRS-EDITADRS-NAME
                                   TO  SPA-GMN-Q99-ADRS (IDX1)
           MOVE    IDX1            TO  SPA-GMN-Q99-MAX
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
      *    行選択チェック
               WHEN    ANY             ALSO    "ADRSLIST"
                   PERFORM 4101-ADRSLISTSEL-SEC
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
           MOVE    1                   TO  SPA-GMN-Q99-CUR
      *
           MOVE    Q99-SELNUM          TO  SPA-GMN-Q99-SELNUM
      *
      *    未入力時はチェックを行わない
           IF  ( SPA-GMN-Q99-SELNUM    =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           IF      ( SPA-GMN-Q99-SELNUM   >=   1 )
            AND    ( SPA-GMN-Q99-SELNUM   <=   SPA-GMN-Q99-MAX )
      *
               PERFORM 490-KAKUTEI-SEC
      *
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-Q99-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4101-ADRSLISTSEL-SEC            SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >      200 )
               IF      (   Q99-ADRSLIST-SELECT (IDX1)  =   "T"  )
                   MOVE    IDX1            TO      SPA-GMN-Q99-SELNUM
                   PERFORM 490-KAKUTEI-SEC
                   MOVE   200              TO  IDX1
               END-IF
           END-PERFORM
      *
           .
       4101-KEKKALSTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q99"               TO  SPA-MOTOPG
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
      *    確定　処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    選択番号チェック
      *    １．未入力チェック
           IF  ( SPA-GMN-Q99-SELNUM    =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    ２．範囲チェック
           IF      ( SPA-GMN-Q99-SELNUM   >=   1 )
            AND    ( SPA-GMN-Q99-SELNUM   <=   SPA-GMN-Q99-MAX )
      *
               CONTINUE
      *
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    住所を設定し、Ｑ０１に戻る
           MOVE    SPA-GMN-Q99-POST (SPA-GMN-Q99-SELNUM)
                                       TO  SPA-Q01-Q99-POST
           MOVE    SPA-GMN-Q99-ADRS (SPA-GMN-Q99-SELNUM)
                                       TO  SPA-Q01-Q99-JYUSHO
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q99"               TO  SPA-MOTOPG
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
           MOVE    "Q99"               TO  MCP-WINDOW.
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
      *    検索件数
           MOVE    SPA-GMN-Q99-MAX
                                   TO  Q99-ADRSLIST-COUNT
      *
      *    検索結果
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-Q99-MAX)
      *        連番
               MOVE    SPA-GMN-Q99-NUM  (IDX1)
                                   TO  Q99-NUM (IDX1)
      *        郵便番号
               MOVE    SPA-GMN-Q99-POST (IDX1)
                                   TO  Q99-POST (IDX1)
      *        住所
               MOVE    SPA-GMN-Q99-ADRS (IDX1)
                                   TO  Q99-ADRS (IDX1)
      *
               IF      ( SPA-GMN-Q99-SEL =   IDX1 )
                   MOVE    "T"     TO  Q99-ADRSLIST-SELECT (IDX1)
               ELSE
                   MOVE    "F"     TO  Q99-ADRSLIST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    選択番号
           MOVE    SPA-GMN-Q99-SELNUM  TO  Q99-SELNUM
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
           EVALUATE    SPA-GMN-Q99-CUR
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
               MOVE    "範囲外の番号が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号が未入力です"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q99"                TO  SPA-MOTOPG
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
      *    住所マスター検索（郵便番号）
      *****************************************************************
       900-ADRS-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
      *
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-POST           TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING   ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR        TO  ADRS-POST
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
      *
           .
       900-ADRS-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    住所マスター読込（郵便番号）
      *****************************************************************
       900-ADRS-KEY4-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           .
       900-ADRS-KEY4-FET-EXT.
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
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

