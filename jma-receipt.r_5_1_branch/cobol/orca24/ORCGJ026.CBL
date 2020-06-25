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
       PROGRAM-ID.                 ORCGJ026.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計カード
      *  コンポーネント名  : カルテ３号紙印刷指示
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/10    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  05/11/30  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 04.07.00     NACL-多々納  12/09/25  プログラムオプション保険対応
      * 04.08.00     NACL-多々納  14/08/12  クライアント印刷対応
      *****************************************************************
      *
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KAKUTEI         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1              PIC X(40).
               05  WRK-JIDMSG2              PIC X(60).
      *    カルテ３号紙プログラム名
           03  WRK-PRINT-PG            PIC X(20).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-ERRCD           PIC X(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    帳票編集区分情報
           COPY  "CPSK1030.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    カルテ３号様式出力パラメタ
           COPY    "CPORCHC33.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA24SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *H24.9
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
      *H24.9
               IF      WRK-ERRCD           =   "9001"
                                           OR  "9002"
      *            プログラムオプションとのエラー
                   MOVE    SPACE               TO  WRK-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
                   PERFORM 210-BACK
                   GO      TO   100-INIT-EXT
               END-IF
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END         =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "J026"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *        確認画面より
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
               WHEN    OTHER
      *            初期画面編集
                   PERFORM                     310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-J026-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-J026-AREA
      *
           STRING      "＜発行診療科："    DELIMITED  BY  SIZE
                       SPA-GMN-SRYKAMEI    DELIMITED  BY  SPACE
                       "＞"                DELIMITED  BY  SIZE
                                   INTO    SPA-J026-GMN-MIDASI
           END-STRING
      *
           MOVE    1                   TO  SPA-J026-GMN-STRDD
           MOVE    ZERO                TO  SPA-J026-GMN-ENDPAGE
           MOVE    ZERO                TO  SPA-J026-GMN-ENDLINE
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
      *            印刷確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-PRINT
                       PERFORM 2301-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-J026-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-JIDCD
                   MOVE    SPACE               TO  SPA-JID1-FLG
               WHEN    "0102"
      *            印刷確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 2201-HEAD-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-J026-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-JIDCD
                   MOVE    SPACE               TO  SPA-JID1-FLG
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  J026
      *
      *    画面編集
           INITIALIZE                      J026
           MOVE    SPA-J026-GMN-MIDASI     TO  J026-MIDASI
      *
           MOVE    SPA-J026-GMN-STRDD      TO  J026-STRDD
           MOVE    SPA-J026-GMN-ENDPAGE    TO  J026-ENDPAGE
           MOVE    SPA-J026-GMN-ENDLINE    TO  J026-ENDLINE
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-J026-CUR        =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-J026-CUR
               WHEN    1
                   MOVE    "STDD"              TO  MCP-WIDGET
               WHEN    2
                   MOVE    "ENDPAGE"           TO  MCP-WIDGET
               WHEN    3
                   MOVE    "ENDLINE"           TO  MCP-WIDGET
               WHEN    4
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "STRDD"
                   MOVE    2               TO  SPA-J026-CUR
               WHEN    "ENDPAGE"
                   MOVE    3               TO  SPA-J026-CUR
               WHEN    "ENDLINE"
                   MOVE    4               TO  SPA-J026-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        頭書印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 220-HEAD-PRINT-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 230-PRINT-SEC
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
           MOVE    ZERO                TO  SPA-J026-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
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
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-KAKUTEI         =   1     )
               PERFORM 4103-KANREN-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           MOVE    J026-STRDD          TO  SPA-J026-GMN-STRDD
           MOVE    J026-ENDPAGE        TO  SPA-J026-GMN-ENDPAGE
           MOVE    J026-ENDLINE        TO  SPA-J026-GMN-ENDLINE
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
           PERFORM 41021-STDD-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-ENDPAGE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-ENDLINE-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       41021-STDD-SEC                SECTION.
      *
           IF      SPA-J026-GMN-STRDD  =   ZERO
               MOVE    1                   TO  SPA-J026-GMN-STRDD
           END-IF
           IF      SPA-J026-GMN-STRDD  >=  01  AND
                                       <=  SPA-SRYMATU
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-J026-CUR
           END-IF
      *
           .
       41021-STDD-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終ページ処理
      *****************************************************************
       41022-ENDPAGE-SEC                SECTION.
      *
           .
       41022-ENDPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終行処理
      *****************************************************************
       41022-ENDLINE-SEC                SECTION.
      *
           IF      SPA-J026-GMN-ENDLINE    >=  40
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-J026-CUR
           END-IF
      *
           .
       41022-ENDLINE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC                SECTION.
      *
      *
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頭書印刷処理
      *****************************************************************
       220-HEAD-PRINT-SEC                SECTION.
      *
           MOVE    "0102"              TO  SPA-JIDCD
      *
           .
       220-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    頭書印刷処理
      *****************************************************************
       2201-HEAD-PRINT-SYORI-SEC                SECTION.
      *
           MOVE    2                   TO  FLG-PRINT
           PERFORM 2301-PRINT-SYORI-SEC
      *
           .
       2201-HEAD-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷　処理
      *****************************************************************
       230-PRINT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-J026-CUR
           MOVE    1                   TO  FLG-KAKUTEI
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-JIDCD
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷　処理
      *****************************************************************
       2301-PRINT-SYORI-SEC            SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1030-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1030-STYUKYMD(7:2)
           MOVE    SYS-1030-STYUKYMD   TO  SYS-1030-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1030-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
           IF      SYS-1030-KARUTE3KBN     =   SPACE
               MOVE    ZERO                TO  SYS-1030-KARUTE3KBN
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
      *ver.4.8
      *    クライアント印刷
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得処理
               PERFORM 23010-CLIENT-PRT-SEC
           END-IF
      *
           IF      SPA-GMN-NYUGAIKBN    =   "2"
               PERFORM 23011-PRINT-GAIRAI-SEC
           ELSE
               PERFORM 23012-PRINT-NYUIN-SEC
           END-IF
      *
      *ver.4.8
      *    クライアント印刷
           IF     (SPA-CLIENT-PRT-FLG   =   1     )
             AND  (SPA-ERRCD            =   SPACE )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           ELSE
               PERFORM 510-ERRSET-SEC
           END-IF
           .
       2301-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       23010-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
           .
       23010-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来 カルテ３号紙印刷処理
      *****************************************************************
       23011-PRINT-GAIRAI-SEC            SECTION.
      *
      *    カルテ３号紙
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  ORCSPRTNM-SRYYMD(1:6)
           MOVE    "01"                TO  ORCSPRTNM-SRYYMD(7:2)
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHC33"           TO  WRK-PRINT-PG
           END-IF
      *
           INITIALIZE                  ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-GMN-PTNUM       TO  ORCHC33-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCHC33-PTID
           MOVE    SPA-GMN-NAME        TO  ORCHC33-NAME
           MOVE    SPA-GMN-BIRTHDAY    TO  ORCHC33-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  ORCHC33-SEX
           MOVE    SPA-NAI-SRYYM       TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCHC33-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  ORCHC33-SRYKA
      *H24.9
           MOVE    SPA-GMN-HHKNCOMBI   TO  ORCHC33-HKNCOMBI
      *
           IF      FLG-PRINT           =   1
      *        明細
               MOVE    ZERO                TO  ORCHC33-PRINT-FLG
      *        印刷開始日
               MOVE    SPA-J026-GMN-STRDD  TO  ORCHC33-STRDD
      *        最終印刷頁
               MOVE    SPA-J026-GMN-ENDPAGE    TO  ORCHC33-ENDPAGE
      *        最終印刷行
               MOVE    SPA-J026-GMN-ENDLINE    TO  ORCHC33-ENDLINE
           ELSE
      *        頭書
               MOVE    1                   TO  ORCHC33-PRINT-FLG
           END-IF
      *
           MOVE    SYS-1030-KARUTE3KBN
                                       TO  ORCHC33-KARUTE3KBN
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           CALL    WRK-PRINT-PG        USING
                                       SPA-AREA
                                       ORCHC33AREA
      *
           IF      ORCHC33-RC      NOT =   ZERO
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           IF      ORCHC33-RC          =   8
      *        プログラムオプションと保険選択のエラー
               MOVE    "9001"              TO  SPA-ERRCD
           END-IF
      *v4.8
      *    印刷なし
           IF      ORCHC33-RC          =   1
               MOVE    "9002"              TO  SPA-ERRCD
           END-IF
      *
           .
       23011-PRINT-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院 カルテ３号紙印刷処理
      *****************************************************************
       23012-PRINT-NYUIN-SEC            SECTION.
      *
      *    カルテ３号紙
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000004"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  ORCSPRTNM-SRYYMD(1:6)
           MOVE    "01"                TO  ORCSPRTNM-SRYYMD(7:2)
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHCN33"          TO  WRK-PRINT-PG
           END-IF
      *
           INITIALIZE                  ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-GMN-PTNUM       TO  ORCHC33-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCHC33-PTID
           MOVE    SPA-GMN-NAME        TO  ORCHC33-NAME
           MOVE    SPA-GMN-BIRTHDAY    TO  ORCHC33-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  ORCHC33-SEX
           MOVE    SPA-NAI-SRYYM       TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCHC33-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  ORCHC33-SRYKA
      *H24.9
           MOVE    SPA-GMN-HHKNCOMBI   TO  ORCHC33-HKNCOMBI
           IF      FLG-PRINT           =   1
      *        明細
               MOVE    ZERO                TO  ORCHC33-PRINT-FLG
      *        印刷開始日
               MOVE    SPA-J026-GMN-STRDD  TO  ORCHC33-STRDD
      *        最終印刷頁
               MOVE    SPA-J026-GMN-ENDPAGE    TO  ORCHC33-ENDPAGE
      *        最終印刷行
               MOVE    SPA-J026-GMN-ENDLINE    TO  ORCHC33-ENDLINE
           ELSE
      *        頭書
               MOVE    1                   TO  ORCHC33-PRINT-FLG
           END-IF
           MOVE    SYS-1030-KARUTE3KBN
                                       TO  ORCHC33-KARUTE3KBN
      *
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           CALL    WRK-PRINT-PG        USING
                                       SPA-AREA
                                       ORCHC33AREA
      *
           IF      ORCHC33-RC      NOT =   ZERO
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           IF      ORCHC33-RC          =   8
      *        プログラムオプションと保険選択のエラー
               MOVE    "9001"              TO  SPA-ERRCD
           END-IF
      *v4.8
      *    印刷なし
           IF      ORCHC33-RC          =   1
               MOVE    "9002"              TO  SPA-ERRCD
           END-IF
      *
           .
       23012-PRINT-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J026"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J02"               TO  MCP-WINDOW
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J026"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "印刷開始日が違います。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "最終印刷行は４０行未満にして下さい"
                                       TO  SPA-ERRMSG
               WHEN    "9000"
                   MOVE    "印刷エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   STRING  "保険組合せでの印刷となります。"
                           "保険組合せを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9002"
                   MOVE    "印刷対象がありませんでした。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J026"              TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   MOVE    "カルテ３号紙を印刷します。よろしいですか？"
                                       TO  WRK-JIDMSG
               WHEN    "0102"
                   MOVE    "カルテ３号紙の頭書を印刷します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(33:)
      *
               WHEN    OTHER
                   MOVE    SPA-JIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J026"              TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *
