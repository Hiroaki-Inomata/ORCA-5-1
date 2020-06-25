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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGP98.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 住所検索
      *  コンポーネント名  : 周辺住所画面（P98）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/09/03  住所一覧を５０件へ変更
      *  01.00.02    NACL-多々納  05/11/14  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/12/22  検索方法修正、上限なしへ
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面スタイル
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    患者登録用共通パラメタ
           COPY    "P02COMMON-SPA".
      *
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-SYSKANRI                      PIC 9(01).
           03  FLG-ADRS                          PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ADRS-OK                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDXX                              PIC 9(03).
           03  IDXY                              PIC 9(03).
           03  IDXZ                              PIC 9(03).
           03  IDX                               PIC 9(03).
      *
           03  IDX1                              PIC 9(01).
           03  IDX2                              PIC 9(02).
           03  IDX-STR                           PIC 9(04).
      *
        01 WRK-AREA.
           03  WRK-IDX3                          PIC 9(03).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （周辺町域情報）
           COPY    "CPSK1015.INC".
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-FREE                      TO  SPA-P02SCRAREA.
           MOVE    SPA-WORK                      TO  SPA-P02KYOUTU.
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 330-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-P02KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA                TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE                         TO  LINKAREA.
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE.
           MOVE    "P98"                         TO  MCP-WINDOW.
           MOVE    "SELNUM"                      TO  MCP-WIDGET.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE  ZERO                  TO  SPA-GMN98-PAGE
      *    住所登録画面より
           PERFORM 310-P981SPASET-SEC
           MOVE    1                   TO  SPA-GMN98-CUR
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所登録画面遷移処理
      *****************************************************************
       310-P981SPASET-SEC                         SECTION.
      *
           IF      SPA-GMN98-PAGE      =   ZERO
               MOVE    1                   TO  SPA-GMN98-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-GMN98-PAGE   -   1  )
                                       *   50
                                       +   1
      *
           INITIALIZE                      SPA-GMN98-TABLE
           MOVE  ZERO                  TO  SPA-GMN98-MAX
           MOVE  ZERO                  TO  SPA-GMN98-NEXT
      *    システム管理より周辺住所を取る
           INITIALIZE                      SYS-1015-REC.
           MOVE    "1015"              TO  SYS-1015-KANRICD.
           MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1015-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
           MOVE    SYS-1015-REC        TO  MCPDATA-REC.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC                        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDXX
           MOVE    ZERO                TO  IDXZ
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
      *
               MOVE    MCPDATA-REC         TO  SYS-1015-REC
               ADD     1                   TO  IDXZ
               IF      IDXZ                >=  IDX-STR
                   ADD     1               TO  IDXX
                   IF      IDXX            >   50
                       MOVE    1               TO  SPA-GMN98-NEXT
                       MOVE    1               TO  FLG-SYSKANRI
                   ELSE
                       PERFORM 31011-TBLHENSYU-SEC
                       MOVE    IDXX            TO  SPA-GMN98-MAX
                   END-IF
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  SPA-GMN98-INPUT
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       310-P981SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       31011-TBLHENSYU-SEC               SECTION.
      *
           MOVE    SYS-1015-KBNCD     TO  SPA-GMN98-KBNCD  (IDXX)
           MOVE    SYS-1015-LPUBCD    TO  SPA-GMN98-TLPUBCD(IDXX)
           MOVE    SYS-1015-POST      TO  SPA-GMN98-TPOST (IDXX)
           MOVE    SYS-1015-RENNUM    TO  SPA-GMN98-TRENNUM(IDXX)
           MOVE    SYS-1015-TOWNNAMEDSP
                                      TO  SPA-GMN98-TADRS (IDXX)
           .
       31011-TBLHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  210-BACK-SEC
      *        住所登録
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM  420-ADRUP-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  420-CLERA-SEC
      *        前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  430-ADRFW-SEC
      *        次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM  440-ADRBK-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  450-KAKUTEI-SEC
      *        住所一覧
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM  490-ADRS-CHK-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  510-SELNUM-CHK-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
           MOVE    SPACE               TO  P98
           INITIALIZE                      P98
      *
      *    COMPUTE IDXY    =  (SPA-GMN98-PAGE  -   1)  *   50
      *
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >       50
      *        COMPUTE IDXZ    =       IDXY    +   IDXX
               MOVE  "whitebutton"         TO  P98-TADRS-STYLE (IDXX)
               MOVE  SPA-GMN98-KBNCD(IDXX) TO  P98-TADRS(IDXX)(1:3)
               MOVE  SPA-GMN98-TADRS(IDXX)
                                           TO  P98-TADRS(IDXX)(5:16)
               INSPECT P98-TADRS(IDXX)(5:16)
                       REPLACING  ALL "  "  BY  "　"
	   END-PERFORM
      *
           MOVE    SPA-GMN98-ADRS      TO  P98-ADRS 
           MOVE    SPA-GMN98-POST      TO  P98-POST
           MOVE    SPA-GMN98-SELNUM    TO  P98-SELNUM  
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
           EVALUATE    SPA-GMN98-CUR
               WHEN    1
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力された番号は選択できません"
                                                  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "数字のみ入力可能です" TO  SPA-ERRMSG
               WHEN    "0003"
                   STRING  "住所マスタに登録されていません。"
                           "再登録して下さい。"
                                                  INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE.
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P98"               TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       330-SET-SCREEN-SEC                        SECTION.
      *
           PERFORM  500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  330-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE                         TO  LINKAREA
      *
           MOVE    "CURRENT"                     TO  MCP-PUTTYPE.
           MOVE    "P98    "                     TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
       330-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLERA-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN98-SELNUM
           INITIALIZE                  SPA-GMN98-ADRS
           INITIALIZE                  SPA-GMN98-POST
      *
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       420-CLERA-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SEC                              SECTION.
      *
      *    共通ＳＰＡ編集
      *
           MOVE  SPA-GMN98-MOTOPG            TO  SPA-SAKIPG.
      *
           MOVE  "P98"                       TO  SPA-MOTOPG.
      *
           MOVE  SPACE                       TO  JPAR-POST.
           MOVE  SPACE                       TO  JPAR-EDITADRS-NAME 
           MOVE  SPACE                       TO  JPAR-CITYNAME-NAME 
      **   MOVE  ZERO                        TO  JPAR-CNT.
      *    MOVE  ZERO                        TO  JPAR-ERRCD.
      *
      *****MOVE   "BACK"                     TO  MCP-PUTTYPE.
           MOVE   "JOIN"                     TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       410-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       420-ADRUP-SEC             SECTION.
      *
           MOVE    "P98"               TO  SPA-MOTOPG
           MOVE    "P981"              TO  SPA-SAKIPG
      *
           MOVE    "KBNCD"             TO  MCP-WIDGET
      *
           INITIALIZE                  SPA-GMN981-AREA
           MOVE    "P98"               TO  SPA-GMN981-MOTOPG
           MOVE    SPACE               TO  P981
      *
           INITIALIZE                      P981
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "P981   "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       420-ADRUP-EXT.
           EXIT.
      *
      *****************************************************************
      *   前ページ処理
      *****************************************************************
       430-ADRFW-SEC                             SECTION.
      *
           IF      SPA-GMN98-PAGE      >  1
               COMPUTE SPA-GMN98-PAGE  =  SPA-GMN98-PAGE    -  1
      *
               PERFORM 310-P981SPASET-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
      *
       430-ADRFW-EXT.
           EXIT.
      *
      *****************************************************************
      *   後ろページ処理
      *****************************************************************
       440-ADRBK-SEC                             SECTION.
      *
           IF      SPA-GMN98-NEXT      =   1
               COMPUTE  SPA-GMN98-PAGE =   SPA-GMN98-PAGE    +  1
               PERFORM 310-P981SPASET-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       440-ADRBK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC                           SECTION.
      *
           PERFORM 4501-KAKUTEI-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 45012-KAKUTEI-SYORI-SEC
           END-IF
      *
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定チェック処理
      *****************************************************************
       4501-KAKUTEI-CHK-SEC            SECTION.
      *
           IF     (SPA-GMN98-PAGE      =   ZERO )  OR
                  (SPA-GMN98-SELNUM    =   ZERO )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN98-CUR
           ELSE
               PERFORM 510-SELNUM-CHK-SEC
           END-IF
      *
           .
       4501-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       45012-KAKUTEI-SYORI-SEC         SECTION.
      *
      *    住所編集
           PERFORM 45012-ADRS-HEN-SEC
      ***  MOVE    1                   TO  JPAR-CNT
      **   MOVE    ZERO                TO  JPAR-ERRCD
      *
      *    共通ＳＰＡ編集
           MOVE    SPA-GMN98-MOTOPG    TO  SPA-SAKIPG
      *
           MOVE    "P98"               TO  SPA-MOTOPG
      *
           MOVE  "JOIN"                TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE  1                               TO  FLG-END.
      *
       45012-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       45012-ADRS-HEN-SEC                SECTION.
      *
           INITIALIZE                      ADRS-REC
           MOVE    SPA-GMN98-POST      TO  ADRS-POST
           MOVE    SPA-GMN98-EDANUM    TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ADRS            =   ZERO
               MOVE    ADRS-POST           TO  JPAR-POST
               MOVE    ADRS-EDITADRS-NAME  TO  JPAR-EDITADRS-NAME
      *        市町村名＋町域名
               MOVE    SPACE               TO  JPAR-CITYNAME-NAME
               STRING  ADRS-CITYNAME
                       ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-CITYNAME-NAME
               END-STRING
               IF      ADRS-TOWNNAME       =   "以下に掲載がない場合"
                   MOVE    ADRS-CITYNAME   TO  JPAR-CITYNAME-NAME
               END-IF
      *        京都で通り名称を編集する時
               IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                 AND  (ADRS-TOWNNAME2  NOT =   SPACE )
                 AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
                   IF     (ADRS-TOWNNAME2(1:2)     =   "（"  )
                       CONTINUE
                   ELSE
      *                市町村名＋町域名
                       MOVE    SPACE           TO  JPAR-CITYNAME-NAME
                       STRING  ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-CITYNAME-NAME
                       END-STRING
      *                市町村名＋町域名
                       MOVE    SPACE           TO  JPAR-CITYNAME-NAME
                       STRING  ADRS-PREFNAME
                               ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-EDITADRS-NAME
                       END-STRING
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  JPAR-POST
               MOVE    SPACE               TO  JPAR-EDITADRS-NAME
               MOVE    SPACE               TO  JPAR-CITYNAME-NAME
           END-IF
           .
       45012-ADRS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所選択処理
      *****************************************************************
       490-ADRS-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX3
           IF      MCP-WIDGET(1:9)     =   "CHK_TADRS"
               IF      MCP-WIDGET(10:3)    NUMERIC
                   MOVE    MCP-WIDGET(10:3)    TO  WRK-IDX3
               END-IF
           END-IF
           IF      WRK-IDX3            =   ZERO
               GO      TO      490-ADRS-CHK-EXT
           END-IF
      *
           MOVE    WRK-IDX3(1:2)       TO  IDX2
           MOVE    WRK-IDX3(3:1)       TO  IDX1
           COMPUTE IDXZ                =   ((IDX2   -   1  )  *  5 )
                                       +   IDX1
      *    COMPUTE IDXY        =  ((SPA-GMN98-PAGE  -   1)  *   50)
      *                        +  IDXZ
      *    MOVE    IDXY                TO  IDXZ
      *
           MOVE    1                   TO  SPA-GMN98-CUR
           IF     (IDXZ                >   ZERO  AND  <=   100  )  AND
                  (SPA-GMN98-KBNCD(IDXZ)   NOT =   SPACE )
               MOVE    SPA-GMN98-KBNCD(IDXZ)   TO  SPA-GMN98-SELNUM
               PERFORM 5101-ADRS-HENSYU-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    2                   TO  SPA-GMN98-CUR
               END-IF
           END-IF
           .
      *
       490-ADRS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　選択番号チェック
      *****************************************************************
       510-SELNUM-CHK-SEC                        SECTION.
      *
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           MOVE    SPACE               TO  SPA-GMN98-POST
           MOVE    SPACE               TO  SPA-GMN98-ADRS
      *
           MOVE    P98-SELNUM          TO  SPA-GMN98-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXZ    FROM    1   BY  1
                   UNTIL      (IDXZ        >       50  )  OR
                              (FLG-OK      =   1       )
               IF      SPA-GMN98-KBNCD(IDXZ)       =   SPACE
                   MOVE    50                      TO  IDXZ
               ELSE
                   IF      SPA-GMN98-KBNCD(IDXZ)   =   SPA-GMN98-SELNUM
                       MOVE    1                   TO  FLG-OK
                       PERFORM 5101-ADRS-HENSYU-SEC
                   END-IF
               END-IF
	   END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
      *
       510-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　住所編集
      *****************************************************************
       5101-ADRS-HENSYU-SEC                        SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    SPACE               TO  SPA-GMN98-POST
           MOVE    SPACE               TO  SPA-GMN98-ADRS
           MOVE    ZERO                TO  SPA-GMN98-EDANUM
      *    住所データ読み出し
           INITIALIZE                          ADRS-REC
           MOVE    SPA-GMN98-TPOST  (IDXZ) TO  ADRS-POST
           MOVE    SPA-GMN98-TRENNUM(IDXZ) TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ADRS            =   ZERO
               MOVE    ADRS-POST           TO  SPA-GMN98-POST
               MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN98-ADRS
      *        京都で通り名称を編集する時
               IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                 AND  (ADRS-TOWNNAME2  NOT =   SPACE )
                 AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
                   IF     (ADRS-TOWNNAME2(1:2)     =   "（"  )
                       CONTINUE
                   ELSE
      *                市町村名＋町域名
                       MOVE    SPACE           TO  SPA-GMN98-ADRS
                       STRING  ADRS-PREFNAME
                               ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    SPA-GMN98-ADRS
                       END-STRING
                   END-IF
               END-IF
               MOVE    ADRS-EDANUM         TO  SPA-GMN98-EDANUM
               MOVE    2                   TO  SPA-GMN98-CUR
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN98-CUR
           END-IF
           .
      *
       5101-ADRS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
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
      *    住所マスタ読み込み処理
      *****************************************************************
       900-ADRS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ADRS-REC
               MOVE    ZERO                TO  FLG-ADRS
           ELSE
               MOVE    1                   TO  FLG-ADRS
           END-IF
           .
       900-ADRS-READ-EXT.
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
      *    ＤＢクローズ処理
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
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
