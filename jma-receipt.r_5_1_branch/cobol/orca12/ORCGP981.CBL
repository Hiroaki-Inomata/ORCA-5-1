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
       PROGRAM-ID.                               ORCGP981.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 住所検索
      *  コンポーネント名  : 住所登録画面（P981）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/04  住所一覧表追加に伴う修正
      * 01.00.02     NACL-多々納  05/11/14  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/12/22  一括入力対応
      * 04.06.00     NACL-多々納  11/02/10  京都府通り名称編集対応
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
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
           03  FLG-ADRS                           PIC 9(01).
           03  FLG-IN-RENNUM                      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
           03  IDZ1                              PIC 9(04).
      *
      *    一時領域
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
      *
           03  WRK-PIDMSG          PIC X(80).
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
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA                TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P981"              TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-MOTOPG(1:4)
      *            住所一覧から
               WHEN    "P982"
                   IF      SPA-GMN982-LPUBCD   =   SPACE
                       IF      SPA-GMN981-LPUBCD   =   SPACE
                           MOVE     3              TO  SPA-GMN981-CUR
                       ELSE
                           MOVE     2              TO  SPA-GMN981-CUR
                       END-IF
                   ELSE
      *                対象あり
                       PERFORM 4201-ADRS-HENSYU-SEC
                   END-IF
      *            確認画面より
               WHEN    "PID1"
                   PERFORM 3003-PID1-SET-SEC
                   MOVE    SPACE               TO  SPA-PIDCD
      *
               WHEN    OTHER
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-GMN981-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           IF      SPA-GMN981-KBNCD NOT =   ZERO
               MOVE    SPA-GMN981-KBNCD    TO  P981-KBNCD
               PERFORM 4105-KBNCD-CHK-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           .
       310-SPA-SET-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-PID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
      *            削除の確認
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 30031-DELSYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN981-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-PID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  420-CLERA-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  430-DELETE-SEC
      *        一覧表
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM  420-P982-SYORI-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  410-KAKUTEI-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN981-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "KBNCD"
                   PERFORM  4105-KBNCD-CHK-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P981-LPUBCD         TO  SPA-GMN981-LPUBCD
           MOVE    P981-POST           TO  SPA-GMN981-POST
           IF      P981-RENNUM     NOT =   SPA-GMN981-RENNUM
               MOVE    1               TO  FLG-IN-RENNUM
           END-IF
           MOVE    P981-RENNUM         TO  SPA-GMN981-RENNUM
           MOVE    P981-TOWNNAMEDSP    TO  SPA-GMN981-TOWNNAMEDSP
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           IF     (SPA-GMN981-LPUBCD       =   SPACE   )   AND
                  (SPA-GMN981-POST         =   SPACE   )
               CONTINUE
           ELSE
               IF    ((SPA-GMN981-LPUBCD   NOT =   SPA-MAE981-LPUBCD)
                  OR  (SPA-GMN981-POST     NOT =   SPA-MAE981-POST  ))
               AND   ( SPA-GMN981-RENNUM       =   SPA-MAE981-RENNUM )
      *            一覧表示へ
                   PERFORM 420-P982-CHECK-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE   )  AND
                  (FLG-END             =   ZERO    )
               IF     (SPA-GMN981-RENNUM   NOT =   SPA-MAE981-RENNUM )
                  OR  (FLG-IN-RENNUM           =   1                 )
                   PERFORM 4103-RENNUM-CHK-SEC
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P98"               TO  SPA-SAKIPG
           MOVE    "P981"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *  　区分コードチェック
      *****************************************************************
       4105-KBNCD-CHK-SEC                         SECTION.
      *
           MOVE    1                      TO  SPA-GMN981-CUR
      *
           MOVE    P981-KBNCD             TO  SPA-GMN981-KBNCD
      *
      *    INITIALIZE                         SPA-GMN981-TOWNNAMEDSP
      *                                       SPA-GMN981-LPUBCD
      *                                       SPA-GMN981-POST
      *                                       SPA-GMN981-RENNUM
      *                                       SPA-GMN981-TOWNNAME
      *
           IF      SPA-GMN981-KBNCD       NOT NUMERIC
               MOVE    ZERO               TO   SPA-GMN981-KBNCD
           END-IF
      *
           IF      SPA-GMN981-KBNCD    =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN981-CUR
           ELSE     
               INITIALIZE                      SYS-1015-REC
               MOVE    "1015"              TO  SYS-1015-KANRICD
               MOVE    SPA-GMN981-KBNCD (1:3)
                                           TO  SYS-1015-KBNCD (1:3)
               MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1015-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
               MOVE    SYS-1015-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1015-REC
                   MOVE    SYS-1015-TOWNNAMEDSP
                                           TO  SPA-GMN981-TOWNNAMEDSP
                   MOVE    SYS-1015-LPUBCD     TO  SPA-GMN981-LPUBCD
                   MOVE    SYS-1015-POST       TO  SPA-GMN981-POST
                   MOVE    SYS-1015-RENNUM     TO  SPA-GMN981-RENNUM
      *
                   INITIALIZE                  ADRS-REC
                   MOVE    SPA-GMN981-LPUBCD   TO  ADRS-LPUBCD
                   MOVE    SPA-GMN981-POST     TO  ADRS-POST
                   MOVE    SPA-GMN981-RENNUM   TO  ADRS-EDANUM
                   MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
                   MOVE    ADRS-REC            TO  MCPDATA-REC
                   PERFORM 900-ADRS-INV-SEC
                   IF      FLG-ADRS                =   ZERO
                       MOVE    ADRS-EDITADRS-NAME
                                               TO  SPA-GMN981-TOWNNAME
                       IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                         AND  (ADRS-TOWNNAME2      NOT =   SPACE)
                         AND  (ADRS-TOWNNAME2(1:2) NOT =   "（" )
                           STRING  "通り名："
                                   ADRS-TOWNNAME2
                                   ADRS-TOWNNAME
                                               DELIMITED  BY  SPACE
                                           INTO    SPA-GMN981-TOWNNAME2
                           END-STRING
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN981-LPUBCD   TO  SPA-MAE981-LPUBCD
               MOVE    SPA-GMN981-POST     TO  SPA-MAE981-POST
               MOVE    SPA-GMN981-RENNUM   TO  SPA-MAE981-RENNUM
      *
               MOVE    2                   TO  SPA-GMN981-CUR
           END-IF
           .
       4105-KBNCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　枝番チェック
      *****************************************************************
       4103-RENNUM-CHK-SEC                         SECTION.
      *
           INITIALIZE                      ADRS-REC
           MOVE    SPA-GMN981-LPUBCD   TO  ADRS-LPUBCD
           MOVE    SPA-GMN981-POST     TO  ADRS-POST
           MOVE    SPA-GMN981-RENNUM   TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           IF      FLG-ADRS            =   ZERO
               MOVE    ADRS-EDITADRS-NAME
                                           TO  SPA-GMN981-TOWNNAME
               IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                 AND  (ADRS-TOWNNAME2      NOT =   SPACE)
                 AND  (ADRS-TOWNNAME2(1:2) NOT =   "（" )
                   STRING  "通り名："
                           ADRS-TOWNNAME2
                                               DELIMITED  BY  SPACE
                                           INTO    SPA-GMN981-TOWNNAME2
                   END-STRING
               END-IF
               IF      SPA-GMN981-TOWNNAMEDSP  =   SPACE
                   MOVE    ADRS-TOWNNAME   TO  SPA-GMN981-TOWNNAMEDSP
               END-IF
      *
               MOVE    SPA-GMN981-LPUBCD   TO  SPA-MAE981-LPUBCD
               MOVE    SPA-GMN981-POST     TO  SPA-MAE981-POST
               MOVE    SPA-GMN981-RENNUM   TO  SPA-MAE981-RENNUM
               MOVE    5                   TO  SPA-GMN981-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN981-TOWNNAME
               MOVE    SPACE               TO  SPA-GMN981-TOWNNAME2
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN981-CUR
           END-IF
           MOVE    ZERO                TO  FLG-IN-RENNUM
      *
           .
       4103-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　町域名短縮チェック
      *****************************************************************
       4104-TOWNNAMEDSP-CHK-SEC       SECTION.
      *
           MOVE    5                  TO  SPA-GMN981-CUR
      *
           MOVE    P981-TOWNNAMEDSP   TO  SPA-GMN981-TOWNNAMEDSP
      *
           MOVE    12                 TO  SPA-GMN981-CUR
           .
       4104-TOWNNAMEDSP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表遷移処理
      *****************************************************************
       420-P982-SYORI-SEC           SECTION.
      *
           IF     (SPA-GMN981-LPUBCD       =   SPACE )  AND
                  (SPA-GMN981-POST         =   SPACE )
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE     2                  TO  SPA-GMN981-CUR
           ELSE
      *        一覧表示へ
               PERFORM 420-P982-CHECK-SEC
           END-IF
           .
       420-P982-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    一覧表遷移チェック処理
      *****************************************************************
       420-P982-CHECK-SEC           SECTION.
      *
           INITIALIZE                  SPA-P982-AREA
           MOVE    SPA-GMN981-LPUBCD   TO  SPA-GMN982-LPUBCD
           MOVE    SPA-GMN981-POST     TO  SPA-GMN982-POST
           MOVE    SPA-GMN981-RENNUM   TO  SPA-GMN982-RENNUM
           MOVE    "P981"              TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           EVALUATE    SPA-GMN982-MAX
           WHEN    ZERO
      *        対象なし
               MOVE    "0003"              TO  SPA-ERRCD
               IF      SPA-GMN981-LPUBCD   =   SPACE
                   MOVE     3                  TO  SPA-GMN981-CUR
               ELSE
                   MOVE     2                  TO  SPA-GMN981-CUR
               END-IF
           WHEN    1
      *        対象あり
               PERFORM 4201-ADRS-HENSYU-SEC
           WHEN    OTHER
               PERFORM 4201-P982-PUTSYORI-SEC
           END-EVALUATE
      *
           .
       420-P982-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4201-ADRS-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-GMN982-LPUBCD   TO  SPA-GMN981-LPUBCD
           MOVE    SPA-GMN982-POST     TO  SPA-GMN981-POST
           MOVE    SPA-GMN982-RENNUM   TO  SPA-GMN981-RENNUM
      *
           PERFORM 4103-RENNUM-CHK-SEC
           MOVE    5                   TO  SPA-GMN981-CUR
      *
           .
       4201-ADRS-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    一覧表遷移処理
      *****************************************************************
       4201-P982-PUTSYORI-SEC           SECTION.
      *
      *    画面カーソルセット処理
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
      *    一覧へ
           MOVE    "P982"              TO  SPA-SAKIPG
           MOVE    "P981"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P982"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4201-P982-PUTSYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLERA-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN981-AREA
           MOVE    1                   TO  SPA-GMN981-CUR
      *
           .
       420-CLERA-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC           SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD     =   SPACE
               MOVE    "0101"              TO  SPA-PIDCD
           END-IF
      *
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       30031-DELSYORI-SEC           SECTION.
      *
           INITIALIZE                      SYS-1015-REC
           MOVE    "1015"                  TO  SYS-1015-KANRICD
           MOVE    SPA-GMN981-KBNCD (1:3)  TO  SYS-1015-KBNCD (1:3)
           MOVE    SPA-SYSYMD              TO  SYS-1015-STYUKYMD
           MOVE    SPA-SYSYMD              TO  SYS-1015-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-1015-HOSPNUM
           MOVE    SYS-1015-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1015-REC
      *
               MOVE    SYS-1015-REC            TO  MCPDATA-REC
               MOVE    "DBDELETE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN981-AREA
           MOVE    1                   TO  SPA-GMN981-CUR
      *
           .
       30031-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4901-KANRENCHK-SEC
               IF      SPA-ERRCD     =   SPACE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      * 
           INITIALIZE                      SYS-1015-REC
           MOVE    "1015"                  TO  SYS-1015-KANRICD
           MOVE    SPA-GMN981-KBNCD (1:3)  TO  SYS-1015-KBNCD (1:3)
           MOVE    SPA-SYSYMD              TO  SYS-1015-STYUKYMD
           MOVE    SPA-SYSYMD              TO  SYS-1015-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-1015-HOSPNUM
           MOVE    SYS-1015-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1015-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1015-REC
               INITIALIZE                      SYS-1015-REC
               MOVE   "1015"                   TO  SYS-1015-KANRICD
               MOVE    SPA-GMN981-KBNCD (1:3)  TO  SYS-1015-KBNCD (1:3)
               MOVE    ZERO                    TO  SYS-1015-STYUKYMD
               MOVE    ALL "9"                 TO  SYS-1015-EDYUKYMD
           END-IF
           MOVE    SPA-GMN981-TOWNNAMEDSP  TO  SYS-1015-TOWNNAMEDSP
           MOVE    SPA-GMN981-LPUBCD       TO  SYS-1015-LPUBCD  
           MOVE    SPA-GMN981-POST         TO  SYS-1015-POST
           MOVE    SPA-GMN981-RENNUM       TO  SYS-1015-RENNUM
      *
           IF      FLG-SYSKANRI            =   1
               MOVE    SMCNDATE-YMD        TO  SYS-1015-CREYMD
           END-IF
           MOVE    SMCNDATE-YMD        TO  SYS-1015-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1015-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
      *
           MOVE    SYS-1015-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    "DBUPDATE"              TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               MOVE    "DBINSERT"              TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1001"                  TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =    SPACE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
      *
           IF      SPA-GMN981-KBNCD     =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN981-CUR
           END-IF
      *
           IF     (SPA-GMN981-LPUBCD       =   SPACE   )   OR
                  (SPA-GMN981-POST         =   SPACE   )
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0005"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN981-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN981-TOWNNAMEDSP  =   SPACE   )
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN981-CUR
               END-IF
           END-IF
           .
       4901-KANRENCHK-EXT.
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
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-PIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P981    "          TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  P981
           INITIALIZE                      P981
      *
           MOVE    SPA-GMN981-KBNCD    TO  P981-KBNCD
           MOVE    SPA-GMN981-LPUBCD   TO  P981-LPUBCD
           MOVE    SPA-GMN981-POST     TO  P981-POST
           MOVE    SPA-GMN981-RENNUM   TO  P981-RENNUM
           MOVE    SPA-GMN981-TOWNNAMEDSP
                                       TO  P981-TOWNNAMEDSP
           MOVE    SPA-GMN981-TOWNNAME TO  P981-TOWNNAME
           MOVE    SPA-GMN981-TOWNNAME2    TO  P981-TOWNNAME2
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
           IF      SPA-GMN981-CUR      =   ZERO 
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "KBNCD"
                       MOVE    2               TO  SPA-GMN981-CUR
                   WHEN    "LPUBCD"
                       MOVE    3               TO  SPA-GMN981-CUR
                   WHEN    "POST"
                       MOVE    4               TO  SPA-GMN981-CUR
                   WHEN    "RENNUM"
                       MOVE    5               TO  SPA-GMN981-CUR
                   WHEN    "TOWNNAMEDSP"
                       MOVE    12              TO  SPA-GMN981-CUR
               END-EVALUATE
           END-IF
           EVALUATE    SPA-GMN981-CUR
                WHEN   1
                    MOVE  "KBNCD"          TO  MCP-WIDGET
                WHEN   2
                    MOVE  "LPUBCD"         TO  MCP-WIDGET
                WHEN   3
                    MOVE  "POST"           TO  MCP-WIDGET
                WHEN   4
                    MOVE  "RENNUM"         TO  MCP-WIDGET
                WHEN   5
                    MOVE  "TOWNNAMEDSP"    TO  MCP-WIDGET
                 WHEN   12
                    MOVE  "B12"            TO  MCP-WIDGET
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
                   MOVE    "区分コードを入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "住所マスタに登録がありません"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象の住所データがありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                   "郵便番号か地方公共団体コードを入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   STRING  "郵便番号と地方公共団体コードを"
                           "入力して下さい"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "町域名短縮を入力して下さい"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  PERR
           MOVE    SPA-ERRCD            TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  PERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "P981"               TO  SPA-MOTOPG
           MOVE    "PERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "PERR"               TO  MCP-WINDOW
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
      *    確認メッセージ表示理
      *****************************************************************
       520-PIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "削除します。よろしいですか？"
                                       TO  WRK-PIDMSG
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "P981"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       520-PIDSET-EXT.
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
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスター読込（キー）
      *****************************************************************
       900-ADRS-INV-SEC         SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-ADRS
                   MOVE    MCPDATA-REC         TO  ADRS-REC
               ELSE
                   MOVE    1                   TO  FLG-ADRS
                   INITIALIZE                      ADRS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ADRS
               INITIALIZE                      ADRS-REC
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-ADRS-INV-EXT.
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

