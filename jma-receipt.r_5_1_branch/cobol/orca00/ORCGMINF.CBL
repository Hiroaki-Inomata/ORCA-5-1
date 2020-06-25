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
       PROGRAM-ID.                 ORCGMINF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メニュー
      *  コンポーネント名  : 感染症サーベイランス
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/12/04    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 04.06.00     NACL-伊藤    10/02/13  リアルタイム送信
      *                                     初期表示は当日報告日
      *                                     １日１回は前日
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  SPA-XA01.
           03  SPA-MINF-AREA.
               05  SPA-MINF-TAISYOYMD          PIC X(22).
               05  SPA-NAI-TAISYOYMD           PIC X(08).
               05  SPA-MINF-TBL.
                   07  SPA-MINF-OCC            OCCURS  100.
                       09  SPA-MINF-TYMD       PIC X(6).
                       09  SPA-MINF-TBYOGEN    PIC X(22).
                       09  SPA-MINF-TNAME      PIC X(18).
                       09  SPA-MINF-TSEX       PIC X(2).
                       09  SPA-MINF-TSHINRYO   PIC X(2).
                       09  SPA-MINF-TYAKUZAI   PIC X(2).
                       09  SPA-MINF-TBYOMEI    PIC X(60).
                       09  SPA-MINF-TSTRYMD    PIC X(10).
               05  SPA-MINF-MAX                PIC 9(04).
               05  SPA-MINF-SRYYMD             PIC X(09).
               05  SPA-MINF-STR                PIC 9(04).
               05  SPA-MINF-END                PIC 9(01).
               05  SPA-MINF-FLG                PIC 9(01).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-INFECT          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH            PIC X(64).
           03  WRK-LIST-ROW        PIC S9(9)   BINARY.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02). 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  INFECT-REC.
           COPY    "CPINFECTION.INC".
      *
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC".
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
      *
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
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "MINF"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
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
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "MID1"
                   PERFORM 330-MID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
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
           MOVE    1               TO  SPA-MINF-FLG
           MOVE    1               TO  SPA-MINF-STR
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           IF      SPA-DATAINFECTIONKBN    =   2
      *        前日
               PERFORM 31012-ZENJITU-HEN-SEC
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-TAISYOYMD
      *
           PERFORM 320-LIST-SET-SEC
      *
           MOVE    1               TO  FLG-LIST-ROW
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リストセット処理
      *****************************************************************
       320-LIST-SET-SEC            SECTION.
      *
           INITIALIZE                  INFECT-REC
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    SPA-NAI-TAISYOYMD   TO  INFECT-SRYYMD
                   MOVE    "key3"              TO  WRK-PATH
               WHEN    2
                   MOVE    SPA-NAI-TAISYOYMD   TO  INFECT-SENDYMD
                   MOVE    "key2"              TO  WRK-PATH
           END-EVALUATE
      *
           MOVE    SPA-HOSPNUM     TO  INFECT-HOSPNUM
           MOVE    INFECT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-INFECT-FET-SEC
           ELSE 
               MOVE    1               TO  FLG-INFECT
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL ( FLG-INFECT  =   1 )
                   OR    ( IDX         >=  100 )
               IF      IDY         >=  SPA-MINF-STR
                   ADD     1                   TO  IDX
                   EVALUATE    SPA-MINF-FLG
                       WHEN    1
                           MOVE    INFECT-SENDYMD  TO  WRK-SYMD
                       WHEN    2
                           MOVE    INFECT-SRYYMD   TO  WRK-SYMD
                   END-EVALUATE
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD1(5:5)
                                               TO  SPA-MINF-TYMD(IDX)
                   IF      INFECT-DISEASE  =   1
                       MOVE    "インフルエンザ"
                                               TO  SPA-MINF-TBYOGEN(IDX)
                   ELSE
                       MOVE    INFECT-DISEASE  TO  SPA-MINF-TBYOGEN(IDX)
                   END-IF
                   PERFORM 900-PTINF-READ-SEC
                   MOVE    PTINF-NAME          TO  SPA-MINF-TNAME(IDX)
                   EVALUATE    PTINF-SEX
                       WHEN    "1"
                           MOVE    "男"        TO  SPA-MINF-TSEX(IDX)
                       WHEN    "2"
                           MOVE    "女"        TO  SPA-MINF-TSEX(IDX)
                   END-EVALUATE
                   IF      INFECT-SRYCD    NOT =   SPACE
                       MOVE    "○"        TO  SPA-MINF-TSHINRYO(IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-MINF-TSHINRYO(IDX)
                   END-IF
                   IF      INFECT-MEDSRYCD NOT =   SPACE
                       MOVE    "○"        TO  SPA-MINF-TYAKUZAI(IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-MINF-TYAKUZAI(IDX)
                   END-IF
                   PERFORM 900-BYOMEI-READ-SEC
                   MOVE    BYO-BYOMEI          TO  SPA-MINF-TBYOMEI(IDX)
                   MOVE    INFECT-BYOSRYYMD    TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD1    TO  SPA-MINF-TSTRYMD(IDX)
               END-IF
               PERFORM 900-INFECT-FET-SEC
           END-PERFORM
      *
           MOVE    IDX                 TO  SPA-MINF-MAX
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      FLG-INFECT      =   ZERO
               MOVE    ZERO            TO  SPA-MINF-END
           ELSE
               MOVE    1               TO  SPA-MINF-END
           END-IF
      *
           .
       320-LIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-MID1-SET-SEC            SECTION.
      *
           IF      SPA-MID1-FLG        =   "OK"
               CONTINUE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       330-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        表示切替
               WHEN    "CLICKED"   ALSO    "B04"
                   PERFORM 210-KIRIKAE-SEC
      *        前日
               WHEN    "CLICKED"   ALSO    "B05"
                   PERFORM 210-ZENJITU-SEC
      *        前頁
               WHEN    "CLICKED"   ALSO    "B06"
                   PERFORM 210-ZENPAGE-SEC
      *        次頁
               WHEN    "CLICKED"   ALSO    "B07"
                   PERFORM 210-JIPAGE-SEC
      *        次日
               WHEN    "CLICKED"   ALSO    "B08"
                   PERFORM 210-JIJITU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        行選択
               WHEN    "ACTIVATE"  ALSO    "SRYYMD"
                   PERFORM 201-SRYYMD-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日処理
      *****************************************************************
       201-SRYYMD-SEC              SECTION.
      *
           MOVE    MINF-SRYYMD     TO  SPA-MINF-SRYYMD
      *
           IF      MINF-SRYYMD         =   SPACE
               GO  TO  201-SRYYMD-EXT
           END-IF
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    SPA-MINF-SRYYMD TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-MINF-TAISYOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-TAISYOYMD
               MOVE    SPACE           TO  SPA-MINF-SRYYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       201-SRYYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MINF"          TO  SPA-MOTOPG
      *
      *    MOVE    SPACE           TO  SPA-WORK
      *    INITIALIZE                  SPA-KYOTUKEY
      *                                SPA-KYOTU-SET
      *    MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    切替処理
      *****************************************************************
       210-KIRIKAE-SEC             SECTION.
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    2               TO  SPA-MINF-FLG
               WHEN    2
                   MOVE    1               TO  SPA-MINF-FLG
           END-EVALUATE
      *
           MOVE    1               TO  SPA-MINF-STR
      *
           PERFORM 320-LIST-SET-SEC
      *
           MOVE    1               TO  FLG-LIST-ROW
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       210-KIRIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日処理
      *****************************************************************
       210-ZENJITU-SEC             SECTION.
      *
           MOVE    SPA-NAI-TAISYOYMD   TO  WRK-SYMD
           PERFORM 31012-ZENJITU-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-TAISYOYMD
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       210-ZENJITU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       210-ZENPAGE-SEC             SECTION.
      *
           COMPUTE SPA-MINF-STR
               =   SPA-MINF-STR     -   100
           PERFORM 320-LIST-SET-SEC
      *
           .
       210-ZENPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       210-JIPAGE-SEC              SECTION.
      *
           COMPUTE SPA-MINF-STR
               =   SPA-MINF-STR     +   100
           PERFORM 320-LIST-SET-SEC
      *
           .
       210-JIPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日処理
      *****************************************************************
       210-JIJITU-SEC              SECTION.
      *
           MOVE    SPA-NAI-TAISYOYMD   TO  WRK-SYMD
           PERFORM 31012-JIJITU-HEN-SEC
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-MINF-TAISYOYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-TAISYOYMD
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 320-LIST-SET-SEC
           END-IF
      *
           .
       210-JIJITU-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "MINF     "         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    MINF-ROW        TO  WRK-LIST-ROW
      *
           INITIALIZE  MINF
      *
           EVALUATE    FLG-LIST-ROW
               WHEN    1
                   MOVE    ZERO            TO  MINF-ROW
               WHEN    OTHER
                   MOVE    WRK-LIST-ROW    TO  MINF-ROW
           END-EVALUATE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX             >   SPA-MINF-MAX
               MOVE    SPA-MINF-TYMD(IDX)      TO  MINF-YMD(IDX)
               MOVE    SPA-MINF-TBYOGEN(IDX)   TO  MINF-BYOGEN(IDX)
               MOVE    SPA-MINF-TNAME(IDX)     TO  MINF-NAME(IDX)
               MOVE    SPA-MINF-TSEX(IDX)      TO  MINF-SEX(IDX)
               MOVE    SPA-MINF-TSHINRYO(IDX)  TO  MINF-SHINRYO(IDX)
               MOVE    SPA-MINF-TYAKUZAI(IDX)  TO  MINF-YAKUZAI(IDX)
               MOVE    SPA-MINF-TBYOMEI(IDX)   TO  MINF-BYOMEI(IDX)
               MOVE    SPA-MINF-TSTRYMD(IDX)   TO  MINF-STRYMD(IDX)
           END-PERFORM
      *
           MOVE    SPA-MINF-MAX    TO  MINF-COUNT
      *
           EVALUATE    SPA-MINF-FLG
               WHEN    1
                   MOVE    "診療日"    TO  MINF-LABEL
               WHEN    2
                   MOVE    "報告日"    TO  MINF-LABEL
           END-EVALUATE
      *
           MOVE    SPA-MINF-TAISYOYMD  TO  MINF-TAISYOYMD
           MOVE    SPA-MINF-SRYYMD     TO  MINF-SRYYMD
      *
           IF      SPA-MINF-STR    =   1
               MOVE    WIDGET-INSENSITIVE  TO  MINF-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  MINF-B06-STATE
           END-IF
      *
           IF      SPA-MINF-END    =   1
               MOVE    WIDGET-INSENSITIVE  TO  MINF-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  MINF-B07-STATE
           END-IF
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
           END-EVALUATE
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
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "日付が正しくありません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MERR
           MOVE    SPA-ERRCD           TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  MERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "MINF"              TO  SPA-MOTOPG
           MOVE    "MERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
     *****************************************************************
      *    前日変換処理
      *****************************************************************
       31012-ZENJITU-HEN-SEC       SECTION.
      *
      *    前日
           MOVE    SPACE           TO  STS-AREA-DAY
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
           MOVE    -1              TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                        STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
      *
           .
       31012-ZENJITU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日変換処理
      *****************************************************************
       31012-JIJITU-HEN-SEC        SECTION.
      *
      *    次日
           MOVE    SPACE           TO  STS-AREA-DAY
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
           MOVE    1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
      *
           .
       31012-JIJITU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ読み込み
      *****************************************************************
       900-INFECT-FET-SEC          SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_infection" TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INFECT-REC
               MOVE    ZERO            TO  FLG-INFECT
           ELSE
               MOVE    1               TO  FLG-INFECT
           END-IF
      *
           .
       900-INFECT-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者名読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           MOVE    INFECT-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    INFECT-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    SPACE           TO  BYOMEI-REC
           MOVE    INFECT-BYOMEICD TO  BYO-BYOMEICD
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_byomei"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
               END-IF
           ELSE
               MOVE    1               TO  FLG-BYOMEI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
