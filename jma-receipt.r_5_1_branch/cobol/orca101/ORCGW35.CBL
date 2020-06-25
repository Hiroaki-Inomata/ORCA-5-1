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
       PROGRAM-ID.                     ORCGW35.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 外来迅速検体検査情報（Ｗ３５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/08    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W35.
           03  SPA-W35-AREA.
               05  SPA-GMN-CUR             PIC 9(02).
               05  SPA-SYORIFLG            PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
      *
                   07  SPA-GMN-DATA-TBL.
                       09  SPA-GMN-DATA-TBLO            OCCURS  110.
                           11  SPA-GMN-DNUM            PIC 9(03).
                           11  SPA-GMN-DSRYCD          PIC X(09).
                           11  SPA-GMN-DNAME           PIC X(80).
                       09  SPA-DATA-MAX                PIC 9(03).
                   07  SPA-GMN-SELNUM1             PIC 9(03).
      *
                   07  SPA-GMN-KENSA-TBL.
                       09  SPA-GMN-KENSATBLO           OCCURS  110.
                           11  SPA-GMN-KNUM            PIC 9(03).
                           11  SPA-GMN-KSRYCD          PIC X(09).
                           11  SPA-GMN-KNAME           PIC X(80).
                       09  SPA-KENSA-MAX                PIC 9(03).
                   07  SPA-GMN-SELNUM2             PIC 9(03).
      *
               05  SPA-NAI-AREA.
                   09  SPA-NAI-TBL.
                       11  SPA-NAI-KENSATBLO           OCCURS  110.
                           13  SPA-NAI-SRYCD           PIC X(09).
                           13  SPA-NAI-NAME            PIC X(80).
                       11  SPA-NAI-MAX                 PIC 9(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
           03  FLG-TENSU                          PIC 9(01).
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-SYORI-END                      PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-MCP-WIDGET      PIC X(64).
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
           03  WRK-KBNCD           PIC 9(01).
           03  WRK-EDYUKYMD        PIC X(08).
           03  WRK-SELNUM          PIC 9(03).
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
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    外来迅速検体検査算定情報
           COPY  "CPSK1016.INC".
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    検体検査迅速実施加算対象検査テーブル
           COPY    "CMGAIKENSATBL.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
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
           COPY    "ORCA101SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W35
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W35             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK 
           MOVE    SPA-AREA            TO  SPA-COMMON
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
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "W35    "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
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
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-W35
           INITIALIZE                  SPA-W35-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
      *    外来迅速検体検査テーブル編集
           PERFORM 3101-GAIKENSA-TBL-SEC
      *
      *    外来迅速検体検査算定情報
           MOVE    SPACE               TO  SYS-1016-REC
           INITIALIZE                      SYS-1016-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
           MOVE    "1016"              TO  SYS-1016-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-EDYUKYMD
           MOVE    SYS-1016-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *    対象のシステム管理がない場合、新規とする
           IF      FLG-SYSKANRI        =   1
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               PERFORM 3102-SINKI-CHK-SEC
           ELSE
      *
               MOVE    ZERO                TO  IDX
               PERFORM     UNTIL      (FLG-SYSKANRI   =   1    )
                               OR     (IDX            >=  110  )
                   MOVE    MCPDATA-REC         TO  SYS-1016-REC
      *            テーブル編集
                   PERFORM 3103-TBL-HENSYU-SEC
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
      *    検査一覧編集
           PERFORM 3109-KENSATBL-HEN-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      **** MOVE    1                   TO  SPA-GMN-PAGE
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       3103-TBL-HENSYU-SEC            SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       55
               IF      SYS-1016-SRYCD  (IDY)   NOT =   SPACE
                   ADD     1               TO  IDX
                   MOVE    IDX             TO  SPA-GMN-DNUM   (IDX)
                   MOVE    SYS-1016-SRYCD  (IDY)
                                           TO  SPA-GMN-DSRYCD (IDX)
                   MOVE    IDX             TO  SPA-DATA-MAX
               END-IF
           END-PERFORM
           .
       3103-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査算定情報新規編集処理
      *****************************************************************
       3102-SINKI-CHK-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-SYORIFLG
      *    外来迅速検体検査算定情報
           MOVE    SPACE               TO  SYS-1016-REC
           INITIALIZE                      SYS-1016-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
           MOVE    "1016"              TO  SYS-1016-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-EDYUKYMD
           MOVE    SYS-1016-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-EDYUKYMD
           PERFORM     UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1016-REC
               IF      SPA-W01-STYUKYMD    >   SYS-1016-EDYUKYMD
                   IF      WRK-EDYUKYMD        <   SYS-1016-EDYUKYMD
      *                別の日があった場合、クリア
                       MOVE    ZERO                TO  IDX
                       INITIALIZE                  SPA-GMN-DATA-TBL
                       MOVE    SYS-1016-EDYUKYMD   TO  WRK-EDYUKYMD
                   END-IF
      *            前の日付あり（新規）
                   MOVE    1               TO  SPA-SYORIFLG
      *            テーブル編集
                   PERFORM 3103-TBL-HENSYU-SEC
      *
                   MOVE    SYS-1016-EDYUKYMD   TO  WRK-EDYUKYMD
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    有効な検査のみ対象
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-DATA-MAX
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING IDZ         FROM    1   BY  1
                       UNTIL   IDZ         >   SPA-NAI-MAX
                   IF      SPA-GMN-DSRYCD (IDY)
                                           =   SPA-NAI-SRYCD(IDZ)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    SPACE               TO  SPA-GMN-DSRYCD (IDY)
               END-IF
           END-PERFORM
      *    空白行削除
           PERFORM 320-DELETE-GYO-SEC
      *
           .
       3102-SINKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    空白行削除編集処理
      *****************************************************************
       320-DELETE-GYO-SEC            SECTION.
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   110
               IF     (IDX                     <   110  ) AND
                      (SPA-GMN-DSRYCD (IDX)    =   SPACE)
                   MOVE    SPA-GMN-DATA-TBLO (IDX + 1)
                                           TO  SPA-GMN-DATA-TBLO (IDX)
                   INITIALIZE                  SPA-GMN-DATA-TBLO
                                                              (IDX + 1)
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  SPA-DATA-MAX
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   110
               IF      SPA-GMN-DSRYCD (IDX)    NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN-DNUM (IDX)
                   MOVE    IDX             TO  SPA-DATA-MAX
               END-IF
           END-PERFORM
           .
       320-DELETE-GYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査テーブル編集処理
      *****************************************************************
       3101-GAIKENSA-TBL-SEC            SECTION.
      *
           INITIALIZE                      SPA-NAI-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   KEN-TAI-MAX
               MOVE    TBL-TAI-SRYCD (IDX) TO  SPA-NAI-SRYCD (IDX)
               MOVE    TBL-TAI-SRYCD (IDX) TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  SPA-NAI-NAME  (IDX)
               END-IF
               MOVE    IDX                 TO  SPA-NAI-MAX
           END-PERFORM
           .
       3101-GAIKENSA-TBL-EXT.
           EXIT.
      *****************************************************************
      *    検査一覧編集処理
      *****************************************************************
       3109-KENSATBL-HEN-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-KENSA-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL   IDX         >   SPA-NAI-MAX
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL   IDY     >   SPA-DATA-MAX
                   IF      SPA-GMN-DSRYCD (IDY)
                                           =   SPA-NAI-SRYCD(IDX)
                       MOVE    SPA-NAI-NAME (IDX)
                                               TO  SPA-GMN-DNAME (IDY)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   ADD     1                   TO  SPA-KENSA-MAX
                   MOVE    SPA-KENSA-MAX       TO  SPA-GMN-KNUM
                                                       (SPA-KENSA-MAX)
                   MOVE    SPA-NAI-SRYCD (IDX) TO  SPA-GMN-KSRYCD
                                                       (SPA-KENSA-MAX)
                   MOVE    SPA-NAI-NAME (IDX)  TO  SPA-GMN-KNAME
                                                       (SPA-KENSA-MAX)
               END-IF
           END-PERFORM
           .
       3109-KENSATBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
      *        登録
               WHEN    "1001"
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
      *        削除
               WHEN    "1002"
               WHEN    "1004"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
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
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
      *            選択１入力
                   PERFORM 410-SELNUM1-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
      *            選択２
                   PERFORM 420-SELNUM2-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "DATALIST"
                   PERFORM 410-DATALIST-SEC
      *        行選択
               WHEN    ANY             ALSO    "KENSALIST"
                   PERFORM 420-KENSALIST-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択１入力処理
      *****************************************************************
       410-DATALIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-DATA-MAX
               IF      W35-DATALIST-SELECT (IDX)   =   "T"
                   MOVE    IDX             TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM1
               PERFORM 4101-SELNUM1-SYORI-SEC
           END-IF
           .
       410-DATALIST-EXT.
           EXIT.
      *****************************************************************
      *    行選択２入力処理
      *****************************************************************
       420-KENSALIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-KENSA-MAX
               IF      W35-KENSALIST-SELECT (IDX)  =   "T"
                   MOVE    IDX             TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM2
               PERFORM 4201-SELNUM2-SYORI-SEC
           END-IF
           .
       420-KENSALIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択１入力処理
      *****************************************************************
       410-SELNUM1-CHK-SEC       SECTION.
      *
           MOVE    W35-SELNUM1         TO  SPA-GMN-SELNUM1
           IF      SPA-GMN-SELNUM1     >=  1   AND
                                       <=  SPA-DATA-MAX
               PERFORM 4101-SELNUM1-SYORI-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
       410-SELNUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択１入力チェック処理
      *****************************************************************
       4101-SELNUM1-SYORI-SEC       SECTION.
      *
           MOVE    SPA-GMN-SELNUM1     TO  IDX
           INITIALIZE                  SPA-GMN-DATA-TBLO (IDX)
      *    空白行削除
           PERFORM 320-DELETE-GYO-SEC
      *
      *    検査一覧編集
           PERFORM 3109-KENSATBL-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM1
           .
       4101-SELNUM1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択２入力処理
      *****************************************************************
       420-SELNUM2-CHK-SEC       SECTION.
      *
           MOVE    W35-SELNUM2         TO  SPA-GMN-SELNUM2
           IF      SPA-GMN-SELNUM2     >=  1   AND
                                       <=  SPA-KENSA-MAX
               PERFORM 4201-SELNUM2-SYORI-SEC
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
           .
       420-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択２入力チェック処理
      *****************************************************************
       4201-SELNUM2-SYORI-SEC       SECTION.
      *
           MOVE    SPA-GMN-SELNUM2     TO  IDX
           ADD     1                   TO  SPA-DATA-MAX
           MOVE    SPA-DATA-MAX        TO  SPA-GMN-DNUM (SPA-DATA-MAX)
           MOVE    SPA-GMN-KSRYCD (IDX)
                                       TO  SPA-GMN-DSRYCD(SPA-DATA-MAX)
           MOVE    SPA-GMN-KNAME (IDX)
                                       TO  SPA-GMN-KNAME (SPA-DATA-MAX)
      *
      *    検査一覧編集
           PERFORM 3109-KENSATBL-HEN-SEC
           MOVE    ZERO                TO  SPA-GMN-SELNUM2
           .
       4201-SELNUM2-SYORI-EXT.
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
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
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
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W35"               TO  SPA-MOTOPG
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
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    2                   TO  SPA-GMN-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       412-TOUROKU-SEC                               SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               IF      SPA-DATA-MAX        =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    "1001"              TO  SPA-WIDCD
               END-IF
           ELSE
               IF      SPA-DATA-MAX        =   ZERO
                   MOVE    "1004"              TO  SPA-WIDCD
               ELSE
                   MOVE    "1003"              TO  SPA-WIDCD
               END-IF
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      * 
      *    外来迅速検体検査情報削除
           IF      SPA-SYORIFLG        =   ZERO
      *        更新の時のみ
               MOVE    SPACE               TO  SYS-1016-REC
               INITIALIZE                      SYS-1016-REC
               MOVE    "1016"              TO  SYS-1016-KANRICD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
               MOVE    SYS-1016-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "del11"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
      *    新規作成
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    ZERO                TO  WRK-KBNCD
           PERFORM
                   UNTIL  (FLG-SYORI-END   =   1  )
                       OR (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPACE               TO  SYS-1016-REC
               INITIALIZE                      SYS-1016-REC
               MOVE    "1016"                  TO  SYS-1016-KANRICD
               MOVE    SPA-W01-STYUKYMD        TO  SYS-1016-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-1016-EDYUKYMD
               ADD     1                       TO  WRK-KBNCD
               MOVE    WRK-KBNCD               TO  SYS-1016-KBNCD
      *
               MOVE    ZERO                    TO  IDX
               PERFORM
                       UNTIL      (IDY     >=      SPA-DATA-MAX)
                              OR  (IDX     >=      55          )
                   ADD     1                   TO  IDY
                   IF      SPA-GMN-DSRYCD (IDY)    NOT =   SPACE
                       ADD     1               TO  IDX
                       MOVE    SPA-GMN-DSRYCD (IDY)
                                               TO  SYS-1016-SRYCD(IDX)
                   END-IF
               END-PERFORM
               IF      IDY                 >=   SPA-DATA-MAX
                   MOVE    1               TO  FLG-SYORI-END
               END-IF
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
               MOVE    SYS-1016-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W35 SYSKANRI INS ERR:" MCP-RC
                            ",KEY:" SYS-1016-KEY
                   MOVE    "1003"                  TO  SPA-ERRCD
               END-IF
           END-PERFORM
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
       4901-KANRENCHK-SEC           SECTION.
      *
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
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
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1016-REC
           INITIALIZE                      SYS-1016-REC
           MOVE    "1016"              TO  SYS-1016-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1016-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
           MOVE    SYS-1016-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1016-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
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
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W35    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W35
           INITIALIZE                      W35
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATA-MAX
               MOVE    SPA-GMN-DNUM    (IDX)   TO  W35-DNO     (IDX)
               MOVE    SPA-GMN-DSRYCD  (IDX)   TO  W35-DSRYCD  (IDX)
               MOVE    SPA-GMN-DNAME   (IDX)   TO  W35-DNAME (IDX)
           END-PERFORM
           MOVE    SPA-DATA-MAX        TO  W35-DATALIST-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KENSA-MAX
               MOVE    SPA-GMN-KNUM    (IDX)   TO  W35-KNO     (IDX)
               MOVE    SPA-GMN-KSRYCD  (IDX)   TO  W35-KSRYCD  (IDX)
               MOVE    SPA-GMN-KNAME   (IDX)   TO  W35-KNAME (IDX)
           END-PERFORM
           MOVE    SPA-KENSA-MAX        TO  W35-KENSALIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM1      TO  W35-SELNUM1
           MOVE    SPA-GMN-SELNUM2      TO  W35-SELNUM2
      *
           MOVE    SPA-GMN-STYUKYMD     TO  W35-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD     TO  W35-EDYUKYMD
      *
      *    業務権限非活性処理
      **** PERFORM 510-GSRAUTH-SEC
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
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "SELNUM1"
                       MOVE    1               TO  SPA-GMN-CUR
                   WHEN    "SELNUM2"
                       MOVE    2               TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "SELNUM1"        TO  MCP-WIDGET
                WHEN   2
                    MOVE  "SELNUM2"        TO  MCP-WIDGET
                 WHEN   99
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
                   MOVE    "選択コード１入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択コード２入力エラー"    TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "登録済み検査コードを設定して下さい"
                                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W35"                TO  SPA-MOTOPG
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
                   MOVE    "登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "新規の登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします。よろしいですか？"
                                               TO  WRK-WIDMSG
               WHEN    "1004"
                   STRING  "明細がありません。"
                           "削除処理をします。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-WIDCD
               WHEN    "1002"
               WHEN    "1004"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W35"                TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-W01-EDYUKYMD    TO  TNS-YUKOSTYMD
           MOVE    SPA-W01-EDYUKYMD    TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
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

