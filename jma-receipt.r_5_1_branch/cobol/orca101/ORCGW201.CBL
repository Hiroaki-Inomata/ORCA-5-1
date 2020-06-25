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
       PROGRAM-ID.                 ORCGW201.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 入院料加算設定登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/03/08    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W20SCR-SPA".
      *    画面用ＳＰＡ
           COPY    "W25SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
           03  FLG-KENSAKU         PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-HIT             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDXK                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-SST             PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SST             PIC 9(05).
           03  CNT-STR             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(70).
      *
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-KBNCD               PIC 9(08).
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-SSTKJNCD-G.
               05  WRK-SSTKJNCD-OCC                OCCURS 10.
                   07  WRK-SSTKJNCDSEP             PIC X(01).
                   07  WRK-SSTKJNCD                PIC X(04).
                   07  WRK-SSTKJNCD-R              REDEFINES
                       WRK-SSTKJNCD.
                       09  WRK-SSTKJNCD9           PIC 9(04).
           03  WRK-KBN1-G.
               05  WRK-KBN1                        PIC 9(01).
           03  WRK-KBN2-G.
               05  WRK-KBN2                        PIC 9(02).
           03  WRK-PNT                 PIC 9(05).
      *
       01  SSTKJN-AREA.
           03  SSTKJN-VAL.
               05  SSTKJN-VAL-OCC                  PIC X(500)
                                                   OCCURS  20.
           03  SSTKJN-R    REDEFINES       SSTKJN-VAL.
               05  SSTKJN-CD                       PIC X(01)
                                                   OCCURS  10000.
      *
      *    コラムリスト位置
           03  WRK-W201-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    施設基準
           COPY  "CPSK1006.INC".
      *    病棟管理
           COPY  "CPSK5001.INC".
      *    点数マスタ
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院料加算チェックサブ
      *    COPY   "CPORCSNYUKSNCHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
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
           MOVE    SPA-W20FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
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
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W201"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
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
           WHEN    "W25"
                   PERFORM 320-W25SPASET-SEC
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                       PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-W25SPASET-SEC           SECTION.
      *
           IF    ( SPA-W25-SRYCD   NOT =   SPACE )
               MOVE    SPA-W25-SRYCD   TO  SPA-W201-KSNCD2
               PERFORM 22022-KSNCD2-CHK-SEC
           ELSE
               MOVE    2               TO  SPA-W201-CUR
           END-IF 
           .
       320-W25SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            更新
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2201-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W201-CUR
                   END-IF
               WHEN    "1003"
      *            削除
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W201-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W201-AREA
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-W201-LIST-G
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "9000"          TO  SPA-ERRCD
               GO  TO  310-SPASET-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  (IDX             >=  10)
               ADD     1                   TO  IDX
               IF      SYS-5001-KSNCD2 (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  SPA-W201-LIST-MAX
                   MOVE    IDX             TO  SPA-W201-TRENNUM (IDX)
                   MOVE    SYS-5001-KSNCD2 (IDX)
                                           TO  SPA-W201-TKSNCD2 (IDX)
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
                   MOVE    SYS-5001-KSNCD2 (IDX)
                                           TO  TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOSTYMD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_tensu" TO  MCP-TABLE
                       MOVE    "key"       TO  MCP-PATHNAME
                       PERFORM 900-TENSU-READ-SEC
                       IF    FLG-TENSU     =   ZERO
                           MOVE   MCPDATA-REC
                                           TO  TNS-TENSU-REC
                           MOVE   TNS-NAME TO  SPA-W201-TKSNNM2(IDX)
                       ELSE
                           MOVE   SPACE    TO  SPA-W201-TKSNNM2(IDX)
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-TENSU
                   END-IF
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               ELSE
                   MOVE    11             TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-W201-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W201-CUR
           ELSE
               MOVE    1                   TO  SPA-W201-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W201-ROW
           ELSE
               MOVE    W201-MEILIST-ROW    TO  WRK-W201-ROW
           END-IF
      *
           MOVE    SPACE               TO  W201
      *    コラムリストの位置指定
           INITIALIZE                      W201
           MOVE    ZERO                TO  W201-MEILIST-ROW
           IF      WRK-W201-ROW    NOT =   ZERO
               MOVE    WRK-W201-ROW    TO  W201-MEILIST-ROW 
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W201-LIST-MAX
               MOVE    SPA-W201-TRENNUM (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W201-TRENNUM (IDX)
               MOVE    SPA-W201-TKSNCD2(IDX)
                                           TO  W201-TKSNCD2 (IDX)
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SPA-W201-TKSNCD2(IDX)
                                           TO  TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
                   IF    FLG-TENSU         =   ZERO
                       MOVE   MCPDATA-REC  TO  TNS-TENSU-REC
                           MOVE   TNS-NAME TO  SPA-W201-TKSNNM2(IDX)
                                               W201-TKSNNM2 (IDX)
                   ELSE
                       MOVE   SPACE        TO  SPA-W201-TKSNNM2(IDX)
                                               W201-TKSNNM2 (IDX)
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      SPA-W201-SELNUM     =   SPA-W201-TRENNUM (IDX)
                   MOVE    "T"             TO  W201-MEILIST-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  W201-MEILIST-ROW
                   MOVE    2               TO  W201-MEILIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  W201-MEILIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-W201-LIST-MAX    TO  W201-MEILIST-COUNT
      *
           MOVE    SPA-W201-SELNUM      TO  W201-SELNUM
           MOVE    SPA-W201-KSNCD2      TO  W201-KSNCD2
           MOVE    SPA-W201-KSNNM2      TO  W201-KSNNM2
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
           IF      SPA-W201-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-W201-CUR
                   WHEN    "KSNCD2"
                       MOVE    99                  TO  SPA-W201-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W201-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "KSNCD2"            TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLERA-SYORI-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-DELETE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-TOUROKU-SEC
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
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "MEILIST"
                   PERFORM 2001-MEILIST-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W201-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
           PERFORM 2002-SELNUM-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2202-KSNCD2-CHK-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       2001-MEILIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-W201-LIST-MAX
              IF      W201-MEILIST-SELECT (IDX)    =  "T"
                  IF       IDX                 =   SPA-W201-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W201-SELNUM
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-MEILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-W201-SELNUM     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W201-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-W201-SELNUM
           END-IF
      *
           IF      (SPA-W201-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W201-LIST-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W201-TRENNUM (IDX)   =   SPA-W201-SELNUM
                       MOVE    1                   TO  FLG-CHK
                       IF      FLG-SELNUM          =   1
      *                    明細編集
                           PERFORM 20021-MEISAI-HENSYU-SEC
                       END-IF
                       MOVE    SPA-W201-SELNUM     TO
                                                   SPA-W201-MAE-SELNUM
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W201-CUR
               END-IF
           END-IF
      *
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20021-MEISAI-HENSYU-SEC               SECTION.
      *
           MOVE    SPA-W201-TRENNUM (IDX)   TO  SPA-W201-SELNUM
           MOVE    SPA-W201-TKSNCD2 (IDX)   TO  SPA-W201-KSNCD2
           MOVE    SPA-W201-TKSNNM2 (IDX)   TO  SPA-W201-KSNNM2
      *
           .
       20021-MEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           IF      W201-SELNUM         =   SPA-W201-MAE-SELNUM
               MOVE    ZERO                TO  FLG-SELNUM
           ELSE
               MOVE    1                   TO  FLG-SELNUM
           END-IF
           MOVE    W201-SELNUM          TO  SPA-W201-SELNUM
           MOVE    W201-KSNCD2          TO  SPA-W201-KSNCD2
           MOVE    W201-KSNNM2          TO  SPA-W201-KSNNM2
      *
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コードチェック処理
      *****************************************************************
       2202-KSNCD2-CHK-SEC            SECTION.
      *
           IF    ( SPA-W201-KSNCD2     =   SPACE )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-KENSAKU
               MOVE    SPACE           TO  WRK-INPUTCD
      *
      *        "/" 入力の時、検索画面へ
               IF    ( SPA-W201-KSNCD2 (1 : 1)   =  "/" )
                   MOVE    1           TO  FLG-KENSAKU
               ELSE
                   PERFORM 22021-KSNCD2-CHK-SEC
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( FLG-KENSAKU =   ZERO )
                       PERFORM 22022-KSNCD2-CHK-SEC
                   ELSE
                       PERFORM 409-KENSAKU-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       2202-KSNCD2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コードチェック処理
      *****************************************************************
       22021-KSNCD2-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  FLG-KENSAKU
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-W201-KSNCD2     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO
                                       SPA-W201-KSNCD2
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1           TO  FLG-KANJI
                   MOVE    SPACE       TO  SPA-W201-KSNCD2(9:)
               ELSE
                   MOVE    ZERO        TO  FLG-KANJI
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *        半角で９文字の時、点数マスタチェックへ
               IF     (WRK-CD-MCNT         =   9  )  AND
                      (FLG-KANJI           =   ZERO )  AND
                      (SPA-W201-KSNCD2 (1 : 9 )
                                       IS  NUMERIC  )
                   CONTINUE
               ELSE
                   MOVE    1          TO  FLG-KENSAKU
                   MOVE    SPA-W201-KSNCD2
                                      TO  WRK-INPUTCD
               END-IF
           END-IF
      *
           .
       22021-KSNCD2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コードチェック処理
      *****************************************************************
       22022-KSNCD2-CHK-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-W201-KSNCD2     TO  TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
               IF    FLG-TENSU         =   ZERO
                   MOVE   MCPDATA-REC  TO  TNS-TENSU-REC
                   MOVE   TNS-NAME     TO  SPA-W201-KSNNM2
               ELSE
                   MOVE   SPACE        TO  SPA-W201-KSNNM2
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               PERFORM 220221-KSNCD2-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 220221-SSTKJN-CHK-SEC
               END-IF
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-W201-CUR
           END-IF
      *
           .
       22022-KSNCD2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算コードチェック処理
      *****************************************************************
       220221-KSNCD2-CHK-SEC           SECTION.
      *
          IF     ( TNS-HOSPSRYKBN      =   ZERO )
              CONTINUE
          ELSE
              IF     ( TNS-HOSPSRYKBN  =  1 )
               AND   ( SPA-HOSPSBT     =  2 )
                  MOVE    "0005"       TO  SPA-ERRCD
              END-IF
              IF     ( TNS-HOSPSRYKBN  =  2)
                AND  ( SPA-HOSPSBT     =  1)
                  MOVE    "0006"       TO  SPA-ERRCD
              END-IF
          END-IF
      *
           MOVE    ZERO        TO  CNT-STR
           INSPECT TNS-NAME    TALLYING   CNT-STR
                   FOR     ALL "加算"
      *
          IF    ( SPA-ERRCD        =   SPACE )
              IF    ( TNS-NYUTENTTLKBN =   903 OR 920 )
               AND (( TNS-DATAKBN  =   2 )
                 OR ( CNT-STR      >   0 ))
                  CONTINUE
              ELSE
                  MOVE    "0007"       TO  SPA-ERRCD
              END-IF
          END-IF
      *
          IF    ( SPA-ERRCD        =   SPACE )
              IF    ( TNS-MICMESUKSN   =   3 OR 4 OR 5 )
                  MOVE    "0008"       TO  SPA-ERRCD
              END-IF
          END-IF
      *
      *    INITIALIZE  SNYUKSNCHK-LNK
      *    MOVE    SPA-GMN-W20-SBT             TO  SNYUKSNCHK-BTUSBT
      *    MOVE    SPA-NAI-W20-KHNSRYCD        TO  SNYUKSNCHK-KHN-SRYCD
      *    IF    ( SPA-GMN-W20-TOKNYUIN    NOT =   SPACE )
      *        MOVE    SPA-NAI-W20-TOKNYUIN    TO  SNYUKSNCHK-BTU-TOKKBN
      *    END-IF
      *    MOVE    SPA-GMN-W20-RYOYOKBN        TO  SNYUKSNCHK-RYOYOKBN
      *    MOVE    SPA-W201-KSNCD2             TO  SNYUKSNCHK-KSN-SRYCD
      *    MOVE    SPA-NAI-W20-YUKOSTYMD       TO  SNYUKSNCHK-SRYYMD
      *    CALL    "ORCSNYUKSNCHK"         USING
      *                                    SNYUKSNCHK-LNK
      *                                    SPA-AREA
      *    IF    ( SNYUKSNCHK-ERR      NOT =   ZERO )
      *           MOVE    "0009"       TO  SPA-ERRCD
      *    END-IF
      *
           IF    ( SPA-ERRCD        NOT =   SPACE )
                MOVE    2               TO  SPA-W201-CUR
           END-IF
      *
           .
       220221-KSNCD2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       220221-SSTKJN-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SSTKJN-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1006"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1006-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1006-KBNCD (2:)         =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "1" )
                AND  ( SYS-1006-KBNCD  <=  "9" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN1-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN1)
               WHEN  ( SYS-1006-KBNCD (2:1)    NOT =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "10" )
                AND  ( SYS-1006-KBNCD  <=  "19" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN2-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN2)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1006-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO            TO  FLG-HIT
                                       CNT-SST
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   10 )
                    OR   ( FLG-HIT     =   1 )
      *
               IF    ( TNS-SSTKIJUNCD (IDX5)   >   ZERO )
                   COMPUTE CNT-SST =   CNT-SST +   1
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  WRK-SSTKJNCD9(CNT-SST)
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  IDX-SST
                   IF    ( SSTKJN-CD (IDX-SST) =   "1" )
                       MOVE    1   TO  FLG-HIT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SST     >   ZERO )
               IF    ( FLG-HIT =   ZERO )
                   MOVE    "0015"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-W201-CUR
               END-IF
           END-IF
      *
           .
       220221-SSTKJN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-W25-AREA
           MOVE    "2"                 TO  SPA-W25-TYPE
      *
           MOVE    SPA-SYSYMD              TO  SPA-W25-SYSYMD
           IF    ( SPA-SYSYMD              <   SPA-NAI-W20-YUKOSTYMD )
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  SPA-W25-SYSYMD
           END-IF
           IF    ( SPA-SYSYMD              >   SPA-NAI-W20-YUKOEDYMD )
               MOVE    SPA-NAI-W20-YUKOEDYMD  TO  SPA-W25-SYSYMD
           END-IF
      *
      *
           INITIALIZE                  SPA-W25GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        診療行為で検索（オリジナルのＺ９８は内服）
               MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *        MOVE    "3"                 TO  SPA-W25-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-W25-INPUTCD
               MOVE    "1"                 TO  SPA-W25-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-W25-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-W25-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-W25-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-W25-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-W25-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-W25-SET-SEC
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-W25-GMN-MAX     =   ZERO )  AND
                      (SPA-W25-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-W25GMN-AREA
                   MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *            画面編集
                   PERFORM 41011-W25-SET-SEC
               END-IF
           ELSE
      *        画面編集
               MOVE    "8"                 TO  SPA-W25-GMN-SYORI
               PERFORM 41011-W25-SET-SEC
           END-IF
      *
           MOVE    "W201"              TO  SPA-MOTOPG
           MOVE    "W25"               TO  SPA-SAKIPG
           MOVE    "W201"              TO  SPA-W25-MOTOPG
      *
           IF      SPA-W25-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "W25    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-W25-SET-SEC                           SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-W25-GMN-AREA    TO  SPA-W25-KYOUTU
           MOVE    SPA-W20FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW25"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W20FREE
           MOVE    SPA-W25-KYOUTU      TO  SPA-W25-GMN-AREA
      *
           .
       41011-W25-SET-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLERA-SYORI-SEC                   SECTION.
      *
           INITIALIZE                  SPA-W201-GMN-G
           MOVE    ZERO                TO  SPA-W201-MAE-SELNUM
      *
           IF      SPA-W201-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W201-CUR
           ELSE
               MOVE    1                   TO  SPA-W201-CUR
           END-IF
      *
           .
       220-CLERA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       230-DELETE-SEC                   SECTION.
      *
           PERFORM 2002-SELNUM-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-W201-SELNUM     >   ZERO
                   MOVE    "1003"          TO  SPA-WIDCD
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W201-CUR
               END-IF
           END-IF
      *
           .
       230-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DELETE-SYORI-SEC                   SECTION.
      *
           IF    SPA-W201-SELNUM       NOT =   ZERO
               MOVE    SPACE           TO  SPA-W201-TKSNCD2
                                                 (SPA-W201-SELNUM)
           END-IF
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI         NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
               GO  TO  2201-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           MOVE    SPACE               TO  SYS-5001-KSN2
      *
           MOVE    ZERO                TO  IDX2
           PERFORM    VARYING   IDX   FROM   1  BY  1
             UNTIL   (IDX       >   10)
               IF   SPA-W201-TKSNCD2(IDX)   NOT =    SPACE
                  ADD     1            TO  IDX2
                  MOVE    SPA-W201-TKSNCD2(IDX)
                                       TO  SYS-5001-KSNCD2(IDX2)
               END-IF
           END-PERFORM
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD    TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD      TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                       TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC              NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
           END-IF
      *
           IF   SPA-ERRCD    =    SPACE
                PERFORM   310-SPASET-SEC
           END-IF
      *
           .
       2301-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       220-TOUROKU-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *    更新前
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2201-ALL-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2201-TOUROKU-SYORI-SEC
           END-IF
      *
           .
       220-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       2201-ALL-CHK-SEC                SECTION.
      *
           IF      SPA-W201-KSNCD2     =   SPACE
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-W201-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF  SPA-W201-TKSNCD2(10)    NOT =  SPACE
                 MOVE    "0011"          TO  SPA-ERRCD
                 MOVE    2               TO  SPA-W201-CUR
             END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF  SPA-W201-SELNUM   NOT =   ZERO
                 MOVE    "0014"          TO  SPA-ERRCD
                 MOVE    1               TO  SPA-W201-CUR
             END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(1)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(2)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(3)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(4)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(5)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(6)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(7)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(8)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(9)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(10))
                 MOVE    "0012"          TO  SPA-ERRCD
                 MOVE    2               TO  SPA-W201-CUR
             END-IF
           END-IF
           .
       2201-ALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SYORI-SEC                   SECTION.
      *
           IF    SPA-W201-SELNUM       =   ZERO
                 PERFORM    VARYING    IDX   FROM  1   BY   1
                   UNTIL   (IDX     >   10)  OR
                           (SPA-W201-TKSNCD2(IDX)   =   SPACE)
                    CONTINUE
                 END-PERFORM
           END-IF
           MOVE     SPA-W201-KSNCD2    TO  SPA-W201-TKSNCD2(IDX)
           MOVE     IDX                TO  SPA-W201-TRENNUM(IDX)
           ADD      1                  TO  SPA-W201-LIST-MAX
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI         NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
               GO  TO  2201-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           PERFORM    VARYING   IDX   FROM   1  BY  1
             UNTIL   (IDX       >   10)  OR
                     (SPA-W201-TKSNCD2(IDX)  =  SPACE)
             MOVE    SPA-W201-TKSNCD2(IDX)
                                       TO  SYS-5001-KSNCD2(IDX)
           END-PERFORM
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD    TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD      TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                       TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC              NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
           END-IF
      *
           IF   SPA-ERRCD    =    SPACE
                PERFORM   220-CLERA-SYORI-SEC
           END-IF
           .
       2201-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ編集処理
      *****************************************************************
       22011-DBHENSYU-SEC                   SECTION.
      *
           .
       22011-DBHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W20"               TO  SPA-SAKIPG
           MOVE    "W201"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W20"               TO  MCP-WINDOW
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W201"              TO  MCP-WINDOW.
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
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE   "入院料加算コードが点数マスタに存在しません。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE   "入院料加算コードを正しく入力してください。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE   "病院のみ算定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE   "診療所のみ算定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE   "入院料加算のみ登録が可能です。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE   "減算コードは登録できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    SPACE       TO  SPA-ERRMSG
                   STRING  "入院料に対して"
                       "算定できない加算です。"
                                       DELIMITED   BY  SIZE
                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   MOVE    "入院料加算コードが未設定です。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "入院料加算コードの設定は１０件が上限です。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "入院料加算コードが重複しています。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入退院登録画面で設定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "選択された加算は追加できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    SPACE       TO  SPA-ERRMSG
                   MOVE    1           TO  WRK-PNT
                   STRING  "必要な施設基準が設定されていません／"
                           DELIMITED   BY  SIZE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
                   PERFORM VARYING IDX-SST FROM    1   BY  1
                           UNTIL ( IDX-SST >   CNT-SST )
      *
                       STRING  WRK-SSTKJNCD-OCC  (IDX-SST)
                               DELIMITED   BY  SIZE
                       INTO    SPA-ERRMSG
                       WITH    POINTER         WRK-PNT
                       END-STRING
                   END-PERFORM
               WHEN    "9000"
                   MOVE    "病棟情報が取得できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "更新エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W201"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "入院料加算を追加します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "入院料加算を削除します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W201"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC
                                   TO  SYSKANRI-REC
               ELSE
                   MOVE    1       TO  FLG-SYSKANRI
                   INITIALIZE          SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC
                                   TO  SYSKANRI-REC
               ELSE
                   MOVE    1       TO  FLG-SYSKANRI
                   INITIALIZE          SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM  920-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込処理
      *****************************************************************
       900-TENSU-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-READ-EXT.
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
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
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
      *
