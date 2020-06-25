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
       PROGRAM-ID.                 ORCGMUID.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 収集データアップロードユーザ設定（ＭＵＩＤ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/10/11    多々納        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
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
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-USER            PIC 9(01).
           03  FLG-DASCONFIG       PIC 9(01).
           03  FLG-JOBLOG          PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-BEDSU           PIC 9(04).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PROFILE".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
       01  SHELL-AREA.
           05  SHELL-NAME          PIC X(14)   VALUE   "das-profile.sh".
      *
      *    レセ電診療科コードテーブル
           COPY    "CMRECEDENSRYKACD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    定点設定
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *    ジョブログ
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    シェル起動
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
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
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
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
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC               SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-ERRCD
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"            TO  MCP-PUTTYPE
               MOVE   "MUID"           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
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
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
      *            確認画面より
                   PERFORM 320-MID1-SET-SEC
               WHEN    "MID2"
                   PERFORM 320-MID2-SET-SEC
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
           INITIALIZE                  SPA-MUID-AREA
           MOVE    1               TO  SPA-MUID-CUR
      *
           PERFORM 900-DASCONFIG-READ-SEC
           IF      FLG-DASCONFIG       =   ZERO
               PERFORM 3120-CONFIG-SPA-SET-SEC
           END-IF
      *
           MOVE    SPA-HOSPID      TO  SPA-MUID-HOSPID
           MOVE    SPA-SYSYMDWH    TO  SPA-MUID-SINSEIYMD
           MOVE    SPA-SYSYMD      TO  SPA-MUID-NAI-SINSEIYMD
      *
           MOVE    "1 診療所"      TO  SPA-MUID-HOSPSBT-LIST(1)
           MOVE    "2 病院（一般病院・旧総合病院）"
                                   TO  SPA-MUID-HOSPSBT-LIST(2)
           MOVE    "3 精神科病院"  TO  SPA-MUID-HOSPSBT-LIST(3)
           MOVE    "4 結核療養所"  TO  SPA-MUID-HOSPSBT-LIST(4)
           MOVE    "5 特定機能病院"
                                   TO  SPA-MUID-HOSPSBT-LIST(5)
           MOVE    5               TO  SPA-MUID-HOSPSBT-MAX
      *
           MOVE    "0 一般病院"    TO  SPA-MUID-HOSPSBT1-LIST(1)
           MOVE    "1 旧総合病院"  TO  SPA-MUID-HOSPSBT1-LIST(2)
           MOVE    2               TO  SPA-MUID-HOSPSBT1-MAX
      *
           MOVE    "1 個人"        TO  SPA-MUID-SETTI-LIST(1)
           MOVE    "2 法人"        TO  SPA-MUID-SETTI-LIST(2)
           MOVE    "3 国立"        TO  SPA-MUID-SETTI-LIST(3)
           MOVE    "4 公立"        TO  SPA-MUID-SETTI-LIST(4)
           MOVE    "5 公的"        TO  SPA-MUID-SETTI-LIST(5)
           MOVE    "6 その他"      TO  SPA-MUID-SETTI-LIST(6)
           MOVE    6               TO  SPA-MUID-SETTI-MAX
      *
      *    レセ電診療科
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX             >   TBL-RECEDENSRYKACD-MAX
               MOVE    TBL-RECEDEN-SRYKACD(IDX)
                                           TO  SPA-MUID-SYUKAL(IDX)
               MOVE    TBL-RECEDEN-SRYKANAME(IDX)
                                           TO  SPA-MUID-SYUKAMEIL(IDX)
           END-PERFORM
           MOVE    TBL-RECEDENSRYKACD-MAX  TO  SPA-MUID-SYUKA-MAX
      *
           MOVE    "0 地域医療支援病院でない"
                                   TO  SPA-MUID-CIIKI-LIST(1)
           MOVE    "1 地域医療支援病院である"
                                   TO  SPA-MUID-CIIKI-LIST(2)
           MOVE    2               TO  SPA-MUID-CIIKI-MAX
      *
           MOVE    "0 ＤＰＣ対象病院でない"
                                   TO  SPA-MUID-DPCKBN-LIST(1)
           MOVE    "1 ＤＰＣ対象病院である"
                                   TO  SPA-MUID-DPCKBN-LIST(2)
           MOVE    2               TO  SPA-MUID-DPCKBN-MAX
      *
           MOVE    "0 在宅支援診療所でない"
                                   TO  SPA-MUID-ZAITAKU-LIST(1)
           MOVE    "1 在宅支援診療所である"
                                   TO  SPA-MUID-ZAITAKU-LIST(2)
           MOVE    2               TO  SPA-MUID-ZAITAKU-MAX
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データＳＰＡセット処理
      *****************************************************************
       3120-CONFIG-SPA-SET-SEC         SECTION.
      *
           MOVE    DASCF-KANANAME      TO  SPA-MUID-KANANAME
           MOVE    DASCF-NAME          TO  SPA-MUID-NAME
           MOVE    DASCF-EMAIL         TO  SPA-MUID-EMAIL
           MOVE    DASCF-HOSPSBT1      TO  SPA-MUID-HOSPSBT
           MOVE    DASCF-HOSPSBT2      TO  SPA-MUID-HOSPSBT1
           MOVE    DASCF-BEDSUIPPAN    TO  SPA-MUID-BEDSUIPPAN
           MOVE    DASCF-BEDSURYOYO    TO  SPA-MUID-BEDSURYOYO
           MOVE    DASCF-BEDSUSEISIN   TO  SPA-MUID-BEDSUSEISIN
           MOVE    DASCF-BEDSUKEKKAKU  TO  SPA-MUID-BEDSUKEKKAKU
           MOVE    DASCF-BEDSU         TO  SPA-MUID-BEDSU
           MOVE    DASCF-SETTI         TO  SPA-MUID-SETTI
           MOVE    DASCF-SYUKA         TO  SPA-MUID-SYUKA
           MOVE    DASCF-CHIIKI        TO  SPA-MUID-CIIKI
           MOVE    DASCF-DPCKBN        TO  SPA-MUID-DPCKBN
           MOVE    DASCF-ZAITAKU       TO  SPA-MUID-ZAITAKU
           MOVE    DASCF-KAIINYM       TO  SPA-MUID-KAIINYM
           MOVE    DASCF-POST          TO  SPA-MUID-POST
           MOVE    DASCF-USERID        TO  SPA-MUID-USER
           MOVE    DASCF-PASSWORD      TO  SPA-MUID-PASSWORD
           .
       3120-CONFIG-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-MIDCD
           IF      SPA-MID1-FLG        =   "OK"
      *        申請データを出力する
               PERFORM 2120-DASCONFIG-WRITE-SEC
      *        シェルを起動する
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 212-SHELL-OUT-SEC
               END-IF
           END-IF
      *
           .
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ２）ＯＫ処理
      *****************************************************************
       320-MID2-SET-SEC            SECTION.
      *
           IF      SPA-MIDCD       NOT =   "0000"
               GO  TO  320-MID2-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MUID"          TO  SPA-MOTOPG
      *
           MOVE    "M98"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       320-MID2-SET-EXT.
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
                   PERFORM 210-BACK
      *        登録更新
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 212-TOROKU-SEC
      *        タイマーイベント
               WHEN    "CLICKED"   ALSO    "pandatimer1"
                   PERFORM 213-JYOKYO-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
           MOVE    ZERO            TO  SPA-MUID-CUR
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "KANANAME"
                   MOVE    2               TO  SPA-MUID-CUR
                   PERFORM 4102-KANANAME-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "NAME"
                   MOVE    3               TO  SPA-MUID-CUR
                   PERFORM 4102-NAME-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "EMAIL"
                   MOVE    4               TO  SPA-MUID-CUR
                   PERFORM 4102-EMAIL-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "HOSPSBT"
                   MOVE    5               TO  SPA-MUID-CUR
                   PERFORM 4102-HOSPSBT-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "HOSPSBT1"
                   MOVE    6               TO  SPA-MUID-CUR
                   PERFORM 4102-HOSPSBT1-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "BEDSUIPPAN"
                   MOVE    7               TO  SPA-MUID-CUR
               WHEN    "ACTIVATE"  ALSO    "BEDSURYOYO"
                   MOVE    8               TO  SPA-MUID-CUR
               WHEN    "ACTIVATE"  ALSO    "BEDSUSEISIN"
                   MOVE    9               TO  SPA-MUID-CUR
               WHEN    "ACTIVATE"  ALSO    "BEDSUKEKKAKU"
                   MOVE    10              TO  SPA-MUID-CUR
               WHEN    "ACTIVATE"  ALSO    "BEDSU"
                   MOVE    11              TO  SPA-MUID-CUR
               WHEN    "ACTIVATE"  ALSO    "SETTI"
                   MOVE    12              TO  SPA-MUID-CUR
                   PERFORM 4102-SETTI-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "SYUKA"
                   MOVE    13              TO  SPA-MUID-CUR
                   PERFORM 4102-SYUKA-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "CIIKI"
                   MOVE    14              TO  SPA-MUID-CUR
                   PERFORM 4102-CIIKI-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "DPCKBN"
                   MOVE    15              TO  SPA-MUID-CUR
                   PERFORM 4102-DPCKBN-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "ZAITAKU"
                   MOVE    16              TO  SPA-MUID-CUR
                   PERFORM 4102-ZAITAKU-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "KAIINYM"
                   MOVE    17              TO  SPA-MUID-CUR
                   PERFORM 4102-KAIINYM-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "POST"
                   MOVE    18              TO  SPA-MUID-CUR
                   PERFORM 4102-POST-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "USER"
                   MOVE    19              TO  SPA-MUID-CUR
                   PERFORM 4102-USER-CHK-SEC
               WHEN    "ACTIVATE"  ALSO    "PASSWORD"
                   MOVE    20              TO  SPA-MUID-CUR
                   PERFORM 4102-PASSWORD-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           MOVE    MUID-KANANAME       TO  SPA-MUID-KANANAME
           MOVE    MUID-NAME           TO  SPA-MUID-NAME
           MOVE    MUID-EMAIL          TO  SPA-MUID-EMAIL
           MOVE    MUID-HOSPSBT        TO  SPA-MUID-HOSPSBT-G
           MOVE    MUID-HOSPSBT1       TO  SPA-MUID-HOSPSBT1-G
           MOVE    MUID-BEDSUIPPAN     TO  SPA-MUID-BEDSUIPPAN
           MOVE    MUID-BEDSURYOYO     TO  SPA-MUID-BEDSURYOYO
           MOVE    MUID-BEDSUSEISIN    TO  SPA-MUID-BEDSUSEISIN
           MOVE    MUID-BEDSUKEKKAKU   TO  SPA-MUID-BEDSUKEKKAKU
           MOVE    MUID-BEDSU          TO  SPA-MUID-BEDSU
           MOVE    MUID-SETTI          TO  SPA-MUID-SETTI-G
           MOVE    MUID-SYUKA          TO  SPA-MUID-SYUKA-G
           MOVE    MUID-CIIKI          TO  SPA-MUID-CIIKI-G
           MOVE    MUID-DPCKBN         TO  SPA-MUID-DPCKBN-G
           MOVE    MUID-ZAITAKU        TO  SPA-MUID-ZAITAKU-G
           MOVE    MUID-KAIINYM        TO  SPA-MUID-KAIINYM
           MOVE    MUID-POST           TO  SPA-MUID-POST
           MOVE    MUID-USER           TO  SPA-MUID-USER
           MOVE    MUID-PASSWORD       TO  SPA-MUID-PASSWORD
      *****MOVE    MUID-HOSPID         TO  SPA-MUID-HOSPID
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           PERFORM 4102-KANANAME-CHK-SEC
           PERFORM 4102-NAME-CHK-SEC
           PERFORM 4102-EMAIL-CHK-SEC
           PERFORM 4102-HOSPSBT-CHK-SEC
           PERFORM 4102-HOSPSBT1-CHK-SEC
           PERFORM 4102-SETTI-CHK-SEC
           PERFORM 4102-SYUKA-CHK-SEC
           PERFORM 4102-CIIKI-CHK-SEC
           PERFORM 4102-DPCKBN-CHK-SEC
           PERFORM 4102-ZAITAKU-CHK-SEC
           PERFORM 4102-KAIINYM-CHK-SEC
           PERFORM 4102-POST-CHK-SEC
           PERFORM 4102-USER-CHK-SEC
           PERFORM 4102-PASSWORD-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名チェック処理
      *****************************************************************
       4102-KANANAME-CHK-SEC       SECTION.
      *
           IF    ( SPA-MUID-KANANAME   =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KANANAME-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-MUID-KANANAME
                                   TO  KANACHK-MAE-INPUT
           MOVE    50              TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   2     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-KANANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       4102-NAME-CHK-SEC           SECTION.
      *
           IF    ( SPA-MUID-NAME       =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-NAME-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-MUID-NAME   TO  KANACHK-MAE-INPUT
           MOVE    50              TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    KANACHK-MAE-INPUT   TO  SPA-MUID-NAME
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-MUID-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-MUID-NAME
           END-IF
      *
           .
       4102-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅ−ＭＡＩＬチェック処理
      *****************************************************************
       4102-EMAIL-CHK-SEC          SECTION.
      *
           IF    ( SPA-MUID-EMAIL      =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-EMAIL-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-MUID-EMAIL  TO  KANACHK-MAE-INPUT
           MOVE    256             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-EMAIL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟種別チェック処理
      *****************************************************************
       4102-HOSPSBT-CHK-SEC        SECTION.
      *
           IF    ( SPA-MUID-HOSPSBT    =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-HOSPSBT-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-HOSPSBT-MAX
               IF       SPA-MUID-HOSPSBTL(IDX) =  SPA-MUID-HOSPSBT
                    MOVE    SPA-MUID-HOSPSBT-LIST(IDX)
                                           TO  SPA-MUID-HOSPSBT-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-HOSPSBT-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-HOSPSBT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧総合病院チェック処理
      *****************************************************************
       4102-HOSPSBT1-CHK-SEC       SECTION.
      *
           IF    ( SPA-MUID-HOSPSBT1   =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-HOSPSBT1-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-HOSPSBT1-MAX
               IF       SPA-MUID-HOSPSBT1L(IDX) =  SPA-MUID-HOSPSBT1
                    MOVE    SPA-MUID-HOSPSBT1-LIST(IDX)
                                           TO  SPA-MUID-HOSPSBT1-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-HOSPSBT1-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-HOSPSBT1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    設置主体チェック処理
      *****************************************************************
       4102-SETTI-CHK-SEC          SECTION.
      *
           IF    ( SPA-MUID-SETTI      =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-SETTI-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-SETTI-MAX
               IF       SPA-MUID-SETTIL(IDX) =  SPA-MUID-SETTI
                    MOVE    SPA-MUID-SETTI-LIST(IDX)
                                           TO  SPA-MUID-SETTI-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-SETTI-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-SETTI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主たる診療科チェック処理
      *****************************************************************
       4102-SYUKA-CHK-SEC          SECTION.
      *
           IF    ( SPA-MUID-SYUKA      =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-SYUKA-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-SYUKA-MAX
               IF       SPA-MUID-SYUKAL(IDX) =  SPA-MUID-SYUKA
                    MOVE    SPA-MUID-SYUKA-LIST(IDX)
                                           TO  SPA-MUID-SYUKA-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-SYUKA-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    12              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-SYUKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    地域医療支援病院チェック処理
      *****************************************************************
       4102-CIIKI-CHK-SEC          SECTION.
      *
           IF    ( SPA-MUID-CIIKI      =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-CIIKI-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-CIIKI-MAX
               IF       SPA-MUID-CIIKIL(IDX) =  SPA-MUID-CIIKI
                    MOVE    SPA-MUID-CIIKI-LIST(IDX)
                                           TO  SPA-MUID-CIIKI-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-CIIKI-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    13              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-CIIKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＰＣチェック処理
      *****************************************************************
       4102-DPCKBN-CHK-SEC         SECTION.
      *
           IF    ( SPA-MUID-DPCKBN     =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-DPCKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-DPCKBN-MAX
               IF       SPA-MUID-DPCKBNL(IDX)  =  SPA-MUID-DPCKBN
                    MOVE    SPA-MUID-DPCKBN-LIST(IDX)
                                           TO  SPA-MUID-DPCKBN-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-DPCKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    14              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-DPCKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅支援診療所チェック処理
      *****************************************************************
       4102-ZAITAKU-CHK-SEC        SECTION.
      *
           IF    ( SPA-MUID-ZAITAKU    =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-ZAITAKU-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-MUID-ZAITAKU-MAX
               IF       SPA-MUID-ZAITAKUL(IDX) =  SPA-MUID-ZAITAKU
                    MOVE    SPA-MUID-ZAITAKU-LIST(IDX)
                                           TO  SPA-MUID-ZAITAKU-G
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-MUID-ZAITAKU-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    15              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-ZAITAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開院時期チェック処理
      *****************************************************************
       4102-KAIINYM-CHK-SEC        SECTION.
      *
           IF    ( SPA-MUID-KAIINYM    =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               MOVE    SPACE               TO  SPA-MUID-KAIINYM
               MOVE    SPACE               TO  SPA-MUID-NAI-KAIINYM
               GO  TO  4102-KAIINYM-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                  ORCSGDAYAREA
           MOVE    SPA-MUID-KAIINYM    TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-MUID-KAIINYM
               MOVE    SGDAY-SDAY          TO  SPA-MUID-NAI-KAIINYM
           ELSE
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-MUID-CUR
           END-IF
           .
      *
       4102-KAIINYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    郵便番号チェック処理
      *****************************************************************
       4102-POST-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-MUID-PREF
           MOVE    SPACE           TO  SPA-MUID-ADRS
           IF    ( SPA-MUID-POST   =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-POST-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-MUID-POST       TO  KANACHK-MAE-INPUT
           MOVE    7                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )  AND
                  (SPA-MUID-POST       NUMERIC   )
               CONTINUE
           ELSE
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    17              TO  SPA-MUID-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-MUID-PREF
           MOVE    SPACE           TO  SPA-MUID-ADRS
           PERFORM 900-ADRS-READ-SEC
           IF      FLG-ADRS        =   ZERO
               MOVE    ADRS-PREFNAME   TO  SPA-MUID-PREF
               STRING  ADRS-CITYNAME   DELIMITED BY SPACE
                       ADRS-TOWNNAME   DELIMITED BY SPACE
                                       INTO  SPA-MUID-ADRS
               END-STRING
           END-IF
      *
           .
       4102-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザーチェック処理
      *****************************************************************
       4102-USER-CHK-SEC           SECTION.
      *
           IF    ( SPA-MUID-USER       =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-USER-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-MUID-USER       TO  KANACHK-MAE-INPUT
           MOVE    7                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )  AND
                  (SPA-MUID-USER       NUMERIC   )
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    18              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-USER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パスワードチェック処理
      *****************************************************************
       4102-PASSWORD-CHK-SEC       SECTION.
      *
           IF    ( SPA-MUID-PASSWORD   =   SPACE ) OR
                 ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-PASSWORD-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-MUID-PASSWORD   TO  KANACHK-MAE-INPUT
           MOVE    100                 TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    19              TO  SPA-MUID-CUR
           END-IF
      *
           .
       4102-PASSWORD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須チェック処理
      *****************************************************************
       4103-HISSU-CHK-SEC          SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-HOSPSBT     =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF    ( SPA-MUID-HOSPSBT    =   2     ) AND
                 ( SPA-MUID-HOSPSBT1   =   SPACE )
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-SETTI       =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-SYUKA       =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    12              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-CIIKI       =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    13              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-DPCKBN      =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    14              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-ZAITAKU     =   SPACE
               MOVE    "0009"          TO  SPA-ERRCD
               MOVE    15              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-POST        =   SPACE
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    17              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-USER        =   SPACE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    18              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           IF     SPA-MUID-PASSWORD    =   SPACE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    19              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-BEDSU
           COMPUTE WRK-BEDSU   =   SPA-MUID-BEDSUIPPAN
                               +   SPA-MUID-BEDSURYOYO
                               +   SPA-MUID-BEDSUSEISIN
                               +   SPA-MUID-BEDSUKEKKAKU
           IF      WRK-BEDSU   NOT =   SPA-MUID-BEDSU
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-MUID-CUR
               GO  TO  4103-HISSU-CHK-EXT
           END-IF
      *
           .
       4103-HISSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOROKU-SEC                  SECTION.
      *
           MOVE    ZERO                TO  SPA-MUID-END
           MOVE    ZERO                TO  SPA-MUID-CUR
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *    必須チェック処理
           PERFORM 4103-HISSU-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 220-MID1SET-SEC
           END-IF
      *
           .
       212-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       220-MID1SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID1
      *
           INITIALIZE                  MID1
           MOVE    "1001"          TO  MID1-ID1CODE
           MOVE
           "申請情報（参加する）を登録（送信）しますか？"
                                   TO  MID1-ID1MSG
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MUID"          TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID1SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       230-MID2SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID2
      *
           INITIALIZE                  MID2
           MOVE    SPA-MIDCD       TO  MID2-ID1CODE
      *
           EVALUATE    SPA-MIDCD
               WHEN    "1000"
                   MOVE
       "登録作業中です。２〜３秒待ってからボタンをクリックして下さい。"
                                   TO  MID2-ID1MSG
               WHEN    "0000"
                   MOVE    "申請情報を登録（送信）しました。"
                                   TO  MID2-ID1MSG
               WHEN    "0401"
                   MOVE    "定点参加番号またはパスワードが不正です。"
                                   TO  MID2-ID1MSG
               WHEN    "0403"
                   MOVE    "医療機関ＩＤが不正です。"
                                   TO  MID2-ID1MSG
               WHEN    "0404"
                   MOVE    "ファイルが見つかりません。"
                                   TO  MID2-ID1MSG
               WHEN    "0999"
                   MOVE    "収集サーバが見つかりません。"
                                   TO  MID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MUID"          TO  SPA-MOTOPG
           MOVE    "MID2"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID2"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       230-MID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2120-DASCONFIG-WRITE-SEC        SECTION.
      *
           IF      FLG-DASCONFIG       =   ZERO
               INITIALIZE                      DASCF-REC
               MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
               MOVE    DASCF-REC           TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    SPA-MUID-NAI-SINSEIYMD
                                       TO  DASCF-SINSEIYMD
           MOVE    SPA-MUID-KANANAME   TO  DASCF-KANANAME
           MOVE    SPA-MUID-NAME       TO  DASCF-NAME
           MOVE    SPA-MUID-EMAIL      TO  DASCF-EMAIL
           MOVE    SPA-MUID-HOSPSBT    TO  DASCF-HOSPSBT1
           MOVE    SPA-MUID-HOSPSBT1   TO  DASCF-HOSPSBT2
           MOVE    SPA-MUID-BEDSUIPPAN TO  DASCF-BEDSUIPPAN
           MOVE    SPA-MUID-BEDSURYOYO TO  DASCF-BEDSURYOYO
           MOVE    SPA-MUID-BEDSUSEISIN
                                       TO  DASCF-BEDSUSEISIN
           MOVE    SPA-MUID-BEDSUKEKKAKU
                                       TO  DASCF-BEDSUKEKKAKU
           MOVE    SPA-MUID-BEDSU      TO  DASCF-BEDSU
           MOVE    SPA-MUID-SETTI      TO  DASCF-SETTI
           MOVE    SPA-MUID-SYUKA      TO  DASCF-SYUKA
           MOVE    SPA-MUID-CIIKI      TO  DASCF-CHIIKI
           MOVE    SPA-MUID-DPCKBN     TO  DASCF-DPCKBN
           MOVE    SPA-MUID-ZAITAKU    TO  DASCF-ZAITAKU
           MOVE    SPA-MUID-NAI-KAIINYM
                                       TO  DASCF-KAIINYM
           MOVE    SPA-MUID-POST       TO  DASCF-POST
           MOVE    SPA-MUID-USER       TO  DASCF-USERID
           MOVE    SPA-MUID-PASSWORD   TO  DASCF-PASSWORD
           MOVE    "1"                 TO  DASCF-AGREEMENT
           MOVE    SPA-SYSYMD          TO  DASCF-CREYMD
      *
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       2120-DASCONFIG-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       212-SHELL-OUT-SEC           SECTION.
      *
      *    シェル起動処理
           MOVE    SPACE           TO  SHELLTBL
           MOVE    SHELL-NAME      TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG2
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  SPA-MUID-DURATION
               MOVE    "1000"          TO  SPA-MIDCD
               PERFORM 230-MID2SET-SEC
           END-IF
      *
           .
       212-SHELL-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MUID"          TO  SPA-MOTOPG
      *
           MOVE    "M98"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
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
      *    状況
      *****************************************************************
       213-JYOKYO-SEC              SECTION.
      *
           PERFORM 900-JOBLOG-READ-SEC
      *
           IF      FLG-JOBLOG          =   1
               MOVE    5               TO  SPA-MUID-DURATION
               MOVE    SPACE           TO  SPA-MIDCD
           ELSE
               MOVE    SPACE           TO  SPA-MIDCD
               MOVE    ZERO            TO  SPA-MUID-DURATION
               IF      JOBLOG-ERRCD        =   "000"
                                           OR  "401"
                                           OR  "403"
                                           OR  "404"
                                           OR  "999"
                   STRING  "0"                 DELIMITED BY SIZE
                           JOBLOG-ERRCD        DELIMITED BY SIZE
                           INTO                SPA-MIDCD
                   END-STRING
               END-IF
           END-IF
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 230-MID2SET-SEC
           END-IF
      *
           .
       213-JYOKYO-EXT.
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
           END-IF
      *     IF     (SPA-ERRCD           =   SPACE )  AND
      *            (SPA-MIDCD       NOT =   SPACE )
      *         PERFORM 520-MIDSET-SEC
      *     END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "MUID"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
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
           MOVE    SPACE           TO  MUID
           INITIALIZE                  MUID
      *
           MOVE    SPA-MUID-HOSPID     TO  MUID-HOSPID
           MOVE    SPA-MUID-SINSEIYMD  TO  MUID-SINSEIYMD
           MOVE    SPA-MUID-KANANAME   TO  MUID-KANANAME
           MOVE    SPA-MUID-NAME       TO  MUID-NAME
           MOVE    SPA-MUID-EMAIL      TO  MUID-EMAIL
           MOVE    SPA-MUID-HOSPSBT-G  TO  MUID-HOSPSBT
           MOVE    SPA-MUID-HOSPSBT1-G TO  MUID-HOSPSBT1
      *
           MOVE    SPA-MUID-BEDSUIPPAN TO  MUID-BEDSUIPPAN
           MOVE    SPA-MUID-BEDSURYOYO TO  MUID-BEDSURYOYO
           MOVE    SPA-MUID-BEDSUSEISIN
                                       TO  MUID-BEDSUSEISIN
           MOVE    SPA-MUID-BEDSUKEKKAKU
                                       TO  MUID-BEDSUKEKKAKU
           MOVE    SPA-MUID-BEDSU      TO  MUID-BEDSU
      *
           MOVE    SPA-MUID-SETTI-G    TO  MUID-SETTI
           MOVE    SPA-MUID-SYUKA-G    TO  MUID-SYUKA
           MOVE    SPA-MUID-CIIKI-G    TO  MUID-CIIKI
           MOVE    SPA-MUID-DPCKBN-G   TO  MUID-DPCKBN
           MOVE    SPA-MUID-ZAITAKU-G  TO  MUID-ZAITAKU
      *
           MOVE    SPA-MUID-KAIINYM    TO  MUID-KAIINYM
           MOVE    SPA-MUID-POST       TO  MUID-POST
           MOVE    SPA-MUID-PREF       TO  MUID-PREF
           MOVE    SPA-MUID-ADRS       TO  MUID-ADRS
           MOVE    SPA-MUID-USER       TO  MUID-USER
           MOVE    SPA-MUID-PASSWORD   TO  MUID-PASSWORD
           MOVE    SPA-MUID-DURATION   TO  MUID-DURATION
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-HOSPSBT-MAX
               MOVE  SPA-MUID-HOSPSBT-LIST(IDX)
                                           TO  MUID-HOSPSBT-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-HOSPSBT-MAX  TO  MUID-HOSPSBT-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-HOSPSBT1-MAX
               MOVE  SPA-MUID-HOSPSBT1-LIST(IDX)
                                           TO  MUID-HOSPSBT1-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-HOSPSBT1-MAX TO  MUID-HOSPSBT1-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-SETTI-MAX
               MOVE  SPA-MUID-SETTI-LIST(IDX)
                                           TO  MUID-SETTI-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-SETTI-MAX    TO  MUID-SETTI-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-SYUKA-MAX
               MOVE  SPA-MUID-SYUKA-LIST(IDX)
                                           TO  MUID-SYUKA-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-SYUKA-MAX    TO  MUID-SYUKA-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-CIIKI-MAX
               MOVE  SPA-MUID-CIIKI-LIST(IDX)
                                           TO  MUID-CIIKI-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-CIIKI-MAX    TO  MUID-CIIKI-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-DPCKBN-MAX
               MOVE  SPA-MUID-DPCKBN-LIST(IDX)
                                           TO  MUID-DPCKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-DPCKBN-MAX   TO  MUID-DPCKBN-COUNT
      *
           PERFORM  VARYING IDX    FROM    1   BY  1
                    UNTIL   IDX    >   SPA-MUID-ZAITAKU-MAX
               MOVE  SPA-MUID-ZAITAKU-LIST(IDX)
                                           TO  MUID-ZAITAKU-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-MUID-ZAITAKU-MAX  TO  MUID-ZAITAKU-COUNT
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
      *     IF      SPA-MUID-CUR        =   ZERO
      *         PERFORM 50011-CUR-SET-SEC
      *     END-IF
           EVALUATE    SPA-MUID-CUR
               WHEN    1
                   MOVE    "KANANAME"      TO  MCP-WIDGET
               WHEN    2
                   MOVE    "NAME"          TO  MCP-WIDGET
               WHEN    3
                   MOVE    "EMAIL"         TO  MCP-WIDGET
               WHEN    4
                   MOVE    "HOSPSBT"       TO  MCP-WIDGET
               WHEN    5
                   MOVE    "HOSPSBT1"      TO  MCP-WIDGET
               WHEN    6
                   MOVE    "BEDSUIPPAN"    TO  MCP-WIDGET
               WHEN    7
                   MOVE    "BEDSURYOYO"    TO  MCP-WIDGET
               WHEN    8
                   MOVE    "BEDSUSEISIN"   TO  MCP-WIDGET
               WHEN    9
                   MOVE    "BEDSUKEKKAKU"  TO  MCP-WIDGET
               WHEN    10
                   MOVE    "BEDSU"         TO  MCP-WIDGET
               WHEN    11
                   MOVE    "SETTI"         TO  MCP-WIDGET
               WHEN    12
                   MOVE    "SYUKA"         TO  MCP-WIDGET
               WHEN    13
                   MOVE    "CIIKI"         TO  MCP-WIDGET
               WHEN    14
                   MOVE    "DPCKBN"        TO  MCP-WIDGET
               WHEN    15
                   MOVE    "ZAITAKU"       TO  MCP-WIDGET
               WHEN    16
                   MOVE    "KAIINYM"       TO  MCP-WIDGET
               WHEN    17
                   MOVE    "POST"          TO  MCP-WIDGET
               WHEN    18
                   MOVE    "USER"          TO  MCP-WIDGET
               WHEN    19
                   MOVE    "PASSWORD"      TO  MCP-WIDGET
               WHEN    20
                   MOVE    "B12"           TO  MCP-WIDGET
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
           MOVE    1               TO  SPA-MUID-CUR
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "KANANAME"
                   MOVE    2               TO  SPA-MUID-CUR
               WHEN    "NAME"
                   MOVE    3               TO  SPA-MUID-CUR
               WHEN    "EMAIL"
                   MOVE    4               TO  SPA-MUID-CUR
               WHEN    "HOSPSBT"
                   MOVE    5               TO  SPA-MUID-CUR
               WHEN    "HOSPSBT1"
                   MOVE    6               TO  SPA-MUID-CUR
               WHEN    "BEDSUIPPAN"
                   MOVE    7               TO  SPA-MUID-CUR
               WHEN    "BEDSURYOYO"
                   MOVE    8               TO  SPA-MUID-CUR
               WHEN    "BEDSUSEISIN"
                   MOVE    9               TO  SPA-MUID-CUR
               WHEN    "BEDSUKEKKAKU"
                   MOVE    10              TO  SPA-MUID-CUR
               WHEN    "BEDSU"
                   MOVE    11              TO  SPA-MUID-CUR
               WHEN    "SETTI"
                   MOVE    12              TO  SPA-MUID-CUR
               WHEN    "SYUKA"
                   MOVE    13              TO  SPA-MUID-CUR
               WHEN    "CIIKI"
                   MOVE    14              TO  SPA-MUID-CUR
               WHEN    "DPCKBN"
                   MOVE    15              TO  SPA-MUID-CUR
               WHEN    "ZAITAKU"
                   MOVE    16              TO  SPA-MUID-CUR
               WHEN    "KAIINYM"
                   MOVE    17              TO  SPA-MUID-CUR
               WHEN    "POST"
                   MOVE    18              TO  SPA-MUID-CUR
               WHEN    "USER"
                   MOVE    19              TO  SPA-MUID-CUR
               WHEN    "PASSWORD"
                   MOVE    20              TO  SPA-MUID-CUR
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
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "定点参加番号が不正です。"
                                           TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "定点参加番号を入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0003"
                    MOVE    "パスワードが不正です。"
                                           TO  SPA-ERRMSG
                WHEN    "0004"
                    MOVE    "パスワードを入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0005"
                    MOVE    "全角で入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0006"
                    MOVE    "半角で入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0007"
                    MOVE    "選択番号が不正です。"
                                           TO  SPA-ERRMSG
                WHEN    "0008"
                    MOVE    "日付が不正です。"
                                           TO  SPA-ERRMSG
                WHEN    "0009"
                    MOVE    "番号を選択して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0010"
                    MOVE    "病床数の合計が不正です。"
                                           TO  SPA-ERRMSG
                WHEN    "0011"
                    MOVE    "数字７桁で入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "0012"
                    MOVE    "郵便番号を入力して下さい。"
                                           TO  SPA-ERRMSG
                WHEN    "9000"
                    MOVE    "設定ファイルのＯＰＥＮに失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    "9001"
                    MOVE    "設定ファイルの出力に失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE          TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
      *
           MOVE    "B01"           TO  MCP-WIDGET.
           MOVE    "MUID"          TO  SPA-MOTOPG.
           MOVE    "MERR"          TO  SPA-SAKIPG.
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  MID1
      *
           INITIALIZE                  MID1
           MOVE    SPA-MIDCD       TO  MID1-ID1CODE
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MUID"          TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "MID1"          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点設定読込
      *****************************************************************
       900-DASCONFIG-READ-SEC          SECTION.
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONFIG
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONFIG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONFIG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DASCONFIG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所読み込み
      *****************************************************************
       900-ADRS-READ-SEC           SECTION.
      *
      *    住所データ読み出し
           INITIALIZE                  ADRS-REC
           MOVE    SPA-MUID-POST   TO  ADRS-POST
           MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "tbl_adrs"      TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"      TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
            .
       900-ADRS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブログ読み込み
      *****************************************************************
       900-JOBLOG-READ-SEC         SECTION.
      *
           INITIALIZE                  JOBLOG-REC
           MOVE    SPA-HOSPNUM     TO  JOBLOG-HOSPNUM
           MOVE    "das-profile"   TO  JOBLOG-PROGNAME
           MOVE    JOBLOG-REC      TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_joblog"    TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JOBLOG
                   MOVE    MCPDATA-REC     TO  JOBLOG-REC
               ELSE
                   MOVE    1               TO  FLG-JOBLOG
               END-IF
           ELSE
               MOVE    1               TO  FLG-JOBLOG
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
            .
       900-JOBLOG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
