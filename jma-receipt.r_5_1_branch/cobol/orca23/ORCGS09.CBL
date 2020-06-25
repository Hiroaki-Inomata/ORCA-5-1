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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGS09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納業務
      *  コンポーネント名  : 保険情報変更画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09.09.07    NACL-太田     新規作成
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "S01COMMON-SPA".
      *
           COPY    "S08SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
           03  FLG-HKNJAINF                PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SPACE                   PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-KANACHK-MAX             PIC 9(05).
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-LENGTH                  PIC S9(05).
           03  WRK-STR                     PIC X(100).
           03  WRK-TMP                     PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-H2904                 PIC X(08)   VALUE "201704".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY  "CPBUNBEN.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-S08FREE.
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
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
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S08FREE     TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "SERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "S09"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
           MOVE    SPACE           TO  SPA-S09
           INITIALIZE                  SPA-S09
      *
      *    コンボボックス編集処理
           PERFORM 310-COMBO-EDIT-SEC
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
           IF    ( BUNBEN-HKNJANUM     NOT =   SPACE )
               MOVE    BUNBEN-HKNJANUM     TO  SPA-GMN-S09-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               IF    ( FLG-HKNJAINF        =   ZERO )
                   MOVE    HKNJA-HKNJANAME TO  SPA-GMN-S09-HKNJANAME
               END-IF
           END-IF
      *
           MOVE    BUNBEN-KANYUKBN     TO  WRK-CMB-CD
           PERFORM 4200-SYAKOKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-S09-SYAKOK-G
      *
           MOVE    BUNBEN-HONKZKKBN    TO  WRK-CMB-CD
           PERFORM 4200-HONKZKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-S09-HONKZK-G
      *
           MOVE    BUNBEN-KIGO         TO  SPA-GMN-S09-KIGO
           MOVE    BUNBEN-NUM          TO  SPA-GMN-S09-NUM
      *
           MOVE    1                   TO  SPA-GMN-S09-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       310-COMBO-EDIT-SEC              SECTION.
      *
      *    社保・国保区分
           PERFORM 3101-SYAKOKLST-SEC
      *
      *    本人・家族区分
           PERFORM 3101-HONKZKLST-SEC
      *
           .
       310-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    社保・国保区分コンボ初期処理
      *****************************************************************
       3101-SYAKOKLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-S09-SYAKOKLST-G
      *
           MOVE    "1"             TO  SPA-GMN-S09-SYAKOKL   (1)
           MOVE    "社保"          TO  SPA-GMN-S09-SYAKOKMEIL(1)
           MOVE    "2"             TO  SPA-GMN-S09-SYAKOKL   (2)
           MOVE    "国保"          TO  SPA-GMN-S09-SYAKOKMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-S09-SYAKOK-MAX
      *
           .
       3101-SYAKOKLST-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分コンボ初期処理
      *****************************************************************
       3101-HONKZKLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-S09-HONKZKLST-G
      *
           MOVE    "1"             TO  SPA-GMN-S09-HONKZKL   (1)
           MOVE    "本人"          TO  SPA-GMN-S09-HONKZKMEIL(1)
           MOVE    "5"             TO  SPA-GMN-S09-HONKZKL   (2)
           MOVE    "家族"          TO  SPA-GMN-S09-HONKZKMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-S09-HONKZK-MAX
      *
           .
       3101-HONKZKLST-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-S09-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    S09-HKNJANUM    TO  SPA-GMN-S09-HKNJANUM
           MOVE    S09-SYAKOK      TO  SPA-GMN-S09-SYAKOK-G
           MOVE    S09-HONKZK      TO  SPA-GMN-S09-HONKZK-G
           MOVE    S09-KIGO        TO  SPA-GMN-S09-KIGO
           MOVE    S09-NUM         TO  SPA-GMN-S09-NUM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    保険者番号チェック処理
           PERFORM 4101-HKNJANUM-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *        社保・国保区分チェック処理
               PERFORM 4101-SYAKOK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *        本人・家族区分チェック処理
               PERFORM 4101-HONKZK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *        記号チェック処理
               PERFORM 4101-KIGO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *        番号チェック処理
               PERFORM 4101-NUM-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       4101-HKNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SPA-GMN-S09-HKNJANAME
      *
           IF    ( SPA-GMN-S09-HKNJANUM    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO                TO  WRK-KANACHK-MAX
      *
      *        桁数チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-S09-HKNJANUM
                                           TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-S09-HKNJANUM
      *        半角で間に空白がないこと
               IF    ( KANACHK-RC      NOT =   ZERO  )
                OR   ( KANACHK-RC2     NOT =   ZERO  )
                OR   ( KANACHK-SYUBETU     =   2     )
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-S09-CUR
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( KANACHK-MAX     =   4  OR  6  OR  8 )
                       MOVE    KANACHK-MAX TO  WRK-KANACHK-MAX
                   ELSE
                       MOVE    "0012"      TO  SPA-ERRCD
                       MOVE    1           TO  SPA-GMN-S09-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
      *
      *            保険者番号あり
                   IF    ( FLG-HKNJAINF        =   ZERO )
                       MOVE    HKNJA-HKNJANAME TO  SPA-GMN-S09-HKNJANAME
                       MOVE    HKNJA-HKNNUM    TO  WRK-HKNNUM
      *                記号が空白以外の時、再編集
                       IF    ( HKNJA-KIGO  NOT     =   SPACE )
                        AND  ( SPA-GMN-S09-KIGO    =   SPACE )
                           MOVE    HKNJA-KIGO  TO  SPA-GMN-S09-KIGO
                       END-IF
      *
                   ELSE
      *
      *                保険者番号モジュラス10チェック
                       IF    ( WRK-KANACHK-MAX >   4 )
      *
                           INITIALIZE                  LNK-CHKDGT-AREA
                           MOVE    2               TO  LNK-I-SYORIKBN
                           MOVE    SPA-GMN-S09-HKNJANUM
                                                   TO  LNK-I-CODE
                           CALL    "ORCSCHKDGT"        USING
                                                       LNK-CHKDGT-AREA
                           IF    ( LNK-O-ERRCD     NOT =   ZERO )
                               MOVE    "0002"      TO  SPA-ERRCD
                               MOVE    1           TO  SPA-GMN-S09-CUR
                           END-IF
                       END-IF
      *
                       IF    ( SPA-ERRCD              =    SPACE )
      *
      *                    法より編集
                           EVALUATE    WRK-KANACHK-MAX
                           WHEN    4
      *                        政府管掌
                               MOVE    "001"       TO  WRK-HKNNUM
                           WHEN    6
      *                        国保
                               MOVE    "060"       TO  WRK-HKNNUM
                           WHEN    8
      *                        その他
                               PERFORM 41011-HKNNUM-HBTNUM-SEC
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( WRK-HKNNUM      =   SPACE )
                       CONTINUE
                   ELSE
                       IF    ( WRK-HKNNUM  =   "060" OR "067"
                                            OR "068" OR "069" )
                           MOVE    "2"     TO  SPA-GMN-S09-SYAKOK
                       ELSE
                           MOVE    "1"     TO  SPA-GMN-S09-SYAKOK
                       END-IF
                       IF    ( WRK-HKNNUM  =   "039" OR "040" )
                           MOVE    "0003"  TO  SPA-ERRCD
                           MOVE    1       TO  SPA-GMN-S09-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    法別番号から保険種別検索処理
      *****************************************************************
       41011-HKNNUM-HBTNUM-SEC                 SECTION.
      *
           PERFORM 900-HKNNUM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      NOT =  ZERO )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" )
                   MOVE    HKN-HKNNUM      TO  WRK-HKNNUM
               END-IF
      *
               PERFORM 900-HKNNUM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-HKNNUM-HBTNUM-EXT.
           EXIT.
      *****************************************************************
      *    社保・国保区分チェック処理
      *****************************************************************
       4101-SYAKOK-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-S09-SYAKOK-G    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-S09-SYAKOK  TO  WRK-CMB-CD
               PERFORM 4200-SYAKOKLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-S09-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-S09-SYAKOK-G
               END-IF
           END-IF
      *
           .
       4101-SYAKOK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    本人・家族区分チェック処理
      *****************************************************************
       4101-HONKZK-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-S09-HONKZK-G        =   SPACE )
               IF    ( SPA-GMN-S09-HKNJANUM    =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  WRK-CMB-CD
                   PERFORM 4200-HONKZKLST-CHK-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-S09-HONKZK-G
               END-IF
           ELSE
               MOVE    SPA-GMN-S09-HONKZK  TO  WRK-CMB-CD
               PERFORM 4200-HONKZKLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-S09-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-S09-HONKZK-G
               END-IF
           END-IF
      *
           .
       4101-HONKZK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号チェック処理
      *****************************************************************
       4101-KIGO-CHK-SEC               SECTION.
      *
           IF    ( SPA-GMN-S09-KIGO    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-S09-KIGO    TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR     TO  SPA-GMN-S09-KIGO
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-S09-CUR
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SPA-GMN-S09-KIGO    TALLYING
                           CNT-SPACE       FOR ALL "　"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0007"      TO  SPA-ERRCD
                       MOVE    4           TO  SPA-GMN-S09-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    SPA-GMN-S09-KIGO
                                           TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0010"      TO  SPA-ERRCD
                       MOVE    4           TO  SPA-GMN-S09-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       4101-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号チェック処理
      *****************************************************************
       4101-NUM-CHK-SEC                SECTION.
      *
           IF    ( SPA-GMN-S09-NUM    =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-S09-NUM    TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR     TO  SPA-GMN-S09-NUM
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0008"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-S09-CUR
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SPA-GMN-S09-NUM    TALLYING
                           CNT-SPACE       FOR ALL "　"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0009"      TO  SPA-ERRCD
                       MOVE    5           TO  SPA-GMN-S09-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    SPA-GMN-S09-NUM TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0011"      TO  SPA-ERRCD
                       MOVE    5           TO  SPA-GMN-S09-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    社保・国保区分チェック処理
      *****************************************************************
       4200-SYAKOKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-S09-SYAKOK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-S09-SYAKOKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-S09-SYAKOKLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SYAKOKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    本人・家族区分チェック処理
      *****************************************************************
       4200-HONKZKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-S09-HONKZK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-S09-HONKZKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-S09-HONKZKLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HONKZKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       411-KANREN-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-S09-HKNJANUM    =   SPACE )
               MOVE    "1001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-S09-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-S09-SYAKOK-G    =   SPACE )
                   MOVE    "1002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-S09-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-S09-HONKZK-G    =   SPACE )
                   MOVE    "1003"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-S09-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-S09-NUM =   SPACE )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-S09-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPACE           TO  WRK-STR
               STRING  SPA-GMN-S09-KIGO    DELIMITED BY SPACE
                       SPA-GMN-S09-NUM     DELIMITED BY SPACE
               INTO    WRK-STR
               END-STRING
               PERFORM 800-LENGTH-CHK-SEC
               IF    ( WRK-LENGTH  >   38 )
                   MOVE    "1005"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-S09-CUR
               END-IF
           END-IF
      *
           .
      *
       411-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "S08"               TO  SPA-SAKIPG
           MOVE    "S09"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    (  SPA-ERRCD          =   SPACE )
               PERFORM 411-KANREN-CHK-SEC
           END-IF
      *
           IF    (  SPA-ERRCD          =   SPACE )
      *
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
               IF    ( FLG-BUNBEN      =   ZERO )
      *
                   PERFORM 2201-BUNBEN-HKN-SET-SEC
      *
                   PERFORM 800-MCNDATE-SEC
      *
                   MOVE    SPA-OPID        TO  BUNBEN-OPID
                   MOVE    SMCNDATE-YMD    TO  BUNBEN-UPYMD
                   MOVE    SMCNDATE-HMS    TO  BUNBEN-UPHMS
                   MOVE    BUNBEN-REC      TO  MCPDATA-REC
                   MOVE    "tbl_bunben"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4002"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF    ( SPA-S08-NYUGAIKBN =   "1" )
                       MOVE    "4001"      TO  SPA-ERRCD
                       MOVE    "tbl_bunben" TO MCP-TABLE
                       MOVE    "abort"     TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "B12"            TO  SPA-S09-BTN
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    保険情報設定処理
      *****************************************************************
       2201-BUNBEN-HKN-SET-SEC             SECTION.
      *
           MOVE    SPA-GMN-S09-HKNJANUM
                                   TO  BUNBEN-HKNJANUM
           MOVE    SPA-GMN-S09-SYAKOK
                                   TO  BUNBEN-KANYUKBN
           MOVE    SPA-GMN-S09-HONKZK
                                   TO  BUNBEN-HONKZKKBN
           MOVE    SPA-GMN-S09-KIGO
                                   TO  BUNBEN-KIGO
           MOVE    SPA-GMN-S09-NUM TO  BUNBEN-NUM
           IF    ( SPA-GMN-S09-HKNJANUM (1:2)
                                   =   "67" )
            AND  ( SPA-GMN-S09-HKNJANUM (8:1)  NOT =   SPACE )
               MOVE    SPA-GMN-S09-HKNJANUM (3:)
                                   TO  BUNBEN-HKNJANUM-S
           ELSE
               MOVE    SPA-GMN-S09-HKNJANUM
                                   TO  BUNBEN-HKNJANUM-S
           END-IF
      *
      *    USE SPA-NAI-SRYYMD & SPA-NAI-RECESAKI IN SPA-S08 (SPA-S08FREE)
           DISPLAY "SPA-NAI-SRYYMD  =" SPA-NAI-SRYYMD
           DISPLAY "SPA-NAI-RECESAKI=" SPA-NAI-RECESAKI
           IF   (( BUNBEN-SKYYM         NOT =   "000000" )
             AND ( BUNBEN-SKYYM            >=   CONST-H2904 ))
             OR  ( SPA-NAI-SRYYMD (1:6)    >=   CONST-H2904 )
                   MOVE    BUNBEN-KANYUKBN
                                       TO  BUNBEN-TEISYUTUSAKI
           ELSE
               IF    ( BUNBEN-BUNBENKBN    =   "1" )
                       MOVE    "2"     TO  BUNBEN-TEISYUTUSAKI
               ELSE
      *
                   IF    ( SPA-NAI-RECESAKI    =   "1" )
                       MOVE    "1"     TO  BUNBEN-TEISYUTUSAKI
                   END-IF
      *
                   IF    ( SPA-NAI-RECESAKI    =   "2" )
                       MOVE    "2"     TO  BUNBEN-TEISYUTUSAKI
                   END-IF
               END-IF
           END-IF
           
      *
           .
      *
       2201-BUNBEN-HKN-SET-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S09    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  S09
           INITIALIZE                      S09
      *
           MOVE    SPA-GMN-S09-HKNJANUM    TO  S09-HKNJANUM
           MOVE    SPA-GMN-S09-HKNJANAME   TO  S09-HKNJANAME
      *
           MOVE    SPA-GMN-S09-SYAKOK-G    TO  S09-SYAKOK
           MOVE    SPA-GMN-S09-SYAKOK-MAX  TO  S09-SYAKOK-COUNT
           PERFORM VARYING IDX-CMB    FROM 1   BY  1
                   UNTIL ( IDX-CMB    >    SPA-GMN-S09-SYAKOK-MAX )
      *
               MOVE    SPA-GMN-S09-SYAKOKLST (IDX-CMB)
                                           TO  S09-SYAKOK-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-S09-HONKZK-G    TO  S09-HONKZK
           MOVE    SPA-GMN-S09-HONKZK-MAX  TO  S09-HONKZK-COUNT
           PERFORM VARYING IDX-CMB    FROM 1   BY  1
                   UNTIL ( IDX-CMB    >    SPA-GMN-S09-HONKZK-MAX )
      *
               MOVE    SPA-GMN-S09-HONKZKLST (IDX-CMB)
                                           TO  S09-HONKZK-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-S09-KIGO        TO  S09-KIGO
           MOVE    SPA-GMN-S09-NUM         TO  S09-NUM
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-S09-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-S09-CUR
           WHEN    001
               MOVE    "HKNJANUM"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "SYAKOK"        TO  MCP-WIDGET
           WHEN    003
               MOVE    "HONKZK"        TO  MCP-WIDGET
           WHEN    004
               MOVE    "KIGO"          TO  MCP-WIDGET
           WHEN    005
               MOVE    "NUM"           TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "HKNJANUM"
               MOVE    002             TO  SPA-GMN-S09-CUR
           WHEN    "SYAKOK"
               MOVE    003             TO  SPA-GMN-S09-CUR
           WHEN    "HONKZK"
               MOVE    004             TO  SPA-GMN-S09-CUR
           WHEN    "KIGO"
               MOVE    005             TO  SPA-GMN-S09-CUR
           WHEN    "NUM"
               MOVE    212             TO  SPA-GMN-S09-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "保険者番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "保険者番号検証チェックエラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "後期高齢者の保険は選択できません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "加入制度区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "本人・家族区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "記号はすべて全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "記号に全角空白が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "番号はすべて全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "番号に全角空白が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "記号は１９文字以内で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "番号は１９文字以内で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "保険者番号桁数エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "保険者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "加入制度区を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "本人・家族区分を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE "記号と番号は合わせて１９文字以内で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4003"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SERR
           MOVE    SPA-ERRCD            TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  SERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S09"                TO  SPA-MOTOPG
           MOVE    "SERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
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
      *    全角チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-STR   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
      *            全角でない場合はエラーとする。
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長チェック
      *****************************************************************
       800-LENGTH-CHK-SEC              SECTION.
      *
           MOVE    1               TO  WRK-LENGTH
           MOVE    SPACE           TO  WRK-TMP
      *
           STRING  WRK-STR DELIMITED   BY  SPACE
           INTO    WRK-TMP
           WITH POINTER  WRK-LENGTH
           END-STRING
           COMPUTE WRK-LENGTH = WRK-LENGTH -   1
      *
           .
       800-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者情報検索処理
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
      *    保険者番号　検索チェック
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-S09-HKNJANUM
                                       TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号検索処理
      *****************************************************************
       900-HKNNUM-KEY4-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    SPA-GMN-S09-HKNJANUM(1:2)
                                   TO  HKN-HBTNUM
           MOVE    SPA-SYSYMD      TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD      TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号FETCH処理
      *****************************************************************
       900-HKNNUM-KEY4-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID        TO  BUNBEN-PTID
           MOVE    SPA-S08-NYUGAIKBN
                                   TO  BUNBEN-NYUGAIKBN
           MOVE    SPA-S08-RRKNUM  TO  BUNBEN-RRKNUM
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    SPA-S08-RRKNUM      TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
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
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

