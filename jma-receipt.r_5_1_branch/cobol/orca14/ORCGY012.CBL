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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGY012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約メモ文例登録（Ｙ０１２）
      *  管理者            : 
      *  09/01/16    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *
      *    予約パ領域
           COPY    "Y01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYKEXAMPLE      PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *    コラムリスト位置
           03  WRK-Y012-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-GMN-CUR             PIC 9(02).
           03  WRK-MOJI                PIC X(200).
      *
           03  WRK-HYOJI               PIC 9(04).
           03  WRK-YID1MSG             PIC X(80).
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    予約文例雛形マスタ−
       01  YYKEXAMPLE-REC.
           COPY    "CPYYKEXAMPLE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
           COPY  "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           MOVE    SPA-FREE        TO  SPA-Y01
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 210-GMNSENI
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
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y012"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "YID1"
                   PERFORM 3001-YID1-SYORI-SEC
               WHEN    OTHER
                   PERFORM 3000-INIT-SYORI-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3000-INIT-SYORI-SEC              SECTION.
      *
           INITIALIZE              SPA-Y012-AREA
      *    予約情報がない時、文例のみ
           IF     (SPA-GMN-PTNUM       =   SPACE )  AND
                  (SPA-GMN-PTID        =   ZERO  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE )
               MOVE    1                   TO  SPA-Y012-SYORI
           ELSE
               MOVE    ZERO                TO  SPA-Y012-SYORI
           END-IF
           MOVE    SPA-MOTOPG          TO  SPA-Y012-MOTOPG
      *
           PERFORM 310-SPASET-SEC
           .
       3000-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-Y012-PAGE
      *    文例雛形一覧表示
           PERFORM 3101-YYKEXAMPLE-HEN-SEC
      *
           INITIALIZE                      SPA-Y012-GMN-AREA
           MOVE    1                   TO  SPA-Y012-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例雛形一覧表示処理
      *****************************************************************
       3101-YYKEXAMPLE-HEN-SEC         SECTION.
      *
           INITIALIZE                  SPA-Y012-BUNLIST-G
           COMPUTE IDX-STR             =   (SPA-Y012-PAGE   -  1 )
                                       *   100
                                       +   1
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-Y012-NEXT
           PERFORM
                   UNTIL      (IDX     >=  100  )  OR
                              (FLG-YYKEXAMPLE  =   1   )
               ADD     1                   TO  IDY
               IF      IDY                 >= IDX-STR
                   ADD     1                  TO  IDX
                   MOVE    YYKEXAMPLE-RENNUM  TO  SPA-Y012-LNUM (IDX)
                   MOVE    YYKEXAMPLE-COMMENT TO  SPA-Y012-LCOMMENT
                                                                (IDX)
                   MOVE    YYKEXAMPLE-HYOKBN  TO  SPA-Y012-LHYOJI(IDX)
                   MOVE    IDX                TO  SPA-Y012-LMAX
               END-IF
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           END-PERFORM
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    次頁あり
           IF     FLG-YYKEXAMPLE       =   ZERO
               MOVE    1               TO  SPA-Y012-NEXT
           END-IF
           .
       3101-YYKEXAMPLE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面遷移処理
      *****************************************************************
       3001-YID1-SYORI-SEC                  SECTION.
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
               WHEN    "1002"
               WHEN    "1004"
      *            追加・更新
                   IF      SPA-YID1-FLG        =   "OK"
                       PERFORM 41201-YYKEXAMPLE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-Y012-GMN-CUR
                   END-IF
               WHEN    "1003"
      *            削除
                   IF      SPA-YID1-FLG        =   "OK"
                       PERFORM 4901-YYKEXAMPLE-DEL-SEC
                   ELSE
                       MOVE    1               TO  SPA-Y012-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-YID1CD
           MOVE    SPACE               TO  SPA-YID1-FLG
           .
       3001-YID1-SYORI-SEC-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-BACK-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-NEXT-SEC
      *    文例削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 490-BUNREI-DEL-SEC
      *    文例登録
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-BUNREI-ADD-SEC
      *    メモ確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4120-TOROKU-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Y012-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        文例番号選択
               WHEN    "SELECT"        ALSO    "BUNLIST"
                   PERFORM 41011-BUNLIST-SEC
      *        文例番号入力
               WHEN    "ACTIVATE"      ALSO    "BUNNUM"
                   PERFORM 41012-BUNNUM-SEC
      *        一括入力分
               WHEN    OTHER
      *           入力チェック処理
                  MOVE    ZERO                TO  FLG-TOUROKU
                  PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
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
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    Y012-COMMENT        TO  SPA-Y012-COMMENT
           MOVE    Y012-HYOJUN         TO  SPA-Y012-HYOJI
      *
           MOVE    Y012-BUNNUM         TO  SPA-Y012-BUNNUM
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
      *    メモ内容
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41011-COMMENT-CHK-SEC
           END-IF
      *    文例番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-BUNNUM-SEC
           END-IF
      *    表示有無
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-HYOUJI-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    メモ内容チェック処理
      *****************************************************************
       41011-COMMENT-CHK-SEC           SECTION.
      *
           IF      SPA-Y012-COMMENT    =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-Y012-COMMENT    TO  WRK-MOJI
               PERFORM 410111-KANJICHK-SEC
               MOVE    WRK-MOJI            TO  SPA-Y012-COMMENT
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1               TO  SPA-Y012-GMN-CUR
               END-IF
           END-IF
           .
       41011-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       410111-KANJICHK-SEC            SECTION.
      *
      *    全角半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  ) 
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MOJI
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-MOJI
           END-IF
      *
           .
       410111-KANJICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示区分チェック処理
      *****************************************************************
       41012-BUNNUM-SEC            SECTION.
      *
           MOVE    Y012-BUNNUM         TO  SPA-Y012-BUNNUM
      *
           MOVE    ZERO                TO  SPA-Y012-LINE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-Y012-LMAX )
               IF      SPA-Y012-LNUM (IDX) =   SPA-Y012-BUNNUM
                   MOVE    IDX             TO  SPA-Y012-LINE
                   MOVE    SPA-Y012-LMAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-Y012-MAE-BUNNUM =   SPA-Y012-BUNNUM
               CONTINUE
           ELSE
               IF      SPA-Y012-LINE       >   ZERO
                   MOVE    SPA-Y012-LHYOJI (SPA-Y012-LINE)
                                               TO  SPA-Y012-HYOJI
                   MOVE    SPA-Y012-LCOMMENT (SPA-Y012-LINE)
                                               TO  SPA-Y012-COMMENT
                   MOVE    ZERO                TO  SPA-Y012-ADD
               ELSE
                   PERFORM 410121-BUNNUM-DBCHK-SEC
                   IF      FLG-YYKEXAMPLE      =   ZERO
                       MOVE    YYKEXAMPLE-COMMENT  TO  SPA-Y012-COMMENT
                       MOVE    YYKEXAMPLE-HYOKBN   TO  SPA-Y012-HYOJI
                       MOVE    ZERO                TO  SPA-Y012-ADD
                   ELSE
                       MOVE    1                   TO  SPA-Y012-ADD
                   END-IF
               END-IF
           END-IF
           MOVE    SPA-Y012-BUNNUM     TO  SPA-Y012-MAE-BUNNUM
      *
           .
       41012-BUNNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例番号存在チェック処理
      *****************************************************************
       410121-BUNNUM-DBCHK-SEC            SECTION.
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
           MOVE    SPA-Y012-BUNNUM     TO  YYKEXAMPLE-RENNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410121-BUNNUM-DBCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示区分チェック処理
      *****************************************************************
       41012-HYOUJI-SEC            SECTION.
      *
           .
       41012-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例番号選択処理
      *****************************************************************
       41011-BUNLIST-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-Y012-LMAX   )
               IF      Y012-BUNLIST-SELECT (IDX)   =   "T"
                   MOVE    SPA-Y012-LNUM (IDX)
                                               TO  Y012-BUNNUM
                   MOVE    SPA-Y012-LMAX       TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM 41012-BUNNUM-SEC
           MOVE    2                   TO  SPA-Y012-GMN-CUR
           .
       41011-BUNLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理へ
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           IF     (SPA-Y012-BUNNUM     =   ZERO )
               PERFORM 310-SPASET-SEC
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           ELSE
               MOVE    ZERO                TO  SPA-Y012-BUNNUM
               MOVE    ZERO                TO  SPA-Y012-HYOJI
               MOVE    SPACE               TO  SPA-Y012-COMMENT
               MOVE    ZERO                TO  SPA-Y012-LINE
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           END-IF
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例複写　処理
      *****************************************************************
       4120-TOROKU-SEC           SECTION.
      *
           IF     (SPA-Y012-BUNNUM     >   ZERO  )
              AND (SPA-Y012-COMMENT    NOT =   SPACE )
      *
            IF      SPA-Y012-MOTOPG     =   "Y011"
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   6  )
                              OR  (FLG-OK      =   1  )
                   IF      SPA-Y011-COMMENT (IDX)  =   SPACE
                       MOVE    SPA-Y012-COMMENT
                                            TO  SPA-Y011-COMMENT (IDX)
                       MOVE    1            TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK           =   ZERO
                   MOVE    "0003"           TO  SPA-ERRCD
               END-IF
             ELSE
               MOVE    SPA-Y012-COMMENT     TO  SPA-NAI-COMMENT (1)
             END-IF
      *      クリア
             IF      SPA-ERRCD          =   SPACE
                 PERFORM 210-BACK
             END-IF
           END-IF
           .
       4120-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-BACK-SEC           SECTION.
      *
           IF      SPA-Y012-PAGE       =   1
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-Y012-PAGE   =   SPA-Y012-PAGE   -  1
      *        文例雛形一覧表示
               PERFORM 3101-YYKEXAMPLE-HEN-SEC
               INITIALIZE                   SPA-Y012-GMN-AREA
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           END-IF
           .
       460-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-NEXT-SEC           SECTION.
      *
           IF      SPA-Y012-NEXT       =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-Y012-PAGE   =   SPA-Y012-PAGE   +  1
      *        文例雛形一覧表示
               PERFORM 3101-YYKEXAMPLE-HEN-SEC
               INITIALIZE                   SPA-Y012-GMN-AREA
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           END-IF
      *
           .
       470-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例登録　処理
      *****************************************************************
       4100-BUNREI-ADD-SEC            SECTION.
      *
      *    入力チェック処理
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-Y012-COMMENT    =   SPACE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-Y012-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-Y012-ADD        =   ZERO  )
                  AND (SPA-Y012-BUNNUM     >   ZERO  )
                   MOVE    "1001"          TO  SPA-YID1CD
               ELSE
                   IF      SPA-Y012-BUNNUM     =   ZERO
                       MOVE    "1004"          TO  SPA-YID1CD
                   ELSE
                       MOVE    "1002"          TO  SPA-YID1CD
                   END-IF
               END-IF
           END-IF
           .
       4100-BUNREI-ADD-EXT.
           EXIT.
      *****************************************************************
      *    文例登録処理
      *****************************************************************
       41201-YYKEXAMPLE-SYORI-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      SPA-Y012-BUNNUM     =   ZERO
      *        最大番号検索
               PERFORM 41202-MAX-BUNNUM-SEC
               MOVE    1                   TO  FLG-YYKEXAMPLE
           ELSE
      *        文例存在チェック
               PERFORM 410121-BUNNUM-DBCHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-YYKEXAMPLE      =   ZERO
      *            更新
                   PERFORM 41201-YYKEXAMPLE-UPD-SEC
               ELSE
      *            追加
                   PERFORM 41201-YYKEXAMPLE-ADD-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           END-IF
           .
       41201-YYKEXAMPLE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    最大番号検索処理
      *****************************************************************
       41202-MAX-BUNNUM-SEC              SECTION.
      *        
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
               IF      YYKEXAMPLE-RENNUM       =   9999
                   MOVE    "0010"              TO  SPA-ERRCD
               ELSE
                   MOVE    YYKEXAMPLE-RENNUM   TO  SPA-Y012-BUNNUM
                   ADD     1                   TO  SPA-Y012-BUNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41202-MAX-BUNNUM-EXT.
           EXIT.
      *****************************************************************
      *    文例更新処理
      *****************************************************************
       41201-YYKEXAMPLE-UPD-SEC              SECTION.
      *
           MOVE    SPA-Y012-COMMENT    TO  YYKEXAMPLE-COMMENT
           MOVE    SPA-Y012-HYOJI      TO  YYKEXAMPLE-HYOKBN
      *
           MOVE    SMCNDATE-YMD        TO  YYKEXAMPLE-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYKEXAMPLE-UPHMS
           MOVE    SPA-OPID            TO  YYKEXAMPLE-OPID
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1010"      TO  SPA-ERRCD
               DISPLAY "Y012 YYKEXAMPLE-KEY UPD ERR:" MCP-RC "."
                       ",KEY:" YYKEXAMPLE-KEY
           END-IF
           .
       41201-YYKEXAMPLE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    文例追加処理
      *****************************************************************
       41201-YYKEXAMPLE-ADD-SEC              SECTION.
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
           MOVE    SPA-Y012-BUNNUM     TO  YYKEXAMPLE-RENNUM
           MOVE    SPA-Y012-COMMENT    TO  YYKEXAMPLE-COMMENT
           MOVE    SPA-Y012-HYOJI      TO  YYKEXAMPLE-HYOKBN
      *
           MOVE    SPA-OPID            TO  YYKEXAMPLE-OPID
           MOVE    SMCNDATE-YMD        TO  YYKEXAMPLE-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYKEXAMPLE-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYKEXAMPLE-UPHMS
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1010"      TO  SPA-ERRCD
               DISPLAY "Y012 YYKEXAMPLE-KEY ADD ERR:" MCP-RC "."
                       ",KEY:" YYKEXAMPLE-KEY
           END-IF
           .
       41201-YYKEXAMPLE-ADD-EXT.
           EXIT.
      *****************************************************************
      *    文例削除処理
      *****************************************************************
       490-BUNREI-DEL-SEC              SECTION.
      *
           IF     (SPA-Y012-BUNNUM     =   ZERO  )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-Y012-GMN-CUR
           ELSE
               MOVE    "1003"          TO  SPA-YID1CD
           END-IF
      *
           .
       490-BUNREI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    文例削除処理
      *****************************************************************
       4901-YYKEXAMPLE-DEL-SEC             SECTION.
      *
      *     文例削除除
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
           MOVE    SPA-Y012-BUNNUM     TO  YYKEXAMPLE-RENNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1010"      TO  SPA-ERRCD
               DISPLAY "Y012 YYKEXAMPLE-KEY DEL ERR:" MCP-RC "."
                       ",KEY:" YYKEXAMPLE-KEY
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
               MOVE    1                   TO  SPA-Y012-GMN-CUR
           END-IF
          .
       4901-YYKEXAMPLE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-Y012-MOTOPG      TO  SPA-SAKIPG
           MOVE    "Y012"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"                TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-YID1CD       NOT =   SPACE
               PERFORM 510-YID1CDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Y012"              TO  MCP-WINDOW
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
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-Y012-ROW
           ELSE
               MOVE    Y012-BUNLIST-ROW    TO  WRK-Y012-ROW
           END-IF
      *
           MOVE    SPACE               TO  Y012
           INITIALIZE                      Y012
      *
           MOVE    ZERO                TO  Y012-BUNLIST-ROW
           IF      WRK-Y012-ROW    NOT =   ZERO
               MOVE    WRK-Y012-ROW        TO  Y012-BUNLIST-ROW
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-Y012-LMAX
               MOVE    SPA-Y012-LNUM (IDX)
                                           TO  Y012-LNUM (IDX)
               IF      SPA-Y012-LHYOJI (IDX)   =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-Y012-LHYOJI (IDX)
                                               TO  Y012-LHYOJI (IDX)
               END-IF
               MOVE    SPA-Y012-LCOMMENT (IDX)
                                           TO  Y012-LCOMMENT (IDX)
      *
               IF     (SPA-Y012-BUNNUM     NOT =   ZERO )
                  AND (SPA-Y012-BUNNUM     =   SPA-Y012-LNUM (IDX))
                  MOVE    "T"              TO  Y012-BUNLIST-SELECT (IDX)
               ELSE
                  MOVE    "F"              TO  Y012-BUNLIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-Y012-LMAX       TO  Y012-BUNLIST-COUNT
      *
           MOVE    SPA-Y012-BUNNUM     TO  Y012-BUNNUM
           MOVE    SPA-Y012-COMMENT    TO  Y012-COMMENT
           MOVE    SPA-Y012-HYOJI      TO  Y012-HYOJUN
      *
           PERFORM 5001-MAPCUR-SEC
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Y012-B12-STATE
           IF      SPA-Y012-SYORI      =   1
               MOVE    WIDGET-INSENSITIVE  TO  Y012-B12-STATE
           END-IF
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
          IF      SPA-Y012-GMN-CUR          =   ZERO
               PERFORM 50011-GMN-CURSET-SEC
          END-IF
      *
          EVALUATE    SPA-Y012-GMN-CUR
                WHEN    01
                    MOVE  "BUNNUM"      TO   MCP-WIDGET
                WHEN    02
                    MOVE  "HYOJUN"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "COMMENT"     TO   MCP-WIDGET
                WHEN    10
                    MOVE  "B10"         TO   MCP-WIDGET
                WHEN    12
                    MOVE  "B12"         TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力後カーソルセット処理
      *****************************************************************
       50011-GMN-CURSET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "BUNNUM"
                   MOVE    2                   TO  SPA-Y012-GMN-CUR
               WHEN    "HYOJUN"
                   MOVE    3                   TO  SPA-Y012-GMN-CUR
               WHEN    "COMMENT"
                   IF      SPA-Y012-SYORI      =   1
                       MOVE    10              TO  SPA-Y012-GMN-CUR
                   ELSE
                       MOVE    12              TO  SPA-Y012-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       50011-GMN-CURSET-EXT.
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
                   MOVE    "文例内容は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "文例番号を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "メモが６つ登録済みです。追加できません"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "文例内容を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "前頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "次頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   STRING  "文例番号が９９９９以上になります。"
                           "自動採番できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1010"
                   MOVE    "予約メモ雛形データ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y012"               TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-YID1CDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-YID1MSG
           MOVE    SPACE               TO  SPA-YID1-FLG
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
                   STRING  "文例を更新します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-YID1MSG
                   END-STRING
               WHEN    "1004"
                   STRING  "最大文例番号を採番して文例を登録します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-YID1MSG
                   END-STRING
               WHEN    "1002"
                   STRING  "文例番号で追加します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-YID1MSG
                   END-STRING
               WHEN    "1003"
                   STRING  "文例を削除します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-YID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-YID1CD           TO  WRK-YID1MSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YID1
           INITIALIZE                       YID1
           MOVE    SPA-YID1CD           TO  YID1-ID1CODE
           MOVE    WRK-YID1MSG          TO  YID1-ID1MSG
           MOVE    "戻る"               TO  YID1-B01
           EVALUATE    SPA-YID1CD
               WHEN    "1003"
                   MOVE    "B01"                TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "Y012"               TO  SPA-MOTOPG
           MOVE    "YID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-YID1CD-EXT.
           EXIT.
      *****************************************************************
      *    予約文例　ＲＥＡＤ
      *****************************************************************
       900-YYKEXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYKEXAMPLE-REC
               MOVE    ZERO            TO  FLG-YYKEXAMPLE
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
      *
           .
       900-YYKEXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
