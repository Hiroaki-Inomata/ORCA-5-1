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
       PROGRAM-ID.             ORCGW34.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : プログラムオプション登録（Ｗ３４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/01/21    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  12/10/03  区分コード複数登録対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計メモ更新データ
           SELECT  MST-PRGOPT  ASSIGN              ASS-PRGOPT
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-PRGOPT.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  MST-PRGOPT.
       01  PRGWK-REC.
           03  PRGWK-SYORIKBN      PIC  X(1).
           03  PRGWK-HOSPNUM       PIC  9(2).
           03  PRGWK-PRGID         PIC  X(25).
           03  PRGWK-KBNCD         PIC  X(16).
           03  PRGWK-ADDKBN        PIC  X(01).
           03  PRGWK-TERMID        PIC  X(16).
           03  PRGWK-OPID          PIC  X(16).
           03  PRGWK-CREYMD        PIC  X(08).
           03  PRGWK-UPYMD         PIC  X(08).
           03  PRGWK-UPHMS         PIC  X(06).
           03  PRGWK-OPTION        PIC  X(44000).
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
       01  SPA-W34.
           03  SPA-W34-INIT-AREA.
               05  SPA-GMN-PAGE        PIC 9(03).
               05  SPA-GMN-NEXT        PIC 9(01).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(03).
      *
           03  SPA-W34-AREA.
      *        プログラム名
               05  SPA-GMN-PRGID           PIC X(25).
               05  SPA-GMN-INPUT-G.
      *            オプション説明
                   07  SPA-GMN-EXPLANTION      PIC X(44000).
      *            区分コード
                   07  SPA-GMN-KBNCD-G.
                       09  SPA-GMN-KBNCD           PIC X(16).
      *
                   07  SPA-GMN-KBNCD-LST.
                       09  SPA-GMN-KBNCD-OCC       OCCURS   50.
                           11  SPA-GMN-KBNCDL          PIC X(16).
                       09  SPA-KBNCD-MAX               PIC 9(04).
      *            オプション内容
                   07  SPA-GMN-OPTION          PIC X(44000).
      *        一覧
               05  SPA-GMN-PRGLIST-TBL.
                   07  SPA-GMN-PRGLIST-OCC        OCCURS   100.
                       09  SPA-GMN-TNUM          PIC 9(03).
                       09  SPA-GMN-TPRGID        PIC X(25).
                   07  SPA-TBL-MAX               PIC 9(04).
               05  SPA-GMN-SELNUM          PIC 9(03).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-B08-CNT             PIC 9(02).
               05  SPA-NAI-INPUT-G.
      *            オプション説明あり
                   07  SPA-NAI-EXP             PIC 9(01).
                   07  SPA-NAI-ADDKBN          PIC X(01).
      *
                   07  SPA-MAE-PRGID           PIC X(25).
                   07  SPA-MAE-KBNCD           PIC X(16).
      *
           COPY    "ENUM-VALUE".
      *
       01  STS-AREA.
           03  STS-PRGOPT          PIC X(02).
      *
       01  ASS-AREA.
           03  ASS-PRGOPT          PIC X(100).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-GMN-INIT        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(05).
      *
           03  IDX2                PIC 9(05).
           03  IDX-STR             PIC 9(05).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
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
           03  WRK-MAE-TPRGID      PIC X(25).
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-PRGLST-ROW      PIC S9(9)   BINARY.
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
      *    文字列チェック関数パラメタ（
       01  ORCXKANACONVAREA.
      *   出力バイト数
           03 KANACONV-LEN             PIC S9(9) BINARY.
      *   返り値( 0: 正常 1: 範囲外文字 2: 外字、変換不能 )
           03 KANACONV-RETURN          PIC S9(9) BINARY.
      *   変換文字列数(バイト)
           03 KANACONV-MAXLEN          PIC S9(9) BINARY.
      *   変換フラグ (0: 変換しない,     1: 変換する
      *               2: 1+カタカナ変換, 3: 1+ひらがな変換)
      *   ※ 半角カナおよび外字は全てのフラグで変換される
      *      か捨てられる。
      *     (外字は■)
           03 KANACONV-CONV-FLAG       PIC S9(9) BINARY.
      *   出力文字種指定
      *       0: 半角カナ、外字のみ変換
      *      14: 全角   2: ひらがな  4: カタカナ
      *      112: ASCII 16: 数字     32: アルファベット
      *      15: 全角+スペース改行   113: ASCII+スペース改行 
      *   出力文字種は OR をとることで複数指定できる
      *   例: (6: ひらがな、カタカナのみ)
      *       (48: 英数字のみ)
           03 KANACONV-CHAR-TYPE       PIC S9(9) BINARY.
      *   文字領域の長さ(INDATA, OUTDATAの長さと同じであること)
           03 KANACONV-STLEN           PIC S9(9) BINARY.
      *   入力文字列
           03 KANACONV-INDATA          PIC X(44000).
      *   出力文字列
           03 KANACONV-OUTDATA         PIC X(44000).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY  "CPPRGOPTION.INC".
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
      *    カナチェック
           COPY  "CPORCSKANACHK.INC".
      *
      **** COPY  "CPORCXKANACONV.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W34
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W34         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
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
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W34"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
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
                   MOVE    1               TO  FLG-GMN-INIT
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "U001"
               WHEN    "I001"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
               WHEN    "1004"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 430-DELETE-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
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
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W34
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-PGLIST-HEN-SEC
      *
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-PGLIST-HEN-SEC              SECTION.
      *
           INITIALIZE   SPA-W34-AREA
           INITIALIZE   SPA-NAI-AREA
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
      *
      *    開始
           IF     (SPA-GMN-PAGE        =   ZERO )
               MOVE    1                   TO  SPA-GMN-PAGE
           END-IF
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    ZERO                TO  SPA-TBL-MAX
      *
           COMPUTE IDX-STR         =   (SPA-GMN-PAGE -   1)  * 100 
                                   +   1
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-MAE-TPRGID
           PERFORM UNTIL  (FLG-PRGOPTION   =   1   )
                      OR  (SPA-GMN-NEXT    =   1   )
               IF     (WRK-MAE-TPRGID      =   PRGOPTION-PRGID)
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    PRGOPTION-PRGID     TO  WRK-MAE-TPRGID
                   IF      IDX2                <   IDX-STR
                       CONTINUE
                   ELSE
                       ADD     1                   TO  IDX
                       IF      IDX                 >   100
                           MOVE    1               TO  SPA-GMN-NEXT
                       ELSE
                           MOVE    IDX             TO  SPA-GMN-TNUM
                                                              (IDX)
                           MOVE    PRGOPTION-PRGID
                                               TO  SPA-GMN-TPRGID(IDX)
                           MOVE    IDX             TO  SPA-TBL-MAX
                      END-IF
                  END-IF
               END-IF
      *
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    終了
      *    IF     (FLG-PRGOPTION       =   1 )
      *        MOVE    WRK-PAGE            TO  WRK-PAGE-MAX
      *    ELSE
      *        MOVE    ZERO                TO  WRK-PAGE-MAX
      *    END-IF
      *
      *    MOVE    IDX                 TO  SPA-TBL-MAX
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
        3101-PGLIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM 420-CLREA-SEC
      *    削除
               WHEN    "CLICKED"      ALSO    "B03"
                   PERFORM 430-DELETE-SYORI-SEC
               WHEN    "CLICKED"      ALSO    "B08"
      *
                   IF    ( SPA-NAI-B08-CNT >   5 )
                       MOVE    ZERO        TO      SPA-NAI-B08-CNT
                   ELSE
                       COMPUTE SPA-NAI-B08-CNT
                           =   SPA-NAI-B08-CNT     +   1
                   END-IF
      *    登録
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
                   MOVE    ZERO       TO      SPA-NAI-B08-CNT
      *    入力
               WHEN    "CLICKED"      ALSO    "B10"
                   PERFORM 4100-ENTER-SEC
      *    前頁
               WHEN    "CLICKED"      ALSO    "B06"
                   PERFORM 460-MAE-PAGE-SEC
      *    次頁
               WHEN    "CLICKED"      ALSO    "B07"
                   PERFORM 470-NEXT-PAGE-SEC
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
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        一覧選択
               WHEN    ANY             ALSO    "PRGLST"
                   PERFORM 4001-PRGLST-CHK-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4002-SELNUM-CHK-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    一覧選択チェック処理
      *****************************************************************
       4001-PRGLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-TBL-MAX
               IF      W34-PRGLST-SELECT (IDX) =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-TBL-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  W34-SELNUM
           PERFORM 4002-SELNUM-CHK-SEC
      *
           .
       4001-PRGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号選択チェック処理
      *****************************************************************
       4002-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    W34-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM      =   ZERO
      *        未選択はプログラム名へ移動
               INITIALIZE                  SPA-GMN-INPUT-G
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               IF     (SPA-GMN-SELNUM      >=  1   )  AND
                      (SPA-GMN-SELNUM      <=  SPA-TBL-MAX)
      *            選択処理
                   MOVE    SPA-GMN-TPRGID (SPA-GMN-SELNUM)
                                           TO  SPA-GMN-PRGID
      *            プログラム名からの編集
                   PERFORM 40021-PRGHENSYU-SEC
               ELSE
                   MOVE    "0001"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4002-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オプション内容表示処理
      *****************************************************************
       40021-PRGHENSYU-SEC       SECTION.
      *
           INITIALIZE                  SPA-NAI-INPUT-G
           INITIALIZE                  SPA-GMN-INPUT-G
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL  (FLG-PRGOPTION   =   1   )
                      OR  (IDY             >=  50  )
               EVALUATE    PRGOPTION-KBNCD
      *            オプション説明
                   WHEN    "EXPLANATION"
                       MOVE    PRGOPTION-OPTION
                                               TO  SPA-GMN-EXPLANTION
                       MOVE    1               TO  SPA-NAI-EXP
                       MOVE    PRGOPTION-OPTION
                                               TO  SPA-NAI-ADDKBN
                   WHEN    OTHER
                       ADD     1               TO  IDY
                       MOVE    PRGOPTION-KBNCD TO  SPA-GMN-KBNCDL
                                                              (IDY)
               END-EVALUATE
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           END-PERFORM
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDY                 TO  SPA-KBNCD-MAX
           IF      SPA-KBNCD-MAX       =   ZERO
      *        オプション内容がない時
               MOVE    "*"                 TO  SPA-GMN-KBNCDL (1)
               MOVE    SPA-GMN-KBNCDL (1)  TO  SPA-GMN-KBNCD-G
               MOVE    1                   TO  SPA-KBNCD-MAX
           ELSE
      *        １件目を表示とする
               MOVE     SPA-GMN-KBNCDL (1) TO  SPA-GMN-KBNCD-G
               PERFORM 410231-KBNCD-HENSYU-SEC
           END-IF
      *
           MOVE    SPA-GMN-PRGID       TO  SPA-MAE-PRGID
           MOVE    SPA-GMN-KBNCD       TO  SPA-MAE-KBNCD
      *
           IF      SPA-GMN-EXPLANTION  =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *
           .
       40021-PRGHENSYU-EXT.
           EXIT.
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
           MOVE    W34-PRGID           TO  SPA-GMN-PRGID
      *    提供分は変更不可
           IF     (SPA-NAI-EXP         =   1   )  AND
                  (SPA-NAI-ADDKBN      =   "0" )
               CONTINUE
           ELSE
               MOVE    W34-EXPLANTION      TO  SPA-GMN-EXPLANTION
           END-IF
           MOVE    W34-KBNCD           TO  SPA-GMN-KBNCD-G
           MOVE    W34-OPTION          TO  SPA-GMN-OPTION
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
           IF      SPA-GMN-PRGID   NOT =   SPA-MAE-PRGID
               PERFORM 41021-PRGID-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-EXPLANTION-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-KBNCD   NOT =   SPA-MAE-KBNCD )
               PERFORM 41023-KBNCD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41024-OPTION-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プログラム名チェック処理
      *****************************************************************
       41021-PRGID-CHK-SEC             SECTION.
      *
      *    半角であること
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-PRGID       TO  KANACHK-MAE-INPUT
           MOVE    25                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-PRGID
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-SYUBETU     =   1    )  AND
                  (KANACHK-RC2         =   ZERO )
      *        半角で間に空白がないこと
      *        プログラム名からの編集
               PERFORM 40021-PRGHENSYU-SEC
           ELSE
               INITIALIZE                  SPA-GMN-INPUT-G
               INITIALIZE                  SPA-NAI-INPUT-G
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       41021-PRGID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オプション説明チェック処理
      *****************************************************************
       41022-EXPLANTION-CHK-SEC          SECTION.
      *
      *    半角カナは変換する
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    44000               TO  KANACONV-LEN
           MOVE    44000               TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    SPA-GMN-EXPLANTION  TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  SPA-GMN-EXPLANTION
           .
       41022-EXPLANTION-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オプション内容チェック処理
      *****************************************************************
       41024-OPTION-CHK-SEC          SECTION.
      *
      *    半角カナは変換する
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    44000               TO  KANACONV-LEN
           MOVE    44000               TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    SPA-GMN-OPTION      TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  SPA-GMN-OPTION
           .
       41024-OPTION-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実行形式チェック処理
      *****************************************************************
       41023-KBNCD-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-KBNCD       =   SPACE
      *        未選択は１
               MOVE    SPA-GMN-KBNCDL(1)   TO  SPA-GMN-KBNCD-G
           END-IF
      *    半角であること
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-KBNCD       TO  KANACHK-MAE-INPUT
           MOVE    16                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-KBNCD
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-SYUBETU     =   1    )  AND
                  (KANACHK-RC2         =   ZERO )
      *        半角で間に空白がないこと
               CONTINUE
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      41023-KBNCD-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-KBNCD-MAX ) OR
                              (FLG-OK  =   1        )
               IF      SPA-GMN-KBNCD       =   SPA-GMN-KBNCDL
                                                         (IDY)
                    MOVE   SPA-GMN-KBNCDL (IDY)
                                               TO  SPA-GMN-KBNCD-G
                    MOVE    1                  TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        区分追加
               IF      SPA-KBNCD-MAX       >=  50
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               ELSE
                   ADD     1                   TO  SPA-KBNCD-MAX
                   MOVE    SPA-GMN-KBNCD-G     TO  SPA-GMN-KBNCDL
                                                        (SPA-KBNCD-MAX)
               END-IF
           END-IF
           MOVE    SPA-GMN-KBNCD       TO  SPA-MAE-KBNCD
           PERFORM 410231-KBNCD-HENSYU-SEC
      *
           .
       41023-KBNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    区分コードからオプション内容表示処理
      *****************************************************************
       410231-KBNCD-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
           MOVE    SPA-GMN-KBNCD       TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   ZERO
               MOVE    PRGOPTION-OPTION    TO  SPA-GMN-OPTION
           ELSE
               MOVE    SPACE               TO  SPA-GMN-OPTION
           END-IF
           MOVE    5                   TO  SPA-GMN-CUR
      *
           .
       410231-KBNCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W34"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
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
      *    クリア　処理
      *****************************************************************
       420-CLREA-SEC                  SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               PERFORM 310-SPASET-SEC
               MOVE    1                   TO  FLG-GMN-INIT
           ELSE
               MOVE    SPACE               TO  SPA-GMN-PRGID
               INITIALIZE                  SPA-GMN-INPUT-G
               INITIALIZE                  SPA-NAI-INPUT-G
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *       前頁処理
      *****************************************************************
       460-MAE-PAGE-SEC              SECTION.
      *
           IF      SPA-GMN-PAGE        <=   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
           PERFORM 3101-PGLIST-HEN-SEC
           .
       460-MAE-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *       次頁処理
      *****************************************************************
       470-NEXT-PAGE-SEC              SECTION.
      *
           IF      SPA-GMN-NEXT        =   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   +   1
           END-IF
           PERFORM 3101-PGLIST-HEN-SEC
           .
       470-NEXT-PAGE-XT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    更新前のチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-KOUSIN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              EVALUATE    SPA-NAI-B08-CNT
              WHEN    2
                  MOVE    "U001"       TO  SPA-WIDCD
              WHEN    3
                  MOVE    "I001"       TO  SPA-WIDCD
              WHEN    OTHER
                  MOVE    "1001"       TO  SPA-WIDCD
              END-EVALUATE
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       更新前のチェック処理
      *****************************************************************
       420-KOUSIN-CHK-SEC           SECTION.
      *
           IF     (SPA-GMN-PRGID       =   SPACE)
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN-KBNCD       =   SPACE)
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               ELSE
                   IF     (SPA-GMN-OPTION      =   SPACE)
                       MOVE    "0012"          TO  SPA-ERRCD
                       MOVE    5               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       420-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           IF    ( SPA-WIDCD     =     "U001" OR "I001" )
               MOVE    SPACE               TO  ASS-PRGOPT
               INITIALIZE  ASS-PRGOPT
               STRING      "/tmp/ORCADBR-prgoption-"
                                           DELIMITED   BY  SIZE
                           SPA-GMN-PRGID   DELIMITED   BY  SPACE
                           ".dat"          DELIMITED   BY  SIZE
               INTO        ASS-PRGOPT
               END-STRING
               OPEN    OUTPUT    MST-PRGOPT
           END-IF
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *    オプション説明登録
           IF     (SPA-NAI-EXP         =   1     )  AND
                  (SPA-NAI-ADDKBN      =   "0"   )
               CONTINUE
           ELSE
               IF      SPA-GMN-EXPLANTION  NOT =   SPACE
      *            登録
                   PERFORM 4901-EXPLAN-SYORI-SEC
               END-IF
           END-IF
      *    オプション内容登録
           PERFORM 4902-OPTION-SYORI-SEC
      *
           IF    ( SPA-WIDCD     =     "U001" OR "I001" )
               CLOSE    MST-PRGOPT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       オプション説明更新処理
      *****************************************************************
       4901-EXPLAN-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
           MOVE    "EXPLANATION"       TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   1
      *        新規登録
               MOVE    SPACE               TO  PRGOPTION-REC
               INITIALIZE                      PRGOPTION-REC
               MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
               MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
               MOVE    "EXPLANATION"       TO  PRGOPTION-KBNCD
               MOVE    SPA-GMN-EXPLANTION  TO  PRGOPTION-OPTION
               MOVE    "1"                 TO  PRGOPTION-ADDKBN
               PERFORM 4908-PRGOPTION-INS-SEC
           ELSE
      *        更新
               MOVE    SPA-GMN-EXPLANTION  TO  PRGOPTION-OPTION
               PERFORM 4909-PRGOPTION-UPD-SEC
           END-IF
           .
       4901-EXPLAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション登録処理
      *****************************************************************
       4908-PRGOPTION-INS-SEC           SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  PRGOPTION-CREYMD
           MOVE    SMCNDATE-YMD        TO  PRGOPTION-UPYMD
           MOVE    SMCNDATE-HMS        TO  PRGOPTION-UPHMS
           MOVE    SPA-OPID            TO  PRGOPTION-OPID
      *
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W34 DB INS ERR KEY:" PRGOPTION-KEY 
           END-IF
      *
           IF    ( SPA-WIDCD     =     "U001" OR "I001" )
               PERFORM 4910-MSTPRGORT-EDT-SEC
           END-IF
      *
           .
       4908-PRGOPTION-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション更新処理
      *****************************************************************
       4909-PRGOPTION-UPD-SEC          SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  PRGOPTION-UPYMD
           MOVE    SMCNDATE-HMS        TO  PRGOPTION-UPHMS
           MOVE    SPA-OPID            TO  PRGOPTION-OPID
      *
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W34 DB UPD ERR KEY:" PRGOPTION-KEY 
           END-IF
      *
           IF    ( SPA-WIDCD     =     "U001" OR "I001" )
               PERFORM 4910-MSTPRGORT-EDT-SEC
           END-IF
           .
       4909-PRGOPTION-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションマスタ更新データ編集処理
      *****************************************************************
       4910-MSTPRGORT-EDT-SEC          SECTION.
      *
           INITIALIZE                      PRGWK-REC
           MOVE    ZERO                TO  PRGWK-HOSPNUM
           MOVE    PRGOPTION-PRGID     TO  PRGWK-PRGID
           MOVE    PRGOPTION-KBNCD     TO  PRGWK-KBNCD
           MOVE    "0"                 TO  PRGWK-ADDKBN
           MOVE    SPACE               TO  PRGWK-TERMID
           MOVE    SPACE               TO  PRGWK-OPID
           MOVE    SPACE               TO  PRGWK-CREYMD
           MOVE    SPACE               TO  PRGWK-UPYMD
           MOVE    SPACE               TO  PRGWK-UPHMS
      *
           MOVE    "gsub"              TO  ORCSSTR-COMMAND
           MOVE    PRGOPTION-OPTION    TO  ORCSSTR-STRING1
           MOVE    X"0A"               TO  ORCSSTR-STRING2
           MOVE    "\n"                TO  ORCSSTR-STRING3
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  PRGWK-OPTION
      *
           IF    ( SPA-WIDCD     =     "U001" )
               MOVE    "U"             TO  PRGWK-SYORIKBN
           ELSE
               MOVE    "I"             TO  PRGWK-SYORIKBN
           END-IF
      *
           WRITE   PRGWK-REC
      *
           .
       4910-MSTPRGORT-EDT-EXT.
           EXIT.
      *****************************************************************
      *       オプション内容更新処理
      *****************************************************************
       4902-OPTION-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
           MOVE    SPA-GMN-KBNCD       TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   1
      *        新規登録
               MOVE    SPACE               TO  PRGOPTION-REC
               INITIALIZE                      PRGOPTION-REC
               MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
               MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
               MOVE    SPA-GMN-KBNCD       TO  PRGOPTION-KBNCD
               MOVE    SPA-GMN-OPTION      TO  PRGOPTION-OPTION
               MOVE    "1"                 TO  PRGOPTION-ADDKBN
               PERFORM 4908-PRGOPTION-INS-SEC
           ELSE
      *        更新
               MOVE    SPA-GMN-OPTION      TO  PRGOPTION-OPTION
               PERFORM 4909-PRGOPTION-UPD-SEC
           END-IF
           .
       4902-OPTION-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SYORI-SEC        SECTION.
      *
           IF     (SPA-GMN-PRGID       =   SPACE)  OR
                  (SPA-GMN-KBNCD       =   SPACE)
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
      *ver.4.7
               IF      SPA-KBNCD-MAX       >   1
                   MOVE    "1004"          TO  SPA-WIDCD
               ELSE
                   MOVE    "1003"          TO  SPA-WIDCD
               END-IF
           END-IF
            .
       430-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力処理
      *****************************************************************
       4100-ENTER-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    99              TO  SPA-GMN-CUR
           END-IF
           .
       4100-ENTER-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC           SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    SPA-GMN-PRGID       TO  PRGOPTION-PRGID
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           PERFORM UNTIL    FLG-PRGOPTION  =   1
               IF      (PRGOPTION-KBNCD    =   "EXPLANATION" )
                   AND (PRGOPTION-ADDKBN   =   "0"           )
      *            提供分は削除なし
                   CONTINUE
               ELSE
                   IF      PRGOPTION-KBNCD     =   SPA-GMN-KBNCD
                       PERFORM 4301-PRGOPTION-DEL-SEC
                   ELSE
      **************   IF      SPA-GMN-KBNCD   =   "*"
                       IF      SPA-KBNCD-MAX       <=  1
                           PERFORM 4301-PRGOPTION-DEL-SEC
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PRGOPTION-READ-SEC
           END-PERFORM
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
            .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-PRGOPTION-DEL-SEC           SECTION.
      *
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "W34 DB UPD ERR KEY:" PRGOPTION-KEY 
           END-IF
           .
       4301-PRGOPTION-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    "W34"               TO  MCP-WINDOW.
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
           MOVE    W34-ROW             TO  WRK-PRGLST-ROW
      *
           MOVE    SPACE               TO  W34
           INITIALIZE                      W34
      *
           IF    ( FLG-GMN-INIT        =   ZERO )
               MOVE    WRK-PRGLST-ROW  TO  W34-ROW
           END-IF
      *
           MOVE    SPA-GMN-PRGID       TO  W34-PRGID
           MOVE    SPA-GMN-EXPLANTION  TO  W34-EXPLANTION
           MOVE    SPA-GMN-OPTION      TO  W34-OPTION
      *
           MOVE    SPA-GMN-KBNCD-G     TO  W34-KBNCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KBNCD-MAX
               MOVE    SPA-GMN-KBNCDL(IDX)
                                           TO  W34-KBNCD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KBNCD-MAX       TO  W34-KBNCD-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               MOVE    SPA-GMN-TNUM  (IDX)
                                           TO  W34-TNUM (IDX)
               MOVE    SPA-GMN-TPRGID(IDX)
                                           TO  W34-TPRGID(IDX)
               IF      SPA-GMN-SELNUM      =   SPA-GMN-TNUM  (IDX)
                   MOVE    "T"             TO  W34-PRGLST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W34-PRGLST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-TBL-MAX         TO  W34-PRGLST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  W34-SELNUM
      *
      *    前頁
           IF      SPA-GMN-PAGE            =   1
               MOVE    WIDGET-INSENSITIVE  TO  W34-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W34-B06-STATE
           END-IF
      *    次頁
           IF      SPA-GMN-NEXT            =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  W34-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W34-B07-STATE
           END-IF
      *
           IF      W34-OPTION          =   SPACE
               MOVE    "black"             TO  W34-LABEL215-STYLE
           ELSE
               MOVE    "red"               TO  W34-LABEL215-STYLE
           END-IF
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
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "プログラム名は半角英数字で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "区分コードエラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   STRING  "区分コードが５０件登録済みです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                                     INTO  WRK-WIDMSG
                   END-STRING
               WHEN    "0010"
                   MOVE    "プログラム名を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "プログラム名を選択して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "区分を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "オプション内容を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "登録ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W34"                TO  SPA-MOTOPG
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
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "U001"
               WHEN    "I001"
                   STRING  "オプション情報を登録します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                                     INTO  WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "表示されているオプション情報を削除します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                                     INTO  WRK-WIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "1004"
                   STRING  "表示されている区分コードのオプション情報を"
                           "削除します。"
                           "よろしいですか？"
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
      *
           MOVE    "W34"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE     "SELNUM"          TO  MCP-WIDGET
                WHEN    2
                    MOVE     "PRGID"           TO  MCP-WIDGET
                WHEN    3
                    MOVE     "EXPLANTION"      TO  MCP-WIDGET
                WHEN    4
                    MOVE     "KBNCD"           TO  MCP-WIDGET
                WHEN    5
                    MOVE     "OPTION"          TO  MCP-WIDGET
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    "PRGID"
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "KBNCD"
                   MOVE    5                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-PRGOPTION-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PRGOPTION-REC
               MOVE    ZERO            TO  FLG-PRGOPTION
           ELSE
               MOVE    1               TO  FLG-PRGOPTION
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
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
       990-DBCLOSE-SEC           SECTION.
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
