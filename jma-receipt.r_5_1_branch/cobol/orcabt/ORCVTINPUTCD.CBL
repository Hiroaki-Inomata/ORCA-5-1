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
       PROGRAM-ID.                 ORCVTINPUTCD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理
      *                                 （点数マスター情報：入力コード）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/08/21    山本          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-伊藤     01/12/12  ＤＢコールインターフェイス
      *                                     の変更  
      *  01.00.02    NACL-森脇    05/07/13  エラーメッセージ、コミット
      *  01.00.03    NACL-森脇    05/09/28  構造変更
      *  01.00.04    NACL-多々納  06/04/26  チェック追加
      *  03.05.00    NACL-多々納  07/05/16  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  PARA-FILE       ASSIGN
                                   ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    点数マスター情報ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN
                                   ASS-CSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *
      *    出力エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-LST.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-RECKBN             PIC X(01).
           03  PARA-DATKBN             PIC X(04).
           03  FILLER                  PIC X(01).
           03  PARA-DATA               PIC X(100).
      *
      *    点数マスター情報ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(3000).
           03  CSV-RR  REDEFINES       CSV-R.
               05  CSV-X               PIC X(01)  OCCURS   3000.
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PARA                PIC X(256).
           03  ASS-CSV                 PIC X(256).
           03  ASS-ERR                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-LST                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
           03  FLG-KANJI               PIC 9(01).
      *
           03  FLG-ERRARI              PIC 9(01).
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-ERR             PIC 9(01).
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-ICD                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDX-C                   PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-09-5               PIC X(06).
           03  PARA-11-5               PIC X(06).
           03  PARA-HOSPNUM            PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
           03  WRK-DATA                PIC X(200).
           03  WRK-LEN                 PIC 9(03).
           03  WRK-CNT                 PIC 9(04).
           03  WRK-LEN-W               PIC 9(03).
      *
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-INPUTCD-AREA.
               05  WRK-INPUTCD         PIC X(20).
               05  WRK-CDSYU           PIC X(01).
               05  WRK-DSPSEQ          PIC 9(02).
               05  WRK-SRYCD           PIC X(09).
               05  WRK-NUMCNT          PIC 9(04).
               05  WRK-SPCFLG          PIC 9(01).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(25) VALUE
                                       "【PGID:ORCVTINPUTCD.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(10) VALUE "LINENO ERR".
                   07  FILLER          PIC X(42) VALUE
                   " [1]必須未入力        [2]項目桁数不正     ".
                   07  FILLER          PIC X(41) VALUE
                   " [3]誤入力            [4]ＣＤ件数オーバー".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]点数情報取得失敗  [6]入力ＣＤ出力失敗".
                   07  FILLER          PIC X(41) VALUE
                   " [7]入力ＣＤ登録済み  [8]表示キー桁数不正".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
      *    エラー番号１・２・３　で使用
           03  ERR-REC1.
               05  ERR1-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR1-NAIYO          PIC X(03).
               05  ERR-OCCURS          OCCURS   4.
                   07  FILLER          PIC X(01).
                   07  ERR1-ERRNO      PIC X(03).
                   07  ERR1-KOMOKU     PIC X(09).
                   07  ERR1-ATAI       PIC X(08).
      *    エラー番号４・５・６　で使用
           03  ERR-REC2.
               05  ERR2-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR2-NAIYO          PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-ERRNO          PIC X(03).
               05  ERR2-SRYCDMES       PIC X(09).
               05  ERR2-SRYCD          PIC X(09).
               05  FILLER              PIC X(03).
               05  ERR2-DSPSEQMES      PIC X(09).
               05  ERR2-DSPSEQ         PIC 9(02).
               05  FILLER              PIC X(10).
               05  ERR2-INPUTCDMES     PIC X(09).
               05  ERR2-INPUTCD        PIC X(20).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    入力コード
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    共通パラメタ
           COPY    "MCPAREA".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-CSV     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           MOVE    SPACE           TO  ASS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    ASS-PARA
           END-UNSTRING
      *
      *    パラメータ情報取得
           PERFORM 101-PARA-GET-SEC
           IF  FLG-PARA            =   2
               MOVE    1               TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTINPUTCD)* HOSPNUM ERR [" PARA-HOSPNUM"]"
               GO  TO  100-INIT-EXT
           END-IF
      *
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYSYMD (3:)
           ADD     2000            TO  WRK-SYSYY
           MOVE    WRK-SYSYMD      TO  SPA-SYSYMD
           MOVE    SPA-SYSYMD(1:4) TO  MES-TITLE1-YY
           MOVE    SPA-SYSYMD(5:2) TO  MES-TITLE1-MM
           MOVE    SPA-SYSYMD(7:2) TO  MES-TITLE1-DD
      *
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    帳票ＯＰＥＮ処理
           OPEN    INPUT               CSV-FILE
           IF  STS-CSV             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTINPUTCD)* INPUTCD-FILE OPN STS["
                       STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2990-ERRLST-INIT-SEC
           IF  FLG-CSV                 =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 110-CP1001-KENSAKU-SEC
           IF  FLG-CSV                 =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-09-5       TO  WRK-COMMIT
           MOVE    PARA-11-5       TO  WRK-ERR
      *
      *    ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得
      *****************************************************************
       101-PARA-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-PARA
           MOVE    SPACE           TO  PARA-SAVE
           OPEN    INPUT               PARA-FILE
           IF      STS-PARA        NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTINPUTCD)* PARA-FILE OPN STS["
                       STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL   FLG-PARA  =  1
               IF  PARA-RECKBN         =   "@"
                   PERFORM 101-PARA-GET1-SEC
               END-IF
               PERFORM  900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-09-5           IS  NUMERIC )   AND
                 ( PARA-11-5           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
           END-IF
      *
           .
       101-PARA-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得（１）
      *****************************************************************
       101-PARA-GET1-SEC           SECTION.
      *
           EVALUATE    PARA-DATKBN
               WHEN    "01-5"
                   MOVE  PARA-DATA     TO  ASS-CSV
               WHEN  "02-5"
                   MOVE  PARA-DATA     TO  ASS-ERR
               WHEN  "09-5"
                   MOVE  PARA-DATA     TO  PARA-09-5
               WHEN  "11-5"
                   MOVE  PARA-DATA     TO  PARA-11-5
               WHEN    "HN"
                   MOVE    PARA-DATA     TO  PARA-HOSPNUM
           END-EVALUATE
      *
           .
       101-PARA-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       110-CP1001-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
           MOVE    WRK-SYSYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF  FLG-SYSKANRI            =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       110-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF  CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTINPUTCD)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット処理を行う
           IF  CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理を抜ける
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           INITIALIZE                  WRK-INPUTCD-AREA
      *
           INITIALIZE                  IDX-AREA
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    入力コードＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、またはエラー発生まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   20   )
                   OR      ( WRK-PO-ED     >=  3000 )
                   OR      ( FLG-ERRARI    =   1 )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE     WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF  ( CSV-R (WRK-PO-ST:)    =   SPACE )  OR
                   ( FLG-DATAEND           =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1           TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 210-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 220-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 240-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
      *    エラーチェック
           IF      FLG-ERRARI           NOT =  ZERO
      *        エラーリスト出力処理
               PERFORM 2950-ERRLST-OUT-SEC
           END-IF
      *
      *    ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       210-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
           PERFORM VARYING   IDY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDY   >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF  ( CSV-X (IDY)       =   ","  )   OR
                   ((IDX               =   20   )  AND
                    (CSV-X (IDY)       =   SPACE))
                   MOVE    IDY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDY) NOT =   SPACE
                       MOVE    IDY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF  CSV-X (IDY)         =   """"
                   IF  FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       210-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       220-KOUMOKU-KYOTU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF  FLG-DELI1               =   1
      *        デリミタ(")を含んでいる場合
               IF  FLG-DELI2           =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF  WRK-WK              =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF  WRK-PO-ST           =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF  FLG-DELI1               =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE   WRK-MO-ST     =   WRK-PO-ST   +   1
               IF  FLG-DELI2           =   1
                   COMPUTE   WRK-MO-CNT    =   WRK-PO-ED
                                           -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE   WRK-MO-CNT    =  (WRK-PO-ED   -   1)
                                           -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST   TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT  =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( CSV-REC (WRK-MO-ST:WRK-MO-CNT)  =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           .
       220-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別処理
      *****************************************************************
       240-KOUMOKU-BETU-SEC        SECTION.
      *
           EVALUATE    WRK-KOUMOKU-NO
      *    １：診療コード
           WHEN    1
               PERFORM 2410-CD-SET-SEC
      *    １４：レセプト電算コード
           WHEN    14
               PERFORM 2410-SRYCD-SET-SEC
      *    １６：入力コード１
           WHEN    16
               PERFORM 2450-INPUTCD-SET-SEC
      *    １７：入力コード２
           WHEN    17
               PERFORM 2450-INPUTCD-SET-SEC
      *    １８：入力コード３
           WHEN    18
               PERFORM 2450-INPUTCD-SET-SEC
      *    １９：入力コード４
           WHEN    19
               PERFORM 2450-INPUTCD-SET-SEC
      *    ２０：入力コード５
           WHEN    20
               PERFORM 2450-INPUTCD-SET-SEC
           END-EVALUATE
      *
           IF      FLG-ERR     NOT =   ZERO
               PERFORM 290-ERRREC-EDIT-SEC
           END-IF
      *
           .
       240-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理
      *****************************************************************
       2402-ZENKAKU-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
           MOVE    WRK-CNT             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-DATA
           MOVE    KANACHK-MAX         TO  WRK-LEN
           IF      KANACHK-RC      NOT =   ZERO
      *        全角・半角混在の場合は半角を全角へ
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
               MOVE    WRK-CNT             TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-MAX         TO  WRK-LEN
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-DATA
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    2               TO  FLG-ERR
               END-IF
           ELSE
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               END-IF
           END-IF
      *    空白を削除
           IF      WRK-LEN             >   ZERO
               PERFORM 24021-KUHAKU-DEL-SEC
           END-IF
           .
       2402-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角空白削除処理
      *****************************************************************
       24021-KUHAKU-DEL-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    1                   TO  IDZ
           MOVE    ZERO                TO  WRK-LEN-W
           IF      FLG-KANJI           =   1
      *        全角空白削除
               PERFORM VARYING IDS     FROM    1   BY  2
                       UNTIL   IDS     >   WRK-LEN
                   IF      WRK-DATA (IDS:2)  =   "　"
                                             OR  SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-DATA(IDS:2) TO  WRK-INPUTCD(IDZ:2)
                       ADD     2               TO  IDZ
                       ADD     2               TO  WRK-LEN-W
                   END-IF
               END-PERFORM
           ELSE
      *        間の空白削除
               PERFORM VARYING IDS     FROM    1   BY  1
                       UNTIL   IDS     >   WRK-LEN
                   IF      WRK-DATA (IDS:1)  =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-DATA(IDS:1) TO  WRK-INPUTCD(IDZ:1)
                       ADD     1               TO  IDZ
                       ADD     1               TO  WRK-LEN-W
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-LEN-W           TO  WRK-LEN
      *
           .
       2402-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１：診療コード　処理
      *****************************************************************
       2410-CD-SET-SEC             SECTION.
      *
           IF  FLG-NODATA          =   1
               MOVE    1           TO  FLG-ERR
               GO  TO  2410-CD-SET-EXT
           END-IF
           .
       2410-CD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１４：レセプト電算コード　処理
      *****************************************************************
       2410-SRYCD-SET-SEC          SECTION.
      *
           IF  FLG-NODATA          =   1
               MOVE    1           TO  FLG-ERR
               GO  TO  2410-SRYCD-SET-EXT
           END-IF
      *
           IF  WRK-MO-CNT          NOT  =   9
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-SRYCD-SET-EXT
           END-IF
      *
           MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SRYCD
      *    点数マスタ読み込み
           PERFORM 900-TENSU-READ-SEC
           IF  FLG-TENSU           NOT = ZERO
               MOVE    5           TO  FLG-ERR
           END-IF
      *
           .
       2410-SRYCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１６−２０：入力コード　処理
      *****************************************************************
       2450-INPUTCD-SET-SEC        SECTION.
      *
           IF  FLG-NODATA          =   1
               IF      WRK-KOUMOKU-NO  =  16
                   MOVE    1           TO  FLG-ERR
               END-IF
               GO  TO  2450-INPUTCD-SET-EXT
           END-IF
      *
           IF  WRK-MO-CNT          >   20
               MOVE    2           TO  FLG-ERR
               GO  TO  2450-INPUTCD-SET-EXT
           END-IF
      *
      *    入力コードセット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    20              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
      *
           IF     (FLG-ERR         =   ZERO ) AND
                  (WRK-LEN         >   ZERO )
               PERFORM 250-INPUTCD-INSERT-SEC
           END-IF
      *
           .
       2450-INPUTCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ登録　処理
      *****************************************************************
       250-INPUTCD-INSERT-SEC     SECTION.
      *
           MOVE    ZERO            TO  WRK-DSPSEQ
      *
           PERFORM 250-CDSYU-SET-SEC
           IF      FLG-ERR         NOT =   ZERO
               GO  TO  250-INPUTCD-INSERT-EXT
           END-IF
      *
      *    重複データが無いかチェック
           PERFORM 2501-INPUTCD-CHK-SEC
           IF      FLG-ERR         NOT =   ZERO
               GO  TO  250-INPUTCD-INSERT-EXT
           END-IF
      *    連番検索
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-SRYCD           TO  ICD-SRYCD
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 940-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    ZERO                TO  WRK-DSPSEQ
           PERFORM
                   UNTIL   FLG-INPUTCD     =   1
               MOVE    ICD-DSPSEQ          TO  WRK-DSPSEQ
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 940-INPUTCD-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           ADD     1               TO  WRK-DSPSEQ
           IF      WRK-DSPSEQ      >  10
                MOVE    4              TO  FLG-ERR
                GO  TO  250-INPUTCD-INSERT-EXT
           END-IF
      *    表示キーは１０桁までとする
           IF      WRK-DSPSEQ      =   1
               IF      WRK-LEN         >   10
                    MOVE    8              TO  FLG-ERR
                    GO  TO  250-INPUTCD-INSERT-EXT
               END-IF
           END-IF
      *
      *    入力コード作成
           MOVE    SPACE       TO  INPUTCD-REC
           INITIALIZE              INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    WRK-CDSYU       TO  ICD-CDSYU
           MOVE    WRK-INPUTCD     TO  ICD-INPUTCD
           MOVE    SPACE           TO  ICD-SRYKBN
           MOVE    WRK-SRYCD       TO  ICD-SRYCD
           MOVE    WRK-DSPSEQ      TO  ICD-DSPSEQ
           MOVE    SPACE           TO  ICD-DSPNAME
           MOVE    SPA-TERMID      TO  ICD-TERMID
           MOVE    SPACE           TO  ICD-OPID
           MOVE    SPACE           TO  ICD-CREYMD
           MOVE    SPACE           TO  ICD-UPYMD
           MOVE    SPACE           TO  ICD-UPHMS
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                MOVE    6              TO  FLG-ERR
                GO  TO  250-INPUTCD-INSERT-EXT
           END-IF
           ADD     1               TO  CNT-ICD
      *
           .
       250-INPUTCD-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    コード種別　処理
      *****************************************************************
       250-CDSYU-SET-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-CDSYU
      *
           IF      FLG-KANJI           =   1
      *        全角は５文字とする
               IF      WRK-LEN             >   10
                   MOVE    3               TO  FLG-ERR
               END-IF
           ELSE
      *        半角
               IF      WRK-INPUTCD(1:WRK-LEN)  NUMERIC
      *            ４桁から６桁
                   IF      WRK-LEN        <   4   OR  >  6
                       MOVE    3               TO  FLG-ERR
                   END-IF
               END-IF
      *        １文字目判定
               IF      WRK-INPUTCD(1:1)   =   "P"
                                          OR  "S"
                                          OR  "#"
                                          OR  "$"
                   MOVE    3               TO  FLG-ERR
               END-IF
           END-IF
      *    記号は入力不可とする
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDZ     FROM    1   BY  IDX-C
                   UNTIL      (IDZ         >   20  )  OR
                              (FLG-ERR     NOT =   ZERO)
               IF      IDX-C               =   1
                   IF       WRK-INPUTCD(IDZ:1)
                                               =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                                               OR  "_"
                       MOVE    3           TO  FLG-ERR
                   END-IF
               ELSE
                   IF      WRK-INPUTCD (IDZ:2)
                                           =   "＋"   OR  "−"
                                           OR  "／"   OR  "！"
                                           OR  "．"   OR  "＊"
                       MOVE    3           TO  FLG-ERR
                   END-IF
               END-IF
           END-PERFORM
      *    コード種別
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  WRK-CDSYU
           ELSE
               IF      WRK-INPUTCD(1:WRK-LEN)
                                           NUMERIC
                   EVALUATE    WRK-LEN
                       WHEN    4
                           MOVE    "4"                 TO  WRK-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  WRK-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  WRK-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  WRK-CDSYU
               END-IF
           END-IF
           .
       250-CDSYU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    重複データチェック処理
      *****************************************************************
       2501-INPUTCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-CDSYU           TO  ICD-CDSYU
           IF      WRK-INPUTCD(20:1)   =   SPACE
               STRING    WRK-INPUTCD       DELIMITED  BY  SPACE
                         "%"               DELIMITED  BY  SIZE
                                       INTO      ICD-INPUTCD
               END-STRING
           ELSE
               MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
           END-IF
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           PERFORM
                   UNTIL  (FLG-INPUTCD     =   1 )  OR
                          (FLG-ERR     NOT =   ZERO )
               IF      ICD-INPUTCD         =   WRK-INPUTCD
                   MOVE    7               TO  FLG-ERR
               ELSE
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 940-INPUTCD-NEXT-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2501-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       290-ERRREC-EDIT-SEC         SECTION.
      *
           IF  FLG-ERR             <=  3
               ADD     1               TO  IDE
               IF  IDE                 >   4
                   GO  TO  290-ERRREC-EDIT-EXT
               END-IF
               IF  IDE                 =   1
                   ADD     1               TO  CNT-ERR
                   MOVE    SPACE           TO  ERR-REC1
                   MOVE    CNT-CSV         TO  ERR1-LINENO
                   MOVE    "ERR"           TO  ERR1-NAIYO
               END-IF
           END-IF
      *
           IF      WRK-KOUMOKU-NO  <   16
               MOVE    1           TO  FLG-ERRARI
           ELSE
               MOVE    2           TO  FLG-ERRARI
           END-IF
      *
           EVALUATE    FLG-ERR
      *    必須項目未入力エラー
           WHEN    1
      *    桁数エラー
           WHEN    2
      *    項目エラー
           WHEN    3
               PERFORM 2910-ERRREC-EDIT-BETU1-SEC
      *    入力ＣＤ件数オーバーエラー
           WHEN    4
      *    点数情報取得エラー
           WHEN    5
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
      *********PERFORM 2950-ERRLST-OUT-SEC
               MOVE    1           TO  FLG-ERRARI
      *    入力ＣＤ登録済み
           WHEN    7
           WHEN    8
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
               PERFORM 2950-ERRLST-OUT-SEC
               MOVE    0           TO  FLG-ERRARI
      *    入力ＣＤ出力エラー
           WHEN    6
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
               PERFORM 2950-ERRLST-OUT-SEC
               MOVE    0           TO  FLG-ERRARI
           END-EVALUATE
      *
           .
       290-ERRREC-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別１　処理
      *****************************************************************
       2910-ERRREC-EDIT-BETU1-SEC  SECTION.
      *
      *    エラー内容番号転送
           MOVE    "["             TO  ERR1-ERRNO(IDE)(1:1)
           MOVE    FLG-ERR (1:1)   TO  ERR1-ERRNO(IDE)(2:1)
           MOVE    "]"             TO  ERR1-ERRNO(IDE)(3:1)
      *
      *    エラー項目名称転送
           EVALUATE    WRK-KOUMOKU-NO
      *    １：診療コード
           WHEN    1
               MOVE    "診療ＣＤ:"     TO  ERR1-KOMOKU(IDE)
      *    １４：レセプト電算コード
           WHEN    14
               MOVE    "診療行為:"     TO  ERR1-KOMOKU(IDE)
      *    １６：入力コード１
           WHEN    16
               MOVE    "入力ＣＤ:"     TO  ERR1-KOMOKU(IDE)
      *    １７：入力コード２
           WHEN    17
               MOVE    "入力CD２:"     TO  ERR1-KOMOKU(IDE)
      *    １８：入力コード３
           WHEN    18
               MOVE    "入力CD３:"     TO  ERR1-KOMOKU(IDE)
      *    １９：入力コード４
           WHEN    19
               MOVE    "入力CD４:"     TO  ERR1-KOMOKU(IDE)
      *    ２０：入力コード５
           WHEN    20
               MOVE    "入力CD５:"     TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF  WRK-MO-CNT          NOT =   ZERO
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  ERR1-ATAI  (IDE)(1:8)
           END-IF
      *
           .
       2910-ERRREC-EDIT-BETU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別２　処理
      *****************************************************************
       2910-ERRREC-EDIT-BETU2-SEC  SECTION.
      *
           ADD     1               TO  CNT-ERR
           MOVE    SPACE           TO  ERR-REC2
           MOVE    CNT-CSV         TO  ERR2-LINENO
           MOVE    "["             TO  ERR2-ERRNO(1:1)
           MOVE    FLG-ERR(1:1)    TO  ERR2-ERRNO(2:1)
           MOVE    "]"             TO  ERR2-ERRNO(3:1)
           MOVE    "診療行為:"     TO  ERR2-SRYCDMES
           MOVE    "表示連番:"     TO  ERR2-DSPSEQMES
           MOVE    "入力ＣＤ:"     TO  ERR2-INPUTCDMES
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           MOVE    WRK-SRYCD       TO  ERR2-SRYCD
           MOVE    WRK-DSPSEQ      TO  ERR2-DSPSEQ
           MOVE    WRK-INPUTCD     TO  ERR2-INPUTCD
      *
           .
       2910-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       2950-ERRLST-OUT-SEC         SECTION.
      *
           IF  FLG-ERR             <=  3
               MOVE    ERR-REC1        TO      ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2        TO      ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       2950-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2990-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTINPUTCD)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2990-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2990-ERRLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    帳票出力　終了処理
           CLOSE                       CSV-FILE
           CLOSE                       ERR-FILE
      *
           DISPLAY "*(ORCVTINPUTCD)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTINPUTCD)* INPUTCD /O CNT[" CNT-ICD      "]"
           DISPLAY "*(ORCVTINPUTCD)* ERR     /O CNT[" CNT-ERR      "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＦＥＴＣＨ処理
      *****************************************************************
       900-SYSTEM-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードテーブルＦＥＴＣＨ処理
      *****************************************************************
       940-INPUTCD-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
               MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       940-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ存在チェック
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SYSYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD      TO  TNS-YUKOEDYMD
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
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
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
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル検索処理
      *****************************************************************
       900-CSV-KENSAKU-SEC         SECTION.
      *
          MOVE    SPACE            TO  CSV-R
      *
          READ    CSV-FILE
              AT  END
                  MOVE    1            TO  FLG-CSV
              NOT AT  END
                  MOVE    ZERO         TO  FLG-CSV
                  ADD     1            TO  CNT-CSV
                  ADD     1            TO  CNT-COMMIT
          END-READ
      *
          .
       900-CSV-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    パラメータファイル読み込み処理
      *****************************************************************
       900-PARA-READ-SEC           SECTION.
      *
          MOVE    SPACE            TO  PARA-REC
      *
          READ    PARA-FILE
              AT  END
                  MOVE    1            TO  FLG-PARA
              NOT AT  END
                  ADD     1            TO  CNT-PARA
          END-READ
      *
          .
       900-PARA-READ-EXT.
          EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBOPEN ERROR"
           END-IF
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBSTART ERROR"
           END-IF
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBCOMMIT ERROR"
           END-IF
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBDISCONNECT ERROR"
           END-IF
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
