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
       PROGRAM-ID.                 ORCVTUSERBYOMEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理（自院病名）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/09/11    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.06.01    NACL-森脇     11/01/13  慢性疾患区分修正
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
      *    自院病名ＣＳＶファイル
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
      *    自院病名ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(300).
           03  CSV-RR  REDEFINES       CSV-R.
               05  CSV-X               PIC X(01)  OCCURS   300.
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
       WORKING-STORAGE             SECTION.
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
           03  FLG-USBYOMEI            PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
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
           03  FLG-KONZAI              PIC 9(01).
           03  FLG-KANJI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-USBYOMEI            PIC 9(06).
           03  CNT-BUNRUI              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDI                     PIC 9(02).
           03  IDJ                     PIC 9(02).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-09-8               PIC X(06).
           03  PARA-11-8               PIC X(06).
           03  PARA-HOSPNUM            PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
      *    日付作業領域
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
      *    文字作業領域
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-CNT          PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
           03  WRK-DATA                PIC X(200).
           03  WRK-LEN                 PIC 9(03). 
           03  WRK-CNT                 PIC 9(04).
      *
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-BUNRUICD            PIC 9(03).
           03  WRK-BUNRUIMEI           PIC X(30).
           03  WRK-MOJISU              PIC 9(02).
      *
      *    エラー帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(28) VALUE
                                       "【PGID:ORCVTUSERBYOMEI.CBL】".
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
                   " [3]誤入力            [4]自院病名出力失敗".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
      *    エラー番号１・２・３で使用
           03  ERR-REC1.
               05  ERR1-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR1-NAIYO          PIC X(03).
               05  ERR-OCCURS          OCCURS   4.
                   07  FILLER          PIC X(01).
                   07  ERR1-ERRNO      PIC X(03).
                   07  ERR1-KOMOKU     PIC X(09).
                   07  ERR1-ATAI       PIC X(08).
      *    エラー番号４で使用
           03  ERR-REC2.
               05  ERR2-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR2-NAIYO          PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-ERRNO          PIC X(03).
               05  ERR2-BYOINPUTCDMES  PIC X(09).
               05  ERR2-BYOINPUTCD     PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR2-BYOMEIMES      PIC X(09).
               05  ERR2-BYOMEI         PIC X(50).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *
      *    自院病名
       01  USBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *    作業用自院病名
       01  WKUSBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC"    REPLACING
                                       //USBYO-// BY //WKUSBYO-//.
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCXKANACONV.INC".
      *
      *    病名検索
           COPY    "CPORCSBYOMEI.INC".
      *
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
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
      *    項目クリア
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
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
           IF      FLG-PARA        =   2
               MOVE    1           TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
           ELSE
               MOVE    1           TO  FLG-CSV
               DISPLAY "*(ORCVTUSERBYOMEI)* HOSPNUM ERR ["
                                                    PARA-HOSPNUM "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    システム日付セット
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
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 120-CP1001-KENSAKU-SEC
           IF      FLG-CSV         =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＣＳＶ入力ファイル　初期処理
           OPEN    INPUT   CSV-FILE
           IF      STS-CSV     NOT =   ZERO
               MOVE    1           TO  FLG-CSV
               DISPLAY "*(ORCVTUSERBYOMEI)* CSV-FILE OPN STS["
                                                         STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF      FLG-CSV         =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-09-8       TO  WRK-COMMIT
           MOVE    PARA-11-8       TO  WRK-ERR
      *
      *    自院保険ＣＳＶファイル　検索処理
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
      *    パラメタファイルOPEN
           OPEN    INPUT   PARA-FILE
           IF      STS-PARA    NOT =   ZERO
               MOVE    2           TO  FLG-PARA
               DISPLAY "*(ORCVTUSERBYOMEI)* PARA-FILE OPN STS["
                                                          STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
      *    パラメタファイル読み込み
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL   FLG-PARA    =   1
               IF      PARA-RECKBN     =   "@"
      *            パラメータ情報取得（１）
                   PERFORM 101-PARA-GET1-SEC
               END-IF
      *        パラメタファイル読み込み
               PERFORM  900-PARA-READ-SEC
           END-PERFORM
      *
      *    パラメタファイルCLOSE
           CLOSE   PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-09-8       IS  NUMERIC )   AND
                 ( PARA-11-8       IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-CSV
               GO  TO  101-PARA-GET-EXT
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
               WHEN  "01-8"
                   MOVE  PARA-DATA TO  ASS-CSV
               WHEN  "02-8"
                   MOVE  PARA-DATA TO  ASS-ERR
               WHEN  "09-8"
                   MOVE  PARA-DATA TO  PARA-09-8
               WHEN  "11-8"
                   MOVE  PARA-DATA TO  PARA-11-8
               WHEN  "HN"
                   MOVE  PARA-DATA TO  PARA-HOSPNUM
           END-EVALUATE
      *
           .
       101-PARA-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1001-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
      *        システム管理情報読み込み
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           IF      FLG-SYSKANRI    =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID TO  SPA-HOSPID
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      CNT-CSV (4:3)   =   ZERO
               DISPLAY "***(ORCVTUSERBYOMEI)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット処理を行う
           IF      CNT-COMMIT      =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO        TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理を抜ける
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1           TO  FLG-CSV
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE           TO  WKUSBYOMEI-REC
           INITIALIZE                  WKUSBYOMEI-REC
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDX-AREA
      *    対象自院病名にエラーが存在するかどうか
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
           MOVE    ZERO            TO  WRK-BUNRUICD
           MOVE    SPACE           TO  WRK-BUNRUIMEI
      *
      *    自院病名ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー４個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   8   )
                   OR      ( WRK-PO-ED     >=  300  )
                   OR      ( IDE           >   4    )
      *
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
               MOVE    ZERO        TO  FLG-AREA1
      *
               IF    ( CSV-R (WRK-PO-ST:)  =   SPACE ) OR
                     ( FLG-DATAEND         =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1           TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 210-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理デリミタチェック
               PERFORM 220-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 240-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
      *    エラーチェック
           IF      FLG-ERRARI          =   ZERO
      *        自院病名マスタ　更新処理
               PERFORM 260-USBYOMEI-INSERT-SEC
               IF      FLG-ERR     NOT =   ZERO
      *            エラーリスト編集/出力処理
                   PERFORM 270-ERRREC-EDIT-SEC
                   MOVE    2           TO  FLG-ERRARI
                   PERFORM 270-ERRLST-OUT-SEC
               END-IF
           ELSE
      *        エラーリスト出力処理
               PERFORM 270-ERRLST-OUT-SEC
           END-IF
      *
      *    自院病名ＣＳＶファイル　検索処理
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
           MOVE    WRK-PO-ST       TO  WRK-PO-ED
           PERFORM VARYING   IDY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDY       >=  300  )
                   OR      ( FLG-COMMA =   1    )
      *
      *        カンマの位置
               IF    ( CSV-X (IDY)     =   "," ) OR
                     ((IDX             =   8 )  AND
                     ( CSV-X (IDY)     =   SPACE))
                   MOVE    IDY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDY) NOT =   SPACE
                       MOVE    IDY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *
      *        デリミタ(")存在するかどうか
               IF      CSV-X (IDY)     =   """"
                   IF      FLG-DELI1   =   ZERO
                       MOVE    1           TO  FLG-DELI1
                   ELSE
                       MOVE    1           TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       210-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理デリミタチェック
      *****************************************************************
       220-KOUMOKU-KYOTU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF      FLG-DELI1       =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK          =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF      FLG-DELI1       =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE WRK-MO-ST   =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       - ( WRK-PO-ST   +   2 )
               ELSE
                   COMPUTE  WRK-MO-CNT = ( WRK-PO-ED   -   1 )
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT  =   WRK-PO-ED   -   WRK-PO-ST
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
      *    １：病名入力コード
           WHEN    1
               PERFORM 2410-BYOMEIINPUTCD-SET-SEC
      *    ２：分類コード
           WHEN    2
               PERFORM 2410-BUNRUICD-SET-SEC
      *    ３：分類名
           WHEN    3
               PERFORM 2410-BUNRUIMEI-SET-SEC
      *    ４：病名
           WHEN    4
               PERFORM 2410-BYOMEI-SET-SEC
      *    ５：カルテ病名
           WHEN    5
               PERFORM 2410-CHARTBYOMEI-SET-SEC
      *    ６：表示連番
           WHEN    6
               PERFORM 2410-DSPSEQ-SET-SEC
      *    ７：慢性疾患区分
           WHEN    7
               PERFORM 2410-MANSEIKBN-SET-SEC
      *    ８：保険病名フラグ
           WHEN    8
               PERFORM 2410-HKNBYOMEIFLG-SET-SEC
           END-EVALUATE
      *
           IF      FLG-ERR         NOT =   ZERO
               MOVE    1           TO  FLG-ERRARI
               PERFORM 270-ERRREC-EDIT-SEC
           END-IF
      *
           .
       240-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    混在チェック処理
      *****************************************************************
       2401-KONZAI-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-KONZAI
           MOVE    ZERO            TO  FLG-KANJI
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
           MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-DATA
           MOVE    KANACHK-MAX     TO  WRK-LEN
      *
      *    混在の場合エラー
           IF      KANACHK-RC      NOT =   ZERO
               MOVE    1           TO  FLG-KONZAI
               GO  TO  2401-KONZAI-CHK-EXT
           END-IF
      *     
           IF      KANACHK-SYUBETU     =   2
               MOVE    2           TO  FLG-KANJI
           ELSE
               MOVE    1           TO  FLG-KANJI
           END-IF
      *    
           IF      WRK-LEN         >   1
               PERFORM 24012-ZENKAKU-DEL-SEC
           END-IF
      *
           .
       2401-KONZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角空白削除処理
      *****************************************************************
       24012-ZENKAKU-DEL-SEC       SECTION.
      *
           COMPUTE IDZ         =   WRK-LEN     -   1
           IF      WRK-DATA (IDZ:2)    =   "　"
               MOVE    ZERO        TO  WRK-LEN
               PERFORM VARYING IDS     FROM    IDZ BY  -2
                       UNTIL   IDS     <   2
                   IF      WRK-DATA (IDS:2)    NOT =   "　"
                       COMPUTE WRK-LEN     =   IDS     +   1
                       MOVE    2           TO  IDS
                   ELSE
                       IF      IDS             =   3
                           IF      WRK-DATA (1:2)  NOT =   "　"
                               MOVE    2           TO  WRK-LEN
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM    
           END-IF
      *
           .
       24012-ZENKAKU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１：病名入力コード　処理
      *****************************************************************
       2410-BYOMEIINPUTCD-SET-SEC  SECTION.
      *
      *    データがない場合エラー
           IF      FLG-NODATA      =   1
               MOVE    1           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   10
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
      *
      *    カナチェック
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    10              TO  WRK-CNT
           PERFORM 2401-KONZAI-CHK-SEC
           IF      FLG-KONZAI      =   1
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
      *     
           IF      WRK-LEN     NOT =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  WKUSBYO-BYOMEIINPUTCD
           ELSE
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
      *
      *    ＣＤ種別チェック
           IF      FLG-KANJI       =   2 
               MOVE    "J"         TO  WKUSBYO-CDSYU
           ELSE
               MOVE    "A"         TO  WKUSBYO-CDSYU
           END-IF
      *
      *    入力コードチェック
           IF    ( WKUSBYO-BYOMEIINPUTCD(1:1)  =   "." )   OR
                 ( WKUSBYO-BYOMEIINPUTCD(1:1)  =   "/" )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
           IF    ( WKUSBYO-BYOMEIINPUTCD(WRK-LEN:1)  =   "+" ) OR
                 ( WKUSBYO-BYOMEIINPUTCD(WRK-LEN:1)  =   "-" )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
           COMPUTE WRK-MOJISU  =   WRK-LEN -   1
           IF    ( WKUSBYO-BYOMEIINPUTCD(WRK-MOJISU:2) =   "＋" )  OR
                 ( WKUSBYO-BYOMEIINPUTCD(WRK-MOJISU:2) =   "−" )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           END-IF
      *     GO  TO  2410-BYOMEIINPUTCD-SET-EXT
           IF      WRK-LEN =   7
               INITIALIZE                  ORCSNUMAREA
               MOVE    WKUSBYO-BYOMEIINPUTCD
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             =   ZERO
                   MOVE    3           TO  FLG-ERR
               ELSE
                   IF    ( WKUSBYO-BYOMEIINPUTCD(1:3)  =   "ZZZ" ) OR
                         ( WKUSBYO-BYOMEIINPUTCD(4:1)  =   "0" )   OR
                         ( WKUSBYO-BYOMEIINPUTCD(4:1)  =   "C" )
                       MOVE    3           TO  FLG-ERR
                   ELSE
      *                病名検索チェック
                       INITIALIZE                  BYOMEI-REC
                       MOVE    WKUSBYO-BYOMEIINPUTCD
                                               TO  BYO-BYOMEICD
                       MOVE    BYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "tbl_byomei"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE    "tbl_byomei"    TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 920-DBFETCH-SEC
                           IF      MCP-RC          =   ZERO
                               MOVE    ZERO            TO  FLG-BYOMEI
                           ELSE
                               MOVE    1               TO  FLG-BYOMEI
                           END-IF
                       ELSE
                           MOVE    1               TO  FLG-BYOMEI
                       END-IF
                       MOVE    "tbl_byomei"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF      BYO-BYOMEI      NOT =   SPACE
                           MOVE    3           TO  FLG-ERR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2410-BYOMEIINPUTCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２：分類コード　処理
      *****************************************************************
       2410-BUNRUICD-SET-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-BUNRUICD
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               GO  TO  2410-BUNRUICD-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   3
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-BUNRUICD-SET-EXT
           END-IF
      *
      *    数字以外はエラー
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  NUMERIC
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-BUNRUICD
           ELSE
               MOVE    3           TO  FLG-ERR
           END-IF
      *
           .
       2410-BUNRUICD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：分類名　処理
      *****************************************************************
       2410-BUNRUIMEI-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-BUNRUIMEI
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               GO  TO  2410-BUNRUIMEI-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   30
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-BUNRUIMEI-SET-EXT
           END-IF
      *
      *    カナチェック
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    30              TO  WRK-CNT
           PERFORM 2401-KONZAI-CHK-SEC
           IF    ( FLG-KONZAI      =   1 ) OR
                 ( FLG-KANJI   NOT =   2 )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BUNRUIMEI-SET-EXT
           END-IF
      *
      *    データがあればセットする      
           IF      WRK-LEN     NOT =   ZERO 
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  WRK-BUNRUIMEI
           END-IF
      *
           .
       2410-BUNRUIMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：病名　処理
      *****************************************************************
       2410-BYOMEI-SET-SEC         SECTION.
      *
      *    データがない場合エラー
           IF      FLG-NODATA      =   1
               MOVE    1           TO  FLG-ERR
               GO  TO  2410-BYOMEI-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   80
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-BYOMEI-SET-EXT
           END-IF
      *
      *    カナチェック
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    80              TO  WRK-CNT
           PERFORM 2401-KONZAI-CHK-SEC
           IF    ( FLG-KONZAI      =   1 ) OR
                 ( FLG-KANJI   NOT =   2 )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-BYOMEI-SET-EXT
           END-IF
      *
      *    データがあればセットする
           IF      WRK-LEN     NOT =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  WKUSBYO-BYOMEI
               COMPUTE WRK-MOJISU  =   WRK-LEN / 2
               MOVE    WRK-MOJISU  TO  WKUSBYO-BYOMEIMOJI
      *        病名コードセット
               PERFORM 24101-BYOMEICD-CHECK-SEC
           ELSE
               MOVE    2           TO  FLG-ERR
           END-IF
      *
           .
       2410-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目５：カルテ病名　処理
      *****************************************************************
       2410-CHARTBYOMEI-SET-SEC    SECTION.
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               GO  TO  2410-CHARTBYOMEI-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   80
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-CHARTBYOMEI-SET-EXT
           END-IF
      *
      *    カナチェック
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    80              TO  WRK-CNT
           PERFORM 2401-KONZAI-CHK-SEC
           IF    ( FLG-KONZAI      =   1 ) OR
                 ( FLG-KANJI   NOT =   2 )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-CHARTBYOMEI-SET-EXT
           END-IF
      *
      *    データがあればセットする
           IF      WRK-LEN     NOT =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  WKUSBYO-CHARTBYOMEI
               COMPUTE WRK-MOJISU  =   WRK-LEN / 2
               MOVE    WRK-MOJISU  TO  WKUSBYO-CHARTBYOMEIMOJI
           END-IF
      *
           .
       2410-CHARTBYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目６：表示連番　処理
      *****************************************************************
       2410-DSPSEQ-SET-SEC         SECTION.
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               MOVE    50          TO  WKUSBYO-DSPSEQ
               GO  TO  2410-DSPSEQ-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-DSPSEQ-SET-EXT
           END-IF
      *
      *    数字以外はエラー
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  NUMERIC
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WKUSBYO-DSPSEQ
           ELSE
               MOVE    3           TO  FLG-ERR
           END-IF
      *
           .
       2410-DSPSEQ-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目７：慢性疾患区分　処理
      *****************************************************************
       2410-MANSEIKBN-SET-SEC      SECTION.
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               GO  TO  2410-MANSEIKBN-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   2
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-MANSEIKBN-SET-EXT
           END-IF
      *
      *    該当する区分以外はエラー
           IF      WRK-MO-CNT      =   1
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "0"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
               WHEN    "7"
               WHEN    "8"
               WHEN    "9"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WKUSBYO-MANSEIKBN
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
               END-EVALUATE
           ELSE
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "00"
               WHEN    "03"
               WHEN    "04"
               WHEN    "05"
               WHEN    "07"
               WHEN    "08"
               WHEN    "09"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WKUSBYO-MANSEIKBN
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-MANSEIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目８：保険病名フラグ　処理
      *****************************************************************
       2410-HKNBYOMEIFLG-SET-SEC   SECTION.
      *
      *    データがない場合処理抜ける
           IF      FLG-NODATA      =   1
               GO  TO  2410-HKNBYOMEIFLG-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   1
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-HKNBYOMEIFLG-SET-EXT
           END-IF
      *
      *    該当する区分以外はエラー
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT) =   "1"
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WKUSBYO-HKNBYOMEIFLG
           ELSE
               MOVE    3           TO  FLG-ERR
           END-IF
      *
           .
       2410-HKNBYOMEIFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック
      *****************************************************************
       24101-BYOMEICD-CHECK-SEC    SECTION.
      *
      *    病名検索チェック
           MOVE    SPACE           TO  ORCSBYOMEIAREA
           INITIALIZE                  ORCSBYOMEIAREA
           MOVE    "1"             TO  LNK-BYO-SYORI
           MOVE    WKUSBYO-BYOMEI  TO  LNK-BYO-BYOMEI
           CALL    "ORCSBYOMEI"        USING
                                       ORCSBYOMEIAREA
                                       SPA-AREA
      *
      *    エラーチェック
           IF      LNK-BYO-KANACHK-ERR =   1
               MOVE    3               TO  FLG-ERR
               GO  TO  24101-BYOMEICD-CHECK-EXT
           END-IF
           IF      LNK-BYO-KANACHK-ERR =   2
               MOVE    2               TO  FLG-ERR
               GO  TO  24101-BYOMEICD-CHECK-EXT
           END-IF
      *
      *    病名コード数
           MOVE    LNK-BYO-BYOMEICDSU  TO  WKUSBYO-BYOMEICDSU
      *
      *    病名コードセット
           MOVE    ZERO                TO  IDJ 
           PERFORM VARYING IDI FROM  1 BY  1
                   UNTIL ( IDI >   LNK-BYO-BYOMEI-MAX )
               IF      LNK-BYO-TBYOMEICD(IDI)  NOT =   SPACE
                   ADD     1               TO  IDJ
                   MOVE    LNK-BYO-TBYOMEICD(IDI)
                                       TO  WKUSBYO-BYOMEICD(IDJ)
               END-IF
           END-PERFORM
           IF      LNK-BYO-BYOMEICDSU  <   21   
               ADD     1               TO  IDJ 
               MOVE    ALL "9"         TO  WKUSBYO-BYOMEICD(IDJ)
           END-IF 
      *
      *    疑いフラグ
           MOVE    LNK-BYO-UTAGAIFLG   TO  WKUSBYO-UTAGAIFLG
      *
      *    難病外来
           IF      LNK-BYO-NANBYOCD
                                   NOT =   ZERO
               MOVE     LNK-BYO-NANBYOCD
                                       TO  WKUSBYO-MANSEIKBN
           ELSE
      *        慢性フラグ
               IF      WKUSBYO-MANSEIKBN   =  ZERO
                   MOVE    LNK-BYO-MANSEIKBN
                                           TO  WKUSBYO-MANSEIKBN
               END-IF
           END-IF
      *
      *    修飾語でない場合
           IF      LNK-BYO-BYOMEICD-ERR    NOT =   1
               MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  WKUSBYO-BYOMEIHENFLG
           END-IF
      *
      *    病名種別
           MOVE    LNK-BYO-BYOMEISBT   TO  WKUSBYO-BYOMEISBT
      *
      *    基本病名コード
           MOVE    LNK-BYO-KHNBYOMEICD TO  WKUSBYO-KHNBYOMEICD
      *
      *    基本部位コード
           MOVE    LNK-BYO-KHNBUICD(1) TO  WKUSBYO-KHNBUICD(1)
           MOVE    LNK-BYO-KHNBUICD(2) TO  WKUSBYO-KHNBUICD(2)
           MOVE    LNK-BYO-KHNBUICD(3) TO  WKUSBYO-KHNBUICD(3)
      *
      *    分類コードがある場合
           IF    ( WRK-BUNRUICD    NOT =   ZERO )  AND 
                 ( WRK-BUNRUIMEI   NOT =   SPACE )
               MOVE    WRK-BUNRUICD    TO  WKUSBYO-BUNRUICD
           END-IF
      *
           .
       24101-BYOMEICD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ　登録処理
      *****************************************************************
       260-USBYOMEI-INSERT-SEC     SECTION.
      *
      *    必須項目チェック
           IF    ( WKUSBYO-CDSYU   =   SPACE ) OR
                 ( WKUSBYO-BYOMEI  =   SPACE ) OR
                 ( WKUSBYO-BYOMEIINPUTCD   =   SPACE )
               MOVE    4               TO  FLG-ERR
               GO  TO  260-USBYOMEI-INSERT-EXT
           END-IF
      *
      *    重複データがないかチェック
           MOVE    ZERO            TO  USBYOMEI-REC
           INITIALIZE                  USBYOMEI-REC
           MOVE    WKUSBYOMEI-REC  TO  USBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
           MOVE    USBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_userbyomei"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 940-USBYOMEI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-USBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    データがすでにある場合はエラー
           IF      FLG-USBYOMEI    =   ZERO
               MOVE    4               TO  FLG-ERR
               GO  TO  260-USBYOMEI-INSERT-EXT
           END-IF
      *
      *    自院病名追加処理
           MOVE    ZERO            TO  USBYOMEI-REC
           INITIALIZE                  USBYOMEI-REC
           MOVE    WKUSBYOMEI-REC  TO  USBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
           MOVE    SPA-TERMID      TO  USBYO-TERMID
           MOVE    USBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               MOVE    4               TO  FLG-ERR
               GO  TO  260-USBYOMEI-INSERT-EXT
           END-IF
      *
           COMPUTE CNT-USBYOMEI    =   CNT-USBYOMEI    +   1
      *
      *    分類コードがある場合
           IF    ( WRK-BUNRUICD    NOT =   ZERO )  AND 
                 ( WRK-BUNRUIMEI   NOT =   SPACE )
               PERFORM 2601-BUNRUI-INSERT-SEC
           END-IF
      *
           .
       260-USBYOMEI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスタ分類　登録処理
      *****************************************************************
       2601-BUNRUI-INSERT-SEC      SECTION.
      *
      *    重複データがないかチェック
           MOVE    ZERO            TO  USBYOMEI-REC
           INITIALIZE                  USBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
           MOVE    "C"             TO  USBYO-CDSYU
           MOVE    WRK-BUNRUICD    TO  USBYO-BYOMEIINPUTCD 
           MOVE    USBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_userbyomei"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 940-USBYOMEI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-USBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    データがすでにある場合は処理抜ける
           IF      FLG-USBYOMEI    =   ZERO
               GO  TO  2601-BUNRUI-INSERT-EXT
           END-IF
      *
      *    自院病名追加処理
           MOVE    ZERO            TO  USBYOMEI-REC
           INITIALIZE                  USBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
           MOVE    "C"             TO  USBYO-CDSYU
           MOVE    WRK-BUNRUICD    TO  USBYO-BYOMEIINPUTCD 
           MOVE    WRK-BUNRUIMEI   TO  USBYO-BYOMEI 
           MOVE    SPA-TERMID      TO  USBYO-TERMID
           MOVE    USBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               MOVE    4               TO  FLG-ERR
               GO  TO  2601-BUNRUI-INSERT-EXT
           END-IF
      *
           COMPUTE CNT-BUNRUI      =   CNT-BUNRUI      +   1
      *
           .
       2601-BUNRUI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       270-ERRREC-EDIT-SEC         SECTION.
      *
           IF      FLG-ERR         <=  3
               ADD     1               TO  IDE
               IF      IDE             >   4
                   GO  TO  270-ERRREC-EDIT-EXT
               END-IF
               IF      IDE             =   1
                   ADD     1               TO  CNT-ERR
                   MOVE    SPACE           TO  ERR-REC1
                   MOVE    CNT-CSV         TO  ERR1-LINENO
                   MOVE    "ERR"           TO  ERR1-NAIYO
               END-IF
           END-IF
      *
           EVALUATE    FLG-ERR
      *    必須項目未入力エラー
           WHEN    1
      *    項目桁数エラー
           WHEN    2
      *    誤入力エラー
           WHEN    3
               PERFORM 2910-ERRREC-EDIT-BETU1-SEC
      *    自院病名ＤＢ出力エラー
           WHEN    4
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
           END-EVALUATE
      *
           .
       270-ERRREC-EDIT-EXT.
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
      *    １：病名入力コード
           WHEN    1
               MOVE    "入力ＣＤ:"       TO  ERR1-KOMOKU(IDE)
      *    ２：分類コード
           WHEN    2
               MOVE    "分類ＣＤ:"       TO  ERR1-KOMOKU(IDE)
      *    ３：分類名
           WHEN    3
               MOVE    "分類名　:"       TO  ERR1-KOMOKU(IDE)
      *    ４：病名
           WHEN    4
               MOVE    "病名　　:"       TO  ERR1-KOMOKU(IDE)
      *    ５：カルテ病名
           WHEN    5
               MOVE    "カルテ名:"       TO  ERR1-KOMOKU(IDE)
      *    ６：表示連番
           WHEN    6
               MOVE    "表示連番:"       TO  ERR1-KOMOKU(IDE)
      *    ７：慢性疾患区分
           WHEN    7
               MOVE    "慢性区分:"       TO  ERR1-KOMOKU(IDE)
      *    ８：保険病名フラグ
           WHEN    8
               MOVE    "保険病名:"       TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF      WRK-MO-CNT  NOT =   ZERO
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                         TO  ERR1-ATAI(IDE)(1:8)
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
           MOVE    "入力ＣＤ:"     TO  ERR2-BYOINPUTCDMES
           MOVE    "自院病名:"     TO  ERR2-BYOMEIMES
      *
           MOVE    WKUSBYO-BYOMEIINPUTCD
                                   TO  ERR2-BYOINPUTCD
           MOVE    WKUSBYO-BYOMEI  TO  ERR2-BYOMEI
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           .
       2910-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       270-ERRLST-OUT-SEC          SECTION.
      *
           IF      FLG-ERRARI      =   1
               MOVE    ERR-REC1        TO  ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2        TO  ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       270-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT  ERR-FILE
      *
           IF      STS-LST     NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTUSERBYOMEI)* ERRFILE OPN STS["
                                                        STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2710-ERRLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    帳票出力　終了処理
           CLOSE   CSV-FILE
           CLOSE   ERR-FILE
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTUSERBYOMEI)* CSV       /I CNT[" CNT-CSV "]"
           DISPLAY "*(ORCVTUSERBYOMEI)* BUNRUI    /O CNT["
                                                     CNT-BUNRUI    "]"
           DISPLAY "*(ORCVTUSERBYOMEI)* USERBYOMEI/O CNT["
                                                     CNT-USBYOMEI  "]"
           DISPLAY "*(ORCVTUSERBYOMEI)* ERR       /O CNT[" CNT-ERR "]"
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
      *    自院病名マスタＦＥＴＣＨ処理
      *****************************************************************
       940-USBYOMEI-NEXT-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-USBYOMEI
           ELSE
               MOVE    1               TO  FLG-USBYOMEI
           END-IF
      *
           .
       940-USBYOMEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名ＣＳＶファイル　検索処理
      *****************************************************************
       900-CSV-KENSAKU-SEC         SECTION.
      *
           MOVE    SPACE           TO  CSV-R
      *
           READ    CSV-FILE
               AT  END
                   MOVE    1           TO  FLG-CSV
               NOT AT  END
                   MOVE    ZERO        TO  FLG-CSV
                   ADD     1           TO  CNT-CSV
                   ADD     1           TO  CNT-COMMIT
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
           MOVE    SPACE           TO  PARA-REC
      *
           READ    PARA-FILE
               AT  END
                   MOVE    1           TO  FLG-PARA
               NOT AT  END
                   ADD     1           TO  CNT-PARA
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
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
