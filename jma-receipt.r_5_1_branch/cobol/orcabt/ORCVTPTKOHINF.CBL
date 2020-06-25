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
       PROGRAM-ID.                 ORCVTPTKOHINF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理（患者公費）
      *  管理者            : 
      *  01/04/11    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/11/01  患者番号の空白までで桁数
      *                                     を編集  
      *  01.00.02    MCC-伊藤     01/12/12  患者番号構成に合わせて
      *                                     前０詰め編集を行う  
      *  01.00.03    MCC-伊藤     01/12/12  ＤＢコールインターフェイス
      *                                     の変更  
      *  01.00.04    MCC-藤原     02/01/07  適用開始年月日の編集修正
      ************************************  OpenCOBOL対応
      *  01.00.05    MCC-伊藤     02/02/14  文字と数字の変換
      *  01.01.01    NACL-森脇    02/11/28  支払区分の追加
      *  01.01.02    NACL-伊藤    03/12/03  老人保健の適用開始日の設定
      *  01.01.03    NACL-森脇    05/07/15  エラーメッセージ
      *  01.01.04    NACL-森脇    05/08/09  負担区分追加
      *  01.01.05    NACL-森脇    05/09/28  構造変更
      *  01.01.06    NACL-森脇    06/07/26  MONFUNC対応
      *  03.05.00    NACL-多々納  07/05/16  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/06  公費追加処理削除
      *  04.02.00    NACL-多々納  08/04/28  Ｈ２０．４改正対応他
      *  04.05.00    NACL-多々納  12/12/26  負担者番号全角対応
      *  04.08.00    NACL-多々納  17/09/26  法別番号６２判定追加
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
      *    入力患者公費情報ＣＳＶファイル
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
      *    分割ファイル
           SELECT  ISAM-FILE       ASSIGN
                                   ASS-ISAM
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  ISAM-KEY
                                   FILE    STATUS  IS  STS-ISAM.
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
      *    入力患者公費情報ＣＳＶファイル
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
      *    分割ファイル
       FD  ISAM-FILE.
       01  ISAM-REC.
           03  ISAM-KEY.
               05  ISAM-PTID           PIC 9(10).
               05  ISAM-KOHID          PIC 9(04).
           03  ISAM-TBL.
               05  ISAM-DATA           PIC X(400).
               05  ISAM-PTNUM          PIC X(20).
               05  ISAM-FTNKBN         PIC X(01).
               05  ISAM-CNT            PIC 9(06).
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
      *    中間レコードファイル
           03  ASS-ISAM                PIC X(256)  VALUE
               "/tmp/ORCVTPTKOHINF_ISAM".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-LST                 PIC X(02).
           03  STS-ISAM                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
           03  FLG-ISAM                PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
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
           03  FLG-OK                  PIC 9(01).
           03  FLG-YMD                 PIC 9(01).
           03  FLG-ZENKAKU             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ISAM                PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTKOHINF            PIC 9(06).
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
           03  IDM                     PIC 9(02).
      *
       01  PARA2-REC.
           03  PARA2-RECKBN            PIC X(01).
           03  PARA2-DATKBN            PIC X(02).
           03  FILLER                  PIC X(01).
           03  PARA2-IDX               PIC X(02).
           03  FILLER                  PIC X(01).
           03  PARA2-HBTNUM            PIC X(02).
           03  FILLER                  PIC X(01).
           03  PARA2-WARIAI            PIC X(01).
           03  FILLER                  PIC X(01).
           03  PARA2-FTNNUM            PIC X(08).
           03  FILLER                  PIC X(01).
           03  PARA2-STRAGE            PIC X(03).
           03  FILLER                  PIC X(01).
           03  PARA2-ENDAGE            PIC X(03).
           03  FILLER                  PIC X(01).
           03  PARA2-HKNNUM            PIC X(03).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-06-2               PIC X(08).
           03  PARA-07-00              PIC X(02).
           03  PARA-07-TBL             OCCURS 99.
               05  PARA-07-HBTNUM      PIC X(02).
               05  PARA-07-WARIAI      PIC X(01).
               05  PARA-07-FTNNUM      PIC X(08).
               05  PARA-07-STRAGE      PIC 9(03).
               05  PARA-07-ENDAGE      PIC 9(03).
               05  PARA-07-HKNNUM      PIC X(03).
           03  PARA-00-3               PIC X(01).
           03  PARA-09-3               PIC X(06).
           03  PARA-11-3               PIC X(06).
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
      *
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-DBPATH              PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-PTID                PIC 9(10).
           03  WRK-KOHID               PIC 9(10).
           03  WRK-NUM1                PIC 9(01).
           03  WRK-NUM2                PIC 9(02).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-BASEYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-AGE                 PIC 9(03).
      *
      *    新フォーマット用
       01  WRK-HEN-AREA.
           03  WRK-HEN-PTNUM           PIC X(20).
           03  WRK-HEN-FTNKBN          PIC X(01).
           03  WRK-HEN-HBTNUM          PIC X(02).
           03  WRK-HEN-TEKSTYMD        PIC X(08).
           03  WRK-HEN-TEKEDYMD        PIC X(08).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTPTKOHINF.CBL】".
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
                   07  FILLER          PIC X(37) VALUE
                   " [3]誤入力            [4]地方公費不正".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]患者番号取得失敗  [6]患者情報取得失敗 ".
                   07  FILLER          PIC X(42) VALUE
                   " [7]患者番号更新失敗  [8]患者公費出力失敗".
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
               05  ERR2-PTNUMMES       PIC X(09).
               05  ERR2-PTNUM          PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR2-PTIDMES        PIC X(09).
               05  ERR2-PTID           PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR2-KOHNUMMES      PIC X(09).
               05  ERR2-KOHNUM         PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-KOHIDMES       PIC X(05).
               05  ERR2-KOHID          PIC X(02).
               05  FILLER              PIC X(01).
               05  ERR2-TEKSTEDMES     PIC X(05).
               05  ERR2-TEKSTYMD       PIC X(08).
               05  ERR2-TEKBAR         PIC X(01).
               05  ERR2-TEKEDYMD       PIC X(08).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *    分割レコードがあれば処理
      **** PERFORM 250-PTKOHINF-BUNKATU-SET-SEC
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
               MOVE    1           TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTKOHINF)* HOSPNUM ERR [" PARA-HOSPNUM"]"
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
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 120-CP1001-KENSAKU-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 120-CP1009-KENSAKU-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＣＳＶ入力ファイル　初期処理
           OPEN    INPUT           CSV-FILE
           IF  STS-CSV         NOT =   ZERO
               MOVE    1           TO  FLG-CSV
               DISPLAY "*(ORCVTPTKOHINF)* PTKOH-FILE OPN STS["
                                       STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      **** 分割レコードファイル　初期処理
      *    MOVE    "/tmp/ORCVTPTKOHINF_ISAM"
      ****                         TO  ASS-ISAM
      *    ISAMファイルクリア
           OPEN    OUTPUT              ISAM-FILE
           CLOSE                       ISAM-FILE
      *
           OPEN    I-O                 ISAM-FILE
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-3       TO  WRK-COMMIT
           MOVE    PARA-11-3       TO  WRK-ERR
      *
      *    患者公費ＣＳＶファイル　検索処理
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
           MOVE    SPACE           TO  PARA2-REC
           OPEN    INPUT               PARA-FILE
           IF      STS-PARA        NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTPTKOHINF)* PARA-FILE OPN STS["
                       STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL   FLG-PARA    =   1
               IF      PARA-RECKBN         =   "@" 
                   PERFORM 101-PARA-GET1-SEC
               END-IF
               PERFORM 900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-05-1           IS  NUMERIC )   AND
                 ( PARA-09-3           IS  NUMERIC )   AND
                 ( PARA-11-3           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
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
               WHEN    "01-3"
                   MOVE    PARA-DATA   TO  ASS-CSV
               WHEN    "02-3"
                   MOVE    PARA-DATA   TO  ASS-ERR
               WHEN    "05-1"
                   MOVE    PARA-DATA   TO  PARA-05-1
               WHEN    "06-2"
                   MOVE    PARA-DATA   TO  PARA-06-2
               WHEN    "09-3"
                   MOVE    PARA-DATA   TO  PARA-09-3
               WHEN    "00-3"
                   MOVE    PARA-DATA   TO  PARA-00-3
               WHEN    "11-3"
                   MOVE    PARA-DATA   TO  PARA-11-3
               WHEN    "HN"
                   MOVE    PARA-DATA     TO  PARA-HOSPNUM
               WHEN    OTHER
                   IF      PARA-DATKBN(1:2)    =   "07"
                       MOVE    PARA-REC          TO  PARA2-REC
                       IF    PARA2-IDX           =   "00"
                           MOVE    PARA2-HBTNUM  TO  PARA-07-00
                       ELSE
                           MOVE    PARA2-IDX     TO  IDJ
                           MOVE    PARA2-HBTNUM  TO  PARA-07-HBTNUM(IDJ)
                           MOVE    PARA2-WARIAI  TO  PARA-07-WARIAI(IDJ)
                           MOVE    PARA2-FTNNUM  TO  PARA-07-FTNNUM(IDJ)
                           MOVE    PARA2-STRAGE  TO  PARA-07-STRAGE(IDJ)
                           MOVE    PARA2-ENDAGE  TO  PARA-07-ENDAGE(IDJ)
                           MOVE    PARA2-HKNNUM  TO  PARA-07-HKNNUM(IDJ)
                       END-IF
                   END-IF
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
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1009-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1009-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF      CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTPTKOHINF)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      **** 指定した件数になったらコミット処理を行う
      *    IF  CNT-COMMIT              =   WRK-COMMIT
      *        PERFORM 900-DBCOMMIT-SEC
      *        MOVE    ZERO            TO  CNT-COMMIT
      **** END-IF
      *    エラーが指定した件数になったら処理を抜ける
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
               GO  TO  200-MAIN-EXT
           END-IF
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPACE           TO  PTKOHINF-REC
           INITIALIZE                  WRK-HEN-AREA
           MOVE    SPACE           TO  WRK-HEN-AREA
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDX-AREA
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者公費ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー４個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX   >       14 )
                   OR      ( WRK-PO-ED     >=  300  )
                   OR      ( IDE           >   4    )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF  ( CSV-R (WRK-PO-ST:)    =   SPACE )  OR
                   ( FLG-DATAEND           =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1           TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得
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
           IF      FLG-ERRARI          =   ZERO
      *        患者公費セット処理
      ******** PERFORM 250-PTKOHINF-SET-SEC
      *        患者公費中間データ出力
               PERFORM 400-PTKOHINF-ISM-SEC
               IF      FLG-ERR         NOT =   ZERO
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
      *    患者公費ＣＳＶファイル　検索処理
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
                   UNTIL   ( IDY           >=  300  )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF  ( CSV-X (IDY)       =   ","   )  OR
                   ((IDX               =   14 )  AND
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
               IF      CSV-X (IDY)         =   """"
                   IF      FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-COMMA           =   ZERO
               IF      WRK-PO-ST       NOT =   WRK-PO-ED
                   COMPUTE WRK-PO-ED       =   WRK-PO-ED   +   1
               END-IF
           END-IF
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
           IF      FLG-DELI1           =   1
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
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE   WRK-MO-ST     =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT      =   WRK-PO-ED  -  WRK-PO-ST
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
      *    １：患者番号
           WHEN    1
               PERFORM 2410-PTNUM-SET-SEC
      *    ２：レコード番号（公費番号）
           WHEN    2
               PERFORM 2410-RECORD-KOH-SET-SEC
      *    ３：法別番号
           WHEN    3
               PERFORM 2410-HBTNUM-SET-SEC
      *    ４：法別番号（地方公費）
           WHEN    4
               PERFORM 2410-HBTNUM-CHIHO-SET-SEC
      *    ５：負担区分
           WHEN    5
               PERFORM 2410-FTNKBN-SET-SEC
      *    ６：保険番号
           WHEN    6
               PERFORM 2410-HKNNUM-SET-SEC
      *    ７：負担者番号
           WHEN    7
               PERFORM 2410-FTNJANUM-SET-SEC
      *    ８：受給者番号
           WHEN    8
               PERFORM 2410-JKYSNUM-SET-SEC
      **    ９：公費一部負担金
      *     WHEN    9
      *         CONTINUE
      *    １０：適用開始日
           WHEN    10
               PERFORM 2410-TEKSTYMD-SET-SEC
      *    １１：適用終了日
           WHEN    11
               PERFORM 2410-TEKEDYMD-SET-SEC
      *    １２：公費確認日
           WHEN    12
               PERFORM 2410-KAKUNINYMD-SET-SEC
      *    １３：登録日
           WHEN    13
               PERFORM 2410-CREYMD-SET-SEC
      *    １４：更新日
           WHEN    14
               PERFORM 2410-UPYMD-SET-SEC
      *    フォーマットエラー
           WHEN    OTHER
      *        旧フォーマットの場合
               IF      PARA-00-3       =  "1"
                   IF      IDX         >   12
                       MOVE    3       TO  FLG-ERR
                   END-IF
               END-IF
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
      *    項目空白処理
      *****************************************************************
       2401-SPACE-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-LEN
      *
           IF      WRK-DATA            =   SPACE
               MOVE    1               TO  WRK-LEN
           ELSE
               PERFORM VARYING IDS FROM    200 BY  -1
                       UNTIL   IDS <       1
                   IF      WRK-DATA (IDS:1)    NOT =   SPACE
                       MOVE    IDS             TO  WRK-LEN
                       MOVE    1               TO  IDS
                   END-IF
               END-PERFORM
      *
      *        後ろの全角空白を削除
               IF      WRK-LEN             >   1
                   PERFORM 24021-KUHAKU-DEL-SEC
               END-IF
      *        半角英数字であること
               IF      WRK-LEN             >   1
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI
                   MOVE    WRK-DATA(1:WRK-LEN)
                                           TO  KANACHK-MAE-INPUT
                   MOVE    WRK-LEN         TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      FLG-ZENKAKU         =   1
      *                受給者番号は混在をエラー
                       IF     (KANACHK-RC          NOT =   ZERO )  OR
                              (KANACHK-SYUBETU         =   ZERO )  OR
                              (KANACHK-RC2         NOT =   ZERO )  OR
                              (WRK-DATA(1:WRK-LEN) NOT =
                                                   KANACHK-OUT-INPUT)
                           MOVE    3               TO  FLG-ERR
                       END-IF
                   ELSE
                       IF     (KANACHK-RC          =   ZERO  )  AND
                              (KANACHK-SYUBETU     =   1     )  AND
                              (KANACHK-MAE-INPUT   =
                                                   KANACHK-OUT-INPUT)
                           CONTINUE
                       ELSE
      *                    全角・混在、間の空白はエラー
                           MOVE    3               TO  FLG-ERR
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       2401-SPACE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角空白削除処理
      *****************************************************************
       24021-KUHAKU-DEL-SEC        SECTION.
      *
           COMPUTE IDZ             =   WRK-LEN     -   1
           IF      WRK-DATA (IDZ:2)    =   "　"
               MOVE    1                   TO  WRK-LEN
               PERFORM VARYING IDS     FROM    IDZ BY  -2
                       UNTIL   IDS     <   2
                   IF      WRK-DATA (IDS:2)    NOT =   "　"
                       COMPUTE WRK-LEN         =   IDS     +   1
                       MOVE    2               TO  IDS
                   ELSE
                       IF      IDS             =   3
                           IF      WRK-DATA (1:2)      NOT =   "　"
                               MOVE    2               TO  WRK-LEN
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       2402-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１：患者番号　処理
      *****************************************************************
       2410-PTNUM-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-PTNUM
      *
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-PTNUM-SET-EXT
           END-IF
      *
      *    空白までを編集する
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-HEN-PTNUM
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL ( IDZ >       WRK-MO-CNT )
                   OR    ( WRK-HEN-PTNUM (IDZ:1)   =   SPACE )
               MOVE    IDZ             TO    WRK-CNT
           END-PERFORM
           MOVE    WRK-CNT             TO  WRK-MO-CNT
      *
           IF    ( WRK-MO-CNT          =   0  )    OR
                 ( WRK-MO-CNT          >   20 )
               GO  TO  2410-PTNUM-SET-EXT
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      (( SYS-1009-PTNUMKSIKBN    =   "1" ) AND
                    ( SYS-1009-JIYKSIKBN      =   "2" ))    OR
                    (( SYS-1009-PTNUMKSIKBN   =   "2" ) AND
                     ( SYS-1009-HJNKSIKBN     =   "3" OR "4" ))
               MOVE    ZERO            TO  SPA-PTNUM(1:WRK-RENKETA)
               COMPUTE IDZ             =   WRK-RENKETA
                                       -   WRK-MO-CNT
                                       +   1
               MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM(IDZ:)
           ELSE
               MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM
           END-IF
      *
           .
       2410-PTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２：レコード番号（公費番号）　処理
      *****************************************************************
       2410-RECORD-KOH-SET-SEC     SECTION.
      *
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-RECORD-KOH-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          >   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-RECORD-KOH-SET-EXT
           END-IF
      *
           IF  ( CSV-REC (WRK-MO-ST:WRK-MO-CNT) IS NOT  NUMERIC  ) OR
               ( CSV-REC (WRK-MO-ST:WRK-MO-CNT) =   ZERO )
               MOVE    3               TO  FLG-ERR
               GO  TO  2410-RECORD-KOH-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT  =   1
               MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-NUM1
               MOVE  WRK-NUM1          TO  PTKOH-KOHID
           ELSE
               MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-NUM2
               MOVE  WRK-NUM2          TO  PTKOH-KOHID
           END-IF
      *
           .
       2410-RECORD-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：法別番号
      *****************************************************************
       2410-HBTNUM-SET-SEC         SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-HBTNUM-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   2
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-HBTNUM-SET-EXT
           END-IF
      *
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "10"  THRU   "21"
               WHEN    "23"
               WHEN    "27"  THRU   "29"
               WHEN    "50"  THRU   "53"
               WHEN    "24"
               WHEN    "25"
               WHEN    "30"
               WHEN    "38"
               WHEN    "66"
               WHEN    "79"
      *H27.2
               WHEN    "54"
      *H29.10
               WHEN    "62"
      **コメント START ---------------------------------------------
      *                                 OR  "55"  OR  "56"
      **コメント END   ---------------------------------------------
                   CONTINUE
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
                   GO  TO  2410-HBTNUM-SET-EXT
           END-EVALUATE
      *
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
      *    特定疾患（負担無し）
           WHEN    "50"
               MOVE    "091"           TO  PTKOH-KOHNUM
      **コメント START ---------------------------------------------
      **    福祉
      *     WHEN    "55"
      *         MOVE    "191"           TO  PTKOH-KOHNUM
      **    乳児
      *     WHEN    "56"
      *         MOVE    "190"           TO  PTKOH-KOHNUM
      **コメント END   ---------------------------------------------
           WHEN    OTHER
               MOVE    "0"             TO  PTKOH-KOHNUM (1:1)
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  PTKOH-KOHNUM (2:2)
           END-EVALUATE
      *
           .
       2410-HBTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：法別番号（地方公費）
      *****************************************************************
       2410-HBTNUM-CHIHO-SET-SEC   SECTION.
      *
           IF       FLG-NODATA          =   1
               IF      PTKOH-KOHNUM    =   SPACE
                   MOVE    1           TO  FLG-ERR
               END-IF
               GO  TO  2410-HBTNUM-CHIHO-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-HBTNUM-CHIHO-SET-EXT
           END-IF
      *
           IF      PTKOH-KOHNUM    NOT =   SPACE
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-HBTNUM-CHIHO-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-HEN-HBTNUM
      *
           .
       2410-HBTNUM-CHIHO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目５：負担区分
      *****************************************************************
       2410-FTNKBN-SET-SEC       SECTION.
      *
      *    旧フォーマットの場合次の項目
           IF      PARA-00-3         =  "1"
      *        老人保健なら負担区分に１をセット
               IF      PTKOH-KOHNUM   =   "027"
                   MOVE    "1"        TO  WRK-HEN-FTNKBN
               ELSE
                   MOVE    "0"        TO  WRK-HEN-FTNKBN
               END-IF
               ADD     2              TO  WRK-KOUMOKU-NO
               PERFORM 2410-FTNJANUM-SET-SEC
               GO  TO  2410-FTNKBN-SET-EXT
           END-IF
      *
           IF      FLG-NODATA          =   1
               MOVE    "0"        TO  WRK-HEN-FTNKBN
               GO  TO  2410-FTNKBN-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   1
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-FTNKBN-SET-EXT
           END-IF
      *
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-HEN-FTNKBN
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
                   GO  TO  2410-FTNKBN-SET-EXT
           END-EVALUATE
      *
           .
       2410-FTNKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目６：保険番号
      *****************************************************************
       2410-HKNNUM-SET-SEC         SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-HKNNUM-SET-EXT
           END-IF
      *
           IF     (WRK-MO-CNT          NOT =   3 )
              OR  (CSV-REC (WRK-MO-ST:WRK-MO-CNT) NOT NUMERIC)
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-HKNNUM-SET-EXT
           END-IF
      *
           IF      PTKOH-KOHNUM    =   SPACE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  PTKOH-KOHNUM
           END-IF
      *
           .
       2410-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目７：負担者番号
      *****************************************************************
       2410-FTNJANUM-SET-SEC       SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-FTNJANUM-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-FTNJANUM-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    1               TO  FLG-ZENKAKU
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTKOH-FTNJANUM
      *
           .
       2410-FTNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目８：受給者番号
      *****************************************************************
       2410-JKYSNUM-SET-SEC        SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-JKYSNUM-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >   20
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-JKYSNUM-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    1               TO  FLG-ZENKAKU
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTKOH-JKYSNUM
      *    医療観察法の受給者番号入力はエラー
           IF      PTKOH-KOHNUM    =   "030"
               IF      PTKOH-JKYSNUM   NOT =   SPACE
                   MOVE    3           TO  FLG-ERR
               END-IF
           END-IF
           .
       2410-JKYSNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１０：適用開始日　処理
      *****************************************************************
       2410-TEKSTYMD-SET-SEC       SECTION.
      *
      **** IF      FLG-NODATA          =   1
      *        MOVE    PARA-06-2   TO  WRK-HEN-TEKSTYMD
      *        GO  TO  2410-TEKSTYMD-SET-EXT
      *****END-IF
      *
           IF      WRK-MO-CNT      >   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-TEKSTYMD-SET-EXT
           END-IF
      *
           IF     (FLG-NODATA          =   1     )  OR
                  (CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       =   ALL "0")
               MOVE    PARA-06-2   TO  WRK-SYMD
           ELSE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  WRK-HEN-TEKSTYMD
           END-IF
      *
           .
       2410-TEKSTYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１１：適用終了日　処理
      *****************************************************************
       2410-TEKEDYMD-SET-SEC       SECTION.
      *
           IF      FLG-NODATA          =   1
               MOVE    ALL "9"     TO  WRK-HEN-TEKEDYMD
               GO  TO  2410-TEKEDYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-TEKEDYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
      *
           IF    ( PTKOH-KOHNUM        =   "027" ) AND
                 ( WRK-SYMD            <   "20021001" )
               MOVE    3           TO  FLG-ERR
               GO  TO  2410-TEKEDYMD-SET-EXT
           END-IF
           IF      (WRK-SYMD        =   ALL "9")
               MOVE    WRK-SYMD        TO  WRK-HEN-TEKEDYMD
               GO  TO  2410-TEKEDYMD-SET-EXT
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  WRK-HEN-TEKEDYMD
           END-IF
      *
           IF      WRK-HEN-TEKSTYMD    >   WRK-HEN-TEKEDYMD
               MOVE    3               TO  FLG-ERR
           END-IF
           .
       2410-TEKEDYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１２：公費確認日　処理
      *****************************************************************
       2410-KAKUNINYMD-SET-SEC     SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-KAKUNINYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-KAKUNINYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           IF  WRK-SYMD (7:2)      =  ZERO
               MOVE    "01"        TO  WRK-SYMD (7:2)
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTKOH-KAKUNINYMD
           END-IF
      *
           .
       2410-KAKUNINYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１３：登録日　処理
      *****************************************************************
       2410-CREYMD-SET-SEC         SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-CREYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-CREYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTKOH-CREYMD
           END-IF
      *
           .
       2410-CREYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１４：更新日　処理
      *****************************************************************
       2410-UPYMD-SET-SEC          SECTION.
      *
           IF      FLG-NODATA          =   1
               GO  TO  2410-UPYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT          NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-UPYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTKOH-UPYMD
           END-IF
      *
           .
       2410-UPYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者公費マスタＩＳＡＭ処理
      *****************************************************************
       400-PTKOHINF-ISM-SEC        SECTION.
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM           =   1
               MOVE    5               TO  FLG-ERR
           END-IF
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    6               TO  FLG-ERR
           END-IF
      *    地方公費チェック
           IF     (FLG-ERR             =   ZERO )
             AND  (PTKOH-KOHNUM        =  SPACE )
               PERFORM 4001-CHIHOU-HEN-SEC
           END-IF
      *    支払い区分セット
           IF      FLG-ERR             =   ZERO
               PERFORM 4002-PAYKBN-HEN-SEC
           END-IF
      *
           IF      FLG-ERR         NOT =   ZERO
               GO      TO      400-PTKOHINF-ISM-EXT
           END-IF
      *
      *    ＩＳＡＭ読込み
           MOVE    ZERO            TO  FLG-ISAM
           MOVE    ZERO            TO  IDI
           PERFORM UNTIL   FLG-ISAM    NOT =   ZERO
               ADD     1               TO  IDI
               INITIALIZE                  ISAM-REC
               MOVE    PTNUM-PTID      TO  ISAM-PTID
               MOVE    IDI             TO  ISAM-KOHID
      *        存在チェック
               PERFORM 900-ISAM-READ-SEC
           END-PERFORM
      *
           ADD     1                   TO  CNT-ISAM
      *
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    PTNUM-PTID          TO  PTKOH-PTID
           MOVE    SPA-TERMID          TO  PTKOH-TERMID
      *
           MOVE    WRK-HEN-TEKSTYMD    TO  PTKOH-TEKSTYMD
           MOVE    WRK-HEN-TEKEDYMD    TO  PTKOH-TEKEDYMD
      *
      *    分割レコードに追加
           INITIALIZE                      ISAM-REC
           MOVE    PTNUM-PTID          TO  ISAM-PTID
           MOVE    IDI                 TO  ISAM-KOHID
           MOVE    PTKOHINF-REC        TO  ISAM-DATA
      *
           MOVE    WRK-HEN-FTNKBN      TO  ISAM-FTNKBN
           MOVE    SPA-PTNUM           TO  ISAM-PTNUM
           MOVE    CNT-CSV             TO  ISAM-CNT
      *
           WRITE   ISAM-REC
      *
           .
       400-PTKOHINF-ISM-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費チェック処理
      *****************************************************************
       4001-CHIHOU-HEN-SEC              SECTION.
      *
      *    生年月日から年齢を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    ZERO                TO  WRK-BASEYMD
           MOVE    ZERO                TO  WRK-AGE
           MOVE    PTINF-BIRTHDAY      TO  WRK-BASEYMD
           IF          WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01              TO  WRK-BDD
           END-IF
      *    年齢
           COMPUTE WRK-AGE         =   WRK-SYSYY - WRK-BYY
           IF      WRK-AGE         >   0
               IF      WRK-SYSMM     <   WRK-BMM
                   COMPUTE WRK-AGE     =   WRK-AGE - 1
               END-IF
           END-IF
      *
           MOVE    PARA-07-00      TO  IDM
           PERFORM VARYING IDJ  FROM   1   BY      1
                   UNTIL   IDJ  >  IDM
               IF  WRK-HEN-HBTNUM      =   PARA-07-HBTNUM(IDJ)
                   MOVE    ZERO        TO  FLG-OK
                   IF      PARA-00-3   =   "2"
                       IF (PARA-07-WARIAI(IDJ) NOT =   WRK-HEN-FTNKBN)
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-IF
                   IF (PARA-07-FTNNUM(IDJ) NOT =   ZERO ) AND
                      (PARA-07-FTNNUM(IDJ) NOT =   PTKOH-FTNJANUM )
                       MOVE    1               TO  FLG-OK
                   END-IF
                   IF (PARA-07-STRAGE(IDJ)     =   "000" ) AND
                      (PARA-07-ENDAGE(IDJ)     =   "999" )
                       CONTINUE
                   ELSE
                       IF (WRK-AGE < PARA-07-STRAGE(IDJ)) OR
                          (WRK-AGE > PARA-07-ENDAGE(IDJ))
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-IF
                   IF  FLG-OK    =   ZERO
                       MOVE  PARA-07-HKNNUM(IDJ)
                                            TO  PTKOH-KOHNUM
                       MOVE  IDM            TO  IDJ
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      PTKOH-KOHNUM    =   SPACE
               MOVE    4       TO  FLG-ERR
           END-IF
      *
           .
       4001-CHIHOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    支払い区分セット処理
      *****************************************************************
       4002-PAYKBN-HEN-SEC              SECTION.
      *
      *    支払い区分セット
           IF      PTKOH-KOHNUM    =   "027"
      *****    IF      WRK-HEN-TEKEDYMD    <=  "20020930"
               IF      WRK-HEN-TEKSTYMD    <=  "20020930"
                   MOVE    "00"            TO  PTKOH-PAYKBN
               ELSE
                   MOVE    "01"            TO  PTKOH-PAYKBN
                   EVALUATE    WRK-HEN-FTNKBN
                       WHEN    "1"
                           MOVE    "01"    TO  PTKOH-PAYKBN
                       WHEN    "2"
                       WHEN    "3"
      *                    ２、３割負担は開始日による
                           IF      WRK-HEN-TEKSTYMD    >=  "20061001"
                               MOVE    "03"        TO  PTKOH-PAYKBN
                           ELSE
                               MOVE    "02"        TO  PTKOH-PAYKBN
                           END-IF
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    "00"            TO  PTKOH-PAYKBN
           END-IF
      *
           .
       4002-PAYKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       270-ERRREC-EDIT-SEC         SECTION.
      *
           IF  FLG-ERR             <=  3
               ADD     1               TO  IDE
               IF  IDE                 >   4
                   GO  TO  270-ERRREC-EDIT-EXT
               END-IF
               IF  IDE                     =   1
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
               PERFORM 2710-ERRREC-EDIT-BETU1-SEC
      *    地方公費エラー
           WHEN    4
      *    患者番号取得エラー
           WHEN    5
      *    患者情報取得エラー
           WHEN    6
      *    番号ＤＢ更新エラー
           WHEN    7
      *    患者公費ＤＢ出力エラー
           WHEN    8
               PERFORM 2710-ERRREC-EDIT-BETU2-SEC
           END-EVALUATE
      *
           .
       270-ERRREC-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別１　処理
      *****************************************************************
       2710-ERRREC-EDIT-BETU1-SEC  SECTION.
      *
      *    エラー内容番号転送
           MOVE    "["             TO  ERR1-ERRNO (IDE)(1:1)
           MOVE    FLG-ERR (1:1)   TO  ERR1-ERRNO (IDE)(2:1)
           MOVE    "]"             TO  ERR1-ERRNO (IDE)(3:1)
      *
      *    エラー項目名称転送
           EVALUATE    WRK-KOUMOKU-NO
      *    １：患者番号
           WHEN    1
               MOVE    "患者番号:"     TO  ERR1-KOMOKU(IDE)
      *    ２：レコード番号（公費番号）
           WHEN    2
               MOVE    "レコ番  :"     TO  ERR1-KOMOKU(IDE)
      *    ３：法別番号
           WHEN    3
               MOVE    "法別番号:"     TO  ERR1-KOMOKU(IDE)
      *    ４：法別番号（地方公費）
           WHEN    4
      *        法別番号・法別番号（地方公費）両方に入っている場合
               IF  ( FLG-NODATA        =   ZERO  )  AND
                   ( PTKOH-KOHNUM  NOT =   SPACE )
                   MOVE    "法別番号:"     TO  ERR1-KOMOKU(IDE)
                   MOVE    PTKOH-KOHNUM    TO  ERR1-ATAI  (IDE)(1:8)
                   ADD     1               TO  IDE
                   IF  IDE                 >   4
                       GO  TO 2710-ERRREC-EDIT-BETU1-EXT
                   END-IF
                   MOVE    "["             TO  ERR1-ERRNO (IDE)(1:1)
                   MOVE    FLG-ERR (1:1)   TO  ERR1-ERRNO (IDE)(2:1)
                   MOVE    "]"             TO  ERR1-ERRNO (IDE)(3:1)
                   MOVE    "法別地方:"     TO  ERR1-KOMOKU(IDE)
               ELSE
                   MOVE    "法別地方:"     TO  ERR1-KOMOKU(IDE)
               END-IF
      *    ５：負担区分
           WHEN    5
               MOVE    "負担区分:"     TO  ERR1-KOMOKU(IDE)
      *    ６：保険番号
           WHEN    6
               MOVE    "保険番号:"     TO  ERR1-KOMOKU(IDE)
      *    ７：負担者番号
           WHEN    7
               MOVE    "負担者番:"     TO  ERR1-KOMOKU(IDE)
      *    ８：受給者番号
           WHEN    8
               MOVE    "受給者番:"     TO  ERR1-KOMOKU(IDE)
      *    ９：公費一部負担金
           WHEN    9
               MOVE    "公費一部:"     TO  ERR1-KOMOKU(IDE)
      *    １０：適用開始日
           WHEN    10
               MOVE    "適用開始:"     TO  ERR1-KOMOKU(IDE)
      *    １１：適用終了日
           WHEN    11
               MOVE    "適用終了:"     TO  ERR1-KOMOKU(IDE)
      *    １２：公費確認日
           WHEN    12
               MOVE    "公費確認:"     TO  ERR1-KOMOKU(IDE)
      *    １３：登録日
           WHEN    13
               MOVE    "登録日  :"     TO  ERR1-KOMOKU(IDE)
      *    １４：更新日
           WHEN    14
               MOVE    "更新日  :"     TO  ERR1-KOMOKU(IDE)
      *    フォーマットエラー
           WHEN    OTHER
               MOVE    "過多項目:"     TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF  WRK-MO-CNT          NOT =   ZERO
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  ERR1-ATAI  (IDE)(1:8)
           END-IF
      *
           .
       2710-ERRREC-EDIT-BETU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別２　処理
      *****************************************************************
       2710-ERRREC-EDIT-BETU2-SEC  SECTION.
      *
           ADD     1               TO  CNT-ERR
           MOVE    SPACE           TO  ERR-REC2
           MOVE    CNT-CSV         TO  ERR2-LINENO
           MOVE    "["             TO  ERR2-ERRNO (1:1)
           MOVE    FLG-ERR(1:1)    TO  ERR2-ERRNO (2:1)
           MOVE    "]"             TO  ERR2-ERRNO (3:1)
           MOVE    "患者番号:"     TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"     TO  ERR2-PTIDMES
           MOVE    "公費番号:"     TO  ERR2-KOHNUMMES
           MOVE    "連番:"         TO  ERR2-KOHIDMES
           MOVE    "期間:"         TO  ERR2-TEKSTEDMES
      *
           MOVE    SPA-PTNUM       TO  ERR2-PTNUM
           MOVE    PTKOH-PTID      TO  ERR2-PTID
      *
           IF      PTKOH-KOHNUM    NOT =   SPACE
               MOVE    PTKOH-KOHNUM    TO  ERR2-KOHNUM
               IF      PTKOH-KOHID     <   10
                   MOVE    PTKOH-KOHID     TO  WRK-NUM1
                   MOVE    WRK-NUM1        TO  ERR2-KOHID
               ELSE
                   MOVE    PTKOH-KOHID     TO  WRK-NUM2
                   MOVE    WRK-NUM2        TO  ERR2-KOHID
               END-IF
           END-IF
      *
           IF    ( WRK-HEN-TEKSTYMD    NOT =   SPACE ) AND
                 ( WRK-HEN-TEKEDYMD    NOT =   SPACE )
               MOVE    WRK-HEN-TEKSTYMD    TO  ERR2-TEKSTYMD
               MOVE    WRK-HEN-TEKEDYMD    TO  ERR2-TEKEDYMD
               MOVE    "-"             TO  ERR2-TEKBAR
           END-IF
      *
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           .
       2710-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTKOHINF)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
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
       2710-ERRLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       270-ERRLST-OUT-SEC          SECTION.
      *
           IF  FLG-ERRARI              =   1
               MOVE    ERR-REC1        TO      ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2        TO      ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       270-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1     =   ZERO
               MOVE    0               TO  FLG-YMD
           ELSE
               MOVE    1               TO  FLG-YMD
           END-IF
           .
       31012-SEIWA-HEN-EXT.
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
           CLOSE                       ISAM-FILE
      *
           DISPLAY "*(ORCVTPTKOHINF)* CSV     /I CNT[" CNT-CSV      "]"
      *****DISPLAY "*(ORCVTPTKOHINF)* PTKOHINF/O CNT[" CNT-PTKOHINF "]"
           DISPLAY "*(ORCVTPTKOHINF)* ERR     /O CNT[" CNT-ERR      "]"
           DISPLAY "*(ORCVTPTKOHINF)* ISAM    /O CNT[" CNT-ISAM     "]"
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
           IF      MCP-RC              =   ZERO
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
      *    患者番号変換マスタ　読み込み処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTNUM       TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF          MCP-RC          =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF          MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTNUM-PTID      TO  PTINF-PTID
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF          MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスタ　読み込み処理
      *****************************************************************
       900-PTKOHINF-READ-SEC       SECTION.
      *
           MOVE    PTKOHINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  WRK-TABLE
           MOVE    "key"           TO  WRK-DBPATH
           PERFORM 900-PTKOHINF-SELECT-SEC
           PERFORM 900-PTKOHINF-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       900-PTKOHINF-SELECT-SEC     SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF          MCP-RC          =   ZERO
               PERFORM 900-PTKOHINF-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       900-PTKOHINF-FETCH-SEC      SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-DBPATH      TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF          MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       900-PTKOHINF-CLOSE-SEC      SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTKOHINF-CLOSE-EXT.
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
      *    患者公費ＣＳＶファイル　検索処理
      *****************************************************************
       900-CSV-KENSAKU-SEC         SECTION.
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
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
     *****************************************************************
      *    分割レコードファイル読込
      *****************************************************************
       900-ISAM-READ-SEC           SECTION.
      *
           READ    ISAM-FILE
               INVALID
                   MOVE    1           TO  FLG-ISAM
               NOT INVALID
                   MOVE    ZERO        TO  FLG-ISAM
           END-READ
           .
       900-ISAM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    分割レコードファイル順読込
      *****************************************************************
       900-ISAM-NEXT-SEC           SECTION.
      *
           READ    ISAM-FILE       NEXT
               AT  END
                   MOVE    1           TO  FLG-ISAM
               NOT AT  END
                   MOVE    ZERO        TO  FLG-ISAM
           END-READ
           .
       900-ISAM-NEXT-EXT.
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
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
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
