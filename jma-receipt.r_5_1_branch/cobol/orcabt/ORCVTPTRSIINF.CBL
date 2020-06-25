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
       PROGRAM-ID.                 ORCVTPTRSIINF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理
      *                     （患者労災保険・自賠責保険）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/XX    NACL-多々納   新規作成
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
      *    パラメータファイル
           SELECT  PARA-FILE       ASSIGN
                                   ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    入力患者保険情報ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN
                                   ASS-CSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *    ＩＳＡＭファイル
           SELECT  ISAM-FILE       ASSIGN
                                   ASS-ISAM
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  ISAM-KEY
                                   FILE    STATUS  IS  STS-ISAM.
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
      *    入力患者保険情報ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(300).
           03  CSV-RR  REDEFINES       CSV-R.
               05  CSV-X               PIC X(01)  OCCURS   300.
      *
      *    ＩＳＡＭファイル
       FD  ISAM-FILE.
       01  ISAM-REC.
           03  ISAM-KEY.
               05  ISAM-PTID           PIC 9(10).
               05  ISAM-RENNUM         PIC 9(02).
           03  ISAM-TBL.
               05  ISAM-HKNID          PIC 9(10).
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
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
           03  ASS-ISAM                PIC X(256).
           03  ASS-ERR                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-ISAM                PIC X(02).
           03  STS-LST                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
           03  FLG-ISAM                PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
           03  FLG-OK                  PIC 9(01).
           03  FLG-ERRARI              PIC 9(01).
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-ERR             PIC 9(01).
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
           03  FLG-YMD                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTRSIINF            PIC 9(06).
           03  CNT-PTHKNINF            PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDH                     PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-09-9               PIC X(06).
           03  PARA-11-9               PIC X(06).
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
           03  WRK-LEN2                PIC 9(03).
           03  WRK-CNT                 PIC 9(04).
      *
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-SONSHOUKBN          PIC  9(02).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-HKNID               PIC 9(10).
           03  WRK-RENNUM              PIC X(02).
           03  WRK-KOUMOKU-NO          PIC 9(04).
           03  WRK-HEN-PTNUM           PIC X(20).
      *
      *    保険番号テーブル
       01  TBL-HKNNUM-AREA.
           02  TBL-HKNNUM-REC          OCCURS  10.
           COPY    "CPHKNNUM.INC"      REPLACING
                                       //HKN-// BY //TBL-HKN-//.
       01  IDX-HKN-MAX                 PIC 9(04).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTPTRSIINF.CBL】".
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
                   07  FILLER          PIC X(42) VALUE
                   " [3]誤入力".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [4]患者番号取得失敗  [5]保険番号不一致".
                   07  FILLER          PIC X(42) VALUE
                   " [6]患者労災出力失敗  [7]患者保険出力失敗".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [9]公務災害開始誤".
               05  MES-TITLE2-L4.
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
               05  FILLER              PIC X(01).
               05  ERR1-PTNUMMES       PIC X(09).
               05  ERR1-PTNUM          PIC X(10).
               05  ERR-OCCURS          OCCURS   3.
                   07  FILLER          PIC X(01).
                   07  ERR1-ERRNO      PIC X(03).
                   07  ERR1-KOMOKU     PIC X(09).
                   07  ERR1-ATAI       PIC X(08).
      *    エラー番号５・６・７・８・９　で使用
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
               05  ERR2-HKNNUMMES      PIC X(09).
               05  ERR2-HKNNUM         PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-HKNIDMES       PIC X(05).
               05  ERR2-RENNUM         PIC X(02).
               05  FILLER              PIC X(01).
               05  ERR2-TEKSTEDMES     PIC X(05).
               05  ERR2-TEKSTYMD       PIC X(08).
               05  ERR2-TEKBAR         PIC X(01).
               05  ERR2-TEKEDYMD       PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-ISAM              PIC X(256)
               VALUE               "/var/tmp/ORCVTPTRSIINF-HKNID_ISAM".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCXKANACONV.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  ASS-AREA
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
               DISPLAY "*(ORCVTPTRSIINF)* HOSPNUM ERR [" PARA-HOSPNUM"]"
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
               DISPLAY "*(ORCVTPTRSIINF)* PTHKN-FILE OPN STS["
                                       STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＩＳＡＭ初期処理
           OPEN    OUTPUT          ISAM-FILE
           IF  STS-ISAM        NOT =   ZERO
               MOVE    1           TO  FLG-ISAM
               DISPLAY "*(ORCVTPTRSIINF)* ISAM-FILE OPN STS["
                                       STS-ISAM "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    保険番号テーブル設定
           PERFORM 1009-HKNNUM-TBL-SEC
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-9       TO  WRK-COMMIT
           MOVE    PARA-11-9       TO  WRK-ERR
      *
      *    患者保険ＣＳＶファイル　検索処理
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
               DISPLAY "*(ORCVTPTRSIINF)* PARA-FILE OPN STS["
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
               PERFORM  900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-05-1           IS  NUMERIC )   AND
                 ( PARA-09-9           IS  NUMERIC )   AND
                 ( PARA-11-9           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
           END-IF
      *
      *    労災のISAMファイルがない場合デフォルトをセットする
           IF      ASS-ISAM        =   SPACE
               MOVE    CONST-ISAM      TO  ASS-ISAM
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
               WHEN    "01-9"
                   MOVE    PARA-DATA     TO  ASS-CSV
               WHEN    "02-9"
                   MOVE    PARA-DATA     TO  ASS-ERR
               WHEN    "05-1"
                   MOVE    PARA-DATA     TO  PARA-05-1
               WHEN    "09-9"
                   MOVE    PARA-DATA     TO  PARA-09-9
               WHEN    "11-9"
                   MOVE    PARA-DATA     TO  PARA-11-9
               WHEN    "12-3"
                   MOVE    PARA-DATA     TO  ASS-ISAM
               WHEN    "HN"
                   MOVE    PARA-DATA     TO  PARA-HOSPNUM
           END-EVALUATE
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
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
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
           MOVE    SPA-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
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
      *****************************************************************
      *    保険番号テーブル設定
      *****************************************************************
       1009-HKNNUM-TBL-SEC            SECTION.
      *
           INITIALIZE                 TBL-HKNNUM-AREA
           MOVE    ZERO               TO  IDX-HKN-MAX
      *
           MOVE    SPACE               TO  HKNNUM-REC
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 950-HKNNUM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           PERFORM UNTIL      (FLG-HKNNUM  =   1 )
                           OR (IDX-HKN-MAX >=  10 )
      *       労災・自賠責保険のもの
              IF       HKN-HKNKOHSBTKBN        =   "4"
                   ADD     1                   TO  IDX-HKN-MAX
                   MOVE    HKNNUM-REC          TO  TBL-HKNNUM-REC
                                                       (IDX-HKN-MAX)
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 950-HKNNUM-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-HKN-MAX         =   ZERO
               MOVE    1                   TO  FLG-CSV
               DISPLAY "*(ORCVTPTRSIINF)* TBL-HKNNUM NASI ERR"
           END-IF
           .
       1009-HKNNUM-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    1000件ごとにメッセージを表示
           IF      CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTPTRSIINF)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット処理
           IF      CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *
           MOVE    SPACE           TO  PTRSIINF-REC
           INITIALIZE                  PTRSIINF-REC
      *
           INITIALIZE                  WRK-POINT-AREA
           INITIALIZE                  IDX-AREA
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者労災保険ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー３個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   26   )
                   OR      ( WRK-PO-ED     >=  300  )
                   OR      ( IDE           >=  3    )
      *
               INITIALIZE                  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF  ( CSV-R (WRK-PO-ST:)    =   SPACE )  OR
                   ( FLG-DATAEND           =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1             TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得
                   PERFORM 210-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 220-KOUMOKU-KYOTU-SEC
      *        項目別チェック処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 240-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-ERRARI              =   ZERO
      *        患者労災保険情報セット
               PERFORM 250-PTRSIINF-SET-SEC
               IF      FLG-ERR             NOT =   ZERO
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
      *    患者保険ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
      *    エラーが指定した件数になったら処理終了
           IF    ( WRK-ERR         NOT =   ZERO    ) AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
           END-IF
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
                   UNTIL   ( IDY           >=  300  )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( CSV-X (IDY)     =   ","  )  OR
                     ((IDX             =   26   )  AND
                      (CSV-X (IDY)     =   SPACE))
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
               IF  WRK-WK              =   WRK-PO-ED
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
               COMPUTE WRK-MO-ST       =   WRK-PO-ST   +   1
               IF  FLG-DELI2           =   1
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
      *    ２：レコード番号（保険番号）
           WHEN    2
               PERFORM 2410-RECORD-NUM-SET-SEC
      *    ３：労災保険区分
           WHEN    3
               PERFORM 2410-HKNKBN-SET-SEC
      *    ４：自賠責請求区分
           WHEN    4
               PERFORM 2410-JIBAISEIKBN-SET-SEC
      *    ５：患者交付番号
           WHEN    5
               PERFORM 2410-KOFUNUM-SET-SEC
      *    ６：災害区分
           WHEN    6
               PERFORM 2410-SAIGAIKBN-SET-SEC
      *    ７：新継再別
           WHEN    7
               PERFORM 2410-SINKEI-SET-SEC
      *    ８：転帰事由
           WHEN    8
               PERFORM 2410-TENKI-SET-SEC
      *    ９：四肢特例区分
           WHEN    9
               PERFORM 2410-SISIKBN-SET-SEC
      *    １０：傷病年月日
           WHEN    10
               PERFORM 2410-SHOBYOYMD-SET-SEC
      *    １１：療養開始年月日
           WHEN    11
               PERFORM 2410-RYOSTYMD-SET-SEC
      *    １２：療養終了年月日
           WHEN    12
               PERFORM 2410-RYOEDYMD-SET-SEC
      *    １３：事務所名称
           WHEN    13
               PERFORM 2410-JIGYOUNAME-SET-SEC
      *    １４：所在地都道府県名
           WHEN    14
               PERFORM 2410-PREFNAME-SET-SEC
      *    １５：所在地都道府県区分
           WHEN    15
               PERFORM 2410-PREFKBN-SET-SEC
      *    １６：所在地都市区名
           WHEN    16
               PERFORM 2410-CITYNAME-SET-SEC
      *    １７：所在地都市区区分
           WHEN    17
               PERFORM 2410-CITYKBN-SET-SEC
      *    １８：傷病部位（コメント）
           WHEN    18
               PERFORM 2410-COMMENT-SET-SEC
      *    １９：基準年月
           WHEN    19
               PERFORM 2410-KIJYUNYM-SET-SEC
      *    ２０：回数
           WHEN    20
               PERFORM 2410-KAISU-SET-SEC
      *    ２１：自賠責担当医コード
           WHEN    21
               PERFORM 2410-DRCD-SET-SEC
      *    ２２：自賠責保険会社名
           WHEN    22
               PERFORM 2410-HOKENNAME-SET-SEC
      *    ２３：損傷区分
           WHEN    23
               PERFORM 2410-SONSHOUKBN-SET-SEC
      *    ２４：損傷区分−枝番
           WHEN    24
               PERFORM 2410-SONSHOUKBN-EDANUM-SET-SEC
      *    ２５：登録日
           WHEN    25
               PERFORM 2410-CREYMD-SET-SEC
      *    ２６：更新日
           WHEN    26
               PERFORM 2410-UPYMD-SET-SEC
           END-EVALUATE
      *
           IF      FLG-ERR         NOT =   ZERO
               MOVE    1               TO  FLG-ERRARI
               PERFORM 270-ERRREC-EDIT-SEC
           END-IF
      *
           .
       240-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    項目１：患者番号　処理
      *****************************************************************
       2410-PTNUM-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-PTNUM
      *
      *    必須未入力
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
               MOVE    IDZ             TO  WRK-CNT
           END-PERFORM
           MOVE    WRK-CNT             TO  WRK-MO-CNT
      *
      *    項目桁数不正
           IF    ( WRK-MO-CNT          =   0  )    OR
                 ( WRK-MO-CNT          >   20 )
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-PTNUM-SET-EXT
           END-IF
      *
      *    患者番号セット
           IF   (( SYS-1009-PTNUMKSIKBN    =   "1" ) AND
                 ( SYS-1009-JIYKSIKBN      =   "2" ))    OR
                (( SYS-1009-PTNUMKSIKBN    =   "2" ) AND
                 ( SYS-1009-HJNKSIKBN      =   "3" OR "4" ))
               MOVE    ZERO            TO  SPA-PTNUM(1:WRK-RENKETA)
               COMPUTE IDZ             =   WRK-RENKETA
                                       -   WRK-MO-CNT
                                       +   1
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM(IDZ:)
           ELSE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM
           END-IF
      *
           .
       2410-PTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２：レコード番号（保険番号）　処理
      *****************************************************************
       2410-RECORD-NUM-SET-SEC     SECTION.
      *
           IF     (FLG-NODATA          =   1     )  OR
                  (WRK-MO-CNT          >   2     )
               MOVE    ZERO                TO  WRK-RENNUM
           ELSE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-RENNUM
           END-IF
           .
       2410-RECORD-NUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：労災保険区分
      *****************************************************************
       2410-HKNKBN-SET-SEC         SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
           END-IF
      *    桁数
           IF     (FLG-ERR             =   ZERO  )  AND
                  (WRK-MO-CNT      NOT =   1     )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
      *        労災保険区分
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  PTRSI-HKNKBN
      *        誤入力
               WHEN    OTHER
                   MOVE    3               TO  FLG-ERR
           END-EVALUATE
      *
           END-IF
           .
       2410-HKNKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：：自賠責請求区分　処理
      *****************************************************************
       2410-JIBAISEIKBN-SET-SEC         SECTION.
      *
      *    自賠責・公務災害のみ設定
           IF      PTRSI-HKNKBN        =   "4"
                                       OR  "5"
               CONTINUE
           ELSE
               GO      TO  2410-JIBAISEIKBN-SET-EXT
           END-IF
      *
           IF      FLG-NODATA          =   1
               MOVE    "0"                 TO  WRK-DATA
           END-IF
      *    項目桁数不正
           IF     (WRK-MO-CNT      NOT =   1    )
              AND (FLG-NODATA          =   ZERO )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )
              AND (FLG-NODATA          =   ZERO )
      *    自賠責請求区分
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "0"
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
      *            誤入力
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           IF      FLG-ERR             =   ZERO
               IF      WRK-DATA        =   "0"
                   MOVE    "1"             TO  PTRSI-JIBAISEIKBN
               ELSE
                   MOVE    WRK-DATA(1:1)   TO  PTRSI-JIBAISEIKBN
               END-IF
           END-IF
           .
       2410-JIBAISEIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目５：患者交付番号　処理
      *****************************************************************
       2410-KOFUNUM-SET-SEC       SECTION.
      *
      *    自賠責・公務災害 設定なし
           IF      PTRSI-HKNKBN        =   "4"
                                       OR  "5"
               GO      TO      2410-KOFUNUM-SET-EXT
           END-IF
      *    項目桁数不正
           IF      WRK-MO-CNT          >   14
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF    (FLG-NODATA          =   1 )   OR
                 (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        患者交付番号
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-DATA
      *        半角チェック等
               PERFORM 2401-SPACE-HENSYU-SEC
               MOVE    WRK-DATA (1:WRK-LEN)
                                       TO  PTRSI-KOFUNUM
           END-IF
      *
           .
       2410-KOFUNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目６：災害区分　処理
      *****************************************************************
       2410-SAIGAIKBN-SET-SEC      SECTION.
      *
      *    短期給付・傷病年金のみ 設定
           IF      PTRSI-HKNKBN        =   "1"
                                       OR  "2"
               CONTINUE
           ELSE
               GO      TO      2410-SAIGAIKBN-SET-EXT
           END-IF
      *
      *    未入力
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
           END-IF
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *    災害区分セット
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "1" 
               WHEN    "2"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  PTRSI-SAIGAIKBN
      *        誤入力
               WHEN    OTHER
                   MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-SAIGAIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目７：新継再別　処理
      *****************************************************************
       2410-SINKEI-SET-SEC        SECTION.
      *
      *    短期給付・傷病年金のみ 設定
           IF      PTRSI-HKNKBN        =   "1"
                                       OR  "2"
               CONTINUE
           ELSE
               GO      TO      2410-SINKEI-SET-EXT
           END-IF
      *
      *    未入力は １：初診
           IF      FLG-NODATA          =   1
               MOVE    "1"                 TO  PTRSI-SINKEI
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )  AND
                  (FLG-NODATA          =   ZERO )
      *    新継再別セット
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "1"
                   WHEN    "3"
                   WHEN    "5"
                   WHEN    "7"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  PTRSI-SINKEI
      *            未設定
                   WHEN    "0"
                       MOVE    "1"             TO  PTRSI-SINKEI
      *            誤入力
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-SINKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目８：転帰事由　処理
      *****************************************************************
       2410-TENKI-SET-SEC        SECTION.
      *
      *    アフターケア以外設定
           IF      PTRSI-HKNKBN        =   "3"
               GO      TO      2410-TENKI-SET-EXT
           END-IF
      *
      *    未入力は ３：継続
           IF      FLG-NODATA          =   1
               MOVE    "3"                 TO  PTRSI-TENKI
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )  AND
                  (FLG-NODATA          =   ZERO )
      *    転帰事由セット
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "1"
                   WHEN    "3"
                   WHEN    "5"
                   WHEN    "7"
                   WHEN    "9"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  PTRSI-TENKI
      *            未設定
                   WHEN    "0"
                       MOVE    "3"             TO  PTRSI-TENKI
      *            誤入力
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-TENKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目９：四肢特例区分　処理
      *****************************************************************
       2410-SISIKBN-SET-SEC           SECTION.
      *
      *    アフターケア以外設定
           IF      PTRSI-HKNKBN        =   "3"
               GO      TO      2410-SISIKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        未入力
               IF      FLG-NODATA          =   1
                   MOVE    "0"                 TO  PTRSI-SISIKBN
               END-IF
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )  AND
                  (FLG-NODATA          =   ZERO )
      *        四肢特例区分セット
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "0"
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  PTRSI-SISIKBN
      *            誤入力
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-SISIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目１０：傷病年月日　処理
      *****************************************************************
       2410-SHOBYOYMD-SET-SEC            SECTION.
      *
      *    アフターケア以外設定
           IF      PTRSI-HKNKBN        =   "3"
               GO      TO      2410-SHOBYOYMD-SET-EXT
           END-IF
      *
      *    必須未入力
           IF      FLG-NODATA          =   1
                MOVE    1              TO  FLG-ERR
           END-IF
      *
      *    項目桁数不正
           IF     (WRK-MO-CNT      NOT =   8    )
              AND (FLG-NODATA          =   ZERO )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        日付チェック
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO   WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      FLG-YMD             =   1
                   MOVE    3               TO  FLG-ERR
               ELSE
                   MOVE    WRK-SYMD        TO  PTRSI-SHOBYOYMD
               END-IF
           END-IF
      *
           .
       2410-SHOBYOYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目１１：療養開始年月日　処理
      *****************************************************************
       2410-RYOSTYMD-SET-SEC            SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA          =   1
                MOVE    1              TO  FLG-ERR
           END-IF
      *
      *    項目桁数不正
           IF     (WRK-MO-CNT      NOT =   8    )
              AND (FLG-NODATA          =   ZERO )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        日付チェック
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO   WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      FLG-YMD             =   1
                   MOVE    3               TO  FLG-ERR
               ELSE
                   IF      PTRSI-HKNKBN        =   "3"
      *                アフターケア
                       MOVE    WRK-SYMD        TO  PTRSI-SINSATUYMD
                   ELSE
      *                アフターケア以外
                       MOVE    WRK-SYMD        TO  PTRSI-RYOSTYMD
                   END-IF
               END-IF
           END-IF
      *
           .
       2410-RYOSTYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目１２：療養終了年月日　処理
      *****************************************************************
       2410-RYOEDYMD-SET-SEC            SECTION.
      *
      *    項目桁数不正
           IF     (FLG-NODATA          =   ZERO )  AND
                  (WRK-MO-CNT      NOT =   8    )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
               IF      (FLG-NODATA          =   1  )  OR
                       (CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                            =   "00000000"
                                            OR  "99999999" )
                   MOVE    "99999999"           TO  WRK-SYMD
               ELSE
                   MOVE    SPACE                TO  WRK-SYMD
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
               IF      WRK-SYMD            =   SPACE
      *            日付チェック
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO   WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   IF      FLG-YMD             =   1
                       MOVE    3               TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        療養終了年月日編集
               IF      PTRSI-HKNKBN        =   "3"
      *            アフターケア
                   MOVE    WRK-SYMD        TO  PTRSI-SINSATUEND
               ELSE
      *            アフターケア以外
                   MOVE    WRK-SYMD        TO  PTRSI-RYOEDYMD
               END-IF
           END-IF
      *
           .
       2410-RYOEDYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１３：事業所名称　処理
      *****************************************************************
       2410-JIGYOUNAME-SET-SEC    SECTION.
      *
      *    短期給付 設定
           IF      PTRSI-HKNKBN        =   "1"
               CONTINUE
           ELSE
               GO      TO      2410-JIGYOUNAME-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   80
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        事業所名称
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               MOVE    80                  TO  WRK-CNT
               PERFORM 2402-ZENKAKU-HEN-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-DATA (1:WRK-LEN)
                                           TO  PTRSI-JIGYOUNAME
               END-IF
           END-IF
      *
           .
       2410-JIGYOUNAME-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目１３：所在地都道府県名　処理
      *****************************************************************
       2410-PREFNAME-SET-SEC    SECTION.
      *
      *    短期給付 設定
           IF      PTRSI-HKNKBN        =   "1"
               CONTINUE
           ELSE
               GO      TO      2410-PREFNAME-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   20
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        所在地都道府県名
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               MOVE    20                  TO  WRK-CNT
               PERFORM 2402-ZENKAKU-HEN-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-DATA (1:WRK-LEN)
                                           TO  PTRSI-PREFNAME
               END-IF
           END-IF
      *
           .
       2410-PREFNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１５：所在地都道府県区分　処理
      *****************************************************************
       2410-PREFKBN-SET-SEC         SECTION.
      *
      *    短期給付 設定
           IF      PTRSI-HKNKBN        =   "1"
               CONTINUE
           ELSE
               GO      TO      2410-PREFKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        所在地都道府県区分
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "0"
                       CONTINUE
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                   WHEN    "4"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  PTRSI-PREFKBN
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-PREFKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目１６：所在地郡市区名　処理
      *****************************************************************
       2410-CITYNAME-SET-SEC    SECTION.
      *
      *    短期給付 設定
           IF      PTRSI-HKNKBN        =   "1"
               CONTINUE
           ELSE
               GO      TO      2410-CITYNAME-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   80
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        所在地郡市区名
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               MOVE    80                  TO  WRK-CNT
               PERFORM 2402-ZENKAKU-HEN-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-DATA (1:WRK-LEN)
                                           TO  PTRSI-CITYNAME
               END-IF
           END-IF
      *
           .
       2410-CITYNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１７：所在地郡市区区分　処理
      *****************************************************************
       2410-CITYKBN-SET-SEC         SECTION.
      *
      *    短期給付 設定
           IF      PTRSI-HKNKBN        =   "1"
               CONTINUE
           ELSE
               GO      TO      2410-CITYKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        所在地郡市区区分
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "0"
                       CONTINUE
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  PTRSI-CITYKBN
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           .
       2410-CITYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１８：傷病部位　処理
      *****************************************************************
       2410-COMMENT-SET-SEC         SECTION.
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   200
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        傷病部位（労災コメント）
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               MOVE    200                 TO  WRK-CNT
               PERFORM 2402-ZENKAKU-HEN-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-DATA (1:WRK-LEN)
                                           TO  PTRSI-COMMENT
               END-IF
           END-IF
      *
           .
       2410-COMMENT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１９：基準年月　処理
      *****************************************************************
       2410-KIJYUNYM-SET-SEC       SECTION.
      *
      *    短期給付・傷病年金 設定
           IF      PTRSI-HKNKBN        =   "1"
                                       OR  "2"
               CONTINUE
           ELSE
               GO      TO      2410-KIJYUNYM-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   6
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        労災レセ回数記載：基準年月
               IF     (FLG-NODATA          =   1   )  OR
                      (CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           =   ALL "0" )
                   MOVE    PTRSI-RYOSTYMD (1:6)
                                               TO  PTRSI-KIJYUNYM
               ELSE
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-SYMD(1:6)
                   MOVE    "01"                TO  WRK-SYMD(7:2)
                   PERFORM 31012-SEIWA-HEN-SEC
                   IF      FLG-YMD         =   1
                       MOVE    3               TO  FLG-ERR
                   ELSE
                       MOVE    WRK-SYMD(1:6)   TO  PTRSI-KIJYUNYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2410-KIJYUNYM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２０：回数　処理
      *****************************************************************
       2410-KAISU-SET-SEC       SECTION.
      *
      *    短期給付・傷病年金 設定
           IF      PTRSI-HKNKBN        =   "1"
                                       OR  "2"
               CONTINUE
           ELSE
               GO      TO      2410-KAISU-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   3
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        労災レセ回数記載：回数
               IF     (FLG-NODATA          =   1   )  OR
                      (CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           =   ALL "0" )
                   MOVE    1                   TO  PTRSI-KAISU
               ELSE
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-DATA
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-DATA            TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   3     )   OR
                          (SNUM-SYOSUU         >   ZERO  )
                       MOVE    3               TO  FLG-ERR
                   ELSE
                       MOVE    SNUM-OUTNUM     TO  PTRSI-KAISU
                   END-IF
               END-IF
           END-IF
      *
           .
       2410-KAISU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２１：自賠責担当医コード　処理
      *****************************************************************
       2410-DRCD-SET-SEC       SECTION.
      *
      *    自賠責・公務災害 設定
           IF      PTRSI-HKNKBN        =   "4"
                                       OR  "5"
               CONTINUE
           ELSE
               GO      TO      2410-DRCD-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF     (FLG-NODATA          =   ZERO)
              AND (WRK-MO-CNT      NOT =   5   )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        担当医コード
               IF     (FLG-NODATA          =   1   )  OR
                      (CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           =   ALL "0" )
                   MOVE    SPACE               TO  PTRSI-DRCD
               ELSE
      *        １桁目は１であること
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-DATA
                   IF      WRK-DATA(1:1)       =   "1"
                       MOVE    WRK-DATA            TO  PTRSI-DRCD
                   ELSE
                       MOVE    3                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
      *
           .
       2410-DRCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２２：自賠責保険会社名　処理
      *****************************************************************
       2410-HOKENNAME-SET-SEC         SECTION.
      *
      *    自賠責・公務災害 設定
           IF      PTRSI-HKNKBN        =   "4"
                                       OR  "5"
               CONTINUE
           ELSE
               GO      TO      2410-HOKENNAME-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT          >   80
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-NODATA          =   1  )  OR
                  (FLG-ERR         NOT =   ZERO )
               CONTINUE
           ELSE
      *        自賠責保険会社名
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               MOVE    80                  TO  WRK-CNT
               PERFORM 2402-ZENKAKU-HEN-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-DATA (1:WRK-LEN)
                                           TO  PTRSI-HOKENNAME
               END-IF
           END-IF
      *
           .
       2410-HOKENNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２３：損傷区分　処理
      *****************************************************************
       2410-SONSHOUKBN-SET-SEC       SECTION.
      *
      *    アフターケア 設定
           IF      PTRSI-HKNKBN        =   "3"
               CONTINUE
           ELSE
               GO      TO      2410-SONSHOUKBN-SET-EXT
           END-IF
      *
      *    未入力
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
           END-IF
      *    項目桁数不正
           IF      WRK-MO-CNT          >   2
               MOVE    2               TO  FLG-ERR
           END-IF
      *
      *
           IF      FLG-ERR             =   ZERO 
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-DATA
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-DATA            TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   2     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    2                   TO  FLG-ERR
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-SONSHOUKBN
                   IF      WRK-SONSHOUKBN      >=  00   AND
                                               <=  17
                       MOVE    WRK-SONSHOUKBN      TO  PTRSI-SONSHOUKBN
                   ELSE
                       MOVE    3                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
           .
       2410-SONSHOUKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２３：損傷区分　処理
      *****************************************************************
       2410-SONSHOUKBN-EDANUM-SET-SEC       SECTION.
      *
      *    アフターケア で 損傷区分が”０２”
           IF     (PTRSI-HKNKBN        =   "3"  )  AND
                  (PTRSI-SONSHOUKBN    =   "02" )
               CONTINUE
           ELSE
               GO      TO      2410-SONSHOUKBN-EDANUM-SET-EXT
           END-IF
      *
      *    未入力
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
           END-IF
      *    項目桁数不正
           IF      WRK-MO-CNT          >   1
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   ZERO
               EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                   WHEN    "4"
                   WHEN    "5"
                   WHEN    "6"
                       MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  PTRSI-SONSHOUKBN-EDANUM
                   WHEN    OTHER
                       MOVE    3               TO  FLG-ERR
               END-EVALUATE
           END-IF
           .
       2410-SONSHOUKBN-EDANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２５：登録日　処理
      *****************************************************************
       2410-CREYMD-SET-SEC         SECTION.
      *
      *    項目桁数不正
           IF     (WRK-MO-CNT      NOT =   8    )
              AND (FLG-NODATA          =   ZERO )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )  AND
                  (FLG-NODATA          =   1   )
              CONTINUE
           ELSE
      *    登録日セット
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO   WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      FLG-YMD         =   1
                   MOVE    3               TO  FLG-ERR
               ELSE
                   MOVE    WRK-SYMD        TO  PTRSI-CREYMD
               END-IF
           END-IF
      *
           .
       2410-CREYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２６：更新日　処理
      *****************************************************************
       2410-UPYMD-SET-SEC          SECTION.
      *
      *    項目桁数不正
           IF     (WRK-MO-CNT      NOT =   8    )
              AND (FLG-NODATA          =   ZERO )
               MOVE    2               TO  FLG-ERR
           END-IF
      *
           IF     (FLG-ERR             =   ZERO )  AND
                  (FLG-NODATA          =   1   )
              CONTINUE
           ELSE
      *
      *        更新日セット
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO   WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      FLG-YMD         =   1
                   MOVE    3               TO  FLG-ERR
               ELSE
                   MOVE    WRK-SYMD        TO  PTRSI-UPYMD
               END-IF
           END-IF
      *
           .
       2410-UPYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険マスタ　セット処理
      *****************************************************************
       250-PTRSIINF-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-PTID
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM           =   1
               MOVE    4               TO  FLG-ERR
               GO  TO  250-PTRSIINF-SET-EXT
           END-IF
      *
           MOVE    PTNUM-PTID          TO  WRK-PTID
      *    保険ＩＤ
           MOVE    PTNUM-HKNID         TO  WRK-HKNID
           ADD     1                   TO  WRK-HKNID
      *
      *    労災患者情報設定
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    PTNUM-PTID          TO  PTRSI-PTID
           MOVE    WRK-HKNID           TO  PTRSI-HKNID
           MOVE    SPA-TERMID          TO  PTRSI-TERMID
      *    保険番号設定
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "5"
                   MOVE    "971"           TO  PTRSI-HKNNUM
               WHEN    "4"
                   MOVE    "973"           TO  PTRSI-HKNNUM
           END-EVALUATE
      *
      *    患者保険情報設定
           MOVE    SPACE               TO  PTHKNINF-REC
           INITIALIZE                      PTHKNINF-REC
           MOVE    PTRSI-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    PTRSI-PTID          TO  PTHKN-PTID
           MOVE    PTRSI-HKNNUM        TO  PTHKN-HKNNUM
      *    保険ＩＤ
           MOVE    WRK-HKNID           TO  PTHKN-HKNID
      *    適用期間
           IF      PTRSI-HKNKBN        =   "3" 
      *        アフター療養開始年月日
               MOVE    PTRSI-SINSATUYMD    TO  PTHKN-TEKSTYMD
               MOVE    PTRSI-SINSATUEND    TO  PTHKN-TEKEDYMD
           ELSE
      *        以外の時、療養開始年月日
               MOVE    PTRSI-RYOSTYMD      TO  PTHKN-TEKSTYMD
               MOVE    PTRSI-RYOEDYMD      TO  PTHKN-TEKEDYMD
           END-IF
      *    番号へ傷病名の編集
           MOVE    PTRSI-COMMENT       TO  PTHKN-NUM
      *    保険番号マスタの存在チェック
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDH      FROM    1   BY  1
                   UNTIL     (IDH      >   IDX-HKN-MAX  )
                        OR   (FLG-OK       =   1        )
                        OR   (FLG-ERR  NOT =   ZERO     )
               IF     (PTRSI-HKNNUM        =   TBL-HKN-HKNNUM (IDH))
                   IF     (PTHKN-TEKSTYMD      >=  TBL-HKN-TEKSTYMD
                                                             (IDH)) AND
                          (PTHKN-TEKEDYMD      <=  TBL-HKN-TEKEDYMD
                                                             (IDH))
      *                短縮制度名を記号へ
                       MOVE    TBL-HKN-TANSEIDONAME (IDH)
                                                TO  PTHKN-KIGO
                       MOVE    1                TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    5               TO  FLG-ERR
           END-IF
      *    公務災害はＨ１８．４から
           IF      PTRSI-HKNKBN        =   "5"
               IF      PTHKN-TEKSTYMD      <   "20060401"
                   MOVE    9               TO  FLG-ERR
               END-IF
           END-IF
      *
      *    患者番号変換マスタ更新処理
           IF      FLG-ERR             =   ZERO
               PERFORM 2501-PTNUM-UPDATE-SEC
           END-IF
      *    患者労災保険マスタ　登録処理
           IF      FLG-ERR             =   ZERO
               PERFORM 2502-PTRSIINF-INSERT-SEC
           END-IF
      *    患者保険マスタ　登録処理
           IF      FLG-ERR             =   ZERO
               PERFORM 2503-PTHKNINF-INSERT-SEC
           END-IF
      *    ＩＳＡＭファイル出力
           IF      FLG-ERR             =   ZERO
               INITIALIZE                  ISAM-REC
               MOVE    WRK-PTID        TO  ISAM-PTID
               MOVE    WRK-RENNUM      TO  ISAM-RENNUM
               MOVE    WRK-HKNID       TO  ISAM-HKNID
               WRITE   ISAM-REC
           END-IF
      *
           .
       250-PTRSIINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       2501-PTNUM-UPDATE-SEC        SECTION.
      *
      *    最大保険ＩＤで患者番号変換マスタを更新
           IF      PTNUM-HKNID         <   PTHKN-HKNID
               MOVE    PTHKN-HKNID     TO  PTNUM-HKNID
               MOVE    SPA-SYSYMD      TO  PTNUM-UPYMD
      *
               MOVE    PTNUM-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    4               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       2501-PTNUM-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険マスタ　登録処理
      *****************************************************************
       2502-PTRSIINF-INSERT-SEC     SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    6               TO  FLG-ERR
           ELSE
               ADD     1               TO  CNT-PTRSIINF
           END-IF
      *
           .
       2502-PTRSIINF-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ　登録処理
      *****************************************************************
       2503-PTHKNINF-INSERT-SEC     SECTION.
      *
           MOVE    SPA-TERMID          TO  PTHKN-TERMID
           MOVE    SPA-SYSYMD          TO  PTHKN-CREYMD
      *
           MOVE    PTHKNINF-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"         TO  MCP-FUNC
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING    MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC         NOT =   ZERO
               MOVE    7              TO  FLG-ERR
           ELSE
               ADD     1              TO  CNT-PTHKNINF
           END-IF
      *
           .
       2503-PTHKNINF-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       270-ERRREC-EDIT-SEC         SECTION.
      *
           EVALUATE    FLG-ERR
      *    必須項目未入力エラー
           WHEN    1
      *    項目桁数エラー
           WHEN    2
      *    誤入力エラー
           WHEN    3
               PERFORM 2910-ERRREC-EDIT-BETU1-SEC
      *    患者番号取得エラー
           WHEN    4
      *    保険番号取得エラー
           WHEN    5
      *    患者番号更新エラー
           WHEN    6
      *    患者労災保険出力エラー
           WHEN    7
      *    患者保険出力エラー
           WHEN    8
      *    公務災害開始日エラー
           WHEN    9
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
           ADD     1                   TO  IDE
           IF      IDE                 >   3
               GO  TO  2910-ERRREC-EDIT-BETU1-EXT
           END-IF
      *
           IF      IDE                 =   1
               MOVE    SPACE               TO  ERR-REC1
               MOVE    CNT-CSV             TO  ERR1-LINENO
               MOVE    "ERR"               TO  ERR1-NAIYO
               IF      WRK-KOUMOKU-NO  NOT =   1
      *            患者番号以外のエラー
                   MOVE    "患者番号:"         TO  ERR1-PTNUMMES
                   MOVE    SPA-PTNUM           TO  ERR1-PTNUM
               END-IF
           END-IF
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
      *    ２：レコード番号（保険番号）
           WHEN    2
               MOVE    "レコ番  :"     TO  ERR1-KOMOKU(IDE)
      *    ３：労災保険区分
           WHEN    3
               MOVE    "労災保険:"     TO  ERR1-KOMOKU(IDE)
      *    ４：自賠責患者請求区分
           WHEN    4
               MOVE    "請求区分:"     TO  ERR1-KOMOKU(IDE)
      *    ５：患者交付番号
           WHEN    5
               MOVE    "交付番号:"     TO  ERR1-KOMOKU(IDE)
      *    ６：災害区分
           WHEN    6
               MOVE    "災害区分:"     TO  ERR1-KOMOKU(IDE)
      *    ７：新継再別
           WHEN    7
               MOVE    "新継再別:"     TO  ERR1-KOMOKU(IDE)
      *    ８：転帰事由
           WHEN    8
               MOVE    "転帰事由:"     TO  ERR1-KOMOKU(IDE)
      *    ９：四肢特例区分
           WHEN    9
               MOVE    "四肢区分:"     TO  ERR1-KOMOKU(IDE)
      *    １０：傷病年月日
           WHEN    10
               MOVE    "傷病日付:"     TO  ERR1-KOMOKU(IDE)
      *    １１：療養開始年月日
           WHEN    11
               MOVE    "療養開始:"     TO  ERR1-KOMOKU(IDE)
      *    １２：療養終了年月日
           WHEN    12
               MOVE    "療養終了:"     TO  ERR1-KOMOKU(IDE)
      *    １３：事業所名称
           WHEN    13
               MOVE    "事業所名:"     TO  ERR1-KOMOKU(IDE)
      *    １４：所在地都道府県名
           WHEN    14
               MOVE    "県名    :"     TO  ERR1-KOMOKU(IDE)
      *    １５：所在地都道府県区分
           WHEN    15
               MOVE    "県区分  :"     TO  ERR1-KOMOKU(IDE)
      *    １６：所在地都市区名
           WHEN    16
               MOVE    "都市区名:"     TO  ERR1-KOMOKU(IDE)
      *    １7：所在地都市区区分
           WHEN    17
               MOVE    "都市区分:"     TO  ERR1-KOMOKU(IDE)
      *    １８：傷病部位
           WHEN    18
               MOVE    "傷病部位:"     TO  ERR1-KOMOKU(IDE)
      *    １９：基準年月
           WHEN    19
               MOVE    "基準年月:"     TO  ERR1-KOMOKU(IDE)
      *    ２０：回数
           WHEN    20
               MOVE    "回数    :"     TO  ERR1-KOMOKU(IDE)
      *    ２１：担当医コード
           WHEN    21
               MOVE    "担当医  :"     TO  ERR1-KOMOKU(IDE)
      *    ２２：自賠責保険会社名
           WHEN    22
               MOVE    "保険会社:"     TO  ERR1-KOMOKU(IDE)
      *    ２３：傷病区分
           WHEN    23
               MOVE    "傷病区分:"     TO  ERR1-KOMOKU(IDE)
      *    ２４：傷病区分−枝番
           WHEN    24
               MOVE    "傷病枝番:"     TO  ERR1-KOMOKU(IDE)
      *    ２５：登録日
           WHEN    25
               MOVE    "登録日  :"     TO  ERR1-KOMOKU(IDE)
      *    ２６：更新日
           WHEN    26
               MOVE    "更新日  :"     TO  ERR1-KOMOKU(IDE)
      *    フォーマットエラー
           WHEN    OTHER
               MOVE    "過多項目:"     TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF      WRK-MO-CNT      NOT =   ZERO
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
           MOVE    SPACE               TO  ERR-REC2
           MOVE    CNT-CSV             TO  ERR2-LINENO
           MOVE    "["                 TO  ERR2-ERRNO (1:1)
           MOVE    FLG-ERR(1:1)        TO  ERR2-ERRNO (2:1)
           MOVE    "]"                 TO  ERR2-ERRNO (3:1)
           MOVE    "患者番号:"         TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"         TO  ERR2-PTIDMES
           MOVE    "保険番号:"         TO  ERR2-HKNNUMMES
           MOVE    "連番:"             TO  ERR2-HKNIDMES
           MOVE    "期間:"             TO  ERR2-TEKSTEDMES
      *
           MOVE    SPA-PTNUM           TO  ERR2-PTNUM
           MOVE    WRK-PTID            TO  ERR2-PTID
           MOVE    WRK-RENNUM          TO  ERR2-RENNUM
      *
           IF      PTRSI-HKNNUM    NOT =   SPACE
               MOVE    PTRSI-HKNNUM    TO  ERR2-HKNNUM
           END-IF
      *
      *    適用期間
           IF      PTRSI-HKNKBN        =   "3" 
               MOVE    "-"                 TO  ERR2-TEKBAR
               MOVE    PTRSI-SINSATUYMD    TO  ERR2-TEKSTYMD
               MOVE    PTRSI-SINSATUEND    TO  ERR2-TEKEDYMD
           ELSE
      *        以外の時、療養開始年月日
               MOVE    "-"                 TO  ERR2-TEKBAR
               MOVE    PTRSI-RYOSTYMD      TO  ERR2-TEKSTYMD
               MOVE    PTRSI-RYOEDYMD      TO  ERR2-TEKEDYMD
           END-IF
      *
           MOVE    "ERR"               TO  ERR2-NAIYO
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
           ADD     1                   TO  CNT-ERR
           IF      FLG-ERRARI          =   1
               MOVE    ERR-REC1            TO  ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2            TO  ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       270-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF      STS-LST         NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTRSIINF)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L4   TO      ERR-REC
           WRITE   ERR-REC
      *
           .
       2710-ERRLST-INIT-EXT.
           EXIT.
      *****************************************************************
      *    項目空白処理（半角項目）
      *****************************************************************
       2401-SPACE-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-LEN
      *
           IF      WRK-DATA            =   SPACE
               MOVE    1               TO  WRK-LEN
           ELSE
               PERFORM VARYING IDS     FROM    200 BY  -1
                       UNTIL   IDS     <       1
                   IF      WRK-DATA (IDS:1)    NOT =   SPACE
                       MOVE    IDS             TO  WRK-LEN
                       MOVE    1               TO  IDS
                   END-IF
               END-PERFORM
      *        半角英数字であること
               IF      WRK-LEN             >   1
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI
                   MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
                   MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF     (KANACHK-RC          =   ZERO  )  AND
                          (KANACHK-SYUBETU     =   1     )  AND
                          (KANACHK-MAE-INPUT   =   KANACHK-OUT-INPUT)
                       CONTINUE
                   ELSE
      *                全角、半角カナはエラー
                       MOVE    3               TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
           .
       2401-SPACE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理
      *****************************************************************
       2402-ZENKAKU-HEN-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
           MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-DATA
           MOVE    KANACHK-MAX         TO  WRK-LEN
           IF      KANACHK-RC      NOT =   ZERO
      *        全角・半角混在エラ−
               MOVE    3               TO  FLG-ERR
           ELSE
               IF      KANACHK-SYUBETU =   1
      *        半角を全角へ
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
                   MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-MAX         TO  WRK-LEN
                       MOVE    KANACHK-OUT-INPUT   TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-DATA
               END-IF
           END-IF
           IF      WRK-LEN         =   ZERO
               MOVE    1               TO  WRK-LEN
           END-IF
      *    後ろの全角空白を削除
           IF      WRK-LEN         >   1
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
           MOVE    ZERO                TO  WRK-LEN2
           COMPUTE IDZ                 =   WRK-LEN     +   1
           PERFORM VARYING IDS     FROM    IDZ     BY  -2
                   UNTIL   IDS     <   1
               IF      WRK-DATA (IDS:2)        =   "　"
                                               OR  SPACE
                   CONTINUE
               ELSE
                   COMPUTE WRK-LEN2        =   IDS   +  1
                   MOVE    2               TO  IDS
               END-IF
           END-PERFORM
           IF      WRK-LEN2            =   ZERO
      *        すべて全角空白
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-LEN
           ELSE
               MOVE    WRK-LEN2            TO  WRK-LEN
           END-IF
      *
           .
       2402-ZENKAKU-HEN-EXT.
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
      *    帳票出力　終了処理
           CLOSE                       CSV-FILE
           CLOSE                       ISAM-FILE
           CLOSE                       ERR-FILE
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTPTRSIINF)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTPTRSIINF)* PTRSIINF/O CNT[" CNT-PTRSIINF "]"
           DISPLAY "*(ORCVTPTRSIINF)* PTHKNINF/O CNT[" CNT-PTHKNINF "]"
           DISPLAY "*(ORCVTPTRSIINF)* ERR     /O CNT[" CNT-ERR      "]"
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
           IF          MCP-RC          =   ZERO
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
      *    患者保険マスタ読込
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    PTHKNINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       900-PTHKNINF-FETCH-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF          MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       950-HKNNUM-NEXT-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       950-HKNNUM-NEXT-EXT.
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
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険ＣＳＶファイル　検索処理
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
       900-DBCLOSE-EXT.
           EXIT.
      *
