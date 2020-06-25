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
       PROGRAM-ID.                 ORCVTPTBYOMEI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理（患者病名）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/27    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  01.00.01    NACL-森脇     05/07/13  エラーメッセージ、コミット
      *  01.00.02    NACL-森脇     05/09/28  構造変更
      *  01.00.03    NACL-森脇     06/07/26  MONFUNC対応
      *  03.05.00    NACL-多々納   07/05/16  グループ診療対応
      *
      *  04.06.01    NACL-藤原     09/06/20  病名の入力順対応
      *  04.06.02    NACL-森脇     11/01/24  補足コメント追加
      *
      *  04.07.01    NACL-藤原     15/07/29  端末ＩＤのセット
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
      *    患者病名ＣＳＶファイル
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
      *    患者病名ＣＳＶファイル
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
       WORKING-STORAGE             SECTION.
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
           03  FLG-COMB                PIC 9(01).
           03  FLG-PTBYOMEI            PIC 9(01).
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
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-YMD                 PIC 9(01).
           03  FLG-TENKI               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTBYOMEI            PIC 9(06).
      *
           03  CNT-BYOMEICD            PIC 9(01).
           03  CNT-RENNUM              PIC 9(02).
           03  CNT-HKNCOMB             PIC 9(01).
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
           03  PARA-00-7               PIC X(01).
           03  PARA-05-1               PIC X(02).
           03  PARA-08-G.
               05  PARA-08-KENSU       PIC 9(02).
               05  PARA-08             OCCURS  9.
                   07  PARA-08-9       PIC 9(01).
                   07  FILLER          PIC X(01).
                   07  PARA-08-X       PIC X(01).
           03  PARA-09-7               PIC X(06).
           03  PARA-11-7               PIC X(06).
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
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-RENNUM              PIC 9(02).
      *
      *    保険組合せ用テーブル
           03  WRK-HEN-AREA.
               05  WRK-HEN-PTNUM       PIC X(20).
               05  WRK-HEN-HKNCOMBI    PIC 9(10)   OCCURS  4.
      *
      *    エラー帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTPTBYOMEI.CBL】".
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
                   " [3]誤入力            [4]病名適応期間重複".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]病名カナ変換失敗  [6]病名ＣＤ変換失敗 ".
                   07  FILLER          PIC X(41) VALUE
                   " [7]患者番号取得失敗  [8]患者病名出力失敗".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
               05  MES-TITLE2-L4.
                   07  FILLER          PIC X(10) VALUE "       MSG".
                   07  FILLER          PIC X(20) VALUE
                   " [A]補足コメント警告".
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
      *    エラー番号４・５・６で使用
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
               05  ERR2-BYOMEIMES      PIC X(09).
               05  ERR2-BYOMEI         PIC X(30).
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
      *    システム管理（診療科）
           COPY    "CPSK1005.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *
      *    保険組合せ
       01  COMB-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    作業用患者病名
       01  WKPTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"    REPLACING
                                       //PTBYO-// BY //WKPTBYO-//.
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
      *    COPY    "CPORCMCP.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
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
           INITIALIZE                  ASS-AREA
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
               MOVE    PARA-HOSPNUM        TO  SPA-HOSPNUM
           ELSE
               MOVE    1                   TO  FLG-CSV
               DISPLAY "*(ORCVTPTBYOMEI)* HOSPNUM ERR ["
                                                 PARA-HOSPNUM"]"
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
      *    患者番号情報
           PERFORM 120-CP1009-KENSAKU-SEC
           IF      FLG-CSV         =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＣＳＶ入力ファイル　初期処理
           OPEN    INPUT   CSV-FILE
           IF      STS-CSV     NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTBYOMEI)* CSV-FILE OPN STS["
                                          STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＩＳＡＭ初期処理
           OPEN    INPUT           ISAM-FILE
           IF  STS-ISAM        NOT =   ZERO
               MOVE    1           TO  FLG-ISAM
               MOVE    1           TO  FLG-CHK
               DISPLAY "*(ORCVTPTBYOMEI)* ISAM-FILE OPN STS["
                                       STS-ISAM "]"
      *         労災ファイルがなくても処理続ける
      *         GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF      FLG-CSV         =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-7       TO  WRK-COMMIT
           MOVE    PARA-11-7       TO  WRK-ERR
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
      *    パラメタファイルOPEN
           OPEN    INPUT   PARA-FILE
           IF      STS-PARA    NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTPTBYOMEI)* PARA-FILE OPN STS["
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
           IF    ( PARA-05-1       IS  NUMERIC )   AND
                 ( PARA-09-7       IS  NUMERIC )   AND
                 ( PARA-11-7       IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
               GO  TO  101-PARA-GET-EXT
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
               WHEN  "00-7"
                   MOVE  PARA-DATA TO  PARA-00-7
               WHEN  "01-7"
                   MOVE  PARA-DATA TO  ASS-CSV
               WHEN  "02-7"
                   MOVE  PARA-DATA TO  ASS-ERR
               WHEN  "05-1"
                   MOVE  PARA-DATA TO  PARA-05-1
               WHEN  "08-0"
                   MOVE  PARA-DATA TO  PARA-08-KENSU
               WHEN  "08-1"
                   MOVE  PARA-DATA TO  PARA-08(1)
               WHEN  "08-2"
                   MOVE  PARA-DATA TO  PARA-08(2)
               WHEN  "08-3"
                   MOVE  PARA-DATA TO  PARA-08(3)
               WHEN  "08-4"
                   MOVE  PARA-DATA TO  PARA-08(4)
               WHEN  "08-5"
                   MOVE  PARA-DATA TO  PARA-08(5)
               WHEN  "08-6"
                   MOVE  PARA-DATA TO  PARA-08(6)
               WHEN  "08-7"
                   MOVE  PARA-DATA TO  PARA-08(7)
               WHEN  "08-8"
                   MOVE  PARA-DATA TO  PARA-08(8)
               WHEN  "08-9"
                   MOVE  PARA-DATA TO  PARA-08(9)
               WHEN  "09-7"
                   MOVE  PARA-DATA TO  PARA-09-7
               WHEN  "11-7"
                   MOVE  PARA-DATA TO  PARA-11-7
               WHEN  "12-3"
                   MOVE  PARA-DATA TO  ASS-ISAM
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
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
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
      *    システム管理情報　検索処理 (患者番号構成管理情報)
      *****************************************************************
       120-CP1009-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
      *        システム管理情報読み込み
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           IF      FLG-SYSKANRI    =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
      *        拡張構成区分
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1009-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1005-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    WKPTBYO-SRYKA   TO  SYS-1005-KBNCD
      *     INITIALIZE                  ORCSADDSIGNAREA
      *     MOVE    WKPTBYO-SRYKA   TO  SADDSIGN-STR
      *     CALL    "ORCSADDSIGN"       USING
      *                                 ORCSADDSIGNAREA
      *     MOVE    SADDSIGN-STR    TO  SYS-1005-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
      *        システム管理情報読み込み
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       120-CP1005-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      CNT-CSV (4:3)   =   ZERO
               DISPLAY "***(ORCVTPTBYOMEI)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット処理を行う
           IF      CNT-COMMIT      =   WRK-COMMIT
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
           MOVE    SPACE           TO  WKPTBYOMEI-REC
           INITIALIZE                  WKPTBYOMEI-REC
           MOVE    SPACE           TO  WRK-HEN-AREA
           INITIALIZE                  WRK-HEN-AREA
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDX-AREA
      *    対象患者にエラーが存在するかどうか
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  CNT-HKNCOMB
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
           MOVE    ZERO            TO  WRK-RENNUM
      *
      *    患者病名ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー４個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   35   )
                   OR      ( WRK-PO-ED     >=  300  )
                   OR      ( IDE           >   4    )
      *
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
               MOVE    ZERO            TO  FLG-AREA1
      *
               IF    ( CSV-R (WRK-PO-ST:)  =   SPACE ) OR
                     ( FLG-DATAEND         =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1             TO  WRK-KOUMOKU-NO
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
      *        患者病名マスタ　更新処理
               PERFORM 260-PTBYOMEI-INSERT-SEC
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
      *    患者病名ＣＳＶファイル　検索処理
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
                   UNTIL   ( IDY           >=  300  )
                   OR      ( FLG-COMMA   =   1    )
      *
      *        カンマの位置
               IF    ( CSV-X (IDY)         =   "," ) OR
                     ((IDX                 =   35 )  AND
                     ( CSV-X (IDY)         =   SPACE))
                   MOVE    IDY                 TO  WRK-PO-ED
                   MOVE    1                   TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDY) NOT =   SPACE
                       MOVE    IDY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *
      *        デリミタ(")存在するかどうか
               IF      CSV-X (IDY)             =   """"
                   IF      FLG-DELI1           =   ZERO
                       MOVE    1               TO  FLG-DELI1
                   ELSE
                       MOVE    1               TO  FLG-DELI2
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
                   MOVE    1               TO  FLG-NODATA
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
      *    １：患者番号
           WHEN    1
               PERFORM 2410-PTNUM-SET-SEC
      *    ２：診療科
           WHEN    2
               PERFORM 2410-SRYKA-SET-SEC
      *    ３：診療開始日
           WHEN    3
               PERFORM 2410-SRYYMD-SET-SEC
      *    ４：病名
           WHEN    4
               PERFORM 2410-BYOMEI-SET-SEC
      *    １３：疑いフラグ
           WHEN    13
               PERFORM 2410-UTAGAIFLG-SET-SEC
      *    １４：主病名フラグ
           WHEN    14
               PERFORM 2410-SYUBYOFLG-SET-SEC
      *    １６：入外区分
           WHEN    16
               PERFORM 2410-NYUGAIKBN-SET-SEC
      *    １７〜２０：保険限定レコード番号
           WHEN    17
           WHEN    18
           WHEN    19
           WHEN    20
               PERFORM 2410-HKNCOMBI-SET-SEC
      *    ２１：レセプト表示月数
           WHEN    21
               PERFORM 2410-REZEMM-SET-SEC
      *    ２２：転帰事由区分
           WHEN    22
               PERFORM 2410-TENKIKBN-SET-SEC
      *    ２４：転帰日
           WHEN    24
               PERFORM 2410-TENKIYMD-SET-SEC
      *    ３４：補足コメント
           WHEN    34
               PERFORM 2410-HOSOKU-SET-SEC
      *    ３５：労災・自賠責保険レコード番号
           WHEN    35
               PERFORM 2410-RENNUM-SET-SEC
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
      *    全角桁数・空白処理
      *****************************************************************
       2402-ZENKAKU-HEN-SEC        SECTION.
      *
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
               END-IF
           END-IF
           IF      WRK-LEN             =   ZERO
               MOVE    1                   TO  WRK-LEN
           END-IF
      *    後ろの全角空白を削除
           IF      WRK-LEN             >   1
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
           MOVE    ZERO            TO  WRK-CNT
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( IDZ     >   WRK-MO-CNT )
                   OR    ( WRK-HEN-PTNUM (IDZ:1)   =   SPACE )
               MOVE    IDZ             TO    WRK-CNT
           END-PERFORM
           MOVE    WRK-CNT         TO  WRK-MO-CNT
      *
      *    桁チェック
           IF    ( WRK-MO-CNT      =   0  )  OR
                 ( WRK-MO-CNT      >   20 )
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
               MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
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
      *    項目２：診療科　処理
      *****************************************************************
       2410-SRYKA-SET-SEC          SECTION.
      *
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
      *
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT) NOT NUMERIC
               MOVE    3               TO  FLG-ERR
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      =   1
                MOVE    "0"        TO  WKPTBYO-SRYKA(1:1)
                MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WKPTBYO-SRYKA(2:1)
           ELSE
                MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WKPTBYO-SRYKA
           END-IF
      *
      *    診療科チェック
           PERFORM 120-CP1005-KENSAKU-SEC
           IF      FLG-SYSKANRI    =   1
               MOVE    3               TO  FLG-ERR
           END-IF
      *
           .
       2410-SRYKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：診療開始日　処理
      *****************************************************************
       2410-SRYYMD-SET-SEC         SECTION.
      *
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-SRYYMD-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT  NOT =   8
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-SRYYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
      *    日付チェック
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  WKPTBYO-SRYYMD
           END-IF
      *
           .
       2410-SRYYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：病名　処理
      *****************************************************************
       2410-BYOMEI-SET-SEC         SECTION.
      *
           IF      FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-BYOMEI-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   100
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-BYOMEI-SET-EXT
           END-IF
      *
      *    病名セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  WKPTBYO-BYOMEI
           END-IF
      *
           .
       2410-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１３：疑いフラグ　処理
      *****************************************************************
       2410-UTAGAIFLG-SET-SEC      SECTION.
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-UTAGAIFLG-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-UTAGAIFLG-SET-EXT
           END-IF
      *
      *    疑いフラグセット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
           WHEN    "0"
               CONTINUE
           WHEN    "1"
               MOVE    "1"             TO  WKPTBYO-UTAGAIFLG
           WHEN    "2"
               MOVE    "2"             TO  WKPTBYO-UTAGAIFLG
           WHEN    OTHER
               MOVE    3               TO  FLG-ERR
           END-EVALUATE
      *
           .
       2410-UTAGAIFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１４：主病名フラグ　処理
      *****************************************************************
       2410-SYUBYOFLG-SET-SEC      SECTION.
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-SYUBYOFLG-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-SYUBYOFLG-SET-EXT
           END-IF
      *
      *    主病名フラグセット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
           WHEN    "0"
               CONTINUE
           WHEN    "1"
               MOVE    "1"             TO  WKPTBYO-SYUBYOFLG
           WHEN    OTHER
               MOVE    3               TO  FLG-ERR
           END-EVALUATE
      *
           .
       2410-SYUBYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１６：入外区分　処理
      *****************************************************************
       2410-NYUGAIKBN-SET-SEC      SECTION.
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-NYUGAIKBN-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-NYUGAIKBN-SET-EXT
           END-IF
      *
      *    入外区分チェック
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
           WHEN    "0"
               CONTINUE
           WHEN    "1"
           WHEN    "2"
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WKPTBYO-NYUGAIKBN
           WHEN    OTHER
               MOVE    3               TO  FLG-ERR
           END-EVALUATE
      *
           .
       2410-NYUGAIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１７〜２０：保険限定レコード番号　処理
      *****************************************************************
       2410-HKNCOMBI-SET-SEC       SECTION.
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-HKNCOMBI-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-HKNCOMBI-SET-EXT
           END-IF
      *
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  =   ZERO
              GO  TO  2410-HKNCOMBI-SET-EXT
           END-IF
      *
      *    保険組み合わせレコード番号セット
           COMPUTE IDZ = 11 - WRK-MO-CNT
           EVALUATE    WRK-KOUMOKU-NO
           WHEN    17
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                       TO  WRK-HEN-HKNCOMBI(1)(IDZ:WRK-MO-CNT)
           WHEN    18
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                       TO  WRK-HEN-HKNCOMBI(2)(IDZ:WRK-MO-CNT)
           WHEN    19
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                       TO  WRK-HEN-HKNCOMBI(3)(IDZ:WRK-MO-CNT)
           WHEN    20
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                       TO  WRK-HEN-HKNCOMBI(4)(IDZ:WRK-MO-CNT)
           END-EVALUATE
      *
           ADD     1                   TO  CNT-HKNCOMB
      *
           .
       2410-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２１：レセプト表示月数　処理
      *****************************************************************
       2410-REZEMM-SET-SEC         SECTION.
      *
      *    桁チェック
           IF      FLG-NODATA      =   1
               GO  TO  2410-REZEMM-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >  2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-REZEMM-SET-EXT
           END-IF
      *
      *    空白チェック
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT) = SPACE
               GO  TO  2410-REZEMM-SET-EXT
           END-IF
      *
      *    レセプト表示月数セット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
           WHEN    "99"
               MOVE    00              TO  WKPTBYO-REZEMM
           WHEN    OTHER
               COMPUTE IDZ = 3 - WRK-MO-CNT
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                       TO  WKPTBYO-REZEMM(IDZ:WRK-MO-CNT)
           END-EVALUATE
      *
           .
       2410-REZEMM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２２：転帰区分　処理
      *****************************************************************
       2410-TENKIKBN-SET-SEC       SECTION.
      *
      *    桁チェック
           IF      WRK-MO-CNT      NOT =   1
               GO  TO  2410-TENKIKBN-SET-EXT
           END-IF
      *
      *    空白チェック
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT) = SPACE
               GO  TO  2410-TENKIKBN-SET-EXT
           END-IF
      *
      *    転帰区分チェック(パラメタに転帰区分がある場合移行する)
           MOVE    ZERO          TO  FLG-TENKI
           PERFORM VARYING IDZ   FROM      1   BY        1
                   UNTIL  (IDZ        >  PARA-08-KENSU)
                     OR   (FLG-TENKI  =  1)
               IF  CSV-REC(WRK-MO-ST:WRK-MO-CNT)   =   PARA-08-9(IDZ)
                   IF      PARA-08-X(IDZ)      NOT =   SPACE
                       MOVE    PARA-08-X(IDZ)  TO  WKPTBYO-TENKIKBN
                       MOVE    1               TO  FLG-TENKI
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-TENKI    =   ZERO
               MOVE    3               TO  FLG-ERR
           END-IF
      *
           .
       2410-TENKIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２４：転帰日　処理
      *****************************************************************
       2410-TENKIYMD-SET-SEC       SECTION.
      *
      *    桁チェック
           IF      WRK-MO-CNT      NOT =   8
               GO  TO  2410-TENKIYMD-SET-EXT
           END-IF
      *
      *    空白チェック
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  =   SPACE
               GO  TO  2410-TENKIYMD-SET-EXT
           END-IF
      *
      *    日付チェック
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
      *        転帰日チェック(転帰日が診療日より大きいか)
               IF      WKPTBYO-SRYYMD  <=  WRK-SYMD
                   MOVE    WRK-SYMD        TO  WKPTBYO-TENKIYMD
               ELSE
                   MOVE    3               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       2410-TENKIYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３４：補足コメント　処理
      *****************************************************************
       2410-HOSOKU-SET-SEC         SECTION.
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOSOKU-SET-EXT
           END-IF
      *
      *    桁チェック
           IF      WRK-MO-CNT      >   40
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-HOSOKU-SET-EXT
           END-IF
      *
      *    空白チェック
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  =   SPACE
               GO  TO  2410-HOSOKU-SET-EXT
           END-IF
      *
      *    病名セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    40              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                       TO  WKPTBYO-HOSOKU-COMMENT
               COMPUTE IDZ     =   WRK-LEN     -   1
               IF    ( WRK-DATA (1:2)      =   "（" )  OR
                     ( WRK-DATA (IDZ:2)    =   "）" )
                   PERFORM 2910-ERRREC-EDIT-HOSOKU-SEC
               END-IF
           END-IF
      *
           .
       2410-HOSOKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３５：労災・自賠責保険レコード番号　処理
      *****************************************************************
       2410-RENNUM-SET-SEC         SECTION.
      *
           IF      PARA-00-7       NOT =   "2"
               GO  TO  2410-RENNUM-SET-EXT
           END-IF
      *
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENNUM-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >   2
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-RENNUM-SET-EXT
           END-IF
      *
           IF      CSV-REC (WRK-MO-ST:WRK-MO-CNT)  =   ZERO
               GO  TO  2410-RENNUM-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-RENNUM
      *
           .
       2410-RENNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　登録処理
      *****************************************************************
       260-PTBYOMEI-INSERT-SEC     SECTION.
      *
      *    患者番号変換
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM           NOT =   ZERO
               MOVE    7               TO  FLG-ERR
               GO  TO  260-PTBYOMEI-INSERT-EXT
           END-IF
      *
      *    労災・自賠責保険レコード番号指定されていれば
           IF      WRK-RENNUM  NOT =   ZERO
      *        労災ファイルがあれば
               IF      FLG-CHK        =   ZERO
                   INITIALIZE                  ISAM-REC
                   MOVE    PTNUM-PTID      TO  ISAM-PTID
                   MOVE    WRK-RENNUM      TO  ISAM-RENNUM
                   PERFORM 900-ISAM-READ-SEC
                   INITIALIZE                  WRK-HEN-AREA
                   MOVE    ISAM-HKNID      TO  WRK-HEN-HKNCOMBI(1) 
                   MOVE    1               TO  CNT-HKNCOMB
               END-IF
           END-IF
      *
           MOVE    PTNUM-PTID      TO  WKPTBYO-PTID
           MOVE    ZERO            TO  CNT-RENNUM
      *
      *    追加前病名チェック
           PERFORM 2601-BYOMEI-CHECK-SEC
           IF      FLG-ERR         NOT =   ZERO
               GO  TO  260-PTBYOMEI-INSERT-EXT
           END-IF
      *    追加前転帰日・転帰区分チェック
           PERFORM 2601-TENKIYMD-CHECK-SEC
           IF      FLG-ERR         NOT =   ZERO
               GO  TO  260-PTBYOMEI-INSERT-EXT
           END-IF
      *    追加前保険組合せチェック
           PERFORM 2601-HKNCOMBI-CHECK-SEC
      *
      *    重複データがないかチェック
           MOVE    ZERO            TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WKPTBYOMEI-REC  TO  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 940-PTBYOMEI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    データがすでにある場合は最大連番から連番を振る
           IF      FLG-PTBYOMEI    =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               COMPUTE CNT-RENNUM      =   PTBYO-RENNUM +   1
           ELSE
               MOVE     1              TO  CNT-RENNUM
           END-IF
      *
      *    患者病名追加処理
           MOVE    ZERO            TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WKPTBYOMEI-REC  TO  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    CNT-RENNUM      TO  PTBYO-RENNUM
           MOVE    9999            TO  PTBYO-SEQNUM
           MOVE    SPA-TERMID      TO  PTBYO-TERMID
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               MOVE    8               TO  FLG-ERR
               GO  TO  260-PTBYOMEI-INSERT-EXT
           END-IF
      *
           COMPUTE CNT-PTBYOMEI    =   CNT-PTBYOMEI    +   1
      *
           .
       260-PTBYOMEI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加前病名チェック
      *****************************************************************
       2601-BYOMEI-CHECK-SEC       SECTION.
      *
      *    病名検索チェック
           MOVE    SPACE           TO  ORCSBYOMEIAREA
           INITIALIZE                  ORCSBYOMEIAREA
           MOVE    "1"             TO  LNK-BYO-SYORI
           MOVE    WKPTBYO-UTAGAIFLG
                                   TO  LNK-BYO-UTAGAIFLG
           MOVE    WKPTBYO-SYUBYOFLG
                                   TO  LNK-BYO-SYUBYOFLG
           MOVE    WKPTBYO-BYOMEI  TO  LNK-BYO-BYOMEI
           MOVE    WKPTBYO-SRYYMD  TO  LNK-BYO-SRYYMD-IN
           CALL    "ORCSBYOMEI"        USING
                                       ORCSBYOMEIAREA
                                       SPA-AREA
      *
      *    エラーチェック
           IF    ( LNK-BYO-KANACHK-ERR     =   1 ) OR
                 ( LNK-BYO-RC          NOT =   SPACE )
               MOVE    5               TO  FLG-ERR
               GO  TO  2601-BYOMEI-CHECK-EXT
           END-IF
      *    エラーチェック
           IF    ( LNK-BYO-TANDOKUKBN-ERR  =   1 )
               MOVE    6               TO  FLG-ERR
               GO  TO  2601-BYOMEI-CHECK-EXT
           END-IF
      *
      *    基本病名コード
           MOVE    LNK-BYO-KHNBYOMEICD TO  WKPTBYO-KHNBYOMEICD
      *    基本部位コード
           MOVE    LNK-BYO-KHNBUICD(1) TO  WKPTBYO-KHNBUICD(1)
           MOVE    LNK-BYO-KHNBUICD(2) TO  WKPTBYO-KHNBUICD(2)
           MOVE    LNK-BYO-KHNBUICD(3) TO  WKPTBYO-KHNBUICD(3)
      *    病名コード数
           MOVE    LNK-BYO-BYOMEICDSU  TO  WKPTBYO-BYOMEICDSU
      *
      *    病名コードセット
           PERFORM VARYING IDI FROM  1 BY  1
                   UNTIL  (IDI >   LNK-BYO-BYOMEICDSU)
                   OR     (LNK-BYO-TBYOMEICD(IDI)  =   SPACE)
      *        診療開始日＜廃止年月日
      *         IF    ( LNK-BYO-THAISIYMD(IDY)  NOT =   SPACE ) AND
      *               ( WKPTBYO-SRYYMD  <   LNK-BYO-THAISIYMD(IDY))
                   MOVE    LNK-BYO-TBYOMEICD(IDI)
                                       TO  WKPTBYO-BYOMEICD(IDI)
      *         END-IF
           END-PERFORM
      *
      *    疑いフラグ
           MOVE    LNK-BYO-UTAGAIFLG   TO  WKPTBYO-UTAGAIFLG
      *
      *    難病外来
           IF      LNK-BYO-NANBYOCD
                                   NOT =   ZERO
               MOVE     LNK-BYO-NANBYOCD
                                       TO  WKPTBYO-MANSEIKBN
           ELSE
      *        慢性フラグ
               IF      WKPTBYO-MANSEIKBN   =  ZERO
                   MOVE    LNK-BYO-MANSEIKBN
                                           TO  WKPTBYO-MANSEIKBN
               END-IF
           END-IF
      *
      *    病名セット
           MOVE    LNK-BYO-BYOMEI      TO  WKPTBYO-BYOMEI
      *    病名文字数セット
           MOVE    LNK-BYO-BYOMEIMOJI  TO  WKPTBYO-BYOMEIMOJI
      *
      *    病名編集フラグ
           MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  WKPTBYO-BYOMEIHENFLG
      *    レセ電エラーフラグ
           MOVE    LNK-BYO-RECEDENFLG  TO  WKPTBYO-RECEDENFLG
      *
           .
       2601-BYOMEI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加前保険組合せチェック
      *****************************************************************
       2601-HKNCOMBI-CHECK-SEC     SECTION.
      *
           IF      CNT-HKNCOMB     =   ZERO
               GO  TO  2601-HKNCOMBI-CHECK-EXT
           END-IF
      *
      *    保険組合せ読み込み
           MOVE    ZERO            TO  FLG-COMB
           MOVE    SPACE           TO  COMB-REC
           INITIALIZE                  COMB-REC
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    WKPTBYO-PTID    TO  COMB-PTID
           MOVE    COMB-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
      *        保険組み合わせ読み込み
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-COMB
           END-IF
      *
           PERFORM UNTIL   FLG-COMB    =   1
      *        保険組合せセット
               PERFORM 26011-HKNCOMBI-SET-SEC
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2601-HKNCOMBI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　保険組合せ
      *****************************************************************
       26011-HKNCOMBI-SET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNCOMBI
      *
      *    主病名コード
           IF    ( COMB-HKNID      NOT =   SPACE ) OR
                 ( COMB-HKNID          =   ZERO )
               IF      WRK-HEN-HKNCOMBI(1)     =   COMB-HKNID
                   ADD     1           TO  FLG-HKNCOMBI
               ELSE
                   MOVE    9           TO  FLG-HKNCOMBI
               END-IF
           END-IF
      *
      *    公費病名コード
           PERFORM VARYING IDI     FROM    1   BY  1
                   UNTIL ( IDI     >   4 )
                   OR    ( FLG-HKNCOMBI    =   9 )
               IF    ( COMB-KOHID(IDI) NOT =   SPACE ) AND
                     ( COMB-KOHID(IDI) NOT =   ZERO )
                   PERFORM VARYING IDJ     FROM    2   BY  1
                           UNTIL ( IDJ     >   4 )
                       IF  COMB-KOHID(IDI) =  WRK-HEN-HKNCOMBI(IDJ)
                           ADD     1          TO  FLG-HKNCOMBI
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    該当する組み合わせがない、もしくは公費の数があわない場合
           IF     ( FLG-HKNCOMBI   =   9 )  OR
                  ( CNT-HKNCOMB    NOT =   FLG-HKNCOMBI )
      *        保険組み合わせ読み込み
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    COMB-HKNCOMBINUM    TO  WKPTBYO-HKNCOMBI
               MOVE    1                   TO  FLG-COMB
           END-IF
      *
           .
       26011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   追加前転帰日・転帰区分チェック
      *****************************************************************
       2601-TENKIYMD-CHECK-SEC     SECTION.
      *
           IF      WKPTBYO-TENKIKBN    NOT =   SPACE
      *        転帰区分しかないとき
               IF      WKPTBYO-TENKIYMD    =   SPACE
                   MOVE    WKPTBYO-SRYYMD  TO  WRK-SYMD
      *            翌日算出処理
                   PERFORM 20021-YOKUZITU-GET-SEC
                   MOVE    WRK-SYMD        TO  WKPTBYO-TENKIYMD
               END-IF
           ELSE
      *        転帰日しかないとき
               IF      WKPTBYO-TENKIYMD    NOT =   SPACE
      *            治癒セット
                   MOVE    "1"         TO  WKPTBYO-TENKIKBN
               END-IF
           END-IF
      *
      *    同一病名読み込み
           MOVE    ZERO                TO  PTBYOMEI-REC
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    WKPTBYO-PTID        TO  PTBYO-PTID
           MOVE    WKPTBYO-SRYKA       TO  PTBYO-SRYKA
           MOVE    WKPTBYO-BYOMEI      TO  PTBYO-BYOMEI
           MOVE    WKPTBYO-NYUGAIKBN   TO  PTBYO-NYUGAIKBN
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptbyomei"   TO  MCP-TABLE
               MOVE    "key21"          TO  MCP-PATHNAME
      *        患者病名読み込み
               PERFORM 940-PTBYOMEI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL   FLG-PTBYOMEI    =   1
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
      *        転帰日・診療開始日チェック
               IF    ( PTBYO-TENKIYMD  =   SPACE ) 
                   IF    ( WKPTBYO-SRYYMD      >   PTBYO-SRYYMD )
                       MOVE    4               TO  FLG-ERR
                   END-IF
                   IF    ( WKPTBYO-TENKIYMD    =   SPACE )
                       MOVE    4               TO  FLG-ERR
                   ELSE
                       IF    ( WKPTBYO-TENKIYMD    > PTBYO-SRYYMD )
                           MOVE    4               TO  FLG-ERR
                       END-IF
                   END-IF
               ELSE
                   IF    ( WKPTBYO-SRYYMD  >   PTBYO-SRYYMD ) AND
                         ( WKPTBYO-SRYYMD  <   PTBYO-TENKIYMD )
                       MOVE    4               TO  FLG-ERR
                   END-IF
                   IF      WKPTBYO-TENKIYMD    NOT =   SPACE
                       IF    ( WKPTBYO-TENKIYMD    >   PTBYO-SRYYMD )
                       AND   ( WKPTBYO-TENKIYMD    <   PTBYO-TENKIYMD )
                           MOVE    4               TO  FLG-ERR
                       END-IF
                       IF    ( WKPTBYO-SRYYMD      =   PTBYO-SRYYMD )
                       AND   ( WKPTBYO-TENKIYMD    =   PTBYO-TENKIYMD )
                           MOVE    4               TO  FLG-ERR
                       END-IF
                   END-IF
               END-IF
      *        患者病名読み込み
               PERFORM 940-PTBYOMEI-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"   TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2601-TENKIYMD-CHECK-EXT.
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
      *    追加前エラー（病名適応期間不正）
           WHEN    4
      *    追加前エラー（病名カナ変換失敗）
           WHEN    5
      *    追加前エラー（病名ＣＤ変換失敗）
           WHEN    6
      *    患者番号取得エラー
           WHEN    7
      *    患者保険ＤＢ出力エラー
           WHEN    8
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
      *    １：患者番号
           WHEN    1
               MOVE    "患者番号:"       TO  ERR1-KOMOKU(IDE)
      *    ２：診療科
           WHEN    2
               MOVE    "診療科  :"       TO  ERR1-KOMOKU(IDE)
      *    ３：診療開始日
           WHEN    3
               MOVE    "開始日  :"       TO  ERR1-KOMOKU(IDE)
      *    ４：病名
           WHEN    4
               MOVE    "病名    :"       TO  ERR1-KOMOKU(IDE)
      *    １３：疑いフラグ
           WHEN    13
               MOVE    "疑い区分:"       TO  ERR1-KOMOKU(IDE)
      *    １４：主病名フラグ
           WHEN    14
               MOVE    "主病区分:"       TO  ERR1-KOMOKU(IDE)
      *    １６：入外区分
           WHEN    16
               MOVE    "入外区分:"       TO  ERR1-KOMOKU(IDE)
      *    １７〜２０：保険限定レコード番号
           WHEN    17
               MOVE    "保険組１:"       TO  ERR1-KOMOKU(IDE)
           WHEN    18
               MOVE    "保険組２:"       TO  ERR1-KOMOKU(IDE)
           WHEN    19
               MOVE    "保険組３:"       TO  ERR1-KOMOKU(IDE)
           WHEN    20
               MOVE    "保険組４:"       TO  ERR1-KOMOKU(IDE)
      *    ２１：レセプト表示月数
           WHEN    21
               MOVE    "表示月数:"       TO  ERR1-KOMOKU(IDE)
      *    ２２：転帰事由区分
           WHEN    22
               MOVE    "転帰事由:"       TO  ERR1-KOMOKU(IDE)
      *    ２４：転帰日
           WHEN    24
               MOVE    "転帰日  :"       TO  ERR1-KOMOKU(IDE)
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
           MOVE    "患者番号:"     TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"     TO  ERR2-PTIDMES
           MOVE    "患者病名:"     TO  ERR2-BYOMEIMES
      *
           MOVE    SPA-PTNUM       TO  ERR2-PTNUM
           MOVE    WKPTBYO-PTID    TO  ERR2-PTID
           MOVE    WKPTBYO-BYOMEI  TO  ERR2-BYOMEI
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           .
       2910-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足リスト編集　処理
      *****************************************************************
       2910-ERRREC-EDIT-HOSOKU-SEC SECTION.
      *
           MOVE    SPACE           TO  ERR-REC2
           MOVE    CNT-CSV         TO  ERR2-LINENO
           MOVE    "[A]"           TO  ERR2-ERRNO
           MOVE    "患者番号:"     TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"     TO  ERR2-PTIDMES
           MOVE    "コメント:"     TO  ERR2-BYOMEIMES
      *
           MOVE    SPA-PTNUM       TO  ERR2-PTNUM
           MOVE    WKPTBYO-PTID    TO  ERR2-PTID
           MOVE    WKPTBYO-HOSOKU-COMMENT
                                   TO  ERR2-BYOMEI
           MOVE    "MSG"           TO  ERR2-NAIYO
      *
           MOVE    ERR-REC2        TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2910-ERRREC-EDIT-HOSOKU-EXT.
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
               DISPLAY "*(ORCVTPTBYOMEI)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L4   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2710-ERRLST-INIT-EXT.
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
      *    翌日取得処理
      *****************************************************************
       20021-YOKUZITU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
           MOVE    1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
      *
           .
       20021-YOKUZITU-GET-EXT.
            EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    帳票出力　終了処理
           CLOSE   CSV-FILE
      *    労災ファイルがあれば
           IF      STS-ISAM        =   ZERO
               CLOSE   ISAM-FILE
           END-IF
           CLOSE   ERR-FILE
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTPTBYOMEI)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTPTBYOMEI)* PTBYOMEI/O CNT[" CNT-PTBYOMEI "]"
           DISPLAY "*(ORCVTPTBYOMEI)* ERR     /O CNT[" CNT-ERR      "]"
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
      *    患者病名マスタＦＥＴＣＨ処理
      *****************************************************************
       940-PTBYOMEI-NEXT-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       940-PTBYOMEI-NEXT-EXT.
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
           INITIALIZE                  ORCSADDSIGNAREA
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
      *    保険組合せマスタＦＥＴＣＨ処理
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  COMB-REC
               MOVE    ZERO            TO  FLG-COMB
           ELSE
               INITIALIZE                  COMB-REC
               MOVE    1               TO  FLG-COMB
           END-IF
      *
           .
       940-HKNCOMBI-NEXT-EXT.
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
      *    患者病名ＣＳＶファイル　検索処理
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
      *    ＩＳＡＭファイル読み込み処理
      *****************************************************************
       900-ISAM-READ-SEC           SECTION.
      *
           READ    ISAM-FILE
               INVALID
                   MOVE    1           TO  FLG-ISAM
               NOT INVALID
                   MOVE    ZERO        TO  FLG-ISAM
           END-READ
      *
           .
       900-ISAM-READ-EXT.
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
       900-DBDISCONNECT-EXT.
           EXIT.
      *
