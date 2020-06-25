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
       PROGRAM-ID.                 ORCVTPTHKNINF02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理
      *                     （患者保険情報登録処理）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/05    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.02.00    NACL-多々納   08/04/28  後期高齢者対応
      *  04.07.00    NACL-多々納   14/04/14  Ｈ２６年４月改正対応
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
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
      *    分割レコード
       FD  ISAM-FILE.
       01  ISAM-REC.
           03  ISAM-KEY.
               05  ISAM-PTID           PIC 9(10).
               05  ISAM-HKNID          PIC 9(04).
           03  ISAM-TBL.
               05  ISAM-DATA           PIC X(400).
               05  ISAM-PTNUM          PIC X(20).
               05  ISAM-FTNKBN         PIC X(01).
               05  ISAM-GAIFTN         PIC 9(03).
               05  ISAM-NYUFTN         PIC 9(03).
               05  ISAM-CNT            PIC 9(06).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PARA                PIC X(256)  VALUE   SPACE.
           03  ASS-ERR                 PIC X(256)  VALUE   SPACE.
           03  ASS-ISAM                PIC X(256)  VALUE
               "/tmp/ORCVTPTHKNINF_ISAM".
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
           03  FLG-END                 PIC 9(01).
           03  FLG-ISAM                PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNNUM             PIC 9(01).
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
           03  FLG-YMD                 PIC 9(01).
      *
           03  FLG-ROUJIN              PIC 9(01).
           03  FLG-JUFUKU              PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ISAM                PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTNUM               PIC 9(06).
           03  CNT-PTHKNINF            PIC 9(06).
           03  CNT-IN                  PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDI                     PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDH                     PIC 9(04).
           03  IDX-C                   PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-06-1               PIC X(08).
           03  PARA-00-2               PIC X(01).
           03  PARA-09-2               PIC X(06).
           03  PARA-11-2               PIC X(06).
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
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-HKNID               PIC 9(10).
           03  WRK-NUM1                PIC 9(01).
           03  WRK-Z9                  PIC Z9.
     *
           03  WRK-HEN-FTNKBN2         PIC X(01).
     *
           03  WRK-BASEYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-BYMD-70.
               05  WRK-B70-YY          PIC 9(04).
               05  WRK-B70-MM          PIC 9(02).
               05  WRK-B70-DD          PIC 9(02).
           03  WRK-BYMD-70-1           PIC X(08).
           03  WRK-BYMD-75.
               05  WRK-B75-YY          PIC 9(04).
               05  WRK-B75-MM          PIC 9(02).
               05  WRK-B75-DD          PIC 9(02).
      *
           03  WRK-HEN-GAIFTN          PIC 9(03).
           03  WRK-HEN-NYUFTN          PIC 9(03).
           03  WRK-HEN-TEKSTYMD        PIC X(08).
           03  WRK-HEN-TEKEDYMD        PIC X(08).
           03  WRK-HEN-FTNKBN          PIC X(01).
           03  WRK-HEN-HOJOKBN         PIC X(01).
      *
      *    テーブル
       01  TBL-DATA-AREA.
           03  TBL-DATA-REC            OCCURS   20.
               05  TBL-ISAM-PTNUM          PIC X(20).
               05  TBL-ISAM-FTNKBN         PIC X(01).
               05  TBL-ISAM-GAIFTN         PIC 9(03).
               05  TBL-ISAM-NYUFTN         PIC 9(03).
               05  TBL-ISAM-CNT            PIC 9(06).
               05  TBL-ISAM-SYORI          PIC X(01).
               05  TBL-ISAM-ERR            PIC X(01).
      *
      *    患者保険情報
       01  TBL-PTHKNINF-AREA.
           02  TBL-PTHKNINF-REC        OCCURS   20.
           COPY    "CPPTHKNINF.INC"   REPLACING
                                      //PTHKN-// BY //TBL-PTHKN-//.
      *
      *    保険番号テーブル
       01  TBL-HKNNUM-AREA.
           02  TBL-HKNNUM-REC          OCCURS  300.
           COPY    "CPHKNNUM.INC"      REPLACING
                                       //HKN-// BY //TBL-HKN-//.
       01  IDX-HKN-MAX                 PIC 9(04).
      *
       01  HZN-HKNNUM-AREA.
           02  HZN-HKNNUM-REC          OCCURS  2.
           COPY    "CPHKNNUM.INC"      REPLACING
                                       //HKN-// BY //HZN-HKN-//.
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(28) VALUE
                                       "【PGID:ORCVTPTHKNINF02.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(10) VALUE "LINENO ERR".
                   07  FILLER          PIC X(42) VALUE
                   " [1]患者情報取得失敗  [2]保険期間重複 ".
                   07  FILLER          PIC X(36) VALUE
                   " [3]保険番号なし [4]患者保険登録済".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]補助区分エラー [6]開始日＞後期高齢年齢".
                   07  FILLER          PIC X(42) VALUE
                   " [7]患者番号更新失敗[8]患者保険出力失敗".
               05  MES-TITLE2-L4.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
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
               05  ERR2-HKNID          PIC X(02).
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
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
                   UNTIL   FLG-END     =   1
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
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    ASS-PARA
           END-UNSTRING
      *
      *    パラメータ情報取得
           PERFORM 101-PARA-GET-SEC
           IF  FLG-PARA            =   2
               MOVE    1           TO  FLG-END
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-END
               DISPLAY "*(ORCVTPTHKNINF02)* HOSPNUM ERR ["
                                           PARA-HOSPNUM"]"
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
           IF  FLG-END             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 120-CP1009-KENSAKU-SEC
           IF  FLG-END             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ISAMファイル  OPEN
           OPEN    INPUT           ISAM-FILE
           IF      STS-ISAM        NOT =   ZERO
               MOVE    1               TO  FLG-END
               DISPLAY "*(ORCVTPTHKNINF02)* ISAM-FILE OPN STS["
                                       STS-ISAM "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF  FLG-END             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-2       TO  WRK-COMMIT
           MOVE    PARA-11-2       TO  WRK-ERR
      *
      *    保険番号テーブル設定
           PERFORM 1009-HKNNUM-TBL-SEC
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
               DISPLAY "*(ORCVTPTHKNINF02)* PARA-FILE OPN STS["
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
                 ( PARA-09-2           IS  NUMERIC )   AND
                 ( PARA-11-2           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-END
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
               WHEN    "02-2"
                   IF      ASS-ERR     =   SPACE
                       MOVE    PARA-DATA   TO  ASS-ERR
                   END-IF
               WHEN    "02-A"
                   MOVE    PARA-DATA   TO  ASS-ERR
               WHEN    "05-1"
                   MOVE    PARA-DATA   TO  PARA-05-1
               WHEN    "06-1"
                   MOVE    PARA-DATA   TO  PARA-06-1
               WHEN    "09-2"
                   MOVE    PARA-DATA   TO  PARA-09-2
               WHEN    "00-2"
                   MOVE    PARA-DATA   TO  PARA-00-2
               WHEN    "11-2"
                   MOVE    PARA-DATA   TO  PARA-11-2
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
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   1
               MOVE    1               TO  FLG-END
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
               MOVE    1               TO  FLG-END
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
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  TBL-DATA-AREA
           INITIALIZE                  TBL-PTHKNINF-AREA
           MOVE    ZERO                TO  IDX-T
      *
           MOVE    ZERO                TO  WRK-PTID
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ISAM-REC
           START    ISAM-FILE
                    KEY IS    >=   ISAM-KEY
               INVALID
                   MOVE    1           TO  FLG-ISAM
               NOT INVALID
                   PERFORM 900-ISAM-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-ISAM    =   1 )
      *
      *    1000件ごとにメッセージを表示
               IF      CNT-ISAM(4:3)       =   ZERO
                   DISPLAY "***(ORCVTPTHKNINF02)*** CNT-ISAM[" 
                                               CNT-ISAM "]"
               END-IF
      *        患者毎に処理を行う
               IF    (WRK-PTID         NOT =   ISAM-PTID ) AND
                     (IDX-T                >   ZERO    )
                   PERFORM 5001-PTHKNINF-SYORI-SEC
      *            DISPLAY表示
                   PERFORM 2001-DISPLAY-SEC
                   INITIALIZE                  TBL-PTHKNINF-AREA
                   INITIALIZE                  TBL-DATA-AREA
                   MOVE    ZERO                TO  IDX-T
                   MOVE    ZERO                TO  FLG-ERR
               END-IF
               ADD     1                   TO  IDX-T
               MOVE    ISAM-DATA           TO  TBL-PTHKNINF-REC(IDX-T)
               MOVE    ISAM-PTNUM          TO  TBL-ISAM-PTNUM (IDX-T)
               MOVE    ISAM-FTNKBN         TO  TBL-ISAM-FTNKBN (IDX-T)
               MOVE    ISAM-GAIFTN         TO  TBL-ISAM-GAIFTN (IDX-T)
               MOVE    ISAM-NYUFTN         TO  TBL-ISAM-NYUFTN (IDX-T)
               MOVE    "1"                 TO  TBL-ISAM-SYORI  (IDX-T)
               MOVE    ISAM-CNT            TO  TBL-ISAM-CNT    (IDX-T)
               MOVE    ISAM-PTID           TO  WRK-PTID
      *
               PERFORM 900-ISAM-NEXT-SEC
           END-PERFORM
      *
           IF      IDX-T            >   ZERO
               PERFORM 5001-PTHKNINF-SYORI-SEC
           END-IF
      *    処理終了
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY表示処理
      *****************************************************************
       2001-DISPLAY-SEC        SECTION.
      *
      *    指定した件数になったらコミット処理
           IF      CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理終了
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2001-DISPLAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ登録処理
      *****************************************************************
       5001-PTHKNINF-SYORI-SEC        SECTION.
      *
      *    同一保険の重複チェック
           IF      IDX-T            >   1
               PERFORM 50011-PTHKNINF-JUFUKU-SEC
           END-IF
      *
      *    保険番号期間チェック
           IF      FLG-ERR              =   ZERO
               PERFORM 50012-HKNNUM-CHK-SEC
           END-IF
      *    患者保険マスタ登録処理
           IF      FLG-ERR              =   ZERO
               PERFORM 5001-PTHKNINF-INS-SYORI-SEC
           END-IF
           IF      FLG-ERR         NOT =   ZERO
               PERFORM 290-ERRR-SYORI-SEC
           END-IF
           .
       5001-PTHKNINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    同一保険の重複チェック処理
      *****************************************************************
       50011-PTHKNINF-JUFUKU-SEC        SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20  )
                        OR   (TBL-ISAM-SYORI (IDX) =   SPACE )
      *
               IF     (TBL-PTHKN-HKNNUM (IDX)  =   "980" ) OR
                      (TBL-PTHKN-CONTKBN (IDX) NOT =   SPACE )
                  OR ((TBL-PTHKN-HKNNUM (IDX)   =   "002" )  AND
                      (TBL-PTHKN-HOJOKBN  (IDX) NOT =   SPACE) AND
                      (TBL-PTHKN-HONKZKKBN(IDX) =   "1"   ))
                   CONTINUE
               ELSE
                   MOVE    TBL-PTHKN-TEKSTYMD(IDX) TO  WRK-HEN-TEKSTYMD
                   MOVE    TBL-PTHKN-TEKEDYMD(IDX) TO  WRK-HEN-TEKEDYMD
                   PERFORM 500211-JUFUKU-CHK-SEC
                   IF      FLG-JUFUKU          =   1
                       MOVE    2               TO  FLG-ERR
                       MOVE    "2"             TO  TBL-ISAM-ERR  (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       50011-PTHKNINF-JUFUKU-EXT.
           EXIT.
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       500211-JUFUKU-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JUFUKU
           PERFORM VARYING     IDX-C   FROM    IDX  BY  1
                   UNTIL       IDX-C   >   20
               IF      (IDX-C                      =   IDX  )  OR
                       (TBL-PTHKN-HKNNUM (IDX-C)   =   "980" ) OR
                       (TBL-PTHKN-CONTKBN (IDX-C)  NOT =   SPACE )
                   OR  (TBL-PTHKN-HKNNUM (IDX-C)   =   SPACE )
                   OR ((TBL-PTHKN-HKNNUM (IDX-C)   =   "002" )  AND
                       (TBL-PTHKN-HOJOKBN  (IDX-C) NOT =   SPACE) AND
                       (TBL-PTHKN-HONKZKKBN(IDX-C) =   "1"   ))
                   CONTINUE
               ELSE
                   IF     (WRK-HEN-TEKEDYMD    <   TBL-PTHKN-TEKSTYMD
                                                              (IDX-C))
                      AND (WRK-HEN-TEKSTYMD    <   TBL-PTHKN-TEKEDYMD
                                                              (IDX-C))
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-JUFUKU
                       MOVE    "1"             TO  TBL-ISAM-ERR(IDX-C)
                   END-IF
               END-IF
           END-PERFORM
           .
       500211-JUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタの期間チェック処理
      *****************************************************************
       50012-HKNNUM-CHK-SEC        SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20  )
                        OR   (TBL-ISAM-SYORI (IDX) =   SPACE )
               MOVE    TBL-PTHKNINF-REC (IDX)  TO  PTHKNINF-REC
               MOVE    ZERO                    TO  FLG-OK
               MOVE    ZERO                    TO  FLG-ARI
               MOVE    SPACE           TO  HZN-HKNNUM-AREA
               INITIALIZE                  HZN-HKNNUM-AREA
               PERFORM VARYING    IDH      FROM    1   BY  1
                       UNTIL     (IDH      >   IDX-HKN-MAX  )
                            OR   (FLG-OK   =   1   )
                   IF     (PTHKN-HKNNUM    =   TBL-HKN-HKNNUM (IDH))
                       MOVE    1               TO  FLG-ARI
                       IF     (PTHKN-TEKSTYMD
                                                >=  TBL-HKN-TEKSTYMD
                                                             (IDH)) AND
                              (PTHKN-TEKEDYMD
                                                <=  TBL-HKN-TEKEDYMD
                                                             (IDH))
                           MOVE    1            TO  FLG-OK
                       ELSE
      *                終了日が対象の時、対象とする
                           IF      PTHKN-TEKEDYMD  <=  TBL-HKN-TEKEDYMD
                                                                (IDH)
                               MOVE    TBL-HKNNUM-REC(IDH)
                                                   TO  HZN-HKNNUM-REC(1)
                           ELSE
                               MOVE    TBL-HKNNUM-REC(IDH)
                                                   TO  HZN-HKNNUM-REC(2)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      FLG-ARI             =   ZERO
      *            対象なし
                   MOVE    3               TO  FLG-ERR
                   MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
               ELSE
                   IF      FLG-OK              =   ZERO
      *               期間変更
                      PERFORM 50013-HKNNUM-HENKAN-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       50012-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ期間による期間変更処理
      *****************************************************************
       50013-HKNNUM-HENKAN-SEC        SECTION.
      *
      *    保険変更があった時、終了日を変更する
           IF     (HZN-HKN-TEKSTYMD (2)    NOT =   SPACE )
              AND (HZN-HKN-TEKEDYMD (2)    >=  PTHKN-TEKSTYMD )
      *        新しい保険を追加する
               IF      HZN-HKN-TEKSTYMD (1)    NOT =   SPACE
                   ADD     1                   TO  IDX-T
                   MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC (IDX-T)
                   MOVE    TBL-PTHKNINF-REC (IDX)
                                           TO  TBL-PTHKNINF-REC (IDX-T)
                   MOVE    HZN-HKN-TEKSTYMD (1)
                                           TO  TBL-PTHKN-TEKSTYMD(IDX-T)
                   MOVE    "A"             TO  TBL-ISAM-ERR (IDX-T)
               ELSE
                   MOVE    "C"             TO  TBL-ISAM-ERR (IDX)
               END-IF
      *        終了日を変更する
               MOVE    HZN-HKN-TEKEDYMD (2)    TO  PTHKN-TEKEDYMD
      *        開始日が保険より前の時、開始日を保険の開始日にする
               IF      HZN-HKN-TEKSTYMD (2)    >   PTHKN-TEKSTYMD
                   MOVE    HZN-HKN-TEKSTYMD(2) TO  PTHKN-TEKSTYMD
               END-IF
               MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC (IDX)
           ELSE
      *        保険の開始日を変更する
               IF      HZN-HKN-TEKSTYMD(1) <   TBL-PTHKN-TEKEDYMD(IDX)
                   MOVE    HZN-HKN-TEKSTYMD(1) TO  TBL-PTHKN-TEKSTYMD
                                                                (IDX)
                   MOVE    "B"                 TO  TBL-ISAM-ERR (IDX)
               ELSE
      *            対象なし
                   MOVE    3               TO  FLG-ERR
                   MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
               END-IF
           END-IF
           .
       50013-HKNNUM-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ登録処理
      *****************************************************************
       5001-PTHKNINF-INS-SYORI-SEC        SECTION.
      *
           MOVE    TBL-PTHKN-PTID (1)  TO  SPA-PTID
      *
      *    患者番号読み込み
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 930-PTNUM-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTNUM       NOT =   ZERO
               MOVE    1               TO  FLG-ERR
               GO  TO  5001-PTHKNINF-INS-SYORI-EXT
           END-IF
      *
      *    既に保険登録済みはエラー
           IF      PTNUM-HKNID         >   ZERO
               MOVE    4               TO  FLG-ERR
               GO  TO  5001-PTHKNINF-INS-SYORI-EXT
           END-IF
      *
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1               TO  FLG-ERR
               GO  TO  5001-PTHKNINF-INS-SYORI-EXT
           END-IF
      *
      *    前期高齢者判定
           MOVE    ZERO                TO  FLG-ROUJIN
      *    生年月日から７０歳到達日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    ZERO                TO  WRK-BASEYMD
           MOVE    ZERO                TO  WRK-BYMD-70
           MOVE    ZERO                TO  WRK-BYMD-75
           MOVE    PTINF-BIRTHDAY      TO  WRK-BASEYMD
           IF      WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01              TO  WRK-BDD
           END-IF
      *    ７０歳を迎える日
           MOVE    WRK-BASEYMD         TO  WRK-BYMD-70
           COMPUTE WRK-B70-YY          =   WRK-B70-YY  +   70
      *    前日
           MOVE    SPACE                TO  STS-AREA-DAY
           INITIALIZE                       STS-AREA-DAY
           INITIALIZE                       LNK-DAY6-AREA
           MOVE    "61"                 TO  LNK-DAY6-IRAI
           MOVE    WRK-BYMD-70          TO  LNK-DAY6-KIJUN
           MOVE    "1"                  TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN      TO  WRK-BYMD-70-1
      *    前期高齢者
           IF     (PTINF-BIRTHDAY      >   "19320930" ) AND
                  (WRK-BYMD-70         <=  WRK-SYSYMD )
               MOVE    1                   TO  FLG-ROUJIN
           END-IF
      *    ７５歳を迎える日
           MOVE    WRK-BASEYMD         TO  WRK-BYMD-75
           COMPUTE WRK-B75-YY          =   WRK-B75-YY  +   75
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20  )
                        OR   (TBL-ISAM-SYORI (IDX) =   SPACE )
      *
               MOVE    TBL-PTHKNINF-REC (IDX)  TO  PTHKNINF-REC
               MOVE    TBL-ISAM-FTNKBN  (IDX)  TO  WRK-HEN-FTNKBN
               MOVE    TBL-ISAM-GAIFTN  (IDX)  TO  WRK-HEN-GAIFTN
               MOVE    TBL-ISAM-NYUFTN  (IDX)  TO  WRK-HEN-NYUFTN
               MOVE    PTHKN-HOJOKBN           TO  WRK-HEN-HOJOKBN
      *
               EVALUATE    PTHKN-HKNNUM
                   WHEN    "980"
      *            自費
                       PERFORM 5004-HKNNUM980-HEN-SEC
                   WHEN    "039"
                   WHEN    "040"
      *            後期高齢者
                       PERFORM 5004-HKNNUM039-HEN-SEC
                   WHEN    OTHER
                       IF     (FLG-ROUJIN          =   1       )
                         AND  (PTHKN-CONTKBN       =   SPACE   )
      *                    前期高齢者
                           PERFORM 5002-ROUJIN-CHK-SEC
                       ELSE
      *                    補助区分再セット
                           PERFORM 5003-HOJYOKBN-HEN-SEC
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    患者保険マスター登録
           MOVE    ZERO                TO  WRK-HKNID
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20     )  OR
                             (TBL-ISAM-SYORI (IDX)  =   SPACE)
                        OR   (FLG-ERR      NOT =   ZERO )
               MOVE    TBL-PTHKNINF-REC (IDX)  TO  PTHKNINF-REC
               PERFORM 5009-PTHKNINF-INS-SEC
           END-PERFORM
      *    患者番号変換マスタ　更新処理
           IF      FLG-ERR             =   ZERO
               PERFORM 5010-PTNUM-UPDATE-SEC
           END-IF
      *
           .
       5001-PTHKNINF-INS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    補助区分再セット処理（前期高齢者以外）
      *****************************************************************
       5003-HOJYOKBN-HEN-SEC        SECTION.
      *
           EVALUATE    PTHKN-HKNNUM
      *    国保
           WHEN    "060"
      *    国保（特別療養費）
           WHEN    "068"
               IF    ( PTHKN-HKNJANUM(7:)      =   SPACE ) AND
                     ( PTHKN-HKNJANUM(3:1)     =   "3" )
      *            組合国保
                   EVALUATE    WRK-HEN-GAIFTN  ALSO  WRK-HEN-NYUFTN
                   WHEN    10      ALSO    0
                       MOVE    "4"             TO  PTHKN-HOJOKBN
                   WHEN    20      ALSO    10
                       MOVE    "5"             TO  PTHKN-HOJOKBN
                   WHEN    30      ALSO    20
                       MOVE    "6"             TO  PTHKN-HOJOKBN
                   END-EVALUATE
      *
                   MOVE    PTHKNINF-REC    TO  TBL-PTHKNINF-REC (IDX)
               END-IF
      *        補助区分未設定はエラー
               IF      PTHKN-HOJOKBN       =   SPACE
                   MOVE    5               TO  FLG-ERR
                   MOVE    "5"             TO  TBL-ISAM-ERR (IDX)
               END-IF
      *    船員
           WHEN    "002"
               IF      (PTINF-BIRTHDAY     <=  "19320930" )
                  AND  (PTHKN-HONKZKKBN    =   "1"    )
      *            前期高齢者でない高齢者の補助区分設定
                   IF      WRK-HEN-FTNKBN  =   SPACE
                                           OR  "0"
                       MOVE    "1"         TO  WRK-HEN-FTNKBN
                   END-IF
                   IF      (PTHKN-TEKSTYMD     >=   "20061001" )
                       IF      WRK-HEN-FTNKBN  =   "2"
                           MOVE    "3"         TO  WRK-HEN-FTNKBN
                       END-IF
                   ELSE
                       IF      WRK-HEN-FTNKBN  =   "3"
                           MOVE    "2"         TO  WRK-HEN-FTNKBN
                       END-IF
                   END-IF
                   PERFORM 5003-ROJIN-HOJYOKBN-HEN-SEC
                   MOVE    PTHKNINF-REC    TO  TBL-PTHKNINF-REC (IDX)
               END-IF
      *    共済組合
           WHEN    "031"
           WHEN    "032"
           WHEN    "033"
           WHEN    "034"
               CONTINUE
           WHEN    OTHER
               IF      PTHKN-HOJOKBN   NOT =   SPACE
                   MOVE    5               TO  FLG-ERR
                   MOVE    "5"             TO  TBL-ISAM-ERR (IDX)
               END-IF
           END-EVALUATE
      *
      *    Ｈ２０．４から開始日が７５歳以上の日はエラー
           IF      (PTHKN-HKNNUM       =   "002"  )
              AND  (PTHKN-HONKZKKBN    =   "1"    )
               CONTINUE
           ELSE
               IF      (PTHKN-TEKSTYMD     >=   "20080401"  )
                  AND  (PTHKN-TEKSTYMD     >=   WRK-BYMD-75 )
                   MOVE    6               TO  FLG-ERR
                   MOVE    "6"             TO  TBL-ISAM-ERR (IDX)
               END-IF
           END-IF
           .
       5003-HOJYOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    前期高齢者セット処理
      *****************************************************************
       5002-ROUJIN-CHK-SEC        SECTION.
      *
      *    前期高齢者到達前
           IF     (PTHKN-TEKEDYMD      <    "20021001" ) OR
                  (PTHKN-TEKEDYMD      <   WRK-BYMD-70 ) 
      *        一般の補助区分再セット
               PERFORM 5003-HOJYOKBN-HEN-SEC
           END-IF
      *
           IF      WRK-HEN-FTNKBN      =   SPACE
                                       OR  "0"
               MOVE    "1"             TO  WRK-HEN-FTNKBN
           END-IF
      *    前期高齢者到達後
           IF     (PTHKN-TEKEDYMD      >=   "20021001" ) AND
                  (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
      *        Ｈ１８．１０以降に前期高齢者
               IF      (PTHKN-TEKSTYMD     >=   "20061001" )
                   IF      (PTHKN-TEKSTYMD     <    WRK-BYMD-70 )
      *                前期高齢者到達日で切り替え
                       PERFORM 50022-HKNINF-BYMD-SEC
                   ELSE
      *H26.5
      *                H26.5.1以降のS19.4.2生以降の高齢者負担割合
                       IF      (PTINF-BIRTHDAY     >=  "19440402")
                           PERFORM 50024-FTNKBN-201405-SEC
                       ELSE
      *
      *                ２割は３割へ
                       IF      WRK-HEN-FTNKBN      =   "2"
                           MOVE    "3"             TO  WRK-HEN-FTNKBN
                       END-IF
      *
                       END-IF
      *                開始日が７５歳以上の日はエラー
                       IF      (PTHKN-TEKSTYMD     <    "20080401" )
                          OR   (PTHKN-TEKSTYMD     <    WRK-BYMD-75 )
                          OR  ((PTHKN-HKNNUM       =   "002"  ) AND
                               (PTHKN-HONKZKKBN    =   "1"    ))
                           PERFORM 5003-ROJIN-HOJYOKBN-HEN-SEC
                       ELSE
                           MOVE    6               TO  FLG-ERR
                           MOVE    "6"             TO  TBL-ISAM-ERR(IDX)
                       END-IF
                   END-IF
               ELSE
      *            Ｈ１８．１０以降に前期高齢者
                   IF      (PTHKN-TEKSTYMD     >=   WRK-BYMD-70 )
                      AND  (PTHKN-TEKSTYMD     >=   "20021001"  )
                       IF      (PTHKN-TEKEDYMD     <    "20061001" )
      *               Ｈ１４．１０からＨ１８．９まで
      *                    ３割は２割へ
                           IF      WRK-HEN-FTNKBN  =   "3"
                               MOVE    "2"         TO  WRK-HEN-FTNKBN
                           END-IF
                           PERFORM 5003-ROJIN-HOJYOKBN-HEN-SEC
                       ELSE
      *                    Ｈ１８．９で期間切替え
                           PERFORM 50021-HKNINF-H1810-SEC
                       END-IF
                   ELSE
      *                前期高齢者到達日で切り替え
                       PERFORM 50022-HKNINF-BYMD-SEC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC (IDX)
           .
       5002-ROUJIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    S19.4.2生以降の高齢者負担割合処理
      *****************************************************************
       50024-FTNKBN-201405-SEC        SECTION.
      *
           IF      (PTHKN-TEKSTYMD     >=   WRK-BYMD-70 )
      *        １割は２割へ
               IF      WRK-HEN-FTNKBN      =   "1"
                   MOVE    "2"             TO  WRK-HEN-FTNKBN
               END-IF
           ELSE
      *
      *        ２割は３割へ
               IF      WRK-HEN-FTNKBN      =   "2"
                   MOVE    "3"             TO  WRK-HEN-FTNKBN
               END-IF
           END-IF
           .
       50024-FTNKBN-201405-EXT.
           EXIT.
      *****************************************************************
      *    Ｈ１８．１０からの切替え処理
      *****************************************************************
       50021-HKNINF-H1810-SEC        SECTION.
      *
      *    ３割は２割へ
           IF      WRK-HEN-FTNKBN  =   "3"
               MOVE    "2"         TO  WRK-HEN-FTNKBN
           END-IF
           PERFORM 5003-ROJIN-HOJYOKBN-HEN-SEC
           IF      PTHKN-HOJOKBN       =   "2"
                                       OR  "8"
                                       OR  "D"
                                       OR  "E"
                                       OR  "F"
      *        ２割は期間変更して、３割へ
               MOVE    "20060930"          TO  WRK-HEN-TEKEDYMD
               MOVE    WRK-HEN-TEKEDYMD    TO  PTHKN-TEKEDYMD
      *        追加
               ADD     1                   TO  IDX-T
               MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC (IDX-T)
               MOVE    TBL-PTHKNINF-REC (IDX)
                                           TO  TBL-PTHKNINF-REC (IDX-T)
               MOVE    "20061001"          TO  TBL-PTHKN-TEKSTYMD(IDX-T)
           END-IF
           .
       50021-HKNINF-H1810-EXT.
           EXIT.
      *****************************************************************
      *    前期高齢者到達日で切替え処理
      *****************************************************************
       50022-HKNINF-BYMD-SEC        SECTION.
      *
           IF     (PTHKN-TEKSTYMD      <   WRK-BYMD-70 )
      *        追加
               ADD     1                   TO  IDX-T
               MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC (IDX-T)
               MOVE    TBL-PTHKNINF-REC (IDX)
                                           TO  TBL-PTHKNINF-REC
                                                               (IDX-T)
               MOVE    WRK-BYMD-70         TO  TBL-PTHKN-TEKSTYMD
                                                               (IDX-T)
      *        到達日前日
               MOVE    WRK-BYMD-70-1       TO  PTHKN-TEKEDYMD
      *        一般の補助区分再セット
               PERFORM 5003-HOJYOKBN-HEN-SEC
           ELSE
      *        Ｈ１４．１０で
               IF      PTHKN-TEKSTYMD      <   "20021001"
      *            追加
                   ADD     1                   TO  IDX-T
                   MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC
                                                               (IDX-T)
                   MOVE    TBL-PTHKNINF-REC (IDX)
                                               TO  TBL-PTHKNINF-REC
                                                               (IDX-T)
                   MOVE    "20021001"          TO  TBL-PTHKN-TEKSTYMD
                                                               (IDX-T)
      *            到達日前日
                   MOVE    "20020930"          TO  PTHKN-TEKEDYMD
      *            一般の補助区分再セット
                   PERFORM 5003-HOJYOKBN-HEN-SEC
               END-IF
           END-IF
           .
       50022-HKNINF-BYMD-EXT.
           EXIT.
      *****************************************************************
      *    前期高齢者分補助区分セット処理
      *****************************************************************
       5003-ROJIN-HOJYOKBN-HEN-SEC        SECTION.
      *
           EVALUATE    PTHKN-HKNNUM
      *    船員
           WHEN    "002"
               EVALUATE  WRK-HEN-FTNKBN    ALSO    WRK-HEN-HOJOKBN
      *        負担割合１
                   WHEN    "1"     ALSO    SPACE
                   WHEN    "1"     ALSO    "0"
                       MOVE    "9"             TO  PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "1"
                       MOVE    "A"             TO  PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "2"
                       MOVE    "B"             TO  PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "3"
                       MOVE    "C"             TO  PTHKN-HOJOKBN
      *        負担割合２
                   WHEN    "2"     ALSO    SPACE
                   WHEN    "2"     ALSO    "0"
                       MOVE    "8"             TO  PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "1"
                       MOVE    "D"             TO  PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "2"
                       MOVE    "E"             TO  PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "3"
                       MOVE    "F"             TO  PTHKN-HOJOKBN
      *        負担割合３
                   WHEN    "3"     ALSO    SPACE
                   WHEN    "3"     ALSO    "0"
                       MOVE    "7"             TO  PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "1"
                       MOVE    "G"             TO  PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "2"
                       MOVE    "H"             TO  PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "3"
                       MOVE    "I"             TO  PTHKN-HOJOKBN
               END-EVALUATE
      *    共済
           WHEN    "031"   THRU    "034"
               EVALUATE    WRK-HEN-FTNKBN  ALSO    WRK-HEN-HOJOKBN
                   WHEN    "1"     ALSO    "2"
                       MOVE    "B"             TO  PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "2"
                       MOVE    "E"             TO  PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    ANY
                       MOVE    "9"             TO  PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    ANY
                       MOVE    "8"             TO  PTHKN-HOJOKBN
      *            ３割
                   WHEN    "3"     ALSO    "2"
                       MOVE    "H"             TO  PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    ANY
                       MOVE    "7"             TO  PTHKN-HOJOKBN
               END-EVALUATE
      *    国保
           WHEN    "060"
      *    国保（特別療養費）
           WHEN    "068"
               IF    ( PTHKN-HKNJANUM(7:)      =   SPACE ) AND
                     ( PTHKN-HKNJANUM(3:1)     =   "3" )
                   EVALUATE    WRK-HEN-GAIFTN  ALSO  WRK-HEN-NYUFTN
                       WHEN    10      ALSO    0
                           MOVE    "4"             TO  PTHKN-HOJOKBN
                       WHEN    20      ALSO    10
                           MOVE    "5"             TO  PTHKN-HOJOKBN
                       WHEN    30      ALSO    20
                           MOVE    "6"             TO  PTHKN-HOJOKBN
                       WHEN    OTHER
      *                負担割合設定なしの時、負担区分の判定
                           EVALUATE    WRK-HEN-FTNKBN
      *                負担割合１
                           WHEN    "1"
                               MOVE    "1"             TO  PTHKN-HOJOKBN
      *                負担割合２
                           WHEN    "2"
                               MOVE    "2"             TO  PTHKN-HOJOKBN
      *                負担割合３
                           WHEN    "3"
                               MOVE    "3"             TO  PTHKN-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               ELSE
                   EVALUATE    WRK-HEN-FTNKBN
      *            負担割合１
                       WHEN    "1"
                           MOVE    "1"             TO  PTHKN-HOJOKBN
      *            負担割合２
                       WHEN    "2"
                           MOVE    "2"             TO  PTHKN-HOJOKBN
      *            負担割合３
                       WHEN    "3"
                           MOVE    "3"             TO  PTHKN-HOJOKBN
                   END-EVALUATE
               END-IF
           WHEN    OTHER
      *    通常の場合
               EVALUATE    WRK-HEN-FTNKBN
      *        負担割合１
                   WHEN    "1"
                       MOVE    "9"             TO  PTHKN-HOJOKBN
      *        負担割合２
                   WHEN    "2"
                       MOVE    "8"             TO  PTHKN-HOJOKBN
      *        負担割合３
                   WHEN    "3"
                       MOVE    "7"             TO  PTHKN-HOJOKBN
               END-EVALUATE
           END-EVALUATE
      *    補助区分未設定はエラー
           IF      PTHKN-HOJOKBN       =   SPACE
               MOVE    5               TO  FLG-ERR
               MOVE    "5"             TO  TBL-ISAM-ERR(IDX)
           END-IF
      *
           .
       5003-ROJIN-HOJYOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者処理
      *****************************************************************
       5004-HKNNUM039-HEN-SEC            SECTION.
      *
           IF      WRK-HEN-FTNKBN      =   SPACE
                                       OR  "0"
               MOVE    "1"             TO  WRK-HEN-FTNKBN
           END-IF
      *
           EVALUATE    WRK-HEN-FTNKBN
      *    負担割合１
               WHEN    "1"
                   MOVE    "1"             TO  PTHKN-HOJOKBN
      *    負担割合３
               WHEN    "3"
                   MOVE    "3"             TO  PTHKN-HOJOKBN
               WHEN    OTHER
      *            １、３以外はエラー
                   MOVE    5               TO  FLG-ERR
                   MOVE    "5"             TO  TBL-ISAM-ERR(IDX)
           END-EVALUATE
      *
           MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC (IDX)
           .
       5004-HKNNUM039-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費保険処理
      *****************************************************************
       5004-HKNNUM980-HEN-SEC            SECTION.
      *
      *    負担割合
           IF      PTHKN-HOJOKBN       =   SPACE
               MOVE    "1"             TO  PTHKN-HOJOKBN
           END-IF
      *    本人家族区分はすべて本人とする
           IF      PTHKN-HONKZKKBN NOT =   "1"
               MOVE    "1"             TO  PTHKN-HONKZKKBN
           END-IF
      *
           MOVE    PTHKNINF-REC        TO  TBL-PTHKNINF-REC  (IDX)
           .
       5004-HKNNUM980-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険マスター登録処理
      *****************************************************************
       5009-PTHKNINF-INS-SEC            SECTION.
      *
           ADD     1                   TO  WRK-HKNID
           MOVE    WRK-HKNID           TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    8               TO  FLG-ERR
               MOVE    "8"             TO  TBL-ISAM-ERR (IDX)
           ELSE
               ADD     1              TO  CNT-PTHKNINF
           END-IF
      *
           .
       5009-PTHKNINF-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       5010-PTNUM-UPDATE-SEC        SECTION.
      *
      *    最大保険ＩＤで患者番号変換マスタを更新
           IF      PTNUM-HKNID         <   WRK-HKNID
               MOVE    WRK-HKNID       TO  PTNUM-HKNID
               MOVE    SPA-SYSYMD      TO  PTNUM-UPYMD
      *
               MOVE    PTNUM-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF          MCP-RC      NOT =   ZERO
                   MOVE    7               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       5010-PTNUM-UPDATE-EXT.
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
                           OR (IDX-HKN-MAX >=  300 )
      *       国保の支払区分＝空白は対象外
              IF      (HKN-HKNNUM              =   "060" )
                 AND  (HKN-PAYKBN          NOT =   "00"  )
                   CONTINUE
              ELSE
      *
      *       保険のもの
              IF       HKN-HKNKOHSBTKBN        =   "1"  OR  "4"
                                               OR  "8"  OR  "9"
                   ADD     1                   TO  IDX-HKN-MAX
                   MOVE    HKNNUM-REC          TO  TBL-HKNNUM-REC
                                             (IDX-HKN-MAX)
               END-IF
      *
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
               MOVE    1                   TO  FLG-END
               DISPLAY "*(ORCVTPTHKNINF02)* TBL-HKNNUM NASI ERR"
           END-IF
           .
       1009-HKNNUM-TBL-EXT.
           EXIT.
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       290-ERRR-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  ERR-REC2
           MOVE    "["                 TO  ERR2-ERRNO (1:1)
           MOVE    FLG-ERR(1:1)        TO  ERR2-ERRNO (2:1)
           MOVE    "]"                 TO  ERR2-ERRNO (3:1)
           MOVE    "患者番号:"         TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"         TO  ERR2-PTIDMES
           MOVE    "保険番号:"         TO  ERR2-HKNNUMMES
           MOVE    "連番:"             TO  ERR2-HKNIDMES
           MOVE    "期間:"             TO  ERR2-TEKSTEDMES
      *
           MOVE    TBL-ISAM-PTNUM (1)  TO  ERR2-PTNUM
           MOVE    TBL-PTHKN-PTID (1)  TO  ERR2-PTID
      *
           EVALUATE    FLG-ERR
               WHEN    1
               WHEN    7
               WHEN    4
                   MOVE    TBL-ISAM-CNT (1)    TO  ERR2-LINENO
                   MOVE    "ERR"               TO  ERR2-NAIYO
                   PERFORM 2901-ERRLST-OUT-SEC
               WHEN    OTHER
                   PERFORM 2902-ERRTBL-OUT-SEC
           END-EVALUATE
           .
       290-ERRR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       2902-ERRTBL-OUT-SEC         SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                    UNTIL     (IDX      >   20     )  OR
                              (TBL-ISAM-SYORI (IDX)  =   SPACE)
               IF      TBL-ISAM-ERR (IDX)  NOT =   SPACE
                   MOVE    TBL-ISAM-CNT (IDX)  TO  ERR2-LINENO
                   MOVE    TBL-PTHKN-HKNNUM(IDX)   TO  ERR2-HKNNUM
                   MOVE    IDX                 TO  WRK-Z9
                   MOVE    WRK-Z9              TO  ERR2-HKNID
                   MOVE    TBL-PTHKN-TEKSTYMD (IDX)
                                               TO  ERR2-TEKSTYMD
                   MOVE    TBL-PTHKN-TEKEDYMD (IDX)
                                               TO  ERR2-TEKEDYMD
                   MOVE    "-"                 TO  ERR2-TEKBAR
                   MOVE    "ERR"               TO  ERR2-NAIYO
                   PERFORM 2901-ERRLST-OUT-SEC
               END-IF
           END-PERFORM
           .
       290-ERRR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       2901-ERRLST-OUT-SEC          SECTION.
      *
           ADD     1                   TO  CNT-ERR
      *
           MOVE    ERR-REC2            TO      ERR-REC
           WRITE   ERR-REC
      *
           .
       2901-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF      STS-LST         NOT =   ZERO
               MOVE    1               TO  FLG-END
               DISPLAY "*(ORCVTPTHKNINF02)* ERRFILE OPN STS["
                                                    STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO      ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO      ERR-REC
           WRITE   ERR-REC
      ***  MOVE    MES-TITLE2-L3   TO      ERR-REC
      ***  WRITE   ERR-REC
           MOVE    MES-TITLE2-L4   TO      ERR-REC
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
           CLOSE                       ISAM-FILE
           CLOSE                       ERR-FILE
      *
      *    ISAMファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS-ISAM        TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTPTHKNINF02)* IN      /I CNT[" CNT-ISAM "]"
           DISPLAY "*(ORCVTPTHKNINF02)* PTHKNINF/O CNT["
                                                     CNT-PTHKNINF "]"
           DISPLAY "*(ORCVTPTHKNINF02)* ERR     /O CNT["
                                                     CNT-ERR      "]"
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    患者保険マスタ読込
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
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
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       930-PTNUM-DBFETCH-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF          MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       930-PTNUM-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       950-HKNNUM-NEXT-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF          MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
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
      *    分割レコードファイル順読込
      *****************************************************************
       900-ISAM-NEXT-SEC           SECTION.
      *
           READ    ISAM-FILE       NEXT
               AT  END
                   MOVE    1           TO  FLG-ISAM
               NOT AT  END
                   MOVE    ZERO        TO  FLG-ISAM
                   ADD     1           TO  CNT-ISAM
                   ADD     1           TO  CNT-COMMIT
           END-READ
           .
       900-ISAM-NEXT-EXT.
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
