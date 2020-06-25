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
       PROGRAM-ID.                 ORCVTPTKOHINF02.
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
      *  04.02.00    NACL-多々納  08/04/28  Ｈ２０．４改正対応他
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
       01  ERR-REC                     PIC X(104).
      *
      *    分割レコード
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
           03  ASS-PARA                PIC X(256)  VALUE   SPACE.
           03  ASS-ERR                 PIC X(256)  VALUE   SPACE.
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
           03  FLG-END                 PIC 9(01).
           03  FLG-ISAM                PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
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
           03  CNT-PTKOHINF            PIC 9(06).
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
           03  IDX-HKN                 PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-06-2               PIC X(08).
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
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-KOHID               PIC 9(10).
           03  WRK-NUM1                PIC 9(01).
           03  WRK-Z9                  PIC Z9.
     *
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
      *
           03  WRK-HEN-TEKSTYMD        PIC X(08).
           03  WRK-HEN-TEKEDYMD        PIC X(08).
           03  WRK-HEN-FTNKBN          PIC X(01).
           03  WRK-HEN-PAYKBN          PIC X(02).
      *
      *    テーブル
       01  TBL-DATA-AREA.
           03  TBL-DATA-REC            OCCURS   20.
               05  TBL-ISAM-PTNUM          PIC X(20).
               05  TBL-ISAM-FTNKBN         PIC X(01).
               05  TBL-ISAM-CNT            PIC 9(06).
               05  TBL-ISAM-SYORI          PIC X(01).
               05  TBL-ISAM-ERR            PIC X(01).
      *
      *    患者保険情報
       01  TBL-PTKOHINF-AREA.
           02  TBL-PTKOHINF-REC        OCCURS   20.
           COPY    "CPPTKOHINF.INC"   REPLACING
                                      //PTKOH-// BY //TBL-PTKOH-//.
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
                                       "【PGID:ORCVTPTKOHINF02.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(10) VALUE "LINENO ERR".
                   07  FILLER          PIC X(42) VALUE
                   " [1]患者情報取得失敗  [2]公費期間重複 ".
                   07  FILLER          PIC X(36) VALUE
                   " [3]保険番号なし [4]患者公費登録済".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [7]患者番号更新失敗  [8]患者公費出力失敗".
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
               05  FILLER              PIC X(01).
               05  ERR2-PTIDMES        PIC X(09).
               05  ERR2-PTID           PIC X(10).
               05  FILLER              PIC X(01).
               05  ERR2-KOHNUMMES      PIC X(05).
               05  ERR2-KOHNUM         PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-KOHIDMES       PIC X(05).
               05  ERR2-KOHID          PIC X(02).
               05  FILLER              PIC X(01).
               05  ERR2-PAYKBNMES      PIC X(05).
               05  ERR2-PAYKBN         PIC X(02).
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
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
               DISPLAY "*(ORCVTPTKOHINF02)* HOSPNUM ERR ["
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
               DISPLAY "*(ORCVTPTKOHINF02)* ISAM-FILE OPN STS["
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
           MOVE    PARA-09-3       TO  WRK-COMMIT
           MOVE    PARA-11-3       TO  WRK-ERR
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
               DISPLAY "*(ORCVTPTKOHINF02)* PARA-FILE OPN STS["
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
                 ( PARA-09-3           IS  NUMERIC )   AND
                 ( PARA-11-3           IS  NUMERIC )
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
               WHEN    "02-3"
                   IF      ASS-ERR     =   SPACE
                       MOVE    PARA-DATA   TO  ASS-ERR
                   END-IF
               WHEN    "02-B"
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
      *
           INITIALIZE                  TBL-DATA-AREA
           INITIALIZE                  TBL-PTKOHINF-AREA
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
                   DISPLAY "***(ORCVTPTKOHINF02)*** CNT-ISAM[" 
                                               CNT-ISAM "]"
               END-IF
      *        患者毎に処理を行う
               IF    (WRK-PTID         NOT =   ISAM-PTID ) AND
                     (IDX-T                >   ZERO    )
                   PERFORM 5001-PTKOHINF-SYORI-SEC
      *            DISPLAY表示
                   PERFORM 2001-DISPLAY-SEC
      *
                   INITIALIZE                  TBL-PTKOHINF-AREA
                   INITIALIZE                  TBL-DATA-AREA
                   MOVE    ZERO                TO  IDX-T
                   MOVE    ZERO                TO  FLG-ERR
               END-IF
               ADD     1                   TO  IDX-T
               MOVE    ISAM-DATA           TO  TBL-PTKOHINF-REC(IDX-T)
               MOVE    ISAM-PTNUM          TO  TBL-ISAM-PTNUM (IDX-T)
               MOVE    ISAM-FTNKBN         TO  TBL-ISAM-FTNKBN (IDX-T)
               MOVE    ISAM-CNT            TO  TBL-ISAM-CNT    (IDX-T)
               MOVE    "1"                 TO  TBL-ISAM-SYORI  (IDX-T)
               MOVE    ISAM-PTID           TO  WRK-PTID
      *
               PERFORM 900-ISAM-NEXT-SEC
           END-PERFORM
      *
           IF      IDX-T            >   ZERO
               PERFORM 5001-PTKOHINF-SYORI-SEC
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
       5001-PTKOHINF-SYORI-SEC        SECTION.
      *
      *    老人公費の重複チェック
           IF      IDX-T            >   1
               PERFORM 50011-PTKOHINF-JUFUKU-SEC
           END-IF
      *    老人公費期間チェック
           IF      FLG-ERR              =   ZERO
               PERFORM 50014-KOH027-CHK-SEC
           END-IF
      *
      *    老人公費以外の保険番号期間チェック
           IF      FLG-ERR              =   ZERO
               PERFORM 50012-HKNNUM-CHK-SEC
           END-IF
      *    患者公費マスタ登録処理
           IF      FLG-ERR              =   ZERO
               PERFORM 5001-PTKOHINF-INS-SYORI-SEC
           END-IF
           IF      FLG-ERR         NOT =   ZERO
               PERFORM 290-ERRR-SYORI-SEC
           END-IF
           .
       5001-PTKOHINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費の重複チェック処理
      *****************************************************************
       50011-PTKOHINF-JUFUKU-SEC        SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20  )
                        OR   (TBL-ISAM-SYORI (IDX) =   SPACE )
      *        老人公費のみ行う
               IF     (TBL-PTKOH-KOHNUM (IDX)      =   "027" ) 
                   MOVE    TBL-PTKOH-TEKSTYMD(IDX) TO  WRK-HEN-TEKSTYMD
                   MOVE    TBL-PTKOH-TEKEDYMD(IDX) TO  WRK-HEN-TEKEDYMD
                   PERFORM 500211-JUFUKU-CHK-SEC
                   IF      FLG-JUFUKU          =   1
                       MOVE    2               TO  FLG-ERR
                       MOVE    "2"             TO  TBL-ISAM-ERR  (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       50011-PTKOHINF-JUFUKU-EXT.
           EXIT.
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       500211-JUFUKU-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JUFUKU
           PERFORM VARYING     IDX-C   FROM    IDX   BY  1
                   UNTIL       IDX-C   >   20
               IF      (IDX-C                  NOT =   IDX  )  AND
                       (TBL-PTKOH-KOHNUM (IDX-C)   =   "027" )
                   IF     (WRK-HEN-TEKEDYMD    <   TBL-PTKOH-TEKSTYMD
                                                              (IDX-C))
                      AND (WRK-HEN-TEKSTYMD    <   TBL-PTKOH-TEKEDYMD
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
             MOVE    TBL-PTKOHINF-REC (IDX)  TO  PTKOHINF-REC
             IF     (PTKOH-KOHNUM    NOT =   "027" )
               MOVE    ZERO                    TO  FLG-OK
               MOVE    ZERO                    TO  FLG-ARI
               MOVE    SPACE           TO  HZN-HKNNUM-AREA
               INITIALIZE                  HZN-HKNNUM-AREA
               PERFORM VARYING    IDH      FROM    1   BY  1
                       UNTIL     (IDH      >   IDX-HKN-MAX  )
                            OR   (FLG-OK   =   1   )
                   IF     (PTKOH-KOHNUM        =   TBL-HKN-HKNNUM (IDH))
                     AND  (PTKOH-PAYKBN        =   TBL-HKN-PAYKBN (IDH))
                       MOVE    1               TO  FLG-ARI
                       IF     (PTKOH-TEKSTYMD
                                                >=  TBL-HKN-TEKSTYMD
                                                             (IDH)) AND
                              (PTKOH-TEKEDYMD
                                                <=  TBL-HKN-TEKEDYMD
                                                             (IDH))
                           MOVE    1            TO  FLG-OK
                       ELSE
      *                終了日が対象の時、対象とする
                           IF      PTKOH-TEKEDYMD  <=  TBL-HKN-TEKEDYMD
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
      *        対象なし
               IF      FLG-ARI             =   ZERO
                   MOVE    3               TO  FLG-ERR
                   MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
               ELSE
                   IF      FLG-OK              =   ZERO
      *               期間変更
                       PERFORM 50013-HKNNUM-HENKAN-SEC
                   END-IF
               END-IF
             END-IF
           END-PERFORM
           .
       50012-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスタ期間による期間変更処理
      *****************************************************************
       50013-HKNNUM-HENKAN-SEC        SECTION.
      *
      *    保険変更があった時、終了日を変更する
           IF     (HZN-HKN-TEKSTYMD (2)    NOT =   SPACE )
              AND (HZN-HKN-TEKEDYMD (2)    >=  PTKOH-TEKSTYMD )
      *        新しい公費を追加する
               ADD     1                   TO  IDX-T
               MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC (IDX-T)
               MOVE    TBL-PTKOHINF-REC (IDX)
                                           TO  TBL-PTKOHINF-REC (IDX-T)
               MOVE    HZN-HKN-TEKSTYMD (1)
                                           TO  TBL-PTKOH-TEKSTYMD(IDX-T)
               MOVE    "A"                 TO  TBL-ISAM-ERR (IDX-T)
      *        終了日を変更する
               MOVE    HZN-HKN-TEKEDYMD (2)    TO  PTKOH-TEKEDYMD
      *        開始日が公費より前の時、開始日を保険の開始日にする
               IF      HZN-HKN-TEKSTYMD (2)    >   PTKOH-TEKSTYMD
                   MOVE    HZN-HKN-TEKSTYMD(2) TO  PTKOH-TEKSTYMD
               END-IF
               MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC (IDX)
           ELSE
               IF      HZN-HKN-TEKSTYMD(1) <   TBL-PTKOH-TEKEDYMD(IDX)
      *        保険の開始日を変更する
                   MOVE    HZN-HKN-TEKSTYMD(1) TO  TBL-PTKOH-TEKSTYMD
                                                                (IDX)
                   MOVE    "B"                 TO  TBL-ISAM-ERR (IDX)
               ELSE
                   MOVE    3               TO  FLG-ERR
                   MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
               END-IF
           END-IF
           .
       50013-HKNNUM-HENKAN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    老人公費期間チェック処理
      *****************************************************************
       50014-KOH027-CHK-SEC        SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20  )
                        OR   (TBL-ISAM-SYORI (IDX) =   SPACE )
               MOVE    TBL-PTKOHINF-REC (IDX)  TO  PTKOHINF-REC
               MOVE    TBL-ISAM-FTNKBN  (IDX)  TO  WRK-HEN-FTNKBN
      *
               IF     (TBL-PTKOH-KOHNUM (IDX)  =   "027" )
                   PERFORM 500141-KOH027-CHKSYORI-SEC
               END-IF
           END-PERFORM
           .
       50014-KOH027-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費期間チェック処理
      *****************************************************************
       500141-KOH027-CHKSYORI-SEC        SECTION.
      *
      *    老人公費は平成２０．３．３１で終了
           IF     (PTKOH-TEKSTYMD      <=  "20080331"  )  AND
                  (PTKOH-TEKEDYMD      >   "20080331"  )
               MOVE    "20080331"          TO  TBL-PTKOH-TEKEDYMD (IDX)
               MOVE    "20080331"          TO  PTKOH-TEKEDYMD
           END-IF
           EVALUATE    PTKOH-PAYKBN
      *
               WHEN    "00"
                   PERFORM 500142-PAYKBN00-SEC
      *
               WHEN    "01"
      *            保険番号期間チェック
                   PERFORM 50014-KOHNUM-CHEK02-SEC
                   IF      FLG-ARI         =   ZERO
      *                保険者マスタなし
                       MOVE    3               TO  FLG-ERR
                       MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
                   END-IF
      *
               WHEN    "02"
                   PERFORM 500142-PAYKBN02-SEC
      *
               WHEN    "03"
      *            保険番号期間チェック
                   PERFORM 50014-KOHNUM-CHEK02-SEC
                   IF      FLG-ARI         =   ZERO
      *                保険者マスタなし
                       MOVE    3               TO  FLG-ERR
                       MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
                   END-IF
           END-EVALUATE
           .
       500141-KOH027-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費支払区分＝００チェック処理
      *****************************************************************
       500142-PAYKBN00-SEC        SECTION.
      *
           IF      PTKOH-TEKSTYMD      <   "20010101"
      *        支払区分＝００ のチェック
               PERFORM 50014-KOHNUM-CHEK02-SEC
               IF      FLG-ARI         =   ZERO
                   IF      IDX-HKN         >   ZERO
                       MOVE    "20010101"          TO  WRK-HEN-TEKSTYMD
                       MOVE    "00"                TO  WRK-HEN-PAYKBN
                       PERFORM 5001421-KOHHENKOU-SEC
                       MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                              (IDX)
                   ELSE
      *                保険者マスタなし
                       MOVE    3               TO  FLG-ERR
                       MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
                   END-IF
               END-IF
           ELSE
      *        支払区分をシステム管理より設定
               MOVE    SPA-HOSPSBT         TO  PTKOH-PAYKBN (1:1) 
               MOVE    SPA-ROUPAYKBN       TO  PTKOH-PAYKBN (2:1)
               PERFORM 50014-KOHNUM-CHEK02-SEC
               IF      FLG-ARI             =   ZERO
                   IF      IDX-HKN             >   ZERO
                       MOVE    "20021001"          TO  WRK-HEN-TEKSTYMD
                       PERFORM 4002-PAYKBN-HEN-SEC
                       PERFORM 5001421-KOHHENKOU-SEC
                       MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                              (IDX)
                   ELSE
      *                保険者マスタなし
                       MOVE    3               TO  FLG-ERR
                       MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
                   END-IF
               ELSE
                   MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                              (IDX)
               END-IF
           END-IF
           .
       500142-PAYKBN00-EXT.
           EXIT.
      *****************************************************************
      *    老人公費支払区分００チェック処理
      *****************************************************************
       50014-KOHNUM-CHEK02-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-HKN
           PERFORM VARYING    IDH      FROM    1   BY  1
                   UNTIL     (IDH      >   IDX-HKN-MAX  )
                        OR   (FLG-ARI  =   1   )
               IF     (PTKOH-KOHNUM        =   TBL-HKN-HKNNUM (IDH))
                 AND  (PTKOH-PAYKBN        =   TBL-HKN-PAYKBN (IDH))
                   MOVE    IDH             TO  IDX-HKN
                   IF     (PTKOH-TEKSTYMD      >=  TBL-HKN-TEKSTYMD
                                                             (IDH)) AND
                          (PTKOH-TEKEDYMD      <=  TBL-HKN-TEKEDYMD
                                                             (IDH))
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-IF
           END-PERFORM
           .
       50014-KOHNUM-CHEK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費支払区分＝０２チェック処理
      *****************************************************************
       500142-PAYKBN02-SEC        SECTION.
      *
           PERFORM 50014-KOHNUM-CHEK02-SEC
           IF      FLG-ARI             =   1
      *        Ｈ１８．１０から期間変更
               IF      PTKOH-TEKEDYMD      >=  "20061001"
                   MOVE    "20060930"          TO  PTKOH-TEKEDYMD
      *            追加
                   ADD     1                   TO  IDX-T
                   MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC
                                                               (IDX-T)
                   MOVE    TBL-PTKOHINF-REC (IDX)
                                               TO  TBL-PTKOHINF-REC
                                                               (IDX-T)
                   MOVE    "20061001"          TO  TBL-PTKOH-TEKSTYMD
                                                              (IDX-T)
                   MOVE    "03"                TO  TBL-PTKOH-PAYKBN
                                                              (IDX-T)
      *
                   MOVE    PTKOHINF-REC        TO  TBL-PTKOHINF-REC
                                                              (IDX)
               END-IF
           ELSE
      *         保険者マスタなし
                MOVE    3               TO  FLG-ERR
                MOVE    "3"             TO  TBL-ISAM-ERR (IDX)
           END-IF
           .
       500142-PAYKBN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費支払区分＝００チェック処理
      *****************************************************************
       5001421-KOHHENKOU-SEC        SECTION.
      *
      *    終了日変更
           IF      PTKOH-TEKSTYMD      >=  TBL-HKN-TEKSTYMD (IDX-HKN)
               IF      PTKOH-TEKEDYMD      >   TBL-HKN-TEKEDYMD
                                                             (IDX-HKN)
                   MOVE    TBL-HKN-TEKEDYMD(IDX-HKN)
                                               TO  PTKOH-TEKEDYMD
      *            追加
                   ADD     1                   TO  IDX-T
                   MOVE    TBL-DATA-REC (IDX)  TO  TBL-DATA-REC
                                                               (IDX-T)
                   MOVE    TBL-PTKOHINF-REC (IDX)
                                               TO  TBL-PTKOHINF-REC
                                                               (IDX-T)
                   MOVE    WRK-HEN-TEKSTYMD    TO  TBL-PTKOH-TEKSTYMD
                                                              (IDX-T)
                   MOVE    WRK-HEN-PAYKBN      TO  TBL-PTKOH-PAYKBN
                                                              (IDX-T)
               END-IF
           ELSE
      *        開始日のみ変更
               MOVE    TBL-HKN-TEKSTYMD(IDX-HKN)
                                           TO  PTKOH-TEKSTYMD
           END-IF
           .
       5001421-KOHHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費支払い区分セット処理
      *****************************************************************
       4002-PAYKBN-HEN-SEC              SECTION.
      *
      *    支払い区分セット
           EVALUATE    WRK-HEN-FTNKBN
               WHEN    "1"
                   MOVE    "01"            TO  WRK-HEN-PAYKBN
               WHEN    "2"
               WHEN    "3"
      *            ２、３割負担は開始日による
                   IF      WRK-HEN-TEKSTYMD    >=  "20061001"
                       MOVE    "03"            TO  WRK-HEN-PAYKBN
                   ELSE
                       MOVE    "02"            TO  WRK-HEN-PAYKBN
                   END-IF
           END-EVALUATE
      *
           .
       4002-PAYKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険マスタ登録処理
      *****************************************************************
       5001-PTKOHINF-INS-SYORI-SEC        SECTION.
      *
           MOVE    TBL-PTKOH-PTID (1)  TO  SPA-PTID
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
               GO  TO  5001-PTKOHINF-INS-SYORI-EXT
           END-IF
      *    既に保険登録済みはエラー
           IF      PTNUM-KOHID         >   ZERO
               MOVE    4               TO  FLG-ERR
               GO  TO  5001-PTKOHINF-INS-SYORI-EXT
           END-IF
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1               TO  FLG-ERR
               GO  TO  5001-PTKOHINF-INS-SYORI-EXT
           END-IF
      *
      *    患者保険マスター登録
           MOVE    ZERO                TO  WRK-KOHID
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   20     )  OR
                             (TBL-ISAM-SYORI (IDX)  =   SPACE)
                        OR   (FLG-ERR      NOT =   ZERO )
               MOVE    TBL-PTKOHINF-REC (IDX)  TO  PTKOHINF-REC
               PERFORM 5009-PTKOHINF-INS-SEC
           END-PERFORM
      *    患者番号変換マスタ　更新処理
           IF      FLG-ERR             =   ZERO
               PERFORM 5010-PTNUM-UPDATE-SEC
           END-IF
      *
           .
       5001-PTKOHINF-INS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者保険マスター登録処理
      *****************************************************************
       5009-PTKOHINF-INS-SEC            SECTION.
      *
           ADD     1                   TO  WRK-KOHID
           MOVE    WRK-KOHID           TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    8               TO  FLG-ERR
               MOVE    "8"             TO  TBL-ISAM-ERR (IDX)
           ELSE
               ADD     1              TO  CNT-PTKOHINF
           END-IF
      *
           .
       5009-PTKOHINF-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       5010-PTNUM-UPDATE-SEC        SECTION.
      *
      *    最大保険ＩＤで患者番号変換マスタを更新
           IF      PTNUM-KOHID         <   WRK-KOHID
               MOVE    WRK-KOHID       TO  PTNUM-KOHID
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
      *       公費のもの
              IF       HKN-HKNKOHSBTKBN        =   "1"  OR  "4"
                                               OR  "8"  OR  "9"
                   CONTINUE
              ELSE
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
               MOVE    1                   TO  FLG-END
               DISPLAY "*(ORCVTPTKOHINF02)* TBL-HKNNUM NASI ERR"
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
           MOVE    "公費:"             TO  ERR2-KOHNUMMES
           MOVE    "区分:"             TO  ERR2-PAYKBNMES
           MOVE    "連番:"             TO  ERR2-KOHIDMES
           MOVE    "期間:"             TO  ERR2-TEKSTEDMES
      *
           MOVE    TBL-ISAM-PTNUM (1)  TO  ERR2-PTNUM
           MOVE    TBL-PTKOH-PTID (1)  TO  ERR2-PTID
      *
           EVALUATE    FLG-ERR
               WHEN    1
               WHEN    4
               WHEN    7
                   MOVE    "ERR"               TO  ERR2-NAIYO
                   MOVE    TBL-ISAM-CNT (1)    TO  ERR2-LINENO
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
                   MOVE    TBL-ISAM-CNT (IDX)      TO  ERR2-LINENO
                   MOVE    TBL-PTKOH-KOHNUM(IDX)   TO  ERR2-KOHNUM
                   IF      TBL-PTKOH-KOHNUM(IDX)   =   "027"
                       MOVE    TBL-PTKOH-PAYKBN(IDX)   TO  ERR2-PAYKBN
                   END-IF
                   MOVE    IDX                 TO  WRK-Z9
                   MOVE    WRK-Z9              TO  ERR2-KOHID
                   MOVE    TBL-PTKOH-TEKSTYMD (IDX)
                                               TO  ERR2-TEKSTYMD
                   MOVE    TBL-PTKOH-TEKEDYMD (IDX)
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
           MOVE    ERR-REC2            TO  ERR-REC
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
               DISPLAY "*(ORCVTPTKOHINF02)* ERRFILE OPN STS["
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
           DISPLAY "*(ORCVTPTKOHINF02)* IN      /I CNT[" CNT-ISAM "]"
           DISPLAY "*(ORCVTPTKOHINF02)* PTKOHINF/O CNT["
                                                     CNT-PTKOHINF "]"
           DISPLAY "*(ORCVTPTKOHINF02)* ERR     /O CNT["
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
       900-PTKOHINF-READ-SEC       SECTION.
      *
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_PTKOHINF"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_PTKOHINF"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_PTKOHINF"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタ読込
      *****************************************************************
       900-PTKOHINF-FETCH-SEC      SECTION.
      *
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
