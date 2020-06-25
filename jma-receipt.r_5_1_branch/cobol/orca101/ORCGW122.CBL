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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW122.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : レセプト印刷情報−公費記載順設定（Ｗ１２２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/03/25    NACL−藤原    新規作成
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
       DATA                    DIVISION.
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
       01  SPA-W122.
           03  SPA-GMN-PAGE        PIC 9(02).
           03  SPA-GMN-LINE        PIC 9(02).
           03  SPA-GMN-CUR         PIC 9(03).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-HEADLIST.
                   07  SPA-GMN-HEADLIST-TBL    OCCURS  10.
                       09  SPA-GMN-HEADLIST-TNO
                                               PIC 9(02).
                       09  SPA-GMN-HEADLIST-TSTYUKYMD
                                               PIC X(09).
                       09  SPA-GMN-HEADLIST-TEDYUKYMD
                                               PIC X(09).
                   07  SPA-GMN-HEADLIST-MAX    PIC 9(02).
      * 
               05  SPA-GMN-HEAD-KOUMOKU.
                   07  SPA-GMN-SELNUM1     PIC 9(02).
                   07  SPA-GMN-STYUKYMD    PIC X(10).
                   07  SPA-GMN-EDYUKYMD    PIC X(10).
      *
               05  SPA-GMN-BODYLIST.
                   07  SPA-GMN-BODYLIST-TBL    OCCURS  50.
                       09  SPA-GMN-BODYLIST-TGRP
                                               PIC 9(02).
                       09  SPA-GMN-BODYLIST-TKOHNUM
                                               PIC X(03).
                       09  SPA-GMN-BODYLIST-TSEIDONAME
                                               PIC X(50).
                    07  SPA-GMN-BODYLIST-MAX   PIC 9(02).
      *
               05  SPA-GMN-BODY-KOUMOKU.
                   07  SPA-GMN-GRP         PIC 9(02).
                   07  SPA-GMN-KOHNUM      PIC X(03).
                   07  SPA-GMN-SEIDONAME   PIC X(50).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-HEADLIST.
                   07  SPA-NAI-HEADLIST-TBL    OCCURS  10.
                       09  SPA-NAI-HEADLIST-TSTYUKYMD
                                               PIC 9(08).
                       09  SPA-NAI-HEADLIST-TEDYUKYMD
                                               PIC 9(08).
      *
               05  SPA-NAI-HEAD-KOUMOKU.
                   07  SPA-NAI-STYUKYMD    PIC 9(08).
                   07  SPA-NAI-EDYUKYMD    PIC 9(08).
      *
               05  SPA-NAI-BODY-KOUMOKU.
                   07  SPA-NAI-SELNUM2     PIC 9(02).
                   07  SPA-NAI-SELNUM2-ROW PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-HEADLIST-ROW    PIC 9(01).
           03  FLG-BODYLIST-ROW    PIC 9(01).
           03  FLG-UPDFLG          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
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
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-HKNNUM          PIC X(03).
      *
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-W122-HEADLIST-ROW
                                   PIC S9(09).
           03  WRK-W122-BODYLIST-ROW
                                   PIC S9(09).
      *
           03  WRK-GRP             PIC 9(02).
           03  WRK-GRP-9           PIC 9(02).
           03  WRK-GRP-X           REDEFINES   WRK-GRP-9
                                   PIC X(02).
      *
      *    公費編集用ワーク
           03  WRK-GMN-BODYLIST-AREA.
               05  WRK-GMN-BODYLIST.
                   07  WRK-GMN-BODYLIST-TBL    OCCURS  50.
                       09  WRK-GMN-BODYLIST-TGRP
                                               PIC 9(02).
                       09  WRK-GMN-BODYLIST-TKOHNUM
                                               PIC X(03).
                       09  WRK-GMN-BODYLIST-TSEIDONAME
                                               PIC X(50).
                    07  WRK-GMN-BODYLIST-MAX   PIC 9(02).
      *    公費チェック用ワーク
           03  WRK-CHK-KOHNUM-AREA.
               05  WRK-CHK-KOHNUM-OCC.
                   07  WRK-CHK-KOHNUM          PIC X(03)
                                                   OCCURS  50.
               05  WRK-CHK-KOHNUM-MAX          PIC 9(02).
               05  WRK-CHK-KOHNUM-MAX9         PIC 9(02).
           03  WRK-CHK1-KOHNUM-AREA.
               05  WRK-CHK1-KOHNUM-OCC.
                   07  WRK-CHK1-KOHNUM         PIC X(03)
                                                   OCCURS  50.
               05  WRK-CHK1-KOHNUM-MAX         PIC 9(02).
           03  WRK-CHK2-KOHNUM-AREA.
               05  WRK-CHK2-KOHNUM-OCC.
                   07  WRK-CHK2-KOHNUM         PIC X(03)
                                                   OCCURS  50.
               05  WRK-CHK2-KOHNUM-MAX         PIC 9(02).
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
       01  WRK-CONST-AREA.
           03  WRK-CONS-HEADLIST-MAX
                                   PIC 9(02)   VALUE   10.
           03  WRK-CONS-BODYLIST-MAX
                                   PIC 9(02)   VALUE   50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    公費記載順設定情報
           COPY    "CPSK2013.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W122
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W122        TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
           INITIALIZE              WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG      =   "WERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD   NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      * 
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "W122"          TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
      *            確認画面戻り処理
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面戻り処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                   WHEN    "1009"
                       PERFORM 490-KOUSHIN-SEC
                   WHEN    "1002"
                       PERFORM 490-CLEAR-SEC
               END-EVALUATE
           END-IF
      *
           IF      SPA-NAI-SELNUM2 >   ZERO
               MOVE    SPA-NAI-SELNUM2 TO  W122-BODYLIST-ROW
               MOVE    ZERO            TO  FLG-BODYLIST-ROW
           END-IF
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
           INITIALIZE              SPA-W122
      *
      *    レセプト公費記載順設定情報処理
           PERFORM 3101-SPA-HENSYU-SEC
      *
           MOVE    1               TO  FLG-HEADLIST-ROW
                                       FLG-BODYLIST-ROW
      *
           MOVE    1               TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト公費記載順設定情報処理
      *****************************************************************
       3101-SPA-HENSYU-SEC         SECTION.
      *
           INITIALIZE              SPA-GMN-HEADLIST
                                   SPA-NAI-HEADLIST
      *
           MOVE    SPACE           TO  SYS-2013-REC
           INITIALIZE                  SYS-2013-REC
           MOVE    SPA-HOSPNUM     TO  SYS-2013-HOSPNUM
           MOVE    "2013"          TO  SYS-2013-KANRICD
           MOVE    SYS-2013-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key12"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI
                                           =   1
                           OR      IDX     >=  WRK-CONS-HEADLIST-MAX
               MOVE    MCPDATA-REC     TO  SYS-2013-REC
      *
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-HEADLIST-TNO   (IDX)
               MOVE    SYS-2013-STYUKYMD
                                       TO
                                       SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
               MOVE    SYS-2013-STYUKYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO
                                       SPA-GMN-HEADLIST-TSTYUKYMD (IDX)
               MOVE    SYS-2013-EDYUKYMD
                                       TO
                                       SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
               MOVE    SYS-2013-EDYUKYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO
                                       SPA-GMN-HEADLIST-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key12"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-HEADLIST-MAX
      *
          MOVE    1                TO  SPA-GMN-CUR
           .
       3101-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.  
      * 
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 490-CLEAR-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    ↑
               WHEN    "CLICKED"       ALSO    "KOHNUM_UP"
                   PERFORM 490-KOHNUM-UP-SEC
      *    ↓
               WHEN    "CLICKED"       ALSO    "KOHNUM_DOWN"
                   PERFORM 490-KOHNUM-DOWN-SEC
      *    更新
               WHEN    "CLICKED"       ALSO    "KOHNUM_UPDATE"
                   PERFORM 490-KOHNUM-UPDATE-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "KOHNUM_DELETE"
                   PERFORM 490-KOHNUM-DELETE-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    ANY             ALSO    "HEADLIST"
                   PERFORM 41010-HEADLIST-SEL-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-SELNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "STYUKYMD"
                   PERFORM 41021-STYUKYMD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "EDYUKYMD"
                   PERFORM 41021-EDYUKYMD-CHK-SEC
      *
               WHEN    OTHER
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC        SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                       SPA-GMN-BODY-KOUMOKU
                                       SPA-GMN-BODYLIST
                                       SPA-NAI-HEAD-KOUMOKU
                                       SPA-NAI-BODY-KOUMOKU
      *
           MOVE    W122-SELNUM    TO  SPA-GMN-SELNUM1
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM1 =   ZERO
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           ELSE
               IF    ( SPA-GMN-SELNUM1 >=  1                    )
               AND   ( SPA-GMN-SELNUM1 <=  SPA-GMN-HEADLIST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE    
               PERFORM 41011-SELNUM1-HENSYU-SEC
           END-IF
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41011-SELNUM1-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-GMN-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-GMN-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                   TO  SPA-NAI-STYUKYMD
           MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                   TO  SPA-NAI-EDYUKYMD
      *
           MOVE    SPACE           TO  SYS-2013-REC
           INITIALIZE                  SYS-2013-REC
           MOVE    SPA-HOSPNUM     TO  SYS-2013-HOSPNUM
           MOVE    "2013"          TO  SYS-2013-KANRICD
           MOVE    SPA-NAI-STYUKYMD
                                   TO  SYS-2013-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD
                                   TO  SYS-2013-EDYUKYMD
           MOVE    SYS-2013-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDZ
           PERFORM     UNTIL   FLG-SYSKANRI
                                       =   1
                       OR      IDZ     >=  WRK-CONS-BODYLIST-MAX
               MOVE    MCPDATA-REC     TO  SYS-2013-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   SYS-2013-KOHNUM (IDX)
                                           =       SPACE
                       OR      IDZ         >=  WRK-CONS-BODYLIST-MAX
                   ADD     1               TO  IDZ
                   MOVE    SYS-2013-KBNCD (1:2)
                                           TO  WRK-GRP-X
                   MOVE    WRK-GRP-9       TO  SPA-GMN-BODYLIST-TGRP
                                                                   (IDZ)
                   MOVE    SYS-2013-KOHNUM (IDX)
                                           TO  
                                           SPA-GMN-BODYLIST-TKOHNUM(IDZ)
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
                   MOVE    SYS-2013-KOHNUM (IDX)
                                           TO  HKN-HKNNUM
                   MOVE    "00"            TO  HKN-PAYKBN
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-READ-SEC
                   MOVE    HKN-TANSEIDONAME
                                           TO
                                           SPA-GMN-BODYLIST-TSEIDONAME
                                                                   (IDZ)
               END-PERFORM
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDZ             TO  SPA-GMN-BODYLIST-MAX
      *
           MOVE    SPA-GMN-SELNUM1 TO  W122-HEADLIST-ROW
           MOVE    ZERO            TO  FLG-HEADLIST-ROW
      *
           MOVE    4               TO  SPA-GMN-CUR
      *
           .
       41011-SELNUM1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41010-HEADLIST-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-HEADLIST-MAX
               IF      W122-HEADLIST-SELECT (IDX)  =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM1
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-HEADLIST-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                       SPA-GMN-BODY-KOUMOKU
                                       SPA-GMN-BODYLIST
                                       SPA-NAI-HEAD-KOUMOKU
                                       SPA-NAI-BODY-KOUMOKU
      *
           IF      WRK-SELNUM         >   ZERO
               MOVE    WRK-SELNUM         TO  SPA-GMN-SELNUM1
           END-IF
      *
           PERFORM 41011-SELNUM1-HENSYU-SEC
           .
       41010-HEADLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    ANY             ALSO    "BODYLIST"
                   PERFORM 41011-BODYLIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41011-BODYLIST-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-BODYLIST-MAX
               IF      W122-BODYLIST-SELECT (IDX)  =   "T"
                   IF      IDX             =   SPA-NAI-SELNUM2
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-BODYLIST-MAX
                                               TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               INITIALIZE              SPA-NAI-SELNUM2-ROW
               MOVE    WRK-SELNUM      TO  SPA-NAI-SELNUM2
           END-IF
      *
           PERFORM 41021-SELNUM2-HENSYU-SEC
      *
           .
       41011-BODYLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41021-SELNUM2-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-GMN-BODYLIST-TGRP       (SPA-NAI-SELNUM2)
                                   TO  SPA-GMN-GRP
           MOVE    SPA-GMN-BODYLIST-TKOHNUM    (SPA-NAI-SELNUM2)
                                   TO  SPA-GMN-KOHNUM
           MOVE    SPA-GMN-BODYLIST-TSEIDONAME (SPA-NAI-SELNUM2)
                                   TO  SPA-GMN-SEIDONAME
      *
           MOVE    SPA-NAI-SELNUM2 TO  W122-BODYLIST-ROW
           MOVE    ZERO            TO  FLG-BODYLIST-ROW
      *
           MOVE    5               TO  SPA-GMN-CUR
           .
       41021-SELNUM2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
      *****IF      SPA-ERRCD       =   SPACE
      *****    関連チェック
      *****    PERFORM 4103-KANREN-CHK-SEC
      *****END-IF
      *
           IF      SPA-NAI-SELNUM2     >   ZERO
               MOVE    SPA-NAI-SELNUM2     TO  W122-BODYLIST-ROW
               MOVE    ZERO                TO  FLG-BODYLIST-ROW
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W122-GRP-NO         TO  SPA-GMN-GRP
           MOVE    W122-KOHNUM         TO  SPA-GMN-KOHNUM
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
           IF    ( SPA-NAI-STYUKYMD    =   ZERO )
           AND   ( SPA-NAI-EDYUKYMD    =   ZERO )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
               
      *
           PERFORM 41021-GRP-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41021-KOHNUM-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-GRP     =   ZERO  )
           AND   ( SPA-GMN-KOHNUM
                               NOT =   SPACE )
               MOVE    "0010"          TO  SPA-ERRCD
               GO  TO  4103-KANREN-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-KOHNUM  =   SPACE )
           OR    ( SPA-GMN-SEIDONAME
                                   =   SPACE )
           OR    ( SPA-GMN-GRP     =   ZERO  )
               MOVE    "0103"          TO  SPA-ERRCD
               GO  TO  4103-KANREN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
               IF    ( SPA-NAI-SELNUM2 >   ZERO )
               AND   ( SPA-NAI-SELNUM2 =   IDX  )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-KOHNUM  =
                                       SPA-GMN-BODYLIST-TKOHNUM (IDX) )
                   AND   ( SPA-GMN-GRP     =
                                       SPA-GMN-BODYLIST-TGRP    (IDX) )
                       MOVE    "0009"          TO  SPA-ERRCD
                       GO  TO  4103-KANREN-CHK-EXT
                   END-IF
               END-IF    
           END-PERFORM
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC      SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-STYUKYMD
           MOVE    W122-STYUKYMD       TO  SPA-GMN-STYUKYMD 
      *
           IF    ( SPA-GMN-STYUKYMD    =   SPACE )
           AND   ( SPA-GMN-HEADLIST-MAX
                                       =   ZERO  )
               MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               MOVE    99999999           TO  SPA-NAI-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
      *        開始日チェック
               IF      SPA-NAI-STYUKYMD    NOT =   ZERO
                   IF      SPA-NAI-STYUKYMD (7:2)  =   "01"
                       CONTINUE
                   ELSE
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF   
           .
       41021-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-EDYUKYMD
           MOVE    W122-EDYUKYMD       TO  SPA-GMN-EDYUKYMD
      *
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
               MOVE    99999999            TO  SPA-NAI-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *        末日チェック
               IF      SPA-NAI-EDYUKYMD
                                       NOT =   99999999
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-EDYUKYMD (1:6)
                                               TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                   IF      LNK-DAY7-KOYOMI NOT =   SPA-NAI-EDYUKYMD
                       MOVE    "0007"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    開始日との関連チェック
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-STYUKYMD    NOT =   SPACE )
               AND   ( SPA-GMN-EDYUKYMD    NOT =   SPACE )
                   IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    有効期間の重複チェック
           IF      SPA-ERRCD       =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-HEADLIST-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF      SPA-GMN-SELNUM1     =   IDX
      *                選択された内容の開始日が変更された場合
                       MOVE    ZERO            TO  FLG-CHK
                   ELSE
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF      FLG-CHK             =   1
                       IF    ( SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
                                               <   SPA-NAI-STYUKYMD )
                       OR    ( SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                                               >   SPA-NAI-EDYUKYMD )
                           CONTINUE
                       ELSE
                           MOVE    "0004"              TO  SPA-ERRCD
                           MOVE    2                   TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ番号チェック処理
      *****************************************************************
       41021-GRP-CHK-SEC           SECTION.
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-GRP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       41021-KOHNUM-CHK-SEC        SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-SEIDONAME
      *
           IF      SPA-GMN-KOHNUM  =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE           TO  HKNNUM-REC
               INITIALIZE                  HKNNUM-REC
               MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
               MOVE    SPA-GMN-KOHNUM  TO  HKN-HKNNUM
               MOVE    "00"            TO  HKN-PAYKBN
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-READ-SEC
               IF      FLG-HKNNUM      =   ZERO
                   MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-SEIDONAME
      *            保険公費種別区分が公費かどうか
                   IF      HKN-HKNKOHSBTKBN
                                           =   "5" OR  "6" OR  "7"
      *                レセプト記載を行うか
      *****************IF      HKN-RECESKYKBN  =   "3"
      *****************    MOVE    "0012"          TO  SPA-ERRCD
      *****************END-IF
                       CONTINUE
                   ELSE
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0013"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-KOHNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W12"           TO  SPA-SAKIPG
           MOVE    "W122"          TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費↑処理
      *****************************************************************
       490-KOHNUM-UP-SEC           SECTION.
      *
           IF      SPA-NAI-SELNUM2 =   ZERO
               MOVE    "0103"          TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM2     =   1 )
               OR    ( SPA-GMN-BODYLIST-MAX
                                           =   1 )
                   GO  TO  490-KOHNUM-UP-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-SELNUM2     -   1
      *        上のグループと違うときの処理
               IF      SPA-GMN-BODYLIST-TGRP (SPA-NAI-SELNUM2)
                                       NOT =   SPA-GMN-BODYLIST-TGRP
                                                               (IDX)
                   MOVE    "0039"      TO  SPA-ERRCD
                   GO  TO  490-KOHNUM-UP-EXT
               ELSE
      *            グループが同じときの処理
                   MOVE    SPA-GMN-BODYLIST-TBL (SPA-NAI-SELNUM2)
                                           TO  WRK-GMN-BODYLIST-TBL (1)
                   MOVE    SPA-GMN-BODYLIST-TBL (IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (SPA-NAI-SELNUM2)
                   MOVE    WRK-GMN-BODYLIST-TBL (1)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (IDX)
                   MOVE    IDX             TO  SPA-NAI-SELNUM2
                                               W122-BODYLIST-ROW
               END-IF
           END-IF
           .
       490-KOHNUM-UP-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費↓処理
      *****************************************************************
       490-KOHNUM-DOWN-SEC              SECTION.
      *
           IF      SPA-NAI-SELNUM2     =   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-SELNUM2     =   SPA-GMN-BODYLIST-MAX )
               OR    ( SPA-GMN-BODYLIST-MAX
                                           =   1                    )
                   GO  TO  490-KOHNUM-DOWN-EXT
               END-IF
           END-IF
      *    
           IF      SPA-ERRCD           =   SPACE
               COMPUTE IDX =   SPA-NAI-SELNUM2     +   1
      *        下のグループと違うときの処理
               IF      SPA-GMN-BODYLIST-TGRP (SPA-NAI-SELNUM2)
                                       NOT =   SPA-GMN-BODYLIST-TGRP
                                                               (IDX)
                   MOVE    "0039"      TO  SPA-ERRCD
                               GO  TO  490-KOHNUM-DOWN-EXT
               ELSE
      *            グループが同じときの処理
                   MOVE    SPA-GMN-BODYLIST-TBL (SPA-NAI-SELNUM2)
                                           TO  WRK-GMN-BODYLIST-TBL (1)
                   MOVE    SPA-GMN-BODYLIST-TBL (IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (SPA-NAI-SELNUM2)
                   MOVE    WRK-GMN-BODYLIST-TBL (1)
                                           TO  SPA-GMN-BODYLIST-TBL
                                                       (IDX)
                   MOVE    IDX             TO  SPA-NAI-SELNUM2
                                               W122-BODYLIST-ROW
               END-IF
           END-IF
           .
       490-KOHNUM-DOWN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加編集処理
      *****************************************************************
       490-KOHNUM-UPDATE-SEC          SECTION.
      *
           MOVE    4               TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KOHNUM-UPDATE-EXT
           END-IF
      *
      *    関連チェック
           PERFORM 4103-KANREN-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KOHNUM-UPDATE-EXT
           END-IF
      *
           IF      SPA-NAI-SELNUM2 =   ZERO
               IF      SPA-GMN-BODYLIST-MAX
                                       >=  WRK-CONS-BODYLIST-MAX
      *            追加できないとき
                   MOVE    "0105"          TO  SPA-ERRCD
                   INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                               SPA-NAI-BODY-KOUMOKU
                   GO  TO  490-KOHNUM-UPDATE-EXT
               ELSE
      *            公費追加編集処理
                   PERFORM 4901-KOHNUM-HENSYU-SEC
               END-IF
           ELSE
               IF    ( SPA-GMN-BODYLIST-TKOHNUM (SPA-NAI-SELNUM2)
                                       =   SPA-GMN-KOHNUM )
               AND   ( SPA-GMN-BODYLIST-TGRP    (SPA-NAI-SELNUM2)
                                       =   SPA-GMN-GRP    )
      *            選択した項目に変更がないとき
                   INITIALIZE              SPA-GMN-BODY-KOUMOKU
                                           SPA-NAI-BODY-KOUMOKU
                   GO  TO  490-KOHNUM-UPDATE-EXT
               ELSE
      *********    MOVE    SPA-GMN-GRP     TO
      *********                SPA-GMN-BODYLIST-TGRP   (SPA-NAI-SELNUM2)
                   MOVE    SPA-GMN-KOHNUM  TO
                               SPA-GMN-BODYLIST-TKOHNUM(SPA-NAI-SELNUM2)
                   MOVE    SPA-GMN-SEIDONAME
                                           TO
                               SPA-GMN-BODYLIST-TSEIDONAME
                                                       (SPA-NAI-SELNUM2)
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                       SPA-NAI-BODY-KOUMOKU
           .
       490-KOHNUM-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *       公費追加編集処理
      *****************************************************************
       4901-KOHNUM-HENSYU-SEC          SECTION.
      *
           INITIALIZE              WRK-GMN-BODYLIST-AREA
      *
           MOVE    SPA-GMN-BODYLIST
                                   TO  WRK-GMN-BODYLIST
           INITIALIZE                  SPA-GMN-BODYLIST
     *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-GMN-BODYLIST-MAX
               ADD     1               TO  SPA-GMN-BODYLIST-MAX
      *
      *        設定したグループの最後に追加
               IF    ( WRK-GMN-BODYLIST-TGRP (IDX)
                                       >   SPA-GMN-GRP )
               AND   ( SPA-GMN-GRP     >   ZERO        )
                   MOVE    SPA-GMN-GRP     TO
                               SPA-GMN-BODYLIST-TGRP
                                               (SPA-GMN-BODYLIST-MAX)
                   MOVE    SPA-GMN-KOHNUM  TO
                               SPA-GMN-BODYLIST-TKOHNUM
                                               (SPA-GMN-BODYLIST-MAX)
                   MOVE    SPA-GMN-SEIDONAME
                                           TO
                               SPA-GMN-BODYLIST-TSEIDONAME
                                               (SPA-GMN-BODYLIST-MAX)
       *
                   INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                               SPA-NAI-BODY-KOUMOKU
                   MOVE    SPA-GMN-BODYLIST-MAX
                                           TO  W122-BODYLIST-ROW
       *
                   ADD     1               TO  SPA-GMN-BODYLIST-MAX
                   MOVE    WRK-GMN-BODYLIST-TBL (IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                                               (SPA-GMN-BODYLIST-MAX)
               ELSE
                   MOVE    WRK-GMN-BODYLIST-TBL (IDX)
                                           TO  SPA-GMN-BODYLIST-TBL
                                               (SPA-GMN-BODYLIST-MAX)
               END-IF
           END-PERFORM
      *
      *    末尾に追加
           IF      SPA-GMN-GRP     >   ZERO
               ADD     1               TO  SPA-GMN-BODYLIST-MAX
      *
               MOVE    SPA-GMN-GRP     TO  SPA-GMN-BODYLIST-TGRP
                                               (SPA-GMN-BODYLIST-MAX)
               MOVE    SPA-GMN-KOHNUM  TO  SPA-GMN-BODYLIST-TKOHNUM
                                               (SPA-GMN-BODYLIST-MAX)
               MOVE    SPA-GMN-SEIDONAME
                                       TO  SPA-GMN-BODYLIST-TSEIDONAME
                                               (SPA-GMN-BODYLIST-MAX)
       *
               INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                           SPA-NAI-BODY-KOUMOKU
               MOVE    SPA-GMN-BODYLIST-MAX
                                       TO  W122-BODYLIST-ROW
           END-IF
      *
           MOVE    ZERO            TO  FLG-BODYLIST-ROW
           .
       4901-KOHNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費削除処理
      *****************************************************************
       490-KOHNUM-DELETE-SEC          SECTION.
      *
           MOVE    4               TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KOHNUM-UPDATE-EXT
           END-IF
      *
      *    関連チェック
      **** PERFORM 4103-KANREN-CHK-SEC
      **** IF      SPA-ERRCD   NOT =   SPACE
      ****     GO  TO  490-KOHNUM-UPDATE-EXT
      **** END-IF
      *
           IF      SPA-NAI-SELNUM2     =   ZERO
      *****    MOVE    "0044"              TO  SPA-ERRCD
               GO  TO  490-KOHNUM-DELETE-EXT
           END-IF
      *  
           PERFORM VARYING IDX FROM    SPA-NAI-SELNUM2 BY  1
                   UNTIL   IDX >       SPA-GMN-BODYLIST-MAX
               IF      IDX             =   SPA-GMN-BODYLIST-MAX
                   INITIALIZE                  SPA-GMN-BODYLIST-TBL
                                                                (IDX)
               ELSE
                   COMPUTE IDY     =   IDX     +   1
                   MOVE    SPA-GMN-BODYLIST-TBL (IDY)
                                       TO  SPA-GMN-BODYLIST-TBL (IDX)
               END-IF
           END-PERFORM
           COMPUTE SPA-GMN-BODYLIST-MAX    =   SPA-GMN-BODYLIST-MAX
                                           -   1
      *
           INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                       SPA-NAI-BODY-KOUMOKU
           .
       490-KOHNUM-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *       クリア確認処理
      *****************************************************************
       490-CLEAR-GID-SEC              SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
           .
       490-CLEAR-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *       クリア処理
      *****************************************************************
       490-CLEAR-SEC              SECTION.
      *
           IF    ( SPA-GMN-GRP     >   ZERO  )
           OR    ( SPA-GMN-KOHNUM
                               NOT =   SPACE )
               INITIALIZE                  SPA-GMN-BODY-KOUMOKU
                                           SPA-NAI-BODY-KOUMOKU
               MOVE    1                   TO  FLG-BODYLIST-ROW
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               INITIALIZE                  SPA-GMN-HEAD-KOUMOKU
                                           SPA-NAI-HEAD-KOUMOKU
                                           SPA-GMN-BODYLIST
               MOVE    1                   TO  FLG-HEADLIST-ROW
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       490-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      490-TOUROKU-EXT
           END-IF
      *
      *    登録前関連チェック処理
           PERFORM 4900-KANRENCHK-SEC
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    有効期間の未入力チェック
           IF      SPA-NAI-STYUKYMD    =   ZERO
               IF      SPA-GMN-STYUKYMD    =   "00000000"
                   CONTINUE
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
           IF      SPA-NAI-EDYUKYMD    =   ZERO
               IF      SPA-GMN-EDYUKYMD    =   SPACE
                   MOVE    99999999            TO  SPA-NAI-EDYUKYMD
                   MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    3                   TO  SPA-GMN-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    有効期間の重複チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-HEADLIST-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-GMN-SELNUM1     =   IDX
      *            選択された内容の開始日が変更された場合
      ***             IF      SPA-NAI-STYUKYMD    =
      ***                             SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                       MOVE    ZERO            TO  FLG-CHK
      ***             ELSE
      ***                 MOVE    1               TO  FLG-CHK
      ***             END-IF
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF      FLG-CHK             =   1
                   IF    ( SPA-NAI-HEADLIST-TEDYUKYMD (IDX)
                                           <   SPA-NAI-STYUKYMD )
                   OR    ( SPA-NAI-HEADLIST-TSTYUKYMD (IDX)
                                           >   SPA-NAI-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    別グループとの重複チェック
           INITIALIZE              WRK-CHK-KOHNUM-AREA
                                   WRK-CHK1-KOHNUM-AREA
                                   WRK-CHK2-KOHNUM-AREA
           MOVE    1               TO  IDX
      *
           PERFORM     UNTIL   IDX     >   SPA-GMN-BODYLIST-MAX
                       OR      SPA-ERRCD
                                   NOT =   SPACE
               INITIALIZE              WRK-CHK-KOHNUM-AREA
                                       WRK-CHK2-KOHNUM-AREA
      *
      *        チェック元のグループ情報
               PERFORM VARYING IDY     FROM    IDX BY  1
                       UNTIL   IDY     >       SPA-GMN-BODYLIST-MAX
                       OR      SPA-GMN-BODYLIST-TGRP (IDX)
                                   NOT =   SPA-GMN-BODYLIST-TGRP (IDY)
                   ADD     1               TO  WRK-CHK-KOHNUM-MAX
                   IF      SPA-GMN-BODYLIST-TKOHNUM (IDY) (1:1)
                                           =   "0" OR  "9"
                       ADD     1               TO  WRK-CHK-KOHNUM-MAX9
                   END-IF    
                   MOVE    SPA-GMN-BODYLIST-TKOHNUM (IDY)
                                           TO  WRK-CHK-KOHNUM
                                                  (WRK-CHK-KOHNUM-MAX)
      *
                   MOVE    IDY             TO  IDXX
                                               IDYY
               END-PERFORM
      *        グループ内の公費が１件のときの警告チェック
               IF      WRK-CHK-KOHNUM-MAX  =   1
                   MOVE    "1009"              TO  SPA-WIDCD
               END-IF
      *        グループ内の公費が全て全国公費のとき
               IF      WRK-CHK-KOHNUM-MAX  =   WRK-CHK-KOHNUM-MAX9
                   MOVE    "0045"              TO  SPA-ERRCD
               END-IF
      *
               PERFORM     UNTIL   IDXX    >=  SPA-GMN-BODYLIST-MAX
                           OR      SPA-ERRCD
                                       NOT =   SPACE
                   COMPUTE IDXX    =   IDXX    +   1
                   MOVE    WRK-CHK-KOHNUM-AREA
                                       TO  WRK-CHK2-KOHNUM-AREA
                   INITIALIZE              WRK-CHK1-KOHNUM-AREA
      *            チェック先のグループ情報
                   PERFORM VARYING IDY     FROM    IDXX    BY  1
                           UNTIL   IDY     >       SPA-GMN-BODYLIST-MAX
                           OR      SPA-GMN-BODYLIST-TGRP (IDXX)
                                   NOT =   SPA-GMN-BODYLIST-TGRP (IDY)
                       ADD     1               TO  WRK-CHK1-KOHNUM-MAX
                       MOVE    SPA-GMN-BODYLIST-TKOHNUM(IDY)
                                               TO  WRK-CHK1-KOHNUM
                                                   (WRK-CHK1-KOHNUM-MAX)
      *
                       MOVE    IDY             TO  IDXX
                   END-PERFORM
                   IF      WRK-CHK1-KOHNUM-MAX =   1
                       MOVE    "1009"              TO  SPA-WIDCD
                   END-IF
      *
      *            重複チェック             
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       WRK-CHK2-KOHNUM-MAX
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       WRK-CHK1-KOHNUM-MAX
                           IF      WRK-CHK2-KOHNUM (IDY)
                                           =   WRK-CHK1-KOHNUM (IDZ)
                               MOVE    SPACE       TO
                                               WRK-CHK2-KOHNUM (IDY)
                               MOVE    SPACE       TO
                                               WRK-CHK1-KOHNUM (IDZ)
                               MOVE    WRK-CHK1-KOHNUM-MAX
                                                   TO  IDZ
                           END-IF
                       END-PERFORM
                   END-PERFORM
      *
      *            全て重複したとき
                   IF    ( WRK-CHK1-KOHNUM-OCC     =   SPACE )
                   OR    ( WRK-CHK2-KOHNUM-OCC     =   SPACE )
                       MOVE    "0043"              TO  SPA-ERRCD
      *****            GO  TO  4900-KANRENCHK-EXT
                   END-IF
               END-PERFORM
      *
               COMPUTE IDX     =   IDYY    +   1
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-WIDCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF      SPA-WIDCD       =   SPACE     
               MOVE    "1001"              TO  SPA-WIDCD
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-KOUSHIN-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    元のデータを削除
           IF      SPA-GMN-SELNUM1 >   ZERO
               MOVE    SPACE           TO  SYS-2013-REC
               INITIALIZE                  SYS-2013-REC
               MOVE    SPA-HOSPNUM     TO  SYS-2013-HOSPNUM
               MOVE    "2013"          TO  SYS-2013-KANRICD
               MOVE    SPA-NAI-HEADLIST-TSTYUKYMD (SPA-GMN-SELNUM1)
                                       TO  SYS-2013-STYUKYMD
               MOVE    SPA-NAI-HEADLIST-TEDYUKYMD (SPA-GMN-SELNUM1)
                                       TO  SYS-2013-EDYUKYMD
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "del11"         TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    SYS-2013-REC    TO  MCPDATA-REC
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "1003"          TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-IF
      * 
           IF      SPA-GMN-BODYLIST-MAX
                                   >   ZERO
               MOVE    ZERO            TO  WRK-GRP
               MOVE    1               TO  IDXX
               PERFORM UNTIL   IDXX    >   SPA-GMN-BODYLIST-MAX
                   MOVE    SPA-GMN-BODYLIST-TGRP (IDXX)
                                           TO  WRK-GRP
                   MOVE    ZERO            TO  IDYY
      * 
                   MOVE    SPACE           TO  SYS-2013-REC
                   INITIALIZE                  SYS-2013-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-2013-HOSPNUM
                   MOVE    "2013"          TO  SYS-2013-KANRICD
                   MOVE    SPA-GMN-BODYLIST-TGRP (IDXX)
                                           TO  WRK-GRP-9
                   MOVE    WRK-GRP-X       TO  SYS-2013-KBNCD
                   MOVE    SPA-NAI-STYUKYMD
                                           TO  SYS-2013-STYUKYMD
                   MOVE    SPA-NAI-EDYUKYMD
                                           TO  SYS-2013-EDYUKYMD
                   MOVE    ZERO            TO  IDZZ
      *            グループ別公費設定
                   PERFORM UNTIL   IDXX    >   SPA-GMN-BODYLIST-MAX
                           OR      SPA-GMN-BODYLIST-TGRP (IDXX)
                                       NOT =   WRK-GRP
                       ADD     1               TO  IDZZ
                       MOVE    SPA-GMN-BODYLIST-TKOHNUM (IDXX)
                                               TO  SYS-2013-KOHNUM(IDZZ)
      *
                       ADD     1               TO  IDXX
                   END-PERFORM
      *
      *            更新処理
                   MOVE    SPA-TERMID      TO  SYS-2013-TERMID
                   MOVE    SPA-OPID        TO  SYS-2013-OPID
                   MOVE    SMCNDATE-YMD    TO  SYS-2013-CREYMD
                                               SYS-2013-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-2013-UPHMS
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    SYS-2013-REC    TO  MCPDATA-REC
                   CALL    "ORCDBMAIN"     USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "1001"          TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 490-CLEAR-SEC
      *
      *        管理部一覧編集処理
               PERFORM 3101-SPA-HENSYU-SEC
           END-IF    
           .
       490-KOUSHIN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W122"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-HEADLIST-ROW
                                   =   1
               MOVE    ZERO            TO  WRK-W122-HEADLIST-ROW
           ELSE
               MOVE    W122-HEADLIST-ROW
                                       TO  WRK-W122-HEADLIST-ROW
           END-IF
      **    IF      FLG-BODYLIST-ROW =   1
      **         MOVE    ZERO            TO  WRK-W122-BODYLIST-ROW
      **     ELSE
               MOVE    W122-BODYLIST-ROW
                                       TO  WRK-W122-BODYLIST-ROW
      **     END-IF
      *
           INITIALIZE                  W122
      *
           MOVE    SPA-GMN-SELNUM1 TO  W122-SELNUM
           MOVE    SPA-GMN-STYUKYMD
                                   TO  W122-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD
                                   TO  W122-EDYUKYMD
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-HEADLIST-MAX
               MOVE    SPA-GMN-HEADLIST-TNO  (IDX) 
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  W122-HEADLIST-TNO  (IDX)
               MOVE    SPA-GMN-HEADLIST-TSTYUKYMD   (IDX)
                                       TO  W122-HEADLIST-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-HEADLIST-TEDYUKYMD(IDX)
                                       TO  W122-HEADLIST-TEDYUKYMD (IDX)
      *
               IF      IDX             =   SPA-GMN-SELNUM1
                   MOVE    "T"         TO  W122-HEADLIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  W122-HEADLIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-HEADLIST-MAX
                                       TO  W122-HEADLIST-COUNT
           MOVE    ZERO                TO  W122-HEADLIST-ROW
           IF      WRK-W122-HEADLIST-ROW   >   ZERO
               MOVE    WRK-W122-HEADLIST-ROW
                                           TO  W122-HEADLIST-ROW
           END-IF
      *
           MOVE    SPA-GMN-GRP     TO  W122-GRP-NO
           MOVE    SPA-GMN-KOHNUM  TO  W122-KOHNUM
           MOVE    SPA-GMN-SEIDONAME
                                   TO  W122-SEIDONAME
      *
           MOVE    ZERO            TO  WRK-GRP         
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-BODYLIST-MAX
               IF      WRK-GRP     NOT =   SPA-GMN-BODYLIST-TGRP (IDX)
                   MOVE    SPA-GMN-BODYLIST-TGRP       (IDX)
                                           TO  WRK-GRP
                                               WRK-Z9
                   MOVE    WRK-Z9          TO  W122-BODYLIST-TGRP  (IDX)
               END-IF
               MOVE    SPA-GMN-BODYLIST-TKOHNUM    (IDX)
                                       TO  W122-BODYLIST-TKOHNUM   (IDX)
               MOVE    SPA-GMN-BODYLIST-TSEIDONAME (IDX)
                                       TO  W122-BODYLIST-TSEIDONAME(IDX)
      *
               IF      IDX             =   SPA-NAI-SELNUM2
                   MOVE    "T"             TO  W122-BODYLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W122-BODYLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-BODYLIST-MAX
                                       TO  W122-BODYLIST-COUNT
           MOVE    ZERO                TO  W122-BODYLIST-ROW
           IF      WRK-W122-BODYLIST-ROW
                                       >   ZERO
               MOVE    WRK-W122-BODYLIST-ROW
                                           TO  W122-BODYLIST-ROW
               MOVE    2                   TO  W122-BODYLIST-ROWATTR
           END-IF
      *
           IF      SPA-NAI-SELNUM2 =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  W122-KOHNUM-UP-STATE
                                               W122-KOHNUM-DOWN-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W122-KOHNUM-UP-STATE
                                               W122-KOHNUM-DOWN-STATE
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
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません"     TO  WERR-ERRMSG
               WHEN    "0002"
                   MOVE    "有効期間の入力がありません"
                                                      TO  WERR-ERRMSG
               WHEN    "0003"
                   MOVE    "エラーがある項目があります"
                                                      TO  WERR-ERRMSG
               WHEN    "0004"
                   MOVE    "有効期間が重複します"     TO  WERR-ERRMSG
               WHEN    "0005"
                   MOVE    "日付入力エラー"           TO  WERR-ERRMSG
               WHEN    "0006"
                   MOVE    "１日で入力して下さい"     TO  WERR-ERRMSG
               WHEN    "0007"
                   MOVE    "末日で入力して下さい"     TO  WERR-ERRMSG
               WHEN    "0008"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                                      TO  WERR-ERRMSG
               WHEN    "0009"
                   MOVE    "既に登録してあります"
                                           TO  WERR-ERRMSG
               WHEN    "0010"
                   MOVE    "グループ番号が未入力です"
                                           TO  WERR-ERRMSG
               WHEN    "0011"
                   MOVE    "入力できない公費です"
                                           TO  WERR-ERRMSG
               WHEN    "0012"
                   MOVE    "レセプト請求する地方公費のみ入力できます"
                                           TO  WERR-ERRMSG
               WHEN    "0013"
                   MOVE    "公費番号入力エラー"
                                           TO  WERR-ERRMSG
               WHEN    "0039"
                   MOVE    "グループが異なるので移動はできません"
                                           TO  WERR-ERRMSG
               WHEN    "0043"
                   MOVE    "同じ設定が別のグループで登録されています"
                                           TO  WERR-ERRMSG
               WHEN    "0044"
                   MOVE    "対象の公費が選択されていません"
                                           TO  WERR-ERRMSG
               WHEN    "0045"
                   MOVE    "全国公費のみ設定されたグループがあります"
                                           TO  WERR-ERRMSG
               WHEN    "0101"
                   MOVE
                       "選択された編綴順詳細設定のデータがありません"
                                           TO  WERR-ERRMSG
               WHEN    "0102"
                   MOVE    "種別一覧が表示してありません"
                                           TO  WERR-ERRMSG
               WHEN    "0103"
                   MOVE    "処理対象の公費が選択してありません"
                                           TO  WERR-ERRMSG
               WHEN    "0105"
                   MOVE    "これ以上公費の登録ができません"
                                           TO  WERR-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "公費記載順設定の追加処理ができませんでした"
                                           TO  WERR-ERRMSG
               WHEN    "1002"
                   MOVE    "公費記載順設定の更新処理ができませんでした"
                                           TO  WERR-ERRMSG
               WHEN    "1003"
                   MOVE    "公費記載順設定の削除処理ができませんでした"
                                           TO  WERR-ERRMSG
           END-EVALUATE
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W122"          TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WID1
           INITIALIZE                  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "公費記載順設定情報を登録します"
                                                TO  WID1-ID1MSG
                   MOVE    "B12"                TO  MCP-WIDGET
               WHEN    "1002"
                   MOVE    "入力内容をクリアします。よろしいですか。"
                                                TO  WID1-ID1MSG
                   MOVE    "B01"                TO  MCP-WIDGET
               WHEN    "1009"
                   STRING   "公費の登録が１件のグループがあります。"
                                                DELIMITED  BY  SIZE
                            "このまま公費記載順設定情報を登録しますか"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W122"          TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
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
                    MOVE  "SELNUM"         TO  MCP-WIDGET
                WHEN    2
                    MOVE  "STYUKYMD"       TO  MCP-WIDGET
                WHEN    3
                    MOVE  "EDYUKYMD"       TO  MCP-WIDGET
                WHEN    4
                    MOVE  "GRP_NO"         TO  MCP-WIDGET
                WHEN    5
                    MOVE  "KOHNUM"         TO  MCP-WIDGET
                WHEN    10
                    MOVE  "KOHNUM_UPDATE"  TO  MCP-WIDGET
      *
                WHEN    999
                    MOVE  "B12"            TO  MCP-WIDGET
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
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "GRP_NO"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "KOHNUM"
                   IF      SPA-NAI-SELNUM2 =   ZERO
                       MOVE    10          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    5           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
           MOVE    ZERO            TO  WRK-SYMD
      *
           IF    ( WRK-YMD         =   "00000000"
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD             TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
                   MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               END-IF
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
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
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           PERFORM     UNTIL (     FLG-HKNNUM    =  1                  )
                        OR   (    (HKN-TEKSTYMD  <= SPA-NAI-EDYUKYMD )
                              AND (HKN-TEKEDYMD  >= SPA-NAI-STYUKYMD ) )
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-N-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNNUM
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
