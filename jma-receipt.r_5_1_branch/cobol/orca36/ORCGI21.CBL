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
       PROGRAM-ID.                 ORCGI21.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院患者検索
      *  コンポーネント名  : 病棟別患者入院患者数表示
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/11/02    NACL−太田  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    入院患者照会共通ＳＰＡ
           COPY    "I2SCR-SPA".
      *
      *    入院患者照会用共通パラメタ
           COPY    "I2COMMON-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-PTSRH                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-BTULIST-ROW             PIC 9(01).
           03  FLG-CHGSRYYMD               PIC 9(01).
           03  FLG-JOBEND                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXM                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-SYMD                    PIC X(10).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU-G.
               05  WRK-NISSU1              PIC 9(05).
               05  WRK-NISSU2              PIC 9(05).
      *
           03  WRK-YMD                     PIC X(10).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-STSRYYMD                PIC X(08).
           03  WRK-STATE                   PIC S9(9)   BINARY.
           03  WRK-BTUNUM-G.
               05  WRK-BTUNUM              PIC 9(02).
           03  WRK-NINZUZ                  PIC Z,ZZZ,ZZ9.
           03  WRK-AVGZ                    PIC ZZZ,ZZ9.9.
           03  WRK-LBLKIKAN-G.
               05  WRK-LBLKIKAN-TITLE      PIC X(10).
               05  WRK-LBLKIKAN-STSRYYMD   PIC X(09).
               05  WRK-LBLKIKAN-SEP        PIC X(05).
               05  WRK-LBLKIKAN-EDSRYYMD   PIC X(09).
           03  WRK-LBLNISSU-G.
               05  WRK-LBLNISSU-FIL1       PIC X(02).
               05  WRK-LBLNISSU-TITLE      PIC X(04).
               05  WRK-LBLNISSU-NISSU      PIC ZZZ9.
               05  WRK-LBLNISSU-FIL2       PIC X(04).
           03  WRK-SHORIERR                PIC X(200).
           03  WRK-PROC-FLG                PIC 9(1).
           03  WRK-DATAKBN                 PIC X(01).
      *
           03  WRK-FILENM-AREA.
               07  WRK-FILENM              PIC X(100).
               07  WRK-FILENM-FOLDER       PIC X(100).
      *
       01  WRK-ROW.
           05  WRK-BTULIST-ROW             PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-BTULIST-MAX       PIC 9(05)   VALUE   100.
           03  CONST-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-SHELLID           PIC X(08)   VALUE   "ORCBI203".
      *
       01  BTUSUM-AREA.
           COPY    "CPBTUSUM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5001.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者情報照会テーブル
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    シェルテーブル
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    患者日単位入院情報取得（最大１年）
           COPY    "CPORCSNDAYINF.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
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
       01 SCRAREA.
           COPY    "ORCA36SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "I2ERR"
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I21"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
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
      *    情報削除画面
           WHEN    "XB01"
               MOVE    LNK-COMMON          TO  SPA-KYOUTU
               MOVE    LNK-SCRSPA          TO  SPA-I2FREE
               MOVE    SPA-WORK            TO  SPA-I2KYOUTU
           WHEN    "I22"
               PERFORM 3001-I22SPASET-SEC
           WHEN    "I23"
               PERFORM 3001-I23SPASET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｉ２２戻りスパ編集処理
      *****************************************************************
       3001-I22SPASET-SEC             SECTION.
      *
           IF  ( SPA-I22-RTOK-FLG =   ZERO )
               GO  TO  3001-I22SPASET-EXT
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 900-PTSRH-INS2-SEC
      *
           MOVE    ZERO            TO  SPA-I23-FILESV-CHK-FLG
           MOVE    SPACE           TO  SPA-I23-SELHMS
      *
           MOVE    SPACE           TO  WRK-FILENM-AREA
           PERFORM VARYING IDX4    FROM    100 BY  -1
                   UNTIL   IDX4     <       1
               IF      ( SPA-I22-FILENAME (IDX4:1)
                                               =   "/" )
                   MOVE    SPA-GMN-I22-FILENAME (1:IDX4)
                                           TO  WRK-FILENM-FOLDER
                   COMPUTE IDX5    =   IDX4    +   1
                   MOVE    SPA-GMN-I22-FILENAME (IDX5:)
                                           TO  WRK-FILENM
                   MOVE    1               TO  IDX4
               END-IF
           END-PERFORM
           IF      WRK-FILENM          =   SPACE
               MOVE    "/tmp/"             TO  WRK-FILENM-FOLDER
               MOVE    SPA-I22-FILENAME
                                           TO  WRK-FILENM
           END-IF
      *
      *    ファイル保存情報削除
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    CONST-SHELLID   TO  SFILESV-TBL-KEY
           MOVE    SPACE           TO  SFILESV-SRYYM(1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
      *
      *    ジョブ開始セット処理
           PERFORM 900-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
      *
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  SHELLTBL
      *
           MOVE    "orcbi203.sh"   TO  SHELLTBL-NAME
      *
           MOVE    SPA-I22-PROC-FLG
                                   TO  SHELLTBL-ARG1
           IF    ( SPA-I22-FILE   =   "1" )
               STRING   WRK-FILENM-FOLDER      DELIMITED  BY  SPACE
                        WRK-FILENM             DELIMITED  BY  SPACE
                                               INTO  SHELLTBL-ARG2
               END-STRING
           ELSE
               MOVE    SPA-I22-FILENAME
                                       TO  SHELLTBL-ARG2
           END-IF
           MOVE    SPACE           TO  SHELLTBL-ARG3
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG4
           MOVE    SPA-SYSYMD      TO  SHELLTBL-ARG5
           MOVE    SPACE           TO  SHELLTBL-ARG6
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG7
           MOVE    SPA-STAFFCD     TO  SHELLTBL-ARG8
           MOVE    CONST-JOBID     TO  SHELLTBL-ARG9
           MOVE    CONST-SHELLID   TO  SHELLTBL-ARG10
           MOVE    SPA-I22-FILE    TO  SHELLTBL-ARG11
           MOVE    SPA-I22-CDKBN   TO  SHELLTBL-ARG12
           MOVE    WRK-FILENM-FOLDER
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-FILENM      TO  SHELLTBL-ARG14
           STRING  "I21.csv."              DELIMITED  BY  SIZE
                    SPA-TERMID             DELIMITED  BY  SIZE
                    ".wk"                  DELIMITED  BY  SIZE
           INTO  SHELLTBL-ARG15
           END-STRING
      *
           MOVE    SPA-NAI-I21-BTULIST-SELYMD
                                   TO  SHELLTBL-ARG16
           MOVE    SPA-NAI-I21-BTULIST-SELHMS
                                   TO  SHELLTBL-ARG17
      *
           MOVE    SPA-NAI-I21-STSRYYMD
                                   TO  SHELLTBL-ARG18
           MOVE    SPA-NAI-I21-EDSRYYMD
                                   TO  SHELLTBL-ARG19
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF    ( SPA-I22-FILE   =   "1" )
               MOVE    "SV"            TO  SPA-I2ID1-FLG
           ELSE
               MOVE    SPACE           TO  SPA-I2ID1-FLG
           END-IF
           MOVE    SPACE           TO  SPA-I2IDCD
      *
           PERFORM 440-I23-SENI-SEC
      *
           .
       3001-I22SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       3001-I23SPASET-SEC             SECTION.
      *
           IF    ( SPA-I2ID1-FLG    =   "SV" )
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    CONST-SHELLID   TO  SFILESV-TBL-KEY
               MOVE    SPA-GMN-SELHMS  TO  SFILESV-SRYYM(1)
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *
           MOVE    SPACE           TO  SPA-I2ID1-FLG
           MOVE    SPACE           TO  SPA-I2IDCD
           MOVE    SPACE           TO  SPA-I23-MOTOPG
      *
           .
       3001-I23SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      *
           INITIALIZE                  SPA-I21-AREA
      *
           MOVE    1                   TO  SPA-GMN-I21-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟リスト編集処理
      *****************************************************************
       3101-BTULIST-EDIT-SEC           SECTION.
      *
           MOVE    1                       TO  FLG-BTULIST-ROW
      *
           INITIALIZE                          SPA-GMN-I21-BTULIST-AREA
                                               SPA-NAI-I21-BTULIST-AREA
           MOVE    SPA-GMN-I21-STSRYYMD
                                       TO  SPA-GMN-I21-BTULIST-STSRYYMD
           MOVE    SPA-GMN-I21-EDSRYYMD
                                       TO  SPA-GMN-I21-BTULIST-EDSRYYMD
           MOVE    SPA-NAI-I21-STSRYYMD
                                       TO  SPA-NAI-I21-BTULIST-STSRYYMD
           MOVE    SPA-NAI-I21-EDSRYYMD
                                       TO  SPA-NAI-I21-BTULIST-EDSRYYMD
      *
           MOVE    SPA-NAI-I21-STSRYYMD
                                       TO  WRK-STYMD
           MOVE    SPA-NAI-I21-EDSRYYMD
                                       TO  WRK-EDYMD
           PERFORM 800-KIKAN-NISSU-SEC
           MOVE    WRK-NISSU2          TO  SPA-GMN-I21-BTULIST-NISSU
      *
           MOVE    SPA-NAI-I21-SELYMD  TO  SPA-NAI-I21-BTULIST-SELYMD
           MOVE    SPA-NAI-I21-SELHMS  TO  SPA-NAI-I21-BTULIST-SELHMS
      *
           PERFORM 900-PTSRH-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTSRH   NOT =   ZERO )
                    OR   ( IDXL           >=   CONST-BTULIST-MAX )
      *
               COMPUTE SPA-GMN-I21-BTULIST-MAX
                   =   SPA-GMN-I21-BTULIST-MAX
                   +   1
      *
               MOVE    SPA-GMN-I21-BTULIST-MAX
                                       TO  IDXL
      *
               MOVE    PTSRH-MEMO      TO  BTUSUM-AREA
      *
               MOVE    BTUSUM-TTLKBN   TO  SPA-GMN-I21-TTTLKBN (IDXL)
               MOVE    BTUSUM-BTUNUM   TO  SPA-GMN-I21-TBTUNUM (IDXL)
                                           WRK-BTUNUM
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5001"          TO  SYS-KANRICD
               MOVE    WRK-BTUNUM-G    TO  SYS-KBNCD
               MOVE    SPA-NAI-I21-EDSRYYMD
                                       TO  SYS-STYUKYMD
               MOVE    SPA-NAI-I21-STSRYYMD
                                       TO  SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY26-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-TANNAME
                                       TO  SPA-GMN-I21-TBTUNAME   (IDXL)
      *
               MOVE    BTUSUM-ST       TO  SPA-GMN-I21-TCNT-ST    (IDXL)
               MOVE    BTUSUM-NYUIN    TO  SPA-GMN-I21-TCNT-NYUIN (IDXL)
               MOVE    BTUSUM-TENNYU   TO  SPA-GMN-I21-TCNT-TENNYU(IDXL)
               MOVE    BTUSUM-TENSTU   TO  SPA-GMN-I21-TCNT-TENSTU(IDXL)
               MOVE    BTUSUM-TAIIN    TO  SPA-GMN-I21-TCNT-TAIIN (IDXL)
               MOVE    BTUSUM-ED       TO  SPA-GMN-I21-TCNT-ED    (IDXL)
               MOVE    BTUSUM-TOTAL    TO  SPA-GMN-I21-TCNT-TOTAL (IDXL)
               COMPUTE SPA-GMN-I21-TCNT-AVG (IDXL)
                   =   SPA-GMN-I21-TCNT-TOTAL (IDXL)
                   /   SPA-GMN-I21-BTULIST-NISSU
                   +   0.05
      *
               PERFORM 900-PTSRH-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    ZERO        TO  SPA-NAI-I21-DURATION
           MOVE    SPACE       TO  SPA-NAI-I21-SELYMD
                                   SPA-NAI-I21-SELHMS
      *
           .
       3101-BTULIST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM  310-SPASET-SEC
      *    診療日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM  4100-SRYYMD-SET-SEC
      *    診療月
           WHEN    "CLICKED"       ALSO    "B04S"
               PERFORM 4100-SRYYM-SET-SEC
      *    前月・前日
           WHEN    "CLICKED"       ALSO    "B06"
               MOVE    ZERO        TO  FLG-CHGSRYYMD
               PERFORM 4100-CHGSRYYMD-SEC
      *    次月・次日
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE    1           TO  FLG-CHGSRYYMD
               PERFORM 4100-CHGSRYYMD-SEC
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 440-XB01-SENI-SEC
      *    ＣＳＶ出力
           WHEN    "CLICKED"       ALSO    "B10"
               MOVE    ZERO        TO  WRK-PROC-FLG
               PERFORM 440-I22-SENI-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B11"
               MOVE    1           TO  WRK-PROC-FLG
               PERFORM 440-I22-SENI-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-KAKUTEI-SEC
      *    処理結果画面
           WHEN    "CLICKED"       ALSO    "B12S"
               MOVE    1           TO  WRK-PROC-FLG
               PERFORM 440-I23-SENI-SEC
      *    検索中確認
           WHEN    "CLICKED"       ALSO    "pandatimer1"
               IF    ( SPA-NAI-I21-SELYMD  NOT =   SPACE )
                   PERFORM 490-JOUTAI-SEC
               END-IF
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    WHEN    "XXXX"
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-I21-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    I21-STSRYYMD    TO  SPA-GMN-I21-STSRYYMD
           MOVE    I21-EDSRYYMD    TO  SPA-GMN-I21-EDSRYYMD
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    期間開始日チェック
           PERFORM 4101-STSRYYMD-CHK-SEC
      *
      *    期間終了日チェック
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-EDSRYYMD-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間開始日チェック処理
      *****************************************************************
       4101-STSRYYMD-CHK-SEC           SECTION.
      *
            MOVE    SPACE              TO  SPA-NAI-I21-STSRYYMD
      *
            MOVE    SPA-GMN-I21-STSRYYMD   TO  WRK-SYMD
            PERFORM 800-HIZUKE-CHK-SEC
            IF    ( WRK-SYMD           =   LOW-VALUE )
                MOVE    "0001"         TO  SPA-ERRCD
                MOVE    1              TO  SPA-GMN-I21-CUR
            ELSE
                MOVE    WRK-SYMD       TO  SPA-NAI-I21-STSRYYMD
                MOVE    WRK-EDTYMD1    TO  SPA-GMN-I21-STSRYYMD
            END-IF
      *
           .
      *
       4101-STSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間終了日チェック処理
      *****************************************************************
       4101-EDSRYYMD-CHK-SEC           SECTION.
      *
            MOVE    SPACE              TO  SPA-NAI-I21-EDSRYYMD
      *
            MOVE    SPA-GMN-I21-EDSRYYMD   TO  WRK-SYMD
            PERFORM 800-HIZUKE-CHK-SEC
            IF    ( WRK-SYMD           =   LOW-VALUE )
                MOVE    "0002"         TO  SPA-ERRCD
                MOVE    2              TO  SPA-GMN-I21-CUR
            ELSE
                MOVE    WRK-SYMD       TO  SPA-NAI-I21-EDSRYYMD
                MOVE    WRK-EDTYMD1    TO  SPA-GMN-I21-EDSRYYMD
            END-IF
      *
           .
      *
       4101-EDSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日設定処理
      *****************************************************************
       4100-SRYYMD-SET-SEC             SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-I21-STSRYYMD
                                           SPA-NAI-I21-EDSRYYMD
      *
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-I21-STSRYYMD
                                           SPA-GMN-I21-EDSRYYMD
      *
           MOVE    1                   TO  SPA-GMN-I21-MMORDD
      *
           MOVE    1                   TO  SPA-GMN-I21-CUR
      *
           .
       4100-SRYYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月設定処理
      *****************************************************************
       4100-SRYYM-SET-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-SYSYMD          TO  WRK-STSRYYMD
           MOVE    "01"                TO  WRK-STSRYYMD(7:2)
           PERFORM 41001-SRYYM-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-I21-MMORDD
      *
           MOVE    1                   TO  SPA-GMN-I21-CUR
      *
           .
       4100-SRYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月設定処理
      *****************************************************************
       41001-SRYYM-SET-SEC             SECTION.
      *
           MOVE    WRK-STSRYYMD        TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-SYMD            TO  SPA-NAI-I21-STSRYYMD
           MOVE    WRK-EDTYMD1         TO  SPA-GMN-I21-STSRYYMD
      *
           MOVE    WRK-SYMD            TO  WRK-YMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
      *
           MOVE    WRK-KEISANYMD       TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
      *
           MOVE    WRK-KEISANYMD       TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-SYMD            TO  SPA-NAI-I21-EDSRYYMD
           MOVE    WRK-EDTYMD1         TO  SPA-GMN-I21-EDSRYYMD
      *
           .
       41001-SRYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療日変更処理
      *****************************************************************
       4100-CHGSRYYMD-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4101-STSRYYMD-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-I21-STSRYYMD    =   SPACE )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
                   MOVE    "01"            TO  WRK-YMD(7:2)
               ELSE
                   MOVE    SPA-NAI-I21-STSRYYMD
                                           TO  WRK-YMD
               END-IF
               IF    ( FLG-CHGSRYYMD       =   ZERO )
                   MOVE    -1              TO  WRK-ZOGEN
               ELSE
                   MOVE    1               TO  WRK-ZOGEN
               END-IF
               IF    ( SPA-GMN-I21-MMORDD  =   1 )
                   MOVE    "1"             TO  WRK-ZOGENPTN
               ELSE
                   MOVE    "2"             TO  WRK-ZOGENPTN
               END-IF
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD       TO  WRK-STSRYYMD
               PERFORM 41001-SRYYM-SET-SEC
               IF    ( SPA-GMN-I21-MMORDD  =   1 )
                   MOVE    SPA-NAI-I21-STSRYYMD
                                           TO  SPA-NAI-I21-EDSRYYMD
                   MOVE    SPA-GMN-I21-STSRYYMD
                                           TO  SPA-GMN-I21-EDSRYYMD
               END-IF
      *
               MOVE    1                   TO  SPA-GMN-I21-CUR
      *
           END-IF
      *
           .
       4100-CHGSRYYMD-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4200-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SPA-NAI-I21-STSRYYMD    =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-I21-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-I21-EDSRYYMD    =   SPACE )
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-I21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-I21-EDSRYYMD
                                       <   SPA-NAI-I21-STSRYYMD )
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-I21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-NAI-I21-STSRYYMD
                                       TO  WRK-YMD
               MOVE    11              TO  WRK-ZOGEN
               MOVE    "2"             TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
               IF    ( WRK-KEISANYMD (1:6)
                                       <   SPA-NAI-I21-EDSRYYMD (1:6))
                   MOVE    "0006"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-I21-CUR
               END-IF
           END-IF
      *
           .
       4200-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      *
           MOVE    "I20"               TO  SPA-SAKIPG
           MOVE    "I21"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "I20"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *************************************************************
      *    情報削除画面遷移処理
      *************************************************************
       440-XB01-SENI-SEC               SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
      *
      *    画面移行用のパラメタ変更
           MOVE    "I21"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID       TO  SPA-JOB-SHELLID
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I2KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I2FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-XB01-SENI-EXT.
           EXIT.
      *************************************************************
      *    印刷指示画面遷移処理
      *************************************************************
       440-I22-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =  ZERO )
               MOVE    "7777"      TO  SPA-ERRCD
               MOVE    SPACE       TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  440-I22-SENI-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-I22-SENI-AREA
           INITIALIZE                  SPA-I22-SENI-AREA
      *
           MOVE    WRK-PROC-FLG    TO  SPA-I22-PROC-FLG
           IF    ( WRK-PROC-FLG    =   ZERO )
               STRING  "BYOTO_"                        DELIMITED BY SIZE
                       SPA-NAI-I21-BTULIST-STSRYYMD    DELIMITED BY SIZE
                       "-"                             DELIMITED BY SIZE
                       SPA-NAI-I21-BTULIST-EDSRYYMD    DELIMITED BY SIZE
                       ".csv"                          DELIMITED BY SIZE
               INTO    SPA-I22-FILENAME
               END-STRING
           END-IF
           MOVE    "I21"           TO  SPA-I22-MOTOPG
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI22"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE  "B12"             TO  MCP-WIDGET
      *
           MOVE  "I20"             TO  SPA-MOTOPG
           MOVE  "I22"             TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "I22"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       440-I22-SENI-EXT.
           EXIT.
      *************************************************************
      *    処理結果確認画面遷移処理
      *************************************************************
       440-I23-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-I23-AREA
      *
           MOVE  "I21"             TO  SPA-I23-MOTOPG
           MOVE  CONST-SHELLID     TO  SPA-I23-SHELLID
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI23"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE  "B01"             TO  MCP-WIDGET
      *
           MOVE  "I21"             TO  SPA-MOTOPG
           MOVE  "I23"             TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "I23"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       440-I23-SENI-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       480-KAKUTEI-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4200-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 800-MCNDATE-SEC
      *
               MOVE    SMCNDATE-YMD    TO  SPA-NAI-I21-SELYMD
               MOVE    SMCNDATE-HMS    TO  SPA-NAI-I21-SELHMS
      *
               MOVE    ZERO            TO  SPA-NAI-I21-DURATION
      *
               INITIALIZE                  SPA-GMN-I21-BTULIST-AREA
                                           SPA-NAI-I21-BTULIST-AREA
      *
               MOVE    SPACE           TO  SHELLTBL
               INITIALIZE                  SHELLTBL
      *
               MOVE    "orcbi202.sh"   TO  SHELLTBL-NAME
      *
               MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    SMCNDATE-YMD    TO  SHELLTBL-ARG3
               MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG4
               MOVE    SPA-NAI-I21-STSRYYMD
                                       TO  SHELLTBL-ARG5
               MOVE    SPA-NAI-I21-EDSRYYMD
                                       TO  SHELLTBL-ARG6
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "allways"       TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           END-IF
      *
           .
       480-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    状態確認画面
      *****************************************************************
       490-JOUTAI-SEC                  SECTION.
      *
           PERFORM 900-PTSRH-KEY2-SEL-SEC
           IF    ( FLG-PTSRH   =   ZERO )
               PERFORM 3101-BTULIST-EDIT-SEC
           END-IF
      *
           .
       490-JOUTAI-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I21"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    I21-BTULIST-ROW TO  WRK-BTULIST-ROW
      *
           IF    ( FLG-BTULIST-ROW     =   1 )
               MOVE    ZERO            TO  I21-BTULIST-ROW
               MOVE    ZERO            TO  I21-BTULIST-ROWATTR
           ELSE
               MOVE    WRK-BTULIST-ROW TO  I21-BTULIST-ROW
               MOVE    ZERO            TO  I21-BTULIST-ROWATTR
           END-IF
      *
           MOVE    SPACE           TO  I21
           INITIALIZE                  I21
      *
           MOVE    SPA-GMN-I21-STSRYYMD
                                   TO  I21-STSRYYMD
           MOVE    SPA-GMN-I21-EDSRYYMD
                                   TO  I21-EDSRYYMD
      *
           MOVE    WIDGET-NORMAL   TO  WRK-STATE
      *
           IF    ( SPA-NAI-I21-SELYMD  NOT =   SPACE )
               IF    ( SPA-NAI-I21-DURATION    <   5 )
                   COMPUTE SPA-NAI-I21-DURATION
                       =   SPA-NAI-I21-DURATION
                       +   1
               END-IF
               MOVE    "＊＊＊　集計中　＊＊＊"
                                   TO  I21-LBLMSG01
               MOVE    "red"       TO  I21-LBLMSG01-STYLE
      *
               MOVE    WIDGET-INSENSITIVE
                                   TO  WRK-STATE
      *
           ELSE
               MOVE    ZERO        TO  SPA-NAI-I21-DURATION
               IF    ( SPA-GMN-I21-BTULIST-STSRYYMD
                                   NOT =   SPACE )
                   INITIALIZE          WRK-LBLKIKAN-G
                   MOVE    "対象期間："    TO  WRK-LBLKIKAN-TITLE
                   MOVE    SPA-GMN-I21-BTULIST-STSRYYMD
                                           TO  WRK-LBLKIKAN-STSRYYMD
                   MOVE    " 〜 "          TO  WRK-LBLKIKAN-SEP
                   MOVE    SPA-GMN-I21-BTULIST-EDSRYYMD
                                           TO  WRK-LBLKIKAN-EDSRYYMD
      *
                   INITIALIZE          WRK-LBLNISSU-G
                   MOVE    "（"            TO  WRK-LBLNISSU-FIL1
                   MOVE    "日数"          TO  WRK-LBLNISSU-TITLE
                   MOVE    SPA-GMN-I21-BTULIST-NISSU
                                           TO  WRK-LBLNISSU-NISSU
                   MOVE    "日）"            TO  WRK-LBLNISSU-FIL2
      *
                   STRING  WRK-LBLKIKAN-G  DELIMITED BY SIZE
                           " "             DELIMITED BY SIZE
                           WRK-LBLNISSU-G  DELIMITED BY SIZE
                   INTO    I21-LBLMSG01
                   END-STRING
      *
                   MOVE    "black"         TO  I21-LBLMSG01-STYLE
               END-IF
           END-IF
      *
      *    前月(前日)/次月(次日)
           IF    ( SPA-GMN-I21-MMORDD      =   ZERO )
               MOVE    "前月"              TO  I21-B06-LABEL
               MOVE    "次月"              TO  I21-B07-LABEL
           ELSE
               MOVE    "前日"              TO  I21-B06-LABEL
               MOVE    "次日"              TO  I21-B07-LABEL
           END-IF
      *
           MOVE    WRK-STATE       TO  I21-B04-STATE
                                       I21-B04S-STATE
                                       I21-B06-STATE
                                       I21-B07-STATE
                                       I21-B12-STATE
                                       I21-STSRYYMD-STATE
                                       I21-EDSRYYMD-STATE
      *
           IF    ( SPA-GMN-I21-BTULIST-MAX <=   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  I21-B10-STATE
                                               I21-B11-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  I21-B10-STATE
                                               I21-B11-STATE
           END-IF
      *
           MOVE    SPA-NAI-I21-DURATION
                                   TO  I21-DURATION
      *
           MOVE    ZERO            TO  IDXM
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-I21-BTULIST-MAX )
      *
               COMPUTE IDXM    =   IDXM    +   1
      *
               IF    ( SPA-GMN-I21-TTTLKBN (IDXL)  =   "1" )
      *
                   MOVE    ALL "-" TO  I21-BTULIST-ITEM (IDXM)
      *
                   COMPUTE IDXM    =   IDXM    +   1
      *
                   MOVE    "合計"  TO  I21-TBTUNAME (IDXM)
      *
               ELSE
      *
                   MOVE    SPA-GMN-I21-TBTUNUM (IDXL)
                                   TO  WRK-BTUNUM
                   MOVE    WRK-BTUNUM-G
                                   TO I21-TBTUNUM (IDXM)
      *
                   MOVE    SPA-GMN-I21-TBTUNAME (IDXL)
                                   TO  I21-TBTUNAME (IDXM)
               END-IF
      *
               MOVE    SPA-GMN-I21-TCNT-ST (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-ST (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-NYUIN (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-NYUIN (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-TENNYU (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-TENNYU (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-TENSTU (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-TENSTU (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-TAIIN (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-TAIIN (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-ED (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-ED (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-TOTAL (IDXL)
                                   TO  WRK-NINZUZ
               MOVE    WRK-NINZUZ  TO  I21-TCNT-TOTAL (IDXM)
      *
               MOVE    SPA-GMN-I21-TCNT-AVG (IDXL)
                                   TO  WRK-AVGZ
               MOVE    WRK-AVGZ    TO  I21-TCNT-AVG (IDXM)
      *
           END-PERFORM
      *
           MOVE    IDXM            TO  I21-BTULIST-COUNT
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I21-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I21-CUR
           WHEN    001
               MOVE    "STSRYYMD"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "EDSRYYMD"      TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "STSRYYMD"
               MOVE    002             TO  SPA-GMN-I21-CUR
           WHEN    "EDSRYYMD"
               MOVE    212             TO  SPA-GMN-I21-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "期間開始日が正しく入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "期間終了日が正しく入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "期間終了日が期間開始日よりも前になっています"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "１２ヶ月以内の期間を指定してください"
                                       TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I2ERR
           MOVE    SPA-ERRCD           TO  I2ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I2ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I21"               TO  SPA-MOTOPG
           MOVE    "I2ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I2ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
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
      *    日付入力表示理
      *****************************************************************
       800-HIZUKE-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF    ( WRK-SYMD        =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-SYMD        TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                       ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY TO  WRK-EDTYMD1
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    LOW-VALUE   TO  WRK-SYMD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    期間計算処理
      *****************************************************************
       800-KIKAN-NISSU-SEC              SECTION.
      *
           INITIALIZE                  WRK-NISSU-G
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1       =   ZERO )
               MOVE    LNK-DAY5-NISSU1 TO  WRK-NISSU1
               MOVE    LNK-DAY5-NISSU2 TO  WRK-NISSU2
           END-IF
      *
           .
       800-KIKAN-NISSU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       900-JOBKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOBKANRI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOBKANRI-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-JOBEND
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-JOBEND
               END-IF
           ELSE
                   MOVE    2           TO  FLG-JOBEND
           END-IF
      *
           .
       900-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "a"                 TO  PTSRH-DATAKBN
           MOVE    SPA-NAI-I21-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-NAI-I21-SELHMS  TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "b"                 TO  PTSRH-DATAKBN
           MOVE    SPA-NAI-I21-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-NAI-I21-SELHMS  TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS2-SEC              SECTION.
      *
           MOVE    "c"             TO  WRK-DATAKBN
           PERFORM 900-PTSRH-DEL1-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "c"             TO  PTSRH-DATAKBN
           MOVE    SPA-NAI-I21-BTULIST-SELYMD
                                   TO  PTSRH-UPYMD
           MOVE    SPA-NAI-I21-BTULIST-SELHMS
                                   TO  PTSRH-UPHMS
           MOVE    "b"             TO  PTSRH-UPDATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins2"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-PTSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    WRK-DATAKBN     TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "a"             TO  PTSRH-VSTR (1)
           MOVE    "b"             TO  PTSRH-VSTR (2)
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del5"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY26-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"       TO   MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
