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
       PROGRAM-ID.                 ORCGXG04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療情報システム用データ出力
      *  コンポーネント名  : 出力選択画面（ＸＧ０４）
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/02/02    NACL-伊藤     新規作成
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ＣＳＶ出力指定処理共通パラメタ
           COPY    "XG01COMMON-SPA".
      *
           COPY    "XG01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "CMJOBNAME.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-FILEMIS         PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-FILE-MIS-TBL-GROUP
                                   PIC X(14).
           03  WRK-FILE-MIS-RENNUM PIC 9(04).
           03  WRK-FILE-MIS-ERR    PIC X(200).
           03  WRK-XGIDMSG         PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-ZZ              PIC ZZ9.
           03  WRK-KENSU           PIC ZZZZ9.
           03  WRK-MOTOPG          PIC X(08).
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
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-LIST-ROW        PIC S9(9)   BINARY.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PUTCSV".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療情報システム
       01  FILE-MIS-REC.
           COPY    "CPFILE-MIS.INC".
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           COPY    "ORCA07SCRAREA.INC".
      ******************************************************************
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      IDX-AREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-XG01FREE
           MOVE    SPA-WORK            TO  SPA-XG01KYOUTU
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XG01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XG01FREE        TO  SPA-FREE
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面からの戻り時
           IF      SPA-MOTOPG          =   "XGERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面処理
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集処理
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "XG04"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-MOTOPG
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "XG99"
                   PERFORM 340-XG99-SET-SEC
                   PERFORM 310-SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "XGID1"
                   PERFORM 330-XGID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "XG02"
                   PERFORM 350-XG02-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               EVALUATE    WRK-MOTOPG
                   WHEN    "XB01"
                       MOVE    SPA-WORK        TO   WRK-MOTOPG
                       INITIALIZE                   SPA-XG01FREE
                                                    SPA-XG01KYOUTU
                       MOVE    WRK-MOTOPG      TO   SPA-XG01-MOTOPG
                       PERFORM 310-SPASET-SEC
                       GO  TO  300-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           INITIALIZE                      SPA-XG01FREE
                                           SPA-XG01KYOUTU
           MOVE    WRK-MOTOPG          TO  SPA-XG01-MOTOPG
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA
                                           SPA-NAI-AREA
      *
           PERFORM 3101-DATACHK-SEC
           IF      FLG-FILEMIS             =   1
               GO  TO  310-SPASET-90
           END-IF
      *
           IF      SPA-NAI-XG04-TBL-KEY
                                       =   SPACE
               MOVE    "RENKEI1"           TO  SPA-NAI-XG04-TBL-KEY
           END-IF
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    SPA-NAI-XG04-TBL-KEY
                                       TO  FILE-MIS-TBL-KEY
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILEMIS
           END-IF
      *
           IF      FLG-FILEMIS             =   1
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               PERFORM 4401-DATACHANGE-SEC
               GO  TO  310-SPASET-SEC
           END-IF
           MOVE    SPACE               TO  WRK-FILE-MIS-TBL-GROUP
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   FLG-FILEMIS     =   1
                   OR      IDX             =   100
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN-TNO        (IDX)
               MOVE    FILE-MIS-TBL-KEY    TO  SPA-NAI-TTBL-KEY   (IDX)
               MOVE    FILE-MIS-TBL-GROUP  TO  SPA-NAI-TTBL-GROUP (IDX)
               IF      FILE-MIS-TBL-GROUP
                                       NOT =   WRK-FILE-MIS-TBL-GROUP
                   MOVE    FILE-MIS-TBL-GROUP  TO
                                               SPA-GMN-TTBL-GROUP (IDX)
                                               WRK-FILE-MIS-TBL-GROUP
               ELSE
                   MOVE    "　〃"              TO
                                               SPA-GMN-TTBL-GROUP (IDX)
               END-IF
               MOVE    FILE-MIS-SRYYM      TO  SPA-NAI-TSRYYM (IDX)
               IF      FILE-MIS-SRYYM  NOT =   SPACE   AND ZERO
                   MOVE    FILE-MIS-SRYYM  TO  WRK-SYMD (1:6)
                   MOVE    "01"            TO  WRK-SYMD (7:2)
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)
                                           TO  SPA-GMN-TSRYYM (IDX)
               END-IF
               IF     (FILE-MIS-TBL-KEY    =   "RENKEI1")    AND
                      (FILE-MIS-RENNUM     =   1)
                   CONTINUE
               ELSE
                   MOVE    FILE-MIS-SRYYMD     TO  SPA-NAI-TSRYYMD(IDX)
                   IF      FILE-MIS-SRYYMD NOT =   SPACE   AND ZERO
                       MOVE    FILE-MIS-SRYYMD TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD(IDX)
                   END-IF
               END-IF
               EVALUATE    FILE-MIS-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN
                                                              (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN
                                                              (IDX)
               END-EVALUATE
               SET     TBL-IDX             TO  1
               SEARCH      MNAME-TBL-OCC      VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE       TO
                                       SPA-GMN-TSHORI-TITLE (IDX)
                   WHEN    FILE-MIS-TBL-KEY    =   MNAME-SHELLID
                                                   (TBL-IDX)
                       MOVE    MNAME-JOBNAME (TBL-IDX)
                                           TO
                                       SPA-GMN-TSHORI-TITLE (IDX)
               END-SEARCH
               MOVE    FILE-MIS-RENNUM     TO  SPA-GMN-TRENNUM (IDX)
               MOVE    FILE-MIS-SHORI-RENNUM
                                           TO  SPA-GMN-TSHORI-RENNUM
                                                                  (IDX)
               MOVE    FILE-MIS-CNT        TO  SPA-GMN-TKENSU     (IDX)
               MOVE    FILE-MIS-TITLE      TO  SPA-GMN-TPRT-TITLE (IDX)
               MOVE    FILE-MIS-PUTFLG     TO  SPA-NAI-TPUTFLG    (IDX)
               IF      SPA-NAI-TPUTFLG (IDX)   =   SPACE
                   MOVE    "未"                TO  SPA-GMN-TPUTFLG (IDX)
               END-IF
               IF      FILE-MIS-TO-FOLDER  =   SPACE
                   MOVE    "/tmp/"         TO  SPA-NAI-TTO-FOLDER (IDX)
               ELSE
                   MOVE    FILE-MIS-TO-FOLDER
                                           TO  SPA-NAI-TTO-FOLDER (IDX)
               END-IF
               MOVE    FILE-MIS-TO-DATA    TO  SPA-NAI-TTO-DATA   (IDX)
               MOVE    FILE-MIS-CODE-TYPE  TO  SPA-NAI-TCODE-TYPE (IDX)
      *
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       310-SPASET-90.
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
           MOVE    1                   TO  FLG-LIST-ROW
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-DATACHK-SEC            SECTION.
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILEMIS
           END-IF
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3101-DATACHK-EXT.
           EXIT.
      *****************************************************************
      *    確認画面処理
      *****************************************************************
       330-XGID1-SET-SEC           SECTION.
      *
           IF      SPA-XGID1-FLG       =   "OK"
               EVALUATE    SPA-XGIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"              TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1002"
                   WHEN    "1003"
                       PERFORM 430-DELETE-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    SPACE               TO  SPA-XGID1-FLG
                                           SPA-XG99-FLG
           MOVE    SPACE               TO  SPA-XGIDCD
           .
       330-XGID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-XG99-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-XGID1-FLG
                                           SPA-XG99-FLG
           MOVE    SPACE               TO  SPA-XGIDCD
           INITIALIZE                      SPA-XG99-AREA
           .
       340-XG99-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶデータ作成処理
      *****************************************************************
       350-XG02-SET-SEC            SECTION.
      *
           IF      SPA-XGID1-FLG       =   "OK"
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 3501-CSV-HENSYU-SEC
                   GO  TO  350-XG02-SET-EXT
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPA-XG01-MOTOPG     TO  WRK-MOTOPG
           INITIALIZE                      SPA-XG01KYOUTU
           MOVE    WRK-MOTOPG          TO  SPA-XG01-MOTOPG
           .
       350-XG02-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       3501-CSV-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-XG99-FILESV-CHK-FLG
      *
      *    ジョブ開始セット
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           MOVE    SPA-GMN-TPRT-TITLE(SPA-NAI-SELNUM)
                                       TO  JOB-SHELLMSG
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "export_monblob.sh" TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    FILE-MIS-TBL-UUID   TO  SHELLTBL-ARG2
           IF      SPA-GMN-XG02-FILE   =   SPACE
               MOVE    "9"                 TO  SHELLTBL-ARG3
           ELSE
               MOVE    SPA-GMN-XG02-FILE   TO  SHELLTBL-ARG3
           END-IF
           MOVE    SPA-GMN-XG02-FILENAME
                                       TO  SHELLTBL-ARG4
           MOVE    SPA-NAI-XG02-TO-DATA
                                       TO  SHELLTBL-ARG5
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG6
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    SPA-GMN-XG02-CDKBN  TO  SHELLTBL-ARG8
           MOVE    SPA-NAI-XG04-TBL-KEY
                                       TO  SHELLTBL-ARG9
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF    ( SPA-GMN-XG02-FILE   =   "1" )
               MOVE    "SV"                TO  SPA-XGID1-FLG
           ELSE
               MOVE    SPACE               TO  SPA-XGID1-FLG
           END-IF
           MOVE    SPACE               TO  SPA-XGIDCD
                                           SPA-XG99-FLG
      *
      *    情報確認画面を表示
           PERFORM 490-KAKUNIN-SEC
           .
       3501-CSV-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"   ALSO    "B03"
                   PERFORM 430-DELETE-GID-SEC
      *        データ切替
               WHEN    "CLICKED"   ALSO    "B05"
                   PERFORM 440-DATACHANGE-SEC
      *        情報削除
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        ＣＳＶ出力
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 450-KAKUTEI-GID-SEC
      *        処理確認
               WHEN    "CLICKED"   ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 201-SELNUM-SEC
      *        行選択
               WHEN    ANY         ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
           INITIALIZE                      SPA-GMN-BODY
                                           SPA-NAI-BODY
           MOVE    XG04-SELNUM         TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-SELNUM      >   SPA-GMN-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNUM
                   PERFORM 2011-SELECT-HENSYU-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    99                  TO  SPA-GMN-CUR
           END-IF
           .
       201-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    選択行編集処理
      *****************************************************************
       2011-SELECT-HENSYU-SEC      SECTION.
      *
           MOVE    SPA-NAI-TTBL-GROUP    (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-TBL-GROUP
           MOVE    SPA-GMN-TRENNUM       (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-RENNUM
           MOVE    SPA-GMN-TSHORI-RENNUM (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SHORI-RENNUM
           .
       2011-SELECT-HENSYU-EXT.
           EXIT.
           .
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC              SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX FROM    1   BY  1
                   UNTIL       IDX >   SPA-GMN-MAX
               IF      XG04-SELECT (IDX)   =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                      SPA-GMN-BODY
           MOVE    SPA-NAI-SELNUM      TO  SPA-GMN-SELNUM
           PERFORM 2011-SELECT-HENSYU-SEC
           MOVE    99                  TO  SPA-GMN-CUR
           .
       203-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    戻る前チェック処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    "6666"              TO  SPA-XGIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC          SECTION.
      *
           MOVE    SPA-XG01-MOTOPG     TO  SPA-SAKIPG
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-WORK
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           INITIALIZE                      SPA-GMN-BODY
                                           SPA-NAI-BODY
           MOVE    1                   TO  SPA-GMN-CUR
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除前チェック処理
      *****************************************************************
       430-DELETE-GID-SEC          SECTION.
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    SPA-NAI-XG04-TBL-KEY
                                       TO  FILE-MIS-TBL-KEY
           MOVE    SPA-GMN-RENNUM      TO  FILE-MIS-RENNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  FILE-MIS-TBL-GROUP
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  FILE-MIS-SHORI-RENNUM
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILEMIS
           END-IF
      *
           IF      FLG-FILEMIS         =   1
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
      *        未処理のデータのとき
               IF      FILE-MIS-PUTFLG     =   SPACE
                   MOVE    "1003"              TO  SPA-XGIDCD
               ELSE
                   MOVE    "1002"              TO  SPA-XGIDCD
               END-IF
           END-IF
      *
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       430-DELETE-GID-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC              SECTION.
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    SPA-NAI-XG04-TBL-KEY
                                       TO  FILE-MIS-TBL-KEY
           MOVE    SPA-GMN-RENNUM      TO  FILE-MIS-RENNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  FILE-MIS-TBL-GROUP
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  FILE-MIS-SHORI-RENNUM
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "del_monblob"       TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
      *    monblobの削除処理が必要だがmonblobの操作ができないため
      *    tbl_file_mis.db内のdel_monblobでmonblobの削除処理を行う
      *
           PERFORM 310-SPASET-SEC
           .
       430-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    データ切替
      *****************************************************************
       440-DATACHANGE-SEC          SECTION.
      *
           PERFORM 4401-DATACHANGE-SEC
      *
           PERFORM 310-SPASET-SEC
           .
       440-DATACHANGE-EXT.
           EXIT.
      *****************************************************************
      *    データ切替
      *****************************************************************
       4401-DATACHANGE-SEC         SECTION.
      *
           EVALUATE    SPA-NAI-XG04-TBL-KEY
               WHEN    "RENKEI1"
                   MOVE    "MISRECE1"          TO  SPA-NAI-XG04-TBL-KEY
               WHEN    "MISRECE1"
                   MOVE    "MISJMAMR"          TO  SPA-NAI-XG04-TBL-KEY
               WHEN    "MISJMAMR"
                   MOVE    "MIGRATE1"          TO  SPA-NAI-XG04-TBL-KEY
               WHEN    "MIGRATE1"
                   MOVE    "RENKEI1"           TO  SPA-NAI-XG04-TBL-KEY
           END-EVALUATE
           .
       4401-DATACHANGE-EXT.
           EXIT.
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC         SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
           MOVE    SPA-XG01-MOTOPG     TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "XB01"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       450-KAKUTEI-GID-SEC         SECTION.
      *
      *    入力チェック
           IF      SPA-NAI-SELNUM      =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-GID-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  450-KAKUTEI-GID-EXT
           END-IF
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    SPA-NAI-XG04-TBL-KEY
                                       TO  FILE-MIS-TBL-KEY
           MOVE    SPA-GMN-RENNUM      TO  FILE-MIS-RENNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  FILE-MIS-TBL-GROUP
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  FILE-MIS-SHORI-RENNUM
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILEMIS
           END-IF
      *
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-FILEMIS         =   1
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-GID-EXT
           END-IF
      *    出力指示画面遷移処理
           PERFORM 4502-CSV-SIJI-CALL-SEC
           .
       450-KAKUTEI-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    "7777"              TO  SPA-ERRCD
                   MOVE    SPACE               TO  WRK-FILE-MIS-ERR
                   STRING  "ＣＳＶ出力処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE
                                             INTO  WRK-FILE-MIS-ERR
                   END-STRING
               END-IF
           END-IF
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出力指示画面遷移処理
      *****************************************************************
       4502-CSV-SIJI-CALL-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-XG01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XG01FREE        TO  SPA-FREE
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           CALL    "ORCGXG02"      USING   MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-XG01FREE
           MOVE    SPA-WORK            TO  SPA-XG01KYOUTU
           MOVE    "FILENAME"          TO  MCP-WIDGET
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    "XG02"              TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       4502-CSV-SIJI-CALL-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC             SECTION.
      *
           INITIALIZE                      XG99
           MOVE    ZERO                TO  XG99-COUNT
           MOVE    5                   TO  XG99-DURATION
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               MOVE    1                   TO  IDX
               MOVE    JOB-JOBID           TO  XG99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG        TO  XG99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)  TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)  TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)  TO  WRK-SS
               MOVE    WRK-HENTIME         TO  XG99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)    TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)    TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)    TO  WRK-SS
               MOVE    WRK-HENTIME         TO  XG99-ENDTIME  (IDX)
               MOVE    JOB-YOBI            TO  XG99-ERRMSG   (IDX)
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    "処理中です"        TO  XG99-MSG
               ELSE
                   IF      JOB-ERRCD           =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  XG99-MSG
                       PERFORM 4901-JOB-ERRSET-SEC
                       IF      XG99-MSG            =   SPACE
                           STRING  JOB-YOBI        DELIMITED BY SPACE
                                   "【"            DELIMITED BY SIZE
                                   JOB-SHELLMSG    DELIMITED BY SPACE
                                   "】"            DELIMITED BY SIZE
                                                   INTO  XG99-MSG
                           END-STRING
                       END-IF
                   END-IF
               END-IF
               MOVE    IDX                 TO  XG99-COUNT
           END-IF
      *
           IF      XG99-MSG            =   SPACE
               IF      XG99-COUNT          >   ZERO
                   MOVE    "処理は正常に終了しました"
                                               TO  XG99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                               TO  XG99-MSG
               END-IF
               MOVE    "B01"               TO  MCP-WIDGET
           ELSE
               MOVE    "B11"               TO  MCP-WIDGET
           END-IF
      *
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    "XG99"              TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XG99"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC         SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】"
                                               TO  XG99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】"
                                               TO  XG99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】"
                                               TO  XG99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】"
                                               TO  XG99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】"
                                               TO  XG99-MSG
           END-EVALUATE
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-XGIDCD      NOT =   SPACE
               PERFORM 520-XGIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "XG04"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    XG04-ROW            TO  WRK-LIST-ROW
      *
           INITIALIZE                      XG04
      *
           EVALUATE    FLG-LIST-ROW
               WHEN    1
                   MOVE    ZERO                TO  XG04-ROW
                   MOVE    ZERO                TO  XG04-ROWATTR
               WHEN    OTHER
                   MOVE    WRK-LIST-ROW        TO  XG04-ROW
                   MOVE    ZERO                TO  XG04-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-SELNUM      TO  XG04-SELNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  XG04-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  XG04-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  XG04-SHORI-RENNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  XG04-TNO       (IDX)
               MOVE    SPA-GMN-TTBL-GROUP(IDX) TO  XG04-TTBL-GROUP(IDX)
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  XG04-TRENNUM   (IDX)
               MOVE    SPA-GMN-TSHORI-RENNUM(IDX)
                                               TO
                                               XG04-TSHORI-RENNUM (IDX)
               MOVE    SPA-GMN-TKENSU    (IDX) TO  WRK-KENSU
               MOVE    WRK-KENSU               TO  XG04-TKENSU    (IDX)
               MOVE    SPA-GMN-TSRYYM    (IDX) TO  XG04-TSRYYM    (IDX)
               MOVE    SPA-GMN-TSRYYMD   (IDX) TO  XG04-TSRYYMD   (IDX)
               MOVE    SPA-GMN-TNYUGAIKBN(IDX) TO  XG04-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-TSHORI-TITLE (IDX)  TO
                                               XG04-TSHORI-TITLE  (IDX)
               MOVE    SPA-GMN-TPRT-TITLE(IDX) (1:60)
                                               TO  XG04-TPRT-TITLE(IDX)
               MOVE    SPA-GMN-TPUTFLG   (IDX) TO  XG04-TPUTFLG   (IDX)
      *
               IF      IDX                 =   SPA-NAI-SELNUM
                   MOVE    "T"                 TO  XG04-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  XG04-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  XG04-COUNT
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
          .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の選択番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当のデータはありません"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "該当のデータが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-FILE-MIS-ERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XGERR
           MOVE    SPA-ERRCD           TO  XGERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XGERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    "XGERR"             TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XGERR"             TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-XGIDSET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-XGIDMSG
      *
           EVALUATE    SPA-XGIDCD
               WHEN    "1002"
                   MOVE    "該当データの削除処理を行います"
                                       TO  WRK-XGIDMSG
               WHEN    "1003"
                   MOVE
                   "処理中の可能性がありますが削除処理を行いますか"
                                       TO  WRK-XGIDMSG
               WHEN    "6666"
                   MOVE    "処理中です"
                                       TO  WRK-XGIDMSG
           END-EVALUATE
      *
           INITIALIZE                      XGID1
           MOVE    SPA-XGIDCD          TO  XGID1-ID1CODE
           MOVE    WRK-XGIDMSG         TO  XGID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    "XG04"              TO  SPA-MOTOPG
           MOVE    "XGID1"             TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XGID1"             TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       520-XGIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC             SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *     出力ファイル情報管理読込処理
      *****************************************************************
       900-FILE-MIS-READ-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  FILE-MIS-REC
               MOVE    ZERO                TO  FLG-FILEMIS
           ELSE
               INITIALIZE                      FILE-MIS-REC
               MOVE    1                   TO  FLG-FILEMIS
           END-IF
           .
       900-FILE-MIS-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
