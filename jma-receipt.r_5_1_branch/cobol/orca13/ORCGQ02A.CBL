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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQ02A.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 点数検索画面（Ｑ０２Ａ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/10/07    NACL−太田    新規作成
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
      *    パラメータファイル
           SELECT  BQ01-FILE   ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //PRFNAME//.
      *
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-EOF                             PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-PTSRH                           PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).
           03  FLG-JYOKEN                          PIC 9(01).
           03  FLG-JYOKEN-AND                      PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-JOBEND                          PIC 9(01).
           03  FLG-HKNKOHSEL                       PIC 9(01).
           03  FLG-GMN-INIT                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDXP                                PIC 9(05).
           03  IDX-LEN                             PIC 9(05).
           03  IDX-GMN                             PIC 9(05).
           03  IDX-JOBID                           PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TTL-PTINF                       PIC 9(05).
           03  CNT-JYOKEN                          PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                            PIC 9(08).
           03  WRK-HENYMDG1                        PIC X(09).
           03  WRK-HENYMDG3                        PIC X(22).
           03  WRK-STR                             PIC X(20000).
           03  WRK-STR-LEN                         PIC 9(05).
           03  WRK-STR-PNT                         PIC 9(05).
           03  WRK-STR1                            PIC X(50).
           03  WRK-STR2                            PIC X(50).
           03  WRK-PTINF-ADR-LEN                   PIC 9(05).
           03  WRK-PTINF-BANTI-LEN                 PIC 9(05).
           03  WRK-SQL-PNT                         PIC 9(05).
           03  WRK-SQL                             PIC X(20000).
           03  WRK-SQL-ORDER                       PIC X(500).
           03  WRK-EDPTNUM                         PIC X(20).
           03  WRK-ZZZZ9                           PIC ZZZZ9.
           03  WRK-TOTAL                           PIC ZZZ,ZZZ,ZZ9.
           03  WRK-TOTALTENZ                       PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-JYOKEN-PNT                      PIC 9(05).
           03  WRK-JYOKEN-TBL.
               05  WRK-JYOKEN                      PIC X(01)
                                                   OCCURS  5.
           03  WRK-NUM                             PIC 9(10).
           03  WRK-KEKKALST-NO-ST                  PIC 9(05).
           03  WRK-KEKKALST-NO-ED                  PIC 9(05).
           03  WRK-QIDMSG                          PIC X(80).
           03  WRK-MAE-INPUT               PIC X(400).
           03  WRK-PROC-FLG                        PIC 9(1).
           03  WRK-SHORIERR                        PIC X(200).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1                    PIC 9(04).
               05  WRK-CALC-MM1                    PIC 9(02).
               05  WRK-CALC-DD1                    PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2                    PIC 9(04).
               05  WRK-CALC-MM2                    PIC 9(02).
               05  WRK-CALC-DD2                    PIC 9(02).
           03  WRK-AGE-YY                          PIC S9(04).
           03  WRK-AGE-MM                          PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z                       PIC ---9.
               05  WRK-AGE-MEI                     PIC X(04).
           03  WRK-KJYNYMD                         PIC 9(08).
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-JOBID                           PIC 9(07).
           03  WRK-WINDOW                          PIC X(20).
           03  WRK-DATAKBN                         PIC X(01).
           03  WRK-HKNNUM                          PIC X(03).
      *
           03  WRK-FILENM-AREA.
               07  WRK-FILENM      PIC X(100).
               07  WRK-FILENM-FOLDER 
                                   PIC X(100).
           03  WRK-MOTOPG                          PIC X(08).
           03  WRK-REQUEST-G.
               05  WRK-REQUEST-CSV                 PIC X(01).
               05  WRK-REQUEST-PRINT               PIC X(01).
      *
       01  WRK-ROW-AREA.
           03  WRK-KEKKALST-ROW                    PIC S9(9)   BINARY.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-JYOKENMEI-AREA.
               05  FILLER          PIC X(10)   VALUE   "基本情報".
               05  FILLER          PIC X(10)   VALUE   "保険".
               05  FILLER          PIC X(10)   VALUE   "公費".
               05  FILLER          PIC X(10)   VALUE   "病名".
               05  FILLER          PIC X(10)   VALUE   "診療行為".
           03  CONST-JYOKENMEI-R   REDEFINES   CONST-JYOKENMEI-AREA.
               05  CONST-JYOKENMEI PIC X(10)   OCCURS  5.
           03  CONST-SHELLNAME      PIC X(100)
               VALUE   "ORCBQ01.sh".
           03  CONST-SHELLNAME2     PIC X(100)
               VALUE   "ORCBQ02.sh".
           03  CONST-SHELLNAME3     PIC X(100)
               VALUE   "ORCBQ03.sh".
           03  CONST-LINE                          PIC 9(05)
               VALUE   45.
           03  CONST-JOBID                         PIC 9(07)
               VALUE   1.
           03  CONST-JOBID2                        PIC 9(07)
               VALUE   2.
           03  CONST-SHELLID                   PIC X(08)
               VALUE   "ORCBQ01".
           03  CONST-BTPARA-SHELLID-LNK01          PIC X(08)
               VALUE   "ORCBQL01".
           03  CONST-RECE-MAX      PIC 9(05)   VALUE 2000.
           03  CONST-TOUKEI-MAX    PIC 9(05)   VALUE 9999.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE 200.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療行為照会テーブル
       01  SRYSRH-REC.
           COPY    "CPSRYSRH.INC".
      *
      *    患者情報照会テーブル
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    患者情報テーブル
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴照会テーブル
       01  RRKSRH-REC.
           COPY    "CPRRKSRH.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *    uid取得
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  ST                          PIC 9(2).
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    照会用サブ
           COPY    "CPQSUB01.INC".
           COPY    "CPQSUB02.INC".
           COPY    "CPQSUB06.INC".
      *
      *    患者一覧印刷パラメタ
           COPY    "CPORCHC28.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    最終受診日取得
           COPY    "CPORCSLASTYMD.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    バッチジョブ制御
           COPY    "CPORCSMONBATCH.INC".
      *     
           COPY    "ORCA-DBMETA".
      *
           COPY    "ORCA-BLOB".
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
           COPY    "ORCA13SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
      *    エラー画面より
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      **** 暫定 ****
      *
               IF  ( SPA-ERRCD                 =   "9999" )
                   MOVE    "検索条件が多すぎます。"
                                   TO  SPA-GMN-Q02-TNAME (16)
                   MOVE    16      TO  SPA-GMN-Q02-KEKKA-MAX
               END-IF
      **************
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q02A"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-Q02-CUR
      *
      *    他のＬＤより
           IF  ( LINKAREA      NOT =   SPACE )
               MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG  TO  WRK-MOTOPG
      *
               IF    ( SPA-MOTOPG  =           "P02"
                                           OR  "C02"
                                           OR  "K02"
                                           OR  "K02N" )
                   CONTINUE
               ELSE
                   MOVE    LNK-COMMON  TO  SPA-KYOUTU
               END-IF
      *
               MOVE    WRK-MOTOPG  TO  SPA-MOTOPG
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    処理確認画面
           WHEN    "Q97"
               PERFORM 3001-Q97SPASET-SEC
               GO  TO      300-SCREEN-EXT
      *    出力指示画面
           WHEN    "Q100"
               PERFORM 3001-Q100SPASET-SEC
               GO  TO      300-SCREEN-EXT
      *    明細書連携確認画面
           WHEN    "Q03"
               PERFORM 3001-Q03SPASET-SEC
               GO  TO      300-SCREEN-EXT
      *    確認メッセージ画面
           WHEN    "QID1"
               PERFORM 3001-QID1SPASET-SEC
               GO  TO      300-SCREEN-EXT
      *    情報削除画面
           WHEN    "XB01"
               MOVE    LNK-COMMON          TO  SPA-KYOUTU
               MOVE    LNK-SCRSPA          TO  SPA-Q01FREE
               GO  TO      300-SCREEN-EXT
      *    予約登録
           WHEN    "Y01"
      *    受付一覧
           WHEN    "U01"
               MOVE    LNK-COMMON          TO  SPA-KYOUTU
               MOVE    LNK-SCRSPA          TO  SPA-Q01FREE
               GO  TO      300-SCREEN-EXT
      *    患者登録
           WHEN    "P02"
           WHEN    "C02"
           WHEN    "K02"
           WHEN    "K02N"
      *
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  QSUB01-LNK
               MOVE    "I"             TO  QSUB01-KBN
               CALL    "ORCGQSUB01"    USING   QSUB01-LNK
                                               SPA-AREA
      *
               MOVE    QSUB01-SPA-AREA TO  SPA-Q01FREE
      *
               GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-Q00-QID-AREA
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       3001-Q97SPASET-SEC             SECTION.
      *
           IF    ( SPA-QID1-FLG    =   "SV" )
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    CONST-SHELLID
                                       TO  SFILESV-TBL-KEY
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *
           MOVE    SPACE           TO  SPA-QID1-FLG
           MOVE    SPACE           TO  SPA-QIDCD
           MOVE    SPACE           TO  SPA-Q97-MOTOPG
      *
           .
       3001-Q97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＩＤ１戻りスパ編集処理
      *****************************************************************
       3001-QID1SPASET-SEC             SECTION.
      *
      *
           IF    ( SPA-QID1-FLG    =   "OK" )
               CONTINUE
           ELSE
               INITIALIZE  SPA-Q00-QID-AREA
               GO  TO  3001-QID1SPASET-EXT
           END-IF
      *
           EVALUATE    SPA-QIDCD
           WHEN    "6666"
               MOVE    "JOIN"      TO  MCP-PUTTYPE
               PERFORM 210-BACK-SYORI-SEC
      *
           END-EVALUATE
      *
           INITIALIZE  SPA-Q00-QID-AREA
           .
      *
       3001-QID1SPASET-EXT.
           EXIT.
      *****************************************************************
      *    Ｑ１００戻りスパ編集処理
      *****************************************************************
       3001-Q100SPASET-SEC             SECTION.
      *
           IF  ( SPA-Q02-Q100-RTOK-FLG =   ZERO )
               GO  TO  3001-Q100SPASET-EXT
           END-IF
      *
           PERFORM 900-PTSRH-INS2-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  QSUB06-LNK
      *
           MOVE    SPA-Q02-Q100-FILENAME
                                       TO  QSUB06-CSVFILENAME
           MOVE    SPA-GMN-Q02-HYODAI  TO  QSUB06-HYODAI
           MOVE    SPA-GMN-Q02-JYOKEN1 TO  QSUB06-JYOKEN1
           MOVE    SPA-GMN-Q02-JYOKEN2 TO  QSUB06-JYOKEN2
           MOVE    SPA-Q02-Q100-STPAGE TO  QSUB06-STPAGE
           MOVE    SPA-Q02-Q100-EDPAGE TO  QSUB06-EDPAGE
           MOVE    SPA-Q02-Q100-STKNS  TO  QSUB06-STKNS
           MOVE    SPA-Q02-Q100-EDKNS  TO  QSUB06-EDKNS
      *
      *    年齢
           IF  (   SPA-Q01-AGE-CALC-KBN    =   ZERO )
               MOVE    SPA-SYSYMD      TO  QSUB06-KJYNYMD
           ELSE
               MOVE    SPA-Q01-KJYNYMD TO  QSUB06-KJYNYMD
           END-IF
      *
      *    診療年月
           MOVE    SPA-Q01-STSRYYMD    TO  QSUB06-SRYYM
      *
      *     PERFORM 3101-SQL-EDIT-SEC
           INITIALIZE  QSUB02-LNK
           CALL    "ORCGQSUB02"    USING
                                   SPA-AREA
                                   SPA-Q01FREE
                                   QSUB02-LNK
      *
           MOVE    QSUB02-SQL-ORDER
                                   TO  QSUB06-SQL
      *
           MOVE    SPA-GMN-Q02-SELHMS
                                   TO  SPA-Q97-SELHMS
           MOVE    ZERO            TO  SPA-Q97-FILESV-CHK-FLG
      *
           MOVE    SPACE           TO  WRK-FILENM-AREA
           PERFORM VARYING IDX4    FROM    100 BY  -1
                   UNTIL   IDX4     <       1
               IF      ( SPA-Q02-Q100-FILENAME (IDX4:1)
                                               =   "/" )
                   MOVE    SPA-GMN-Q100-FILENAME (1:IDX4)
                                           TO  WRK-FILENM-FOLDER
                   COMPUTE IDX5    =   IDX4    +   1
                   MOVE    SPA-GMN-Q100-FILENAME (IDX5:)
                                           TO  WRK-FILENM
                   MOVE    1               TO  IDX4
               END-IF
           END-PERFORM
           IF      WRK-FILENM          =   SPACE
               MOVE    "/tmp/"             TO  WRK-FILENM-FOLDER
               MOVE    SPA-Q02-Q100-FILENAME
                                           TO  WRK-FILENM
           END-IF             
      *
      *    ファイル保存情報削除
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    CONST-SHELLID
                                   TO  SFILESV-TBL-KEY
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
      *
      *    ジョブ開始セット処理
           MOVE    CONST-JOBID     TO  WRK-JOBID
           PERFORM 900-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
           MOVE    CONST-SHELLNAME TO  QSUB06-SHELLNAME
           MOVE    CONST-JOBID     TO  QSUB06-JOBID
           MOVE    CONST-SHELLID   TO  QSUB06-SHELLID
      *
           MOVE    ZERO            TO  QSUB06-REQUEST-G
           IF    ( SPA-Q02-Q100-PROC-FLG    =   "0" )
               MOVE    "1"         TO  QSUB06-REQUEST-CSV
           ELSE
               MOVE    "1"         TO  QSUB06-REQUEST-PRINT
           END-IF
           MOVE    SPA-Q02-Q100-FILE
                                   TO  QSUB06-FILESVKBN
           MOVE    SPA-GMN-Q02-SELHMS
                                   TO  QSUB06-SELHMS
           MOVE    SPA-Q02-Q100-CDKBN
                                   TO  QSUB06-ENCODEKBN
           MOVE    WRK-FILENM-FOLDER
                                   TO  QSUB06-DIRNAME
           MOVE    WRK-FILENM      TO  QSUB06-FILENAME
      *
      *    検索結果パターン
           MOVE    "2"             TO  QSUB06-OUTKBN
      *
           CALL    "ORCGQSUB06"    USING
                                   QSUB06-LNK
                                   SPA-AREA
      *
           IF    ( SPA-Q02-Q100-FILE   =   "1" )
               MOVE    "SV"        TO  SPA-QID1-FLG
           ELSE
               MOVE    SPACE       TO  SPA-QID1-FLG
           END-IF
           MOVE    SPACE           TO  SPA-QIDCD
      *
           PERFORM 490-KAKUNIN-SEC
      *
           .
       3001-Q100SPASET-EXT.
           EXIT.
      *****************************************************************
      *    Ｑ０３戻りスパ編集処理
      *****************************************************************
       3001-Q03SPASET-SEC             SECTION.
      *
           IF  ( SPA-Q02-Q03-RTOK-FLG =   ZERO )
               GO  TO  3001-Q03SPASET-EXT
           END-IF
      *
           PERFORM 900-PTSRH-INS2-SEC
      *
           MOVE    SPA-HOSPNUM         TO  PRFNAME-HOSPNUM
           MOVE    "OR013002"          TO  PRFNAME-FILE-ID
           MOVE    2                   TO  PRFNAME-SYOKBN1
           MOVE    SPA-TERMID          TO  PRFNAME-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    PRFNAME-BASENAME    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGNPARA
           MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR013ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    2                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
           OPEN    OUTPUT              BQ01-FILE
      *
           INITIALIZE                  BQ01
      *
           MOVE    SPA-GMN-Q02-HYODAI  TO  BQ01-HYODAI
           MOVE    SPA-GMN-Q02-JYOKEN1 TO  BQ01-JYOKEN1
           MOVE    SPA-GMN-Q02-JYOKEN2 TO  BQ01-JYOKEN2
      *
           MOVE    SPA-SYSYMD          TO  BQ01-KJYNYMD
           MOVE    SPA-Q02-Q03-SRYYM   TO  BQ01-SRYYM
           MOVE    SPA-Q02-Q03-NYUGAIKBN TO BQ01-NYUGAIKBN
           MOVE    SPA-Q02-Q03-LNKKBN  TO  BQ01-LNKKBN
      *
      *     PERFORM 3101-SQL-EDIT-SEC
           INITIALIZE  QSUB02-LNK
           CALL    "ORCGQSUB02"    USING
                                   SPA-AREA
                                   SPA-Q01FREE
                                   QSUB02-LNK
           MOVE    QSUB02-SQL-ORDER
                                       TO  BQ01-SQL
      *
           WRITE   BQ01
      *
           CLOSE   BQ01-FILE
      *
      *    ジョブ開始セット処理
           MOVE    CONST-JOBID2    TO  WRK-JOBID
           PERFORM 900-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
      *
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  SHELLTBL
      *
           MOVE    CONST-SHELLNAME2 TO  SHELLTBL-NAME
           MOVE    CONST-JOBID2     TO  SHELLTBL-JOBID
           MOVE    CONST-SHELLID    TO  SHELLTBL-SHELLID
      *
           MOVE    ASGNPARA(ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG1
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG2
           MOVE    SPA-SYSYMD      TO  SHELLTBL-ARG3
           MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG5
           MOVE    SPA-OPID        TO  SHELLTBL-ARG6
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG7
           MOVE    CONST-JOBID2    TO  SHELLTBL-ARG8
      *    IDは検索結果印刷に併せる
           MOVE    CONST-SHELLID
                                   TO  SHELLTBL-ARG9
           IF    ( SPA-Q02-Q03-LNKKBN  =   "1" )
               MOVE    CONST-SHELLID
                                   TO  SHELLTBL-ARG10
           ELSE
               MOVE    CONST-BTPARA-SHELLID-LNK01
                                   TO  SHELLTBL-ARG10
           END-IF
      *
           INITIALIZE                  ORCA-BLOB
           MOVE    ASGNPARA        TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT
                                   TO  SHELLTBL-ARG11
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM 490-KAKUNIN-SEC
      *
      *
           .
       3001-Q03SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           MOVE    SPACE           TO  SPA-Q02
      *
           INITIALIZE                  SPA-Q02
      *
           MOVE    ZERO            TO  SPA-GMN-Q02-TOTAL-GET-FLG
      *
           MOVE    SPA-Q01-HYODAI  TO  SPA-GMN-Q02-HYODAI
      *
      *    画面条件編集処理
           PERFORM 3101-JYOKEN-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  SPA-GMN-Q02-SELYMD
           MOVE    SMCNDATE-HMS    TO  SPA-GMN-Q02-SELHMS
      *
      *    ＳＱＬ文編集処理
           INITIALIZE              QSUB02-LNK
           MOVE    SMCNDATE-YMD    TO  QSUB02-YMD
           MOVE    SMCNDATE-HMS    TO  QSUB02-HMS
           CALL    "ORCGQSUB02"    USING
                                   SPA-AREA
                                   SPA-Q01FREE
                                   QSUB02-LNK
           MOVE     20000          TO   WRK-STR-LEN
           MOVE     QSUB02-SQL     TO   WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           IF  ( WRK-STR-LEN       >    15000 )
      *
      *        ＳＱＬ解析限界値オーバー
               MOVE    "9999"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-Q02-MODE
           ELSE
      *
               INITIALIZE                  SMONBATCH-AREA
               MOVE    "ORCBQ03"       TO  SMONBATCH-SHELLID
               MOVE    SPA-TERMID      TO  SMONBATCH-TERMID
               MOVE    SPA-OPID        TO  SMONBATCH-OPID
               MOVE    "DEL"           TO  SMONBATCH-MODE
               CALL    "ORCSMONBATCH"      USING
                                           SMONBATCH-AREA
                                           SPA-AREA
      *
               INITIALIZE              UIDIO-AREA
               CALL    "orcuidset"     USING
                                       ST
                                       UIDIO-AREA
      *
               MOVE    SPA-HOSPNUM         TO  PRFNAME-HOSPNUM
               MOVE    "OR013003"          TO  PRFNAME-FILE-ID
               MOVE    2                   TO  PRFNAME-SYOKBN1
               MOVE    ALL "0"             TO  PRFNAME-TERMID
               STRING  C-UID               DELIMITED   BY  SIZE
               INTO    PRFNAME-TERMID
               END-STRING
      *
               INITIALIZE                      SGETTEMP-AREA
               MOVE    PRFNAME-BASENAME    TO  SGETTEMP-BASENAMES (1)
               CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                           TO  ASGNPARA
               MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *
               OPEN    OUTPUT              BQ01-FILE
               INITIALIZE                  BQ01
               MOVE    QSUB02-SQL      TO  BQ01-SQL
               MOVE    QSUB02-SQL-ORDER
                                       TO  BQ01-SQL-ORDER
               MOVE    SPA-Q01KYOUTU   TO  BQ01-Q01KYOUTU
               WRITE   BQ01
               CLOSE   BQ01-FILE
      *
               MOVE    SPACE           TO  SHELLTBL
               INITIALIZE                  SHELLTBL
      *
               MOVE    CONST-SHELLNAME3 TO  SHELLTBL-NAME
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
               MOVE    "ORCBQ03"       TO  SHELLTBL-SHELLID
               MOVE    1               TO  SHELLTBL-JOBID
               MOVE    SPA-OPID        TO  SHELLTBL-RUN-OPID
               MOVE    SPA-TERMID      TO  SHELLTBL-TERMID
      *
               MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG3
               MOVE    SMCNDATE-YMD    TO  SHELLTBL-ARG4
               MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG5
               MOVE    SPA-OPID        TO  SHELLTBL-ARG6
               MOVE    SPACE           TO  SHELLTBL-ARG7
      *
      **       ARG8は APIで使用
      *
               INITIALIZE                  ORCA-BLOB
               MOVE    ASGNPARA        TO  ORCA-BLOB-FILE
               PERFORM 910-BLOBIMPORT-SEC
               MOVE    ORCA-BLOB-OBJECT
                                       TO  SHELLTBL-ARG9
      *
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "allways"       TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
      *    カーソルキー制御項目
           MOVE   1                TO  SPA-GMN-Q02-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-Q02-GMN-EDIT-SEC            SECTION.
      *
      *    総件数編集
           IF  ( SPA-GMN-Q02-TOTAL-GET-FLG =   ZERO )
               MOVE    1              TO   SPA-GMN-Q02-TOTAL-GET-FLG
               PERFORM 3101-TOTAL-KNS-EDIT-SEC
           END-IF
      *
           MOVE   1                TO  FLG-GMN-INIT
      *
           MOVE    SPACE           TO  SPA-GMN-Q02-KEKKATBL
                                       SPA-NAI-Q02-KEKKATBL
      *
           INITIALIZE                  SPA-GMN-Q02-KEKKATBL
                                       SPA-NAI-Q02-KEKKATBL
      *
      *    ＳＱＬ文編集処理
           INITIALIZE              QSUB02-LNK
           MOVE    1               TO  QSUB02-LIMITCONTROL
           COMPUTE QSUB02-OFFSET
               =   SPA-GMN-Q02-SKIP-KNS
           COMPUTE QSUB02-LIMIT
               =   CONST-LINE-MAX  +   1
           CALL    "ORCGQSUB02"    USING
                                   SPA-AREA
                                   SPA-Q01FREE
                                   QSUB02-LNK
      *
           MOVE    QSUB02-SQL-ORDER    TO  WRK-SQL-ORDER
      *
      *    患者マスタ検索処理
           PERFORM 900-PTSRH-ODR-SEL-SEC
      *
           PERFORM VARYING IDX-GMN FROM    1   BY  1
               UNTIL   (   IDX-GMN     >   CONST-LINE-MAX )
                OR     (   FLG-PTSRH   =   1   )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *        番号
               COMPUTE WRK-NUM     =   SPA-GMN-Q02-SKIP-KNS
                                   +   IDX-GMN
               MOVE    WRK-NUM     TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-Q02-TNO
                                                           (IDX-GMN)
      *
      *        患者番号
               MOVE    PTSRH-PTNUM
                                   TO  SPA-GMN-Q02-TPTNUM
                                                           (IDX-GMN)
      *
      *        氏名
               MOVE    PTINF-NAME
                                   TO  SPA-GMN-Q02-TNAME
                                                           (IDX-GMN)
      *        性別
               EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"    TO  SPA-GMN-Q02-TSEX
                                                           (IDX-GMN)
               WHEN    "2"
                   MOVE    "女"    TO  SPA-GMN-Q02-TSEX
                                                           (IDX-GMN)
               END-EVALUATE
      *
      *        誕生日
               INITIALIZE              ORCSGDAYAREA
      *
               MOVE    PTINF-BIRTHDAY
                                   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY
                               TO  SPA-GMN-Q02-TBIRTH (IDX-GMN)
               ELSE
                   MOVE    SPACE
                               TO  SPA-GMN-Q02-TBIRTH (IDX-GMN)
               END-IF
      *
      *        年齢
               IF  (   SPA-Q01-AGE-CALC-KBN    =   ZERO )
                   MOVE    SPA-SYSYMD      TO  WRK-KJYNYMD
               ELSE
                   MOVE    SPA-Q01-KJYNYMD TO  WRK-KJYNYMD
               END-IF
      *
               IF  (   PTINF-BIRTHDAY    <   WRK-KJYNYMD )
                   MOVE    PTINF-BIRTHDAY
                                           TO  WRK-CALC-YMD1
                   MOVE    WRK-KJYNYMD     TO  WRK-CALC-YMD2
                   PERFORM 3101-AGE-CALC-SEC
               ELSE
                   MOVE    WRK-KJYNYMD     TO  WRK-CALC-YMD1
                   MOVE    PTINF-BIRTHDAY
                                           TO  WRK-CALC-YMD2
                   PERFORM 3101-AGE-CALC-SEC
                   COMPUTE WRK-AGE-YY   =   WRK-AGE-YY
                                           *   -1
                   COMPUTE WRK-AGE-MM   =   WRK-AGE-MM
                                           *   -1
               END-IF
      *
               IF    ( WRK-AGE-YY          =   ZERO )
                   MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
                   MOVE    "ヶ月"          TO  WRK-AGE-MEI
               ELSE
                   MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
                   MOVE    "歳"            TO  WRK-AGE-MEI
               END-IF
      *
               MOVE    WRK-AGE-G           TO  SPA-GMN-Q02-TAGE
                                                               (IDX-GMN)
      *        入外区分
               MOVE    PTSRH-NYUGAIKBN TO  SPA-GMN-Q02-TNYUGAI
                                                               (IDX-GMN)
      *
      *        前回来院日編集
               INITIALIZE                  ORCSLASTYMDAREA
               MOVE    PTINF-PTID      TO  ORCSLASTYMD-PTID
               CALL    "ORCSLASTYMD"       USING
                                           ORCSLASTYMDAREA
                                           SPA-AREA
               IF    ( ORCSLASTYMD-BRMNUM  NOT =   SPACE )
                   MOVE    "1"     TO  SPA-NAI-Q02-TNYUINKBN (IDX-GMN)
               END-IF
      *
      *        保険
               IF    ( PTSRH-HKNNUM    NOT =   SPACE )
                   MOVE    PTSRH-HKNNUM    TO  WRK-HKNNUM
                   PERFORM 900-HKNNUM-KEY5-SEL-SEC
                   MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-Q02-THKN
                                                               (IDX-GMN)
               END-IF
      *        公費１
               IF    ( PTSRH-KOHNUM1   NOT =   SPACE )
                   MOVE    PTSRH-KOHNUM1   TO  WRK-HKNNUM
                   PERFORM 900-HKNNUM-KEY5-SEL-SEC
                   MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-Q02-TKOH1
                                                               (IDX-GMN)
               END-IF
      *        公費２
               IF    ( PTSRH-KOHNUM2   NOT =   SPACE )
                   MOVE    PTSRH-KOHNUM2   TO  WRK-HKNNUM
                   PERFORM 900-HKNNUM-KEY5-SEL-SEC
                   MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-Q02-TKOH2
                                                               (IDX-GMN)
               END-IF
      *
      *        点数
               MOVE    PTSRH-TOTALTEN  TO  SPA-GMN-Q02-TTOTALTEN
                                                               (IDX-GMN)
      *
      *        患者ＩＤ
               MOVE    PTINF-PTID
                                   TO  SPA-NAI-Q02-TPTID (IDX-GMN)
      *
               MOVE    IDX-GMN     TO  SPA-GMN-Q02-KEKKA-MAX
      *
               PERFORM 900-PTSRH-ODR-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE   FLG-PTSRH           TO  SPA-GMN-Q02-FLG-PTINF
      *    前頁ボタン活性編集
           IF  ( SPA-GMN-Q02-SKIP-KNS      >   0 )
               MOVE    WIDGET-NORMAL       TO  SPA-GMN-Q02-B06-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-GMN-Q02-B06-STATE
           END-IF
      *    次頁ボタン活性編集
           IF  ( SPA-GMN-Q02-FLG-PTINF     =   0 )
               MOVE    WIDGET-NORMAL       TO  SPA-GMN-Q02-B07-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-GMN-Q02-B07-STATE
           END-IF
      *
           .
       3101-Q02-GMN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       3101-AGE-CALC-SEC               SECTION.
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
           .
      *
       3101-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    条件編集処理
      *****************************************************************
       3101-JYOKEN-EDIT-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-Q02-JYOKEN1
                                       SPA-GMN-Q02-JYOKEN2
                                       WRK-JYOKEN-TBL
           MOVE    1               TO  WRK-JYOKEN-PNT
           MOVE    1               TO  IDXP
      *
           MOVE    SPA-Q01-STSRYYMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           STRING  WRK-HENYMDG3(1:16)      DELIMITED BY SIZE
           INTO    SPA-GMN-Q02-JYOKEN1
           WITH    POINTER IDXP
           END-STRING
      *
           MOVE    SPA-Q01-STTOTALTEN    TO  WRK-TOTALTENZ
           UNSTRING     WRK-TOTALTENZ      DELIMITED BY ALL SPACE
           INTO         WRK-STR1
                        WRK-STR2
           END-UNSTRING
      *
           IF    ( WRK-STR2    NOT =   SPACE )
               MOVE    WRK-STR2    TO  WRK-STR1
           END-IF
      *
           STRING  "　"            DELIMITED   BY  SIZE
                   WRK-STR1        DELIMITED   BY  SPACE
                   "点以上"        DELIMITED   BY  SIZE
           INTO    SPA-GMN-Q02-JYOKEN1
           WITH    POINTER IDXP
           END-STRING
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    SPA-GMN-Q02-JYOKEN1
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  SPA-GMN-Q02-JYOKEN1
      *
           IF  ( SPA-Q01-GTBL (1)  =   "1" )
            OR ( SPA-Q01-GTBL (2)  =   "1" )
            OR ( SPA-Q01-GTBL (3)  =   "1" )
            OR ( SPA-Q01-GTBL (7)  =   "1" )
            OR ( SPA-Q01-GTBL (8)  =   "1" )
            OR ( SPA-Q01-GTBL (10) =   "1" )
            OR ( SPA-Q01-GTBL (12) =   "1" )
               MOVE    "1"         TO  WRK-JYOKEN (1)
           END-IF
      *
           IF  ( SPA-Q01-GTBL (4)  =   "1" )
               MOVE    "1"         TO  WRK-JYOKEN (4)
           END-IF
      *
           IF  ( SPA-Q01-GTBL (5)  =   "1" )
               MOVE    "1"         TO  WRK-JYOKEN (2)
           END-IF
      *
           IF  ( SPA-Q01-GTBL (6)  =   "1" )
               MOVE    "1"         TO  WRK-JYOKEN (3)
           END-IF
      *
           IF  ( SPA-Q01-GTBL (9)  =   "1" )
            OR ( SPA-Q01-GTBL (11) =   "1" )
               MOVE    "1"         TO  WRK-JYOKEN (5)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF   ( WRK-JYOKEN (IDX1)        =   "1" )
                   IF   ( SPA-GMN-Q02-JYOKEN2  =   SPACE )
                       STRING  CONST-JYOKENMEI (IDX1)
                                                   DELIMITED   BY  SPACE
                       INTO    SPA-GMN-Q02-JYOKEN2
                       WITH    POINTER     WRK-JYOKEN-PNT
                       END-STRING
                   ELSE
                       STRING  "、"                DELIMITED   BY  SIZE
                               CONST-JYOKENMEI (IDX1)
                                                   DELIMITED   BY  SPACE
                       INTO    SPA-GMN-Q02-JYOKEN2
                       WITH    POINTER     WRK-JYOKEN-PNT
                       END-STRING
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       3101-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    総件数編集処理
      *****************************************************************
       3101-TOTAL-KNS-EDIT-SEC             SECTION.
      *
      *    検索結果件数を取得
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "1"                 TO  PTSRH-DATAKBN
           MOVE    SPA-GMN-Q02-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-GMN-Q02-SELHMS  TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "cnt"               TO  MCP-PATHNAME
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF  (   MCP-RC              =   ZERO    )
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "cnt"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    PTSRH-PTID          TO  WRK-TOTAL
                                           SPA-GMN-Q02-TOTAL-NUM
      *
           MOVE    PTSRH-TOTALTEN      TO  SPA-GMN-Q02-NINZU
      *
      *    左詰で編集を行う
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( IDX1    >   11 )
                OR   ( WRK-TOTAL (IDX1 : 1)    NOT =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF  ( IDX1         <=   11 )
               MOVE    SPACE   TO  SPA-GMN-Q02-TOTAL
               STRING  "総件数："          DELIMITED   BY    SIZE
                       WRK-TOTAL (IDX1 : ) DELIMITED   BY    SIZE
               INTO    SPA-GMN-Q02-TOTAL
               END-STRING
           END-IF
      *
           .
      *
       3101-TOTAL-KNS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE        MCP-WIDGET      TO      WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 480-CLEAR-SEC
      *    他業務へ
               WHEN    "CLICKED"       ALSO    "B05"
                   MOVE    "P02"       TO      WRK-WINDOW
                   PERFORM 480-GYOUMU-SENI-SEC
               WHEN    "CLICKED"       ALSO    "B05S"
                   MOVE    "K02"       TO      WRK-WINDOW
                   PERFORM 480-GYOUMU-SENI-SEC
               WHEN    "CLICKED"       ALSO    "B06S"
                   MOVE    "C02"       TO      WRK-WINDOW
                   PERFORM 480-GYOUMU-SENI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 210-MAE-PAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 210-TUGI-PAGE-SEC
      *    他業務連携
               WHEN    "CLICKED"       ALSO    "B09"
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 480-Q03-SENI-SEC
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-JYOUHOU-DEL-SEC
      *    ＣＳＶ確認
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 480-CSV-KAKUNIN-SEC
      *    印刷確認
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 480-PRINT-KAKUNIN-SEC
      *    処理確認画面
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 490-KAKUNIN-SEC
      *    検索中確認
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B04"
                   IF    ( SPA-GMN-Q02-MODE    =   ZERO )
                       PERFORM 490-JOUTAI-SEC
                   END-IF
           END-EVALUATE
      *
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE        MCP-WIDGET      TO      WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    行選択
           WHEN    ANY             ALSO    "KEKKALST"
               PERFORM 4102-KEKKALSTSEL-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
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
           MOVE    ZERO        TO      SPA-GMN-Q02-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    表題チェック
           PERFORM 4100-HYODAI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    条件上段チェック
           PERFORM 4101-JYOKEN1-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    条件下段チェック
           PERFORM 4102-JYOKEN2-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    選択番号チェック
           PERFORM 4102-SELUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    表題
           MOVE    Q02A-HYODAI      TO  SPA-GMN-Q02-HYODAI
      *    条件１
           MOVE    Q02A-JYOKEN1     TO  SPA-GMN-Q02-JYOKEN1
      *    条件２
           MOVE    Q02A-JYOKEN2     TO  SPA-GMN-Q02-JYOKEN2
      *    選択番号
           MOVE    Q02A-SELNUM      TO  SPA-GMN-Q02-SELNUM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    表題チェック
      *****************************************************************
       4100-HYODAI-CHK-SEC         SECTION.
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-Q02-HYODAI
                                       TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    1               TO  SPA-GMN-Q02-CUR
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO  4100-HYODAI-CHK-EXIT
           END-IF
      *
           .
       4100-HYODAI-CHK-EXIT.
           EXIT.
      *****************************************************************
      *    条件上段チェック
      *****************************************************************
       4101-JYOKEN1-CHK-SEC        SECTION.
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-Q02-JYOKEN1
                                       TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    2               TO  SPA-GMN-Q02-CUR
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO  4101-JYOKEN1-CHK-EXT
           END-IF
      *
           .
       4101-JYOKEN1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    条件下段チェック
      *****************************************************************
       4102-JYOKEN2-CHK-SEC        SECTION.
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-Q02-JYOKEN2
                                       TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    3               TO  SPA-GMN-Q02-CUR
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO  4102-JYOKEN2-CHK-EXT
           END-IF
      *
           .
       4102-JYOKEN2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4102-SELUM-CHK-SEC          SECTION.
      *
      *    未入力時はチェックを行わない
           IF  ( SPA-GMN-Q02-SELNUM    =   ZERO )
               GO  TO  4102-SELUM-CHK-EXT
           END-IF
      *
      *    選択番号範囲チェック
           COMPUTE WRK-KEKKALST-NO-ST  =   SPA-GMN-Q02-SKIP-KNS
                                       +   1
      *
           COMPUTE WRK-KEKKALST-NO-ED  =   SPA-GMN-Q02-SKIP-KNS
                                       +   SPA-GMN-Q02-KEKKA-MAX
      *
           IF      ( SPA-GMN-Q02-SELNUM   >=   WRK-KEKKALST-NO-ST )
            AND    ( SPA-GMN-Q02-SELNUM   <=   WRK-KEKKALST-NO-ED )
      *
               COMPUTE SPA-GMN-Q02-KEKKA-SEL
                   =   SPA-GMN-Q02-SELNUM  -  SPA-GMN-Q02-SKIP-KNS
      *
           ELSE
               MOVE    4               TO  SPA-GMN-Q02-CUR
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  4102-SELUM-CHK-EXT
           END-IF
      *
           .
       4102-SELUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4102-KEKKALSTSEL-SEC        SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >       CONST-LINE-MAX )
               IF      (  Q02A-KEKKALST-SELECT (IDX1)  =   "T"  )
                   COMPUTE SPA-GMN-Q02-SELNUM
                                   =   IDX1
                                   +   SPA-GMN-Q02-SKIP-KNS
                   MOVE    IDX1            TO  SPA-GMN-Q02-KEKKA-SEL
                   MOVE    CONST-LINE-MAX  TO  IDX1
               END-IF
           END-PERFORM
      *
           .
       4102-KEKKALSTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "6666"          TO  SPA-QIDCD
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  210-BACK-EXT
           END-IF
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 210-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      * 
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       210-MAE-PAGE-SEC                SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF  ( SPA-GMN-Q02-SKIP-KNS      >   0 )
               COMPUTE SPA-GMN-Q02-SKIP-KNS    =   SPA-GMN-Q02-SKIP-KNS
                                               -   CONST-LINE-MAX
               PERFORM 3101-Q02-GMN-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q02-SELNUM
           END-IF
      *
           .
       210-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       210-TUGI-PAGE-SEC               SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF  ( SPA-GMN-Q02-FLG-PTINF     =   0 )
               COMPUTE SPA-GMN-Q02-SKIP-KNS    =   SPA-GMN-Q02-SKIP-KNS
                                               +   CONST-LINE-MAX
               PERFORM 3101-Q02-GMN-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q02-SELNUM
           END-IF
      *
           .
       210-TUGI-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    情報削除へ
      *****************************************************************
       480-JYOUHOU-DEL-SEC             SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    画面移行用のパラメタ変更
           MOVE    "Q02"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC              SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    画面移行用のパラメタ変更
           MOVE    "Q02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "Q02"               TO  SPA-MOTOPG3
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       480-CLEAR-SEC               SECTION.
      *
           INITIALIZE              SPA-GMN-Q02-HYODAI
                                   SPA-GMN-Q02-JYOKEN1
                                   SPA-GMN-Q02-JYOKEN2
                                   SPA-GMN-Q02-SELNUM
                                   SPA-GMN-Q02-KEKKA-SEL
      *
           MOVE    1               TO  SPA-GMN-Q02-CUR
      *
           .
       480-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    他業務へ
      *****************************************************************
       480-GYOUMU-SENI-SEC             SECTION.
      *
           MOVE    SPA-SYSYMD      TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-SRYYMDW
      *
      *    入力時チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  ( SPA-ERRCD         NOT =   SPACE )
               GO  TO  480-GYOUMU-SENI-EXT
           END-IF
      *
      *    画面遷移時のチェック処理
           PERFORM 4801-GAMEN-SENIJI-CHK-SEC
           IF  ( SPA-ERRCD         NOT =   SPACE )
               GO  TO  480-GYOUMU-SENI-EXT
           END-IF
      *
           COMPUTE IDX1    =   SPA-GMN-Q02-SELNUM
                           -   SPA-GMN-Q02-SKIP-KNS
      *
           IF  SPA-GMN-Q02-TPTNUM (IDX1)  =  SPACE
               GO  TO  480-GYOUMU-SENI-EXT
           END-IF
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  QSUB01-LNK
           MOVE    "O"                 TO  QSUB01-KBN
           MOVE    SPA-Q01FREE         TO  QSUB01-SPA-AREA
           CALL    "ORCGQSUB01"        USING
                                       QSUB01-LNK
                                       SPA-AREA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *
           MOVE    SPA-NAI-Q02-TPTID (IDX1)
                                       TO  SPA-PTID
           MOVE  SPA-GMN-Q02-TPTNUM (IDX1)
                                       TO  SPA-PTNUM
      *
           IF    ( WRK-WINDOW          =   "K02" )
               IF    ( SPA-NAI-Q02-TNYUINKBN (IDX1)    =   "1" )
                   MOVE    "K02N"      TO  WRK-WINDOW
               END-IF
           END-IF
      *
      *    受付と患者検索以外の元をセット
           MOVE    "Q02"               TO  SPA-MOTOPG
           MOVE    WRK-WINDOW          TO  SPA-SAKIPG
           IF    ( WRK-WINDOW          =   "K02" OR "K02N" )
               MOVE    SPA-MOTOPG      TO  SPA-MACHIPG
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   WRK-WINDOW           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-GYOUMU-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Q01FREE         TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "Q02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細書連携確認画面
      *****************************************************************
       480-Q03-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  480-Q03-SENI-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "7777"          TO  SPA-ERRCD
               MOVE    SPACE           TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  480-Q03-SENI-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-PROC-FLG
      *
           PERFORM 4801-Q03-SENI-SEC
      *
           .
       480-Q03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    明細書連携確認画面
      *****************************************************************
       4801-Q03-SENI-SEC               SECTION.
      *
           MOVE    SPACE           TO  SPA-Q02-Q03-SENI-AREA
           INITIALIZE                  SPA-Q02-Q03-SENI-AREA
      *
           IF    ( WRK-MCP-WIDGET  =   "B09" )
               MOVE    "1"         TO  SPA-Q02-Q03-LNKKBN
           ELSE
               MOVE    "2"         TO  SPA-Q02-Q03-LNKKBN
           END-IF
      *
           MOVE    SPA-Q01-NYUGAIKBN
                                   TO  SPA-Q02-Q03-NYUGAIKBN
           EVALUATE    TRUE
           WHEN  ( SPA-Q01-EDSRYYMD    NOT =   SPACE )
               MOVE    SPA-Q01-EDSRYYMD (1 : 6)
                                   TO  SPA-Q02-Q03-SRYYM
           WHEN  ( SPA-Q01-STSRYYMD    NOT =   SPACE )
               MOVE    SPA-Q01-STSRYYMD (1 : 6)
                                   TO  SPA-Q02-Q03-SRYYM
           WHEN    OTHER
               MOVE    SPA-SYSYMD (1:6)
                                   TO  SPA-Q02-Q03-SRYYM
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGQ03"      USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE  "B12"             TO  MCP-WIDGET
      *
           MOVE  "Q02"             TO  SPA-MOTOPG
           MOVE  "Q03"            TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "Q03"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
      *
           .
       4801-Q03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    印刷確認
      *****************************************************************
       480-CSV-KAKUNIN-SEC             SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "7777"          TO  SPA-ERRCD
               MOVE    SPACE           TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  480-CSV-KAKUNIN-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-PROC-FLG
      *
           PERFORM 480-OUT-SIJI-CALL-SEC
      *
           .
       480-CSV-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷確認
      *****************************************************************
       480-PRINT-KAKUNIN-SEC           SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "7777"          TO  SPA-ERRCD
               MOVE    SPACE           TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  480-PRINT-KAKUNIN-EXT
           END-IF
      *
      *    表題全角半角混在チェック
           MOVE    SPA-GMN-Q02-HYODAI  TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-Q02-CUR
               GO  TO  480-PRINT-KAKUNIN-EXT
           END-IF
      *
      *
      *    条件１全角半角混在チェック
           MOVE    SPA-GMN-Q02-JYOKEN1 TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-Q02-CUR
               GO  TO  480-PRINT-KAKUNIN-EXT
           END-IF
      *
      *
      *    条件２全角半角混在チェック
           MOVE    SPA-GMN-Q02-JYOKEN2 TO  WRK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           IF  ( FLG-ENTRYCHK      NOT =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-Q02-CUR
               GO  TO  480-PRINT-KAKUNIN-EXT
           END-IF
      *
      *
           MOVE    1               TO  WRK-PROC-FLG
      *
           PERFORM 480-OUT-SIJI-CALL-SEC
      *
           .
       480-PRINT-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認画面
      *****************************************************************
       490-KAKUNIN-SEC                 SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-Q97-AREA
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGQ97"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    SPA-Q01-SAKIPG  TO  SPA-MOTOPG
                                       SPA-Q97-MOTOPG
           MOVE    "Q97"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "Q97"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態確認画面
      *****************************************************************
       490-JOUTAI-SEC                  SECTION.
      *
           PERFORM 900-PTSRH-KEY2-SEL-SEC
           IF    ( FLG-PTSRH   =   ZERO )
               MOVE    1           TO  SPA-GMN-Q02-MODE
               PERFORM 3101-Q02-GMN-EDIT-SEC
               IF    ( SPA-GMN-Q02-TOTAL-NUM   =   ZERO )
                   MOVE    "0001"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       490-JOUTAI-EXT.
           EXIT.
      *****************************************************************
      *    出力指示画面呼出処理
      *****************************************************************
       480-OUT-SIJI-CALL-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-Q02-Q100-SENI-AREA
           INITIALIZE                  SPA-Q02-Q100-SENI-AREA
      *
           MOVE    WRK-PROC-FLG    TO  SPA-Q02-Q100-PROC-FLG
           MOVE    1               TO  SPA-Q02-Q100-STNUM
      *
           IF  ( WRK-PROC-FLG      =   ZERO )
               MOVE    SPA-GMN-Q02-TOTAL-NUM
                                   TO  SPA-Q02-Q100-EDNUM
           ELSE
               COMPUTE SPA-Q02-Q100-EDNUM
                   = ( SPA-GMN-Q02-TOTAL-NUM   +   CONST-LINE  -   1 )
                   /   CONST-LINE
           END-IF
      *
           MOVE    CONST-LINE      TO  SPA-Q02-Q100-LINE
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGQ100"      USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE  "B12"             TO  MCP-WIDGET
      *
           MOVE  "Q02"             TO  SPA-MOTOPG
           MOVE  "Q100"            TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "Q100"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       480-OUT-SIJI-CALL-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移時のチェック処理
      *****************************************************************
       4801-GAMEN-SENIJI-CHK-SEC   SECTION.
      *
      *    選択番号チェック
      *    １．未入力チェック
           IF  ( SPA-GMN-Q02-SELNUM    =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  4801-GAMEN-SENIJI-CHK-EXT
           END-IF
      *
      *    ２．範囲チェック
           COMPUTE WRK-KEKKALST-NO-ST  =   SPA-GMN-Q02-SKIP-KNS
                                       +   1
      *
           COMPUTE WRK-KEKKALST-NO-ED  =   SPA-GMN-Q02-SKIP-KNS
                                       +   SPA-GMN-Q02-KEKKA-MAX
      *
           IF      ( SPA-GMN-Q02-SELNUM   >=   WRK-KEKKALST-NO-ST )
            AND    ( SPA-GMN-Q02-SELNUM   <=   WRK-KEKKALST-NO-ED )
                   CONTINUE
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  4801-GAMEN-SENIJI-CHK-EXT
           END-IF
           .
      *
       4801-GAMEN-SENIJI-CHK-EXT.
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
           IF      SPA-QIDCD       NOT =   SPACE
               PERFORM 520-QIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q02A"              TO  MCP-WINDOW.
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
           MOVE    Q02A-KEKKALST-ROW       TO  WRK-KEKKALST-ROW
      *
           MOVE    SPACE                   TO  Q02A
           INITIALIZE                          Q02A
      *
           IF    ( FLG-GMN-INIT            =   1 )
               MOVE    ZERO                TO  Q02A-KEKKALST-ROW
               MOVE    ZERO                TO  Q02A-KEKKALST-ROWATTR
           ELSE
               MOVE    WRK-KEKKALST-ROW    TO  Q02A-KEKKALST-ROW
               MOVE    ZERO                TO  Q02A-KEKKALST-ROWATTR
           END-IF
      *
           MOVE    WIDGET-NORMAL           TO  Q02A-B10-STATE
                                               Q02A-B12S-STATE
                                               Q02A-B09-STATE
                                               Q02A-B09S-STATE
      *    検索結果件数がゼロ件でない場合はボタンを活性にする
           IF  ( SPA-GMN-Q02-TOTAL-NUM     >   ZERO )
               MOVE    WIDGET-NORMAL       TO  Q02A-B05-STATE
                                               Q02A-B05S-STATE
                                               Q02A-B11-STATE
                                               Q02A-B12-STATE
               MOVE    SPA-GMN-Q02-B06-STATE
                                           TO  Q02A-B06-STATE
               MOVE    SPA-GMN-Q02-B07-STATE
                                           TO  Q02A-B07-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B05-STATE
                                               Q02A-B05S-STATE
                                               Q02A-B06-STATE
                                               Q02A-B07-STATE
                                               Q02A-B09-STATE
                                               Q02A-B09S-STATE
                                               Q02A-B11-STATE
                                               Q02A-B12-STATE
           END-IF
      *
           IF  ( SPA-MW                    =   SPA-GINBEE )
            AND( SPA-Q01-CLIENT-SAVE   NOT =   "1" )
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B11-STATE
           END-IF
      *
           IF  ( SPA-GMN-Q02-MODE          =   ZERO )
               MOVE    "検索しています。"
                                   TO  SPA-GMN-Q02-TNAME (15)
               MOVE    "しばらくお待ちください。"
                                   TO  SPA-GMN-Q02-TNAME (16)
               MOVE    16          TO  SPA-GMN-Q02-KEKKA-MAX
               IF    ( SPA-GMN-Q02-DURATION
                                   >=  5 )
                   CONTINUE
               ELSE
                   COMPUTE SPA-GMN-Q02-DURATION
                       =   SPA-GMN-Q02-DURATION    +   1
               END-IF
           ELSE
               IF  ( SPA-GMN-Q02-TOTAL-NUM     =   ZERO )
                   MOVE    "該当患者は存在しませんでした。"
                                   TO  SPA-GMN-Q02-TNAME (16)
                   MOVE    16      TO  SPA-GMN-Q02-KEKKA-MAX
               END-IF
               MOVE    ZERO        TO  SPA-GMN-Q02-DURATION
           END-IF
      *
           MOVE    SPA-GMN-Q02-DURATION
                                   TO  Q02A-DURATION
      *
           IF  ( SPA-GMN-Q02-NINZU         >   CONST-RECE-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B09-STATE
           END-IF
      *
           IF  ( SPA-GMN-Q02-NINZU         >   CONST-TOUKEI-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B09S-STATE
           END-IF
      *
           IF    ( SPA-GSRAUTH-12          =   "1" )
               MOVE    WIDGET-NORMAL       TO  Q02A-B05-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B05-STATE
           END-IF
      *
           IF    ( SPA-GSRAUTH-21          =   "1" )
               MOVE    WIDGET-NORMAL       TO  Q02A-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B05S-STATE
           END-IF
      *
           IF    ( SPA-GSRAUTH-22          =   "1" )
               MOVE    WIDGET-NORMAL       TO  Q02A-B06S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  Q02A-B06S-STATE
           END-IF
      *
      *    表題
           MOVE    SPA-GMN-Q02-HYODAI      TO  Q02A-HYODAI
      *    条件上段
           MOVE    SPA-GMN-Q02-JYOKEN1     TO  Q02A-JYOKEN1
      *    条件下段
           MOVE    SPA-GMN-Q02-JYOKEN2     TO  Q02A-JYOKEN2
      *    検索件数
           MOVE    SPA-GMN-Q02-KEKKA-MAX
                                   TO  Q02A-KEKKALST-COUNT
      *    検索結果
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-Q02-KEKKA-MAX)
      *        連番
               MOVE    SPA-GMN-Q02-TNO   (IDX1)
                                   TO  Q02A-KEKKALST-NO (IDX1)
      *
      *        患者番号
               MOVE    SPA-GMN-Q02-TPTNUM   (IDX1)
                                   TO  Q02A-KEKKALST-PTNUM (IDX1)
      *
      *        氏名
               MOVE    SPA-GMN-Q02-TNAME (IDX1)
                                   TO  Q02A-KEKKALST-NAME (IDX1)
      *        性別
               MOVE    SPA-GMN-Q02-TSEX (IDX1)
                                   TO  Q02A-KEKKALST-SEX (IDX1)
      *        誕生日
               MOVE    SPA-GMN-Q02-TBIRTH (IDX1)
                                   TO  Q02A-KEKKALST-BIRTH (IDX1)
      *        年齢
               MOVE    SPA-GMN-Q02-TAGE (IDX1)
                                   TO  Q02A-KEKKALST-AGE (IDX1)
      *        入外区分
               EVALUATE    SPA-GMN-Q02-TNYUGAI (IDX1)
               WHEN    "1"
                   MOVE    "入"    TO  Q02A-KEKKALST-NYUGAI (IDX1)
               WHEN    "2"
                   MOVE    "外"    TO  Q02A-KEKKALST-NYUGAI (IDX1)
               END-EVALUATE
      *        保険
               MOVE    SPA-GMN-Q02-THKN (IDX1)
                                   TO  Q02A-KEKKALST-HKN (IDX1)
      *        公費１
               MOVE    SPA-GMN-Q02-TKOH1 (IDX1)
                                   TO  Q02A-KEKKALST-KOH1 (IDX1)
      *        公費２
               MOVE    SPA-GMN-Q02-TKOH2 (IDX1)
                                   TO  Q02A-KEKKALST-KOH2 (IDX1)
      *        合計点数
               MOVE    SPA-GMN-Q02-TTOTALTEN (IDX1)
                                   TO  WRK-TOTALTENZ
               MOVE    WRK-TOTALTENZ
                                   TO  Q02A-KEKKALST-TOTALTEN (IDX1)
      *
               IF      ( SPA-GMN-Q02-KEKKA-SEL =   IDX1 )
                   MOVE    "T"     TO  Q02A-KEKKALST-SELECT (IDX1)
               ELSE
                   MOVE    "F"     TO  Q02A-KEKKALST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF  ( SPA-GMN-Q02-MODE          =   ZERO )
               MOVE    "red"       TO  Q02A-TOTAL-STYLE
               MOVE    "＊＊＊検索中＊＊＊"
                                           TO  Q02A-TOTAL
           ELSE
      *        総件数
               MOVE    "black"             TO  Q02A-TOTAL-STYLE
               MOVE    SPA-GMN-Q02-TOTAL   TO  Q02A-TOTAL
           END-IF
      *
      *    選択番号
           MOVE    SPA-GMN-Q02-SELNUM  TO  Q02A-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-Q02-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-Q02-CUR
      *
      *    表題
           WHEN   001
               MOVE  "HYODAI"      TO  MCP-WIDGET
      *
      *    条件上段
           WHEN   002
               MOVE  "JYOKEN1"     TO  MCP-WIDGET
      *
      *    条件下段
           WHEN   003
               MOVE  "JYOKEN2"     TO  MCP-WIDGET
      *
      *    選択番号
           WHEN   004
               MOVE  "SELNUM"      TO  MCP-WIDGET
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
      *
      *    表題
           WHEN    "HYODAI"
               MOVE    001         TO  SPA-GMN-Q02-CUR
      *
      *    条件上段
           WHEN    "JYOKEN1"
               MOVE    002         TO  SPA-GMN-Q02-CUR
      *
      *    条件下段
           WHEN    "JYOKEN2"
               MOVE    003         TO  SPA-GMN-Q02-CUR
      *
      *    選択番号
           WHEN    "SELNUM"
               MOVE    004         TO  SPA-GMN-Q02-CUR
           END-EVALUATE
      *
      *    次カーソル位置を設定
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    001         TO  SPA-GMN-Q02-CUR
           WHEN    OTHER
               COMPUTE SPA-GMN-Q02-CUR =   SPA-GMN-Q02-CUR +   1
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "条件に該当する患者は存在しませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "範囲外の番号が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "全角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号が未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q02A"               TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-QIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-QIDMSG
      *
           EVALUATE    SPA-QIDCD
           WHEN    "6666"
               MOVE    WRK-SHORIERR    TO  WRK-QIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  QID1
           INITIALIZE                      QID1
           MOVE    SPA-QIDCD           TO  QID1-ID1CODE
           MOVE    WRK-QIDMSG          TO  QID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "Q02A"              TO  SPA-MOTOPG
           MOVE    "QID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "QID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-QIDSET-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           INSPECT WRK-HENYMDG3   REPLACING  ALL "  "  BY  "　"
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       900-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM      TO  HKN-HKNNUM
           MOVE    SPA-Q01-STSRYYMD(1:6)
                                   TO  HKN-TEKSTYMD(1:6)
           MOVE    "31"            TO  HKN-TEKSTYMD(7:2)
           MOVE    SPA-Q01-EDSRYYMD(1:6)
                                   TO  HKN-TEKEDYMD(1:6)
           MOVE    "01"            TO  HKN-TEKEDYMD(7:2)
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               MOVE    1           TO  FLG-HKNNUM
               INITIALIZE              HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "1"                 TO  PTSRH-DATAKBN
           MOVE    SPA-GMN-Q02-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-GMN-Q02-SELHMS  TO  PTSRH-UPHMS
           MOVE    WRK-SQL-ORDER       TO  PTSRH-SQL
           DISPLAY "WRK-SQL-ORDER[" WRK-SQL-ORDER "]"
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       900-PTSRH-ODR-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-FET-EXT.
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
           MOVE    "0"                 TO  PTSRH-DATAKBN
           MOVE    SPA-GMN-Q02-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-GMN-Q02-SELHMS  TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
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
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS2-SEC              SECTION.
      *
           MOVE    "2"             TO  WRK-DATAKBN
           PERFORM 900-PTSRH-DEL1-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    SPA-GMN-Q02-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-GMN-Q02-SELHMS  TO  PTSRH-UPHMS
           MOVE    "1"             TO  PTSRH-UPDATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins2"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-PTSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
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
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
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
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    ORCA-BLOB       TO  MCPDATA-REC
      *
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    MCPDATA-REC     TO  ORCA-BLOB
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ORCA-BLOB-FILE
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       900-JOBKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOBID2     TO  JOB-JOBID
           MOVE    CONST-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-JOBID       TO  JOB-JOBID
           MOVE    CONST-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           MOVE    1               TO  FLG-JOBEND
      *
           PERFORM VARYING IDX-JOBID   FROM    1   BY  1
                   UNTIL ( IDX-JOBID       >   CONST-JOBID2 )
                    OR   ( FLG-JOBEND      =   ZERO )
      *
      *        ジョブ管理チェック処理
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    IDX-JOBID       TO  JOB-JOBID
               MOVE    CONST-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
               IF    ( SJOBKANRI-RETURN    =  ZERO )
                   IF    ( JOB-ENDYMD      =   SPACE )
                       MOVE    ZERO    TO  FLG-JOBEND
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       900-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
