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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG028.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 訪問診療等の状況
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/03/07    NACL-森脇     新規作成
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
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-PTID          PIC 9(10).
           03  TOKEI-SRYCD             PIC 9(01)   OCCURS 2.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMSL80.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SYUKEI              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-TOKEI               PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-KJNYM          PIC X(06).
           03  WRK-PARA-STRYM          PIC X(06).
           03  WRK-PARA-ENDYM          PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-Y-SYUKEI-G.
               05  WRK-Y-OUSHIN        PIC 9(10).
               05  WRK-Y-KINKYU        PIC 9(10).
               05  WRK-Y-HOUMON        PIC 9(10).
               05  WRK-Y-KANGO         PIC 9(10).
               05  WRK-Y-TOTAL         PIC 9(10).
      *
           03  WRK-M-SYUKEI-G.
               05  WRK-M-TOTAL         PIC 9(10).
               05  WRK-M-HOUMON        PIC 9(10).
               05  WRK-M-WARIAI        PIC 9(08)V9(01).
      *
           03  WRK-Z10                 PIC ZZZZZZZZZ9.
      *
       01  WRK-PRT-AREA.
           03  WRK-PRT-LINE1-1.
               05  FILLER              PIC X(47)   VALUE   SPACE.
               05  FILLER              PIC X(16)   VALUE
                   "訪問診療等の状況".
           03  WRK-PRT-LINE1-2.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(22)   VALUE
                   "保険医療機関名　　：　".
               05  WRK-PRT-HOSPNAME    PIC X(78)   VALUE   SPACE.
           03  WRK-PRT-LINE1-3.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(22)   VALUE
                   "保険医療機関コード：　".
               05  WRK-PRT-HOSPCD      PIC X(78)   VALUE   SPACE.
           03  WRK-PRT-LINE1-4.
               05  FILLER              PIC X(66)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "報告年月日：".
               05  WRK-PRT-PRTYMD      PIC X(22)   VALUE   SPACE.
           03  WRK-PRT-LINE2-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(42)   VALUE
               "○直近１年間の訪問診療等の実施回数について".
               05  FILLER              PIC X(02)   VALUE   "（".
               05  WRK-PRT-STRYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "〜".
               05  WRK-PRT-ENDYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "）".
           03  WRK-PRT-LINE2-2.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE   "往診".
               05  FILLER              PIC X(10)   VALUE   "…　[１]".
               05  WRK-PRT-Y-OUSHIN    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE2-3.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "【再掲】うち緊急の往診".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  WRK-PRT-Y-KINKYU    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE2-4.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE   "訪問診療".
               05  FILLER              PIC X(10)   VALUE   "…　[２]".
               05  WRK-PRT-Y-HOUMON    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE2-5.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "訪問看護(緊急を含む)".
               05  FILLER              PIC X(10)   VALUE   "…　[３]".
               05  WRK-PRT-Y-KANGO     PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE2-6.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "訪問診療の合計回数([１]＋[２]＋[３])".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  WRK-PRT-Y-TOTAL     PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE3-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(50)   VALUE
               "○直近１月間における往診又は訪問診療の状況について".
               05  FILLER              PIC X(02)   VALUE   "（".
               05  WRK-PRT-KJNYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "）".
           03  WRK-PRT-LINE3-2.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "初診、再診、往診又は訪問診療を実施した患者数".
               05  FILLER              PIC X(10)   VALUE   "…　[１]".
               05  WRK-PRT-M-TOTAL     PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE3-3.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "往診又は訪問診療を実施した患者数".
               05  FILLER              PIC X(10)   VALUE   "…　[２]".
               05  WRK-PRT-M-HOUMON    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE3-4.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(56)   VALUE
               "往診又は訪問診療を実施した患者の割合([２]／[１])".
               05  FILLER              PIC X(10)   VALUE   "…　[３]".
               05  WRK-PRT-M-WARIAI    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　％".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-KJNYM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG028"          TO  JOB-PGID
           MOVE    "訪問診療等の状況"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG02801"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES(1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                       TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    １年前をセット
           MOVE    WRK-PARA-KJNYM      TO  WRK-SYMD(1:6)
           IF      WRK-SMM     =   12
               MOVE    1               TO  WRK-SMM
           ELSE
               COMPUTE WRK-SYY =   WRK-SYY - 1
               COMPUTE WRK-SMM =   WRK-SMM + 1
           END-IF
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PRT-STRYM
           MOVE    WRK-SYMD(1:6)       TO  WRK-PARA-STRYM
      *
      *    基準月セット
           MOVE    WRK-PARA-KJNYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PRT-ENDYM
                                           WRK-PRT-KJNYM
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRT-PRTYMD
      *
      *    医療機関情報セット
           PERFORM 900-SYS1001-DBREAD-SEC
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *
      *    診療行為集計処理
           PERFORM 220-MAIN-SRYACT-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           IF      CNT-TOKEI       NOT =   ZERO
               OPEN    INPUT               TOKEI-FILE
      *
      *        統計出力処理
               PERFORM 260-TOKEI-OUT-SEC
      *
               CLOSE                       TOKEI-FILE
           END-IF
      *
      *    統計出力処理
           PERFORM 300-PRT-OUT-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為集計処理
      *****************************************************************
       220-MAIN-SRYACT-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-Y-SYUKEI-G
      *
      *    診療行為読み込み
           PERFORM 900-SRYACT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACT  =   1
      *        診療会計集計処理
               PERFORM 230-MAIN-SRYACCT-SEC
      *        診療行為読み込み
               PERFORM 900-SRYACT-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
            .
       220-MAIN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       230-MAIN-SRYACCT-SEC           SECTION.
      *
           IF    ( SRY-SRYYM   =   WRK-PARA-KJNYM )
               IF    ( SRY-SRYKBN      NOT =   "11" ) AND
                     ( SRY-SRYKBN      NOT =   "12" ) AND
                     ( SRY-SRYKBN      NOT =   "14" )
                   GO  TO  230-MAIN-SRYACCT-EXT
               END-IF
           ELSE
               IF      SRY-SRYKBN      NOT =   "14"
                   GO  TO  230-MAIN-SRYACCT-EXT
               END-IF
           END-IF
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-READ-SEC
           IF      FLG-SRYACCT        =   1
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *
      *    保険組合せ読み込み
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI       =   1
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   5 )
                   OR    ( SRY-SRYCD(IDX)      =   SPACE )
      *        コードチェック
               PERFORM 240-SRYCD-CHEK-SEC
           END-PERFORM
      *
           .
       230-MAIN-SRYACCT-EXT.
           EXIT.
      *****************************************************************
      *    コードチェック処理
      *****************************************************************
       240-SRYCD-CHEK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUKEI
      *
           EVALUATE    SRY-SRYCD(IDX)
      *        往診
               WHEN    "114000110"
               WHEN    "114001610"
                   MOVE    1           TO  FLG-SYUKEI
      *        往診緊急（再掲）
               WHEN    "114000370"
               WHEN    "114001870"
               WHEN    "114011570"
               WHEN    "114011870"
               WHEN    "114017470"
               WHEN    "114017770"
               WHEN    "114022370"
               WHEN    "114022670"
               WHEN    "114000470"
               WHEN    "114000570"
               WHEN    "114001970"
               WHEN    "114002070"
               WHEN    "114011670"
               WHEN    "114011770"
               WHEN    "114011970"
               WHEN    "114012070"
               WHEN    "114017570"
               WHEN    "114017670"
               WHEN    "114017870"
               WHEN    "114017970"
               WHEN    "114022470"
               WHEN    "114022570"
               WHEN    "114022770"
               WHEN    "114022870"
               WHEN    "114029270"
               WHEN    "114029370"
               WHEN    "114029470"
               WHEN    "114029570"
               WHEN    "114029970"
               WHEN    "114030070"
               WHEN    "114030170"
               WHEN    "114030270"
                   MOVE    2           TO  FLG-SYUKEI
      *        訪問診療
               WHEN    "114001110"
               WHEN    "114030310"
               WHEN    "114019510"
               WHEN    "114019610"
               WHEN    "114019710"
               WHEN    "114019810"
               WHEN    "114007610"
               WHEN    "114007710"
      *    H30.4改正
               WHEN    "114042110"
               WHEN    "114042210"
               WHEN    "114042810"
               WHEN    "114046310"
                   MOVE    3           TO  FLG-SYUKEI
      *        訪問看護（緊急を含む）
               WHEN    "114004510"
               WHEN    "114010610"
               WHEN    "114004610"
               WHEN    "114010710"
               WHEN    "114020110"
               WHEN    "114026810"
               WHEN    "114026910"
               WHEN    "114027010"
               WHEN    "114027110"
               WHEN    "114027210"
               WHEN    "114027310"
               WHEN    "114027410"
               WHEN    "114027510"
               WHEN    "114020510"
                   MOVE    4           TO  FLG-SYUKEI
      *        初診
               WHEN    "111000110"
               WHEN    "111012510"
               WHEN    "111012710"
               WHEN    "113003510"
               WHEN    "113003710"
               WHEN    "113019710"
               WHEN    "113019910"
               WHEN    "099110001"
                   MOVE    5           TO  FLG-SYUKEI
      *        再診
               WHEN    "112007410"
               WHEN    "112007950"
               WHEN    "112008350"
               WHEN    "112008850"
               WHEN    "112016610"
               WHEN    "112016750"
               WHEN    "112016850"
               WHEN    "112016950"
               WHEN    "113003610"
               WHEN    "113003810"
               WHEN    "113019810"
               WHEN    "113020010"
               WHEN    "099120001"
      *    H30.4改正
               WHEN    "112023210"
                   MOVE    6           TO  FLG-SYUKEI
           END-EVALUATE
      *
           IF      FLG-SYUKEI      =   ZERO
               GO  TO  240-SRYCD-CHEK-EXT
           END-IF
      *
           IF    ( SRY-SRYYM   =   WRK-PARA-KJNYM )
               IF    ( FLG-SYUKEI      =   1 ) OR
                     ( FLG-SYUKEI      =   3 ) OR
                     ( FLG-SYUKEI      =   5 ) OR
                     ( FLG-SYUKEI      =   6 )
                   PERFORM 250-TOKEI-SET-SEC 
               END-IF
           END-IF
      *
           EVALUATE    FLG-SYUKEI
               WHEN    1
                   COMPUTE WRK-Y-OUSHIN    =   WRK-Y-OUSHIN
                                           +   ACCT-ZAIKAISU
                   COMPUTE WRK-Y-TOTAL     =   WRK-Y-TOTAL
                                           +   ACCT-ZAIKAISU
               WHEN    2
                   COMPUTE WRK-Y-KINKYU    =   WRK-Y-KINKYU
                                           +   ACCT-ZAIKAISU
               WHEN    3
                   COMPUTE WRK-Y-HOUMON    =   WRK-Y-HOUMON
                                           +   ACCT-ZAIKAISU
                   COMPUTE WRK-Y-TOTAL     =   WRK-Y-TOTAL
                                           +   ACCT-ZAIKAISU
               WHEN    4
                   COMPUTE WRK-Y-KANGO     =   WRK-Y-KANGO
                                           +   ACCT-ZAIKAISU
                   COMPUTE WRK-Y-TOTAL     =   WRK-Y-TOTAL
                                           +   ACCT-ZAIKAISU
           END-EVALUATE
           .
       240-SRYCD-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-SET-SEC           SECTION.
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    SRY-PTID        TO  TOKEI-PTID
           PERFORM 900-TOKEI-READ-SEC
      *
           IF    ( FLG-SYUKEI      =   5 ) OR
                 ( FLG-SYUKEI      =   6 )
               MOVE    1           TO  TOKEI-SRYCD(1)
           END-IF
           IF    ( FLG-SYUKEI      =   1 ) OR
                 ( FLG-SYUKEI      =   3 )
               MOVE    1           TO  TOKEI-SRYCD(2)
           END-IF
      *
           IF      FLG-TOKEI       =   1
               COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
               WRITE   TOKEI-REC
           ELSE
               REWRITE TOKEI-REC
           END-IF
           .
       250-TOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-M-SYUKEI-G
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
      *        初診、再診、往診、訪問患者数
               IF    ( TOKEI-SRYCD(1)  NOT =   ZERO )  OR
                     ( TOKEI-SRYCD(2)  NOT =   ZERO )
                   COMPUTE WRK-M-TOTAL     =   WRK-M-TOTAL     +   1
               END-IF
      *        往診、訪問患者数
               IF    ( TOKEI-SRYCD(2)  NOT =   ZERO )
                   COMPUTE WRK-M-HOUMON    =   WRK-M-HOUMON    +   1
               END-IF
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
           COMPUTE WRK-M-WARIAI    =   WRK-M-HOUMON
                                   /   WRK-M-TOTAL
                                   *   100
                                   +   0.5
      *
             .
       260-TOKEI-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-PRT-OUT-SEC             SECTION.
      *
           MOVE    WRK-Y-OUSHIN    TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-Y-OUSHIN
      *
           MOVE    WRK-Y-KINKYU    TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-Y-KINKYU
      *
           MOVE    WRK-Y-HOUMON    TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-Y-HOUMON
      *
           MOVE    WRK-Y-KANGO     TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-Y-KANGO
      *
           MOVE    WRK-Y-TOTAL     TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-Y-TOTAL
      *
           MOVE    WRK-M-TOTAL     TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-M-TOTAL
      *
           MOVE    WRK-M-HOUMON    TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-M-HOUMON
      *
           MOVE    WRK-M-WARIAI    TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-M-WARIAI
      *
           MOVE    1               TO  CNT-PAGE
      *
           MOVE    SPACE           TO  HCMSL80
      *
           MOVE    WRK-PRT-LINE1-1 TO  HCMSL80-MOJI(3)
           MOVE    WRK-PRT-LINE1-2 TO  HCMSL80-MOJI(7)
           MOVE    WRK-PRT-LINE1-3 TO  HCMSL80-MOJI(9)
           MOVE    WRK-PRT-LINE1-4 TO  HCMSL80-MOJI(12)
      *
           MOVE    WRK-PRT-LINE2-1 TO  HCMSL80-MOJI(15)
           MOVE    WRK-PRT-LINE2-2 TO  HCMSL80-MOJI(17)
           MOVE    WRK-PRT-LINE2-3 TO  HCMSL80-MOJI(19)
           MOVE    WRK-PRT-LINE2-4 TO  HCMSL80-MOJI(21)
           MOVE    WRK-PRT-LINE2-5 TO  HCMSL80-MOJI(23)
           MOVE    WRK-PRT-LINE2-6 TO  HCMSL80-MOJI(25)
      *
           MOVE    WRK-PRT-LINE3-1 TO  HCMSL80-MOJI(28)
           MOVE    WRK-PRT-LINE3-2 TO  HCMSL80-MOJI(30)
           MOVE    WRK-PRT-LINE3-3 TO  HCMSL80-MOJI(32)
           MOVE    WRK-PRT-LINE3-4 TO  HCMSL80-MOJI(34)
      *
      *    印刷処理
           PERFORM 330-PRINT-OUT-SEC
           .
       300-PRT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    SPACE               TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "訪問診療等の状況"  TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN     =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           DISPLAY "*** ORCBG028 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG028 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
     *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
                   
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関マスタ検索
      *****************************************************************
       900-SYS1001-DBREAD-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-HOSPNAME
                                       WRK-PRT-HOSPCD
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SKYYMD TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-DBREAD-SEC 
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNAME   TO  WRK-PRT-HOSPNAME
               MOVE    SYS-1001-HOSPCD     TO  WRK-PRT-HOSPCD
           END-IF
           .
       900-SYS1001-DBREAD-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索
      *****************************************************************
       900-SYSKANRI-DBREAD-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-DBREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBSELECT-SEC     SECTION.
      *
      *    対象診療年月で検索
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-PARA-STRYM  TO  SRY-SRYYM
           MOVE    WRK-PARA-KJNYM  TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBFETCH-SEC      SECTION.
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ検索
      *****************************************************************
       900-HKNCOMBI-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    ACCT-PTID       TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
      *
               IF    ( COMB-HKNNUM(1:2)    =   "98" ) OR
                     ( COMB-HKNNUM     =   "975" ) OR
                     ( COMB-HKNNUM     =   "973" ) OR
                     ( COMB-HKNNUM     =   "971" )
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       900-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
