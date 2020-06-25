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
       PROGRAM-ID.             ORCBG027.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : ニコチン依存症管理料に係る報告書
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/07/21    NACL-森脇     新規作成
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
           03  TOKEI-KAISU             PIC 9(03)   OCCURS 3.
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
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
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
           03  WRK-PARA-NENDO          PIC X(04).
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
           03  WRK-STRYM               PIC X(06).
           03  WRK-ENDYM               PIC X(06).
      *
           03  WRK-GOUKEI-G.
               05  WRK-NINZU           PIC 9(10).
               05  WRK-SYOKAI          PIC 9(10).
               05  WRK-TOTAL           PIC 9(10).
               05  WRK-HEIKIN          PIC 9(08)V9(01).
      *
           03  WRK-Z10                 PIC ZZZZZZZZZ9.
           03  WRK-Z8V1                PIC ZZZZZZZ9.9.
      *
       01  WRK-PRT-AREA.
           03  WRK-PRT-LINE1-1.
               05  FILLER              PIC X(39)   VALUE   SPACE.
               05  FILLER              PIC X(32)   VALUE
                   "ニコチン依存症管理料に係る報告書".
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
               05  FILLER              PIC X(34)   VALUE
                   "報告年月日：　　　　年　７月　　日".
           03  WRK-PRT-LINE2-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(68)   VALUE
                   "本管理料を算定した患者数".
               05  FILLER              PIC X(08)   VALUE   "…　[１]".
               05  WRK-PRT-NINZU       PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE2-2.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "（期間：".
               05  WRK-PRT-STRYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "〜".
               05  WRK-PRT-ENDYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "）".
           03  WRK-PRT-LINE3-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(48)   VALUE
                   "[１]のうち、当該期間後の６月末日までに１２週間に".
               05  FILLER              PIC X(20)   VALUE   "わたる".
               05  FILLER              PIC X(08)   VALUE   "…　[２]".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE3-2.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(68)   VALUE
                   "計５回の禁煙治療を終了した者".
           03  WRK-PRT-LINE4.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(68)   VALUE
                   "[２]のうち、禁煙に成功した者".
               05  FILLER              PIC X(08)   VALUE   "…　[３]".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE5-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(44)   VALUE
                   "５回の指導を最後まで行わずに治療を中止した者".
               05  FILLER              PIC X(24)   VALUE
                   "([１]−[２])".
               05  FILLER              PIC X(08)   VALUE   "…　[４]".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　名".
           03  WRK-PRT-LINE5-2.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(68)   VALUE
                   "のうち、中止時に禁煙していた者".
           03  WRK-PRT-LINE6-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(42)   VALUE
                   "ニコチン依存症管理料の初回の治療の一年間の".
               05  FILLER              PIC X(26)   VALUE   "算定回数".
               05  FILLER              PIC X(08)   VALUE   "…　[５]".
               05  WRK-PRT-SYOKAI      PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE6-2.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(01)   VALUE   "(".
               05  WRK-PRT-STRYMD1     PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "から".
               05  WRK-PRT-ENDYMD1     PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(14)   VALUE
                                                   "までの一年間)".
           03  WRK-PRT-LINE7-1.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(68)   VALUE
                   "ニコチン依存症管理料の一年間の延べ算定回数".
               05  FILLER              PIC X(08)   VALUE   "…　[６]".
               05  WRK-PRT-TOTAL       PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
           03  WRK-PRT-LINE7-2.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(01)   VALUE   "(".
               05  WRK-PRT-STRYMD2     PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "から".
               05  WRK-PRT-ENDYMD2     PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(14)   VALUE
                                                   "までの一年間に".
           03  WRK-PRT-LINE7-3.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(38)   VALUE
                   "おける初回から５回目までの治療を含む)".
           03  WRK-PRT-LINE8.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(58)   VALUE
                   "喫煙を止めた者の割合＝([３]＋[４])／[１]".
               05  FILLER              PIC X(08)   VALUE   "…　　　".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　％".
           03  WRK-PRT-LINE9.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(58)   VALUE
                   "治療の平均継続回数＝[６]／[５]".
               05  FILLER              PIC X(08)   VALUE   "…　　　".
               05  WRK-PRT-HEIKIN      PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(04)   VALUE   "　回".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
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
                                       WRK-PARA-NENDO
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
           MOVE    "ORCBG027"          TO  JOB-PGID
           MOVE    "ニコチン依存症管理料に係る報告書"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG02701"           TO  TOKEIPARA-FILE-ID
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
      *    開始日セット
           MOVE    WRK-PARA-NENDO      TO  WRK-SYMD(1:4)
           MOVE    "0401"              TO  WRK-SYMD(5:4)
           PERFORM 800-SEIWA-HEN-SEC
           IF      STS-DAY-RC1         NOT =   ZERO
               MOVE    "パラメタが正しくありません"  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
           MOVE    WRK-SYMD(1:6)       TO  WRK-STRYM
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PRT-STRYM
           MOVE    WRK-HENYMDG         TO  WRK-PRT-STRYMD1
                                           WRK-PRT-STRYMD2
      *
      *    終了日セット
           COMPUTE WRK-SYY =   WRK-SYY +   1
           MOVE    "0331"              TO  WRK-SYMD(5:4)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-SYMD(1:6)       TO  WRK-ENDYM
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PRT-ENDYM
           MOVE    WRK-HENYMDG         TO  WRK-PRT-ENDYMD1
                                           WRK-PRT-ENDYMD2
      *
      *    医療機関情報セット
           PERFORM 900-SYS1001-DBREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-PRT-HOSPNAME
               MOVE    SYS-1001-HOSPCD     TO  WRK-PRT-HOSPCD
           END-IF
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
           IF      CNT-TOKEI       =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    統計出力処理
           PERFORM 260-TOKEI-OUT-SEC
      *
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
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
      *    診療行為読み込み
           PERFORM 900-SRYACT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACT  =   1
      *        主編集処理
               PERFORM 220-MAIN-HEN-SEC
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
      *    主編集処理
      *****************************************************************
       220-MAIN-HEN-SEC            SECTION.
      *
           IF    ( SRY-SRYKBN      NOT =   "13" )
               GO  TO  220-MAIN-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   5 )
                   OR    ( SRY-SRYCD(IDX)      =   SPACE )
      *        ニコチン依存症管理料を対象とする
               IF    ( SRY-SRYCD(IDX)    =   "113008310" )   OR
                     ( SRY-SRYCD(IDX)    =   "113008410" )   OR
                     ( SRY-SRYCD(IDX)    =   "113008510" )
                   PERFORM 220-TOKEI-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       220-MAIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       220-TOKEI-SET-SEC           SECTION.
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-READ-SEC
           IF      FLG-SRYACCT        =   1
               GO  TO  220-TOKEI-SET-EXT
           END-IF
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    SRY-PTID        TO  TOKEI-PTID
           PERFORM 900-TOKEI-READ-SEC
      *
           MOVE    SRY-PTID        TO  TOKEI-PTID
      *    初回
           IF    ( SRY-SRYCD(IDX)  =   "113008310" )
               COMPUTE TOKEI-KAISU(1)  =   TOKEI-KAISU(1)
                                       +   ACCT-ZAIKAISU
           END-IF
      *    ２〜４回目
           IF    ( SRY-SRYCD(IDX)  =   "113008410" )
               COMPUTE TOKEI-KAISU(2)  =   TOKEI-KAISU(2)
                                       +   ACCT-ZAIKAISU
           END-IF
      *    ５回目
           IF    ( SRY-SRYCD(IDX)  =   "113008510" )
               COMPUTE TOKEI-KAISU(3)  =   TOKEI-KAISU(3)
                                       +   ACCT-ZAIKAISU
           END-IF
      *
           IF      FLG-TOKEI       =   1
               COMPUTE CNT-TOKEI       =   CNT-TOKEI   +   1
               WRITE   TOKEI-REC
           ELSE
               REWRITE TOKEI-REC
           END-IF
      *
           .
       220-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-GOUKEI-G
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
      *        初回患者数
               IF      TOKEI-KAISU(1)  NOT =   ZERO
                   COMPUTE WRK-NINZU   =   WRK-NINZU   +   1
               END-IF
      *        初回算定数
               COMPUTE WRK-SYOKAI  =   WRK-SYOKAI  +   TOKEI-KAISU(1)
      *        延べ算定数
               COMPUTE WRK-TOTAL   =   WRK-TOTAL
                                   +   TOKEI-KAISU(1)
                                   +   TOKEI-KAISU(2)
                                   +   TOKEI-KAISU(3)
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
           PERFORM 300-PRT-OUT-SEC 
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
           MOVE    WRK-NINZU       TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-NINZU
      *
           MOVE    WRK-SYOKAI      TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-SYOKAI
      *
           MOVE    WRK-TOTAL       TO  WRK-Z10
           MOVE    WRK-Z10         TO  WRK-PRT-TOTAL
      *
           COMPUTE WRK-HEIKIN      =   WRK-TOTAL   /   WRK-SYOKAI
           MOVE    WRK-HEIKIN      TO  WRK-Z8V1
           MOVE    WRK-Z8V1        TO  WRK-PRT-HEIKIN
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
           MOVE    WRK-PRT-LINE2-2 TO  HCMSL80-MOJI(16)
      *
           MOVE    WRK-PRT-LINE3-1 TO  HCMSL80-MOJI(19)
           MOVE    WRK-PRT-LINE3-2 TO  HCMSL80-MOJI(20)
      *
           MOVE    WRK-PRT-LINE4   TO  HCMSL80-MOJI(23)
      *
           MOVE    WRK-PRT-LINE5-1 TO  HCMSL80-MOJI(26)
           MOVE    WRK-PRT-LINE5-2 TO  HCMSL80-MOJI(27)
      *
           MOVE    WRK-PRT-LINE6-1 TO  HCMSL80-MOJI(30)
           MOVE    WRK-PRT-LINE6-2 TO  HCMSL80-MOJI(31)
      *
           MOVE    WRK-PRT-LINE7-1 TO  HCMSL80-MOJI(34)
           MOVE    WRK-PRT-LINE7-2 TO  HCMSL80-MOJI(35)
           MOVE    WRK-PRT-LINE7-3 TO  HCMSL80-MOJI(36)
      *
           MOVE    WRK-PRT-LINE8   TO  HCMSL80-MOJI(39)
      *
           MOVE    WRK-PRT-LINE9   TO  HCMSL80-MOJI(42)
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
           MOVE    "ニコチン依存症管理料に係る報告書"
                                       TO  SPRT-TITLE
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
           IF      CNT-TOKEI       =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG027 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG027 END ***"
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
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SKYYMD TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
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
       900-SYS1001-DBREAD-EXT.
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
           MOVE    WRK-STRYM       TO  SRY-SRYYM
           MOVE    WRK-ENDYM       TO  SRY-CREYMD
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
