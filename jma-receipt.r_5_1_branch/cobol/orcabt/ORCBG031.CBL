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
       PROGRAM-ID.             ORCBG031.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 長期投与患者一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/04/19    NACL-森脇     新規作成
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
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-PTNUM         PIC X(20).
               05  TOKEI-SRYKA         PIC X(08).
               05  TOKEI-HKNCOMBI      PIC 9(04).
               05  TOKEI-SRYYMD        PIC X(08).
           03  TOKEI-NAME              PIC X(20).
           03  TOKEI-PTID              PIC 9(10).
           03  TOKEI-SRYKAMEI          PIC X(10).
           03  TOKEI-NISSU             PIC 9(03).
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-GPAGE               PIC 9(03).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME                PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(05).
           03  IDX-D                   PIC 9(02).
           03  IDX-K                   PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-KBTKBN         PIC X(01).
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
           03  WRK-HENYMD              PIC X(09).
      *
           03  WRK-PRTTIME             PIC X(08).
           03  WRK-HENTIME.
               05  WRK-HH              PIC X(02).
               05  WRK-FILLER1         PIC X(01).
               05  WRK-MM              PIC X(02).
               05  WRK-FILLER2         PIC X(01).
               05  WRK-SS              PIC X(02).
      *
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-D        PIC X(02).
      *
           03  WRK-Z03                 PIC ZZ9.
      *
       01  WRK-PRT-AREA.
      *    明細頁ページ見出し
           03  WRK-PRT-LINE1.
               05  FILLER              PIC X(16)   VALUE
                                                   "長期投薬患者一覧".
               05  WRK-PRT-NYUGAIKBN   PIC X(08)   VALUE   SPACE.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  WRK-PRT-SRYYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "分".
               05  FILLER              PIC X(26)   VALUE   SPACE.
               05  WRK-PRT-PRTYMD      PIC X(09)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   " (".
               05  WRK-PRT-PRTTIME     PIC X(05)   VALUE   SPACE.
               05  FILLER              PIC X(01)   VALUE   ")".
               05  WRK-PRT-PAGE        PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  WRK-PRT-LINE2.
               05  FILLER              PIC X(20)   VALUE   "患者番号".
               05  FILLER              PIC X(42)   VALUE   "患者氏名".
               05  FILLER              PIC X(12)   VALUE   "診療日".
               05  FILLER              PIC X(12)   VALUE   "投薬日数".
               05  FILLER              PIC X(12)   VALUE   "科".
               05  FILLER              PIC X(10)   VALUE   "保険組合せ".
           03  WRK-PRT-MEISAI.
               05  WRK-PRT-PTNUM       PIC X(20)   VALUE   SPACE.
               05  WRK-PRT-NAME        PIC X(40)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  WRK-PRT-SRYYMD      PIC X(09)   VALUE   SPACE.
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  WRK-PRT-NISSU       PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  WRK-PRT-SRYKAMEI    PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  WRK-PRT-HKNCOMBI    PIC X(04)   VALUE   SPACE.
      *
       01  CONST-AREA.
           03  CONST-BTPARA-SHELLID    PIC X(08)   VALUE   "ORCBQL01".
           03  CONST-BTPARA-JOBID      PIC 9(07)   VALUE   2.
           03  CONST-BTPARA-DSP-RENNUM PIC 9(03)   VALUE   2.
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    診療科
           COPY    "CPSK1005.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
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
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    会計カード編集サブ
           COPY    "CPORCHCM34.INC".
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
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
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
           INITIALIZE                  TTNS-AREA
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-KBTKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG031"          TO  JOB-PGID
           MOVE    "長期投薬患者一覧"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG03101"           TO  TOKEIPARA-FILE-ID
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
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    入外区分セット
           IF    ( WRK-PARA-NYUGAIKBN  NOT =   "1" )   AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   "2" )
               MOVE    "%"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
           IF      WRK-PARA-SRYYM      =   SPACE
               MOVE    LNK-PRTKANRI-SKYYMD(1:6)
                                           TO  WRK-PARA-SRYYM
           END-IF
      *
           IF      WRK-PARA-KBTKBN     NOT =   "1"
               MOVE    ZERO                TO  WRK-PARA-KBTKBN
           END-IF
      *
      *    開始月セット
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PRT-SRYYM
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  SYS-YMD
                                           WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-PRT-PRTYMD
      *
           ACCEPT  WRK-PRTTIME         FROM    TIME
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SPACE               TO  WRK-HENTIME
           MOVE    WRK-PRTTIME(1:2)    TO  WRK-HH
           MOVE    ":"                 TO  WRK-FILLER1
           MOVE    WRK-PRTTIME(3:2)    TO  WRK-MM
           MOVE    ":"                 TO  WRK-FILLER2
           MOVE    WRK-PRTTIME(5:2)    TO  WRK-SS
           MOVE    SPACE               TO  WRK-PRTTIME
           MOVE    WRK-HENTIME         TO  WRK-PRTTIME
           MOVE    WRK-PRTTIME(1:5)    TO  WRK-PRT-PRTTIME
           .
       110-PARA-HENSYU-EXT.
           EXIT.
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
      *    診療会計集計処理
           PERFORM 210-MAIN-SRYACCT-SEC
      *
           IF      CNT-TOKEI       NOT =   ZERO
      *
               CLOSE                       TOKEI-FILE
               OPEN    INPUT               TOKEI-FILE
      *
               PERFORM 260-TOKEI-OUT-SEC
           END-IF
      *
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *   診療会計集計処理
      *****************************************************************
       210-MAIN-SRYACCT-SEC        SECTION.
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACCT =   1
               IF    ( ACCT-ZAIKAISU   >   ZERO )
      *            チェック処理
                   PERFORM 230-MAIN-CHK-SYORI-SEC
                   IF      FLG-OK          =   1
      *                診療会計集計処理
                       PERFORM 220-MAIN-SRYACCT-CHK-SEC
                   END-IF
               END-IF
      *        診療会計読み込み
               PERFORM 900-SRYACCT-DBFETCH-SEC
           END-PERFORM
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-DBCLOSE-SEC
           .
       210-MAIN-SRYACCT-EXT.
           EXIT.
      *****************************************************************
      *   診療会計集計処理
      *****************************************************************
       220-MAIN-SRYACCT-CHK-SEC    SECTION.
      *
      *    回数が３１以上の場合
           PERFORM VARYING IDX-K FROM  2   BY  1
                   UNTIL   IDX-K >     10
               PERFORM VARYING IDX-D FROM  1   BY  1
                       UNTIL   IDX-D >     31
                   IF      ACCT-DAY(IDX-K IDX-D)   >=  31
      *                集計処理
                       PERFORM 240-TOKEI-SET-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       220-MAIN-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック処理
      *****************************************************************
       230-MAIN-CHK-SYORI-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    内服、頓服以外は読み飛ばし
           IF    ( ACCT-SRYKBN     NOT =   "21" )  AND
                 ( ACCT-SRYKBN     NOT =   "22" )
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
      *    テスト患者のときは読み飛ばす
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF           =   ZERO )  AND
                 ( PTINF-TSTPTNUMKBN   =   "1"  )
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM       =   1
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
      *    診療科読み込み
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI       =   1
               GO  TO  230-MAIN-CHK-SYORI-EXT
           END-IF
      *
           MOVE    1               TO  FLG-OK
           .
       230-MAIN-CHK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       240-TOKEI-SET-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-SRYYMD
           MOVE    ACCT-SRYYM      TO  WRK-SRYYMD-YM
           MOVE    IDX-D           TO  WRK-SRYYMD-D
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    ACCT-NYUGAIKBN  TO  TOKEI-NYUGAIKBN
           MOVE    PTNUM-PTNUM     TO  TOKEI-PTNUM
           MOVE    ACCT-SRYKA      TO  TOKEI-SRYKA
           MOVE    ACCT-HKNCOMBI   TO  TOKEI-HKNCOMBI
           MOVE    WRK-SRYYMD      TO  TOKEI-SRYYMD
           MOVE    ACCT-PTID       TO  TOKEI-PTID
           MOVE    PTINF-NAME      TO  TOKEI-NAME
           MOVE    SYS-1005-SRYKANAME
                                   TO  TOKEI-SRYKAMEI
           MOVE    ACCT-DAY(IDX-K IDX-D)
                                   TO  TOKEI-NISSU
      *
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
               WRITE   TOKEI-REC
           END-IF
           .
       240-TOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
           MOVE    ZERO            TO  CNT-TOKEI
           MOVE    ZERO            TO  CNT-GPAGE
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1  )
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
               MOVE    TOKEI-NYUGAIKBN     TO  WRK-NYUGAIKBN
               MOVE    SPACE               TO  WRK-PTNUM
               PERFORM UNTIL ( FLG-TOKEI           =   1 )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
      *            明細
                   PERFORM 320-PRT-MEISAI-SEC
      *            統計ファイル読み込み
                   PERFORM 900-TOKEI-NEXT-SEC
               END-PERFORM
      *        帳票編集処理
               PERFORM 390-PRINT-MEISAI-OUT-SEC
           END-PERFORM
           .
       260-TOKEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    見出し
      *****************************************************************
       310-PRT-HEAD-SEC            SECTION.
      *
           EVALUATE    TOKEI-NYUGAIKBN
               WHEN    "1"
                   MOVE    "（入院）"      TO  WRK-PRT-NYUGAIKBN
               WHEN    "2"
                   MOVE    "（外来）"      TO  WRK-PRT-NYUGAIKBN
           END-EVALUATE
      *
           MOVE    SPACE           TO  HCMSL80
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           COMPUTE CNT-GPAGE   =   CNT-GPAGE   +   1
      *
           MOVE    CNT-PAGE        TO  WRK-Z03
           MOVE    WRK-Z03         TO  WRK-PRT-PAGE
      *
           MOVE    WRK-PRT-LINE1   TO  HCMSL80-MOJI(1)
           MOVE    WRK-PRT-LINE2   TO  HCMSL80-MOJI(3)
           MOVE    ALL "-"         TO  HCMSL80-MOJI(4)
      *
           MOVE    4               TO  CNT-LINE
           .
       310-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細
      *****************************************************************
       320-PRT-MEISAI-SEC          SECTION.
      *
           IF      CNT-LINE        >=  80
      *        帳票編集処理
               PERFORM 390-PRINT-MEISAI-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
      *
           INITIALIZE                  WRK-PRT-MEISAI
      *
           IF    ( WRK-PTNUM       =   SPACE ) OR
                 ( WRK-PTNUM   NOT =   TOKEI-PTNUM )
               MOVE    TOKEI-PTNUM     TO  WRK-PRT-PTNUM
               MOVE    TOKEI-NAME      TO  WRK-PRT-NAME
               MOVE    TOKEI-PTNUM     TO  WRK-PTNUM
               COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
      *        個別指示
               IF      WRK-PARA-KBTKBN     =   "1"
                   PERFORM 800-ORCHCM34-SEC
               END-IF
               IF      CNT-LINE    NOT =   4
                   COMPUTE CNT-LINE    =   CNT-LINE    +   1
                   MOVE    ALL "-"         TO  HCMSL80-MOJI(CNT-LINE)
               END-IF
           END-IF
      *
           IF      CNT-LINE        >=  80
      *        帳票編集処理
               PERFORM 390-PRINT-MEISAI-OUT-SEC
      *        見出し
               PERFORM 310-PRT-HEAD-SEC
           END-IF
      *
           MOVE    TOKEI-SRYYMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD      TO  WRK-PRT-SRYYMD
      *
           MOVE    TOKEI-NISSU     TO  WRK-Z03
           MOVE    WRK-Z03         TO  WRK-PRT-NISSU
      *
           MOVE    TOKEI-SRYKAMEI  TO  WRK-PRT-SRYKAMEI
           MOVE    TOKEI-HKNCOMBI  TO  WRK-PRT-HKNCOMBI
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-PRT-MEISAI  TO  HCMSL80-MOJI(CNT-LINE)
           .
       320-PRT-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       390-PRINT-MEISAI-OUT-SEC    SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM
                                       TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "長期投与患者一覧"  TO  SPRT-TITLE
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
           .
       390-PRINT-MEISAI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    会計カード作成処理
      *****************************************************************
       800-ORCHCM34-SEC            SECTION.
      *
           MOVE    TOKEI-PTID  TO  SPA-PTID
      *
           INITIALIZE              ORCHCM34AREA
           MOVE    "2"         TO  ORCHCM34-KBN
           MOVE    WRK-PARA-SRYYM
                                   TO  ORCHCM34-SRYYM
           MOVE    TOKEI-NYUGAIKBN
                                   TO  ORCHCM34-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM  TO  ORCHCM34-STSRYYMD
           MOVE    "01"            TO  ORCHCM34-STSRYYMD(7:2)
           MOVE    WRK-PARA-SRYYM  TO  ORCHCM34-EDSRYYMD
           MOVE    "31"            TO  ORCHCM34-EDSRYYMD(7:2)
           MOVE    "1"         TO  ORCHCM34-PRTCTRLKBN
           MOVE    1               TO  ORCHCM34-STDAY
           MOVE    31              TO  ORCHCM34-EDDAY
      *
           INITIALIZE              ORCSPRTAREA
           MOVE    "INS"       TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                               TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                               TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                               TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM
                               TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                               TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                               TO  SPRT-SHELLID
           COMPUTE SPRT-SHORI-RENNUM
               =   LNK-PRTKANRI-SHORI-RENNUM   +   1
           MOVE    LNK-PRTKANRI-PRIORITY
                               TO  SPRT-PRIORITY
           MOVE    "会計カード"
                               TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID
                               TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                               TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                               TO  SPRT-PRTNM
           MOVE    "1"         TO  SPRT-SITEKBN
      *
           MOVE    ORCSPRTAREA TO  ORCHCM34-PRTAREA
      *
           CALL    "ORCHCM34"      USING
                                       SPA-AREA
                                       ORCHCM34AREA
                                       TTNS-AREA
                                       TTNS-CONST-MAX
           COMPUTE CNT-GPAGE
               =   CNT-GPAGE   +   ORCHCM34-PAGE
      *
           .
       800-ORCHCM34-EXT.
           EXIT.
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
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
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
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
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
           DISPLAY "*** ORCBG031 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG031 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-GPAGE       TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
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
           .
       900-TOKEI-READ-EXT.
           EXIT.
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
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBSELECT-SEC    SECTION.
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  ACCT-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM  TO  ACCT-STSRYYM
           MOVE    WRK-PARA-SRYYM  TO  ACCT-EDSRYYM
           MOVE    "%"             TO  ACCT-SRYKBN
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBFETCH-SEC     SECTION.
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-DBCLOSE-SEC     SECTION.
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key59"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SRYACCT-DBCLOSE-EXT.
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    ACCT-PTID       TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスタ読込
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    ACCT-PTID       TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療科読込
      *****************************************************************
       900-SYSKANRI-READ-SEC        SECTION.
      *
           INITIALIZE                  SYS-1005-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    ACCT-SRYKA      TO  SYS-1005-KBNCD
           MOVE    ACCT-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1005-STYUKYMD(7:2)
           MOVE    ACCT-SRYYM      TO  SYS-1005-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1005-EDYUKYMD(7:2)
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
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
           .
       900-DBSELECT-EXT.
           EXIT.
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
           .
       900-DBFETCH-EXT.
           EXIT.
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
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       900-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       900-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBUPDATE-EXT.
           EXIT.
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
