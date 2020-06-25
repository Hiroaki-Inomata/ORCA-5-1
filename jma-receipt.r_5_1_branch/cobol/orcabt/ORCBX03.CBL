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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBX03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 算定回数チェック一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/01/14    NACL−太田    新規作成
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
           SELECT  IN01-FILE       ASSIGN  BX0301PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN01-KEY
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-SRYKBN     PIC X(02).
               05  IN01-KANANAME   PIC X(100).
               05  IN01-SRYCD      PIC X(09).
               05  IN01-TANICD     PIC 9(03).
           03  IN01-SRYNAME        PIC X(100).
           03  IN01-TANINAME       PIC X(12).
           03  IN01-KAISU          PIC 9(05).
           03  IN01-JGNCNTERR      PIC 9(1).
           03  IN01-MSTKBN         PIC X(1).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                         PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    158バイトx55行
           COPY    "HCMSL55.INC".
      *
           COPY    "CPCOMMONDAT2.INC"      REPLACING   //RECE01//
                                           BY          //BX0301//.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
           03  STS-IN01                            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-IN01                            PIC 9(01).
           03  FLG-ETENSU5                         PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-TENSUPLUS                       PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDXL                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-KJNYMD                     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TITLE                           PIC X(100).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-KANACHK-STR                     PIC X(500).
           03  WRK-YMD                             PIC X(08).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TSUKISU                         PIC 9(02).
           03  WRK-TSUKISUZ1                       PIC Z.
           03  WRK-TSUKISUZ2                       PIC ZZ.
      *
       01  MASTER-AREA.
           03  MASTER-TANICD                       PIC 9(03).
           03  MASTER-TANINAME                     PIC X(12).
           03  MASTER-KAISU                        PIC 9(05).
      *
       01  CONST-AREA.
           03  CONST-HCMSL55-MAX           PIC 9(05)   VALUE 55.
      *
       01  FRM-AREA.
           03  FRM-HEAD1.
               05  FILLER                          PIC X(50)
               VALUE "＜　算定回数のチェックを行う診療行為一覧表　＞".
               05  FILLER                          PIC X(08)
                   VALUE   "基準日：".
               05  FRM-HEAD1-KJNYMD                PIC X(09).
               05  FILLER                          PIC X(64)
                   VALUE   SPACE.
               05  FILLER                          PIC X(8)
                   VALUE   "作成日：".
               05  FRM-HEAD1-SAKYMD                PIC X(09).
               05  FILLER                          PIC X(03)
                   VALUE   SPACE.
               05  FILLER                          PIC X(02)
                   VALUE   "P-".
               05  FRM-HEAD1-PCNT                  PIC ZZZZ9.
           03  FRM-HEAD2.
               05  FILLER                          PIC X(14)
                   VALUE   "診療行為コード".
               05  FILLER                          PIC X(71)
                   VALUE   SPACE.
               05  FILLER                          PIC X(04)
                   VALUE   "単位".
               05  FILLER                          PIC X(11)
                   VALUE   SPACE.
               05  FILLER                          PIC X(08)
                   VALUE   "上限回数".
           03  FRM-BODY.
               05  FRM-BODY-SRYCD                  PIC X(9).
               05  FILLER                          PIC X(5)
                   VALUE   SPACE.
               05  FRM-BODY-SRYNAME                PIC X(70).
               05  FILLER                          PIC X(1)
                   VALUE   SPACE.
               05  FRM-BODY-TANINAME               PIC X(12).
               05  FILLER                          PIC X(5)
                   VALUE   SPACE.
               05  FRM-BODY-KAISU                  PIC ZZ,ZZZ.
               05  FILLER                          PIC X(1)
                   VALUE   SPACE.
               05  FRM-BODY-WRNMSG                 PIC X(4).
               05  FILLER                          PIC X(1)
                   VALUE   SPACE.
               05  FRM-BODY-MSTMSG                 PIC X(14).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
      *
      *    電子点数表（算定回数）
       01  ETENSU5-REC.
           COPY    "CPETENSU5.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加情報
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
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
                                       WRK-PARA-KJNYMD
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
           MOVE    WRK-PARA-KJNYMD     TO  SPA-SRYYMD
      *
           MOVE    "算定回数チェック一覧表"
                                       TO  WRK-TITLE
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBX03"           TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPA-HOSPNUM         TO  BX0301PARA-HOSPNUM
           MOVE    "BX03001"           TO  BX0301PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BX0301PARA-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BX0301PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BX0301PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
           IF    ( SYS-1101-SRYKAISUMSTKBN =   SPACE )
               MOVE    "2"         TO  SYS-1101-SRYKAISUMSTKBN
           END-IF
           IF    ( SYS-1101-SRYKAISUWRNKBN =   SPACE )
               MOVE    "2"         TO  SYS-1101-SRYKAISUWRNKBN
           END-IF
      *
           OPEN    OUTPUT  IN01-FILE
           CLOSE           IN01-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-IN01-EDIT-SEC
      *
           IF    ( SYS-1101-SRYKAISUMSTKBN    =    "1" )
               PERFORM 2002-IN01-EDIT-SEC
           END-IF
      *
           PERFORM 2001-HCMSL55-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイル編集処理
      *****************************************************************
       2001-IN01-EDIT-SEC        SECTION.
      *
           OPEN    OUTPUT  IN01-FILE
      *
           PERFORM 900-TENSU-KEY12M-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
               PERFORM 20011-IN01-EDIT-SEC
      *
               PERFORM 900-TENSU-KEY12M-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key12m"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   IN01-FILE
      *
           .
       2001-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル編集処理（点数マスタより）
      *****************************************************************
       20011-IN01-EDIT-SEC       SECTION.
      *
           PERFORM 900-TENSUPLUS-KEY-SEL-SEC
           IF    ( FLG-TENSU       =   ZERO )
            AND  ( TNSP-SANTEIRRKKBN   =   1 )
               MOVE    TNSP-JGNCNT TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D
                                   TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNT1W
                                   TO  TNS-JGNCNT1W
               MOVE    TNSP-JGNCNTETCM
                                   TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC
                                   TO  TNS-JGNCNTETC
               MOVE    TNSP-JGNCNTERR
                                   TO  TNS-JGNCNTERR
           END-IF
      *
      *    月
           IF    ( TNS-JGNCNT      >  ZERO )
               MOVE    131         TO  MASTER-TANICD
               MOVE    "月"        TO  MASTER-TANINAME
               MOVE    TNS-JGNCNT  TO  MASTER-KAISU
               PERFORM 200111-IN01-EDIT-SEC
           END-IF
      *
      *    日
           IF    ( TNS-JGNCNT1D        >   ZERO )
               MOVE    121             TO  MASTER-TANICD
               MOVE    "日"            TO  MASTER-TANINAME
               MOVE    TNS-JGNCNT1D    TO  MASTER-KAISU
               PERFORM 200111-IN01-EDIT-SEC
           END-IF
      *
      *    週
           IF    ( TNS-JGNCNT1W        >   ZERO )
               MOVE    138             TO  MASTER-TANICD
               MOVE    "週"            TO  MASTER-TANINAME
               MOVE    TNS-JGNCNT1W    TO  MASTER-KAISU
               PERFORM 200111-IN01-EDIT-SEC
           END-IF
      *
      *    患者当りor月フリー
           IF    ( TNS-JGNCNTETCM      >   ZERO )
            AND  ( TNS-JGNCNTETC       >   ZERO )
               IF    ( TNS-JGNCNTETCM  =   99 )
                   MOVE    53          TO  MASTER-TANICD
                   MOVE    "患者当り"  TO  MASTER-TANINAME
                   MOVE    TNS-JGNCNTETC
                                       TO  MASTER-KAISU
      *
                   PERFORM 200111-IN01-EDIT-SEC
               ELSE
                   MOVE    TNS-JGNCNTETCM
                                       TO  WRK-TSUKISU
                   MOVE    901         TO  MASTER-TANICD
                   MOVE    SPACE       TO  WRK-KANACHK-STR
                   IF    ( WRK-TSUKISU >=  10 )
                       MOVE    WRK-TSUKISU TO  WRK-TSUKISUZ2
                       STRING  WRK-TSUKISUZ2   DELIMITED BY SIZE
                               "月"            DELIMITED BY SIZE
                       INTO    WRK-KANACHK-STR
                       END-STRING
                   ELSE
                       MOVE    WRK-TSUKISU TO  WRK-TSUKISUZ1
                       STRING  WRK-TSUKISUZ1   DELIMITED BY SIZE
                               "月"            DELIMITED BY SIZE
                       INTO    WRK-KANACHK-STR
                       END-STRING
                   END-IF
                   PERFORM 800-ZENKAKU-SEC
                   MOVE    WRK-KANACHK-STR  TO  MASTER-TANINAME
                   MOVE    TNS-JGNCNTETC    TO  MASTER-KAISU
                   PERFORM 200111-IN01-EDIT-SEC
               END-IF
           END-IF
      *
           .
       20011-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル編集処理
      *****************************************************************
       200111-IN01-EDIT-SEC          SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    TNS-SRYKBN      TO  IN01-SRYKBN
           MOVE    TNS-KANANAME    TO  IN01-KANANAME
           MOVE    TNS-SRYCD       TO  IN01-SRYCD
           MOVE    MASTER-TANICD   TO  IN01-TANICD
           MOVE    TNS-NAME        TO  IN01-SRYNAME
           MOVE    MASTER-TANINAME TO  IN01-TANINAME
           MOVE    MASTER-KAISU    TO  IN01-KAISU
           MOVE    TNS-JGNCNTERR   TO  IN01-JGNCNTERR
           MOVE    "1"             TO  IN01-MSTKBN
      *
           WRITE   IN01-REC
      *
           .
       200111-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル編集処理
      *****************************************************************
       2002-IN01-EDIT-SEC        SECTION.
      *
           OPEN    I-O             IN01-FILE
      *
           PERFORM 900-ETENSU5-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ETENSU5 NOT =   ZERO )
      *
               MOVE    ETENSU5-SRYCD
                                   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               PERFORM 20021-IN01-EDIT-SEC
      *
               PERFORM 900-ETENSU5-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_etensu_5"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   IN01-FILE
      *
           .
       2002-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル編集処理（電子点数表より）
      *****************************************************************
       20021-IN01-EDIT-SEC       SECTION.
      *
           IF    ( ETENSU5-TANICD  =      53 OR 121 OR 131 OR 132
                                      OR 133 OR 134 OR 138 OR 142
                                      OR 143 OR 144 OR 145 OR 146
                                      OR 147 OR 148 )
      *
               PERFORM 200211-IN01-EDIT-SEC
      *
           END-IF
      *
           .
       20021-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル編集処理
      *****************************************************************
       200211-IN01-EDIT-SEC          SECTION.
      *
           INITIALIZE                      IN01-REC
           MOVE    TNS-SRYKBN          TO  IN01-SRYKBN
           MOVE    TNS-KANANAME        TO  IN01-KANANAME
           MOVE    TNS-SRYCD           TO  IN01-SRYCD
           MOVE    ETENSU5-TANICD      TO  IN01-TANICD
           PERFORM 900-IN01-READ-SEC
           IF    ( FLG-IN01            =   ZERO )
               CONTINUE
           ELSE
               MOVE    TNS-NAME        TO  IN01-SRYNAME
               MOVE    ETENSU5-TANINAME
                                       TO  IN01-TANINAME
               MOVE    ETENSU5-KAISU   TO  IN01-KAISU
               IF    ( ETENSU5-TOKUREI =   1 )
                   MOVE    9           TO  IN01-JGNCNTERR
               END-IF
               MOVE    "2"             TO  IN01-MSTKBN
               WRITE   IN01-REC
           END-IF
      *
           .
       200211-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2001-HCMSL55-EDIT-SEC           SECTION.
      *
           OPEN    INPUT           IN01-FILE
      *
           PERFORM 901-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM 20011-HEAD-SEC
      *
               PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                        OR   ( FLG-END     NOT =   ZERO )
                        OR   ( IDXL           >=   CONST-HCMSL55-MAX )
      *
                   PERFORM 20011-BODY-SEC
      *
               END-PERFORM
      *
               PERFORM 20011-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE                   IN01-FILE
      *
           .
       2001-HCMSL55-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダ編集処理
      *****************************************************************
       20011-HEAD-SEC                  SECTION.
      *
           INITIALIZE                  HCMSL55
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    SPA-SRYYMD          TO WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1         TO FRM-HEAD1-KJNYMD
      *
           MOVE    SPA-SYSYMD          TO WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1         TO FRM-HEAD1-SAKYMD
      *
           MOVE    CNT-PAGE            TO FRM-HEAD1-PCNT
      *
           MOVE    FRM-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    FRM-HEAD2       TO  HCMSL55-MOJI (2)
           MOVE    ALL "-"         TO  HCMSL55-MOJI (3)
      *
           MOVE    3               TO  IDXL
      *
           .
       20011-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20011-BODY-SEC                  SECTION.
      *
           COMPUTE IDXL    =   IDXL    +   1
      *
           INITIALIZE                  FRM-BODY
      *
           MOVE    IN01-SRYCD      TO  FRM-BODY-SRYCD
           MOVE    IN01-SRYNAME    TO  FRM-BODY-SRYNAME
           MOVE    IN01-TANINAME   TO  FRM-BODY-TANINAME
           MOVE    IN01-KAISU      TO  FRM-BODY-KAISU
           IF    ( IN01-JGNCNTERR  =   9 )
               MOVE    "警告"      TO  FRM-BODY-WRNMSG
           END-IF
           IF    ( IN01-MSTKBN     =   "2" )
               MOVE    "（電子点数表）"
                                   TO  FRM-BODY-MSTMSG
           END-IF
           MOVE    FRM-BODY        TO  HCMSL55-MOJI (IDXL)
      *
           PERFORM 901-IN01-READ-SEC
      *
           .
       20011-BODY-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20011-PRINT-SEC                 SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"   TO  SPRT-PRTID
           MOVE    WRK-TITLE       TO  SPRT-TITLE
           MOVE    HCMSL55         TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       20011-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 800-FILE-DEL-SEC
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
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
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    ( STS-RECEERR         =   ZERO )
               CLOSE   RECEERR-FILE
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
               MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
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
      *
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル削除処理
      *****************************************************************
       800-FILE-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BX0301PARA      TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-FILE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF        ( WRK-SYMD        =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
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
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
           INVALID
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込処理
      *****************************************************************
       901-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    IN01-FILE       NEXT
               AT  END
                   MOVE    1       TO  FLG-IN01
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF  ( IN01-JGNCNTERR    =   9 )
                    AND( SYS-1101-SRYKAISUWRNKBN
                                           =   "2" )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               END-READ
      *
           END-PERFORM
      *
           .
       901-IN01-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）検索処理
      *****************************************************************
       900-ETENSU5-KEY4-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU5
      *
           INITIALIZE                      ETENSU5-REC
           MOVE    SPA-SRYYMD          TO  ETENSU5-YUKOSTYMD
                                           ETENSU5-YUKOEDYMD
           MOVE    ETENSU5-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU5-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU5
               INITIALIZE                  ETENSU5-REC
           END-IF
      *
           .
      *
       900-ETENSU5-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）FETCH処理
      *****************************************************************
       900-ETENSU5-KEY4-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU5
      *
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU5-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU5
               INITIALIZE                  ETENSU5-REC
           END-IF
      *
           .
      *
       900-ETENSU5-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数付加検索処理
      *****************************************************************
       900-TENSUPLUS-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUPLUS
      *
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD       TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD      TO  TNSP-YUKOSTYMD
                                       TNSP-YUKOEDYMD
           MOVE    TENSUPLUS-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TENSUPLUS-REC
           ELSE
               INITIALIZE              TENSUPLUS-REC
               MOVE    1           TO  FLG-TENSUPLUS
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSUPLUS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数検索処理
      *****************************************************************
       900-TENSU-KEY12M-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    "100000000"     TO  TNS-SRYCD
           MOVE    "199999999"     TO  TNSUP-SRYCD
           MOVE    SPA-SRYYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key12m"        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY12M-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数検索処理
      *****************************************************************
       900-TENSU-KEY12M-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key12m"        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-KEY12M-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-SRYYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理(FETCHは行なわない）
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
