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
       PROGRAM-ID.             ORCBX02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : チェックマスタ登録漏れ確認リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/14    NACL−太田    新規作成
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
           SELECT  TENSU-FILE      ASSIGN  SS004-FILENAME
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TNS-KEY
                                   FILE    STATUS  IS  STS-BX0201.
      *
           SELECT  BX0202-FILE     ASSIGN  BX0202PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BX0202-KEY
                                   FILE    STATUS  IS  STS-BX0202.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  TENSU-FILE.
           COPY    "CPTENSU.INC".
      *
       FD  BX0202-FILE.
       01  BX0202-REC.
           03  BX0202-KEY          PIC X(200).
           03  BX0202-SRYCD        PIC X(09).
           03  BX0202-NAME         PIC X(200).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                         PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCMSL55.INC".
      *
           COPY    "CPCOMMONDAT2.INC"      REPLACING   //RECE01//
                                           BY          //BX0202//.
      *
           COPY    "CPRECEERR.INC".
      *
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
           03  STS-BX0201                          PIC X(02).
           03  STS-BX0202                          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-BX0202                          PIC 9(01).
           03  FLG-CHKSND                          PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(05).
           03  CNT-SEQ                             PIC 9(05).
           03  CNT-X                               PIC 9(05).
           03  CNT-Y                               PIC 9(05).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX-PNT                             PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-CHKMORE                    PIC X(01).
           03  WRK-PARA-MOREKIKAN                  PIC 9(05).
           03  WRK-PARA-MORERANGE-G.
               05  WRK-PARA-MOREKENSA              PIC X(01).
               05  WRK-PARA-MOREBUI                PIC X(01).
           03  WRK-PARA-SRTKBN                     PIC X(01).
           03  WRK-PARA-KJNYMD                     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TITLE                           PIC X(100).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-SAKYMD-S                        PIC X(10).
           03  WRK-KIKAN-G.
               05  WRK-KIKAN                       PIC Z9.
               05  WRK-KIKANMES                    PIC X(10).
           03  WRK-KANA-STR                        PIC X(500).
           03  WRK-STSRYYM                         PIC X(6).
           03  WRK-EDSRYYM                         PIC X(6).
           03  WRK-YMD                             PIC X(08).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
      *
       01  SORTKEY-AREA.
           03  SORTKEY-KEY                         PIC X(200).
           03  SORTKEY1-KEY    REDEFINES           SORTKEY-KEY.
               05  SORTKEY1-SRYCD                  PIC X(09).
               05  SORTKEY1-DUMMY                  PIC X(191).
           03  SORTKEY2-KEY    REDEFINES           SORTKEY-KEY.
               05  SORTKEY2-KANANAME               PIC X(100).
               05  SORTKEY2-SRYCD                  PIC X(09).
               05  SORTKEY2-DUMMY                  PIC X(91).
           03  SORTKEY3-KEY    REDEFINES           SORTKEY-KEY.
               05  SORTKEY3-CDKBN-KBN              PIC X(01).
               05  SORTKEY3-CDKBN-KBNNUM           PIC 9(03).
               05  SORTKEY3-CDKBN-KBNNUM-EDA       PIC 9(02).
               05  SORTKEY3-CDKBN-KOUBAN           PIC 9(02).
               05  SORTKEY3-SRYCD                  PIC X(09).
               05  SORTKEY3-DUMMY                  PIC X(93).
      *
       01  FRM-AREA.
           03  FRM-HEAD1.
               05  FRM-HEAD1-MIDASI    PIC X(80).
               05  FRM-HEAD1-FL2       PIC X(08)   VALUE   "作成日：".
               05  FRM-HEAD1-SAKYMD    PIC X(09).
               05  FRM-HEAD1-FL5       PIC X(04)   VALUE   "  P-".
               05  FRM-HEAD1-PCNT      PIC ZZ9.
           03  FRM-HEAD2.
               05  FRM-HEAD2-FL1       PIC X(14)
                                       VALUE   "期間指定：".
               05  FRM-HEAD2-KIKAN     PIC X(10).
               05  FRM-HEAD2-FROM-TO   PIC X(19).
               05  FRM-HEAD2-FL2       PIC X(04)   VALUE   SPACE.
               05  FRM-HEAD2-RANGE     PIC X(20).
           03  FRM-BODY.
               05  FRM-BODY-OCC1                   OCCURS  52.
                   07  FRM-BODY-OCC2               OCCURS  2.
                       09  FRM-BODY-SRYCD          PIC X(09).
                       09  FRM-BODY-FL1            PIC X(01).
                       09  FRM-BODY-NAME           PIC X(60).
                       09  FRM-BODY-FL2            PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-X-MAX                     PIC 9(03)   VALUE   2.
           03  CONST-Y-MAX                     PIC 9(03)   VALUE   52.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "COMMON-SPA"            REPLACING   //SPA-//
                                           BY          //WSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    チェックマスタ２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSS004.INC".
           COPY    "CPORCSS013.INC".
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
           INITIALIZE                  SORTKEY-AREA
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
                                       WRK-PARA-CHKMORE
                                       WRK-PARA-MOREKIKAN
                                       WRK-PARA-MORERANGE-G
                                       WRK-PARA-SRTKBN
                                       WRK-PARA-KJNYMD
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-CHKMORE[" WRK-PARA-CHKMORE "]"
                   "MOREKIKAN[" WRK-PARA-MOREKIKAN "]"
                   "MORERANGE-G[" WRK-PARA-MORERANGE-G "]"
                   "SRTKBN[" WRK-PARA-SRTKBN "]"
                   "KJNYMD[" WRK-PARA-KJNYMD "]"
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
           EVALUATE    WRK-PARA-CHKMORE
           WHEN    "1"
               MOVE    "薬剤と病名登録漏れ確認リスト"
                                       TO  WRK-TITLE
           WHEN    "2"
               MOVE    "診療行為と病名登録漏れ確認リスト"
                                       TO  WRK-TITLE
           END-EVALUATE
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBX02"           TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPA-HOSPNUM         TO  BX0202PARA-HOSPNUM
           MOVE    "BX02002"           TO  BX0202PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BX0202PARA-TERMID
      *
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BX0202PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BX0202PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
           INITIALIZE                  SS004-AREA
           MOVE    WRK-PARA-CHKMORE    TO  SS004-SRYCDKBN
           MOVE    WRK-PARA-MOREKIKAN  TO  SS004-KIKAN
           MOVE    SPA-AREA            TO  WSPA-AREA
           MOVE    WRK-PARA-KJNYMD     TO  WSPA-SYSYMD
           CALL    "ORCSS004"          USING
                                       SS004-AREA
                                       WSPA-AREA
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
           PERFORM 2001-BX0202-EDIT-SEC
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
       2001-BX0202-EDIT-SEC        SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE    "DBCREATE"          TO  SS013-FUNC
           MOVE    WRK-PARA-CHKMORE    TO  SS013-CHKKBN
           MOVE    WRK-PARA-KJNYMD     TO  SS013-KJNYMD
           MOVE    "0"                 TO  SS013-TEKBYOKBN
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           OPEN    INPUT   TENSU-FILE
           OPEN    OUTPUT  BX0202-FILE
      *
           PERFORM 900-TENSU-READ-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
               INITIALIZE                  SS013-AREA
               MOVE    WRK-PARA-CHKMORE TO  SS013-CHKKBN
               MOVE    TNS-SRYCD       TO  SS013-SRYCD
               MOVE    "2"             TO  SS013-TEKBYOKBN
               MOVE    WRK-PARA-KJNYMD TO  SS013-KJNYMD
               MOVE    "DBINSERT"      TO  SS013-FUNC
               CALL     "ORCSS013"     USING
                                       SS013-AREA
                                       SPA-AREA
      *
               PERFORM 900-CHKSND-KEY3-SEL-SEC
               IF    ( FLG-CHKSND      =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 2001-BX0202-WRITE-SEC
               END-IF
      *
               PERFORM 900-TENSU-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   TENSU-FILE
                   BX0202-FILE
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       2001-BX0202-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル出力処理
      *****************************************************************
       2001-BX0202-WRITE-SEC           SECTION.
      *
           INITIALIZE                  BX0202-REC
           INITIALIZE                  SORTKEY-AREA
      *
           EVALUATE    WRK-PARA-SRTKBN
           WHEN    "2"
               MOVE    TNS-KANANAME    TO  SORTKEY2-KANANAME
               MOVE    TNS-SRYCD       TO  SORTKEY2-SRYCD
           WHEN    "3"
               MOVE    TNS-CDKBN-KBN   TO  SORTKEY3-CDKBN-KBN
               MOVE    TNS-CDKBN-KBNNUM
                                       TO  SORTKEY3-CDKBN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  SORTKEY3-CDKBN-KBNNUM-EDA
               MOVE    TNS-CDKBN-KOUBAN
                                       TO  SORTKEY3-CDKBN-KOUBAN
               MOVE    TNS-SRYCD       TO  SORTKEY3-SRYCD
           WHEN    OTHER
               MOVE    TNS-SRYCD       TO  SORTKEY1-SRYCD
           END-EVALUATE
      *
           MOVE    SORTKEY-KEY         TO  BX0202-KEY
           MOVE    TNS-SRYCD           TO  BX0202-SRYCD
           MOVE    TNS-NAME            TO  BX0202-NAME
      *
           WRITE   BX0202-REC
      *
           .
       2001-BX0202-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2001-HCMSL55-EDIT-SEC           SECTION.
      *
           OPEN    INPUT           BX0202-FILE
      *
           PERFORM 900-BX0202-READ-SEC
      *
           PERFORM UNTIL ( FLG-BX0202  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM 20011-HEAD-SEC
      *
               PERFORM UNTIL ( FLG-BX0202  NOT =   ZERO )
                        OR   ( FLG-END     NOT =   ZERO )
                        OR   ( CNT-X           >   CONST-X-MAX )
      *
                   PERFORM 20011-BODY-SEC
      *
               END-PERFORM
      *
               PERFORM 20011-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE                   BX0202-FILE
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
                                       FRM-BODY
      *
           MOVE    1               TO  CNT-X
           MOVE    ZERO            TO  CNT-Y
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           EVALUATE    WRK-PARA-CHKMORE
           WHEN    "1"
               MOVE    "＜　薬剤と病名登録漏れ確認リスト　＞"
                                       TO  FRM-HEAD1-MIDASI
           WHEN    "2"
               MOVE    "＜　診療行為と病名登録漏れ確認リスト　＞"
                                       TO  FRM-HEAD1-MIDASI
           END-EVALUATE
      *
           MOVE    SPA-SYSYMD          TO WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1         TO FRM-HEAD1-SAKYMD
      *
           MOVE    CNT-PAGE            TO FRM-HEAD1-PCNT
      *
           IF    ( WRK-PARA-MOREKIKAN  =  ZERO )
               MOVE    "なし"          TO FRM-HEAD2-KIKAN
           ELSE
               MOVE    WRK-PARA-MOREKIKAN
                                       TO  WRK-KIKAN
               MOVE    "ヶ月分"        TO  WRK-KIKANMES
               MOVE    WRK-KIKAN-G     TO  WRK-KANA-STR
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-KANA-STR    TO  FRM-HEAD2-KIKAN
      *
               MOVE    WRK-PARA-KJNYMD TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               COMPUTE WRK-ZOGEN
                   = ( WRK-PARA-MOREKIKAN  -   1 )  *   -1
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-YMD         TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  WRK-STSRYYM
      *
               MOVE    WRK-PARA-KJNYMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  WRK-EDSRYYM
      *
               STRING  "（"            DELIMITED BY SIZE
                       WRK-STSRYYM     DELIMITED BY SIZE
                       " - "           DELIMITED BY SIZE
                       WRK-EDSRYYM     DELIMITED BY SIZE
                       "）"            DELIMITED BY SIZE
               INTO    FRM-HEAD2-FROM-TO
               END-STRING
      *
           END-IF
      *
           MOVE    SPACE               TO  FRM-HEAD2-RANGE
           IF    ( WRK-PARA-CHKMORE    =  "2" )
      *
               MOVE    1               TO  IDX-PNT
               STRING  "対象："    DELIMITED   BY  SIZE
               INTO    FRM-HEAD2-RANGE
               WITH    POINTER     IDX-PNT
               END-STRING
      *
               IF    ( WRK-PARA-MOREKENSA  =   "1" )
                   STRING  "　"        DELIMITED   BY  SIZE
                           "検査"      DELIMITED   BY  SIZE
                   INTO    FRM-HEAD2-RANGE
                   WITH    POINTER     IDX-PNT
                   END-STRING
               END-IF
      *
               IF    ( WRK-PARA-MOREBUI    =   "1" )
                   STRING  "　"        DELIMITED   BY  SIZE
                           "部位"      DELIMITED   BY  SIZE
                   INTO    FRM-HEAD2-RANGE
                   WITH    POINTER     IDX-PNT
                   END-STRING
               END-IF
      *
           END-IF
      *
           MOVE    FRM-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    FRM-HEAD2       TO  HCMSL55-MOJI (2)
           MOVE    ALL "-"         TO  HCMSL55-MOJI (3)
      *
           .
       20011-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20011-BODY-SEC                  SECTION.
      *
           COMPUTE CNT-Y   =   CNT-Y +   1
      *
           MOVE    BX0202-SRYCD    TO  FRM-BODY-SRYCD (CNT-Y CNT-X)
           MOVE    BX0202-NAME     TO  FRM-BODY-NAME  (CNT-Y CNT-X)
      *
           INSPECT FRM-BODY-NAME (CNT-Y CNT-X)
                   REPLACING       ALL "  "    BY  "　"
      *
           IF    ( CNT-Y         >=  CONST-Y-MAX )
               COMPUTE CNT-X =   CNT-X  +   1
               MOVE    ZERO        TO  CNT-Y
           END-IF
      *
           PERFORM 900-BX0202-READ-SEC
      *
           .
       20011-BODY-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20011-PRINT-SEC                 SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-Y-MAX )
      *
               MOVE    FRM-BODY-OCC1 (IDX0)
                                   TO  HCMSL55-MOJI (IDX0 + 3)
      *
           END-PERFORM
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
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           IF    ( WRK-PARA-CHKMORE    =   "1" )
               MOVE    "薬剤と病名登録漏れ確認リスト"
                                   TO  SPRT-TITLE
           ELSE
               MOVE    "診療行為と病名登録漏れ確認リスト"
                                   TO  SPRT-TITLE
           END-IF
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
           MOVE    SS004-FILENAME  TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BX0202PARA      TO  IF-FILENAME
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
           MOVE    WRK-KANA-STR    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANA-STR
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
       900-TENSU-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    TENSU-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TENSU
               NOT AT  END
                   EVALUATE    WRK-PARA-CHKMORE
                   WHEN    "1"
                       IF    ( TNS-YKZKBN              =   3 )
                           CONTINUE
                       ELSE
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
                   WHEN    "2"
                       IF    ( WRK-PARA-MOREKENSA      =   "1" )
                        AND  ( TNS-SRYCD (1:2)         =   "16" )
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
                       IF    ( WRK-PARA-MOREBUI        =   "1" )
                        AND  ( TNS-SRYCD (1:3)         =   "002" )
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
                   END-EVALUATE
               END-READ
      *
           END-PERFORM
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込処理
      *****************************************************************
       900-BX0202-READ-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BX0202
      *
           READ    BX0202-FILE     NEXT
           AT  END
               MOVE    1           TO  FLG-BX0202
           END-READ
      *
           .
       900-BX0202-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    WRK-PARA-HOSPNUM TO CHKSND-HOSPNUM
           MOVE    WRK-PARA-CHKMORE TO CHKSND-CHKKBN
           MOVE    TNS-SRYCD       TO  CHKSND-SRYCD
           MOVE    WRK-PARA-KJNYMD TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           MOVE    "0"             TO  CHKSND-CDKBN
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKSND-REC
           ELSE
               INITIALIZE              CHKSND-REC
               MOVE    1           TO  FLG-CHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY3-SEL-EXT.
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
