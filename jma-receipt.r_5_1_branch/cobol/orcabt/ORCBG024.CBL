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
       PROGRAM-ID.             ORCBG024.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 医療区分・ＡＤＬ区分に係わる評価票一括発行
      *  管理者            : 太田
      *  作成日付    作業者        記述
      *  15/09/09    NACL-太田     新規作成
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
      *    一時ファイル
           SELECT  IN01-FILE       ASSIGN  BG0241
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
      *    一時ファイル
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-SRT-KEY    PIC     X(200).
               05  IN01-SRT-SEQ    PIC     9(10).
           03  IN01-PTID           PIC     9(10).
           03  IN01-PTNUM          PIC     X(20).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    一時ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0241//.
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-IN01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-NACCT           PIC 9(01).
           03  FLG-NSRY            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-IN01            PIC 9(01).
           03  FLG-BREAK           PIC 9(01).
           03  FLG-SKIP            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(03).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-SEQ             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDXD                PIC 9(05).
           03  IDXK                PIC 9(05).
           03  IDXR                PIC 9(05).
           03  IDXS                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SRYYM      PIC X(08).
           03  WRK-PARA-PTNUM      PIC X(20).
           03  WRK-PARA-BTUNUM     PIC X(02).
           03  WRK-PARA-SRTKBN     PIC X(01).
           03  WRK-PARA-RESTRICTION PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(100).
           03  WRK-TITLE               PIC X(100).
           03  WRK-PTID                PIC 9(10).
           03  WRK-SYMD                PIC X(08).
           03  WRK-EOMYMD              PIC X(08).
           03  WRK-TNS-SRYCD           PIC X(09).
           03  WRK-TNS-KJNYMD          PIC X(08).
           03  WRK-RYOYO-DAY-G.
               05  WRK-RYOYO-DAY       PIC 9(01)
                                       OCCURS  31.
      *
       01  TSRT-AREA.
           03  TSRT-KEY                PIC X(200).
           03  TSRT-KEY1    REDEFINES  TSRT-KEY.
               05  TSRT-KEY1-BRMNUM    PIC X(08).
               05  TSRT-KEY1-KANANAME  PIC X(100).
               05  TSRT-KEY1-PTNUM     PIC X(20).
               05  TSRT-KEY1-FILLER    PIC X(72).
           03  TSRT-KEY2    REDEFINES  TSRT-KEY.
               05  TSRT-KEY2-BRMNUM    PIC X(08).
               05  TSRT-KEY2-PTNUM     PIC X(20).
               05  TSRT-KEY2-FILLER    PIC X(172).
           03  TSRT-KEY3    REDEFINES  TSRT-KEY.
               05  TSRT-KEY3-KANANAME  PIC X(100).
               05  TSRT-KEY3-PTNUM     PIC X(20).
               05  TSRT-KEY3-FILLER    PIC X(80).
           03  TSRT-KEY4    REDEFINES  TSRT-KEY.
               05  TSRT-KEY4-PTNUM     PIC X(20).
               05  TSRT-KEY4-FILLER    PIC X(180).
           03  TSRT-KEY5    REDEFINES  TSRT-KEY.
               05  TSRT-KEY5-RENNUM    PIC 9(10).
               05  TSRT-KEY5-FILLER    PIC X(190).
      *
       01  TRYOYO-AREA.
           05  TRYOYO-DAY-G.
               07  TRYOYO-DAY          PIC 9(1)
                                       OCCURS  31.
      *
       01  CONST-AREA.
           03  CONST-BTPARA-SHELLID        PIC X(08)
                                           VALUE   "ORCBQL01".
           03  CONST-BTPARA-JOBID          PIC 9(07)
                                           VALUE   2.
           03  CONST-BTPARA-DSP-RENNUM     PIC 9(03)
                                           VALUE   2.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPRYOYOCD.INC".
      *
       01  TNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TNRRK-//.
      *
       01  TNACCT-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT-//
                                       BY          //TNACCT-//.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    療養レセコメントデータ作成
           COPY    "CPORCSS005.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    医療区分・ＡＤＬ評価票
           COPY    "CPORCHCN68.INC".
           COPY    "CPONPRTDATA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ****************************************************************
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
           STOP   RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-PTNUM
                                               WRK-PARA-BTUNUM
                                               WRK-PARA-SRTKBN
                                               WRK-PARA-RESTRICTION
           END-UNSTRING
      *
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
                                           SPA-SRYYMD
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
      *
           MOVE    SPACE               TO  WRK-TITLE
           STRING  "医療区分・ＡＤＬ区分に係わる評価票一括発行"
                                       DELIMITED   BY  SIZE
           INTO    WRK-TITLE
           END-STRING
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG024"      TO  JOB-PGID
           MOVE    WRK-TITLE       TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "BG02401"       TO  BG0241-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0241-TERMID
           MOVE    SPA-HOSPNUM     TO  BG0241-HOSPNUM
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BG0241-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BG0241-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-EOMYMD-SEC
           MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
           PERFORM 110-PARA-SEC
      *
           PERFORM 110-ORCSPRTAREA-EDIT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-SEC                    SECTION.
      *
           .
       110-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷パラメタエリアセット処理
      *****************************************************************
       110-ORCSPRTAREA-EDIT-SEC        SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           .
       110-ORCSPRTAREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2000-RYOYO-NYUIN-CHK-SEC
      *
           PERFORM 2001-ORCHCN68-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病床入院チェック処理
      *****************************************************************
       2000-RYOYO-NYUIN-CHK-SEC        SECTION.
      *
           OPEN    OUTPUT              IN01-FILE
      *
           MOVE    ZERO            TO  CNT-SEQ
      *
           MOVE    ZERO            TO  WRK-PTID
      *
           EVALUATE    TRUE
           WHEN  ( WRK-PARA-PTNUM      =   "*" )
      *
               PERFORM 900-BTPARA-KEY8-SEL-SEC
      *
               PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
      *
                   MOVE    BTPARA-INFO-PARA    TO  QTOANY-AREA
                   MOVE    QTOANY-PTID TO  WRK-PTID
                   PERFORM 2001-RYOYO-NYUIN-CHK-SEC
      *
                   PERFORM 900-BTPARA-KEY8-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_btpara"    TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           WHEN  ( WRK-PARA-PTNUM  NOT =   SPACE )
      *
               PERFORM 900-PTNUM-KEY4-SEL-SEC
               IF    ( FLG-PTNUM       =   ZERO )
                   MOVE    PTNUM-PTID      TO  WRK-PTID
                   PERFORM 2001-RYOYO-NYUIN-CHK-SEC
               END-IF
      *
           WHEN    OTHER
               PERFORM 2001-RYOYO-NYUIN-CHK-SEC
           END-EVALUATE
      *
           CLOSE   IN01-FILE
      *
           .
       2000-RYOYO-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養病床入院チェック処理
      *****************************************************************
       2001-RYOYO-NYUIN-CHK-SEC        SECTION.
      *
           COMPUTE CNT-SEQ =   CNT-SEQ +   1
      *
           PERFORM 900-NYUINACCT-KEY57-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
      *
               INITIALIZE                  TNACCT-AREA
                                           TRYOYO-AREA
               MOVE    NACCT-PTID      TO  SPA-PTID
      *
               PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
                        OR   ( NACCT-PTID  NOT =   SPA-PTID )
      *
                   EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       PERFORM 20011-RYOYO-NYUIN-CHK-SEC
                   WHEN    "5"
                       MOVE    NYUINACCT-REC
                                       TO  TNACCT-AREA
                   END-EVALUATE
      *
                   PERFORM 900-NYUINACCT-KEY57-FET-SEC
      *
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                        OR   ( TRYOYO-DAY-G    =   ZERO )
      *
                   IF    ( TNACCT-DAY (IDXD)   =   ZERO )
                       MOVE    ZERO    TO  TRYOYO-DAY (IDXD)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TRYOYO-DAY-G    NOT =   ZERO )
                   PERFORM 20011-IN01-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-RYOYO-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養病床入院チェック処理
      *****************************************************************
       20011-RYOYO-NYUIN-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-RYOYO-DAY-G
           MOVE    ZERO            TO  FLG-BREAK
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL  ( FLG-NSRY   NOT =   ZERO )
                    OR    ( FLG-BREAK  NOT =   ZERO )
      *
               PERFORM VARYING IDXS    FROM    1   BY  1
                       UNTIL ( IDXS    >   5 )
                        OR   ( FLG-BREAK   NOT =   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDXS)   NOT =   SPACE )
                       PERFORM 200111-RYOYO-NYUIN-CHK-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF     ( WRK-RYOYO-DAY (IDXD)   =   1 )
                   MOVE    1       TO  TRYOYO-DAY (IDXD)
               END-IF
      *
           END-PERFORM
      *
           .
       20011-RYOYO-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養病床入院チェック処理
      *****************************************************************
       200111-RYOYO-NYUIN-CHK-SEC      SECTION.
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   CONST-RYOYOCD-KHN-MAX )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   CONST-RYOYOCD-CD-MAX  )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDXS)
                                   =   CONST-RYOYOCD  (IDXK IDXR))
      *
                       PERFORM VARYING IDXD FROM    1   BY  1
                               UNTIL ( IDXD >   31 )
      *
                           IF    ( NACCT-DAY (IDXD) >   ZERO )
                               MOVE    1   TO  WRK-RYOYO-DAY (IDXD)
                           END-IF
      *
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF    ( WRK-RYOYO-DAY-G         =   ZERO )
               MOVE    NSRY-SRYCD (IDXS)   TO  WRK-TNS-SRYCD
               MOVE    WRK-PARA-SRYYM      TO  WRK-TNS-KJNYMD
               MOVE    "01"                TO  WRK-TNS-KJNYMD (7:2)
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-SRYCD (1:1) =      "1" )
                AND  ( TNS-DATAKBN     =       1 )
      *
                   PERFORM VARYING IDXD FROM    1   BY  1
                           UNTIL ( IDXD >   31 )
      *
                       IF    ( NACCT-DAY (IDXD) >   ZERO )
                           MOVE     9      TO  WRK-RYOYO-DAY (IDXD)
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           IF    ( WRK-RYOYO-DAY-G     NOT =   ZERO )
               MOVE    1                   TO  FLG-BREAK
           END-IF
      *
           .
       200111-RYOYO-NYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル編集処理
      *****************************************************************
       20011-IN01-EDIT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SKIP
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           IF    ( WRK-PARA-RESTRICTION  =   "1" )
               PERFORM 900-PTNYUINRRK-KEY10-SEL-SEC
               IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
                   MOVE    1           TO  FLG-SKIP
               END-IF
           END-IF
      *
           PERFORM 900-PTNYUINRRK-KEY29-SEL-SEC
      *
           IF    ( WRK-PARA-BTUNUM     NOT =   SPACE )
            AND  ( WRK-PARA-BTUNUM     NOT =   PTNYUINRRK-BTUNUM )
               MOVE    1               TO  FLG-SKIP
           END-IF
      *
           IF    ( WRK-PARA-PTNUM  =   SPACE )
               IF    ( PTINF-TSTPTNUMKBN   =   "1" )
                   MOVE    1           TO  FLG-SKIP
               END-IF
           END-IF
      *
           IF    ( FLG-SKIP        =   ZERO )
               PERFORM 200111-IN01-EDIT-SEC
           END-IF
      *
           .
       20011-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル編集処理
      *****************************************************************
       200111-IN01-EDIT-SEC             SECTION.
      *
           INITIALIZE                  IN01-REC
                                       TSRT-AREA
      *
           EVALUATE    WRK-PARA-SRTKBN
           WHEN    "2"
               MOVE    PTNYUINRRK-BRMNUM   TO  TSRT-KEY2-BRMNUM
               MOVE    PTNUM-PTNUM         TO  TSRT-KEY2-PTNUM
           WHEN    "3"
               MOVE    PTINF-KANANAME      TO  TSRT-KEY3-KANANAME
               MOVE    PTNUM-PTNUM         TO  TSRT-KEY3-PTNUM
           WHEN    "4"
               MOVE    PTNUM-PTNUM         TO  TSRT-KEY4-PTNUM
           WHEN    OTHER
               MOVE    PTNYUINRRK-BRMNUM   TO  TSRT-KEY1-BRMNUM
               MOVE    PTINF-KANANAME      TO  TSRT-KEY1-KANANAME
               MOVE    PTNUM-PTNUM         TO  TSRT-KEY1-PTNUM
           END-EVALUATE
      *
           IF    ( WRK-PARA-SRTKBN         =   "5" )
            AND  ( WRK-PARA-PTNUM          =   "*" )
               MOVE    CNT-SEQ             TO  TSRT-KEY5-RENNUM
           END-IF
      *
           MOVE    TSRT-KEY            TO  IN01-SRT-KEY
           MOVE    CNT-SEQ             TO  IN01-SRT-SEQ
           MOVE    SPA-PTID            TO  IN01-PTID
           MOVE    PTNUM-PTNUM         TO  IN01-PTNUM
           WRITE   IN01-REC
      *
           .
       200111-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       2001-ORCHCN68-SEC                   SECTION.
      *
           OPEN    INPUT                   IN01-FILE
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM 20011-ORCHCN68-SEC
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   IN01-FILE
      *
           .
       2001-ORCHCN68-EXT.
           EXIT.
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       20011-ORCHCN68-SEC                   SECTION.
      *
           MOVE    SPA-AREA        TO  TSPA-AREA
           MOVE    IN01-PTID       TO  TSPA-PTID
      *
           INITIALIZE                  SS005-AREA
           MOVE    WRK-PARA-SRYYM  TO  SS005-SRYYM
           CALL    "ORCSS005"      USING
                                   SS005-AREA
                                   TSPA-AREA
           IF    ( SS005-RC        =     ZERO )
      *
               INITIALIZE                    ORCHCN68AREA
               MOVE    "1"             TO    ORCHCN68-PRTKBN
      *
               MOVE    WRK-PARA-SRYYM  TO    ORCHCN68-SRYYM
               MOVE    IN01-PTID       TO    ORCHCN68-PTID
               MOVE    IN01-PTNUM      TO    ORCHCN68-PTNUM
               MOVE    "1"             TO    ORCHCN68-BATCH
               MOVE    ORCSPRTAREA     TO    ORCHCN68-PRTAREA
               MOVE    ORCHCN68AREA    TO    ONPRTDATA-DATA
               CALL    "ORCHCN68"      USING
                                       TSPA-AREA
                                       ONPRTDATA
               MOVE    ONPRTDATA-DATA  TO    ORCHCN68AREA
      *
               COMPUTE CNT-PAGE  = CNT-PAGE  + ORCHCN68-CNT
      *
           ELSE
               MOVE    "印刷処理に失敗しました"
                                       TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-ORCHCN68-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
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
           MOVE    BG0241          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           IF    ( CNT-PAGE        =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
      *    月末日取得処理
      *****************************************************************
       800-EOMYMD-SEC                  SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-EOMYMD-EXT.
            EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    WRK-PARA-PTNUM      TO  PTNUM-PTNUM
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY10-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      TNRRK-AREA
           MOVE    SPA-HOSPNUM         TO  TNRRK-HOSPNUM
           MOVE    SPA-PTID            TO  TNRRK-PTID
           MOVE    WRK-EOMYMD          TO  TNRRK-NYUINYMD
                                           TNRRK-TAIINYMD
           MOVE    TNRRK-AREA           TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNRRK-AREA
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  TNRRK-AREA
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY29-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY29-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY57-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    WRK-PTID            TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY57-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY57-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NSRY
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NSRY
           END-IF
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為FETCH処理
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NSRY
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NSRY
           END-IF
      *
           .
       900-NYUINACT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
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
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY8-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID
                                   TO  BTPARA-JOBID
           MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  BTPARA-DSP-RENNUM
      *
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY8-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-IN01
      *
           READ    IN01-FILE       NEXT
           AT  END
               MOVE    1               TO  FLG-IN01
           END-READ
      *
           .
      *
       900-IN01-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
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
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
