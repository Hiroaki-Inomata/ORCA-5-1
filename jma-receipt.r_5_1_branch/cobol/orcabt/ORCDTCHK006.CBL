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
       PROGRAM-ID.                     ORCDTCHK006.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 入院会計情報抽出
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/03/31    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    データチェック中間ファイル（受診患者）
           SELECT  DTCHK002-FILE   ASSIGN  DTCHK002PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック診療会計
           SELECT  DCACCT-FILE
                                   ASSIGN  DTCHKACCTPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCACCT-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック診療行為
           SELECT  DCSRY-FILE
                                   ASSIGN  DTCHKSRYPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCSRY-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック中間ファイル（受診患者）
       FD  DTCHK002-FILE.
           COPY    "CPDTCHK002.INC".
      *
      *    データチェック診療会計
       FD  DCACCT-FILE.
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    データチェック診療行為
       FD  DCSRY-FILE.
       01  DCSRY-REC.
           COPY    "CPDCSRY.INC".
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK002PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKACCTPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSRYPARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                  PIC X(02).
           03  STS-DTCHKERR                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TNYUINRRK               PIC 9(01).
           03  FLG-DTCHK002                PIC 9(01).
           03  FLG-LOOP-END                PIC 9(01).
           03  FLG-NODATA                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ZAINUM                  PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-DTCHKERR                PIC X(200).
           03  WRK-JOB-YOBI                PIC X(500).
           03  WRK-COMB-HKNCOMBINUM        PIC 9(04).
           03  WRK-HKNCOMBINUM-G.
               05  WRK-HKNCOMBINUM         PIC 9(04).
               05  WRK-HKNCOMBINUM-SKJHKN  PIC X(01).
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-KOHNUM                  PIC X(03).
           03  WRK-RSI-HKNKBN              PIC X(01).
           03  WRK-KOHID                   PIC 9(10).
           03  WRK-LASTCOMB                PIC X(04).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-HBTSRYKA-G.
               05  WRK-HBTSRYKA-ALL        PIC 9(01)   OCCURS  31.
               05  WRK-HBTSRYKA-HBTKA      PIC X(02)   OCCURS  31.
               05  WRK-HBTSRYKA-MAX        PIC 9(05).
               05  WRK-HBTSRYKA-OCC        OCCURS  100.
                   07  WRK-HBTSRYKA        PIC X(02).
                   07  WRK-HBTSRYKA-DAY    PIC 9(01)   OCCURS  31.
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                 PIC 9(03)   OCCURS  31.
           03  WRK-HKNCOMBI.
               05  WRK-HKNCOMBI9           PIC 9(04).
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
      *
       01  TCOMBI-AREA.
           03  TCOMBI-MAX                  PIC 9(05).
           03  TCOMBI-HKNCOMBI-OCC         OCCURS  50.
               05  TCOMBI-HKNCOMBI         PIC 9(04).
               05  TCOMBI-HKNCOMBI-CNT     PIC 9(03).
               05  TCOMBI-HKNCOMBI-DAY     PIC 9(01)
                                           OCCURS  31.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H2910                 PIC X(06) VALUE "201710".
           03  CONST-HKNCOMBICD            PIC X(09) VALUE  "999999912".
           03  CONST-HBTSRYKA-MAX          PIC 9(05) VALUE  100.
           03  CONST-SRYCD-MAX             PIC 9(05) VALUE 20.
           03  CONST-TCOMBI-MAX            PIC 9(05) VALUE 50.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入院履歴
       01  TNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                   REPLACING  //PTNYUINRRK//
                                   BY         //TNYUINRRK//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  DTCHK-RC        PIC S9(09).
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //LNKDTCHKERRPARA//.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
                                               DTCHK-RC
                                               WRK-PARA
                                               LNKDTCHKERRPARA
                                               ORCBSD01PARA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           DISPLAY "*** ORCDTCHK006 START ***"
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  DTCHK-RC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           MOVE    LNKDTCHKERRPARA     TO  DTCHKERRPARA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               PERFORM 900-DBSTART-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCDTCHK006"       TO  JOB-PGID
               MOVE    "入院会計情報抽出"  TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK002PARA-HOSPNUM
           MOVE    "DTCHK002"          TO  DTCHK002PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK002PARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKACCTPARA-HOSPNUM
           MOVE    "DCACCT00"          TO  DTCHKACCTPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKACCTPARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSRYPARA-HOSPNUM
           MOVE    "DCSRY000"          TO  DTCHKSRYPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSRYPARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
      *
           MOVE    DTCHK002PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    DTCHKACCTPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (2)
           MOVE    DTCHKSRYPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (3)
      *
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK002PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  DTCHKACCTPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  DTCHKSRYPARA-FULLNAME
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    診療会計編集処理
           IF    ( ORCBSD01-IKTKBT     =   "2" )
               PERFORM 2002-SRYACCT-EDIT-SEC
           ELSE
               PERFORM 2001-SRYACCT-EDIT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       2001-SRYACCT-EDIT-SEC           SECTION.
      *
           OPEN    I-O     DCACCT-FILE
                           DCSRY-FILE
      *
           PERFORM 900-PTNYUINRRK-KEY43-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TNYUINRRK NOT =   ZERO )
      *
               PERFORM 2001-HBTSRYKA-HEN-SEC
      *
               PERFORM 20011-SRYACCT-EDIT-SEC
      *
               PERFORM 900-PTNYUINRRK-KEY43-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE           DCACCT-FILE
                           DCSRY-FILE
      *
           .
       2001-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       20011-SRYACCT-EDIT-SEC          SECTION.
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT   =   ZERO )
      *
               PERFORM 2001-TCOMBI-HEN-SEC
      *
               PERFORM 2001-NACCT-EDIT-SEC
      *
           ELSE
      *
               PERFORM 200111-HKNCOMBI-EDIT-SEC
               MOVE    WRK-HKNCOMBI    TO  WRK-COMB-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               INITIALIZE                  PTRSIINF-REC
               IF    ( COMB-HKNNUM         =   "971"  OR  "973" )
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
               END-IF
      *
               INITIALIZE                  NYUINACCT-REC
               MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
               MOVE    "1"             TO  NACCT-NYUGAIKBN
               MOVE    TNYUINRRK-PTID  TO  NACCT-PTID
               MOVE    WRK-HKNCOMBI    TO  NACCT-HKNCOMBI
                                           NACCT-SRYKA
               MOVE    "90"            TO  NACCT-SRYKBN
               MOVE    ORCBSD01-SRYYM  TO  NACCT-SRYYM
               MOVE    ZERO            TO  NACCT-ZAITEN
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( WRK-HBTSRYKA-ALL (IDX0) >   ZERO )
                       COMPUTE NACCT-ZAIKAISU
                           =   NACCT-ZAIKAISU  +   1
                       MOVE    1       TO  NACCT-DAY (IDX0)
                   END-IF
               END-PERFORM
               MOVE    1               TO  FLG-NODATA
               MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOHHKNNUM (1)
                                       TO  WRK-KOHNUM
               MOVE    COMB-KOHID  (1) TO  WRK-KOHID
               MOVE    PTRSI-HKNKBN    TO  WRK-RSI-HKNKBN
               PERFORM 20012-DCACCT-INS-SEC
           END-IF
      *
           .
       20011-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       200111-HKNCOMBI-EDIT-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-HKNCOMBI9
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM     TO  WRK-SRYYM
           PERFORM VARYING IDX0    FROM    31   BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( WRK-HKNCOMBI9   NOT  =   ZERO )
               IF    ( NACCT-DAY (IDX0)    >    ZERO )
                   MOVE    IDX0    TO  WRK-SRYDD
                   IF    ( WRK-NYUINYMD    <=  WRK-SRYYMD )
                       MOVE    NACCT-DAY (IDX0)
                                   TO  WRK-HKNCOMBI9
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( WRK-HKNCOMBI9   =   ZERO )
               MOVE    WRK-LASTCOMB    TO  WRK-HKNCOMBI
           END-IF
      *
           .
       200111-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       2002-SRYACCT-EDIT-SEC           SECTION.
      *
           OPEN    INPUT   DTCHK002-FILE
           OPEN    I-O     DCACCT-FILE
                           DCSRY-FILE
      *
           PERFORM 900-DTCHK002-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK002    NOT =   ZERO )
      *
               INITIALIZE                  TNYUINRRK-REC
               MOVE    DTCHK002-PTID   TO  TNYUINRRK-PTID
               PERFORM 2001-HBTSRYKA-HEN-SEC
      *
               PERFORM 20011-SRYACCT-EDIT-SEC
      *
               PERFORM 900-DTCHK002-READ-SEC
      *
           END-PERFORM
      *
           CLOSE           DTCHK002-FILE
           CLOSE           DCACCT-FILE
                           DCSRY-FILE
      *
           .
       2002-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日別診療科編集処理
      *****************************************************************
       2001-HBTSRYKA-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-HBTSRYKA-G
           MOVE    ZERO                TO  WRK-LASTCOMB
           MOVE    ALL "9"             TO  WRK-NYUINYMD
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   ORCBSD01-SRYYM )
                   MOVE    1           TO  WRK-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  WRK-STIDX
               END-IF
      *
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   ORCBSD01-SRYYM )
                   MOVE    31          TO  WRK-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  WRK-EDIDX
               END-IF
      *
               PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       WRK-EDIDX )
                   MOVE    1           TO  WRK-HBTSRYKA-ALL   (IDX-DAY)
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-HBTSRYKA-HBTKA (IDX-DAY)
                   MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-LASTCOMB
                   IF    ( PTNYUINRRK-NYUINYMD     <   WRK-NYUINYMD )
                       MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NYUINYMD
                   END-IF
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( WRK-HBTSRYKA-HBTKA (IDX-DAY)    NOT =   ZERO )
      *
                   PERFORM VARYING IDX5    FROM    1   BY  1
                       UNTIL ( IDX5    >   WRK-HBTSRYKA-MAX )
                       OR    ( WRK-HBTSRYKA (IDX5)
                                       = WRK-HBTSRYKA-HBTKA (IDX-DAY) )
      *
                       CONTINUE
      *
                   END-PERFORM
      *
                   MOVE    ZERO        TO  IDX6
      *
                   IF    ( IDX5    >   WRK-HBTSRYKA-MAX )
      *
                       IF    ( WRK-HBTSRYKA-MAX 
                                               <   CONST-HBTSRYKA-MAX )
                           COMPUTE WRK-HBTSRYKA-MAX
                               =   WRK-HBTSRYKA-MAX    +   1
      *
                           MOVE    WRK-HBTSRYKA-MAX    TO  IDX6
      *
                           MOVE    WRK-HBTSRYKA-HBTKA (IDX-DAY)
                                       TO  WRK-HBTSRYKA (IDX6)
                       END-IF
                   ELSE
                       MOVE    IDX5    TO  IDX6
                   END-IF
      *
                   IF    ( IDX6        >   ZERO )
                       MOVE    1   TO  WRK-HBTSRYKA-DAY (IDX6 IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *
           .
       2001-HBTSRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       2001-TCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  TCOMBI-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   31 )
      *
               IF    ( NACCT-DAY (IDX1)    >   ZERO )
      *
                   INITIALIZE                      WRK-HKNCOMBINUM-G
                   MOVE    NACCT-DAY (IDX1)    TO  WRK-HKNCOMBINUM
                   PERFORM 20011-TCOMBI-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    NACCT-HOSPNUM       TO  LNK-NYUINDAY-HOSPNUM
           MOVE    NACCT-PTID          TO  LNK-NYUINDAY-PTID
           MOVE    NACCT-SRYYM         TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
               INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
      *
           PERFORM VARYING IDX1   FROM    1   BY  1
                   UNTIL ( IDX1   >       31 )
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDX1) = "1" )
                   INITIALIZE              WRK-HKNCOMBINUM-G
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                                       TO  WRK-HKNCOMBINUM
                   PERFORM 20011-TCOMBI-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( NACCT-SRYYM         >= CONST-H2910 )
               INITIALIZE              ORCSSKJHKN-AREA
               MOVE    NACCT-HOSPNUM   TO  LNK-SKJHKN-HOSPNUM
               MOVE    NACCT-PTID      TO  LNK-SKJHKN-PTID
               MOVE    NACCT-SRYYM     TO  LNK-SKJHKN-SANTEIYM
               CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
               IF    ( LNK-SKJHKN-RC   =   ZERO )
                   PERFORM VARYING IDX1   FROM    1   BY  1
                           UNTIL ( IDX1   >       31 )
                       PERFORM VARYING IDX9    FROM    1   BY  1
                               UNTIL ( IDX9    >   2 )
                           IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDX1 IDX9)
                                       NOT =   SPACE )
                               MOVE LNK-SKJHKN-HKNCOMBINUM (IDX1 IDX9)
                                       TO  WRK-HKNCOMBINUM-G
                               MOVE "1"
                                       TO  WRK-HKNCOMBINUM-SKJHKN
                               PERFORM 20011-TCOMBI-HEN-SEC
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       2001-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       20011-TCOMBI-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2   >   TCOMBI-MAX )
                   OR    ( WRK-HKNCOMBINUM
                                   =   TCOMBI-HKNCOMBI (IDX2) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    (     IDX2                >   TCOMBI-MAX )
               IF    ( CONST-TCOMBI-MAX    >   TCOMBI-MAX )
                   COMPUTE TCOMBI-MAX  =   TCOMBI-MAX  +   1
                   MOVE    WRK-HKNCOMBINUM
                                           TO  TCOMBI-HKNCOMBI
                                                       (TCOMBI-MAX)
                   COMPUTE TCOMBI-HKNCOMBI-CNT (TCOMBI-MAX)
                       =   TCOMBI-HKNCOMBI-CNT (TCOMBI-MAX)
                       +   1
                   IF    ( WRK-HKNCOMBINUM-SKJHKN  =   SPACE )
                       MOVE    1       TO  TCOMBI-HKNCOMBI-DAY
                                               (TCOMBI-MAX IDX1)
                   END-IF
               ELSE
                   IF    ( TCOMBI-HKNCOMBI-DAY (IDX2 IDX1)
                                           =    ZERO )
                       COMPUTE TCOMBI-HKNCOMBI-CNT (IDX2)
                           =   TCOMBI-HKNCOMBI-CNT (IDX2)
                           +   1
                       IF    ( WRK-HKNCOMBINUM-SKJHKN =   SPACE )
                           MOVE    1   TO  TCOMBI-HKNCOMBI-DAY
                                               (IDX2 IDX1)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       2001-NACCT-EDIT-SEC             SECTION.
      *
           MOVE    ZERO                TO  CNT-ZAINUM
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   TCOMBI-MAX )
      *
               MOVE    TCOMBI-HKNCOMBI (IDX3)  TO  WRK-COMB-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI            NOT =   ZERO )
                   MOVE    WRK-COMB-HKNCOMBINUM
                                               TO  COMB-HKNCOMBINUM
               END-IF
               INITIALIZE                  PTRSIINF-REC
               IF    ( COMB-HKNNUM         =   "971"  OR  "973" )
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
               END-IF
      *
               INITIALIZE                  HCM34S01-AREA
      *
               MOVE    WRK-PARA-HOSPNUM     TO  HCM34S01-HOSPNUM
               MOVE    NACCT-PTID          TO  HCM34S01-PTID
               MOVE    NACCT-SRYYM         TO  HCM34S01-SANTEIYM
               MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
               MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
      *
               IF    ( COMB-HKNNUM         =     "971" OR "973" )
      *
                   CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                     SPA-AREA
               ELSE
                   CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                     SPA-AREA
               END-IF
      *
               PERFORM 20011-DCACCT-INS-SEC
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  FLG-NODATA
           MOVE    SPACE           TO  WRK-HKNNUM
                                       WRK-KOHNUM
                                       WRK-RSI-HKNKBN
           MOVE    ZERO            TO  WRK-KOHID
           PERFORM 20012-DCACCT-INS-SEC
      *
           .
       2001-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       20011-DCACCT-INS-SEC                SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   HCM34S01-HENKYAKU-MAX )
      *
               IF    ( HCM34S01-ZAISKBKBN (IDX4)   NOT =   "3" )
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   WRK-HBTSRYKA-MAX )
      *
                       MOVE    ZERO    TO  WRK-DAY-G
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( WRK-HBTSRYKA-DAY (IDX7 IDX-DAY )
                                               > ZERO )
                            AND  ( HCM34S01-DAY     (IDX4 IDX-DAY )
                                               > ZERO )
                               MOVE    HCM34S01-DAY     (IDX4 IDX-DAY )
                                       TO  WRK-DAY (IDX-DAY)
                           END-IF
                       END-PERFORM
      *
                       IF    ( WRK-DAY-G   NOT =   ZERO )
                           PERFORM 200111-DCACCT-INS-SEC
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       20011-DCACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       200111-DCACCT-INS-SEC           SECTION.
      *
           COMPUTE CNT-ZAINUM  =   CNT-ZAINUM  +   1
      *
           PERFORM 200111-DCSRY-INS-SEC
      *
           INITIALIZE                      DCACCT-REC
           MOVE    NACCT-PTID          TO  DCACCT-PTID
           MOVE    CNT-ZAINUM          TO  DCACCT-ZAINUM
           MOVE    "2"                 TO  DCACCT-ACCTKBN
           MOVE    WRK-HBTSRYKA    (IDX7)
                                       TO  DCACCT-SRYKA
           MOVE    HCM34S01-SRYKBN (IDX4)
                                       TO  DCACCT-SRYKBN
           MOVE    SPACE               TO  DCACCT-JIHOKBN
           MOVE    COMB-HKNCOMBINUM    TO  DCACCT-HKNCOMBI
           MOVE    HCM34S01-ZAITEN (IDX4)
                                       TO  DCACCT-ZAITEN
           MOVE    HCM34S01-ZAIMINUS (IDX4)
                                       TO  DCACCT-MINUS
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               COMPUTE DCACCT-ZAIKAISU
                   =   DCACCT-ZAIKAISU
                   +   WRK-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    COMB-HKNNUM         TO  DCACCT-HKNNUM
           MOVE    COMB-KOHHKNNUM (1)  TO  DCACCT-KOHNUM
           MOVE    COMB-KOHID  (1)     TO  DCACCT-KOHID
           MOVE    PTRSI-HKNKBN        TO  DCACCT-RSI-HKNKBN
           MOVE    NACCT-NYUGAIKBN     TO  DCACCT-NYUGAIKBN
      *
           IF    ( HCM34S01-ZAISKBKBN (IDX4) =   "1" )
               MOVE    1               TO  DCACCT-STS-JITDAY
               MOVE    1               TO  DCACCT-STS-NYUKHN
           END-IF
      *
           IF    ( HCM34S01-ZAISKBKBN (IDX4) =   "2" )
               MOVE    1               TO  DCACCT-STS-GAIHAKU
           END-IF
      *
           IF    ( HCM34S01-ZAISKBKBN (IDX4)  =   "6" )
               IF    ( HCM34S01-ZAIMINUS (IDX4)    =   ZERO )
                   MOVE    1           TO  DCACCT-STS-NYUKSN
               ELSE
                   MOVE    1           TO  DCACCT-STS-NYUGEN
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( WRK-DAY (IDX-DAY)   >   ZERO )
                   COMPUTE DCACCT-ACCTRENNUM
                       =   DCACCT-ACCTRENNUM  +   1
                   MOVE    NACCT-SRYYM TO  DCACCT-SRYYMD
                   MOVE    IDX-DAY     TO  DCACCT-SRYYMD (7:2)
                   MOVE    WRK-DAY (IDX-DAY)
                                       TO  DCACCT-DAYCNT
                                           DCACCT-DAYCNT2 (1)
                   MOVE    IDX-DAY     TO  DCACCT-DAY
      *
                   WRITE   DCACCT-REC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-DCACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為情報追加処理
      *****************************************************************
       200111-DCSRY-INS-SEC            SECTION.
      *
           INITIALIZE                  DCSRY-REC
           MOVE    NACCT-PTID      TO  DCSRY-PTID
           MOVE    CNT-ZAINUM      TO  DCSRY-ZAINUM
           MOVE    "2"             TO  DCSRY-ACCTKBN
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   CONST-SRYCD-MAX )
                    OR   ( HCM34S01-SRYCD (IDX4 IDX8)
                                   =   SPACE )
      *
               MOVE    IDX8        TO  DCSRY-SRYRENNUM
               MOVE    HCM34S01-SRYCD (IDX4 IDX8)
                                   TO  DCSRY-SRYCD
               MOVE    HCM34S01-SURYO (IDX4 IDX8)
                                   TO  DCSRY-SURYO
      *
               MOVE    SMCNDATE-YMD    TO  DCSRY-CREYMD
                                           DCSRY-UPYMD
               MOVE    SMCNDATE-HMS    TO  DCSRY-UPHMS
      *
               WRITE   DCSRY-REC
      *
           END-PERFORM
      *
           .
       200111-DCSRY-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理（保険組合せ剤追加）
      *****************************************************************
       20012-DCACCT-INS-SEC            SECTION.
      *
           COMPUTE CNT-ZAINUM  =   CNT-ZAINUM  +   1
      *
           INITIALIZE                      DCSRY-REC
           MOVE    NACCT-PTID          TO  DCSRY-PTID
           MOVE    CNT-ZAINUM          TO  DCSRY-ZAINUM
           MOVE    "2"                 TO  DCSRY-ACCTKBN
           MOVE    1                   TO  DCSRY-SRYRENNUM
           MOVE    CONST-HKNCOMBICD    TO  DCSRY-SRYCD
      *
           MOVE    SMCNDATE-YMD        TO  DCSRY-CREYMD
                                           DCSRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  DCSRY-UPHMS
      *
           WRITE   DCSRY-REC
      *
           INITIALIZE                      DCACCT-REC
           MOVE    NACCT-PTID          TO  DCACCT-PTID
           MOVE    CNT-ZAINUM          TO  DCACCT-ZAINUM
           MOVE    "2"                 TO  DCACCT-ACCTKBN
           MOVE    NACCT-SRYKA         TO  DCACCT-SRYKA
           MOVE    NACCT-SRYKBN        TO  DCACCT-SRYKBN
           MOVE    SPACE               TO  DCACCT-JIHOKBN
           MOVE    WRK-HKNNUM          TO  DCACCT-HKNNUM
           MOVE    WRK-KOHNUM          TO  DCACCT-KOHNUM
           MOVE    WRK-RSI-HKNKBN      TO  DCACCT-RSI-HKNKBN
           MOVE    NACCT-HKNCOMBI      TO  DCACCT-HKNCOMBI
           MOVE    NACCT-ZAITEN        TO  DCACCT-ZAITEN
           MOVE    NACCT-ZAIKAISU      TO  DCACCT-ZAIKAISU
           MOVE    NACCT-NYUGAIKBN     TO  DCACCT-NYUGAIKBN
           MOVE    1                   TO  DCACCT-STS-NYUINDAY
           MOVE    FLG-NODATA          TO  DCACCT-STS-NODATA
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( NACCT-DAY (IDX-DAY)   >   ZERO )
                   COMPUTE DCACCT-ACCTRENNUM
                       =   DCACCT-ACCTRENNUM  +   1
                   MOVE    NACCT-SRYYM TO  DCACCT-SRYYMD
                   MOVE    IDX-DAY     TO  DCACCT-SRYYMD (7:2)
                   MOVE    1           TO  DCACCT-DAYCNT
                   MOVE    1           TO  DCACCT-DAYCNT2 (1)
                   MOVE    IDX-DAY     TO  DCACCT-DAY
                   WRITE   DCACCT-REC
               END-IF
      *
           END-PERFORM
      *
           .
       20012-DCACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DTCHKERR-FILE
           IF    ( STS-DTCHKERR        =   ZERO )
               CLOSE   DTCHKERR-FILE
           ELSE
               OPEN    OUTPUT              DTCHKERR-FILE
      *
               MOVE    WRK-DTCHKERR    TO  DTCHKERR-REC
               WRITE   DTCHKERR-REC
               CLOSE   DTCHKERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    ZERO            TO  JOB-UPDCNT
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *     
           DISPLAY "*** ORCDTCHK006 END ***"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理途中経過表示処理
      *****************************************************************
       800-ORCSJOB-STM-SEC             SECTION.
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-JOB-YOBI        TO  JOB-YOBI
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCDTCHK006"       TO  JOB-PGID
           MOVE    "入院会計情報抽出"  TO  JOB-SHELLMSG
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
           .
       800-ORCSJOB-STM-EXT.
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
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    TNYUINRRK-PTID      TO  PTNYUINRRK-PTID
           MOVE    ORCBSD01-SRYYM      TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD
           MOVE    ORCBSD01-SRYYM      TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY43)
      *****************************************************************
       900-PTNYUINRRK-KEY43-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-TNYUINRRK
           INITIALIZE                      TNYUINRRK-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  TNYUINRRK-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  TNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  TNYUINRRK-TENNYUYMD(7:2)
           MOVE    ORCBSD01-SRYYM      TO  TNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  TNYUINRRK-TENSTUYMD(7:2)
           MOVE    TNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-TNYUINRRK
               INITIALIZE                  TNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY43-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY43)
      *****************************************************************
       900-PTNYUINRRK-KEY43-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-TNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-TNYUINRRK
               INITIALIZE                  TNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY43-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY27)
      *****************************************************************
       900-NYUINACCT-KEY27-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY27)
      *****************************************************************
       900-NYUINACCT-KEY27-FET-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    TNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY19)
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    TNYUINRRK-PTID       TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    WRK-PARA-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    TNYUINRRK-PTID          TO  COMB-PTID
           MOVE    WRK-COMB-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（受診患者）読込処理
      *****************************************************************
       900-DTCHK002-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK002
      *
           READ    DTCHK002-FILE
           AT  END
               MOVE    1           TO  FLG-DTCHK002
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DTCHK002
           END-IF
      *
           .
       900-DTCHK002-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
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
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
