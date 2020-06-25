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
       PROGRAM-ID.                     ORCDTCHK001.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 対象患者抽出
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/11/21    NACL−太田    新規作成
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
      *    データチェック診療会計
           SELECT  DCACCT-FILE     ASSIGN  DTCHKACCTPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCACCT-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（対象患者情報）
           SELECT  DTCHK003-FILE   ASSIGN  DTCHK003PARA
                                   FILE    STATUS  IS  STS-COMMON
                                   ORGANIZATION    IS  LINE SEQUENTIAL.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック診療会計
       FD  DCACCT-FILE.
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    データチェック中間ファイル（患者情報）
       FD  DTCHK003-FILE.
           COPY    "CPDTCHK001.INC"
                                   REPLACING  //DTCHK001//
                                   BY         //DTCHK003//.
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK003PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKACCTPARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-DCACCT                              PIC 9(01).
           03  FLG-LOOP-END                            PIC 9(01).
           03  FLG-TAISYO-HKN                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DCACCT                              PIC 9(06).
           03  CNT-DTCHK003                            PIC 9(06).
           03  CNT-HKNKBN-G.
               05  CNT-HKNKBN-VAL.
                   07  CNT-SYAHO                       PIC 9(03).
                   07  CNT-KOKUHO                      PIC 9(03).
                   07  CNT-ROSAI                       PIC 9(03).
                   07  CNT-JIBAI                       PIC 9(03).
                   07  CNT-JIHI                        PIC 9(03).
                   07  CNT-KOUGAI                      PIC 9(03).
                   07  CNT-KOUKI                       PIC 9(03).
                   07  CNT-CHIKEN                      PIC 9(03).
                   07  CNT-DAISAN                      PIC 9(03).
               05  CNT-HKNKBN-R    REDEFINES   CNT-HKNKBN-VAL.
                   07  CNT-HKNKBN                      PIC 9(03)
                                                       OCCURS   9.
           03  CNT-HKNKBN-TTL                          PIC 9(03).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05). 
      *
       01  WRK-AREA.
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-PTID                                PIC 9(10).
           03  WRK-ZAIHKNNUM                           PIC X(03).
           03  WRK-ZAIHKNCOMBI                         PIC 9(04).
           03  WRK-STATUS                              PIC 9(05).
           03  WRK-HKNKBN-CNT                          PIC 9(05).
           03  WRK-HKNKBN-G.
               05  WRK-HKNKBN-VAL.
                   07  WRK-SYAHOPROCKBN                PIC X(01).
                   07  WRK-KOKUHOPROCKBN               PIC X(01).
                   07  WRK-RSIPROCKBN                  PIC X(01).
                   07  WRK-JBIPROCKBN                  PIC X(01).
                   07  WRK-JIHIPROCKBN                 PIC X(01).
                   07  WRK-KGIPROCKBN                  PIC X(01).
                   07  WRK-KOUKIPROCKBN                PIC X(01).
                   07  WRK-CHIKENPROCKBN               PIC X(01).
                   07  WRK-DAISANPROCKBN               PIC X(01).
               05  WRK-HKNKBN-R    REDEFINES           WRK-HKNKBN-VAL.
                   07  WRK-HKNKBN                      PIC X(01)
                                                       OCCURS  9.
      *
       01  CONST-AREA.
           03  CONST-HKNKBN-MAX                PIC 9(03)   VALUE    9.
           03  CONST-HKNKBN-G.
               05  CONST-HKNKBN-VAL.
                   07  CONST-SYAHO             PIC X(01)   VALUE   "1".
                   07  CONST-KOKUHO            PIC X(01)   VALUE   "2".
                   07  CONST-ROSAI             PIC X(01)   VALUE   "3".
                   07  CONST-JIBAI             PIC X(01)   VALUE   "4".
                   07  CONST-JIHI              PIC X(01)   VALUE   "5".
                   07  CONST-KOUGAI            PIC X(01)   VALUE   "6".
                   07  CONST-KOUKI             PIC X(01)   VALUE   "7".
                   07  CONST-CHIKEN            PIC X(01)   VALUE   "8".
                   07  CONST-DAISAN            PIC X(01)   VALUE   "9".
               05  CONST-HKNKBN-R      REDEFINES   CONST-HKNKBN-VAL.
                   07  CONST-HKNKBN            PIC X(01)   OCCURS   9.
      *
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
      *    データチェック機能情報２
           COPY    "CPSK1101.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //LNKDTCHKERRPARA//.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
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
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR             TO  DTCHK-RC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
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
               MOVE    "ORCDTCHK001"       TO  JOB-PGID
               MOVE    "対象患者抽出"      TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKACCTPARA-HOSPNUM
           MOVE    "DCACCT00"          TO  DTCHKACCTPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKACCTPARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK003PARA-HOSPNUM
           MOVE    "DTCHK003"          TO  DTCHK003PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK003PARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHKACCTPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    DTCHK003PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHKACCTPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  DTCHK003PARA-FULLNAME
      *
      *    データチェック２機能情報
           PERFORM 1001-SYS1101-GET-SEC
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    データチェック２機能情報
      *****************************************************************
       1001-SYS1101-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1101-REC
           MOVE    "1101"              TO  SYS-1101-KANRICD
           MOVE    "*"                 TO  SYS-1101-KBNCD
           MOVE    ORCBSD01-SRYYM      TO  SYS-1101-STYUKYMD
           MOVE    "01"                TO  SYS-1101-STYUKYMD (7:2)
           MOVE    SYS-1101-STYUKYMD   TO  SYS-1101-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1101-HOSPNUM
           MOVE    SYS-1101-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1101-REC
           ELSE
               INITIALIZE                  SYS-1101-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SYS1101-GET-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    対象患者情報編集処理
           PERFORM 2001-DTCHK003-HEN-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    対象患者編集処理
      *****************************************************************
       2001-DTCHK003-HEN-SEC           SECTION.
      *
           OPEN    INPUT                   DCACCT-FILE
           OPEN    OUTPUT                  DTCHK003-FILE
      *
           MOVE    ZERO                TO  CNT-DTCHK003
           MOVE    ZERO                TO  FLG-DCACCT
      *
      *    データチェック診療会計読込処理
           PERFORM 900-DCACCT-READ-SEC
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
      *
               MOVE    DCACCT-PTID     TO  WRK-PTID
               INITIALIZE                  WRK-HKNKBN-G
                                           CNT-HKNKBN-G
      *
               PERFORM UNTIL ( WRK-PTID    NOT =   DCACCT-PTID )
                        OR   ( FLG-DCACCT  NOT =   ZERO )
      *
                   IF    ( DCACCT-STS-NYUINDAY =   ZERO )
                    OR   ( DCACCT-STS-NODATA   =   "1" )
                       MOVE    DCACCT-HKNNUM   TO  WRK-ZAIHKNNUM
                       MOVE    DCACCT-HKNCOMBI TO  WRK-ZAIHKNCOMBI
                       IF    ( DCACCT-KOHNUM   =   "970" )
                           MOVE    DCACCT-KOHNUM  
                                               TO  WRK-ZAIHKNNUM
                           MOVE    ZERO        TO  WRK-ZAIHKNCOMBI
                       END-IF
                       PERFORM 220-TAISYO-HKN-CHK-SEC
                   END-IF
      *
                   PERFORM 900-DCACCT-READ-SEC
      *
               END-PERFORM
      *
               MOVE    ZERO                TO  CNT-HKNKBN-TTL
                                               WRK-HKNKBN-CNT
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-HKNKBN-MAX )
      *
                   IF    ( CNT-HKNKBN (IDX0)   =   1 )
                       COMPUTE CNT-HKNKBN-TTL
                           =   CNT-HKNKBN-TTL
                           +   1
                   END-IF
      *
                   IF    ( WRK-HKNKBN (IDX0)   =   "1" )
                       COMPUTE WRK-HKNKBN-CNT
                           =   WRK-HKNKBN-CNT
                           +   1
                   END-IF
      *
               END-PERFORM
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-HKNKBN-MAX )
      *
                   IF    ( WRK-HKNKBN (IDX0)   =   "1" )
                       INITIALIZE          DTCHK003-REC
                       MOVE    CONST-HKNKBN (IDX0)
                                       TO  DTCHK003-HKNKBN
                       PERFORM 210-DTCHK003-WRITE-SEC
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE                           DCACCT-FILE
                                           DTCHK003-FILE
      *
           .
       2001-DTCHK003-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報ファイル出力処理
      *****************************************************************
       210-DTCHK003-WRITE-SEC        SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM TO  DTCHK003-HOSPNUM
           MOVE    WRK-PTID        TO  DTCHK003-PTID
      *
           IF    ( CNT-HKNKBN-TTL  >   1 )
               MOVE    "1"         TO  DTCHK003-MULHKNKBN
           END-IF
      *
           WRITE                       DTCHK003-REC
      *
           COMPUTE CNT-DTCHK003    =   CNT-DTCHK003    +   1
      *
           .
       210-DTCHK003-WRITE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象保険チェック処理
      *****************************************************************
       220-TAISYO-HKN-CHK-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN      ( WRK-ZAIHKNNUM   =   "060" )
           WHEN      ( WRK-ZAIHKNNUM   =   "067" )
           WHEN      ( WRK-ZAIHKNNUM   =   "068" )
           WHEN      ( WRK-ZAIHKNNUM   =   "069" )
      *
               MOVE    1               TO  CNT-KOKUHO
      *
               IF    ( ORCBSD01-KOKUHOPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-KOKUHOPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "971" )
      *
               MOVE    1               TO  CNT-ROSAI
      *
               IF    ( ORCBSD01-RSIPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-RSIPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "973" )
      *
               MOVE    1               TO  CNT-JIBAI
      *
               IF    ( ORCBSD01-JBIPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-JBIPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "970" )
      *
               MOVE    1               TO  CNT-DAISAN
      *
               IF    ( ORCBSD01-DAISANPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-DAISANPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "975" )
      *
               MOVE    1               TO  CNT-KOUGAI
      *
               IF    ( ORCBSD01-KGIPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-KGIPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "040" )
           WHEN      ( WRK-ZAIHKNNUM   =   "039" )
      *
               MOVE    1               TO  CNT-KOUKI
      *
               IF    ( ORCBSD01-KOUKIPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-KOUKIPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "98" )
      *
               MOVE    1               TO  CNT-JIHI
      *
               IF    ( ORCBSD01-JIHIPROCKBN    =   "1" )
                   MOVE    "1"         TO  WRK-JIHIPROCKBN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "90" OR "91" )
      *
               MOVE    1               TO  CNT-CHIKEN
      *
               IF    ( ORCBSD01-CHIKENPROCKBN  =   "1" )
                   MOVE    "1"         TO  WRK-CHIKENPROCKBN
               END-IF
           WHEN      OTHER
      *
               IF    ( WRK-ZAIHKNCOMBI NOT =   9999 )
                   MOVE    1           TO  CNT-SYAHO
      *
                   IF    ( ORCBSD01-SYAHOPROCKBN   =   "1" )
                       MOVE    "1"     TO  WRK-SYAHOPROCKBN
                   END-IF
               END-IF
           END-EVALUATE
      *
           .
       220-TAISYO-HKN-CHK-EXT.
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
             IF    LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
             END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           MOVE    1                   TO  FLG-END
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
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *     
           DISPLAY "*** ORCDTCHK001 END ***"
           DISPLAY "I DCACCT   CNT [" CNT-DCACCT "]"
           DISPLAY "O DTCHK003 CNT [" CNT-DTCHK003 "]"
           DISPLAY "***********************"
           .
       300-END-EXT.
           EXIT.
      *
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
      *    データチェック診療会計読込処理
      *****************************************************************
       900-DCACCT-READ-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCACCT-FILE         NEXT
               AT  END
                   MOVE    1       TO  FLG-DCACCT
               NOT AT  END
      *
                   COMPUTE CNT-DCACCT  =   CNT-DCACCT  +   1
      *
                   MOVE    1   TO  FLG-LOOP-END
      *
                   IF    ( ORCBSD01-NYUGAIKBN
                                       NOT =   DCACCT-NYUGAIKBN )
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DCACCT
               END-IF
      *
           END-PERFORM
      *
           .
       900-DCACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           MOVE    "DBSTART"            TO  MCP-FUNC
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
       900-DBCOMMIT-SEC                 SECTION.
      *
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           MOVE    "DBCOMMIT"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
