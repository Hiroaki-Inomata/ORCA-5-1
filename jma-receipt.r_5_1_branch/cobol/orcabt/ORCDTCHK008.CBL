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
       PROGRAM-ID.                     ORCDTCHK008.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェックリスト前処理
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
      *
      *    データチェック中間ファイル（チェック結果）
           SELECT  DTCHK004-FILE
                                   ASSIGN  DTCHK004PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK004-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（複数保険エラー患者）
           SELECT  DTCHK007-FILE
                                   ASSIGN  DTCHK007PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK007-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック中間ファイル（チェック結果）
       FD  DTCHK004-FILE.
           COPY    "CPDTCHK004.INC".
      *
      *    データチェック中間ファイル（複数保険エラー患者）
       FD  DTCHK007-FILE.
           COPY    "CPDTCHK007.INC".
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK004PARA//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK007PARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-DTCHK004                            PIC 9(01).
           03  FLG-DTCHK007                            PIC 9(01).
           03  FLG-LOOPEND                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DTCHK004                            PIC 9(06).
           03  CNT-DTCHK007                            PIC 9(06).
           03  CNT-ERRHKN                              PIC 9(06).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05). 
      *
       01  WRK-AREA.
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-HKNOTHERS                           PIC X(1).
           03  WRK-PTID                                PIC 9(10).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNKBN                        PIC X(1).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNKBN                        PIC X(1).
      *
      *    固定値領域
       01  CONST-AREA.
               05  CONST-HKNKBN-MAX            PIC 9(03)   VALUE    9.
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
                   07  CONST-HOUKATU           PIC X(01)   VALUE   "H".
               05  CONST-HKNKBN-R      REDEFINES   CONST-HKNKBN-VAL.
                   07  CONST-HKNKBN            PIC X(01)   OCCURS   9.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    データチェック機能情報２
           COPY    "CPSK1101.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL ( FLG-END =   1 )
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  DTCHK-RC
      *
           EXIT    PROGRAM
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
                                       KEY-AREA
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
               MOVE    "ORCDTCHK008"       TO  JOB-PGID
               MOVE    "リスト作成前処理"
                                           TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK004PARA-HOSPNUM
           MOVE    "DTCHK004"          TO  DTCHK004PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK004PARA-TERMID
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK007PARA-HOSPNUM
           MOVE    "DTCHK007"          TO  DTCHK007PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK007PARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHK004PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    DTCHK007PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (2)
      *
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK004PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  DTCHK007PARA-FULLNAME
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
           PERFORM 2001-DTCHK007-EDIT-SEC
      *
           PERFORM 2001-DTCHK004-EDIT-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    複数保険エラー患者ファイル編集処理
      *****************************************************************
       2001-DTCHK007-EDIT-SEC          SECTION.
      *
           OPEN    INPUT               DTCHK004-FILE
      *
           OPEN    OUTPUT              DTCHK007-FILE
           CLOSE                       DTCHK007-FILE
      *
           OPEN    I-O                 DTCHK007-FILE
      *
           MOVE    ZERO            TO  FLG-DTCHK004
      *
      *    チェック結果ファイル読込
           PERFORM 900-DTCHK004-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK004    =   1 )
      *
               MOVE    KEY-NEW     TO  KEY-OLD
      *
               PERFORM UNTIL ( FLG-DTCHK004    =   1 )
                        OR   ( KEY-N-PTID      NOT =   KEY-O-PTID )
                        OR   ( KEY-N-HKNKBN    NOT =   KEY-O-HKNKBN)
      *
      *            チェック結果ファイル読込
                   PERFORM 900-DTCHK004-READ-SEC
      *
               END-PERFORM
      *
               INITIALIZE              DTCHK007-REC
               MOVE    KEY-O-PTID  TO  WRK-PTID
               PERFORM 900-DTCHK007-READ-SEC
               IF    ( FLG-DTCHK007    =   ZERO )
                   PERFORM 20011-HKN-CHK-SEC
      *
                   REWRITE DTCHK007-REC
      *
               ELSE
                   INITIALIZE              DTCHK007-REC
                   MOVE    KEY-O-PTID  TO  DTCHK007-PTID
                   PERFORM 20011-HKN-CHK-SEC
      *
                   WRITE DTCHK007-REC
      *
               END-IF
      *
           END-PERFORM
      *
           CLOSE                       DTCHK004-FILE
                                       DTCHK007-FILE
      *
           .
       2001-DTCHK007-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       20011-HKN-CHK-SEC               SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-HKNKBN-MAX )
      *
               IF    ( CONST-HKNKBN (IDX0) =   KEY-O-HKNKBN )
                   MOVE   1            TO  DTCHK007-ERRHKN (IDX0)
                   MOVE   CONST-HKNKBN-MAX
                                       TO  IDX0
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  CNT-ERRHKN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-HKNKBN-MAX )
      *
               IF    ( DTCHK007-ERRHKN (IDX0)  =   1 )
                   COMPUTE CNT-ERRHKN
                       =   CNT-ERRHKN
                       +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-ERRHKN          >   1 )
               MOVE    "1"             TO  DTCHK007-HKNOTHERS
           END-IF
      *
           .
       20011-HKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ファイル複数保険エラー編集処理
      *****************************************************************
       2001-DTCHK004-EDIT-SEC          SECTION.
      *
           OPEN    I-O                 DTCHK004-FILE
           OPEN    INPUT               DTCHK007-FILE
      *
           MOVE    ZERO            TO  FLG-DTCHK004
      *
      *    チェック結果ファイル読込
           PERFORM 900-DTCHK004-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK004    =   1 )
      *
               MOVE    KEY-NEW     TO  KEY-OLD
      *
               MOVE    SPACE       TO  WRK-HKNOTHERS
               MOVE    DTCHK004-PTID   TO  WRK-PTID
               PERFORM 900-DTCHK007-READ-SEC
               IF    ( FLG-DTCHK007    =   ZERO )
                   MOVE    DTCHK007-HKNOTHERS
                                   TO  WRK-HKNOTHERS
               END-IF
      *
               PERFORM UNTIL ( FLG-DTCHK004    =   1 )
                        OR   ( KEY-N-PTID  NOT =   KEY-O-PTID )
      *
                   MOVE    WRK-HKNOTHERS   TO  DTCHK004-HKNOTHERS
      *
                   REWRITE DTCHK004-REC
      *
      *            チェック結果ファイル読込
                   PERFORM 900-DTCHK004-READ-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE                       DTCHK004-FILE
                                       DTCHK007-FILE
      *
           .
       2001-DTCHK004-EDIT-EXT.
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
               MOVE    ZERO            TO  JOB-UPDCNT
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *     
           DISPLAY "*** ORCDTCHK008 END ***"
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
      *    チェック結果ファイル読込処理
      *****************************************************************
       900-DTCHK004-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK004
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-DTCHK004    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               READ    DTCHK004-FILE       NEXT
               AT  END
      *
                   MOVE    1           TO  FLG-DTCHK004
      *
               NOT AT  END
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( DTCHK004-ERRSBT NOT =   "0" )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1           TO  FLG-DTCHK004
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-DTCHK004        =   ZERO )
               MOVE    DTCHK004-PTID   TO  KEY-N-PTID
               MOVE    DTCHK004-HKNKBN TO  KEY-N-HKNKBN
               COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
      *
           .
       900-DTCHK004-READ-EXT.
           EXIT.
      *****************************************************************
      *    複数保険エラー患者ファイル読み込み処理
      *****************************************************************
       900-DTCHK007-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK007
      *
           INITIALIZE                  DTCHK007-REC
           MOVE    WRK-PTID        TO  DTCHK007-PTID
           READ    DTCHK007-FILE
           INVALID KEY
               MOVE    1           TO  FLG-DTCHK007
           END-READ
      *
           .
       900-DTCHK007-READ-EXT.
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
      *    ＤＢ　オープン処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
