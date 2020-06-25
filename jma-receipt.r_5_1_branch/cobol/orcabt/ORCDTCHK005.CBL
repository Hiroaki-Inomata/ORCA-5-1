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
       PROGRAM-ID.                     ORCDTCHK005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 診療行為情報抽出
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
      *    データチェック中間ファイル（受診患者）
           SELECT  DTCHK002-FILE   ASSIGN  DTCHK002PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（診療行為:入院）
           SELECT  DTCHK006N-FILE
                                   ASSIGN  DTCHK006NPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK006N-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（診療行為:外来）
           SELECT  DTCHK006G-FILE
                                   ASSIGN  DTCHK006GPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK006G-KEY
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
      *    データチェック算定履歴
           SELECT  DCSANTEI-FILE
                                   ASSIGN  DTCHKSANTEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCSANTEI-KEY
                                   FILE    STATUS  IS  STS-COMMON.
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
      *    データチェック中間ファイル（診療行為:入院）
       FD  DTCHK006N-FILE.
           COPY    "CPDTCHK006.INC"
                                   REPLACING  //DTCHK006//
                                   BY         //DTCHK006N//.
      *
      *    データチェック中間ファイル（診療行為:外来）
       FD  DTCHK006G-FILE.
           COPY    "CPDTCHK006.INC"
                                   REPLACING  //DTCHK006//
                                   BY         //DTCHK006G//.
      *
      *    データチェック診療行為
       FD  DCSRY-FILE.
       01  DCSRY-REC.
           COPY    "CPDCSRY.INC".
      *
      *    データチェック算定履歴
       FD  DCSANTEI-FILE.
       01  DCSANTEI-REC.
           COPY    "CPSANTEI.INC"  REPLACING  //SANTEI-//
                                   BY         //DCSANTEI-//.
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
                                   BY         //DTCHK002PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK006NPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK006GPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSRYPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSANTEIPARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SANTEI                              PIC 9(01).
           03  FLG-DTCHK002                            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DCSRY                               PIC 9(08). 
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05). 
           03  IDX1                                    PIC 9(05). 
           03  IDX2                                    PIC 9(05). 
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                           PIC X(06).
               05  WRK-SRYDAY                          PIC 9(02).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-JOB-YOBI                            PIC X(500).
      *
           COPY    "CPDTCHK006.INC".
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-SRYCD-MAX                         PIC 9(05)
                                                       VALUE   5.
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
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
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
           DISPLAY "*** ORCDTCHK005 START ***"
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
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
               MOVE    "ORCDTCHK005"       TO  JOB-PGID
               MOVE    "診療行為情報抽出"
                                           TO  JOB-SHELLMSG
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
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK006NPARA-HOSPNUM
           MOVE    "DTCHK06N"          TO  DTCHK006NPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK006NPARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK006GPARA-HOSPNUM
           MOVE    "DTCHK06G"          TO  DTCHK006GPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK006GPARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSRYPARA-HOSPNUM
           MOVE    "DCSRY000"          TO  DTCHKSRYPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSRYPARA-TERMID
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSANTEIPARA-HOSPNUM
           MOVE    "DCSANTEI"          TO  DTCHKSANTEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSANTEIPARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHK002PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    DTCHK006NPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (2)
           MOVE    DTCHK006GPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (3)
           MOVE    DTCHKSRYPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (4)
           MOVE    DTCHKSANTEIPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (5)
      *
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK002PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  DTCHK006NPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  DTCHK006GPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  DTCHKSRYPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  DTCHKSANTEIPARA-FULLNAME
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
           MOVE    ORCBSD01-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
      *    診療行為編集処理
           IF    ( ORCBSD01-IKTKBT     =   "2" )
            OR   ( ORCBSD01-SRYYMDKBN  =   "1" )
               PERFORM 2002-SRYACT-EDIT-SEC
               PERFORM 2002-DCSANTEI-EDIT-SEC
           ELSE
               PERFORM 2001-SRYACT-EDIT-SEC
               PERFORM 2001-DCSANTEI-EDIT-SEC
           END-IF
      *
      *    データチェック診療行為追加処理
           PERFORM 2001-DCSRY-INS-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       2001-SRYACT-EDIT-SEC            SECTION.
      *
           OPEN    OUTPUT  DTCHK006N-FILE
           OPEN    OUTPUT  DTCHK006G-FILE
      *
           PERFORM 900-SRYACT-ALL2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM 20011-SRYACT-EDIT-SEC
      *
               PERFORM 900-SRYACT-ALL2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "all2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   DTCHK006N-FILE
           CLOSE   DTCHK006G-FILE
      *
           .
       2001-SRYACT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20011-SRYACT-EDIT-SEC           SECTION.
      *
            INITIALIZE                  DTCHK006-REC
            MOVE    SRY-PTID        TO  DTCHK006-PTID
            MOVE    SRY-ZAINUM      TO  DTCHK006-ZAINUM
            MOVE    SRY-SRYSYUKBN   TO  DTCHK006-SRYSYUKBN
      *
            PERFORM VARYING IDX0    FROM    1   BY  1
                    UNTIL ( IDX0    >   CONST-SRYCD-MAX )
                     OR   ( SRY-SRYCD (IDX0)    =   SPACE )
      *
                COMPUTE DTCHK006-SRYRENNUM
                    = ( SRY-RENNUM  -   1 ) *   CONST-SRYCD-MAX
                    +   IDX0
      *
                MOVE    SRY-SRYCD (IDX0)
                                    TO  DTCHK006-SRYCD
                MOVE    SRY-SRYSURYO (IDX0)
                                    TO  DTCHK006-SURYO
                MOVE    SRY-INPUTNUM (IDX0)
                                    TO  DTCHK006-INPUTNUM
                MOVE    SRY-MEISKYFLG(IDX0)
                                    TO  DTCHK006-MEISKYFLG
      *
                IF    ( SRY-NYUGAIKBN   =   "1" )
      *
                    MOVE    DTCHK006-REC    TO  DTCHK006N-REC
                    WRITE   DTCHK006N-REC
      *
                ELSE
      *
                    MOVE    DTCHK006-REC    TO  DTCHK006G-REC
                    WRITE   DTCHK006G-REC
      *
                END-IF
      *
            END-PERFORM
      *
        .
       20011-SRYACT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       2002-SRYACT-EDIT-SEC            SECTION.
      *
           OPEN    INPUT   DTCHK002-FILE
           OPEN    OUTPUT  DTCHK006N-FILE
           OPEN    OUTPUT  DTCHK006G-FILE
      *
           PERFORM 900-DTCHK002-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK002    NOT =   ZERO )
      *
               PERFORM 900-SRYACT-KEY15-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
                   PERFORM 20011-SRYACT-EDIT-SEC
      *
                   PERFORM 900-SRYACT-KEY15-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key15"         TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-DTCHK002-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DTCHK002-FILE
           CLOSE   DTCHK006N-FILE
           CLOSE   DTCHK006G-FILE
      *
           .
       2002-SRYACT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴編集処理
      *****************************************************************
       2001-DCSANTEI-EDIT-SEC          SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           OPEN    OUTPUT  DCSANTEI-FILE
      *
           PERFORM 900-SANTEI-ALL2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SANTEI  NOT =   ZERO )
      *
               MOVE    SANTEI-REC  TO  DCSANTEI-REC
               WRITE   DCSANTEI-REC
      *
               PERFORM 900-SANTEI-ALL2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "all2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   DCSANTEI-FILE
      *
           .
       2001-DCSANTEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴編集処理
      *****************************************************************
       2002-DCSANTEI-EDIT-SEC          SECTION.
      *
           OPEN    INPUT   DTCHK002-FILE
           OPEN    OUTPUT  DCSANTEI-FILE
      *
           PERFORM 900-DTCHK002-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK002    NOT =   ZERO )
      *
               PERFORM 900-SANTEI-KEY10-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SANTEI  NOT =   ZERO )
      *
                   MOVE    SANTEI-REC  TO  DCSANTEI-REC
                   WRITE   DCSANTEI-REC
      *
                   PERFORM 900-SANTEI-KEY10-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-DTCHK002-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DTCHK002-FILE
                   DCSANTEI-FILE
      *
           .
       2002-DCSANTEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為テーブル追加処理
      *****************************************************************
       2001-DCSRY-INS-SEC              SECTION.
      *
           OPEN    INPUT               DTCHK006N-FILE
           OPEN    INPUT               DTCHK006G-FILE
      *
           OPEN    OUTPUT              DCSRY-FILE
      *
           PERFORM 900-DTCHK006N-READ-SEC
      *
           PERFORM UNTIL ( DTCHK006N-KEY   =   HIGH-VALUE )
      *
               MOVE    DTCHK006N-REC   TO  DTCHK006-REC
               PERFORM 20011-DCSRY-INS-SEC
               PERFORM 900-DTCHK006N-READ-SEC
      *
           END-PERFORM
      *
           PERFORM 900-DTCHK006G-READ-SEC
      *
           PERFORM UNTIL ( DTCHK006G-KEY   =   HIGH-VALUE )
      *
               MOVE    DTCHK006G-REC   TO  DTCHK006-REC
               PERFORM 20011-DCSRY-INS-SEC
               PERFORM 900-DTCHK006G-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DTCHK006N-FILE
           CLOSE   DTCHK006G-FILE
      *
           CLOSE   DCSRY-FILE
      *
           .
       2001-DCSRY-INS-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為テーブル追加処理
      *****************************************************************
       20011-DCSRY-INS-SEC              SECTION.
      *
           INITIALIZE              DCSRY-REC
      *
           MOVE    DTCHK006-PTID   TO  DCSRY-PTID
           MOVE    DTCHK006-ZAINUM TO  DCSRY-ZAINUM
           MOVE    "1"             TO  DCSRY-ACCTKBN
           MOVE    DTCHK006-SRYRENNUM
                                       TO  DCSRY-SRYRENNUM
           MOVE    DTCHK006-SRYSYUKBN
                                       TO  DCSRY-SRYSYUKBN
           MOVE    DTCHK006-SRYCD  TO  DCSRY-SRYCD
           MOVE    DTCHK006-SURYO  TO  DCSRY-SURYO
           MOVE    DTCHK006-INPUTNUM
                                   TO  DCSRY-INPUTNUM
           MOVE    DTCHK006-MEISKYFLG
                                   TO  DCSRY-MEISKYFLG
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  DCSRY-CREYMD
                                       DCSRY-UPYMD
           MOVE    SMCNDATE-HMS    TO  DCSRY-UPHMS
      *
           WRITE   DCSRY-REC
      *
           .
       20011-DCSRY-INS-EXT.
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
           DISPLAY "*** ORCDTCHK005 END ***"
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
           MOVE    "ORCDTCHK004"       TO  JOB-PGID
           MOVE    "診療行為情報抽出"  TO  JOB-SHELLMSG
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
      *    データチェック中間ファイル（診療行為：入院）読込処理
      *****************************************************************
       900-DTCHK006N-READ-SEC          SECTION.
      *
           READ    DTCHK006N-FILE      NEXT
           AT  END
               MOVE    HIGH-VALUE  TO  DTCHK006N-KEY
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    HIGH-VALUE  TO  DTCHK006N-KEY
           END-IF
      *
           .
       900-DTCHK006N-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（診療行為：外来）読込処理
      *****************************************************************
       900-DTCHK006G-READ-SEC          SECTION.
      *
           READ    DTCHK006G-FILE      NEXT
           AT  END
               MOVE    HIGH-VALUE  TO  DTCHK006G-KEY
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    HIGH-VALUE  TO  DTCHK006G-KEY
           END-IF
      *
           .
       900-DTCHK006G-READ-EXT.
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
      *    診療行為検索処理(ALL2)
      *****************************************************************
       900-SRYACT-ALL2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                  SRYACT-REC
      *
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  SRY-SRYYM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "all2"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-ALL2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理(ALL2)
      *****************************************************************
       900-SRYACT-ALL2-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                  SRYACT-REC
      *
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "all2"                  TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-ALL2-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       900-SRYACT-KEY15-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                  SRYACT-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  SRY-SRYYM
           MOVE    DTCHK002-PTID       TO  SRY-PTID
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理
      *****************************************************************
       900-SRYACT-KEY15-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                  SRYACT-REC
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY15-FET-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       900-SANTEI-KEY10-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI
           INITIALIZE                  SANTEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SANTEI-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  SANTEI-SRYYM
           MOVE    DTCHK002-PTID       TO  SANTEI-PTID
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴FETCH処理
      *****************************************************************
       900-SANTEI-KEY10-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-KEY10-FET-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       900-SANTEI-ALL2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI
           INITIALIZE                  SANTEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SANTEI-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-ALL2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴FETCH処理
      *****************************************************************
       900-SANTEI-ALL2-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-ALL2-FET-EXT.
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
       910-DBCLOSECURSOR-SEC               SECTION.
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
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
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
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
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
