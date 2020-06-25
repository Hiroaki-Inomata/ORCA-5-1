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
       PROGRAM-ID.                     ORCDTCHK010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : エラー患者情報登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  07/02/    NACL−太田    新規作成
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
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-DTCHK004                            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-CHK004                              PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-JOB-YOBI                            PIC X(500).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-KBN                       PIC X(01).
               05  WRK-KBNCD-SEQ                       PIC 9(07).
           03  WRK-KBNCD1200-G.
               05  WRK-KBNCD1200-SEQ                   PIC 9(06).
               05  WRK-KBNCD1200-CD                    PIC X(02).
      *
       01  KEY-AREA.
           03  KEY-ERRSBT                              PIC X(01).
           03  KEY-PTID                                PIC 9(10).
      *
      *    固定値領域
      *01  CONST-AREA.
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
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
      *
      *    個別処理基本情報
           COPY    "CPCK2000.INC".
      *
      *    個別処理患者情報
           COPY    "CPCK2001.INC".
      *
      *    前回エラー情報 
           COPY    "CPCK1200.INC".
      *
      *    エラー情報詳細
           COPY    "CPCK1201.INC".
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
           DISPLAY "*** ORCDTCHK010 START ***"
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
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
               MOVE    "ORCDTCHK010"       TO  JOB-PGID
               MOVE    "エラー患者情報登録"
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
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHK004PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
      *
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK004PARA-FULLNAME
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               PERFORM 900-DBCOMMIT-SEC
      *
               PERFORM 900-DBSTART-SEC
           END-IF
      *
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           INITIALIZE                  CK4-2000-REC
           MOVE    "2000"              TO  CK4-2000-KANRICD
           MOVE    "4%"                TO  CK4-2000-KBNCD
           MOVE    WRK-SYMD            TO  CK4-2000-STYUKYMD
                                           CK4-2000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  CK4-2000-HOSPNUM
           MOVE    CK4-2000-REC        TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "前回処理情報の削除に失敗しました"
                                       TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               INITIALIZE                  CK4-2001-REC
               MOVE    "2001"              TO  CK4-2001-KANRICD
               MOVE    "4%"                TO  CK4-2001-KBNCD
               MOVE    WRK-SYMD            TO  CK4-2001-STYUKYMD
                                               CK4-2001-EDYUKYMD
               MOVE    WRK-PARA-HOSPNUM    TO  CK4-2001-HOSPNUM
               MOVE    CK4-2001-REC        TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "エラー患者情報の削除に失敗しました"
                                       TO  WRK-DTCHKERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               INITIALIZE                  CHK004-REC
               MOVE    "1200"              TO  CHK004-KANRICD
               MOVE    "1201"              TO  CHK004-UPKANRICD
               MOVE    "%"                 TO  CHK004-KBNCD
               MOVE    WRK-SYMD            TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
               MOVE    WRK-PARA-HOSPNUM    TO  CHK004-HOSPNUM
               MOVE    CHK004-REC          TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "前回エラー情報の削除に失敗しました"
                                       TO  WRK-DTCHKERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *      
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               PERFORM 900-DBCOMMIT-SEC
      *
               PERFORM 900-DBSTART-SEC
           END-IF
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
           INITIALIZE                  CK4-2000-REC
           MOVE    "2000"          TO  CK4-2000-KANRICD
           MOVE    "4"             TO  CK4-2000-KBNCD
           MOVE    "00000000"      TO  CK4-2000-STYUKYMD
           MOVE    "99999999"      TO  CK4-2000-EDYUKYMD
           MOVE    ORCBSD01PARA    TO  CK4-2000-TBL
           MOVE    SMCNDATE-YMD    TO  CK4-2000-CREYMD
                                       CK4-2000-UPYMD
           MOVE    SMCNDATE-YMD    TO  CK4-2000-UPHMS
           MOVE    SPA-HOSPNUM     TO  CK4-2000-HOSPNUM
           MOVE    CK4-2000-REC    TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "前回処理情報の登録に失敗しました"
                                   TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 2001-PTID-EDIT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    エラー患者編集処理
      *****************************************************************
       2001-PTID-EDIT-SEC              SECTION.
      *
           INITIALIZE                  WRK-KBNCD-G
           MOVE    "4"             TO  WRK-KBNCD-KBN
      *
           OPEN    INPUT   DTCHK004-FILE
      *
           PERFORM 900-DTCHK004-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK004    NOT =   ZERO )
                    OR   ( FLG-END         NOT =   ZERO )
      *
               MOVE    DTCHK004-ERRSBT     TO  KEY-ERRSBT
               MOVE    DTCHK004-PTID       TO  KEY-PTID
      *
               PERFORM UNTIL ( FLG-DTCHK004    NOT =   ZERO )
                        OR   ( FLG-END         NOT =   ZERO )
                        OR   ( KEY-ERRSBT      NOT =   DTCHK004-ERRSBT )
                        OR   ( KEY-PTID        NOT =   DTCHK004-PTID )
      *
                   PERFORM 20011-ERRINF-EDIT-SEC
      *
                   PERFORM 900-DTCHK004-READ-SEC
      *
               END-PERFORM
      *
               IF    ( FLG-END             =   ZERO )
                AND  ( KEY-ERRSBT          =   "0" )
      *
                   MOVE    SPACE               TO  CHK004-REC
                   INITIALIZE                      CHK004-REC
                   MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
                   MOVE    "2001"              TO  CHK004-KANRICD
                   MOVE    "00000000"          TO  CHK004-STYUKYMD
                   MOVE    "99999999"          TO  CHK004-EDYUKYMD
                   MOVE    KEY-PTID            TO  CHK004-PTID
                   MOVE    "4%"                TO  CHK004-KBNCD
                   MOVE    CHK004-REC          TO  MCPDATA-REC
                   MOVE    "tbl_chk004"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
      *
                       MOVE    CHK004-REC      TO  CK4-2001-REC
      *
                       COMPUTE WRK-KBNCD-SEQ   =   WRK-KBNCD-SEQ   +   1
                       MOVE    WRK-KBNCD-G     TO  CK4-2001-KBNCD
      *
                       MOVE    SMCNDATE-YMD    TO  CK4-2001-CREYMD
                                                   CK4-2001-UPYMD
                       MOVE    SMCNDATE-YMD    TO  CK4-2001-UPHMS
                       MOVE    CK4-2001-REC    TO  MCPDATA-REC
                       MOVE    "tbl_chk004"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                           MOVE    "エラー患者情報の登録に失敗しました"
                                               TO  WRK-DTCHKERR
                           PERFORM 500-ERR-HENSYU-SEC
                           MOVE    1           TO  FLG-DTCHK004
                       END-IF
                   END-IF
                   MOVE    "tbl_chk004"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
      *
           END-PERFORM
      *
           CLOSE   DTCHK004-FILE
      *
           .
       2001-PTID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    エラー情報編集処理
      *****************************************************************
       20011-ERRINF-EDIT-SEC           SECTION.
      *
           COMPUTE WRK-KBNCD1200-SEQ
               =   WRK-KBNCD1200-SEQ   +   1
           MOVE    DTCHK004-ERRCD (1:2)
                                   TO  WRK-KBNCD1200-CD
      *
           INITIALIZE                  CK4-1200-REC
           MOVE    SPA-HOSPNUM     TO  CK4-1200-HOSPNUM
           MOVE    "1200"          TO  CK4-1200-KANRICD
           MOVE    WRK-KBNCD1200-G TO  CK4-1200-KBNCD
           MOVE    "00000000"      TO  CK4-1200-STYUKYMD
           MOVE    "99999999"      TO  CK4-1200-EDYUKYMD
           MOVE    DTCHK004-PTID   TO  CK4-1200-PTID
           MOVE    DTCHK004-HKNKBN TO  CK4-1200-HKNKBN
           MOVE    DTCHK004-ERRCD  TO  CK4-1200-ERRCD
           MOVE    DTCHK004-ERRMSG TO  CK4-1200-ERRMSG
           MOVE    SMCNDATE-YMD    TO  CK4-1200-CREYMD
                                       CK4-1200-UPYMD
           MOVE    SMCNDATE-YMD    TO  CK4-1200-UPHMS
           MOVE    CK4-1200-REC    TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "エラー情報の登録に失敗しました"
                                   TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  CK4-1201-REC
           MOVE    SPA-HOSPNUM     TO  CK4-1201-HOSPNUM
           MOVE    "1201"          TO  CK4-1201-KANRICD
           MOVE    WRK-KBNCD1200-G TO  CK4-1201-KBNCD
           MOVE    "00000000"      TO  CK4-1201-STYUKYMD
           MOVE    "99999999"      TO  CK4-1201-EDYUKYMD
           MOVE    DTCHK004-PTID   TO  CK4-1201-PTID
      *
           MOVE    DTCHK004-DAY-G  TO  CK4-1201-DAY-G
           MOVE    DTCHK004-LASTYMD TO CK4-1201-LASTYMD
           MOVE    DTCHK004-SRYCD  TO  CK4-1201-SRYCD
           MOVE    DTCHK004-SRYKA  TO  CK4-1201-SRYKA
           MOVE    DTCHK004-BYOMEI TO  CK4-1201-BYOMEI
           MOVE    DTCHK004-CHKKBN TO  CK4-1201-CHKKBN
           MOVE    DTCHK004-TEKCD  TO  CK4-1201-TEKCD
           MOVE    DTCHK004-TEKBYOMEI
                                   TO  CK4-1201-TEKBYOMEI
           MOVE    DTCHK004-TEKCOMPKBN
                                   TO  CK4-1201-TEKCOMPKBN
           MOVE    DTCHK004-TANICD
                                   TO  CK4-1201-TANICD
           MOVE    DTCHK004-TANINAME
                                   TO  CK4-1201-TANINAME
           MOVE    DTCHK004-SHONUM
                                   TO  CK4-1201-SHONUM
           MOVE    DTCHK004-SRYSTYMD
                                   TO  CK4-1201-SRYSTYMD
           MOVE    DTCHK004-SRYEDYMD
                                   TO  CK4-1201-SRYEDYMD
           MOVE    DTCHK004-LIMIT
                                   TO  CK4-1201-LIMIT
           MOVE    DTCHK004-TSUKISU
                                   TO  CK4-1201-TSUKISU
      *
           MOVE    SMCNDATE-YMD    TO  CK4-1201-CREYMD
                                       CK4-1201-UPYMD
           MOVE    SMCNDATE-YMD    TO  CK4-1201-UPHMS
           MOVE    CK4-1201-REC    TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "エラー情報詳細登録に失敗しました"
                                   TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-ERRINF-EDIT-EXT.
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
           DISPLAY "*** ORCDTCHK010 END ***"
           .
       300-END-EXT.
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
      *    チェック結果ファイル読込処理
      *****************************************************************
       900-DTCHK004-READ-SEC           SECTION.
      *
           READ    DTCHK004-FILE       NEXT
           AT  END
               MOVE    1           TO  FLG-DTCHK004
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DTCHK004
           END-IF
      *
           .
       900-DTCHK004-READ-EXT.
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
