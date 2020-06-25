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
       PROGRAM-ID.                     ORCDTCHK009.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 受診履歴情報抽出
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
                                   FILE    STATUS  IS  STS-DTCHK002.
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
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DTCHK002                            PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-JYURRK                              PIC 9(01).
           03  FLG-CHK004                              PIC 9(01).
           03  FLG-TARGET                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DTCHK002                            PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                           PIC X(06).
               05  WRK-SRYDAY                          PIC 9(02).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-JOB-YOBI                            PIC X(500).
      *
       01  KEY-AREA.
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
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
           DISPLAY "*** ORCDTCHK009 START ***"
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
                                       KEY-AREA
                                       SPA-AREA
      *
           MOVE    LNKDTCHKERRPARA     TO  DTCHKERRPARA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 900-DBSTART-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCDTCHK009"       TO  JOB-PGID
               MOVE    "受診患者抽出"      TO  JOB-SHELLMSG
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
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHK002PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
      *
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK002PARA-FULLNAME
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 900-DBCOMMIT-SEC
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
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-JYURRK
      *
      *    受診患者編集処理
           IF    ( ORCBSD01-IKTKBT =   "2" )
               PERFORM 2002-PTID-EDIT-SEC
           ELSE
               PERFORM 2001-PTID-EDIT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    受診患者編集処理
      *****************************************************************
       2001-PTID-EDIT-SEC              SECTION.
      *
           OPEN    OUTPUT  DTCHK002-FILE
      *
           PERFORM 900-JYURRK-KEY36-SEL-SEC
      *
           PERFORM UNTIL ( FLG-JYURRK  NOT =   ZERO )
      *
               MOVE    JYURRK-PTID     TO  KEY-PTID
      *
               PERFORM UNTIL ( FLG-JYURRK  NOT =   ZERO )
                        OR   ( KEY-PTID    NOT =   JYURRK-PTID )
      *
                   PERFORM 900-JYURRK-KEY36-FET-SEC
      *
               END-PERFORM
      *
               INITIALIZE                  DTCHK002-REC
               MOVE    KEY-PTID        TO  DTCHK002-PTID
               WRITE   DTCHK002-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   DTCHK002-FILE
      *
           .
       2001-PTID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    受診患者編集処理
      *****************************************************************
       2002-PTID-EDIT-SEC              SECTION.
      *
           OPEN    OUTPUT  DTCHK002-FILE
      *
           INITIALIZE                  CHK004-REC
           MOVE    "2001"          TO  CHK004-KANRICD
           IF    ( ORCBSD01-FROM-API   =   "1" )
               MOVE    "A%"        TO  CHK004-KBNCD
           ELSE
               MOVE    "1%"        TO  CHK004-KBNCD
           END-IF
           MOVE    WRK-SYMD        TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           DISPLAY "009 mcp rc = " MCP-RC " " WRK-SYMD
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( ORCBSD01-SRYYMDKBN  =   "1" )
                   PERFORM 900-JYURRK-KEY13-SEL-SEC
                   IF    ( FLG-JYURRK      =   ZERO )
                       MOVE    1       TO  FLG-TARGET
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   INITIALIZE              DTCHK002-REC
                   MOVE    CHK004-PTID TO  DTCHK002-PTID
                   WRITE   DTCHK002-REC
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
           DISPLAY "009 chk004 loop  " MCP-RC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   DTCHK002-FILE
      *
           .
       2002-PTID-EDIT-EXT.
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
             IF    WRK-PARA-SHELLID    NOT =   "RECEPTX"
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
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    ZERO            TO  JOB-UPDCNT
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 800-ORCSJOB-SEC
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *     
           DISPLAY "*** ORCDTCHK009 END ***"
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
      *    受診履歴検索処理
      *****************************************************************
       900-JYURRK-KEY13-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
           INITIALIZE                      JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    ORCBSD01-NYUGAIKBN  TO  JYURRK-NYUGAIKBN
           MOVE    CHK004-PTID         TO  JYURRK-PTID
           MOVE    ORCBSD01-SRYYM      TO  JYURRK-SRYYMD
                                           JYURRK-UPSRYYMD
           MOVE    ORCBSD01-STSRYYMD   TO  JYURRK-SRYYMD   (7:2)
           MOVE    ORCBSD01-EDSRYYMD   TO  JYURRK-UPSRYYMD (7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-JYURRK-KEY13-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴検索処理
      *****************************************************************
       900-JYURRK-KEY36-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
           INITIALIZE                      JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    ORCBSD01-NYUGAIKBN  TO  JYURRK-NYUGAIKBN
           MOVE    ORCBSD01-SRYYM      TO  JYURRK-SRYYMD
                                           JYURRK-UPSRYYMD
           MOVE    ORCBSD01-STSRYYMD   TO  JYURRK-SRYYMD   (7:2)
           MOVE    ORCBSD01-EDSRYYMD   TO  JYURRK-UPSRYYMD (7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       900-JYURRK-KEY36-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴FETCH処理
      *****************************************************************
       900-JYURRK-KEY36-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
           INITIALIZE                      JYURRK-REC
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       900-JYURRK-KEY36-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-FET-EXT.
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
