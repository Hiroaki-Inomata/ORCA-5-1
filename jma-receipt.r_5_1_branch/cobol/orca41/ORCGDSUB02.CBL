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
       PROGRAM-ID.                     ORCGDSUB02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェックサブ
      *  コンポーネント名  : データチェック実行
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/01/06    NACL−太田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  DTCHKPRM-FILE   ASSIGN          ASS-DTCHKPRM
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  DTCHKPRM-FILE.
           COPY    "CPORCBSD01PARA.INC"
                       REPLACING  //ORCBSD01PARA// BY //DTCHKPRM-REC//
                                  //ORCBSD01-//    BY //DTCHKPRM-//.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //ASS-DTCHKPRM//.
      *
      *    エラー用ファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //DTCHKERR//.
      *
       01  FLG-AREA.
           03  FLG-CHK004              PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-PRTKANRI-RENNUM     PIC 9(04).
           03  WRK-STPAGE              PIC 9(05).
           03  WRK-EDPAGE              PIC 9(05).
           03  WRK-KANRICD             PIC X(04).
           03  WRK-KBNCD               PIC X(08).
           03  WRK-TERMID              PIC X(64).
           03  WRK-JOBNAME             PIC X(16).
           03  WRK-JOB-PARAM.
               05  WRK-JOB-JOBNAME     PIC X(16).
      *
       01  CONST-AREA.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD        PIC X(08) VALUE "041".
      *    印刷管理ＤＢのキー値
           03  CONST-TBL-KEY           PIC X(08) VALUE "ORCBSD1".
           03  CONST-TBL-GROUP         PIC X(14) VALUE SPACE.
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID           PIC X(08) VALUE "ORCBSD1".
           03  CONST-JOBID             PIC 9(07) VALUE ZERO.
           03  CONST-PUSH-EVENT        PIC X(16) VALUE "data_check".
      *
       01  ORCSPRTLNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1031.INC".
           COPY    "CPSK3003.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
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
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCBSD01PARA.INC".
           COPY    "COMMON-SPA".
           COPY    "D01COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCBSD01PARA
           SPA-AREA
           SPA-D01KYOUTU.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 300-END-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
                                       ORCSPRTLNK-AREA
      *
           PERFORM 1001-SYS-3003-DEL-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *
      *        ジョブ管理削除処理
               MOVE    "ALL"       TO  SJOBKANRI-MODE
               INITIALIZE              JOBKANRI-REC
               MOVE    CONST-SHELLID
                                   TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4002"  TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理３００３削除処理（MCP-TENANTの取得も兼ねる）
      *****************************************************************
       1001-SYS-3003-DEL-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "3003"          TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD TO SYS-KBNCD
                                       SYSUP-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"      TO  SPA-ERRCD
           END-IF
      *
           .
       1001-SYS-3003-DEL-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      SPA-D01-TBL-GROUP
                                           SPA-D01-JOB-JOBID
      *
           MOVE    ALL "Z"             TO  WRK-TERMID
           PERFORM 800-ORCUIDSET-SEC
           STRING  C-UID                   DELIMITED BY SPACE
           INTO    WRK-TERMID
           END-STRING
      *
           MOVE    ZERO                TO  WRK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  CONST-JOBID
           MOVE    SPACE               TO  CONST-TBL-GROUP
      *
           MOVE    SPACE               TO  ASS-DTCHKPRM
           INITIALIZE                      ASS-DTCHKPRM
           MOVE    SPA-HOSPNUM         TO  ASS-DTCHKPRM-HOSPNUM
           MOVE    "DTCHKPRM"          TO  ASS-DTCHKPRM-FILE-ID
           MOVE    WRK-TERMID          TO  ASS-DTCHKPRM-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASS-DTCHKPRM-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-DTCHKPRM-FULLNAME
      *
           OPEN    OUTPUT                  DTCHKPRM-FILE
           MOVE    ORCBSD01PARA        TO  DTCHKPRM-REC
           WRITE   DTCHKPRM-REC
           CLOSE   DTCHKPRM-FILE
      *
           MOVE    SPA-HOSPNUM         TO  DTCHKERR-HOSPNUM
           MOVE    "OR044ERR"          TO  DTCHKERR-FILE-ID
           MOVE    WRK-TERMID          TO  DTCHKERR-TERMID
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"             USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           MOVE    SPA-D01-UPYMD        TO  CONST-TBL-GROUP (1:8)
           MOVE    SPA-D01-UPHMS        TO  CONST-TBL-GROUP (9:)
      *
           MOVE    "DATA_CHECK"         TO  WRK-JOBNAME
           PERFORM 2001-JOBKANRI-JBS-SEC
      *
      *    印刷用スクリプト処理
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
      *
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
      *
           MOVE    CONST-TBL-KEY       TO  LNK-PRTKANRI-TBL-KEY
           MOVE    CONST-TBL-GROUP     TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ORCBSD01-SRYYM      TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "orcbsd01.sh"       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (11) TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE               TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           MOVE    "orcbsd01.sh"       TO  SHELLTBL-NAME
      *
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    ORCBSD01-KARTE-UID  TO  SHELLTBL-COMMENT
           MOVE    CONST-JOBID         TO  SHELLTBL-JOBID
           MOVE    CONST-SHELLID       TO  SHELLTBL-SHELLID
      *
           MOVE    MCP-TENANT              TO  SHELLTBL-ARG1
      *
           MOVE    SPA-HOSPNUM             TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-RENNUM
                                           TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SRYYM
                                           TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-TERMID
                                           TO  SHELLTBL-ARG11
           MOVE    LNK-PRTKANRI-OPID
                                           TO  SHELLTBL-ARG12
           MOVE    LNK-PRTKANRI-PRTNM
                                           TO  SHELLTBL-ARG13
      *
           MOVE    CONST-JOBID
                                           TO  SHELLTBL-ARG14
           MOVE    CONST-SHELLID
                                           TO  SHELLTBL-ARG15
           MOVE    SPA-HOSPNUM             TO  SHELLTBL-ARG16
      *
           MOVE    DTCHKERR-BASENAME       TO  SHELLTBL-ARG17
      *
           INITIALIZE                          ORCA-BLOB
           MOVE    ASS-DTCHKPRM            TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT        TO  SHELLTBL-ARG18
      *
           MOVE    "DTCHKPRM"              TO  SHELLTBL-ARG19
      *
           IF    ( ORCBSD01-FROM-API       =   "1" )
               MOVE    CONST-PUSH-EVENT    TO  SHELLTBL-ARG20
           END-IF
      *
           MOVE    SHELLTBL                TO  MCPDATA-REC
           MOVE    "SHELL"                 TO  MCP-FUNC
           MOVE    "shell"                 TO  MCP-TABLE
           MOVE    "allways"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    印刷処理
           IF     ( SPA-DID1-FLG        =   "OK"  OR "PV")
            OR    ( ORCBSD01-FROM-API   =   "1" )
      *
               MOVE    "PRINT"             TO  WRK-JOBNAME
               PERFORM 2001-JOBKANRI-JBS-SEC
      *
               MOVE    CONST-TBL-GROUP     TO  SPA-D01-TBL-GROUP
               MOVE    CONST-JOBID         TO  SPA-D01-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    SPACE               TO  SHELLTBL
      *
      *        ファイルデイレクトリ位置指定
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
               MOVE    ORCBSD01-KARTE-UID  TO  SHELLTBL-COMMENT
               MOVE    CONST-JOBID         TO  SHELLTBL-JOBID
               MOVE    CONST-SHELLID       TO  SHELLTBL-SHELLID
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    CONST-TBL-KEY       TO  LNK-PRTKANRI-TBL-KEY
               MOVE    CONST-TBL-GROUP     TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    1                   TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    ORCBSD01-SRYYM      TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
               MOVE    WRK-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *
               MOVE    DTCHKERR-BASENAME   TO  SHELLTBL-ARG12
               MOVE    CONST-JOBID         TO  SHELLTBL-ARG13
               MOVE    CONST-SHELLID       TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               IF    ( ORCBSD01-FROM-API   =   "1" )
                       MOVE    "7"         TO  SHELLTBL-ARG17
               ELSE
                   EVALUATE    SPA-DID1-FLG
                   WHEN    "PV"
                       MOVE    "3"         TO  SHELLTBL-ARG17
                   WHEN    "OK"
                       IF      SPA-CLIENT-PRT-FLG  =  2
                           MOVE    "5"     TO  SHELLTBL-ARG17
                       ELSE
                           MOVE    "2"     TO  SHELLTBL-ARG17
                       END-IF
                   END-EVALUATE
               END-IF
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           IF    ( ORCBSD01-FROM-API       =    "1" )
               PERFORM 2001-MONBLOBAPI-SEC
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 2001-JOBID-UPD-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       2001-JOBKANRI-JBS-SEC           SECTION.
      *
           INITIALIZE                  WRK-JOB-PARAM
      *
           COMPUTE CONST-JOBID     =   CONST-JOBID +   1
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-JOBNAME     TO  WRK-JOB-JOBNAME
           MOVE    WRK-JOB-PARAM   TO  JOB-PARAM
           MOVE    ORCBSD01-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    ORCBSD01-ORCA-UID
                                   TO  JOB-ORCA-UID
           PERFORM 800-ORCSJOB-SEC
      *
           .
       2001-JOBKANRI-JBS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       2001-JOBID-UPD-SEC    SECTION.
      *
           PERFORM 1001-SYS-3003-DEL-SEC
      *
           IF    ( SPA-ERRCD               =   SPACE )
               INITIALIZE                      SYS-3003-REC
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
               MOVE    CONST-TBL-KEY       TO  SYS-3003-TBL-KEY
               MOVE    CONST-TBL-GROUP     TO  SYS-3003-TBL-GROUP
               MOVE    CONST-JOBID         TO  SYS-3003-JOBID
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-JOBID-UPD-EXT.
           EXIT.
      *****************************************************************
      *    MONBLOBAPI処理
      *****************************************************************
       2001-MONBLOBAPI-SEC        SECTION.
      *
           MOVE    "MONBLOBAPI"        TO  WRK-JOBNAME
           PERFORM 2001-JOBKANRI-JBS-SEC
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "haori_monblobapi.sh"
                                       TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    ORCBSD01-KARTE-UID  TO  SHELLTBL-COMMENT
           MOVE    CONST-JOBID         TO  SHELLTBL-JOBID
           MOVE    CONST-SHELLID       TO  SHELLTBL-SHELLID
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    CONST-TBL-KEY       TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    CONST-TBL-GROUP     TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    2                   TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    ORCBSD01-SRYYM      TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "haori_monblobapi.sh"
                                       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
           MOVE    CONST-JOBID         TO  SHELLTBL-ARG12
           MOVE    CONST-SHELLID       TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG14
           MOVE    DTCHKERR-BASENAME   TO  SHELLTBL-ARG15
      *
           MOVE    ORCBSD01-PRINT-ID   TO  SHELLTBL-ARG16
           MOVE    CONST-PUSH-EVENT    TO  SHELLTBL-ARG17
           MOVE    ORCBSD01-DATA-ID    TO  SHELLTBL-ARG18
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       2001-MONBLOBAPI-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
      *
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
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
           MOVE    "DBINSERT"      TO  MCP-FUNC
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
           MOVE    "DBUPDATE"      TO  MCP-FUNC
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
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    ORCA-BLOB       TO  MCPDATA-REC
      *
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    MCPDATA-REC     TO  ORCA-BLOB
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ORCA-BLOB-FILE
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
