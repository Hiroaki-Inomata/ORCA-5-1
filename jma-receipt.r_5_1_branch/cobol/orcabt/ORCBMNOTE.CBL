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
       PROGRAM-ID.             ORCBMNOTE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳データ管理
      *  コンポーネント名  : お薬手帳データ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/07/12    NACL−太田    新規作成
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
      *    保存用データ
           SELECT  SAVE-FILE       ASSIGN  ASS-SAVE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SAVE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    保存用データ
       FD  SAVE-FILE.
       01  SAVE-REC                PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-SAVE//.
      *
       01  STS-AREA.
           03  STS-SAVE            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SAVE            PIC 9(01).
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    お薬手帳制御サブ
           COPY    "CPORCSMNOTE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
           COPY    "CPCOMMAND-PARAM.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM
           .
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
           .
      *
       000-PROC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              STS-AREA
                                   FLG-AREA
                                   SPA-AREA
                                   ORCSMNOTEAREA
                                   ASS-SAVE
      *
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO                        SPA-TERMID
                                       SPA-HOSPNUM
                                       SMNOTE-MODE
                                       SMNOTE-TBL-KEY
                                       SMNOTE-RENNUM
                                       SMNOTE-TBL-GROUP
                                       SMNOTE-SHORI-RENNUM
                                       SMNOTE-SHELLID
                                       SMNOTE-SRYYM
                                       SMNOTE-SRYYMD
                                       SMNOTE-PUTFLG
                                       SMNOTE-NYUGAIKBN
                                       SMNOTE-PTID
                                       SMNOTE-DRCD
                                       SMNOTE-TO-FOLDER
                                       SMNOTE-TO-DATA
                                       SMNOTE-CODE-TYPE
                                       SMNOTE-TITLE
                                       ASS-SAVE
                                       SMNOTE-TBL-UUID
                                       SMNOTE-GYOUMU-CD
                                       SMNOTE-LOCATION-CD
                                       SMNOTE-DATA-VERSION
                                       SPA-OPID
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASS-SAVE            TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-SAVE
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
           OPEN    INPUT           SAVE-FILE
      *
           PERFORM 900-SAVE-READ-SEC
      *
           PERFORM UNTIL ( FLG-SAVE    NOT =  ZERO )
      *
               MOVE    SAVE-REC        TO  SMNOTE-CSVDATA
      *
               CALL    "ORCSCTRLMNOTE"     USING
                                           ORCSMNOTEAREA
                                           SPA-AREA
      *
               PERFORM 900-SAVE-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   SAVE-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保存用データ読込処理
      *****************************************************************
       900-SAVE-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SAVE
      *
           READ    SAVE-FILE
           AT  END
               MOVE    1           TO  FLG-SAVE
           END-READ
           IF    ( STS-SAVE    =   "00" OR "10" )
               CONTINUE
           ELSE
               DISPLAY "READ ERROR[" STS-SAVE "]" ASS-SAVE
               MOVE    1           TO  FLG-SAVE
           END-IF
      *
           .
       900-SAVE-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
