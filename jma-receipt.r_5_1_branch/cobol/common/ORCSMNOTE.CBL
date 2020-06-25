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
       PROGRAM-ID.             ORCSMNOTE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳ＣＳＶ管理
      *  コンポーネント名  : お薬手帳ＣＳＶ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/11/30    NACL−太田    新規作成
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
           03  FLG-END                 PIC 9(01).
           03  FLG-SAVE                PIC 9(01).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-SHORI-RENNUMX.
               05  WRK-SHORI-RENNUM    PIC 9(02).
      *
       01  SGYOUMU-CODE                PIC 9(03).

      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
           COPY    "CPORCSMNOTE.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   ORCSMNOTEAREA
                                   SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   WRK-AREA
      *
           IF    ( SMNOTE-TBL-KEY  =   SPACE )
               MOVE    "MNOTE"     TO  SMNOTE-TBL-KEY
           END-IF
           IF    ( SMNOTE-RENNUM   =   ZERO )
               MOVE    1           TO  SMNOTE-RENNUM
           END-IF
           MOVE    SPA-PRT-GROUP   TO  SMNOTE-TBL-GROUP
           IF    ( SPA-PRT-FLG     IS  NUMERIC )
               MOVE SPA-PRT-FLG    TO  WRK-SHORI-RENNUMX
           ELSE
               MOVE ZERO           TO  WRK-SHORI-RENNUM
           END-IF
           MOVE    WRK-SHORI-RENNUM TO SMNOTE-SHORI-RENNUM
           MOVE    "store_mnote.sh" TO  SMNOTE-SHELLID
           MOVE    SPA-SRYYMD      TO  SMNOTE-SRYYM
                                       SMNOTE-SRYYMD
           MOVE    SPA-PRT-UUID    TO  SMNOTE-TBL-UUID
      *
           IF    ( SPA-API-FLG     =   ZERO OR 1 )
               IF    ( SPA-API-FLG =   ZERO )
                   PERFORM 200-MAIN-SEC
               ELSE
                   PERFORM 201-MAIN-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    主処理（画面処理より）
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           CALL "ORCSGYOUMUGET"    USING
                                   SPA-AREA
                                   SGYOUMU-CODE
           MOVE    SGYOUMU-CODE    TO  SMNOTE-GYOUMU-CD
      *
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  SHELLTBL
      *
           MOVE    "store_mnote.sh"
                                   TO  SHELLTBL-NAME
      *
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
      *
           MOVE    SMNOTE-MODE TO  SHELLTBL-ARG3
           MOVE    SMNOTE-TBL-KEY
                                   TO  SHELLTBL-ARG4
           MOVE    SMNOTE-RENNUM
                                   TO  SHELLTBL-ARG5
           MOVE    SMNOTE-TBL-GROUP
                                   TO  SHELLTBL-ARG6
           MOVE    SMNOTE-SHORI-RENNUM
                                   TO  SHELLTBL-ARG7
           MOVE    SMNOTE-SHELLID
                                   TO  SHELLTBL-ARG8
           MOVE    SMNOTE-SRYYM
                                   TO  SHELLTBL-ARG9
           MOVE    SMNOTE-SRYYMD
                                   TO  SHELLTBL-ARG10
           MOVE    SMNOTE-PUTFLG
                                   TO  SHELLTBL-ARG11
           MOVE    SMNOTE-NYUGAIKBN
                                   TO  SHELLTBL-ARG12
           MOVE    SMNOTE-PTID
                                   TO  SHELLTBL-ARG13
           MOVE    SMNOTE-DRCD
                                   TO  SHELLTBL-ARG14
           MOVE    SMNOTE-TO-FOLDER
                                   TO  SHELLTBL-ARG15
           MOVE    SMNOTE-TO-DATA
                                   TO  SHELLTBL-ARG16
           MOVE    SMNOTE-CODE-TYPE
                                   TO  SHELLTBL-ARG17
           MOVE    SMNOTE-TITLE
                                   TO  SHELLTBL-ARG18
      *
           MOVE    SMNOTE-CSVFILE  TO  SHELLTBL-ARG19
      *
           MOVE    SMNOTE-TBL-UUID
                                   TO  SHELLTBL-ARG20
           MOVE    SMNOTE-GYOUMU-CD
                                   TO  SHELLTBL-ARG21
           MOVE    SMNOTE-LOCATION-CD
                                   TO  SHELLTBL-ARG22
           MOVE    SMNOTE-DATA-VERSION
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-OPID        TO  SHELLTBL-ARG24
      *
           MOVE    SHELLTBL        TO      MCPDATA-REC
           MOVE    "SHELL"         TO      MCP-FUNC
           MOVE    "shell"         TO      MCP-TABLE
           MOVE    "allways"       TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
          .
       200-MAIN-EXT.
          EXIT.
      *
      *****************************************************************
      *    主処理(HAORIより）
      *****************************************************************
       201-MAIN-SEC                SECTION.
      *
           MOVE    21              TO  SMNOTE-GYOUMU-CD
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    SMNOTE-CSVFILE  TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASS-SAVE
      *
           OPEN    INPUT           SAVE-FILE
      *
           PERFORM 900-SAVE-READ-SEC
      *
           PERFORM UNTIL ( FLG-SAVE    NOT =  ZERO )
      *
               MOVE    SAVE-REC    TO  SMNOTE-CSVDATA
      *
               CALL    "ORCSCTRLMNOTE" USING
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
       201-MAIN-EXT.
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
      *
           .
       900-SAVE-READ-EXT.
           EXIT.
