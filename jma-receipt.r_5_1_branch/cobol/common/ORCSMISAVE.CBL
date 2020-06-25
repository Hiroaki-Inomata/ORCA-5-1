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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSMISAVE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 薬剤イメージ登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/07/28    NACL−伊藤    新規作成
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
      *    イメージデータファイル
           SELECT  IMAGE-FILE      ASSIGN  IMAGE-FILENAME
                                   FILE    STATUS  IS  STS-IMAGE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    イメージデータファイル
       FD  IMAGE-FILE.
       01  IMAGE-REC.
           03  IMAGE-REC-1         PIC X(500000).
           03  IMAGE-REC-2         PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IMAGE           PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OPEN            PIC 9(01).
           03  FLG-MEDIMAGE        PIC 9(01).
      *
      *    イメージデータファイル名
       01  IMAGE-FILENAME          PIC X(256).
      *
      *    コマンド
       01  COMMAND                 PIC X(512).
      *
      *    返却値
       01  FILESIZE                PIC 9(09).
      *
      *    画像ファイルフォルダ名
       01  WRK-CONS-AREA.
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPMEDIMAGE.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
       01  MCPDATA.
           03  MCPREC              PIC X(510000).
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSMIAREA.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION
                                       USING
                                       ORCSMIAREA
                                       SPA-AREA
           .
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
      *
      *    イメージデータファイル名
           MOVE    SPACE           TO  IMAGE-FILENAME
           STRING  ORCSMI-TEMPDIR      DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   ORCSMI-SRYCD        DELIMITED BY SPACE
                   ".txt"              DELIMITED BY SIZE
                               INTO    IMAGE-FILENAME
           END-STRING
      *
      *    画像ファイルからイメージデータ作成コマンド
           MOVE    LOW-VALUE       TO  COMMAND
           STRING  "base64 "           DELIMITED BY SIZE
                   ORCSMI-TEMPDIR      DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                   "/"                 DELIMITED BY SIZE
                   ORCSMI-FILENAME     DELIMITED BY SPACE
                   " > "               DELIMITED BY SIZE
                   IMAGE-FILENAME      DELIMITED BY SPACE
                   " || rm -f  "       DELIMITED BY SIZE
                   IMAGE-FILENAME      DELIMITED BY SPACE
                               INTO    COMMAND
           END-STRING
      *    コマンド実行
           CALL    "system"            USING
                                       COMMAND
      *
      *    イメージデータファイル読み込み
           OPEN    INPUT               IMAGE-FILE
           IF      STS-IMAGE       NOT =   ZERO
               DISPLAY "ORCSMISAVE IMAGE FILE OPEN ERROR"
               MOVE    1                   TO  ORCSMI-RETCD
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    LOW-VALUE           TO  IMAGE-REC
               READ    IMAGE-FILE
                   AT  END
                       MOVE    1               TO  FLG-END
               END-READ
               IF      IMAGE-REC-2 (1:10)  NOT =   LOW-VALUE
                   DISPLAY "ORCSMISAVE IMAGE FILE SIZE OVER"
                   MOVE    2                   TO  ORCSMI-RETCD
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           MOVE    1               TO  FLG-OPEN
      *
      *    ファイルサイズチェックの判定
           IF      ORCSMI-SRYCD        =   "misizechk"
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    イメージデータ読み込み
           PERFORM 900-MEDIMAGE-READ-SEC
      *
           INITIALIZE                  MEDIMAGE-REC
           MOVE    SPA-HOSPNUM     TO  MEDIMG-HOSPNUM
           MOVE    ORCSMI-SRYCD    TO  MEDIMG-SRYCD
           MOVE    ORCSMI-IMAGEKBN TO  MEDIMG-IMAGEKBN
           MOVE    IMAGE-REC-1     TO  MEDIMG-IMAGEDATA
           MOVE    SPA-OPID        TO  MEDIMG-OPID
           MOVE    SPA-SYSYMD      TO  MEDIMG-UPYMD
           MOVE    MEDIMAGE-REC    TO  MCPREC
      *
           IF      FLG-MEDIMAGE        =   ZERO
      *        イメージデータ更新
               PERFORM 900-MEDIMAGE-UPD-SEC
           ELSE
      *        イメージデータ追加
               PERFORM 900-MEDIMAGE-INS-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      FLG-OPEN            =   1
               CLOSE   IMAGE-FILE
           END-IF
      *
      *    イメージデータファイル削除コマンド
           MOVE    LOW-VALUE       TO  COMMAND
           STRING  "rm -f "            DELIMITED BY SIZE
                   IMAGE-FILENAME      DELIMITED BY SPACE
                               INTO    COMMAND
           END-STRING
      *    コマンド実行
           CALL    "system"            USING
                                       COMMAND
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    イメージデータ読み込み  処理
      *****************************************************************
       900-MEDIMAGE-READ-SEC       SECTION.
      *
           INITIALIZE                  MEDIMAGE-REC
           MOVE    SPA-HOSPNUM     TO  MEDIMG-HOSPNUM
           MOVE    ORCSMI-SRYCD    TO  MEDIMG-SRYCD
           MOVE    MEDIMAGE-REC    TO  MCPREC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_med_image" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPREC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-MEDIMAGE
               ELSE
                   MOVE    1               TO  FLG-MEDIMAGE
               END-IF
           ELSE
               MOVE    1               TO  FLG-MEDIMAGE
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           .
       900-MEDIMAGE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    イメージデータ追加  処理
      *****************************************************************
       900-MEDIMAGE-INS-SEC        SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSMISAVE INSERT ERROR :"  MCP-RC "."
               MOVE    1                   TO  ORCSMI-RETCD
           END-IF
           .
       900-MEDIMAGE-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    イメージデータ更新  処理
      *****************************************************************
       900-MEDIMAGE-UPD-SEC        SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSMISAVE UPDATE ERROR :"  MCP-RC "."
               MOVE    1                   TO  ORCSMI-RETCD
           END-IF
           .
       900-MEDIMAGE-UPD-EXT.
           EXIT.
      *
