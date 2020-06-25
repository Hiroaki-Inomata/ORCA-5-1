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
       PROGRAM-ID.                 ORCSFILECHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : ファイル存在チェック
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/07    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 00.00.01     NACL-森脇    03/05/30  ファイルチェック修正
      * 04.08.00     NACL-多々納  14/07/30  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    入力：ファイル
           SELECT  IN-FILE     ASSIGN  ASS-IN
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-IN.
      *
       DATA                DIVISION.
       FILE                    SECTION.
      *
      *    入力ファイル
       FD  IN-FILE.
       01  IN-REC          PIC X(01).
      *
       WORKING-STORAGE         SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
      **** 03  ASS-IN      PIC X(256).
           03  ASS-IN      PIC X(1024).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN      PIC 9(01).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPORCSFILECHK.INC".
      *
      ******************************************************************
       PROCEDURE           DIVISION
                               USING
                               ORCSFILECHKAREA.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
      *     PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           MOVE    ZERO        TO  FLG-AREA
      *
      *    入力ファイル
           MOVE    FILECHK-NAME    TO  ASS-IN
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           IF          STS-IN      NOT     =   ZERO
               MOVE    2           TO  FILECHK-RC
           ELSE
      *        レコード管理ファイル読込み
               PERFORM 900-IN-READ-SEC
      *
               IF          FLG-IN      =   ZERO
                   MOVE    0           TO  FILECHK-RC
               ELSE
                   MOVE    1           TO  FILECHK-RC
               END-IF
      *        終了処理
               PERFORM 300-END-SEC
           END-IF
          .
      *
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC             SECTION.
      *
      *    入力ファイル　終了処理
           CLOSE               IN-FILE
           .
      *
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ファイル読込み処理
      *****************************************************************
       900-IN-READ-SEC         SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1           TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO        TO  FLG-IN
          END-READ
          .
      *
       900-IN-READ-EXT.
          EXIT.
      *
