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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSSRYSYU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 診療種別名テーブル編集処理
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/00/00    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    NACL-多々納  02/09/12  ディレクトリをサブプロへ
      *  01.00.01    NACL-多々納  02/10/09  最大数を１００へ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為テーブル
           SELECT  SRYKBN-FILE     ASSIGN  FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SRYKBN.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為テーブル
       FD  SRYKBN-FILE.
       01  SRYKBN-REC.
           03  SYRKBN-DATA.
               05  SYRKBN-SRYKBN         PIC X(02).
               05  FILLER                PIC X(01).
               05  SYRKBN-SRYSYUKBN      PIC X(03).
               05  SYRKBN-SRYMEI         PIC X(40).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME.
      *    03  FILLER          PIC X(16)   VALUE   "/usr/local/orca/".
      *    03  FILLER          PIC X(05)   VALUE   "data/".
      *    03  FILLER           PIC X(15)   VALUE   "SRYKBNNAME.TXT".
      *
       01  FILE-NAME               PIC X(512).
      *
      *    ファイル名
       01  DATA-FILE1.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(15)   VALUE   "SRYKBNNAME.TXT".
       01  DATA-FILE2.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(15)   VALUE   "SRYKBNNAME.TXT".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-SRYKBN          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSRYSYU.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
           MSYU-DATA-REC.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE1          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
      *    診療種別テーブルセット
           OPEN    INPUT               SRYKBN-FILE
           IF      STS-SRYKBN      NOT =   ZERO
      *        DISPLAY "PATCH-LIB SRYKBN-FILE OPEN ERR :" STS-SRYKBN "."
      *
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE2          TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               SRYKBN-FILE
               IF      STS-SRYKBN      NOT =   ZERO
                   DISPLAY "SRYKBN-FILE OPEN ERR :" STS-SRYKBN "."
               END-IF
      *
           END-IF
      *
           PERFORM 1001-SRYKBN-SET-SEC
      *
           CLOSE                   SRYKBN-FILE
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  MSYU-DATA-REC
           INITIALIZE                  MSYU-DATA-REC
           MOVE    ZERO                TO  FLG-SRYKBN
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    FLG-SRYKBN     =   1
               READ                    SRYKBN-FILE
                   AT  END
                       MOVE    1           TO  FLG-SRYKBN
                   NOT AT  END
                      ADD     1           TO  IDX
                      IF      IDX         >   200
                           DISPLAY "SRYSYU 200 OVER !!! TBL ERR"
                           MOVE    1           TO  FLG-SRYKBN
                       ELSE
                           MOVE    SRYKBN-REC      TO  MSYU-TBL-OCC
                                                       (IDX)
                       END-IF
               END-READ
           END-PERFORM
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
