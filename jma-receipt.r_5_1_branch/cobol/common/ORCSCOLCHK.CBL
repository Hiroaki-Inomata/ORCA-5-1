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
       PROGRAM-ID.             ORCSCOLCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 定点調査研究事業処理チェック処理
      *  返却エラーコード  : 00:正常  10:データ作成なし
      *                      20:データ作成処理中  30:ファイル存在
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.07.31    伊藤          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    定点調査収集データ
           SELECT  COLLECT-FILE    ASSIGN  COLLECT
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COLLECT.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    定点調査収集データ
       FD  COLLECT-FILE.
       01  COLLECT-REC             PIC X(3000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    定点調査収集データ
       01  COLLECT.
           03  FILLER              PIC X(08)  VALUE "/var/lib".
           03  FILLER              PIC X(12)  VALUE "/jma-receipt".
           03  FILLER              PIC X(05)  VALUE "/das/".
           03  COLLECT-HOSPNUM     PIC 9(02).
           03  FILLER              PIC X(05)  VALUE "/tmp/".
           03  FILLER              PIC X(07)  VALUE "collect".
           03  COLLECT-GN          PIC X(01).
           03  FILLER              PIC X(04)  VALUE ".csv".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COLLECT         PIC X(02).
      *
      *    ワーク領域
       01  WRK-AREA.
      *    ジョブ管理ＤＢのキー値（データ収集）
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                   PIC X(08)   VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSCOLCHK.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSCOLCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  COLCHK-RC
      *
      *    ジョブ管理チェック（データ収集）
           IF      SPA-DATACREATEFLG   NOT =   1
               MOVE    10                  TO  COLCHK-RC
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               PERFORM 100-JOBEXEC-CHK-SEC
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               PERFORM 200-FILEEXIST-CHK-SEC
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    収集データ作成処理チェック処理
      *****************************************************************
       100-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                       TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    20          TO  COLCHK-RC
               END-IF
           END-IF
           .
       100-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データファイル有無チェック処理
      *****************************************************************
       200-FILEEXIST-CHK-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  COLLECT-HOSPNUM
           MOVE    "g"                 TO  COLLECT-GN
      *
           OPEN    INPUT    COLLECT-FILE
           IF      STS-COLLECT         =   "00"
                   MOVE    30          TO  COLCHK-RC
                   CLOSE   COLLECT-FILE
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               MOVE    SPA-HOSPNUM     TO  COLLECT-HOSPNUM
               MOVE    "n"             TO  COLLECT-GN
      *
               OPEN    INPUT    COLLECT-FILE
               IF      STS-COLLECT     =   "00"
                       MOVE    30      TO  COLCHK-RC
                       CLOSE   COLLECT-FILE
               END-IF
           END-IF
           .
       200-FILEEXIST-CHK-EXT.
           EXIT.
      *
