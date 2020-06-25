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
       PROGRAM-ID.             ORCSINFSTATE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 感染症サーベイランス
      *                      参加送信状態チェック処理
      *  返却コード        : 00:正常
      *                      10:データ作成なし(システム管理)
      *                      30:データ作成処理中
      *                      40:データ作成処理済み(ジョブ管理あり含む)
      *                      90:データ作成処理中(開始日が当日でない)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11.05.19    伊藤          新規作成
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
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-YOBI            PIC 9(01).
           03  WRK-STR-YMD         PIC X(08).
           03  WRK-END-YMD         PIC X(08).
      *
       01  CONST-AREA.
      *    感染症参加  
           03  CONST-INFAJOBID     PIC 9(07)  VALUE    1.
           03  CONST-INFASHELLID   PIC X(08)  VALUE    "INFAGREE".
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
           COPY    "CPORCSINFCHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           SPA-AREA
           ORCSINFCHKAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  INFCHK-RC
      *
      *    ジョブ管理チェック（感染症）
           IF     (SPA-DATAINFECTIONKBN    NOT =   1)  AND
                  (SPA-DATAINFECTIONKBN    NOT =   2)
               MOVE    10                  TO  INFCHK-RC
           END-IF
      *
      *    マシン日付の週の月曜日の日付を求める
      **** PERFORM 100-GET-YMD-SEC
      *
      *    ジョブ管理から今週処理の実績を調べる
      **** PERFORM 200-JOBEXEC-CHK-SEC
      *
      *    ジョブ管理の有無を調べる
           PERFORM 210-JOBEXIST-CHK-SEC
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    月曜日算出処理
      *****************************************************************
       100-GET-YMD-SEC             SECTION.
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-END-YMD
      *
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-END-YMD         TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  WRK-YOBI
           ELSE
               MOVE    ZERO                TO  WRK-YOBI
           END-IF
      *
           IF      WRK-YOBI                =   1
      *        月曜日
               MOVE    WRK-END-YMD         TO  WRK-STR-YMD
           ELSE
      *        マシン日付の週の月曜日は？
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-END-YMD         TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN      =   (WRK-YOBI  *  -1)  +  1
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-IF
           .
       100-GET-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実績チェック処理
      *****************************************************************
       200-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    30              TO  INFCHK-RC
                   IF      JOB-STARTYMD    NOT =   WRK-END-YMD
                       MOVE    90          TO  INFCHK-RC
                   END-IF
               ELSE
                   IF     (JOB-ENDYMD      >=  WRK-STR-YMD)  AND
                          (JOB-ENDYMD      <=  WRK-END-YMD)
                       MOVE    40          TO  INFCHK-RC
                   END-IF
               END-IF
           END-IF
           .
       200-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理有無チェック処理
      *****************************************************************
       210-JOBEXIST-CHK-SEC        SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
                   MOVE    40          TO  INFCHK-RC
           END-IF
           .
       210-JOBEXIST-CHK-EXT.
           EXIT.
      *
