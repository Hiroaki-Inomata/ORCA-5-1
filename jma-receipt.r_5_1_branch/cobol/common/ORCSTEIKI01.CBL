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
       PROGRAM-ID.                     ORCSTEIKI01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 定期請求サブ０１
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/01/23    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX01                   PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-JOBID               PIC 9(07).
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
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSTEIKI01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           ORCSTEIKI01AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       STEIKI01-OT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( STEIKI01-JOBID      >   1 )
               PERFORM 2001-CHK-PREJOB-SEC
           END-IF
      *
           IF    ( STEIKI01-STOP       =   ZERO )
               PERFORM 2001-CHK-SELFJOB-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前ジョブチェック処理
      *****************************************************************
       2001-CHK-PREJOB-SEC             SECTION.
      *
           COMPUTE WRK-JOBID   =   STEIKI01-JOBID  -   1
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
            AND  ( JOB-STOPFLG         =   2    )
               MOVE    1                   TO  STEIKI01-STOP
           END-IF
      *
           .
       2001-CHK-PREJOB-EXT.
           EXIT.
      *****************************************************************
      *    自ジョブチェック処理
      *****************************************************************
       2001-CHK-SELFJOB-SEC             SECTION.
      *
           MOVE    STEIKI01-JOBID          TO  WRK-JOBID
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
            AND  ( JOB-STOPFLG         =   2    )
               MOVE    1                   TO  STEIKI01-STOP
           END-IF
      *
           .
       2001-CHK-PREJOB-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-CHK-ORCSJOB-SEC                 SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-JOBID           TO  JOB-JOBID
           MOVE    STEIKI01-SHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
      *
       800-CHK-ORCSJOB-EXT.
           EXIT.
