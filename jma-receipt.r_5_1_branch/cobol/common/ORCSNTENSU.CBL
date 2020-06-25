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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNTENSU.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院料点数マスタ付加情報
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/03/03    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  CONST-AREA.
           03  CONST-R020401       PIC X(08)   VALUE "20200401".
           03  CONST-H300401       PIC X(08)   VALUE "20180401".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSNTENSU.INC".
           COPY    "CPTENSU.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNTENSU-AREA
           TNS-TENSU-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SNTENSU-BASEYMD    >=   CONST-R020401 )
               CALL    "ORCSNTENSUR02" USING
                                       SNTENSU-AREA
                                       TNS-TENSU-REC
                                       SPA-AREA
           WHEN  ( SNTENSU-BASEYMD    >=   CONST-H300401 )
               CALL    "ORCSNTENSUH30" USING
                                       SNTENSU-AREA
                                       TNS-TENSU-REC
                                       SPA-AREA
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
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
