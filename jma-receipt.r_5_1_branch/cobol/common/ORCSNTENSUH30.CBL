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
       PROGRAM-ID.         ORCSNTENSUH30.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院料点数マスタ付加情報（平成３０年４月）
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
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    平成３０年度改定情報
           COPY    "CPORCSNTENSUH30.INC".
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
           INITIALIZE                  SNTENSU-RETURN-AREA
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
           MOVE    ZERO            TO  SNTENSU-IDX
      *
           PERFORM VARYING SNTENSU-IDX FROM    1   BY  1
                   UNTIL ( SNTENSU-IDX         >   SNTENSUH30-MAX )
                    OR   ( SNTENSU-KBNNUM  NOT =   ZERO )
      *
               IF    ( SNTENSU-SRYCD = SNTENSUH30-SRYCD (SNTENSU-IDX))
                   MOVE    SNTENSUH30-KBNNUM (SNTENSU-IDX)
                                   TO  SNTENSU-KBNNUM
                   MOVE    SNTENSUH30-EDANUM (SNTENSU-IDX)
                                   TO  SNTENSU-EDANUM
                   MOVE    SNTENSUH30-DEKIDAKAKBN (SNTENSU-IDX)
                                   TO  SNTENSU-DEKIDAKAKBN
                   MOVE    SNTENSUH30-TOKUBETSUKBN (SNTENSU-IDX)
                                   TO  SNTENSU-TOKUBETSUKBN
      *
               END-IF
      *
           END-PERFORM
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
