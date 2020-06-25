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
       PROGRAM-ID.             ORCHC02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 処方箋　印刷（Ａ５）
      *                      （ORCHC02Q の QR印刷を削除）
      *                      印刷ＰＧ　ＣＡＬＬ用
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/02/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCHC19.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC19AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    TRUE
               WHEN    SPA-SRYYMD          <   "20120401"
      *        平成２４年３月まで
                   CALL    "ORCHC02V01"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
      *        平成２４年４月から平成２８年３月まで
                   CALL    "ORCHC02V02"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )   AND
                      (SPA-SRYYMD          <   "20180401" )
      *        平成２８年４月から
                   CALL    "ORCHC02V03"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )
                 AND  (SPA-SRYYMD          <   "20200501" )
      *        平成３０年４月から
                   CALL    "ORCHC02V04"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20200501" )
                 AND  (SPA-SRYYMD          <   "99999999" )
      *        令和０２年５月から（枝番対応）
                   CALL    "ORCHC02V05"        USING   SPA-AREA
                                                       ORCHC19AREA
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *
