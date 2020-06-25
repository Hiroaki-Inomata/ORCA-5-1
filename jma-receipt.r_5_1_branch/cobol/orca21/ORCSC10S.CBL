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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC10S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為診察料自動発生編集処理
      *                      （Ｋ０２のみ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/05/23  Ｈ１５年６月改正追加
      * 01.00.02     NACL-多々納  04/03/16  Ｈ１６年４月改正追加
      * 01.00.03     NACL-多々納  06/03/16  Ｈ１８年４月改正追加
      * 04.05.00     NACL-多々納  10/03/XX  Ｈ２２年４月改正追加
      * 04.07.00     NACL-多々納  13/03/XX  Ｈ２５年７月労災レセ電対応
      * 04.08.00     NACL-多々納  18/03/XX  Ｈ３０年４月改正追加
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年４月改正追加
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *WORKING-STORAGE             SECTION.
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSC10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-K02
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    TRUE
               WHEN    SPA-SRYYMD          <=  "20020331"
                   CALL    "ORCSC10S200203"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20020401" )  AND
                      (SPA-SRYYMD          <   "20030601" )
                   CALL    "ORCSC10S200204"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20030601" )  AND
                      (SPA-SRYYMD          <   "20040401" )
                   CALL    "ORCSC10S200306"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20040401" )  AND
                      (SPA-SRYYMD          <   "20060401" )
                   CALL    "ORCSC10S200404"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20060401" )  AND
                      (SPA-SRYYMD          <   "20080401" )
                   CALL    "ORCSC10S200604"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSC10S200804"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20120401" )
                   CALL    "ORCSC10S201004"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
      *!!             (SPA-SRYYMD          <   "99999999" )
                      (SPA-SRYYMD          <   "20130701" )
                   CALL    "ORCSC10S201204"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20130701" )  AND
                      (SPA-SRYYMD          <   "20140401" )
                   CALL    "ORCSC10S201307"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20140401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSC10S201404"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "20180401" )
                   CALL    "ORCSC10S201604"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )  AND
                      (SPA-SRYYMD          <   "20200401" )
                   CALL    "ORCSC10S201804"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
      *
               WHEN   (SPA-SRYYMD          >=  "20200401" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSC10S202004"    USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
           END-EVALUATE
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
