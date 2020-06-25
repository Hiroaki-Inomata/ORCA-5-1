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
      *----------------------------------------------------------*
      *    請求書兼領収書収納編集パラメタ  COPY (CPORCHC03SUB.INC)
      *                                          (ORCHC03SUB で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHC03SUBAREA.
      *出力
           03  ORCHC03SUB-MAX          PIC 9(04).
      *
           03  ORCHC03SUB-SYUNOU-TBL       OCCURS  15.
      *        収納マスタ
               05  ORCHC03SUB-SYUNOU-REC       PIC X(2000).
      *        請求額等
               05  ORCHC03SUB-SKYMONEY         PIC S9(07).
               05  ORCHC03SUB-SKYMONEY-TAX-SAI PIC S9(07).
               05  ORCHC03SUB-NYUKIN-TOTAL     PIC S9(07).
               05  ORCHC03SUB-HENKIN           PIC S9(07).
      *        同時診療科
               05  ORCHC03SUB-DOUJI-SRYKA-G.
                   07  ORCHC03SUB-DOUJI-SRYKA  PIC  X(02)
                                               OCCURS  10.
      *        同時保険組合せ
               05  ORCHC03SUB-DOUJI-HKNCOMBI-G.
                   07  ORCHC03SUB-DOUJI-HKNCOMBI   PIC  X(04)
                                               OCCURS  10.
      *        同時伝票番号（診療費明細書用）
               05  ORCHC03SUB-DOUJI-DENPNUM-G.
                   07  ORCHC03SUB-DOUJI-DENPNUM    PIC  9(07)
                                               OCCURS  15.
      *
