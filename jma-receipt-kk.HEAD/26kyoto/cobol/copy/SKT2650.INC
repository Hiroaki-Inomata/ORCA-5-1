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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/11/02  特別療養費国保対応
      *  01.00.02    吉川         11/04/22  東北地方太平洋沖地震対応
      *  01.00.03    門間         18/12/04  修正履歴コメント削除
      ********************************************************************
        01  SKT2650.
            02  SKT2650-SRYYM   PIC X(16).
            02  SKT2650-SRYKA   PIC X(20).
            02  SKT2650-POST    PIC X(8).
            02  SKT2650-ADRS    PIC X(100).
            02  SKT2650-HOSPNAME    PIC X(100).
            02  SKT2650-KAISETUNAME PIC X(40).
            02  SKT2650-TEL PIC X(15).
            02  SKT2650-HOSPCDN PIC X(7).
            02  SKT2650-SEIKYU-TBL      OCCURS  11 TIMES.
              03  SKT2650-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT2650-KENSU   PIC X(6).
                04  SKT2650-TENSU   PIC X(9).
                04  SKT2650-ITBFTN  PIC X(9).
                04  SKT2650-TYOKI   PIC X(2).
                04  SKT2650-GENBAKU PIC X(2).
                04  SKT2650-NISSU   PIC X(3).
                04  SKT2650-S-KENSU PIC X(6).
                04  SKT2650-S-KINGAKU   PIC X(9).
                04  SKT2650-S-FTNGAKU   PIC X(9).
            02  SKT2650-GENMEN-TBL      OCCURS  2 TIMES.
              03  SKT2650-G-KESNU PIC X(6).
              03  SKT2650-G-TENSU PIC X(9).
            02  SKT2650-T-KENSU PIC X(3).
            02  SKT2650-T-TENSU PIC X(9).
            02  SKT2650-SAIGAI-TBL      OCCURS  2 TIMES.
              03  SKT2650-SAI-KBN   PIC X(4).
              03  SKT2650-SAI-KENSU PIC X(6).
              03  SKT2650-SAI-TENSU PIC X(9).
