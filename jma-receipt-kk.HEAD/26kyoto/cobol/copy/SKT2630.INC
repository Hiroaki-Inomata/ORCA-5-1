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
      *  01.00.01    吉川      2010/11/02   特別療養費国保対応
      *  01.00.02    門間      2018/12/04   修正履歴コメント削除
      ********************************************************************
        01  SKT2630.
            02  SKT2630-NO  PIC X(4).
            02  SKT2630-SRYYM   PIC X(16).
            02  SKT2630-SEIYMD  PIC X(22).
            02  SKT2630-ADRS    PIC X(100).
            02  SKT2630-HOSPNAME    PIC X(100).
            02  SKT2630-KAISETUNAME PIC X(40).
            02  SKT2630-TEL PIC X(15).
            02  SKT2630-HKNJANUM    PIC X(16).
            02  SKT2630-HOSPCD  PIC X(14).
            02  SKT2630-ZAI1    PIC X(2).
            02  SKT2630-ZAI2    PIC X(2).
            02  SKT2630-TOKUBETU    PIC  X(10).
            02  SKT2630-SEIKYU-TBL      OCCURS  14 TIMES.
              03  SKT2630-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT2630-KENSU   PIC X(5).
                04  SKT2630-NISSU   PIC X(5).
                04  SKT2630-TENSU   PIC X(9).
                04  SKT2630-ITBFTN  PIC X(9).
              03  SKT2630-S-KENSU PIC X(5).
              03  SKT2630-S-NISSU PIC X(5).
              03  SKT2630-S-KINGAKU   PIC X(9).
              03  SKT2630-S-FTNGAKU   PIC X(9).
            02  SKT2630-KOH-TBL         OCCURS  2 TIMES.
              03  SKT2630-KOHNAME PIC X(10).
              03  SKT2630-K-NYUGAIKBN-TBL         OCCURS  2 TIMES.
                04  SKT2630-KKENSU  PIC X(5).
                04  SKT2630-KNISSU  PIC X(5).
                04  SKT2630-KTENSU  PIC X(9).
                04  SKT2630-KITBFTN PIC X(9).
              03  SKT2630-S-KKENSU    PIC X(5).
              03  SKT2630-S-KNISSU    PIC X(5).
              03  SKT2630-S-KKINGAKU  PIC X(9).
              03  SKT2630-S-KFTNGAKU  PIC X(9).
