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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2011/06/15  特別療養費国保対応
      *  01.00.02    井上    2019/12/09  修正履歴コメント削除
      ********************************************************************
        01  SKT0230.
            02  SKT0230-HOSPCDN PIC X(40).
            02  SKT0230-NO  PIC X(4).
            02  SKT0230-TOKUBETU PIC X(10).
            02  SKT0230-SEIKYU-TBL      OCCURS  2 TIMES.
              03  SKT0230-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT0230-KENSU   PIC X(5).
                04  SKT0230-NISSU   PIC X(5).
                04  SKT0230-TENSU   PIC X(9).
                04  SKT0230-ITBFTN  PIC X(11).
              03  SKT0230-S-KENSU PIC X(5).
              03  SKT0230-S-NISSU PIC X(5).
              03  SKT0230-S-KINGAKU   PIC X(9).
              03  SKT0230-S-FTNGAKU   PIC X(9).
            02  SKT0230-KOH-TBL         OCCURS  8 TIMES.
              03  SKT0230-HBTNUM  PIC X(2).
              03  SKT0230-KOHNAME PIC X(10).
              03  SKT0230-K-NYUGAI-TBL            OCCURS  2 TIMES.
                04  SKT0230-KKENSU  PIC X(5).
                04  SKT0230-KNISSU  PIC X(5).
                04  SKT0230-KTENSU  PIC X(9).
                04  SKT0230-KITBFTN PIC X(9).
              03  SKT0230-S-KKENSU    PIC X(5).
              03  SKT0230-S-KNISSU    PIC X(5).
              03  SKT0230-S-KKINGAKU  PIC X(9).
              03  SKT0230-S-KFTNGAKU  PIC X(9).
