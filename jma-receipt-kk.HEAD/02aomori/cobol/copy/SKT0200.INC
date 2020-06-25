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
        01  SKT0200.
            02  SKT0200-SRYYM   PIC X(16).
            02  SKT0200-HOSPCDN PIC X(40).
            02  SKT0200-ADRS    PIC X(100).
            02  SKT0200-HOSPNAME    PIC X(100).
            02  SKT0200-KAISETUNAME PIC X(40).
            02  SKT0200-TEL PIC X(15).
            02  SKT0200-TOKUBETU PIC X(10).
            02  SKT0200-SEIKYU-TBL      OCCURS  13 TIMES.
              03  SKT0200-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT0200-KENSU   PIC X(5).
                04  SKT0200-NISSU   PIC X(5).
                04  SKT0200-TENSU   PIC X(9).
                04  SKT0200-ITBFTN  PIC X(9).
              03  SKT0200-S-KENSU PIC X(5).
              03  SKT0200-S-NISSU PIC X(5).
              03  SKT0200-S-KINGAKU   PIC X(9).
              03  SKT0200-S-FTNGAKU   PIC X(9).
