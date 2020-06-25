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
        01  SKT0220.
            02  SKT0220-SRYYM   PIC X(16).
            02  SKT0220-HKNJANAME   PIC X(26).
            02  SKT0220-HOSPCDN PIC X(40).
            02  SKT0220-SEIYMD  PIC X(22).
            02  SKT0220-ADRS    PIC X(100).
            02  SKT0220-HOSPNAME    PIC X(100).
            02  SKT0220-KAISETUNAME PIC X(40).
            02  SKT0220-TEL PIC X(15).
            02  SKT0220-NO  PIC X(4).
            02  SKT0220-TOKUBETU PIC X(10).
            02  SKT0220-KYURATE-TBL     OCCURS  2 TIMES.
              03  SKT0220-KYURATE PIC X(1).
            02  SKT0220-SEIKYU-TBL      OCCURS  12 TIMES.
              03  SKT0220-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT0220-KENSU   PIC X(5).
                04  SKT0220-NISSU   PIC X(5).
                04  SKT0220-TENSU   PIC X(9).
                04  SKT0220-ITBFTN  PIC X(9).
              03  SKT0220-S-KENSU PIC X(5).
              03  SKT0220-S-NISSU PIC X(5).
              03  SKT0220-S-KINGAKU   PIC X(9).
              03  SKT0220-S-FTNGAKU   PIC X(9).
