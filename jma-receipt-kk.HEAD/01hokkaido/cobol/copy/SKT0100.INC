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
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    萩野    2018/09/10  修正履歴コメント削除
      ********************************************************************
        01  SKT0100.
            02  SKT0100-SEIYMD  PIC X(22).
            02  SKT0100-HOSPCD  PIC X(7).
            02  SKT0100-ADRS    PIC X(100).
            02  SKT0100-HOSPNAME    PIC X(100).
            02  SKT0100-ZAI1    PIC X(2).
            02  SKT0100-ZAI2    PIC X(2).
            02  SKT0100-TOKUBETU    PIC X(10).
            02  SKT0100-HKN-TBL         OCCURS  2 TIMES.
              03  SKT0100-MAISU   PIC X(5).
              03  SKT0100-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT0100-KENSU   PIC X(5).
                04  SKT0100-TENSU   PIC X(14).
            02  SKT0100-ETC-TBL         OCCURS  2 TIMES.
              03  SKT0100-ETC-KENSU   PIC X(5).
              03  SKT0100-ETC-TENSU   PIC X(14).
            02  SKT0100-FUKUSI-TBL      OCCURS  2 TIMES.
              03  SKT0100-F-KENSU
                      PIC X(5)    OCCURS  4 TIMES.
              03  SKT0100-F-TENSU
                      PIC X(11)   OCCURS  4 TIMES.
