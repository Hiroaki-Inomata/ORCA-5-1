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
      *  01.00.01    吉川    2009/06/08  点数、金額、食事回数のバイト数増加
      *  01.00.02    吉川    2015/02/12  開設者氏名のバイト数増加
      ********************************************************************
        01  SKT3600.
            02  SKT3600-SRYYM   PIC X(16).
            02  SKT3600-SEIYMD  PIC X(22).
            02  SKT3600-HOSPCD  PIC X(20).
            02  SKT3600-POST    PIC X(8).
            02  SKT3600-ADRS    PIC X(100).
            02  SKT3600-HOSPNAME    PIC X(100).
            02  SKT3600-KAISETU PIC X(60).
            02  SKT3600-PREFKBN PIC X(8).
            02  SKT3600-ZAI1    PIC X(2).
            02  SKT3600-ZAI2    PIC X(2).
            02  SKT3600-SEIKYU          OCCURS  11 TIMES.
              03  SKT3600-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT3600-KENSU   PIC X(5).
                04  SKT3600-NISSU   PIC X(5).
                04  SKT3600-TENSU   PIC X(10).
                04  SKT3600-ITBFTN2 PIC X(10).
              03  SKT3600-ITBFTN  PIC X(9).
              03  SKT3600-SKENSU  PIC X(5).
              03  SKT3600-SNISSU  PIC X(6).
              03  SKT3600-SKINGK  PIC X(10).
              03  SKT3600-SFTNGK  PIC X(9).
            02  SKT3600-KOH-TBL         OCCURS  7 TIMES.
              03  SKT3600-KOHNAME PIC X(12).
              03  SKT3600-KOHNAME2 PIC X(20).
              03  SKT3600-K-NYUGAI        OCCURS  2 TIMES.
                04  SKT3600-K-KENSU PIC X(5).
                04  SKT3600-K-NISSU PIC X(5).
                04  SKT3600-K-TENSU PIC X(10).
                04  SKT3600-K-ITBFTN    PIC X(9).
              03  SKT3600-K-SKENSU    PIC X(5).
              03  SKT3600-K-SNISSU    PIC X(6).
              03  SKT3600-K-SKINGK    PIC X(10).
              03  SKT3600-K-SFTNGK    PIC X(9).
