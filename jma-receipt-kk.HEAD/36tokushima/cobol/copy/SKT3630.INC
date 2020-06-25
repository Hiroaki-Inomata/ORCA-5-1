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
      *  01.00.01    吉川    2011/01/11  特別療養費国保対応
      *  01.00.02    吉川    2015/02/12  開設者氏名のバイト数増加
      *  01.00.03    萩野    2019/01/11  修正履歴コメント削除
      ********************************************************************
        01  SKT3630.
            02  SKT3630-SRYYM   PIC X(16).
            02  SKT3630-HOSPCDN PIC X(20).
            02  SKT3630-HKNJANUM    PIC X(12).
            02  SKT3630-ADRS    PIC X(100).
            02  SKT3630-HOSPNAME    PIC X(100).
            02  SKT3630-TEL PIC X(15).
            02  SKT3630-KAISETUNAME PIC X(60).
            02  SKT3630-ZAI1    PIC X(2).
            02  SKT3630-ZAI2    PIC X(2).
            02  SKT3630-NO  PIC X(4).
            02  SKT3630-TOKUBETU    PIC X(10).
            02  SKT3630-SEIKYU-TBL      OCCURS  2 TIMES.
              03  SKT3630-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT3630-KENSU   PIC X(5).
                04  SKT3630-NISSU   PIC X(5).
                04  SKT3630-TENSU   PIC X(9).
                04  SKT3630-ITBFTN  PIC X(9).
              03  SKT3630-S-KENSU PIC X(5).
              03  SKT3630-S-NISSU PIC X(5).
              03  SKT3630-S-KINGK PIC X(9).
              03  SKT3630-S-FTNGK PIC X(9).
            02  SKT3630-KOUHI-TBL       OCCURS  7 TIMES.
              03  SKT3630-KNYUGAI-TBL     OCCURS  2 TIMES.
                04  SKT3630-K-KENSU  PIC X(5).
                04  SKT3630-K-NISSU  PIC X(5).
                04  SKT3630-K-TENSU  PIC X(9).
                04  SKT3630-K-ITBFTN PIC X(9).
              03  SKT3630-KOHNAME PIC X(10).
              03  SKT3630-KOHNAME2 PIC X(10).
              03  SKT3630-HBTNUM  PIC X(4).
              03  SKT3630-K-SKENSU    PIC X(5).
              03  SKT3630-K-SNISSU    PIC X(5).
              03  SKT3630-K-SKINGK    PIC X(9).
              03  SKT3630-K-SFTNGK    PIC X(9).
