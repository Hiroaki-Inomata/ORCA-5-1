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
      *  01.00.01    吉川      2010/10/01   特別療養費国保対応
      *  01.00.02    萩野      2018/12/12   修正履歴コメント削除
      ********************************************************************
        01  SKT2020.
            02  SKT2020-SRYYM   PIC X(16).
            02  SKT2020-HKNJANAME   PIC X(20).
            02  SKT2020-SEIYMD  PIC X(22).
            02  SKT2020-ADRS    PIC X(100).
            02  SKT2020-HOSPNAME    PIC X(100).
            02  SKT2020-KAISETUNAME PIC X(80).
            02  SKT2020-TEL PIC X(15).
            02  SKT2020-HKNJANUM    PIC X(12).
            02  SKT2020-HOSPCD  PIC X(14).
            02  SKT2020-ZAI1    PIC X(2).
            02  SKT2020-ZAI2    PIC X(2).
            02  SKT2020-NO  PIC X(4).
            02  SKT2020-TOKUBETU  PIC X(10).
            02  SKT2020-SEIKYU-TBL      OCCURS  7 TIMES.
              03  SKT2020-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT2020-KENSU   PIC X(8).
                04  SKT2020-NISSU   PIC X(8).
                04  SKT2020-TENSU   PIC X(18).
                04  SKT2020-ITBFTN  PIC X(16).
              03  SKT2020-SKENSU  PIC X(8).
              03  SKT2020-SNISSU  PIC X(8).
              03  SKT2020-SKINGK  PIC X(18).
              03  SKT2020-SFTNGK  PIC X(18).
