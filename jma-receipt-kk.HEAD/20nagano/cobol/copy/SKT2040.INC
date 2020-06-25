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
        01  SKT2040.
            02  SKT2040-HKNJANUM    PIC X(12).
            02  SKT2040-HOSPCD  PIC X(14).
            02  SKT2040-ZAI1    PIC X(2).
            02  SKT2040-ZAI2    PIC X(2).
            02  SKT2040-NO  PIC X(4).
            02  SKT2040-TOKUBETU  PIC X(10).
            02  SKT2040-KOH-TBL         OCCURS  7 TIMES.
              03  SKT2040-KOHNUM  PIC X(4).
              03  SKT2040-KOHNAME PIC X(12).
              03  SKT2040-K-NYUGAI-TBL            OCCURS  2 TIMES.
                04  SKT2040-K-KENSU PIC X(8).
                04  SKT2040-K-NISSU PIC X(8).
                04  SKT2040-K-TENSU PIC X(16).
                04  SKT2040-K-ITBFTN    PIC X(16).
              03  SKT2040-K-SKENSU    PIC X(8).
              03  SKT2040-K-SNISSU    PIC X(8).
              03  SKT2040-K-SKINGK    PIC X(16).
              03  SKT2040-K-SFTNGK    PIC X(16).
            02  SKT2040-TYOKI-TBL       OCCURS  3 TIMES.
              03  SKT2040-T-KENSU PIC X(6).
