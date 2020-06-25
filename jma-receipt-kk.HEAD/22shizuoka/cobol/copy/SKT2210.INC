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
      *  01.00.01    吉川      2010/10/13   特別療養費国保対応
      *  01.00.02    萩野      2018/09/10   修正履歴コメント削除
      ********************************************************************
        01  SKT2210.
            02  SKT2210-PRTYM   PIC X(16).
            02  SKT2210-HKNJANUM    PIC X(12).
            02  SKT2210-HKNJANAME   PIC X(26).
            02  SKT2210-ZAIMARU PIC X(2).
            02  SKT2210-ZAIMOJI PIC X(2).
            02  SKT2210-SEIYMD  PIC X(22).
            02  SKT2210-HOSPCD  PIC X(7).
            02  SKT2210-ADRS    PIC X(100).
            02  SKT2210-HOSPNAME    PIC X(100).
            02  SKT2210-KAISETUNAME PIC X(40).
            02  SKT2210-TEL PIC X(15).
            02  SKT2210-NO  PIC X(4).
            02  SKT2210-TOKUBETU            PIC  X(10).
            02  SKT2210-DATA1           OCCURS  9 TIMES.
              03  SKT2210-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT2210-KENSU   PIC X(5).
                04  SKT2210-NISSU   PIC X(5).
                04  SKT2210-TENSU   PIC X(9).
                04  SKT2210-ITBFTN  PIC X(9).
              03  SKT2210-SKENSU  PIC X(5).
              03  SKT2210-SNISSU  PIC X(5).
              03  SKT2210-SKINGK  PIC X(9).
              03  SKT2210-SFTNGK  PIC X(9).
            02  SKT2210-DATA2           OCCURS  4 TIMES.
              03  SKT2210-KOHNAME PIC X(8).
              03  SKT2210-HBTNUM  PIC X(2).
              03  SKT2210-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT2210-K-KENSU PIC X(5).
                04  SKT2210-K-NISSU PIC X(5).
                04  SKT2210-K-TENSU PIC X(9).
                04  SKT2210-K-ITBFTN    PIC X(9).
              03  SKT2210-K-SKENSU    PIC X(5).
              03  SKT2210-K-SNISSU    PIC X(5).
              03  SKT2210-K-SKINGK    PIC X(9).
              03  SKT2210-K-SFTNGK    PIC X(9).
            02  SKT2210-TYOKI-TBL       OCCURS  2 TIMES.
              03  SKT2210-TYOKI   PIC X(7).
            02  SKT2210-GOKEI-TBL       OCCURS  2 TIMES.
              03  SKT2210-GKENSU  PIC X(5).
              03  SKT2210-GKINGAKU    PIC X(9).
