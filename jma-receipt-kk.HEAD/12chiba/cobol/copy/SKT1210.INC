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
      *  01.00.01    吉川    2010/04/02  特別療養費国保対応
      *  01.00.02    門間    2018/11/01  修正履歴コメント削除
      ********************************************************************
        01  SKT1210.
            02  SKT1210-SRYYM   PIC X(16).
            02  SKT1210-SEIYMD  PIC X(22).
            02  SKT1210-ADRS    PIC X(100).
            02  SKT1210-HOSPNAME    PIC X(100).
            02  SKT1210-TEL PIC X(15).
            02  SKT1210-KAISETUNAME PIC X(40).
            02  SKT1210-HKNJANUM    PIC X(12).
            02  SKT1210-HOSPCD  PIC X(14).
            02  SKT1210-RECEKA  PIC X(20).
            02  SKT1210-KYURATE-TBL     OCCURS  3 TIMES.
              03  SKT1210-KYURATE PIC X(2).
            02  SKT1210-NO  PIC X(4).
            02  SKT1210-SEIKYU-TBL      OCCURS  11 TIMES.
              03  SKT1210-KENSU   PIC X(5).
              03  SKT1210-NISSU   PIC X(5).
              03  SKT1210-TENSU   PIC X(9).
              03  SKT1210-ITBFTN  PIC X(9).
              03  SKT1210-SKENSU  PIC X(5).
              03  SKT1210-SNISSU  PIC X(5).
              03  SKT1210-SKINGK  PIC X(9).
              03  SKT1210-SFTNGK  PIC X(9).
            02  SKT1210-KOH-TBL         OCCURS  5 TIMES.
              03  SKT1210-KOHNAME PIC X(8).
              03  SKT1210-K-KBN   PIC X(4).
              03  SKT1210-K-KENSU PIC X(5).
              03  SKT1210-K-NISSU PIC X(5).
              03  SKT1210-K-TENSU PIC X(9).
              03  SKT1210-K-ITBFTN    PIC X(7).
              03  SKT1210-K-KOHTENSU  PIC X(7).
              03  SKT1210-K-SKENSU    PIC X(5).
              03  SKT1210-K-SNISSU    PIC X(5).
              03  SKT1210-K-SKINGK    PIC X(9).
              03  SKT1210-K-SFTNGK    PIC X(9).
            02  SKT1210-TOKUBETU  PIC X(10).
