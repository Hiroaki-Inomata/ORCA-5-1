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
      * Maj/Min/Rev  修正者    日付         内容
      *  01.00.01    吉川      2010/11/02   特別療養費国保対応
      *  01.00.02    門間      2018/12/03   修正履歴コメント削除
      ********************************************************************
        01  SKT2710.
            02  SKT2710-SRYYM   PIC X(16).
            02  SKT2710-SEIYMD  PIC X(22).
            02  SKT2710-PREFNAME    PIC X(6).
            02  SKT2710-HKNJANAME   PIC X(18).
            02  SKT2710-HKNJANUM    PIC X(12).
            02  SKT2710-HOSPCDN PIC X(20).
            02  SKT2710-ADRS    PIC X(100).
            02  SKT2710-HOSPNAME    PIC X(100).
            02  SKT2710-KAISETU PIC X(40).
            02  SKT2710-TEL PIC X(15).
            02  SKT2710-TOKUBETU    PIC  X(10).
            02  SKT2710-SEIKYU-TBL      OCCURS  12 TIMES.
              03  SKT2710-NYUGAI-TBL      OCCURS  2 TIMES.
                04  SKT2710-KENSU   PIC X(5).
                04  SKT2710-NISSU   PIC X(5).
                04  SKT2710-TENSU   PIC X(9).
                04  SKT2710-ITBFTN  PIC X(9).
              03  SKT2710-SKENSU  PIC X(5).
              03  SKT2710-SNISSU  PIC X(5).
              03  SKT2710-SKINGK  PIC X(9).
              03  SKT2710-SFTNGK  PIC X(9).
            02  SKT2710-GENBK-TBL       OCCURS  2 TIMES.
              03  SKT2710-GENBK-NYUGAI            OCCURS  2 TIMES.
                04  SKT2710-GENBK-KENSU PIC X(5).
            02  SKT2710-MENJO   PIC X(4).
            02  SKT2710-KOH-TBL         OCCURS  2 TIMES.
              03  SKT2710-KOHNAME PIC X(8).
