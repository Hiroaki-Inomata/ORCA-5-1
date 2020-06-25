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
      *  01.00.02    吉川    2011/01/11  特別療養費国保対応
      *  01.00.03    吉川    2015/02/12  開設者氏名のバイト数増加
      *  01.00.04    萩野    2019/01/11  修正履歴コメント削除
      ********************************************************************
        01  SKT3605.
            02  SKT3605-PREF    PIC X(08).
            02  SKT3605-SRYYM   PIC X(16).
            02  SKT3605-SEIYMD  PIC X(22).
            02  SKT3605-HOSPCD  PIC X(20).
            02  SKT3605-POST    PIC X(8).
            02  SKT3605-ADRS    PIC X(100).
            02  SKT3605-HOSPNAME    PIC X(100).
            02  SKT3605-KAISETU PIC X(60).
            02  SKT3605-PREFKBN PIC X(8).
            02  SKT3605-ZAI1    PIC X(2).
            02  SKT3605-ZAI2    PIC X(2).
            02  SKT3605-TOKUBETU    PIC X(10).
            02  SKT3605-SEIKYU          OCCURS  11 TIMES.
              03  SKT3605-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT3605-KENSU   PIC X(5).
                04  SKT3605-NISSU   PIC X(5).
                04  SKT3605-TENSU   PIC X(10).
                04  SKT3605-ITBFTN2 PIC X(10).
              03  SKT3605-ITBFTN  PIC X(9).
              03  SKT3605-SKENSU  PIC X(5).
              03  SKT3605-SNISSU  PIC X(6).
              03  SKT3605-SKINGK  PIC X(10).
              03  SKT3605-SFTNGK  PIC X(9).
            02  SKT3605-KOH-TBL         OCCURS  7 TIMES.
              03  SKT3605-KOHNAME PIC X(12).
              03  SKT3605-KOHNAME2 PIC X(20).
              03  SKT3605-K-NYUGAI        OCCURS  2 TIMES.
                04  SKT3605-K-KENSU PIC X(5).
                04  SKT3605-K-NISSU PIC X(5).
                04  SKT3605-K-TENSU PIC X(10).
                04  SKT3605-K-ITBFTN    PIC X(9).
              03  SKT3605-K-SKENSU    PIC X(5).
              03  SKT3605-K-SNISSU    PIC X(6).
              03  SKT3605-K-SKINGK    PIC X(10).
              03  SKT3605-K-SFTNGK    PIC X(9).
