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
        01  SKT3620.
            02  SKT3620-SRYYM   PIC X(16).
            02  SKT3620-SEIYMD  PIC X(22).
            02  SKT3620-HOSPCDN PIC X(20).
            02  SKT3620-HKNJANUM    PIC X(12).
            02  SKT3620-KYURATE
                    PIC X(2)    OCCURS  4 TIMES.
            02  SKT3620-ADRS    PIC X(100).
            02  SKT3620-HOSPNAME    PIC X(100).
            02  SKT3620-TEL PIC X(15).
            02  SKT3620-KAISETUNAME PIC X(60).
            02  SKT3620-ZAI1    PIC X(2).
            02  SKT3620-ZAI2    PIC X(2).
            02  SKT3620-NO  PIC X(4).
            02  SKT3620-TOKUBETU    PIC X(10).
            02  SKT3620-SEIKYU          OCCURS  9 TIMES.
              03  SKT3620-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT3620-KENSU   PIC X(5).
                04  SKT3620-NISSU   PIC X(5).
                04  SKT3620-TENSU   PIC X(9).
                04  SKT3620-ITBFTN  PIC X(9).
              03  SKT3620-SKENSU PIC X(5).
              03  SKT3620-SNISSU PIC X(5).
              03  SKT3620-SKINGK PIC X(9).
              03  SKT3620-SFTNGK PIC X(9).
