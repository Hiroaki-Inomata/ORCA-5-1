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
      *  01.00.01    吉川    2010/08/19  特別療養費国保対応
      *  01.00.02    門間    2019/04/23  修正履歴コメント削除
      ********************************************************************
        01  SKT1710.
            03  SKT1710-SRYYM           PIC X(16).
            03  SKT1710-SEIYMD          PIC X(22).
            03  SKT1710-HOSPCD          PIC X(14).
            03  SKT1710-ADRS            PIC X(100).
            03  SKT1710-HOSPNAME        PIC X(100).
            03  SKT1710-KAISETU         PIC X(80).
            03  SKT1710-HKNJANUM        PIC X(12).
            03  SKT1710-NO              PIC X(04).
            03  SKT1710-TOKUBETU        PIC X(10).
            03  SKT1710-SEIKYU-TBL      OCCURS  13 TIMES.
                05  SKT1710-NYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT1710-KENSU   PIC X(05).
                    07  SKT1710-NISSU   PIC X(05).
                    07  SKT1710-TENSU   PIC X(09).
                    07  SKT1710-YKZFTN  PIC X(07).
                    07  SKT1710-ITBFTN  PIC X(09).
                    07  SKT1710-TYOKI   PIC X(03).
                05  SKT1710-SKENSU      PIC X(05).
                05  SKT1710-SNISSU      PIC X(05).
                05  SKT1710-SKINGK      PIC X(09).
                05  SKT1710-SFTNGK      PIC X(09).
            03  SKT1710-ZAITAKU-TBL     OCCURS  6 TIMES.
                05  SKT1710-Z-KENSU     PIC X(05).
                05  SKT1710-Z-NISSU     PIC X(05).
                05  SKT1710-Z-TENSU     PIC X(09).
                05  SKT1710-Z-ITBFTN    PIC X(09).
                05  SKT1710-Z-TYOKI     PIC X(03).
            03  SKT1710-KOHI-TBL        OCCURS  9 TIMES.
                05  SKT1710-K-KENSU     PIC X(05).
                05  SKT1710-K-NISSU     PIC X(05).
                05  SKT1710-K-TENSU     PIC X(09).
                05  SKT1710-K-YKZFTN    PIC X(07).
                05  SKT1710-K-ITBFTN    PIC X(09).
                05  SKT1710-K-SKENSU    PIC X(05).
                05  SKT1710-K-SNISSU    PIC X(05).
                05  SKT1710-K-SKINGK    PIC X(09).
                05  SKT1710-K-SFTNGK    PIC X(09).
