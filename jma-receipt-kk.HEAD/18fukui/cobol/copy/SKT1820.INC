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
      *  01.00.02    萩野      2018/11/07   修正履歴コメント削除
      ********************************************************************
        01  SKT1820.
            03  SKT1820-SRYYM           PIC X(16).
            03  SKT1820-SEIYMD          PIC X(22).
            03  SKT1820-HOSPCD          PIC X(14).
            03  SKT1820-HKNJANUM        PIC X(16).
            03  SKT1820-ZAI1            PIC X(02).
            03  SKT1820-ZAI2            PIC X(02).
            03  SKT1820-NO              PIC X(04).
            03  SKT1820-TOKUBETU        PIC X(10).
            03  SKT1820-SEIKYU-TBL      OCCURS  7 TIMES.
              05  SKT1820-NYUGAI-TBL      OCCURS  2 TIMES.
                07  SKT1820-KENSU       PIC X(05).
                07  SKT1820-NISSU       PIC X(06).
                07  SKT1820-TENSU       PIC X(09).
                07  SKT1820-KESEKYUFU   PIC X(09).
                07  SKT1820-ITBFTN      PIC X(09).
              05  SKT1820-SKENSU        PIC X(05).
              05  SKT1820-SNISSU        PIC X(06).
              05  SKT1820-SKINGK        PIC X(09).
              05  SKT1820-SFTNGK        PIC X(09).
