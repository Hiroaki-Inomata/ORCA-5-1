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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2007/10/05   システム管理マスタ「1901」の
      *                                     医療機関情報を参照
      *  01.00.02    吉川      2010/10/01   特別療養費国保対応
      *  01.00.03    萩野      2018/11/07   修正履歴コメント削除
      ************************************************************
        01  SKT1810.
            03  SKT1810-SRYYM         PIC X(16).
            03  SKT1810-SEIYMD        PIC X(22).
            03  SKT1810-HOSPCD        PIC X(14).
            03  SKT1810-ADRS          PIC X(100).
            03  SKT1810-HOSPNAME      PIC X(100).
            03  SKT1810-TEL           PIC X(15).
            03  SKT1810-KAISETU       PIC X(60).
            03  SKT1810-HKNJANUM      PIC X(16).
            03  SKT1810-KYURATE       PIC X(04) OCCURS  2 TIMES.
            03  SKT1810-ZAI1          PIC X(02).
            03  SKT1810-ZAI2          PIC X(02).
            03  SKT1810-NO            PIC X(04).
            03  SKT1810-TOKUBETU      PIC X(10).
            03  SKT1810-SEIKYU-TBL      OCCURS  5 TIMES.
                05  SKT1810-NYUGAI-TBL      OCCURS  2 TIMES.
                    07  SKT1810-KENSU     PIC X(05).
                    07  SKT1810-NISSU     PIC X(06).
                    07  SKT1810-TENSU     PIC X(09).
                    07  SKT1810-KESEKYUFU PIC X(09).
                    07  SKT1810-ITBFTN    PIC X(09).
                05  SKT1810-SKENSU      PIC X(05).
                05  SKT1810-SNISSU      PIC X(06).
                05  SKT1810-SKINGK      PIC X(09).
                05  SKT1810-SFTNGK      PIC X(09).
            03  SKT1810-KESE-TBL.
                05  SKT1810-KESE-KENSU  PIC X(05).
                05  SKT1810-KESE-NISSU  PIC X(06).
                05  SKT1810-KESE-TENSU  PIC X(09).
            03  SKT1810-KOH-TBL         OCCURS  3 TIMES.
                05  SKT1810-KOHNAME     PIC X(08).
                05  SKT1810-HBTNUM      PIC X(02).
                05  SKT1810-HBTMARU     PIC X(02).
                05  SKT1810-K-KENSU     PIC X(05).
                05  SKT1810-K-NISSU     PIC X(06).
                05  SKT1810-K-TENSU     PIC X(09).
                05  SKT1810-K-ITBFTN    PIC X(09).
                05  SKT1810-K-SKENSU    PIC X(05).
                05  SKT1810-K-SNISSU    PIC X(06).
                05  SKT1810-K-SKINGK    PIC X(09).
                05  SKT1810-K-SFTNGK    PIC X(09).
