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
      *  コピー句正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/05/13  開設者名のバイト数増加
      *  01.00.02    吉川    2010/08/04  特別療養費国保対応
      *  01.00.03    門間    2020/01/07  修正履歴コメント削除
      *****************************************************************
        01  SKT1610.
            03  SKT1610-NO              PIC X(04).
            03  SKT1610-SRYYM           PIC X(16).
            03  SKT1610-SEIYMD          PIC X(22).
            03  SKT1610-HOSPCD          PIC X(14).
            03  SKT1610-HOSPADRS        PIC X(100).
            03  SKT1610-HOSPNAME        PIC X(100).
            03  SKT1610-KAISETU         PIC X(80).
            03  SKT1610-HKNJANUM        PIC X(12).
            03  SKT1610-HKNJANAME       PIC X(26).
            03  SKT1610-TOKUBETU        PIC X(10).
            03  SKT1610-SEIKYU          OCCURS  11 TIMES.
                05  SKT1610-NYUGAI-TBL      OCCURS  2 TIMES.
                    07  SKT1610-KENSU   PIC X(04).
                    07  SKT1610-NISSU   PIC X(04).
                    07  SKT1610-TENSU   PIC X(07).
                    07  SKT1610-ITBFTN  PIC X(07).
                    07  SKT1610-TYOKI   PIC X(04).
                05  SKT1610-SKENSU      PIC X(04).
                05  SKT1610-SNISSU      PIC X(04).
                05  SKT1610-SKINGK      PIC X(07).
                05  SKT1610-SFTNGK      PIC X(07).
