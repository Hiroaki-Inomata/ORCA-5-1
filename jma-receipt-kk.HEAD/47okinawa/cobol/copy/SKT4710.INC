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
      *  01.00.01    吉川         11/03/04  特別療養費国保対応
      *  01.00.02    井上         18/11/01  修正履歴コメント削除
      ********************************************************************
        01  SKT4710.
            03  SKT4710-NO              PIC X(04).
            03  SKT4710-SRYYM           PIC X(16).
            03  SKT4710-SEIYMD          PIC X(22).
            03  SKT4710-HOSPCD          PIC X(14).
            03  SKT4710-HOSPADRS        PIC X(100).
            03  SKT4710-HOSPNAME        PIC X(100).
            03  SKT4710-KAISETU         PIC X(40).
            03  SKT4710-TEL             PIC X(15).
            03  SKT4710-HKNJANAME       PIC X(20).
            03  SKT4710-HKNJANUM        PIC X(12).
            03  SKT4710-KYURATE         PIC X(02) OCCURS  4 TIMES.
            03  SKT4710-ZAI1            PIC X(02).
            03  SKT4710-ZAI2            PIC X(02).
            03  SKT4710-RECEKA          PIC X(20).
            03  SKT4710-TOKUBETU        PIC X(10).
            03  SKT4710-SEIKYU-TBL      OCCURS  9 TIMES.
                05  SKT4710-NYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT4710-KENSU   PIC X(05).
                    07  SKT4710-NISSU   PIC X(05).
                    07  SKT4710-TENSU   PIC X(09).
                    07  SKT4710-ITBFTN  PIC X(09).
                05  SKT4710-SKENSU      PIC X(05).
                05  SKT4710-SNISSU      PIC X(05).
                05  SKT4710-SKINGK      PIC X(09).
                05  SKT4710-SFTNGK      PIC X(09).
