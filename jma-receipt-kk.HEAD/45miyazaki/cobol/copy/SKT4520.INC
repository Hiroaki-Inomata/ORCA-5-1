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
        01  SKT4520.
            03  SKT4520-NO              PIC X(04).
            03  SKT4520-SRYYM           PIC X(16).
            03  SKT4520-SEIYMD          PIC X(22).
            03  SKT4520-ADRS            PIC X(100).
            03  SKT4520-HOSPNAME        PIC X(100).
            03  SKT4520-TEL             PIC X(15).
            03  SKT4520-KAISETU         PIC X(80).
            03  SKT4520-HKNJANUM        PIC X(12).
            03  SKT4520-HOSPCD          PIC X(14).
            03  SKT4520-HKNJANAME       PIC X(40).
            03  SKT4520-KYURATE         PIC X(02).
            03  SKT4520-ZAI1            PIC X(02).
            03  SKT4520-ZAI2            PIC X(02).
            03  SKT4520-SEIKYU-TBL      OCCURS  9 TIMES.
                05  SKT4520-NYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT4520-KENSU   PIC X(05).
                    07  SKT4520-NISSU   PIC X(05).
                    07  SKT4520-TENSU   PIC X(09).
                    07  SKT4520-ITBFTN  PIC X(09).
                05  SKT4520-SKENSU      PIC X(05).
                05  SKT4520-SNISSU      PIC X(05).
                05  SKT4520-SKINGK      PIC X(09).
                05  SKT4520-SFTNGK      PIC X(09).
