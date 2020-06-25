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
        01  SKT4521.
            03  SKT4521-NO                  PIC X(04).
            03  SKT4521-SRYYM               PIC X(16).
            03  SKT4521-SEIYMD              PIC X(22).
            03  SKT4521-ADRS                PIC X(100).
            03  SKT4521-HOSPNAME            PIC X(100).
            03  SKT4521-TEL                 PIC X(15).
            03  SKT4521-KAISETU             PIC X(80).
            03  SKT4521-HKNJANUM            PIC X(12).
            03  SKT4521-HOSPCD              PIC X(14).
            03  SKT4521-HKNJANAME           PIC X(40).
            03  SKT4521-ZAI1                PIC X(02).
            03  SKT4521-ZAI2                PIC X(02).
            03  SKT4521-SEIKYU-TBL          OCCURS  2 TIMES.
                05  SKT4521-NYUGAI-TBL1     OCCURS  2 TIMES.
                    07  SKT4521-KENSU       PIC X(05).
                    07  SKT4521-NISSU       PIC X(05).
                    07  SKT4521-TENSU       PIC X(09).
                    07  SKT4521-ITBFTN      PIC X(09).
                05  SKT4521-SKENSU          PIC X(05).
                05  SKT4521-SNISSU          PIC X(05).
                05  SKT4521-SKINGK          PIC X(09).
                05  SKT4521-SFTNGK          PIC X(09).
            03  SKT4521-KOH-TBL             OCCURS  5 TIMES.
                05  SKT4521-HBTNUM          PIC X(02).
                05  SKT4521-FTNRATE         PIC X(01).
                05  SKT4521-NYUGAI-TBL2     OCCURS  2 TIMES.
                    07  SKT4521-K-KENSU     PIC X(05).
                    07  SKT4521-K-NISSU     PIC X(05).
                    07  SKT4521-K-TENSU     PIC X(09).
                    07  SKT4521-K-ITBFTN    PIC X(09).
                05  SKT4521-K-SKENSU        PIC X(05).
                05  SKT4521-K-SNISSU        PIC X(05).
                05  SKT4521-K-SKINGK        PIC X(09).
                05  SKT4521-K-SFTNGK        PIC X(09).
