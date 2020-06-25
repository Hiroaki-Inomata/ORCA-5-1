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
      *  01.00.01    吉川         11/02/10  特別療養費国保対応
      *  01.00.02    門間         19/02/01  修正履歴コメント削除
      ********************************************************************
        01  SKT4511.
            03  SKT4511-NO                  PIC X(04).
            03  SKT4511-SRYYM               PIC X(16).
            03  SKT4511-SEIYMD              PIC X(22).
            03  SKT4511-ADRS                PIC X(100).
            03  SKT4511-HOSPNAME            PIC X(100).
            03  SKT4511-TEL                 PIC X(15).
            03  SKT4511-KAISETU             PIC X(80).
            03  SKT4511-HKNJANUM            PIC X(12).
            03  SKT4511-HOSPCD              PIC X(14).
            03  SKT4511-HKNJANAME           PIC X(40).
            03  SKT4511-ZAI1                PIC X(02).
            03  SKT4511-ZAI2                PIC X(02).
            03  SKT4511-PREFKBN             PIC X(12).
            03  SKT4511-TOKUBETU            PIC X(10).
            03  SKT4511-SEIKYU-TBL          OCCURS  2 TIMES.
                05  SKT4511-NYUGAI-TBL1     OCCURS  2 TIMES.
                    07  SKT4511-KENSU       PIC X(05).
                    07  SKT4511-NISSU       PIC X(05).
                    07  SKT4511-TENSU       PIC X(09).
                    07  SKT4511-ITBFTN      PIC X(09).
                05  SKT4511-SKENSU          PIC X(05).
                05  SKT4511-SNISSU          PIC X(05).
                05  SKT4511-SKINGK          PIC X(09).
                05  SKT4511-SFTNGK          PIC X(09).
            03  SKT4511-KOH-TBL             OCCURS  7 TIMES.
                05  SKT4511-HBTNUM          PIC X(02).
                05  SKT4511-FTNRATE         PIC X(01).
                05  SKT4511-NYUGAI-TBL2     OCCURS  2 TIMES.
                    07  SKT4511-K-KENSU     PIC X(05).
                    07  SKT4511-K-NISSU     PIC X(05).
                    07  SKT4511-K-TENSU     PIC X(09).
                    07  SKT4511-K-ITBFTN    PIC X(09).
                05  SKT4511-K-SKENSU        PIC X(05).
                05  SKT4511-K-SNISSU        PIC X(05).
                05  SKT4511-K-SKINGK        PIC X(09).
                05  SKT4511-K-SFTNGK        PIC X(09).
