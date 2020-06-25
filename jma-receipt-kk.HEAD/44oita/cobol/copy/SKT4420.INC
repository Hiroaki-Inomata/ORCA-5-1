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
      *  01.00.02    井上         19/02/05  修正履歴コメント削除
      ********************************************************************
        01  SKT4420.
            03  SKT4420-NO                  PIC X(04).
            03  SKT4420-SRYYM               PIC X(16).
            03  SKT4420-SEIYMD              PIC X(22).
            03  SKT4420-HOSPCD              PIC X(14).
            03  SKT4420-ADRS                PIC X(100).
            03  SKT4420-HOSPNAME            PIC X(100).
            03  SKT4420-KAISETU             PIC X(60).
            03  SKT4420-HKNJANAME           PIC X(24).
            03  SKT4420-HKNJANUM            PIC X(06).
            03  SKT4420-ZAI1                PIC X(02).
            03  SKT4420-ZAI2                PIC X(02).
            03  SKT4420-TOKUBETU            PIC X(10).
            03  SKT4420-SEIKYU-TBL      OCCURS  2 TIMES.
                05  SKT4420-NYUGAI-TBL      OCCURS  2 TIMES.
                    07  SKT4420-KENSU       PIC X(05).
                    07  SKT4420-NISSU       PIC X(05).
                    07  SKT4420-TENSU       PIC X(09).
                    07  SKT4420-ITBFTN      PIC X(09).
                05  SKT4420-SKENSU          PIC X(05).
                05  SKT4420-SNISSU          PIC X(05).
                05  SKT4420-SKINGK          PIC X(09).
                05  SKT4420-SFTNGK          PIC X(09).
            03  SKT4420-KSEIKYU-TBL     OCCURS  4 TIMES.
                05  SKT4420-KOHNAME         PIC X(08).
                05  SKT4420-HBTNUM          PIC X(02).
                05  SKT4420-KNYUGAI-TBL     OCCURS  2 TIMES.
                    07  SKT4420-K-KENSU     PIC X(05).
                    07  SKT4420-K-NISSU     PIC X(05).
                    07  SKT4420-K-TENSU     PIC X(09).
                    07  SKT4420-K-ITBFTN    PIC X(09).
                05  SKT4420-K-SKENSU        PIC X(05).
                05  SKT4420-K-SNISSU        PIC X(05).
                05  SKT4420-K-SKINGK        PIC X(09).
                05  SKT4420-K-SFTNGK        PIC X(09).
