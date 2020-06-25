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
        01  SKT4712.
            03  SKT4712-NO                 PIC X(04).
            03  SKT4712-SRYYM              PIC X(16).
            03  SKT4712-SEIYMD             PIC X(22).
            03  SKT4712-HOSPCD             PIC X(14).
            03  SKT4712-HKNJANUM           PIC X(12).
            03  SKT4712-ZAI1               PIC X(02).
            03  SKT4712-ZAI2               PIC X(02).
            03  SKT4712-RECEKA             PIC X(20).
            03  SKT4712-TOKUBETU           PIC X(10).
            03  SKT4712-KOH-TBL            OCCURS  9 TIMES.
                05  SKT4712-HBTNUM         PIC X(02).
                05  SKT4712-HKNBETU-TBL    OCCURS  3 TIMES.
                    07  SKT4712-HKNBETU    PIC X(02).
                05  SKT4712-KOHNYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT4712-K-KENSU    PIC X(05).
                    07  SKT4712-K-NISSU    PIC X(05).
                    07  SKT4712-K-TENSU    PIC X(09).
                    07  SKT4712-K-ITBFTN   PIC X(09).
                05  SKT4712-K-SKENSU       PIC X(05).
                05  SKT4712-K-SNISSU       PIC X(05).
                05  SKT4712-K-SKINGK       PIC X(09).
                05  SKT4712-K-SFTNGK       PIC X(09).
