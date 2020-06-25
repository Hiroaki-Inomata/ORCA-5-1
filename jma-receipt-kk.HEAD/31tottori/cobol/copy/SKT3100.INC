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
      *  01.00.01    吉川      2010/12/08   特別療養費国保対応
      ********************************************************************
        01  SKT3100.
            03  SKT3100-ADRS            PIC X(100).
            03  SKT3100-HOSPNAME        PIC X(100).
            03  SKT3100-KAISETU         PIC X(40).
            03  SKT3100-SRYYM           PIC X(16).
            03  SKT3100-SEIYMD          PIC X(22).
            03  SKT3100-SEIKYU-TBL      OCCURS  33 TIMES.
                05  SKT3100-HKNJANAME   PIC X(32).
                05  SKT3100-KENSU       PIC X(05).
            03  SKT3100-GKENSU          PIC X(06).
            03  SKT3100-TOKUBETU        PIC X(10).
