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
        01  SKT4400.
            03  SKT4400-SRYYM           PIC X(16).
            03  SKT4400-HOSPCD          PIC X(14).
            03  SKT4400-ADRS            PIC X(100).
            03  SKT4400-HOSPNAME        PIC X(100).
            03  SKT4400-KAISETU         PIC X(60).
            03  SKT4400-TEL             PIC X(15).
            03  SKT4400-KENKBN          PIC X(02)    OCCURS  2 TIMES.
            03  SKT4400-TOKUBETU        PIC X(10).
            03  SKT4400-SEIKYU-TBL      OCCURS  2 TIMES.
                05  SKT4400-NYUGAI-TBL      OCCURS  2 TIMES.
                    07  SKT4400-KENSU   PIC X(06).
                    07  SKT4400-TENSU   PIC X(10).
                    07  SKT4400-HEIKIN  PIC X(07).
                05  SKT4400-GKENSU      PIC X(06).
            03  SKT4400-SAI-KENSU       PIC X(06)    OCCURS  2 TIMES.
