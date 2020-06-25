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
      * Maj/Min/Rev  修正者    日付      内容
      *  01.00.01    吉川    11/11/10    診療科を５行まで印字できるように修正
      *  01.00.02    萩野    18/12/12    修正履歴コメント削除
      ********************************************************************
        01  SKT2000.
            02  SKT2000-SRYYM   PIC X(16).
            02  SKT2000-SEIKYU-TBL      OCCURS  32 TIMES.
              03  SKT2000-HKNJANAME   PIC X(20).
              03  SKT2000-KENSU   PIC X(5).
            02  SKT2000-SKENSU  PIC X(6).
            02  SKT2000-GKENSU  PIC X(6).
            02  SKT2000-KOUREI-TBL      OCCURS  10 TIMES.
              03  SKT2000-KHKNJANAME  PIC X(20).
              03  SKT2000-KKENSU  PIC X(5).
            02  SKT2000-KSKENSU PIC X(6).
            02  SKT2000-KGKENSU PIC X(6).
            02  SKT2000-TOKUBETU    PIC X(5).
            02  SKT2000-HKNJASU PIC X(3).
            02  SKT2000-TENHYO-TBL      OCCURS  3 TIMES.
              03  SKT2000-TENHYOKBN   PIC X(2).
            02  SKT2000-HOSPCD  PIC X(14).
            02  SKT2000-SRYKA
                    PIC X(20)   OCCURS  5 TIMES.
            02  SKT2000-ADRS    PIC X(100).
            02  SKT2000-HOSPNAME    PIC X(100).
            02  SKT2000-KAISETUNAME PIC X(80).
            02  SKT2000-TYOKI
                    PIC X(7)    OCCURS  32 TIMES.
            02  SKT2000-STYOKI  PIC X(8).
            02  SKT2000-GTYOKI  PIC X(8).
