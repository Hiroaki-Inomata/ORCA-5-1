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
      *  01.00.01    吉川      2010/10/13   特別療養費国保対応
      *  01.00.02    萩野      2018/09/10   修正履歴コメント削除
      ********************************************************************
        01  SKT2200.
            02  SKT2200-PRTYM   PIC X(16).
            02  SKT2200-DATA            OCCURS  8 TIMES.
              03  SKT2200-HKNJANAME   PIC X(20).
              03  SKT2200-TODOHUKEN   PIC X(20).
              03  SKT2200-KENSU-TBL       OCCURS  26 TIMES.
                04  SKT2200-KENSU   PIC X(6).
              03  SKT2200-TENSU-TBL       OCCURS  26 TIMES.
                04  SKT2200-TENSU   PIC X(9).
            02  SKT2200-G-KENSU-TBL     OCCURS  26 TIMES.
              03  SKT2200-G-KENSU PIC X(6).
            02  SKT2200-G-TENSU-TBL     OCCURS  26 TIMES.
              03  SKT2200-G-TENSU PIC X(9).
            02  SKT2200-GKENSU
                    PIC X(9)    OCCURS  2 TIMES.
            02  SKT2200-GTENSU
                    PIC X(10)   OCCURS  2 TIMES.
            02  SKT2200-RYOYOHI
                    PIC X(10)   OCCURS  2 TIMES.
            02  SKT2200-HOSPCDN PIC X(40).
            02  SKT2200-ADRS    PIC X(100).
            02  SKT2200-HOSPNAME    PIC X(100).
            02  SKT2200-TENHYOKBN-TBL           OCCURS  5 TIMES.
              03  SKT2200-TENHYOKBN   PIC X(2).
            02  SKT2200-TOKUBETU            PIC  X(10).
