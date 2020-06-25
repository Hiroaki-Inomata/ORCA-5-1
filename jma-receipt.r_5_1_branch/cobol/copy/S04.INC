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
        02  S04.
            03  S04-FIXED2.
              04  S04-B10-STATE            PIC S9(9)   BINARY.
              04  S04-B12-STATE            PIC S9(9)   BINARY.
              04  S04-LBLKARA              PIC X(2).
              04  S04-LBLSUMKBN            PIC X(30).
              04  S04-LBLSYORIKBN          PIC X(22).
              04  S04-SIHARAICOMBO.
                  05  S04-SIHARAIKBN       PIC X(12).
                  05  S04-SIHARAI-COUNT    PIC S9(9)   BINARY.
                  05  S04-SIHARAI-LIST     PIC X(12)   OCCURS  5.
              04  S04-NYUGAICOMBO.
                  05  S04-NYUGAIKBN        PIC X(6).
                  05  S04-NYUGAI-COUNT     PIC S9(9)   BINARY.
                  05  S04-NYUGAI-LIST      PIC X(6)    OCCURS  5.
              04  S04-SRYCOMBO.
                  05  S04-SRYKBN           PIC X(8).
                  05  S04-SRY-COUNT        PIC S9(9)   BINARY.
                  05  S04-SRY-LIST         PIC X(8)    OCCURS  5.
              04  S04-STRSRYYMD            PIC X(10).
              04  S04-ENDSRYYMD            PIC X(10).
              04  S04-ENDSRYYMD-STATE      PIC S9(9)   BINARY.
              04  S04-KIKANCOMBO.
                  05  S04-KIKANKBN         PIC X(10).
                  05  S04-KIKAN-COUNT      PIC S9(9)   BINARY.
                  05  S04-KIKAN-LIST       PIC X(10)   OCCURS  5.
              04  S04-DSPCOMBO.
                  05  S04-DSPKBN           PIC X(20).
                  05  S04-DSPKBN-STATE     PIC S9(9)   BINARY.
                  05  S04-DSP-COUNT        PIC S9(9)   BINARY.
                  05  S04-DSP-LIST         PIC X(20)   OCCURS  5.
                  05  S04-DSP-LIST-STATE   PIC S9(9)   BINARY.
              04  S04-SUMKBNCOMBO.
                  05  S04-SUMKBN            PIC X(18).
                  05  S04-SUMKBN-STATE      PIC S9(9)   BINARY.
                  05  S04-SUMKBN-COUNT      PIC S9(9)   BINARY.
                  05  S04-SUMKBN-LIST       PIC X(18)   OCCURS 5.
                  05  S04-SUMKBN-LIST-STATE PIC S9(9)   BINARY.
              04  S04-SYORICOMBO.
                  05  S04-SYORIKBN         PIC X(18).
                  05  S04-SYORIKBN-STATE   PIC S9(9)   BINARY.
                  05  S04-SYORI-COUNT      PIC S9(9)   BINARY.
                  05  S04-SYORI-LIST       PIC X(18)   OCCURS  5.
                  05  S04-SYORI-LIST-STATE PIC S9(9)   BINARY.
              04  S04-JIHICOMBO.
                  05  S04-JIHIKBN          PIC X(12).
                  05  S04-JIHIKBN-STATE    PIC S9(9)   BINARY.
                  05  S04-JIHI-COUNT       PIC S9(9)   BINARY.
                  05  S04-JIHI-LIST        PIC X(12)   OCCURS  5.
                  05  S04-JIHI-LIST-STATE  PIC S9(9)   BINARY.
