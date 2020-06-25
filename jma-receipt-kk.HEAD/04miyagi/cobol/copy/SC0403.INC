      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
        01  SC03.
      *     ø«Œ≈«Ø∑Ó
            03  SC03-SRYYM          PIC X(16).
      *     ∞ÂŒ≈µ°¥ÿ•≥°º•…
            03  SC03-HOSPCD         PIC X(14).
      *     ∞ÂŒ≈µ°¥ÿΩÍ∫ﬂ√œ
            03  SC03-ADRS           PIC X(100).
      *     ∞ÂŒ≈µ°¥ÿÃææŒ
            03  SC03-HOSPNAME       PIC X(100).
      *     ∞ÂŒ≈µ°¥ÿ≈≈œ√»÷πÊ
            03  SC03-TEL            PIC X(15).
      *     ∞ÂŒ≈µ°¥ÿ≥´¿ﬂº‘ª·Ãæ
            03  SC03-KAISETU        PIC X(40).
      *
            03  SC03-SEIKYU-TBL         OCCURS  3 TIMES.
      *         Œ≈Õ‹§ŒµÎ…’°›¡Ì∑ÔøÙ
                05  SC03-KENSU      PIC X(05).
      *         Œ≈Õ‹§ŒµÎ…’°›¡Ì≈¿øÙ
                05  SC03-TENSU      PIC X(09).
      *         Œ≈Õ‹§ŒµÎ…’°›¡Ì∆˝Õƒª˘∞ÂŒ≈»Ò¿¡µ·≥€
                05  SC03-SEIKYUGK   PIC X(09).
      *         ø©ªˆŒ≈Õ‹»Ò°›¡Ì∑ÔøÙ
                05  SC03-SKENSU     PIC X(05).
      *         ø©ªˆŒ≈Õ‹»Ò°›¡Ì ›∏±¿¡µ·≥€
                05  SC03-SKINGK     PIC X(09).
      *         ø©ªˆŒ≈Õ‹»Ò°›¡Ì…∏Ω‡…È√¥≥€
                05  SC03-SFTNGK     PIC X(09).
