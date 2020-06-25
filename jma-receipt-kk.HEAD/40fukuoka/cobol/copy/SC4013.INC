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
       01  SC13.
      *∞ı∫˛∆¸…’
           03  SC13-SRYYM             PIC  X(16).
      *¿¡µ·∆¸…’
           03  SC13-SEIYMD            PIC  X(22).
      *∞ÂŒ≈µ°¥ÿ•≥°º•…
           03  SC13-HOSPCDN           PIC  X(20).
      *∞ÂŒ≈µ°¥ÿΩÍ∫ﬂ√œ
           03  SC13-ADRS              PIC  X(100).
      *∞ÂŒ≈µ°¥ÿÃææŒ
           03  SC13-HOSPNAME          PIC  X(100).
      *∞ÂŒ≈µ°¥ÿ≈≈œ√»÷πÊ
           03  SC13-TEL               PIC  X(15).
      *¥…Õ˝º‘Ãæ
           03  SC13-KAISETU           PIC  X(40).
      *≈¿øÙ…Ω∂Ë ¨
           03  SC13-TENSUHYO          PIC  X(02) OCCURS 4.
      *£∏£∞æ„≥≤º‘°‹£≤£∑œ∑øÕ¿¡µ·•®•Í•¢
           03  SC13-80-AREA.
      *∑ÔøÙ
               05  SC13-80-KENSU      PIC  X(05).
      *∞Ï…Ù…È√¥∂‚≥€
               05  SC13-80-ITBFTN     PIC  X(10).
      *¿¡µ·≥€
               05  SC13-80-SEIKYUGK   PIC  X(10).
      *£¥£±œ∑øÕ¿¡µ·•®•Í•¢
           03  SC13-41-AREA OCCURS 5.
      *∑ÔøÙ
               05  SC13-41-KENSU      PIC  X(05).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SC13-41-YKZFTN     PIC  X(09).
      *∞Ï…Ù…È√¥∂‚
               05  SC13-41-ITBFTN     PIC  X(09).
      *¿¡µ·∂‚≥€
               05  SC13-41-SEIKYUGK   PIC  X(09).
