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
      ************************************************************
      *    ø«Œ≈ ÛΩ∑¿¡µ·ΩÒ     COPY (CPSKT0110TMP.INC)            *
      *
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
       01  SKT0410TMP-REC.
           03    SKT0410TMP-KEY.
               05    SKT0410TMP-SYUTURYOKU    PIC 9(01).
               05    SKT0410TMP-HKNJANUM      PIC X(06).
               05    SKT0410TMP-ZAITAKU       PIC X(01).
      *    ¥‰ºÍµ‹æÎ∆‚Œ¶√œøÃ∏∫Ã»
               05    SKT0410TMP-GENMEN        PIC X(05).
      *
           03    SKT0410TMP-WARI              PIC 9(02).
           03    SKT0410TMP-SEIKYU-TBL        OCCURS 13.
               05    SKT0410TMP-NYUGAI-TBL    OCCURS 2.
                 07    SKT0410TMP-KENSU       PIC 9(05).
                 07    SKT0410TMP-NISSU       PIC 9(05).
                 07    SKT0410TMP-TENSU       PIC 9(09).
                 07    SKT0410TMP-YKZFTN      PIC 9(07).
                 07    SKT0410TMP-ITBFTN      PIC 9(07).
               05    SKT0410TMP-S-KENSU        PIC 9(05).
               05    SKT0410TMP-S-NISSU        PIC 9(05).
               05    SKT0410TMP-S-KINGK        PIC 9(09).
               05    SKT0410TMP-S-FTNGK        PIC 9(09).
      *
           03    SKT0410TMP-KSEIKYU-TBL        OCCURS 9.
               05    SKT0410TMP-KNYUGAI-TBL    OCCURS 2.
                 07    SKT0410TMP-KKENSU       PIC 9(05).
                 07    SKT0410TMP-KNISSU       PIC 9(05).
                 07    SKT0410TMP-KTENSU       PIC 9(09).
                 07    SKT0410TMP-KYKZFTN      PIC 9(07).
                 07    SKT0410TMP-KITBFTN      PIC 9(07).
               05    SKT0410TMP-S-KKENSU        PIC 9(05).
               05    SKT0410TMP-S-KNISSU        PIC 9(05).
               05    SKT0410TMP-S-KKINGK        PIC 9(09).
               05    SKT0410TMP-S-KFTNGK        PIC 9(09).
