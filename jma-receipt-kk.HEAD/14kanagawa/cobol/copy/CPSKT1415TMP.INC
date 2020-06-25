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
      *    ø«Œ≈ ÛΩ∑¿¡µ·ΩÒ     COPY (CPSKT1415TMP.INC)            
      *
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘  ∆¸…’        ∆‚Õ∆
      *  01.00.01    µ»¿Ó    2011/06/23  ø∑µ¨∫Ó¿Æ
      *  01.00.02    µ»¿Ó    2015/01/30   ø¿Æ£≤£∑«Ø£±∑Ó≤˛¿µ¬–±˛
      ********************************************************************
       01  SKT1415TMP-REC.
           03    SKT1415TMP-KEY.
               05    SKT1415TMP-HKNJANUM      PIC X(06).
               05    SKT1415TMP-ZAITAKU       PIC X(01).
               05    SKT1415TMP-SRYYM         PIC X(06).
               05    SKT1415TMP-GENMEN        PIC X(05).
      *
           03    SKT1415TMP-SEIKYU-TBL        OCCURS 2.
               05    SKT1415TMP-NYUGAI-TBL    OCCURS 2.
                 07    SKT1415TMP-KENSU       PIC 9(05).
                 07    SKT1415TMP-NISSU       PIC 9(05).
                 07    SKT1415TMP-TENSU       PIC 9(09).
                 07    SKT1415TMP-ITBFTN      PIC 9(09).
               05    SKT1415TMP-S-KENSU       PIC 9(05).
               05    SKT1415TMP-S-NISSU       PIC 9(05).
               05    SKT1415TMP-S-KINGK       PIC 9(09).
               05    SKT1415TMP-S-FTNGK       PIC 9(09).
      *
           03  SKT1415TMP-KOH-TBL      OCCURS  25.
               05  SKT1415TMP-K-KENSU         PIC  9(06).
