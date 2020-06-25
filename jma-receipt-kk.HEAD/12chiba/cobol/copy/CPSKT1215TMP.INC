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
      *    ø«Œ≈ ÛΩ∑¿¡µ·ΩÒ     COPY (CPSKT1215TMP.INC)            
      *
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘  ∆¸…’        ∆‚Õ∆
      *  01.00.01    µ»¿Ó    2012/08/08  ø∑µ¨∫Ó¿Æ
      ********************************************************************
       01  SKT1215TMP-REC.
           03    SKT1215TMP-KEY.
               05    SKT1215TMP-HKNJANUM      PIC X(06).
               05    SKT1215TMP-NYUGAIKBN     PIC X(01).
               05    SKT1215TMP-RECEKA        PIC X(02).
               05    SKT1215TMP-ZAITAKU       PIC X(01).
               05    SKT1215TMP-GENMEN        PIC X(05).
               05    SKT1215TMP-SRYYM         PIC X(06).
      *
           03    SKT1215TMP-HKNJANAME         PIC X(20).
           03    SKT1215TMP-SEIKYU-TBL        OCCURS 2.
               05    SKT1215TMP-KENSU         PIC 9(05).
               05    SKT1215TMP-NISSU         PIC 9(05).
               05    SKT1215TMP-TENSU         PIC 9(09).
               05    SKT1215TMP-ITBFTN        PIC 9(09).
               05    SKT1215TMP-S-KENSU       PIC 9(05).
               05    SKT1215TMP-S-NISSU       PIC 9(05).
               05    SKT1215TMP-S-KINGK       PIC 9(09).
               05    SKT1215TMP-S-FTNGK       PIC 9(09).
      *
           03    SKT1215TMP-KOH-TBL           OCCURS 20.
               05  SKT1215TMP-KOHNUM          PIC X(03).
               05  SKT1215TMP-KOHNAME         PIC X(10).
               05    SKT1215TMP-KKENSU        PIC 9(05).
               05    SKT1215TMP-KNISSU        PIC 9(05).
               05    SKT1215TMP-KTENSU        PIC 9(09).
               05    SKT1215TMP-KITBFTN       PIC 9(09).
               05    SKT1215TMP-S-KKENSU      PIC 9(05).
               05    SKT1215TMP-S-KNISSU      PIC 9(05).
               05    SKT1215TMP-S-KKINGK      PIC 9(09).
               05    SKT1215TMP-S-KFTNGK      PIC 9(09).
