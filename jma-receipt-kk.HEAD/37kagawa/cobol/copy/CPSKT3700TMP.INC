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
      *    ø«Œ≈ ÛΩ∑¿¡µ·ΩÒ     COPY (CPSKT3700TMP.INC)              *
      *
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01    µ»¿Ó         10/09/24  ºÁ≤ ¬–±˛
      *  01.00.02    «ÎÃÓ         19/01/11  Ω§¿µÕ˙ŒÚ•≥•·•Û•»∫ÔΩ¸
      ************************************************************
       01  SKT3700TMP-REC.
           03  SKT3700TMP-KEY.
      *∏©∆‚°¶∏©≥∞
               05  SKT3700TMP-PREFKBN           PIC  X(01).
      *ø«Œ≈≤ 
               05  SKT3700TMP-RECEKA            PIC  X(02).
      *
      *¿¡µ·•∆°º•÷•Î
           03  SKT3700TMP-HKN-AREA  OCCURS  11.
               05  SKT3700TMP-NYUGAI-TBL  OCCURS  2.
      *∑ÔøÙ
                   07  SKT3700TMP-KENSU        PIC 9(05).
      *≈¿øÙ
                   07  SKT3700TMP-TENSU        PIC 9(09).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥…È√¥∂‚
                   07  SKT3700TMP-YKZFTN       PIC 9(09).
      *∞Ï…Ù…È√¥∂‚
                   07  SKT3700TMP-ITBFTN       PIC 9(09).
      *
      *∑Î≥À°¶¿∫ø¿
           03  SKT3700TMP-KZN-AREA  OCCURS  2.
               05  SKT3700TMP-KZN-NYUGAI  OCCURS  2.
      *∑Î≥À°¶¿∫ø¿°ø∑ÔøÙ
                   07  SKT3700TMP-Z-KENSU      PIC 9(05).
      *∑Î≥À°¶¿∫ø¿°ø≈¿øÙ
                   07  SKT3700TMP-Z-TENSU      PIC 9(09).
      *∑Î≥À°¶¿∫ø¿°øÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                   07  SKT3700TMP-Z-YKZFTN     PIC 9(09).
      *∑Î≥À°¶¿∫ø¿°ø∞Ï…Ù…È√¥∂‚
                   07  SKT3700TMP-Z-ITBFTN     PIC 9(09).
      *
      *∏¯»Ò•∆°º•÷•Î
           03  SKT3700TMP-KOH-AREA  OCCURS  20.
      *∏¯»Ò»÷πÊ
               05  SKT3700TMP-KOHNUM           PIC X(03).
      *∏¯»ÒÃææŒ
               05  SKT3700TMP-KOHNAME          PIC X(10).
               05  SKT3700TMP-K-NYUGAI-TBL  OCCURS  2.
      *∏¯»Ò°ø∑ÔøÙ
                   07  SKT3700TMP-K-KENSU      PIC 9(05).
      *∏¯»Ò°ø≈¿øÙ
                   07  SKT3700TMP-K-TENSU      PIC 9(09).
      *∏¯»Ò°øÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                   07  SKT3700TMP-K-YKZFTN     PIC 9(09).
      *∏¯»Ò°ø∞Ï…Ù…È√¥∂‚
                   07  SKT3700TMP-K-ITBFTN     PIC 9(09).
