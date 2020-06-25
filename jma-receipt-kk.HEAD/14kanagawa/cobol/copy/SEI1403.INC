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
      *          ∞ÂŒ≈»Ò¿¡µ·ΩÒ COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *ª‘ƒÆ¬º»÷πÊ
               05  MF100-CITYNUM         PIC  X(04).
      *¿¡µ·•«°º•ø
           03  MF100-SEIKYU-TBL.
      *∑ÔøÙ
               05  MF100-KENSU           PIC  9(05).
      *≈¿øÙ
               05  MF100-TENSU           PIC  9(09).
      *ÃÙ∫ﬁ∑ÔøÙ
               05  MF100-YKZKENSU        PIC  9(05).
      *ÃÙ∫ﬁ∂‚≥€
               05  MF100-YKZFTN          PIC  9(09).
      *∞Ï…Ù…È√¥∂‚∑ÔøÙ
               05  MF100-ITBKENSU        PIC  9(05).
      *∞Ï…Ù…È√¥∂‚
               05  MF100-ITBFTN          PIC  9(09).
      *ø©ªˆ∑ÔøÙ
               05  MF100-SKJKENSU        PIC  9(05).
      *…∏Ω‡…È√¥≥€
               05  MF100-SKJFTN          PIC  9(09).
      *∫∆∑«Õ—•®•Í•¢
               05  MF100-SAIKEI-TBL  OCCURS 16.
                   07  MF100-SAIKEI      PIC  9(05).
