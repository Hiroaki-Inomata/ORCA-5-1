      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *       °ª„∞ÂŒ≈»Ò¿¡µ·ΩÒ                 COPY (SEI0603.INC) *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01    «ÎÃÓ         15/10/08   ÷Ã·Õ—•≠°ºπ‡Ã‹§Úƒ…≤√
      *  01.00.02    ∞ÊæÂ         18/10/12  Ω§¿µÕ˙ŒÚ•≥•·•Û•»∫ÔΩ¸
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *ª‘ƒÆ¬º»÷πÊ
               05  MF100-CITYNUM         PIC  X(04).
               05  MF100-HENREIKBN       PIC  X(01).
      *•«°º•ø
           03  MF100-SEIKYU81-TBL       OCCURS  7 TIMES.
      *£∏£±°ß∑ÔøÙ
               05  MF100-KENSU81       PIC 9(5).
      *£∏£±°ß≈¿øÙ
               05  MF100-TENSU81       PIC 9(9).
           03  MF100-SEIKYU82-TBL       OCCURS  7 TIMES.
      *£∏£≤°ß∑ÔøÙ
               05  MF100-KENSU82       PIC 9(5).
      *£∏£≤°ß≈¿øÙ
               05  MF100-TENSU82       PIC 9(9).
      *£∏£≤°ß∞Ï…Ù…È√¥∂‚
               05  MF100-ITBFTN82      PIC 9(9).
           03  MF100-SEIKYU83-TBL       OCCURS  2 TIMES.
      *£∏£≥°ß∑ÔøÙ
               05  MF100-KENSU83       PIC 9(5).
      *£∏£≥°ß≈¿øÙ
               05  MF100-TENSU83       PIC 9(9).
           03  MF100-SEIKYU84-TBL       OCCURS  2 TIMES.
      *£∏£¥°ß∑ÔøÙ
               05  MF100-KENSU84       PIC 9(5).
      *£∏£¥°ß≈¿øÙ
               05  MF100-TENSU84       PIC 9(9).
      *£∏£¥°ß∞Ï…Ù…È√¥∂‚
               05  MF100-ITBFTN84      PIC 9(9).
           03  MF100-SEIKYU85-TBL       OCCURS  7 TIMES.
      *£∏£µ°ß∑ÔøÙ
               05  MF100-KENSU85       PIC 9(5).
      *£∏£µ°ß≈¿øÙ
               05  MF100-TENSU85       PIC 9(9).
