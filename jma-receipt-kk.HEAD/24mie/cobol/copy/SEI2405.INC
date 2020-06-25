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
      *  01.00.01    «ÎÃÓ      2019/05/23   Ω§¿µÕ˙ŒÚ•≥•·•Û•»∫ÔΩ¸
      ************************************************************
           03  MF100-KEY.
      *∞ÂŒ≈µ°¥ÿ£…£ƒ
               05  MF100-HOSPNUM         PIC  X(02).
      *…È√¥º‘»÷πÊ
               05  MF100-CITYNUM         PIC  X(03).
      *∏¯»Ò∂Ë ¨
               05  MF100-KBN             PIC  X(01).
      *¥µº‘»÷πÊ
               05  MF100-PTNUM           PIC  X(20).
      *ø«Œ≈≤ 
               05  MF100-SRYKA           PIC  X(02).
      *ø«Œ≈«Ø∑Ó
               05  MF100-SRYYM           PIC  X(06).
      *∆˛≥∞∂Ë ¨
               05  MF100-NYUGAIKBN       PIC  X(01).
      *¿¡µ·•«°º•ø•®•Í•¢
           03  MF100-SEIKYU-AREA.
      *∑ÔøÙ•∆°º•÷•Î
               05  MF100-SEIKYU-TBL OCCURS 6.
      *    ∑ÔøÙ
                   07  MF100-KENSU       PIC 9(05).
