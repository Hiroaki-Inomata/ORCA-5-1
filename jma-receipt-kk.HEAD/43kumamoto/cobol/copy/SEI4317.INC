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
      *ø«Œ≈«Ø∑Ó
               05  MF100-SRYYM            PIC  X(06).
      *¿¡µ·•∆°º•÷•Î
           03  MF100-NYUGAI-TBL OCCURS 2.
               05  MF100-SEIKYU-TBL OCCURS 7.
      *∑ÔøÙ
                   07  MF100-KENSU            PIC  9(05).
      *∆¸øÙ
                   07  MF100-NISSU            PIC  9(05).
      *≈¿øÙ
                   07  MF100-TENSU            PIC  9(09).
      *Œ≈Õ‹»Ò¿¡µ·≥€
                   07  MF100-SEIKYUGK         PIC  9(09).
      *∏¯»Ò•∆°º•÷•Î
           03  MF100-KOH-NYUGAI-TBL  OCCURS 2.
               05  MF100-KOH-TBL  OCCURS 20.
      *∏¯»Ò»÷πÊ
                   07  MF100-KOHNUM      PIC  X(03).
      *∑ÔøÙ
                   07  MF100-KKENSU      PIC  9(05).
      *∆¸øÙ 
                   07  MF100-KNISSU      PIC  9(05).
      *≈¿øÙ
                   07  MF100-KTENSU      PIC  9(09).
      *Œ≈Õ‹»Ò¿¡µ·≥€
                   07  MF100-KSEIKYUGK   PIC  9(09).
      *æÆ∑◊
           03  MF100-SYOKEI-TBL.
      *∑ÔøÙ
               05  MF100-S-KENSU        PIC  9(05).
      *∆¸øÙ 
               05  MF100-S-NISSU        PIC  9(05).
      *≈¿øÙ
               05  MF100-S-TENSU        PIC  9(09).
      *Œ≈Õ‹»Ò¿¡µ·≥€
               05  MF100-S-SEIKYUGK     PIC  9(09).
