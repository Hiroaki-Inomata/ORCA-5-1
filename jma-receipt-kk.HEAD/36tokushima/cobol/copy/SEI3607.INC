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
      * ›∏±∂Ë ¨
               05  MF100-KENTAN          PIC  9(01).
      *ª‘ƒÆ¬º»÷πÊ
               05  MF100-CITYNUM         PIC  X(03).
      *µÎ…’≥‰πÁ
               05  MF100-KYURATE1        PIC  9(01).
               05  MF100-KYURATE2        PIC  9(03).
      *¿¡µ·•®•Í•¢
           03  MF100-SEIKYU-AREA.
      *∆˛≥∞∂Ë ¨
               05  MF100-NYUGAIKBN OCCURS 2.
                   07  MF100-KENSU         PIC  9(04).
                   07  MF100-NISSU         PIC  9(04).
                   07  MF100-TENSU         PIC  9(07).
                   07  MF100-TAHO          PIC  9(07).
