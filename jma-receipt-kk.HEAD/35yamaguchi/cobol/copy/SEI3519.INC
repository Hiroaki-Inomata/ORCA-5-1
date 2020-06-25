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
      *01  MF102-REC.
           03  MF102-KEY.
      *∏¯»Ò°›…È√¥º‘»÷πÊ
               05  MF102-FTNJANUM         PIC  X(08).
      *¥µº‘»÷πÊ
               05  MF102-PTNUM            PIC  X(20).
      *∆˛≥∞∂Ë ¨
               05  MF102-NYUGAIKBN        PIC  X(02).
      *ø«Œ≈«Ø∑Ó
               05  MF102-SRYYM            PIC  X(06).
      * ›∏±º‘»÷πÊ
               05  MF102-HKNJANUM         PIC  X(08).
      *ºıµÎº‘»÷πÊ
               05  MF102-JKYSNUM          PIC  X(20).
      *µ≠πÊ°¶»÷πÊ
           03  MF102-KIGO                 PIC  X(38).
           03  MF102-NUM                  PIC  X(38).
      *¿≠ Ã
           03  MF102-SEX                  PIC  X(01).
      *Ãæ¡∞
           03  MF102-NAME                 PIC  X(50).
      *¿∏«Ø∑Ó∆¸
           03  MF102-SEIGYMD.
               05  MF102-SEIG             PIC  X(01).
               05  MF102-SEIY             PIC  X(02).
               05  MF102-SEIM             PIC  X(02).
               05  MF102-SEID             PIC  X(02).
      * ›∏±»÷πÊ
           03  MF102-HKNNUM               PIC  X(03).
      *¬‡ø¶°¶«§∑—
           03  MF102-NINKEI               PIC  X(01).
      *µÎ…’≥‰πÁ
           03  MF102-KYURATE              PIC  X(01).
      *∆√ºÏ(ƒπ¥¸)
           03  MF102-TYOKI                PIC  X(01).
      *∆˛±°«Ø∑Ó∆¸
           03  MF102-NYUINGYMD.
               05  MF102-NYUING           PIC  X(01).
               05  MF102-NYUINY           PIC  X(02).
               05  MF102-NYUINM           PIC  X(02).
               05  MF102-NYUIND           PIC  X(02).
      *ø«Œ≈≤ 
           03  MF102-SRYKA                PIC  X(02).
      *∆¸øÙ
           03  MF102-NISSU                PIC  9(02).
      * °ª„∞ÂŒ≈»Ò¿¡µ·≈¿øÙ
           03  MF102-TENSU                PIC  9(08).
      *∏¯»Ò¬–æ›≈¿øÙ
           03  MF102-KESETENSU            PIC  9(08).
      *ø©ªˆ∆¸øÙ
           03  MF102-SHOKUJINISSU         PIC  9(02).
      *ø©ªˆŒ≈Õ‹»Ò¡Ì»ÒÕ—≥€
           03  MF102-SHOKUJIRYOYOHI       PIC  9(07).
      *ø©ªˆŒ≈Õ‹»Ò…∏Ω‡…È√¥≥€
           03  MF102-SHOKUJIFTN           PIC  9(06).
      * °ª„∞ÂŒ≈»Ò¿¡µ·≥€
           03  MF102-YKZFTN               PIC  9(08).
      *∞Ï…Ù…È√¥∂‚
           03  MF102-KESEYKZFTN           PIC  9(07).
      * °ª„∞ÂŒ≈»Òø©ªˆΩı¿Æ≥€
           03  MF102-JOSEIGK              PIC  9(07).
      * ›∏±∂Ë ¨
           03  MF102-HKNKBN               PIC  X(01).
      *∏¯»Ò»÷πÊ
           03  MF102-TAHONUM              PIC  X(03).
      *∏¯»Ò£…£ƒ
           03  MF102-TAHOKOHID            PIC  9(10).
      *∏¯»Ò…È√¥º‘»÷πÊ
           03  MF102-TAHOFTNJANUM         PIC  X(08).
