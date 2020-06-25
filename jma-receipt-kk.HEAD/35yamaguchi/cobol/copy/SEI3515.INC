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
      *01  MF101-REC.
           03  MF101-KEY.
      *∏¯»Ò°›…È√¥º‘»÷πÊ
               05  MF101-FTNJANUM         PIC  X(08).
      *¥µº‘»÷πÊ
               05  MF101-PTNUM            PIC  X(20).
      *∆˛≥∞∂Ë ¨
               05  MF101-NYUGAIKBN        PIC  X(02).
      *ø«Œ≈«Ø∑Ó
               05  MF101-SRYYM            PIC  X(06).
      * ›∏±º‘»÷πÊ
               05  MF101-HKNJANUM         PIC  X(08).
      *ºıµÎº‘»÷πÊ
               05  MF101-JKYSNUM          PIC  X(20).
      *µ≠πÊ°¶»÷πÊ
           03  MF101-KIGO                 PIC  X(06).
           03  MF101-NUM                  PIC  X(10).
      *¿≠ Ã
           03  MF101-SEX                  PIC  X(01).
      *Ãæ¡∞
           03  MF101-NAME                 PIC  X(50).
      *¿∏«Ø∑Ó∆¸
           03  MF101-SEIGYMD.
               05  MF101-SEIG             PIC  X(01).
               05  MF101-SEIY             PIC  X(02).
               05  MF101-SEIM             PIC  X(02).
               05  MF101-SEID             PIC  X(02).
      * ›∏±»÷πÊ
           03  MF101-HKNNUM               PIC  X(03).
      *¬‡ø¶°¶«§∑—
           03  MF101-NINKEI               PIC  X(01).
      *µÎ…’≥‰πÁ
           03  MF101-KYURATE              PIC  X(01).
      *∆√ºÏ(ƒπ¥¸)
           03  MF101-TYOKI                PIC  X(01).
      *∆˛±°«Ø∑Ó∆¸
           03  MF101-NYUINGYMD.
               05  MF101-NYUING           PIC  X(01).
               05  MF101-NYUINY           PIC  X(02).
               05  MF101-NYUINM           PIC  X(02).
               05  MF101-NYUIND           PIC  X(02).
      *ø«Œ≈≤ 
           03  MF101-SRYKA                PIC  X(02).
      *∆¸øÙ
           03  MF101-NISSU                PIC  9(02).
      * °ª„∞ÂŒ≈»Ò¿¡µ·≈¿øÙ(∂‚≥€)
           03  MF101-TENSU                PIC  9(08).
      *∑Î≥À¿∫ø¿≈¿øÙ
           03  MF101-KESETENSU            PIC  9(08).
      *ø©ªˆ∆¸øÙ
           03  MF101-SHOKUJINISSU         PIC  9(02).
      *ø©ªˆŒ≈Õ‹»Ò¡Ì»ÒÕ—≥€
           03  MF101-SHOKUJIRYOYOHI       PIC  9(07).
      *ø©ªˆŒ≈Õ‹»Ò…∏Ω‡…È√¥≥€
           03  MF101-SHOKUJIFTN           PIC  9(06).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  MF101-YKZFTN               PIC  9(08).
      *∑Î≥À¿∫ø¿ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  MF101-KESEYKZFTN           PIC  9(07).
      * °ª„∞ÂŒ≈»Òø©ªˆΩı¿Æ≥€
           03  MF101-JOSEIGK              PIC  9(07).
      * ›∏±∂Ë ¨
           03  MF101-HKNKBN               PIC  X(01).
