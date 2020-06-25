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
        01  SC11.
      *∞ÂŒ≈µ°¥ÿ•≥°º•…
            03  SC11-HOSPCD                           PIC X(14).
      *∞ÂŒ≈µ°¥ÿΩªΩÍ
            03  SC11-ADRS                             PIC X(100).
      *∞ÂŒ≈µ°¥ÿÃæ
            03  SC11-HOSPNAME                         PIC X(100).
      *≥´¿ﬂº‘Ãæ
            03  SC11-KAISETU                          PIC X(60).
      *ø«Œ≈«Ø∑Ó
            03  SC11-SRYYM                            PIC X(16).
      *¿¡µ·«Ø∑Ó∆¸
            03  SC11-SEIYMD.
              05  SC11-SEIYY                          PIC X(04).
              05  SC11-SEIMM                          PIC X(04).
              05  SC11-SEIDD                          PIC X(04).
      *¿¡µ·•∆°º•÷•Î
            03  SC11-SEIKYU-TBL                       OCCURS  5 TIMES.
      *ºÁ ›∏±∂Ë ¨
              05  SC11-SHUHKN-TBL                     OCCURS  2 TIMES.
      *∆˛≥∞∂Ë ¨
                07  SC11-NYUGAI-TBL                   OCCURS  2 TIMES.
      *∑ÔøÙ
                  09  SC11-KENSU                      PIC X(05).
      *≈¿øÙ
                  09  SC11-TENSU                      PIC X(09).
      *∑ÎÕΩ≈˘∏¯»Ò…È√¥≈¿øÙ
                  09  SC11-KOHTENSU                   PIC X(07).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                  09  SC11-YKZFTN                     PIC X(07).
      *ºıµÎº‘ªŸ ß≥€
                  09  SC11-SIHARAIGK                  PIC X(07).
      *ø©ªˆŒ≈Õ‹»Ò…∏Ω‡…È√¥≥€
                  09  SC11-SFTNGK                     PIC X(09).
      *∆√ Ã∞ÂŒ≈»Ò¿¡µ·≥€
                  09  SC11-SEIKYUGK                   PIC X(09).
      *πÁ∑◊°º∑ÔøÙ
            03  SC11-GKENSU                         PIC X(05).
      *πÁ∑◊°º∆√ Ã∞ÂŒ≈»Ò¿¡µ·≥€
            03  SC11-GSEIKYUGK                      PIC X(10).
