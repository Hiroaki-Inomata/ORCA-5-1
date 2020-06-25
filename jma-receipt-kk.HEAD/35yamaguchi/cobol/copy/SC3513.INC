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
        01  SC13.
      *ø«Œ≈«Ø∑Ó
            03  SC13-PRTYY                  PIC X(02).
            03  SC13-PRTMM                  PIC X(02).
      *∏©»÷πÊ
            03  SC13-PREFNUM.
                05  SC13-PREFNUM1           PIC X(01).
                05  SC13-PREFNUM2           PIC X(01).
      *∞ÂŒ≈µ°¥ÿ•≥°º•…
            03  SC13-HOSPCD.
                05  SC13-HOSPCD1            PIC X(01).
                05  SC13-HOSPCD2            PIC X(01).
                05  SC13-HOSPCD3            PIC X(01).
                05  SC13-HOSPCD4            PIC X(01).
                05  SC13-HOSPCD5            PIC X(01).
                05  SC13-HOSPCD6            PIC X(01).
                05  SC13-HOSPCD7            PIC X(01).
      *∞ÂŒ≈µ°¥ÿΩªΩÍ
            03  SC13-ADRS                   PIC X(100).
      *∞ÂŒ≈µ°¥ÿÃææŒ
            03  SC13-HOSPNAME               PIC X(100).
      *≥´¿ﬂº‘Ãæ
            03  SC13-KAISETU                PIC X(40).
      *œ∑øÕ°¶∞Ï»Ã∂Ë ¨
            03  SC13-KBN                    PIC X(01).
      *…È√¥º‘»÷πÊ
            03  SC13-FTNJANUM-TBL        OCCURS  5 TIMES.
                05  SC13-FTNJANUM.
                    07  SC13-FTNJANUM1      PIC X(01).
                    07  SC13-FTNJANUM2      PIC X(01).
                    07  SC13-FTNJANUM3      PIC X(01).
                    07  SC13-FTNJANUM4      PIC X(01).
                    07  SC13-FTNJANUM5      PIC X(01).
                    07  SC13-FTNJANUM6      PIC X(01).
                    07  SC13-FTNJANUM7      PIC X(01).
                    07  SC13-FTNJANUM8      PIC X(01).
      *∑ÔøÙ
            03  SC13-KENSU-TBL           OCCURS  5 TIMES.
                05  SC13-KENSU.
                    07  SC13-KENSU1         PIC X(01).
                    07  SC13-KENSU2         PIC X(01).
                    07  SC13-KENSU3         PIC X(01).
                    07  SC13-KENSU4         PIC X(01).
      *≈¿øÙ
            03  SC13-TENSU-TBL           OCCURS  5 TIMES.
                05  SC13-TENSU.
                    07  SC13-TENSU1         PIC X(01).
                    07  SC13-TENSU2         PIC X(01).
                    07  SC13-TENSU3         PIC X(01).
                    07  SC13-TENSU4         PIC X(01).
                    07  SC13-TENSU5         PIC X(01).
                    07  SC13-TENSU6         PIC X(01).
                    07  SC13-TENSU7         PIC X(01).
                    07  SC13-TENSU8         PIC X(01).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
            03  SC13-YKZFTN-TBL          OCCURS  5 TIMES.
                05  SC13-YKZFTN.
                    07  SC13-YKZFTN1        PIC X(01).
                    07  SC13-YKZFTN2        PIC X(01).
                    07  SC13-YKZFTN3        PIC X(01).
                    07  SC13-YKZFTN4        PIC X(01).
                    07  SC13-YKZFTN5        PIC X(01).
                    07  SC13-YKZFTN6        PIC X(01).
                    07  SC13-YKZFTN7        PIC X(01).
                    07  SC13-YKZFTN8        PIC X(01).
      *∑Î≥À°¶¿∫ø¿≈¿øÙ
            03  SC13-KESETENSU-TBL       OCCURS  5 TIMES.
                05  SC13-KESETENSU.
                    07  SC13-KESETENSU1     PIC X(01).
                    07  SC13-KESETENSU2     PIC X(01).
                    07  SC13-KESETENSU3     PIC X(01).
                    07  SC13-KESETENSU4     PIC X(01).
                    07  SC13-KESETENSU5     PIC X(01).
                    07  SC13-KESETENSU6     PIC X(01).
                    07  SC13-KESETENSU7     PIC X(01).
      *∑Î≥À°¶¿∫ø¿ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
            03  SC13-KESEYKZFTN-TBL      OCCURS  5 TIMES.
                05  SC13-KESEYKZFTN.
                    07  SC13-KESEYKZFTN1    PIC X(01).
                    07  SC13-KESEYKZFTN2    PIC X(01).
                    07  SC13-KESEYKZFTN3    PIC X(01).
                    07  SC13-KESEYKZFTN4    PIC X(01).
                    07  SC13-KESEYKZFTN5    PIC X(01).
                    07  SC13-KESEYKZFTN6    PIC X(01).
                    07  SC13-KESEYKZFTN7    PIC X(01).
      *Ωı¿Æ≥€
            03  SC13-JOSEIGK-TBL         OCCURS  5 TIMES.
                05  SC13-JOSEIGK.
                    07  SC13-JOSEIGK1       PIC X(01).
                    07  SC13-JOSEIGK2       PIC X(01).
                    07  SC13-JOSEIGK3       PIC X(01).
                    07  SC13-JOSEIGK4       PIC X(01).
                    07  SC13-JOSEIGK5       PIC X(01).
                    07  SC13-JOSEIGK6       PIC X(01).
                    07  SC13-JOSEIGK7       PIC X(01).
