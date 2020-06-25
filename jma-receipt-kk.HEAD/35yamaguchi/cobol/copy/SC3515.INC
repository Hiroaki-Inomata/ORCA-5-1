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
        01  SC15.
      *∞ı∫˛«Ø∑Ó∆¸
            03  SC15-PRTYY                     PIC X(02).
            03  SC15-PRTMM                     PIC X(02).
            03  SC15-PRTDD                     PIC X(02).
      *ª‘ƒÆ¬ºÃæ
            03  SC15-CITYNAME                  PIC X(16).
      *…È√¥º‘»÷πÊ
            03  SC15-FTNJANUM.
                05  SC15-FTNJANUM1             PIC X(01).
                05  SC15-FTNJANUM2             PIC X(01).
                05  SC15-FTNJANUM3             PIC X(01).
                05  SC15-FTNJANUM4             PIC X(01).
                05  SC15-FTNJANUM5             PIC X(01).
                05  SC15-FTNJANUM6             PIC X(01).
                05  SC15-FTNJANUM7             PIC X(01).
                05  SC15-FTNJANUM8             PIC X(01).
      *∞ÂŒ≈µ°¥ÿ•≥°º•…
            03  SC15-HOSPCD.
                05  SC15-HOSPCD1               PIC X(01).
                05  SC15-HOSPCD2               PIC X(01).
                05  SC15-HOSPCD3               PIC X(01).
                05  SC15-HOSPCD4               PIC X(01).
                05  SC15-HOSPCD5               PIC X(01).
                05  SC15-HOSPCD6               PIC X(01).
                05  SC15-HOSPCD7               PIC X(01).
      *∞ÂŒ≈µ°¥ÿΩªΩÍ
            03  SC15-ADRS                      PIC X(100).
      *∞ÂŒ≈µ°¥ÿÃææŒ
            03  SC15-HOSPNAME                  PIC X(100).
      *≥´¿ﬂº‘Ãæ
            03  SC15-KAISETU                   PIC X(40).
      *ø«Œ≈«Ø∑Ó
            03  SC15-SRYYM-TBL             OCCURS  3 TIMES.
                05  SC15-SRYYM.
                    07  SC15-SRYYM1            PIC X(01).
                    07  SC15-SRYYM2            PIC X(01).
                    07  SC15-SRYYM3            PIC X(01).
                    07  SC15-SRYYM4            PIC X(01).
      *∆˛≥∞∂Ë ¨
            03  SC15-NYUGAIKBN-TBL         OCCURS  3 TIMES.
                05  SC15-NYUGAIKBN             PIC X(01).
      *ºıµÎº‘»÷πÊ
            03  SC15-KIGO-TBL              OCCURS  3 TIMES.
                05  SC15-KIGO.
                    07  SC15-KIGO1             PIC X(01).
                    07  SC15-KIGO2             PIC X(01).
                    07  SC15-KIGO3             PIC X(01).
            03  SC15-BANGO-TBL             OCCURS  3 TIMES.
                05  SC15-BANGO.
                    07  SC15-BANGO1            PIC X(01).
                    07  SC15-BANGO2            PIC X(01).
                    07  SC15-BANGO3            PIC X(01).
                    07  SC15-BANGO4            PIC X(01).
                    07  SC15-BANGO5            PIC X(01).
      *¿≠ Ã
            03  SC15-SEX-TBL               OCCURS  3 TIMES.
                05  SC15-SEX                   PIC X(01).
      *ª·Ãæ
            03  SC15-NAME-TBL              OCCURS  3 TIMES.
                05  SC15-NAME                  PIC X(30).
      *¿∏«Ø∑Ó∆¸
            03  SC15-TANJOGYMD-TBL         OCCURS  3 TIMES.
                05  SC15-TANJOGYMD.
                    07  SC15-TANJOGYMD1        PIC X(01).
                    07  SC15-TANJOGYMD2        PIC X(01).
                    07  SC15-TANJOGYMD3        PIC X(01).
                    07  SC15-TANJOGYMD4        PIC X(01).
                    07  SC15-TANJOGYMD5        PIC X(01).
                    07  SC15-TANJOGYMD6        PIC X(01).
                    07  SC15-TANJOGYMD7        PIC X(01).
      * ›∏±º‘»÷πÊ
            03  SC15-HKNJANUM-TBL          OCCURS  3 TIMES.
                05  SC15-HKNJANUM.
                    07  SC15-HKNJANUM1         PIC X(01).
                    07  SC15-HKNJANUM2         PIC X(01).
                    07  SC15-HKNJANUM3         PIC X(01).
                    07  SC15-HKNJANUM4         PIC X(01).
                    07  SC15-HKNJANUM5         PIC X(01).
                    07  SC15-HKNJANUM6         PIC X(01).
                    07  SC15-HKNJANUM7         PIC X(01).
                    07  SC15-HKNJANUM8         PIC X(01).
      *¬‡ø¶°¶«§∑—
            03  SC15-TAININ-TBL            OCCURS  3 TIMES.
                05  SC15-TAININ                PIC X(01).
      *µÎ…’≥‰πÁ
            03  SC15-KYURATE-TBL           OCCURS  3 TIMES.
                05  SC15-KYURATE               PIC X(01).
      *∆√ºÏ
            03  SC15-TYOKI-TBL             OCCURS  3 TIMES.
                05  SC15-TYOKI                 PIC X(01).
      *∆˛±°«Ø∑Ó∆¸
            03  SC15-NYUINYMD-TBL          OCCURS  3 TIMES.
                05  SC15-NYUINGYMD.
                    07  SC15-NYUINGYMD1        PIC X(01).
                    07  SC15-NYUINGYMD2        PIC X(01).
                    07  SC15-NYUINGYMD3        PIC X(01).
                    07  SC15-NYUINGYMD4        PIC X(01).
                    07  SC15-NYUINGYMD5        PIC X(01).
                    07  SC15-NYUINGYMD6        PIC X(01).
                    07  SC15-NYUINGYMD7        PIC X(01).
      *ø«Œ≈≤ 
            03  SC15-SRKA-TBL              OCCURS  3 TIMES.
                05  SC15-SRYKA.
                    07  SC15-SRYKA1            PIC X(01).
                    07  SC15-SRYKA2            PIC X(01).
      *∆¸øÙ
            03  SC15-NISSU-TBL             OCCURS  3 TIMES.
                05  SC15-NISSU.
                    07  SC15-NISSU1            PIC X(01).
                    07  SC15-NISSU2            PIC X(01).
      *≈¿øÙ
            03  SC15-TENSU-TBL             OCCURS  3 TIMES.
                05  SC15-TENSU.
                    07  SC15-TENSU1            PIC X(01).
                    07  SC15-TENSU2            PIC X(01).
                    07  SC15-TENSU3            PIC X(01).
                    07  SC15-TENSU4            PIC X(01).
                    07  SC15-TENSU5            PIC X(01).
                    07  SC15-TENSU6            PIC X(01).
                    07  SC15-TENSU7            PIC X(01).
      *∑Î≥À°¶¿∫ø¿≈¿øÙ
            03  SC15-KESETENSU-TBL         OCCURS  3 TIMES.
                05  SC15-KESETENSU.
                    07  SC15-KESETENSU1        PIC X(01).
                    07  SC15-KESETENSU2        PIC X(01).
                    07  SC15-KESETENSU3        PIC X(01).
                    07  SC15-KESETENSU4        PIC X(01).
                    07  SC15-KESETENSU5        PIC X(01).
                    07  SC15-KESETENSU6        PIC X(01).
      *ø©ªˆ∆¸øÙ
            03  SC15-SNISSU-TBL            OCCURS  3 TIMES.
                05  SC15-SNISSU.
                    07  SC15-SNISSU1           PIC X(01).
                    07  SC15-SNISSU2           PIC X(01).
      *ø©ªˆŒ≈Õ‹¡Ì»ÒÕ—≥€
            03  SC15-SKINGK-TBL            OCCURS  3 TIMES.
                05  SC15-SKINGK.
                    07  SC15-SKINGK1           PIC X(01).
                    07  SC15-SKINGK2           PIC X(01).
                    07  SC15-SKINGK3           PIC X(01).
                    07  SC15-SKINGK4           PIC X(01).
                    07  SC15-SKINGK5           PIC X(01).
                    07  SC15-SKINGK6           PIC X(01).
                    07  SC15-SKINGK7           PIC X(01).
      *ø©ªˆ…∏Ω‡…È√¥≥€
            03  SC15-SFTNGK-TBL            OCCURS  3 TIMES.
                05  SC15-SFTNGK.
                    07  SC15-SFTNGK1           PIC X(01).
                    07  SC15-SFTNGK2           PIC X(01).
                    07  SC15-SFTNGK3           PIC X(01).
                    07  SC15-SFTNGK4           PIC X(01).
                    07  SC15-SFTNGK5           PIC X(01).
                    07  SC15-SFTNGK6           PIC X(01).
      *Ωı¿Æ≥€
            03  SC15-JOSEIGK-TBL           OCCURS  3 TIMES.
                05  SC15-JOSEIGK.
                    07  SC15-JOSEIGK1          PIC X(01).
                    07  SC15-JOSEIGK2          PIC X(01).
                    07  SC15-JOSEIGK3          PIC X(01).
                    07  SC15-JOSEIGK4          PIC X(01).
                    07  SC15-JOSEIGK5          PIC X(01).
                    07  SC15-JOSEIGK6          PIC X(01).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
            03  SC15-YKZFTN-TBL            OCCURS  3 TIMES.
                05  SC15-YKZFTN.
                    07  SC15-YKZFTN1           PIC X(01).
                    07  SC15-YKZFTN2           PIC X(01).
                    07  SC15-YKZFTN3           PIC X(01).
                    07  SC15-YKZFTN4           PIC X(01).
                    07  SC15-YKZFTN5           PIC X(01).
                    07  SC15-YKZFTN6           PIC X(01).
                    07  SC15-YKZFTN7           PIC X(01).
      *∑Î≥À°¶¿∫ø¿ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
            03  SC15-KESEYKZFTN-TBL        OCCURS  3 TIMES.
                05  SC15-KESEYKZFTN.
                    07  SC15-KESEYKZFTN1       PIC X(01).
                    07  SC15-KESEYKZFTN2       PIC X(01).
                    07  SC15-KESEYKZFTN3       PIC X(01).
                    07  SC15-KESEYKZFTN4       PIC X(01).
                    07  SC15-KESEYKZFTN5       PIC X(01).
                    07  SC15-KESEYKZFTN6       PIC X(01).
