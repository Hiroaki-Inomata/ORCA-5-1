        01  SC33.
      *•⁄°º•∏øÙ
            02  SC33-PAGE   PIC X(3).
      *¿¡µ·∆¸
            02  SC33-SEIYMD-TBL.
              03  SC33-SEIYY  PIC X(4).
              03  SC33-SEIMM  PIC X(4).
              03  SC33-SEIDD  PIC X(4).
      *ø«Œ≈∂Ë ¨
            02  SC33-SRKBN
                    PIC X(2)    OCCURS  6 TIMES.
      *∞ÂŒ≈µ°¥ÿΩªΩÍ
            02  SC33-HOSPADRS   PIC X(100).
      *∞ÂŒ≈µ°¥ÿÃææŒ
            02  SC33-HOSPNAME   PIC X(100).
      *≥´»Øº‘ª·Ãæ
            02  SC33-DAIHYOUNAME    PIC X(40).
      *∞ÂŒ≈µ°¥ÿ≈≈œ√
            02  SC33-HOSPTEL    PIC X(15).
      *∞ÂŒ≈µ°¥ÿ•≥°º•…
            02  SC33-HOSPCD-TBL.
              03  SC33-HOSPCD1    PIC X(2).
              03  SC33-HOSPCD2    PIC X(2).
              03  SC33-HOSPCD3    PIC X(2).
              03  SC33-HOSPCD4    PIC X(2).
              03  SC33-HOSPCD5    PIC X(2).
              03  SC33-HOSPCD6    PIC X(2).
              03  SC33-HOSPCD7    PIC X(2).
              03  SC33-HOSPCD8    PIC X(2).
      *ø«Œ≈«Ø∑Ó
            02  SC33-SRYM1-TBL.
              03  SC33-SRYY1  PIC X(4).
              03  SC33-SRMM1  PIC X(4).
      *ºıµÎº‘»÷πÊ
            02  SC33-JKYSNUM
                    PIC X(14)   OCCURS  10 TIMES.
      *ºıø«º‘Ãæ
            02  SC33-KATAKANA
                    PIC X(30)   OCCURS  10 TIMES.
      *¿∏«Ø∑Ó∆¸
            02  SC33-BRITHDAY-TBL       OCCURS  10 TIMES.
              03  SC33-BRITHH PIC X(2).
              03  SC33-BRITHYY    PIC X(4).
              03  SC33-BRITHMM    PIC X(4).
              03  SC33-BRITHDD    PIC X(4).
      *≤√∆˛∑ÚπØ ›∏±
            02  SC33-HKKBN-TBL          OCCURS  10 TIMES.
              03  SC33-HKKBN
                      PIC X(2)    OCCURS  4 TIMES.
            02  SC33-HKNAME
                    PIC X(20)   OCCURS  10 TIMES.
      * ›∏±ø«Œ≈∞Ï…Ù…È√¥∂‚
            02  SC33-HKNFTN
                    PIC X(7)    OCCURS  10 TIMES.
      *ø«Œ≈«Ø∑Ó
            02  SC33-SRYM-TBL           OCCURS  10 TIMES.
              03  SC33-SRYY   PIC X(2).
              03  SC33-SRMM   PIC X(2).
      *ø«Œ≈∂Ë ¨
            02  SC33-SRKBN1
                    PIC X(1)    OCCURS  10 TIMES.
      *∆˛≥∞∂Ë ¨
            02  SC33-NGKBN-TBL          OCCURS  10 TIMES.
              03  SC33-NGKBN
                      PIC X(2)    OCCURS  2 TIMES.
      *∆¸øÙ
            02  SC33-NITISU
                    PIC X(2)    OCCURS  10 TIMES.
      * ›∏±ø«Œ≈¡Ì≈¿øÙ
            02  SC33-HKSRYTEN
                    PIC X(7)    OCCURS  10 TIMES.
            02  SC33-HOUBETU
                    PIC X(2)    OCCURS  10 TIMES.
            02  SC33-TAHOUTEN
                    PIC X(7)    OCCURS  10 TIMES.
      *…È√¥≥‰πÁ
            02  SC33-FTNWARIAI
                    PIC X(1)    OCCURS  10 TIMES.
      *∆˛±°ª˛ø©ªˆŒ≈Õ‹»Ò
            02  SC33-SKJFTN
                    PIC X(7)    OCCURS  10 TIMES.
            02  SC33-SANTEIKAI
                    PIC X(3)    OCCURS  10 TIMES.
      *æÆ∑◊
            02  SC33-SYOUKEI-TBL.
              03  SC33-SK-HKNFTN  PIC X(9).
              03  SC33-SK-SKJFTN  PIC X(9).
      *πÁ∑◊
            02  SC33-GAOKEI-TBL.
              03  SC33-GK-HKNFTN  PIC X(9).
              03  SC33-GK-SKJFTN  PIC X(9).
