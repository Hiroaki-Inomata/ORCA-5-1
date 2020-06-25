        01  HCMG001.
            02  HCMG001-DAI
                        PIC X(100).
            02  HCMG001-YMD
                        PIC X(26).
            02  HCMG001-PAGE
                        PIC X(7).
            02  HCMG001-TAI-TBL.
              03  HCMG001-TAI-CD
                        PIC X(14).
              03  HCMG001-TAI-NAME
                        PIC X(40).
              03  HCMG001-TAI-TEN
                        PIC X(14).
              03  HCMG001-TAI-TANI
                        PIC X(10).
              03  HCMG001-TAI-NAIYO
                        PIC X(14)       OCCURS 7 TIMES.
            02  HCMG001-NAI-TBL
                                        OCCURS 35 TIMES.
              03  HCMG001-CD
                        PIC X(9).
              03  HCMG001-NAME
                        PIC X(40).
              03  HCMG001-TEN
                        PIC X(10).
              03  HCMG001-TANI
                        PIC X(10).
              03  HCMG001-NAIYO
                        PIC X(10)       OCCURS 7 TIMES.
            02  HCMG001-KIKAN
                        PIC X(50).
            02  HCMG001-KOME
                        PIC X(1)        OCCURS 35 TIMES.
            02  HCMG001-LBL
                        PIC X(40).
