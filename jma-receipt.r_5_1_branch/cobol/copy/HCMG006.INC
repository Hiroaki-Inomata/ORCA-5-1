        01  HCMG006.
            02  HCMG006-DAI
                        PIC X(100).
            02  HCMG006-YMD
                        PIC X(26).
            02  HCMG006-PAGE
                        PIC X(7).
            02  HCMG006-TAI-TBL.
              03  HCMG006-TAI-CD
                        PIC X(14).
              03  HCMG006-TAI-NAME
                        PIC X(40).
              03  HCMG006-TAI-TEN
                        PIC X(14).
              03  HCMG006-TAI-TANI
                        PIC X(10).
              03  HCMG006-TAI-NAIYO
                        PIC X(14)       OCCURS 5 TIMES.
            02  HCMG006-NAI-TBL
                                        OCCURS 25 TIMES.
              03  HCMG006-CD
                        PIC X(9).
              03  HCMG006-NAME
                        PIC X(40).
              03  HCMG006-TEN
                        PIC X(10).
              03  HCMG006-TANI
                        PIC X(10).
              03  HCMG006-SURYO
                        PIC X(14)       OCCURS 5 TIMES.
              03  HCMG006-GAKU
                        PIC X(14)       OCCURS 5 TIMES.
            02  HCMG006-KIKAN
                        PIC X(50).
            02  HCMG006-KOME
                        PIC X(1)        OCCURS 25 TIMES.
            02  HCMG006-LBL
                        PIC X(30).
