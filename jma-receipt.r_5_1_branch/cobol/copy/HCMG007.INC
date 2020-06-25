        01  HCMG007.
            02  HCMG007-KIKAN
                        PIC X(34).
            02  HCMG007-TAISYO
                        PIC X(16).
            02  HCMG007-PRTYMD
                        PIC X(22).
            02  HCMG007-PAGE
                        PIC X(3).
            02  HCMG007-LBL
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCMG007-TBL
                                        OCCURS 51 TIMES.
              03  HCMG007-YKZKBN
                        PIC X(2).
              03  HCMG007-SRYCD
                        PIC X(9).
              03  HCMG007-NAME
                        PIC X(44).
              03  HCMG007-KOUHATU
                        PIC X(1).
              03  HCMG007-YAKKAKJNCD
                        PIC X(12).
              03  HCMG007-TANISU
                        PIC X(4).
              03  HCMG007-KIKAKU
                        PIC X(7)        OCCURS 4 TIMES.
