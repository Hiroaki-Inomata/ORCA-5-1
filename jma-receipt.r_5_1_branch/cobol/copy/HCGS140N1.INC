        01  HCGS140N1.
            02  HCGS140N1-PAGE
                        PIC X(3).
            02  HCGS140N1-SRYYM
                        PIC X(16).
            02  HCGS140N1-SYSYMD
                        PIC X(22).
            02  HCGS140N1-NYUGAI
                        PIC X(4).
            02  HCGS140N1-SRYKANAME
                        PIC X(20).
            02  HCGS140N1-KOHMSG
                        PIC X(50).
            02  HCGS140N1-ETCMSG
                        PIC X(100).
            02  HCGS140N1-CITY-AREA.
              03  HCGS140N1-KBN1-TBL
                                        OCCURS 3 TIMES.
                04  HCGS140N1-KENSU
                        PIC X(4).
                04  HCGS140N1-NISSU
                        PIC X(4).
                04  HCGS140N1-TENSU
                        PIC X(9).
                04  HCGS140N1-FTNMONEY
                        PIC X(9).
                04  HCGS140N1-HKNMONEY
                        PIC X(9).
                04  HCGS140N1-KOHMONEY
                        PIC X(9).
                04  HCGS140N1-TTLMONEY
                        PIC X(9).
                04  HCGS140N1-SKJKAISU
                        PIC X(5).
                04  HCGS140N1-SKJTTLMONEY
                        PIC X(9).
                04  HCGS140N1-SKJHKNMONEY
                        PIC X(9).
                04  HCGS140N1-SKJKOHMONEY
                        PIC X(9).
                04  HCGS140N1-SKJFTNMONEY
                        PIC X(9).
                04  HCGS140N1-TTLTTLMONEY
                        PIC X(9).
              03  HCGS140N1-KOHI-TBL
                                        OCCURS 21 TIMES.
                04  HCGS140N1-KOHNAME
                        PIC X(24).
                04  HCGS140N1-KOHKENSU
                        PIC X(4).
                04  HCGS140N1-KOHNISSU
                        PIC X(4).
                04  HCGS140N1-KOHTENSU
                        PIC X(9).
              03  HCGS140N1-ETC-TBL.
                04  HCGS140N1-JIHIMONEY
                        PIC X(9).
                04  HCGS140N1-GENMEN
                        PIC X(9).
                04  HCGS140N1-CHOSEI1
                        PIC X(9).
                04  HCGS140N1-CHOSEI2
                        PIC X(9).
                04  HCGS140N1-HASU
                        PIC X(9).
            02  HCGS140N1-SKYSBT
                        PIC X(50).
            02  HCGS140N1-LOPTION
                        PIC X(500).
