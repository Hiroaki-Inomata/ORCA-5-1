        01  HCGS138G2.
            02  HCGS138G2-PAGE
                        PIC X(3).
            02  HCGS138G2-SRYYM
                        PIC X(16).
            02  HCGS138G2-SYSYMD
                        PIC X(22).
            02  HCGS138G2-NYUGAI
                        PIC X(4).
            02  HCGS138G2-SRYKANAME
                        PIC X(20).
            02  HCGS138G2-CITYKBN
                        PIC X(12).
            02  HCGS138G2-HKNJA-AREA.
              03  HCGS138G2-HKNJA-TBL
                                        OCCURS 55 TIMES.
                04  HCGS138G2-HKNJANAME
                        PIC X(24).
                04  HCGS138G2-KENSU
                        PIC X(4).
                04  HCGS138G2-NISSU
                        PIC X(4).
                04  HCGS138G2-TENSU
                        PIC X(9).
                04  HCGS138G2-FTNMONEY
                        PIC X(9).
                04  HCGS138G2-HKNMONEY
                        PIC X(9).
                04  HCGS138G2-KOHMONEY
                        PIC X(9).
                04  HCGS138G2-TTLMONEY
                        PIC X(9).
            02  HCGS138G2-HKNJANUM
                        PIC X(8)        OCCURS 55 TIMES.
            02  HCGS138G2-SKYSBT
                        PIC X(50).
            02  HCGS138G2-HKNKBNNAME
                        PIC X(20)       OCCURS 55 TIMES.
