        01  HCM2Q.
            02  HCM2Q-PRTYM PIC X(16).
            02  HCM2Q-SEIYMD    PIC X(22).
            02  HCM2Q-HOSPCD    PIC X(20).
            02  HCM2Q-ADRS  PIC X(50).
            02  HCM2Q-HOSPNAME  PIC X(100).
            02  HCM2Q-KAISETUNAME   PIC X(60).
            02  HCM2Q-PAGE  PIC X(4).
            02  HCM2Q-MEI-TBL           OCCURS  23 TIMES.
              03  HCM2Q-HKNJANUM  PIC X(8).
              03  HCM2Q-ROU-KENSU
                      PIC X(4)    OCCURS  2 TIMES.
              03  HCM2Q-ROU-TENSU
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2Q-ROU-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2Q-IPN-KENSU
                      PIC X(4)    OCCURS  4 TIMES.
              03  HCM2Q-IPN-TENSU
                      PIC X(8)    OCCURS  4 TIMES.
              03  HCM2Q-IPN-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2Q-SYO-KENSU PIC X(4).
              03  HCM2Q-SYO-TENSU PIC X(8).
              03  HCM2Q-SYO-ITBFTN    PIC X(8).
