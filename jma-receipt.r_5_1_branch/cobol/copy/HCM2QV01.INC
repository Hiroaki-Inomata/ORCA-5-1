        01  HCM2QV01.
            02  HCM2QV01-PRTYM  PIC X(16).
            02  HCM2QV01-SEIYMD PIC X(22).
            02  HCM2QV01-HOSPCD PIC X(20).
            02  HCM2QV01-ADRS   PIC X(50).
            02  HCM2QV01-HOSPNAME   PIC X(100).
            02  HCM2QV01-KAISETUNAME    PIC X(60).
            02  HCM2QV01-PAGE   PIC X(4).
            02  HCM2QV01-MEI-TBL        OCCURS  23 TIMES.
              03  HCM2QV01-HKNJANUM   PIC X(8).
              03  HCM2QV01-ROU-KENSU
                      PIC X(4)    OCCURS  2 TIMES.
              03  HCM2QV01-ROU-TENSU
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV01-ROU-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV01-IPN-KENSU
                      PIC X(4)    OCCURS  4 TIMES.
              03  HCM2QV01-IPN-TENSU
                      PIC X(8)    OCCURS  4 TIMES.
              03  HCM2QV01-IPN-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV01-SYO-KENSU  PIC X(4).
              03  HCM2QV01-SYO-TENSU  PIC X(8).
              03  HCM2QV01-SYO-ITBFTN PIC X(8).
            02  HCM2QV01-TITLE1 PIC X(18).
            02  HCM2QV01-TITLE2 PIC X(2).
            02  HCM2QV01-TITLE3 PIC X(2).
