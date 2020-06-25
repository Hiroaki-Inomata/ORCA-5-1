        01  HCM2QV02.
            02  HCM2QV02-PRTYM  PIC X(16).
            02  HCM2QV02-SEIYMD PIC X(22).
            02  HCM2QV02-HOSPCD PIC X(20).
            02  HCM2QV02-ADRS   PIC X(100).
            02  HCM2QV02-HOSPNAME   PIC X(100).
            02  HCM2QV02-KAISETUNAME    PIC X(60).
            02  HCM2QV02-PAGE   PIC X(4).
            02  HCM2QV02-MEI-TBL        OCCURS  23 TIMES.
              03  HCM2QV02-HKNJANUM   PIC X(8).
              03  HCM2QV02-ROU-KENSU
                      PIC X(4)    OCCURS  2 TIMES.
              03  HCM2QV02-ROU-TENSU
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV02-ROU-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV02-IPN-KENSU
                      PIC X(4)    OCCURS  4 TIMES.
              03  HCM2QV02-IPN-TENSU
                      PIC X(8)    OCCURS  4 TIMES.
              03  HCM2QV02-IPN-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2QV02-SYO-KENSU  PIC X(4).
              03  HCM2QV02-SYO-TENSU  PIC X(8).
              03  HCM2QV02-SYO-ITBFTN PIC X(8).
            02  HCM2QV02-TITLE1 PIC X(18).
            02  HCM2QV02-TITLE2 PIC X(2).
            02  HCM2QV02-TITLE3 PIC X(2).
