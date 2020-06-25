        01  HCM2R.
            02  HCM2R-HOSPCD    PIC X(20).
            02  HCM2R-PAGE  PIC X(4).
            02  HCM2R-MEI-TBL           OCCURS  23 TIMES.
              03  HCM2R-HKNJANUM  PIC X(8).
              03  HCM2R-TAI-KENSU
                      PIC X(4)    OCCURS  5 TIMES.
              03  HCM2R-TAI-TENSU
                      PIC X(8)    OCCURS  5 TIMES.
              03  HCM2R-TAI-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2R-KOU-KENSU PIC X(4).
              03  HCM2R-KOU-TENSU PIC X(8).
              03  HCM2R-KOU-ITBFTN    PIC X(8).
              03  HCM2R-SYO-KENSU PIC X(4).
              03  HCM2R-SYO-TENSU PIC X(8).
              03  HCM2R-SYO-ITBFTN    PIC X(8).
