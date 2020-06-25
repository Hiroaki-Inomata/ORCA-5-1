        01  HCM2RV01.
            02  HCM2RV01-HOSPCD PIC X(20).
            02  HCM2RV01-PAGE   PIC X(4).
            02  HCM2RV01-MEI-TBL        OCCURS  23 TIMES.
              03  HCM2RV01-HKNJANUM   PIC X(8).
              03  HCM2RV01-TAI-KENSU
                      PIC X(4)    OCCURS  5 TIMES.
              03  HCM2RV01-TAI-TENSU
                      PIC X(8)    OCCURS  5 TIMES.
              03  HCM2RV01-TAI-ITBFTN
                      PIC X(8)    OCCURS  2 TIMES.
              03  HCM2RV01-KOU-KENSU  PIC X(4).
              03  HCM2RV01-KOU-TENSU  PIC X(8).
              03  HCM2RV01-KOU-ITBFTN PIC X(8).
              03  HCM2RV01-SYO-KENSU  PIC X(4).
              03  HCM2RV01-SYO-TENSU  PIC X(8).
              03  HCM2RV01-SYO-ITBFTN PIC X(8).
            02  HCM2RV01-TITLE1 PIC X(18).
            02  HCM2RV01-TITLE2 PIC X(2).
