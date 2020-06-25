        01  HCM2EV01.
            02  HCM2EV01-PAGE   PIC X(4).
            02  HCM2EV01-PRTYM  PIC X(16).
            02  HCM2EV01-HKNJANUM   PIC X(8).
            02  HCM2EV01-HKNJANAME  PIC X(20).
            02  HCM2EV01-SEIYMD PIC X(22).
            02  HCM2EV01-HOSPCD PIC X(20).
            02  HCM2EV01-ADRS   PIC X(100).
            02  HCM2EV01-HOSPNAME   PIC X(100).
            02  HCM2EV01-KAISETUNAME    PIC X(30).
            02  HCM2EV01-HKN-TBL.
              03  HCM2EV01-HKN-TBLG       OCCURS  9 TIMES.
                04  HCM2EV01-HKN-G          OCCURS  2 TIMES.
                  05  HCM2EV01-KENSU  PIC X(6).
                  05  HCM2EV01-NISSU  PIC X(6).
                  05  HCM2EV01-TENSU  PIC X(13).
                  05  HCM2EV01-YKZFTN PIC X(8).
                  05  HCM2EV01-ITBFTN PIC X(13).
            02  HCM2EV01-SYO-TBL.
              03  HCM2EV01-SYO-TBLG       OCCURS  9 TIMES.
                04  HCM2EV01-SYOKENSU   PIC X(6).
                04  HCM2EV01-SYONISSU   PIC X(6).
                04  HCM2EV01-SYOKINGAK  PIC X(13).
                04  HCM2EV01-SYOFTNGAK  PIC X(13).
            02  HCM2EV01-TITLE  PIC X(36).
