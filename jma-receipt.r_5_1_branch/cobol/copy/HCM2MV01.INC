        01  HCM2MV01.
            02  HCM2MV01-PAGE   PIC X(4).
            02  HCM2MV01-PRTYM  PIC X(16).
            02  HCM2MV01-HKNJANUM   PIC X(8).
            02  HCM2MV01-HKNJANAME  PIC X(20).
            02  HCM2MV01-SEIYMD PIC X(22).
            02  HCM2MV01-HOSPCD PIC X(20).
            02  HCM2MV01-ADRS   PIC X(100).
            02  HCM2MV01-HOSPNAME   PIC X(100).
            02  HCM2MV01-KAISETUNAME    PIC X(30).
            02  HCM2MV01-HKN-TBL.
              03  HCM2MV01-HKN-TBLG       OCCURS  9 TIMES.
                04  HCM2MV01-HKN-G          OCCURS  2 TIMES.
                  05  HCM2MV01-KENSU  PIC X(6).
                  05  HCM2MV01-NISSU  PIC X(6).
                  05  HCM2MV01-TENSU  PIC X(13).
                  05  HCM2MV01-ITBFTN PIC X(13).
            02  HCM2MV01-SYO-TBL.
              03  HCM2MV01-SYO-TBLG       OCCURS  9 TIMES.
                04  HCM2MV01-SYOKENSU   PIC X(6).
                04  HCM2MV01-SYONISSU   PIC X(6).
                04  HCM2MV01-SYOKINGAK  PIC X(13).
                04  HCM2MV01-SYOFTNGAK  PIC X(13).
            02  HCM2MV01-TITLE  PIC X(50).
            02  HCM2MV01-TITLE1 PIC X(10).
