        01  HCM2WV01.
            02  HCM2WV01-PAGE   PIC X(4).
            02  HCM2WV01-PRTYM  PIC X(16).
            02  HCM2WV01-HKNJANUM   PIC X(8).
            02  HCM2WV01-HKNJANAME  PIC X(20).
            02  HCM2WV01-SEIYMD PIC X(22).
            02  HCM2WV01-HOSPCD PIC X(20).
            02  HCM2WV01-ADRS   PIC X(100).
            02  HCM2WV01-HOSPNAME   PIC X(100).
            02  HCM2WV01-KAISETUNAME    PIC X(30).
            02  HCM2WV01-HKN-TBL.
              03  HCM2WV01-HKN-TBLG       OCCURS  9 TIMES.
                04  HCM2WV01-HKN-G          OCCURS  2 TIMES.
                  05  HCM2WV01-KENSU  PIC X(6).
                  05  HCM2WV01-NISSU  PIC X(6).
                  05  HCM2WV01-TENSU  PIC X(13).
                  05  HCM2WV01-ITBFTN PIC X(13).
            02  HCM2WV01-SYO-TBL.
              03  HCM2WV01-SYO-TBLG       OCCURS  9 TIMES.
                04  HCM2WV01-SYOKENSU   PIC X(6).
                04  HCM2WV01-SYONISSU   PIC X(6).
                04  HCM2WV01-SYOKINGAK  PIC X(13).
                04  HCM2WV01-SYOFTNGAK  PIC X(13).
            02  HCM2WV01-TITLE  PIC X(50).
            02  HCM2WV01-TITLE1 PIC X(10).
            02  HCM2WV01-MOJI   PIC X(10).
            02  HCM2WV01-TITLE2 PIC X(4).
            02  HCM2WV01-SRYKA  PIC X(40).
            02  HCM2WV01-TITLE3 PIC X(18).
            02  HCM2WV01-TITLE4 PIC X(2).
            02  HCM2WV01-TITLE5 PIC X(2).
