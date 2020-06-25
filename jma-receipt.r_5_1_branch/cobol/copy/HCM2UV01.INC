        01  HCM2UV01.
            02  HCM2UV01-PAGE   PIC X(4).
            02  HCM2UV01-PRTYM  PIC X(16).
            02  HCM2UV01-HKNJANUM   PIC X(8).
            02  HCM2UV01-HKNJANAME  PIC X(20).
            02  HCM2UV01-SEIYMD PIC X(22).
            02  HCM2UV01-HOSPCD PIC X(20).
            02  HCM2UV01-ADRS   PIC X(100).
            02  HCM2UV01-HOSPNAME   PIC X(100).
            02  HCM2UV01-KAISETUNAME    PIC X(30).
            02  HCM2UV01-HKN-TBL.
              03  HCM2UV01-HKN-TBLG       OCCURS  12 TIMES.
                04  HCM2UV01-HKN-G          OCCURS  2 TIMES.
                  05  HCM2UV01-KENSU  PIC X(6).
                  05  HCM2UV01-NISSU  PIC X(6).
                  05  HCM2UV01-TENSU  PIC X(13).
                  05  HCM2UV01-ITBFTN PIC X(13).
            02  HCM2UV01-SYO-TBL.
              03  HCM2UV01-SYO-TBLG       OCCURS  12 TIMES.
                04  HCM2UV01-SYOKENSU   PIC X(6).
                04  HCM2UV01-SYONISSU   PIC X(6).
                04  HCM2UV01-SYOKINGAK  PIC X(13).
                04  HCM2UV01-SYOFTNGAK  PIC X(13).
            02  HCM2UV01-TITLE  PIC X(50).
            02  HCM2UV01-TITLE1 PIC X(10).
            02  HCM2UV01-MOJI   PIC X(10).
            02  HCM2UV01-TITLE2 PIC X(4).
            02  HCM2UV01-SRYKA  PIC X(40).
            02  HCM2UV01-TITLE3 PIC X(18).
            02  HCM2UV01-TITLE4 PIC X(2).
            02  HCM2UV01-TITLE5 PIC X(2).
