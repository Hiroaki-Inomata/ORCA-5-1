        01  HCM2WV02.
            02  HCM2WV02-PAGE   PIC X(4).
            02  HCM2WV02-PRTYM  PIC X(16).
            02  HCM2WV02-HKNJANUM   PIC X(8).
            02  HCM2WV02-HKNJANAME  PIC X(20).
            02  HCM2WV02-SEIYMD PIC X(22).
            02  HCM2WV02-HOSPCD PIC X(20).
            02  HCM2WV02-ADRS   PIC X(100).
            02  HCM2WV02-HOSPNAME   PIC X(100).
            02  HCM2WV02-KAISETUNAME    PIC X(30).
            02  HCM2WV02-HKN-TBL.
              03  HCM2WV02-HKN-TBLG       OCCURS  9 TIMES.
                04  HCM2WV02-HKN-G          OCCURS  2 TIMES.
                  05  HCM2WV02-KENSU  PIC X(6).
                  05  HCM2WV02-NISSU  PIC X(6).
                  05  HCM2WV02-TENSU  PIC X(13).
                  05  HCM2WV02-ITBFTN PIC X(13).
            02  HCM2WV02-SYO-TBL.
              03  HCM2WV02-SYO-TBLG       OCCURS  9 TIMES.
                04  HCM2WV02-SYOKENSU   PIC X(6).
                04  HCM2WV02-SYONISSU   PIC X(6).
                04  HCM2WV02-SYOKINGAK  PIC X(13).
                04  HCM2WV02-SYOFTNGAK  PIC X(13).
            02  HCM2WV02-TITLE  PIC X(50).
            02  HCM2WV02-TITLE1 PIC X(10).
            02  HCM2WV02-MOJI   PIC X(10).
            02  HCM2WV02-TITLE2 PIC X(4).
            02  HCM2WV02-SRYKA  PIC X(40).
            02  HCM2WV02-TITLE3 PIC X(18).
            02  HCM2WV02-TITLE4 PIC X(2).
            02  HCM2WV02-TITLE5 PIC X(2).
