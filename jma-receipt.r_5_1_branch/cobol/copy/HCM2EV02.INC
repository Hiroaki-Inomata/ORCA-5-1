        01  HCM2EV02.
            02  HCM2EV02-PAGE   PIC X(4).
            02  HCM2EV02-PRTYM  PIC X(16).
            02  HCM2EV02-HKNJANUM   PIC X(8).
            02  HCM2EV02-HKNJANAME  PIC X(20).
            02  HCM2EV02-SEIYMD PIC X(22).
            02  HCM2EV02-HOSPCD PIC X(20).
            02  HCM2EV02-ADRS   PIC X(100).
            02  HCM2EV02-HOSPNAME   PIC X(100).
            02  HCM2EV02-KAISETUNAME    PIC X(30).
            02  HCM2EV02-HKN-TBL.
              03  HCM2EV02-HKN-TBLG       OCCURS  9 TIMES.
                04  HCM2EV02-HKN-G          OCCURS  2 TIMES.
                  05  HCM2EV02-KENSU  PIC X(6).
                  05  HCM2EV02-NISSU  PIC X(6).
                  05  HCM2EV02-TENSU  PIC X(13).
                  05  HCM2EV02-YKZFTN PIC X(8).
                  05  HCM2EV02-ITBFTN PIC X(13).
            02  HCM2EV02-SYO-TBL.
              03  HCM2EV02-SYO-TBLG       OCCURS  9 TIMES.
                04  HCM2EV02-SYOKENSU   PIC X(6).
                04  HCM2EV02-SYONISSU   PIC X(6).
                04  HCM2EV02-SYOKINGAK  PIC X(13).
                04  HCM2EV02-SYOFTNGAK  PIC X(13).
            02  HCM2EV02-TITLE  PIC X(50).
