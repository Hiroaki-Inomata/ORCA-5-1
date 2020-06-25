        01  HCM2GV02.
            02  HCM2GV02-PAGE   PIC X(4).
            02  HCM2GV02-PRTYM  PIC X(16).
            02  HCM2GV02-HKNJANUM   PIC X(8).
            02  HCM2GV02-HKNJANAME  PIC X(20).
            02  HCM2GV02-SEIYMD PIC X(22).
            02  HCM2GV02-HOSPCD PIC X(20).
            02  HCM2GV02-ADRS   PIC X(100).
            02  HCM2GV02-HOSPNAME   PIC X(100).
            02  HCM2GV02-KAISETUNAME    PIC X(30).
            02  HCM2GV02-HKN-TBL.
              03  HCM2GV02-HKN-TBLG       OCCURS  12 TIMES.
                04  HCM2GV02-HKN-G          OCCURS  2 TIMES.
                  05  HCM2GV02-KENSU  PIC X(6).
                  05  HCM2GV02-NISSU  PIC X(6).
                  05  HCM2GV02-TENSU  PIC X(13).
                  05  HCM2GV02-YKZFTN PIC X(8).
                  05  HCM2GV02-ITBFTN PIC X(13).
            02  HCM2GV02-SYO-TBL.
              03  HCM2GV02-SYO-TBLG       OCCURS  12 TIMES.
                04  HCM2GV02-SYOKENSU   PIC X(6).
                04  HCM2GV02-SYONISSU   PIC X(6).
                04  HCM2GV02-SYOKINGAK  PIC X(13).
                04  HCM2GV02-SYOFTNGAK  PIC X(13).
            02  HCM2GV02-TITLE  PIC X(50).
