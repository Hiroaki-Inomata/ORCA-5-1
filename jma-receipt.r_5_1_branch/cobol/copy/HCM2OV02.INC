        01  HCM2OV02.
            02  HCM2OV02-PAGE   PIC X(4).
            02  HCM2OV02-PRTYM  PIC X(16).
            02  HCM2OV02-HKNJANUM   PIC X(8).
            02  HCM2OV02-HKNJANAME  PIC X(20).
            02  HCM2OV02-SEIYMD PIC X(22).
            02  HCM2OV02-HOSPCD PIC X(20).
            02  HCM2OV02-ADRS   PIC X(100).
            02  HCM2OV02-HOSPNAME   PIC X(100).
            02  HCM2OV02-KAISETUNAME    PIC X(30).
            02  HCM2OV02-HKN-TBL.
              03  HCM2OV02-HKN-TBLG       OCCURS  12 TIMES.
                04  HCM2OV02-HKN-G          OCCURS  2 TIMES.
                  05  HCM2OV02-KENSU  PIC X(6).
                  05  HCM2OV02-NISSU  PIC X(6).
                  05  HCM2OV02-TENSU  PIC X(13).
                  05  HCM2OV02-ITBFTN PIC X(13).
            02  HCM2OV02-SYO-TBL.
              03  HCM2OV02-SYO-TBLG       OCCURS  12 TIMES.
                04  HCM2OV02-SYOKENSU   PIC X(6).
                04  HCM2OV02-SYONISSU   PIC X(6).
                04  HCM2OV02-SYOKINGAK  PIC X(13).
                04  HCM2OV02-SYOFTNGAK  PIC X(13).
            02  HCM2OV02-TITLE  PIC X(50).
            02  HCM2OV02-TITLE1 PIC X(10).
            02  HCM2OV02-MOJI   PIC X(10).
