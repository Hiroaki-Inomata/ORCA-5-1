        01  HCM2PV02.
            02  HCM2PV02-PAGE   PIC X(4).
            02  HCM2PV02-HKNJANUM   PIC X(8).
            02  HCM2PV02-HOSPCD PIC X(20).
            02  HCM2PV02-TITLE  PIC X(50).
            02  HCM2PV02-HKN-TBL        OCCURS  2 TIMES.
              03  HCM2PV02-HKN-G          OCCURS  2 TIMES.
                04  HCM2PV02-HKN-KENSU  PIC X(6).
                04  HCM2PV02-HKN-NISSU  PIC X(6).
                04  HCM2PV02-HKN-TENSU  PIC X(13).
                04  HCM2PV02-HKN-ITBFTN PIC X(13).
              03  HCM2PV02-HKN-SYOG.
                04  HCM2PV02-HKN-SYOKENSU   PIC X(6).
                04  HCM2PV02-HKN-SYONISSU   PIC X(6).
                04  HCM2PV02-HKN-SYOKINGAK  PIC X(13).
                04  HCM2PV02-HKN-SYOFTNGAK  PIC X(13).
            02  HCM2PV02-KOH-TBL        OCCURS  2 TIMES.
              03  HCM2PV02-KOH-G          OCCURS  2 TIMES.
                04  HCM2PV02-KOH-KENSU  PIC X(6).
                04  HCM2PV02-KOH-NISSU  PIC X(6).
                04  HCM2PV02-KOH-TENSU  PIC X(13).
                04  HCM2PV02-KOH-ITBFTN PIC X(13).
              03  HCM2PV02-KOH-SYOG.
                04  HCM2PV02-KOH-SYOKENSU   PIC X(6).
                04  HCM2PV02-KOH-SYONISSU   PIC X(6).
                04  HCM2PV02-KOH-SYOKINGAK  PIC X(13).
                04  HCM2PV02-KOH-SYOFTNGAK  PIC X(13).
              03  HCM2PV02-KOH-NUM    PIC X(3).
              03  HCM2PV02-KOH-NAME   PIC X(12).
            02  HCM2PV02-TITLE1 PIC X(10).
            02  HCM2PV02-MOJI   PIC X(10).
