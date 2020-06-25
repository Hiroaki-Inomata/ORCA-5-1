        01  HCM2XV01.
            02  HCM2XV01-PAGE   PIC X(4).
            02  HCM2XV01-HKNJANUM   PIC X(8).
            02  HCM2XV01-HOSPCD PIC X(20).
            02  HCM2XV01-TITLE  PIC X(50).
            02  HCM2XV01-HKN-TBL        OCCURS  2 TIMES.
              03  HCM2XV01-HKN-G          OCCURS  2 TIMES.
                04  HCM2XV01-HKN-KENSU  PIC X(6).
                04  HCM2XV01-HKN-NISSU  PIC X(6).
                04  HCM2XV01-HKN-TENSU  PIC X(13).
                04  HCM2XV01-HKN-ITBFTN PIC X(13).
              03  HCM2XV01-HKN-SYOG.
                04  HCM2XV01-HKN-SYOKENSU   PIC X(6).
                04  HCM2XV01-HKN-SYONISSU   PIC X(6).
                04  HCM2XV01-HKN-SYOKINGAK  PIC X(13).
                04  HCM2XV01-HKN-SYOFTNGAK  PIC X(13).
            02  HCM2XV01-KOH-TBL        OCCURS  2 TIMES.
              03  HCM2XV01-KOH-G          OCCURS  2 TIMES.
                04  HCM2XV01-KOH-KENSU  PIC X(6).
                04  HCM2XV01-KOH-NISSU  PIC X(6).
                04  HCM2XV01-KOH-TENSU  PIC X(13).
                04  HCM2XV01-KOH-ITBFTN PIC X(13).
              03  HCM2XV01-KOH-SYOG.
                04  HCM2XV01-KOH-SYOKENSU   PIC X(6).
                04  HCM2XV01-KOH-SYONISSU   PIC X(6).
                04  HCM2XV01-KOH-SYOKINGAK  PIC X(13).
                04  HCM2XV01-KOH-SYOFTNGAK  PIC X(13).
              03  HCM2XV01-KOH-NUM    PIC X(3).
              03  HCM2XV01-KOH-NAME   PIC X(12).
            02  HCM2XV01-TITLE1 PIC X(10).
            02  HCM2XV01-MOJI   PIC X(10).
            02  HCM2XV01-TITLE2
                    PIC X(4)    OCCURS  2 TIMES.
            02  HCM2XV01-SRYKA  PIC X(40).
            02  HCM2XV01-TITLE3 PIC X(18).
            02  HCM2XV01-TITLE4 PIC X(18).
            02  HCM2XV01-TITLE5 PIC X(2).
