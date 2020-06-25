        01  HCM2X.
            02  HCM2X-PAGE  PIC X(4).
            02  HCM2X-HKNJANUM  PIC X(8).
            02  HCM2X-HOSPCD    PIC X(20).
            02  HCM2X-TITLE PIC X(50).
            02  HCM2X-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2X-HKN-G     OCCURS  2 TIMES.
                04  HCM2X-HKN-KENSU PIC X(6).
                04  HCM2X-HKN-NISSU PIC X(6).
                04  HCM2X-HKN-TENSU PIC X(13).
                04  HCM2X-HKN-ITBFTN    PIC X(13).
              03  HCM2X-HKN-SYOG.
                04  HCM2X-HKN-SYOKENSU  PIC X(6).
                04  HCM2X-HKN-SYONISSU  PIC X(6).
                04  HCM2X-HKN-SYOKINGAK PIC X(13).
                04  HCM2X-HKN-SYOFTNGAK PIC X(13).
            02  HCM2X-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2X-KOH-G     OCCURS  2 TIMES.
                04  HCM2X-KOH-KENSU PIC X(6).
                04  HCM2X-KOH-NISSU PIC X(6).
                04  HCM2X-KOH-TENSU PIC X(13).
                04  HCM2X-KOH-ITBFTN    PIC X(13).
              03  HCM2X-KOH-SYOG.
                04  HCM2X-KOH-SYOKENSU  PIC X(6).
                04  HCM2X-KOH-SYONISSU  PIC X(6).
                04  HCM2X-KOH-SYOKINGAK PIC X(13).
                04  HCM2X-KOH-SYOFTNGAK PIC X(13).
              03  HCM2X-KOH-NUM   PIC X(3).
              03  HCM2X-KOH-NAME  PIC X(12).
            02  HCM2X-TITLE1    PIC X(10).
            02  HCM2X-MOJI  PIC X(10).
            02  HCM2X-TITLE2
                    PIC X(4)    OCCURS  2 TIMES.
