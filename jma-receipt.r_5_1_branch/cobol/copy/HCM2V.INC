        01  HCM2V.
            02  HCM2V-PAGE  PIC X(4).
            02  HCM2V-HKNJANUM  PIC X(8).
            02  HCM2V-HOSPCD    PIC X(20).
            02  HCM2V-TITLE PIC X(50).
            02  HCM2V-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2V-HKN-G     OCCURS  2 TIMES.
                04  HCM2V-HKN-KENSU PIC X(6).
                04  HCM2V-HKN-NISSU PIC X(6).
                04  HCM2V-HKN-TENSU PIC X(13).
                04  HCM2V-HKN-ITBFTN    PIC X(13).
              03  HCM2V-HKN-SYOG.
                04  HCM2V-HKN-SYOKENSU  PIC X(6).
                04  HCM2V-HKN-SYONISSU  PIC X(6).
                04  HCM2V-HKN-SYOKINGAK PIC X(13).
                04  HCM2V-HKN-SYOFTNGAK PIC X(13).
            02  HCM2V-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2V-KOH-G     OCCURS  2 TIMES.
                04  HCM2V-KOH-KENSU PIC X(6).
                04  HCM2V-KOH-NISSU PIC X(6).
                04  HCM2V-KOH-TENSU PIC X(13).
                04  HCM2V-KOH-ITBFTN    PIC X(13).
              03  HCM2V-KOH-SYOG.
                04  HCM2V-KOH-SYOKENSU  PIC X(6).
                04  HCM2V-KOH-SYONISSU  PIC X(6).
                04  HCM2V-KOH-SYOKINGAK PIC X(13).
                04  HCM2V-KOH-SYOFTNGAK PIC X(13).
              03  HCM2V-KOH-NUM   PIC X(3).
              03  HCM2V-KOH-NAME  PIC X(12).
            02  HCM2V-TITLE1    PIC X(10).
            02  HCM2V-MOJI  PIC X(10).
            02  HCM2V-TITLE2
                    PIC X(4)    OCCURS  2 TIMES.
