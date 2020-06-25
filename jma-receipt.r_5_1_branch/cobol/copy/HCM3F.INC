        01  HCM3F.
            02  HCM3F-PAGE  PIC X(4).
            02  HCM3F-HKNJANUM  PIC X(8).
            02  HCM3F-HOSPCD    PIC X(20).
            02  HCM3F-TITLE PIC X(50).
            02  HCM3F-KOH-TBL           OCCURS  2 TIMES.
              03  HCM3F-KOH-G     OCCURS  2 TIMES.
                04  HCM3F-KOH-KENSU PIC X(6).
                04  HCM3F-KOH-NISSU PIC X(6).
                04  HCM3F-KOH-TENSU PIC X(13).
                04  HCM3F-KOH-ITBFTN    PIC X(13).
              03  HCM3F-KOH-SYOG.
                04  HCM3F-KOH-SYOKENSU  PIC X(6).
                04  HCM3F-KOH-SYONISSU  PIC X(6).
                04  HCM3F-KOH-SYOKINGAK PIC X(13).
                04  HCM3F-KOH-SYOFTNGAK PIC X(13).
              03  HCM3F-KOH-NUM   PIC X(3).
              03  HCM3F-KOH-NAME  PIC X(12).
            02  HCM3F-TITLE1    PIC X(10).
            02  HCM3F-MOJI  PIC X(10).
            02  HCM3F-SRYKA PIC X(40).
