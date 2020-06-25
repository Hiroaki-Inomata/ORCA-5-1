        01  HCM3G.
            02  HCM3G-PAGE  PIC X(4).
            02  HCM3G-PRTYM PIC X(16).
            02  HCM3G-HKNJANUM  PIC X(8).
            02  HCM3G-HKNJANAME PIC X(100).
            02  HCM3G-SEIYMD    PIC X(22).
            02  HCM3G-HOSPCD    PIC X(20).
            02  HCM3G-ADRS  PIC X(100).
            02  HCM3G-HOSPNAME  PIC X(100).
            02  HCM3G-KAISETUNAME   PIC X(30).
            02  HCM3G-HKN-TBL           OCCURS  2 TIMES.
              03  HCM3G-HKN-G     OCCURS  2 TIMES.
                04  HCM3G-HKN-KENSU PIC X(6).
                04  HCM3G-HKN-NISSU PIC X(6).
                04  HCM3G-HKN-TENSU PIC X(13).
                04  HCM3G-HKN-ITBFTN    PIC X(13).
              03  HCM3G-HKN-SYOG.
                04  HCM3G-HKN-SYOKENSU  PIC X(6).
                04  HCM3G-HKN-SYONISSU  PIC X(6).
                04  HCM3G-HKN-SYOKINGAK PIC X(13).
                04  HCM3G-HKN-SYOFTNGAK PIC X(13).
            02  HCM3G-KOH-TBL           OCCURS  2 TIMES.
              03  HCM3G-KOH-G     OCCURS  2 TIMES.
                04  HCM3G-KOH-KENSU PIC X(6).
                04  HCM3G-KOH-NISSU PIC X(6).
                04  HCM3G-KOH-TENSU PIC X(13).
                04  HCM3G-KOH-ITBFTN    PIC X(13).
              03  HCM3G-KOH-SYOG.
                04  HCM3G-KOH-SYOKENSU  PIC X(6).
                04  HCM3G-KOH-SYONISSU  PIC X(6).
                04  HCM3G-KOH-SYOKINGAK PIC X(13).
                04  HCM3G-KOH-SYOFTNGAK PIC X(13).
              03  HCM3G-KOH-NUM   PIC X(3).
              03  HCM3G-KOH-NAME  PIC X(12).
            02  HCM3G-TITLE PIC X(50).
            02  HCM3G-TITLE1    PIC X(10).
            02  HCM3G-MOJI  PIC X(10).
            02  HCM3G-SRYKA PIC X(40).
