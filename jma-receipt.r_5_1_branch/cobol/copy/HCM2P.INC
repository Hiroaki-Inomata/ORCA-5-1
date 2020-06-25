        01  HCM2P.
            02  HCM2P-PAGE  PIC X(4).
            02  HCM2P-HKNJANUM  PIC X(8).
            02  HCM2P-HOSPCD    PIC X(20).
            02  HCM2P-TITLE PIC X(50).
            02  HCM2P-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2P-HKN-G     OCCURS  2 TIMES.
                04  HCM2P-HKN-KENSU PIC X(6).
                04  HCM2P-HKN-NISSU PIC X(6).
                04  HCM2P-HKN-TENSU PIC X(13).
                04  HCM2P-HKN-ITBFTN    PIC X(13).
              03  HCM2P-HKN-SYOG.
                04  HCM2P-HKN-SYOKENSU  PIC X(6).
                04  HCM2P-HKN-SYONISSU  PIC X(6).
                04  HCM2P-HKN-SYOKINGAK PIC X(13).
                04  HCM2P-HKN-SYOFTNGAK PIC X(13).
            02  HCM2P-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2P-KOH-G     OCCURS  2 TIMES.
                04  HCM2P-KOH-KENSU PIC X(6).
                04  HCM2P-KOH-NISSU PIC X(6).
                04  HCM2P-KOH-TENSU PIC X(13).
                04  HCM2P-KOH-ITBFTN    PIC X(13).
              03  HCM2P-KOH-SYOG.
                04  HCM2P-KOH-SYOKENSU  PIC X(6).
                04  HCM2P-KOH-SYONISSU  PIC X(6).
                04  HCM2P-KOH-SYOKINGAK PIC X(13).
                04  HCM2P-KOH-SYOFTNGAK PIC X(13).
              03  HCM2P-KOH-NUM   PIC X(3).
              03  HCM2P-KOH-NAME  PIC X(12).
