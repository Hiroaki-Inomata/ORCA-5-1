        01  HCM2F.
            02  HCM2F-HOSPCD    PIC X(20).
            02  HCM2F-TITLE PIC X(36).
            02  HCM2F-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2F-HKN-G     OCCURS  2 TIMES.
                04  HCM2F-HKN-KENSU PIC X(6).
                04  HCM2F-HKN-NISSU PIC X(6).
                04  HCM2F-HKN-TENSU PIC X(10).
                04  HCM2F-HKN-ITBFTN    PIC X(9).
              03  HCM2F-HKN-SYOG.
                04  HCM2F-HKN-SYOKENSU  PIC X(6).
                04  HCM2F-HKN-SYONISSU  PIC X(6).
                04  HCM2F-HKN-SYOKINGAK PIC X(9).
                04  HCM2F-HKN-SYOFTNGAK PIC X(9).
            02  HCM2F-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2F-KOH-G     OCCURS  2 TIMES.
                04  HCM2F-KOH-KENSU PIC X(6).
                04  HCM2F-KOH-YKZFTN    PIC X(8).
                04  HCM2F-KOH-NISSU PIC X(6).
                04  HCM2F-KOH-TENSU PIC X(10).
                04  HCM2F-KOH-ITBFTN    PIC X(9).
              03  HCM2F-KOH-SYOG.
                04  HCM2F-KOH-SYOKENSU  PIC X(6).
                04  HCM2F-KOH-SYONISSU  PIC X(6).
                04  HCM2F-KOH-SYOKINGAK PIC X(9).
                04  HCM2F-KOH-SYOFTNGAK PIC X(9).
              03  HCM2F-KOH-NUM   PIC X(3).
              03  HCM2F-KOH-NAME  PIC X(12).
