        01  HCM2H.
            02  HCM2H-HOSPCD    PIC X(20).
            02  HCM2H-TITLE PIC X(36).
            02  HCM2H-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2H-HKN-G     OCCURS  2 TIMES.
                04  HCM2H-HKN-KENSU PIC X(6).
                04  HCM2H-HKN-NISSU PIC X(6).
                04  HCM2H-HKN-TENSU PIC X(10).
                04  HCM2H-HKN-ITBFTN    PIC X(9).
              03  HCM2H-HKN-SYOG.
                04  HCM2H-HKN-SYOKENSU  PIC X(6).
                04  HCM2H-HKN-SYONISSU  PIC X(6).
                04  HCM2H-HKN-SYOKINGAK PIC X(9).
                04  HCM2H-HKN-SYOFTNGAK PIC X(9).
            02  HCM2H-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2H-KOH-G     OCCURS  2 TIMES.
                04  HCM2H-KOH-KENSU PIC X(6).
                04  HCM2H-KOH-YKZFTN    PIC X(8).
                04  HCM2H-KOH-NISSU PIC X(6).
                04  HCM2H-KOH-TENSU PIC X(10).
                04  HCM2H-KOH-ITBFTN    PIC X(9).
              03  HCM2H-KOH-SYOG.
                04  HCM2H-KOH-SYOKENSU  PIC X(6).
                04  HCM2H-KOH-SYONISSU  PIC X(6).
                04  HCM2H-KOH-SYOKINGAK PIC X(9).
                04  HCM2H-KOH-SYOFTNGAK PIC X(9).
              03  HCM2H-KOH-NUM   PIC X(3).
              03  HCM2H-KOH-NAME  PIC X(12).
