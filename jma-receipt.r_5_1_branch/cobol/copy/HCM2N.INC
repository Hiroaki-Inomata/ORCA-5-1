        01  HCM2N.
            02  HCM2N-PAGE  PIC X(4).
            02  HCM2N-HKNJANUM  PIC X(8).
            02  HCM2N-HOSPCD    PIC X(20).
            02  HCM2N-TITLE PIC X(50).
            02  HCM2N-HKN-TBL           OCCURS  2 TIMES.
              03  HCM2N-HKN-G     OCCURS  2 TIMES.
                04  HCM2N-HKN-KENSU PIC X(6).
                04  HCM2N-HKN-NISSU PIC X(6).
                04  HCM2N-HKN-TENSU PIC X(13).
                04  HCM2N-HKN-ITBFTN    PIC X(13).
              03  HCM2N-HKN-SYOG.
                04  HCM2N-HKN-SYOKENSU  PIC X(6).
                04  HCM2N-HKN-SYONISSU  PIC X(6).
                04  HCM2N-HKN-SYOKINGAK PIC X(13).
                04  HCM2N-HKN-SYOFTNGAK PIC X(13).
            02  HCM2N-KOH-TBL           OCCURS  2 TIMES.
              03  HCM2N-KOH-G     OCCURS  2 TIMES.
                04  HCM2N-KOH-KENSU PIC X(6).
                04  HCM2N-KOH-NISSU PIC X(6).
                04  HCM2N-KOH-TENSU PIC X(13).
                04  HCM2N-KOH-ITBFTN    PIC X(13).
              03  HCM2N-KOH-SYOG.
                04  HCM2N-KOH-SYOKENSU  PIC X(6).
                04  HCM2N-KOH-SYONISSU  PIC X(6).
                04  HCM2N-KOH-SYOKINGAK PIC X(13).
                04  HCM2N-KOH-SYOFTNGAK PIC X(13).
              03  HCM2N-KOH-NUM   PIC X(3).
              03  HCM2N-KOH-NAME  PIC X(12).
