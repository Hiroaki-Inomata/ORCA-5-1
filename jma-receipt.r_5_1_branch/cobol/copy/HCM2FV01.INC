        01  HCM2FV01.
            02  HCM2FV01-PAGE   PIC X(4).
            02  HCM2FV01-HKNJANUM   PIC X(8).
            02  HCM2FV01-HOSPCD PIC X(20).
            02  HCM2FV01-TITLE  PIC X(36).
            02  HCM2FV01-HKN-TBL        OCCURS  2 TIMES.
              03  HCM2FV01-HKN-G          OCCURS  2 TIMES.
                04  HCM2FV01-HKN-KENSU  PIC X(6).
                04  HCM2FV01-HKN-NISSU  PIC X(6).
                04  HCM2FV01-HKN-TENSU  PIC X(13).
                04  HCM2FV01-HKN-ITBFTN PIC X(13).
              03  HCM2FV01-HKN-SYOG.
                04  HCM2FV01-HKN-SYOKENSU   PIC X(6).
                04  HCM2FV01-HKN-SYONISSU   PIC X(6).
                04  HCM2FV01-HKN-SYOKINGAK  PIC X(13).
                04  HCM2FV01-HKN-SYOFTNGAK  PIC X(13).
            02  HCM2FV01-KOH-TBL        OCCURS  2 TIMES.
              03  HCM2FV01-KOH-G          OCCURS  2 TIMES.
                04  HCM2FV01-KOH-KENSU  PIC X(6).
                04  HCM2FV01-KOH-YKZFTN PIC X(8).
                04  HCM2FV01-KOH-NISSU  PIC X(6).
                04  HCM2FV01-KOH-TENSU  PIC X(13).
                04  HCM2FV01-KOH-ITBFTN PIC X(13).
              03  HCM2FV01-KOH-SYOG.
                04  HCM2FV01-KOH-SYOKENSU   PIC X(6).
                04  HCM2FV01-KOH-SYONISSU   PIC X(6).
                04  HCM2FV01-KOH-SYOKINGAK  PIC X(13).
                04  HCM2FV01-KOH-SYOFTNGAK  PIC X(13).
              03  HCM2FV01-KOH-NUM    PIC X(3).
              03  HCM2FV01-KOH-NAME   PIC X(12).
