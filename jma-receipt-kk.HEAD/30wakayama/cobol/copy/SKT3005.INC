        01  SKT3005.
            02  SKT3005-PAGE  PIC X(4).
            02  SKT3005-PRTYM PIC X(16).
            02  SKT3005-HKNJANUM  PIC X(8).
            02  SKT3005-HKNJANAME PIC X(100).
            02  SKT3005-SEIYMD    PIC X(22).
            02  SKT3005-HOSPCD    PIC X(20).
            02  SKT3005-ADRS  PIC X(100).
            02  SKT3005-HOSPNAME  PIC X(100).
            02  SKT3005-KAISETUNAME   PIC X(30).
            02  SKT3005-HKN-TBL           OCCURS  2 TIMES.
              03  SKT3005-HKN-G     OCCURS  2 TIMES.
                04  SKT3005-HKN-KENSU PIC X(6).
                04  SKT3005-HKN-NISSU PIC X(6).
                04  SKT3005-HKN-TENSU PIC X(13).
                04  SKT3005-HKN-ITBFTN    PIC X(13).
              03  SKT3005-HKN-SYOG.
                04  SKT3005-HKN-SYOKENSU  PIC X(6).
                04  SKT3005-HKN-SYONISSU  PIC X(6).
                04  SKT3005-HKN-SYOKINGAK PIC X(13).
                04  SKT3005-HKN-SYOFTNGAK PIC X(13).
            02  SKT3005-KOH-TBL           OCCURS  12 TIMES.
              03  SKT3005-KOH-G     OCCURS  2 TIMES.
                04  SKT3005-KOH-KENSU PIC X(6).
                04  SKT3005-KOH-NISSU PIC X(6).
                04  SKT3005-KOH-TENSU PIC X(13).
                04  SKT3005-KOH-ITBFTN    PIC X(13).
              03  SKT3005-KOH-SYOG.
                04  SKT3005-KOH-SYOKENSU  PIC X(6).
                04  SKT3005-KOH-SYONISSU  PIC X(6).
                04  SKT3005-KOH-SYOKINGAK PIC X(13).
                04  SKT3005-KOH-SYOFTNGAK PIC X(13).
              03  SKT3005-KOH-NUM   PIC X(3).
              03  SKT3005-KOH-NAME  PIC X(12).
            02  SKT3005-TITLE PIC X(50).
            02  SKT3005-TITLE1    PIC X(10).
            02  SKT3005-MOJI  PIC X(10).
            02  SKT3005-SRYKA PIC X(40).
            02  SKT3005-TEL   PIC X(15).
