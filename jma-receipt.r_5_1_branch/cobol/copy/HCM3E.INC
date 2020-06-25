        01  HCM3E.
            02  HCM3E-PAGE  PIC X(4).
            02  HCM3E-PRTYM PIC X(16).
            02  HCM3E-HKNJANUM  PIC X(8).
            02  HCM3E-HKNJANAME PIC X(20).
            02  HCM3E-SEIYMD    PIC X(22).
            02  HCM3E-HOSPCD    PIC X(20).
            02  HCM3E-ADRS  PIC X(100).
            02  HCM3E-HOSPNAME  PIC X(100).
            02  HCM3E-KAISETUNAME   PIC X(30).
            02  HCM3E-HKN-TBL.
              03  HCM3E-HKN-TBLG          OCCURS  7 TIMES.
                04  HCM3E-HKN-G     OCCURS  2 TIMES.
                  05  HCM3E-KENSU PIC X(6).
                  05  HCM3E-NISSU PIC X(6).
                  05  HCM3E-TENSU PIC X(13).
                  05  HCM3E-ITBFTN    PIC X(13).
            02  HCM3E-SYO-TBL.
              03  HCM3E-SYO-TBLG          OCCURS  7 TIMES.
                04  HCM3E-SYOKENSU  PIC X(6).
                04  HCM3E-SYONISSU  PIC X(6).
                04  HCM3E-SYOKINGAK PIC X(13).
                04  HCM3E-SYOFTNGAK PIC X(13).
            02  HCM3E-TITLE PIC X(50).
            02  HCM3E-TITLE1    PIC X(10).
            02  HCM3E-MOJI  PIC X(10).
            02  HCM3E-SRYKA PIC X(40).
