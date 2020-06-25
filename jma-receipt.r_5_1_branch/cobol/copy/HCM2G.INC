        01  HCM2G.
            02  HCM2G-PRTYM PIC X(16).
            02  HCM2G-HKNJANUM  PIC X(8).
            02  HCM2G-HKNJANAME PIC X(20).
            02  HCM2G-SEIYMD    PIC X(22).
            02  HCM2G-HOSPCD    PIC X(20).
            02  HCM2G-ADRS  PIC X(50).
            02  HCM2G-HOSPNAME  PIC X(100).
            02  HCM2G-KAISETUNAME   PIC X(30).
            02  HCM2G-HKN-TBL.
              03  HCM2G-HKN-TBLG          OCCURS  12 TIMES.
                04  HCM2G-HKN-G     OCCURS  2 TIMES.
                  05  HCM2G-KENSU PIC X(6).
                  05  HCM2G-NISSU PIC X(6).
                  05  HCM2G-TENSU PIC X(10).
                  05  HCM2G-YKZFTN    PIC X(8).
                  05  HCM2G-ITBFTN    PIC X(9).
            02  HCM2G-SYO-TBL.
              03  HCM2G-SYO-TBLG          OCCURS  12 TIMES.
                04  HCM2G-SYOKENSU  PIC X(6).
                04  HCM2G-SYONISSU  PIC X(6).
                04  HCM2G-SYOKINGAK PIC X(9).
                04  HCM2G-SYOFTNGAK PIC X(9).
            02  HCM2G-TITLE PIC X(36).
