        01  HCM2E.
            02  HCM2E-PRTYM PIC X(16).
            02  HCM2E-HKNJANUM  PIC X(8).
            02  HCM2E-HKNJANAME PIC X(20).
            02  HCM2E-SEIYMD    PIC X(22).
            02  HCM2E-HOSPCD    PIC X(20).
            02  HCM2E-ADRS  PIC X(50).
            02  HCM2E-HOSPNAME  PIC X(100).
            02  HCM2E-KAISETUNAME   PIC X(30).
            02  HCM2E-HKN-TBL.
              03  HCM2E-HKN-TBLG          OCCURS  9 TIMES.
                04  HCM2E-HKN-G     OCCURS  2 TIMES.
                  05  HCM2E-KENSU PIC X(6).
                  05  HCM2E-NISSU PIC X(6).
                  05  HCM2E-TENSU PIC X(10).
                  05  HCM2E-YKZFTN    PIC X(8).
                  05  HCM2E-ITBFTN    PIC X(9).
            02  HCM2E-SYO-TBL.
              03  HCM2E-SYO-TBLG          OCCURS  9 TIMES.
                04  HCM2E-SYOKENSU  PIC X(6).
                04  HCM2E-SYONISSU  PIC X(6).
                04  HCM2E-SYOKINGAK PIC X(9).
                04  HCM2E-SYOFTNGAK PIC X(9).
            02  HCM2E-TITLE PIC X(36).
