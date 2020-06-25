        01  HCM2O.
            02  HCM2O-PAGE  PIC X(4).
            02  HCM2O-PRTYM PIC X(16).
            02  HCM2O-HKNJANUM  PIC X(8).
            02  HCM2O-HKNJANAME PIC X(20).
            02  HCM2O-SEIYMD    PIC X(22).
            02  HCM2O-HOSPCD    PIC X(20).
            02  HCM2O-ADRS  PIC X(100).
            02  HCM2O-HOSPNAME  PIC X(100).
            02  HCM2O-KAISETUNAME   PIC X(30).
            02  HCM2O-HKN-TBL.
              03  HCM2O-HKN-TBLG          OCCURS  12 TIMES.
                04  HCM2O-HKN-G     OCCURS  2 TIMES.
                  05  HCM2O-KENSU PIC X(6).
                  05  HCM2O-NISSU PIC X(6).
                  05  HCM2O-TENSU PIC X(13).
                  05  HCM2O-ITBFTN    PIC X(13).
            02  HCM2O-SYO-TBL.
              03  HCM2O-SYO-TBLG          OCCURS  12 TIMES.
                04  HCM2O-SYOKENSU  PIC X(6).
                04  HCM2O-SYONISSU  PIC X(6).
                04  HCM2O-SYOKINGAK PIC X(13).
                04  HCM2O-SYOFTNGAK PIC X(13).
            02  HCM2O-TITLE PIC X(50).
