        01  HCM2M.
            02  HCM2M-PAGE  PIC X(4).
            02  HCM2M-PRTYM PIC X(16).
            02  HCM2M-HKNJANUM  PIC X(8).
            02  HCM2M-HKNJANAME PIC X(20).
            02  HCM2M-SEIYMD    PIC X(22).
            02  HCM2M-HOSPCD    PIC X(20).
            02  HCM2M-ADRS  PIC X(100).
            02  HCM2M-HOSPNAME  PIC X(100).
            02  HCM2M-KAISETUNAME   PIC X(30).
            02  HCM2M-HKN-TBL.
              03  HCM2M-HKN-TBLG          OCCURS  9 TIMES.
                04  HCM2M-HKN-G     OCCURS  2 TIMES.
                  05  HCM2M-KENSU PIC X(6).
                  05  HCM2M-NISSU PIC X(6).
                  05  HCM2M-TENSU PIC X(13).
                  05  HCM2M-ITBFTN    PIC X(13).
            02  HCM2M-SYO-TBL.
              03  HCM2M-SYO-TBLG          OCCURS  9 TIMES.
                04  HCM2M-SYOKENSU  PIC X(6).
                04  HCM2M-SYONISSU  PIC X(6).
                04  HCM2M-SYOKINGAK PIC X(13).
                04  HCM2M-SYOFTNGAK PIC X(13).
            02  HCM2M-TITLE PIC X(50).
