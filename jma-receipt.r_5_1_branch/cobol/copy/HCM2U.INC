        01  HCM2U.
            02  HCM2U-PAGE  PIC X(4).
            02  HCM2U-PRTYM PIC X(16).
            02  HCM2U-HKNJANUM  PIC X(8).
            02  HCM2U-HKNJANAME PIC X(20).
            02  HCM2U-SEIYMD    PIC X(22).
            02  HCM2U-HOSPCD    PIC X(20).
            02  HCM2U-ADRS  PIC X(100).
            02  HCM2U-HOSPNAME  PIC X(100).
            02  HCM2U-KAISETUNAME   PIC X(30).
            02  HCM2U-HKN-TBL.
              03  HCM2U-HKN-TBLG          OCCURS  12 TIMES.
                04  HCM2U-HKN-G     OCCURS  2 TIMES.
                  05  HCM2U-KENSU PIC X(6).
                  05  HCM2U-NISSU PIC X(6).
                  05  HCM2U-TENSU PIC X(13).
                  05  HCM2U-ITBFTN    PIC X(13).
            02  HCM2U-SYO-TBL.
              03  HCM2U-SYO-TBLG          OCCURS  12 TIMES.
                04  HCM2U-SYOKENSU  PIC X(6).
                04  HCM2U-SYONISSU  PIC X(6).
                04  HCM2U-SYOKINGAK PIC X(13).
                04  HCM2U-SYOFTNGAK PIC X(13).
            02  HCM2U-TITLE PIC X(50).
            02  HCM2U-TITLE1    PIC X(10).
            02  HCM2U-MOJI  PIC X(10).
            02  HCM2U-TITLE2    PIC X(4).
