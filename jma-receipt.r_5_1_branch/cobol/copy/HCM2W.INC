        01  HCM2W.
            02  HCM2W-PAGE  PIC X(4).
            02  HCM2W-PRTYM PIC X(16).
            02  HCM2W-HKNJANUM  PIC X(8).
            02  HCM2W-HKNJANAME PIC X(20).
            02  HCM2W-SEIYMD    PIC X(22).
            02  HCM2W-HOSPCD    PIC X(20).
            02  HCM2W-ADRS  PIC X(100).
            02  HCM2W-HOSPNAME  PIC X(100).
            02  HCM2W-KAISETUNAME   PIC X(30).
            02  HCM2W-HKN-TBL.
              03  HCM2W-HKN-TBLG          OCCURS  9 TIMES.
                04  HCM2W-HKN-G     OCCURS  2 TIMES.
                  05  HCM2W-KENSU PIC X(6).
                  05  HCM2W-NISSU PIC X(6).
                  05  HCM2W-TENSU PIC X(13).
                  05  HCM2W-ITBFTN    PIC X(13).
            02  HCM2W-SYO-TBL.
              03  HCM2W-SYO-TBLG          OCCURS  9 TIMES.
                04  HCM2W-SYOKENSU  PIC X(6).
                04  HCM2W-SYONISSU  PIC X(6).
                04  HCM2W-SYOKINGAK PIC X(13).
                04  HCM2W-SYOFTNGAK PIC X(13).
            02  HCM2W-TITLE PIC X(50).
            02  HCM2W-TITLE1    PIC X(10).
            02  HCM2W-MOJI  PIC X(10).
            02  HCM2W-TITLE2    PIC X(4).
