        01  HCM66.
            02  HCM66-KIGO  PIC X(20).
            02  HCM66-KGIIRYOCD.
              03  HCM66-KGIIRYOCD-S
                      PIC X(1)    OCCURS  5 TIMES.
            02  HCM66-PRTYMG    PIC X(4).
            02  HCM66-PRTYMY.
              03  HCM66-PRTYMY-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM66-PRTYMM.
              03  HCM66-PRTYMM-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM66-HOSPSBT
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM66-KENSU     OCCURS  2 TIMES.
              03  HCM66-KENSU-S
                      PIC X(1)    OCCURS  4 TIMES.
            02  HCM66-KINGAK            OCCURS  2 TIMES.
              03  HCM66-KINGAK-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM66-SEIYMD    PIC X(22).
            02  HCM66-HOSPCD    PIC X(7).
            02  HCM66-ADRS  PIC X(100).
            02  HCM66-HOSPNAME  PIC X(100).
            02  HCM66-KAISETUNAME   PIC X(60).
            02  HCM66-TEL   PIC X(15).
