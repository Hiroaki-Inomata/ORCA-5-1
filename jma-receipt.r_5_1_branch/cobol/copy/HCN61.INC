        01  HCN61.
            02  HCN61-SKYYM PIC X(16).
            02  HCN61-BUNBENKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN61-HKNJANUM.
              03  HCN61-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN61-HOSPCD.
              03  HCN61-HOSPCD-S
                      PIC X(1)    OCCURS  10 TIMES.
            02  HCN61-SANKANUM  PIC X(10).
            02  HCN61-HOSPADRS
                    PIC X(50)   OCCURS  2 TIMES.
            02  HCN61-HOSPNAME
                    PIC X(40)   OCCURS  2 TIMES.
            02  HCN61-KANJYA            OCCURS  3 TIMES.
              03  HCN61-TEISYUTUSAKI
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61-HONKZKKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61-KIGO  PIC X(22).
              03  HCN61-NUM   PIC X(22).
              03  HCN61-NAME  PIC X(34).
              03  HCN61-KANANAME
                      PIC X(34)   OCCURS  2 TIMES.
              03  HCN61-BIRTHDAY.
                04  HCN61-BIRTHG
                        PIC X(2)    OCCURS  2 TIMES.
                04  HCN61-BIRTHY    PIC X(2).
                04  HCN61-BIRTHM    PIC X(2).
                04  HCN61-BIRTHD    PIC X(2).
              03  HCN61-ZAIWEEK   PIC X(2).
              03  HCN61-CHILD-BIRTHDAY.
                04  HCN61-CBIRTHG   PIC X(2).
                04  HCN61-CBIRTHY   PIC X(2).
                04  HCN61-CBIRTHM   PIC X(2).
                04  HCN61-CBIRTHD   PIC X(2).
              03  HCN61-SIZANKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61-BIRTHCNT  PIC X(1).
              03  HCN61-NYUINDAY  PIC X(3).
              03  HCN61-SANKAKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61-NYU-MONEY PIC X(10).
              03  HCN61-RMSGK-MONEY   PIC X(10).
              03  HCN61-KAIJO-MONEY   PIC X(10).
              03  HCN61-BUNBEN-MONEY  PIC X(10).
              03  HCN61-SHIN-MONEY    PIC X(10).
              03  HCN61-KNS-MONEY PIC X(10).
              03  HCN61-SYC-MONEY PIC X(10).
              03  HCN61-SNK-MONEY PIC X(10).
              03  HCN61-ETC-MONEY PIC X(10).
              03  HCN61-ICBFTN-MONEY  PIC X(10).
              03  HCN61-NINPUFTN-MONEY    PIC X(10).
              03  HCN61-DAIRI-MONEY   PIC X(10).
              03  HCN61-BIKOU PIC X(40).
              03  HCN61-BIKOU2
                      PIC X(40)   OCCURS  2 TIMES.
            02  HCN61-GCNT  PIC X(3).
            02  HCN61-GBIRTHCNT PIC X(3).
            02  HCN61-GDAIRI-MONEY  PIC X(10).
            02  HCN61-PAGE  PIC X(7).
