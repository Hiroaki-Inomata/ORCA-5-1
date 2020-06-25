        01  HCN61V01.
            02  HCN61V01-SKYYM  PIC X(16).
            02  HCN61V01-BUNBENKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN61V01-HKNJANUM.
              03  HCN61V01-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN61V01-HOSPCD.
              03  HCN61V01-HOSPCD-S
                      PIC X(1)    OCCURS  10 TIMES.
            02  HCN61V01-SANKANUM   PIC X(10).
            02  HCN61V01-HOSPADRS
                    PIC X(50)   OCCURS  2 TIMES.
            02  HCN61V01-HOSPNAME
                    PIC X(40)   OCCURS  2 TIMES.
            02  HCN61V01-KANJYA         OCCURS  3 TIMES.
              03  HCN61V01-TEISYUTUSAKI
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61V01-HONKZKKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61V01-KIGO   PIC X(22).
              03  HCN61V01-KIGO2
                      PIC X(20)   OCCURS  2 TIMES.
              03  HCN61V01-NUM    PIC X(22).
              03  HCN61V01-NUM2
                      PIC X(20)   OCCURS  2 TIMES.
              03  HCN61V01-NAME   PIC X(34).
              03  HCN61V01-NAME2  PIC X(40).
              03  HCN61V01-KANANAME
                      PIC X(34)   OCCURS  2 TIMES.
              03  HCN61V01-BIRTHDAY.
                04  HCN61V01-BIRTHG
                        PIC X(2)    OCCURS  2 TIMES.
                04  HCN61V01-BIRTHY PIC X(2).
                04  HCN61V01-BIRTHM PIC X(2).
                04  HCN61V01-BIRTHD PIC X(2).
              03  HCN61V01-ZAIWEEK    PIC X(2).
              03  HCN61V01-CHILD-BIRTHDAY.
                04  HCN61V01-CBIRTHG    PIC X(2).
                04  HCN61V01-CBIRTHY    PIC X(2).
                04  HCN61V01-CBIRTHM    PIC X(2).
                04  HCN61V01-CBIRTHD    PIC X(2).
              03  HCN61V01-SIZANKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61V01-BIRTHCNT   PIC X(1).
              03  HCN61V01-NYUINDAY   PIC X(3).
              03  HCN61V01-SANKAKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61V01-NYU-MONEY  PIC X(10).
              03  HCN61V01-RMSGK-MONEY    PIC X(10).
              03  HCN61V01-KAIJO-MONEY    PIC X(10).
              03  HCN61V01-BUNBEN-MONEY   PIC X(10).
              03  HCN61V01-SHIN-MONEY PIC X(10).
              03  HCN61V01-KNS-MONEY  PIC X(10).
              03  HCN61V01-SYC-MONEY  PIC X(10).
              03  HCN61V01-SNK-MONEY  PIC X(10).
              03  HCN61V01-ETC-MONEY  PIC X(10).
              03  HCN61V01-ICBFTN-MONEY   PIC X(10).
              03  HCN61V01-NINPUFTN-MONEY PIC X(10).
              03  HCN61V01-DAIRI-MONEY    PIC X(10).
              03  HCN61V01-BIKOU  PIC X(40).
              03  HCN61V01-BIKOU2
                      PIC X(40)   OCCURS  2 TIMES.
            02  HCN61V01-GCNT   PIC X(3).
            02  HCN61V01-GBIRTHCNT  PIC X(3).
            02  HCN61V01-GDAIRI-MONEY   PIC X(10).
            02  HCN61V01-PAGE   PIC X(7).
