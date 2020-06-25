        01  HCN61V02.
            02  HCN61V02-SKYYM  PIC X(16).
            02  HCN61V02-BUNBENKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCN61V02-HKNJANUM.
              03  HCN61V02-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCN61V02-HOSPCD.
              03  HCN61V02-HOSPCD-S
                      PIC X(1)    OCCURS  10 TIMES.
            02  HCN61V02-SANKANUM.
              03  HCN61V02-SANKANUM-S
                      PIC X(1)    OCCURS  10 TIMES.
            02  HCN61V02-HOSPADRS
                    PIC X(50)   OCCURS  2 TIMES.
            02  HCN61V02-HOSPNAME
                    PIC X(40)   OCCURS  2 TIMES.
            02  HCN61V02-KANJYA.
              03  HCN61V02-TEISYUTUSAKI
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61V02-HONKZKKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  HCN61V02-KIGO   PIC X(22).
              03  HCN61V02-KIGO2
                      PIC X(20)   OCCURS  2 TIMES.
              03  HCN61V02-NUM    PIC X(22).
              03  HCN61V02-NUM2
                      PIC X(20)   OCCURS  2 TIMES.
              03  HCN61V02-NAME   PIC X(34).
              03  HCN61V02-NAME2  PIC X(40).
              03  HCN61V02-KANANAME
                      PIC X(34)   OCCURS  2 TIMES.
              03  HCN61V02-BIRTHDAY.
                04  HCN61V02-BIRTHG
                        PIC X(2)    OCCURS  2 TIMES.
                04  HCN61V02-BIRTHY PIC X(2).
                04  HCN61V02-BIRTHM PIC X(2).
                04  HCN61V02-BIRTHD PIC X(2).
              03  HCN61V02-ZAIWEEK    PIC X(2).
              03  HCN61V02-CHILD-BIRTHDAY.
                04  HCN61V02-CBIRTHG    PIC X(2).
                04  HCN61V02-CBIRTHY    PIC X(2).
                04  HCN61V02-CBIRTHM    PIC X(2).
                04  HCN61V02-CBIRTHD    PIC X(2).
              03  HCN61V02-SIZANKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61V02-BIRTHCNT   PIC X(1).
              03  HCN61V02-NYUINDAY   PIC X(3).
              03  HCN61V02-SANKAKBN
                      PIC X(2)    OCCURS  3 TIMES.
              03  HCN61V02-NYU-MONEY  PIC X(10).
              03  HCN61V02-RMSGK-MONEY    PIC X(10).
              03  HCN61V02-KAIJO-MONEY    PIC X(10).
              03  HCN61V02-BUNBEN-MONEY   PIC X(10).
              03  HCN61V02-SHIN-MONEY PIC X(10).
              03  HCN61V02-KNS-MONEY  PIC X(10).
              03  HCN61V02-SYC-MONEY  PIC X(10).
              03  HCN61V02-SNK-MONEY  PIC X(10).
              03  HCN61V02-ETC-MONEY  PIC X(10).
              03  HCN61V02-ICBFTN-MONEY   PIC X(10).
              03  HCN61V02-NINPUFTN-MONEY PIC X(10).
              03  HCN61V02-DAIRI-MONEY    PIC X(10).
              03  HCN61V02-BIKOU  PIC X(40).
              03  HCN61V02-BIKOU2
                      PIC X(40)   OCCURS  2 TIMES.
            02  HCN61V02-GCNT   PIC X(3).
            02  HCN61V02-GBIRTHCNT  PIC X(3).
            02  HCN61V02-GDAIRI-MONEY   PIC X(10).
            02  HCN61V02-PAGE   PIC X(7).
