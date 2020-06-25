        01  HC02.
            02  HC02-PTNUM  PIC X(20).
            02  HC02-FTNJA  PIC X(16).
            02  HC02-FTNJAN PIC X(20).
            02  HC02-JKYSNUM    PIC X(14).
            02  HC02-JKYSNUMN   PIC X(20).
            02  HC02-JKYSNUM12  PIC X(24).
            02  HC02-HKNJA  PIC X(16).
            02  HC02-KIGONUM    PIC X(32).
            02  HC02-BIRTH  PIC X(22).
            02  HC02-KANANAME   PIC X(30).
            02  HC02-KANANAME2  PIC X(60).
            02  HC02-NAME   PIC X(26).
            02  HC02-NAME2  PIC X(52).
            02  HC02-SEX    PIC X(2).
            02  HC02-HONKZKKBN  PIC X(16).
            02  HC02-KOFYMD PIC X(22).
            02  HC02-KIKYY  PIC X(4).
            02  HC02-KIKMM  PIC X(4).
            02  HC02-KIKDD  PIC X(4).
            02  HC02-HOSPADRS   PIC X(80).
            02  HC02-HOSPNAME   PIC X(80).
            02  HC02-HOSPTEL    PIC X(15).
            02  HC02-DRNAME PIC X(26).
            02  HC02-FTNJA2 PIC X(16).
            02  HC02-FTNJA2N    PIC X(20).
            02  HC02-JKYSNUM2   PIC X(14).
            02  HC02-JKYSNUM2N  PIC X(20).
            02  HC02-JKYSNUM212 PIC X(24).
            02  HC02-NAIYO
                    PIC X(86)   OCCURS  20 TIMES.
            02  HC02-MENJYO PIC X(6).
            02  HC02-BIKOH
                    PIC X(86)   OCCURS  9 TIMES.
            02  HC02-HENKOUKA   PIC X(16).
            02  HC02-HKNINAME   PIC X(16).
            02  HC02-SRYKAMEI   PIC X(10).
            02  HC02-PAGE   PIC X(5).
            02  HC02-FTNRATE    PIC X(3).
            02  HC02-FTNWARI    PIC X(2).
            02  HC02-MIDASHI    PIC X(20).
            02  HC02-KIGO   PIC X(32).
            02  HC02-NUM    PIC X(32).
            02  HC02-MIDASHI2.
              03  HC02-MIDASHI2-S
                      PIC X(44)   OCCURS  2 TIMES.
            02  HC02-PREFNUM.
              03  HC02-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HC02-TENHYOKBN  PIC X(2).
            02  HC02-HOSPCD.
              03  HC02-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            02  HC02-JKYSNUM15  PIC X(30).
            02  HC02-JKYSNUM215 PIC X(30).
