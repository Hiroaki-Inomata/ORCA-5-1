        01  HC02Q.
            02  HC02Q-PTNUM PIC X(20).
            02  HC02Q-FTNJA PIC X(16).
            02  HC02Q-FTNJAN    PIC X(20).
            02  HC02Q-JKYSNUM   PIC X(14).
            02  HC02Q-JKYSNUMN  PIC X(20).
            02  HC02Q-JKYSNUM12 PIC X(24).
            02  HC02Q-HKNJA PIC X(16).
            02  HC02Q-KIGONUM   PIC X(32).
            02  HC02Q-BIRTH PIC X(22).
            02  HC02Q-KANANAME  PIC X(30).
            02  HC02Q-KANANAME2 PIC X(60).
            02  HC02Q-NAME  PIC X(26).
            02  HC02Q-NAME2 PIC X(52).
            02  HC02Q-SEX   PIC X(2).
            02  HC02Q-HONKZKKBN PIC X(16).
            02  HC02Q-KOFYMD    PIC X(22).
            02  HC02Q-KIKYY PIC X(4).
            02  HC02Q-KIKMM PIC X(4).
            02  HC02Q-KIKDD PIC X(4).
            02  HC02Q-HOSPADRS  PIC X(80).
            02  HC02Q-HOSPNAME  PIC X(80).
            02  HC02Q-HOSPTEL   PIC X(15).
            02  HC02Q-DRNAME    PIC X(26).
            02  HC02Q-FTNJA2    PIC X(16).
            02  HC02Q-FTNJA2N   PIC X(20).
            02  HC02Q-JKYSNUM2  PIC X(14).
            02  HC02Q-JKYSNUM2N PIC X(20).
            02  HC02Q-JKYSNUM212    PIC X(24).
            02  HC02Q-NAIYO
                    PIC X(86)   OCCURS  20 TIMES.
            02  HC02Q-MENJYO    PIC X(6).
            02  HC02Q-BIKOH
                    PIC X(86)   OCCURS  9 TIMES.
            02  HC02Q-HENKOUKA  PIC X(16).
            02  HC02Q-HKNINAME  PIC X(16).
            02  HC02Q-QR
                    PIC X(1024) OCCURS  3 TIMES.
            02  HC02Q-SRYKAMEI  PIC X(10).
            02  HC02Q-PAGE  PIC X(5).
            02  HC02Q-FTNRATE   PIC X(3).
            02  HC02Q-FTNWARI   PIC X(2).
            02  HC02Q-MIDASHI   PIC X(20).
            02  HC02Q-KIGO  PIC X(32).
            02  HC02Q-NUM   PIC X(32).
            02  HC02Q-MIDASHI2.
              03  HC02Q-MIDASHI2-S
                      PIC X(44)   OCCURS  2 TIMES.
            02  HC02Q-PREFNUM.
              03  HC02Q-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HC02Q-TENHYOKBN PIC X(2).
            02  HC02Q-HOSPCD.
              03  HC02Q-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            02  HC02Q-JKYSNUM15 PIC X(30).
            02  HC02Q-JKYSNUM215    PIC X(30).
