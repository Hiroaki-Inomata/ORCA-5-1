        01  HCM19.
            02  HCM19-PTNUM PIC X(20).
            02  HCM19-FTNJA PIC X(16).
            02  HCM19-FTNJAN    PIC X(20).
            02  HCM19-JKYSNUM   PIC X(14).
            02  HCM19-JKYSNUMN  PIC X(20).
            02  HCM19-JKYSNUM12 PIC X(24).
            02  HCM19-HKNJA PIC X(16).
            02  HCM19-KIGONUM   PIC X(32).
            02  HCM19-BIRTH PIC X(22).
            02  HCM19-KANANAME  PIC X(30).
            02  HCM19-KANANAME2 PIC X(60).
            02  HCM19-NAME  PIC X(26).
            02  HCM19-NAME2 PIC X(52).
            02  HCM19-SEX   PIC X(2).
            02  HCM19-HONKZKKBN PIC X(16).
            02  HCM19-KOFYMD    PIC X(22).
            02  HCM19-KIKYY PIC X(4).
            02  HCM19-KIKMM PIC X(4).
            02  HCM19-KIKDD PIC X(4).
            02  HCM19-HOSPADRS  PIC X(80).
            02  HCM19-HOSPNAME  PIC X(80).
            02  HCM19-HOSPTEL   PIC X(15).
            02  HCM19-DRNAME    PIC X(26).
            02  HCM19-FTNJA2    PIC X(16).
            02  HCM19-FTNJA2N   PIC X(20).
            02  HCM19-JKYSNUM2  PIC X(14).
            02  HCM19-JKYSNUM2N PIC X(20).
            02  HCM19-JKYSNUM212    PIC X(24).
            02  HCM19-NAIYO
                    PIC X(86)   OCCURS  20 TIMES.
            02  HCM19-MENJYO    PIC X(6).
            02  HCM19-BIKOH
                    PIC X(86)   OCCURS  9 TIMES.
            02  HCM19-HENKOUKA  PIC X(16).
            02  HCM19-HKNINAME  PIC X(16).
            02  HCM19-SRYKAMEI  PIC X(10).
            02  HCM19-PAGE  PIC X(5).
            02  HCM19-FTNRATE   PIC X(3).
            02  HCM19-FTNWARI   PIC X(2).
            02  HCM19-MIDASHI   PIC X(20).
            02  HCM19-KIGO  PIC X(32).
            02  HCM19-NUM   PIC X(32).
            02  HCM19-MIDASHI2.
              03  HCM19-MIDASHI2-S
                      PIC X(44)   OCCURS  2 TIMES.
            02  HCM19-PREFNUM.
              03  HCM19-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            02  HCM19-TENHYOKBN PIC X(2).
            02  HCM19-HOSPCD.
              03  HCM19-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCM19-JKYSNUM15 PIC X(30).
            02  HCM19-JKYSNUM215    PIC X(30).
