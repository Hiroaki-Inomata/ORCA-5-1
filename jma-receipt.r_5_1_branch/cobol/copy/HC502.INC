        01  HC502.
            02  HC502-TITLE PIC X(30).
            02  HC502-MAYAKU    PIC X(2).
            02  HC502-PTNUM PIC X(20).
            02  HC502-KANANAME  PIC X(40).
            02  HC502-NAME  PIC X(40).
            02  HC502-BIRTH PIC X(22).
            02  HC502-SEX   PIC X(2).
            02  HC502-AGE   PIC X(8).
            02  HC502-BYOTO PIC X(20).
            02  HC502-ROOMNO    PIC X(6).
            02  HC502-SRYYMD    PIC X(22).
            02  HC502-KAISU PIC X(8).
            02  HC502-SYSYMD    PIC X(10).
            02  HC502-TIME  PIC X(5).
            02  HC502-SRYKA PIC X(20).
            02  HC502-DRNAME    PIC X(30).
            02  HC502-NAIYO1    PIC X(88).
            02  HC502-NAIYO
                    PIC X(106)  OCCURS  34 TIMES.
            02  HC502-BIKOH
                    PIC X(110)  OCCURS  3 TIMES.
            02  HC502-OPID  PIC X(16).
