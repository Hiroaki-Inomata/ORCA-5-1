        01  HC501.
            02  HC501-TITLE PIC X(30).
            02  HC501-MAYAKU    PIC X(2).
            02  HC501-PTNUM PIC X(20).
            02  HC501-KANANAME  PIC X(40).
            02  HC501-NAME  PIC X(40).
            02  HC501-BIRTH PIC X(22).
            02  HC501-SEX   PIC X(2).
            02  HC501-AGE   PIC X(8).
            02  HC501-BYOTO PIC X(20).
            02  HC501-ROOMNO    PIC X(6).
            02  HC501-SRYYMD    PIC X(22).
            02  HC501-SYSYMD    PIC X(10).
            02  HC501-TIME  PIC X(5).
            02  HC501-SRYKA PIC X(20).
            02  HC501-DRNAME    PIC X(30).
            02  HC501-NAIYO
                    PIC X(86)   OCCURS  35 TIMES.
            02  HC501-RINJI PIC X(2).
            02  HC501-BIKOH
                    PIC X(110)  OCCURS  3 TIMES.
            02  HC501-OPID  PIC X(16).
