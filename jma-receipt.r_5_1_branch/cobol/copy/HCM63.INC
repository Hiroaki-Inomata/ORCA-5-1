        01  HCM63.
            02  HCM63-KIKAN PIC X(34).
            02  HCM63-PRTYMD    PIC X(22).
            02  HCM63-PAGE  PIC X(5).
            02  HCM63-TBL       OCCURS  70 TIMES.
              03  HCM63-NUM   PIC X(5).
              03  HCM63-PTNUM PIC X(20).
              03  HCM63-PTNUM2    PIC X(10).
              03  HCM63-NAME  PIC X(22).
              03  HCM63-LASTYMD   PIC X(9).
