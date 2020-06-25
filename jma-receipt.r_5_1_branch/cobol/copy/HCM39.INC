        01  HCM39.
            02  HCM39-KIKAN PIC X(40).
            02  HCM39-TITLE PIC X(32).
            02  HCM39-SAKUSEIBI PIC X(22).
            02  HCM39-PAGE  PIC X(6).
            02  HCM39-MEISAI            OCCURS  30 TIMES.
              03  HCM39-CD    PIC X(5).
              03  HCM39-PTNUM PIC X(10).
              03  HCM39-PTNUMUP   PIC X(10).
              03  HCM39-PTNUMDN   PIC X(10).
              03  HCM39-NAME  PIC X(20).
              03  HCM39-NAME2 PIC X(40).
              03  HCM39-DENPNUM   PIC X(9).
              03  HCM39-SRYKA PIC X(20).
              03  HCM39-SRYYMDG   PIC X(10).
              03  HCM39-SRYYMDN   PIC X(20).
              03  HCM39-SEIKYU    PIC X(11).
              03  HCM39-MISYU PIC X(11).
              03  HCM39-NYUKIN    PIC X(11).
              03  HCM39-BIKO  PIC X(14).
            02  HCM39-MES   PIC X(50).
            02  HCM39-SEIKYUG   PIC X(11).
            02  HCM39-MISYUG    PIC X(11).
            02  HCM39-NYUKING   PIC X(11).
            02  HCM39-GTTL  PIC X(10).
            02  HCM39-DAI   PIC X(8).
