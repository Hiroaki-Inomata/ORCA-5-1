        01  HCM40.
            02  HCM40-KIKAN PIC X(40).
            02  HCM40-TITLE PIC X(32).
            02  HCM40-SAKUSEIBI PIC X(22).
            02  HCM40-PAGE  PIC X(6).
            02  HCM40-MEISAI            OCCURS  30 TIMES.
              03  HCM40-CD    PIC X(5).
              03  HCM40-PTNUM PIC X(10).
              03  HCM40-PTNUMUP   PIC X(10).
              03  HCM40-PTNUMDN   PIC X(10).
              03  HCM40-NAME  PIC X(20).
              03  HCM40-NAME2 PIC X(48).
              03  HCM40-SEX   PIC X(2).
              03  HCM40-AGEZ  PIC X(4).
              03  HCM40-AGEMEI    PIC X(4).
              03  HCM40-HKNMEI    PIC X(40).
              03  HCM40-TANNAME   PIC X(10).
              03  HCM40-BRMNUM    PIC X(6).
              03  HCM40-NYUINYMD  PIC X(9).
              03  HCM40-TAIINYMD  PIC X(9).
              03  HCM40-TAIINCD   PIC X(10).
              03  HCM40-SRYKA PIC X(10).
              03  HCM40-YUBINNUM  PIC X(8).
              03  HCM40-JYUSYO    PIC X(44).
              03  HCM40-JYUSYO2   PIC X(84).
