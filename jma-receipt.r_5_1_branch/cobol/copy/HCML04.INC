        01  HCML04.
            02  HCML04-PAGE PIC X(6).
            02  HCML04-MSTYMD   PIC X(22).
            02  HCML04-HOSPNAME PIC X(60).
            02  HCML04-NAIYO-TBL        OCCURS  15 TIMES.
              03  HCML04-SRYCD    PIC X(9).
              03  HCML04-NAME PIC X(100).
              03  HCML04-TEN  PIC X(14).
              03  HCML04-HKNTEK   PIC X(1).
              03  HCML04-BUI  PIC X(10).
              03  HCML04-GAITEN   PIC X(3).
              03  HCML04-NYUTEN   PIC X(3).
              03  HCML04-KRM1 PIC X(3).
              03  HCML04-KET1 PIC X(3).
              03  HCML04-KRM2 PIC X(3).
              03  HCML04-KET2 PIC X(3).
              03  HCML04-KRM3 PIC X(3).
              03  HCML04-KET3 PIC X(3).
              03  HCML04-KRM4 PIC X(3).
              03  HCML04-KET4 PIC X(3).
              03  HCML04-FUKUYOU1 PIC X(2).
              03  HCML04-FUKUYOU2 PIC X(2).
              03  HCML04-FUKUYOU3 PIC X(2).
              03  HCML04-FUKUYOU4 PIC X(2).
              03  HCML04-YUKOSTYMD    PIC X(8).
              03  HCML04-NYORO    PIC X(1).
              03  HCML04-YUKOEDYMD    PIC X(8).
