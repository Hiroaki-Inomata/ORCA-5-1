        01  HCML04V01.
            02  HCML04V01-PAGE  PIC X(6).
            02  HCML04V01-MSTYMD    PIC X(22).
            02  HCML04V01-HOSPNAME  PIC X(60).
            02  HCML04V01-NAIYO-TBL     OCCURS  15 TIMES.
              03  HCML04V01-SRYCD PIC X(9).
              03  HCML04V01-NAME  PIC X(100).
              03  HCML04V01-TEN   PIC X(14).
              03  HCML04V01-HKNTEK    PIC X(1).
              03  HCML04V01-BUI   PIC X(10).
              03  HCML04V01-GAITEN    PIC X(3).
              03  HCML04V01-NYUTEN    PIC X(3).
              03  HCML04V01-KRM1  PIC X(3).
              03  HCML04V01-KET1  PIC X(3).
              03  HCML04V01-KRM2  PIC X(3).
              03  HCML04V01-KET2  PIC X(3).
              03  HCML04V01-KRM3  PIC X(3).
              03  HCML04V01-KET3  PIC X(3).
              03  HCML04V01-KRM4  PIC X(3).
              03  HCML04V01-KET4  PIC X(3).
              03  HCML04V01-FUKUYOU1  PIC X(2).
              03  HCML04V01-FUKUYOU2  PIC X(2).
              03  HCML04V01-FUKUYOU3  PIC X(2).
              03  HCML04V01-FUKUYOU4  PIC X(2).
              03  HCML04V01-FUKUYOU5  PIC X(2).
              03  HCML04V01-YUKOSTYMD PIC X(8).
              03  HCML04V01-NYORO PIC X(1).
              03  HCML04V01-YUKOEDYMD PIC X(8).
