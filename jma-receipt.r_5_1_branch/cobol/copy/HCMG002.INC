        01  HCMG002.
            02  HCMG002-TAISYO  PIC X(60).
            02  HCMG002-SYORIYMD    PIC X(22).
            02  HCMG002-PAGE    PIC X(3).
            02  HCMG002-NAIYO-TBL       OCCURS  30 TIMES.
              03  HCMG002-BTUNAME PIC X(10).
              03  HCMG002-BRMNUM  PIC X(6).
              03  HCMG002-SRYKA   PIC X(8).
              03  HCMG002-PTNUM   PIC X(10).
              03  HCMG002-PTNUM2  PIC X(20).
              03  HCMG002-NAME    PIC X(30).
              03  HCMG002-SEX PIC X(2).
              03  HCMG002-BIRTHDAY    PIC X(22).
              03  HCMG002-DENPNUM PIC X(7).
              03  HCMG002-SEIKYU  PIC X(12).
              03  HCMG002-JYOTAI  PIC X(16).
            02  HCMG002-TTL-TBL         OCCURS  2 TIMES.
              03  HCMG002-TTLMSG  PIC X(10).
              03  HCMG002-SEIKYUTTL   PIC X(12).
