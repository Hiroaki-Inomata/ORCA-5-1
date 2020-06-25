        01  HCMC001.
            02  HCMC001-DAI PIC X(82).
            02  HCMC001-YMD PIC X(22).
            02  HCMC001-PAGE    PIC X(3).
            02  HCMC001-TAI-TBL.
              03  HCMC001-TAI-NAME    PIC X(48).
              03  HCMC001-TAI-NAIYO   PIC X(14)   OCCURS  9 TIMES.
            02  HCMC001-NAI-TBL         OCCURS  35 TIMES.
              03  HCMC001-NAME    PIC X(48).
              03  HCMC001-NAIYO   PIC X(10)       OCCURS  9 TIMES.
