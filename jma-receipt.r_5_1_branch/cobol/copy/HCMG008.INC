        01  HCMG008.
            02  HCMG008-SRYYM
                        PIC X(16).
            02  HCMG008-IKTFLG
                        PIC X(70).
            02  HCMG008-PAGE
                        PIC X(3).
            02  HCMG008-PRTYMD
                        PIC X(22).
            02  HCMG008-PRTTIME
                        PIC X(10).
            02  HCMG008-GLBL
                        PIC X(10).
            02  HCMG008-GKEN
                        PIC X(2).
            02  HCMG008-GKENSU
                        PIC X(5).
            02  HCMG008-TBL
                                        OCCURS 50 TIMES.
              03  HCMG008-PTNUM
                        PIC X(20).
              03  HCMG008-NAME
                        PIC X(28).
              03  HCMG008-NAIYO
                        PIC X(60).
