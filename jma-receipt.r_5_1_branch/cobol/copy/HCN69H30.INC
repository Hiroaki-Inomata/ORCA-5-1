        01  HCN69H30.
            02  HCN69H30-SRYYM
                        PIC X(16).
            02  HCN69H30-PTNUM
                        PIC X(20).
            02  HCN69H30-NAME
                        PIC X(50).
            02  HCN69H30-BIRTHDAY
                        PIC X(22).
            02  HCN69H30-SEX
                        PIC X(2).
            02  HCN69H30-SAKYMD
                        PIC X(22).
            02  HCN69H30-LBLSAKYMD
                        PIC X(10).
            02  HCN69H30-ADLTEN-G
                                        OCCURS 4 TIMES.
              03  HCN69H30-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69H30-TTL-ADLTEN-G.
              03  HCN69H30-TTL-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69H30-HPTSTS-G
                                        OCCURS 9 TIMES.
              03  HCN69H30-HPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69H30-CPTSTS-G
                                        OCCURS 5 TIMES.
              03  HCN69H30-CPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69H30-LOPTION
                        PIC X(100).
            02  HCN69H30-DESIGNR-G.
              03  HCN69H30-DESIGNR-DAY
                        PIC X(2)        OCCURS 31 TIMES.
