        01  HCN69.
            02  HCN69-SRYYM
                        PIC X(16).
            02  HCN69-PTNUM
                        PIC X(20).
            02  HCN69-NAME
                        PIC X(50).
            02  HCN69-BIRTHDAY
                        PIC X(22).
            02  HCN69-SEX
                        PIC X(2).
            02  HCN69-SAKYMD
                        PIC X(22).
            02  HCN69-LBLSAKYMD
                        PIC X(10).
            02  HCN69-ADLTEN-G
                                        OCCURS 4 TIMES.
              03  HCN69-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69-TTL-ADLTEN-G.
              03  HCN69-TTL-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69-LBLHPTSTS
                        PIC X(20).
            02  HCN69-HPTSTS-G
                                        OCCURS 9 TIMES.
              03  HCN69-HPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69-LBLCPTSTS
                        PIC X(20).
            02  HCN69-CPTSTS-G
                                        OCCURS 5 TIMES.
              03  HCN69-CPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69-LOPTION
                        PIC X(100).
