        01  HCN69R02.
            02  HCN69R02-SRYYM
                        PIC X(16).
            02  HCN69R02-PTNUM
                        PIC X(20).
            02  HCN69R02-NAME
                        PIC X(50).
            02  HCN69R02-BIRTHDAY
                        PIC X(22).
            02  HCN69R02-SEX
                        PIC X(2).
            02  HCN69R02-SAKYMD
                        PIC X(22).
            02  HCN69R02-LBLSAKYMD
                        PIC X(10).
            02  HCN69R02-ADLTEN-G
                                        OCCURS 4 TIMES.
              03  HCN69R02-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R02-TTL-ADLTEN-G.
              03  HCN69R02-TTL-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R02-HPTSTS-G
                                        OCCURS 9 TIMES.
              03  HCN69R02-HPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R02-CPTSTS-G
                                        OCCURS 5 TIMES.
              03  HCN69R02-CPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R02-LOPTION
                        PIC X(100).
            02  HCN69R02-DESIGNR-G.
              03  HCN69R02-DESIGNR-DAY
                        PIC X(2)        OCCURS 31 TIMES.
