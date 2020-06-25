        01  HCN68R02.
            02  HCN68R02-SRYYM
                        PIC X(16).
            02  HCN68R02-PTNUM
                        PIC X(20).
            02  HCN68R02-NAME
                        PIC X(50).
            02  HCN68R02-BIRTHDAY
                        PIC X(22).
            02  HCN68R02-SEX
                        PIC X(2).
            02  HCN68R02-LBLSAKYMD
                        PIC X(10).
            02  HCN68R02-SAKYMD
                        PIC X(22).
            02  HCN68R02-MEDKBN-G
                                        OCCURS 47 TIMES.
              03  HCN68R02-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68R02-MEDKBN-ALL
                        PIC X(2).
            02  HCN68R02-TTL-MEDKBN-G
                                        OCCURS 3 TIMES.
              03  HCN68R02-TTL-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68R02-TTL-MEDKBN-ALL
                        PIC X(2).
