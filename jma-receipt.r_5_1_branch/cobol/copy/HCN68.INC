        01  HCN68.
            02  HCN68-SRYYM
                        PIC X(16).
            02  HCN68-PTNUM
                        PIC X(20).
            02  HCN68-NAME
                        PIC X(50).
            02  HCN68-BIRTHDAY
                        PIC X(22).
            02  HCN68-SEX
                        PIC X(2).
            02  HCN68-LBLSAKYMD
                        PIC X(10).
            02  HCN68-SAKYMD
                        PIC X(22).
            02  HCN68-MEDKBN-G
                                        OCCURS 38 TIMES.
              03  HCN68-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68-MEDKBN-ALL
                        PIC X(2).
            02  HCN68-TTL-MEDKBN-G
                                        OCCURS 3 TIMES.
              03  HCN68-TTL-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68-TTL-MEDKBN-ALL
                        PIC X(2).
