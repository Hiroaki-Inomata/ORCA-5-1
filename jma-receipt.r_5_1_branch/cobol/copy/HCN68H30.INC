        01  HCN68H30.
            02  HCN68H30-SRYYM
                        PIC X(16).
            02  HCN68H30-PTNUM
                        PIC X(20).
            02  HCN68H30-NAME
                        PIC X(50).
            02  HCN68H30-BIRTHDAY
                        PIC X(22).
            02  HCN68H30-SEX
                        PIC X(2).
            02  HCN68H30-LBLSAKYMD
                        PIC X(10).
            02  HCN68H30-SAKYMD
                        PIC X(22).
            02  HCN68H30-MEDKBN-G
                                        OCCURS 46 TIMES.
              03  HCN68H30-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68H30-MEDKBN-ALL
                        PIC X(2).
            02  HCN68H30-TTL-MEDKBN-G
                                        OCCURS 3 TIMES.
              03  HCN68H30-TTL-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68H30-TTL-MEDKBN-ALL
                        PIC X(2).
