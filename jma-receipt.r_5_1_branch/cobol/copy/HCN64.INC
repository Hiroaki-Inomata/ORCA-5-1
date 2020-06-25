        01  HCN64.
            02  HCN64-TEISYUTUSAKI  PIC X(90).
            02  HCN64-HOSPADRS  PIC X(100).
            02  HCN64-KAISETUNAME   PIC X(80).
            02  HCN64-HOSPTEL   PIC X(15).
            02  HCN64-HOSPCD    PIC X(10).
            02  HCN64-HOSPNAME  PIC X(100).
            02  HCN64-SANKANUM  PIC X(10).
            02  HCN64-HOSPSBT
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN64-SKYYM PIC X(16).
            02  HCN64-TEIYMD.
              03  HCN64-TEIG  PIC X(4).
              03  HCN64-TEIY  PIC X(4).
              03  HCN64-TEIM  PIC X(4).
              03  HCN64-TEID  PIC X(4).
            02  HCN64-DATAKBN
                    PIC X(2)    OCCURS  3 TIMES.
            02  HCN64-DATACNT   PIC X(2).
            02  HCN64-BIKOU PIC X(100).
