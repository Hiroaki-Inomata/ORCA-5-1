        01  HCN03.
            02  HCN03-NO    PIC X(7).
            02  HCN03-NYUINSTRYMD   PIC X(22).
            02  HCN03-NYUINENDYMD   PIC X(22).
            02  HCN03-HAKYMD    PIC X(22).
            02  HCN03-KNJNAMW   PIC X(20).
            02  HCN03-KNJNO PIC X(20).
            02  HCN03-ROOM  PIC X(50).
            02  HCN03-HOKEN PIC X(20).
            02  HCN03-FUTAN PIC X(6).
            02  HCN03-MIDASI    PIC X(20).
            02  HCN03-HKNRYO
                    PIC X(24)   OCCURS  12 TIMES.
            02  HCN03-HOKENBUN  PIC X(24).
            02  HCN03-HOKENGAI  PIC X(24).
            02  HCN03-JIHI
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN03-JIHIMSG
                    PIC X(20)   OCCURS  5 TIMES.
            02  HCN03-SYOKUJI   PIC X(24).
            02  HCN03-ROUJIN    PIC X(24).
            02  HCN03-KOUHI PIC X(24).
            02  HCN03-RMSAGAKU  PIC X(24).
            02  HCN03-CHOSEI    PIC X(24).
            02  HCN03-KONSEIKYU PIC X(24).
            02  HCN03-KONNYUKIN PIC X(24).
            02  HCN03-IRYOADRS  PIC X(60).
            02  HCN03-IRYOMEI   PIC X(40).
            02  HCN03-IRYOTEL   PIC X(15).
            02  HCN03-LBL-G-MISYU   PIC X(20).
            02  HCN03-LBL-G-MISYU-EN    PIC X(2).
            02  HCN03-GAI-MISYUKIN  PIC X(24).
            02  HCN03-LBL-N-MISYU   PIC X(20).
            02  HCN03-LBL-N-MISYU-EN    PIC X(2).
            02  HCN03-NYU-MISYUKIN  PIC X(24).
            02  HCN03-MIDASHI   PIC X(10).
            02  HCN03-RSIMIDASI PIC X(16).
            02  HCN03-RSIBUN    PIC X(24).
            02  HCN03-RSIEN PIC X(2).
