        01  HCN03A.
            02  HCN03A-NO   PIC X(7).
            02  HCN03A-NYUINSTRYMD  PIC X(22).
            02  HCN03A-NYUINENDYMD  PIC X(22).
            02  HCN03A-HAKYMD   PIC X(22).
            02  HCN03A-KNJNAMW  PIC X(20).
            02  HCN03A-KNJNO    PIC X(20).
            02  HCN03A-ROOM PIC X(50).
            02  HCN03A-HOKEN    PIC X(20).
            02  HCN03A-FUTAN    PIC X(6).
            02  HCN03A-MIDASI   PIC X(20).
            02  HCN03A-MEISAI
                    PIC X(20)   OCCURS  15 TIMES.
            02  HCN03A-HKNRYO
                    PIC X(24)   OCCURS  16 TIMES.
            02  HCN03A-HOKENBUN PIC X(24).
            02  HCN03A-HOKENGAI PIC X(24).
            02  HCN03A-JIHI
                    PIC X(24)   OCCURS  8 TIMES.
            02  HCN03A-JIHIMSG
                    PIC X(20)   OCCURS  5 TIMES.
            02  HCN03A-SYOKUJI  PIC X(24).
            02  HCN03A-ROUJIN   PIC X(24).
            02  HCN03A-KOUHI    PIC X(24).
            02  HCN03A-RMSAGAKU PIC X(24).
            02  HCN03A-CHOSEI   PIC X(24).
            02  HCN03A-KONSEIKYU    PIC X(24).
            02  HCN03A-KONNYUKIN    PIC X(24).
            02  HCN03A-IRYOADRS PIC X(100).
            02  HCN03A-IRYOMEI  PIC X(100).
            02  HCN03A-IRYOTEL  PIC X(15).
            02  HCN03A-LBL-G-MISYU  PIC X(20).
            02  HCN03A-LBL-G-MISYU-EN   PIC X(2).
            02  HCN03A-GAI-MISYUKIN PIC X(24).
            02  HCN03A-LBL-N-MISYU  PIC X(20).
            02  HCN03A-LBL-N-MISYU-EN   PIC X(2).
            02  HCN03A-NYU-MISYUKIN PIC X(24).
            02  HCN03A-MIDASHI  PIC X(10).
            02  HCN03A-RSIMIDASI    PIC X(16).
            02  HCN03A-RSIBUN   PIC X(24).
            02  HCN03A-RSIEN    PIC X(2).
            02  HCN03A-SKJRYO   PIC X(24).
            02  HCN03A-LIFERYO  PIC X(24).
