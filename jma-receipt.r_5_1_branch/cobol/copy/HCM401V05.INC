        01  HCM401V05.
            02  HCM401V05-SRYYM PIC X(16).
            02  HCM401V05-NYUGAIKBN PIC X(20).
            02  HCM401V05-SAKYMD    PIC X(36).
            02  HCM401V05-PAGE  PIC X(6).
            02  HCM401V05-RENBAN
                    PIC X(10)   OCCURS  20 TIMES.
            02  HCM401V05-PTNUM
                    PIC X(20)   OCCURS  20 TIMES.
            02  HCM401V05-PTNUM2
                    PIC X(40)   OCCURS  20 TIMES.
            02  HCM401V05-NAME
                    PIC X(30)   OCCURS  20 TIMES.
            02  HCM401V05-HKNINF
                    PIC X(100)  OCCURS  20 TIMES.
            02  HCM401V05-NENREI
                    PIC X(6)    OCCURS  20 TIMES.
            02  HCM401V05-MESG
                    PIC X(110)  OCCURS  20 TIMES.
            02  HCM401V05-MESG2
                    PIC X(220)  OCCURS  20 TIMES.
            02  HCM401V05-MARK
                    PIC X(2)    OCCURS  20 TIMES.
            02  HCM401V05-BIKOMARK  PIC X(2).
            02  HCM401V05-BIKO  PIC X(20).
            02  HCM401V05-CHKKOMOKU
                    PIC X(220)  OCCURS  2 TIMES.
            02  HCM401V05-INGAIMSG
                        PIC X(40).
            02  HCM401V05-KIKANMSG
                        PIC X(40).
