        01  HCM402V03.
            02  HCM402V03-SRYYM PIC X(16).
            02  HCM402V03-NYUGAIKBN PIC X(20).
            02  HCM402V03-SAKYMD    PIC X(36).
            02  HCM402V03-PAGE  PIC X(18).
            02  HCM402V03-PTNUM PIC X(20).
            02  HCM402V03-NAME  PIC X(50).
            02  HCM402V03-NENREI    PIC X(6).
            02  HCM402V03-MEISAI        OCCURS  20 TIMES.
              03  HCM402V03-RENBAN    PIC X(6).
              03  HCM402V03-MESG  PIC X(156).
              03  HCM402V03-MESG2 PIC X(220).
            02  HCM402V03-BIKO  PIC X(20).
            02  HCM402V03-CHKKOMOKU
                    PIC X(200)  OCCURS  2 TIMES.
            02  HCM402V03-INGAIMSG
                        PIC X(40).
            02  HCM402V03-KIKANMSG
                        PIC X(40).
