        01  HCM402V02.
            02  HCM402V02-SRYYM PIC X(16).
            02  HCM402V02-NYUGAIKBN PIC X(20).
            02  HCM402V02-SAKYMD    PIC X(36).
            02  HCM402V02-PAGE  PIC X(18).
            02  HCM402V02-PTNUM PIC X(20).
            02  HCM402V02-NAME  PIC X(50).
            02  HCM402V02-NENREI    PIC X(6).
            02  HCM402V02-MEISAI        OCCURS  20 TIMES.
              03  HCM402V02-RENBAN    PIC X(6).
              03  HCM402V02-MESG  PIC X(156).
            02  HCM402V02-BIKO  PIC X(20).
            02  HCM402V02-CHKKOMOKU
                    PIC X(200)  OCCURS  2 TIMES.
