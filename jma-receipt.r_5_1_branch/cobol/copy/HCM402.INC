        01  HCM402.
            02  HCM402-SRYYM    PIC X(16).
            02  HCM402-NYUGAIKBN    PIC X(8).
            02  HCM402-SAKYMD   PIC X(36).
            02  HCM402-PAGE PIC X(18).
            02  HCM402-PTNUM    PIC X(20).
            02  HCM402-NAME PIC X(50).
            02  HCM402-NENREI   PIC X(6).
            02  HCM402-MEISAI           OCCURS  20 TIMES.
              03  HCM402-RENBAN   PIC X(6).
              03  HCM402-MESG PIC X(156).
