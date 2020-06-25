        02  K081.
     ****** 02  K081-FIXED32.
              03  K081-CHUSYA-COMB.
                04  K081-CHUSYA.
                  05  K081-VALUE  PIC X(12).
                04  K081-CHUSYA-COUNT  PIC S9(9)   BINARY.
                04  K081-CHUSYA-ITEM
                        PIC X(12)   OCCURS  2 TIMES.
              03  K081-DRCD-COMB.
                04  K081-DRCD.
                  05  K081-VALUE  PIC X(30).
                04  K081-DRCD-COUNT  PIC S9(9)   BINARY.
                04  K081-DRCD-ITEM
                        PIC X(30)   OCCURS  99 TIMES.
              03  K081-CHK1-G       OCCURS  4.
                04  K081-CHK1-STATE  PIC S9(9)   BINARY.
                04  K081-CHK1-STYLE  PIC X(20).
                04  K081-CHK1-VALUE  PIC X.
                04  K081-CHK1        PIC X(30).
              03  K081-CHK2-G        OCCURS  4.
                04  K081-CHK2-STATE  PIC S9(9)   BINARY.
                04  K081-CHK2-STYLE  PIC X(20).
                04  K081-CHK2-VALUE  PIC X.
                04  K081-CHK2        PIC X(30).
              03  K081-CHK3-G        OCCURS  4.
                04  K081-CHK3-STATE  PIC S9(9)   BINARY.
                04  K081-CHK3-STYLE  PIC X(20).
                04  K081-CHK3-VALUE  PIC X.
                04  K081-CHK3        PIC X(30).
              03  K081-SIJISEN-COMB.
                04  K081-SIJISEN.
                  05  K081-VALUE  PIC X(12).
                04  K081-SIJISEN-COUNT  PIC S9(9)   BINARY.
                04  K081-SIJISEN-ITEM
                        PIC X(12)   OCCURS  2 TIMES.
              03  K081-SYOHOU-COMB.
                04  K081-SYOHOU.
                  05  K081-VALUE  PIC X(12).
                04  K081-SYOHOU-COUNT  PIC S9(9)   BINARY.
                04  K081-SYOHOU-ITEM
                        PIC X(12)   OCCURS  2 TIMES.
              03  K081-USERPG-COMB.
                04  K081-USERPG-STATE   PIC S9(9)   BINARY.
                04  K081-USERPG.
                  05  K081-VALUE  PIC X(30).
                04  K081-USERPG-COUNT  PIC S9(9)   BINARY.
                04  K081-USERPG-ITEM
                        PIC X(30)   OCCURS  2 TIMES.
