          02  L02.
              03  L02-B12-STATE   PIC S9(9)   BINARY.
              03  L02-SRYYMD.
                04  L02-VALUE   PIC X(10).
              03  L02-FILE1-COMBO.
                04  L02-FILE1.
                  05  L02-VALUE    PIC X(22).
                04  L02-FILE1-COUNT PIC S9(9)   BINARY.
                04  L02-FILE1-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
              03  L02-KOJINKBN1.
                04  L02-VALUE   PIC X.
              03  L02-KOJINKBN2.
                04  L02-VALUE   PIC X.
              03  L02-NYUGAIKBNCOMBO.
                04  L02-NYUGAIKBN.
                  05  L02-VALUE   PIC X(8).
                04  L02-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  L02-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  L02-CDKBN PIC X OCCURS 3.
              03  L02-FILENM1.
                04  L02-VALUE   PIC X(150).
