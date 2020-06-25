          02  G02.
              03  G02-B12-STATE   PIC S9(9)   BINARY.
              03  G02-SRYYM.
                04  G02-VALUE   PIC X(7).
              03  G02-FILE1-COMBO.
                04  G02-FILE1.
                  05  G02-VALUE    PIC X(22).
                04  G02-FILE1-COUNT PIC S9(9)   BINARY.
                04  G02-FILE1-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
              03  G02-FILE2-COMBO.
                04  G02-FILE2.
                  05  G02-VALUE    PIC X(22).
                04  G02-FILE2-COUNT PIC S9(9)   BINARY.
                04  G02-FILE2-ITEM
                        PIC X(22)   OCCURS  6 TIMES.
              03  G02-HOUKATUKBN1.
                04  G02-VALUE   PIC X.
              03  G02-HOUKATUKBN2.
                04  G02-VALUE   PIC X.
              03  G02-KOJINKBN1.
                04  G02-VALUE   PIC X.
              03  G02-KOJINKBN2.
                04  G02-VALUE   PIC X.
              03  G02-NYUGAIKBNCOMBO.
                04  G02-NYUGAIKBN.
                  05  G02-VALUE   PIC X(8).
                04  G02-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  G02-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  G02-DATAKBN3.
                04  G02-VALUE   PIC X.
              03  G02-DATAKBN2.
                04  G02-VALUE   PIC X.
              03  G02-DATAKBN1.
                04  G02-VALUE   PIC X.
              03  G02-CDKBN     PIC X OCCURS 3.
              03  G02-FILENM1.
                04  G02-VALUE   PIC X(150).
              03  G02-FILENM2.
                04  G02-VALUE   PIC X(150).
