        02  I041.
     ****** 02  I041-FIXED4.
              03  I041-SRYKA-G.
                  04  I041-SRYKA    OCCURS   5.
                    05  I041-VALUE  PIC X(16).
              03  I041-DRCD-COMB-G.
                04  I041-DRCD-COMB  OCCURS   5.
                  05  I041-DRCD.
                    06  I041-VALUE  PIC X(29).
                  05  I041-DRCD-COUNT  PIC S9(9)   BINARY.
                  05  I041-DRCD-ITEM
                        PIC X(29)   OCCURS  100 TIMES.
