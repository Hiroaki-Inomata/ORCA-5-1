        02  P02S.
   ******** 02  P02S-FIXED10.
              03  P02S-SRYYM.
                04  P02S-VALUE  PIC X(7).
              03  P02S-SYUKA-COMB.
                04  P02S-SYUKA.
                  05  P02S-VALUE  PIC X(20).
                04  P02S-SYUKA-COUNT  PIC S9(9)   BINARY.
                04  P02S-SYUKA-ITEM
                        PIC X(20)   OCCURS  99 TIMES.
