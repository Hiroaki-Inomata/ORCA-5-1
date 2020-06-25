        02  R03.
              03  R03-B10-STATE   PIC S9(09) BINARY.
              03  R03-B23-STATE   PIC S9(09) BINARY.
              03  R03-SRYYM.
                04  R03-VALUE   PIC X(7).
              03  R03-SYOKBN.
                04  R03-SYOKBN-VALUE   PIC X.
                04  R03-SYOKBN-LABEL   PIC X(20).
              03  R03-IKKATU.
                04  R03-VALUE   PIC X.
              03  R03-NYUGAIKBNCOMBO.
                04  R03-NYUGAIKBN.
                  05  R03-VALUE   PIC X(8).
                04  R03-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  R03-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  R03-PRTKBNCOMBO.
                04  R03-PRTKBN-STATE   PIC S9(09) BINARY.
                04  R03-PRTKBN.
                  05  R03-VALUE   PIC X(32).
                04  R03-PRTKBN-COUNT   PIC S9(9)   BINARY.
                04  R03-PRTKBN-ITEM    PIC X(32)    OCCURS  10 TIMES.
              03  R03-PRTKBN-LABEL-STYLE   PIC X(20).
              03  R03-PRTKBN-LABEL.
                04  R03-VALUE   PIC X(36).
              03  R03-MSG-STYLE PIC X(20).
              03  R03-MSG-LABEL.
                04  R03-VALUE   PIC X(140).
              03  R03-KOBETU.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN1.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN2.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN3.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN5.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN32.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN33.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN11.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN31.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN4.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN21.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN22.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN23.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN24.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN25.
                04  R03-VALUE   PIC X.
              03  R03-DATAKBN26.
                04  R03-VALUE   PIC X.
