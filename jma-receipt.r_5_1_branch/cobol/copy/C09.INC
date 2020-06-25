            02  C09.
              03  C09-SCROLLEDWINDOW28.
                04  C09-BYOMEICDLST.
                  05  C09-COUNT       PIC S9(9)   BINARY.
                  05  C09-ROW         PIC S9(9)   BINARY.
                  05  C09-ROWATTR     PIC S9(9)   BINARY.
                  05  C09-ITEM                OCCURS  100 TIMES.
                    06  C09-TBANGO    PIC X(2).
                    06  C09-TBYOMEICD PIC X(7).
                    06  C09-TBYOMEI   PIC X(40).
                  05  C09-SELECT      PIC X   OCCURS  100 TIMES.
