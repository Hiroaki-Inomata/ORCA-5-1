            02  X100.
              03  X100-SCROLLEDWINDOW28.
                04  X100-BYOMEICDLST.
                  05  X100-COUNT       PIC S9(9)   BINARY.
                  05  X100-ROW         PIC S9(9)   BINARY.
                  05  X100-ROWATTR     PIC S9(9)   BINARY.
                  05  X100-ITEM                OCCURS  200 TIMES.
                    06  X100-TBANGO    PIC X(3).
                    06  X100-TBYOMEICD PIC X(11).
                    06  X100-TBYOMEI   PIC X(100).
                  05  X100-SELECT      PIC X   OCCURS  200 TIMES.
