            02  W142.
              03  W142-SCROLLEDWINDOW28.
                04  W142-HPOS    PIC S9(9)   BINARY.
                04  W142-VPOS    PIC S9(9)   BINARY.
                04  W142-DOCERILST.
                  05  W142-DOCER-COUNT       PIC S9(9)   BINARY.
                  05  W142-DOCER-ROW         PIC S9(9)   BINARY.
                  05  W142-DOCER-ROWATTR     PIC S9(9)   BINARY.
                  05  W142-DOCER-ITEM        OCCURS  25  TIMES.
                    06  W142-CD              PIC X(4).
                    06  W142-SHELLID         PIC X(20).
                    06  W142-NAME            PIC X(128).
                  05  W142-DOCER-SELECT      PIC X   OCCURS  25 TIMES.
