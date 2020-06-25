              02  W62.
                04  W62-FIXED32.
                  05  W62-USERKBNCOMBO.
                    06  W62-PRTNUM            PIC X(20).
                    06  W62-PRTNUM-COUNT      PIC S9(9)   BINARY.
                    06  W62-PRTNUMLST         PIC X(20)
                                               OCCURS  20 TIMES.
                  05  W62-PSAREA.
                    06  W62-VALUE  PIC X(36).
