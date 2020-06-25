            02  T03.
              03  T03-SELNUM.
                04  T03-VALUE   PIC S9(3).
              03  T03-SHORIMSG.
                04  T03-VALUE   PIC X(40).
              03  T03-SCROLLEDWINDOW28.
                04  T03-HPOS    PIC S9(9)   BINARY.
                04  T03-VPOS    PIC S9(9)   BINARY.
                04  T03-SHELLIDLST.
                  05  T03-SHELLIDLST-COUNT   PIC S9(9)   BINARY.
                  05  T03-SHELLIDLST-ROW PIC S9(9)   BINARY.
                  05  T03-SHELLIDLST-ITEM        OCCURS  30 TIMES.
                    06  T03-SHELLIDLST-RENNUM    PIC X(3).
                    06  T03-SHELLIDLST-SHORIMSG  PIC X(40).
                    06  T03-SHELLIDLST-PRTNM     PIC X(16).
                  05  T03-SHELLIDLST-SELECT
                          PIC X       OCCURS  30 TIMES.
              03  T03-PRTNM-COMBO.
                04  T03-PRTNM.
                  05  T03-VALUE   PIC X(19).
                04  T03-PRTNM-COUNT   PIC S9(9)   BINARY.
                04  T03-PRTNM-ITEM
                        PIC X(19)   OCCURS  100 TIMES.
