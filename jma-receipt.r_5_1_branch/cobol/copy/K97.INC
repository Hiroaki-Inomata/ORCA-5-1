        02  K97.
    ******  02  K97-FIXED32.
              03  K97-SELNUM1.
                04  K97-VALUE   PIC X(4).
              03  K97-SELNUM2.
                04  K97-VALUE   PIC X(2).
              03  K97-SELNUM3.
                04  K97-VALUE   PIC X(4).
              03  K97-SCROLLEDWINDOW28.
                04  K97-DRCDLIST.
                  05  K97-DRCD-COUNT   PIC S9(9)   BINARY.
                  05  K97-DRCD-ROW PIC S9(9)   BINARY.
                  05  K97-DRCD-ROWATTR PIC S9(9)   BINARY.
                  05  K97-DRCD-ITEM        OCCURS  99 TIMES.
                    06  K97-DRCD    PIC X(4).
                    06  K97-DRCDMEI PIC X(24).
                  05  K97-DRCD-SELECT
                          PIC X       OCCURS  99 TIMES.
              03  K97-SCROLLEDWINDOW27.
                04  K97-SRYKALIST.
                  05  K97-SRYKA-COUNT   PIC S9(9)   BINARY.
                  05  K97-SRYKA-ROW PIC S9(9)   BINARY.
                  05  K97-SRYKA-ROWATTR PIC S9(9)   BINARY.
                  05  K97-SRYKA-ITEM        OCCURS  99 TIMES.
                    06  K97-SRYKA   PIC X(2).
                    06  K97-SRYKAMEI PIC X(12).
                  05  K97-SRYKA-SELECT
                          PIC X       OCCURS  99 TIMES.
              03  K97-SCROLLEDWINDOW26.
                04  K97-HKNCOMBILST.
                  05  K97-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                  05  K97-HKNCOMBI-ROW PIC S9(9)   BINARY.
                  05  K97-HKNCOMBI-ROWATTR PIC S9(9)   BINARY.
                  05  K97-HKNCOMBI-ITEM        OCCURS  30 TIMES.
                    06  K97-HKNCOMBI  PIC X(4).
                    06  K97-HKNCOMBIMEI  PIC X(60).
                  05  K97-HKNCOMBI-SELECT
                          PIC X       OCCURS  30 TIMES.
