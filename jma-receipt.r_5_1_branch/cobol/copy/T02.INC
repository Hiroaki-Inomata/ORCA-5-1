            02  T02.
              03  T02-SELNUM1.
                04  T02-VALUE   PIC S9(3).
              03  T02-SYORIFLGCOMBO.
                04  T02-SYORIFLG         PIC X(14).
                04  T02-SYORIFLG-COUNT   PIC S9(9)   BINARY.
                04  T02-SYORIFLG-ITEM    PIC X(14)   OCCURS  3 TIMES.
              03  T02-SELNUM2-STATE      PIC S9(9)   BINARY.
              03  T02-SELNUM2            PIC S9(3).
              03  T02-PARA-STATE         PIC S9(9)   BINARY.
              03  T02-PARA               PIC X(20).
              03  T02-SCROLLEDWINDOW29.
                04  T02-PARALST.
                  05  T02-TITLE1.
                    06  T02-VALUE   PIC X(10).
                  05  T02-TITLE2.
                    06  T02-VALUE   PIC X(8).
                  05  T02-PARALST-COUNT   PIC S9(9)   BINARY.
                  05  T02-PARALST-ROW PIC S9(9)   BINARY.
                  05  T02-PARALST-ITEM        OCCURS  100 TIMES.
                    06  T02-PARALST-RENNUM    PIC X(3).
                    06  T02-PARALST-PARA      PIC X(20).
                    06  T02-PARALST-NAME      PIC X(50).
                  05  T02-PARALST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  T02-SCROLLEDWINDOW28.
                04  T02-SHELLIDLST.
                  05  T02-SHELLIDLST-COUNT   PIC S9(9)   BINARY.
                  05  T02-SHELLIDLST-ROW PIC S9(9)   BINARY.
                  05  T02-SHELLIDLST-ITEM        OCCURS  25 TIMES.
                    06  T02-SHELLIDLST-RENNUM    PIC X(3).
                    06  T02-SHELLIDLST-SYORIFLG  PIC X(2).
                    06  T02-SHELLIDLST-SHORIMSG  PIC X(40).
                    06  T02-SHELLIDLST-PARAKBN   PIC X(10).
                  05  T02-SHELLIDLST-SELECT
                          PIC X       OCCURS  25 TIMES.
              03  T02-NAME.
                04  T02-VALUE   PIC X(50).
              03  T02-SHORIMSG.
                04  T02-VALUE   PIC X(40).
