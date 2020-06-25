        02  KT013.
      ***** 02  KT013-FIXED32.
              03  KT013-SCROLLEDWINDOW26.
                04  KT013-HPOS  PIC S9(9)   BINARY.
                04  KT013-VPOS  PIC S9(9)   BINARY.
                04  KT013-LIST.
                  05  KT013-COUNT PIC S9(9)   BINARY.
                  05  KT013-ROW   PIC S9(9)   BINARY.
                  05  KT013-ITEM      OCCURS  150 TIMES.
                    06  KT013-NO      PIC X(4).
                    06  KT013-SRYYMD  PIC X(14).
                    06  KT013-SINSATSU  PIC X(4).
                    06  KT013-NAIFUKU  PIC X(4).
                    06  KT013-TONPUKU PIC X(4).
                    06  KT013-GAIYOU  PIC X(4).
                    06  KT013-CYUSYA  PIC X(4).
                    06  KT013-SYOCHI  PIC X(4).
                    06  KT013-OP      PIC X(4).
                    06  KT013-MASUI   PIC X(4).
                    06  KT013-KENSA   PIC X(4).
                    06  KT013-GAZOU  PIC X(4).
                    06  KT013-SONOTA  PIC X(4).
                  05  KT013-SELECT
                          PIC X       OCCURS  150 TIMES.
              03  KT013-SELNUM.
                04  KT013-VALUE PIC S9(4).
