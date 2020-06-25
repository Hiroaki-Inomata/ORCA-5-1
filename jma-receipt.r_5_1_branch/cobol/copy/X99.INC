        02  X99.
      *     02  X99-FIXED2.
              03  X99-B07-STATE PIC S9(9)   BINARY.
              03  X99-B07S-STATE PIC S9(9)   BINARY.
              03  X99-SELNUM.
                04  X99-VALUE   PIC X(3).
              03  X99-SEL-BYOMEI  PIC X(100).
              03  X99-SEL-BYOMEI-STATE PIC S9(9)   BINARY.
              03  X99-CMB-SRCBYOKBN.
                04  X99-SRCBYOKBN         PIC X(22).
                04  X99-SRCBYOKBN-STATE PIC S9(9)   BINARY.
                04  X99-SRCBYOKBN-COUNT   PIC S9(9)   BINARY.
                04  X99-SRCBYOKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
                04  X99-SRCBYOKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  X99-CMB-SRTKBN.
                04  X99-SRTKBN         PIC X(23).
                04  X99-SRTKBN-STATE PIC S9(9)   BINARY.
                04  X99-SRTKBN-COUNT   PIC S9(9)   BINARY.
                04  X99-SRTKBN-ITEM
                        PIC X(23)   OCCURS  50 TIMES.
                04  X99-SRTKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  X99-SCROLLEDWINDOW3.
                04  X99-SRCBYOLST.
                  05  X99-LBLSRCBYOCD      PIC X(10).
                  05  X99-SRCBYOLST-COUNT  PIC S9(9)   BINARY.
                  05  X99-SRCBYOLST-ROW    PIC S9(9)   BINARY.
                  05  X99-SRCBYOLST-ROWATTR    PIC S9(9)   BINARY.
                  05  X99-SRCBYOLST-ITEM   OCCURS  100 TIMES.
                    06  X99-TSRCNO         PIC X(3).
                    06  X99-TSRCBYOMEI     PIC X(100).
                    06  X99-TSRCCD         PIC X(12).
                  05  X99-SRCBYOLST-SELECT
                          PIC X       OCCURS  100 TIMES.
