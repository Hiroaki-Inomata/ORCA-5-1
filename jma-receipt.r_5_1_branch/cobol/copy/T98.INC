            02  T98.
              03  T98-ID1MSG.
                04  T98-VALUE   PIC X(80).
              03  T98-PRTKBN-COMBO.
                04  T98-PRTKBN.
                  05  T98-VALUE    PIC X(32).
                04  T98-PRTKBN-COUNT PIC S9(9)   BINARY.
                04  T98-PRTKBN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
              03  T98-DATA-COMBO.
                04  T98-DATA.
                  05  T98-1VALUE    PIC X(38).
                04  T98-DATA-COUNT PIC S9(9)   BINARY.
                04  T98-DATA-ITEM
                        PIC X(38)   OCCURS  6 TIMES.
              03  T98-B10-VISIBLE  PIC X(01).
              03  T98-B11-VISIBLE  PIC X(01).
              03  T98-B12-LABEL  PIC X(14).
              03  T98-LABEL-STYLE  PIC X(20).
              03  T98-LABEL        PIC X(40).
