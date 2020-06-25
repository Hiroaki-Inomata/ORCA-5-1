       02  W66.
      *     02  W66-FIXED1.
              03 W66-B03-STATE PIC S9(9)   BINARY.
              03  W66-YUKOSTYMD.
                04  W66-VALUE   PIC X(9).
              03  W66-YUKOEDYMD.
                04  W66-VALUE   PIC X(9).
              03  W66-KGIIRYOCD.
                04  W66-VALUE   PIC X(20).
              03  W66-CMB-KGISKYKBN.
                04  W66-KGISKYKBN PIC X(23).
                04  W66-KGISKYKBN-COUNT   PIC S9(9)   BINARY.
                04  W66-KGISKYKBN-ITEM
                                PIC X(23)   OCCURS  20 TIMES.
