        02  W67.
      *      02  W67-FIXED1.
              03 W67-B03-STATE PIC S9(9)   BINARY.
              03 W67-YUKOSTYMD.
                04  W67-VALUE   PIC X(9).
              03  W67-YUKOEDYMD.
                04  W67-VALUE   PIC X(9).
              03  W67-LBLMEISAI PIC X(20)  OCCURS 16.
              03  W67-CMB-MEISAIKBN        OCCURS 16.
                04  W67-MEISAIKBN          PIC X(23).
                04  W67-MEISAIKBN-COUNT    PIC S9(9)   BINARY.
                04  W67-MEISAIKBN-ITEM     PIC X(23)   OCCURS  20 TIMES.
              03  W67-LBLNETCJIHI PIC X(20) OCCURS 10.
              03  W67-LBLGETCJIHI PIC X(20) OCCURS 10.
              03  W67-CMB-NETCJIHIKBN       OCCURS 10.
                04  W67-NETCJIHIKBN         PIC X(23).
                04  W67-NETCJIHIKBN-COUNT   PIC S9(9)   BINARY.
                04  W67-NETCJIHIKBN-ITEM    PIC X(23)   OCCURS 20 TIMES.
              03  W67-CMB-GETCJIHIKBN       OCCURS 10.
                04  W67-GETCJIHIKBN         PIC X(23).
                04  W67-GETCJIHIKBN-COUNT   PIC S9(9)   BINARY.
                04  W67-GETCJIHIKBN-ITEM    PIC X(23)   OCCURS 20 TIMES.
