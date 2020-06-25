        02  X08.
            03  X08-TITLE   PIC X(256).
            03  X08-POPUP-SUMMARY   PIC X(256).
            03  X08-POPUP-BODY  PIC X(256).
            03  X08-POPUP-ICON  PIC X(256).
            03  X08-POPUP-TIMEOUT   PIC S9(9)   BINARY.
            03  X08-FIXED2.
              04  X08-NAME.
                05  X08-TEXTDATA    PIC X(100).
              04  X08-B03.
                05  X08-B03-STATE   PIC S9(9)   BINARY.
              04  X08-NO-G          OCCURS  20.
                05  X08-NO-STYLE    PIC X(20).
                05  X08-NO          PIC S9(3).
              04  X08-TEKCD-G       OCCURS  20.
                05  X08-TEKCD-STYLE PIC X(20).
                05  X08-TEKCD       PIC X(20).
              04  X08-DSPNAME-G     OCCURS  20.
                05  X08-DSPNAME-STYLE   PIC X(20).
                05  X08-DSPNAME PIC X(100).
              04  X08-SRYCD.
                05  X08-TEXTDATA    PIC X(9).
              04  X08-KIJUNYM.
                05  X08-TEXTDATA    PIC X(10).
              04  X08-CHKKBN.
                05  X08-TEXTDATA    PIC X(50).
              04  X08-NGBNDKBN-G OCCURS  20.
                05  X08-NGBNDKBN-STATE   PIC S9(9)   BINARY.
                05  X08-NGBNDKBN-STYLE   PIC X(20).
                05  X08-NGBNDKBN         PIC S9(1).
              04  X08-CHKERR-G OCCURS  20.
                05  X08-CHKERR-STATE    PIC S9(9)   BINARY.
                05  X08-CHKERR-STYLE    PIC X(20).
                05  X08-CHKERR   PIC S9(1).
              04  X08-LBLCHKERR-G OCCURS  20.
                05  X08-LBLCHKERR-STYLE    PIC X(20).
                05  X08-LBLCHKERR   PIC X(1).
              04  X08-YUKOSTYMD-G    OCCURS   20.
                05  X08-YUKOSTYMD-STYLE   PIC X(20).
                05  X08-YUKOSTYMD         PIC X(10).
              04  X08-YUKOEDYMD-G    OCCURS   20.
                05  X08-YUKOEDYMD-STYLE   PIC X(20).
                05  X08-YUKOEDYMD         PIC X(10).
              04  X08-LBLPAGE.
                05  X08-TEXTDATA    PIC X(10).
              04  X08-LBLCAUTION-G.
                05  X08-LBLCAUTION-STYLE
                                    PIC X(20).
                05  X08-LBLCAUTION  PIC X(200).
