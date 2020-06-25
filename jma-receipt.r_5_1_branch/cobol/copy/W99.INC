            02  W99.
              03  W99-B06.
                04  W99-B06-STATE     PIC S9(9)   BINARY.
              03  W99-B07.
                04  W99-B07-STATE     PIC S9(9)   BINARY.
              03  W99-YUKOSTYMD.
                04  W99-VALUE   PIC X(9).
              03  W99-YUKOEDYMD.
                04  W99-VALUE   PIC X(9).
              03  W99-SETUZOKU-COMBO.
                04  W99-SETUZOKU.
                  05  W99-VALUE   PIC X(20).
                04  W99-SETUZOKU-COUNT   PIC S9(9)   BINARY.
                04  W99-SETUZOKUL PIC X(20)   OCCURS  2 TIMES.
              03  W99-SCODE-COMBO.
                04  W99-SCODE.
                  05  W99-VALUE       PIC X(20).
                04  W99-SCODE-COUNT   PIC S9(9)   BINARY.
                04  W99-SCODEL        PIC X(20)   OCCURS  3 TIMES.
              03  W99-SADDRESS-PORT               OCCURS 10 TIMES.
                04  W99-ADDRESS PIC X(40).
                04  W99-PORT1   PIC X(5).
                04  W99-PORT2   PIC X(5).
                04  W99-MEMO    PIC X(40).
              03  W99-UKEBUTTON1.
                04  W99-VALUE   PIC X.
              03  W99-UKEBUTTON2.
                04  W99-VALUE   PIC X.
              03  W99-KANBUTTON1.
                04  W99-VALUE   PIC X.
              03  W99-KANBUTTON2.
                04  W99-VALUE   PIC X.
              03  W99-SYNBUTTON1.
                04  W99-VALUE   PIC X.
              03  W99-SYNBUTTON2.
                04  W99-VALUE   PIC X.
              03  W99-LBLNO-G.
                04 W99-LBLNO    PIC X(04)  OCCURS  10.
              03  W99-FRAME1.
                04  W99-FIXED2.
                  05  W99-PTINF-SEND1.
                    06  W99-VALUE   PIC X.
                  05  W99-PTINF-SEND2.
                    06  W99-VALUE   PIC X.
                  05  W99-RECEDENKA-SEND1.
                    06  W99-VALUE   PIC X.
                  05  W99-RECEDENKA-SEND2.
                    06  W99-VALUE   PIC X.
                  05  W99-BYOMEI-AGG1.
                    06  W99-VALUE   PIC X.
                  05  W99-BYOMEI-AGG2.
                    06  W99-VALUE   PIC X.
                  05  W99-BYOMEI-SYNC1.
                    06  W99-VALUE   PIC X.
                  05  W99-BYOMEI-SYNC2.
                    06  W99-VALUE   PIC X.
                  05  W99-NYU-ORDER1.
                    06  W99-VALUE   PIC X.
                  05  W99-NYU-ORDER2.
                    06  W99-VALUE   PIC X.
                  05  W99-MULTIHOST1.
                    06  W99-VALUE   PIC X.
                  05  W99-MULTIHOST2.
                    06  W99-VALUE   PIC X.
                  05  W99-RESEND1.
                    06  W99-VALUE   PIC X.
                  05  W99-RESEND2.
                    06  W99-VALUE   PIC X.
              03  W99-SINSATU-FLG-COMBO.
                04  W99-SINSATU-FLG.
                  05  W99-VALUE   PIC X(30).
                04  W99-SINSATU-FLG-COUNT   PIC S9(9)   BINARY.
                04  W99-SINSATU-FLG-ITEM
                        PIC X(30)   OCCURS  5 TIMES.

