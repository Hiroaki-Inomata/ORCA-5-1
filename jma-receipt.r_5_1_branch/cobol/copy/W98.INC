            02  W98.
              03  W98-FRA-HATTA.
                04  W98-FIXED2.
                  05  W98-FIXED2-STATE    PIC S9(09) BINARY.
                  05  W98-CHK-TABLE.
                    06  W98-CHK-TABLE1    OCCURS  26.
                      07  W98-CHK-TABLE2  OCCURS  26.
                        08  W98-CHK-STYLE PIC X(20).
                        08  W98-FLG-VALUE PIC X(2).
              03  W98-HAITAUMU-COMBO.
                04  W98-HAITAUMU.
                  05  W98-VALUE   PIC X(21).
                04  W98-COUNT   PIC S9(9)   BINARY.
                04  W98-ITEM    PIC X(21)   OCCURS  2 TIMES.
              03  W98-YOKO-TABLE.
                04  W98-YOKO-TABLE1    OCCURS  26.
                  05  W98-YOKO-VALUE   PIC X(3).
              03  W98-TATE-TABLE.
                04  W98-TATE-TABLE1    OCCURS  26.
                  05  W98-TATE-VALUE   PIC X(15).
