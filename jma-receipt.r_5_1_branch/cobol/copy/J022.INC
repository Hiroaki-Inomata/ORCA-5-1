        02  J022.
    ******  02  J022-FIXED1.
              03  J022-SRYYMD.
                04  J022-VALUE  PIC X(14).
              03  J022-DAY-G   OCCURS   9.
                04  J022-DAY-STATE  PIC S9(9)   BINARY.
                04  J022-DAY  PIC S9(3).
              03  J022-MSG1.
                04  J022-VALUE  PIC X(10).
              03  J022-MSG2.
                04  J022-VALUE  PIC X(10).
              03  J022-MSG3.
                04  J022-VALUE  PIC X(10).
