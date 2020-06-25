        02  SESSION.
            03  SESSION-END.
              04  DUMMY   PIC X(256).
            03  SESSION-START.
              04  ARGUMENTS.
                05  USER    PIC X(16).
                05  PASSWORD    PIC X(256).
                05  SESSION-TYPE    PIC X(256).
                05  RESERVE1    PIC X(256).
                05  RESERVE2    PIC X(256).
                05  RESERVE3    PIC X(256).
              04  HTTP-METHOD PIC X(8).
              04  HTTP-STATUS PIC S9(9)   BINARY.
              04  CONTENT-TYPE    PIC X(64).
              04  SESSION-BODY    PIC X(36).
