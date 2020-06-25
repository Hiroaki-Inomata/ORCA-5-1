        01  TBL-LDMAP.
            02  LDMAP-HOSPNUM           PIC 9(2).
            02  LDMAP-LDNAME            PIC X(20).
            02  LDMAP-GROUPNUM          PIC 9(02).
            02  LDMAP-EDANUM            PIC 9(02).
            02  LDMAP-LDMSG             PIC X(20).
            02  LDMAP-DSP-FLG           PIC 9(01). 
            02  LDMAP-HAITA-FLG         PIC 9(01). 
            02  LDMAP-TERMID            PIC X(64).
            02  LDMAP-OPID              PIC X(16).
            02  LDMAP-CHK-LDMAP.
              03  LDMAP-CHK-LDNAME      PIC X(20)
                                        OCCURS  50 TIMES.
