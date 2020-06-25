        01  HCM342.
            02  HCM342-SRYYM    PIC X(16).
            02  HCM342-BTUNAME  PIC X(30).
            02  HCM342-NYUGAI   PIC X(4).
            02  HCM342-SAKYMD   PIC X(22).
            02  HCM342-PTNUM    PIC X(10).
            02  HCM342-PAGE PIC X(3).
            02  HCM342-PTNUM2   PIC X(20).
            02  HCM342-KANANAME PIC X(50).
            02  HCM342-NAME PIC X(50).
            02  HCM342-BIRTHDAY PIC X(22).
            02  HCM342-SRYKA    PIC X(16)       OCCURS  2 TIMES.
            02  HCM342-NYUINYMD PIC X(22).
            02  HCM342-TAIINYMD PIC X(22).
            02  HCM342-LBLSRYKA PIC X(6).
            02  HCM342-LBLNYUINYMD  PIC X(6).
            02  HCM342-LBLTAIINYMD  PIC X(6).
            02  HCM342-SRYAREA          OCCURS  39 TIMES.
              03  HCM342-SRYKBN   PIC X(2).
              03  HCM342-HKNCOMBINUM2 PIC X(4).
              03  HCM342-TEKIYO   PIC X(52).
              03  HCM342-DAY  PIC X(3)    OCCURS  31 TIMES.
