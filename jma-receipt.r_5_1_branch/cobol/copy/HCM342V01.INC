        01  HCM342V01.
            02  HCM342V01-COMMENT   PIC X(100).
            02  HCM342V01-SRYYM PIC X(16).
            02  HCM342V01-BTUNAME   PIC X(30).
            02  HCM342V01-NYUGAI    PIC X(4).
            02  HCM342V01-SAKYMD    PIC X(22).
            02  HCM342V01-PTNUM PIC X(10).
            02  HCM342V01-PAGE  PIC X(3).
            02  HCM342V01-PTNUM2    PIC X(20).
            02  HCM342V01-KANANAME  PIC X(50).
            02  HCM342V01-NAME  PIC X(50).
            02  HCM342V01-BIRTHDAY  PIC X(22).
            02  HCM342V01-SRYKA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCM342V01-NYUINYMD  PIC X(22).
            02  HCM342V01-TAIINYMD  PIC X(22).
            02  HCM342V01-LBLSRYKA  PIC X(6).
            02  HCM342V01-LBLNYUINYMD   PIC X(6).
            02  HCM342V01-LBLTAIINYMD   PIC X(6).
            02  HCM342V01-SRYAREA       OCCURS  39 TIMES.
              03  HCM342V01-SRYKBN    PIC X(2).
              03  HCM342V01-MARK  PIC X(4).
              03  HCM342V01-HKNCOMBINUM2  PIC X(4).
              03  HCM342V01-TEKIYO    PIC X(52).
              03  HCM342V01-DAY
                      PIC X(3)    OCCURS  31 TIMES.
            02  HCM342V01-KIKAN PIC X(20).
            02  HCM342V01-SRYAREA2      OCCURS  39 TIMES.
              03  HCM342V01-TOTAL PIC X(11).
