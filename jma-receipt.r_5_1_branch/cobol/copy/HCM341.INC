        01  HCM341.
            02  HCM341-SRYYM    PIC X(16).
            02  HCM341-BTUNAME  PIC X(30).
            02  HCM341-NYUGAI   PIC X(4).
            02  HCM341-SAKYMD   PIC X(22).
            02  HCM341-PTNUM    PIC X(10).
            02  HCM341-PAGE PIC X(3).
            02  HCM341-PTNUM2   PIC X(20).
            02  HCM341-KANANAME PIC X(50).
            02  HCM341-NAME PIC X(50).
            02  HCM341-BIRTHDAY PIC X(22).
            02  HCM341-SRYKA    PIC X(16)       OCCURS  2 TIMES.
            02  HCM341-NYUINYMD PIC X(22).
            02  HCM341-TAIINYMD PIC X(22).
            02  HCM341-LBLSRYKA PIC X(6).
            02  HCM341-LBLNYUINYMD  PIC X(6).
            02  HCM341-LBLTAIINYMD  PIC X(6).
            02  HCM341-HKNAREA          OCCURS  6 TIMES.
              03  HCM341-HKNCOMBINUM  PIC X(4).
              03  HCM341-HKNCOMBIMEI  PIC X(30).
              03  HCM341-TEKSTYMD PIC X(9).
              03  HCM341-TEKEDYMD PIC X(9).
              03  HCM341-HKNNAME  PIC X(10).
              03  HCM341-HKNJANUM PIC X(8).
              03  HCM341-HONKZK   PIC X(4).
              03  HCM341-FTNRATE  PIC X(4).
              03  HCM341-CONTKBN  PIC X(4).
              03  HCM341-KIGO PIC X(34).
              03  HCM341-NUM  PIC X(34).
            02  HCM341-BYOMEIAREA       OCCURS  10 TIMES.
              03  HCM341-BYOMEI   PIC X(52).
              03  HCM341-BYOSRYKA PIC X(4).
              03  HCM341-SYUBYO   PIC X(2).
              03  HCM341-SRYYMD   PIC X(9).
              03  HCM341-TENKI    PIC X(10).
            02  HCM341-SRYAREA          OCCURS  28 TIMES.
              03  HCM341-HKNCOMBINUM2 PIC X(4).
              03  HCM341-SRYKBN   PIC X(2).
              03  HCM341-TEKIYO   PIC X(52).
              03  HCM341-DAY  PIC X(3)    OCCURS  31 TIMES.
