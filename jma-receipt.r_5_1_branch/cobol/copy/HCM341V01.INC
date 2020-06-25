        01  HCM341V01.
            02  HCM341V01-COMMENT   PIC X(100).
            02  HCM341V01-SRYYM PIC X(16).
            02  HCM341V01-BTUNAME   PIC X(30).
            02  HCM341V01-NYUGAI    PIC X(4).
            02  HCM341V01-SAKYMD    PIC X(22).
            02  HCM341V01-PTNUM PIC X(10).
            02  HCM341V01-PAGE  PIC X(3).
            02  HCM341V01-PTNUM2    PIC X(20).
            02  HCM341V01-KANANAME  PIC X(50).
            02  HCM341V01-NAME  PIC X(50).
            02  HCM341V01-BIRTHDAY  PIC X(22).
            02  HCM341V01-SRYKA
                    PIC X(16)   OCCURS  2 TIMES.
            02  HCM341V01-NYUINYMD  PIC X(22).
            02  HCM341V01-TAIINYMD  PIC X(22).
            02  HCM341V01-LBLSRYKA  PIC X(6).
            02  HCM341V01-LBLNYUINYMD   PIC X(6).
            02  HCM341V01-LBLTAIINYMD   PIC X(6).
            02  HCM341V01-HKNAREA       OCCURS  6 TIMES.
              03  HCM341V01-HKNCOMBINUM   PIC X(4).
              03  HCM341V01-HKNCOMBIMEI   PIC X(30).
              03  HCM341V01-TEKSTYMD  PIC X(9).
              03  HCM341V01-TEKEDYMD  PIC X(9).
              03  HCM341V01-HKNNAME   PIC X(10).
              03  HCM341V01-HKNJANUM  PIC X(8).
              03  HCM341V01-HONKZK    PIC X(4).
              03  HCM341V01-FTNRATE   PIC X(4).
              03  HCM341V01-CONTKBN   PIC X(4).
              03  HCM341V01-KIGO  PIC X(34).
              03  HCM341V01-NUM   PIC X(34).
            02  HCM341V01-BYOMEIAREA            OCCURS  10 TIMES.
              03  HCM341V01-BYOMEI    PIC X(52).
              03  HCM341V01-BYOSRYKA  PIC X(4).
              03  HCM341V01-SYUBYO    PIC X(2).
              03  HCM341V01-SRYYMD    PIC X(9).
              03  HCM341V01-TENKI PIC X(10).
            02  HCM341V01-SRYAREA       OCCURS  28 TIMES.
              03  HCM341V01-HKNCOMBINUM2  PIC X(4).
              03  HCM341V01-SRYKBN    PIC X(2).
              03  HCM341V01-MARK  PIC X(4).
              03  HCM341V01-TEKIYO    PIC X(52).
              03  HCM341V01-DAY
                      PIC X(3)    OCCURS  31 TIMES.
            02  HCM341V01-KIKAN PIC X(20).
            02  HCM341V01-SRYAREA2      OCCURS  28 TIMES.
              03  HCM341V01-TOTAL PIC X(11).
            02  HCM341V01-BYOMEI2
                    PIC X(56)   OCCURS  10 TIMES.
