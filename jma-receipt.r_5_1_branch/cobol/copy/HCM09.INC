        01  HCM09.
            02  HCM09-TEISYUTUSAKI  PIC X(6).
            02  HCM09-PTNUM PIC X(20).
            02  HCM09-SRYY  PIC X(2).
            02  HCM09-SRYM  PIC X(2).
            02  HCM09-PREFNUM   PIC X(2).
            02  HCM09-HOSPCDN   PIC X(20).
            02  HCM09-KHNTEN2   PIC X(7)        OCCURS  2 TIMES.
            02  HCM09-KHNTEN1   PIC X(7).
            02  HCM09-SRYKBN    PIC X(2)        OCCURS  35 TIMES.
            02  HCM09-TEKIYO    PIC X(52)       OCCURS  35 TIMES.
            02  HCM09-KOUTEN    PIC X(7)        OCCURS  35 TIMES.
            02  HCM09-TOTALTEN  PIC X(7)        OCCURS  3 TIMES.
            02  HCM09-FTNMONEY  PIC X(8)        OCCURS  3 TIMES.
            02  HCM09-OCR1  PIC X(67).
            02  HCM09-OCR2  PIC X(67).
            02  HCM09-OCR3  PIC X(67).
            02  HCM09-RJNFTNJANUM   PIC X(8).
            02  HCM09-KOH-TBL1.
              03  HCM09-KOHFTNJANUM1  PIC X(8).
              03  HCM09-KOHFTNJANUM1N PIC X(20).
              03  HCM09-KOHJKYSNUM1   PIC X(7).
              03  HCM09-KOHJKYSNUM1N  PIC X(20).
            02  HCM09-KOH-TBL2.
              03  HCM09-KOHFTNJANUM2  PIC X(8).
              03  HCM09-KOHFTNJANUM2N PIC X(20).
              03  HCM09-KOHJKYSNUM2   PIC X(7).
              03  HCM09-KOHJKYSNUM2N  PIC X(20).
            02  HCM09-HKNJANUM  PIC X(8).
            02  HCM09-KYURATE   PIC X(8).
            02  HCM09-RJNJKYSNUM    PIC X(7).
            02  HCM09-HKNSBT1   PIC X(6).
            02  HCM09-HKNSBT2   PIC X(6).
            02  HCM09-HONKZK    PIC X(10).
            02  HCM09-KIGO  PIC X(34).
            02  HCM09-NUM   PIC X(34).
            02  HCM09-KANANAME  PIC X(30).
            02  HCM09-SEXNUM    PIC X(2).
            02  HCM09-BIRTHDAY  PIC X(22).
            02  HCM09-JIYU  PIC X(20).
            02  HCM09-SEXN  PIC X(2).
            02  HCM09-TOKKI PIC X(12)   OCCURS  4 TIMES.
            02  HCM09-HOSPTEL   PIC X(15).
            02  HCM09-SRYKA PIC X(30).
            02  HCM09-BEDSU PIC X(3).
            02  HCM09-BYOMEI    PIC X(54)       OCCURS  5 TIMES.
            02  HCM09-SRYYMD    PIC X(24)       OCCURS  5 TIMES.
            02  HCM09-TENKI PIC X(10)   OCCURS  5 TIMES.
            02  HCM09-JINISSU   PIC X(2).
            02  HCM09-JINISSU1  PIC X(2).
            02  HCM09-JINISSU2  PIC X(2).
            02  HCM09-TENSU PIC X(7)    OCCURS  35 TIMES.
            02  HCM09-KAISU PIC X(3)    OCCURS  35 TIMES.
            02  HCM09-KHNTEN    PIC X(7)        OCCURS  5 TIMES.
            02  HCM09-HOSPNAME  PIC X(80).
            02  HCM09-HOSPADRS  PIC X(100).
            02  HCM09-NAME  PIC X(50).
            02  HCM09-HEAD1-G.
              03  HCM09-HEAD1 PIC X(46)   OCCURS  2 TIMES.
            02  HCM09-HEAD2-G.
              03  HCM09-HEAD2 PIC X(2)    OCCURS  7 TIMES.
            02  HCM09-HEAD3-G.
              03  HCM09-HEAD3 PIC X(50)   OCCURS  2 TIMES.
            02  HCM09-BODY  PIC X(52).
            02  HCM09-FOOT-G.
              03  HCM09-FOOT  PIC X(46)   OCCURS  4 TIMES.
            02  HCM09-SHOSHINTIME   PIC X(10).
            02  HCM09-TENKIYMD  PIC X(6)        OCCURS  5 TIMES.
            02  HCM09-FTNMONEYSAI   PIC X(11).
            02  HCM09-RENNUM    PIC X(7).
            02  HCM09-SRYKA2    PIC X(60).
            02  HCM09-KHNTEN3   PIC X(7).
            02  HCM09-SONOTA    PIC X(10).
