        01  HCM09V02.
            02  HCM09V02-TEISYUTUSAKI   PIC X(6).
            02  HCM09V02-PTNUM  PIC X(20).
            02  HCM09V02-SRYY   PIC X(2).
            02  HCM09V02-SRYM   PIC X(2).
            02  HCM09V02-PREFNUM    PIC X(2).
            02  HCM09V02-HOSPCDN    PIC X(20).
            02  HCM09V02-KHNTEN2    PIC X(7)    OCCURS  2 TIMES.
            02  HCM09V02-KHNTEN1    PIC X(7).
            02  HCM09V02-SRYKBN PIC X(2)    OCCURS  35 TIMES.
            02  HCM09V02-TEKIYO PIC X(52)   OCCURS  35 TIMES.
            02  HCM09V02-KOUTEN PIC X(7)    OCCURS  35 TIMES.
            02  HCM09V02-TOTALTEN   PIC X(7)    OCCURS  3 TIMES.
            02  HCM09V02-FTNMONEY   PIC X(8)    OCCURS  3 TIMES.
            02  HCM09V02-OCR1   PIC X(67).
            02  HCM09V02-OCR2   PIC X(67).
            02  HCM09V02-OCR3   PIC X(67).
            02  HCM09V02-RJNFTNJANUM.
              03  HCM09V02-RJNFTNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM09V02-KOH-TBL1.
              03  HCM09V02-KOHFTNJANUM1.
                04  HCM09V02-KOHFTNJANUM1-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM09V02-KOHFTNJANUM1N  PIC X(20).
              03  HCM09V02-KOHJKYSNUM1.
                04  HCM09V02-KOHJKYSNUM1-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM09V02-KOHJKYSNUM1N   PIC X(20).
            02  HCM09V02-KOH-TBL2.
              03  HCM09V02-KOHFTNJANUM2.
                04  HCM09V02-KOHFTNJANUM2-S
                        PIC X(1)    OCCURS  8 TIMES.
              03  HCM09V02-KOHFTNJANUM2N  PIC X(20).
              03  HCM09V02-KOHJKYSNUM2.
                04  HCM09V02-KOHJKYSNUM2-S
                        PIC X(1)    OCCURS  7 TIMES.
              03  HCM09V02-KOHJKYSNUM2N   PIC X(20).
            02  HCM09V02-HKNJANUM.
              03  HCM09V02-HKNJANUM-S
                      PIC X(1)    OCCURS  8 TIMES.
            02  HCM09V02-KYURATE    PIC X(8).
            02  HCM09V02-RJNJKYSNUM.
              03  HCM09V02-RJNJKYSNUM-S
                      PIC X(1)    OCCURS  7 TIMES.
            02  HCM09V02-HKNSBT1    PIC X(6).
            02  HCM09V02-HKNSBT2    PIC X(6).
            02  HCM09V02-HONKZK PIC X(10).
            02  HCM09V02-KIGO   PIC X(34).
            02  HCM09V02-NUM    PIC X(34).
            02  HCM09V02-KANANAME   PIC X(40).
            02  HCM09V02-KANANAME2  PIC X(50).
            02  HCM09V02-SEXNUM PIC X(2).
            02  HCM09V02-BIRTHDAY   PIC X(22).
            02  HCM09V02-JIYU   PIC X(20).
            02  HCM09V02-SEXN   PIC X(2).
            02  HCM09V02-TOKKI  PIC X(12)   OCCURS  4 TIMES.
            02  HCM09V02-HOSPTEL    PIC X(15).
            02  HCM09V02-SRYKA  PIC X(30).
            02  HCM09V02-BEDSU  PIC X(4).
            02  HCM09V02-BYOMEI PIC X(54)   OCCURS  5 TIMES.
            02  HCM09V02-SRYYMD PIC X(26)   OCCURS  5 TIMES.
            02  HCM09V02-TENKI  PIC X(10)   OCCURS  5 TIMES.
            02  HCM09V02-JINISSU    PIC X(2).
            02  HCM09V02-JINISSU1   PIC X(2).
            02  HCM09V02-JINISSU2   PIC X(2).
            02  HCM09V02-TENSU  PIC X(7)    OCCURS  35 TIMES.
            02  HCM09V02-KAISU  PIC X(3)    OCCURS  35 TIMES.
            02  HCM09V02-KHNTEN PIC X(7)    OCCURS  5 TIMES.
            02  HCM09V02-HOSPNAME   PIC X(100).
            02  HCM09V02-HOSPADRS   PIC X(100).
            02  HCM09V02-NAME   PIC X(50).
            02  HCM09V02-HEAD1-G.
              03  HCM09V02-HEAD1
                      PIC X(46)   OCCURS  2 TIMES.
            02  HCM09V02-HEAD2-G.
              03  HCM09V02-HEAD2
                      PIC X(2)    OCCURS  7 TIMES.
            02  HCM09V02-HEAD3-G.
              03  HCM09V02-HEAD3
                      PIC X(50)   OCCURS  2 TIMES.
            02  HCM09V02-HEAD4-G.
              03  HCM09V02-HEAD4
                      PIC X(62)   OCCURS  2 TIMES.
            02  HCM09V02-BODY   PIC X(52).
            02  HCM09V02-FOOT-G.
              03  HCM09V02-FOOT
                      PIC X(46)   OCCURS  4 TIMES.
            02  HCM09V02-SHOSHINTIME    PIC X(10).
            02  HCM09V02-TENKIYMD   PIC X(6)    OCCURS  5 TIMES.
            02  HCM09V02-FTNMONEYSAI    PIC X(11).
            02  HCM09V02-RENNUM PIC X(7).
            02  HCM09V02-SRYKA2 PIC X(60).
            02  HCM09V02-KHNTEN3    PIC X(7).
            02  HCM09V02-SONOTA PIC X(10).
            02  HCM09V02-CST-FTNJANUM   PIC X(8).
            02  HCM09V02-CST-JKYSNUM    PIC X(7).
            02  HCM09V02-CST-KYUFUGAI   PIC X(6).
            02  HCM09V02-SRYG   PIC X(4).
