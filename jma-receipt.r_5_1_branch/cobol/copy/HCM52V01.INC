        01  HCM52V01.
            02  HCM52V01-HEAD.
              03  HCM52V01-NYUGAI PIC X(8).
              03  HCM52V01-HSRYYMD    PIC X(64).
              03  HCM52V01-HSRYKA PIC X(10).
              03  HCM52V01-SAKYMD PIC X(22).
              03  HCM52V01-PAGE   PIC X(5).
              03  HCM52V01-SYOSINMSG  PIC X(10).
              03  HCM52V01-SAISINMSG  PIC X(10).
              03  HCM52V01-SYOSAIMSG  PIC X(10).
            02  HCM52V01-SKYTBL         OCCURS  4 TIMES.
              03  HCM52V01-GOKEI  PIC X(8).
              03  HCM52V01-HOKEN  PIC X(8).
              03  HCM52V01-SYAHON PIC X(8).
              03  HCM52V01-SYAKZK PIC X(8).
              03  HCM52V01-SYARJN PIC X(8).
              03  HCM52V01-KOKIPN PIC X(8).
              03  HCM52V01-CHOSEI PIC X(8).
              03  HCM52V01-KOKRJN PIC X(8).
              03  HCM52V01-KOHTAN PIC X(8).
              03  HCM52V01-GENMEN PIC X(8).
              03  HCM52V01-ROUSAI PIC X(8).
              03  HCM52V01-JIBAI  PIC X(8).
              03  HCM52V01-KOUGAI PIC X(8).
              03  HCM52V01-JIHSRY PIC X(8).
              03  HCM52V01-KOKTAI PIC X(8).
              03  HCM52V01-JIHHKN PIC X(8).
            02  HCM52V01-JIHITBL.
              03  HCM52V01-TTLJIHIMSG PIC X(10).
              03  HCM52V01-TTLJIHI    PIC X(8).
              03  HCM52V01-JIHIMSG
                      PIC X(10)   OCCURS  10 TIMES.
              03  HCM52V01-JIHI
                      PIC X(8)    OCCURS  10 TIMES.
            02  HCM52V01-NYUKINTBL.
              03  HCM52V01-TTLHOHOMSG PIC X(10).
              03  HCM52V01-TTLNYUKIN  PIC X(8).
              03  HCM52V01-HOHOMSG
                      PIC X(10)   OCCURS  15 TIMES.
              03  HCM52V01-NYUKIN
                      PIC X(8)    OCCURS  15 TIMES.
