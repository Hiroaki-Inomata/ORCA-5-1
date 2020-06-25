        01  HCM52.
            02  HCM52-HEAD.
              03  HCM52-NYUGAI    PIC X(8).
              03  HCM52-HSRYYMD   PIC X(64).
              03  HCM52-HSRYKA    PIC X(10).
              03  HCM52-SAKYMD    PIC X(22).
              03  HCM52-PAGE  PIC X(5).
              03  HCM52-SYOSINMSG PIC X(10).
              03  HCM52-SAISINMSG PIC X(10).
              03  HCM52-SYOSAIMSG PIC X(10).
            02  HCM52-SKYTBL            OCCURS  4 TIMES.
              03  HCM52-GOKEI PIC X(8).
              03  HCM52-HOKEN PIC X(8).
              03  HCM52-SYAHON    PIC X(8).
              03  HCM52-SYAKZK    PIC X(8).
              03  HCM52-SYARJN    PIC X(8).
              03  HCM52-KOKIPN    PIC X(8).
              03  HCM52-CHOSEI    PIC X(8).
              03  HCM52-KOKRJN    PIC X(8).
              03  HCM52-KOHTAN    PIC X(8).
              03  HCM52-GENMEN    PIC X(8).
              03  HCM52-ROUSAI    PIC X(8).
              03  HCM52-JIBAI PIC X(8).
              03  HCM52-JIHSRY    PIC X(8).
              03  HCM52-KOKTAI    PIC X(8).
              03  HCM52-JIHHKN    PIC X(8).
            02  HCM52-JIHITBL.
              03  HCM52-TTLJIHIMSG    PIC X(10).
              03  HCM52-TTLJIHI   PIC X(8).
              03  HCM52-JIHIMSG
                      PIC X(10)   OCCURS  10 TIMES.
              03  HCM52-JIHI
                      PIC X(8)    OCCURS  10 TIMES.
            02  HCM52-NYUKINTBL.
              03  HCM52-TTLHOHOMSG    PIC X(10).
              03  HCM52-TTLNYUKIN PIC X(8).
              03  HCM52-HOHOMSG
                      PIC X(10)   OCCURS  15 TIMES.
              03  HCM52-NYUKIN
                      PIC X(8)    OCCURS  15 TIMES.
