        01  HCM42.
            02  HCM42-NYUGAI    PIC X(10).
            02  HCM42-SRTKBN    PIC X(20).
            02  HCM42-KKNYM PIC X(100).
            02  HCM42-SAKYMD    PIC X(22).
            02  HCM42-PAGE  PIC X(3).
            02  HCM42-NAIYO-TBL         OCCURS  30 TIMES.
              03  HCM42-PTNUM PIC X(20).
              03  HCM42-NAME  PIC X(36).
              03  HCM42-DENPNUM   PIC X(7).
              03  HCM42-SRYKA PIC X(10).
              03  HCM42-SRYMD PIC X(12).
              03  HCM42-SKYKKN    PIC X(20).
              03  HCM42-CHOSEIP   PIC X(11).
              03  HCM42-CHOSEIM   PIC X(11).
            02  HCM42-KACHOSEIP PIC X(11).
            02  HCM42-KACHOSEIM PIC X(11).
            02  HCM42-KACHOSEI  PIC X(11).
            02  HCM42-KACHOSEIKNS   PIC X(6).
            02  HCM42-LBLPAG    PIC X(10).
            02  HCM42-TTLCHOSEIP    PIC X(11).
            02  HCM42-TTLCHOSEIM    PIC X(11).
            02  HCM42-TTLCHOSEI PIC X(11).
            02  HCM42-TTLCHOSEIKNS  PIC X(6).
            02  HCM42-LBLTTL    PIC X(10).
