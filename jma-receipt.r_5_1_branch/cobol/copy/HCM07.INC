        01  HCM07.
            02  HCM07-SRYKA PIC X(40).
            02  HCM07-SYORIYMD  PIC X(24).
            02  HCM07-PAGE  PIC X(3).
            02  HCM07-NAIYO-TBL         OCCURS  30 TIMES.
              03  HCM07-PTNUM PIC X(20).
              03  HCM07-NAME  PIC X(36).
              03  HCM07-SEX   PIC X(2).
              03  HCM07-BIRTHDAY  PIC X(22).
              03  HCM07-DENPNUM   PIC X(7).
              03  HCM07-SRYMD PIC X(12).
              03  HCM07-SEIKYU    PIC X(11).
              03  HCM07-RYOSYU    PIC X(11).
              03  HCM07-MISYU PIC X(11).
              03  HCM07-JYOTAI    PIC X(10).
            02  HCM07-TTL-TBL           OCCURS  3 TIMES.
              03  HCM07-TTLMSG    PIC X(10).
              03  HCM07-SEIKYUTTL PIC X(11).
              03  HCM07-RYOSYUTTL PIC X(11).
              03  HCM07-MISYUTTL  PIC X(11).
            02  HCM07-SEIKYUTTLMSG  PIC X(10).
            02  HCM07-RYOSYUTTLMSG  PIC X(10).
            02  HCM07-MISYUTTLMSG   PIC X(10).
