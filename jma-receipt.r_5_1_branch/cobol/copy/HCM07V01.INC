        01  HCM07V01.
            02  HCM07V01-SYORIKBN   PIC X(14).
            02  HCM07V01-SRYKA  PIC X(40).
            02  HCM07V01-SYORIYMD   PIC X(24).
            02  HCM07V01-PAGE   PIC X(3).
            02  HCM07V01-NAIYO-TBL      OCCURS  30 TIMES.
              03  HCM07V01-PTNUM  PIC X(20).
              03  HCM07V01-NAME   PIC X(36).
              03  HCM07V01-SEX    PIC X(2).
              03  HCM07V01-BIRTHDAY   PIC X(22).
              03  HCM07V01-DENPNUM    PIC X(7).
              03  HCM07V01-SRYMD  PIC X(12).
              03  HCM07V01-SEIKYU PIC X(11).
              03  HCM07V01-RYOSYU PIC X(11).
              03  HCM07V01-MISYU  PIC X(11).
              03  HCM07V01-JYOTAI PIC X(10).
            02  HCM07V01-TTL-TBL        OCCURS  3 TIMES.
              03  HCM07V01-TTLMSG PIC X(10).
              03  HCM07V01-SEIKYUTTL  PIC X(11).
              03  HCM07V01-RYOSYUTTL  PIC X(11).
              03  HCM07V01-MISYUTTL   PIC X(11).
            02  HCM07V01-SEIKYUTTLMSG   PIC X(10).
            02  HCM07V01-RYOSYUTTLMSG   PIC X(10).
            02  HCM07V01-MISYUTTLMSG    PIC X(10).
