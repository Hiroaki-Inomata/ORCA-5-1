        01  HCM08V01.
            02  HCM08V01-SYORIKBN   PIC X(14).
            02  HCM08V01-SYORIYMD   PIC X(24).
            02  HCM08V01-PAGE   PIC X(3).
            02  HCM08V01-NAIYO-TBL      OCCURS  30 TIMES.
              03  HCM08V01-BTUNAME    PIC X(10).
              03  HCM08V01-BRMNUM PIC X(6).
              03  HCM08V01-SRYKA  PIC X(8).
              03  HCM08V01-PTNUM  PIC X(10).
              03  HCM08V01-PTNUM2 PIC X(20).
              03  HCM08V01-NAME   PIC X(30).
              03  HCM08V01-SEX    PIC X(2).
              03  HCM08V01-BIRTHDAY   PIC X(22).
              03  HCM08V01-DENPNUM    PIC X(7).
              03  HCM08V01-SRYMD  PIC X(5).
              03  HCM08V01-SEIKYU PIC X(12).
              03  HCM08V01-RYOSYU PIC X(12).
              03  HCM08V01-MISYU  PIC X(12).
              03  HCM08V01-JYOTAI PIC X(16).
            02  HCM08V01-TTL-TBL        OCCURS  3 TIMES.
              03  HCM08V01-TTLMSG PIC X(10).
              03  HCM08V01-SEIKYUTTL  PIC X(12).
              03  HCM08V01-RYOSYUTTL  PIC X(12).
              03  HCM08V01-MISYUTTL   PIC X(12).
            02  HCM08V01-SEIKYUTTLMSG   PIC X(10).
            02  HCM08V01-RYOSYUTTLMSG   PIC X(10).
            02  HCM08V01-MISYUTTLMSG    PIC X(10).
