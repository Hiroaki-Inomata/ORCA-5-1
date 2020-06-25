        01  HCM68.
            02  HCM68-TITLE PIC X(80).
            02  HCM68-PAGE  PIC X(5).
            02  HCM68-SAKYMD    PIC X(22).
            02  HCM68-NAIYO PIC X(50).
            02  HCM68-MEISAI            OCCURS  30 TIMES.
              03  HCM68-NO    PIC X(5).
              03  HCM68-YKZKBN    PIC X(4).
              03  HCM68-OLDCD PIC X(9).
              03  HCM68-NEWCD PIC X(9).
              03  HCM68-OLDNAME   PIC X(72).
              03  HCM68-OLDTANI   PIC X(10).
              03  HCM68-OLDTEN    PIC X(10).
              03  HCM68-NEWNAME   PIC X(72).
              03  HCM68-NEWTANI   PIC X(10).
              03  HCM68-NEWTEN    PIC X(10).
              03  HCM68-OLDINPUTCD    PIC X(20).
              03  HCM68-NEWINPUTCD    PIC X(20).
