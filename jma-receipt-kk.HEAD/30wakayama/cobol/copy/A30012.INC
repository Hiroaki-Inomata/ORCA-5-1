        01  HC01.
            02  HC01-WORK   PIC X(16).
            02  HC01-TUZUKI PIC X(8).
            02  HC01-HPTNUM PIC X(16).
            02  HC01-KIGONUM    PIC X(32).
            02  HC01-HIHKNJNAME PIC X(22).
            02  HC01-PTNUM  PIC X(20).
            02  HC01-KANANAME   PIC X(30).
            02  HC01-KANANAME2  PIC X(60).
            02  HC01-NAME   PIC X(30).
            02  HC01-NAME2  PIC X(60).
            02  HC01-JKOH-TBL           OCCURS  3 TIMES.
              03  HC01-JKYSNO PIC X(14).
            02  HC01-FKOH-TBL           OCCURS  3 TIMES.
              03  HC01-FTNSNO PIC X(16).
            02  HC01-OTOKO  PIC X(2).
            02  HC01-ONNA   PIC X(2).
            02  HC01-TENKI-TBL          OCCURS  20 TIMES.
              03  HC01-TENKIG.
                04  HC01-TENKI
                        PIC X(2)    OCCURS  3 TIMES.
            02  HC01-STRYMD-TBL         OCCURS  20 TIMES.
              03  HC01-STRYMDG.
                04  HC01-STRYMD
                        PIC X(2)    OCCURS  3 TIMES.
            02  HC01-ENDYMD-TBL         OCCURS  20 TIMES.
              03  HC01-ENDYMDG.
                04  HC01-ENDYMD
                        PIC X(2)    OCCURS  3 TIMES.
            02  HC01-TANJYOG    PIC X(4).
            02  HC01-TANJYOY    PIC X(2).
            02  HC01-TANJYOM    PIC X(2).
            02  HC01-TANJYOD    PIC X(2).
            02  HC01-NENREI PIC X(3).
            02  HC01-YUKOG  PIC X(4).
            02  HC01-YUKOY  PIC X(2).
            02  HC01-YUKOM  PIC X(2).
            02  HC01-YUKOD  PIC X(2).
            02  HC01-SIKAKUG    PIC X(4).
            02  HC01-SIKAKUY    PIC X(2).
            02  HC01-SIKAKUM    PIC X(2).
            02  HC01-SIKAKUD    PIC X(2).
            02  HC01-ADRS   PIC X(64).
            02  HC01-ADRSTEL    PIC X(20).
            02  HC01-KINADRS    PIC X(40).
            02  HC01-KINNAME    PIC X(30).
            02  HC01-KINADRSTEL PIC X(20).
            02  HC01-HKNADRS    PIC X(30).
            02  HC01-HKNNAME    PIC X(30).
            02  HC01-HKNADRSTEL PIC X(20).
            02  HC01-BYOMEI-TBL         OCCURS  20 TIMES.
              03  HC01-BYOMEIG.
                04  HC01-BYOMEI PIC X(112).
            02  HC01-JKOHN-TBL          OCCURS  3 TIMES.
              03  HC01-JKYSNON    PIC X(20).
            02  HC01-FKOHN-TBL          OCCURS  3 TIMES.
              03  HC01-FTNSNON    PIC X(20).
            02  HC01-YUBINNO    PIC X(8).
            02  HC01-TANSEIDONAME   PIC X(40).
            02  HC01-HKNCOMBI   PIC X(80).
            02  HC01-FTNRATE    PIC X(3).
            02  HC01-HONKZ  PIC X(4).
            02  HC01-KOMENTO    PIC X(40).
            02  HC01-GENGO1-TBL         OCCURS  4 TIMES.
              03  HC01-GENGO1 PIC X(2).
            02  HC01-GENGO2     OCCURS  2 TIMES.
              03  HC01-GENGO2 PIC X(2).
            02  HC01-KIGO   PIC X(20).
            02  HC01-BANGO  PIC X(20).
