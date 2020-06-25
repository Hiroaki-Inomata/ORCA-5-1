        01  HCM37.
            02  HCM37-PAGE  PIC X(3).
            02  HCM37-HKNSBT1   PIC X(6).
            02  HCM37-HKNSBT2   PIC X(6).
            02  HCM37-HONKZK    PIC X(10).
            02  HCM37-SRYM  PIC X(16).
            02  HCM37-HOSPCD    PIC X(7).
            02  HCM37-HOSPNAME  PIC X(40).
            02  HCM37-SRYKA PIC X(30).
            02  HCM37-NAME  PIC X(40).
            02  HCM37-BIRTHG
                    PIC X(2)    OCCURS  4 TIMES.
            02  HCM37-BIRTHY    PIC X(4).
            02  HCM37-BIRTHM    PIC X(4).
            02  HCM37-BIRTHD    PIC X(4).
            02  HCM37-NYUGAIKBN
                    PIC X(2)    OCCURS  2 TIMES.
            02  HCM37-HKNJANUM  PIC X(8).
            02  HCM37-RJNFTNJANUM   PIC X(8).
            02  HCM37-RJNJKYSNUM    PIC X(7).
            02  HCM37-KOHFTNJANUM   PIC X(8).
            02  HCM37-KOHJKYSNUM    PIC X(7).
            02  HCM37-DEMPNUM   PIC X(10).
            02  HCM37-NAIYO
                    PIC X(100)  OCCURS  32 TIMES.
            02  HCM37-KIGO  PIC X(40).
            02  HCM37-NUM   PIC X(40).
