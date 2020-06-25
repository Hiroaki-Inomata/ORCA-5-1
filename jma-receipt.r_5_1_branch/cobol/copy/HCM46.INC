        01  HCM46.
            02  HCM46-DENPPRTYM PIC X(18).
            02  HCM46-SAKUSEIYMD    PIC X(22).
            02  HCM46-PAGE  PIC X(3).
            02  HCM46-MEISAI            OCCURS  23 TIMES.
              03  HCM46-CD    PIC X(3).
              03  HCM46-PTNUM PIC X(10).
              03  HCM46-PTNUM2    PIC X(20).
              03  HCM46-NAME  PIC X(30).
              03  HCM46-DENPNUM   PIC X(7).
              03  HCM46-SEIKYU    PIC X(12).
              03  HCM46-SEIKYUPTNUM   PIC X(12).
              03  HCM46-SEIKYUSETAI   PIC X(12).
              03  HCM46-KIGO  PIC X(20).
              03  HCM46-NUM   PIC X(20).
              03  HCM46-LABEL PIC X(10).
              03  HCM46-HIHKNJA   PIC X(22).
              03  HCM46-NYUGAI    PIC X(4).
              03  HCM46-SRYYM PIC X(20).
              03  HCM46-SEN   PIC X(50).
              03  HCM46-SEN2  PIC X(104).
            02  HCM46-GSEIKYU   PIC X(12).
            02  HCM46-GSEIKYUPTNUM  PIC X(12).
            02  HCM46-GSEIKYUSETAI  PIC X(12).
