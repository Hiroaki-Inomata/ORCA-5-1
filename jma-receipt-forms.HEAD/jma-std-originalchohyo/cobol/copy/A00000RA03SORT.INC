           03  SORT-KEY.
      *    fÃæª(O2)
             05  SORT-SRYKBN              PIC 9(02).
      *    fÃæª(3)
             05  SORT-SRYSYUKBN           PIC 9(03).
      *    p@e[u
             05  SORT-YOHO-TBL            OCCURS 10.
      *    p@CD
               07  SORT-YOHOCD            PIC X(09).
      *    p@¼Ì
               07  SORT-YOHONAME          PIC X(140).
      *    Ü^æª(Üp=3A»êÈO=0)
               07  SORT-ZAIGATAKBN        PIC 9(01).
           03  SORT-AREA.
      *    fÃàee[u
             05  SORT-SRYACT-TBL          OCCURS 50.
      *    fÃCD
               07  SORT-SRYCD             PIC X(80).
      *    fÃ¼Ì
               07  SORT-SRYNAME           PIC X(140).
      *    Ê
               07  SORT-SRYSURYO          PIC 9(05)V9(05).
      *    Rg¼Ì
               07  SORT-INPUTCOMENT       PIC X(140).
      *    Üñ
             05  SORT-ZAIKAIKEI           PIC 9(03).
      *
