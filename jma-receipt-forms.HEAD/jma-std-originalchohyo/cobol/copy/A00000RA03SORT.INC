           03  SORT-KEY.
      *    診療区分(前2桁)
             05  SORT-SRYKBN              PIC 9(02).
      *    診療区分(3桁)
             05  SORT-SRYSYUKBN           PIC 9(03).
      *    用法テーブル
             05  SORT-YOHO-TBL            OCCURS 10.
      *    用法CD
               07  SORT-YOHOCD            PIC X(09).
      *    用法名称
               07  SORT-YOHONAME          PIC X(140).
      *    剤型区分(水剤用=3、それ以外=0)
               07  SORT-ZAIGATAKBN        PIC 9(01).
           03  SORT-AREA.
      *    診療内容テーブル
             05  SORT-SRYACT-TBL          OCCURS 50.
      *    診療CD
               07  SORT-SRYCD             PIC X(80).
      *    診療名称
               07  SORT-SRYNAME           PIC X(140).
      *    数量
               07  SORT-SRYSURYO          PIC 9(05)V9(05).
      *    コメント名称
               07  SORT-INPUTCOMENT       PIC X(140).
      *    剤回数
             05  SORT-ZAIKAIKEI           PIC 9(03).
      *
