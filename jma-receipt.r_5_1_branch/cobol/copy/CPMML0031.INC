      ******************************************************************
      *    CPMML0031.INC
      ******************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * ³°Éô¥Õ¥¡¥¤¥ë¤Ë½ÐÎÏ¤¹¤ë¤È¤­¤Ï¡¢ZZZÊÔ½¸¤¹¤ë
      * Ã¢¤·¡¢¹ñÊÝ¤Î¾ì¹ç¤Î¤ß¡Ö00¡×¤òÀßÄê¤¹¤ë
      ************************************************************
       01  MML0031-VALUES.
           02  MML0031-T.
               05  FILLER               PIC  X(3)   VALUE   "001" .
               05  FILLER               PIC  X(40)  
                   VALUE   "À¯ÉÜ´É¾¸" .
               05  FILLER               PIC  X(3)   VALUE   "002" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Á¥°÷" .
               05  FILLER               PIC  X(3)   VALUE   "003" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æü¸Û" .
               05  FILLER               PIC  X(3)   VALUE   "004" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æü¸Û¡¦ÆÃÊÌ" .
               05  FILLER               PIC  X(3)   VALUE   "006" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÁÈ¹ç" .
               05  FILLER               PIC  X(3)   VALUE   "007" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«±Ò´±Åù" .
               05  FILLER               PIC  X(3)   VALUE   "031" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¹ñ²È¶¦ºÑ" .
               05  FILLER               PIC  X(3)   VALUE   "032" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÃÏÊý¶¦ºÑ" .
               05  FILLER               PIC  X(3)   VALUE   "033" .
               05  FILLER               PIC  X(40)  
                   VALUE   "·Ù»¡¶¦ºÑ" .
               05  FILLER               PIC  X(3)   VALUE   "034" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¶µ¿¦¶¦ºÑ" .
               05  FILLER               PIC  X(3)   VALUE   "060" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¹ñÊÝ" .
               05  FILLER               PIC  X(3)   VALUE   "010" .
               05  FILLER               PIC  X(40)  
                   VALUE   "·ëÍ½£³£´¾ò" .
               05  FILLER               PIC  X(3)   VALUE   "011" .
               05  FILLER               PIC  X(40)  
                   VALUE   "µËÍ½£³£µ¾ò" .
               05  FILLER               PIC  X(3)   VALUE   "012" .
               05  FILLER               PIC  X(40)  
                   VALUE   "À¸³èÊÝ¸î" .
               05  FILLER               PIC  X(3)   VALUE   "013" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Àï½ýÎÅÍÜ" .
               05  FILLER               PIC  X(3)   VALUE   "014" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Àï½ý¹¹À¸" .
               05  FILLER               PIC  X(3)   VALUE   "015" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¿È¾ã¹¹À¸" .
               05  FILLER               PIC  X(3)   VALUE   "016" .
               05  FILLER               PIC  X(40)  
                   VALUE   "»ùÆ¸°éÀ®" .
               05  FILLER               PIC  X(3)   VALUE   "017" .
               05  FILLER               PIC  X(40)  
                   VALUE   "»ùÆ¸ÎÅÍÜ" .
               05  FILLER               PIC  X(3)   VALUE   "018" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸¶ÇúÇ§Äê" .
               05  FILLER               PIC  X(3)   VALUE   "019" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸¶Çú°ìÈÌ" .
               05  FILLER               PIC  X(3)   VALUE   "020" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Àº¿À£²£¹¾ò" .
               05  FILLER               PIC  X(3)   VALUE   "021" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Àº¿À£³£²¾ò" .
               05  FILLER               PIC  X(3)   VALUE   "022" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ËãÌô" .
               05  FILLER               PIC  X(3)   VALUE   "023" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Êì»Ò¶µ°é" .
               05  FILLER               PIC  X(3)   VALUE   "027" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ï·¿ÍÊÝ¸±" .
               05  FILLER               PIC  X(3)   VALUE   "041" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ï·¿Í¡¦¸©" .
               05  FILLER               PIC  X(3)   VALUE   "042" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ï·¿Í¡¦»Ô" .
               05  FILLER               PIC  X(3)   VALUE   "051" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÆÃÄê¼À´µ" .
               05  FILLER               PIC  X(3)   VALUE   "052" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¾®»ùËýÀ­" .
               05  FILLER               PIC  X(3)   VALUE   "053" .
               05  FILLER               PIC  X(40)  
                   VALUE   "»ùÆ¸ÊÝ¸î" .
               05  FILLER               PIC  X(3)   VALUE   "067" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Âà¿¦¼Ô" .
               05  FILLER               PIC  X(3)   VALUE   "068" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÆÃÎÅÈñ" .
               05  FILLER               PIC  X(3)   VALUE   "069" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÂàÆÃÎÅÈñ" .
               05  FILLER               PIC  X(3)   VALUE   "063" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÆÃÄê·òÊÝ" .
               05  FILLER               PIC  X(3)   VALUE   "072" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¹ñ²ÈÆÃÄê" .
               05  FILLER               PIC  X(3)   VALUE   "073" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÃÏÊýÆÃÄê" .
               05  FILLER               PIC  X(3)   VALUE   "074" .
               05  FILLER               PIC  X(40)  
                   VALUE   "·Ù»¡ÆÃÄê" .
               05  FILLER               PIC  X(3)   VALUE   "075" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¶µ¿¦ÆÃÄê" .
               05  FILLER               PIC  X(3)   VALUE   "0Z1" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«Èñ£±" .
               05  FILLER               PIC  X(3)   VALUE   "0Z2" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«Èñ£²" .
               05  FILLER               PIC  X(3)   VALUE   "0Z3" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«Èñ£³" .
               05  FILLER               PIC  X(3)   VALUE   "039" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸å´ü¹âÎð¼Ô" .
               05  FILLER               PIC  X(3)   VALUE   "040" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸å´üÆÃÎÅÈñ" .
               05  FILLER               PIC  X(3)   VALUE   "975" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸ø³²" .
               05  FILLER               PIC  X(3)   VALUE   "009" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¶¨²ñ¤±¤ó¤Ý" .
           02  MML0031-R  REDEFINES   MML0031-T.
             03  MML0031-TX           OCCURS  46.
               05  MML0031-HOKEN      PIC  X(3).
               05  MML0031-NAME       PIC  X(40).
      *
       01  MML0031-TMAX               PIC 9(03)  VALUE    46.
      *
