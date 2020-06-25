      ******************************************************************
      *    CPCLAIM007.INC
      ******************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.02.01    NACL-Â¿¡¹Ç¼  08/03/14  £È£²£°Ç¯£´·î²þÀµÂÐ±þ
      ************************************************************
       01  CLAIM007-VALUES.
           02  CLAIM007-T.
             03  CLAIM007-T000.
      *        05  FILLER               PIC  X(2)   VALUE   "00" .
               05  FILLER               PIC  X(3)   VALUE   "000" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¤½¤ÎÂ¾" .
             03  CLAIM007-T110.
      *        05  CLAIM007-ORCA-T110   PIC  X(2)   VALUE   "11" .
               05  CLAIM007-CLAIM-T110  PIC  X(3)   VALUE   "110" .
               05  CLAIM007-NAME-T110   PIC  X(40)  
                   VALUE   "½é¿Ç" .
             03  CLAIM007-T120.
      *        05  CLAIM007-ORCA-T120   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T120  PIC  X(3)   VALUE   "120" .
               05  CLAIM007-NAME-T120   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡ÊºÆ¿Ç¡Ë" .
             03  CLAIM007-T122.
      *        05  CLAIM007-ORCA-T122   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T122  PIC  X(3)   VALUE   "122" .
               05  CLAIM007-NAME-T122   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡Ê³°Íè´ÉÍý²Ã»»¡Ë" .
             03  CLAIM007-T123.
      *        05  CLAIM007-ORCA-T123   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T123  PIC  X(3)   VALUE   "123" .
               05  CLAIM007-NAME-T123   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡Ê»þ´Ö³°¡Ë" .
             03  CLAIM007-T124.
      *        05  CLAIM007-ORCA-T124   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T124  PIC  X(3)   VALUE   "124" .
               05  CLAIM007-NAME-T124   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡ÊµÙÆü¡Ë" .
             03  CLAIM007-T125.
      *        05  CLAIM007-ORCA-T125   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T125  PIC  X(3)   VALUE   "125" .
               05  CLAIM007-NAME-T125   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡Ê¿¼Ìë¡Ë" .
             03  CLAIM007-T126.
      *        05  CLAIM007-ORCA-T126   PIC  X(2)   VALUE   "12" .
               05  CLAIM007-CLAIM-T126  PIC  X(3)   VALUE   "126" .
               05  CLAIM007-NAME-T126   PIC  X(40)  
                   VALUE   "ºÆ¿Ç¡ÊÌë´Ö¡¦ÁáÄ«¡Ë" .
             03  CLAIM007-T130.
      *        05  CLAIM007-ORCA-T130   PIC  X(2)   VALUE   "13" .
               05  CLAIM007-CLAIM-T130  PIC  X(3)   VALUE   "130" .
               05  CLAIM007-NAME-T130   PIC  X(40)  
                   VALUE   "»ØÆ³" .
             03  CLAIM007-T140.
      *        05  CLAIM007-ORCA-T140   PIC  X(2)   VALUE   "14" .
               05  CLAIM007-CLAIM-T140  PIC  X(3)   VALUE   "140" .
               05  CLAIM007-NAME-T140   PIC  X(40)  
                   VALUE   "ºßÂð" .
             03  CLAIM007-T210.
      *        05  CLAIM007-ORCA-T210   PIC  X(2)   VALUE   "21" .
               05  CLAIM007-CLAIM-T210  PIC  X(3)   VALUE   "210" .
               05  CLAIM007-NAME-T210   PIC  X(40)  
                   VALUE   "ÅêÌô¡ÊÆâÉþ¡¦Ä´ºÞ¡Ë¡ÊÆþ±¡³°¡Ë" .
             03  CLAIM007-T210.
      *        05  CLAIM007-ORCA-T220   PIC  X(2)   VALUE   "22" .
               05  CLAIM007-CLAIM-T220  PIC  X(3)   VALUE   "220" .
               05  CLAIM007-NAME-T220   PIC  X(40)  
                   VALUE   "ÅêÌô¡ÊÆÖÉþ¡Ë" .
             03  CLAIM007-T230.
      *        05  CLAIM007-ORCA-T230   PIC  X(2)   VALUE   "23" .
               05  CLAIM007-CLAIM-T230  PIC  X(3)   VALUE   "230" .
               05  CLAIM007-NAME-T230   PIC  X(40)  
                   VALUE   "ÅêÌô¡Ê³°ÍÑ¡¦Ä´ºÞ¡Ë¡ÊÆþ±¡³°¡Ë" .
             03  CLAIM007-T240.
      *        05  FILLER               PIC  X(2)   VALUE   "24" .
               05  FILLER               PIC  X(3)   VALUE   "240" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÅêÌô¡ÊÄ´ºÞ¡Ë¡ÊÆþ±¡¡Ë" .
             03  CLAIM007-T250.
      *        05  FILLER               PIC  X(2)   VALUE   "25" .
               05  FILLER               PIC  X(3)   VALUE   "250" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÅêÌô¡Ê½èÊý¡Ë" .
             03  CLAIM007-T260.
      *        05  FILLER               PIC  X(2)   VALUE   "26" .
               05  FILLER               PIC  X(3)   VALUE   "260" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÅêÌô¡ÊËãÆÇ¡Ë" .
             03  CLAIM007-T270.
      *        05  FILLER               PIC  X(2)   VALUE   "27" .
               05  FILLER               PIC  X(3)   VALUE   "270" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÅêÌô¡ÊÄ´´ð¡Ë" .
             03  CLAIM007-T300.
      *        05  FILLER               PIC  X(2)   VALUE   "30" .
               05  FILLER               PIC  X(3)   VALUE   "300" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ãí¼Í¡ÊÀ¸Êª³ØÅªÀ½ºÞ¡¦ÀºÌ©»ýÂ³ÅÀÅ©¡¦ËãÌô¡Ë" .
             03  CLAIM007-T311.
      *        05  FILLER               PIC  X(2)   VALUE   "31" .
               05  FILLER               PIC  X(3)   VALUE   "311" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ãí¼Í¡ÊÈé²¼¶ÚÆùÆâ¡Ë" .
             03  CLAIM007-T321.
      *        05  FILLER               PIC  X(2)   VALUE   "32" .
               05  FILLER               PIC  X(3)   VALUE   "321" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ãí¼Í¡ÊÀÅÌ®Æâ¡Ë" .
             03  CLAIM007-T331.
      *        05  FILLER               PIC  X(2)   VALUE   "33" .
               05  FILLER               PIC  X(3)   VALUE   "331" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ãí¼Í¡Ê¤½¤ÎÂ¾¡Ë" .
             03  CLAIM007-T400.
      *        05  FILLER               PIC  X(2)   VALUE   "40" .
               05  FILLER               PIC  X(3)   VALUE   "400" .
               05  FILLER               PIC  X(40)  
                   VALUE   "½èÃÖ" .
             03  CLAIM007-T500.
      *        05  FILLER               PIC  X(2)   VALUE   "50" .
               05  FILLER               PIC  X(3)   VALUE   "500" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼ê½Ñ¡Ê¼ê½Ñ¡Ë" .
             03  CLAIM007-T502.
      *        05  FILLER               PIC  X(2)   VALUE   "50" .
               05  FILLER               PIC  X(3)   VALUE   "502" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼ê½Ñ¡ÊÍ¢·ì¡Ë" .
             03  CLAIM007-T503.
      *        05  FILLER               PIC  X(2)   VALUE   "50" .
               05  FILLER               PIC  X(3)   VALUE   "503" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼ê½Ñ¡Ê¥®¥×¥¹¡Ë" .
             03  CLAIM007-T540.
      *        05  FILLER               PIC  X(2)   VALUE   "54" .
               05  FILLER               PIC  X(3)   VALUE   "540" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Ëã¿ì" .
             03  CLAIM007-T600.
      *        05  FILLER               PIC  X(2)   VALUE   "60" .
               05  FILLER               PIC  X(3)   VALUE   "600" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¸¡ºº" .
             03  CLAIM007-T640.
      *        05  FILLER               PIC  X(2)   VALUE   "64" .
               05  FILLER               PIC  X(3)   VALUE   "640" .
               05  FILLER               PIC  X(40)  
                   VALUE   "ÉÂÍý¿ÇÃÇ" .
             03  CLAIM007-T700.
      *        05  FILLER               PIC  X(2)   VALUE   "70" .
               05  FILLER               PIC  X(3)   VALUE   "700" .
               05  FILLER               PIC  X(40)  
                   VALUE   "²èÁü¿ÇÃÇ" .
             03  CLAIM007-T800.
      *        05  FILLER               PIC  X(2)   VALUE   "80" .
               05  FILLER               PIC  X(3)   VALUE   "800" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¤½¤ÎÂ¾" .
             03  CLAIM007-T903.
      *        05  FILLER               PIC  X(2)   VALUE   "90" .
               05  FILLER               PIC  X(3)   VALUE   "903" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡ÊÆþ±¡ÎÁ¡Ë" .
             03  CLAIM007-T906.
      *        05  FILLER               PIC  X(2)   VALUE   "90" .
               05  FILLER               PIC  X(3)   VALUE   "906" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡Ê³°Çñ¡Ë" .
             03  CLAIM007-T910.
      *        05  FILLER               PIC  X(2)   VALUE   "91" .
               05  FILLER               PIC  X(3)   VALUE   "910" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡ÊÆþ±¡»þ°å³Ø´ÉÍý²¼¡Ë" .
             03  CLAIM007-T920.
      *        05  FILLER               PIC  X(2)   VALUE   "92" .
               05  FILLER               PIC  X(3)   VALUE   "920" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡ÊÆÃÄêÆþ±¡ÎÁ¡¦¤½¤ÎÂ¾¡Ë" .
             03  CLAIM007-T970.
      *        05  FILLER               PIC  X(2)   VALUE   "97" .
               05  FILLER               PIC  X(3)   VALUE   "970" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡Ê¿©»öÎÅÍÜ¡Ë" .
             03  CLAIM007-T971.
      *        05  FILLER               PIC  X(2)   VALUE   "97" .
               05  FILLER               PIC  X(3)   VALUE   "971" .
               05  FILLER               PIC  X(40)  
                   VALUE   "Æþ±¡¡ÊÉ¸½àÉéÃ´³Û¡Ë" .
             03  CLAIM007-T014.
      *        05  FILLER               PIC  X(2)   VALUE   "95" .
               05  FILLER               PIC  X(3)   VALUE   "014" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«Èñ¡Ê¾ÃÈñÀÇ¤Ê¤·¡Ë" .
             03  CLAIM007-T016.
      *        05  FILLER               PIC  X(2)   VALUE   "96" .
               05  FILLER               PIC  X(3)   VALUE   "016" .
               05  FILLER               PIC  X(40)  
                   VALUE   "¼«Èñ¡Ê¾ÃÈñÀÇ¤¢¤ê¡Ë" .
           02  CLAIM007-R  REDEFINES   CLAIM007-T.
             03  CLAIM007-TX           OCCURS  38.
      *        05  CLAIM007-ORCA       PIC  X(2).
               05  CLAIM007-CLAIM      PIC  X(3).
               05  CLAIM007-NAME       PIC  X(40).
       01  CLAIM007-MAX                PIC  9(03)  VALUE   38.
      *
