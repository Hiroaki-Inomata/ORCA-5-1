       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSDPCNYUIN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡£Ä£Ð£Ã¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/02/09    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *              NACL-¾®Æ¦Âô  xx/xx/xx  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *              NACL-¾®Æ¦Âô  08/06/17  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡ÍâÆü¤Î
      *                                     ¿©»ö¤ÈÆþ±¡ÎÁ²Ã»»ÂÐ±þ
      *              NACL-¾®Æ¦Âô  08/06/17  É¸·ç¤ÈÄê¿ôÄ¶²á¤Î³°Çñ½¤Àµ
      *              NACL-¾®Æ¦Âô  08/06/17  Âà±¡²Ã»»¤Î¥ì¥»ÅÅÂÐ±þ¡ÊÆÉ¤ßÈô¤Ð¤·¡Ë
      *                                     ¡¦Âà±¡²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
      *                                     ¡¦Âà±¡²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
      *                                     ¡¦Áá´üÂà±¡²Ã»»
      *
      *              NACL-¾®Æ¦Âô  08/06/17  Í­¾²¿ÇÎÅ½ê¤Î²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                                     ¡¦°å»ÕÇÛÃÖÅ¬¹ç²Ã»»
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£²
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *
      *              NACL-¾®Æ¦Âô  08/06/17  ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ¤È²óÉü´ü¥ê¥Ï¥Ó¥ê¤Î
      *                                     ²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
      *                                     ¡¦¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë
      *                                     ¡¦½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
      *
      *              NACL-¾®Æ¦Âô  08/06/17  ÆÃ¼ì¼À´µÉÂÅïÆþ±¡ÎÁ¤Î²Ã»»¤òÃí²Ã»»
      *                                     ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë
      *
      *              NACL-¾®Æ¦Âô  08/06/17  ÆÃÄêµ¡Ç½ÉÂ±¡¡ÝÀº¿ÀÉÂÅï¤Î²Ã»»¤òÃí²Ã»»
      *                                     ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
      *
      *              NACL-¾®Æ¦Âô  08/06/17  Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ µÚ¤Ó
      *                                     Àº¿À²ÊµßµÞÆþ±¡ÎÁ¤Î²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦ÈóÄê·¿¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
      *
      *              NACL-¾®Æ¦Âô  08/06/17  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ µÚ¤Ó
      *                                     Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¤Î²Ã»»¤ò
      *                                     Ãí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
      *                                     ¡¦êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
      *
      *              NACL-¾®Æ¦Âô  14/04/15  Ê¿À®£²£¶Ç¯ÅÙ²þÄêÂÐ±þ
      *              NACL-¾®Æ¦Âô  15/02/03  Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ¤ÇÆþ±¡ÎÁ¤¬
      *                                     Ì¤»»ÄêÆü¤ËÆþÎÏ¤µ¤ì¤¿¿ÇÎÅ¹Ô°Ù¤Ë¤Ä¤¤¤Æ
      *                                     ¡¦°å»Õ¥³¡¼¥É
      *                                     ¡¦ÉÂÅï¥³¡¼¥É
      *                                     ¡¦ÉÂÅï¶èÊ¬
      *                                     ¤¬µ­Ï¿¤Ç¤­¤Ê¤«¤Ã¤¿ÉÔ¶ñ¹ç¤ò½¤Àµ
      *              NACL-¾®Æ¦Âô  15/02/03  Ï«ºÒÊÝ¸±¤ÇÆþ±¡¤·¤¿´µ¼Ô¤¬°åÊÝ¤Ç
      *                                     ÆþÎÏ¤·¤¿¿ÇÎÅ¹Ô°Ù¤Ë¤Ä¤¤¤Æ
      *                                     ¡¦°å»Õ¥³¡¼¥É
      *                                     ¡¦ÉÂÅï¥³¡¼¥É
      *                                     ¡¦ÉÂÅï¶èÊ¬
      *                                     ¤¬µ­Ï¿¤Ç¤­¤Ê¤«¤Ã¤¿ÉÔ¶ñ¹ç¤ò½¤Àµ
      *              NACL-¾®Æ¦Âô  15/06/10  ÁªÄêÆþ±¡»þ¤Î³°Çñ¤â¥ì¥³¡¼¥Éµ­Ï¿
      *                                     ¤¹¤ëÂÐ±þ
      *              NACL-¾®Æ¦Âô  15/06/10  ÁªÄêÆþ±¡»þ¤Î³°Çñ¤â¥ì¥³¡¼¥Éµ­Ï¿
      *                                     ¤¹¤ëÂÐ±þ
      *              NACL-¾®Æ¦Âô  16/03/10  Ê¿À®£²£¸Ç¯ÅÙ²þÄêÂÐ±þ
      *              NACL-¾®Æ¦Âô  16/08/01  °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¥³¥á¥ó¥È¤Î
      *                                     µ­Ï¿ÂÐ±þ¡ÊÊ¿À®£²£¸Ç¯£±£°·î¿ÇÎÅÊ¬¤«¤é¡Ë
      *Ê¿À®£²£¹Ç¯£±£°·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  17/08/31  µï½»Èñ¶â³ÛÊÑ¹¹ÂÐ±þ
      *                                     ¶­³¦ÁØÂÐ±þ
      *                                     À¸³èÎÅÍÜÉéÃ´¥³¡¼¥ÉÊ¬³äÂÐ±þ
      *  05.00.01    NACL-¾®Æ¦Âô  17/12/07  Æ±ÆüºÆÆþ±¡¤Î¿©»öµ­Ï¿½¤Àµ
      *Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/02/27  Æþ±¡»þ¿©»öÎÅÍÜ¤Î¶â³ÛÊÑ¹¹ÂÐ±þ
      *                                     ¿©»öÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *                                     À¸³èÎÅÍÜ¿©»öÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *                                     µï½»ÈñÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/3   Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»ÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/11  Â¾°å¼õ¿ÇÂÐ±þ
      *ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  20/03/13  Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÊÔ½¸
      *
      *  05.00.00    NACL-¾®Æ¦Âô  20/03/18  ¡ó¸º»»·×»»»þ¤ÎÃ¼¿ô½èÍýÊÑ¹¹
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-ROUSAI                  PIC 9(01).
           03  FLG-RYAKU                   PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-KSN                     PIC 9(01).
           03  FLG-SENTEI-SET              PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-TSYRRK                  PIC 9(01).
           03  FLG-TNKRRK                  PIC 9(01).
           03  FLG-TENSUWRK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HASUU                   PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  KSN-IDX                     PIC 9(04).
           03  IDV                         PIC 9(04).
           03  TSY90-IDX                   PIC 9(02).
           03  IDX-TSY90                   PIC 9(04).
           03  IDX-NYUKSN                  PIC 9(02).
           03  IDX-CHUKSN1                 PIC 9(02).
           03  IDX-CHUKSN1S                PIC 9(02).
           03  IDX-CHUKSN1SS               PIC 9(02).
           03  IDX-CHUKSN2                 PIC 9(02).
           03  IDX-CHUKSN2S                PIC 9(02).
           03  IDX-CHUKSN2SS               PIC 9(02).
           03  IDX-COM                     PIC 9(02).
      *    Æ±ÆüºÆÆþ±¡¤Î¿©»öÊÔ½¸»þ¥¤¥ó¥Ç¥Ã¥¯¥¹ÂàÈòÎÎ°è
           03  SET-IDX1                    PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    É¸·ç¡Ê£±£°£°Ê¬¤Î£±£°¸º³Û¡Ë
           03  CONS-HYOKETU1               PIC X(09) VALUE "190107890".
      *    É¸·ç¡Ê£±£°£°Ê¬¤Î£±£µ¸º³Û¡Ë
           03  CONS-HYOKETU2               PIC X(09) VALUE "190107990".
      *    É¸·ç¡Ê£±£°£°Ê¬¤Î£³¸º³Û¡Ë¡ÊÎ¥ÅçÅù¡Ë
           03  CONS-HYOKETU3               PIC X(09) VALUE "190108590".
      *    É¸·ç¡Ê£±£°£°Ê¬¤Î£²¸º³Û¡Ë¡ÊÎ¥ÅçÅù¡Ë
           03  CONS-HYOKETU4               PIC X(09) VALUE "190108690".
      *    Äê¿ôÄ¶²áÆþ±¡´ðËÜÎÁ¸º³Û¡Ê£±£°£°Ê¬¤Î£±£°¡Ë
           03  CONS-TEISU1                 PIC X(09) VALUE "190107690".
      *    Äê¿ôÄ¶²áÆþ±¡´ðËÜÎÁ¸º³Û¡Ê£±£°£°Ê¬¤Î£²£°¡Ë
           03  CONS-TEISU2                 PIC X(09) VALUE "190107790".
      *    Äê¿ôÄ¶²áÆþ±¡´ðËÜÎÁ¸º³Û¡Ê£±£°£°Ê¬¤Î£±£°¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           03  CONS-TEISU3                 PIC X(09) VALUE "190122990".
      *    Äê¿ôÄ¶²áÆþ±¡´ðËÜÎÁ¸º³Û¡Ê£±£°£°Ê¬¤Î£²£°¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           03  CONS-TEISU4                 PIC X(09) VALUE "190123090".
      *
      *    Ê¿À®£²£¸Ç¯ÅÙ¿©»öÎÅÍÜ
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©°Ê³°¡Ë
           03  CONS-SKJ-RYOYO-1-1          PIC X(09) VALUE "197000110".
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©°Ê³°¡Ë
           03  CONS-SKJ-RYOYO-2-1          PIC X(09) VALUE "197000710".
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©¡Ë
           03  CONS-SKJ-RYOYO-1-2          PIC X(09) VALUE "197003110".
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©¡Ë
           03  CONS-SKJ-RYOYO-2-2          PIC X(09) VALUE "197003210".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©°Ê³°¡Ë
           03  CONS-SKJSEI-RYOYO-1-1       PIC X(09) VALUE "197001310".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©¡Ë
           03  CONS-SKJSEI-RYOYO-1-2       PIC X(09) VALUE "197003510".
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
           03  CONS-SKJSEI-RYOYO-2         PIC X(09) VALUE "197001710".
      *
      *    Ê¿À®£²£¸Ç¯ÅÙ¿©»öÉéÃ´
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
           03  CONS-NANBYO-SKJFTN          PIC X(09) VALUE "197003310".
      *    ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
           03  CONS-SEISIN-SKJFTN          PIC X(09) VALUE "197003410".
      *
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  CONS-ETCNYUIN-TBL.
             05  CONS-ETCNYUIN-KSN         PIC X(09)  OCCURS 10.
      *    ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß¥ï¡¼¥¯ÍÑ
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
      *    °åÎÅµ¡´Ø¼ïÊÌ
           03  WRK-HOSPSBT                 PIC 9(01).
      *
           03  WRK-MM-X                    PIC X(02).
           03  WRK-MM-9     REDEFINES   WRK-MM-X
                                           PIC 9(02).
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
           03  WRK-NYUKHNKBN-XXX           PIC 9(03).
      *
      *    Æ±ÆüºÆÆþ±¡½èÍýÍÑ
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY   PIC 9(04).
               05  WRK-GETSUMATSU-MM   PIC 9(02).
               05  WRK-GETSUMATSU-DD   PIC 9(02).
           03  WRK-DOUJITSU-KBN        PIC X(01).
      *
      *    Æþ±¡²ñ·×¥ï¡¼¥¯
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SRYKBN          PIC X(02).
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOUKETSU       PIC X(09).
                   07  WRK-TEISU           PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-GAIHAKU2        PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
                   07  WRK-GAIHAKU-KBN     PIC 9(02).
                   07  WRK-CHUKSNCD        PIC X(04).
                   07  WRK-NYUKHNKBN       PIC 9(03).
      *
      *     °åÎÅ´Ñ»¡ÍÑÂàÈòÎÎ°è
           03  WRK-IRYOKNS-TABLE.
               05  WRK-IRYOKNS-TBL         OCCURS  31.
                   06  WRK-IRYOKNS-SRYCD-TBL.
                     07  WRK-IRYOKNS-SRYCD1        PIC X(09).
                     07  WRK-IRYOKNS-CHUKSNTUBAN1  PIC X(01).
                     07  WRK-IRYOKNS-SRYCD2        PIC X(09).
                     07  WRK-IRYOKNS-CHUKSNTUBAN2  PIC X(01).
                     07  WRK-IRYOKNS-SRYCD3        PIC X(09).
                     07  WRK-IRYOKNS-CHUKSNTUBAN3  PIC X(01).
                     07  WRK-IRYOKNS-SRYCD4        PIC X(09).
                     07  WRK-IRYOKNS-CHUKSNTUBAN4  PIC X(01).
      *              Ê¿À®£²£¸Ç¯ÅÙ²þÄê¤Ç¿·Àß¤µ¤ì¤¿Å¾±¡Ä´À°²Ã»»¤Ï
      *              ¿ÇÎÅ¹Ô°Ù²èÌÌ¤è¤êÆþÎÏ¤¹¤ë°Ù¡¢SRYCD5¤Ë³ÊÇ¼
                     07  WRK-IRYOKNS-SRYCD5        PIC X(09).
                     07  WRK-IRYOKNS-CHUKSNTUBAN5  PIC X(01).
                   06   WRK-IRYOKNS-SRYCD-TBLR   REDEFINES
                                             WRK-IRYOKNS-SRYCD-TBL.
                     07  WRK-IRYOKNS-SRYCD-OCC     OCCURS  5.
                       08  WRK-IRYOKNS-SRYCD       PIC X(09).
                       08  WRK-IRYOKNS-CHUKSNTUBAN PIC X(01).
                   06  WRK-IRYOKNS-SORT-FLG  PIC X(01).
           03   WRK-IRYOKNS-TAIHI                  PIC X(10).
      *
      *    Æþ±¡²ñ·×ÊÂ¤ÓÂØ¤¨¥ï¡¼¥¯
           03  WRK-NYUIN-SORT-TABLE.
               05  WRK-NYUIN-SORT-TBL      OCCURS  31.
                   07  WRK-SO-MEISAISU     PIC 9(02).
                   07  WRK-SO-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SO-SRYKBN       PIC X(02).
                   07  WRK-SO-SRYCD        PIC X(09)  OCCURS 51.
                   07  WRK-SO-HASUU        PIC X(01)  OCCURS 51.
                   07  WRK-SO-SRYCDTOTAL   PIC 9(14).
                   07  WRK-SO-TEN          PIC 9(07)V9(5).
                   07  WRK-SO-KAISU        PIC 9(02).
                   07  WRK-SO-ADL-TEN-MOJI PIC X(04).
                   07  WRK-SO-GAIHAKU-TBL.
                     09  WRK-SO-GAIHAKU-KBN  PIC 9(02)  OCCURS 31.
                   07  WRK-SO-IRYOADL-DAY  PIC 9(02).
      *
      *    Ã¼¿ôÀÚ¼Î¤ÆÍÑ
           03  WRK-SO-TEN-SEISU            PIC 9(07).
      *
      *-------------------------------------------------------------
      *  Ê¿À®£²£´Ç¯£´·îÆþ±¡Ê¬¤«¤é¤ÎÃí²Ã»»ÂàÈòÎÎ°è
      *
      *    Æþ±¡ÎÁ¡Ê£Á£±£°£°ÈÖÂæ¡Ë¡¢ÆÃÄêÆþ±¡ÎÁ¡Ê£Á£³£°£°ÈÖÂæ¡Ë¤ÎÃí²Ã»»ÂàÈòÍÑ
           03  WRK-CHUKSN-AREA-1.
             05   WRK-CHUKSN-TBL-1      OCCURS  50.
               07  WRK-CHUKSN-SRYCD-1          PIC X(09).
               07  WRK-CHUKSN-CHUCD-1.
                 09  WRK-CHUKSN-CHUKSNCD-1     PIC X(04).
                 09  WRK-CHUKSN-CHUKSNTUBAN-1  PIC X(01).
               07  WRK-CHUKSN-ZAITEN-1         PIC 9(08).
               07  WRK-CHUKSN-DAY-1            PIC 9(03)  OCCURS 31.
      *
      *    Æþ±¡´ðËÜÎÁÅù²Ã»»¡Ê£Á£²£°£°ÈÖÂæ¡Ë¤ÎÃí²Ã»»ÂàÈòÍÑ
           03  WRK-CHUKSN-AREA-2.
             05   WRK-CHUKSN-TBL-2      OCCURS  50.
               07  WRK-CHUKSN-SRYCD-2          PIC X(09).
               07  WRK-CHUKSN-CHUCD-2.
                 09  WRK-CHUKSN-CHUKSNCD-2     PIC X(04).
                 09  WRK-CHUKSN-CHUKSNTUBAN-2  PIC X(01).
               07  WRK-CHUKSN-ZAITEN-2         PIC 9(08).
               07  WRK-CHUKSN-DAY-2            PIC 9(03)  OCCURS 31.
      *
      *    Ãí²Ã»»°Ê³°¤Î²Ã»»ÂàÈòÍÑ
           03  WRK-NYUKSN-AREA-1.
             05   WRK-NYUKSN-TBL-1             OCCURS  50.
               07  WRK-NYUKSN-SRYCD-1          PIC X(09).
               07  WRK-NYUKSN-ZAITEN-1         PIC 9(08).
               07  WRK-NYUKSN-DAY-1            PIC 9(03)  OCCURS 31.
      *
      *    ÊÂ¤ÓÂØ¤¨»þ¤ÎÂàÈòÍÑ
           03  WRK-CHUKSN-TAIHI.
             05  WRK-CHUKSN-SRYCD-T          PIC X(09).
             05  WRK-CHUKSN-CHUCD-T.
               07  WRK-CHUKSN-CHUKSNCD-T     PIC X(04).
               07  WRK-CHUKSN-CHUKSNTUBAN-T  PIC X(01).
             05  WRK-CHUKSN-ZAITEN-T         PIC 9(08).
             05  WRK-CHUKSN-DAY-T            PIC 9(03)  OCCURS 31.
      *-------------------------------------------------------------
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×½¸·×ÍÑ
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¿©»öÂàÈòÎÎ°è
           03  WRK-SYS5000-SKJ             OCCURS  5.
               05  WRK-SYS5000-SHOKUCD     PIC X(09) OCCURS 31.
      *    ¿©»ö¥ï¡¼¥¯
           03  WRK-SKJ-HKNCOMBI-ASA        PIC 9(03) OCCURS 31.
           03  WRK-SKJ-HKNCOMBI-HIRU       PIC 9(03) OCCURS 31.
           03  WRK-SKJ-HKNCOMBI-YORU       PIC 9(03) OCCURS 31.
      *    ¿©Æ²²Ã»»È½ÄêÍÑ¿©»ö¥ï¡¼¥¯
           03  WRK-SKJ-ASA-ARI             PIC 9(03) OCCURS 31.
           03  WRK-SKJ-HIRU-ARI            PIC 9(03) OCCURS 31.
      *
      *    Â¾ÊÝ¸±¤Ç¤Î¿©Æ²²Ã»»È½ÄêÍÑÎÎ°è
           03  WRK-TAHOKEN-SHOKUDOU-TABLE.
               05  WRK-TAHOKEN-SHOKUDOU-FLG    PIC  X(01)  OCCURS 31.
      *
      *    ¿©»ö¥ï¡¼¥¯£´¡Ê¿©»öÎÅÍÜÈñ¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ4-TABLE.
               05  WRK-SKJ4-TBL          OCCURS  93.
                   07  WRK-SKJ4-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ4-RYOYO          PIC X(09).
                   07  WRK-SKJ4-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£µ¡ÊÆÃÊÌ¿©²Ã»»¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ5-TABLE.
               05  WRK-SKJ5-TBL          OCCURS  93.
                   07  WRK-SKJ5-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ5-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ5-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£¶¡Ê¿©Æ²²Ã»»¡Ë¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJ6-TABLE.
               05  WRK-SKJ6-TBL          OCCURS  31.
                   07  WRK-SKJ6-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ6-SHOKUDOU       PIC X(09).
                   07  WRK-SKJ6-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£·
           03  WRK-SKJ7-TABLE.
               05  WRK-SKJ7-TBL           OCCURS  62.
                   07  WRK-SKJ7-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ7-GRP.
                       09  WRK-SKJ7-RYOYO       PIC X(09).
                       09  WRK-SKJ7-RYOYO-SU    PIC 9(01).
                       09  WRK-SKJ7-RYOYO-RYU       PIC X(09).
                       09  WRK-SKJ7-RYOYO-RYU-SU    PIC 9(01).
                       09  WRK-SKJ7-SHOKUDOU    PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU  PIC X(09).
                       09  WRK-SKJ7-TOKU-SHOKU-SU PIC 9(01).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU1 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU2 PIC 9(02).
                       09  WRK-SKJ7-TOKU-SHOKU-KAISU3 PIC 9(02).
                       09  WRK-SKJ7-SKJFTN      PIC X(09).
                       09  WRK-SKJ7-SKJFTN-SU   PIC 9(01).
                   07  WRK-SKJ7-SRYCDTOTAL      PIC 9(14).
                   07  WRK-SKJ7-KAISU           PIC 9(02).
                   07  WRK-SKJ7-SAINYUINKBN     PIC 9(01).
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¥ï¡¼¥¯
           03  WRK-KANKYO-TABLE.
               05  WRK-KANKYO-TBL           OCCURS  31.
                   07  WRK-KANKYO-HKNCOMBI-DAY  PIC 9(03).
                   07  WRK-KANKYO-GRP.
                       09  WRK-KANKYO-RYOYO     PIC X(09).
                       09  WRK-KANKYO-FTN       PIC X(09).
                   07  WRK-KANKYO-SRYCDTOTAL    PIC 9(14).
                   07  WRK-KANKYO-KAISU         PIC 9(02).
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           03  WRK-TENSU-OCC                   OCCURS  200.
               05  WRK-TENSU-SRYCD             PIC X(09).
               05  WRK-TENSU-DATA              PIC X(2000).
           03  WRK-TENSU-CD                    PIC X(09).
           03  TTNS-IDX                        PIC 9(03).
           03  TTNS-MAX                        PIC 9(03).
      *
      *    ¿©»öÉéÃ´¥ï¡¼¥¯¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJFTN2-TABLE.
               05  WRK-SKJFTN2-TBL       OCCURS  93.
                   07  WRK-SKJFTN2-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SKJFTN2-SRYCD        PIC X(09).
                   07  WRK-SKJFTN2-KAISU        PIC 9(02).
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ÎºÆÆþ±¡Ê¬¿©»ö»»Äê¤Ë¤Ä¤¤¤Æ¡¢ÆþÂà±¡Æü¤òÊÑ¹¹¤¹¤ë°Ù
      *    ¤Î¶èÊ¬
           03  WRK-SAINYUINKBN-ASA             PIC X(01)   OCCURS  31.
           03  WRK-SAINYUINKBN-HIRU            PIC X(01)   OCCURS  31.
           03  WRK-SAINYUINKBN-YORU            PIC X(01)   OCCURS  31.
      *
      *    Æþ±¡ÎÁ»»Äê¤ÎÈ½ÄêÍÑÎÎ°è
           03  WRK-NACCT-DAY-TBL.
               05  WRK-NACCT-DAY               PIC  9(03)  OCCURS 31.
      *
      *    ÁªÄêÎÅÍÜÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-SENTEI.
               05  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      *    ÉÂÅï¶èÊ¬ÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-BTUKBN.
               05  TBL-RRKNUM                  PIC 9(03)   OCCURS  31.
               05  TBL-BTUNUM                  PIC 9(02)   OCCURS  31.
               05  TBL-BTUKBN                  PIC X(02)   OCCURS  31.
               05  TBL-SRYKA                   PIC X(02)   OCCURS  31.
               05  TBL-DRCD                    PIC X(05)   OCCURS  31.
      *    ÉÂÅï¶èÊ¬ÂàÈòÍÑ¥Æ¡¼¥Ö¥ë¡ÊÆ±ÆüºÆÆþ±¡Ê¬¡Ë
           03  TABLE-BTUKBN2.
               05  TBL-BTUNUM2                 PIC 9(02)   OCCURS  31.
               05  TBL-BTUKBN2                 PIC X(02)   OCCURS  31.
               05  TBL-SRYKA2                  PIC X(02)   OCCURS  31.
               05  TBL-DRCD2                   PIC X(05)   OCCURS  31.
           03  WRK-SET-DD                      PIC 9(02).
      *
           03  WRK-DAY-X                       PIC X(02).
           03  WRK-DAY-9        REDEFINES      WRK-DAY-X
                                               PIC 9(02).
      *    ÁªÄêÆþ±¡»þ¤Î³°ÇñÈ½ÄêÍÑ
           03  WRK-SENTEI-GAIGAKU              PIC 9(01)   OCCURS  31.
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»ÎÎ°è
           03  WRK-KANGOGEN                    PIC X(09)   OCCURS  31.
      *
      *    ´µ¼Ô½êÆÀ¾ðÊó¤ÎÂàÈòÎÎ°è
       01  WRK-SYOTOKU-SKJKBN-AREA.
           03  WRK-SYOTOKU-SKJKBN       PIC X(01)   OCCURS  93.
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
      *
      *    Æþ±¡¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    Äã½êÆÀ
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    Ç¯¶â
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYULIFE.INC".
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö¡ÊÊ¿À®£²£¸Ç¯£´·î²þÄêÂÐ±þ¡Ë
           COPY    "CPORCSNYULIFE2.INC".
      *
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSSKJKEEP.INC".
      *
      *    ¿©»öÊÝ¸±¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö
           COPY    "CPORCSRYOCOM.INC".
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    Æþ±¡ÅÀ¿ô¥Þ¥¹¥¿ÉÕ²Ã¾ðÊó¥µ¥Ö
           COPY    "CPORCSNTENSU.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Æþ±¡£Ä£Ð£Ã¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSDPCNYUIN.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSDPCNYUIN-AREA
            SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-DPCNYUIN-HENKYAKU-AREA
           INITIALIZE                  CONS-ETCNYUIN-TBL
           MOVE    ZERO                TO  LNK-DPCNYUIN-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-DPCNYUIN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-DPCNYUIN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     DISPLAY "------------HENKYAKU--------------------------"
      *     DISPLAY "LNK-DPCNYUIN-PTID = " LNK-DPCNYUIN-PTID
      *     PERFORM   VARYING  IDX FROM 1 BY 1
      *               UNTIL IDX > 40
      *     DISPLAY "IDX = " IDX
      *     DISPLAY "LNK-DPCNYUIN-PTID = " LNK-DPCNYUIN-PTID
      *     DISPLAY "LNK-DPCNYUIN-SRYKA   = " LNK-DPCNYUIN-SRYKA(3 IDX)
      *     DISPLAY "LNK-DPCNYUIN-SRYKA2  = " LNK-DPCNYUIN-SRYKA2(3 IDX)
      *     DISPLAY "LNK-DPCNYUIN-RECSKB = " LNK-DPCNYUIN-RECSKB(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYSKB = " LNK-DPCNYUIN-SRYSKB(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYSKB2= " LNK-DPCNYUIN-SRYSKB(3 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SRYCD = " LNK-DPCNYUIN-SRYCD(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYCD2= " LNK-DPCNYUIN-SRYCD(3 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SURYO = " LNK-DPCNYUIN-SURYO(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-TEN   = " LNK-DPCNYUIN-TEN(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(1) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(3 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(2) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(3 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(3) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(3 IDX 3)
      *     DISPLAY "    --- SEIKATSU -----"
      *     DISPLAY "LNK-DPCNYUIN-PTID = " LNK-DPCNYUIN-PTID
      *     DISPLAY "LNK-DPCNYUIN-SRYKA   = " LNK-DPCNYUIN-SRYKA(4 IDX)
      *     DISPLAY "LNK-DPCNYUIN-SRYKA2  = " LNK-DPCNYUIN-SRYKA2(4 IDX)
      *     DISPLAY "LNK-DPCNYUIN-RECSKB = " LNK-DPCNYUIN-RECSKB(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYSKB = " LNK-DPCNYUIN-SRYSKB(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYSKB2= " LNK-DPCNYUIN-SRYSKB(4 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SRYCD = " LNK-DPCNYUIN-SRYCD(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SRYCD2= " LNK-DPCNYUIN-SRYCD(4 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SURYO = " LNK-DPCNYUIN-SURYO(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-TEN   = " LNK-DPCNYUIN-TEN(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(1) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(4 IDX 1)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(2) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(4 IDX 2)
      *     DISPLAY "LNK-DPCNYUIN-SAINYUINKBN(3) = "
      *              LNK-DPCNYUIN-SAINYUINKBN(4 IDX 3)
      *     END-PERFORM
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    »»ÄêÇ¯·î¥Á¥§¥Ã¥¯
           IF    ( LNK-DPCNYUIN-SANTEIYM    =   SPACE )
               MOVE    1                 TO  LNK-DPCNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                        PTINF-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF             NOT =   ZERO
               MOVE    2                 TO  LNK-DPCNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬¥Á¥§¥Ã¥¯
      **     IF     LNK-DPCNYUIN-HKNKBN  =   "1"  OR  "2"
      **         CONTINUE
      **     ELSE
      **         MOVE    3               TO  LNK-DPCNYUIN-RC
      **         GO  TO  100-PRM-CHK-EXT
      **     END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4                   TO  LNK-DPCNYUIN-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-DPCNYUIN-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        MOVE   NACCT-DAY (IDX1)
                                           TO  WRK-HKNCOMBI-DAY(IDX1)
                    END-PERFORM
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    °åÎÅµ¡´Ø¼ïÊÌ¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-DPCNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-DPCNYUIN-HOSPNUM TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =    ZERO
                   MOVE   SYS-1001-HOSPSBT  TO  WRK-HOSPSBT
               ELSE
                   MOVE   ZERO              TO  WRK-HOSPSBT
               END-IF
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤È£³¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤Ë¤Ä¤¤¤Æ¼èÆÀ
           INITIALIZE                             ORCSNYUINDAY-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM       TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID          TO  LNK-NYUINDAY-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM      TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
      *¿©»ö¤ÎÊÝ¸±ÁÈ¹ç¤»¼èÆÀ
           INITIALIZE                             ORCSSKJHKN-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM       TO  LNK-SKJHKN-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID          TO  LNK-SKJHKN-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM      TO  LNK-SKJHKN-SANTEIYM
           CALL    "ORCSSKJHKN"         USING     ORCSSKJHKN-AREA
                                                  ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
      *Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ
           IF      LNK-DPCNYUIN-SANTEIYM       >=  "201804"
             INITIALIZE                            ORCSKANGOGEN-AREA
             MOVE    LNK-DPCNYUIN-HOSPNUM      TO  LNK-KANGOGEN-HOSPNUM
             MOVE    LNK-DPCNYUIN-PTID         TO  LNK-KANGOGEN-PTID
             MOVE    LNK-DPCNYUIN-SANTEIYM     TO  LNK-KANGOGEN-SANTEIYM
             CALL    "ORCSKANGOGEN"     USING      ORCSKANGOGEN-AREA
                                                   SPA-AREA
             IF      LNK-KANGOGEN-RC           =   ZERO
               PERFORM    VARYING   IDX1  FROM   1   BY   1
                           UNTIL    IDX1   >    31
                 MOVE  LNK-KANGOGEN-SRYCD(IDX1) TO  WRK-KANGOGEN(IDX1)
               END-PERFORM
             END-IF
           END-IF
      *
      *°åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¥³¥á¥ó¥È¤Î¼èÆÀ¡ÊÊ¿À®£²£¸Ç¯£±£°·î¿ÇÎÅÊ¬°Ê¹ß¡Ë
           IF     LNK-DPCNYUIN-SANTEIYM       >=  "201610"
              INITIALIZE                          ORCSRYOCOM-AREA
              MOVE    LNK-DPCNYUIN-HOSPNUM    TO  LNK-RYOCOM-HOSPNUM
              MOVE    LNK-DPCNYUIN-PTID       TO  LNK-RYOCOM-PTID
              MOVE    LNK-DPCNYUIN-SANTEIYM   TO  LNK-RYOCOM-SANTEIYM
              CALL    "ORCSRYOCOM"        USING   ORCSRYOCOM-AREA
                                                  SPA-AREA
           END-IF
      *
      *¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-DPCNYUIN-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-DPCNYUIN-PTID     TO  COMB-PTID
                   MOVE    WRK-HKNCOMBI-DAY(IDX)   TO  COMB-HKNCOMBINUM
                   MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-SELECT-SEC
                   IF      FLG-HKNCOMBI            =   ZERO
                       MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM   910-HKNCOMBI-READ-SEC
                       IF   FLG-HKNCOMBI         =  ZERO
                            IF    ( COMB-HKNNUM   = "971" OR "973" OR
                                                    "975")
                                            OR
                                  ( COMB-HKNNUM(1:2)  =    "98")
                                MOVE   ZERO   TO  WRK-HKNCOMBI-DAY(IDX)
                            ELSE
      *
                                CONTINUE
      *
                            END-IF
                       ELSE
                           MOVE    5          TO  LNK-DPCNYUIN-RC
                       END-IF
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                   ELSE
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"          TO  MCP-TABLE
                       MOVE    "key"                   TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                       MOVE    5              TO  LNK-DPCNYUIN-RC
                   END-IF
               END-IF
           END-PERFORM
      *
      *----------TEST----------------------
      ***      MOVE    2    TO  LNK-DPCNYUIN-SET-HKNCOMBI(2)
      *
      *¿©»ö»»ÄêÍÑ¤ÎÊÝ¸±ÁÈ¹ç¤»È½ÄêÎÎ°è¤Ë¥»¥Ã¥È
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   IF (WRK-HKNCOMBI-DAY(IDX) =
                                 LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(10))
                       MOVE   WRK-HKNCOMBI-DAY(IDX)
                                   TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                        WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                        WRK-SKJ-HKNCOMBI-YORU(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *Æ±ÆüºÆÆþ±¡Æü¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     LNK-NYUINDAY-DOUJITSU-KBN(IDX)   =   "1"
      *            Âà±¡Ê¬
                   IF (LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX) =
                                 LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(10))
                       IF     LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX) = "0"
                           MOVE   ZERO  TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                       IF     LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX) = "1"
                           MOVE   LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                           MOVE   ZERO  TO   WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                       IF     LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX) = "2"
                           MOVE   LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX)
                           MOVE   ZERO  TO   WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                       IF     LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX) = "3"
                           MOVE   LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                   END-IF
      *            ºÆÆþ±¡Ê¬
                   IF (LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX) =
                                 LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(10))
                       IF     LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX) = "1"
                           MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                       IF     LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX) = "2"
                           MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                       IF     LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX) = "3"
                           MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
                                        TO   WRK-SKJ-HKNCOMBI-YORU(IDX)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *·îËö¤ËÆ±ÆüºÆÆþ±¡»þ¤Î¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
      *    ·îËöÆü¤ò¼èÆÀ
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-DPCNYUIN-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DOUJITSU-KBN
           IF (LNK-NYUINDAY-DOUJITSU-KBN(WRK-GETSUMATSU-DD) = "1") AND
              (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(WRK-GETSUMATSU-DD) = "3")
              IF   LNK-DPCNYUIN-HKNKBN  = LNK-NYUINDAY-DOUJITSU-HKNSYU
                                            (WRK-GETSUMATSU-DD)
                MOVE      "1"              TO  WRK-DOUJITSU-KBN
              END-IF
           END-IF
      *
      *¿©»öÊÝ¸±»ØÄê¤Çµ­Ï¿ÂÐ¾Ý¤È¤Ê¤ëÆþÎÏ¤¬¤¢¤ë¤«³ÎÇ§
           PERFORM    VARYING  IDX1   FROM  1  BY  1
             UNTIL    IDX1     >   31
                IF LNK-DPCNYUIN-HKNKBN  =  LNK-SKJHKN-HKNSYU(IDX1 1)
                    MOVE      "1"              TO  WRK-DOUJITSU-KBN
                END-IF
                IF LNK-DPCNYUIN-HKNKBN  =  LNK-SKJHKN-HKNSYU(IDX1 2)
                    MOVE      "1"              TO  WRK-DOUJITSU-KBN
                END-IF
           END-PERFORM
      *
      *¿©»öÊÝ¸±»ØÄê¤Çµ­Ï¿ÂÐ¾Ý¤È¤Ê¤ëÆþÎÏ¤¬¤¢¤ì¤ÐÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤ò¥»¥Ã¥È
           PERFORM    VARYING  IDX1   FROM  1  BY  1
             UNTIL    IDX1     >   31
      **          DISPLAY "IDX1 = " IDX1 
      **                  " T-SKJKBN = " 
      **                  LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)
      **                  " SAI-SKJKBN = " 
      **                  LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)
      *         Æþ±¡Ê¬¡ÊÂà±¡Ê¬¡Ë
                IF  LNK-SKJHKN-HKNCOMBINUM(IDX1 1) NOT = SPACE
                    IF   LNK-SKJHKN-HKNCOMBINUM(IDX1 1)  =
                                 LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(10)
      *
      *                 Æ±ÆüºÆÆþ±¡Æü°Ê³°
                        IF   LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = SPACE
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 1)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Ä«¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "1"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 1)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                        END-IF
      *                 Ãë¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "2"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 1)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                        END-IF
      *                 Í¼¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "3"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 1)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
                    ELSE
      *                 ¥¯¥ê¥¢½èÍý
      *                 Æ±ÆüºÆÆþ±¡Æü°Ê³°
                        IF   LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = SPACE
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Ä«¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "1"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                        END-IF
      *                 Ãë¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "2"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                        END-IF
      *                 Í¼¿©¤Þ¤Ç
                        IF   LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1)  = "3"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
                    END-IF
                END-IF
      *         Æ±ÆüºÆÆþ±¡Ê¬
                IF  LNK-SKJHKN-HKNCOMBINUM(IDX1 2) NOT = SPACE
                    IF   LNK-SKJHKN-HKNCOMBINUM(IDX1 2)  =
                                 LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                 LNK-DPCNYUIN-SET-HKNCOMBI(10)
      *
      *                 Ä«¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "1"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 2)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Ãë¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "2"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 2)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Í¼¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "3"
                            MOVE  LNK-SKJHKN-HKNCOMBINUM(IDX1 2)(2:3)
                                         TO  WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
                    ELSE
      *                 ¥¯¥ê¥¢½èÍý
      *                 Ä«¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "1"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                             WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Ãë¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "2"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                             WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
      *                 Í¼¿©¤«¤é
                        IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)  = "3"
                            MOVE  ZERO
                                         TO  WRK-SKJ-HKNCOMBI-YORU(IDX1)
                        END-IF
                    END-IF
                END-IF
           END-PERFORM
      **     PERFORM    VARYING  IDX1   FROM  1  BY  1
      **       UNTIL    IDX1     >   31
      **         DISPLAY "IDX1 = " IDX1
      **         DISPLAY "WRK-SKJ-HKNCOMBI-ASA(IDX1)  = "
      **                  WRK-SKJ-HKNCOMBI-ASA(IDX1)
      **         DISPLAY "WRK-SKJ-HKNCOMBI-HIRU(IDX1) = "
      **                  WRK-SKJ-HKNCOMBI-HIRU(IDX1)
      **         DISPLAY "WRK-SKJ-HKNCOMBI-YORU(IDX1) = "
      **                  WRK-SKJ-HKNCOMBI-YORU(IDX1)
      **     END-PERFORM
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           MOVE    SPACE                   TO  WRK-NACCT-DAY-TBL
           INITIALIZE                          WRK-NACCT-DAY-TBL
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   MOVE      ZERO     TO      WRK-NYUKHNKBN-XXX
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF     NACCT-DAY(IDX1)  NOT =  ZERO
                             MOVE   NACCT-DAY(IDX1)  TO   WRK-NACCT-DAY
                                                                (IDX1)
                           END-IF
                           IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
      *                      °åÎÅ´Ñ»¡
                             IF  NSRY-SRYCD(2)(1:3) =  "188"
                                  MOVE  NSRY-SRYCD(2)   TO
                                              WRK-IRYOKNS-SRYCD1(IDX1)
                             END-IF
                             IF  NSRY-SRYCD(3)(1:3) =  "188"
                                MOVE  NSRY-SRYCD(3)   TO
                                              WRK-IRYOKNS-SRYCD2(IDX1)
                             END-IF
                             IF  NSRY-SRYCD(4)(1:3) =  "188"
                                MOVE  NSRY-SRYCD(4)   TO
                                              WRK-IRYOKNS-SRYCD3(IDX1)
                             END-IF
                             IF  NSRY-SRYCD(5)(1:3) =  "188"
                                MOVE  NSRY-SRYCD(5)   TO
                                              WRK-IRYOKNS-SRYCD4(IDX1)
                             END-IF
      *
      *                      Æþ±¡¤Î´ü´Ö²Ã»»
                             IF  (NSRY-SRYCD(2) NOT =  SPACE)  AND
                                 (NSRY-SRYCD(2) NOT = CONS-HYOKETU1 AND
                                                      CONS-HYOKETU2 AND
                                                      CONS-HYOKETU3 AND
                                                      CONS-HYOKETU4 AND
                                                      CONS-TEISU1   AND
                                                      CONS-TEISU2   AND
                                                      CONS-TEISU3   AND
                                                      CONS-TEISU4)
                                                                AND
                                   (NSRY-SRYCD(2)(1:3) NOT = "188")
                                 MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-SHOKIKSN(IDX1)
                             END-IF
      *                      É¸·ç¥³¡¼¥É
                             IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(2)  =   CONS-HYOKETU1 OR
                                                     CONS-HYOKETU2 OR
                                                     CONS-HYOKETU3 OR
                                                     CONS-HYOKETU4)
                                 MOVE   NSRY-SRYCD(2)  TO
                                                 WRK-HYOUKETSU(IDX1)
                             END-IF
      *                      É¸·ç¥³¡¼¥É
                             IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(3)  =   CONS-HYOKETU1 OR
                                                     CONS-HYOKETU2 OR
                                                     CONS-HYOKETU3 OR
                                                     CONS-HYOKETU4)
                                 MOVE   NSRY-SRYCD(3)  TO
                                                   WRK-HYOUKETSU(IDX1)
                             END-IF
      *                      Äê¿ôÄ¶²á¥³¡¼¥É
                             IF  (NSRY-SRYCD(2)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(2)  =   CONS-TEISU1 OR
                                                     CONS-TEISU2 OR
                                                     CONS-TEISU3 OR
                                                     CONS-TEISU4)
                                 MOVE   NSRY-SRYCD(2)  TO
                                                 WRK-TEISU(IDX1)
                             END-IF
      *                      Äê¿ôÄ¶²á¥³¡¼¥É
                             IF  (NSRY-SRYCD(3)  NOT =   SPACE)  AND
                                 (NSRY-SRYCD(3)  =   CONS-TEISU1 OR
                                                     CONS-TEISU2 OR
                                                     CONS-TEISU3 OR
                                                     CONS-TEISU4)
                                 MOVE   NSRY-SRYCD(3)  TO
                                                 WRK-TEISU(IDX1)
                             END-IF
      *                      ¿ÇÎÅ¶èÊ¬
                             MOVE    NACCT-SRYKBN
                                              TO  WRK-SRYKBN(IDX1)
                             IF   FLG-RYAKU   =   ZERO
      *
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                             END-IF
      *
                             MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
                             MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                             MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                             MOVE    1    TO   FLG-OK
                           END-IF
                           IF    LNK-NYUINDAY-KBN(IDX1)  =  "1"
                               MOVE    1    TO   FLG-OK
                           END-IF                           
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF   FLG-OK     =   ZERO
                   MOVE     3      TO      LNK-DPCNYUIN-RC
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *            ÉÂÅïÈÖ¹æ¡ÊÉÂÅï¶èÊ¬¡Ë¤ÎÊÔ½¸
                   PERFORM   2105-BTUNUM-HENSHU-SEC
                   GO   TO    200-MAIN-EXT
               END-IF
           ELSE
               MOVE     3      TO      LNK-DPCNYUIN-RC
      *        Æþ±¡²ñ·×¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *        ÉÂÅïÈÖ¹æ¡ÊÉÂÅï¶èÊ¬¡Ë¤ÎÊÔ½¸
               PERFORM   2105-BTUNUM-HENSHU-SEC
               GO   TO    200-MAIN-EXT
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÎÁ¤Î»»Äê¤¬Ìµ¤¤Æü¤Ï¿©»öÊÝ¸±¤ò¥¯¥ê¥¢
           PERFORM    VARYING  IDX1   FROM  1  BY  1
             UNTIL    IDX1     >   31
              IF  (WRK-NYUINKHN(IDX1)    =    SPACE)    AND
                  (LNK-NYUINDAY-KBN(IDX1)  =  SPACE)    AND
                  (WRK-NACCT-DAY(IDX1)     =  ZERO)
                   MOVE  ZERO          TO  WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                           WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                           WRK-SKJ-HKNCOMBI-YORU(IDX1)
              END-IF
           END-PERFORM
      *
      *Æþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *ÁªÄêÎÅÍÜ¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-SENTEI-SET
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key22"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ÁªÄêÆþ±¡¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                           MOVE   NACCT-DAY(IDX1)
                                              TO  TBL-SENTEI (IDX1)
                           MOVE     1         TO  FLG-SENTEI-SET 
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key22"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key22"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           PERFORM   210-RJN-TAISYO-CHK-SEC
      *
      *¿©»öÎÅÍÜ¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-DPCNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        ¿©»ö¾ðÊó¤ÎÂàÈò
               PERFORM    VARYING   IDX  FROM  1  BY  1
                          UNTIL     IDX   >    31
      *            Æþ±¡»þ¿©»öÎÅÍÜÈñ
                   IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                       MOVE  SYS-5000-SHOKUJIRYOYOCD
                                          TO WRK-SYS5000-SHOKUCD(1 IDX)
                   END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©Æ²²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                       AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                             (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
      *                  Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç¤Î¤ß¥»¥Ã¥È
                           IF  WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                          "197000110"
                               MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
                 END-IF
                 MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                 MOVE    "key17"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
             MOVE     "tbl_nyuinacct"      TO  MCP-TABLE
             MOVE     "key17"              TO  MCP-PATHNAME
             PERFORM   910-NYUINACCT-READ-SEC
             PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
              IF       NACCT-ZAIKAISU   NOT =    ZERO
      *        ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
               PERFORM    210-NYUINACT-GET-SEC
      *        ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÃí²Ã»»¤ÎÈ½Äê
               INITIALIZE                      TNS-TENSU-REC
               MOVE    LNK-DPCNYUIN-HOSPNUM
                                           TO  TNS-HOSPNUM
               MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
               MOVE    LNK-DPCNYUIN-SANTEIYM
                                           TO  TNS-YUKOSTYMD(1:6)
               MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
               MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
               PERFORM   910-TENSU-READ-SEC
      *        Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
      *
               IF   TNS-CHUKSNCD     =   "0000"
                   ADD    1         TO  IDX-NYUKSN
                   MOVE   NSRY-SRYCD(1)    TO  WRK-NYUKSN-SRYCD-1
                                                      (IDX-NYUKSN)
                   MOVE   NACCT-ZAITEN     TO  WRK-NYUKSN-ZAITEN-1
                                                      (IDX-NYUKSN)
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                     UNTIL     IDX1   >   31
                        IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                        AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                        AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                          MOVE   NACCT-DAY(IDX1)  TO WRK-NYUKSN-DAY-1
                                                      (IDX-NYUKSN IDX1)
                        END-IF
                   END-PERFORM
               ELSE
                 IF ((TNS-CDKBN-KBNNUM   >=   100)   AND
                     (TNS-CDKBN-KBNNUM   <=   199))  OR
                    ((TNS-CDKBN-KBNNUM   >=   300)   AND
                     (TNS-CDKBN-KBNNUM   <=   399))
                      ADD    1         TO  IDX-CHUKSN1
                      MOVE   NSRY-SRYCD(1) TO  WRK-CHUKSN-SRYCD-1
                                                      (IDX-CHUKSN1)
                      MOVE   TNS-CHUKSNCD  TO  WRK-CHUKSN-CHUKSNCD-1
                                                      (IDX-CHUKSN1)
                      MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-1
                                                          (IDX-CHUKSN1)
                      MOVE   NACCT-ZAITEN  TO  WRK-CHUKSN-ZAITEN-1
                                                      (IDX-CHUKSN1)
                      PERFORM    VARYING   IDX1   FROM   1   BY   1
                        UNTIL     IDX1   >   31
                          IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                (LNK-NYUINDAY-KBN(IDX1) = "1"))
                          AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                          AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                             MOVE   NACCT-DAY(IDX1)
                                                 TO WRK-CHUKSN-DAY-1
                                                     (IDX-CHUKSN1 IDX1)
                          END-IF
                      END-PERFORM
                 END-IF
      *
                 IF ((TNS-CDKBN-KBNNUM   >=   200)   AND
                     (TNS-CDKBN-KBNNUM   <=   299))
                    ADD    1         TO  IDX-CHUKSN2
                    MOVE   NSRY-SRYCD(1)   TO  WRK-CHUKSN-SRYCD-2
                                                     (IDX-CHUKSN2)
                    MOVE   TNS-CHUKSNCD    TO  WRK-CHUKSN-CHUKSNCD-2
                                                     (IDX-CHUKSN2)
                    MOVE   TNS-CHUKSNTUBAN TO  WRK-CHUKSN-CHUKSNTUBAN-2
                                                          (IDX-CHUKSN2)
                    MOVE   NACCT-ZAITEN    TO  WRK-CHUKSN-ZAITEN-2
                                                          (IDX-CHUKSN2)
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL     IDX1   >   31
                        IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                        AND  ((WRK-GAIHAKU-KBN(IDX1)  = ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1)  >=  4))
                        AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                           MOVE   NACCT-DAY(IDX1)  TO WRK-CHUKSN-DAY-2
                                                     (IDX-CHUKSN2 IDX1)
                        END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
              END-IF
      *
              MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
              MOVE    "key17"                 TO  MCP-PATHNAME
              PERFORM 910-NYUINACCT-READ-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¡Ë
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-DPCNYUIN-PTID       TO  ACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-ZAIKBN    =        ZERO)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
                     INITIALIZE      SRYACT-REC
                     MOVE    ACCT-HOSPNUM   TO   SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN TO   SRY-NYUGAIKBN
                     MOVE    ACCT-PTID      TO   SRY-PTID
                     MOVE    ACCT-SRYKA     TO   SRY-SRYKA
                     MOVE    ACCT-SRYYM     TO   SRY-SRYYM
                     MOVE    ACCT-ZAINUM    TO   SRY-ZAINUM
                     MOVE    1              TO   SRY-RENNUM
                     MOVE    "tbl_sryact"   TO  MCP-TABLE
                     MOVE    "key"          TO  MCP-PATHNAME
                     PERFORM 910-SRYACT-SELECT-SEC
                     IF      FLG-SRYACT     =    ZERO
                         MOVE     "tbl_sryact"   TO  MCP-TABLE
                         MOVE     "key"          TO  MCP-PATHNAME
                         PERFORM   910-SRYACT-READ-SEC
                         IF  FLG-SRYACT    =    ZERO
      *                    ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÃí²Ã»»¤ÎÈ½Äê
                           INITIALIZE                TNS-TENSU-REC
                           MOVE    LNK-DPCNYUIN-HOSPNUM
                                                 TO  TNS-HOSPNUM
                           MOVE    SRY-SRYCD(1)  TO  TNS-SRYCD
                           MOVE    LNK-DPCNYUIN-SANTEIYM
                                                 TO  TNS-YUKOSTYMD(1:6)
                           MOVE    "01"          TO  TNS-YUKOSTYMD(7:2)
                           MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                           PERFORM   910-TENSU-READ-SEC
                         END-IF
                     END-IF
      *              ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                     MOVE     "tbl_sryact"   TO  MCP-TABLE
                     MOVE     "key"          TO  MCP-PATHNAME
                     PERFORM   900-CLOSE-SEC
      *             Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                    IF  TNS-CHUKSNCD         =   "0000"
                       ADD    1              TO  IDX-NYUKSN
                       MOVE   SRY-SRYCD(1)   TO  WRK-NYUKSN-SRYCD-1
                                                        (IDX-NYUKSN)
                       MOVE   ACCT-ZAITEN    TO  WRK-NYUKSN-ZAITEN-1
                                                        (IDX-NYUKSN)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                         UNTIL     IDX1   >   31
                         IF  ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                              (LNK-NYUINDAY-KBN(IDX1) = "1"))
                         AND ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR 
                              (WRK-GAIHAKU-KBN(IDX1)  >=  4))
                         AND  (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                           MOVE   ACCT-DAY(1 IDX1)  TO WRK-NYUKSN-DAY-1
                                                      (IDX-NYUKSN IDX1)
                         END-IF
                       END-PERFORM
                    ELSE
      *
                      IF ((TNS-CDKBN-KBNNUM   >=   100)   AND
                          (TNS-CDKBN-KBNNUM   <=   199))  OR
                         ((TNS-CDKBN-KBNNUM   >=   300)   AND
                          (TNS-CDKBN-KBNNUM   <=   399))
                         ADD    1         TO  IDX-CHUKSN1
                         MOVE   SRY-SRYCD(1)
                                           TO  WRK-CHUKSN-SRYCD-1
                                                          (IDX-CHUKSN1)
                         MOVE   TNS-CHUKSNCD
                                           TO  WRK-CHUKSN-CHUKSNCD-1
                                                          (IDX-CHUKSN1)
                         MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-1
                                                          (IDX-CHUKSN1)
                         MOVE   ACCT-ZAITEN
                                           TO  WRK-CHUKSN-ZAITEN-1
                                                          (IDX-CHUKSN1)
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                           UNTIL     IDX1   >   31
                            IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                  (LNK-NYUINDAY-KBN(IDX1) = "1"))
                            AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                  (WRK-GAIHAKU-KBN(IDX1)  >=  4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                              MOVE   ACCT-DAY(1 IDX1)
                                                  TO  WRK-CHUKSN-DAY-1
                                                     (IDX-CHUKSN1 IDX1)
                            END-IF
                         END-PERFORM
                      END-IF
      *
                      IF ((TNS-CDKBN-KBNNUM   >=   200)   AND
                          (TNS-CDKBN-KBNNUM   <=   299))
                         ADD    1         TO  IDX-CHUKSN2
                         MOVE   SRY-SRYCD(1)
                                           TO  WRK-CHUKSN-SRYCD-2
                                                          (IDX-CHUKSN2)
                         MOVE   TNS-CHUKSNCD
                                           TO  WRK-CHUKSN-CHUKSNCD-2
                                                          (IDX-CHUKSN2)
                         MOVE   TNS-CHUKSNTUBAN
                                           TO  WRK-CHUKSN-CHUKSNTUBAN-2
                                                          (IDX-CHUKSN2)
                         MOVE   ACCT-ZAITEN
                                           TO  WRK-CHUKSN-ZAITEN-2
                                                          (IDX-CHUKSN2)
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                           UNTIL     IDX1   >   31
                            IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                  (LNK-NYUINDAY-KBN(IDX1) = "1"))
                            AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                  (WRK-GAIHAKU-KBN(IDX1)  >=   4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                              MOVE   ACCT-DAY(1 IDX1)
                                                  TO  WRK-CHUKSN-DAY-2
                                                     (IDX-CHUKSN2 IDX1)
                            END-IF
                         END-PERFORM
                      END-IF
      *
      *               °åÎÅ´Ñ»¡¤Î²Ã»»¼êÆþÎÏÊ¬
                      IF   SRY-SRYCD(1)(1:3)    =     "188"
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                           UNTIL     IDX1   >   31
                            IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                  (LNK-NYUINDAY-KBN(IDX1) = "1"))
                            AND  ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                                  (WRK-GAIHAKU-KBN(IDX1) >=   4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                            AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
                               MOVE   SRY-SRYCD(1)  TO
                                            WRK-IRYOKNS-SRYCD5(IDX1)
                               MOVE   "1"           TO
                                            WRK-IRYOKNS-SORT-FLG(IDX1)
                            END-IF
                         END-PERFORM
                      END-IF
      *
      *
                    END-IF
      *
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key30"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_sryacct"   TO  MCP-TABLE
           MOVE     "key30"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Æþ±¡ÎÁ²Ã»»¤òÃí²Ã»»¥³¡¼¥É¡¢Ãí²Ã»»ÄÌÈÖ½ç¤ËÊÂ¤ÓÂØ¤¨
           PERFORM      VARYING    IDX-CHUKSN1   FROM   1  BY  1
             UNTIL     (IDX-CHUKSN1   >   50)   OR
                       (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)  =   SPACE)
               COMPUTE   IDX-CHUKSN1SS   =   IDX-CHUKSN1  +  1
               PERFORM   VARYING IDX-CHUKSN1S FROM IDX-CHUKSN1SS  BY  1
                 UNTIL   (IDX-CHUKSN1S   >   50)   OR
                         (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1S) =  SPACE)
                   IF  WRK-CHUKSN-CHUCD-1(IDX-CHUKSN1)
                                   <=  WRK-CHUKSN-CHUCD-1(IDX-CHUKSN1S)
                       CONTINUE
                   ELSE
                       MOVE   WRK-CHUKSN-TBL-1(IDX-CHUKSN1)
                                      TO   WRK-CHUKSN-TAIHI
                       MOVE   WRK-CHUKSN-TBL-1(IDX-CHUKSN1S)
                                      TO  WRK-CHUKSN-TBL-1(IDX-CHUKSN1)
                       MOVE   WRK-CHUKSN-TAIHI
                                      TO  WRK-CHUKSN-TBL-1(IDX-CHUKSN1S)
                   END-IF
               END-PERFORM
           END-PERFORM
           PERFORM      VARYING    IDX-CHUKSN2   FROM   1  BY  1
             UNTIL     (IDX-CHUKSN2   >   50)   OR
                       (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)  =   SPACE)
               COMPUTE   IDX-CHUKSN2SS   =   IDX-CHUKSN2  +  1
               PERFORM   VARYING IDX-CHUKSN2S FROM IDX-CHUKSN2SS  BY  1
                 UNTIL   (IDX-CHUKSN2S   >   50)   OR
                         (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2S) =  SPACE)
                   IF  WRK-CHUKSN-CHUCD-2(IDX-CHUKSN2)
                                   <=  WRK-CHUKSN-CHUCD-2(IDX-CHUKSN2S)
                       CONTINUE
                   ELSE
                       MOVE   WRK-CHUKSN-TBL-2(IDX-CHUKSN2)
                                      TO   WRK-CHUKSN-TAIHI
                       MOVE   WRK-CHUKSN-TBL-2(IDX-CHUKSN2S)
                                      TO  WRK-CHUKSN-TBL-2(IDX-CHUKSN2)
                       MOVE   WRK-CHUKSN-TAIHI
                                      TO  WRK-CHUKSN-TBL-2(IDX-CHUKSN2S)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *
      *    °åÎÅ´Ñ»¡¤Ç¼êÆþÎÏ¤µ¤ì¤¿²Ã»»¡ÊÅ¾±¡Ä´À°²Ã»»¡Ë¤¬¤¢¤ë¾ì¹ç¤ËÊÂ¤ÓÂØ¤¨
      *    Ãí²Ã»»ÄÌÈÖ¤Î¥»¥Ã¥È
           PERFORM      VARYING    IDX1   FROM   1  BY  1
             UNTIL      IDX1      >   31
               IF   WRK-IRYOKNS-SORT-FLG(IDX1)  =   "1"
                    PERFORM     VARYING  IDX2   FROM  1 BY 1
                      UNTIL     IDX2  >  5
                      IF  WRK-IRYOKNS-SRYCD(IDX1 IDX2) NOT = SPACE
      *                 ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÃí²Ã»»ÄÌÈÖ¤Î¥»¥Ã¥È
                        INITIALIZE                TNS-TENSU-REC
                        MOVE    LNK-DPCNYUIN-HOSPNUM
                                              TO  TNS-HOSPNUM
                        MOVE    WRK-IRYOKNS-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
                        MOVE    LNK-DPCNYUIN-SANTEIYM
                                              TO TNS-YUKOSTYMD(1:6)
                        MOVE    "01"          TO TNS-YUKOSTYMD(7:2)
                        MOVE    TNS-YUKOSTYMD TO  TNS-YUKOEDYMD
                        PERFORM   910-TENSU-READ-SEC
                        MOVE    TNS-CHUKSNTUBAN
                                            TO  WRK-IRYOKNS-CHUKSNTUBAN
                                                       (IDX1 IDX2)
                      END-IF
                    END-PERFORM
               END-IF
           END-PERFORM
      *    Ãí²Ã»»ÄÌÈÖ¤ÎÊÂ¤ÓÂØ¤¨
           PERFORM      VARYING    IDX1   FROM   1  BY  1
             UNTIL      IDX1      >   31
               IF  WRK-IRYOKNS-SORT-FLG(IDX1)  =   "1"
                   PERFORM     VARYING  IDX2   FROM  1 BY 1
                     UNTIL     IDX2  >  4
                     COMPUTE   IDX4   =    IDX2   +   1
                     PERFORM   VARYING   IDX3  FROM  IDX4  BY  1
                       UNTIL   IDX3    >  5
                       IF (WRK-IRYOKNS-SRYCD(IDX1 IDX2) NOT = SPACE) AND
                          (WRK-IRYOKNS-SRYCD(IDX1 IDX3) NOT = SPACE)
                           IF  WRK-IRYOKNS-CHUKSNTUBAN(IDX1 IDX2)
                                 <=  WRK-IRYOKNS-CHUKSNTUBAN(IDX1 IDX3)
                               CONTINUE
                           ELSE
                              MOVE  WRK-IRYOKNS-SRYCD-OCC(IDX1 IDX2)
                                           TO  WRK-IRYOKNS-TAIHI
                              MOVE  WRK-IRYOKNS-SRYCD-OCC(IDX1 IDX3)
                                           TO
                                        WRK-IRYOKNS-SRYCD-OCC(IDX1 IDX2)
                              MOVE  WRK-IRYOKNS-TAIHI
                                           TO
                                        WRK-IRYOKNS-SRYCD-OCC(IDX1 IDX3)
                           END-IF
                       END-IF
                     END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *
      *    ¼èÆÀ¤·¤¿Æþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿ÇÎÅ¶èÊ¬
               MOVE   WRK-SRYKBN(IDX1)  TO  WRK-SO-SRYKBN(IDX1)
               IF    (LNK-NYUINDAY-KBN(IDX1)   =    "1")   AND
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  NOT =    SPACE)
                   MOVE   "92"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
               IF    (LNK-NYUINDAY-KBN(IDX1)   =    "1")   AND
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  =    SPACE)  AND
                     (WRK-NYUINKHN(IDX1)             =    SPACE)
                   MOVE   "90"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
      *        ÊÝ¸±ÁÈ¹ç¤»
               MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                        TO  WRK-SO-HKNCOMBI-DAY(IDX1)
      *
               MOVE     ZERO                TO  WRK-MEISAISU
      *
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   IF     FLG-SENTEI-SET    NOT =   ZERO
                       IF   TBL-SENTEI(IDX1)    =   1
                           MOVE    ZERO    TO  FLG-SENTEICDCHG
                           INITIALIZE  SENTEICDCHG-REC
                           MOVE    WRK-NYUINKHN(IDX1)
                                           TO  SENCHG-IPNSRYCD
                           MOVE     "tbl_senteicdchg"  TO  MCP-TABLE
                           MOVE     "key2"             TO  MCP-PATHNAME
                           PERFORM   910-SENTEICDCHG-SELECT-SEC
                           IF      FLG-SENTEICDCHG  =    ZERO
                               MOVE     "tbl_senteicdchg"
                                                      TO  MCP-TABLE
                               MOVE     "key2"        TO  MCP-PATHNAME
                               PERFORM  910-SENTEICDCHG-READ-SEC
                               IF    FLG-SENTEICDCHG  =   ZERO
                                  IF    WRK-NYUINKHN(IDX1)  NOT =
                                                   SENCHG-SENTEISRYCD
                                       MOVE  SENCHG-SENTEISRYCD
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                                  ELSE
      *                               Ìë¶Ð»þ´ÖÄ¶²á¸º»»¤ÎÁªÄê½èÍý
                                      IF  LNK-DPCNYUIN-SANTEIYM
                                                      >=    "201404"
                                        ADD    1      TO  WRK-MEISAISU
                                        MOVE  "190122890"
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                                      END-IF
                                  END-IF
                               END-IF
                          END-IF
                          MOVE     "tbl_senteicdchg"   TO  MCP-TABLE
                          MOVE     "key2"              TO  MCP-PATHNAME
                         PERFORM   900-CLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»
               IF         WRK-KANGOGEN(IDX1)  NOT =        SPACE
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-KANGOGEN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        °åÎÅ´Ñ»¡
               IF         WRK-IRYOKNS-SRYCD1(IDX1)  NOT =   SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-IRYOKNS-SRYCD1(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-IRYOKNS-SRYCD2(IDX1)  NOT =   SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-IRYOKNS-SRYCD2(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-IRYOKNS-SRYCD3(IDX1)  NOT =   SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-IRYOKNS-SRYCD3(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-IRYOKNS-SRYCD4(IDX1)  NOT =   SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-IRYOKNS-SRYCD4(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-IRYOKNS-SRYCD5(IDX1)  NOT =   SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-IRYOKNS-SRYCD5(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        Â¾°åÎÅµ¡´Ø¼õ¿Ç¡Ê£³£°¡ó¸º»»¤È£·£°¡ó¸º»»¡Ë
      *        ÆÃÄê»þ´ÖÂà±¡¸º»»
      *        ÆÃÄêÍËÆüÆþÂà±¡¸º»»
               IF     ((WRK-GAIHAKU(IDX1)        =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590" OR  "190213790"))
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   IF        (WRK-NYUINKHN(IDX1)  NOT =   SPACE)
                   AND       (WRK-GAIHAKU2(IDX1)  NOT =   SPACE)
                       ADD      1               TO  WRK-MEISAISU
                       MOVE   WRK-GAIHAKU2(IDX1) TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
               END-IF
      *
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590" OR  "190213790"))
               AND     (WRK-SHOKIKSN(IDX1)      NOT =   SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        É¸·ç
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590" OR  "190213790"))
               AND     (WRK-HYOUKETSU(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOUKETSU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        Äê¿ôÄ¶²á
               IF     ((WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590" OR  "190213790"))
               AND     (WRK-TEISU(IDX1)       NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-TEISU(IDX1)  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        ³°Çñ
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                 IF     ((WRK-GAIHAKU(IDX1)     =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161670" OR  "190161770" OR
                                    "190161870" OR  "190161970" OR
                                    "190162070" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )  OR
                       (WRK-GAIHAKU2(IDX1)       =
                                    "190108890" OR  "190142190" OR
                                    "190141590" OR  "190213790"))
                          CONTINUE
                 ELSE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE   WRK-GAIHAKU-KBN(IDX1)
                                            TO  WRK-SO-GAIHAKU-KBN
                                                           (IDX1 IDX1)
                 END-IF
               END-IF
      *
      *
      *        Æþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡Ê£Á£±£°£°ÈÖÂæ¡¢£Á£³£°£°ÈÖÂæ¡Ë
               PERFORM    VARYING   IDX-CHUKSN1  FROM  1   BY  1
                 UNTIL    (IDX-CHUKSN1    >   50)    OR
                          (WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)  =  SPACE)
                   IF    WRK-CHUKSN-DAY-1(IDX-CHUKSN1 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-SRYCD-1(IDX-CHUKSN1)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
               END-PERFORM
      *
      *        Æþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡Ê£Á£²£°£°ÈÖÂæ¡Ë
               PERFORM    VARYING   IDX-CHUKSN2  FROM  1   BY  1
                 UNTIL    (IDX-CHUKSN2    >   50)    OR
                          (WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)  =  SPACE)
                   IF    WRK-CHUKSN-DAY-2(IDX-CHUKSN2 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-SRYCD-2(IDX-CHUKSN2)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
               END-PERFORM
      *
      *        ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»¥»¥Ã¥È
               PERFORM   VARYING   IDX-NYUKSN   FROM   1  BY   1
                 UNTIL  (IDX-NYUKSN    >     50)    OR
                        (WRK-NYUKSN-SRYCD-1(IDX-NYUKSN)  =  SPACE)
                  IF    WRK-NYUKSN-DAY-1(IDX-NYUKSN IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD   1    TO  WRK-MEISAISU
                      MOVE  WRK-NYUKSN-SRYCD-1(IDX-NYUKSN)
                                 TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                  END-IF
               END-PERFORM
      *
      *
      *        ÁªÄêÆþ±¡»þ¤Î³°Çñ
               IF   (TBL-SENTEI(IDX1)    =         1)   AND
                    (WRK-GAIHAKU-KBN(IDX1)     NOT =  ZERO)
                   EVALUATE   WRK-GAIHAKU-KBN(IDX1)
      *                ³°Çñ
                       WHEN   3
      *            AZU-MOD START 2015.6.10
      *            ÁªÄêÆþ±¡»þ¤Î³°Çñ¤â¥ì¥³¡¼¥Éµ­Ï¿¤¹¤ë¤è¤¦¤ËÂÐ½è
                           IF    LNK-DPCNYUIN-SANTEIYM  <=  "201503"
                              INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
                           ELSE
                              MOVE  1   TO  WRK-SENTEI-GAIGAKU(IDX1)
                           END-IF
      *            AZU-MOD END 2015.6.10
      *                Â¾°åÎÅµ¡´Ø¼õ¿Ç
                       WHEN   4
      ******                 WHEN   5
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
                   END-EVALUATE
               END-IF
      *
      *        °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¥³¥á¥ó¥È¡ÊÊ¿À®£²£¸Ç¯£±£°·î¿ÇÎÅÊ¬°Ê¹ß¡Ë
               IF     LNK-DPCNYUIN-SANTEIYM       >=  "201610"
                 IF     LNK-RYOCOM-NYUINKBN(IDX1)    =   "1"
                   PERFORM    VARYING  IDX-COM  FROM  1  BY  1
                     UNTIL   (IDX-COM    >   30)   OR
                             (LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX-COM) =
                                                             SPACE)
                       ADD    1             TO  WRK-MEISAISU
                       MOVE     LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX-COM)
                                            TO  WRK-SO-SRYCD
                                                (IDX1 WRK-MEISAISU)
                   END-PERFORM
                   ADD      1               TO  WRK-MEISAISU
                   MOVE     LNK-RYOCOM-ADL-SRYCD-H28(IDX1 1)
                                            TO  WRK-SO-SRYCD
                                                (IDX1 WRK-MEISAISU)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE     LNK-RYOCOM-ADL-SRYCD-H28(IDX1 2)
                                            TO  WRK-SO-SRYCD
                                                (IDX1 WRK-MEISAISU)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE     LNK-RYOCOM-ADL-SRYCD-H28(IDX1 3)
                                            TO  WRK-SO-SRYCD
                                                (IDX1 WRK-MEISAISU)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE     LNK-RYOCOM-ADL-SRYCD-H28(IDX1 4)
                                            TO  WRK-SO-SRYCD
                                                (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ÅÀ¿ô¥Þ¥¹¥¿¤òÆÉ¤ß¹þ¤ßÃ¼¿ô½èÍýÂÐ¾Ý¹Ô¤Î¼èÆÀ
               MOVE     ZERO      TO     FLG-HASUU
               PERFORM   VARYING   IDX2    FROM   20  BY   -1
                         UNTIL  (IDX2        =  1)   OR
                                (FLG-HASUU   =  1)
                 IF   WRK-SO-SRYCD(IDX1 IDX2)(1:1)   =  "1"
                   MOVE  LNK-DPCNYUIN-HOSPNUM    TO  TNS-HOSPNUM
                   MOVE  LNK-DPCNYUIN-SANTEIYM   TO  TNS-YUKOSTYMD(1:6)
                   MOVE  "01"                    TO  TNS-YUKOSTYMD(7:2)
                   MOVE  TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
                   MOVE  WRK-SO-SRYCD(IDX1 IDX2) TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU             =   ZERO
                      PERFORM 910-TENSU-READ-SEC
                   END-IF
      *            ¸º»»¥³¡¼¥É¤ÎÈ½Äê
                   IF    TNS-MICMESUKSN     >=    3
                       IF     FLG-HASUU   =   ZERO
                         MOVE   "1"   TO   WRK-SO-HASUU(IDX1 IDX2)
                         MOVE   1     TO   FLG-HASUU
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *         DISPLAY "WRK-SO-SRYCD(IDX1 1) = " WRK-SO-SRYCD(IDX1 1)
      *         DISPLAY "WRK-SO-HASUU(IDX1 1) = " WRK-SO-HASUU(IDX1 1)
      *         DISPLAY "WRK-SO-SRYCD(IDX1 2) = " WRK-SO-SRYCD(IDX1 2)
      *         DISPLAY "WRK-SO-HASUU(IDX1 2) = " WRK-SO-HASUU(IDX1 2)
      *         DISPLAY "WRK-SO-SRYCD(IDX1 3) = " WRK-SO-SRYCD(IDX1 3)
      *         DISPLAY "WRK-SO-HASUU(IDX1 3) = " WRK-SO-HASUU(IDX1 3)
      *         DISPLAY "WRK-SO-SRYCD(IDX1 4) = " WRK-SO-SRYCD(IDX1 4)
      *         DISPLAY "WRK-SO-HASUU(IDX1 4) = " WRK-SO-HASUU(IDX1 4)
      *         DISPLAY "WRK-SO-SRYCD(IDX1 5) = " WRK-SO-SRYCD(IDX1 5)
      *         DISPLAY "WRK-SO-HASUU(IDX1 5) = " WRK-SO-HASUU(IDX1 5)
      *         DISPLAY "WRK-SO-SRYCD(IDX1 6) = " WRK-SO-SRYCD(IDX1 6)
      *         DISPLAY "WRK-SO-HASUU(IDX1 6) = " WRK-SO-HASUU(IDX1 6)
      *        ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (WRK-SO-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      50)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
      *
      *        ¡ó¸º»»¤ÎÃ¼¿ô½èÍý
               COMPUTE    WRK-SO-TEN(IDX1)  =
                                  WRK-SO-TEN(IDX1)  +  0.5
      *        ¾®¿ôÅÀ°Ê²¼ÀÚ¼Î¤Æ
               MOVE   ZERO              TO  WRK-SO-TEN-SEISU
               MOVE   WRK-SO-TEN(IDX1)  TO  WRK-SO-TEN-SEISU
               MOVE   WRK-SO-TEN-SEISU  TO  WRK-SO-TEN(IDX1)
      *
           END-PERFORM
      *
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      **                                                         (IDX1)
      **          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      **          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      **          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      **          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      **          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      **          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      **          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      **          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      **          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      **          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      **     END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£°¡Ë
           MOVE       ZERO      TO     IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
      *AZU-DEL START 2015.6.10
      **            (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
      *AZU-DEL END 2015.6.10
                    (WRK-SO-SRYKBN(IDX1)    =       "90")
                     MOVE  IDX1   TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-DPCNYUIN-HKNCOMBI(1 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-DPCNYUIN-MEISAISU(1 IDX2)
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   50)
                         MOVE   "SI"   TO
                                    LNK-DPCNYUIN-RECSKB(1 IDX2 IDX3)
      **                   IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-DPCNYUIN-SRYSKB(1 IDX2 IDX3)
      **                   END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-DPCNYUIN-SRYCD(1 IDX2 IDX3)
                         MOVE  WRK-SO-HASUU(IDX1 IDX3) TO
                                    LNK-DPCNYUIN-HASUU(1 IDX2 IDX3)
      *AZU-ADD START 2015.6.10
                         IF  (WRK-SO-SRYCD(IDX1 IDX3) = "190107290") AND
                             (WRK-SENTEI-GAIGAKU(IDX1) = 1)
                            MOVE  1  TO
                               LNK-DPCNYUIN-SENTEI-GAIHAKU(1 IDX2 IDX3)
                         END-IF
      *AZU-ADD END 2015.6.10
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =   SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-DPCNYUIN-TEN(1 IDX2 IDX3)
                               MOVE   WRK-SO-KAISU(IDX1)   TO
                                     LNK-DPCNYUIN-KAISU(1 IDX2 IDX3)
                         END-IF
                     END-PERFORM
               END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆÃÄêÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£²¡Ë
           MOVE      ZERO     TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "92")
                     MOVE   IDX1     TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-DPCNYUIN-HKNCOMBI(2 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-DPCNYUIN-MEISAISU(2 IDX2)
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   50)
                         MOVE   "SI"   TO   
                                    LNK-DPCNYUIN-RECSKB(2 IDX2 IDX3)
      **                   IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-DPCNYUIN-SRYSKB(2 IDX2 IDX3)
      **                   END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-DPCNYUIN-SRYCD(2 IDX2 IDX3)
                         MOVE  WRK-SO-HASUU(IDX1 IDX3) TO
                                    LNK-DPCNYUIN-HASUU(2 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =    SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-DPCNYUIN-TEN(2 IDX2 IDX3)
                               MOVE   WRK-SO-KAISU(IDX1)   TO
                                     LNK-DPCNYUIN-KAISU(2 IDX2 IDX3)
                         END-IF
                     END-PERFORM
                END-IF
           END-PERFORM
      *
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸
           IF    LNK-DPCNYUIN-SANTEIYM  <=  "201603"
               PERFORM   2103-SHOKUJI-HENSHU3-SEC
           ELSE
               IF    LNK-DPCNYUIN-SANTEIYM  <=  "201709"
                   PERFORM   2103-SHOKUJI-HENSHU4-SEC
               ELSE
                   PERFORM   2103-SHOKUJI-HENSHU5-SEC
               END-IF
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-DPCNYUIN-SANTEIYM  >=  "200610")  AND
               (LNK-DPCNYUIN-SANTEIYM  <=  "201603")
               IF  LNK-NYULIFE-RC         =    ZERO
                   PERFORM   2104-KANKYO-HENSHU-SEC
               END-IF
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-DPCNYUIN-SANTEIYM  >=  "201604")  AND
               (LNK-DPCNYUIN-SANTEIYM  <=  "201709")  AND
               (LNK-NYULIFE2-RC         =    ZERO)
               PERFORM   2104-KANKYO-HENSHU2-SEC
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-DPCNYUIN-SANTEIYM  >=  "201710")  AND
               (LNK-NYULIFE2-RC         =    ZERO)
               PERFORM   2104-KANKYO-HENSHU3-SEC
           END-IF
      *
      *    ÉÂÅïÈÖ¹æ¡ÊÉÂÅï¶èÊ¬¡Ë¤ÎÊÔ½¸
           PERFORM   2105-BTUNUM-HENSHU-SEC
      *
      *    À¸³èÎÅÍÜ¿©»ö¥Æ¥¹¥ÈÉ½¼¨
      **     DISPLAY "==SEIKATSU-SOKUJI=="
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "SRYCD,TEN,KAISU(1) = " 
      **                                     LNK-DPCNYUIN-SRYCD(4 IDX1 1)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(4 IDX1 1)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(4 IDX1 1)
      **          DISPLAY  "SRYCD,TEN,KAISU(2) = " 
      **                                     LNK-DPCNYUIN-SRYCD(4 IDX1 2)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(4 IDX1 2)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(4 IDX1 2)
      **          DISPLAY  "SRYCD,TEN,KAISU(3) = " 
      **                                     LNK-DPCNYUIN-SRYCD(4 IDX1 3)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(4 IDX1 3)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(4 IDX1 3)
      **     END-PERFORM
      *    À¸³èÎÅÍÜµï½»Èñ¥Æ¥¹¥ÈÉ½¼¨
      **     DISPLAY "==SEIKATSU-SOKUJI=="
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "SRYCD,TEN,KAISU(1) = " 
      **                                     LNK-DPCNYUIN-SRYCD(5 IDX1 1)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(5 IDX1 1)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(5 IDX1 1)
      **          DISPLAY  "SRYCD,TEN,KAISU(2) = " 
      **                                     LNK-DPCNYUIN-SRYCD(5 IDX1 2)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(5 IDX1 2)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(5 IDX1 2)
      **          DISPLAY  "SRYCD,TEN,KAISU(3) = " 
      **                                     LNK-DPCNYUIN-SRYCD(5 IDX1 3)
      **                                     ","
      **                                     LNK-DPCNYUIN-TEN(5 IDX1 3)
      **                                     ","
      **                                     LNK-DPCNYUIN-KAISU(5 IDX1 3)
      **     END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°ÇñÁ°½èÍý
      *****************************************************************
       210-GAIHAKU-MAE-SEC            SECTION.
      *
      *    ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¤È¿ÇÎÅ¶èÊ¬¤ÎÂàÈò
           INITIALIZE                      TNS-TENSU-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM
                                       TO  TNS-HOSPNUM
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-DPCNYUIN-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           IF  (LNK-DPCNYUIN-SANTEIYM >= "202004")
           AND (TNS-SRYCD             NOT = SPACE   )
           AND (TNS-NYUKHNKSNKBN          = ZERO    )
             INITIALIZE                          SNTENSU-AREA
             MOVE  TNS-SRYCD                 TO  SNTENSU-SRYCD
             MOVE  LNK-DPCNYUIN-SANTEIYM     TO  SNTENSU-BASEYMD(1:6)
             MOVE  "01"                      TO  SNTENSU-BASEYMD(7:2)
             CALL  "ORCSNTENSU"              USING
                                             SNTENSU-AREA
                                             TNS-TENSU-REC
                                             SPA-AREA
           END-IF
           MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSNKBN
           MOVE    TNS-CHUKSNCD        TO  WRK-CHUKSNCD-XXX
           MOVE    TNS-NYUKHNKBN       TO  WRK-NYUKHNKBN-XXX
      *
      *    Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÃÖ¤­´¹¤¨
           IF  LNK-DPCNYUIN-SANTEIYM   >= "201804"
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
               IF   (TNS-NYUKHNKSNKBN = 27 )
               AND  (TNS-CDKBN-KBNNUM = 101)
                    MOVE   901   TO   WRK-KSNKBN
               END-IF
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
               IF   (TNS-NYUKHNKSNKBN = 27 )
               AND  (TNS-CDKBN-KBNNUM = 109)
                    MOVE   909   TO   WRK-KSNKBN
               END-IF
           END-IF
      *
           .
       210-GAIHAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       210-GAIHAKU-SET-SEC            SECTION.
      *
      *     DISPLAY "210-GAIHAKU-SET-SEC"
           MOVE   NACCT-DAY(IDX1)     TO    WRK-GAIHAKU-KBN(IDX1)
      *     DISPLAY "WRK-SRYKBN(IDX1) = " WRK-SRYKBN(IDX1)
      *
           EVALUATE   WRK-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
      *            AZU-ADD START 2015.6.10
                    WHEN     3
      *            AZU-ADD END   2015.6.10
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                    WHEN     8
                    WHEN     9
      ***           WHEN    10
      ***           WHEN    11
      ***           WHEN    12
                    WHEN    13
                    WHEN    17
                      PERFORM   2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
              WHEN     "92"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                    WHEN     8
                    WHEN     9
      ***           WHEN    10
      ***           WHEN    11
      ***           WHEN    12
                    WHEN    13
                    WHEN    17
                      PERFORM   2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
           END-EVALUATE
      *     DISPLAY "IDX1 = " IDX1
      *     DISPLAY "WRK-GAIHAKU(IDX1) = " WRK-GAIHAKU(IDX1)
      *
           .
       210-GAIHAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEC           SECTION.
      *
           IF       LNK-DPCNYUIN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-TAKIKAN3-SEC
           ELSE
             IF     LNK-DPCNYUIN-SANTEIYM  >=  "201604"
                   PERFORM   2101-GAIHAKU-TAKIKAN2-SEC
             ELSE
                   PERFORM   2101-GAIHAKU-TAKIKAN1-SEC
             END-IF
           END-IF
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ£±
      *****************************************************************
       2101-GAIHAKU-TAKIKAN1-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *
               WHEN   099  THRU  100
                 IF   LNK-DPCNYUIN-SANTEIYM  >=  "201004"
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  229
               WHEN   230  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  255
      *        (H26-ADD-ST)
               WHEN   256
               WHEN   257  THRU  259
               WHEN   260  THRU  261
               WHEN   262  THRU  265
               WHEN   266  THRU  267
      *        (H26-ADD-ED)
                 IF    WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                      MOVE   "190153190"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153390"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   IF  WRK-NYUKHNKSNKBN(IDX1) =
                           229 OR 238 OR 215 OR 216 OR 217 OR 239
                                                           OR 252
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                       WHEN     6
                         MOVE   "190153190" TO   WRK-GAIHAKU(IDX1)
                     END-EVALUATE
                   ELSE
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                     END-EVALUATE
                   END-IF
                 END-IF
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
               WHEN   173  THRU  190
               WHEN   196
      *        (H26-ADD-ST)
               WHEN   199
               WHEN   004  THRU  008
               WHEN   010  THRU  012
      *        (H26-ADD-ED)
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   075  THRU  080
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                 IF    WRK-NYUKHNKSNKBN(IDX1) =
                           153 OR 154 OR 155 OR 156 OR 157 OR 158
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153290"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
               WHEN   226
               WHEN   240
                 CONTINUE
               WHEN   031
               WHEN   033
               WHEN   034
               WHEN   035
               WHEN   038
               WHEN   170
               WHEN   191
      *        (H26-ADD-ST)
               WHEN   017  THRU  020
      *        (H26-ADD-ED)
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   022
               WHEN   023
               WHEN   024
               WHEN   025
               WHEN   026
               WHEN   029
               WHEN   136
               WHEN   168
               WHEN   169
      *        (H26-ADD-ST)
               WHEN   013  THRU  016
      *        (H26-ADD-ED)
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   081
               WHEN   083
               WHEN   172
      *        (H26-ADD-ST)
               WHEN   032  THRU  037
      *        (H26-ADD-ED)
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   OTHER
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ£²  201604
      *****************************************************************
       2101-GAIHAKU-TAKIKAN2-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *        ÆÃÄêÆþ±¡ÎÁ
               WHEN   201  THRU  206
               WHEN   208  THRU  210
               WHEN   212  THRU  217
               WHEN   221
               WHEN   227  THRU  229
               WHEN   232  THRU  236
               WHEN   238  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  254
               WHEN   257  THRU  267
      *        --- H28.4 TUIKA ---
               WHEN   268  THRU  270
      *          Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ¡¢Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
      *          ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ
                 IF  WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232 OR 217 OR 268
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                      MOVE   "190182990"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
               WHEN   004  THRU  008
               WHEN   010  THRU  012
               WHEN   152
               WHEN   173  THRU  190
               WHEN   196
               WHEN   199
      *        --- H28.4 TUIKA ---
               WHEN   048  THRU  049
               WHEN   051  THRU  054
               WHEN   057  THRU  068
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
               WHEN   153  THRU  158
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *        Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
               WHEN   226
               WHEN   240
                 CONTINUE
               WHEN   OTHER
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ£³  201804
      *****************************************************************
       2101-GAIHAKU-TAKIKAN3-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *        ÆÃÄêÆþ±¡ÎÁ
               WHEN   200  THRU  257
               WHEN   260  THRU  263
      *          Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ¡¢Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
      *          ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ
                 IF  WRK-NYUKHNKSNKBN(IDX1) = 255 OR 254 OR 257
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                      MOVE   "190182990"    TO   WRK-GAIHAKU(IDX1)
                    WHEN    14
                      MOVE   "190214390"    TO   WRK-GAIHAKU(IDX1)
                    WHEN    15
                      MOVE   "190213990"    TO   WRK-GAIHAKU(IDX1)
                    WHEN    16
                      MOVE   "190214190"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN    14
                      MOVE   "190214390"    TO   WRK-GAIHAKU(IDX1)
                    WHEN    15
                      MOVE   "190213990"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 END-IF
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
               WHEN    11  THRU   14
               WHEN    51
               WHEN   901
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    14
                    MOVE   "190214290"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    15
                    MOVE   "190213890"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
               WHEN    26
               WHEN    53
               WHEN   909
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    14
                    MOVE   "190214290"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    15
                    MOVE   "190213890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    16
                    MOVE   "190214090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *        Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
               WHEN   258  THRU  259
                 CONTINUE
               WHEN   OTHER
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    14
                    MOVE   "190213790"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸º»»¥³¡¼¥É¼èÆÀ
      *****************************************************************
       2101-GAIHAKU-GENSAN-SEC           SECTION.
      *
           IF       LNK-DPCNYUIN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-GENSAN3-SEC
           ELSE
             IF     LNK-DPCNYUIN-SANTEIYM  >=  "201604"
                   PERFORM   2101-GAIHAKU-GENSAN2-SEC
             ELSE
                   PERFORM   2101-GAIHAKU-GENSAN1-SEC
             END-IF
           END-IF
      *
           .
       2101-GAIHAKU-GENSAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸º»»¥³¡¼¥É¼èÆÀ£±
      *****************************************************************
       2101-GAIHAKU-GENSAN1-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      IF   LNK-DPCNYUIN-SANTEIYM  >=  "201404"
                        IF  (WRK-NYUKHNKBN(IDX1) =   79 OR 88  )
                        OR  (WRK-NYUINKHN(IDX1)  =  "190799510")
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        ELSE
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                        END-IF
                      ELSE
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-IF
                   WHEN   "9018"
                      MOVE   "190161570"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 11
                    EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                     WHEN   099  THRU  100
                      MOVE   "190142190"    TO   WRK-GAIHAKU2(IDX1)
                     WHEN   OTHER
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                    END-EVALUATE
                END-IF
             WHEN     9
             WHEN    13
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      IF   LNK-DPCNYUIN-SANTEIYM  >=  "201404"
                        IF  (WRK-NYUKHNKBN(IDX1) =   79 OR 88  )
                        OR  (WRK-NYUINKHN(IDX1)  =  "190799510")
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        ELSE
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                        END-IF
                      ELSE
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-IF
                   WHEN   "9018"
                      MOVE   "190161770"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 13
                    EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                     WHEN   099  THRU  100
                      MOVE   "190142190"    TO   WRK-GAIHAKU2(IDX1)
                     WHEN   OTHER
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                    END-EVALUATE
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸º»»¥³¡¼¥É¼èÆÀ£²  201604
      *****************************************************************
       2101-GAIHAKU-GENSAN2-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN     9
                        WHEN    42   THRU   44
                        WHEN    47
                        WHEN   166   THRU  167
                        WHEN   197   THRU  198
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198570"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 11
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                END-IF
             WHEN     9
             WHEN    13
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN     9
                        WHEN    42   THRU   44
                        WHEN    47
                        WHEN   166   THRU  167
                        WHEN   197   THRU  198
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198670"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 13
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸º»»¥³¡¼¥É¼èÆÀ£³  201804
      *****************************************************************
       2101-GAIHAKU-GENSAN3-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN    27
                        WHEN    40
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198570"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 11
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                END-IF
             WHEN     9
             WHEN    13
             WHEN    17
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN    27
                        WHEN    40
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198670"    TO   WRK-GAIHAKU(IDX1)
                END-EVALUATE
                IF   NACCT-DAY(IDX1) = 13
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                END-IF
                IF   NACCT-DAY(IDX1) = 17
                      MOVE   "190213790"    TO   WRK-GAIHAKU2(IDX1)
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       210-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÊÝ¸±È½Äê
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE                       HKNCOMBI-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM TO  COMB-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID  TO   COMB-PTID
           PERFORM   VARYING   IDX   FROM  1  BY  1
                     UNTIL     (WRK-HKNCOMBI-DAY(IDX)  NOT =  ZERO) OR
                               (IDX     >   31)
               IF   WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                  MOVE    WRK-HKNCOMBI-DAY(IDX)
                                       TO   COMB-HKNCOMBINUM
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    LNK-DPCNYUIN-HOSPNUM
                                       TO  AGECHK-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM
                                       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC           =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI        =   ZERO)    AND
                  (COMB-KOH1HKNNUM     =   "027"))   OR
                 ( AGECHK-NENREI      >=    75)      OR
                 ((AGECHK-NENREI      >=    70)     AND
                  (PTINF-BIRTHDAY     <=   "19320930"))
               MOVE    1               TO  FLG-RJN
           END-IF
      *
           .
       210-RJN-TAISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF     (IDX2                   =     1  )
           AND    (WRK-GAIHAKU(IDX1)     NOT =   SPACE)
               MOVE    SPACE             TO  WRK-GAIHAKU-SRYCD
               IF    LNK-DPCNYUIN-SANTEIYM   <=  "201203"
                  PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               END-IF
               IF    WRK-GAIHAKU-SRYCD   NOT =  SPACE
                  MOVE    WRK-GAIHAKU-SRYCD
                                         TO  TNS-SRYCD
               ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                         TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                         TO  TNS-SRYCD
           END-IF
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  TNS-HOSPNUM
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                  TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD         TO  TNS-YUKOEDYMD
           MOVE    TNS-SRYCD             TO  WRK-TENSU-CD
           PERFORM 910-TENSUWRK-CHK-SEC
           IF      FLG-TENSUWRK   =    ZERO
               PERFORM 910-TENSU-READ-SEC
               PERFORM 910-TTNS-HEN-SEC
           END-IF
      *
      *    ÅÀ¿ô»»Äê
           EVALUATE    TNS-TENSIKIBETU
      *        ÅÀ¿ô¡Ê¥×¥é¥¹¡Ë
               WHEN    3
                   ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
      *        ¡ó²Ã»»
               WHEN    5
                   COMPUTE    WRK-SO-TEN(IDX1)  =
                              WRK-SO-TEN(IDX1)  +
                             (WRK-SO-TEN(IDX1)  *
                              (TNS-TEN   /  100))
      *        ¡ó¸º»»
               WHEN    6
      *            AZU-MOD START 2015.6.10
                   IF (WRK-SENTEI-GAIGAKU(IDX1)  =   1)  AND
                      (TNS-SRYCD                 =  "190107290")
                      MOVE    100     TO    TNS-TEN
                   END-IF
                   IF   WRK-SO-SRYCD(IDX1 IDX2)   =   "190122890"
      *                Ã¼¿ô½èÍý¤¢¤ê
                       COMPUTE    WRK-SO-TEN(IDX1)  =
                                ( WRK-SO-TEN(IDX1)  -
                                 (WRK-SO-TEN(IDX1)  *
                                 (TNS-TEN   /  100)))    +  0.5
      *                ¾®¿ôÅÀ°Ê²¼ÀÚ¼Î¤Æ
                       MOVE   ZERO              TO  WRK-SO-TEN-SEISU
                       MOVE   WRK-SO-TEN(IDX1)  TO  WRK-SO-TEN-SEISU
                       MOVE   WRK-SO-TEN-SEISU  TO  WRK-SO-TEN(IDX1)
                   ELSE
      *                Ã¼¿ô½èÍý¤Ê¤·
                       COMPUTE    WRK-SO-TEN(IDX1)  =
                                  WRK-SO-TEN(IDX1)  -
                                 (WRK-SO-TEN(IDX1)  *
                                 (TNS-TEN   /  100))
                   END-IF
      *        ÅÀ¿ô¡Ê¥Þ¥¤¥Ê¥¹¡Ë
               WHEN    8
                   COMPUTE  WRK-SO-TEN(IDX1)  =
                            WRK-SO-TEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  WRK-SO-TEN(IDX1)
           END-EVALUATE
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×
      *    ¢¨ÅÀ¿ô¥Þ¥¹¥¿¤¬ÆÉ¤ß¹þ¤ß¤Ç¤­¤Ê¤«¤Ã¤¿¾ì¹ç¤Î°Û¾ï½ªÎ»²óÈò½èÍýÄÉ²Ã
           IF      TNS-SRYCD         =    SPACE
               MOVE    "000000000"   TO   TNS-SRYCD
           END-IF
           MOVE    TNS-SRYCD          TO  WRK-SRYCD-X
           COMPUTE WRK-SO-SRYCDTOTAL(IDX1)  =
                   WRK-SO-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  WRK-SO-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
      *
           EVALUATE  WRK-SO-SRYCD(IDX1 IDX2)
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (LNK-DPCNYUIN-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD   NOT =   SPACE
               MOVE   WRK-GAIHAKU-SRYCD   TO   WRK-SO-SRYCD(IDX1 IDX2)
           END-IF
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡¡¿©»öÎÅÍÜÈñ¿©¿ô¥Ù¡¼¥¹¡Ë
      *                  ¡ÊÊ¿À®£²£¸Ç¯£³·î¤Þ¤Ç¡Ë
      *****************************************************************
       2103-SHOKUJI-HENSHU3-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÃë¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                            TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  TSYRRK-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-DPCNYUIN-SANTEIYM AND
                    LNK-DPCNYUIN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-DPCNYUIN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
              AND (TSYRRK-NINSTYMD(7:2)      NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197000910"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *£¹£°Æü±Û¤¨¡ÊÄã½êÆÀ¾ðÊó¡Ë
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-DPCNYUIN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-DPCNYUIN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-DPCNYUIN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                    COMPUTE   IDX-TSY90  =  IDX1  +  31
                    IF    WRK-SKJ4-RYOYO(IDX-TSY90)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
                    COMPUTE   IDX-TSY90  =  IDX1  +  62
                    IF    WRK-SKJ4-RYOYO(IDX-TSY90)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  TNKRRK-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-DPCNYUIN-SANTEIYM AND
                    LNK-DPCNYUIN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-DPCNYUIN-SANTEIYM   =  TNKRRK-STYMD(1:6))
              AND (TNKRRK-STYMD(7:2)     NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    93
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001110"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                    END-IF
                END-PERFORM
              END-IF
           END-IF
      *
           MOVE     "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE     "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                          ORCSNYULIFE-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  LNK-NYULIFE-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  LNK-NYULIFE-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  LNK-NYULIFE-SANTEIYM
           CALL    "ORCSNYULIFE"     USING  ORCSNYULIFE-AREA
                                            SPA-AREA
           IF      LNK-NYULIFE-RC          NOT =  ZERO
               DISPLAY  "LNK-NYULIFE-RC = " LNK-NYULIFE-RC
           END-IF
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÃë¡Ë
               MOVE      ZERO          TO  IDX2
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX2)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX3)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJ-SRYCD(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»ö²ó¿ô
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        ¿©Æ²²Ã»»
               IF    WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE-SKJ-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
      **         IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
      **             MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
      **             COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
      **                     WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
      **         END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
      **         ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      **                               TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               EVALUATE    WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                   WHEN    1
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                   WHEN    2
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                   WHEN    3
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
               END-EVALUATE
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *          DISPLAY "WRK-SKJ7-SRYCDTOTAL =(IDX1) = "
      *                   WRK-SKJ7-SRYCDTOTAL (IDX1)
      *     END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *          DISPLAY "WRK-SKJ7  MATOME AFTER"
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
              IF    WRK-SKJ7-RYOYO(IDX1)   NOT =  SPACE
                IF (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001310")  AND
                   (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001710")
                    MOVE   IDX1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                        MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    MOVE  IDX1      TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
      **                  IF     IDX5    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                       MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-DPCNYUIN-MEISAISU(4 IDX3)
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£²£¸Ç¯£´·î°Ê¹ß¡Ë
      *                  ¡ÊÊ¿À®£²£¹Ç¯£¹·î¤Þ¤Ç¡Ë
      *                  ¡Ê¿©»öÎÅÍÜÈñ¤ÎÎ®Æ°¿©ÂÐ±þ¡Ë
      *                  ¡Ê¿©»öÉéÃ´¤ÎÆñÉÂÅù¡¢Àº¿À£±Ç¯Ä¶¤Î¿ø¤¨ÃÖ¤­ÂÐ±þ¡Ë
      *****************************************************************
       2103-SHOKUJI-HENSHU4-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÃë¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                            TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
           INITIALIZE                          WRK-SYOTOKU-SKJKBN-AREA
           INITIALIZE                          ORCSSYOTOKUAREA
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  LNK-SYOTOKU-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  LNK-SYOTOKU-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  LNK-SYOTOKU-SRYYM
           MOVE    "1"                     TO  LNK-SYOTOKU-SKJCHKFLG
           CALL    "ORCSSYOTOKU"    USING      ORCSSYOTOKUAREA
                                               SPA-AREA
           IF    LNK-SYOTOKU-RC      =         ZERO
                PERFORM    VARYING   IDX1   FROM   1   BY  1
                  UNTIL   (IDX1      >     31)
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX1)
                      COMPUTE    IDX2   =   IDX1   +   31
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX2)
                      COMPUTE    IDX2   =   IDX1   +   62
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX2)
                END-PERFORM
           ELSE
                DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
           END-IF
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý¡Ý¡Ý
      *
      *½êÆÀ¾ðÊó¤Î¥»¥Ã¥È
           PERFORM   VARYING   IDX1   FROM   1  BY  1
                     UNTIL     IDX1   >    93
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                   EVALUATE    WRK-SYOTOKU-SKJKBN (IDX1)
      *                Äã½êÆÀ£²
                       WHEN    "2"
                           MOVE   "197000910"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
      *                Äã½êÆÀ£²¡Ê£¹£°Æü±Û¤¨¡Ë
                       WHEN    "3"
                           MOVE   "197001010"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
      *                Äã½êÆÀ£±
                       WHEN    "1"
                       WHEN    "4"
                           MOVE   "197001110"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                        ORCSNYULIFE2-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  LNK-NYULIFE2-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  LNK-NYULIFE2-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"     USING  ORCSNYULIFE2-AREA
                                             SPA-AREA
           IF      LNK-NYULIFE2-RC          NOT =  ZERO
               DISPLAY  "LNK-NYULIFE2-RC = " LNK-NYULIFE2-RC
           END-IF
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥µ¥Ö
           INITIALIZE                          ORCSSKJKEEP-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  LNK-SKJKEEP-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  LNK-SKJKEEP-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  LNK-SKJKEEP-SANTEIYM
           CALL    "ORCSSKJKEEP"    USING      ORCSSKJKEEP-AREA
                                               SPA-AREA
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               IF   WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  =   SPACE
                     MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                     IF   WRK-SKJ4-RYOYO(IDX1)   =   CONS-SKJ-RYOYO-1-1
                                                 OR  CONS-SKJ-RYOYO-2-1
                         MOVE  WRK-SKJ4-RYOYO(IDX1)
                                         TO  WRK-SKJ7-RYOYO(IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                     END-IF
                     IF   WRK-SKJ4-RYOYO(IDX1)   =   CONS-SKJ-RYOYO-1-2
                                                 OR  CONS-SKJ-RYOYO-2-2
                         MOVE  WRK-SKJ4-RYOYO(IDX1)
                                         TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                          ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                     END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)
                                       TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                       MOVE   CONS-NANBYO-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                       MOVE   CONS-SEISIN-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÃë¡Ë
               MOVE      ZERO          TO  IDX2
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  =   SPACE
                     MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                         TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                     IF   WRK-SKJ4-RYOYO(IDX2)   =   CONS-SKJ-RYOYO-1-1
                                                 OR  CONS-SKJ-RYOYO-2-1
                         MOVE  WRK-SKJ4-RYOYO(IDX2)
                                         TO  WRK-SKJ7-RYOYO(IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                     END-IF
                     IF   WRK-SKJ4-RYOYO(IDX2)   =   CONS-SKJ-RYOYO-1-2
                                                 OR  CONS-SKJ-RYOYO-2-2
                         MOVE  WRK-SKJ4-RYOYO(IDX2)
                                         TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                     END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                         TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)
                                       TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                       MOVE   CONS-NANBYO-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                       MOVE   CONS-SEISIN-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  =   SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                         TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                      IF   WRK-SKJ4-RYOYO(IDX3)   =   CONS-SKJ-RYOYO-1-1
                                                  OR  CONS-SKJ-RYOYO-2-1
                          MOVE  WRK-SKJ4-RYOYO(IDX3)
                                          TO  WRK-SKJ7-RYOYO(IDX1)
                          ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                      END-IF
                      IF   WRK-SKJ4-RYOYO(IDX3)   =   CONS-SKJ-RYOYO-1-2
                                                  OR  CONS-SKJ-RYOYO-2-2
                          MOVE  WRK-SKJ4-RYOYO(IDX3)
                                          TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                          ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                      END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                         TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)
                                       TO  WRK-SKJ7-RYOYO-RYU(IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                       MOVE  "197001570"
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                       MOVE   CONS-NANBYO-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                       MOVE   CONS-SEISIN-SKJFTN
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(IDX1)
               END-IF
      *        ¿©»ö²ó¿ô
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        ¿©Æ²²Ã»»
               IF    WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF  (LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) = SPACE)
                         CONTINUE
                   ELSE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(IDX1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO-RYU(IDX1)   TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
      **         IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
      **             MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
      **             COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
      **                     WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
      **         END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
      **         ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      **                               TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               EVALUATE    WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                   WHEN    1
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                   WHEN    2
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                   WHEN    3
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
               END-EVALUATE
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *          DISPLAY "WRK-SKJ7-SRYCDTOTAL =(IDX1) = "
      *                   WRK-SKJ7-SRYCDTOTAL (IDX1)
      *     END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *          DISPLAY "WRK-SKJ7  MATOME AFTER"
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
              IF   (WRK-SKJ7-RYOYO(IDX1)       NOT =  SPACE)  OR
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT =  SPACE)
                IF (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001310")  AND
                   (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001710")  AND
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT = "197003510")
                    MOVE    IDX1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
      **                  IF     IDX4   =   1
                              MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003110"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   575     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1150     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1725     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003210"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   455     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   910     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1365     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                              IF  (LNK-DPCNYUIN-SANTEIYM
                                                        >= "201604") AND
                                  (LNK-DPCNYUIN-SANTEIYM
                                                        <= "201803")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  360  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  720  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1080  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
                              IF  (LNK-DPCNYUIN-SANTEIYM
                                                        >= "201804")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    MOVE   IDX1       TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
      **                  IF     IDX5    =   1
                            MOVE   "97"   TO
                            LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
      **                  IF     IDX5    =   1
                            MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003510"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   500     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1000     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1500     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                       MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                              IF   LNK-DPCNYUIN-SANTEIYM
                                                        >= "201604"
                                EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  360  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  720  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1080  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                END-EVALUATE
                              END-IF
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡¦°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-DPCNYUIN-MEISAISU(4 IDX3)
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£²£¹Ç¯£±£°·î°Ê¹ß¡Ë
      *                  ¡Ê¶­³¦ÁØÂÐ±þ¡Ë
      *                  ¡ÊÀ¸³èÎÅÍÜÉéÃ´¥³¡¼¥ÉÊ¬³äÂÐ±þ¡Ë
      *****************************************************************
       2103-SHOKUJI-HENSHU5-SEC        SECTION.
      **     DISPLAY "2103-SHOKUJI-HENSHU5-SEC"
      **     DISPLAY "LNK-DPCNYUIN-SET-HKNCOMBI(1) = "
      **              LNK-DPCNYUIN-SET-HKNCOMBI(1)
      **     DISPLAY "LNK-DPCNYUIN-SET-HKNCOMBI(2) = "
      **              LNK-DPCNYUIN-SET-HKNCOMBI(2)
      **     DISPLAY "LNK-DPCNYUIN-SET-HKNCOMBI(3) = "
      **              LNK-DPCNYUIN-SET-HKNCOMBI(3)
      *
      *¿©»ö¤ÎÊÝ¸±ÁÈ¹ç¤»¤¬¥»¥Ã¥È¤µ¤ì¤Æ¤¤¤ëÆü¤ÏÆþ±¡°·¤¤¤È¤¹¤ë
           PERFORM   VARYING   IDX1   FROM  1   BY  1
                     UNTIL     IDX1   >    31
                IF   (WRK-SKJ-HKNCOMBI-ASA(IDX1) NOT = ZERO) OR
                     (WRK-SKJ-HKNCOMBI-HIRU(IDX1) NOT = ZERO) OR
                     (WRK-SKJ-HKNCOMBI-YORU(IDX1) NOT = ZERO)
                     MOVE   "XXXXXXXXX"   TO  WRK-NYUINKHN(IDX1)
                END-IF
           END-PERFORM
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key30"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
      *
                   MOVE    NACCT-DAY(IDX1) TO  WRK-SKJ-ASA-ARI(IDX1)
      *
                   IF    (WRK-SKJ-HKNCOMBI-ASA(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-SKJ-HKNCOMBI-ASA(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX1)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX1)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX1)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key30"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÃë¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key31"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key31"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX2   =    IDX1   +   31
      *
                   MOVE    NACCT-DAY(IDX1) TO  WRK-SKJ-HIRU-ARI(IDX1)
      *
                   IF    (WRK-SKJ-HKNCOMBI-HIRU(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-SKJ-HKNCOMBI-HIRU(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX2)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX2)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX2)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX2)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX2)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX2)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX2)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  NACCT-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key32"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   COMPUTE   IDX3   =    IDX1   +   62
                   IF    (WRK-SKJ-HKNCOMBI-YORU(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                         (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                       MOVE   WRK-SKJ-HKNCOMBI-YORU(IDX1)
                                       TO  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ5-HKNCOMBI-DAY(IDX3)
      *                                     WRK-SKJ6-HKNCOMBI-DAY(IDX3)
                                        WRK-SKJFTN2-HKNCOMBI-DAY(IDX3)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                            TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                        ÆÃÊÌ¿©²Ã»»¤ÏÆþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç
      *                        ¤Î¤ß¥»¥Ã¥È
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                            "197000110"
                                   MOVE   "197000470"
                                           TO  WRK-SKJ5-TOKU-SHOKU(IDX3)
                               END-IF
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                           TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
      *                    Î®Æ°¿©
                           WHEN   3
                               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£±¡Ë
                                   WHEN   "197000110"
                                      MOVE   "197003110"
                                              TO  WRK-SKJ4-RYOYO(IDX3)
      *                            Æþ±¡»þ¿©»öÎÅÍÜÈñ¡Ê£²¡Ë
                                   WHEN   "197000710"
                                      MOVE   "197003210"
                                              TO  WRK-SKJ4-RYOYO(IDX3)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN2-SRYCD(IDX3)
                               END-IF
                       END-EVALUATE
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *     DISPLAY "---------SKJ4 SKJ2 CLEAR MAE -----------------"
      *     PERFORM    VARYING  IDX1   FROM 1 BY 1
      *       UNTIL    IDX1 > 93
      *           DISPLAY  "IDX1 = " IDX1
      *                    " WRK-SKJ4-HKNCOMBI-DAY = "
      *                    WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                    " WRK-SKJFTN2-HKNCOMBI-DAY = "
      *                    WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
      *     END-PERFORM
      *
      *    °ú¿ô¤Ç¥»¥Ã¥È¤µ¤ì¤¿ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ°Ê³°¤ÏSKJ4,SKJ5,SKJFTN2
      *    ÎÎ°è¤Î¥»¥Ã¥ÈÆâÍÆ¤ò¥¯¥ê¥¢¤¹¤ë
           PERFORM    VARYING  IDX1   FROM 1 BY 1
             UNTIL    IDX1 > 93
                 IF   WRK-SKJ4-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
                   IF   WRK-SKJ4-HKNCOMBI-DAY(IDX1)  =
                                  LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(10)
                       CONTINUE
                   ELSE
                       MOVE    SPACE   TO   WRK-SKJ4-TBL(IDX1)
                       INITIALIZE           WRK-SKJ4-TBL(IDX1)
                   END-IF
                 END-IF
                 IF   WRK-SKJ5-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
                   IF   WRK-SKJ5-HKNCOMBI-DAY(IDX1)  =
                                  LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(10)
                       CONTINUE
                   ELSE
                       MOVE    SPACE   TO   WRK-SKJ5-TBL(IDX1)
                       INITIALIZE           WRK-SKJ5-TBL(IDX1)
                   END-IF
                 END-IF
                 IF   WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
                   IF   WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)  =
                                  LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(10)
                       CONTINUE
                   ELSE
                       MOVE    SPACE   TO   WRK-SKJFTN2-TBL(IDX1)
                       INITIALIZE           WRK-SKJFTN2-TBL(IDX1)
                   END-IF
                 END-IF
           END-PERFORM
      *     DISPLAY "---------SKJ4 SKJ2 CLEAR ATO -----------------"
      *     PERFORM    VARYING  IDX1   FROM 1 BY 1
      *       UNTIL    IDX1 > 93
      *           DISPLAY  "IDX1 = " IDX1
      *                    " WRK-SKJ4-HKNCOMBI-DAY = "
      *                    WRK-SKJ4-HKNCOMBI-DAY(IDX1)
      *                    " WRK-SKJFTN2-HKNCOMBI-DAY = "
      *                    WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)
      *     END-PERFORM
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *    ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
                IF   LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "1" 
                   IF  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1) =
                                  LNK-DPCNYUIN-SET-HKNCOMBI(1)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(2)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(3)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(4)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(5)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(6)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(7)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(8)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(9)  OR
                                  LNK-DPCNYUIN-SET-HKNCOMBI(10)
                      IF   WRK-SKJ-HKNCOMBI-ASA(IDX1)  NOT =  ZERO
                           CONTINUE
                      END-IF
                      IF   WRK-SKJ-HKNCOMBI-HIRU(IDX1)  NOT =  ZERO
                        IF (WRK-SKJ-HKNCOMBI-ASA(IDX1)  =  ZERO  AND
                            WRK-SKJ-ASA-ARI(IDX1)       NOT = ZERO)
                                   MOVE   "1"  TO
                                        WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                        END-IF
                      END-IF
                      IF   WRK-SKJ-HKNCOMBI-YORU(IDX1)  NOT =  ZERO
                        IF (WRK-SKJ-HKNCOMBI-ASA(IDX1)  =  ZERO  AND
                            WRK-SKJ-ASA-ARI(IDX1)       NOT = ZERO) OR
                           (WRK-SKJ-HKNCOMBI-HIRU(IDX1)  =  ZERO  AND
                            WRK-SKJ-HIRU-ARI(IDX1)      NOT = ZERO)
                                   MOVE   "1"  TO
                                        WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                        END-IF
                      END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *    ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
                IF   LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  =  "1"
      *            Ä«¿©¤òÂ¾ÊÝ¸±¤Ç»»Äê
                   IF  WRK-SKJ-ASA-ARI(IDX1)  NOT =  ZERO
                      IF   WRK-SKJ-HKNCOMBI-ASA(IDX1)   =  ZERO
                          MOVE   "1"  TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                      END-IF
                   END-IF
      *            Ä«¿©Ëô¤ÏÃë¿©¤òÂ¾ÊÝ¸±¤Ç»»Äê
                   IF (WRK-SKJ-ASA-ARI(IDX1)   NOT =  ZERO)  OR
                      (WRK-SKJ-HIRU-ARI(IDX1)  NOT =  ZERO)
                      IF  (WRK-SKJ-HKNCOMBI-ASA(IDX1)   =  ZERO) AND
                          (WRK-SKJ-HKNCOMBI-HIRU(IDX1)  =  ZERO)
                          MOVE   "1"  TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                      END-IF
                   END-IF
               END-IF
           END-PERFORM
      **     PERFORM      VARYING   IDX1   FROM   1  BY  1
      **       UNTIL      IDX1     >    31
      **        DISPLAY "IDX1 = " IDX1 " WRK-TAHOKEN-SHOKUDOU-FLG(IDX1) ="
      **                WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
      **     END-PERFORM
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ÎºÆÆþ±¡Ê¬¿©»ö»»Äê¤Ë¤Ä¤¤¤Æ¡¢ÆþÂà±¡Æü¤òÊÑ¹¹¤¹¤ë°Ù
      *    ¤Î¶èÊ¬¤ò¥»¥Ã¥È
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
                MOVE    SPACE       TO   WRK-SAINYUINKBN-ASA(IDX1)
                MOVE    SPACE       TO   WRK-SAINYUINKBN-HIRU(IDX1)
                MOVE    SPACE       TO   WRK-SAINYUINKBN-YORU(IDX1)
                IF   LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  =  "1"
      *            Ä«¿©
                   IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)   =  "1"
                       MOVE   "1"  TO  WRK-SAINYUINKBN-ASA(IDX1)
                   END-IF
      *            Ãë¿©
                   IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)   =  "1" OR
                                                                "2"
                       MOVE   "1"  TO  WRK-SAINYUINKBN-HIRU(IDX1)
                   END-IF
      *            Í¼¿©
                   IF   LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1)   =  "1" OR
                                                                "2" OR
                                                                "3"
                       MOVE   "1"  TO  WRK-SAINYUINKBN-YORU(IDX1)
                   END-IF
               END-IF
      **         DISPLAY "IDX1 = " IDX1
      **         DISPLAY "ASA  = " WRK-SKJ-HKNCOMBI-ASA(IDX1)
      **         DISPLAY "HIRU = " WRK-SKJ-HKNCOMBI-HIRU(IDX1)
      **         DISPLAY "YORU = " WRK-SKJ-HKNCOMBI-YORU(IDX1)
      **         DISPLAY "DOUJITSU-KBN = " LNK-NYUINDAY-DOUJITSU-KBN(IDX1)
      **                 " LNK-SKJHKN = "LNK-SKJHKN-HKNCOMBINUM(IDX1 2)
      **             " WRK-SAINYUINKBN-ASA = " WRK-SAINYUINKBN-ASA(IDX1)
      **             " WRK-SAINYUINKBN-HIRU = " WRK-SAINYUINKBN-HIRU(IDX1)
      **             " WRK-SAINYUINKBN-YORU = " WRK-SAINYUINKBN-YORU(IDX1)
           END-PERFORM
      *
      *    ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Ë¤è¤ë¿©Æ²²Ã»»ÎÎ°è¤Î¥¯¥ê¥¢
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
      **       DISPLAY "IDX1 = " IDX1 " WRK-TAHOKEN-SHOKUDOU-FLG = "
      **                                WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
      **                       " SHOKUDOU = "
      **                                WRK-SKJ6-SHOKUDOU(IDX1)
             IF    WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)  =   "1"
                 MOVE    SPACE     TO  WRK-SKJ6-SHOKUDOU(IDX1)
             END-IF
           END-PERFORM
      *
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
           INITIALIZE                          WRK-SYOTOKU-SKJKBN-AREA
           INITIALIZE                          ORCSSYOTOKUAREA
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  LNK-SYOTOKU-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  LNK-SYOTOKU-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  LNK-SYOTOKU-SRYYM
           MOVE    "1"                     TO  LNK-SYOTOKU-SKJCHKFLG
           CALL    "ORCSSYOTOKU"    USING      ORCSSYOTOKUAREA
                                               SPA-AREA
           IF    LNK-SYOTOKU-RC      =         ZERO
                PERFORM    VARYING   IDX1   FROM   1   BY  1
                  UNTIL   (IDX1      >     31)
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX1)
                      COMPUTE    IDX2   =   IDX1   +   31
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX2)
                      COMPUTE    IDX2   =   IDX1   +   62
                      MOVE    LNK-SYOTOKU-SKJKBN(IDX1)
                                    TO  WRK-SYOTOKU-SKJKBN (IDX2)
                END-PERFORM
           ELSE
                DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
           END-IF
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý¡Ý¡Ý
      *
      *½êÆÀ¾ðÊó¤Î¥»¥Ã¥È
           PERFORM   VARYING   IDX1   FROM   1  BY  1
                     UNTIL     IDX1   >    93
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                   EVALUATE    WRK-SYOTOKU-SKJKBN (IDX1)
      *                Äã½êÆÀ£²
                       WHEN    "2"
                           MOVE   "197000910"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
      *                Äã½êÆÀ£²¡Ê£¹£°Æü±Û¤¨¡Ë
                       WHEN    "3"
                           MOVE   "197001010"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
      *                Äã½êÆÀ£±
                       WHEN    "1"
                       WHEN    "4"
                           MOVE   "197001110"
                                         TO  WRK-SKJFTN2-SRYCD(IDX1)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                        ORCSNYULIFE2-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM  TO  LNK-NYULIFE2-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID     TO  LNK-NYULIFE2-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"     USING  ORCSNYULIFE2-AREA
                                             SPA-AREA
           IF      LNK-NYULIFE2-RC          NOT =  ZERO
               DISPLAY  "LNK-NYULIFE2-RC = " LNK-NYULIFE2-RC
           END-IF
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥µ¥Ö
           INITIALIZE                          ORCSSKJKEEP-AREA
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  LNK-SKJKEEP-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  LNK-SKJKEEP-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  LNK-SKJKEEP-SANTEIYM
           CALL    "ORCSSKJKEEP"    USING      ORCSSKJKEEP-AREA
                                               SPA-AREA
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
      *    ¢¨Åº»ú¤Î£³£²¤«¤é£¶£²¤ÏÆ±ÆüºÆÆþ±¡ÊÔ½¸ÍÑ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               MOVE     ZERO          TO  SET-IDX1
      **         DISPLAY "ASA"
      **         DISPLAY "WRK-SKJ4-RYOYO(IDX1) = " WRK-SKJ4-RYOYO(IDX1)
               IF   WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»ö¤ÏÊÔ½¸Àè¤òÊÑ¹¹
                   MOVE  IDX1          TO  SET-IDX1
                   IF   WRK-SAINYUINKBN-ASA(IDX1)  =   "1"
                        COMPUTE  SET-IDX1  =   IDX1  +  31
                   ELSE
                        MOVE     IDX1      TO  SET-IDX1
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»öÈ½ÄêÍÑ
                   IF   WRK-SAINYUINKBN-ASA(IDX1)   =   "1"
                        MOVE    1   TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
                   IF   SET-IDX1     >=    32
                        MOVE    1   TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
      *
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  =   SPACE
                     MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                    TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                     IF   WRK-SKJ4-RYOYO(IDX1)   =   CONS-SKJ-RYOYO-1-1
                                                 OR  CONS-SKJ-RYOYO-2-1
                         MOVE  WRK-SKJ4-RYOYO(IDX1)
                                         TO  WRK-SKJ7-RYOYO(SET-IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                     END-IF
                     IF   WRK-SKJ4-RYOYO(IDX1)   =   CONS-SKJ-RYOYO-1-2
                                                 OR  CONS-SKJ-RYOYO-2-2
                         MOVE  WRK-SKJ4-RYOYO(IDX1)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                          ADD   1    TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                     END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)
                                       TO  WRK-SKJ7-RYOYO(SET-IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                           ADD   1   TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                       MOVE  "197001570"
                                    TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
                   END-IF
                   ADD   1          TO  WRK-SKJ7-TOKU-SHOKU-SU(SET-IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SYOTOKU-SKJKBN(IDX1)  =  SPACE
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                           MOVE   CONS-NANBYO-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                           MOVE   CONS-SEISIN-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 1)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(SET-IDX1)
               END-IF
      *        ¿©Æ²²Ã»»
      **         DISPLAY "=====  SHOUDOU ASA ========"
      **         DISPLAY "IDX1 = " IDX1
      **         DISPLAY "WRK-SKJ6-SHOKUDOU(IDX1) = " 
      **                  WRK-SKJ6-SHOKUDOU(IDX1)
      **         DISPLAY "SET-IDX1 = " SET-IDX1
               IF    (WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE)  AND
                     (SET-IDX1     NOT =   ZERO)
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF  (LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) = SPACE)
                         CONTINUE
                   ELSE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
                   END-IF
                   MOVE    SPACE       TO  WRK-SKJ6-SHOKUDOU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÃë¡Ë
               MOVE      ZERO          TO  IDX2
               MOVE      ZERO          TO  SET-IDX1
               COMPUTE   IDX2     =    IDX1    +   31
      **         DISPLAY "HIRU"
      **         DISPLAY "WRK-SKJ4-RYOYO(IDX2) = " WRK-SKJ4-RYOYO(IDX2)
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»ö¤ÏÊÔ½¸Àè¤òÊÑ¹¹
                   MOVE  IDX1          TO  SET-IDX1
                   IF   WRK-SAINYUINKBN-HIRU(IDX1)  =   "1"
                        COMPUTE  SET-IDX1  =   IDX1  +  31
                   ELSE
                        MOVE     IDX1      TO  SET-IDX1
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»öÈ½ÄêÍÑ
                   IF   WRK-SAINYUINKBN-HIRU(IDX1)   =   "1"
                        MOVE    1   TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
                   IF   SET-IDX1     >=    32
                        MOVE    1    TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
      *
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  =   SPACE
                     MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                     TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                     IF   WRK-SKJ4-RYOYO(IDX2)   =   CONS-SKJ-RYOYO-1-1
                                                 OR  CONS-SKJ-RYOYO-2-1
                         MOVE  WRK-SKJ4-RYOYO(IDX2)
                                         TO  WRK-SKJ7-RYOYO(SET-IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                     END-IF
                     IF   WRK-SKJ4-RYOYO(IDX2)   =   CONS-SKJ-RYOYO-1-2
                                                 OR  CONS-SKJ-RYOYO-2-2
                         MOVE  WRK-SKJ4-RYOYO(IDX2)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                         ADD   1     TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                     END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX2)
                                     TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)
                                       TO  WRK-SKJ7-RYOYO(SET-IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                           ADD   1   TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                       MOVE  "197001570"
                                    TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
                   END-IF
                   ADD   1          TO  WRK-SKJ7-TOKU-SHOKU-SU(SET-IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SYOTOKU-SKJKBN(IDX1)  =  SPACE
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                           MOVE   CONS-NANBYO-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                           MOVE   CONS-SEISIN-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 2)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 2)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(SET-IDX1)
               END-IF
      *        ¿©Æ²²Ã»»
      **         DISPLAY "=====  SHOUDOU HIRU ========"
      **         DISPLAY "IDX1 = " IDX1
      **         DISPLAY "WRK-SKJ6-SHOKUDOU(IDX1) = " 
      **                  WRK-SKJ6-SHOKUDOU(IDX1)
      **         DISPLAY "SET-IDX1 = " SET-IDX1
               IF    (WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE)  AND
                     (SET-IDX1     NOT =   ZERO)
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF  (LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) = SPACE)
                         CONTINUE
                   ELSE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
                   END-IF
                   MOVE    SPACE       TO  WRK-SKJ6-SHOKUDOU(IDX1)
               END-IF
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               MOVE      ZERO          TO  SET-IDX1
               COMPUTE   IDX3     =    IDX1    +   62
      **         DISPLAY "YORU"
      **         DISPLAY "WRK-SKJ4-RYOYO(IDX3) = " WRK-SKJ4-RYOYO(IDX3)
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»ö¤ÏÊÔ½¸Àè¤òÊÑ¹¹
                   MOVE  IDX1          TO  SET-IDX1
                   IF   WRK-SAINYUINKBN-YORU(IDX1)  =   "1"
                        COMPUTE  SET-IDX1  =   IDX1  +  31
                   ELSE
                        MOVE     IDX1      TO  SET-IDX1
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡¤Î¿©»öÈ½ÄêÍÑ
                   IF   WRK-SAINYUINKBN-YORU(IDX1)   =   "1"
                        MOVE    1   TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
                   IF   SET-IDX1     >=    32
                        MOVE    1   TO   WRK-SKJ7-SAINYUINKBN(SET-IDX1)
                   END-IF
      *
                   IF  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  =   SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                     TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                      IF   WRK-SKJ4-RYOYO(IDX3)   =   CONS-SKJ-RYOYO-1-1
                                                  OR  CONS-SKJ-RYOYO-2-1
                          MOVE  WRK-SKJ4-RYOYO(IDX3)
                                          TO  WRK-SKJ7-RYOYO(SET-IDX1)
                          ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                      END-IF
                      IF   WRK-SKJ4-RYOYO(IDX3)   =   CONS-SKJ-RYOYO-1-2
                                                  OR  CONS-SKJ-RYOYO-2-2
                          MOVE  WRK-SKJ4-RYOYO(IDX3)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                          ADD   1    TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                      END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                      MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX3)
                                     TO  WRK-SKJ7-HKNCOMBI-DAY(SET-IDX1)
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) =
                                               CONS-SKJSEI-RYOYO-1-1 OR
                                               CONS-SKJSEI-RYOYO-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)
                                       TO  WRK-SKJ7-RYOYO(SET-IDX1)
                           ADD   1     TO  WRK-SKJ7-RYOYO-SU(SET-IDX1)
                      END-IF
                      IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) =
                                               CONS-SKJSEI-RYOYO-1-2
                           MOVE  LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)
                                     TO  WRK-SKJ7-RYOYO-RYU(SET-IDX1)
                           ADD   1   TO  WRK-SKJ7-RYOYO-RYU-SU(SET-IDX1)
                      END-IF
                   END-IF
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                       MOVE  "197001570"
                                    TO  WRK-SKJ7-TOKU-SHOKU(SET-IDX1)
                   END-IF
                   ADD   1          TO  WRK-SKJ7-TOKU-SHOKU-SU(SET-IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SYOTOKU-SKJKBN(IDX1)  =  SPACE
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                           MOVE   CONS-NANBYO-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                           MOVE   CONS-SEISIN-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                       END-IF
                   END-IF
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF   LNK-NYULIFE2-SKJ-SRYCD(IDX1 3)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-SKJFTN-SRYCD(IDX1 3)
                                       TO  WRK-SKJ7-SKJFTN(SET-IDX1)
                   END-IF
                   ADD   1             TO  WRK-SKJ7-SKJFTN-SU(SET-IDX1)
               END-IF
      *        ¿©»ö²ó¿ô
      **         IF    (WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE)   AND
      **               (WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE)
      **             MOVE      1         TO   WRK-SKJ7-KAISU(IDX1)
      **         END-IF
      *        ¿©Æ²²Ã»»
      **         DISPLAY "=====  SHOUDOU YORU ========"
      **         DISPLAY "IDX1 = " IDX1
      **         DISPLAY "WRK-SKJ6-SHOKUDOU(IDX1) = " 
      **                  WRK-SKJ6-SHOKUDOU(IDX1)
      **         DISPLAY "SET-IDX1 = " SET-IDX1
               IF    (WRK-SKJ6-SHOKUDOU(IDX1)  NOT =   SPACE)  AND
                     (SET-IDX1     NOT =   ZERO)
                   MOVE  WRK-SKJ6-SHOKUDOU(IDX1)
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
      *            Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤ÎÃÖ´¹¤¨
                   IF  (LNK-NYULIFE2-SKJ-SRYCD(IDX1 1) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 2) = SPACE) AND
                       (LNK-NYULIFE2-SKJ-SRYCD(IDX1 3) = SPACE)
                         CONTINUE
                   ELSE
                       MOVE  "197001670"
                                       TO  WRK-SKJ7-SHOKUDOU(SET-IDX1)
                   END-IF
                   MOVE    SPACE       TO  WRK-SKJ6-SHOKUDOU(IDX1)
               END-IF
           END-PERFORM
      *
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   62
               IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO(IDX1)       TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-RYOYO-RYU(IDX1)   TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-SHOKUDOU(IDX1)    TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
      **         IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
      **             MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
      **             COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
      **                     WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
      **         END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
      **         ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      **                               TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               EVALUATE    WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                   WHEN    1
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                   WHEN    2
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                   WHEN    3
                       ADD  1   TO   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
               END-EVALUATE
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               IF    IDX1     >=     32
                   COMPUTE   IDX2    =    IDX1   -  31
               ELSE
                   MOVE      IDX1    TO   IDX2
               END-IF
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   62
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *          DISPLAY "WRK-SKJ7-SRYCDTOTAL =(IDX1) = "
      *                   WRK-SKJ7-SRYCDTOTAL (IDX1)
      *          DISPLAY "WRK-SKJ7-SAINYUINKBN =(IDX1) = "
      *                   WRK-SKJ7-SAINYUINKBN (IDX1)
      *     END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     62)
      *           DISPLAY "LNK-DPCNYUIN-SET"
      *           DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7  MATOME AFTER"
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-SHOKUDOU(IDX1) = "
      *                   WRK-SKJ7-SHOKUDOU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-SU(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
      *          DISPLAY "WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) = "
      *                   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
              IF   (WRK-SKJ7-RYOYO(IDX1)       NOT =  SPACE)  OR
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT =  SPACE)
                IF (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001310")  AND
                   (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001710")  AND
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT = "197003510")
      *-----------------------------------------------
      **              MOVE    IDX1     TO    IDX2
                    ADD     1        TO    IDX2
      *-----------------------------------------------
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    END-IF
      *             Æ±ÆüºÆÆþ±¡È½Äê¹àÌÜ
                    IF      WRK-SKJ7-SAINYUINKBN(IDX1)    =   1
                        MOVE  "1"  TO  LNK-DPCNYUIN-SAINYUINKBN
                                                        (3 IDX2 IDX4)
                    END-IF
      *             ÊÖµÑÃÍ¤ËÂÐ¾ÝÆüÉÕÄÉ²Ã
                    IF       IDX1    >=   32
                         COMPUTE  LNK-DPCNYUIN-DD(3 IDX2)  =
                                  IDX1   -  31
                    ELSE
                        MOVE  IDX1    TO   LNK-DPCNYUIN-DD(3 IDX2)
                    END-IF
      *
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
      **                  IF     IDX4   =   1
                              MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003110"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   575     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1150     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1725     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003210"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   455     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   910     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1365     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *                 Ê¿À®£³£°Ç¯£´·î²þÀµ
                        IF    LNK-DPCNYUIN-SANTEIYM  >=  "201804"
                          EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   460     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   920     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1380     TO  
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                          END-EVALUATE
                        END-IF
                    END-EVALUATE
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                              IF  (LNK-DPCNYUIN-SANTEIYM
                                                        >= "201604") AND
                                  (LNK-DPCNYUIN-SANTEIYM
                                                        <= "201803")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  360  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  720  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1080  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
                              IF  (LNK-DPCNYUIN-SANTEIYM
                                                        >= "201804")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
      *-----------------------------------------------
      **              MOVE   IDX1       TO    IDX3
                    ADD     1         TO    IDX3
      *              DISPLAY "IDX3 = " IDX3
      *-----------------------------------------------
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
      **                  IF     IDX5    =   1
                            MOVE   "97"   TO
                            LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                    END-IF
      *             Æ±ÆüºÆÆþ±¡È½Äê¹àÌÜ
                    IF      WRK-SKJ7-SAINYUINKBN(IDX1)    =   1
                        MOVE  "1"  TO  LNK-DPCNYUIN-SAINYUINKBN
                                                        (4 IDX3 IDX5)
                    END-IF
      *             ÊÖµÑÃÍ¤ËÂÐ¾ÝÆüÉÕÄÉ²Ã
                    IF       IDX1    >=   32
                         COMPUTE  LNK-DPCNYUIN-DD(4 IDX3)  =
                                  IDX1   -  31
                    ELSE
                        MOVE  IDX1    TO   LNK-DPCNYUIN-DD(4 IDX3)
                    END-IF
      *
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
      **                  IF     IDX5    =   1
                            MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
      **                  END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003510"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   500     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1000     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1500     TO  
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(4 IDX3 IDX5)
                       MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-DPCNYUIN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                              IF   LNK-DPCNYUIN-SANTEIYM
                                                        >= "201604"
                                EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  360  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  720  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1080  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                END-EVALUATE
                              END-IF
      *             ¡ÊÊ¿À®£³£°Ç¯²þÀµ¡Ë
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197005110"
                              EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                 WHEN   1
                                    MOVE  460  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                 WHEN   2
                                    MOVE  920  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                 WHEN   3
                                    MOVE 1380  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                              END-EVALUATE
      *             ¡ÊÊ¿À®£³£°Ç¯²þÀµ¡Ë
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197005210"
                              EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                 WHEN   1
                                    MOVE  420  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                 WHEN   2
                                    MOVE  840  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                 WHEN   3
                                    MOVE 1260  TO 
                                       LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                              END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡¦°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¤³¤³¤«¤éÊ¿À®£²£¹Ç¯£±£°·î¥³¡¼¥ÉÄÉ²ÃÊ¬
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã¡Ë
                            WHEN   "197003910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã¡Ë
                            WHEN   "197004010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197004110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡Ë
                            WHEN   "197004610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197004910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197005010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-DPCNYUIN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-DPCNYUIN-MEISAISU(4 IDX3)
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU5-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¡ÊÊ¿À®£±£¸Ç¯£±£°·î¤«¤éÊ¿À®£²£¸Ç¯£³·îËø¡Ë
      *****************************************************************
       2104-KANKYO-HENSHU-SEC        SECTION.
      *
           PERFORM   VARYING   IDX1    FROM   1   BY   1
             UNTIL   IDX1      >       31
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)            AND
                     (LNK-NYULIFE-KAN-SRYCD(IDX1)    NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1) NOT =  SPACE)              OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                     (WRK-GAIHAKU-KBN(IDX1)  >=    4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   LNK-NYULIFE-KAN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-RYOYO(IDX1)
                   MOVE   LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-FTN(IDX1)
                   IF   LNK-NYULIFE-KAN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-KAN-SRYCD(IDX1) TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
                   IF   LNK-NYULIFE-KANFTN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                                                        TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    MOVE   IDX1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                         MOVE   320     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(5 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2104-KANKYO-HENSHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¸Ç¯£´·î¤«¤éÊ¿À®£²£¹Ç¯£¹·îËø¡Ë
      *    ¥í¥¸¥Ã¥¯¤Ï 2104-KANKYO-HENSHU-SEC ¤ÈÆ±¤¸¤Ç¡¢ORCSNYULIFE ¤¬
      *    ORCSNYULIFE2 ¤ËÊÑ¹¹¤Î¤ß
      *****************************************************************
       2104-KANKYO-HENSHU2-SEC        SECTION.
      *
           PERFORM   VARYING   IDX1    FROM   1   BY   1
             UNTIL   IDX1      >       31
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)            AND
                     (LNK-NYULIFE2-KAN-SRYCD(IDX1)   NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1) NOT =  SPACE)              OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                     (WRK-GAIHAKU-KBN(IDX1)  >=    4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   LNK-NYULIFE2-KAN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-RYOYO(IDX1)
                   MOVE   LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-FTN(IDX1)
                   IF   LNK-NYULIFE2-KAN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-KAN-SRYCD(IDX1) TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
                   IF   LNK-NYULIFE2-KANFTN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                                                        TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    MOVE   IDX1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                         MOVE   320     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(5 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2104-KANKYO-HENSHU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¡ÊÊ¿À®£²£¹Ç¯£±£°·î°Ê¹ß¡Ë
      *****************************************************************
       2104-KANKYO-HENSHU3-SEC        SECTION.
      *
           PERFORM   VARYING   IDX1    FROM   1   BY   1
             UNTIL   IDX1      >       31
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)            AND
                     (LNK-NYULIFE2-KAN-SRYCD(IDX1)   NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1) NOT =  SPACE)              OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR
                     (WRK-GAIHAKU-KBN(IDX1)  >=    4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   LNK-NYULIFE2-KAN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-RYOYO(IDX1)
                   MOVE   LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                                     TO  WRK-KANKYO-FTN(IDX1)
                   IF   LNK-NYULIFE2-KAN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-KAN-SRYCD(IDX1) TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
                   IF   LNK-NYULIFE2-KANFTN-SRYCD(IDX1)  NOT =  SPACE
                       MOVE  LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                                                        TO WRK-SRYCD-X
                       COMPUTE WRK-KANKYO-SRYCDTOTAL(IDX1)  =
                           WRK-KANKYO-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    MOVE   IDX1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-DPCNYUIN-HKNCOMBI(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
      **                  IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
      **                  END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)   =  "197001910"
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                         MOVE   370     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    END-IF
                    IF   WRK-KANKYO-FTN(IDX1)   =  "197003810"
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-DPCNYUIN-RECSKB(5 IDX2 IDX4)
                         MOVE   "97"   TO
                              LNK-DPCNYUIN-SRYSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-DPCNYUIN-SRYCD(5 IDX2 IDX4)
                         MOVE   200     TO  
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
      *                 Ê¿À®£³£°Ç¯²þÀµ¡¡°åÎÅ¶èÊ¬£²¡¢£³¡Ê£±Æü£³£·£°±ß¡Ë
                        IF   LNK-DPCNYUIN-SANTEIYM >=  "201804"
                             MOVE  370       TO
                                      LNK-DPCNYUIN-TEN(5 IDX2 IDX4)
                        END-IF
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-DPCNYUIN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-DPCNYUIN-MEISAISU(5 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2104-KANKYO-HENSHU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÅïÈÖ¹æ¡ÊÉÂÅï¶èÊ¬¡Ë¤ÎÊÔ½¸½èÍý
      *****************************************************************
       2105-BTUNUM-HENSHU-SEC        SECTION.
      *
           MOVE    SPACE                   TO  TABLE-BTUKBN
           INITIALIZE                          TABLE-BTUKBN
           MOVE    SPACE                   TO  TABLE-BTUKBN2
           INITIALIZE                          TABLE-BTUKBN2
      *    ´µ¼ÔÆþ±¡ÍúÎò¤Î¼èÆÀ
           INITIALIZE                          PTNYUINRRK-REC
           MOVE    LNK-DPCNYUIN-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-DPCNYUIN-PTID       TO  PTNYUINRRK-PTID
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  PTNYUINRRK-TENNYUYMD(1:6)
           MOVE    "99"                    TO  PTNYUINRRK-TENNYUYMD(7:2)
           MOVE    LNK-DPCNYUIN-SANTEIYM   TO  PTNYUINRRK-TENSTUYMD(1:6)
           MOVE    "01"                    TO  PTNYUINRRK-TENSTUYMD(7:2)
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key44"                 TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-PTNYUINRRK         =   1  )
      *
      **           DISPLAY "PTNYUINRRK-FETCH"
      **           DISPLAY "PTNYUINRRK-TENNYUYMD = " PTNYUINRRK-TENNYUYMD
      **           DISPLAY "PTNYUINRRK-TENSTUYMD = " PTNYUINRRK-TENSTUYMD
                   MOVE    1     TO     IDX1
                   MOVE    31    TO     IDX2
                   IF      LNK-DPCNYUIN-SANTEIYM
                                     =   PTNYUINRRK-TENNYUYMD(1:6)
                       MOVE   PTNYUINRRK-TENNYUYMD(7:2)
                                         TO    WRK-DAY-X
                       MOVE   WRK-DAY-9  TO    IDX1
                   END-IF
                   IF      LNK-DPCNYUIN-SANTEIYM
                                     =   PTNYUINRRK-TENSTUYMD(1:6)
                       MOVE   PTNYUINRRK-TENSTUYMD(7:2)
                                         TO    WRK-DAY-X
                       MOVE   WRK-DAY-9  TO    IDX2
                   END-IF
      *
      **             DISPLAY "IDX1 = " IDX1
      **             DISPLAY "IDX2 = " IDX2
                   PERFORM   VARYING   IDX    FROM  IDX1   BY  1
                             UNTIL    (IDX    >   IDX2)
      *                 ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡ÊÆ±ÆüºÆÆþ±¡Æü¤ÏÊÌ¹àÌÜ¤Ë¥»¥Ã¥È¤¹¤ë¡Ë
                        IF     TBL-BTUNUM(IDX)  =   ZERO
                          MOVE   PTNYUINRRK-RRKNUM  TO  TBL-RRKNUM(IDX)
                          MOVE   PTNYUINRRK-BTUNUM  TO  WRK-DAY-X
                          MOVE   WRK-DAY-9          TO  TBL-BTUNUM(IDX)
                          MOVE   PTNYUINRRK-NYUINKA TO  TBL-SRYKA(IDX)
                          MOVE   PTNYUINRRK-DRCD1   TO  TBL-DRCD(IDX)
                        ELSE
                          IF   PTNYUINRRK-RRKNUM    =   TBL-RRKNUM(IDX)
                            MOVE PTNYUINRRK-BTUNUM  TO  WRK-DAY-X
                            MOVE WRK-DAY-9          TO  TBL-BTUNUM(IDX)
                            MOVE PTNYUINRRK-NYUINKA TO  TBL-SRYKA(IDX)
                            MOVE PTNYUINRRK-DRCD1   TO  TBL-DRCD(IDX)
                          ELSE
                            MOVE PTNYUINRRK-BTUNUM  TO  WRK-DAY-X
                            MOVE WRK-DAY-9          TO  TBL-BTUNUM2(IDX)
                            MOVE PTNYUINRRK-NYUINKA TO  TBL-SRYKA2(IDX)
                            MOVE PTNYUINRRK-DRCD1   TO  TBL-DRCD2(IDX)
                          END-IF
                        END-IF
                   END-PERFORM
                   MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
                   MOVE    "key44"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key44"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    ÉÂÅï¶èÊ¬¥»¥Ã¥È
           PERFORM   VARYING   IDX   FROM   1  BY  1
                     UNTIL    (IDX    >   31)
               IF   TBL-BTUNUM(IDX)    NOT =   ZERO
                   IF    IDX    >   1
                        COMPUTE   IDX3   =   IDX   -  1
                        IF    TBL-BTUNUM(IDX)   =  TBL-BTUNUM(IDX3)
                           MOVE  TBL-BTUKBN(IDX3)
                                          TO  TBL-BTUKBN(IDX)
                        ELSE
                           MOVE    ZERO           TO  FLG-SYSKANRI
                           INITIALIZE                 SYSKANRI-REC
                           MOVE    "5001"         TO  SYS-KANRICD
                           MOVE    TBL-BTUNUM(IDX)
                                                  TO  SYS-KBNCD
                           MOVE    LNK-DPCNYUIN-SANTEIYM
                                                  TO  SYS-STYUKYMD(1:6)
                           MOVE    "01"           TO  SYS-STYUKYMD(7:2)
                           MOVE    SYS-STYUKYMD   TO  SYS-EDYUKYMD
                           MOVE    SPA-HOSPNUM    TO  SYS-HOSPNUM
                           MOVE    SYSKANRI-REC   TO  MCPDATA-REC
                           MOVE    "tbl_syskanri" TO  MCP-TABLE
                           MOVE    "key10"        TO  MCP-PATHNAME
                           PERFORM   900-DBSELECT-SEC
                           IF      MCP-RC         =   ZERO
                               MOVE    "tbl_syskanri"
                                                  TO  MCP-TABLE
                               MOVE    "key10"    TO  MCP-PATHNAME
                               PERFORM  910-SYSKANRI-READ-SEC
                               MOVE     SYS-5001-BTU-SBT
                                                  TO  TBL-BTUKBN(IDX)
                           END-IF
                           MOVE    "tbl_syskanri" TO  MCP-TABLE
                           MOVE    "key10"        TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
                        END-IF
                   ELSE
                        MOVE    ZERO              TO  FLG-SYSKANRI
                        INITIALIZE                    SYSKANRI-REC
                        MOVE    "5001"            TO  SYS-KANRICD
                        MOVE    TBL-BTUNUM(IDX)   TO  SYS-KBNCD
                        MOVE    LNK-DPCNYUIN-SANTEIYM
                                                  TO  SYS-STYUKYMD(1:6)
                        MOVE    "01"              TO  SYS-STYUKYMD(7:2)
                        MOVE    SYS-STYUKYMD      TO  SYS-EDYUKYMD
                        MOVE    SPA-HOSPNUM       TO  SYS-HOSPNUM
                        MOVE    SYSKANRI-REC      TO  MCPDATA-REC
                        MOVE    "tbl_syskanri"    TO  MCP-TABLE
                        MOVE    "key10"           TO  MCP-PATHNAME
                        PERFORM   900-DBSELECT-SEC
                        IF      MCP-RC            =   ZERO
                            MOVE    "tbl_syskanri" TO MCP-TABLE
                            MOVE    "key10"       TO  MCP-PATHNAME
                            PERFORM  910-SYSKANRI-READ-SEC
                            MOVE     SYS-5001-BTU-SBT
                                                  TO  TBL-BTUKBN(IDX)
                        END-IF
                        MOVE    "tbl_syskanri"    TO  MCP-TABLE
                        MOVE    "key10"           TO  MCP-PATHNAME
                        PERFORM   900-CLOSE-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ÉÂÅï¶èÊ¬¥»¥Ã¥È¡ÊÆ±ÆüºÆÆþ±¡Ê¬¡Ë
           PERFORM   VARYING   IDX   FROM   1  BY  1
                     UNTIL    (IDX    >   31)
               IF   TBL-BTUNUM2(IDX)    NOT =   ZERO
                   IF    IDX    >   1
                        COMPUTE   IDX3   =   IDX   -  1
                        IF    TBL-BTUNUM2(IDX)   =  TBL-BTUNUM2(IDX3)
                           MOVE  TBL-BTUKBN2(IDX3)
                                          TO  TBL-BTUKBN2(IDX)
                        ELSE
                           MOVE    ZERO           TO  FLG-SYSKANRI
                           INITIALIZE                 SYSKANRI-REC
                           MOVE    "5001"         TO  SYS-KANRICD
                           MOVE    TBL-BTUNUM2(IDX)
                                                  TO  SYS-KBNCD
                           MOVE    LNK-DPCNYUIN-SANTEIYM
                                                  TO  SYS-STYUKYMD(1:6)
                           MOVE    "01"           TO  SYS-STYUKYMD(7:2)
                           MOVE    SYS-STYUKYMD   TO  SYS-EDYUKYMD
                           MOVE    SPA-HOSPNUM    TO  SYS-HOSPNUM
                           MOVE    SYSKANRI-REC   TO  MCPDATA-REC
                           MOVE    "tbl_syskanri" TO  MCP-TABLE
                           MOVE    "key10"        TO  MCP-PATHNAME
                           PERFORM   900-DBSELECT-SEC
                           IF      MCP-RC         =   ZERO
                               MOVE    "tbl_syskanri"
                                                  TO  MCP-TABLE
                               MOVE    "key10"    TO  MCP-PATHNAME
                               PERFORM  910-SYSKANRI-READ-SEC
                               MOVE     SYS-5001-BTU-SBT
                                                  TO  TBL-BTUKBN2(IDX)
                           END-IF
                           MOVE    "tbl_syskanri" TO  MCP-TABLE
                           MOVE    "key10"        TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
                        END-IF
                   ELSE
                        MOVE    ZERO              TO  FLG-SYSKANRI
                        INITIALIZE                    SYSKANRI-REC
                        MOVE    "5001"            TO  SYS-KANRICD
                        MOVE    TBL-BTUNUM2(IDX)  TO  SYS-KBNCD
                        MOVE    LNK-DPCNYUIN-SANTEIYM
                                                  TO  SYS-STYUKYMD(1:6)
                        MOVE    "01"              TO  SYS-STYUKYMD(7:2)
                        MOVE    SYS-STYUKYMD      TO  SYS-EDYUKYMD
                        MOVE    SPA-HOSPNUM       TO  SYS-HOSPNUM
                        MOVE    SYSKANRI-REC      TO  MCPDATA-REC
                        MOVE    "tbl_syskanri"    TO  MCP-TABLE
                        MOVE    "key10"           TO  MCP-PATHNAME
                        PERFORM   900-DBSELECT-SEC
                        IF      MCP-RC            =   ZERO
                            MOVE    "tbl_syskanri" TO MCP-TABLE
                            MOVE    "key10"       TO  MCP-PATHNAME
                            PERFORM  910-SYSKANRI-READ-SEC
                            MOVE     SYS-5001-BTU-SBT
                                                  TO  TBL-BTUKBN2(IDX)
                        END-IF
                        MOVE    "tbl_syskanri"    TO  MCP-TABLE
                        MOVE    "key10"           TO  MCP-PATHNAME
                        PERFORM   900-CLOSE-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *     PERFORM    VARYING   SET-IDX1   FROM  1  BY  1
      *       UNTIL    SET-IDX1   >   31
      *         DISPLAY "SET-IDX1 = " SET-IDX1
      *         DISPLAY "TBL-BTUNUM = " TBL-BTUNUM(SET-IDX1)
      *         DISPLAY "TBL-BTUKBN = " TBL-BTUKBN(SET-IDX1)
      *         DISPLAY "TBL-SRYKA  = " TBL-SRYKA(SET-IDX1)
      *         DISPLAY "TBL-DRCD   = " TBL-DRCD(SET-IDX1)
      *         DISPLAY "TBL-BTUNUM2 = " TBL-BTUNUM2(SET-IDX1)
      *         DISPLAY "TBL-BTUKBN2 = " TBL-BTUKBN2(SET-IDX1)
      *         DISPLAY "TBL-SRYKA2  = " TBL-SRYKA2(SET-IDX1)
      *         DISPLAY "TBL-DRCD2   = " TBL-DRCD2(SET-IDX1)
      *     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È
           
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡ÊÆþ±¡ÎÁ¡Ë
      **          IF    LNK-DPCNYUIN-SRYCD(1 IDX1 1)   NOT =  SPACE
                    MOVE   TBL-BTUNUM(IDX1)   TO
                                          LNK-DPCNYUIN-BTUNUM(1 IDX1)
                    MOVE   TBL-BTUKBN(IDX1)   TO
                                          LNK-DPCNYUIN-BTUKBN(1 IDX1)
                    MOVE   TBL-SRYKA(IDX1)    TO
                                          LNK-DPCNYUIN-SRYKA(1 IDX1)
                    MOVE   TBL-DRCD(IDX1)     TO
                                          LNK-DPCNYUIN-DRCD(1 IDX1)
      **          END-IF
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
      **          IF    LNK-DPCNYUIN-SRYCD(2 IDX1 1)   NOT =  SPACE
                    MOVE   TBL-BTUNUM(IDX1)   TO
                                          LNK-DPCNYUIN-BTUNUM(2 IDX1)
                    MOVE   TBL-BTUKBN(IDX1)   TO
                                          LNK-DPCNYUIN-BTUKBN(2 IDX1)
                    MOVE   TBL-SRYKA(IDX1)    TO
                                          LNK-DPCNYUIN-SRYKA(2 IDX1)
                    MOVE   TBL-DRCD(IDX1)     TO
                                          LNK-DPCNYUIN-DRCD(2 IDX1)
      **          END-IF
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡Ê´Ä¶­ÎÅÍÜ¡Ë
      **          IF    LNK-DPCNYUIN-SRYCD(5 IDX1 1)   NOT =  SPACE
                    MOVE   TBL-BTUNUM(IDX1)   TO
                                          LNK-DPCNYUIN-BTUNUM(5 IDX1)
                    MOVE   TBL-BTUKBN(IDX1)   TO
                                          LNK-DPCNYUIN-BTUKBN(5 IDX1)
                    MOVE   TBL-SRYKA(IDX1)    TO
                                          LNK-DPCNYUIN-SRYKA(5 IDX1)
                    MOVE   TBL-DRCD(IDX1)     TO
                                          LNK-DPCNYUIN-DRCD(5 IDX1)
      **          END-IF
           END-PERFORM
      *
           IF    LNK-DPCNYUIN-SANTEIYM  <=  "201709"
              PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡Ê¿©»ö¡Ë
      **          IF    LNK-DPCNYUIN-SRYCD(3 IDX1 1)   NOT =  SPACE
                       MOVE   TBL-BTUNUM(IDX1)   TO
                                          LNK-DPCNYUIN-BTUNUM(3 IDX1)
                       MOVE   TBL-BTUKBN(IDX1)   TO
                                          LNK-DPCNYUIN-BTUKBN(3 IDX1)
                       MOVE   TBL-SRYKA(IDX1)    TO
                                          LNK-DPCNYUIN-SRYKA(3 IDX1)
                       MOVE   TBL-DRCD(IDX1)     TO
                                          LNK-DPCNYUIN-DRCD(3 IDX1)
      **          END-IF
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡ÊÀ¸³èÎÅÍÜ¡Ë
      **          IF    LNK-DPCNYUIN-SRYCD(4 IDX1 1)   NOT =  SPACE
                       MOVE   TBL-BTUNUM(IDX1)   TO
                                          LNK-DPCNYUIN-BTUNUM(4 IDX1)
                       MOVE   TBL-BTUKBN(IDX1)   TO
                                          LNK-DPCNYUIN-BTUKBN(4 IDX1)
                       MOVE   TBL-SRYKA(IDX1)    TO
                                          LNK-DPCNYUIN-SRYKA(4 IDX1)
                       MOVE   TBL-DRCD(IDX1)     TO
                                          LNK-DPCNYUIN-DRCD(4 IDX1)
      **          END-IF
              END-PERFORM
           ELSE
              PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     40)
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡Ê¿©»ö¡Ë
                IF   LNK-DPCNYUIN-DD(3 IDX1)  NOT =   ZERO
                     MOVE   LNK-DPCNYUIN-DD(3 IDX1)   TO  WRK-SET-DD
                     MOVE   TBL-BTUNUM(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUNUM(3 IDX1)
                     MOVE   TBL-BTUKBN(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUKBN(3 IDX1)
                     MOVE   TBL-SRYKA(WRK-SET-DD)    TO
                                          LNK-DPCNYUIN-SRYKA(3 IDX1)
                     MOVE   TBL-DRCD(WRK-SET-DD)     TO
                                          LNK-DPCNYUIN-DRCD(3 IDX1)
                     PERFORM   VARYING  IDX2 FROM  1 BY  1
                       UNTIL  (IDX2      >   100)    OR
                               (LNK-DPCNYUIN-SRYCD(3 IDX1 IDX2) = SPACE)
                             IF   LNK-DPCNYUIN-SAINYUINKBN(3 IDX1 IDX2)
                                                       =    "1"
                                MOVE   TBL-BTUNUM2(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUNUM2(3 IDX1)
                                MOVE   TBL-BTUKBN2(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUKBN2(3 IDX1)
                                MOVE   TBL-SRYKA2(WRK-SET-DD)    TO
                                          LNK-DPCNYUIN-SRYKA2(3 IDX1)
                                MOVE   TBL-DRCD2(WRK-SET-DD)     TO
                                          LNK-DPCNYUIN-DRCD2(3 IDX1)
                             END-IF
                     END-PERFORM
                END-IF
      *         ÉÂÅïÈÖ¹æ¥»¥Ã¥È¡ÊÀ¸³èÎÅÍÜ¡Ë
                IF   LNK-DPCNYUIN-DD(4 IDX1)  NOT =   ZERO
                     MOVE   LNK-DPCNYUIN-DD(4 IDX1)   TO  WRK-SET-DD
                     MOVE   TBL-BTUNUM(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUNUM(4 IDX1)
                     MOVE   TBL-BTUKBN(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUKBN(4 IDX1)
                     MOVE   TBL-SRYKA(WRK-SET-DD)    TO
                                          LNK-DPCNYUIN-SRYKA(4 IDX1)
                     MOVE   TBL-DRCD(WRK-SET-DD)     TO
                                          LNK-DPCNYUIN-DRCD(4 IDX1)
                     PERFORM   VARYING  IDX2 FROM  1 BY  1
                       UNTIL  (IDX2      >   100)    OR
                               (LNK-DPCNYUIN-SRYCD(4 IDX1 IDX2) = SPACE)
                             IF   LNK-DPCNYUIN-SAINYUINKBN(4 IDX1 IDX2)
                                                       =    "1"
                                MOVE   TBL-BTUNUM2(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUNUM2(4 IDX1)
                                MOVE   TBL-BTUKBN2(WRK-SET-DD)   TO
                                          LNK-DPCNYUIN-BTUKBN2(4 IDX1)
                                MOVE   TBL-SRYKA2(WRK-SET-DD)    TO
                                          LNK-DPCNYUIN-SRYKA2(4 IDX1)
                                MOVE   TBL-DRCD2(WRK-SET-DD)     TO
                                          LNK-DPCNYUIN-DRCD2(4 IDX1)
                             END-IF
                     END-PERFORM
                END-IF
              END-PERFORM
           END-IF
      *
           .
       2105-BTUNUM-HENSHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       910-TENSUWRK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUWRK
      *
           PERFORM VARYING TTNS-IDX      FROM    1   BY  1
                   UNTIL ( TTNS-IDX      >   TTNS-MAX ) OR
                         ( FLG-TENSUWRK    =   1 )      OR
                         ( TTNS-IDX      >   200 )
               IF    ( WRK-TENSU-SRYCD (TTNS-IDX)
                                       =   WRK-TENSU-CD )
                   MOVE    1           TO  FLG-TENSUWRK
                   MOVE    WRK-TENSU-DATA (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       910-TENSUWRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       910-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  WRK-TENSU-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  WRK-TENSU-DATA
                                                   (TTNS-MAX)
           END-IF
      *
           .
       910-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
                                               SYS-5001-REC
                                               SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
                                               SYS-5001-REC
                                               SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ¿¶ÂØ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SENTEICDCHG-SELECT-SEC        SECTION.
      *
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÆþ±¡ÎÁ¿¶ÂØ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SENTEICDCHG-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SENTEICDCHG-REC
               MOVE    ZERO               TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                     SENTEICDCHG-REC
               MOVE    1                  TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       910-SENTEICDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-TSYRRK-SELECT-SEC        SECTION.
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TSYRRK-REC
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               INITIALIZE                     TSYRRK-REC
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄãÇ¯¶â¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-TNKRRK-SELECT-SEC        SECTION.
      *
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄãÇ¯¶â¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TNKRRK-REC
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               INITIALIZE                     TNKRRK-REC
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
