      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSNYURECEDEN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»ÅÅÍÑÆþ±¡²ñ·×¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/05/18    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-Æ£¸¶    04/10/19  ¥³¥á¥ó¥ÈËä¤á¹þ¤ßÊ¸»ú¤ÎÊÑ¹¹
      *  01.00.02    NACL-¾®Æ¦Âô  05/06/14  Äã½êÆÀ´µ¼Ô¤Î¿©»öÉ¸½àÉéÃ´³Û¤Î
      *                                     È½ÄêÊýË¡¤òÊÑ¹¹¡Ê£±ÆüÍ­¸ú¤ÇÈ½Äê¡Ë
      *  01.00.03    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.04    NACL-¾®Æ¦Âô  06/01/06  ¿©»ö¤ÎÆÃÊÌ´ÉÍý²Ã»»¤ò·îÅÓÃæ¤ÇÊÑ¹¹
      *                                     ¤·¤¿¾ì¹ç¤ÎÂÐ½èÄÉ²Ã
      *Ê¿À®£±£¸Ç¯£´·î²þÄêÂÐ±þ
      *  01.00.05    NACL-¾®Æ¦Âô  06/03/22  ÆÃÄêÆþ±¡ÎÁÄÉ²ÃÊ¬¡ÊÇ¾Â´Ãæ¥±¥¢¥æ¥Ë¡Ë
      *                                     ³°ÇñÃÖ¤­´¹¤¨½èÍý
      *  01.00.06    NACL-¾®Æ¦Âô  06/04/05  MONFUNCÂÐ±þ
      *  01.00.07    NACL-¾®Æ¦Âô  06/04/19  ¿©»öµ­Ï¿ÊýË¡¤ÎÊÑ¹¹ÂÐ±þ
      *  01.00.08    NACL-¾®Æ¦Âô  06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  01.00.09    NACL-¾®Æ¦Âô  06/09/08  É¸·çÂÐ±þ
      *  01.00.10    NACL-¾®Æ¦Âô  06/09/13  Äê¿ôÄ¶²áÂÐ±þ
      *  01.00.11    NACL-¾®Æ¦Âô  06/10/06  ÎÅÍÜÉÂ¾²¥ì¥»¥³¥á¥ó¥ÈÂÐ±þ
      *  01.00.12    NACL-¾®Æ¦Âô  06/10/11  Æþ±¡»þÀ¸³èÎÅÍÜÈñÂÐ±þ
      *  03.05.00    NACL-¾®Æ¦Âô  07/05/29  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.01    NACL-¾®Æ¦Âô  07/06/19  °Ê²¼Æþ±¡ÎÁ²Ã»»¤Îµ­Ï¿½ç½¤Àµ
      *                                     Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
      *                                     ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
      *  03.05.02    NACL-¾®Æ¦Âô  07/07/20  Ê¿À®£±£¸Ç¯£´·î°Ê¹ß¤Î¿©»ö¾ðÊó
      *                                     ÊÔ½¸¤Î´µ¼Ô½êÆÀ¾ðÊó¼èÆÀ¤ò¥µ¥Ö
      *                                     ¥ë¡¼¥Á¥ó¤ËÊÑ¹¹
      *  04.01.01    NACL-¾®Æ¦Âô  07/11/20  ¸ø³²ÊÝ¸±ÂÐ±þ¡Ê¥ì¥»ÅÅÂÐ¾Ý³°¡Ë
      *  04.01.00    NACL-¾®Æ¦Âô  07/12/19  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡ÍâÆü¤Î
      *                                     ¿©»ö¤ÈÆþ±¡ÎÁ²Ã»»ÂÐ±þ
      *  04.01.00    NACL-¾®Æ¦Âô  08/02/04  É¸·ç¤ÈÄê¿ôÄ¶²á¤Î³°Çñ½¤Àµ
      *Ê¿À®£²£°Ç¯£´·î²þÄêÂÐ±þ
      *  04.02.00    NACL-¾®Æ¦Âô  08/03/25  ¹­°èÏ¢¹ç¡Ê¸å´ü¹âÎð¼Ô¡Ë¤Î¥ì¥»ÅÅÂÐ±þ
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/11  Âà±¡²Ã»»¤Î¥ì¥»ÅÅÂÐ±þ¡ÊÆÉ¤ßÈô¤Ð¤·¡Ë
      *                                     ¡¦Âà±¡²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
      *                                     ¡¦Âà±¡²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
      *                                     ¡¦Áá´üÂà±¡²Ã»»
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/14  Í­¾²¿ÇÎÅ½ê¤Î²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                                     ¡¦°å»ÕÇÛÃÖÅ¬¹ç²Ã»»
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£²
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/14  ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ¤È²óÉü´ü¥ê¥Ï¥Ó¥ê¤Î
      *                                     ²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
      *                                     ¡¦¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë
      *                                     ¡¦½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/15  ÆÃ¼ì¼À´µÉÂÅïÆþ±¡ÎÁ¤Î²Ã»»¤òÃí²Ã»»
      *                                     ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/15  ÆÃÄêµ¡Ç½ÉÂ±¡¡ÝÀº¿ÀÉÂÅï¤Î²Ã»»¤òÃí²Ã»»
      *                                     ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/15  Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ µÚ¤Ó
      *                                     Àº¿À²ÊµßµÞÆþ±¡ÎÁ¤Î²Ã»»¤òÃí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦ÈóÄê·¿¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
      *
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/15  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ µÚ¤Ó
      *                                     Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¤Î²Ã»»¤ò
      *                                     Ãí²Ã»»ÄÌÈÖ½ç¤Ëµ­Ï¿
      *                                     ¡¦êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
      *                                     ¡¦êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
      *  04.05.00    NACL-¾®Æ¦Âô  09/12/03  ¿ÇÎÅÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤ÎÂÐ±þ
      *  04.05.00    NACL-¾®Æ¦Âô  10/01/13  ¼£¸³¸ºÅÀÆþÎÏ¤ÎÆÉ¤ßÈô¤Ð¤·
      *  04.07.00    NACL-¾®Æ¦Âô  12/02/08  ²ó¿ôµ­Ï¿¤òÁ´¤Æ¤Î£Ó£É¥ì¥³¡¼¥É¤òÂÐ¾Ý¤È¤¹¤ë
      *  04.07.00    NACL-¾®Æ¦Âô  12/05/30  ÆÃÊÌÎÅÍÜ¡ÊµìÆÃÊÌÆü¸Û¡Ë¤âºÒ³²¸ºÌÈ¤ÎÂÐ¾Ý¤È
      *                                     ¤¹¤ë¤è¤¦¤ËÊÑ¹¹
      *Ê¿À®£²£¹Ç¯£±£°·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  17/08/16  µï½»Èñ¶â³ÛÊÑ¹¹ÂÐ±þ
      *                                     ¶­³¦ÁØÂÐ±þ
      *                                     À¸³èÎÅÍÜÉéÃ´¥³¡¼¥ÉÊ¬³äÂÐ±þ
      *  05.00.01    NACL-¾®Æ¦Âô  17/11/07  Æ±ÆüºÆÆþ±¡¤Î¿©»öµ­Ï¿½¤Àµ
      *
      *Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/02/27  Æþ±¡»þ¿©»öÎÅÍÜ¤Î¶â³ÛÊÑ¹¹ÂÐ±þ
      *                                     ¿©»öÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *                                     À¸³èÎÅÍÜ¿©»öÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *                                     µï½»ÈñÉéÃ´³ÛÊÑ¹¹ÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/3   Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»ÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/11  Â¾°å¼õ¿ÇÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/04/16  °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¾ðÊó¤Î
      *                                     µ­Ï¿¤òÊ¿À®£³£°Ç¯£³·îËø¤È¤¹¤ë
      *
      *ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  20/03/10  Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÊÔ½¸
      *
      *  05.00.00    NACL-¾®Æ¦Âô  20/03/18  ¡ó¸º»»·×»»»þ¤ÎÃ¼¿ô½èÍýÊÑ¹¹
      *  05.00.00    NACL-¾®Æ¦Âô  20/04/13  Æþ±¡ÎÁ¤È²Ã»»¤ÎÌÀºÙ¿ô¹ç·×¤¬
      *                                     £±£¹¤òÄ¶¤¨¤¿»þ¤ËÅÀ¿ô¤¬µ­Ï¿
      *                                     ¤µ¤ì¤Ê¤¤ÉÔ¶ñ¹ç¤ò½¤Àµ
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
           03  FLG-TOKU-SET                PIC 9(01).
           03  FLG-TOKU-SHOKU              PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
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
           03  GENSAN-IDX                  PIC 9(04).
           03  GENSAN-IDX2                 PIC 9(04).
           03  IDV                         PIC 9(04).
           03  TSY90-IDX                   PIC 9(02).
           03  ETC-IDX                     PIC 9(04).
           03  IDX-TSY90                   PIC 9(04).
           03  IDX-ADL                     PIC 9(04).
           03  MATOME-IDX                  PIC 9(04).
           03  CIDX                        PIC 9(04).
           03  CIDX2                       PIC 9(04).
           03  IDX-DAY                     PIC 9(04).
           03  IDX-SAIGAI                  PIC 9(02).
           03  IDX-SAIGAI-ST               PIC 9(02).
           03  IDX-SAIGAI-ED               PIC 9(02).
           03  IDX-NYUKSN                  PIC 9(02).
           03  IDX-CHUKSN1                 PIC 9(02).
           03  IDX-CHUKSN1S                PIC 9(02).
           03  IDX-CHUKSN1SS               PIC 9(02).
           03  IDX-CHUKSN2                 PIC 9(02).
           03  IDX-CHUKSN2S                PIC 9(02).
           03  IDX-CHUKSN2SS               PIC 9(02).
           03  IDX-IRYOKNS                 PIC 9(02).
           03  IDX-IRYOKNS2                PIC 9(02).
      *    Æ±ÆüºÆÆþ±¡¤Î¿©»öÊÔ½¸»þ¥¤¥ó¥Ç¥Ã¥¯¥¹ÂàÈòÎÎ°è
           03  SET-IDX1                    PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    Ç§ÃÎ¾É²Ã»»
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *    ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
           03  CONS-TOKUSEIYAKU-KSN        PIC X(09) VALUE "190117570".
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  CONS-ETCNYUIN-TBL.
             05  CONS-ETCNYUIN-KSN         PIC X(09)  OCCURS 10.
      *    ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß¥ï¡¼¥¯ÍÑ
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *
      *    Âà±¡²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
           03  CONS-TAIIN1                 PIC X(09) VALUE "190128410".
      *    Âà±¡²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë
           03  CONS-TAIIN2                 PIC X(09) VALUE "190128310".
      *    Áá´üÂà±¡²Ã»»
           03  CONS-TAIIN3                 PIC X(09) VALUE "190128570".
      *    Âà±¡²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁÅù»»Äê´µ¼Ô¡Ë¡ÜÁá´üÂà±¡²Ã»»
           03  CONS-TAIIN4                 PIC X(09) VALUE "380256980".
      *
      *
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
      *    Ê¿À®£²£¸Ç¯ÅÙ¿©»öÉéÃ´
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
      *    °ì»þÎÎ°è
       01  WRK-AREA.
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
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *    Æ±ÆüºÆÆþ±¡½èÍýÍÑ
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
           03  WRK-DOUJITSU-KBN            PIC X(01).
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
                   07  WRK-SO-DAY-TBL.
                     09  WRK-SO-DAY        PIC 9(01)  OCCURS 31.
                   07  WRK-SO-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SO-SRYKBN       PIC X(02).
                   07  WRK-SO-SRYCD        PIC X(09)  OCCURS 30.
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
      *    Æ±°ìºÞ¤Þ¤È¤áÍÑÈæ³Ó¥­¡¼
           03  WRK-OLD-KEY.
               05  WRK-OLD-MEISAISU        PIC 9(02).
               05  WRK-OLD-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-OLD-SRYCDTOTAL      PIC 9(14).
               05  WRK-OLD-TEN             PIC 9(07).
           03  WRK-NEW-KEY.
               05  WRK-NEW-MEISAISU        PIC 9(02).
               05  WRK-NEW-HKNCOMBI-DAY    PIC 9(03).
               05  WRK-NEW-SRYCDTOTAL      PIC 9(14).
               05  WRK-NEW-TEN             PIC 9(07).
      *
      *    ÎÅÍÜÉÂ¾²¥³¥á¥ó¥ÈÊÂ¤ÓÂØ¤¨¥ï¡¼¥¯
           03  WRK-IRYOADL-TABLE.
               05  WRK-IRYOADL-SORT-TBL      OCCURS  31.
                   07  WRK-IRYOADL-DAY          PIC 9(02).
                   07  WRK-IRYOADL-MEISAISU     PIC 9(02).
                   07  WRK-IRYOADL-SRYCD        PIC X(09)  OCCURS 30.
                   07  WRK-IRYOADL-SRYCDTOTAL   PIC 9(14).
                   07  WRK-IRYOADL-ADL-SRYCD    PIC X(09).
                   07  WRK-IRYOADL-TEN          PIC 9(07).
                   07  WRK-IRYOADL-TEN-MOJI     PIC X(04).
                   07  WRK-IRYOADL-ADL-SRYCD-H22 PIC X(09) OCCURS 4.
                   07  WRK-IRYOADL-TEN-H22      PIC 9(07)  OCCURS 4.
                   07  WRK-IRYOADL-TEN-MOJI-H22 PIC X(04)  OCCURS 4.
                   07  WRK-IRYOADL-GAITOU-TBL.
                     09  WRK-IRYOADL-GAITOU-KBN   PIC 9(01)  OCCURS 31.
      *
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»ÂàÈòÎÎ°è¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  WRK-FUKUSU-NYUKSN-AREA.
                   07  WRK-FUKUSU-NYUKSN         PIC X(09).
                   07  WRK-FUKUSU-NYUKSN-ZAITEN  PIC 9(08).
                   07  WRK-FUKUSU-NYUKSN-DAY     PIC 9(03)  OCCURS 31.
      *
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»ÂàÈòÎÎ°è¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  WRK-KANGO-NYUKSN-AREA.
                   07  WRK-KANGO-NYUKSN          PIC X(09).
                   07  WRK-KANGO-NYUKSN-ZAITEN   PIC 9(08).
                   07  WRK-KANGO-NYUKSN-DAY      PIC 9(03)  OCCURS 31.
      *
      *    Ãí²Ã»»ÄÌÈÖ¤ò¤â¤ÄÆþ±¡ÎÁ²Ã»»¤ÎÂàÈòÎÎ°è¡ÊÊ¿À®£²£°Ç¯£´·î¤è¤ê¡Ë
      *    £±Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *    £²°å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤ê°å»ÕÇÛÃÖ²Ã»»£²¡Ë
      *    £³´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤ê´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
      *    £´´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤ê´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
      *    £µÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤êÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
      *    £¶Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤êÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
      *    £·¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë
      *    £¸¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
      *    £¹¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë
      *  £±£°½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
      *  £±£±ÈóÄê·¿¹³Àº¿ÀÌô¼£ÎÅ´ÉÍý²Ã»»¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤êÈóÄê·¿¹³Àº¿ÀÌô²Ã»»£²¡Ë
      *  £±£²êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
      *  £±£³êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
      *  £±£´½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
      *   Ãí²Ã»»ÄÌÈÖ¤ò¤â¤ÄÆþ±¡ÎÁ²Ã»»¤ÎÂàÈòÎÎ°è¡ÊÊ¿À®£²£²Ç¯£´·îÄÉ²ÃÊ¬¡Ë
      *  £±£µ°å»ÕÇÛÃÖ²Ã»»£±
      *  £±£¶½¼¼ÂÃÊ³¬£Á²Ã»»
      *  £±£·½¼¼ÂÃÊ³¬£Â²Ã»»
      *  £±£¸¹âÅÙ°åÎÅÂÎÀ©²Ã»»
      *  £±£¹¾®»ù²Ã»»¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡¡£·Æü°ÊÆâ¡Ë
      *  £²£°¾®»ù²Ã»»¡ÊÆÃÄê½¸Ãæ¼£ÎÅ¼¼´ÉÍýÎÁ¡¡£¸Æü°Ê¾å£±£´ÆüÌ¤Ëþ¡Ë
      *  £²£±½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»
      *  £²£²ÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£±
      *  £²£³µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
      *  £²£´¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó½¼¼Â²Ã»»
      *  £²£µÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
      *  £²£¶½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
      *  £²£·Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
      *  £²£¸½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
      *  £²£¹ºßÂð½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþ²Ã»»
      *  £³£°°åÎÅ°ÂÁ´ÂÐºö²Ã»»£±
      *  £³£±´¶À÷ËÉ»ßÂÐºö²Ã»»
      *  £³£²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
      *  £³£³½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê´ð½àÌ¤Å¬¹ç¡Ë
           03  WRK-CHUKSN-NYUKSN-AREA.
             05   WRK-CHUKSN-NYUKSN-TBL      OCCURS  33.
               07  WRK-CHUKSN-NYUKSN         PIC X(09).
               07  WRK-CHUKSN-NYUKSN-ZAITEN  PIC 9(08).
               07  WRK-CHUKSN-NYUKSN-DAY     PIC 9(03)  OCCURS 31.
      *
      *    Æþ±¡ÎÁ²Ã»»ÂàÈòÎÎ°è
           03  WRK-NYUKSN-TABLE.
               05  WRK-NYUKSN-TBL            OCCURS  20.
                   07  WRK-NYUKSN            PIC X(09).
                   07  WRK-NYUKSN-ZAITEN     PIC 9(08).
                   07  WRK-NYUKSN-DAY        PIC 9(03)  OCCURS 31.
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
      *    Ì¤¼Â»Ü¸º»»ÂàÈòÍÑ¥Õ¥é¥°¥Æ¡¼¥Ö¥ë
           03  WRK-GENSAN-TABLE.
               05  WRK-GENSAN-INF          OCCURS  31.
                   07  WRK-GENSAN-INNAI    PIC X(01).
                   07  WRK-GENSAN-IRYO     PIC X(01).
                   07  WRK-GENSAN-JYOKU    PIC X(01).
      *
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN2                 PIC X(09).
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN3                 PIC X(09).
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN4                 PIC X(09).
      *
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-KEIKAKU-GENSAN1         PIC 9(03) OCCURS 31.
           03  WRK-KEIKAKU-GENSAN2         PIC 9(03) OCCURS 31.
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-INNAI-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-INNAI-GENSAN2           PIC 9(03) OCCURS 31.
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-ANZEN-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-ANZEN-GENSAN2           PIC 9(03) OCCURS 31.
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-JYOKU-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-GENSAN2           PIC 9(03) OCCURS 31.
      *
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»ÂàÈò
           03  WRK-NICHIJOU-KSN1           PIC 9(03) OCCURS 31.
           03  WRK-NICHIJOU-KSN2           PIC 9(03) OCCURS 31.
      *
      *    Ç§ÃÎ¾É²Ã»»ÂàÈò
           03  WRK-CHIHOU-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-CHIHOU-KSN2             PIC 9(03) OCCURS 31.
      *
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  WRK-JYOKU-KSN1              PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-KSN2              PIC 9(03) OCCURS 31.
      *
      *    ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
           03  WRK-TOKUSEIYAKU-KSN         PIC 9(03) OCCURS 31.
      *
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  WRK-ETCNYUIN-TBL            OCCURS  10.
             05  WRK-ETCNYUIN-KSN          PIC 9(03) OCCURS 31.
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
           03  WRK-SKJ-TABLE.
               05  WRK-SKJ-TBL           OCCURS  31.
                   07  WRK-SKJ-HKNCOMBI-DAY    PIC 9(03).
                   07  WRK-SKJ-GRP.
                       09  WRK-SKJ-RYOYO       PIC X(09).
                       09  WRK-SKJ-TOKU-KANRI  PIC X(09).
                       09  WRK-SKJ-SHOKUDOU    PIC X(09).
                   07  WRK-SKJ-GRPR    REDEFINES  WRK-SKJ-GRP.
                       09  WRK-SKJ-SRYCD       PIC X(09)
                                               OCCURS  3.
                   07  WRK-SKJ-SRYCDTOTAL      PIC 9(14).
                   07  WRK-SKJ-KAISU           PIC 9(02).
                   07  WRK-SKJ-MONEY           PIC 9(07).
      *    ¿©»ö¥ï¡¼¥¯£²¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           03  WRK-SKJ2-TABLE.
               05  WRK-SKJ2-TBL          OCCURS  31.
                   07  WRK-SKJ2-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ2-FUKUSU         PIC X(09).
                   07  WRK-SKJ2-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£³¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           03  WRK-SKJ3-TABLE.
               05  WRK-SKJ3-TBL          OCCURS  31.
                   07  WRK-SKJ3-HKNCOMBI-DAY   PIC 9(03).
                   07  WRK-SKJ3-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ3-KAISU          PIC 9(02).
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
                   07  WRK-SKJ7-DAY-TBL.
                       09  WRK-SKJ7-DAY         PIC 9(01)  OCCURS  31.
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
                   07  WRK-KANKYO-DAY-TBL.
                       09  WRK-KANKYO-DAY       PIC 9(01)  OCCURS  31.
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           03  WRK-TENSU-OCC                   OCCURS  200.
               05  WRK-TENSU-SRYCD             PIC X(09).
               05  WRK-TENSU-DATA              PIC X(2000).
           03  WRK-TENSU-CD                    PIC X(09).
           03  TTNS-IDX                        PIC 9(03).
           03  TTNS-MAX                        PIC 9(03).
      *
      *    ¿©»öÉéÃ´¥ï¡¼¥¯
           03  WRK-SKJFTN-TABLE.
               05  WRK-SKJFTN-TBL        OCCURS  31.
                   07  WRK-SKJFTN-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SKJFTN-SRYCD        PIC X(09).
                   07  WRK-SKJFTN-KAISU        PIC 9(02).
      *
      *    ¿©»öÉéÃ´¥ï¡¼¥¯¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
           03  WRK-SKJFTN2-TABLE.
               05  WRK-SKJFTN2-TBL       OCCURS  93.
                   07  WRK-SKJFTN2-HKNCOMBI-DAY PIC 9(03).
                   07  WRK-SKJFTN2-SRYCD        PIC X(09).
                   07  WRK-SKJFTN2-KAISU        PIC 9(02).
      *
      *    ÁªÄêÎÅÍÜÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-SENTEI.
               05  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      *    ³°Çñ¥³¥á¥ó¥ÈÍÑÆüÉÕÂàÈòÎÎ°è
           03  WRK-GAIHAKU-DAY-TABLE.
               05  WRK-GAIHAKU-DAY             OCCURS  31.
                   07  WRK-GAIHAKU-DAY-ST      PIC  9(02).
                   07  WRK-GAIHAKU-DAY-ED      PIC  9(02).
      *
      *    ÎÅÍÜ¥³¥á¥ó¥ÈÍÑÆüÉÕÂàÈòÎÎ°è
           03  WRK-RYOYO-DAY-TABLE.
               05  WRK-RYOYO-DAY               OCCURS  31.
                   07  WRK-RYOYO-DAY-ST        PIC  9(02).
                   07  WRK-RYOYO-DAY-ED        PIC  9(02).
      *
      *    Â¾ÊÝ¸±¤Ç¤Î¿©Æ²²Ã»»È½ÄêÍÑÎÎ°è
           03  WRK-TAHOKEN-SHOKUDOU-TABLE.
               05  WRK-TAHOKEN-SHOKUDOU-FLG    PIC  X(01)  OCCURS 31.
      *
      *    ºÒ³²ÂÐ±þÎÎ°è¡Ê¿©»öÉéÃ´¡¢À¸³èÎÅÍÜÉéÃ´¤òµ­Ï¿¤·¤Ê¤¤È½ÄêÍÑ¡Ë
           03  WRK-SAIGAI-AREA.
               07  WRK-SAIGAI-962              PIC X(01)  OCCURS  31.
               07  WRK-SAIGAI-963              PIC X(01)  OCCURS  31.
               07  WRK-SAIGAI-959              PIC X(01)  OCCURS  31.
      *
      *    ºÒ³²ÂÐ±þÎÎ°è
           03  WRK-SAIGAI-AREA2.
      *        ÆÃÊÌÎÅÍÜ¡ÊµìÆÃÊÌÆü¸ÛÊÝ¸±¡Ë¤ÏºÒ³²¸ºÌÈ¤ÎÂÐ¾Ý³°¤È¤¹¤ëÈ½ÄêÍÑ
               07  WRK-SAIGAI-004              PIC X(01)  OCCURS  31.
      *        ¼çÊÝ¸±¤¬Ìµ¤¤¾ì¹ç¤ÏºÒ³²¸ºÌÈ¤ÎÂÐ¾Ý³°¤È¤¹¤ëÈ½ÄêÍÑ
               07  WRK-SAIGAI-SYUHKN           PIC X(01)  OCCURS  31.
      *
      *    Æþ±¡ÎÁ»»Äê¤ÎÈ½ÄêÍÑÎÎ°è
           03  WRK-NACCT-DAY-TBL.
               05  WRK-NACCT-DAY               PIC  9(03)  OCCURS 31.
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»ÎÎ°è
           03  WRK-KANGOGEN                    PIC X(09)   OCCURS 31.
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "£°£±".
           03  FILLER              PIC X(04)  VALUE "£°£²".
           03  FILLER              PIC X(04)  VALUE "£°£³".
           03  FILLER              PIC X(04)  VALUE "£°£´".
           03  FILLER              PIC X(04)  VALUE "£°£µ".
           03  FILLER              PIC X(04)  VALUE "£°£¶".
           03  FILLER              PIC X(04)  VALUE "£°£·".
           03  FILLER              PIC X(04)  VALUE "£°£¸".
           03  FILLER              PIC X(04)  VALUE "£°£¹".
           03  FILLER              PIC X(04)  VALUE "£±£°".
           03  FILLER              PIC X(04)  VALUE "£±£±".
           03  FILLER              PIC X(04)  VALUE "£±£²".
           03  FILLER              PIC X(04)  VALUE "£±£³".
           03  FILLER              PIC X(04)  VALUE "£±£´".
           03  FILLER              PIC X(04)  VALUE "£±£µ".
           03  FILLER              PIC X(04)  VALUE "£±£¶".
           03  FILLER              PIC X(04)  VALUE "£±£·".
           03  FILLER              PIC X(04)  VALUE "£±£¸".
           03  FILLER              PIC X(04)  VALUE "£±£¹".
           03  FILLER              PIC X(04)  VALUE "£²£°".
           03  FILLER              PIC X(04)  VALUE "£²£±".
           03  FILLER              PIC X(04)  VALUE "£²£²".
           03  FILLER              PIC X(04)  VALUE "£²£³".
           03  FILLER              PIC X(04)  VALUE "£²£´".
           03  FILLER              PIC X(04)  VALUE "£²£µ".
           03  FILLER              PIC X(04)  VALUE "£²£¶".
           03  FILLER              PIC X(04)  VALUE "£²£·".
           03  FILLER              PIC X(04)  VALUE "£²£¸".
           03  FILLER              PIC X(04)  VALUE "£²£¹".
           03  FILLER              PIC X(04)  VALUE "£³£°".
           03  FILLER              PIC X(04)  VALUE "£³£±".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
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
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Þ¥¹¥¿¡Ý
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿¡Ý
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
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
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö
           COPY    "CPORCSRYOCOM.INC".
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
      *    ¿©»öÊÝ¸±¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSSKJKEEP.INC".
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
      *    Æþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSNYURECEDEN.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYURECEDEN-AREA
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
           INITIALIZE                  LNK-NYURECEDEN-HENKYAKU-AREA
           INITIALIZE                  CONS-ETCNYUIN-TBL
           MOVE    ZERO                TO  LNK-NYURECEDEN-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
      *
      *    ¼ç½èÍý
           IF      LNK-NYURECEDEN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°ÙÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤ÎÊÔ½¸
           PERFORM   2103-TOKU-SHOKU-HENSHU-SEC
           IF        FLG-TOKU-SET        =     1
               MOVE   ZERO             TO  LNK-NYURECEDEN-RC
           END-IF
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
           IF    ( LNK-NYURECEDEN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ÊÝ¸±¶èÊ¬¥Á¥§¥Ã¥¯
           IF     LNK-NYURECEDEN-HKNKBN    =   "1"  OR  "2"  OR  "3"
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-NYURECEDEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¤ÎÊÝ¸±ÁÈ¹ç¤»¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4               TO  LNK-NYURECEDEN-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-NYURECEDEN-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        MOVE   NACCT-DAY (IDX1) TO 
                                            WRK-HKNCOMBI-DAY(IDX1)
                    END-PERFORM
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    °åÎÅµ¡´Ø¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   0
                   MOVE   SYS-1001-HOSPSBT
                                       TO  SPA-HOSPSBT
               END-IF
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           IF      LNK-NYURECEDEN-SANTEIYM    <=   "201203"
               PERFORM   200-MAIN-1-SEC
           ELSE
               PERFORM   200-MAIN-2-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ¡ÊÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î¥ì¥»ÅÅÊÖµÑ¥¨¥ê¥¢ÊÔ½¸¡Ë
      *****************************************************************
       200-MAIN-1-SEC            SECTION.
      *
      *Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤È£³¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤Ë¤Ä¤¤¤Æ¼èÆÀ
           INITIALIZE                             ORCSNYUINDAY-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
           MOVE    SPACE                      TO  WRK-SAIGAI-AREA2
      *
      *¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-NYURECEDEN-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-NYURECEDEN-PTID     TO  COMB-PTID
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
                                EVALUATE   LNK-NYURECEDEN-HKNKBN
      *                             ¼ÒÊÝ
                                    WHEN   "1"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"  OR
                                                           "068"  OR
                                                           "069"  OR
                                                           "039"  OR
                                                           "040"
                                        MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
      *                                 ºÒ³²¸ºÌÈÂÐ¾Ý³°¤È¤¹¤ë°Ù¤ËÆÃÊÌÎÅÍÜ
      *                                 ¡ÊµìÆÃÊÌÆü¸Û¡Ë¤òµ­Ï¿
                                        IF   COMB-HKNNUM = "004"
                                           PERFORM  210-TOKUBETSU-SEC
                                           IF  PTHKN-HKNJANUM(1:4) =
                                                                "0404"
                                               MOVE  SPACE  TO  
                                                   WRK-SAIGAI-004(IDX)
                                           ELSE
                                               MOVE  "1"    TO
                                                   WRK-SAIGAI-004(IDX)
                                           END-IF
                                        END-IF
      *                                 ºÒ³²¸ºÌÈÂÐ¾Ý³°¤È¤¹¤ë°Ù¤Ë¼çÊÝ¸±
      *                                 Ìµ¤·¤òµ­Ï¿
                                        IF   COMB-HKNNUM = SPACE
                                            MOVE  "1"  TO  
                                                 WRK-SAIGAI-SYUHKN(IDX)
                                        END-IF
      *                             ¹ñÊÝ
                                    WHEN   "2"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"
                                            CONTINUE
                                        ELSE
                                            MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
      *                             ¹­°èÏ¢¹ç
                                    WHEN   "3"
                                        IF   COMB-HKNNUM = "039"
                                            CONTINUE
                                        ELSE
                                            MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
                                END-EVALUATE
                            END-IF
                       ELSE
                           MOVE    5          TO  LNK-NYURECEDEN-RC
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
                       MOVE    5              TO  LNK-NYURECEDEN-RC
                   END-IF
               END-IF
           END-PERFORM
      *
      *·îËö¤ËÆ±ÆüºÆÆþ±¡»þ¤Î¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
      *    ·îËöÆü¤ò¼èÆÀ
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
           MOVE    SPACE                   TO  WRK-DOUJITSU-KBN
           IF (LNK-NYUINDAY-DOUJITSU-KBN(WRK-GETSUMATSU-DD) = "1") AND
              (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(WRK-GETSUMATSU-DD) = "3")
              IF   LNK-NYURECEDEN-HKNKBN  = LNK-NYUINDAY-DOUJITSU-HKNSYU
                                            (WRK-GETSUMATSU-DD)
                MOVE      "1"              TO  WRK-DOUJITSU-KBN
              END-IF
           END-IF
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
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
      *                         ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                         ¤ÎÂàÈò
                                PERFORM   210-GAIHAKU-MAE-SEC
      *
                                MOVE    1     TO    FLG-RYAKU
                             END-IF
      *
                             MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                             MOVE    1    TO   FLG-OK
                         END-IF
                         IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                         AND   (LNK-NYUINDAY-KBN(IDX1)  =  "1")
                             MOVE    1    TO   FLG-OK
                         END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF  (FLG-OK     =   ZERO)   AND
                   (WRK-DOUJITSU-KBN   =   SPACE)
                   MOVE     4      TO      LNK-NYURECEDEN-RC
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
                   GO   TO    200-MAIN-1-EXT
               END-IF
           ELSE
               MOVE     4      TO      LNK-NYURECEDEN-RC
      *        Æþ±¡²ñ·×¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
               GO   TO    200-MAIN-1-EXT
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *Æþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *·îÆâ¤ËÊ£¿ô¤ÎÆþ±¡´ðËÜ¾ðÊó¡Ê£µ£°£°£°¡Ë¤¬¤Ê¤¤¤«¸¡º÷
      *¤³¤ÎÂÐ±þ¤ÏÆÃÊÌ´ÉÍý²Ã»»ÍÑ¤Î°Ù¡¢Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               PERFORM   210-SYS-5000-CHK-SEC
           END-IF
      *
      *°åÎÅµ¡´Ø¸º»»¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        ¸º»»¾ðÊó¤Î¼èÆÀ¤ÏÊ¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
               IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
                  PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190080570"  TO  WRK-GENSAN2
                                ELSE
                                    MOVE  "190799770"  TO  WRK-GENSAN2
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-INNAI(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  3
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190111690"  TO  WRK-GENSAN3
                                ELSE
                                    MOVE  "190809090"  TO  WRK-GENSAN3
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-IRYO(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  4
                                IF   FLG-RJN    =    ZERO
                                    MOVE  "190111790"  TO  WRK-GENSAN4
                                ELSE
                                    MOVE  "190809190"  TO  WRK-GENSAN4
                                END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-JYOKU(GENSAN-IDX2)
                                END-PERFORM
                        END-EVALUATE
                    END-IF
                  END-PERFORM
               END-IF
      *        ¿©»ö¾ðÊó¤ÎÂàÈò
               PERFORM    VARYING   IDX  FROM  1  BY  1
                          UNTIL     IDX   >    31
      *            Æþ±¡»þ¿©»öÎÅÍÜÈñ
                   IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                       MOVE  SYS-5000-SHOKUJIRYOYOCD
                                          TO WRK-SYS5000-SHOKUCD(1 IDX)
                   END-IF
      *            ÆÃÊÌ´ÉÍý²Ã»»
      *            Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
                   IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
                       IF     SYS-5000-SHOKUCD(1)  NOT =  SPACE
                           IF  SYS-5000-SHOKUKBN(1) =     "2"
                               MOVE  SYS-5000-SHOKUCD(1)
                                          TO WRK-SYS5000-SHOKUCD(2 IDX)
                           END-IF
                       END-IF
                   END-IF
      *             ¿©Æ²²Ã»»¡ÊÆþ±¡²ñ·×¤«¤é¤Î¼èÆÀ¤ËÊÑ¹¹¡Ë
      *             IF     SYS-5000-SHOKUCD(2)  NOT =  SPACE
      *                 IF  SYS-5000-SHOKUKBN(2) =     "2"
      *                     MOVE  SYS-5000-SHOKUCD(2)
      *                                    TO WRK-SYS5000-SHOKUCD(3 IDX)
      *                 END-IF
      *             END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¤³¤ÎÂÐ±þ¤ÏÆÃÊÌ´ÉÍý²Ã»»ÍÑ¤Î°Ù¡¢Ê¿À®£±£¸Ç¯£³·îËø¤È¤¹¤ë
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               IF        WRK-SYS5000-COUNT    =     ZERO
                     CONTINUE
               ELSE
                     PERFORM  210-SYS-5000-GET-SEC
               END-IF
           END-IF
      *
      *¿©Æ²²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key43"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            ¿©Æ²²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
      *                  Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç¤Î¤ß¥»¥Ã¥È
                           IF  WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                          "197000110"
                               MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
      *            ¿©Æ²²Ã»»¤ÎÂàÈò¡ÊÆ±ÆüºÆÆþ±¡Æü¡Ë
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF  (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = "1")
                       AND (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                       AND (LNK-NYURECEDEN-HKNKBN  =
                                     LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1))
                       AND (NACCT-DAY(IDX1)    NOT =  ZERO)
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
                 MOVE    "key43"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           DISPLAY "ORCSNYURECEDEN"
           DISPLAY "SPA-HOSPSBT = " SPA-HOSPSBT
      *
      *    Í­¾²¿ÇÎÅ½ê¤ÎÆþ±¡ÎÁ²Ã»»¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           IF   (SPA-HOSPSBT                 =    2)    AND
                (LNK-NYURECEDEN-SANTEIYM    <=   "200803")
      *
      *      Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
      *      ¡ÊÊ£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡Ë
             MOVE    ZERO                    TO  KSN-IDX
             INITIALIZE                          NYUINACCT-REC
             MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
             MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
             MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
             MOVE    "6"                     TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM 910-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1) = "1"))
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190117070
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-FUKUSU-NYUKSN
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-FUKUSU-NYUKSN-ZAITEN
                           MOVE   NACCT-DAY(IDX1)
                                        TO WRK-FUKUSU-NYUKSN-DAY(IDX1)
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *      Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
      *      ¡Ê´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡Ë
             MOVE    ZERO                    TO  KSN-IDX
             INITIALIZE                          NYUINACCT-REC
             MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
             MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
             MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
             MOVE    "6"                     TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM 910-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1) = "1"))
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190109670
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-KANGO-NYUKSN
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-KANGO-NYUKSN-ZAITEN
                           MOVE   NACCT-DAY(IDX1)
                                        TO WRK-KANGO-NYUKSN-DAY(IDX1)
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *    Í­¾²¿ÇÎÅ½ê¤ÎÆþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë¡ÊÊ¿À®£²£°Ç¯£´·î¤«¤é¡Ë
      *    ¢¨Ãí²Ã»»¥³¡¼¥É¤ÎÉÕÈÖ¤µ¤ì¤Æ¤¤¤ë¤â¤Î
           IF   (SPA-HOSPSBT                 =    2)    AND
                (LNK-NYURECEDEN-SANTEIYM    >=   "200804")
      *
      *      Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
             MOVE    ZERO                    TO  KSN-IDX
             INITIALIZE                          NYUINACCT-REC
             MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
             MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
             MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
             MOVE    "6"                     TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM 910-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1) = "1"))
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
      *
      *                  Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190126470
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(1)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(1)
                           MOVE   NACCT-DAY(IDX1)
                                     TO WRK-CHUKSN-NYUKSN-DAY(1 IDX1)
                         END-IF
      *
      *                  °å»ÕÇÛÃÖ²Ã»»£±¡ÊÊ¿À®£²£²Ç¯£´·î¿·Àß¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190135570
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(15)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(15)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(15 IDX1)
                         END-IF
      *
      *                  °å»ÕÇÛÃÖ²Ã»»£²¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç°å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190117070
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(2)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(2)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(2 IDX1)
                         END-IF
      *
      *                  ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190126570
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(3)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(3)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(3 IDX1)
                         END-IF
      *
      *                  ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190109670
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(4)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(4)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(4 IDX1)
                         END-IF
      *
      *                  Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤ÇÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190126670
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(5)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(5)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(5 IDX1)
                         END-IF
      *
      *                  Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤ÇÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190126770
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(6)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(6)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(6 IDX1)
                         END-IF
      *
      *                  êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190122670
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(13)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(13)
                           MOVE   NACCT-DAY(IDX1)
                                     TO WRK-CHUKSN-NYUKSN-DAY(13 IDX1)
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *    ÉÂ±¡¤ÎÆþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë¡ÊÊ¿À®£²£°Ç¯£´·î¤«¤é¡Ë
      *    ¢¨Ãí²Ã»»¥³¡¼¥É¤ÎÉÕÈÖ¤µ¤ì¤Æ¤¤¤ë¤â¤Î
           IF   (SPA-HOSPSBT                 =    1)    AND
                (LNK-NYURECEDEN-SANTEIYM    >=   "200804")
      *
      *      Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
             MOVE    ZERO                    TO  KSN-IDX
             INITIALIZE                          NYUINACCT-REC
             MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
             MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
             MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
             MOVE    "6"                     TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM 910-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1) = "1"))
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
      *
      *                  ¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190055470
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(7)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(7)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(7 IDX1)
                         END-IF
      *
      *                  ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190111370
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(8)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(8)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(8 IDX1)
                         END-IF
      *
      *                  ¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190120970
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(9)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(9)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(9 IDX1)
                         END-IF
      *
      *                  ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190119210
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(10)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(10)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(10 IDX1)
                         END-IF
      *
      *                  ÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£²
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190117570
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(11)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(11)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(11 IDX1)
                         END-IF
      *
      *                  êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190121970
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(12)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(12)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(12 IDX1)
                         END-IF
      *
      *                  ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190129870
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(14)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(14)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(14 IDX1)
                         END-IF
      *
      *                  ½¼¼ÂÃÊ³¬£Á²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190139470
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(16)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(16)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(16 IDX1)
                         END-IF
      *
      *                  ½¼¼ÂÃÊ³¬£Â²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190139570
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(17)
                           MOVE   NACCT-ZAITEN
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(17)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(17 IDX1)
                         END-IF
      *
      *                  ¹âÅÙ°åÎÅÂÎÀ©²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190139670
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                    TO  WRK-CHUKSN-NYUKSN(18)
                           MOVE   NACCT-ZAITEN
                                    TO  WRK-CHUKSN-NYUKSN-ZAITEN(18)
                           MOVE   NACCT-DAY(IDX1)
                                    TO  WRK-CHUKSN-NYUKSN-DAY(18 IDX1)
                         END-IF
      *
      *                  ÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£±¡Ê£²¼ïÎà°Ê²¼¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190141270
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                      TO  WRK-CHUKSN-NYUKSN(22)
                           MOVE   NACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(22)
                           MOVE   NACCT-DAY(IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(22 IDX1)
                         END-IF
      *
      *                  µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190140870
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                      TO  WRK-CHUKSN-NYUKSN(23)
                           MOVE   NACCT-ZAITEN  
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(23)
                           MOVE   NACCT-DAY(IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(23 IDX1)
                         END-IF
      *
      *                  ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó½¼¼Â²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190140970
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                    TO  WRK-CHUKSN-NYUKSN(24)
                           MOVE   NACCT-ZAITEN
                                    TO  WRK-CHUKSN-NYUKSN-ZAITEN(24)
                           MOVE   NACCT-DAY(IDX1)
                                    TO WRK-CHUKSN-NYUKSN-DAY(24 IDX1)
                         END-IF
      *
      *                  ºßÂð½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþ²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190136170
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                    TO  WRK-CHUKSN-NYUKSN(29)
                           MOVE   NACCT-ZAITEN
                                    TO  WRK-CHUKSN-NYUKSN-ZAITEN(29)
                           MOVE   NACCT-DAY(IDX1)
                                    TO WRK-CHUKSN-NYUKSN-DAY(29 IDX1)
                         END-IF
      *
      *                 ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190141070
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                    TO  WRK-CHUKSN-NYUKSN(32)
                           MOVE   NACCT-ZAITEN
                                    TO  WRK-CHUKSN-NYUKSN-ZAITEN(32)
                           MOVE   NACCT-DAY(IDX1)
                                    TO WRK-CHUKSN-NYUKSN-DAY(32 IDX1)
                         END-IF
      *
      *                  ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê´ð½àÌ¤Å¬¹ç¡Ë
                         IF   NACCT-SRYCDTOTAL(6:9)  =   190142370
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                     TO  WRK-CHUKSN-NYUKSN(33)
                           MOVE   NACCT-ZAITEN  
                                     TO  WRK-CHUKSN-NYUKSN-ZAITEN(33)
                           MOVE   NACCT-DAY(IDX1)
                                     TO  WRK-CHUKSN-NYUKSN-DAY(33 IDX1)
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
             END-IF
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
             MOVE    "key17"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
      *    ¢¨Ãí²Ã»»¥³¡¼¥É¤ÎÉÕÈÖ¤µ¤ì¤Æ¤¤¤Ê¤¤¤â¤Î
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1) = "1"))
                       AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5)
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO)
                         IF (((SPA-HOSPSBT                 =    2) AND
                             (LNK-NYURECEDEN-SANTEIYM <= "200803")) AND
                            ((NACCT-SRYCDTOTAL(6:9) =  190117070)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190109670)))
                                        OR
                            (((SPA-HOSPSBT                 =    2) AND
                             (LNK-NYURECEDEN-SANTEIYM >= "200804")) AND
                            ((NACCT-SRYCDTOTAL(6:9) =  190126470)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190135570)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190117070)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190126570)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190109670)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190126670)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190126770)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190122670)))
                                        OR
                            (((SPA-HOSPSBT                 =    1) AND
                             (LNK-NYURECEDEN-SANTEIYM >= "200804")) AND
                            ((NACCT-SRYCDTOTAL(6:9) =  190055470)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190111370)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190120970)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190119210)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190117570)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190121970)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190129870)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190139470)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190139570)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190139670)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190141270)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190140870)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190140970)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190136170)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190141070)  OR
                             (NACCT-SRYCDTOTAL(6:9) =  190142370)))
                             CONTINUE
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO WRK-NYUKSN-DAY(KSN-IDX IDX1)
                         END-IF
      *
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
      *
      *    Ãí²Ã»»¤Ç¤¢¤ëÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¡Ë
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-NYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  ACCT-SRYYM
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
                             MOVE  SRY-SRYCD(1)   TO  
                                            ACCT-SRYCDTOTAL(6:9)   
                         END-IF
                     END-IF
      *              ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                     MOVE     "tbl_sryact"   TO  MCP-TABLE
                     MOVE     "key"          TO  MCP-PATHNAME
                     PERFORM   900-CLOSE-SEC
      *
                     IF  ((LNK-NYURECEDEN-SANTEIYM <= "200803") AND
                          (ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-KEIKAKU-GENSAN1
                                        OR  CONS-KEIKAKU-GENSAN2
                                        OR  CONS-INNAI-GENSAN1
                                        OR  CONS-INNAI-GENSAN2
                                        OR  CONS-ANZEN-GENSAN1
                                        OR  CONS-ANZEN-GENSAN2
                                        OR  CONS-JYOKU-GENSAN1
                                        OR  CONS-JYOKU-GENSAN2
                                        OR  CONS-NICHIJOU-KSN1
                                        OR  CONS-NICHIJOU-KSN2
                                        OR  CONS-CHIHOU-KSN1
                                        OR  CONS-CHIHOU-KSN2
                                        OR  CONS-JYOKU-KSN1
                                        OR  CONS-JYOKU-KSN2
                                        OR  CONS-TOKUSEIYAKU-KSN))
                                   OR
                         ((LNK-NYURECEDEN-SANTEIYM >= "200804") AND
                          (ACCT-SRYCDTOTAL(6:9)  = "190055470"  OR 
                                                   "190120970"  OR
                                                   "190119210"  OR
                                                   "190117570"  OR
                                                   "190121970"  OR
                                                   "190122670"  OR
                                                   "190139470"  OR
                                                   "190139570"  OR
                                                   "190139670"  OR
                                                   "190140270"  OR
                                                   "190140370"  OR
                                                   "190135070"  OR
                                                   "190141270"  OR
                                                   "190140870"  OR
                                                   "190140970"  OR
                                                   "190136170"  OR
                                                   "190141070" ))
                                   OR
                         ((LNK-NYURECEDEN-SANTEIYM >= "201004") AND
                          (ACCT-SRYCDTOTAL(6:9)  = "190076570"  OR
                                                   "190076670"  OR
                                                   "190127510"  OR 
                                                   "190127610" ))
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                 (LNK-NYUINDAY-KBN(IDX1) = "1"))
                           AND   (WRK-GAIHAKU-KBN(IDX1)  = ZERO OR 4 OR
                                                                   5)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                           AND   (ACCT-HKNCOMBI      =
                                                WRK-HKNCOMBI-DAY(IDX1))
                            IF   LNK-NYURECEDEN-SANTEIYM <= "200803"
                               EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-KEIKAKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN1(IDX1)
                                WHEN  CONS-KEIKAKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN2(IDX1)
      *                         ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-INNAI-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN1(IDX1)
                                WHEN  CONS-INNAI-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN2(IDX1)
      *                         °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
                                WHEN  CONS-ANZEN-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN1(IDX1)
                                WHEN  CONS-ANZEN-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN2(IDX1)
      *                         êóáìÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-JYOKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN1(IDX1)
                                WHEN  CONS-JYOKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN2(IDX1)
      *                         Æü¾ïÀ¸³è¾ã³²²Ã»»
                                WHEN  CONS-NICHIJOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN1(IDX1)
                                WHEN  CONS-NICHIJOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN2(IDX1)
      *                         Ç§ÃÎ¾É²Ã»»
                                WHEN  CONS-CHIHOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN1(IDX1)
                                WHEN  CONS-CHIHOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN2(IDX1)
      *                         êóáì´µ¼Ô´ÉÍý²Ã»»
                                WHEN  CONS-JYOKU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN1(IDX1)
                                WHEN  CONS-JYOKU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN2(IDX1)
      *                         ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
                                WHEN  CONS-TOKUSEIYAKU-KSN
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-TOKUSEIYAKU-KSN(IDX1)
                               END-EVALUATE
                             ELSE
                               EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         ¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë
                                WHEN  "190055470"
                                  MOVE   "190055470"
                                      TO  WRK-CHUKSN-NYUKSN(7)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(7)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO  WRK-CHUKSN-NYUKSN-DAY(7 IDX1)
      *                         ¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë
                                WHEN  "190120970"
                                  MOVE   "190120970"
                                      TO  WRK-CHUKSN-NYUKSN(9)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(9)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO  WRK-CHUKSN-NYUKSN-DAY(9 IDX1)
      *                         ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
                                WHEN  "190119210"
                                  MOVE   "190119210"
                                      TO  WRK-CHUKSN-NYUKSN(10)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(10)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(10 IDX1)
      *                         ÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£²
                                WHEN  "190117570"
                                  MOVE   "190117570"
                                      TO  WRK-CHUKSN-NYUKSN(11)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(11)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(11 IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
                                WHEN  "190121970"
                                  MOVE   "190121970"
                                      TO  WRK-CHUKSN-NYUKSN(12)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(12)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(12 IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                                WHEN  "190122670"
                                  MOVE   "190122670"
                                      TO  WRK-CHUKSN-NYUKSN(13)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(13)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(13 IDX1)
      *                        ¤³¤³¤«¤éÊ¿À®£²£²Ç¯£´·î¿·ÀßÊ¬
      *                         ½¼¼ÂÃÊ³¬£Á²Ã»»
                                WHEN  "190139470"
                                  MOVE   "190139470"
                                      TO  WRK-CHUKSN-NYUKSN(16)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(16)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(16 IDX1)
      *                         ½¼¼ÂÃÊ³¬£Â²Ã»»
                                WHEN  "190139570"
                                  MOVE   "190139570"
                                      TO  WRK-CHUKSN-NYUKSN(17)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(17)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(17 IDX1)
      *                         ¹âÅÙ°åÎÅÂÎÀ©²Ã»»
                                WHEN  "190139670"
                                  MOVE   "190139670"
                                      TO  WRK-CHUKSN-NYUKSN(18)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(18)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(18 IDX1)
      *                         ¾®»ù²Ã»»¡ÊÆÃÄê½¸Ãæ¡Ë¡Ê£·Æü°ÊÆâ¡Ë
                                WHEN  "190140270"
                                  MOVE   "190140270"
                                      TO  WRK-CHUKSN-NYUKSN(19)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(19)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(19 IDX1)
      *                         ¾®»ù²Ã»»¡ÊÆÃÄê½¸Ãæ¡Ë¡Ê£¸Æü°Ê¾å£±£´ÆüÌ¤Ëþ¡Ë
                                WHEN  "190140370"
                                  MOVE   "190140370"
                                      TO  WRK-CHUKSN-NYUKSN(20)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(20)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(20 IDX1)
      *                         ½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»
                                WHEN  "190135070"
                                  MOVE   "190135070"
                                      TO  WRK-CHUKSN-NYUKSN(21)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(21)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(21 IDX1)
      *                         ÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£±¡Ê£²¼ïÎà°Ê²¼¡Ë
                                WHEN  "190141270"
                                  MOVE   "190141270"
                                      TO  WRK-CHUKSN-NYUKSN(22)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(22)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(22 IDX1)
      *                         µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
                                WHEN  "190140870"
                                  MOVE   "190140870"
                                      TO  WRK-CHUKSN-NYUKSN(23)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(23)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(23 IDX1)
      *                         ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó½¼¼Â²Ã»»
                                WHEN  "190140970"
                                  MOVE   "190140970"
                                      TO  WRK-CHUKSN-NYUKSN(24)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(24)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(24 IDX1)
      *                         Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
                                WHEN  "190076570"
                                  MOVE   "190076570"
                                      TO  WRK-CHUKSN-NYUKSN(25)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(25)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(25 IDX1)
      *                         ½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
                                WHEN  "190076670"
                                  MOVE   "190076670"
                                      TO  WRK-CHUKSN-NYUKSN(26)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(26)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(26 IDX1)
      *                         Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
                                WHEN  "190127510"
                                  MOVE   "190127510"
                                      TO  WRK-CHUKSN-NYUKSN(27)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(27)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(27 IDX1)
      *                         ½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
                                WHEN  "190127610"
                                  MOVE   "190127610"
                                      TO  WRK-CHUKSN-NYUKSN(28)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(28)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(28 IDX1)
      *                         ºßÂð½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþ²Ã»»
                                WHEN  "190136170"
                                  MOVE   "190136170"
                                      TO  WRK-CHUKSN-NYUKSN(29)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(29)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(29 IDX1)
      *                         ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
                                WHEN  "190141070"
                                  MOVE   "190141070"
                                      TO  WRK-CHUKSN-NYUKSN(32)
                                  MOVE   ACCT-ZAITEN
                                      TO  WRK-CHUKSN-NYUKSN-ZAITEN(32)
                                  MOVE   ACCT-DAY(1 IDX1)
                                      TO WRK-CHUKSN-NYUKSN-DAY(32 IDX1)
                               END-EVALUATE
                             END-IF                                  
      *
                           END-IF
                       END-PERFORM
                     ELSE
                      IF    (LNK-NYURECEDEN-SANTEIYM  <=  "201003") AND
                          (((CONS-TAIIN1   =  ACCT-SRYCDTOTAL(6:9)) AND
                            (ACCT-MEISAISU =  1))                   OR
                           ((CONS-TAIIN2   =  ACCT-SRYCDTOTAL(6:9)) AND
                            (ACCT-MEISAISU =  1))                   OR
                           ((CONS-TAIIN3   =  ACCT-SRYCDTOTAL(6:9)) AND
                            (ACCT-MEISAISU =  1))                   OR
                           ((CONS-TAIIN4   =  ACCT-SRYCDTOTAL(6:9)) AND
                            (ACCT-MEISAISU =  2)))
      *                    Âà±¡²Ã»»¤ÏÆÉ¤ßÈô¤Ð¤¹¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¡Ë
                           CONTINUE
                      ELSE
      *                ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
      *                ¿ÇÎÅ²ñ·×¤«¤é¿ÇÎÅ¹Ô°Ù¤ò¸¡º÷¤·¡¢Æþ±¡ÎÁ²Ã»»¤Î¿ÇÎÅ
      *                ¥³¡¼¥É¤òÆÃÄê¤¹¤ë
                       IF         ACCT-MEISAISU    =      1
                             CONTINUE
                       ELSE
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
                                  MOVE  SRY-SRYCD(1)   TO  
                                                  ACCT-SRYCDTOTAL(6:9)   
                              END-IF
                          END-IF
      *                   ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                          MOVE     "tbl_sryact"   TO  MCP-TABLE
                          MOVE     "key"          TO  MCP-PATHNAME
                          PERFORM   900-CLOSE-SEC
                       END-IF
      *                
                       IF   (LNK-NYURECEDEN-SANTEIYM  <= "201003")  AND
                            (ACCT-SRYCDTOTAL(6:9)  =   CONS-TAIIN1  OR
                                                       CONS-TAIIN2  OR
                                                       CONS-TAIIN3)
      *                    Âà±¡²Ã»»¤ÏÆÉ¤ßÈô¤Ð¤¹¡ÊÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¡Ë
                           CONTINUE
                       ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                      UNTIL     IDX1   >   31
                           IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                 (LNK-NYUINDAY-KBN(IDX1) = "1"))
                           AND   (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4
                                                                 OR 5)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                           AND   (ACCT-HKNCOMBI      =
                                                WRK-HKNCOMBI-DAY(IDX1))
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
                                WHEN  CONS-ETCNYUIN-KSN(1)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(1 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(2)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(2 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(3)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(3 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(4)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(4 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(5)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(5 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(6)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(6 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(7)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(7 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(8)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(8 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(9)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(9 IDX1)
                                WHEN  CONS-ETCNYUIN-KSN(10)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(10 IDX1)
                                WHEN  OTHER
                                  ADD     1     TO   ETC-IDX
                                  MOVE    ACCT-SRYCDTOTAL(6:9) TO  
                                           CONS-ETCNYUIN-KSN(ETC-IDX)
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ETCNYUIN-KSN(ETC-IDX
                                                                 IDX1)
                              END-EVALUATE
      *
                           END-IF
                         END-PERFORM
                       END-IF
                      END-IF
                     END-IF
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
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  =    SPACE)
                   MOVE   "90"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
      *        ÊÝ¸±ÁÈ¹ç¤»
               MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                        TO  WRK-SO-HKNCOMBI-DAY(IDX1)
               MOVE     ZERO                TO  WRK-MEISAISU
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
                                   MOVE  SENCHG-SENTEISRYCD
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                               END-IF
                          END-IF
                          MOVE     "tbl_senteicdchg"   TO  MCP-TABLE
                          MOVE     "key2"              TO  MCP-PATHNAME
                         PERFORM   900-CLOSE-SEC
                       END-IF
                   END-IF
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
      *
      *        Â¾°åÎÅµ¡´Ø¼õ¿Ç¡Ê£³£°¡ó¸º»»¤È£·£°¡ó¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090")
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" )
               AND     (WRK-SHOKIKSN(IDX1)      NOT =   SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        É¸·ç
               IF      (WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090")
               AND     (WRK-HYOUKETSU(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOUKETSU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        Äê¿ôÄ¶²á
               IF      (WRK-GAIHAKU(IDX1)       =   SPACE       OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090")
               AND     (WRK-TEISU(IDX1)       NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-TEISU(IDX1)  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
      *        ³°Çñ
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                 IF      (WRK-GAIHAKU(IDX1)     =
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090")
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
      *        ¿ÇÎÅ½ê¤ÎÆþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
               IF  (SPA-HOSPSBT                 =    2)    AND  
                   (LNK-NYURECEDEN-SANTEIYM     <=   "200803")
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÊ£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡Ë
                   IF    WRK-FUKUSU-NYUKSN-DAY(IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-FUKUSU-NYUKSN
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡Ë
                   IF    WRK-KANGO-NYUKSN-DAY(IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-KANGO-NYUKSN
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
               END-IF
      *
      *        ¿ÇÎÅ½ê¤ÎÆþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡ÊÊ¿À®£²£°Ç¯£´·î¤«¤éÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¡Ë
               IF  (SPA-HOSPSBT                 =    2)    AND  
                   (LNK-NYURECEDEN-SANTEIYM     >=   "200804")  AND
                   (LNK-NYURECEDEN-SANTEIYM     <=   "201003")
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(1 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(1)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê°å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(2 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(2)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(3 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(3)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(4 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(4)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(5 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(5)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(6 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(6)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊêóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(13 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(13)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
               END-IF
      *
      *        ¿ÇÎÅ½ê¤ÎÆþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡ÊÊ¿À®£²£²Ç¯£´·î¤«¤é¡Ë
               IF  (SPA-HOSPSBT                 =    2)    AND  
                   (LNK-NYURECEDEN-SANTEIYM     >=   "201004")
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(1 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(1)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê°å»ÕÇÛÃÖ²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(15 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(15)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê°å»ÕÇÛÃÖ²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(2 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(2)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(4 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(4)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(3 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(3)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(6 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(6)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡ÊÌë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(5 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(5)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊêóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(13 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(13)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐ°Ê¾å¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(25 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(25)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐ°Ê¾å¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(26 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(26)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐÌ¤Ëþ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(27 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(27)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐÌ¤Ëþ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(28 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(28)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊºßÂð½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþ²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(29 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(29)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
               END-IF
      *
      *        ÉÂ±¡¤ÎÆþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡ÊÊ¿À®£²£°Ç¯£´·î¤«¤éÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¡Ë
               IF  (SPA-HOSPSBT                 =    1)    AND  
                   (LNK-NYURECEDEN-SANTEIYM     >=   "200804")  AND
                   (LNK-NYURECEDEN-SANTEIYM     <=   "201003")
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(7 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(7)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(8 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(8)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(9 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(9)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(10 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(10)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÈóÄê·¿¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(11 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(11)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊêóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(12 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(12)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(14 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(14)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
               END-IF
      *
      *        ÉÂ±¡¤ÎÆþ±¡ÎÁ²Ã»»¥»¥Ã¥È¡ÊÊ¿À®£²£²Ç¯£´·î¤«¤é¡Ë
               IF  (SPA-HOSPSBT                 =    1)    AND
                   (LNK-NYURECEDEN-SANTEIYM     >=   "201004")
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡ÊÆÃ¼ì¼À´µ¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(7 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(7)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(8 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(8)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¿Í¹©¸ÆµÛ´ï»ÈÍÑ²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(9 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(9)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½¼¼ÂÃÊ³¬£Á²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(16 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(16)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½¼¼ÂÃÊ³¬£Â²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(17 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(17)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¹âÅÙ°åÎÅÂÎÀ©²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(18 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(18)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¾®»ù²Ã»»£·Æü°ÊÆâ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(19 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(19)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¾®»ù²Ã»»£¸Æü°Ê¾å£±£´ÆüÌ¤Ëþ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(20 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(20)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(10 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(10)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(21 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(21)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£²¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(11 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(11)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÈóÄê·¿¹³Àº¿ÀÉÂÌô²Ã»»£±¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(22 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(22)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊêóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(12 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(12)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(14 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(14)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            Æþ±¡²ñ·×²Ã»»¾ðÊó¡Ê½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê´ð½àÌ¤Å¬¹ç¡Ë¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(33 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(33)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊµÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(23 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(23)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó½¼¼Â²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(24 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(24)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐ°Ê¾å¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(25 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(25)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐ°Ê¾å¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(26 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(26)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐÌ¤Ëþ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(27 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(27)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»£¶ºÐÌ¤Ëþ¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(28 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(28)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡ÊºßÂð½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþ²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(29 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(29)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×²Ã»»¾ðÊó¡Ê¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ë
                   IF    WRK-CHUKSN-NYUKSN-DAY(32 IDX1)  =  ZERO
                       CONTINUE
                   ELSE
                       ADD   1    TO  WRK-MEISAISU
                       MOVE  WRK-CHUKSN-NYUKSN(32)
                                  TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                   END-IF
      *
               END-IF
      *
      *        Æþ±¡²ñ·×²Ã»»¾ðÊó
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD   1               TO  WRK-MEISAISU
                      MOVE  WRK-NYUKSN(KSN-IDX)
                                 TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                  END-IF
               END-PERFORM
      *
               IF  LNK-NYURECEDEN-SANTEIYM     <=    "200803"
      *            Æü¾ïÀ¸³è¾ã³²²Ã»»
                   IF         WRK-NICHIJOU-KSN1(IDX1)   NOT =   ZERO
                       ADD      1                 TO  WRK-MEISAISU
                       MOVE   CONS-NICHIJOU-KSN1  TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
                   IF         WRK-NICHIJOU-KSN2(IDX1)   NOT =   ZERO
                       ADD      1                 TO  WRK-MEISAISU
                       MOVE   CONS-NICHIJOU-KSN2  TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
      *            Ç§ÃÎ¾É²Ã»»
                   IF         WRK-CHIHOU-KSN1(IDX1)     NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-CHIHOU-KSN1     TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
                   IF         WRK-CHIHOU-KSN2(IDX1)     NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-CHIHOU-KSN2     TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
      *            êóáì´µ¼Ô´ÉÍý²Ã»»
                   IF         WRK-JYOKU-KSN1(IDX1)      NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-JYOKU-KSN1      TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
                   IF         WRK-JYOKU-KSN2(IDX1)      NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-JYOKU-KSN2      TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
      *            ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
                   IF         WRK-TOKUSEIYAKU-KSN(IDX1) NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-TOKUSEIYAKU-KSN TO  WRK-SO-SRYCD
                                               (IDX1 WRK-MEISAISU)
                   END-IF
               END-IF
      *
      *        ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»
               PERFORM    VARYING   ETC-IDX   FROM  1  BY  1
                          UNTIL     ETC-IDX  >  10
                   IF  CONS-ETCNYUIN-KSN(ETC-IDX)  NOT =  SPACE
                     IF   WRK-ETCNYUIN-KSN(ETC-IDX IDX1) NOT =   ZERO
                       ADD      1                  TO  WRK-MEISAISU
                       MOVE   CONS-ETCNYUIN-KSN(ETC-IDX)
                                   TO  WRK-SO-SRYCD(IDX1 WRK-MEISAISU)
                     END-IF
                   END-IF
               END-PERFORM
      *        Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
               IF         WRK-KEIKAKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN1 TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-KEIKAKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN2 TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-INNAI-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-INNAI-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
               IF         WRK-ANZEN-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-ANZEN-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-JYOKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN1   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN2   TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¡Ë
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-INNAI(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN2       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN2-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        ÁªÄêÆþ±¡»þ¤Î³°Çñ
               IF   (TBL-SENTEI(IDX1)    =         1)   AND
                    (WRK-GAIHAKU-KBN(IDX1)     NOT =  ZERO)
                   EVALUATE   WRK-GAIHAKU-KBN(IDX1)
      *                ³°Çñ
                       WHEN   3
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
      *                Â¾°åÎÅµ¡´Ø¼õ¿Ç
                       WHEN   4
                           IF    LNK-NYURECEDEN-SANTEIYM  >=  "201004"
                               CONTINUE
                           ELSE
                               INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (WRK-SO-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      30)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
      *        ²ó¿ô¾ðÊó¤Î¥»¥Ã¥È
               IF    WRK-SO-SRYCD(IDX1 1) NOT  =   SPACE
                   MOVE     1      TO    WRK-SO-DAY(IDX1 IDX1)
               END-IF
           END-PERFORM
      *
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      *          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      *          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      *          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      *          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *     END-PERFORM
      *
      *    °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          ORCSRYOCOM-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-RYOCOM-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-RYOCOM-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-RYOCOM-SANTEIYM
           CALL    "ORCSRYOCOM"     USING  ORCSRYOCOM-AREA
                                           SPA-AREA
           IF      LNK-RYOCOM-RC           NOT =  ZERO
               DISPLAY  "LNK-RYOCOM-RC = " LNK-RYOCOM-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY   1
      *               UNTIL    IDX1    >   31
      *       DISPLAY "IDX1 = " IDX1
      *       DISPLAY "LNK-RYOCOM-NYUINKBN = " LNK-RYOCOM-NYUINKBN(IDX1)
      *       DISPLAY "LNK-RYOCOM-MEISAISU = " LNK-RYOCOM-MEISAISU(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL2 = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL2(IDX1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 1) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 2) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 2)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 3) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 3)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 4) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 4)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 5) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 5)
      *       DISPLAY "LNK-RYOCOM-ADL-SRYCD (IDX1) = "
      *                             LNK-RYOCOM-ADL-SRYCD(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN-MOJI (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
      *     END-PERFORM
      *
           MOVE      ZERO      TO   MATOME-IDX
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)
                   IF    (LNK-RYOCOM-NYUINKBN(IDX1)   =   "1")   AND
                         (LNK-RYOCOM-MEISAISU(IDX1)   NOT =  ZERO)
                       ADD   1              TO  MATOME-IDX
                       MOVE  MATOME-IDX     TO  WRK-IRYOADL-DAY(IDX1)
                       MOVE  MATOME-IDX     TO  WRK-SO-IRYOADL-DAY(IDX1)
                   END-IF
                   ADD   1                  TO  WRK-SO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SO-MEISAISU(IDX3)  =
                                             WRK-SO-MEISAISU(IDX1)) AND
                             (WRK-SO-HKNCOMBI-DAY(IDX3)  =
                                         WRK-SO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SO-SRYCDTOTAL(IDX3)  =
                                         WRK-SO-SRYCDTOTAL(IDX1))   AND
                             (WRK-SO-TEN(IDX3)  =
                                         WRK-SO-TEN(IDX1))
                             IF (LNK-RYOCOM-NYUINKBN(IDX3) = "1")   AND
                                (LNK-RYOCOM-MEISAISU(IDX3) NOT =  ZERO)
                                 MOVE  MATOME-IDX    TO  
                                                 WRK-IRYOADL-DAY(IDX3)
                             END-IF
                             ADD     1    TO    WRK-SO-KAISU(IDX1)
                             MOVE    1    TO    WRK-SO-DAY(IDX1 IDX3)
                             IF  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                                             NOT = ZERO
                                 MOVE  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                          TO  WRK-SO-GAIHAKU-KBN
                                                            (IDX1 IDX3)
                             END-IF
                             INITIALIZE   WRK-NYUIN-SORT-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      **     DISPLAY "===  DAY-SET2 ======"
      **     PERFORM   VARYING   IDX1    FROM   1  BY  1
      **       UNTIL   IDX1     >   31
      **       DISPLAY "IDX1 = "IDX1
      **         DISPLAY "      ----5----0----5----0----5----0-"
      **         DISPLAY "DAY = " WRK-SO-DAY-TBL(IDX1)
      **     END-PERFORM
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤ÎÆ±°ìºÞ¤Þ¤È¤á½èÍý
           IF    (MATOME-IDX     NOT =   ZERO)   AND
                 (LNK-RYOCOM-RC  =       ZERO)
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö¤ÎÊÖµÑ·ë²Ì¤ò¤Þ¤È¤á¥ï¡¼¥¯¤ËÅ¾Á÷
             PERFORM      VARYING    IDX1   FROM   1  BY  1
               UNTIL      IDX1   >   31
                 IF   (LNK-RYOCOM-NYUINKBN(IDX1)  NOT =   ZERO)   AND
                      (LNK-RYOCOM-MEISAISU(IDX1)  NOT =   ZERO)
                     MOVE   LNK-RYOCOM-MEISAISU(IDX1)
                                   TO  WRK-IRYOADL-MEISAISU(IDX1)
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL  IDX2 >  LNK-RYOCOM-MEISAISU(IDX1)
                         MOVE  LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX2)
                                     TO  WRK-IRYOADL-SRYCD(IDX1 IDX2)
                     END-PERFORM
                     MOVE   LNK-RYOCOM-SRYCDTOTAL2(IDX1)
                                   TO  WRK-IRYOADL-SRYCDTOTAL(IDX1)
                     IF     LNK-NYURECEDEN-SANTEIYM   <=  "201003"
                         MOVE   LNK-RYOCOM-ADL-SRYCD(IDX1)
                                       TO  WRK-IRYOADL-ADL-SRYCD(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN(IDX1)
                                       TO  WRK-IRYOADL-TEN(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
                                       TO  WRK-IRYOADL-TEN-MOJI(IDX1)
                     ELSE
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 1)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 2)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 3)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 4)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 4)
                     END-IF
                 END-IF
             END-PERFORM
      *
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Þ¤È¤á
             PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-IRYOADL-DAY(IDX1)  NOT =   ZERO)   AND
                    (WRK-IRYOADL-MEISAISU(IDX1)  NOT =   ZERO)
                   MOVE       1   TO  WRK-IRYOADL-GAITOU-KBN(IDX1 IDX1)
                   MOVE       ZERO      TO  IDX2
                   COMPUTE    IDX2      =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                     IF    LNK-NYURECEDEN-SANTEIYM   <=   "201003"
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN(IDX3)  =
                                         WRK-IRYOADL-TEN(IDX1))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     ELSE
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN-H22(IDX3 1)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 1))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 2)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 2))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 3)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 3))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 4)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 4))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *    ¤Þ¤È¤á·ë²Ì³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-IRYOADL-MEISAISU(IDX1) = "
      *                    WRK-IRYOADL-MEISAISU(IDX1)
      *          DISPLAY  "WRK-IRYOADL-SRYCDTOTAL(IDX1) = "
      *                    WRK-IRYOADL-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-IRYOADL-TEN(IDX1) = "
      *                    WRK-IRYOADL-TEN(IDX1)
      *          DISPLAY  "WRK-IRYOADL-GAITOU-TBL(IDX1) = "
      *                    WRK-IRYOADL-GAITOU-TBL(IDX1)
      *     END-PERFORM
      *
      *    Æþ±¡²ñ·×¤Þ¤È¤á·ë²Ì¤ÈÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Î¥ê¥ó¥¯¤ò³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-SO-IRYOADL-DAY(IDX1) = "
      *                    WRK-SO-IRYOADL-DAY(IDX1)
      *     END-PERFORM
      *
      *   ·ë²ÌÉ½¼¨
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *       IF    WRK-SO-MEISAISU(IDX1)   NOT =   ZERO
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          PERFORM   VARYING  IDX2   FROM  1  BY  1
      *                    UNTIL    IDX2   >   30
      *             DISPLAY  "IDX2 = " IDX2
      *                      " WRK-SO-SRYCD = " WRK-SO-SRYCD(IDX1 IDX2)
      *          END-PERFORM
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *          PERFORM   VARYING  IDX2  FROM  1  BY  1
      *                    UNTIL  IDX2  >  31
      *                     DISPLAY  "IDX2 = " IDX2
      *                     DISPLAY  "WRK-SO-GAIHAKU-KBN = " 
      *                               WRK-SO-GAIHAKU-KBN(IDX1 IDX2)
      *          END-PERFORM
      *       END-IF
      *     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£°¡Ë
           MOVE       ZERO      TO     IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "90")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(1 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(1 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                      LNK-NYURECEDEN-DAY-TBL(1 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   30)
                         MOVE   "SI"   TO
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(1 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-NYURECEDEN-KAISU(1 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =   SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(1 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
      *
                     IF  LNK-RYOCOM-RC  =       ZERO
      *              ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
      *              ·îÆü¥»¥Ã¥È
                     PERFORM   VARYING  CIDX   FROM   1  BY  1
                       UNTIL   CIDX     >   31
                      IF   WRK-SO-IRYOADL-DAY(IDX1)  =
                                         WRK-IRYOADL-DAY(CIDX)
                       INITIALIZE    WRK-RYOYO-DAY-TABLE
                       IF   WRK-IRYOADL-GAITOU-TBL(CIDX)   =  ALL"0"
                         CONTINUE
                       ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-IRYOADL-GAITOU-KBN(CIDX IDX5)
                                                          =       1
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-IRYOADL-GAITOU-KBN(CIDX IDX7)
                                                           =     1
                                        MOVE  IDX5  TO
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
      *                  Ê¿À®£²£²Ç¯£´·î¤è¤êµ­Ï¿¥³¥á¥ó¥È¤ÎÊÑ¹¹
      *                  ¡Êµ­Ï¿¥³¥á¥ó¥È¥³¡¼¥É810000001  ->  840000125 840000134¡Ë
                         IF  LNK-NYURECEDEN-SANTEIYM    <=   "201003"
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü¡Á¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (15:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (21:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         ELSE
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         END-IF
      *
      *                  °åÎÅ¶èÊ¬¥³¥á¥ó¥È
                         PERFORM   VARYING  IDX5  FROM  1  BY  1
                         UNTIL   (IDX5  >  30)   OR
                           (WRK-IRYOADL-SRYCD(CIDX IDX5) =  SPACE)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                             MOVE  WRK-IRYOADL-SRYCD(CIDX IDX5)  TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                             ADD    1     TO   IDX3
                           END-IF
                         END-PERFORM
      *
      *                  £Á£Ä£Ì¥³¥á¥ó¥È¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤êµ­Ï¿¥³¥á¥ó¥È¤ÎÊÑ¹¹¡Ë
                         IF  LNK-NYURECEDEN-SANTEIYM    <=   "201003"
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000103"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN(CIDX)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN(CIDX) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         ELSE
      *                   £Á£Ä£Ì£á
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000126"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 1)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 1) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£â
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000127"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 2)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 2) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£ã
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000128"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 3)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 3) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£ä
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000129"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 4)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 4) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         END-IF
      *
                       END-IF
                      END-IF
                     END-PERFORM
      *
                     END-IF
      *
               END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆÃÄêÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£²¡Ë
           MOVE      ZERO     TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "92")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(2 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(2 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                     LNK-NYURECEDEN-DAY-TBL(2 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   30)
                         MOVE   "SI"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(2 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-NYURECEDEN-KAISU(2 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =    SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(2 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(2 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
                END-IF
           END-PERFORM
      *
      *    ºÒ³²ÂÐ±þ¡Ê£¹£¶£²ÌÈ½ü¡¢£¹£¶£³Í±Í½¡¢£¹£µ£¹ºÒ³²¤ò¸øÈñ¤è¤ê¼èÆÀ¡Ë
           MOVE    SPACE                    TO  WRK-SAIGAI-AREA
           IF      LNK-NYURECEDEN-SANTEIYM  >=  "201101"
      *        £¹£¶£²ÌÈ½ü¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "962"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                       MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                       MOVE    "key4"           TO  MCP-PATHNAME
                       PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
      *        £¹£¶£³Í±Í½¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "963"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
      *        £¹£µ£¹ºÒ³²¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "959"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *    ÆÃÊÌÎÅÍÜ¡ÊµìÆÃÊÌÆü¸Û¡Ë¤Ï¸ºÌÈÅù¤ÎÂÐ¾Ý³°¤È¤¹¤ë
           PERFORM     VARYING   IDX-SAIGAI   FROM   1   BY   1
                       UNTIL    (IDX-SAIGAI   >    31)
                IF    (WRK-SAIGAI-004(IDX-SAIGAI)     =   "1")  OR
                      (WRK-SAIGAI-SYUHKN(IDX-SAIGAI)  =   "1")
                    MOVE  SPACE  TO   WRK-SAIGAI-962(IDX-SAIGAI)
                    MOVE  SPACE  TO   WRK-SAIGAI-963(IDX-SAIGAI)
                END-IF
           END-PERFORM
      *
      *    ÅìÆüËÜÂç¿ÌºÒ¤Ë¤Ä¤¤¤Æ¡¢Æþ±¡»þ¿©»öÎÅÍÜÈñµÚ¤ÓÆþ±¡»þÀ¸³èÎÅÍÜÈñ
      *    ¤Î¼«¸ÊÉéÃ´ÌÈ½ü¤ÏÊ¿À®£²£´Ç¯£²·î¤Þ¤Ç¤È¤¹¤ë
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "201203")
                PERFORM     VARYING   IDX-SAIGAI   FROM   1   BY   1
                       UNTIL    (IDX-SAIGAI   >    31)
                    MOVE  SPACE  TO   WRK-SAIGAI-962(IDX-SAIGAI)
                    MOVE  SPACE  TO   WRK-SAIGAI-963(IDX-SAIGAI)
                END-PERFORM
           END-IF
      *
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               PERFORM   2101-SHOKUJI-HENSHU1-SEC
           ELSE
               PERFORM   2103-SHOKUJI-HENSHU3-SEC
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "200610")  AND
               (LNK-NYULIFE-RC           =    ZERO)
               PERFORM   2104-KANKYO-HENSHU-SEC
           END-IF
      *
           .
       200-MAIN-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤é¤Î¥ì¥»ÅÅÊÖµÑ¥¨¥ê¥¢ÊÔ½¸¡Ë
      *****************************************************************
       200-MAIN-2-SEC            SECTION.
      *
      *Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤È£³¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤Ë¤Ä¤¤¤Æ¼èÆÀ
           INITIALIZE                             ORCSNYUINDAY-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"         USING   ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
      *¿©»ö¤ÎÊÝ¸±ÁÈ¹ç¤»¼èÆÀ
           INITIALIZE                             ORCSSKJHKN-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM     TO  LNK-SKJHKN-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID        TO  LNK-SKJHKN-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM    TO  LNK-SKJHKN-SANTEIYM
           CALL    "ORCSSKJHKN"         USING     ORCSSKJHKN-AREA
                                                  ORCSNYUINDAY-AREA
                                                  SPA-AREA
      *
      *Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ
           IF      LNK-NYURECEDEN-SANTEIYM     >=  "201804"
             INITIALIZE                            ORCSKANGOGEN-AREA
             MOVE    LNK-NYURECEDEN-HOSPNUM    TO  LNK-KANGOGEN-HOSPNUM
             MOVE    LNK-NYURECEDEN-PTID       TO  LNK-KANGOGEN-PTID
             MOVE    LNK-NYURECEDEN-SANTEIYM   TO  LNK-KANGOGEN-SANTEIYM
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
           MOVE    SPACE                      TO  WRK-SAIGAI-AREA2
      *
      *¥ì¥»ÅÅÂÐ¾ÝÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤Î¼èÆÀ
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-NYURECEDEN-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-NYURECEDEN-PTID     TO  COMB-PTID
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
                                EVALUATE   LNK-NYURECEDEN-HKNKBN
      *                             ¼ÒÊÝ
                                    WHEN   "1"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"  OR
                                                           "068"  OR
                                                           "069"  OR
                                                           "039"  OR
                                                           "040"
                                        MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
      *                                 ºÒ³²¸ºÌÈÂÐ¾Ý³°¤È¤¹¤ë°Ù¤ËÆÃÊÌÎÅÍÜ
      *                                 ¡ÊµìÆÃÊÌÆü¸Û¡Ë¤òµ­Ï¿
                                        IF   COMB-HKNNUM = "004"
                                           PERFORM  210-TOKUBETSU-SEC
                                           IF  PTHKN-HKNJANUM(1:4) =
                                                                "0404"
                                               MOVE  SPACE  TO  
                                                   WRK-SAIGAI-004(IDX)
                                           ELSE
                                               MOVE  "1"    TO
                                                   WRK-SAIGAI-004(IDX)
                                           END-IF
                                        END-IF
      *                                 ºÒ³²¸ºÌÈÂÐ¾Ý³°¤È¤¹¤ë°Ù¤Ë¼çÊÝ¸±
      *                                 Ìµ¤·¤òµ­Ï¿
                                        IF   COMB-HKNNUM = SPACE
                                            MOVE  "1"  TO  
                                                 WRK-SAIGAI-SYUHKN(IDX)
                                        END-IF
      *                             ¹ñÊÝ
                                    WHEN   "2"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"
                                            CONTINUE
                                        ELSE
                                            MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
      *                             ¹­°èÏ¢¹ç
                                    WHEN   "3"
                                        IF   COMB-HKNNUM = "039"
                                            CONTINUE
                                        ELSE
                                            MOVE   ZERO  TO  
                                                  WRK-HKNCOMBI-DAY(IDX)
                                        END-IF
                                END-EVALUATE
                            END-IF
                       ELSE
                           MOVE    5             TO  LNK-NYURECEDEN-RC
                       END-IF
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                       MOVE    "key"             TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                   ELSE
      *                ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                       MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                       MOVE    "key"             TO  MCP-PATHNAME
                       PERFORM   900-CLOSE-SEC
                       MOVE    5                 TO  LNK-NYURECEDEN-RC
                   END-IF
               END-IF
           END-PERFORM
      *
      *¿©»ö»»ÄêÍÑ¤ÎÊÝ¸±ÁÈ¹ç¤»È½ÄêÎÎ°è¤Ë¥»¥Ã¥È
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     WRK-HKNCOMBI-DAY(IDX)   NOT =   ZERO
                   IF (WRK-HKNCOMBI-DAY(IDX) =
                                 LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(10))
                       MOVE   WRK-HKNCOMBI-DAY(IDX)
                                   TO   WRK-SKJ-HKNCOMBI-ASA(IDX)
                                        WRK-SKJ-HKNCOMBI-HIRU(IDX)
                                        WRK-SKJ-HKNCOMBI-YORU(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *Æ±ÆüºÆÆþ±¡Æü¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸
           PERFORM    VARYING   IDX   FROM   1   BY   1
                      UNTIL      IDX  >    31
               IF     LNK-NYUINDAY-DOUJITSU-KBN(IDX)   =   "1"
      *            Âà±¡Ê¬
                   IF (LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX) =
                                 LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(10))
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
                                 LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(10))
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
           MOVE    LNK-NYURECEDEN-SANTEIYM
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
              IF   LNK-NYURECEDEN-HKNKBN  = LNK-NYUINDAY-DOUJITSU-HKNSYU
                                            (WRK-GETSUMATSU-DD)
                MOVE      "1"              TO  WRK-DOUJITSU-KBN
              END-IF
           END-IF
      *
      *¿©»öÊÝ¸±»ØÄê¤Çµ­Ï¿ÂÐ¾Ý¤È¤Ê¤ëÆþÎÏ¤¬¤¢¤ë¤«³ÎÇ§
           PERFORM    VARYING  IDX1   FROM  1  BY  1
             UNTIL    IDX1     >   31
                IF LNK-NYURECEDEN-HKNKBN  =  LNK-SKJHKN-HKNSYU(IDX1 1)
                    MOVE      "1"              TO  WRK-DOUJITSU-KBN
                END-IF
                IF LNK-NYURECEDEN-HKNKBN  =  LNK-SKJHKN-HKNSYU(IDX1 2)
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
                                 LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(10)
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
                                 LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                 LNK-NYURECEDEN-SET-HKNCOMBI(10)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *                         ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                         ¤ÎÂàÈò
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
                         IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)
                         AND   (LNK-NYUINDAY-KBN(IDX1)  =  "1")
                             MOVE    1    TO   FLG-OK
                         END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF  (FLG-OK     =   ZERO)   AND
                   (WRK-DOUJITSU-KBN   =   SPACE)
                   MOVE     4      TO      LNK-NYURECEDEN-RC
      *            Æþ±¡²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
                   GO   TO    200-MAIN-2-EXT
               END-IF
           ELSE
               MOVE     4      TO      LNK-NYURECEDEN-RC
      *        Æþ±¡²ñ·×¥¯¥í¡¼¥º
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
               GO   TO    200-MAIN-2-EXT
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *
      *ÁªÄêÎÅÍÜ¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-SENTEI-SET
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *
      *Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           PERFORM   210-RJN-TAISYO-CHK-SEC
      *
      *
      *¿©»öÎÅÍÜ¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
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
      *
      *¿©Æ²²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key43"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            ¿©Æ²²Ã»»¤ÎÂàÈò
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
      *            ¿©Æ²²Ã»»¤ÎÂàÈò¡ÊÆ±ÆüºÆÆþ±¡Æü¡Ë
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF  (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)  = "1")
                       AND (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                       AND (LNK-NYURECEDEN-HKNKBN  =
                                     LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1))
                       AND (NACCT-DAY(IDX1)    NOT =  ZERO)
      *                  Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¤Î¾ì¹ç¤Î¤ß¥»¥Ã¥È
                           IF  WRK-SYS5000-SHOKUCD(1 IDX1)  =
                                                          "197000110"
                               MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
      *            ¿©Æ²²Ã»»¤ÎÂàÈò¡Ê¿©»ö¤ÎÊÝ¸±»ØÄêÆþÎÏÆü¡Ë
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF ((WRK-SKJ-HKNCOMBI-ASA(IDX1)  NOT =  ZERO) OR
                           (WRK-SKJ-HKNCOMBI-HIRU(IDX1) NOT =  ZERO) OR
                           (WRK-SKJ-HKNCOMBI-YORU(IDX1) NOT =  ZERO))
                       AND (NACCT-DAY(IDX1)    NOT =  ZERO)
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
                 MOVE    "key43"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key43"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
               MOVE    LNK-NYURECEDEN-HOSPNUM
                                           TO  TNS-HOSPNUM
               MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
               MOVE    LNK-NYURECEDEN-SANTEIYM
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
                              (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
                          AND  ((WRK-GAIHAKU-KBN(IDX1) = ZERO) OR
                                (WRK-GAIHAKU-KBN(IDX1) >= 4))
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
                        AND  ((WRK-GAIHAKU-KBN(IDX1) = ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1) >= 4))
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-NYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  ACCT-SRYYM
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
                           MOVE    LNK-NYURECEDEN-HOSPNUM
                                                 TO  TNS-HOSPNUM
                           MOVE    SRY-SRYCD(1)  TO  TNS-SRYCD
                           MOVE    LNK-NYURECEDEN-SANTEIYM
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
      *              Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
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
                              (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                         AND  (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                         AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
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
                                  (WRK-GAIHAKU-KBN(IDX1) >=  4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                            AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
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
                                  (WRK-GAIHAKU-KBN(IDX1) >=   4))
                            AND   (ACCT-DAY(1 IDX1)    NOT =  ZERO)
                            AND   (ACCT-HKNCOMBI  =
                                                WRK-HKNCOMBI-DAY(IDX1))
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
                        MOVE    LNK-NYURECEDEN-HOSPNUM
                                              TO  TNS-HOSPNUM
                        MOVE    WRK-IRYOKNS-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
                        MOVE    LNK-NYURECEDEN-SANTEIYM
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
                     (LNK-NYUINDAY-TANKI2-KBN(IDX1)  =    SPACE)
                   MOVE   "90"     TO       WRK-SO-SRYKBN(IDX1)
               END-IF
      *        ÊÝ¸±ÁÈ¹ç¤»
               MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                        TO  WRK-SO-HKNCOMBI-DAY(IDX1)
               MOVE     ZERO                TO  WRK-MEISAISU
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
                                      IF  LNK-NYURECEDEN-SANTEIYM
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
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
      *                Â¾°åÎÅµ¡´Ø¼õ¿Ç
                       WHEN   4
                           IF    LNK-NYURECEDEN-SANTEIYM  >=  "201004"
                               CONTINUE
                           ELSE
                               INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
               PERFORM   VARYING   IDX2    FROM   1  BY   1
                         UNTIL (WRK-SO-SRYCD(IDX1 IDX2) = SPACE)
                            OR (IDX2    >      30)
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
      *        ²ó¿ô¾ðÊó¤Î¥»¥Ã¥È
               IF    WRK-SO-SRYCD(IDX1 1) NOT  =   SPACE
                   MOVE     1      TO    WRK-SO-DAY(IDX1 IDX1)
               END-IF
           END-PERFORM
      *
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          DISPLAY  "WRK-SO-SRYCD(1) = " WRK-SO-SRYCD(IDX1 1)
      *          DISPLAY  "WRK-SO-SRYCD(2) = " WRK-SO-SRYCD(IDX1 2)
      *          DISPLAY  "WRK-SO-SRYCD(3) = " WRK-SO-SRYCD(IDX1 3)
      *          DISPLAY  "WRK-SO-SRYCD(4) = " WRK-SO-SRYCD(IDX1 4)
      *          DISPLAY  "WRK-SO-SRYCD(5) = " WRK-SO-SRYCD(IDX1 5)
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *     END-PERFORM
      *
      *    °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ô¾ðÊó¤Î¼èÆÀ¡ÊÊ¿À®£³£°Ç¯£³·îËø¡Ë
           IF      LNK-NYURECEDEN-SANTEIYM <=  "201803"
               INITIALIZE                          ORCSRYOCOM-AREA
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-RYOCOM-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  LNK-RYOCOM-PTID
               MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-RYOCOM-SANTEIYM
               CALL    "ORCSRYOCOM"     USING  ORCSRYOCOM-AREA
                                               SPA-AREA
               IF      LNK-RYOCOM-RC           NOT =  ZERO
                   DISPLAY  "LNK-RYOCOM-RC = " LNK-RYOCOM-RC
               END-IF
           ELSE
               INITIALIZE                          ORCSRYOCOM-AREA
               MOVE      1                 TO      LNK-RYOCOM-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY   1
      *               UNTIL    IDX1    >   31
      *       DISPLAY "IDX1 = " IDX1
      *       DISPLAY "LNK-RYOCOM-NYUINKBN = " LNK-RYOCOM-NYUINKBN(IDX1)
      *       DISPLAY "LNK-RYOCOM-MEISAISU = " LNK-RYOCOM-MEISAISU(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL(IDX1)
      *       DISPLAY "LNK-RYOCOM-SRYCDTOTAL2 = " 
      *                                     LNK-RYOCOM-SRYCDTOTAL2(IDX1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 1) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 1)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 2) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 2)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 3) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 3)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 4) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 4)
      *       DISPLAY "LNK-RYOCOM-IRYO-SRYCD (IDX1 5) = "
      *                             LNK-RYOCOM-IRYO-SRYCD(IDX1 5)
      *       DISPLAY "LNK-RYOCOM-ADL-SRYCD (IDX1) = "
      *                             LNK-RYOCOM-ADL-SRYCD(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN(IDX1)
      *       DISPLAY "LNK-RYOCOM-ADL-TEN-MOJI (IDX1) = "
      *                             LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
      *     END-PERFORM
      *
           MOVE      ZERO      TO   MATOME-IDX
      *
      *    ºÆ¹½ÃÛ¤·¤¿Æþ±¡²ñ·×¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)
                   IF    (LNK-RYOCOM-NYUINKBN(IDX1)   =   "1")   AND
                         (LNK-RYOCOM-MEISAISU(IDX1)   NOT =  ZERO)
                       ADD   1              TO  MATOME-IDX
                       MOVE  MATOME-IDX     TO  WRK-IRYOADL-DAY(IDX1)
                       MOVE  MATOME-IDX     TO  WRK-SO-IRYOADL-DAY(IDX1)
                   END-IF
                   ADD   1                  TO  WRK-SO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SO-MEISAISU(IDX3)  =
                                             WRK-SO-MEISAISU(IDX1)) AND
                             (WRK-SO-HKNCOMBI-DAY(IDX3)  =
                                         WRK-SO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SO-SRYCDTOTAL(IDX3)  =
                                         WRK-SO-SRYCDTOTAL(IDX1))   AND
                             (WRK-SO-TEN(IDX3)  =
                                         WRK-SO-TEN(IDX1))
                             IF (LNK-RYOCOM-NYUINKBN(IDX3) = "1")   AND
                                (LNK-RYOCOM-MEISAISU(IDX3) NOT =  ZERO)
                                 MOVE  MATOME-IDX    TO  
                                                 WRK-IRYOADL-DAY(IDX3)
                             END-IF
                             ADD     1    TO    WRK-SO-KAISU(IDX1)
                             MOVE    1    TO    WRK-SO-DAY(IDX1 IDX3)
                             IF  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                                             NOT = ZERO
                                 MOVE  WRK-SO-GAIHAKU-KBN(IDX3 IDX3)
                                          TO  WRK-SO-GAIHAKU-KBN
                                                            (IDX1 IDX3)
                             END-IF
                             INITIALIZE   WRK-NYUIN-SORT-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      **     DISPLAY "===  DAY-SET2 ======"
      **     PERFORM   VARYING   IDX1    FROM   1  BY  1
      **       UNTIL   IDX1     >   31
      **       DISPLAY "IDX1 = "IDX1
      **         DISPLAY "      ----5----0----5----0----5----0-"
      **         DISPLAY "DAY = " WRK-SO-DAY-TBL(IDX1)
      **     END-PERFORM
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤ÎÆ±°ìºÞ¤Þ¤È¤á½èÍý
           IF    (MATOME-IDX     NOT =   ZERO)   AND
                 (LNK-RYOCOM-RC  =       ZERO)
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö¤ÎÊÖµÑ·ë²Ì¤ò¤Þ¤È¤á¥ï¡¼¥¯¤ËÅ¾Á÷
             PERFORM      VARYING    IDX1   FROM   1  BY  1
               UNTIL      IDX1   >   31
                 IF   (LNK-RYOCOM-NYUINKBN(IDX1)  NOT =   ZERO)   AND
                      (LNK-RYOCOM-MEISAISU(IDX1)  NOT =   ZERO)
                     MOVE   LNK-RYOCOM-MEISAISU(IDX1)
                                   TO  WRK-IRYOADL-MEISAISU(IDX1)
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL  IDX2 >  LNK-RYOCOM-MEISAISU(IDX1)
                         MOVE  LNK-RYOCOM-IRYO-SRYCD(IDX1 IDX2)
                                     TO  WRK-IRYOADL-SRYCD(IDX1 IDX2)
                     END-PERFORM
                     MOVE   LNK-RYOCOM-SRYCDTOTAL2(IDX1)
                                   TO  WRK-IRYOADL-SRYCDTOTAL(IDX1)
                     IF     LNK-NYURECEDEN-SANTEIYM   <=  "201003"
                         MOVE   LNK-RYOCOM-ADL-SRYCD(IDX1)
                                       TO  WRK-IRYOADL-ADL-SRYCD(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN(IDX1)
                                       TO  WRK-IRYOADL-TEN(IDX1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI(IDX1)
                                       TO  WRK-IRYOADL-TEN-MOJI(IDX1)
                     ELSE
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 1)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 1)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 1)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 2)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 2)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 2)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 3)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 3)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 3)
                         MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX1 4)
                                   TO  WRK-IRYOADL-ADL-SRYCD-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-H22(IDX1 4)
                         MOVE   LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX1 4)
                                   TO  WRK-IRYOADL-TEN-MOJI-H22(IDX1 4)
                     END-IF
                 END-IF
             END-PERFORM
      *
      *      ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Þ¤È¤á
             PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF   (WRK-IRYOADL-DAY(IDX1)  NOT =   ZERO)   AND
                    (WRK-IRYOADL-MEISAISU(IDX1)  NOT =   ZERO)
                   MOVE       1   TO  WRK-IRYOADL-GAITOU-KBN(IDX1 IDX1)
                   MOVE       ZERO      TO  IDX2
                   COMPUTE    IDX2      =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                     IF    LNK-NYURECEDEN-SANTEIYM   <=   "201003"
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN(IDX3)  =
                                         WRK-IRYOADL-TEN(IDX1))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     ELSE
                       IF    (WRK-IRYOADL-DAY(IDX3)  =
                                         WRK-IRYOADL-DAY(IDX1)) AND
                             (WRK-IRYOADL-MEISAISU(IDX3)  =
                                         WRK-IRYOADL-MEISAISU(IDX1)) AND
                             (WRK-IRYOADL-SRYCDTOTAL(IDX3)  =
                                       WRK-IRYOADL-SRYCDTOTAL(IDX1)) AND
                             (WRK-IRYOADL-TEN-H22(IDX3 1)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 1))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 2)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 2))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 3)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 3))  AND
                             (WRK-IRYOADL-TEN-H22(IDX3 4)  =
                                       WRK-IRYOADL-TEN-H22(IDX1 4))
                             MOVE      1    TO
                                       WRK-IRYOADL-GAITOU-KBN(IDX1 IDX3)
                             INITIALIZE   WRK-IRYOADL-SORT-TBL(IDX3)
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *    ¤Þ¤È¤á·ë²Ì³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-IRYOADL-MEISAISU(IDX1) = "
      *                    WRK-IRYOADL-MEISAISU(IDX1)
      *          DISPLAY  "WRK-IRYOADL-SRYCDTOTAL(IDX1) = "
      *                    WRK-IRYOADL-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-IRYOADL-TEN(IDX1) = "
      *                    WRK-IRYOADL-TEN(IDX1)
      *          DISPLAY  "WRK-IRYOADL-GAITOU-TBL(IDX1) = "
      *                    WRK-IRYOADL-GAITOU-TBL(IDX1)
      *     END-PERFORM
      *
      *    Æþ±¡²ñ·×¤Þ¤È¤á·ë²Ì¤ÈÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¤Î¥ê¥ó¥¯¤ò³ÎÇ§
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-IRYOADL-DAY(IDX1) = "
      *                    WRK-IRYOADL-DAY(IDX1)
      *          DISPLAY  "WRK-SO-IRYOADL-DAY(IDX1) = "
      *                    WRK-SO-IRYOADL-DAY(IDX1)
      *     END-PERFORM
      *
      *   ·ë²ÌÉ½¼¨
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     31)
      *       IF    WRK-SO-MEISAISU(IDX1)   NOT =   ZERO
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "WRK-SO-HKNCOMBI-DAY = " WRK-SO-HKNCOMBI-DAY
      *                                                         (IDX1)
      *          DISPLAY  "WRK-SO-SRYKBN   = " WRK-SO-SRYKBN(IDX1)
      *          DISPLAY  "WRK-SO-MEISAISU = " WRK-SO-MEISAISU(IDX1)
      *          PERFORM   VARYING  IDX2   FROM  1  BY  1
      *                    UNTIL    IDX2   >   30
      *             DISPLAY  "IDX2 = " IDX2
      *                      " WRK-SO-SRYCD = " WRK-SO-SRYCD(IDX1 IDX2)
      *          END-PERFORM
      *          DISPLAY  "WRK-SO-SRYCDTOTAL = " WRK-SO-SRYCDTOTAL(IDX1)
      *          DISPLAY  "WRK-SO-TEN      = " WRK-SO-TEN(IDX1)
      *          DISPLAY  "WRK-SO-KAISU    = " WRK-SO-KAISU(IDX1)
      *          PERFORM   VARYING  IDX2  FROM  1  BY  1
      *                    UNTIL  IDX2  >  31
      *                     DISPLAY  "IDX2 = " IDX2
      *                     DISPLAY  "WRK-SO-GAIHAKU-KBN = " 
      *                               WRK-SO-GAIHAKU-KBN(IDX1 IDX2)
      *          END-PERFORM
      *       END-IF
      *     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£°¡Ë
           MOVE       ZERO      TO     IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "90")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(1 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(1 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                      LNK-NYURECEDEN-DAY-TBL(1 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   30)
                         MOVE   "SI"   TO
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(1 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-NYURECEDEN-KAISU(1 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =   SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(1 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
      *
                     IF  LNK-RYOCOM-RC  =       ZERO
      *              ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
      *              ·îÆü¥»¥Ã¥È
                     PERFORM   VARYING  CIDX   FROM   1  BY  1
                       UNTIL   CIDX     >   31
                      IF   WRK-SO-IRYOADL-DAY(IDX1)  =
                                         WRK-IRYOADL-DAY(CIDX)
                       INITIALIZE    WRK-RYOYO-DAY-TABLE
                       IF   WRK-IRYOADL-GAITOU-TBL(CIDX)   =  ALL"0"
                         CONTINUE
                       ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-IRYOADL-GAITOU-KBN(CIDX IDX5)
                                                          =       1
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-RYOYO-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-IRYOADL-GAITOU-KBN(CIDX IDX7)
                                                           =     1
                                        MOVE  IDX5  TO
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-RYOYO-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
      *                  Ê¿À®£²£²Ç¯£´·î¤è¤êµ­Ï¿¥³¥á¥ó¥È¤ÎÊÑ¹¹
      *                  ¡Êµ­Ï¿¥³¥á¥ó¥È¥³¡¼¥É810000001  ->  840000125 840000134¡Ë
                         IF  LNK-NYURECEDEN-SANTEIYM    <=   "201003"
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                MOVE  "810000001"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                MOVE   "¡¡¡¡·î¡¡¡¡Æü¡Á¡¡¡¡·î¡¡¡¡Æü"  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                MOVE   LNK-NYURECEDEN-SANTEIYM(5:2)
                                                       TO  WRK-MM-X
                                MOVE   WRK-MM-9        TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (15:4)
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (7:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (21:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         ELSE
                          PERFORM   VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  31)   OR
                               (WRK-RYOYO-DAY-ST(IDX5) =  ZERO)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             IF   WRK-RYOYO-DAY-ST(IDX5) = 
                                               WRK-RYOYO-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO  IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                                IF   SPA-HOSPSBT    =    1
                                    MOVE  "840000125"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                ELSE
                                    MOVE  "840000134"  TO
                                      LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                                END-IF
                                MOVE   WRK-RYOYO-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-RYOYO-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                                                          (5:4)
                             END-IF
                             ADD    1     TO   IDX3
                           END-IF
                          END-PERFORM
                         END-IF
      *
      *                  °åÎÅ¶èÊ¬¥³¥á¥ó¥È
                         PERFORM   VARYING  IDX5  FROM  1  BY  1
                         UNTIL   (IDX5  >  30)   OR
                           (WRK-IRYOADL-SRYCD(CIDX IDX5) =  SPACE)
                           IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                             ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                             MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                             MOVE  WRK-IRYOADL-SRYCD(CIDX IDX5)  TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                             ADD    1     TO   IDX3
                           END-IF
                         END-PERFORM
      *
      *                  £Á£Ä£Ì¥³¥á¥ó¥È¡ÊÊ¿À®£²£²Ç¯£´·î¤è¤êµ­Ï¿¥³¥á¥ó¥È¤ÎÊÑ¹¹¡Ë
                         IF  LNK-NYURECEDEN-SANTEIYM    <=   "201003"
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000103"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN(CIDX)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN(CIDX) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         ELSE
      *                   £Á£Ä£Ì£á
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000126"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 1)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 1) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£â
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000127"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 2)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 2) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£ã
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000128"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 3)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 3) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
      *                   £Á£Ä£Ì£ä
                          IF   LNK-NYURECEDEN-MEISAISU(1 IDX2) < 300
                           ADD    1     TO
                                   LNK-NYURECEDEN-MEISAISU(1 IDX2)
                           MOVE   "CO"   TO   
                                LNK-NYURECEDEN-RECSKB(1 IDX2 IDX3)
                           MOVE  "840000129"   TO
                                  LNK-NYURECEDEN-SRYCD(1 IDX2 IDX3)
                           IF   WRK-IRYOADL-TEN-H22(CIDX 4)   =   ZERO
                             MOVE  "£°£°"    TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           ELSE
                             MOVE   WRK-IRYOADL-TEN-H22(CIDX 4) TO IDX6
                             MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(1 IDX2 IDX3)
                           END-IF
                           ADD    1     TO   IDX3
                          END-IF
                         END-IF
      *
                       END-IF
                      END-IF
                     END-PERFORM
      *
                     END-IF
      *
               END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È¡ÊÆÃÄêÆþ±¡ÎÁ¡¡¿ÇÎÅ¶èÊ¬£¹£²¡Ë
           MOVE      ZERO     TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               IF   (WRK-SO-MEISAISU(IDX1)  NOT =   ZERO)   AND
                    (WRK-SO-TEN(IDX1)       NOT =   ZERO)   AND
                    (WRK-SO-SRYKBN(IDX1)    =       "92")
                     ADD    1      TO    IDX2
                     MOVE  WRK-SO-HKNCOMBI-DAY(IDX1) TO
                                     LNK-NYURECEDEN-HKNCOMBI(2 IDX2)
                     MOVE  WRK-SO-MEISAISU(IDX1)   TO
                                     LNK-NYURECEDEN-MEISAISU(2 IDX2)
                     MOVE  WRK-SO-DAY-TBL(IDX1)    TO
                                     LNK-NYURECEDEN-DAY-TBL(2 IDX2)
      *
                     PERFORM   VARYING   IDX3   FROM   1   BY  1
                               UNTIL (WRK-SO-SRYCD(IDX1 IDX3) = SPACE)
                                  OR (IDX3   >   30)
                         MOVE   "SI"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                         IF    IDX3   =    1
                               MOVE   WRK-SO-SRYKBN(IDX1)  TO   
                                    LNK-NYURECEDEN-SRYSKB(2 IDX2 IDX3)
                         END-IF
                         MOVE  WRK-SO-SRYCD(IDX1 IDX3) TO
                                    LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                         MOVE   WRK-SO-KAISU(IDX1)   TO
                                      LNK-NYURECEDEN-KAISU(2 IDX2 IDX3)
                         MOVE  ZERO     TO    IDX4
                         COMPUTE   IDX4 =    IDX3   +   1
                         IF    WRK-SO-SRYCD(IDX1 IDX4)    =    SPACE
                               MOVE   WRK-SO-TEN(IDX1)   TO
                                      LNK-NYURECEDEN-TEN(2 IDX2 IDX3)
                         END-IF
                     END-PERFORM
      *              ³°Çñ¥³¥á¥ó¥È¥»¥Ã¥È½èÍý
                     INITIALIZE    WRK-GAIHAKU-DAY-TABLE
                     IF   WRK-SO-GAIHAKU-TBL(IDX1)   =  ALL"0"
                         CONTINUE
                     ELSE
                         MOVE     ZERO    TO   IDX6
                         PERFORM  VARYING  IDX5   FROM  1  BY  1
                           UNTIL  IDX5   >  31
                               IF   WRK-SO-GAIHAKU-KBN(IDX1 IDX5)  =
                                                         1   OR  2
                                 IF  IDX5   =   1
                                    ADD   1    TO   IDX6
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ST(IDX6)
                                    MOVE  IDX5   TO  
                                               WRK-GAIHAKU-DAY-ED(IDX6)
                                 ELSE
                                   COMPUTE   IDX7  =   IDX5  -  1
                                   IF WRK-SO-GAIHAKU-KBN(IDX1 IDX7) =
                                                         1   OR  2
                                        MOVE  IDX5  TO
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   ELSE
                                     ADD   1    TO   IDX6
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ST(IDX6)
                                     MOVE  IDX5   TO  
                                              WRK-GAIHAKU-DAY-ED(IDX6)
                                   END-IF
                                 END-IF
                               END-IF
                         END-PERFORM
                       PERFORM   VARYING  IDX5  FROM  1  BY  1
                       UNTIL   (IDX5  >  31)   OR
                               (WRK-GAIHAKU-DAY-ST(IDX5) =  ZERO)
                           ADD    1     TO
                                       LNK-NYURECEDEN-MEISAISU(2 IDX2)
                           IF   WRK-GAIHAKU-DAY-ST(IDX5) = 
                                               WRK-GAIHAKU-DAY-ED(IDX5)
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000015"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                           ELSE
                                MOVE   "CO"   TO   
                                    LNK-NYURECEDEN-RECSKB(2 IDX2 IDX3)
                                MOVE  "840000016"  TO
                                      LNK-NYURECEDEN-SRYCD(2 IDX2 IDX3)
                                MOVE   WRK-GAIHAKU-DAY-ST(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (1:4)
                                MOVE   WRK-GAIHAKU-DAY-ED(IDX5) TO IDX6
                                MOVE   TBL-DAY(IDX6)  TO
                                    LNK-NYURECEDEN-COMMENT(2 IDX2 IDX3)
                                                          (5:4)
                           END-IF
                           ADD    1     TO   IDX3
                       END-PERFORM
                     END-IF
                END-IF
           END-PERFORM
      *
      *    ºÒ³²ÂÐ±þ¡Ê£¹£¶£²ÌÈ½ü¡¢£¹£¶£³Í±Í½¡¢£¹£µ£¹ºÒ³²¤ò¸øÈñ¤è¤ê¼èÆÀ¡Ë
           MOVE    SPACE                    TO  WRK-SAIGAI-AREA
           IF      LNK-NYURECEDEN-SANTEIYM  >=  "201101"
      *        £¹£¶£²ÌÈ½ü¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "962"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-962
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                       MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                       MOVE    "key4"           TO  MCP-PATHNAME
                       PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
      *        £¹£¶£³Í±Í½¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "963"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-963
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
      *        £¹£µ£¹ºÒ³²¤Î¥Á¥§¥Ã¥¯
               INITIALIZE                          PTKOHINF-REC
               MOVE    LNK-NYURECEDEN-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    LNK-NYURECEDEN-PTID     TO  PTKOH-PTID
               MOVE    "959"                   TO  PTKOH-KOHNUM
               MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
               MOVE    "key4"                  TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-SELECT-SEC
               IF      FLG-PTKOHINF            =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM   1   BY   1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  <
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >  31
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) <
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE      PTKOH-TEKEDYMD(7:2)
                                                    TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  1   BY  1
                                       UNTIL   IDX-SAIGAI   >
                                                          IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                               IF  (PTKOH-TEKSTYMD(1:6) =
                                          LNK-NYURECEDEN-SANTEIYM) AND
                                   (LNK-NYURECEDEN-SANTEIYM  =
                                          PTKOH-TEKEDYMD(1:6))
                                     MOVE     PTKOH-TEKSTYMD(7:2)
                                                     TO   IDX-SAIGAI-ST
                                     MOVE     PTKOH-TEKEDYMD(7:2)
                                                     TO   IDX-SAIGAI-ED
                                     PERFORM   VARYING  IDX-SAIGAI
                                               FROM  IDX-SAIGAI-ST  BY 1
                                       UNTIL   IDX-SAIGAI   >
                                                         IDX-SAIGAI-ED
                                         MOVE  "1"  TO   WRK-SAIGAI-959
                                                            (IDX-SAIGAI)
                                     END-PERFORM
                               END-IF
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
               END-IF
      *        ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *    ÆÃÊÌÎÅÍÜ¡ÊµìÆÃÊÌÆü¸Û¡Ë¤Ï¸ºÌÈÅù¤ÎÂÐ¾Ý³°¤È¤¹¤ë
           PERFORM     VARYING   IDX-SAIGAI   FROM   1   BY   1
                       UNTIL    (IDX-SAIGAI   >    31)
                IF    (WRK-SAIGAI-004(IDX-SAIGAI)     =   "1")  OR
                      (WRK-SAIGAI-SYUHKN(IDX-SAIGAI)  =   "1")
                    MOVE  SPACE  TO   WRK-SAIGAI-962(IDX-SAIGAI)
                    MOVE  SPACE  TO   WRK-SAIGAI-963(IDX-SAIGAI)
                END-IF
           END-PERFORM
      *
      *    ÅìÆüËÜÂç¿ÌºÒ¤Ë¤Ä¤¤¤Æ¡¢Æþ±¡»þ¿©»öÎÅÍÜÈñµÚ¤ÓÆþ±¡»þÀ¸³èÎÅÍÜÈñ
      *    ¤Î¼«¸ÊÉéÃ´ÌÈ½ü¤ÏÊ¿À®£²£´Ç¯£²·î¤Þ¤Ç¤È¤¹¤ë
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "201203")
                PERFORM     VARYING   IDX-SAIGAI   FROM   1   BY   1
                       UNTIL    (IDX-SAIGAI   >    31)
                    MOVE  SPACE  TO   WRK-SAIGAI-962(IDX-SAIGAI)
                    MOVE  SPACE  TO   WRK-SAIGAI-963(IDX-SAIGAI)
                END-PERFORM
           END-IF
      *
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸
           IF   LNK-NYURECEDEN-SANTEIYM  <=  "200603"
               PERFORM   2101-SHOKUJI-HENSHU1-SEC
           ELSE
               IF    LNK-NYURECEDEN-SANTEIYM  <=  "201603"
                   PERFORM   2103-SHOKUJI-HENSHU3-SEC
               ELSE
                   IF    LNK-NYURECEDEN-SANTEIYM  <=  "201709"
                       PERFORM   2103-SHOKUJI-HENSHU4-SEC
                   ELSE
                       PERFORM   2103-SHOKUJI-HENSHU5-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "200610")  AND
               (LNK-NYURECEDEN-SANTEIYM  <=  "201603")
               IF  LNK-NYULIFE-RC           =    ZERO
                   PERFORM   2104-KANKYO-HENSHU-SEC
               END-IF
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "201604")  AND
               (LNK-NYURECEDEN-SANTEIYM  <=  "201709")  AND
               (LNK-NYULIFE2-RC           =    ZERO)
               PERFORM   2104-KANKYO-HENSHU2-SEC
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¤ÎÊÔ½¸
           IF  (LNK-NYURECEDEN-SANTEIYM  >=  "201710")  AND
               (LNK-NYULIFE2-RC           =    ZERO)
               PERFORM   2104-KANKYO-HENSHU3-SEC
           END-IF
      *
           .
       200-MAIN-2-EXT.
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
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  TNS-HOSPNUM
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           IF  (LNK-NYURECEDEN-SANTEIYM >= "202004")
           AND (TNS-SRYCD             NOT = SPACE   )
           AND (TNS-NYUKHNKSNKBN          = ZERO    )
             INITIALIZE                          SNTENSU-AREA
             MOVE  TNS-SRYCD                 TO  SNTENSU-SRYCD
             MOVE  LNK-NYURECEDEN-SANTEIYM   TO  SNTENSU-BASEYMD(1:6)
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
           IF  LNK-NYURECEDEN-SANTEIYM >= "201804"
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
           MOVE   NACCT-DAY(IDX1)     TO    WRK-GAIHAKU-KBN(IDX1)
      *
           EVALUATE   WRK-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
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
           IF       LNK-NYURECEDEN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-TAKIKAN3-SEC
           ELSE
             IF     LNK-NYURECEDEN-SANTEIYM  >=  "201604"
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
                 IF   LNK-NYURECEDEN-SANTEIYM  >=  "201004"
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
           IF       LNK-NYURECEDEN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-GENSAN3-SEC
           ELSE
             IF     LNK-NYURECEDEN-SANTEIYM  >=  "201604"
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
                      IF   LNK-NYURECEDEN-SANTEIYM  >=  "201404"
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
                      IF   LNK-NYURECEDEN-SANTEIYM  >=  "201404"
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
           MOVE    LNK-NYURECEDEN-HOSPNUM TO  COMB-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID  TO   COMB-PTID
           PERFORM   VARYING   IDX   FROM  1  BY  1
                     UNTIL     (WRK-HKNCOMBI-DAY(IDX)  NOT =  ZERO) OR
                               (IDX     >   WRK-GETSUMATSU-DD)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  AGECHK-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM
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
      *    ÆÃÊÌÆü¸Û¡ÊÆÃÊÌÎÅÍÜ¡Ë¤ÎÈ½ÄêÍÑ¤Ë´µ¼ÔÊÝ¸±¾ðÊó¤ÎÆÉ¤ß¹þ¤ß
      *****************************************************************
       210-TOKUBETSU-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                    TO     PTHKN-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID
                                    TO     PTHKN-PTID
           MOVE    COMB-HKNID       TO     PTHKN-HKNID
           MOVE    "tbl_pthkninf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-PTHKNINF-SELECT-SEC
           IF    FLG-PTHKNINF       =       ZERO
               MOVE    "tbl_pthkninf"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-IF
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥¯¥í¡¼¥º
           MOVE    "tbl_pthkninf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-TOKUBETSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF     (IDX2                   =     1  )
           AND    (WRK-GAIHAKU(IDX1)   NOT =   SPACE)
               MOVE    SPACE           TO      WRK-GAIHAKU-SRYCD
               IF    LNK-NYURECEDEN-SANTEIYM   <=  "201203"
                  PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               END-IF
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    WRK-SO-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  TNS-HOSPNUM
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
           MOVE    TNS-SRYCD    TO   WRK-TENSU-CD
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
                   IF   WRK-SO-SRYCD(IDX1 IDX2)   =   "190122890"
      *                ÁªÄêÆþ±¡¤ÎÆÃÄê¥³¡¼¥É¤Î¤ßÃ¼¿ô½èÍý¤¢¤ê
                       COMPUTE    WRK-SO-TEN(IDX1)  =
                                 (WRK-SO-TEN(IDX1)  -
                                 (WRK-SO-TEN(IDX1)  *
                                 (TNS-TEN   /  100)))     +  0.5
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
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ¡Ê¤³¤Î¥í¥¸¥Ã¥¯¤ÏÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¡Ë
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
           IF   (LNK-NYURECEDEN-SANTEIYM  >= "200604"   )
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
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       210-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SYS5000-COUNT
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key11"           TO  MCP-PATHNAME
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       210-SYS-5000-GET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYURECEDEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-NYURECEDEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
      *          ¿©»ö¤ÎÆÃÊÌ´ÉÍý²Ã»»ÂàÈò
                 IF     SYS-5000-SHOKUCD(1)  NOT =  SPACE
                    IF  SYS-5000-SHOKUKBN(1) =     "2"
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                            MOVE  SYS-5000-SHOKUCD(1)
                                  TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                        END-PERFORM
                    ELSE
                        PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                            MOVE  SPACE
                                  TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                        END-PERFORM
                    END-IF
                 ELSE
                    PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                              UNTIL     GENSAN-IDX2   >   31
                        MOVE SPACE TO WRK-SYS5000-SHOKUCD(2 GENSAN-IDX2)
                    END-PERFORM
                 END-IF
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190080570"  TO  WRK-GENSAN2
                              ELSE
                                  MOVE  "190799770"  TO  WRK-GENSAN2
                              END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190111690"  TO  WRK-GENSAN3
                              ELSE
                                  MOVE  "190809090"  TO  WRK-GENSAN3
                              END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              IF   FLG-RJN    =    ZERO
                                  MOVE  "190111790"  TO  WRK-GENSAN4
                              ELSE
                                  MOVE  "190809190"  TO  WRK-GENSAN4
                              END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    ELSE
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    END-IF
                 END-PERFORM
                 MOVE    "tbl_syskanri"    TO  MCP-TABLE
                 MOVE    "key11"           TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£³·îËø¡Ë
      *****************************************************************
       2101-SHOKUJI-HENSHU1-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO) OR 4 OR 5
                                                         OR 6 OR 7)
                       MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ-HKNCOMBI-DAY(IDX1)
                                           WRK-SKJ2-HKNCOMBI-DAY(IDX1)
                                           WRK-SKJ3-HKNCOMBI-DAY(IDX1)
                                         WRK-SKJFTN-HKNCOMBI-DAY(IDX1)
                       EVALUATE   NACCT-DAY(IDX1)
                           WHEN   1
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   3
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                           WHEN   4
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                                   MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               END-IF
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                               IF   WRK-SYS5000-SHOKUCD(1 IDX1) NOT =
                                                                 SPACE
                                   MOVE   "197000810"
                                         TO  WRK-SKJFTN-SRYCD(IDX1)
                               END-IF
                       END-EVALUATE
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(2 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(2 IDX1)
                                          TO  WRK-SKJ-TOKU-KANRI(IDX1)
                           ADD   200          TO  WRK-SKJ-MONEY(IDX1)
                       END-IF
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ-SHOKUDOU(IDX1)
                           ADD    50          TO  WRK-SKJ-MONEY(IDX1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  TSYRRK-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
              AND (TSYRRK-NINSTYMD(7:2)      NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197000910"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
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
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-NYURECEDEN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-NYURECEDEN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-NYURECEDEN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  TNKRRK-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TNKRRK-STYMD(1:6))
              AND (TNKRRK-STYMD(7:2)     NOT =  "01")
                CONTINUE
              ELSE
                PERFORM   VARYING   IDX1   FROM   1  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001110"
                                        TO  WRK-SKJFTN-SRYCD(IDX1)
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
      *    ¿©»ö¤Î¿ÇÎÅ¥³¡¼¥É·×
           PERFORM    VARYING   IDX1    FROM  1  BY  1
                      UNTIL     IDX1    >   31
               PERFORM   VARYING  IDX2  FROM  1  BY  1
                         UNTIL    IDX2    >   3
                   IF   WRK-SKJ-SRYCD(IDX1 IDX2)  NOT =  SPACE
                       MOVE    WRK-SKJ-SRYCD(IDX1 IDX2) TO WRK-SRYCD-X
                       COMPUTE WRK-SKJ-SRYCDTOTAL(IDX1)  =
                               WRK-SKJ-SRYCDTOTAL(IDX1)  + WRK-SRYCD-9
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key28"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-SKJ-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»ö¥ï¡¼¥¯£²¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ2-FUKUSU(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJ2-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ2-HKNCOMBI-DAY(IDX3)  =
                                      WRK-SKJ2-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ2-FUKUSU(IDX3)  =
                                       WRK-SKJ2-FUKUSU(IDX1))
                             ADD     1    TO    WRK-SKJ2-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ2-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»ö¥ï¡¼¥¯£³¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ3-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJ3-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ3-HKNCOMBI-DAY(IDX3)  =
                                      WRK-SKJ3-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ3-TOKU-SHOKU(IDX3)  =
                                       WRK-SKJ3-TOKU-SHOKU(IDX1))
                             ADD     1    TO    WRK-SKJ3-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ3-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ¿©»öÉéÃ´¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJFTN-SRYCD(IDX1)  NOT =   SPACE
                   ADD   1                  TO  WRK-SKJFTN-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJFTN-HKNCOMBI-DAY(IDX3)  =
                                    WRK-SKJFTN-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJFTN-SRYCD(IDX3)  =
                                       WRK-SKJFTN-SRYCD(IDX1))
                             ADD     1    TO    WRK-SKJFTN-KAISU(IDX1)
                             INITIALIZE   WRK-SKJFTN-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *   ·ë²ÌÉ½¼¨
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "WRK-SKJ-HKNCOMBI-DAY = " 
      **                    WRK-SKJ-HKNCOMBI-DAY(IDX1)
      **          DISPLAY  "WRK-SKJ-SRYCD(1) = " WRK-SKJ-SRYCD(IDX1 1)
      **          DISPLAY  "WRK-SKJ-SRYCD(2) = " WRK-SKJ-SRYCD(IDX1 2)
      **          DISPLAY  "WRK-SKJ-SRYCD(3) = " WRK-SKJ-SRYCD(IDX1 3)
      **          DISPLAY  "WRK-SKJ-SRYCDTOTAL = "
      **                                      WRK-SKJ-SRYCDTOTAL(IDX1)
      **          DISPLAY  "WRK-SKJ-MONEY    = " WRK-SKJ-MONEY(IDX1)
      **          DISPLAY  "WRK-SKJ-KAISU    = " WRK-SKJ-KAISU(IDX1)
      **     END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ-SRYCD(IDX1 1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    PERFORM    VARYING    IDX3   FROM  1   BY  1
                               UNTIL      IDX3   >    3
                        IF   WRK-SKJ-SRYCD(IDX1 IDX3)  NOT =  SPACE
                            ADD     1     TO    IDX4
                            MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                            IF     IDX4    =   1
                                MOVE   "97"   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                            END-IF
                            MOVE   WRK-SKJ-SRYCD(IDX1 IDX3)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        END-IF
                    END-PERFORM
                    MOVE  WRK-SKJ-MONEY(IDX1)  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                    MOVE  WRK-SKJ-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ3-TOKU-SHOKU(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ3-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   SPACE   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJ3-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    MOVE  350  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 1)
                    MOVE  WRK-SKJ3-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ2-FUKUSU(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ2-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   SPACE   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJ2-FUKUSU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    MOVE   50  TO
                                LNK-NYURECEDEN-TEN(3 IDX2 1)
                    MOVE  WRK-SKJ2-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÉéÃ´¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJFTN-SRYCD(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJFTN-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   "SI"   TO
                                  LNK-NYURECEDEN-RECSKB(3 IDX2 1)
                    MOVE   "97"   TO
                                  LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
                    MOVE   WRK-SKJFTN-SRYCD(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 1)
                    EVALUATE   WRK-SKJFTN-SRYCD(IDX1)
                        WHEN   "197000810"
                            MOVE  780  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197000910"
                            MOVE  650  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197001010"
                            MOVE  500  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                        WHEN   "197001110"
                            MOVE  300  TO 
                                        LNK-NYURECEDEN-TEN(3 IDX2 1)
                    END-EVALUATE
                    MOVE  WRK-SKJFTN-KAISU(IDX1)  TO
                                  LNK-NYURECEDEN-KAISU(3 IDX2 1)
                    MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2101-SHOKUJI-HENSHU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¤ÎÊÔ½¸¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       2102-SHOKUJI-HENSHU2-SEC        SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÄ«¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5
                                                         OR 6 OR 7)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5
                                                         OR 6 OR 7)
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
           MOVE    "key31"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÍ¼¿©¡Ë¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                         (WRK-GAIHAKU-KBN(IDX1)  =  ZERO OR 4 OR 5
                                                         OR 6 OR 7)
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
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *Äã½êÆÀ¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TSYRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  TSYRRK-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  TSYRRK-PTID
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TSYRRK-SELECT-SEC
           IF      FLG-TSYRRK              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TSYRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TSYRRK          =   1                ) OR
                 (( TSYRRK-NINSTYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TSYRRK-NINEDYMD(1:6) ))
      *
                   MOVE    "tbl_tsyrrk"     TO  MCP-TABLE
                   MOVE    "key3"           TO  MCP-PATHNAME
                   PERFORM 910-TSYRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TSYRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TSYRRK-NINSTYMD(1:6))
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
           IF  (TSYRRK-SKJGNSTYMD(1:6)  <= LNK-NYURECEDEN-SANTEIYM) AND
               (TSYRRK-SKJGNSTYMD       NOT = SPACE)
             IF (LNK-NYURECEDEN-SANTEIYM  = TSYRRK-SKJGNSTYMD(1:6)) AND
                (TSYRRK-SKJGNSTYMD(7:2)   NOT = "01")
               CONTINUE
             ELSE
               IF  TSYRRK-SKJGNSTYMD(1:6)  <  LNK-NYURECEDEN-SANTEIYM
                   MOVE   1             TO   TSY90-IDX
               ELSE
                   MOVE   TSYRRK-SKJGNSTYMD(7:2)   TO  TSY90-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM   TSY90-IDX  BY  1
                          UNTIL     IDX1   >    31
                    IF    WRK-SKJ4-RYOYO(IDX1)  NOT =  SPACE
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX1)
                        COMPUTE   IDX-TSY90  =  IDX1  +  31
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                        COMPUTE   IDX-TSY90  =  IDX1  +  62
                        MOVE   "197001010"
                                        TO  WRK-SKJFTN2-SRYCD(IDX-TSY90)
                    END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *ÄãÇ¯¶â¾ðÊó¤Î¼èÆÀ
           INITIALIZE                          TNKRRK-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  TNKRRK-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  TNKRRK-PTID
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-TNKRRK-SELECT-SEC
           IF      FLG-TNKRRK              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-TNKRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-TNKRRK          =   1                ) OR
                 (( TNKRRK-STYMD(1:6) <= LNK-NYURECEDEN-SANTEIYM AND
                    LNK-NYURECEDEN-SANTEIYM <= TNKRRK-EDYMD(1:6) ))
      *
                   MOVE    "tbl_tnkrrk"       TO  MCP-TABLE
                   MOVE    "key3"             TO  MCP-PATHNAME
                   PERFORM 910-TNKRRK-READ-SEC
      *
               END-PERFORM
           END-IF
           IF      FLG-TNKRRK              =   ZERO
              IF  (LNK-NYURECEDEN-SANTEIYM   =  TNKRRK-STYMD(1:6))
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
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿©»öÎÅÍÜÈñ¡ÊÄ«¡Ë
               IF    WRK-SKJ4-RYOYO(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1)
                                       TO  WRK-SKJ7-HKNCOMBI-DAY(IDX1)
                   MOVE  WRK-SKJ4-RYOYO(IDX1)
                                       TO  WRK-SKJ7-RYOYO(IDX1)
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÄ«¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX1)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÄ«¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
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
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÃë¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX2)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÃë¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX2)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX2)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
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
                   ADD   1             TO  WRK-SKJ7-RYOYO-SU(IDX1)
               END-IF
      *        ÆÃÊÌ¿©²Ã»»¡ÊÍ¼¡Ë
               IF    WRK-SKJ5-TOKU-SHOKU(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJ5-TOKU-SHOKU(IDX3)
                                       TO  WRK-SKJ7-TOKU-SHOKU(IDX1)
                   ADD   1             TO  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
               END-IF
      *        ¿©»öÉéÃ´¡ÊÍ¼¡Ë
               IF    WRK-SKJFTN2-SRYCD(IDX3)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX3)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
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
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
           END-PERFORM
      *
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
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
      *     END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
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
      *          DISPLAY "WRK-SKJ7-SKJFTN(IDX1) = "
      *                   WRK-SKJ7-SKJFTN(IDX1)
      *          DISPLAY "WRK-SKJ7-SKJFTN-SU(IDX1) = "
      *                   WRK-SKJ7-SKJFTN-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-KAISU =(IDX1) = "
      *                   WRK-SKJ7-KAISU (IDX1)
                IF    WRK-SKJ7-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         EVALUATE  WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                             WHEN  1
                                 MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                             WHEN  2
                                 MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                             WHEN  3
                                 MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         END-EVALUATE
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
                        END-EVALUATE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
      **     MOVE       ZERO    TO   IDX2
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÎÅÍÜ¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJ4-RYOYO(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJ4-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   "97"   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ4-RYOYO(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              EVALUATE  WRK-SKJ4-RYOYO(IDX1)
      **                 WHEN   "197000110"
      **                      MOVE  640  TO  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                 WHEN   "197000710"
      **                      MOVE  506  TO  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              END-EVALUATE
      **              MOVE  WRK-SKJ4-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÆÃÊÌ¿©²Ã»»¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJ5-TOKU-SHOKU(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJ5-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   SPACE   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ5-TOKU-SHOKU(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              MOVE  76  TO
      **                          LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              MOVE  WRK-SKJ5-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©Æ²²Ã»»¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          IF    WRK-SKJ6-SHOKUDOU(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **             MOVE  WRK-SKJ6-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   SPACE   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJ6-SHOKUDOU(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              MOVE  50  TO
      **                          LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              MOVE  WRK-SKJ6-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡Ê¿©»öÉéÃ´¡Ë
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     93)
      **          IF    WRK-SKJFTN2-SRYCD(IDX1)   NOT =  SPACE
      **              ADD    1     TO    IDX2
      **              MOVE  WRK-SKJFTN2-HKNCOMBI-DAY(IDX1) TO
      **                              LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
      **              MOVE   "SI"   TO
      **                            LNK-NYURECEDEN-RECSKB(3 IDX2 1)
      **              MOVE   "97"   TO
      **                            LNK-NYURECEDEN-SRYSKB(3 IDX2 1)
      **              MOVE   WRK-SKJFTN2-SRYCD(IDX1)  TO
      **                            LNK-NYURECEDEN-SRYCD(3 IDX2 1)
      **              EVALUATE   WRK-SKJFTN2-SRYCD(IDX1)
      **                  WHEN   "197000810"
      **                      MOVE  260  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197000910"
      **                      MOVE  210  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197001010"
      **                      MOVE  160  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **                  WHEN   "197001110"
      **                      MOVE  100  TO 
      **                                  LNK-NYURECEDEN-TEN(3 IDX2 1)
      **              END-EVALUATE
      **              MOVE  WRK-SKJFTN2-KAISU(IDX1)  TO
      **                            LNK-NYURECEDEN-KAISU(3 IDX2 1)
      **              MOVE  1   TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
      **          END-IF
      **     END-PERFORM
      *
           .
       2102-SHOKUJI-HENSHU2-EXT.
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÄ«¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))          AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "1"   OR
                                                               "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÃë¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1" OR
                                                                 "2")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÍ¼¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2" OR
                                                               "3")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
                       END-EVALUATE
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Ë¤è¤ë¿©Æ²²Ã»»ÎÎ°è¤Î¥¯¥ê¥¢
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
             IF    WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)  =   "1"
                 MOVE    SPACE     TO  WRK-SKJ6-SHOKUDOU(IDX1)
             END-IF
           END-PERFORM
      *
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
           INITIALIZE                          WRK-SYOTOKU-SKJKBN-AREA
           INITIALIZE                          ORCSSYOTOKUAREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-SYOTOKU-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-SYOTOKU-SRYYM
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
           INITIALIZE                          ORCSNYULIFE-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-NYULIFE-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-NYULIFE-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-NYULIFE-SANTEIYM
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX1)
               END-IF
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX2)
               END-IF
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX3)
               END-IF
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
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
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
               MOVE  1               TO  WRK-SKJ7-DAY(IDX1 IDX1)
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
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             IF   WRK-SKJ7-TOKU-SHOKU(IDX3) NOT = SPACE
                                 MOVE  WRK-SKJ7-TOKU-SHOKU(IDX3)  TO
                                           WRK-SKJ7-TOKU-SHOKU(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
                             END-IF
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             MOVE    1    TO    WRK-SKJ7-DAY(IDX1 IDX3)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
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
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    ADD    1     TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(4 IDX3)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        IF     IDX5    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                    END-IF
                    MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-NYURECEDEN-MEISAISU(4 IDX3)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÄ«¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))          AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "1"   OR
                                                               "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÃë¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "2"   OR
                                                               "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *
      *            Æ±ÆüºÆÆþ±¡¤ÇÂà±¡Æü¤Î¿©»öÌµ¤·¤Ç¤¢¤ì¤Ð¡¢²ñ·×¤ò¥¯¥ê¥¢¤¹¤ë
                   IF (LNK-NYUINDAY-DOUJITSU-KBN(IDX1)       = "1") AND
                      (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "2") AND
                         (LNK-NYURECEDEN-HKNKBN
                           =  LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                         (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "0" OR
                                                                 "1" OR
                                                                 "2")
                        MOVE    ZERO     TO   NACCT-DAY(IDX1)
                   END-IF
      *
                   IF    (WRK-HKNCOMBI-DAY(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¡ÊÍ¼¤«¤é¡Ë
                   IF    (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                         (LNK-NYURECEDEN-HKNKBN
                              =  LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX1)) AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)            AND
                         (LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX1) = "1" OR
                                                               "2" OR
                                                               "3")  AND
      ***                  ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
      ***                   (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                           ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                            (WRK-GAIHAKU-KBN(IDX1) >=  4))
                       MOVE   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
      *            Æ±ÆüºÆÆþ±¡Æü¤ËÂ¾ÊÝ¸±¤Ç¿©»ö¤Î»»Äê¤¬¤¢¤ë¾ì¹ç¤Ë
      *            ¿©Æ²²Ã»»¤ò¥¯¥ê¥¢¤¹¤ë
                   IF  (LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX1) = "3")
                                                                     AND
                       (LNK-NYURECEDEN-HKNKBN   NOT =
                               LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX1)) AND
                       (NACCT-DAY(IDX1)    NOT =  ZERO)              AND
                       (LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX1) = "3")  AND
                      ((WRK-GAIHAKU-KBN(IDX1)    =  ZERO)            OR
                       (WRK-GAIHAKU-KBN(IDX1)   >=  4))
      *                ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Î¥»¥Ã¥È
                        MOVE  "1"    TO  WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    ¿©Æ²²Ã»»¥¯¥ê¥¢¥Õ¥é¥°¤Ë¤è¤ë¿©Æ²²Ã»»ÎÎ°è¤Î¥¯¥ê¥¢
           PERFORM      VARYING   IDX1   FROM   1  BY  1
             UNTIL      IDX1     >    31
             IF    WRK-TAHOKEN-SHOKUDOU-FLG(IDX1)  =   "1"
                 MOVE    SPACE     TO  WRK-SKJ6-SHOKUDOU(IDX1)
             END-IF
           END-PERFORM
      *
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
           INITIALIZE                          WRK-SYOTOKU-SKJKBN-AREA
           INITIALIZE                          ORCSSYOTOKUAREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-SYOTOKU-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-SYOTOKU-SRYYM
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
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                          ORCSNYULIFE2-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-NYULIFE2-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-NYULIFE2-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"    USING  ORCSNYULIFE2-AREA
                                            SPA-AREA
           IF      LNK-NYULIFE2-RC         NOT =  ZERO
               DISPLAY  "LNK-NYULIFE2-RC = " LNK-NYULIFE2-RC
           END-IF
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥µ¥Ö
           INITIALIZE                          ORCSSKJKEEP-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-SKJKEEP-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-SKJKEEP-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-SKJKEEP-SANTEIYM
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX1)
               END-IF
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX2)
               END-IF
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX3)
               END-IF
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
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
      **         ADD   WRK-SKJ7-RYOYO-RYU-SU(IDX1)
      **                               TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               COMPUTE    WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                          WRK-SKJ7-SRYCDTOTAL(IDX1)  +
                         (WRK-SKJ7-RYOYO-RYU-SU(IDX1)  *  3)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
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
               MOVE  1               TO  WRK-SKJ7-DAY(IDX1 IDX1)
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
      *          DISPLAY "WRK-SKJ7-RYOYO-RYU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-RYU(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-RYU-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-RYU-SU(IDX1)
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
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             IF   WRK-SKJ7-TOKU-SHOKU(IDX3) NOT = SPACE
                                 MOVE  WRK-SKJ7-TOKU-SHOKU(IDX3)  TO
                                           WRK-SKJ7-TOKU-SHOKU(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
                             END-IF
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             MOVE    1    TO    WRK-SKJ7-DAY(IDX1 IDX3)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
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
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        IF     IDX4   =   1
                            MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003110"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   575     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1150     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1725     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003210"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   455     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   910     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1365     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                              IF  (LNK-NYURECEDEN-SANTEIYM
                                                        >= "201604") AND
                                  (LNK-NYURECEDEN-SANTEIYM
                                                        <= "201803")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  360  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  720  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1080  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
                              IF  (LNK-NYURECEDEN-SANTEIYM
                                                        >= "201804")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    ADD    1     TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(4 IDX3)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        IF     IDX5    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        IF     IDX5    =   1
                            MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003510"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   500     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1000     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1500     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                              IF   LNK-NYURECEDEN-SANTEIYM
                                                        >= "201604"
                                EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  360  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  720  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1080  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                END-EVALUATE
                              END-IF
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡¦°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-NYURECEDEN-MEISAISU(4 IDX3)
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
      **     DISPLAY "LNK-NYURECEDEN-SET-HKNCOMBI(1) = "
      **              LNK-NYURECEDEN-SET-HKNCOMBI(1)
      **     DISPLAY "LNK-NYURECEDEN-SET-HKNCOMBI(2) = "
      **              LNK-NYURECEDEN-SET-HKNCOMBI(2)
      **     DISPLAY "LNK-NYURECEDEN-SET-HKNCOMBI(3) = "
      **              LNK-NYURECEDEN-SET-HKNCOMBI(3)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  NACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  NACCT-SRYYM
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
      *
                   IF    (WRK-SKJ-HKNCOMBI-YORU(IDX1) NOT = ZERO)  AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                        ((WRK-NYUINKHN(IDX1) NOT =  SPACE)    OR
                         (LNK-NYUINDAY-KBN(IDX1) =  "1"  ))   AND
                        ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)     OR
                         (WRK-GAIHAKU-KBN(IDX1) >=  4))
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
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ6-SHOKUDOU(IDX1)
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"             TO  MCP-TABLE
           MOVE    "key32"                     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
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
                                  LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(10)
                       CONTINUE
                   ELSE
                       MOVE    SPACE   TO   WRK-SKJ4-TBL(IDX1)
                       INITIALIZE           WRK-SKJ4-TBL(IDX1)
                   END-IF
                 END-IF
                 IF   WRK-SKJ5-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
                   IF   WRK-SKJ5-HKNCOMBI-DAY(IDX1)  =
                                  LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(10)
                       CONTINUE
                   ELSE
                       MOVE    SPACE   TO   WRK-SKJ5-TBL(IDX1)
                       INITIALIZE           WRK-SKJ5-TBL(IDX1)
                   END-IF
                 END-IF
                 IF   WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
                   IF   WRK-SKJFTN2-HKNCOMBI-DAY(IDX1)  =
                                  LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(10)
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
                                  LNK-NYURECEDEN-SET-HKNCOMBI(1)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(2)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(3)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(4)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(5)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(6)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(7)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(8)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(9)  OR
                                  LNK-NYURECEDEN-SET-HKNCOMBI(10)
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
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-SYOTOKU-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-SYOTOKU-SRYYM
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
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î¾ðÊó¼èÆÀ
           INITIALIZE                          ORCSNYULIFE2-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-NYULIFE2-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-NYULIFE2-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-NYULIFE2-SANTEIYM
           CALL    "ORCSNYULIFE2"    USING  ORCSNYULIFE2-AREA
                                            SPA-AREA
           IF      LNK-NYULIFE2-RC         NOT =  ZERO
               DISPLAY  "LNK-NYULIFE2-RC = " LNK-NYULIFE2-RC
           END-IF
      *
      *    ¿©»öÉéÃ´¿ø¤¨ÃÖ¤­ÂÐ¾Ý¼èÆÀ¥µ¥Ö
           INITIALIZE                          ORCSSKJKEEP-AREA
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  LNK-SKJKEEP-HOSPNUM
           MOVE    LNK-NYURECEDEN-PTID     TO  LNK-SKJKEEP-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  LNK-SKJKEEP-SANTEIYM
           CALL    "ORCSSKJKEEP"    USING      ORCSSKJKEEP-AREA
                                               SPA-AREA
      *
      *    ¿©»ö¥ï¡¼¥¯£·¤ÎÊÔ½¸¡ÊÆ±°ìºÞ¤Þ¤È¤áÍÑ¡Ë
      *    ¢¨Åº»ú¤Î£³£²¤«¤é£¶£²¤ÏÆ±ÆüºÆÆþ±¡ÊÔ½¸ÍÑ
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX1)
               END-IF
               IF    WRK-SKJFTN2-SRYCD(IDX1)  NOT =   SPACE
                   MOVE  WRK-SKJFTN2-SRYCD(IDX1)
                                       TO  WRK-SKJ7-SKJFTN(IDX1)
      *            ÆñÉÂÅù¤ÈÀº¿À£±Ç¯Ä¶¤ÎÃÖ¤­´¹¤¨
                   IF    LNK-SYOTOKU-SKJKBN(IDX1)  =  SPACE
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "1"
                           MOVE   CONS-NANBYO-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(IDX1)
                       END-IF
                       IF    LNK-SKJKEEP-KBN(IDX1)    =   "2"
                           MOVE   CONS-SEISIN-SKJFTN
                                           TO  WRK-SKJ7-SKJFTN(IDX1)
                       END-IF
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
               MOVE      ZERO          TO  SET-IDX1
               COMPUTE   IDX2     =    IDX1    +   31
               IF    WRK-SKJ4-RYOYO(IDX2)  NOT =   SPACE
      *            Æ±ÆüºÆÆþ±¡¤ÇÊÝ¸±ÁÈ¹ç¤»¤¬°Û¤Ê¤ë¾ì¹çÊÔ½¸Àè¤òÊÑ¹¹
                   MOVE  IDX1          TO  SET-IDX1
                   IF  (WRK-SKJ7-HKNCOMBI-DAY(IDX1)  NOT =   ZERO) AND
                       (WRK-SKJ4-HKNCOMBI-DAY(IDX2)  NOT =   ZERO) AND
                       (WRK-SKJ4-HKNCOMBI-DAY(IDX2)  NOT =
                                            WRK-SKJ7-HKNCOMBI-DAY(IDX1))
                        COMPUTE  SET-IDX1  =   IDX1  +  31
                   ELSE
                        MOVE     IDX1      TO  SET-IDX1
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX2)
               END-IF
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
      *        ¿©»öÎÅÍÜÈñ¡ÊÍ¼¡Ë
               MOVE      ZERO          TO  IDX3
               MOVE      ZERO          TO  SET-IDX1
               COMPUTE   IDX3     =    IDX1    +   62
               IF    WRK-SKJ4-RYOYO(IDX3)  NOT =   SPACE
      *            Æ±ÆüºÆÆþ±¡¤ÇÊÝ¸±ÁÈ¹ç¤»¤¬°Û¤Ê¤ë¾ì¹çÊÔ½¸Àè¤òÊÑ¹¹
                   MOVE  IDX1          TO  SET-IDX1
                   IF  (WRK-SKJ7-HKNCOMBI-DAY(IDX1)  NOT =   ZERO) AND
                       (WRK-SKJ4-HKNCOMBI-DAY(IDX3)  NOT =   ZERO) AND
                       (WRK-SKJ4-HKNCOMBI-DAY(IDX3)  NOT =
                                            WRK-SKJ7-HKNCOMBI-DAY(IDX1))
                        COMPUTE  SET-IDX1  =   IDX1  +  31
                   ELSE
                        MOVE     IDX1      TO  SET-IDX1
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
               IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                   ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                    (WRK-SAIGAI-959(IDX1)   =   "1"))
                     MOVE     SPACE    TO   WRK-SKJFTN2-SRYCD(IDX3)
               END-IF
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
               IF   WRK-SKJ7-TOKU-SHOKU(IDX1)  NOT =  SPACE
                   MOVE    WRK-SKJ7-TOKU-SHOKU(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               IF   WRK-SKJ7-SKJFTN(IDX1)      NOT =  SPACE
                   MOVE    WRK-SKJ7-SKJFTN(IDX1)  TO WRK-SRYCD-X
                   COMPUTE WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                           WRK-SKJ7-SRYCDTOTAL(IDX1)  +  WRK-SRYCD-9
               END-IF
               ADD   WRK-SKJ7-RYOYO-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
               COMPUTE    WRK-SKJ7-SRYCDTOTAL(IDX1)  =
                          WRK-SKJ7-SRYCDTOTAL(IDX1)  +
                         (WRK-SKJ7-RYOYO-RYU-SU(IDX1)  *  3)
               ADD   WRK-SKJ7-TOKU-SHOKU-SU(IDX1)
                                     TO  WRK-SKJ7-SRYCDTOTAL(IDX1)
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
               MOVE  1               TO  WRK-SKJ7-DAY(IDX1 IDX2)
           END-PERFORM
      *
      *     DISPLAY "WRK-SKJ7  MATOME BEFORE"
      *     PERFORM      VARYING    IDX1   FROM   1  BY  1
      *                  UNTIL      IDX1   >   62
      *       IF  WRK-SKJ7-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "WRK-SKJ7-HKNCOMBI-DAY(IDX1) = "
      *                   WRK-SKJ7-HKNCOMBI-DAY(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO(IDX1) = "
      *                   WRK-SKJ7-RYOYO(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-SU(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-RYU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-RYU(IDX1)
      *          DISPLAY "WRK-SKJ7-RYOYO-RYU-SU(IDX1) = "
      *                   WRK-SKJ7-RYOYO-RYU-SU(IDX1)
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
      *       END-IF
      *     END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿¿©»ö¾ðÊó¤«¤éÆ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
      *    ¿©»ö¥ï¡¼¥¯
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   62
               IF    WRK-SKJ7-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-SKJ7-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   62
                       IF    (WRK-SKJ7-HKNCOMBI-DAY(IDX3)  =
                                       WRK-SKJ7-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-SKJ7-SRYCDTOTAL(IDX3)  =
                                       WRK-SKJ7-SRYCDTOTAL(IDX1))
                             IF   WRK-SKJ7-TOKU-SHOKU(IDX3) NOT = SPACE
                                 MOVE  WRK-SKJ7-TOKU-SHOKU(IDX3)  TO
                                           WRK-SKJ7-TOKU-SHOKU(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)
                             END-IF
                             IF   WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX3)
                                                      NOT =  ZERO
                                     ADD   1   TO  
                                        WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)
                             END-IF
                             ADD     1    TO    WRK-SKJ7-KAISU(IDX1)
                             MOVE    1    TO    WRK-SKJ7-DAY(IDX1 IDX3)
                             INITIALIZE   WRK-SKJ7-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           MOVE       ZERO    TO   IDX3
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     62)
      *       DISPLAY "WRK-SKJ7  MATOME AFTER"
      *       IF  WRK-SKJ7-HKNCOMBI-DAY(IDX1)  NOT =  ZERO
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
      *       END-IF
              IF   (WRK-SKJ7-RYOYO(IDX1)       NOT =  SPACE)  OR
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT =  SPACE)
                IF (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001310")  AND
                   (WRK-SKJ7-RYOYO(IDX1)   NOT = "197001710")  AND
                   (WRK-SKJ7-RYOYO-RYU(IDX1)   NOT = "197003510")
                    ADD    1     TO    IDX2
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(3 IDX2)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(3 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                      WHEN  "197000110"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   640     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1280     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                      WHEN  "197000710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   506     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1012     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1518     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
                    END-EVALUATE
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        IF     IDX4   =   1
                            MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(3 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003110"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   575     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE  1150     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1725     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *               Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003210"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   455     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   910     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1365     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                        END-EVALUATE
      *                 Ê¿À®£³£°Ç¯£´·î²þÀµ
                        IF    LNK-NYURECEDEN-SANTEIYM  >=  "201804"
                          EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   460     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  2
                                MOVE   920     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                            WHEN  3
                                MOVE  1380     TO  
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                          END-EVALUATE
                        END-IF
                    END-EVALUATE
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(3 IDX2 IDX4)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(3 IDX2 IDX4)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197000810"
                              IF  (LNK-NYURECEDEN-SANTEIYM
                                                        >= "201604") AND
                                  (LNK-NYURECEDEN-SANTEIYM
                                                        <= "201803")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  360  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  720  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1080  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
                              IF  (LNK-NYURECEDEN-SANTEIYM
                                                        >= "201804")
                                 EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                     WHEN   1
                                       MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   2
                                       MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                     WHEN   3
                                       MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                 END-EVALUATE
                              END-IF
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197000910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197001010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *               ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197001110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(3 IDX2 IDX4)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(3 IDX2 IDX4)
                    END-IF
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(3 IDX2)
                ELSE
      *             Æþ±¡»þÀ¸³èÎÅÍÜ¤Î¿©»öÎÅÍÜ¤ÏÅº»ú£´¤ÎÎÎ°è¤ËÊÖµÑ
                    ADD    1     TO    IDX3
                    MOVE  WRK-SKJ7-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(4 IDX3)
                    MOVE  WRK-SKJ7-DAY-TBL(IDX1)  TO
                                    LNK-NYURECEDEN-DAY-TBL(4 IDX3)
                    MOVE   ZERO     TO    IDX5
                    IF   WRK-SKJ7-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        IF     IDX5    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001310"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   554     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1108     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1662     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜ
                      WHEN  "197001710"
                        EVALUATE  WRK-SKJ7-RYOYO-SU(IDX1)
                            WHEN  1
                                MOVE   420     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE   840     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1260     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-RYOYO-RYU(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        IF     IDX5    =   1
                            MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(4 IDX3 IDX5)
                        END-IF
                        MOVE   WRK-SKJ7-RYOYO-RYU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE      WRK-SKJ7-RYOYO-RYU-SU(IDX1)  TO
                                      LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                    END-IF
                    EVALUATE  WRK-SKJ7-RYOYO-RYU(IDX1)
      *               Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜ¡ÊÎ®Æ°¿©¡Ë
                      WHEN  "197003510"
                        EVALUATE  WRK-SKJ7-RYOYO-RYU-SU(IDX1)
                            WHEN  1
                                MOVE   500     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  2
                                MOVE  1000     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                            WHEN  3
                                MOVE  1500     TO  
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                        END-EVALUATE
                    END-EVALUATE
                    MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    IF   WRK-SKJ7-SHOKUDOU(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-SHOKUDOU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   50     TO
                                 LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    IF   WRK-SKJ7-TOKU-SHOKU(IDX1)   =  SPACE
                          CONTINUE
                    ELSE
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   1    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   76     TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU1(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   2    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   152    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU2(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                      IF  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1) NOT = 0
                         ADD     1     TO    IDX5
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                         MOVE   WRK-SKJ7-TOKU-SHOKU(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                         MOVE   3    TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                         MOVE   228    TO
                                      LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                         MOVE  WRK-SKJ7-TOKU-SHOKU-KAISU3(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                      END-IF
                    END-IF
                    IF   WRK-SKJ7-SKJFTN(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX5
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(4 IDX3 IDX5)
                        MOVE   WRK-SKJ7-SKJFTN-SU(IDX1)  TO
                                  LNK-NYURECEDEN-SURYO(4 IDX3 IDX5)
                        EVALUATE   WRK-SKJ7-SKJFTN(IDX1)
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  460  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  920  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1380  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
                            WHEN   "197002110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  840  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
                            WHEN   "197002210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
                            WHEN   "197002310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
                            WHEN   "197002410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  130  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  390  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
                            WHEN   "197002510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                            WHEN   "197002710"
                              IF   LNK-NYURECEDEN-SANTEIYM
                                                        >= "201604"
                                EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  360  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  720  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE 1080  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                END-EVALUATE
                              END-IF
      *             ¡ÊÊ¿À®£³£°Ç¯²þÀµ¡Ë
      *               À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197005110"
                              EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                 WHEN   1
                                    MOVE  460  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                 WHEN   2
                                    MOVE  920  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                 WHEN   3
                                    MOVE 1380  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                              END-EVALUATE
      *             ¡ÊÊ¿À®£³£°Ç¯²þÀµ¡Ë
      *               À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197005210"
                              EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                 WHEN   1
                                    MOVE  420  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                 WHEN   2
                                    MOVE  840  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                 WHEN   3
                                    MOVE 1260  TO 
                                       LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                              END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡¦°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
                            WHEN   "197003710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  260  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  520  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  780  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°Æü°Ê²¼¡Ë¡Ë
                            WHEN   "197002810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¦
      *                                           £¹£°ÆüÄ¶¡Ë¡Ë
                            WHEN   "197002910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                            WHEN   "197003010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               ¤³¤³¤«¤éÊ¿À®£²£¹Ç¯£±£°·î¥³¡¼¥ÉÄÉ²ÃÊ¬
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã¡Ë
                            WHEN   "197003910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã¡Ë
                            WHEN   "197004010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197004110"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004210"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004310"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004410"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004510"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡Ë
                            WHEN   "197004610"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
                            WHEN   "197004710"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  210  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  420  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  630  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
                            WHEN   "197004810"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  160  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  320  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  480  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197004910"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *               À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
                            WHEN   "197005010"
                               EVALUATE   WRK-SKJ7-SKJFTN-SU(IDX1)
                                   WHEN   1
                                      MOVE  100  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   2
                                      MOVE  200  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                                   WHEN   3
                                      MOVE  300  TO 
                                         LNK-NYURECEDEN-TEN(4 IDX3 IDX5)
                               END-EVALUATE
      *
                        END-EVALUATE
      *
                        MOVE  WRK-SKJ7-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(4 IDX3 IDX5)
                    END-IF
                    MOVE  IDX5  TO  LNK-NYURECEDEN-MEISAISU(4 IDX3)
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2103-SHOKUJI-HENSHU5-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤ÎÊÔ½¸
      *****************************************************************
       2103-TOKU-SHOKU-HENSHU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-TOKU-SET
      *
      *ÆÃÊÌ¿©²Ã»»¡Ê¿©»öÎÅÍÜ¡Ë¤Î¼èÆÀ
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-NYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key54"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key54"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT   =  1)
                   MOVE   1                        TO  FLG-TOKU-SHOKU
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-NYURECEDEN-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-NYURECEDEN-PTID     TO  COMB-PTID
                   MOVE    ACCT-HKNCOMBI           TO  COMB-HKNCOMBINUM
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
                                MOVE    ZERO       TO  FLG-TOKU-SHOKU
                            ELSE
                                EVALUATE   LNK-NYURECEDEN-HKNKBN
      *                             ¼ÒÊÝ
                                    WHEN   "1"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"  OR
                                                           "068"  OR
                                                           "069"  OR
                                                           "039"  OR
                                                           "040"
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
      *                             ¹ñÊÝ
                                    WHEN   "2"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"
                                            CONTINUE
                                        ELSE
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
      *                             ¹­°èÏ¢¹ç
                                    WHEN   "3"
                                        IF   COMB-HKNNUM = "039"
                                            CONTINUE
                                        ELSE
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
                                END-EVALUATE
                            END-IF
                       ELSE
                           MOVE    ZERO            TO  FLG-TOKU-SHOKU
                       END-IF
                   END-IF
      *
      *            ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   IF   (FLG-TOKU-SHOKU        =   1)   AND
                        (ACCT-SRYKBN           =   "97")  AND
                        (ACCT-ZAIKAISU         NOT =  ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY  1
                         UNTIL   (IDX1         >   31)     OR
                                 (LNK-NYURECEDEN-HKNCOMBI(3 IDX1)
                                               =   ZERO)
                             CONTINUE
                       END-PERFORM
                       MOVE  1   TO   FLG-TOKU-SET
                       MOVE  ACCT-HKNCOMBI
                                 TO   LNK-NYURECEDEN-HKNCOMBI(3 IDX1)
                       MOVE  1   
                                 TO   LNK-NYURECEDEN-MEISAISU(3 IDX1)
                       PERFORM   VARYING   IDX-DAY   FROM  1   BY  1
                                 UNTIL     IDX-DAY   >  31
                           IF    ACCT-DAY (1 IDX-DAY)   NOT =   ZERO
                               MOVE   ACCT-DAY (1 IDX-DAY)   TO
                                            LNK-NYURECEDEN-DAY
                                                    (3 IDX1 IDX-DAY)
                           END-IF
                       END-PERFORM
                       MOVE   "SI"
                                 TO   LNK-NYURECEDEN-RECSKB(3 IDX1 1)
                       MOVE   "97"
                                 TO   LNK-NYURECEDEN-SRYSKB(3 IDX1 1)
                       MOVE   "197000470"
                                 TO   LNK-NYURECEDEN-SRYCD(3 IDX1 1)
                       MOVE   ACCT-SURYOUTOTAL
                                 TO   LNK-NYURECEDEN-SURYO(3 IDX1 1)
                       MOVE   ACCT-ZAITEN
                                 TO   LNK-NYURECEDEN-TEN(3 IDX1 1)
                       MOVE   ACCT-ZAIKAISU
                                 TO   LNK-NYURECEDEN-KAISU(3 IDX1 1)
                   END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key54"             TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key54"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
      *ÆÃÊÌ¿©²Ã»»¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î¼èÆÀ
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-NYURECEDEN-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-NYURECEDEN-PTID     TO  ACCT-PTID
           MOVE    LNK-NYURECEDEN-SANTEIYM TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key55"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key55"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT   =  1)
                   MOVE   1                        TO  FLG-TOKU-SHOKU
                   INITIALIZE                          HKNCOMBI-REC
                   MOVE    LNK-NYURECEDEN-HOSPNUM  TO  COMB-HOSPNUM
                   MOVE    LNK-NYURECEDEN-PTID     TO  COMB-PTID
                   MOVE    ACCT-HKNCOMBI           TO  COMB-HKNCOMBINUM
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
                                MOVE    ZERO       TO  FLG-TOKU-SHOKU
                            ELSE
                                EVALUATE   LNK-NYURECEDEN-HKNKBN
      *                             ¼ÒÊÝ
                                    WHEN   "1"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"  OR
                                                           "068"  OR
                                                           "069"  OR
                                                           "039"  OR
                                                           "040"
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
      *                             ¹ñÊÝ
                                    WHEN   "2"
                                        IF   COMB-HKNNUM = "060"  OR
                                                           "067"
                                            CONTINUE
                                        ELSE
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
      *                             ¹­°èÏ¢¹ç
                                    WHEN   "3"
                                        IF   COMB-HKNNUM = "039"
                                            CONTINUE
                                        ELSE
                                            MOVE  ZERO  TO
                                                       FLG-TOKU-SHOKU
                                        END-IF
                                END-EVALUATE
                            END-IF
                       ELSE
                           MOVE    ZERO            TO  FLG-TOKU-SHOKU
                       END-IF
                   END-IF
      *
      *            ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   IF   (FLG-TOKU-SHOKU        =   1)   AND
                        (ACCT-SRYKBN           =   "97")  AND
                        (ACCT-ZAIKAISU         NOT =  ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY  1
                         UNTIL   (IDX1         >   31)     OR
                                 (LNK-NYURECEDEN-HKNCOMBI(4 IDX1)
                                               =   ZERO)
                             CONTINUE
                       END-PERFORM
                       MOVE  1   TO   FLG-TOKU-SET
                       MOVE  ACCT-HKNCOMBI
                                 TO   LNK-NYURECEDEN-HKNCOMBI(4 IDX1)
                       MOVE  1   
                                 TO   LNK-NYURECEDEN-MEISAISU(4 IDX1)
                       PERFORM   VARYING   IDX-DAY   FROM  1   BY  1
                                 UNTIL     IDX-DAY   >  31
                           IF    ACCT-DAY (1 IDX-DAY)   NOT =   ZERO
                               MOVE   ACCT-DAY (1 IDX-DAY)   TO
                                            LNK-NYURECEDEN-DAY
                                                    (4 IDX1 IDX-DAY)
                           END-IF
                       END-PERFORM
                       MOVE   "SI"
                                 TO   LNK-NYURECEDEN-RECSKB(4 IDX1 1)
                       MOVE   "97"
                                 TO   LNK-NYURECEDEN-SRYSKB(4 IDX1 1)
                       MOVE   "197001570"
                                 TO   LNK-NYURECEDEN-SRYCD(4 IDX1 1)
                       MOVE   ACCT-SURYOUTOTAL
                                 TO   LNK-NYURECEDEN-SURYO(4 IDX1 1)
                       MOVE   ACCT-ZAITEN
                                 TO   LNK-NYURECEDEN-TEN(4 IDX1 1)
                       MOVE   ACCT-ZAIKAISU
                                 TO   LNK-NYURECEDEN-KAISU(4 IDX1 1)
                   END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key55"             TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key55"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *
           .
       2103-TOKU-SHOKU-HENSHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê´Ä¶­ÎÅÍÜ¡Ë¡ÊÊ¿À®£±£¸Ç¯£±£°·î¤«¤éÊ¿À®£²£¸Ç¯£³·îËø¡Ë
      *****************************************************************
       2104-KANKYO-HENSHU-SEC        SECTION.
      *
           PERFORM   VARYING   IDX1    FROM   1   BY   1
             UNTIL   IDX1      >       31
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT =  ZERO)           AND
                     (LNK-NYULIFE-KAN-SRYCD(IDX1)    NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1)     NOT =  SPACE)          OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)   OR
                     (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   1          TO  WRK-KANKYO-DAY(IDX1 IDX1)
                   IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                       ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))
                         MOVE     SPACE    TO
                                        LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                   END-IF
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
      *    Æ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-KANKYO-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-KANKYO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-KANKYO-HKNCOMBI-DAY(IDX3)  =
                                     WRK-KANKYO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-KANKYO-SRYCDTOTAL(IDX3)  =
                                       WRK-KANKYO-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-KANKYO-KAISU(IDX1)
                             MOVE    1    TO    WRK-KANKYO-DAY
                                                          (IDX1 IDX3)
                             INITIALIZE   WRK-KANKYO-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(5 IDX2)
                    MOVE  WRK-KANKYO-DAY-TBL(IDX1) TO
                                    LNK-NYURECEDEN-DAY-TBL(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(5 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                         MOVE   320     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(5 IDX2)
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
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT =  ZERO)           AND
                     (LNK-NYULIFE2-KAN-SRYCD(IDX1)   NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1)     NOT =  SPACE)          OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)   OR
                     (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   1          TO  WRK-KANKYO-DAY(IDX1 IDX1)
                   IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                       ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))
                         MOVE     SPACE    TO
                                        LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                   END-IF
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
      *    Æ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-KANKYO-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-KANKYO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-KANKYO-HKNCOMBI-DAY(IDX3)  =
                                     WRK-KANKYO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-KANKYO-SRYCDTOTAL(IDX3)  =
                                       WRK-KANKYO-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-KANKYO-KAISU(IDX1)
                             MOVE    1    TO    WRK-KANKYO-DAY
                                                          (IDX1 IDX3)
                             INITIALIZE   WRK-KANKYO-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(5 IDX2)
                    MOVE  WRK-KANKYO-DAY-TBL(IDX1) TO
                                    LNK-NYURECEDEN-DAY-TBL(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(5 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)  NOT =  SPACE
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                         MOVE   320     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(5 IDX2)
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
               IF    (WRK-HKNCOMBI-DAY(IDX1) NOT =  ZERO)           AND
                     (LNK-NYULIFE2-KAN-SRYCD(IDX1)   NOT =  SPACE)  AND
                    ((WRK-NYUINKHN(IDX1)     NOT =  SPACE)          OR
                     (LNK-NYUINDAY-KBN(IDX1)     =  "1"  ))         AND
                    ((WRK-GAIHAKU-KBN(IDX1)  =  ZERO)   OR
                     (WRK-GAIHAKU-KBN(IDX1)  >= 4))
                   MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                     TO  WRK-KANKYO-HKNCOMBI-DAY(IDX1)
                   MOVE   1          TO  WRK-KANKYO-DAY(IDX1 IDX1)
                   IF  ((WRK-SAIGAI-962(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))   OR
                       ((WRK-SAIGAI-963(IDX1)   =   "1")    AND
                        (WRK-SAIGAI-959(IDX1)   =   "1"))
                         MOVE     SPACE    TO
                                        LNK-NYULIFE2-KANFTN-SRYCD(IDX1)
                   END-IF
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
      *    Æ±°ìºÞ¤Î¤Þ¤È¤á½èÍý
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-KANKYO-SRYCDTOTAL(IDX1)  NOT =   ZERO
                   ADD   1                  TO  WRK-KANKYO-KAISU(IDX1)
                   MOVE       ZERO          TO  IDX2
                   COMPUTE    IDX2          =   IDX1    +   1
                   PERFORM    VARYING    IDX3   FROM   IDX2  BY  1
                              UNTIL      IDX3   >   31
                       IF    (WRK-KANKYO-HKNCOMBI-DAY(IDX3)  =
                                     WRK-KANKYO-HKNCOMBI-DAY(IDX1)) AND
                             (WRK-KANKYO-SRYCDTOTAL(IDX3)  =
                                       WRK-KANKYO-SRYCDTOTAL(IDX1))
                             ADD     1    TO    WRK-KANKYO-KAISU(IDX1)
                             MOVE    1    TO    WRK-KANKYO-DAY
                                                          (IDX1 IDX3)
                             INITIALIZE   WRK-KANKYO-TBL(IDX3)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ¥ì¥»ÅÅ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¤Î´Ä¶­ÎÅÍÜ¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-KANKYO-RYOYO(IDX1)   NOT =  SPACE
                    ADD    1     TO    IDX2
                    MOVE  WRK-KANKYO-HKNCOMBI-DAY(IDX1) TO
                                    LNK-NYURECEDEN-HKNCOMBI(5 IDX2)
                    MOVE  WRK-KANKYO-DAY-TBL(IDX1) TO
                                    LNK-NYURECEDEN-DAY-TBL(5 IDX2)
                    MOVE   ZERO     TO    IDX4
                    IF   WRK-KANKYO-RYOYO(IDX1)  NOT =  SPACE
                        ADD     1     TO    IDX4
                        MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                        IF     IDX4    =   1
                            MOVE   "97"   TO
                              LNK-NYURECEDEN-SRYSKB(5 IDX2 IDX4)
                        END-IF
                        MOVE   WRK-KANKYO-RYOYO(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                    END-IF
                    EVALUATE  WRK-KANKYO-RYOYO(IDX1)
                      WHEN  "197001410"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                      WHEN  "197001810"
                          MOVE   398     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                    END-EVALUATE
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    IF   WRK-KANKYO-FTN(IDX1)   =  "197001910"
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                         MOVE   370     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    END-IF
                    IF   WRK-KANKYO-FTN(IDX1)   =  "197003810"
                         ADD     1     TO    IDX4
                         MOVE   "SI"   TO
                              LNK-NYURECEDEN-RECSKB(5 IDX2 IDX4)
                         MOVE   WRK-KANKYO-FTN(IDX1)  TO
                                  LNK-NYURECEDEN-SRYCD(5 IDX2 IDX4)
                         MOVE   200     TO  
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
      *                  Ê¿À®£³£°Ç¯²þÀµ¡¡°åÎÅ¶èÊ¬£²¡¢£³¡Ê£±Æü£³£·£°±ß¡Ë
                         IF   LNK-NYURECEDEN-SANTEIYM >=  "201804"
                              MOVE  370       TO
                                      LNK-NYURECEDEN-TEN(5 IDX2 IDX4)
                         END-IF
                         MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    END-IF
      *
                    MOVE  WRK-KANKYO-KAISU(IDX1)  TO
                                    LNK-NYURECEDEN-KAISU(5 IDX2 IDX4)
                    MOVE  IDX4  TO  LNK-NYURECEDEN-MEISAISU(5 IDX2)
                END-IF
           END-PERFORM
      *
           .
       2104-KANKYO-HENSHU3-EXT.
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
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
                                               SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
                                               SYS-5000-REC
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
      *    ´µ¼ÔÊÝ¸±¾ðÊó¸¡º÷
      *****************************************************************
       910-PTHKNINF-SELECT-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊóÆÉ¹þ
      *****************************************************************
       910-PTHKNINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTHKNINF-REC
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                     PTHKNINF-REC
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊó¸¡º÷
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       910-PTKOHINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTKOHINF-REC
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                     PTKOHINF-REC
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
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
