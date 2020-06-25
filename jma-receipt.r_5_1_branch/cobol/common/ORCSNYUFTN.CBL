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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNYUFTN.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSNYUFTN  /  ÉéÃ´¶â³Û»»Äê¥µ¥Ö               
      *                                ¡ÊÆþ±¡ÍÑ¡Ë                     
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/10/29    NACL¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      ****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  00.00.01    NACL-ÌçÏÆ    03/02/17  ¸øÈñÍ¥Àè¤Ç¤ÎÉéÃ´·×»»
      *  00.00.02    NACL-¾®Æ¦Âô  03/03/06  ÅÀÅ©ÌôºÞ¤Î¤Þ¤È¤á·×»»
      *  00.00.03    NACL-ÂÀÅÄ    03/04/02  ÆþÎÏ°ú¿ô½¤Àµ
      *  00.00.04    NACL-ÌçÏÆ    03/04/16  Á¥°÷ÊÝ¸±¡ÊÈïÊÝ¸±¼Ô¡Ë¡ÜÄÌ¶ÐºÒ³²
      *                                     ¡Ü½é¿Ç»þ¡¡ÂÐ±þ
      *  00.00.05    NACL-¾®Æ¦Âô  03/05/16  ÁªÄêÆþ±¡ÎÁ¤Î¼«¸ÊÉéÃ´·×»»ÊÑ¹¹
      *  00.00.06    NACL-ÌçÏÆ    03/07/28  Ï«ºÒ¡¦¼«ÇåÀÕÆþ±¡ÂÐ±þ
      *  00.00.07    NACL-¾®Æ¦Âô  03/08/04  Æþ±¡ÎÁ¤ÎÊÝ¸±Å¬ÍÑ³°ÂÐ±þ
      *  00.00.08    NACL-ÌçÏÆ    03/12/02  ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿¹¹¿·Åù
      *  00.00.09    NACL-ÌçÏÆ    04/01/14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT7¢ª33
      *                                     SRYACCT9¢ª30
      *  01.00.10    NACL-ÌçÏÆ    04/01/21  Ï«ºÒ¤ÎÆÃÄê´ïºà(09593)ÂÐ±þ
      *  01.00.11    NACL-ÌçÏÆ    04/01/21  Ï«ºÒ¡¦¼«ÇåÀÕ¤ÎÆþ±¡¼¼ÎÁ²Ã»»
      *                                     (09594)ÂÐ±þ
      *  01.00.12    NACL-¾®Æ¦Âô  04/02/24  ·îÅÓÃæ¤Î°åÎÅµ¡´Ø¸º»»ÂÐ±þ
      *  01.00.13    NACL-¾®Æ¦Âô  04/03/11  Í­¾²¿ÇÎÅ½ê£±·²Æþ±¡´ðËÜÎÁ£±
      *                                     ¡Ê°å»ÕÇÛÃÖÅ¬¹ç¡Ë¤ÎÂÐ±þ
      *  01.00.14    NACL-¾®Æ¦Âô  04/03/16  Â¾°åÎÅµ¡´Ø¼õ¿Ç¤Î·×»»³ä¹çÊÑ¹¹
      *                                     Ê¿À®£±£¶Ç¯£´·î¤è¤ê£³£°¡ó
      *  01.00.15    NACL-ÌçÏÆ    04/04/23  ÁªÄêÆþ±¡»þ¤ÎÂ¾°åÎÅµ¡´Ø¼õ¿Ç¤Î
      *                                     »»Äê½¤Àµ
      *  01.00.16    NACL-ÌçÏÆ    04/07/01  ÅÁÉ¼ºîÀ®¶èÊ¬¡¢ÅÁÉ¼¾õÂÖ¶èÊ¬¤Î
      *                                     ÊÔ½¸½¤Àµ
      *  01.00.17    NACL-ÌçÏÆ    04/08/23  Æþ±¡ÎÁÌ¤»»Äê»þ¤Î¼¼ÎÁº¹³Û·×»»¤ò
      *                                     È½Äê½¤Àµ¡Ê¥·¥¹¥Æ¥à´ÉÍý¤è¤ê)
      *  01.00.18    NACL-ÌçÏÆ    04/09/07  ½é¿ÇÎÁ»»Äê¥Õ¥é¥°¡ÊÃÏÊý¸øÈñÍÑ¡Ë
      *                                     ¤Î¥»¥Ã¥È
      *  01.00.19    NACL-ÌçÏÆ    04/10/01  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
      *  01.00.20    NACL-ÌçÏÆ    04/11/25  Ê¡Åç¸©ÆýÍÄ»ùÆÃ¼ì·×»»ÂÐ±þ
      *  01.00.21    NACL-¾®Æ¦Âô  04/12/10  ³°Çñ¤ÈÁªÄêÆþ±¡¤Î²ñ·×ÆÉ¤ß¹þ¤ß
      *                                     ¥­¡¼ÊÑ¹¹
      *  01.00.22    NACL-ÌçÏÆ    04/12/17  ¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤òÌôºÞÅù¤Ø
      *  01.00.23    NACL-ÌçÏÆ    05/01/27  ¹ø¡¦¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤Îµ»½Ñ¡¦ÌôºÞÅù¤ÎÁªÂòÂÐ±þ
      *  01.00.24    NACL-¾®Æ¦Âô  05/05/11  ÎÅÍÜÃ´Åö¼êÅö¡ÊËÌ³¤Æ»¡ËÂÐ±þ
      *  01.00.25    NACL-ÌçÏÆ    05/05/31  ¼¼ÎÁº¹³Û¤Î·×»»½¤Àµ
      *  01.00.26    NACL-¾®Æ¦Âô  05/07/15  ¼«Èñ£±£°¹àÌÜÂÐ±þ
      *  01.00.27    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.28    NACL-ÌçÏÆ    06/01/20  É¸·çÂÐ±þ
      *  01.03.01    NACL-ÌçÏÆ    06/03/09  ¤¤¤ï¤­»Ô½Å¿´ÂÐ±þ
      *  01.03.02    NACL-ÌçÏÆ    06/03/20  ¼«Î©»Ù±çË¡ÂÐ±þ
      *  01.03.03    NACL-ÌçÏÆ    06/03/30  Ï«ºÒÆ±Æü½é¿ÇÂÐ±þ
      *  01.03.04    NACL-ÌçÏÆ    06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  01.03.05    NACL-ÌçÏÆ    06/06/16  ¥Þ¥ëÄ¹¤¬¤¢¤ë¾ì¹ç¤ÎÍ¥Àè½ç°ÌÊÑ¹¹
      *  03.01.01    NACL-ÌçÏÆ    06/07/06  ÎÎ¼ý½ñÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/08/10  ¹âÎð¼Ô¾å¸Â³Û·Ð²áÁ¼ÃÖÂÐ±þ
      *  03.02.02    NACL-ÌçÏÆ    06/08/18  Ê¡²¬¸©µëÉÕ³°ÅÀ¿ô·×»»½¤Àµ
      *  03.02.03    NACL-ÌçÏÆ    06/09/05  ·ò¹¯ÊÝ¸±Ë¡£±£°·î²þÀµÂÐ±þ
      *  03.02.04    NACL-ÌçÏÆ    06/10/04  ·×»»¤ÎÍ¥Àè½ç°ÌÊÔ½¸½¤Àµ
      *  03.04.01    NACL-ÌçÏÆ    07/01/25  ¾ÃÈñÀÇÁí³ÛÉ½¼¨ÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/03/08  ·×»»¤ÎÍ¥Àè½ç°ÌÊÔ½¸½¤Àµ
      *  03.04.03    NACL-ÌçÏÆ    07/03/22  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *                                     ¡ÊÀî¸ý¡¦½ÕÆüÉô»Ô¡Ë
      *  03.04.04    NACL-ÌçÏÆ    07/05/02  ¹âÎð¼Ô¤Î·×»»½¤Àµ
      *  03.04.05    NACL-ÌçÏÆ    07/05/22  ½é¿ÇÎÁ»»Äê»þ¤Î¾ò·ïÄÉ²Ã
      *  03.05.00    NACL-ÂÀÅÄ    07/05/09  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.01    NACL-ÌçÏÆ    07/09/13  ½êÂô»ÔÆýÍÄ»ùÂÐ±þ
      *  04.01.01    NACL-ÂÀÅÄ    07/11/20  ¼¼ÎÁº¹³Û£¶·åÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/12/07  ¸ø³²ÂÐ±þ
      *  04.01.03    NACL-ÌçÏÆ    07/12/19  Æþ±¡ÎÁÌ¤»»ÄêÆüÂÐ±þ
      *  04.01.04    NACL-ÌçÏÆ    08/01/18  ºë¶Ì¸©ÀîÅçÄ®ÃÏÊý¸øÈñÂÐ±þ
      *  04.01.05    NACL-ÌçÏÆ    08/01/21  Æ±ÆüºÆÆþ±¡¿©»öÂÐ±þ
      *  04.01.06    NACL-ÌçÏÆ    08/02/04  É¸·ç³°Çñ·×»»½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/02/29  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.02.02    NACL-ÌçÏÆ    08/04/28  ºë¶Ì¸©Àî±Û»ÔÃÏÊý¸øÈñÂÐ±þ
      *  04.02.03    NACL-ÌçÏÆ    08/05/08  ÅÀÅ©£³£³£²¤Î¼êµ»ÎÁÊÔ½¸
      *  04.02.04    NACL-ÌçÏÆ    08/05/29  Æþ±¡ÎÁ²Ã»»ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¥Á¥§¥Ã¥¯
      *  04.02.05    NACL-ÌçÏÆ    08/06/15  Æþ±¡ÎÁ²Ã»»³°Çñ¥Á¥§¥Ã¥¯
      *  03.02.06    NACL-ÌçÏÆ    08/07/28  ·×»»Í¥Àè½ç°ÌÊÔ½¸½¤Àµ¡ÊÄ¹´ü¡Ë
      *  03.02.07    NACL-ÌçÏÆ    08/08/27  Æþ±¡ÎÁ²Ã»»³°Çñ¥Á¥§¥Ã¥¯½¤Àµ
      *  04.02.08    NACL-ÌçÏÆ    08/10/07  Æþ±¡´ðËÜÎÁÍ­¸ú´ü´Ö¥Á¥§¥Ã¥¯
      *  04.02.09    NACL-ÌçÏÆ    08/10/30  ÊÝ¸±ÁÈ¹ç¤»Í­¸ú´ü´Ö¥Á¥§¥Ã¥¯
      *  04.03.01    NACL-ÌçÏÆ    09/04/01  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *
      *  04.04.01    NACL-ÌçÏÆ    09/08/18  ½Ð»º°ì»þ¶âÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/10/01  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/11/05  ¹âÎð¼ÔÂ¾ÊÝ¸±Æþ±¡ÂÐ±þ
      *  04.04.04    NACL-ÌçÏÆ    09/11/13  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.05    NACL-ÌçÏÆ    09/12/15  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡ÊÈ¬Ä¬»Ô¡Ë
      *  04.04.06    NACL-ÌçÏÆ    10/03/11  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *  04.04.07    NACL-ÌçÏÆ    10/03/31  ²þÀµÂÐ±þ¡ÊðôÉô¸ÇÄêÂÓ²Ã»»¤ÎÂÐ±þ¡Ë
      *  04.04.08    NACL-ÌçÏÆ    10/04/14  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡Ê¹ÔÅÄ»Ô¡Ë
      *  04.04.09    NACL-ÌçÏÆ    10/05/07  Æþ±¡ÎÁºÞÅÀ¥Á¥§¥Ã¥¯
      *  04.04.10    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  04.04.11    NACL-ÌçÏÆ    10/07/20  µßµÞÈÂÁ÷¿ÇÎÅÎÁ¤Î¥Á¥§¥Ã¥¯
      *  04.04.12    NACL-ÌçÏÆ    10/08/09  Â¾°å¼õ¿Ç¤Î¥Á¥§¥Ã¥¯
      *                                     Â¾°å¼õ¿Ç¡Ê³°Çñ£´¡Ë¤ÎÁªÄê»þÂÐ±þ
      *  04.04.13    NACL-ÌçÏÆ    10/12/06  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *
      *  04.05.01    NACL-ÌçÏÆ    09/12/04  ¿©»ö¡ÊÆÃÊÌ¿©¡Ë¤ÎÂ¾ÊÝ¸±ÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    10/01/07  ¼£¸³ÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    11/03/09  Ê¡Åç¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.05.04    NACL-ÌçÏÆ    11/08/01  ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»ÂÐ±þ
      *  04.05.05    NACL-ÌçÏÆ    11/08/16  Ï«ºÒÉÂ°áÂßÍ¿ÎÁÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    11/09/30  Ï«ºÒÉÂ°áÂßÍ¿ÎÁÂÐ±þ¤Î½¤Àµ
      *  04.05.07    NACL-ÌçÏÆ    11/11/04  µßµÞÈÂÁ÷¿ÇÎÅÎÁ»»ÄêÂÐ±þ
      *  04.05.08    NACL-ÌçÏÆ    11/11/09  Æ±ÆüºÆÆþ±¡¿©»öÂÐ±þ
      *  04.05.09    NACL-ÌçÏÆ    11/11/17  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡Ê¾¾ÉúÄ®¡Ë
      *  04.05.10    NACL-ÌçÏÆ    12/03/13  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¶¡¤£·¤ÎÂÐ±þ¡Ë
      *  04.05.11    NACL-ÌçÏÆ    12/09/18  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¸¡¤£¹¤ÎÂÐ±þ¡Ë
      *
      *  04.06.01    NACL-ÌçÏÆ    10/12/06  Ï«ºÒ¡¦¼«ÇåÀÕ¤Î¶â³Û¥Õ¥ê¡¼ÆþÎÏ
      *                                     ¥³¡¼¥ÉÂÐ±þ
      *  04.06.02    NACL-ÌçÏÆ    13/02/18  Ï«ºÒ¥ì¥»ÅÅÂÐ±þ
      *                                     ¡Ê¿·¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÂÐ±þ¡Ë
      *  04.06.03    NACL-ÌçÏÆ    13/02/27  Ê¿À®£²£µÇ¯£´·î²þÀµÂÐ±þ
      *  04.06.04    NACL-ÌçÏÆ    13/06/12  Ï«ºÒ¡¦¼«ÇåÀÕ¤Î
      *                                     ´Ç¸îÎÁ³äÁý²Ã»»ÂÐ±þ
      *  04.06.05    NACL-ÌçÏÆ    13/07/04  ·îËö¤ËÆ±ÆüºÆÆþ±¡¤·¤¿¾ì¹ç¡¢¤«¤Ä¡¢
      *                                     Äê´üÀÁµá»þ¤Î·×»»½¤Àµ
      *  04.06.06    NACL-ÌçÏÆ    13/09/09  ÅÀ¿ô¥Þ¥¹¥¿·îÅÓÃæ¶â³ÛÊÑ¹¹ÂÐ±þ
      *                                     ¡Ê£°£¹£µ£¹£±¡Á£¹£´¡Ë
      *
      *  04.07.01    NACL-ÌçÏÆ    12/08/28  ÌôºÞÎÁ¸ºÅÀÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    13/07/10  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡ËÂÐ±þ
      *  04.07.03    NACL-ÌçÏÆ    14/02/12  ½Å¾ÉÈéÉæÄÙáç´ÉÍý²Ã»»ÂÐ±þ
      *  04.07.04    NACL-ÌçÏÆ    14/02/13  Ï«ºÒ¡¦¼«ÇåÀÕ¤Î»Ü½Ñ²Ã»»ÂÐ±þ
      *  04.07.05    NACL-ÌçÏÆ    14/02/18  Ï«ºÒ¡¦¼«ÇåÀÕ¡¦¸ø³²¤ÇÆþ±¡ÎÁ¤ò»»Äê
      *                                     ¤·¡¢°åÊÝ¤ÇÆþ±¡ÎÁ²Ã»»¤Î¿ÇÎÅ¹Ô°ÙÆþ
      *                                     ÎÏ¤ò¹Ô¤Ã¤¿¾ì¹ç¤Î¥Á¥§¥Ã¥¯½¤Àµ
      *  04.07.06    NACL-ÌçÏÆ    14/03/01  Ï«ºÒ¥Þ¥¹¥¿ÄÉ²ÃÂÐ±þ
      *  04.07.07    NACL-ÌçÏÆ    14/03/04  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.08    NACL-ÌçÏÆ    14/03/26  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÌë¶Ð»þ´Ö·Ð²á¸º»»¤ÎÁªÄêÂÐ±þ¡Ë
      *  04.07.09    NACL-ÌçÏÆ    14/06/17  ±ÉÍÜ´ÉÍýÂÎÀ©¸º»»µ¬Äê³ºÅöÂÐ±þ
      *  04.07.10    NACL-ÌçÏÆ    14/07/17  ¥Þ¥ëÄ¹´µ¼Ô¤Ç¡¢·îÅÓÃæ¤Ë¸øÈñ¤¬ÄÉ²Ã¤È
      *                                     ¤Ê¤Ã¤¿¾ì¹ç¤ÎÂÐ±þ
      *  04.07.11    NACL-ÌçÏÆ    14/07/31  Âè»°¼Ô¹Ô°Ù¤Î¸½ÊªµëÉÕÂÐ±þ
      *  04.07.12    NACL-ÌçÏÆ    14/09/16  Êñ³ç¿ÇÎÅÊ¬¤Î¤ß¤Î¾ì¹ç¤â²¾¼ýÇ¼
      *                                     ¥Ç¡¼¥¿¤òºîÀ®¤¹¤ë
      *  04.07.13    NACL-ÌçÏÆ    14/12/09  Ê¿À®£²£·Ç¯£±·î²þÀµÂÐ±þ
      *                                     ¡Ê½éºÆ¿ÇÎÁ¡¦ÄãÂÅ·ëÎ¨ÂÐ±þ¡Ë
      *  04.07.14    NACL-ÌçÏÆ    15/01/16  Æþ±¡ÅÀÅ©¡§¿ÇÎÅ¼ïÊÌ¡Ö¡¥£³£´£°¡×¤Î
      *                                     ÆþÎÏ¥Á¥§¥Ã¥¯ÂÐ±þ
      *  04.07.15    NACL-ÌçÏÆ    15/02/02  ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°µ¡Ç½ÄÉ²ÃÂÐ±þ
      *  04.07.16    NACL-ÌçÏÆ    15/06/02  Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ»»Äê»þ¤Î
      *                                     Æþ±¡ÉéÃ´¶â·×»»ÂÐ±þ
      *
      *  04.08.01    NACL-ÌçÏÆ    15/11/09  ³ºÅö´ü´Ö¤Ë£±£°Ä¶¤ÎÊÝ¸±ÁÈ¹ç¤»¤Î
      *                                     »ÈÍÑ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
      *  04.08.02    NACL-ÌçÏÆ    16/03/03  ¸ø³²¤ÎÎÅÍÜÃ´Åö¼êÅö¤Ï
      *                                     ÌôºÞ¡¦´ïºàÅÀ¤Î¼è¤ê°·¤¤¤È¤¹¤ë
      *  04.08.03    NACL-ÌçÏÆ    16/03/07  Ê¿À®£²£¸Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.04    NACL-ÌçÏÆ    16/09/06  Æþ±¡¼¼ÎÁ²Ã»»º¹³ÛÀÁµá¥Þ¥¹¥¿¤È
      *                                     ÆÃÊÌÏ«ºÒÉÕÅº´Ç¸îÎÁº¹³ÛÀÁµá
      *                                     ¥Þ¥¹¥¿¤ÎÂÐ±þ
      *  04.08.05    NACL-ÌçÏÆ    17/02/21  ½é¿Ç»»Äê¾ò·ïÄÉ²Ã
      *  04.08.06    NACL-ÌçÏÆ    17/04/06  Æþ±¡´ü´Ö³°¤ÎÆþ±¡¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *                                     ¥Á¥§¥Ã¥¯
      *  04.08.07    NACL-ÌçÏÆ    17/10/12  ÂçºåÉÜÃÏÊý¸øÈñÂÐ±þ
      *  04.08.08    NACL-ÌçÏÆ    17/10/18  Æþ±¡ÎÁ¤È°Û¤Ê¤ëÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
      *                                     ¤Ç¤Î¿©»ö»»ÄêÂÐ±þ
      *  04.08.09    NACL-ÌçÏÆ    17/12/15  Âè»°¼Ô¹Ô°Ù¸½ÊªµëÉÕ¤Ë´Ø¤ï¤ë
      *                                     ÉéÃ´¶â·×»»ÂÐ±þ
      *  04.08.10    NACL-ÌçÏÆ    18/03/01  Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.11    NACL-ÌçÏÆ    18/03/27  Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.12    NACL-ÌçÏÆ    18/05/10  Æþ±¡²ñ·×¾È²ñ¤ÎÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
      *                                     ¡Ê£¹£¹£¹¡Ë¥Á¥§¥Ã¥¯ÂÐ±þ
      *  04.08.13    NACL-ÌçÏÆ    18/05/15  Æ±°ì²ñ·×Ê¬¤Î¥Ç¡¼¥¿ÊÔ½¸½¤Àµ
      *  04.08.14    NACL-ÌçÏÆ    18/05/31  ·×»»¶â³Û·å¿ô¥Á¥§¥Ã¥¯
      *  04.08.15    NACL-¾®Æ¦Âô  18/06/14  ËýÀ­°Ý»ýÆ©ÀÏ´ÉÍý²Ã»»¡ÊÎÅÍÜ
      *                                     ÉÂÅïÆþ±¡ÎÁ£±¡Ë¤Ë¤Ä¤¤¤ÆÆþ±¡ÎÁ
      *                                     ¤È¤ÎÊÝ¸±¥Á¥§¥Ã¥¯ÂÐ¾Ý³°¤Ë¤¹¤ë
      *
      *  05.00.01    NACL-ÌçÏÆ    18/11/07  ´Î¤¬¤ó¡¦½ÅÅÙ´Î¹ÅÊÑ¼£ÎÅ¸¦µæÂ¥¿Ê
      *                                     »ö¶È°åÎÅÂÐ±þ
      *  05.00.02    NACL-ÌçÏÆ    18/11/16  µÜ¾ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  05.00.03    NACL-ÌçÏÆ    19/08/28  ÎáÏÂ¸µÇ¯£±£°·î²þÀµ¡Ê¾ÃÈñÀÇ¡Ë
      *  05.00.04    NACL-ÌçÏÆ    19/09/18  Âà±¡ºÆ·×»»½¤Àµ
      *  05.00.05    NACL-ÌçÏÆ    19/10/11  Æ±ÆüºÆÆþ±¡¤ÎÄê´üÀÁµá·×»»¡Ê·×»»
      *                                     ´ü´Ö£±Æü¡Ë½¤Àµ
      *  05.00.06    NACL-ÌçÏÆ    20/03/09  ÎáÏÂ£²Ç¯£´·î²þÀµ
      *                                     Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÊÔ½¸
      *  05.00.07    NACL-ÌçÏÆ    20/03/16  Â¾°åÎÅµ¡´Ø¼õ¿Ç¡ÜÉ¸·ç¤Î·×»»½¤Àµ
      *  05.00.08    NACL-ÌçÏÆ    20/04/14  ÎáÏÂ£²Ç¯£´·î²þÀµ
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  WRK-AREA.
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-ZAIKAISU                PIC 9(04).
           03  WRK-TAX1001-KBN             PIC X(01).
           03  WRK-4001-KOTEIKBN           PIC X(01).
           03  WRK-KBNCD.
               05  WRK-KBNCD-X             PIC 9(02).
           03  WRK-KBNCD2                  PIC X(06).
           03  WRK-HYOKETU-TEN             PIC 9(09)V99.
           03  WRK-BRMSGK-TAXKBN           PIC X(01).
           03  WRK-KEISAN                  PIC 9(08).
           03  WRK-KEISAN2                 PIC 9(08).
           03  WRK-CHECK-STDAY             PIC 9(02).
           03  WRK-CHECK-EDDAY             PIC 9(02).
           03  WRK-MSGCD                   PIC 9(02).
           03  WRK-NYUINRYO-ERR            PIC 9(01).
           03  WRK-NYUINRYO-ERR2           PIC 9(01).
           03  WRK-GENTEN-ERR              PIC 9(01).
           03  WRK-GENTEN-ERR2             PIC 9(01).
           03  WRK-KYUKYUHANSO-ERR         PIC 9(01).
           03  WRK-TAIJYUSIN-ERR           PIC 9(01).
           03  WRK-GENSAN-ERR              PIC 9(01).
           03  WRK-GENSAN-ERR2             PIC 9(01).
           03  WRK-TENTEKI-ERR             PIC 9(01).
           03  WRK-NYUINKIKANGAI-ERR       PIC 9(01).
           03  WRK-KENSU-ERR               PIC 9(01).
           03  WRK-SKJ-ERR1                PIC 9(01).
           03  WRK-SKJ-ERR2                PIC 9(01).
           03  WRK-HANTEI-YMD              PIC X(08).
           03  WRK-HKNCOMBINUM             PIC 9(04).
           03  WRK-HIKAKU-ZAITEN           PIC 9(08).
           03  WRK-TENSU-KEISAN            PIC 9(08).
           03  WRK-TENSU-KEISAN2           PIC 9(08)V9(5).
           03  WRK-JNISSU-ERR              PIC 9(01).
           03  WRK-DOJITU-TENTEK           PIC 9(07).
           03  WRK-HKNJANUM                PIC X(08).
           03  WRK-YUSENYMD                PIC X(08).
           03  WRK-FUTANWARI               PIC 9V99.
           03  WRK-TAIHI-DAY               PIC 9(02).
           03  WRK-SKYCHK-KEISAN           PIC 9(10).
           03  WRK-SKYCHK-KEISAN2          PIC 9(10).
           03  WRK-SKYCHK-KEISAN3          PIC 9(10).
           03  WRK-S1                      PIC 9(03).
           03  WRK-S2                      PIC 9(03).
           03  WRK-S3                      PIC 9V99999.
           03  WRK-S4                      PIC 9V99.
           03  WRK-S5                      PIC 9V99.
           03  WRK-KANGAN-SRYYMD           PIC X(08).
           03  WRK-KANGAN-SKYEDYMD         PIC X(08).
      *
       01  WRK-ROSAI-AREA.
           03  WRK-SYUTEN                  PIC 9(09).
           03  WRK-TEN                     PIC 9(09).
           03  WRK-RSISHOSHIN              PIC 9(09).
           03  WRK-RSISDO                  PIC 9(09).
           03  WRK-RSIETC                  PIC 9(09).
           03  WRK-RSIETC2                 PIC 9(09).
           03  WRK-TEN-KG                  PIC 9(09).
           03  WRK-TEN-KG2                 PIC 9(09).
           03  WRK-SINDAN                  PIC 9(09).
           03  WRK-MEISAI                  PIC 9(09).
           03  WRK-KYUKYU                  PIC 9(09).
           03  WRK-YOUBUKASAN              PIC 9(09).
           03  WRK-KYOBUKASAN              PIC 9(09).
           03  WRK-KEIBUKASAN              PIC 9(09).
           03  WRK-SOGAIKASAN              PIC 9(09).
           03  WRK-RYOYOTEATE              PIC 9(09).
           03  WRK-KOGAI-TEN               PIC 9(09).
      *
       01  WRK-JIBAI-TABLE.
           02  WRK-JIBAI-TBL     OCCURS   10.
              03 WRK-JIBAI-SYUTEN          PIC 9(09).
              03 WRK-JIBAI-SINDAN          PIC 9(09).
              03 WRK-JIBAI-MEISAI          PIC 9(09).
              03 WRK-JIBAI-KYUKYU          PIC 9(09).
              03 WRK-JIBAI-RYOYO           PIC 9(09).
              03 WRK-JIBAI-RYOYO-NACCT     PIC 9(09).
              03 WRK-JIBAI-SONOTA.
                04  WRK-JIBAI-SONOTA-OCC          OCCURS  50.
                  05  WRK-JIBAI-SONOTA-SRYCD      PIC  X(09).
                  05  WRK-JIBAI-SONOTA-ZAIKAISU   PIC  9(07).
                  05  WRK-JIBAI-SONOTA-TANKA      PIC  9(07).
      *
      *    Æþ±¡ÎÁÌ¤»»Äê¶èÊ¬
       01  WRK-NYUINDAY-TABLE.
           02  WRK-NYUINDAY-TBL  OCCURS   10.
              03 WRK-NYUINDAY-KBN          PIC X(01)  OCCURS  31.
      *
       01  WRK-NYUINDAY2-TABLE.
           02  WRK-NYUINDAY2-TBL OCCURS   10.
              03 WRK-NYUINDAY2-KBN         PIC X(01)  OCCURS  31.
      *
       01  WRK-NYUINDAY-KYUKYU-TABLE.
           02  WRK-NYUINDAY-KYUKYU-TBL
                                 OCCURS   10.
              03 WRK-NYUINDAY-KYUKYU-KBN   PIC X(01)  OCCURS  31.
      *
       01  WRK-NYUINDAY-TANKI-TABLE.
           02  WRK-NYUINDAY-TANKI-TBL
                                 OCCURS   10.
              03 WRK-NYUINDAY-TANKI-KBN    PIC X(01)  OCCURS  31.
      *
       01  WRK-NYUSANTEI-TABLE.
           02  WRK-NYUSANTEI-TBL OCCURS   10.
              03 WRK-NYUSANTEI-KBN         PIC X(01)  OCCURS  31.
      *
      *    Æ±ÆüºÆÆþ±¡¥Æ¡¼¥Ö¥ë
       01  WRK-DOJITU-TABLE.
           02  WRK-DOJITU-TBL    OCCURS   10.
              03 WRK-DOJITU-FLG            PIC 9(01)  OCCURS  31.
              03 WRK-DOJITU-FLG2           PIC 9(01)  OCCURS  31.
              03 WRK-DOJITUSKJST-FLG       PIC X(01)  OCCURS  31.
              03 WRK-DOJITUSKJED-FLG       PIC X(01)  OCCURS  31.
      *
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  WRK-SKJHKN-TABLE.
           02  WRK-SKJHKN-TBL    OCCURS   10.
              03  WRK-SKJHKN-HKNCOMBI      PIC X(04)  OCCURS  31.
              03  WRK-SKJHKN-HKNCOMBI-DOU  PIC X(04)  OCCURS  31.
      *       Æþ±¡Ê¬¤ÎÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
              03  WRK-SKJHKN-HKNCOMBI-XXX  PIC X(04)  OCCURS  31.
      *       Æ±ÆüºÆÆþ±¡Ê¬¤ÎÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
              03  WRK-SKJHKN-HKNCOMBI-DOU-XXX
                                           PIC X(04)  OCCURS  31.
      *
      *    ÆüÊÌÅÀ¿ô
       01  WRK-DAYTEN-TABLE.
           02  WRK-DAYTEN-TBL    OCCURS  10.
              03 WRK-DAYTEN-SRYYM          PIC X(06).
              03 WRK-DAYTEN-HKNCOMBINUM    PIC X(04).
              03 WRK-DAYTEN-TBL2.
                05 WRK-DAYTEN-HKNTEN       PIC 9(07)  OCCURS  31.
      *
       01  WRK-NYUINDAY-TAIHI-TABLE.
           02  WRK-NYUINDAY-TAIHI-TBL      OCCURS    2.
              03  WRK-NYUINDAY-TAIHI-KBN   PIC X(01)  OCCURS  31.
              03  WRK-NYUINDAY-TAIHI-KYUKYU-KBN
                                           PIC X(01)  OCCURS  31.
              03  WRK-NYUINDAY-TAIHI-TANKI-KBN
                                           PIC X(01)  OCCURS  31.
              03  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI
                                           PIC 9(04)  OCCURS  31.
              03  WRK-NYUINDAY-TAIHI-DOU-SKJKBN
                                           PIC X(01)  OCCURS  31.
      *
       01  WRK-SKJHKN-TAIHI-TABLE.
           02  WRK-SKJHKN-TAIHI-TBL        OCCURS    2.
              03  WRK-SKJHKN-TAIHI-HKNCOMBI
                                           PIC X(04)  OCCURS  31.
              03  WRK-SKJHKN-TAIHI-HKNCOMBI-DOU
                                           PIC X(04)  OCCURS  31.
      *
       01  WRK-TAISYOGAI-HANTEI-TABLE.
           02  WRK-TAISYOGAI-HANTEI-TBL    OCCURS   10.
              03 WRK-TAISYOGAI-HANTEI      PIC 9(01).
      *
       01  WRK-SYOSIN-TABLE.
           02  WRK-SYOSIN-TBL    OCCURS   10.
              03 WRK-SYOSIN                PIC X(01).
      *
       01  WRK-SYOSIN-DAY-TABLE.
           02  WRK-SYOSIN-DAY-TBL          OCCURS 10.
               03  WRK-SYOSIN-DAY          PIC X(01)  OCCURS  31.
      *
       01  WRK-SYOSIN2-TABLE.
           02  WRK-SYOSIN2-TBL   OCCURS   10.
              03 WRK-SYOSIN2               PIC X(01).
      *
       01  WRK-KAIKEING-TABLE.
           02  WRK-KAIKEING-TBL  OCCURS   10.
              03 WRK-KAIKEING              PIC 9(01).
      *
       01  WRK-KAIKEING2-TABLE.
           02  WRK-KAIKEING2-TBL OCCURS   10.
              03 WRK-KAIKEING2             PIC 9(01).
      *
      *    ¹âÎð¼ÔÂ¾ÊÝ¸±Æþ±¡¥Õ¥é¥°
       01  WRK-KOREI-TANYUIN-TABLE.
           02  WRK-KOREI-TANYUIN-TBL  OCCURS   10.
              03 WRK-KOREI-TANYUINFLG      PIC X(01).
      *
       01  WRK-DAISANYUSENFLG-TABLE.
           02  WRK-DAISANYUSENFLG-TBL OCCURS    2.
              03 WRK-DAISANYUSENFLG        PIC X(01).
      *
       01  WRK-YAKTEN-TABLE.
           02  WRK-YAKTEN-TBL         OCCURS   10.
              03 WRK-CSY-YAKTEN            PIC 9(07).
      *
       01  WRK-KANGOGEN-TAIHI-TABLE.
           02  WRK-KANGOGEN-TAIHI-TBL      OCCURS  2.
              03 WRK-KANGOGEN-TAIHI-SRYCD  PIC X(09)  OCCURS  31.
      *
       01  WRK-KANGOGEN-TABLE.
           02  WRK-KANGOGEN-TBL            OCCURS 10.
              03 WRK-KANGOGEN-SRYCD        PIC X(09)  OCCURS  31.
      *
      *    ÀÁµáÅÀ¿ô¥Á¥§¥Ã¥¯ÍÑ
       01  WRK-SKYTEN-TABLE.
           02  WRK-SKYTEN-TBL              OCCURS 16.
              03 WRK-SKYTEN                PIC 9(10).
              03 WRK-SKYTEN-GEN            PIC 9(10).
      *
       01  WRK-SKYYM-TABLE.
           02  WRK-SKYYM-TBL     OCCURS    2.
              03   WRK-SKYYM               PIC X(06).
              03   WRK-HI-ST               PIC 9(02).
              03   WRK-HI-ED               PIC 9(02).
      *       ¸©ÈÖ¹æ
              03   WRK-PREFNUM             PIC 9(02).
              03   WRK-DAY-TX    OCCURS   31.
                 04   WRK-DAY1X            PIC 9(04).
                 04   WRK-DAY5X            PIC 9(04).
      *
       01  WRK-DAY-TABLE.
           02  WRK-DAY-TBL       OCCURS    2.
              03 WRK-DAY-T       OCCURS   31.
                 04   WRK-DAY1             PIC 9(04).
                 04   WRK-DAY5             PIC 9(04).
                 04   WRK-DAY6             PIC 9(04).
      *
       01  WRK-HANBETU-TABLE.
           02  WRK-HANBETU-TBL OCCURS   10.
              03 WRK-HANBETU-KEY.
                05 WRK-HANBETU-SKYYM       PIC X(06).
                05 WRK-HANBETU-DAISANFLG   PIC X(01).
                05 WRK-HANBETU-YUSENYMD.
                   07 WRK-HANBETU-YUSENYM  PIC 9(06).
                   07 WRK-HANBETU-YUSENDD  PIC 9(02).
                05 WRK-HANBETU-YUSENBAN    PIC 9(03).
                05 WRK-HANBETU-CYOKIYMD.
                   07 WRK-HANBETU-CYOKIYM  PIC 9(06).
                   07 WRK-HANBETU-CYOKIDD  PIC 9(02).
                05 WRK-HANBETU-YUSENBAN2   PIC 9(03).
                05 WRK-HANBETU-KAISHI      PIC X(02).
              03 WRK-HANBETU-HKNCOMBINUM   PIC 9(04).
              03 WRK-HANBETU-DAY           PIC X(01)   OCCURS   31.
              03 WRK-HANBETU-DAY-NACCT     PIC X(01)   OCCURS   31.
              03 WRK-HANBETU-KAISHI2       PIC X(02).
              03 WRK-HANBETU-SYURYO        PIC X(02).
              03 WRK-HANBETU-SYUHKNNUM     PIC X(03).
              03 WRK-HANBETU-KOH1HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH2HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH3HKNNUM    PIC X(03).
              03 WRK-HANBETU-KOH4HKNNUM    PIC X(03).
              03 WRK-HANBETU-PTFTNRATE     PIC 9(03).
              03 WRK-HANBETU-SENINFLG      PIC X(01).                   
              03 WRK-HANBETU-SENINFLG2     PIC X(01).                   
              03 WRK-HANBETU-TEKSTYMD      PIC X(08).
              03 WRK-HANBETU-TEKEDYMD      PIC X(08).
              03 WRK-HANBETU-HKNJANUM      PIC X(08).
      *
       01  WRK-HANBETU2-TABLE.
           02  WRK-HANBETU2-TBL OCCURS   10.
              03 WRK-HANBETU2-SKYYM        PIC X(06).
              03 WRK-HANBETU2-HKNCOMBINUM  PIC 9(04).
              03 WRK-HANBETU2-DAY          PIC X(01)   OCCURS   31.
      *
       01  WRK-HANBETU3-TABLE.
           02  WRK-HANBETU3-TBL OCCURS   10.
              03 WRK-HANBETU3-SKYYM        PIC X(06).
              03 WRK-HANBETU3-HKNCOMBINUM  PIC 9(04).
              03 WRK-HANBETU3-DAY          PIC X(01)   OCCURS   31.
      *
       01  WRK-HANBETU-DATA                PIC X(145).
      *
      *    Æþ±¡ÅÀ¿ô»»ÄêÍÑ¥ï¡¼¥¯¥¨¥ê¥¢
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                  PIC  9(08)V9(5).
           03  WRK-KHNTEN-MOTO             PIC  9(08).
           03  WRK-KHNTEN-GEN              PIC  9(08)V9(5).
           03  WRK-KHN-ZAITEN              PIC  9(08)V9(5).
           03  WRK-KHN-ZAITEN-MOTO         PIC  9(08).
           03  WRK-KHN-ZAITEN-GEN          PIC  9(08)V9(5).
           03  WRK-SENTEI-KHNTEN           PIC  9(08)V9(5).
           03  WRK-SENTEI-KHNTEN-MOTO      PIC  9(08).
           03  WRK-SENTEI-KHNTEN-GEN       PIC  9(08)V9(5).
           03  WRK-SENTEI-ZAITEN           PIC  9(08)V9(5).
           03  WRK-SENTEI-ZAITEN-MOTO      PIC  9(08).
           03  WRK-SENTEI-ZAITEN-GEN       PIC  9(08)V9(5).
           03  WRK-NYUKHNKSNKBN            PIC  9(03).
           03  WRK-KSNTEN                  PIC  S9(08).
           03  NYUIN-COUNT                 PIC  9(02). 
           03  SINRYO-COUNT                PIC  9(04). 
           03  WRK-CHK-SRYCD               PIC  X(09).
           03  WRK-CHUKSNCD                PIC  X(04).
      *
       01  FLG-AREA.
           03  FLG-HANBETU                 PIC 9(01).
      *
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-GAIHAKU-SET             PIC 9(01).
           03  FLG-SENTEI-SET              PIC 9(01).
           03  FLG-NYUKHN                  PIC 9(01).
           03  FLG-KAIKEI-SAKUSEI          PIC 9(01).
           03  FLG-KAIKEI-SAKUSEI1         PIC 9(01).
           03  FLG-KAIKEI-SAKUSEI2         PIC 9(01).
           03  FLG-SENTEICDCHG             PIC 9(01).
           03  FLG-KYUFUGAI                PIC 9(01).
           03  FLG-SANTEI                  PIC 9(01).
           03  FLG-TOKUSYU                 PIC 9(01).
           03  FLG-KUMIAWASE-HANTEI        PIC 9(01).
           03  FLG-KOREI-TANYUIN           PIC 9(01).
           03  FLG-KOREI-TANYUIN2          PIC 9(01).
           03  FLG-NYUINRYO-KASAN-OK       PIC 9(01).
           03  FLG-NYUINRYO-KASAN2-OK      PIC 9(01).
           03  FLG-NYUINRYO-KASAN-CHK      PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-ZAIARI                  PIC 9(01).
           03  FLG-ZAIARI1                 PIC 9(01).
           03  FLG-SYUARI1                 PIC 9(01).
           03  FLG-KOHARI1                 PIC 9(01).
           03  FLG-DAISANARI1              PIC 9(01).
           03  FLG-ZAIARI2                 PIC 9(01).
           03  FLG-SYUARI2                 PIC 9(01).
           03  FLG-KOHARI2                 PIC 9(01).
           03  FLG-DAISANARI2              PIC 9(01).
           03  FLG-HKNCOMBINUM999          PIC 9(01).
           03  FLG-SKYCHK-KEISAN           PIC 9(01).
           03  FLG-SKYCHK-KEISAN2          PIC 9(01).
           03  FLG-KOREI-KANGAN            PIC 9(01).
           03  FLG-TAISYO-KANGAN           PIC 9(01).
           03  FLG-KOGAKU-KANGAN           PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-KENSU                   PIC 9(03).
      *
      * ¥Þ¥ëÄ¹¥Á¥§¥Ã¥¯ÍÑ¥Æ¡¼¥Ö¥ë
       01  TABLE-CYOKI.
         02  TABLE-CYOKI-XXX               OCCURS  10.
           03  TABLE-CYOKI-SKYYM           PIC X(06).
           03  TABLE-CYOKI-YUSENYMD        PIC X(08).
           03  TABLE-CYOKI-HENSYU1         PIC 9(02).
           03  TABLE-CYOKI-YMD1            PIC X(08).
           03  TABLE-CYOKI-HENSYU2         PIC 9(02).
           03  TABLE-CYOKI-YMD21           PIC X(08).
           03  TABLE-CYOKI-YMD22           PIC X(08).
           03  TABLE-CYOKI-YMD23           PIC X(08).
           03  TABLE-CYOKI-HANTEI          PIC 9(01).
      *
      * ¼¼ÎÁº¹³ÛÂàÈòÍÑ¥Æ¡¼¥Ö¥ë     
       01  TABLE-RMSAGAKU.
           03  TBL-RMSAGAKU                PIC 9(07)   OCCURS  99.
      *
      * Æþ±¡ÎÁ¥Õ¥é¥°ÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  TABLE-NYUIN.                                                 
           03  TBL-NYUIN                   PIC 9(01)   OCCURS  31.      
      *
      * ³°ÇñÂàÈòÍÑ¥Æ¡¼¥Ö¥ë     
       01  TABLE-GAIHAKU.
           03  TBL-GAIHAKU                 PIC 9(03)   OCCURS  31.
      *
      * Ì¤¼Â»Ü¸º»»ÂàÈòÍÑ¥Æ¡¼¥Ö¥ë     
       01  TABLE-GENSAN.
           03  TBL-INNAI-KANSEN            PIC 9(02)   OCCURS  31.
           03  TBL-IRYOU-ANZEN             PIC 9(02)   OCCURS  31.
           03  TBL-JOKUSOU                 PIC 9(02)   OCCURS  31.
       01  WRK-INNAI-KANSEN-GENTEN         PIC 9(03).
       01  WRK-IRYOU-ANZEN-GENTEN          PIC 9(03).
       01  WRK-JOKUSOU-GENTEN              PIC 9(03).
      *
      * Æþ±¡ÎÁÌ¤»»Äê»þ¿©»ö¡¿¼¼ÎÁº¹³Û¼è°·¥Æ¡¼¥Ö¥ë     
       01  TABLE-5000-NACCT.
           03  TBL-5000-NACCT-BRMSGKKBN    PIC X(01)   OCCURS  31.
      *
      * ¿ÇÎÅ²ñ·×ºÞ²ó¿ôÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  TABLE-ZAIKAISU.                                              
           03  TBL-ZAIKAISU                PIC 9(03)   OCCURS  31.      
      *
      * ³°Çñ¥«¥¦¥ó¥ÈÎÎ°è     
       01  WRK-GAIHAKU-COUNT               PIC 9(02).
      *
      * °åÎÅµ¡´Ø¾ðÊó¡ÊÆþ±¡¡Ý´ðËÜ¡Ë
       01  WRK-SYS5000-COUNT               PIC 9(02).
      *
      * ÁªÄêÎÅÍÜÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  TABLE-SENTEI.
           03  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      * ³°Çñ»þ´ðËÜÅÀ¿ô¼èÆÀÎÎ°è
       01  WRK-GAIHAKU-SRYCD               PIC X(09).
       01  WRK-GAIHAKU-KHNTEN              PIC 9(05).
      *
      * ÁªÄê³°Çñ·×»»ÎÎ°è
       01  WRK-SENTEI-GAIHAKU-KEISU        PIC 9V99.
       01  WRK-SENTEI-GAIHAKU1             PIC 9(08).
       01  WRK-SENTEI-GAIHAKU2             PIC 9(08).
      *
      * ÊÝ¸±Å¬ÍÑ³°¶â³ÛÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  HKNTEKGAI-TABLE.
           03  HKNTEKGAI-TBL               OCCURS 16.
               05  TBL-TGMONEY             PIC 9(10).
               05  TBL-TGMONEY-TAX         PIC 9(10).
      *
      * Æþ±¡ÊÝ¸±Å¬ÍÑ³°¶â³ÛÂàÈòÍÑÎÎ°è
       01  HKNTEKGAI-NYUIN.
           03  NYUIN-TGMONEY               PIC 9(10).
           03  NYUIN-TGMONEY-TAX           PIC 9(10).
      *
      * ¼«ÈñÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
       01  JIHI-TABLE.
           03  JIHI-TBL                    OCCURS 10.
               05  TBL-JIHI-TAXNASI        PIC 9(10).
               05  TBL-JIHI-TAXARI         PIC 9(10).
      *
      *    Åº¤¨»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(02).
           03  IDY                         PIC 9(02).
           03  IDY2                        PIC 9(02).
           03  IDZ                         PIC 9(02).
           03  IDZ2                        PIC 9(02).
           03  IDX1                        PIC 9(02).
           03  IDX2                        PIC 9(02).
           03  IDX3                        PIC 9(03).
           03  IDX4                        PIC 9(02).
           03  IDX5                        PIC 9(02).
           03  IDX6                        PIC 9(02).
           03  IDX7                        PIC 9(02).
           03  IDX8                        PIC 9(02).
           03  IDX9                        PIC 9(02).
           03  IDX10                       PIC 9(02).
           03  IDX11                       PIC 9(02).
           03  IDX12                       PIC 9(02).
           03  SYU-IDX                     PIC 9(02).
           03  NYUINACT-IDX                PIC 9(02).
           03  RMSAGAKU-IDX                PIC 9(02).
           03  GENSAN-IDX                  PIC 9(02).
           03  SENTEI-IDX                  PIC 9(02).
           03  KOHFTN-IDX                  PIC 9(02).
           03  KOHFTN-IDX2                 PIC 9(02).
           03  KIRIKAE-IDX                 PIC 9(02).
           03  KIRIKAE-IDXX     REDEFINES   KIRIKAE-IDX
                                           PIC X(02).
           03  FUKU-IDX                    PIC 9(02).
           03  FUKU-IDX2                   PIC 9(02).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY               PIC 9(04).
               05  WRK-SYSMM               PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
      *
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°ÍÑ¥Æ¡¼¥Ö¥ë
       01  TBL-SYOSIN-AREA.
           03  FILLER          PIC X(09)   VALUE    "111003610".
           03  FILLER          PIC X(09)   VALUE    "111700210".
           03  FILLER          PIC X(09)   VALUE    "111000110".
           03  FILLER          PIC X(09)   VALUE    "111700110".
           03  FILLER          PIC X(09)   VALUE    "111012510".
           03  FILLER          PIC X(09)   VALUE    "111012710".
           03  FILLER          PIC X(09)   VALUE    "111011810".
           03  FILLER          PIC X(09)   VALUE    "111012610".
           03  FILLER          PIC X(09)   VALUE    "111012810".
           03  FILLER          PIC X(09)   VALUE    "099110001".
           03  FILLER          PIC X(09)   VALUE    "111013850".
      *
       01  TBL-SYOSIN-R        REDEFINES   TBL-SYOSIN-AREA.
           03  TBL-SYOSIN      PIC X(09)   OCCURS    11.
      *
       01  TBL-SYOSIN-MAX      PIC 9(02)   VALUE     11.
      *
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°ÍÑ¥Æ¡¼¥Ö¥ë¡ÊÊ¡²¬ÍÑ¡Ë
       01  TBL-SYOSIN2-AREA.
           03  FILLER          PIC X(09)   VALUE    "111003610".
           03  FILLER          PIC X(09)   VALUE    "111700210".
           03  FILLER          PIC X(09)   VALUE    "111000110".
           03  FILLER          PIC X(09)   VALUE    "111700110".
           03  FILLER          PIC X(09)   VALUE    "111012510".
           03  FILLER          PIC X(09)   VALUE    "111012710".
           03  FILLER          PIC X(09)   VALUE    "111011810".
           03  FILLER          PIC X(09)   VALUE    "111012610".
           03  FILLER          PIC X(09)   VALUE    "111012810".
           03  FILLER          PIC X(09)   VALUE    "111013850".
      *
       01  TBL-SYOSIN2-R       REDEFINES   TBL-SYOSIN2-AREA.
           03  TBL-SYOSIN2     PIC X(09)   OCCURS    10.
      *
       01  TBL-SYOSIN2-MAX     PIC 9(02)   VALUE     10.
      *
      *    Ê¡²¬¸©¸øÈñ¤ÎµëÉÕÂÐ¾Ý³°È½ÄêÍÑ¤Î½é¿ÇÂàÈò¥Æ¡¼¥Ö¥ë
       01  TBL-FUKUOKA.
           03  TBL-FUKUOKA-SYOSIN          PIC 9(01)  OCCURS  31.
      *
      *    Æþ±¡ÎÁ²Ã»»È½ÄêÍÑ¥Æ¡¼¥Ö¥ë
       01  TBL-NYUINRYO-KASAN-AREA.
           03  FILLER          PIC X(09)   VALUE    "190101770".
           03  FILLER          PIC X(09)   VALUE    "190106770".
           03  FILLER          PIC X(09)   VALUE    "190814010".
      *
       01  TBL-NYUINRYO-KASAN-R   REDEFINES   TBL-NYUINRYO-KASAN-AREA.
           03  TBL-NYUINRYO-KASAN   PIC X(09)   OCCURS    3.
      *
       01  TBL-NYUINRYO-KASAN-MAX   PIC 9(02)   VALUE     3.
      *
      *    Æþ±¡ÎÁ²Ã»»È½ÄêÍÑ¥Æ¡¼¥Ö¥ë£²
       01  TBL-NYUINRYO-KASAN2-AREA.
           03  FILLER          PIC X(09)   VALUE    "101910050".
           03  FILLER          PIC X(09)   VALUE    "101910060".
           03  FILLER          PIC X(09)   VALUE    "101920010".
      *
       01  TBL-NYUINRYO-KASAN2-R  REDEFINES   TBL-NYUINRYO-KASAN2-AREA.
           03  TBL-NYUINRYO-KASAN2  PIC X(09)   OCCURS    3.
      *
       01  TBL-NYUINRYO-KASAN2-MAX  PIC 9(02)   VALUE     3.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5113.INC".
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼
       01  WRK-SYUNOU-REC-T.
         02 WRK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYU-//.
      *    ¥ï¡¼¥¯¼ýÇ¼¥Þ¥¹¥¿¡¼¡Ê¸ºÅÀÍÑ¡Ë
       01  WRK-SYUNOUG-REC-T.
         02 WRK-SYUNOUG-REC    OCCURS 10.
           COPY  "CPSYUNOU.INC" REPLACING  //SYU-// BY //WRK-SYUG-//.
      *
      *    ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿                                              
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    ¥ï¡¼¥¯ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿                                        
       01  WRK-SYUDAY-REC-T.
         02 WRK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC" REPLACING  //SDAY-// BY //WRK-SDAY-//.
      *    ¥ï¡¼¥¯ÆüÊÌ¼ýÇ¼¥Þ¥¹¥¿¡Ê¸ºÅÀÍÑ¡Ë
       01  WRK-SYUDAYG-REC-T.
         02 WRK-SYUDAYG-REC    OCCURS 10.
           COPY  "CPSYUDAY.INC" REPLACING  //SDAY-// BY //WRK-SDAYG-//.
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù
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
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÉéÃ´¶â³Û·×»»ÍÑ¥Ñ¥é¥á¥¿
       01  WRK-ORCS02-REC.
           COPY    "CPORCS02.INC"   REPLACING  //LNK-// BY //WRK-//.
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Æþ±¡Ä´ºÞÎÁ¹¹¿·¥µ¥Ö
           COPY    "CPORCSCHOZAI.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA". 
           COPY    "MCPDATA.INC".
      *
      *    Æþ±¡ÅÀÅ©°ì³ç¹¹¿·¥µ¥Ö
           COPY    "CPORCSTENTEK.INC".
      *
      *    Æþ±¡ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤á¥µ¥Ö
           COPY    "CPORCSTENTEK2.INC".
      *
      *    ÉÂÅïÁªÄêÆþ±¡¼«¸ÊÉéÃ´¼èÆÀ¥µ¥Ö
           COPY    "CPORCSBTUSENTEI.INC".
      *
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSROUNYUIN.INC".
      *
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¼èÆÀ¥µ¥Ö
           COPY    "CPORCSSKJHKN.INC".
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSJGNGET.INC".
      *
      *    Æþ±¡ÅÀ¿ô¥Þ¥¹¥¿ÉÕ²Ã¾ðÊó¥µ¥Ö
           COPY    "CPORCSNTENSU.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "COMMON-SPA".
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.                                           
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      /
       PROCEDURE           DIVISION  USING
                                     SPA-AREA
                                     LNK-ORCS02-REC
                                     LNK-SYUNOU-REC-T
                                     LNK-SYUDAY-REC-T.                 
      ******************************************************************
      *        °ÍÍêÀ©¸æ½èÍý
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
      *    ACCEPT  SYS-YMD         FROM    DATE.
      *    COMPUTE WRK-SYSYY       =   SYS-YY      +   2000.
      *    MOVE    SYS-MM          TO  WRK-SYSMM.
      *    MOVE    SYS-DD          TO  WRK-SYSDD.
      *
           MOVE    ZERO            TO  LNK-S02-RC.
           MOVE    ZERO            TO  NYUIN-COUNT.
      *
           INITIALIZE    LNK-SYUNOU-REC-T.
           INITIALIZE    LNK-SYUDAY-REC-T.                             
      *
           INITIALIZE    WRK-SYUNOU-REC-T.
           INITIALIZE    WRK-SYUDAY-REC-T.                             
      *
           INITIALIZE    WRK-SYUNOUG-REC-T.
           INITIALIZE    WRK-SYUDAYG-REC-T.
      *
           INITIALIZE    WRK-AREA
                         WRK-SKYYM-TABLE
                         WRK-DAY-TABLE
                         WRK-HANBETU-TABLE
                         WRK-HANBETU2-TABLE
                         WRK-HANBETU3-TABLE
                         WRK-JIBAI-TABLE
                         WRK-NYUINDAY-TABLE
                         WRK-NYUINDAY2-TABLE
                         WRK-NYUINDAY-KYUKYU-TABLE
                         WRK-NYUINDAY-TANKI-TABLE
                         WRK-NYUSANTEI-TABLE
                         WRK-SYOSIN-TABLE
                         WRK-SYOSIN-DAY-TABLE
                         WRK-SYOSIN2-TABLE
                         WRK-DOJITU-TABLE
                         WRK-DAYTEN-TABLE
                         WRK-NYUINDAY-TAIHI-TABLE
                         WRK-TAISYOGAI-HANTEI-TABLE
                         WRK-KAIKEING-TABLE
                         WRK-KAIKEING2-TABLE
                         WRK-KOREI-TANYUIN-TABLE
                         WRK-DAISANYUSENFLG-TABLE
                         WRK-KANGOGEN-TAIHI-TABLE
                         WRK-KANGOGEN-TABLE
                         WRK-YAKTEN-TABLE
                         WRK-SKJHKN-TABLE
                         WRK-SKJHKN-TAIHI-TABLE
                         FLG-AREA
                         CNT-AREA.
      *
      *    Ä´ºÞÎÁ°ì³çÊÑ¹¹½èÍý
      ****     PERFORM       NYUIN-CHOZAI-SEC.
      *
           PERFORM       NYUIN-HANBETU-SEC.
      *
           IF   (FLG-KAIKEI-SAKUSEI    =    1)
           OR   (FLG-HKNCOMBINUM999    =    1)
           OR   (WRK-KENSU-ERR         =    1)
             IF    LNK-S02-RC   =   ZERO
               IF  FLG-KAIKEI-SAKUSEI  =    1
                 MOVE   13   TO   LNK-S02-RC
               END-IF
             END-IF
             IF    LNK-S02-RC   =   ZERO
               IF  FLG-HKNCOMBINUM999  =    1
                 MOVE   37   TO   LNK-S02-RC
               END-IF
             END-IF
             IF    LNK-S02-RC   =   ZERO
               IF  WRK-KENSU-ERR       =    1
                 MOVE   30   TO   LNK-S02-RC
               END-IF
             END-IF
             GO    TO   MAIN-EXT
           END-IF.
      *
           PERFORM       SYUNOU-SET-SEC.
      *
           IF    LNK-S02-RC            =    ZERO
           AND   FLG-KUMIAWASE-HANTEI  =    1
              IF   LNK-S02-TEIKI-KBN   =   "X"
                 CONTINUE
              ELSE
                 MOVE   18       TO    LNK-S02-RC
                 GO   TO   MAIN-EXT
              END-IF
           END-IF.
      *
      *    Æþ±¡²ñ·×¤«¤é¼ýÇ¼¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È
           PERFORM    VARYING   IDX    FROM   1   BY   1
                      UNTIL  (IDX            >    10)
                        OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
              IF   LNK-SYU-SYUHKNNUM(IDX)  =  "971"  OR  "973"
                      PERFORM       NYUIN-KAIKEI-SET2-SEC
              ELSE
                      PERFORM       NYUIN-KAIKEI-SET-SEC
              END-IF
           END-PERFORM.
      *
      *    ¿ÇÎÅ²ñ·×¤«¤é¼ýÇ¼¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È
           MOVE     ZERO       TO   FLG-SKYCHK-KEISAN.
           MOVE     ZERO       TO   FLG-SKYCHK-KEISAN2.
           PERFORM    VARYING   IDX    FROM   1   BY   1
                      UNTIL  (IDX            >    10)
                        OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                      PERFORM       SINRYO-KAIKEI-SET-SEC
           END-PERFORM.
      *
           IF   (FLG-SKYCHK-KEISAN     =    1)
           OR   (FLG-SKYCHK-KEISAN2    =    1)
             IF    LNK-S02-RC   =   ZERO
               IF  FLG-SKYCHK-KEISAN   =    1
                 MOVE   38   TO   LNK-S02-RC
               END-IF
             END-IF
             IF    LNK-S02-RC   =   ZERO
               IF  FLG-SKYCHK-KEISAN2  =    1
                 MOVE   39   TO   LNK-S02-RC
               END-IF
             END-IF
             GO    TO   MAIN-EXT
           END-IF.
      *
      *    ÆüÊÌÅÀ¿ôÊÔ½¸
           PERFORM    VARYING   IDX    FROM   1   BY   1
                      UNTIL  (IDX            >    10)
                        OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
             MOVE  LNK-SYU-SRYYMD(IDX)(1:6)
                                TO  WRK-DAYTEN-SRYYM(IDX)
             MOVE  LNK-SYU-HKNCOMBINUM(IDX)
                                TO  WRK-DAYTEN-HKNCOMBINUM(IDX)
             PERFORM  VARYING  IDX10  FROM  1  BY  1
                       UNTIL   IDX10   >   31
               MOVE   LNK-SDAY-HKNTEN(IDX IDX10)
                                TO  WRK-DAYTEN-HKNTEN(IDX IDX10)
             END-PERFORM
           END-PERFORM.
      *
      *    ÉéÃ´¶â·×»»
           PERFORM     SANTEI-SEC.
      *
      *    ÆÃ¼ì·×»»¡ÊºÆ·×»»¡Ë
           IF  (CNT-KENSU        >    1)
           AND (FLG-TOKUSYU      =    1)
             MOVE   "1"   TO   LNK-S02-RETRY-FLG
             PERFORM   SANTEI-SEC
           END-IF.
      *
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ëÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¤Ç¤Î¿©»ö»»Äê¥Á¥§¥Ã¥¯
           PERFORM     SKJ-SANTEI-CHECK-SEC.
      *
      *    ÉÔÍ×¤Ê¼ýÇ¼¤ò½ü³°¤¹¤ë
           PERFORM     TAISYOGAI-DATA-HENSYU-SEC.
      *
           IF    LNK-S02-RC   =   ZERO
             MOVE     ZERO     TO    WRK-MSGCD
             PERFORM   VARYING   IDX    FROM   1   BY   1
                        UNTIL   (IDX                 >   10  )
                         OR     (LNK-SYU-SRYYMD(IDX) =  SPACE)
                EVALUATE   WRK-KAIKEING(IDX)
                  WHEN      1
                    IF   WRK-MSGCD = ZERO OR 16
                        MOVE     14     TO    WRK-MSGCD
                    END-IF
                  WHEN      2
                        MOVE     15     TO    WRK-MSGCD
                  WHEN      3
                    IF   WRK-MSGCD = ZERO
                        MOVE     16     TO    WRK-MSGCD
                    END-IF
                  WHEN      4
                    IF   WRK-MSGCD = ZERO OR 14 OR 16
                        MOVE     24     TO    WRK-MSGCD
                    END-IF
                END-EVALUATE
             END-PERFORM
             IF   WRK-MSGCD   NOT =  ZERO
                MOVE     WRK-MSGCD    TO    LNK-S02-RC
             END-IF
           END-IF.
      *
      ***  IF    LNK-S02-RC   =   ZERO
      ***    IF   WRK-KYUKYUHANSO-ERR  =  1
      ***       MOVE     21     TO    LNK-S02-RC
      ***    END-IF
      ***  END-IF.
      *
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-TAIJYUSIN-ERR    =  1
                MOVE     22     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-GENSAN-ERR       =  1
                MOVE     26     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-GENSAN-ERR2      =  1
                MOVE     27     TO    LNK-S02-RC
             END-IF
           END-IF.
      *
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-NYUINRYO-ERR  =  1
                MOVE     17     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-NYUINRYO-ERR2 =  1
                MOVE     20     TO    LNK-S02-RC
             END-IF
           END-IF.
      *
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-GENTEN-ERR  =  1
                MOVE     19     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-GENTEN-ERR2 =  1
                MOVE     25     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-TENTEKI-ERR =  1
                MOVE     28     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-NYUINKIKANGAI-ERR =  1
                MOVE     32     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-SKJ-ERR1 =  1
                MOVE     35     TO    LNK-S02-RC
             END-IF
           END-IF.
           IF    LNK-S02-RC   =   ZERO
             IF   WRK-SKJ-ERR2 =  1
                MOVE     36     TO    LNK-S02-RC
             END-IF
           END-IF.
      *
      ***  IF    LNK-S02-RC   =   ZERO
      ***    IF   WRK-JNISSU-ERR  =  1
      ***       MOVE     23     TO    LNK-S02-RC
      ***    END-IF
      ***  END-IF.
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      ******************************************************************
      *    ÂÐ¾Ý³°¥Ç¡¼¥¿ÊÔ½¸
      ******************************************************************
       TAISYOGAI-DATA-HENSYU-SEC                     SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                       UNTIL   (IDX                 >    10 )
                        OR     (LNK-SYU-SRYYMD(IDX) =  SPACE)
      *
             IF     WRK-TAISYOGAI-HANTEI(IDX)  =   1
               IF   (LNK-SYU-TOTAL-HKNTEN(IDX)           =  ZERO)
               AND  (LNK-SYU-HKNTEKMONEY(IDX)            =  ZERO)
               AND  (LNK-SYU-KOH-FTN(IDX)                =  ZERO)
               AND  (LNK-SYU-RMSAGAKU(IDX)               =  ZERO)
               AND  (LNK-SYU-SKYMONEY-SKJ-KEI(IDX)       =  ZERO)
               AND  (LNK-SYU-SKYMONEY-SKJ-JIHI-KEI(IDX)  =  ZERO)
               AND  (LNK-SYU-SKYMONEY-LIFE-KEI(IDX)      =  ZERO)
               AND  (LNK-SYU-SKYMONEY-LIFE-JIHI-KEI(IDX) =  ZERO)
               AND  (LNK-SYU-TOTAL-TGMONEY(IDX)          =  ZERO)
               AND  (LNK-SYU-TOTAL-TGMONEY-TAX(IDX)      =  ZERO)
               AND  (LNK-SYU-JIHI-TOTAL(IDX)             =  ZERO)
               AND  (LNK-SYU-JIHI-TOTAL-TAX(IDX)         =  ZERO)
               AND  (LNK-SYU-RSIJIBAI-SKYMONEY(IDX)      =  ZERO)
               AND  (LNK-SYU-RYOYOHI-SKJ(IDX)            =  ZERO)
               AND  (LNK-SYU-RYOYOHI-SKJ-JIHI(IDX)       =  ZERO)
               AND  (LNK-SYU-RYOYOHI-LIFE(IDX)           =  ZERO)
               AND  (LNK-SYU-RYOYOHI-LIFE-JIHI(IDX)      =  ZERO)
                    CONTINUE
               ELSE
                    MOVE     ZERO    TO    WRK-TAISYOGAI-HANTEI(IDX)
               END-IF
             END-IF
      *
           END-PERFORM
      *
      *    ÂÐ¾ÝÊ¬ÂàÈò
      *
           INITIALIZE    WRK-SYUNOU-REC-T
           INITIALIZE    WRK-SYUDAY-REC-T
           INITIALIZE    WRK-KAIKEING2-TABLE
      *
           MOVE    ZERO     TO      IDY
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                       UNTIL   (IDX                 >    10 )
                        OR     (LNK-SYU-SRYYMD(IDX) =  SPACE)
             IF     WRK-TAISYOGAI-HANTEI(IDX)  =   ZERO
                ADD      1                TO     IDY
                MOVE  LNK-SYUNOU-REC(IDX) TO  WRK-SYUNOU-REC(IDY)
                MOVE  LNK-SYUDAY-REC(IDX) TO  WRK-SYUDAY-REC(IDY)
                MOVE  WRK-KAIKEING(IDX)   TO  WRK-KAIKEING2 (IDY)
             END-IF
           END-PERFORM
      *
      *    ÂÐ¾ÝÊ¬Ìá¤¹
      *
           INITIALIZE    LNK-SYUNOU-REC-T
           INITIALIZE    LNK-SYUDAY-REC-T
           INITIALIZE    WRK-KAIKEING-TABLE
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                       UNTIL   (IDX                 >    10 )
                MOVE  WRK-SYUNOU-REC(IDX) TO  LNK-SYUNOU-REC(IDX)
                MOVE  WRK-SYUDAY-REC(IDX) TO  LNK-SYUDAY-REC(IDX)
                MOVE  WRK-KAIKEING2 (IDX) TO  WRK-KAIKEING(IDX)
           END-PERFORM
      *
           .
      *
       TAISYOGAI-DATA-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    »»Äê
      ******************************************************************
       SANTEI-SEC                     SECTION.
      *
           MOVE       ZERO      TO    CNT-KENSU
           MOVE       ZERO      TO    FLG-TOKUSYU
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (LNK-SYU-SRYYMD(IDX) =  SPACE)
                                OR   (LNK-S02-RC        NOT =  ZERO )
      *
                PERFORM  SANTEI-IDX-HENSYU-SEC
      *
                ADD      1                    TO  CNT-KENSU
      *
                MOVE     WRK-HANBETU-SENINFLG2(IDX)                     
                                              TO  LNK-S02-SENINFLG      
      *
                MOVE     WRK-JIBAI-SYUTEN(IDX)
                                              TO  LNK-S02-SYUTEN
                MOVE     WRK-JIBAI-SINDAN(IDX)
                                              TO  LNK-S02-SINDAN
                MOVE     WRK-JIBAI-MEISAI(IDX)
                                              TO  LNK-S02-MEISAI
                MOVE     WRK-JIBAI-RYOYO(IDX) TO  LNK-S02-RYOYO
                MOVE     WRK-JIBAI-KYUKYU(IDX)
                                              TO  LNK-S02-KYUKYU
      *
                MOVE     WRK-SYOSIN(IDX)      TO  LNK-S02-SYOSINFLG
                MOVE     WRK-SYOSIN-DAY-TBL(IDX)
                                        TO  LNK-S02-SYOSINFLG-DAY-TBL
                MOVE     WRK-SYOSIN2(IDX)     TO  LNK-S02-SYOSINFLG2
      *
                MOVE     WRK-JIBAI-SONOTA(IDX)
                                              TO  LNK-S02-SONOTA
      *
                MOVE     WRK-NYUINDAY-TBL(IDX)
                                              TO  LNK-S02-NYUINDAY-TBL
                MOVE     WRK-NYUINDAY-TANKI-TBL(IDX)
                                        TO  LNK-S02-NYUINDAY-TANKI-TBL
                MOVE     WRK-NYUSANTEI-TBL(IDX)
                                              TO  LNK-S02-NYUSANTEI-TBL
      *
                MOVE     WRK-KOREI-TANYUINFLG(IDX)
                                              TO  LNK-S02-KOREI-TANYUIN
      *
                IF       LNK-SYU-SRYYMD(IDX)(1:6)  =   WRK-SKYYM(1)
                  MOVE   WRK-DAISANYUSENFLG(1)
                                              TO  LNK-S02-DAISANYUSENFLG
                END-IF
                IF       LNK-SYU-SRYYMD(IDX)(1:6)  =   WRK-SKYYM(2)
                  MOVE   WRK-DAISANYUSENFLG(2)
                                              TO  LNK-S02-DAISANYUSENFLG
                END-IF
      *
                MOVE     WRK-DOJITU-TBL(IDX)  TO  LNK-S02-DOJITU
      *
                MOVE     WRK-SKJHKN-TBL(IDX)  TO  LNK-S02-SKJHKN
      *
                MOVE     WRK-DAYTEN-TABLE     TO  LNK-S02-DAYTEN
                MOVE     IDX                  TO  LNK-S02-IDX
                MOVE     IDZ2                 TO  LNK-S02-IDX2
      *
                MOVE     LNK-ORCS02-REC       TO  WRK-ORCS02-REC
                MOVE     LNK-SYUNOU-REC(IDX)  TO  SYUNOU-REC
                MOVE     LNK-SYUDAY-REC(IDX)  TO  SYUDAY-REC           
      *
                INITIALIZE    WRK-SYUNOU-REC-T
                INITIALIZE    WRK-SYUDAY-REC-T
                MOVE     ZERO      TO     IDY2
      *
                PERFORM    VARYING   IDY    FROM   1   BY   1
                            UNTIL   (IDY                 >    10 )
                              OR    (LNK-SYU-SRYYMD(IDY) =  SPACE)
                  IF     IDY  =  IDX
                     CONTINUE
                  ELSE
                     ADD    1                  TO IDY2
                     MOVE  LNK-SYUNOU-REC(IDY) TO WRK-SYUNOU-REC(IDY2)
                     MOVE  LNK-SYUDAY-REC(IDY) TO WRK-SYUDAY-REC(IDY2)
                  END-IF
                END-PERFORM
      *
                CALL     "ORCS02"     USING    MCPAREA
                                               WRK-ORCS02-REC
                                               SYUNOU-REC
                                               WRK-SYUNOU-REC-T
                                               SYUDAY-REC              
                                               WRK-SYUDAY-REC-T        
                                               SPA-AREA
      *
                EVALUATE    WRK-S02-SYORI-KBN
                   WHEN    " "
                   WHEN    "1"
                   WHEN    "8"
                      MOVE     "1"            TO  SYU-CREATEKBN
                                                  SDAY-CREATEKBN
                   WHEN    "2"
                   WHEN    "9"
      *               MOVE     "2"            TO  SYU-DENPJTIKBN
                      IF       WRK-S02-TEIKI-KBN    =   "X"
                          MOVE     "3"        TO  SYU-CREATEKBN
                                                  SDAY-CREATEKBN
                      ELSE
                          MOVE     "2"        TO  SYU-CREATEKBN
                                                  SDAY-CREATEKBN
                      END-IF
                END-EVALUATE
      *
                MOVE     SYUNOU-REC           TO  LNK-SYUNOU-REC(IDX)
                MOVE     SYUDAY-REC           TO  LNK-SYUDAY-REC(IDX)  
                MOVE     WRK-ORCS02-REC       TO  LNK-ORCS02-REC 
      *
                IF       LNK-S02-TOKUSYU-SAI-KBN  NOT =  SPACE
                     MOVE      1              TO  FLG-TOKUSYU
                END-IF
      *
           END-PERFORM
      *
           .
      *
       SANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥Á¥§¥Ã¥¯ÍÑ£É£Ä£ØÊÔ½¸
      ******************************************************************
       SANTEI-IDX-HENSYU-SEC             SECTION.
      *
           MOVE     ZERO      TO     IDZ2
      *
           IF       IDX   >   1
             PERFORM   VARYING   IDY   FROM   1   BY   1
                        UNTIL    IDY     >    IDX - 1
               ADD   1        TO     IDZ2
             END-PERFORM
           END-IF
      *
           .
      *
       SANTEI-IDX-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä´ºÞÎÁ°ì³çÊÑ¹¹½èÍý
      *****************************************************************
       NYUIN-CHOZAI-SEC           SECTION.
      *
           INITIALIZE             ORCSCHOZAIAREA
           MOVE       LNK-S02-HOSPNUM
                                   TO   SPA-HOSPNUM
           MOVE       "1"          TO   ORCSCHO-KBN
           MOVE       LNK-S02-PTID TO   ORCSCHO-PTID
           MOVE       LNK-S02-SKYSTYMD(1:6)
                                  TO    ORCSCHO-SRYYM
           CALL  "ORCSCHOZAI"     USING   SPA-AREA
                                          ORCSCHOZAIAREA
      *
           .
      *
       NYUIN-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ëÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¤Ç¤Î¿©»ö»»Äê¥Á¥§¥Ã¥¯
      *****************************************************************
       SKJ-SANTEI-CHECK-SEC           SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                     UNTIL  (IDX             >    2  )
                      OR    (WRK-SKYYM(IDX)  =  SPACE)
                      OR    (WRK-SKJ-ERR2    =    1  )
      *
             IF   WRK-SKYYM(IDX) = LNK-S02-SKJHKN-SRYYM(1)
               PERFORM  VARYING  IDX11  FROM  1  BY  1
                         UNTIL  (IDX11           >  31)
                          OR    (WRK-SKJ-ERR2    =   1)
                 IF ((LNK-S02-SKJHKN-AAA-TAIFLG(1 IDX11) =  "1" ) AND
                     (LNK-S02-SKJHKN-AAA-ARIFLG(1 IDX11) = SPACE))
                 OR ((LNK-S02-SKJHKN-BBB-TAIFLG(1 IDX11) =  "1" ) AND
                     (LNK-S02-SKJHKN-BBB-ARIFLG(1 IDX11) = SPACE))
                     MOVE    1    TO    WRK-SKJ-ERR2
                 END-IF
               END-PERFORM
             END-IF
      *
             IF   WRK-SKYYM(IDX) = LNK-S02-SKJHKN-SRYYM(2)
               PERFORM  VARYING  IDX11  FROM  1  BY  1
                         UNTIL  (IDX11           >  31)
                          OR    (WRK-SKJ-ERR2    =   1)
                 IF ((LNK-S02-SKJHKN-AAA-TAIFLG(2 IDX11) =  "1" ) AND
                     (LNK-S02-SKJHKN-AAA-ARIFLG(2 IDX11) = SPACE))
                 OR ((LNK-S02-SKJHKN-BBB-TAIFLG(2 IDX11) =  "1" ) AND
                     (LNK-S02-SKJHKN-BBB-ARIFLG(2 IDX11) = SPACE))
                     MOVE    1    TO    WRK-SKJ-ERR2
                 END-IF
               END-PERFORM
             END-IF
      *
           END-PERFORM
      *
           .
      *
       SKJ-SANTEI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ·îÊÌÊÝ¸±ÊÌÈ½ÊÌ½èÍý
      *****************************************************************
       NYUIN-HANBETU-SEC           SECTION.
      *
           PERFORM    SKYYM-TABLE-SET-SEC.
      *
           PERFORM    NYUINKAIKEI-SYORI-SEC.
      *
           PERFORM    SINRYOKAIKEI-SYORI-SEC.
      *
           PERFORM    DOJITUKAIKEI-SYORI-SEC.
      *
           PERFORM    KAISHISYURYO-SYORI-SEC.
      *
           PERFORM    HKNNUM-SYUTOKU-SYORI-SEC.
      *
           PERFORM    NARABIKAE-SYORI-SEC.
      *
           PERFORM    YUSENYMD-HENKOU-SYORI-SEC.
      *
       NYUIN-HANBETU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÀÁµáÇ¯·îÊÌ¤Î³«»ÏÆü¡¦½ªÎ»Æü¥»¥Ã¥È                            *
      ******************************************************************
       SKYYM-TABLE-SET-SEC         SECTION.
      *
           MOVE          LNK-S02-SKYSTYMD(1:6)  TO  WRK-SKYYM(1).
      *     
           INITIALIZE                               STS-AREA-DAY.
           INITIALIZE                               LNK-DAY7-AREA.
           MOVE          "71"                   TO  LNK-DAY7-IRAI.
           MOVE          LNK-S02-SKYSTYMD(1:6)  TO  LNK-DAY7-YM.
           CALL          "ORCSDAY"          USING   STS-AREA-DAY
                                                    LNK-DAY7-AREA.
           MOVE          LNK-DAY7-KOYOMI        TO  WRK-EDYMD. 
      *
           MOVE          LNK-S02-SKYSTYMD(7:2)  TO  WRK-HI-ST(1).
      *    IF      LNK-S02-SKYEDYMD(5:2) > LNK-S02-SKYSTYMD(5:2)
           IF      LNK-S02-SKYEDYMD(1:6) > LNK-S02-SKYSTYMD(1:6)
              MOVE       WRK-EDYMD(7:2)         TO  WRK-HI-ED(1) 
           ELSE
              MOVE       LNK-S02-SKYEDYMD(7:2)  TO  WRK-HI-ED(1)
           END-IF.
      *
      *    IF      LNK-S02-SKYEDYMD(5:2) > LNK-S02-SKYSTYMD(5:2)
           IF      LNK-S02-SKYEDYMD(1:6) > LNK-S02-SKYSTYMD(1:6)
              MOVE       LNK-S02-SKYEDYMD(1:6)  TO  WRK-SKYYM(2)
              MOVE       1                      TO  WRK-HI-ST(2)
              MOVE       LNK-S02-SKYEDYMD(7:2)  TO  WRK-HI-ED(2)
           END-IF.
      *
      *    ¸©ÈÖ¹æ¤Î¼èÆÀ
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              INITIALIZE                         SYS-1001-REC
              MOVE    LNK-S02-HOSPNUM      TO    SYS-1001-HOSPNUM
              MOVE    "1001"               TO    SYS-1001-KANRICD
              MOVE    "*"                  TO    SYS-1001-KBNCD
              MOVE    WRK-SKYYM(IDX)       TO    SYS-1001-STYUKYMD
                                                 SYS-1001-EDYUKYMD
              MOVE    "01"                 TO    SYS-1001-STYUKYMD(7:2)
                                                 SYS-1001-EDYUKYMD(7:2)
              MOVE    SYS-1001-REC         TO    MCPDATA-REC
              MOVE    "DBSELECT"           TO    MCP-FUNC
              MOVE    "tbl_syskanri"       TO    MCP-TABLE
              MOVE    "key10"              TO    MCP-PATHNAME
              CALL    "ORCDBMAIN"                USING MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
              IF      MCP-RC               =     ZERO
                  MOVE    "tbl_syskanri"   TO    MCP-TABLE
                  MOVE    "key10"          TO    MCP-PATHNAME
                  PERFORM   SYSKANRI-READ-SEC
                  MOVE      SYS-1001-PREFNUM
                                           TO    WRK-PREFNUM(IDX)
              END-IF
      *
              MOVE    "tbl_syskanri"       TO    MCP-TABLE
              MOVE    "key10"              TO    MCP-PATHNAME
              PERFORM   CSR-CLOSE-SEC
      *
           END-PERFORM.
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆüÅù¤Î¼èÆÀ
      *    Æþ±¡ÎÁ¤È°Û¤Ê¤ë¿©»ö»»ÄêÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¼èÆÀ
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              INITIALIZE                   ORCSNYUINDAY-AREA
              MOVE    LNK-S02-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
              MOVE    LNK-S02-PTID     TO  LNK-NYUINDAY-PTID
              MOVE    WRK-SKYYM(IDX)   TO  LNK-NYUINDAY-SANTEIYM
              CALL    "ORCSNYUINDAY"   USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
              IF      LNK-NYUINDAY-RC  =   ZERO
                 PERFORM    VARYING   IDY   FROM   1   BY   1
                             UNTIL    IDY    >    31
                   MOVE  LNK-NYUINDAY-KBN(IDY) TO
                           WRK-NYUINDAY-TAIHI-KBN(IDX IDY)
                   MOVE  LNK-NYUINDAY-KYUKYU-KBN(IDY) TO
                           WRK-NYUINDAY-TAIHI-KYUKYU-KBN(IDX IDY)
                   MOVE  LNK-NYUINDAY-TANKI-KBN(IDY) TO
                           WRK-NYUINDAY-TAIHI-TANKI-KBN(IDX IDY)
                   MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDY) TO
                           WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX IDY)
                   MOVE  LNK-NYUINDAY-DOUJITSU-SKJKBN(IDY) TO
                           WRK-NYUINDAY-TAIHI-DOU-SKJKBN(IDX IDY)
                 END-PERFORM
              END-IF
      *
              IF   WRK-SKYYM(IDX)  >=  "201710"
                INITIALIZE                   ORCSSKJHKN-AREA
                MOVE    LNK-S02-HOSPNUM  TO  LNK-SKJHKN-HOSPNUM
                MOVE    LNK-S02-PTID     TO  LNK-SKJHKN-PTID
                MOVE    WRK-SKYYM(IDX)   TO  LNK-SKJHKN-SANTEIYM
                CALL    "ORCSSKJHKN"     USING   ORCSSKJHKN-AREA
                                                 ORCSNYUINDAY-AREA
                                                 SPA-AREA
                IF      LNK-SKJHKN-RC    =   ZERO
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     MOVE  LNK-SKJHKN-HKNCOMBINUM(IDY 1) TO
                             WRK-SKJHKN-TAIHI-HKNCOMBI(IDX IDY)
                     MOVE  LNK-SKJHKN-HKNCOMBINUM(IDY 2) TO
                             WRK-SKJHKN-TAIHI-HKNCOMBI-DOU(IDX IDY)
                   END-PERFORM
                ELSE
                   IF      LNK-SKJHKN-RC    =    5
                     MOVE    1    TO    WRK-SKJ-ERR1
                   END-IF
                END-IF
              END-IF
      *
      *       Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ
              IF   WRK-SKYYM(IDX)  >=  "201804"
                INITIALIZE                   ORCSKANGOGEN-AREA
                MOVE    LNK-S02-HOSPNUM  TO  LNK-KANGOGEN-HOSPNUM
                MOVE    LNK-S02-PTID     TO  LNK-KANGOGEN-PTID
                MOVE    WRK-SKYYM(IDX)   TO  LNK-KANGOGEN-SANTEIYM
                CALL    "ORCSKANGOGEN"   USING  ORCSKANGOGEN-AREA
                                                SPA-AREA
                IF      LNK-KANGOGEN-RC  =   ZERO
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                               UNTIL    IDY    >    31
                     MOVE  LNK-KANGOGEN-SRYCD(IDY)
                               TO  WRK-KANGOGEN-TAIHI-SRYCD(IDX IDY)
                   END-PERFORM
                END-IF
              END-IF
      *
           END-PERFORM.
      *
       SKYYM-TABLE-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    Æþ±¡²ñ·×½èÍý                                                *
      ******************************************************************
       NYUINKAIKEI-SYORI-SEC         SECTION.
      *
           MOVE       ZERO      TO     FLG-KAIKEI-SAKUSEI.
           MOVE       ZERO      TO     FLG-KAIKEI-SAKUSEI1.
           MOVE       ZERO      TO     FLG-KAIKEI-SAKUSEI2.
           MOVE       ZERO      TO     FLG-HKNCOMBINUM999.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              INITIALIZE                            NYUINACCT-REC
              MOVE    LNK-S02-HOSPNUM       TO      NACCT-HOSPNUM
              MOVE    LNK-S02-NYUGAIKBN     TO      NACCT-NYUGAIKBN
              MOVE    LNK-S02-PTID          TO      NACCT-PTID
              MOVE    "00"                  TO      NACCT-SRYKA
              MOVE    WRK-SKYYM(IDX)        TO      NACCT-SRYYM
              MOVE    NYUINACCT-REC         TO      MCPDATA-REC
              MOVE    "DBSELECT"            TO      MCP-FUNC
              MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
              MOVE    "key2"                TO      MCP-PATHNAME
              CALL    "ORCDBMAIN"             USING MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
              IF      MCP-RC             =       ZERO
      *
                  MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                  MOVE    "key2"            TO      MCP-PATHNAME
                  PERFORM  NYUINACCT-READ-SEC
                  PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                    EVALUATE  IDX
                      WHEN     1
                         MOVE    1    TO    FLG-KAIKEI-SAKUSEI1
                      WHEN     2
                         MOVE    1    TO    FLG-KAIKEI-SAKUSEI2
                    END-EVALUATE
      *
                    IF     NACCT-ZAISKBKBN    =    "1"  OR  "5"
                                                        OR  "6"
                          PERFORM    NYUIN-DAY-TAIHI-SEC
                    END-IF
      * 
                    MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                    MOVE    "key2"            TO      MCP-PATHNAME
                    PERFORM        NYUINACCT-READ-SEC
                  END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
              MOVE    "key2"            TO      MCP-PATHNAME
              PERFORM  CSR-CLOSE-SEC
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
               IF      ( WRK-DAY1(IDX IDY)   NOT =  ZERO ) 
               AND     ( WRK-DAY5(IDX IDY)   NOT =  ZERO )
      * 
                  MOVE   ZERO     TO    FLG-HANBETU
                  PERFORM    VARYING   IDZ  FROM   1   BY   1
                               UNTIL ( IDZ    >    10 )
                                 OR  ( WRK-HANBETU-SKYYM(IDZ) = SPACE )
                   IF   (WRK-HANBETU-SKYYM(IDZ)    = WRK-SKYYM(IDX)) 
                   AND  (WRK-HANBETU-HKNCOMBINUM(IDZ)
                                                   = WRK-DAY5(IDX IDY))
                     MOVE "1"                TO WRK-HANBETU-DAY(IDZ IDY)
                     MOVE  1                 TO FLG-HANBETU
                     MOVE  10                TO IDZ
                   END-IF
                  END-PERFORM
      *
                  IF   FLG-HANBETU   =   ZERO
                   IF  IDZ <= 10
                     MOVE WRK-SKYYM(IDX)     TO WRK-HANBETU-SKYYM(IDZ)
                     MOVE WRK-DAY5(IDX IDY)  TO  
                                          WRK-HANBETU-HKNCOMBINUM(IDZ)
                     MOVE "1"                TO WRK-HANBETU-DAY(IDZ IDY)
                   ELSE
                     MOVE  1                 TO WRK-KENSU-ERR
                   END-IF
                  END-IF
      *
               ELSE
      *
                IF      ( WRK-DAY1(IDX IDY)       =  ZERO )
                AND     ( WRK-DAY5(IDX IDY)   NOT =  ZERO )
      *
                  MOVE   ZERO     TO    FLG-HANBETU
                  PERFORM    VARYING   IDZ  FROM   1   BY   1
                               UNTIL ( IDZ    >    10 )
                                 OR  ( WRK-HANBETU-SKYYM(IDZ) = SPACE )
                   IF   (WRK-HANBETU-SKYYM(IDZ)    = WRK-SKYYM(IDX))
                   AND  (WRK-HANBETU-HKNCOMBINUM(IDZ)
                                                   = WRK-DAY5(IDX IDY))
                     MOVE "3"                TO WRK-HANBETU-DAY(IDZ IDY)
                     MOVE  1                 TO FLG-HANBETU
                     MOVE  10                TO IDZ
                   END-IF
                  END-PERFORM
      *
                  IF   FLG-HANBETU   =   ZERO
                   IF  IDZ <= 10
                     MOVE WRK-SKYYM(IDX)     TO WRK-HANBETU-SKYYM(IDZ)
                     MOVE WRK-DAY5(IDX IDY)  TO  
                                          WRK-HANBETU-HKNCOMBINUM(IDZ)
                     MOVE "3"                TO WRK-HANBETU-DAY(IDZ IDY)
                   ELSE
                     MOVE  1                 TO WRK-KENSU-ERR
                   END-IF
                  END-IF
      *
                END-IF
      *
               END-IF
      *
              END-PERFORM
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                       UNTIL   (IDX                    >    10 )
                         OR    (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
             PERFORM  VARYING   IDY    FROM   1   BY   1
                       UNTIL    IDY     >    31
                 MOVE   WRK-HANBETU-DAY(IDX IDY)
                                  TO  WRK-HANBETU-DAY-NACCT(IDX IDY)
             END-PERFORM
      *
           END-PERFORM.
      *
           IF   ((WRK-SKYYM(1)        NOT = SPACE) AND
                 (FLG-KAIKEI-SAKUSEI1     = ZERO ))
           OR   ((WRK-SKYYM(2)        NOT = SPACE) AND
                 (FLG-KAIKEI-SAKUSEI2     = ZERO ))
                 MOVE   1         TO  FLG-KAIKEI-SAKUSEI
           END-IF.
      *
       NYUINKAIKEI-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ñ·×¥Õ¥é¥°ÂàÈò½èÍý                                          *
      ******************************************************************
       NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDX IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5(IDX IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "6"
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY6(IDX IDY)
               END-IF
      *
             END-IF
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                       UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO
               IF     NACCT-ZAISKBKBN    =    "1"
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1X(IDX IDY)
               END-IF 
               IF     NACCT-ZAISKBKBN    =    "5"
                    MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5X(IDX IDY)
               END-IF 
             END-IF
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                       UNTIL    IDY     >    WRK-HI-ED(IDX)
      *
             IF       NACCT-ZAISKBKBN    =    "5"
             AND      NACCT-DAY(IDY)     =    999
                    MOVE   1               TO   FLG-HKNCOMBINUM999
             END-IF
      *
           END-PERFORM.
      *
       NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ²ñ·×½èÍý                                                *
      ******************************************************************
       SINRYOKAIKEI-SYORI-SEC         SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL    (IDX            >     2)
                                OR     (WRK-SKYYM(IDX) =  SPACE)
      *
              INITIALIZE                            SRYACCT-REC
              MOVE    LNK-S02-HOSPNUM        TO      ACCT-HOSPNUM
              MOVE    LNK-S02-NYUGAIKBN     TO      ACCT-NYUGAIKBN
              MOVE    LNK-S02-PTID          TO      ACCT-PTID
              MOVE    WRK-SKYYM(IDX)        TO      ACCT-SRYYM
              MOVE    SRYACCT-REC           TO      MCPDATA-REC
              MOVE    "DBSELECT"            TO      MCP-FUNC
              MOVE    "tbl_sryacct"         TO      MCP-TABLE
      ***     MOVE    "key33"               TO      MCP-PATHNAME
              MOVE    "key63"               TO      MCP-PATHNAME
              CALL "ORCDBMAIN"    USING MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
              IF      MCP-RC             =       ZERO
      *
                  MOVE    "tbl_sryacct"     TO      MCP-TABLE
      ***         MOVE    "key33"           TO      MCP-PATHNAME
                  MOVE    "key63"           TO      MCP-PATHNAME
                  PERFORM  SRYACCT-READ-SEC
                  PERFORM  UNTIL  (FLG-SRYACCT  =    "1")
      *
                    PERFORM    NYUIN-DAY-TAIHI2-SEC
      *
                    MOVE    "tbl_sryacct"   TO      MCP-TABLE
      ***           MOVE    "key33"         TO      MCP-PATHNAME
                    MOVE    "key63"         TO      MCP-PATHNAME
                    PERFORM        SRYACCT-READ-SEC
      *
                  END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_sryacct"   TO      MCP-TABLE
      ***     MOVE    "key33"         TO      MCP-PATHNAME
              MOVE    "key63"         TO      MCP-PATHNAME
              PERFORM  CSR-CLOSE-SEC
      *
           END-PERFORM.
      *
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU2-SKYYM(IDX) =  SPACE)
      *
             MOVE       ZERO     TO       FLG-HANBETU
      *
             PERFORM    VARYING   IDY   FROM   1   BY   1
                              UNTIL  (IDY            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDY)  =  SPACE)
      *
               IF  (WRK-HANBETU2-SKYYM(IDX) 
                                       = WRK-HANBETU-SKYYM(IDY))
               AND (WRK-HANBETU2-HKNCOMBINUM(IDX)
                                       = WRK-HANBETU-HKNCOMBINUM(IDY)) 
      *
                   PERFORM    VARYING   IDZ   FROM   1   BY   1
                                         UNTIL    IDZ    >    31
      *
                     IF    WRK-HANBETU2-DAY(IDX IDZ)   NOT =  SPACE 
                        IF  WRK-HANBETU-DAY(IDY IDZ) = SPACE OR "3" 
                           MOVE   "2"   TO   WRK-HANBETU-DAY(IDY IDZ)
                        END-IF
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE       1      TO       FLG-HANBETU
                   MOVE      10      TO       IDY 
      *
               END-IF
      *
             END-PERFORM
      *
             IF   FLG-HANBETU   =   ZERO
               IF IDY <= 10
                   MOVE  WRK-HANBETU2-SKYYM(IDX)
                                 TO  WRK-HANBETU-SKYYM(IDY)
                   MOVE  WRK-HANBETU2-HKNCOMBINUM(IDX)
                                 TO  WRK-HANBETU-HKNCOMBINUM(IDY)
                   PERFORM    VARYING   IDZ   FROM   1   BY   1
                                         UNTIL    IDZ    >    31
                      IF    WRK-HANBETU2-DAY(IDX IDZ)   NOT =  SPACE 
                          MOVE  "2"     TO  WRK-HANBETU-DAY(IDY IDZ)
                      END-IF 
                   END-PERFORM
               ELSE
                   MOVE   1      TO  WRK-KENSU-ERR
               END-IF
             END-IF
      *
           END-PERFORM.
      *
       SINRYOKAIKEI-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ²ñ·×¥Õ¥é¥°ÂàÈò£²½èÍý                                         *
      ******************************************************************
       NYUIN-DAY-TAIHI2-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                                    UNTIL    IDY    >    WRK-HI-ED(IDX)
      *
             IF      ( ACCT-DAY(1 IDY)   NOT =  ZERO )
             AND     ( WRK-DAY5(IDX IDY) NOT =  ZERO )
              IF   ACCT-JIHOKBN   =  SPACE
                 MOVE   ZERO     TO    FLG-HANBETU
                 PERFORM    VARYING   IDZ  FROM   1   BY   1
                              UNTIL  ( IDZ    >    10 )
                                OR   ( WRK-HANBETU2-SKYYM(IDZ) = SPACE )
                   IF    WRK-HANBETU2-SKYYM(IDZ)    = WRK-SKYYM(IDX) 
                   AND   WRK-HANBETU2-HKNCOMBINUM(IDZ)
                                                    = ACCT-HKNCOMBI
                     MOVE   "2"        TO  WRK-HANBETU2-DAY(IDZ IDY)
                     MOVE    1         TO  FLG-HANBETU
                     MOVE    10        TO  IDZ
                   END-IF
                 END-PERFORM
                 IF   FLG-HANBETU   =   ZERO
                   IF IDZ <= 10
                     MOVE   WRK-SKYYM(IDX) 
                                       TO  WRK-HANBETU2-SKYYM(IDZ)
                     MOVE   ACCT-HKNCOMBI      
                                       TO  WRK-HANBETU2-HKNCOMBINUM(IDZ)
                     MOVE   "2"        TO  WRK-HANBETU2-DAY(IDZ IDY)
                   ELSE
                     MOVE    1         TO  WRK-KENSU-ERR
                   END-IF
                 END-IF
              ELSE
                 MOVE   ZERO     TO    FLG-HANBETU
                 PERFORM    VARYING   IDZ  FROM   1   BY   1
                              UNTIL  ( IDZ    >    10 )
                                OR   ( WRK-HANBETU2-SKYYM(IDZ) = SPACE )
                   IF    WRK-HANBETU2-SKYYM(IDZ)    = WRK-SKYYM(IDX) 
                   AND   WRK-HANBETU2-HKNCOMBINUM(IDZ)
                                                    = ACCT-HKNCOMBI
                     MOVE    1         TO  FLG-HANBETU
                     MOVE    10        TO  IDZ
                   END-IF
                 END-PERFORM
                 IF   FLG-HANBETU   =   ZERO
                   IF IDZ <= 10
                     MOVE   WRK-SKYYM(IDX) 
                                       TO  WRK-HANBETU2-SKYYM(IDZ)
                     MOVE   ACCT-HKNCOMBI      
                                       TO  WRK-HANBETU2-HKNCOMBINUM(IDZ)
                   ELSE
                     MOVE    1         TO  WRK-KENSU-ERR
                   END-IF
                 END-IF
              END-IF
             END-IF
      *
           END-PERFORM.
      *
      *    Æþ±¡´ü´Ö³°¤ÎÆþ±¡¿ÇÎÅ¹Ô°ÙÆþÎÏ¥Á¥§¥Ã¥¯¡Ê¥ì¥»¥×¥È½èÍý»þ¡Ë
      *    ¡ÊÆþ±¡´µ¼Ô¤ÎÂà±¡ÅÐÏ¿¤ò¡ÖÂà±¡ÅÐÏ¿¡Ê¿ÇÎÅÊÝÂ¸¡Ë¡×¤Ç¹Ô¤¤¡¢
      *      Æþ±¡´ü´Ö³°¤ËÆþ±¡¿ÇÎÅ¹Ô°ÙÆþÎÏ¤¬Â¸ºß¤¹¤ë»ö¤È¤Ê¤Ã¤¿¾ì¹ç¡Ë
           IF   LNK-S02-TEIKI-KBN   =   "X"
             PERFORM    VARYING   IDY   FROM   1   BY   1
                         UNTIL    IDY    >    31
                 IF  ( ACCT-DAY(1 IDY)   NOT =  ZERO )
                 AND ( WRK-DAY5X(IDX IDY)    =  ZERO )
                     MOVE    1         TO  WRK-NYUINKIKANGAI-ERR
                 END-IF
             END-PERFORM
           END-IF.
      *
       NYUIN-DAY-TAIHI2-EXT.
           EXIT.
      *
      ******************************************************************
      *    Æ±ÆüºÆÆþ±¡²ñ·×½èÍý                                          *
      ******************************************************************
       DOJITUKAIKEI-SYORI-SEC         SECTION.
      *
           PERFORM    VARYING   IDX   FROM   1   BY   1
                       UNTIL   (IDX            >    2  )
                        OR     (WRK-SKYYM(IDX) =  SPACE)
      *
             PERFORM  VARYING   IDY   FROM   WRK-HI-ST(IDX)   BY   1
                       UNTIL    IDY     >    WRK-HI-ED(IDX)
      *
               IF  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX IDY)
                                                         NOT =  ZERO
                 MOVE     ZERO     TO  FLG-HANBETU
                 PERFORM   VARYING  IDZ  FROM   1   BY   1
                            UNTIL  (IDZ                     >  10  )
                             OR    (WRK-HANBETU3-SKYYM(IDZ) = SPACE)
                   IF   WRK-HANBETU3-SKYYM(IDZ) = WRK-SKYYM(IDX) 
                   AND  WRK-HANBETU3-HKNCOMBINUM(IDZ) =
                          WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX IDY)
                     MOVE   "9"    TO  WRK-HANBETU3-DAY(IDZ IDY)
                     MOVE    1     TO  FLG-HANBETU
                     MOVE    10    TO  IDZ
                   END-IF
                 END-PERFORM
                 IF   FLG-HANBETU   =   ZERO
                  IF  IDZ <= 10
                   MOVE   WRK-SKYYM(IDX) 
                                   TO  WRK-HANBETU3-SKYYM(IDZ)
                   MOVE   WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX IDY)
                                   TO  WRK-HANBETU3-HKNCOMBINUM(IDZ)
                   MOVE   "9"      TO  WRK-HANBETU3-DAY(IDZ IDY)
                  ELSE
                   MOVE    1       TO  WRK-KENSU-ERR
                  END-IF
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           END-PERFORM.
      *
      *
           PERFORM    VARYING   IDX   FROM   1   BY   1
                       UNTIL   (IDX                     >   10  )
                        OR     (WRK-HANBETU3-SKYYM(IDX) =  SPACE)
      *
             MOVE       ZERO     TO  FLG-HANBETU
             PERFORM  VARYING   IDY   FROM   1   BY   1
                       UNTIL   (IDY                     >   10  )
                        OR     (WRK-HANBETU-SKYYM(IDY)  =  SPACE)
               IF  (WRK-HANBETU3-SKYYM(IDX) = WRK-HANBETU-SKYYM(IDY))
               AND (WRK-HANBETU3-HKNCOMBINUM(IDX)
                                      = WRK-HANBETU-HKNCOMBINUM(IDY)) 
                   MOVE    1     TO  FLG-HANBETU
                   MOVE   10     TO  IDY
               END-IF
             END-PERFORM
             IF   FLG-HANBETU   =   ZERO
              IF  IDY <= 10
               MOVE  WRK-HANBETU3-SKYYM(IDX)
                                 TO  WRK-HANBETU-SKYYM(IDY)
               MOVE  WRK-HANBETU3-HKNCOMBINUM(IDX)
                                 TO  WRK-HANBETU-HKNCOMBINUM(IDY)
               PERFORM  VARYING  IDZ  FROM   1   BY   1
                         UNTIL   IDZ   >    31
                  IF    WRK-HANBETU3-DAY(IDX IDZ)  NOT =  SPACE
                     MOVE  "9"   TO  WRK-HANBETU-DAY(IDY IDZ)
                  END-IF 
               END-PERFORM
              ELSE
               MOVE     1        TO  WRK-KENSU-ERR
              END-IF
             END-IF
      *
           END-PERFORM.
      *
       DOJITUKAIKEI-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥Ç¡¼¥¿ÊÌ³«»Ï¡¦½ªÎ»Æü¥»¥Ã¥È                                  *
      ******************************************************************
       KAISHISYURYO-SYORI-SEC        SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                                         UNTIL    IDY    >    31
                IF        WRK-HANBETU-DAY(IDX IDY)   NOT =  SPACE 
                   MOVE   IDY     TO    WRK-HANBETU-KAISHI(IDX)
                   MOVE   31      TO    IDY
                END-IF
              END-PERFORM
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  MOVE   WRK-HI-ST(1)  TO  WRK-HANBETU-KAISHI2(IDX)
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                  MOVE   WRK-HI-ST(2)  TO  WRK-HANBETU-KAISHI2(IDX)
              END-IF
      *
      ****    PERFORM    VARYING   IDY   FROM  31   BY  -1
      ****                               UNTIL    IDY    <    1
      ****      IF        WRK-HANBETU-DAY(IDX IDY)   NOT =  SPACE 
      ****         MOVE   IDY     TO    WRK-HANBETU-SYURYO(IDX)
      ****         MOVE   1       TO    IDY
      ****      END-IF
      ****    END-PERFORM
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  MOVE   WRK-HI-ED(1)  TO  WRK-HANBETU-SYURYO(IDX)
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                  MOVE   WRK-HI-ED(2)  TO  WRK-HANBETU-SYURYO(IDX)
              END-IF
      *
           END-PERFORM.
      *
       KAISHISYURYO-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÈÖ¹æ¼èÆÀ                                                *
      ******************************************************************
       HKNNUM-SYUTOKU-SYORI-SEC        SECTION.
      *
           MOVE       ZERO    TO       FLG-ZAIARI1.
           MOVE       ZERO    TO       FLG-ZAIARI2.
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >     2 )
                                OR   (WRK-SKYYM(IDX) =  SPACE)
              PERFORM  SRYACT-KENSAKU-SEC
           END-PERFORM.
      *
           INITIALIZE         TABLE-CYOKI.
           MOVE       ZERO    TO       FLG-SYUARI1.
           MOVE       ZERO    TO       FLG-KOHARI1.
           MOVE       ZERO    TO       FLG-DAISANARI1.
           MOVE       ZERO    TO       FLG-SYUARI2.
           MOVE       ZERO    TO       FLG-KOHARI2.
           MOVE       ZERO    TO       FLG-DAISANARI2.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
      *       ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î¸¡º÷
              INITIALIZE                           HKNCOMBI-REC
              MOVE    LNK-S02-HOSPNUM      TO      COMB-HOSPNUM
              MOVE    LNK-S02-PTID         TO      COMB-PTID
              MOVE    WRK-HANBETU-HKNCOMBINUM(IDX)
                                           TO      COMB-HKNCOMBINUM
              MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
              MOVE    "DBSELECT"           TO      MCP-FUNC
              MOVE    "tbl_hkncombi"       TO      MCP-TABLE
              MOVE    "key"                TO      MCP-PATHNAME
              CALL    "ORCDBMAIN" USING    MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
              IF      MCP-RC               =       ZERO
                  MOVE    "tbl_hkncombi"   TO      MCP-TABLE
                  MOVE    "key"            TO      MCP-PATHNAME
                  PERFORM   HKNCOMBI-READ-SEC
              END-IF
      *
              MOVE    "tbl_hkncombi"   TO      MCP-TABLE
              MOVE    "key"            TO      MCP-PATHNAME
              PERFORM  CSR-CLOSE-SEC
      *
              MOVE COMB-TEKSTYMD      TO WRK-HANBETU-TEKSTYMD(IDX)
              IF   WRK-HANBETU-TEKSTYMD(IDX) = SPACE
                 MOVE  "00000000"     TO WRK-HANBETU-TEKSTYMD(IDX)
              END-IF
              MOVE COMB-TEKEDYMD      TO WRK-HANBETU-TEKEDYMD(IDX)
              IF   WRK-HANBETU-TEKEDYMD(IDX) = SPACE
                 MOVE  "00000000"     TO WRK-HANBETU-TEKEDYMD(IDX)
              END-IF
      *
              MOVE COMB-HKNNUM        TO WRK-HANBETU-SYUHKNNUM(IDX)
              MOVE COMB-KOH1HKNNUM    TO WRK-HANBETU-KOH1HKNNUM(IDX)
              MOVE COMB-KOH2HKNNUM    TO WRK-HANBETU-KOH2HKNNUM(IDX)
              MOVE COMB-KOH3HKNNUM    TO WRK-HANBETU-KOH3HKNNUM(IDX)
              MOVE COMB-KOH4HKNNUM    TO WRK-HANBETU-KOH4HKNNUM(IDX)
              MOVE COMB-NYU-KFTNRATE  TO WRK-HANBETU-PTFTNRATE(IDX)
      *
              PERFORM  KANGAN-CHECK-SYORI-SEC
      *
              IF   COMB-HKNNUM  NOT =  SPACE
                INITIALIZE                           PTHKNINF-REC
                MOVE    LNK-S02-HOSPNUM      TO      PTHKN-HOSPNUM
                MOVE    LNK-S02-PTID         TO      PTHKN-PTID
                MOVE    COMB-HKNID           TO      PTHKN-HKNID
                MOVE    PTHKNINF-REC         TO      MCPDATA-REC
                MOVE    "DBSELECT"           TO      MCP-FUNC
                MOVE    "tbl_pthkninf"       TO      MCP-TABLE
                MOVE    "key"                TO      MCP-PATHNAME
                CALL "ORCDBMAIN" USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                IF      MCP-RC               =       ZERO
                    MOVE    "tbl_pthkninf"   TO      MCP-TABLE
                    MOVE    "key"            TO      MCP-PATHNAME
                    MOVE    "DBFETCH"        TO      MCP-FUNC
                    CALL "ORCDBMAIN" USING MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                    IF      MCP-RC           =       ZERO
                       MOVE  MCPDATA-REC     TO      PTHKNINF-REC
                    END-IF
                END-IF
                MOVE    "tbl_pthkninf"       TO      MCP-TABLE
                MOVE    "key"                TO      MCP-PATHNAME
                PERFORM  CSR-CLOSE-SEC
                IF   (COMB-HKNNUM         =   "060")
                AND  (PTHKN-HKNJANUM  NOT =   SPACE)
                   MOVE  PTHKN-HKNJANUM  TO  WRK-HANBETU-HKNJANUM(IDX)
                END-IF
              ELSE
                INITIALIZE                           PTHKNINF-REC
              END-IF
              IF   PTHKN-TEKSTYMD   NOT =  SPACE
                IF   PTHKN-TEKSTYMD(1:6) <  WRK-HANBETU-SKYYM(IDX)
                   MOVE WRK-HANBETU-SKYYM(IDX)
                                     TO WRK-HANBETU-YUSENYMD(IDX)(1:6)
                   MOVE "01"         TO WRK-HANBETU-YUSENYMD(IDX)(7:2)
                ELSE
                   MOVE PTHKN-TEKSTYMD  TO WRK-HANBETU-YUSENYMD(IDX)
                END-IF
              ELSE
                IF   COMB-TEKSTYMD(1:6)  <  WRK-HANBETU-SKYYM(IDX)
                   MOVE WRK-HANBETU-SKYYM(IDX)
                                     TO WRK-HANBETU-YUSENYMD(IDX)(1:6)
                   MOVE "01"         TO WRK-HANBETU-YUSENYMD(IDX)(7:2)
                ELSE
                   MOVE COMB-TEKSTYMD   TO WRK-HANBETU-YUSENYMD(IDX)
                END-IF
              END-IF
      *
              MOVE     ZERO    TO    IDX5
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                   PERFORM   VARYING   IDX4   FROM  1  BY   1
                              UNTIL    IDX4    >   31
                     IF   WRK-DAY5X(1 IDX4)   NOT =  ZERO
                         MOVE     IDX4    TO   IDX5
                         MOVE      31     TO   IDX4
                     END-IF
                   END-PERFORM
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                   PERFORM   VARYING   IDX4   FROM  1  BY   1
                              UNTIL    IDX4    >   31
                     IF   WRK-DAY5X(2 IDX4)   NOT =  ZERO
                         MOVE     IDX4    TO   IDX5
                         MOVE      31     TO   IDX4
                     END-IF
                   END-PERFORM
              END-IF
              IF   WRK-HANBETU-YUSENDD(IDX)  <  IDX5
                  MOVE    IDX5   TO   WRK-HANBETU-YUSENDD(IDX)
              END-IF
      *
              IF   COMB-HKNNUM        =    "002"                        
                 EVALUATE   COMB-HOJOKBN                                
                    WHEN   "3"                                          
                    WHEN   "C"                                          
                    WHEN   "F"                                          
                    WHEN   "I"                                          
                        MOVE   "1"    TO WRK-HANBETU-SENINFLG(IDX)      
                 END-EVALUATE                                           
              END-IF                                                    
      *
      *       ¸øÈñÍ¥Àè¤ËÍ¥ÀèÈÖ¹æ¥»¥Ã¥È
              IF  WRK-HANBETU-KOH1HKNNUM(IDX) = SPACE OR "027"
                  CONTINUE
              ELSE
                IF   WRK-HANBETU-KOH1HKNNUM(IDX)(1:1) >= "1" AND <= "8"
                  MOVE  990   TO  WRK-HANBETU-YUSENBAN(IDX)
                                  WRK-HANBETU-YUSENBAN2(IDX)
                ELSE
                  MOVE  WRK-HANBETU-KOH1HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN(IDX)
                  IF    WRK-HANBETU-KOH1HKNNUM(IDX) = "972" OR "974"
                     MOVE  1  TO  WRK-HANBETU-YUSENBAN(IDX)
                  ELSE
                     MOVE  WRK-HANBETU-KOH1HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN2(IDX)
                  END-IF
                  IF   (WRK-HANBETU-KOH1HKNNUM(IDX)  =  "092"   )
                  AND  (FLG-TAISYO-KANGAN            =   ZERO   )
                  AND  (WRK-HANBETU-SKYYM(IDX)      >=  "201812")
                     MOVE  998   TO  WRK-HANBETU-YUSENBAN(IDX)
                                     WRK-HANBETU-YUSENBAN2(IDX)
                  END-IF
                END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH2HKNNUM(IDX) = SPACE
                  CONTINUE
              ELSE
                IF   WRK-HANBETU-KOH2HKNNUM(IDX)(1:1) >= "1" AND <= "8"
                  IF ( WRK-HANBETU-KOH1HKNNUM(IDX)  =  "027"      )
                  OR ((WRK-HANBETU-KOH1HKNNUM(IDX)  =  "092"     ) AND
                      (FLG-TAISYO-KANGAN            =   ZERO     ) AND
                      (WRK-HANBETU-SKYYM(IDX)      >=  "201812"  ))
                     MOVE  990   TO  WRK-HANBETU-YUSENBAN(IDX)
                                     WRK-HANBETU-YUSENBAN2(IDX)
                  END-IF
                ELSE
                  IF ( WRK-HANBETU-YUSENBAN(IDX)
                                     > WRK-HANBETU-KOH2HKNNUM(IDX))
                  OR ( WRK-HANBETU-KOH1HKNNUM(IDX)  =  "027"      )
                  OR ((WRK-HANBETU-KOH1HKNNUM(IDX)  =  "092"     ) AND
                      (FLG-TAISYO-KANGAN            =   ZERO     ) AND
                      (WRK-HANBETU-SKYYM(IDX)      >=  "201812"  ))
                     MOVE  WRK-HANBETU-KOH2HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN(IDX)
                  END-IF
                  IF     WRK-HANBETU-KOH2HKNNUM(IDX) = "972" OR "974"
                     MOVE  1  TO  WRK-HANBETU-YUSENBAN(IDX)
                  ELSE
                     IF ( WRK-HANBETU-YUSENBAN2(IDX)
                                     > WRK-HANBETU-KOH2HKNNUM(IDX) )
                     OR ( WRK-HANBETU-KOH1HKNNUM(IDX)  =  "027"    )
                     OR ((WRK-HANBETU-KOH1HKNNUM(IDX)  =  "092"   ) AND
                         (FLG-TAISYO-KANGAN            =   ZERO   ) AND
                         (WRK-HANBETU-SKYYM(IDX)      >=  "201812"))
                        MOVE  WRK-HANBETU-KOH2HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN2(IDX)
                     END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH3HKNNUM(IDX) = SPACE
                  CONTINUE
              ELSE
                IF   WRK-HANBETU-KOH3HKNNUM(IDX)(1:1) >= "1" AND <= "8"
                  CONTINUE
                ELSE
                  IF  WRK-HANBETU-YUSENBAN(IDX)
                                     > WRK-HANBETU-KOH3HKNNUM(IDX)
                     MOVE  WRK-HANBETU-KOH3HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN(IDX)
                  END-IF
                  IF     WRK-HANBETU-KOH3HKNNUM(IDX) = "972" OR "974"
                     MOVE  1  TO  WRK-HANBETU-YUSENBAN(IDX)
                  ELSE
                     IF  WRK-HANBETU-YUSENBAN2(IDX)
                                     > WRK-HANBETU-KOH3HKNNUM(IDX)
                        MOVE  WRK-HANBETU-KOH3HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN2(IDX)
                     END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF  WRK-HANBETU-KOH4HKNNUM(IDX) = SPACE
                  CONTINUE
              ELSE
                IF   WRK-HANBETU-KOH4HKNNUM(IDX)(1:1) >= "1" AND <= "8"
                  CONTINUE
                ELSE
                  IF  WRK-HANBETU-YUSENBAN(IDX)
                                     > WRK-HANBETU-KOH4HKNNUM(IDX)
                     MOVE  WRK-HANBETU-KOH4HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN(IDX)
                  END-IF
                  IF     WRK-HANBETU-KOH4HKNNUM(IDX) = "972" OR "974"
                     MOVE  1  TO  WRK-HANBETU-YUSENBAN(IDX)
                  ELSE
                     IF  WRK-HANBETU-YUSENBAN2(IDX)
                                     > WRK-HANBETU-KOH4HKNNUM(IDX)
                        MOVE  WRK-HANBETU-KOH4HKNNUM(IDX)
                              TO  WRK-HANBETU-YUSENBAN2(IDX)
                     END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF  WRK-HANBETU-YUSENBAN(IDX)  =   ZERO
                   MOVE   999   TO  WRK-HANBETU-YUSENBAN(IDX)
              END-IF
      *
              IF  WRK-HANBETU-YUSENBAN(IDX)  =   1
                IF   WRK-HANBETU-YUSENBAN2(IDX)  =   ZERO
                   MOVE   999   TO  WRK-HANBETU-YUSENBAN2(IDX)
                END-IF
      *         ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»Å¬ÍÑ³«»ÏÆü¥Á¥§¥Ã¥¯
                MOVE  WRK-HANBETU-SKYYM(IDX)
                                TO  WRK-HANBETU-CYOKIYM(IDX)
                IF   COMB-TEKSTYMD(1:6)  >=  WRK-HANBETU-SKYYM(IDX)
                  MOVE  COMB-TEKSTYMD(7:2)
                                TO  WRK-HANBETU-CYOKIDD(IDX)
                ELSE
                  MOVE  01      TO  WRK-HANBETU-CYOKIDD(IDX)
                END-IF
              ELSE
                   MOVE   999   TO  WRK-HANBETU-YUSENBAN2(IDX)
              END-IF
      *
              IF  WRK-HANBETU-YUSENBAN(IDX)  =   1
                IF   (WRK-HANBETU-KOH1HKNNUM(IDX) =  "972" OR "974")
                AND  (WRK-HANBETU-KOH2HKNNUM(IDX) =  SPACE         )
                AND  (WRK-HANBETU-KOH3HKNNUM(IDX) =  SPACE         )
                AND  (WRK-HANBETU-KOH4HKNNUM(IDX) =  SPACE         )
                  MOVE    ZERO    TO    FLG-HANBETU
                  PERFORM  VARYING  IDX9  FROM  1  BY  1
                            UNTIL  (IDX9                    >   10  )
                              OR   (TABLE-CYOKI-SKYYM(IDX9) =  SPACE)
                    IF  (WRK-HANBETU-SKYYM(IDX)
                                        = TABLE-CYOKI-SKYYM(IDX9)   )
                    AND (WRK-HANBETU-YUSENYMD(IDX)
                                        = TABLE-CYOKI-YUSENYMD(IDX9))
                      ADD     1   TO  TABLE-CYOKI-HENSYU1(IDX9)
                      MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD1(IDX9)
                      MOVE    1   TO  FLG-HANBETU
                      MOVE   10   TO  IDX9
                    END-IF
                  END-PERFORM
                  IF   FLG-HANBETU   =   ZERO
                      MOVE WRK-HANBETU-SKYYM(IDX)
                                  TO  TABLE-CYOKI-SKYYM(IDX9)
                      MOVE WRK-HANBETU-YUSENYMD(IDX)
                                  TO  TABLE-CYOKI-YUSENYMD(IDX9)
                      ADD     1   TO  TABLE-CYOKI-HENSYU1(IDX9)
                      MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD1(IDX9)
                  END-IF
                ELSE
                  MOVE    ZERO    TO    FLG-HANBETU
                  PERFORM  VARYING  IDX9  FROM  1  BY  1
                            UNTIL  (IDX9                    >   10  )
                              OR   (TABLE-CYOKI-SKYYM(IDX9) =  SPACE)
                    IF  (WRK-HANBETU-SKYYM(IDX)
                                        = TABLE-CYOKI-SKYYM(IDX9)   )
                    AND (WRK-HANBETU-YUSENYMD(IDX)
                                        = TABLE-CYOKI-YUSENYMD(IDX9))
                      ADD     1   TO  TABLE-CYOKI-HENSYU2(IDX9)
                      EVALUATE  TABLE-CYOKI-HENSYU2(IDX9)
                        WHEN      1
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD21(IDX9)
                        WHEN      2
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD22(IDX9)
                        WHEN      3
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD23(IDX9)
                      END-EVALUATE
                      MOVE    1   TO  FLG-HANBETU
                      MOVE   10   TO  IDX9
                    END-IF
                  END-PERFORM
                  IF   FLG-HANBETU   =   ZERO
                      MOVE WRK-HANBETU-SKYYM(IDX)
                                  TO  TABLE-CYOKI-SKYYM(IDX9)
                      MOVE WRK-HANBETU-YUSENYMD(IDX)
                                  TO  TABLE-CYOKI-YUSENYMD(IDX9)
                      ADD     1   TO  TABLE-CYOKI-HENSYU2(IDX9)
                      EVALUATE  TABLE-CYOKI-HENSYU2(IDX9)
                        WHEN      1
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD21(IDX9)
                        WHEN      2
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD22(IDX9)
                        WHEN      3
                          MOVE WRK-HANBETU-CYOKIYMD(IDX)
                                  TO  TABLE-CYOKI-YMD23(IDX9)
                      END-EVALUATE
                  END-IF
                END-IF
              END-IF
      *
      *       Âè»°¼Ô¹Ô°ÙÊ¬¤Î·×»»Í¥Àè½ç°Ì¤ò²¼¤²¤ë
              IF  WRK-HANBETU-KOH1HKNNUM(IDX) = "970"
                  MOVE    "1"   TO  WRK-HANBETU-DAISANFLG(IDX)
              END-IF
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                IF   (WRK-HANBETU-SYUHKNNUM(IDX)   NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH2HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH3HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH4HKNNUM(IDX)      = SPACE)
                    MOVE     1    TO  FLG-SYUARI1
                END-IF
                IF   (WRK-HANBETU-SYUHKNNUM(IDX)   NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)  NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)  NOT = "970")
                    MOVE     1    TO  FLG-KOHARI1
                END-IF
                IF  WRK-HANBETU-KOH1HKNNUM(IDX) = "970"
                    MOVE     1    TO  FLG-DAISANARI1
                END-IF
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                IF   (WRK-HANBETU-SYUHKNNUM(IDX)   NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH2HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH3HKNNUM(IDX)      = SPACE)
                AND  (WRK-HANBETU-KOH4HKNNUM(IDX)      = SPACE)
                    MOVE     1    TO  FLG-SYUARI2
                END-IF
                IF   (WRK-HANBETU-SYUHKNNUM(IDX)   NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)  NOT = SPACE)
                AND  (WRK-HANBETU-KOH1HKNNUM(IDX)  NOT = "970")
                    MOVE     1    TO  FLG-KOHARI2
                END-IF
                IF  WRK-HANBETU-KOH1HKNNUM(IDX) = "970"
                    MOVE     1    TO  FLG-DAISANARI2
                END-IF
              END-IF
      *
           END-PERFORM.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                IF    FLG-DAISANARI1  =   1
                AND   FLG-ZAIARI1     =   1
                AND   FLG-SYUARI1     =   1
                AND   FLG-KOHARI1     =  ZERO
      *           Âè»°¼Ô¹Ô°ÙÊ¬¤Î·×»»Í¥Àè½ç°Ì¤ò¾å¤²¤ë
                  IF  WRK-HANBETU-KOH1HKNNUM(IDX) = "970"
                      MOVE   SPACE  TO  WRK-HANBETU-DAISANFLG(IDX)
                  ELSE
                      MOVE    "1"   TO  WRK-HANBETU-DAISANFLG(IDX)
                  END-IF
                  MOVE        "1"   TO  WRK-DAISANYUSENFLG(1)
                END-IF
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                IF    FLG-DAISANARI2  =   1
                AND   FLG-ZAIARI2     =   1
                AND   FLG-SYUARI2     =   1
                AND   FLG-KOHARI2     =  ZERO
      *           Âè»°¼Ô¹Ô°ÙÊ¬¤Î·×»»Í¥Àè½ç°Ì¤ò¾å¤²¤ë
                  IF  WRK-HANBETU-KOH1HKNNUM(IDX) = "970"
                      MOVE   SPACE  TO  WRK-HANBETU-DAISANFLG(IDX)
                  ELSE
                      MOVE    "1"   TO  WRK-HANBETU-DAISANFLG(IDX)
                  END-IF
                  MOVE        "1"   TO  WRK-DAISANYUSENFLG(2)
                END-IF
              END-IF
      *
           END-PERFORM.
      *
      ***
      *
           PERFORM  VARYING  IDX9  FROM  1  BY  1
                     UNTIL  (IDX9                    >   10  )
                       OR   (TABLE-CYOKI-SKYYM(IDX9) =  SPACE)
             IF   TABLE-CYOKI-HENSYU1(IDX9)  =   1
               EVALUATE  TABLE-CYOKI-HENSYU2(IDX9)
                 WHEN     1
                   IF  (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD21(IDX9))
                     MOVE   1  TO  TABLE-CYOKI-HANTEI(IDX9)
                   END-IF
                 WHEN     2
                   IF  (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD21(IDX9))
                   AND (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD22(IDX9))
                     MOVE   1  TO  TABLE-CYOKI-HANTEI(IDX9)
                   END-IF
                 WHEN     3
                   IF  (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD21(IDX9))
                   AND (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD22(IDX9))
                   AND (TABLE-CYOKI-YMD1(IDX9) <
                                     TABLE-CYOKI-YMD23(IDX9))
                     MOVE   1  TO  TABLE-CYOKI-HANTEI(IDX9)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM.
      *
      ***
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                     UNTIL  (IDX                    >   10  )
                      OR    (WRK-HANBETU-SKYYM(IDX) =  SPACE)
             IF  WRK-HANBETU-YUSENBAN(IDX)  =   1
               MOVE     ZERO    TO    FLG-HANBETU
               PERFORM  VARYING  IDX9  FROM  1  BY  1
                         UNTIL  (IDX9                    >   10  )
                          OR    (TABLE-CYOKI-SKYYM(IDX9) =  SPACE)
                 IF  (WRK-HANBETU-SKYYM(IDX)
                                     = TABLE-CYOKI-SKYYM(IDX9)   )
                 AND (WRK-HANBETU-YUSENYMD(IDX)
                                     = TABLE-CYOKI-YUSENYMD(IDX9))
                   IF  TABLE-CYOKI-HANTEI(IDX9)  =  ZERO
                     MOVE  "00000000"  TO  WRK-HANBETU-CYOKIYMD(IDX)
                   END-IF
                   MOVE    1   TO  FLG-HANBETU
                   MOVE   10   TO  IDX9
                 END-IF
               END-PERFORM
               IF   FLG-HANBETU   =   ZERO
                     MOVE  "00000000"  TO  WRK-HANBETU-CYOKIYMD(IDX)
               END-IF
             END-IF
           END-PERFORM.
      *
       HKNNUM-SYUTOKU-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ´Î¤¬¤ó¥Á¥§¥Ã¥¯                                              *
      ******************************************************************
       KANGAN-CHECK-SYORI-SEC        SECTION.
      *
           MOVE    SPACE       TO    WRK-KANGAN-SRYYMD
           MOVE    SPACE       TO    WRK-KANGAN-SKYEDYMD
           MOVE    ZERO        TO    FLG-TAISYO-KANGAN
           MOVE    ZERO        TO    FLG-KOREI-KANGAN
           MOVE    ZERO        TO    FLG-KOGAKU-KANGAN
      *
           IF   (COMB-HKNNUM          NOT =  SPACE   )
           AND  (COMB-KOH1HKNNUM          =  "092"   )
           AND  (WRK-HANBETU-SKYYM(IDX)  >=  "201812")
             PERFORM  KANGAN-CHECK-SYORI2-SEC
           END-IF
      *
           .
      *
       KANGAN-CHECK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Î¤¬¤ó¥Á¥§¥Ã¥¯£²
      *****************************************************************
       KANGAN-CHECK-SYORI2-SEC               SECTION.
      *
           MOVE WRK-HANBETU-SKYYM(IDX)
                                   TO  WRK-KANGAN-SRYYMD(1:6)
           IF   WRK-HANBETU-KAISHI(IDX)  NOT = SPACE
             MOVE  WRK-HANBETU-KAISHI(IDX)
                                   TO  WRK-KANGAN-SRYYMD(7:2)
           ELSE
             MOVE  WRK-HANBETU-KAISHI2(IDX)
                                   TO  WRK-KANGAN-SRYYMD(7:2)
           END-IF
      *
           MOVE WRK-HANBETU-SKYYM(IDX)
                                   TO  WRK-KANGAN-SKYEDYMD(1:6)
           MOVE WRK-HANBETU-SYURYO(IDX)
                                   TO  WRK-KANGAN-SKYEDYMD(7:2)
      *
      *    ¸øÈñ·î¾å¸Â³Û¼èÆÀ
           INITIALIZE                       LNK-ORCSJGNGET-REC
           MOVE  LNK-S02-HOSPNUM        TO  LNK-ORCSJGNGET-HOSPNUM
           MOVE  LNK-S02-NYUGAIKBN      TO  LNK-ORCSJGNGET-NYUGAIKBN
           MOVE  LNK-S02-PTID           TO  LNK-ORCSJGNGET-PTID
           MOVE  COMB-KOH1HKNNUM        TO  LNK-ORCSJGNGET-KOHHKNNUM
           MOVE  COMB-KOH1ID            TO  LNK-ORCSJGNGET-KOHID
           MOVE  WRK-HANBETU-SKYYM(IDX) TO  LNK-ORCSJGNGET-SRYYM
           MOVE  WRK-KANGAN-SKYEDYMD    TO  LNK-ORCSJGNGET-SKYEDYMD
           CALL    "ORCSJGNGET"         USING
                                        LNK-ORCSJGNGET-REC
                                        SPA-AREA
           IF    LNK-ORCSJGNGET-RC   =   ZERO
             IF    LNK-ORCSJGNGET-NYUJGNGAK  =  9999999
               CONTINUE
             ELSE
               MOVE    1        TO    FLG-TAISYO-KANGAN
             END-IF
           END-IF
      *
           IF    FLG-TAISYO-KANGAN  =  1
      *
             INITIALIZE                    ORCSAGECHKAREA
             MOVE    LNK-S02-HOSPNUM   TO  AGECHK-HOSPNUM
             MOVE    LNK-S02-PTID      TO  AGECHK-PTID
             MOVE    WRK-KANGAN-SRYYMD TO  AGECHK-SRYYMD
             CALL    "ORCSAGECHK"          USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
             IF      AGECHK-RC     =   ZERO
               IF  (AGECHK-NENREI >=   70  )
               OR  (COMB-HKNNUM    =  "039")
                 MOVE       1          TO  FLG-KOREI-KANGAN
               END-IF
             END-IF
      *
      *      ¹âÎðÈó³ºÅö¼Ô
             IF   (FLG-KOREI-KANGAN  =  1 )
             AND  (AGECHK-NENREI    >=  70)
               IF    COMB-HKNNUM = "039"
                  CONTINUE
               ELSE
                IF ( COMB-HKNNUM      = "060" )
                OR ((COMB-HKNNUM  NOT = "060") AND
                    (COMB-HOJOKBN     = SPACE))
                  INITIALIZE                         PTKOHINF-REC
                  MOVE    LNK-S02-HOSPNUM        TO  PTKOH-HOSPNUM
                  MOVE    LNK-S02-PTID           TO  PTKOH-PTID
                  MOVE    "976"                  TO  PTKOH-KOHNUM
                  MOVE    PTKOHINF-REC           TO  MCPDATA-REC
                  MOVE    "DBSELECT"             TO  MCP-FUNC
                  MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
                  MOVE    "key4"                 TO  MCP-PATHNAME
                  CALL    "ORCDBMAIN"            USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                  IF      MCP-RC  =  ZERO
                    MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                    MOVE    "key4"             TO  MCP-PATHNAME
                    PERFORM  PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF    =   1
                      IF  (PTKOH-TEKSTYMD     <=  WRK-KANGAN-SRYYMD)
                      AND (WRK-KANGAN-SRYYMD  <=  PTKOH-TEKEDYMD   )
                      AND (PTKOH-SAKJOKBN      =  SPACE            )
                        MOVE   ZERO  TO  FLG-KOREI-KANGAN
                        MOVE    1    TO  FLG-PTKOHINF
                      ELSE
                        MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                        MOVE    "key4"          TO  MCP-PATHNAME
                        PERFORM  PTKOHINF-READ-SEC
                      END-IF
                    END-PERFORM
                  END-IF
                  MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                  MOVE    "key4"             TO  MCP-PATHNAME
                  PERFORM  CSR-CLOSE-SEC
                END-IF
               END-IF
             END-IF
      *
             IF    FLG-KOREI-KANGAN  =  1
               MOVE    1    TO  FLG-KOGAKU-KANGAN
             ELSE
               INITIALIZE                         PTKOHINF-REC
               MOVE    LNK-S02-HOSPNUM        TO  PTKOH-HOSPNUM
               MOVE    LNK-S02-PTID           TO  PTKOH-PTID
               MOVE    "966"                  TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC           TO  MCPDATA-REC
               MOVE    "DBSELECT"             TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
               MOVE    "key4"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING  MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
               IF      MCP-RC               =       ZERO
                 MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                 MOVE    "key4"             TO  MCP-PATHNAME
                 PERFORM  PTKOHINF-READ-SEC
                 PERFORM  UNTIL   FLG-PTKOHINF    =   1
                   IF  (PTKOH-TEKSTYMD     <=  WRK-KANGAN-SRYYMD)
                   AND (WRK-KANGAN-SRYYMD  <=  PTKOH-TEKEDYMD   )
                   AND (PTKOH-SAKJOKBN      =  SPACE            )
                     MOVE    1    TO  FLG-KOGAKU-KANGAN
                     MOVE    1    TO  FLG-PTKOHINF
                   ELSE
                     MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                     MOVE    "key4"          TO  MCP-PATHNAME
                     PERFORM  PTKOHINF-READ-SEC
                   END-IF
                 END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE    "key4"             TO  MCP-PATHNAME
               PERFORM  CSR-CLOSE-SEC
      *
               IF    FLG-KOGAKU-KANGAN  =  ZERO
                 INITIALIZE                         PTKOHINF-REC
                 MOVE    LNK-S02-HOSPNUM        TO  PTKOH-HOSPNUM
                 MOVE    LNK-S02-PTID           TO  PTKOH-PTID
                 MOVE    "967"                  TO  PTKOH-KOHNUM
                 MOVE    PTKOHINF-REC           TO  MCPDATA-REC
                 MOVE    "DBSELECT"             TO  MCP-FUNC
                 MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
                 MOVE    "key4"                 TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"            USING  MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                 IF      MCP-RC               =       ZERO
                   MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                   MOVE    "key4"             TO  MCP-PATHNAME
                   PERFORM  PTKOHINF-READ-SEC
                   PERFORM  UNTIL   FLG-PTKOHINF    =   1
                     IF  (PTKOH-TEKSTYMD     <=  WRK-KANGAN-SRYYMD)
                     AND (WRK-KANGAN-SRYYMD  <=  PTKOH-TEKEDYMD   )
                     AND (PTKOH-SAKJOKBN      =  SPACE            )
                       MOVE    1    TO  FLG-KOGAKU-KANGAN
                       MOVE    1    TO  FLG-PTKOHINF
                     ELSE
                       MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                       MOVE    "key4"          TO  MCP-PATHNAME
                       PERFORM  PTKOHINF-READ-SEC
                     END-IF
                   END-PERFORM
                 END-IF
                 MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                 MOVE    "key4"             TO  MCP-PATHNAME
                 PERFORM  CSR-CLOSE-SEC
               END-IF
             END-IF
      *
             IF    FLG-KOGAKU-KANGAN  =  ZERO
               MOVE    ZERO       TO    FLG-TAISYO-KANGAN
             END-IF
      *
           END-IF
      *
           .
       KANGAN-CHECK-SYORI2-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¾ðÊó¸¡º÷                                            *
      ******************************************************************
       SRYACT-KENSAKU-SEC        SECTION.
      *
           MOVE  ZERO                 TO   FLG-ZAIARI
      *
           INITIALIZE                      SRYACT-REC
           MOVE  LNK-S02-HOSPNUM      TO   SRY-HOSPNUM
           MOVE  LNK-S02-NYUGAIKBN    TO   SRY-NYUGAIKBN
           MOVE  LNK-S02-PTID         TO   SRY-PTID
           MOVE  WRK-SKYYM(IDX)       TO   SRY-SRYYM
           MOVE  "099999931"          TO   SRY-SRYCD(1)
           MOVE  "099999931"          TO   SRY-SRYCD(2)
           MOVE  "099999931"          TO   SRY-SRYCD(3)
           MOVE  "099999931"          TO   SRY-SRYCD(4)
           MOVE  "099999931"          TO   SRY-SRYCD(5)
           MOVE  SRYACT-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"           TO   MCP-FUNC
           MOVE  "tbl_sryact"         TO   MCP-TABLE
           MOVE  "key7"               TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"       USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF     MCP-RC  =  ZERO
             MOVE    "tbl_sryact"     TO   MCP-TABLE
             MOVE    "key7"           TO   MCP-PATHNAME
             PERFORM  SRYACT-READ-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
                       OR    FLG-ZAIARI  =  1
               INITIALIZE                  SRYACCT-REC
               MOVE  SRY-HOSPNUM      TO   ACCT-HOSPNUM
               MOVE  SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
               MOVE  SRY-SRYKA        TO   ACCT-SRYKA
               MOVE  SRY-PTID         TO   ACCT-PTID
               MOVE  SRY-SRYYM        TO   ACCT-SRYYM
               MOVE  SRY-ZAINUM       TO   ACCT-ZAINUM
               MOVE  SRYACCT-REC      TO   MCPDATA-REC
               MOVE  "DBSELECT"       TO   MCP-FUNC
               MOVE  "tbl_sryacct"    TO   MCP-TABLE
               MOVE  "key31"          TO   MCP-PATHNAME
               CALL  "ORCDBMAIN"   USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF     MCP-RC  =  ZERO
                 MOVE    "tbl_sryacct"     TO   MCP-TABLE
                 MOVE    "key31"           TO   MCP-PATHNAME
                 PERFORM  SRYACCT-READ-SEC
                 IF    FLG-SRYACCT  =  ZERO
                   IF  ACCT-ZAIKAISU  NOT =  ZERO
                     INITIALIZE                 HKNCOMBI-REC
                     MOVE  ACCT-HOSPNUM    TO   COMB-HOSPNUM
                     MOVE  ACCT-PTID       TO   COMB-PTID
                     MOVE  ACCT-HKNCOMBI   TO   COMB-HKNCOMBINUM
                     MOVE  HKNCOMBI-REC    TO   MCPDATA-REC
                     MOVE  "DBSELECT"      TO   MCP-FUNC
                     MOVE  "tbl_hkncombi"  TO   MCP-TABLE
                     MOVE  "key"           TO   MCP-PATHNAME
                     CALL  "ORCDBMAIN"  USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                     IF      MCP-RC  =  ZERO
                       MOVE    "tbl_hkncombi"   TO   MCP-TABLE
                       MOVE    "key"            TO   MCP-PATHNAME
                       PERFORM  HKNCOMBI-READ-SEC
                       IF    FLG-HKNCOMBI  =  ZERO
                         IF  COMB-KOH1HKNNUM  =  "970"
                           MOVE     1           TO   FLG-ZAIARI
                         END-IF
                       END-IF
                     END-IF
                     MOVE    "tbl_hkncombi"     TO   MCP-TABLE
                     MOVE    "key"              TO   MCP-PATHNAME
                     PERFORM  CSR-CLOSE-SEC
                   END-IF
                 END-IF
               END-IF
               MOVE    "tbl_sryacct"            TO   MCP-TABLE
               MOVE    "key31"                  TO   MCP-PATHNAME
               PERFORM  CSR-CLOSE-SEC
      *
               MOVE    "tbl_sryact"             TO   MCP-TABLE
               MOVE    "key7"                   TO   MCP-PATHNAME
               PERFORM  SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_sryact"             TO   MCP-TABLE
           MOVE        "key7"                   TO   MCP-PATHNAME
           PERFORM      CSR-CLOSE-SEC
      *
           IF   FLG-ZAIARI  =  1
             EVALUATE  IDX
               WHEN     1
                 MOVE     1       TO   FLG-ZAIARI1
               WHEN     2
                 MOVE     1       TO   FLG-ZAIARI2
             END-EVALUATE
           END-IF
      *
           .
      *
       SRYACT-KENSAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥Ç¡¼¥¿¤òÀÁµáÇ¯·î¡¢Í¥ÀèÈÖ¹æ½ç¤ËÊÂ¤ÓÂØ¤¨                      *
      ******************************************************************
       NARABIKAE-SYORI-SEC        SECTION.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              COMPUTE    IDZ  =  IDX  +  1
      *
              PERFORM    VARYING   IDY   FROM   IDZ   BY   1
                              UNTIL  (IDY            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDY) =  SPACE)
      *
                IF       WRK-HANBETU-KEY(IDX)   >
                                                 WRK-HANBETU-KEY(IDY) 
                   MOVE  WRK-HANBETU-TBL(IDX)  TO  WRK-HANBETU-DATA
                   MOVE  WRK-HANBETU-TBL(IDY)  TO  WRK-HANBETU-TBL(IDX)
                   MOVE  WRK-HANBETU-DATA      TO  WRK-HANBETU-TBL(IDY)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM.
      *
       NARABIKAE-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    Í¥ÀèÇ¯·îÆüÊÑ¹¹½èÍý                                          *
      ******************************************************************
       YUSENYMD-HENKOU-SYORI-SEC        SECTION.
      *
           MOVE       SPACE     TO    WRK-HKNJANUM
           MOVE       SPACE     TO    WRK-YUSENYMD
           MOVE       ZERO      TO    FLG-TAISYO
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                       UNTIL   (IDX                    >   10  )
                        OR     (WRK-HANBETU-SKYYM(IDX) =  SPACE)
             IF   WRK-HANBETU-HKNJANUM(IDX)  NOT =  SPACE
               IF ( WRK-HKNJANUM      =  SPACE                    )
               OR ((WRK-HKNJANUM  NOT =  SPACE                   ) AND
                   (WRK-HKNJANUM  NOT = WRK-HANBETU-HKNJANUM(IDX)))
                 MOVE   WRK-HANBETU-HKNJANUM(IDX)
                                        TO   WRK-HKNJANUM
                 MOVE   WRK-HANBETU-YUSENYMD(IDX)
                                        TO   WRK-YUSENYMD
               ELSE
                 MOVE   WRK-YUSENYMD    TO   WRK-HANBETU-YUSENYMD(IDX)
                 MOVE   1               TO   FLG-TAISYO
               END-IF
             END-IF
           END-PERFORM
      *
           IF   FLG-TAISYO   =   1
              PERFORM     NARABIKAE-SYORI-SEC
           END-IF
      *
           .
      *
       YUSENYMD-HENKOU-SYORI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¥ï¡¼¥¯¼ýÇ¼¥ì¥³¡¼¥É¥»¥Ã¥È½èÍý                                *
      ******************************************************************
       SYUNOU-SET-SEC         SECTION.
      *
           MOVE       ZERO      TO     FLG-KUMIAWASE-HANTEI.
      *
           PERFORM    VARYING   IDX    FROM   1   BY   1
                              UNTIL  (IDX            >    10)
                                OR   (WRK-HANBETU-SKYYM(IDX) =  SPACE)
      *
              MOVE LNK-S02-HOSPNUM     TO LNK-SYU-HOSPNUM(IDX)
              MOVE LNK-S02-NYUGAIKBN  TO LNK-SYU-NYUGAIKBN(IDX)
              MOVE LNK-S02-PTID       TO LNK-SYU-PTID(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SRYYMD(IDX)(1:6)
              IF   WRK-HANBETU-KAISHI(IDX)  NOT = SPACE
                MOVE WRK-HANBETU-KAISHI(IDX)
                                      TO LNK-SYU-SRYYMD(IDX)(7:2)
              ELSE
                MOVE WRK-HANBETU-KAISHI2(IDX)
                                      TO LNK-SYU-SRYYMD(IDX)(7:2)
              END-IF
              EVALUATE    LNK-S02-SYORI-KBN
                   WHEN    " "
                   WHEN    "1"
                   WHEN    "8"
      ************>    MOVE "2"       TO LNK-SYU-DENPJTIKBN(IDX)
                       CONTINUE
                   WHEN    "2"
                   WHEN    "9"
                       MOVE "2"       TO LNK-SYU-DENPJTIKBN(IDX)
              END-EVALUATE
      *       MOVE WRK-SYSYMD         TO LNK-SYU-DENPPRTYMD(IDX)
              MOVE LNK-S02-SYSYMD     TO LNK-SYU-DENPPRTYMD(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SKYSTYMD(IDX)(1:6)
              MOVE WRK-HANBETU-KAISHI2(IDX)
                                      TO LNK-SYU-SKYSTYMD(IDX)(7:2)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SYU-SKYEDYMD(IDX)(1:6)
              MOVE WRK-HANBETU-SYURYO(IDX)
                                      TO LNK-SYU-SKYEDYMD(IDX)(7:2)
              MOVE WRK-HANBETU-HKNCOMBINUM(IDX)
                                      TO LNK-SYU-HKNCOMBINUM(IDX)
              MOVE WRK-HANBETU-SYUHKNNUM(IDX)
                                      TO LNK-SYU-SYUHKNNUM(IDX)
              MOVE WRK-HANBETU-KOH1HKNNUM(IDX)
                                      TO LNK-SYU-KOH1HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH2HKNNUM(IDX)
                                      TO LNK-SYU-KOH2HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH3HKNNUM(IDX)
                                      TO LNK-SYU-KOH3HKNNUM(IDX)
              MOVE WRK-HANBETU-KOH4HKNNUM(IDX)
                                      TO LNK-SYU-KOH4HKNNUM(IDX)
              MOVE WRK-HANBETU-PTFTNRATE(IDX)
                                      TO LNK-SYU-PTFTNRATE(IDX)
              PERFORM    VARYING   IDY    FROM   1   BY   1
                             UNTIL  IDY            >    31
                   MOVE  WRK-HANBETU-DAY(IDX IDY) 
                                      TO LNK-SYU-DAY(IDX IDY)
      *
                   IF    WRK-HANBETU-DAY(IDX IDY)  NOT = SPACE
                     MOVE WRK-HANBETU-SKYYM(IDX) TO WRK-HANTEI-YMD(1:6)
                     MOVE IDY                    TO WRK-HANTEI-YMD(7:2)
                     IF  WRK-HANBETU-TEKSTYMD(IDX) <= WRK-HANTEI-YMD
                     AND WRK-HANBETU-TEKEDYMD(IDX) >= WRK-HANTEI-YMD
                         CONTINUE
                     ELSE
                         MOVE      1     TO     FLG-KUMIAWASE-HANTEI
                     END-IF
                   END-IF
              END-PERFORM
      *
      *       ¥ï¡¼¥¯ÆüÊÌ¼ýÇ¼¥ì¥³¡¼¥É¥»¥Ã¥È½èÍý                         
      *
              MOVE LNK-S02-HOSPNUM     TO LNK-SDAY-HOSPNUM(IDX)
              MOVE LNK-S02-NYUGAIKBN  TO LNK-SDAY-NYUGAIKBN(IDX)
              MOVE LNK-S02-PTID       TO LNK-SDAY-PTID(IDX)
              MOVE WRK-HANBETU-SKYYM(IDX)
                                      TO LNK-SDAY-SKYYM(IDX)
      *
      ********
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  MOVE    1     TO    IDX7
              ELSE
                  MOVE    2     TO    IDX7
              END-IF
      *
              PERFORM    VARYING   IDY    FROM   1   BY   1
                          UNTIL    IDY     >    31
      *
                IF   WRK-HANBETU-DAY-NACCT(IDX IDY) NOT =  SPACE
                   MOVE  WRK-NYUINDAY-TAIHI-KBN(IDX7 IDY)
                                   TO  WRK-NYUINDAY-KBN(IDX IDY)
                END-IF
                MOVE  WRK-NYUINDAY-TAIHI-KBN(IDX7 IDY)
                                   TO  WRK-NYUINDAY2-KBN(IDX IDY)
      *
                MOVE  WRK-NYUINDAY-TAIHI-KYUKYU-KBN(IDX7 IDY)
                             TO  WRK-NYUINDAY-KYUKYU-KBN(IDX IDY)
      *
                MOVE  WRK-NYUINDAY-TAIHI-TANKI-KBN(IDX7 IDY)
                             TO  WRK-NYUINDAY-TANKI-KBN(IDX IDY)
      *
                IF    WRK-NYUINDAY-TAIHI-KYUKYU-KBN(IDX7 IDY) =  "1"
                  IF   IDX7  =   1
                    IF   (WRK-DAY1(1 IDY)  NOT =  ZERO)
                    OR   (WRK-DAY6(1 IDY)  NOT =  ZERO)
                        MOVE   1   TO  WRK-KYUKYUHANSO-ERR
                    END-IF
                  END-IF
                  IF   IDX7  =   2
                    IF   (WRK-DAY1(2 IDY)  NOT =  ZERO)
                    OR   (WRK-DAY6(2 IDY)  NOT =  ZERO)
                        MOVE   1   TO  WRK-KYUKYUHANSO-ERR
                    END-IF
                  END-IF
                END-IF
      *
                MOVE      ZERO                   TO   IDX-ST
                IF  LNK-S02-SKYSTYMD(1:6) = LNK-SYU-SRYYMD(IDX)(1:6)
                  MOVE    LNK-S02-SKYSTYMD(7:2)  TO   IDX-ST
                END-IF
                MOVE      ZERO                   TO   IDX-ED
                IF  LNK-S02-SKYEDYMD(1:6) = LNK-SYU-SRYYMD(IDX)(1:6)
                  MOVE    LNK-S02-SKYEDYMD(7:2)  TO   IDX-ED
                END-IF
      *
                MOVE  LNK-SYU-SKYSTYMD(IDX)(7:2) TO  WRK-CHECK-STDAY
                MOVE  LNK-SYU-SKYEDYMD(IDX)(7:2) TO  WRK-CHECK-EDDAY
                IF   (WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX7 IDY)
                                                          NOT = ZERO)
                AND  (WRK-CHECK-STDAY       <=   IDY                )
                AND  (IDY                   <=   WRK-CHECK-EDDAY    )
                  IF  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX7 IDY)
                                          =  LNK-SYU-HKNCOMBINUM(IDX)
                     IF  LNK-SYU-HKNCOMBINUM(IDX)
                                              =  WRK-DAY5(IDX7 IDY)
                       EVALUATE  LNK-S02-TEIKI-KBN
                         WHEN      "X"
                           CONTINUE
                         WHEN     OTHER
                           IF   ((IDX-ST      = IDY             ) AND
                                 (IDX-ED  NOT = IDY             ))
                           OR   ((LNK-S02-TEIKI-KBN = "A" OR "Y") AND
                                 (IDX-ST      = IDY             ) AND
                                 (IDX-ED      = IDY             ))
                             MOVE     9   TO  WRK-DOJITU-FLG (IDX IDY)
                             MOVE    "9"  TO  LNK-SYU-DAY(IDX IDY)
                           ELSE
                             IF   IDX-ED  =  IDY
                               MOVE   1   TO  WRK-DOJITU-FLG (IDX IDY)
                             END-IF
                           END-IF
                       END-EVALUATE
                     ELSE
                       EVALUATE  LNK-S02-TEIKI-KBN
                         WHEN      "X"
                           IF    LNK-SYU-DAY(IDX IDY) NOT = "2"
                             MOVE     1   TO  WRK-JNISSU-ERR
                           END-IF
                           MOVE       9   TO  WRK-DOJITU-FLG (IDX IDY)
                           MOVE      "9"  TO  LNK-SYU-DAY(IDX IDY)
                           MOVE       1   TO  WRK-DOJITU-FLG2(IDX IDY)
                         WHEN     OTHER
                           IF   ((IDX-ST      = IDY             ) AND
                                 (IDX-ED  NOT = IDY             ))
                           OR   ((LNK-S02-TEIKI-KBN = "A" OR "Y") AND
                                 (IDX-ST      = IDY             ) AND
                                 (IDX-ED      = IDY             ))
                           OR   ((LNK-S02-TEIKI-KBN = "Y"       ) AND
                                 (IDX-ST  NOT = IDY             ) AND
                                 (IDX-ED  NOT = IDY             ))
                             IF  LNK-SYU-DAY(IDX IDY) NOT = "2"
                               MOVE   1   TO  WRK-JNISSU-ERR
                             END-IF
                             MOVE     9   TO  WRK-DOJITU-FLG (IDX IDY)
                             MOVE    "9"  TO  LNK-SYU-DAY(IDX IDY)
                             IF  (LNK-S02-TEIKI-KBN = "Y")
                             AND (IDX-ST  NOT = IDY      )
                             AND (IDX-ED  NOT = IDY      )
                                MOVE  1   TO  WRK-DOJITU-FLG2(IDX IDY)
                             END-IF
                           ELSE
                             IF   IDX-ED  =  IDY
                               IF  LNK-SYU-DAY(IDX IDY) = "9"
                                 MOVE  SPACE
                                          TO  LNK-SYU-DAY(IDX IDY)
                               END-IF
                               IF  LNK-SYU-DAY(IDX IDY) = "2"
                                 MOVE 7   TO  WRK-DOJITU-FLG (IDX IDY)
                               END-IF
                               MOVE   1   TO  WRK-TAISYOGAI-HANTEI(IDX)
                             END-IF
                           END-IF
                       END-EVALUATE
                     END-IF
                  ELSE
                     IF  LNK-SYU-HKNCOMBINUM(IDX)
                                              =  WRK-DAY5(IDX7 IDY)
                       EVALUATE  LNK-S02-TEIKI-KBN
                         WHEN      "X"
                           MOVE       1   TO  WRK-DOJITU-FLG (IDX IDY)
                           MOVE       1   TO  WRK-DOJITU-FLG2(IDX IDY)
                         WHEN     OTHER
                          IF     (LNK-S02-TEIKI-KBN = "A" OR "Y")
                          AND    (IDX-ST      = IDY             )
                          AND    (IDX-ED      = IDY             )
                               MOVE   8   TO  WRK-DOJITU-FLG (IDX IDY)
                               MOVE  "8"  TO  LNK-SYU-DAY(IDX IDY)
                               MOVE   1   TO  WRK-TAISYOGAI-HANTEI(IDX)
                          ELSE
                           IF   ( IDX-ED      = IDY       )
                           OR   ((LNK-S02-TEIKI-KBN = "Y") AND
                                 (IDX-ST  NOT = IDY      ) AND
                                 (IDX-ED  NOT = IDY      ))
                             MOVE     1   TO  WRK-DOJITU-FLG (IDX IDY)
                             IF  (LNK-S02-TEIKI-KBN = "Y")
                             AND (IDX-ST  NOT = IDY      )
                             AND (IDX-ED  NOT = IDY      )
                                MOVE  1   TO  WRK-DOJITU-FLG2(IDX IDY)
                             END-IF
                           ELSE
                             IF  (IDX-ST      = IDY)
                             AND (IDX-ED  NOT = IDY)
                               MOVE   8   TO  WRK-DOJITU-FLG (IDX IDY)
                               MOVE  "8"  TO  LNK-SYU-DAY(IDX IDY)
                               MOVE   1   TO  WRK-TAISYOGAI-HANTEI(IDX)
                             END-IF
                           END-IF
                          END-IF
                       END-EVALUATE
                     ELSE
                       EVALUATE  LNK-S02-TEIKI-KBN
                         WHEN      "X"
                           CONTINUE
                         WHEN     OTHER
                           IF  LNK-SYU-DAY(IDX IDY) =  "2"
                             IF   ((IDX-ST      = IDY             ) AND
                                   (IDX-ED  NOT = IDY             ))
                             OR   ((LNK-S02-TEIKI-KBN = "A" OR "Y") AND
                                   (IDX-ST      = IDY             ) AND
                                   (IDX-ED      = IDY             ))
                               MOVE   8   TO  WRK-DOJITU-FLG (IDX IDY)
                               MOVE  "8"  TO  LNK-SYU-DAY(IDX IDY)
                             ELSE
                               IF   IDX-ED  =  IDY
                                 MOVE 7   TO  WRK-DOJITU-FLG (IDX IDY)
                               END-IF
                             END-IF
                             MOVE     1   TO  WRK-TAISYOGAI-HANTEI(IDX)
                           END-IF
                       END-EVALUATE
                     END-IF
                  END-IF
                END-IF
      *
                MOVE  WRK-NYUINDAY-TAIHI-DOU-SKJKBN(IDX7 IDY)
                              TO   WRK-DOJITUSKJST-FLG(IDX IDY)
                EVALUATE  WRK-NYUINDAY-TAIHI-DOU-SKJKBN(IDX7 IDY)
                  WHEN     "0"
                    MOVE "3"  TO   WRK-DOJITUSKJED-FLG(IDX IDY)
                  WHEN     "1"
                    MOVE "0"  TO   WRK-DOJITUSKJED-FLG(IDX IDY)
                  WHEN     "2"
                    MOVE "1"  TO   WRK-DOJITUSKJED-FLG(IDX IDY)
                  WHEN     "3"
                    MOVE "2"  TO   WRK-DOJITUSKJED-FLG(IDX IDY)
                END-EVALUATE
      *
                MOVE  WRK-SKJHKN-TAIHI-HKNCOMBI(IDX7 IDY)
                          TO   WRK-SKJHKN-HKNCOMBI(IDX IDY)
                MOVE  WRK-SKJHKN-TAIHI-HKNCOMBI-DOU(IDX7 IDY)
                          TO   WRK-SKJHKN-HKNCOMBI-DOU(IDX IDY)
      *
                IF    WRK-DAY5X(IDX7 IDY)  NOT = ZERO
                  MOVE  WRK-DAY5X(IDX7 IDY)
                          TO   WRK-SKJHKN-HKNCOMBI-XXX(IDX IDY)
                END-IF
                IF    WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX7 IDY)
                                                           NOT = ZERO
                  MOVE  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(IDX7 IDY)
                          TO   WRK-SKJHKN-HKNCOMBI-DOU-XXX(IDX IDY)
                END-IF
      *
                MOVE  WRK-KANGOGEN-TAIHI-SRYCD(IDX7 IDY)
                          TO   WRK-KANGOGEN-SRYCD(IDX IDY)
      *
              END-PERFORM
      *
      ********
      *
              PERFORM  KOREI-TANYUIN-HANTEI-SEC
      *
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                PERFORM   VARYING   IDY   FROM  1  BY   1
                           UNTIL    IDY    >   31
                  IF  ((WRK-DAY1X(1 IDY)        NOT =  ZERO) OR
                       (WRK-NYUINDAY2-KBN(IDX IDY)  =   "1"))
                  AND ( WRK-DAY5X(1 IDY)        NOT =  ZERO )
                    MOVE  "1"  TO  WRK-NYUSANTEI-KBN(IDX IDY)
                  END-IF
                END-PERFORM
              END-IF
              IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                PERFORM   VARYING   IDY   FROM  1  BY   1
                           UNTIL    IDY    >   31
                  IF  ((WRK-DAY1X(2 IDY)        NOT =  ZERO) OR
                       (WRK-NYUINDAY2-KBN(IDX IDY)  =   "1"))
                  AND ( WRK-DAY5X(2 IDY)        NOT =  ZERO )
                    MOVE  "1"  TO  WRK-NYUSANTEI-KBN(IDX IDY)
                  END-IF
                END-PERFORM
              END-IF
      *
           END-PERFORM. 
      *
       SYUNOU-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¹âÎð¼ÔÂ¾ÊÝ¸±Æþ±¡È½Äê½èÍý
      ******************************************************************
       KOREI-TANYUIN-HANTEI-SEC         SECTION.
      *
           MOVE    ZERO     TO   FLG-KOREI-TANYUIN
                                 FLG-KOREI-TANYUIN2
           IF  (LNK-SYU-SYUHKNNUM(IDX) =
                "971" OR "973" OR "975" OR "068" OR "069" OR "040")
           OR  (LNK-SYU-KOH1HKNNUM(IDX)     = "970"               )
           OR  (LNK-SYU-SYUHKNNUM(IDX)(1:2) = "98" OR "90" OR "91")
             CONTINUE
           ELSE
             PERFORM  VARYING  IDY  FROM   1   BY   1
                       UNTIL   IDY   >    31
                EVALUATE  LNK-SYU-DAY(IDX IDY)
                  WHEN     SPACE
                     CONTINUE
                  WHEN      "2"
                  WHEN      "8"
                     MOVE    1     TO   FLG-KOREI-TANYUIN
                  WHEN      "1"
                  WHEN      "3"
                  WHEN      "9"
                     MOVE   ZERO   TO   FLG-KOREI-TANYUIN
                     MOVE    31    TO   IDY
                END-EVALUATE
             END-PERFORM
             IF  FLG-KOREI-TANYUIN  =  1
                IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(1)
                  PERFORM   VARYING   IDY   FROM  1  BY   1
                             UNTIL    IDY    >   31
                    IF   WRK-DAY1X(1 IDY)   NOT =  ZERO
                    AND  WRK-DAY5X(1 IDY)   NOT =  ZERO
                      MOVE  WRK-DAY5X(1 IDY)  TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                        MOVE    1     TO   FLG-KOREI-TANYUIN2
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                    END-IF
                    IF   WRK-DAY1X(1 IDY)       =  ZERO
                    AND  WRK-DAY5X(1 IDY)   NOT =  ZERO
                      MOVE  WRK-DAY5X(1 IDY)  TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                       IF   WRK-NYUINDAY2-KBN(IDX IDY)  =   "1"
                        MOVE    1     TO   FLG-KOREI-TANYUIN2
                       END-IF
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                    END-IF
                  END-PERFORM
                  IF    FLG-KOREI-TANYUIN2  =  1
                    PERFORM   VARYING   IDY   FROM  1  BY   1
                               UNTIL    IDY    >   31
                     IF  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(1 IDY)
                                                            NOT = ZERO
                      MOVE WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(1 IDY)
                                              TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                        CONTINUE
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                     END-IF
                    END-PERFORM
                  END-IF
                  IF  FLG-KOREI-TANYUIN2  =  1
                      MOVE   "1"    TO   WRK-KOREI-TANYUINFLG(IDX)
                  END-IF
                END-IF
                IF   WRK-HANBETU-SKYYM(IDX)  =   WRK-SKYYM(2)
                  PERFORM   VARYING   IDY   FROM  1  BY   1
                             UNTIL    IDY    >   31
                    IF   WRK-DAY1X(2 IDY)   NOT =  ZERO
                    AND  WRK-DAY5X(2 IDY)   NOT =  ZERO
                      MOVE  WRK-DAY5X(2 IDY)  TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                        MOVE    1     TO   FLG-KOREI-TANYUIN2
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                    END-IF
                    IF   WRK-DAY1X(2 IDY)       =  ZERO
                    AND  WRK-DAY5X(2 IDY)   NOT =  ZERO
                      MOVE  WRK-DAY5X(2 IDY)  TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                       IF   WRK-NYUINDAY2-KBN(IDX IDY)  =   "1"
                        MOVE    1     TO   FLG-KOREI-TANYUIN2
                       END-IF
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                    END-IF
                  END-PERFORM
                  IF    FLG-KOREI-TANYUIN2  =  1
                    PERFORM   VARYING   IDY   FROM  1  BY   1
                               UNTIL    IDY    >   31
                     IF  WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(2 IDY)
                                                            NOT = ZERO
                      MOVE WRK-NYUINDAY-TAIHI-DOU-HKNCOMBI(2 IDY)
                                              TO  WRK-HKNCOMBINUM
                      PERFORM  HKNCOMBI-KENSAKU-SEC
                      IF   COMB-HKNNUM  =  "971" OR "973" OR "975"
                        CONTINUE
                      ELSE
                        MOVE   ZERO   TO   FLG-KOREI-TANYUIN2
                        MOVE    31    TO   IDY
                      END-IF
                     END-IF
                    END-PERFORM
                  END-IF
                  IF  FLG-KOREI-TANYUIN2  =  1
                      MOVE   "1"    TO   WRK-KOREI-TANYUINFLG(IDX)
                  END-IF
                END-IF
             END-IF
           END-IF
      *
           .
      *
       KOREI-TANYUIN-HANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»¸¡º÷
      ******************************************************************
       HKNCOMBI-KENSAKU-SEC         SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE  LNK-S02-HOSPNUM   TO  COMB-HOSPNUM
           MOVE  LNK-S02-PTID      TO  COMB-PTID
           MOVE  WRK-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC      TO  MCPDATA-REC
           MOVE  "DBSELECT"        TO  MCP-FUNC
           MOVE  "tbl_hkncombi"    TO  MCP-TABLE
           MOVE  "key"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"  USING    MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF     MCP-RC  =  ZERO
             MOVE  "tbl_hkncombi"  TO  MCP-TABLE
             MOVE  "key"           TO  MCP-PATHNAME
             PERFORM  HKNCOMBI-READ-SEC
           END-IF
           MOVE  "tbl_hkncombi"    TO  MCP-TABLE
           MOVE  "key"             TO  MCP-PATHNAME
           PERFORM  CSR-CLOSE-SEC
      *
           .
      *
       HKNCOMBI-KENSAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *    Æþ±¡²ñ·×¥»¥Ã¥È½èÍý                                *
      ******************************************************************
       NYUIN-KAIKEI-SET-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
           PERFORM      SYSKANRI-TAXKBN-GET-SEC
      *
           PERFORM      GAIHAKU-HENSYU-SEC
      *
      *ÁªÄêÎÅÍÜ¾ðÊó¤Î¼èÆÀ
           MOVE     ZERO               TO  FLG-SENTEI-SET
           INITIALIZE                      TABLE-SENTEI
      *
           MOVE     ZERO               TO  WRK-SENTEI-GAIHAKU-KEISU
      *
           IF   SYU-SYUHKNNUM    =    "975"
              CONTINUE
           ELSE
              MOVE    ZERO                TO  FLG-NYUINACCT
              INITIALIZE                      NYUINACCT-REC
              MOVE    SYU-HOSPNUM         TO  NACCT-HOSPNUM
              MOVE    SYU-PTID            TO  NACCT-PTID
              MOVE    SYU-SRYYMD (1:6)    TO  NACCT-SRYYM
              MOVE    NYUINACCT-REC       TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
              MOVE    "key22"             TO  MCP-PATHNAME
grpsys        CALL "ORCDBMAIN" USING MCPAREA
                                     MCPDATA-REC
grpsys                               SPA-AREA
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                  MOVE    "key22"         TO  MCP-PATHNAME
                  PERFORM  NYUINACCT-READ-SEC
              ELSE
                  MOVE    1               TO  FLG-NYUINACCT
              END-IF
      *
              PERFORM      SENTEI-SET-SEC
                  UNTIL   (FLG-NYUINACCT   =       1      )   OR
                          (FLG-SENTEI-SET  =       1      )
      *
              MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
              MOVE    "key22"             TO  MCP-PATHNAME
              PERFORM   CSR-CLOSE-SEC
      *
      *       ÁªÄê³°Çñ¤Î·¸¿ô¥»¥Ã¥È
              PERFORM   SENTEI-GAIHAKU-KEISU-SET-SEC
           END-IF
      *
      *Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ
           MOVE    ZERO                TO  NYUIN-COUNT
           INITIALIZE                      TABLE-NYUIN
           INITIALIZE                      TABLE-RMSAGAKU
           MOVE    SPACE               TO  HKNTEKGAI-NYUIN
           INITIALIZE                      HKNTEKGAI-NYUIN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)    TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE     1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM      NYUINACCT-GET-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           COMPUTE  NYUIN-COUNT = NYUIN-COUNT  -  WRK-GAIHAKU-COUNT
      *
      *°åÎÅµ¡´Ø¸º»»¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¥Á¥§¥Ã¥¯
           PERFORM   SYS-5000-CHK-SEC
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
             MOVE    ZERO                TO  FLG-SYSKANRI
             INITIALIZE                      SYSKANRI-REC
             MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                             SYS-EDYUKYMD
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       CALL "ORCDBMAIN" USING MCPAREA
                                    MCPDATA-REC
grpsys                              SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                COMPUTE SYU-HKNTEN(15) = SYU-HKNTEN(15)
                                                    - (5 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            5                          
                                 END-IF                                
                                END-PERFORM                            
                            WHEN  3
                                COMPUTE SYU-HKNTEN(15) = SYU-HKNTEN(15)
                                                   - (10 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            10                         
                                 END-IF                                
                                END-PERFORM                            
                            WHEN  4
                                COMPUTE SYU-HKNTEN(15) = SYU-HKNTEN(15)
                                                    - (5 * NYUIN-COUNT)
                                PERFORM VARYING SYU-IDX FROM 1 BY 1    
                                         UNTIL    SYU-IDX  >   31      
                                 IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                                    (TBL-GAIHAKU(SYU-IDX)   = ZERO)    
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) -     
                                            5                          
                                 END-IF                                
                                END-PERFORM                            
                        END-EVALUATE
                    END-IF
               END-PERFORM
             END-IF
             MOVE      "tbl_syskanri"      TO  MCP-TABLE
             MOVE      "key10"             TO  MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
           ELSE
               PERFORM  SYS-5000-GET-SEC
      *
               MOVE     ZERO   TO  WRK-INNAI-KANSEN-GENTEN
               MOVE     ZERO   TO  WRK-IRYOU-ANZEN-GENTEN
               MOVE     ZERO   TO  WRK-JOKUSOU-GENTEN
      *
               PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                        UNTIL    SYU-IDX  >   31
                   IF (TBL-NYUIN(SYU-IDX) NOT = ZERO) AND
                      (TBL-GAIHAKU(SYU-IDX)   = ZERO)
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-INNAI-KANSEN(SYU-IDX)
                       IF   TBL-INNAI-KANSEN(SYU-IDX)  NOT = ZERO
                            ADD   5     TO   WRK-INNAI-KANSEN-GENTEN
                       END-IF
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-IRYOU-ANZEN(SYU-IDX)
                       IF   TBL-IRYOU-ANZEN(SYU-IDX)  NOT = ZERO
                            ADD   10    TO   WRK-IRYOU-ANZEN-GENTEN
                       END-IF
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =
                               SDAY-HKNTEN(SYU-IDX) -
                               TBL-JOKUSOU(SYU-IDX)
                       IF   TBL-JOKUSOU(SYU-IDX)  NOT = ZERO
                            ADD   5     TO   WRK-INNAI-KANSEN-GENTEN
                       END-IF
                   END-IF
               END-PERFORM
               COMPUTE SYU-HKNTEN(15)  =   SYU-HKNTEN(15)
                                       -   WRK-INNAI-KANSEN-GENTEN
               COMPUTE SYU-HKNTEN(15)  =   SYU-HKNTEN(15)
                                       -   WRK-IRYOU-ANZEN-GENTEN
               COMPUTE SYU-HKNTEN(15)  =   SYU-HKNTEN(15)
                                       -   WRK-JOKUSOU-GENTEN
      *
           END-IF
      *
      *    Æþ±¡ÊÝ¸±Å¬ÍÑ³°
           ADD    NYUIN-TGMONEY        TO  SYU-TGMONEY(15)
           ADD    NYUIN-TGMONEY-TAX    TO  SYU-TGMONEY-TAX(15)
           ADD    NYUIN-TGMONEY        TO  SYU-TGMONEY(17)
           ADD    NYUIN-TGMONEY-TAX    TO  SYU-TGMONEY-TAX(17)
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       NYUIN-KAIKEI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    Æþ±¡²ñ·×¥»¥Ã¥È½èÍý¡ÊÏ«ºÒ¡¦¼«ÇåÀÕÍÑ¡Ë                        *
      ******************************************************************
       NYUIN-KAIKEI-SET2-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
           PERFORM      SYSKANRI-TAXKBN-GET-SEC
      *
           PERFORM      GAIHAKU-HENSYU-SEC
      *
      *    Æþ±¡Ï«ºÒ²ñ·×ºîÀ®¥µ¥Ö¤è¤êÆþ±¡ÎÁ¡¦³°Çñ¤ò¼èÆÀ
      *
           PERFORM      ROSAI-JIBAI-NYUINRYO-SEC
      *
      *Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ
      *
           INITIALIZE                      TABLE-RMSAGAKU
           MOVE    SPACE               TO  HKNTEKGAI-NYUIN
           INITIALIZE                      HKNTEKGAI-NYUIN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)  TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE     1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM      NYUINACCT-GET2-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )
      *
           MOVE      "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE      "key9"            TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       NYUIN-KAIKEI-SET2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬¤Î¼è¤ê¹þ¤ß
      **************************************************************
       SYSKANRI-TAXKBN-GET-SEC           SECTION.
      *
           MOVE    SPACE                TO    WRK-TAX1001-KBN
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬¤Î¼è¤ê¹þ¤ß
           MOVE    ZERO                 TO    FLG-SYSKANRI
           INITIALIZE                         SYSKANRI-REC
           MOVE    LNK-S02-HOSPNUM      TO    SYS-HOSPNUM
           MOVE    "1001"               TO    SYS-KANRICD
           MOVE    "*"                  TO    SYS-KBNCD
           MOVE    SYU-SRYYMD           TO    SYS-STYUKYMD
                                              SYS-EDYUKYMD
           MOVE    SYSKANRI-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE    "tbl_syskanri"   TO    MCP-TABLE
               MOVE    "key10"          TO    MCP-PATHNAME
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-TAX-HASU-KBN
                                        TO    WRK-TAX1001-KBN
               IF   WRK-TAX1001-KBN = SPACE
                  MOVE   "2"            TO    WRK-TAX1001-KBN
               END-IF
           END-IF
      *
           MOVE      "tbl_syskanri"     TO    MCP-TABLE
           MOVE      "key10"            TO    MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
      *    ¾ÃÈñÀÇ·×»»¤Î·¸¿ôÀßÄê
           IF    SYU-SRYYMD(1:6) >= "201910"
               MOVE      10      TO  WRK-S1
               MOVE     110      TO  WRK-S2
               MOVE     0.10     TO  WRK-S4
               MOVE     1.10     TO  WRK-S5
           ELSE
             IF  SYU-SRYYMD(1:6) >= "201404"
               MOVE       8      TO  WRK-S1
               MOVE     108      TO  WRK-S2
               MOVE     0.08     TO  WRK-S4
               MOVE     1.08     TO  WRK-S5
             ELSE
               MOVE       5      TO  WRK-S1
               MOVE     105      TO  WRK-S2
               MOVE     0.05     TO  WRK-S4
               MOVE     1.05     TO  WRK-S5
             END-IF
           END-IF
           EVALUATE  WRK-TAX1001-KBN
               WHEN      "1"
                 MOVE   0.50000  TO  WRK-S3
               WHEN      "2"
                 MOVE   0.00000  TO  WRK-S3
               WHEN      "3"
                 MOVE   0.99999  TO  WRK-S3
           END-EVALUATE
      *
           .
       SYSKANRI-TAXKBN-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ³°ÇñÊÔ½¸½èÍý
      ******************************************************************
       GAIHAKU-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYU-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SYU-PTID            TO  NACCT-PTID
           MOVE    SYU-SRYYMD (1:6)    TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key26"         TO  MCP-PATHNAME
               PERFORM  NYUINACCT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE     ZERO               TO  FLG-GAIHAKU-SET
           INITIALIZE                      TABLE-GAIHAKU
           MOVE     ZERO               TO  WRK-GAIHAKU-COUNT
      *
           PERFORM      GAIHAKU-SET-SEC
               UNTIL   (FLG-NYUINACCT   =       1      )   OR
                       (FLG-GAIHAKU-SET =       1      )
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       GAIHAKU-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ¾ðÊó¤Î¼è¹þ¤ß
      **************************************************************
       GAIHAKU-SET-SEC           SECTION.
      *
           IF  NACCT-SRYYM      =   SYU-SRYYMD(1:6)
               PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                         UNTIL    SYU-IDX  >   31
                   IF   NACCT-DAY(SYU-IDX) NOT =  ZERO
                       MOVE   NACCT-DAY(SYU-IDX)
                                  TO  TBL-GAIHAKU (SYU-IDX)
                       IF  SYU-DAY(SYU-IDX)    =  "1"
                           ADD    1   TO  WRK-GAIHAKU-COUNT
                       END-IF
                   END-IF
               END-PERFORM
               MOVE     1         TO  FLG-GAIHAKU-SET 
           END-IF
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key26"            TO  MCP-PATHNAME
           PERFORM  NYUINACCT-READ-SEC
           .
      *
       GAIHAKU-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄêÎÅÍÜ¾ðÊó¤Î¼è¹þ¤ß
      **************************************************************
       SENTEI-SET-SEC           SECTION.
      *
           IF  NACCT-SRYYM      =   SYU-SRYYMD(1:6)
      *    ÁªÄêÎÅÍÜ¾ðÊó¤Î¥Æ¡¼¥Ö¥ë¥»¥Ã¥È
               PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                         UNTIL    SYU-IDX  >   31
                   IF  (SYU-DAY(SYU-IDX)       =   "1")    AND
                       (NACCT-DAY(SYU-IDX) NOT =  ZERO)
                       MOVE   NACCT-DAY(SYU-IDX)
                                  TO  TBL-SENTEI (SYU-IDX)
                   END-IF
               END-PERFORM
               MOVE     1         TO  FLG-SENTEI-SET 
           END-IF
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key22"            TO  MCP-PATHNAME
           PERFORM  NYUINACCT-READ-SEC
           .
      *
       SENTEI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄê³°Çñ»þ¤Î·¸¿ô¥»¥Ã¥È½èÍý
      **************************************************************
       SENTEI-GAIHAKU-KEISU-SET-SEC           SECTION.
      *
           IF  SYU-SRYYMD(1:6)    <=    "200303"
               MOVE   0.1           TO     WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           IF (SYU-SRYYMD(1:6)    >=    "200304")   AND
              (SYU-SRYYMD(1:6)    <=    "200403")
               MOVE   0.05           TO    WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           IF  SYU-SRYYMD(1:6)    >=    "200404"
               MOVE   ZERO          TO     WRK-SENTEI-GAIHAKU-KEISU
           END-IF
      *
           .
      *
       SENTEI-GAIHAKU-KEISU-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ
      **************************************************************
       NYUINACCT-GET-SEC           SECTION.
      *
           IF    (NACCT-ZAITEN          = ZERO           )
           AND   (NACCT-SRYYM(1:6)      = SYU-SRYYMD(1:6))
           AND   (NACCT-ZAISKBKBN       = "1"            )
           AND   (NACCT-SRYCDTOTAL  NOT = 99999916       )
               MOVE      1      TO     WRK-NYUINRYO-ERR
           END-IF
      *
           IF    (NACCT-ZAITEN    NOT = ZERO           ) AND
                 (NACCT-SRYYM(1:6)    = SYU-SRYYMD(1:6))
      *
      *        Æþ±¡´µ¼Ô¤ÎÏ·¿ÍÈ½Äê
               INITIALIZE                      ORCSAGECHKAREA
               MOVE    SYU-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SYU-PTID            TO  AGECHK-PTID
               MOVE    SYU-SRYYMD          TO  AGECHK-SRYYMD
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      ( AGECHK-RC    =    ZERO )
                   CONTINUE
               ELSE
                   INITIALIZE    ORCSAGECHKAREA
               END-IF
      *
      *        Æþ±¡ÎÁ¤Î½¸·×
               IF  (NACCT-ZAISKBKBN          =  "1" ) AND
                   (NACCT-ZAITEN         NOT =  ZERO)
      *            Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î¼èÆÀ
                   PERFORM   NYUINKHN-GET-SEC
      *            ÉÂÅïÀßÄê¤ÎÁªÄê¼«¸ÊÉéÃ´¾ðÊó¤Î¼èÆÀ
                   INITIALIZE                      ORCSBTUSENTEIAREA
                   MOVE    SYU-HOSPNUM         TO  SBTUSENTEI-HOSPNUM
                   MOVE    SYU-PTID            TO  SBTUSENTEI-PTID
                   MOVE    SYU-SRYYMD (1:6)    TO  SBTUSENTEI-SRYYM
                   CALL    "ORCSBTUSENTEI"     USING
                                               ORCSBTUSENTEIAREA
                                               SPA-AREA
      *            Æþ±¡ÎÁ¤È³°Çñ¤òÈæ³Ó¤·¤ÆÅÀ¿ô»»Äê
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  (SYU-DAY(SYU-IDX)        =  "1" )
                       AND (NACCT-DAY(SYU-IDX)  NOT =  ZERO)
                           IF   WRK-KANGOGEN-SRYCD(IDX SYU-IDX) = SPACE
                             MOVE  WRK-KHNTEN-MOTO  TO  WRK-KHNTEN
                             MOVE  WRK-KHN-ZAITEN-MOTO
                                             TO  WRK-KHN-ZAITEN
                             MOVE  WRK-SENTEI-KHNTEN-MOTO
                                             TO  WRK-SENTEI-KHNTEN
                             MOVE  WRK-SENTEI-ZAITEN-MOTO
                                             TO  WRK-SENTEI-ZAITEN
                           ELSE
                             MOVE  WRK-KHNTEN-GEN   TO  WRK-KHNTEN
                             MOVE  WRK-KHN-ZAITEN-GEN
                                             TO  WRK-KHN-ZAITEN
                             MOVE  WRK-SENTEI-KHNTEN-GEN
                                             TO  WRK-SENTEI-KHNTEN
                             MOVE  WRK-SENTEI-ZAITEN-GEN
                                             TO  WRK-SENTEI-ZAITEN
                           END-IF
                           ADD        1      TO    NYUIN-COUNT
                           MOVE   NACCT-DAY(SYU-IDX)                   
                                      TO  TBL-NYUIN(SYU-IDX)           
                           IF    TBL-SENTEI(SYU-IDX)   =   ZERO
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE WRK-KEISAN2     =
                                             WRK-KHN-ZAITEN  +  0.5
                                     ADD     WRK-KEISAN2
                                           TO  SYU-HKNTEN(15)
                                               SDAY-HKNTEN(SYU-IDX)    
      *                          ³°Çñ¡Ê£±£µ¡ó¡Ë
                                 WHEN   1
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                             ((WRK-KHNTEN    *  0.15)
                                                             +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                             ((WRK-KHNTEN    *  0.15)  
                                                             +  0.5)   
      *                          ³°Çñ¡Ê£³£°¡ó¡Ë
                                 WHEN   2
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                            ((WRK-KHNTEN      *  0.3)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *  0.3)  
                                                              +  0.5)  
      *                          Â¾°åÎÅµ¡´Ø¼õ¿Ç
                                 WHEN   4
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                   PERFORM   NYUKHNKSNKBN-CHECK-SEC
                                   EVALUATE  TBL-GAIHAKU(SYU-IDX)
                                    WHEN       4
                                     IF    SYU-SRYYMD(1:6) >= "201604"
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.6 )
                                     ELSE
                                       IF  SYU-SRYYMD(1:6) >= "200404"
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.3 )
                                       ELSE
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.15)
                                       END-IF
                                     END-IF
                                    WHEN       5
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.9 )
                                     ELSE
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.7 )
                                     END-IF
                                    WHEN       6
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.8 )
                                     ELSE
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.45)
                                     END-IF
                                    WHEN       7
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                        CONTINUE
                                     ELSE
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.85)
                                     END-IF
                                    WHEN      14
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.95)
                                    WHEN      15
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.65)
                                    WHEN      16
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.85)
                                   END-EVALUATE
                                   COMPUTE   WRK-TENSU-KEISAN2 =
                                             WRK-TENSU-KEISAN2 +
                                             WRK-KSNTEN
                                   IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                     COMPUTE WRK-TENSU-KEISAN  =
                                           ( WRK-TENSU-KEISAN2 -
                                            (WRK-TENSU-KEISAN2 *
                                            (WRK-HYOKETU-TEN   / 100)))
                                                               + 0.5
                                   ELSE
                                     COMPUTE WRK-TENSU-KEISAN  =
                                             WRK-TENSU-KEISAN2 + 0.5
                                   END-IF
                                   COMPUTE  SYU-HKNTEN(15)  =
                                            SYU-HKNTEN(15)  +
                                            WRK-TENSU-KEISAN
                                   COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                            SDAY-HKNTEN(SYU-IDX) +
                                            WRK-TENSU-KEISAN
      *                          ¸º³Û
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  17
                                   PERFORM   CHUKSNCD-CHECK-SEC
                                   EVALUATE  TBL-GAIHAKU(SYU-IDX)
                                    WHEN       8
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-KHNTEN * 0.92)
                                    WHEN       9
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-KHNTEN * 0.92)
      ***                           WHEN      11
      ***                             COMPUTE WRK-TENSU-KEISAN2 =
      ***                                    (WRK-KHNTEN * 0.92
      ***                                                * 0.7 )
                                    WHEN      13
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.92
                                                           * 0.9 )
                                     ELSE
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.92
                                                           * 0.7 )
                                     END-IF
                                    WHEN      17
                                        COMPUTE WRK-TENSU-KEISAN2 =
                                               (WRK-KHNTEN * 0.92
                                                           * 0.95)
                                   END-EVALUATE
                                   COMPUTE   WRK-TENSU-KEISAN2 =
                                             WRK-TENSU-KEISAN2 +
                                             WRK-KSNTEN
                                   IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                     COMPUTE WRK-TENSU-KEISAN  =
                                           ( WRK-TENSU-KEISAN2 -
                                            (WRK-TENSU-KEISAN2 *
                                            (WRK-HYOKETU-TEN   / 100)))
                                                               + 0.5
                                   ELSE
                                     COMPUTE WRK-TENSU-KEISAN  =
                                             WRK-TENSU-KEISAN2 + 0.5
                                   END-IF
                                   COMPUTE  SYU-HKNTEN(15)  =
                                            SYU-HKNTEN(15)  +
                                            WRK-TENSU-KEISAN
                                   COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                            SDAY-HKNTEN(SYU-IDX) +
                                            WRK-TENSU-KEISAN
                               END-EVALUATE
                           ELSE
      *                        ÁªÄê´ü´Ö¤ÎÅÀ¿ô»»Äê
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE WRK-KEISAN2        =
                                             WRK-SENTEI-ZAITEN  +  0.5
                                     ADD     WRK-KEISAN2
                                           TO  SYU-HKNTEN(15)
                                               SDAY-HKNTEN(SYU-IDX)    
      *                          ³°Çñ¡ÊÁªÄêÎÅÍÜÈñ¡Ë
                                 WHEN   1
                                 WHEN   3
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                            ((WRK-KHNTEN      *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *        
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)  
      *                          Â¾°åÎÅµ¡´Ø¼õ¿Ç
                                 WHEN   4
                                   PERFORM    NYUKHNKSNKBN-CHECK-SEC
                                   IF   SYU-SRYYMD(1:6) >= "201604"
                                     COMPUTE  WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.6 )
                                     COMPUTE  WRK-TENSU-KEISAN2 =
                                              WRK-TENSU-KEISAN2 +
                                              WRK-KSNTEN
                                     IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                      COMPUTE WRK-TENSU-KEISAN  =
                                            ( WRK-TENSU-KEISAN2 -
                                             (WRK-TENSU-KEISAN2 *
                                             (WRK-HYOKETU-TEN   / 100)))
                                                                + 0.5
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN  =
                                              WRK-TENSU-KEISAN2 + 0.5
                                     END-IF
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                              WRK-TENSU-KEISAN
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                              SDAY-HKNTEN(SYU-IDX) +
                                              WRK-TENSU-KEISAN
                                   ELSE
                                    IF  SYU-SRYYMD(1:6) >= "201004"
                                     COMPUTE  WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.3 )
                                     COMPUTE  WRK-TENSU-KEISAN2 =
                                              WRK-TENSU-KEISAN2 +
                                              WRK-KSNTEN
                                     IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                      COMPUTE WRK-TENSU-KEISAN  =
                                            ( WRK-TENSU-KEISAN2 -
                                             (WRK-TENSU-KEISAN2 *
                                             (WRK-HYOKETU-TEN   / 100)))
                                                                + 0.5
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN  =
                                              WRK-TENSU-KEISAN2 + 0.5
                                     END-IF
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                              WRK-TENSU-KEISAN
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                              SDAY-HKNTEN(SYU-IDX) +
                                              WRK-TENSU-KEISAN
                                    ELSE
                                     COMPUTE  SYU-HKNTEN(15)  =
                                              SYU-HKNTEN(15)  +
                                            ((WRK-KHNTEN      *
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)
                                     COMPUTE  SDAY-HKNTEN(SYU-IDX) =   
                                              SDAY-HKNTEN(SYU-IDX) +   
                                            ((WRK-KHNTEN      *        
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                              +  0.5)  
                                    END-IF
                                   END-IF
      *                          Â¾°åÎÅµ¡´Ø¼õ¿Ç
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                   PERFORM   NYUKHNKSNKBN-CHECK-SEC
                                   EVALUATE  TBL-GAIHAKU(SYU-IDX)
                                    WHEN       5
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.9 )
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.7 )
                                     END-IF
                                    WHEN       6
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.8 )
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.45)
                                     END-IF
                                    WHEN       7
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                      CONTINUE
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.85)
                                     END-IF
                                    WHEN      14
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.95)
                                    WHEN      15
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.65)
                                    WHEN      16
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.85)
                                   END-EVALUATE
                                   COMPUTE   WRK-TENSU-KEISAN2 =
                                             WRK-TENSU-KEISAN2 +
                                             WRK-KSNTEN
                                   IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                     COMPUTE WRK-TENSU-KEISAN  =
                                           ( WRK-TENSU-KEISAN2 -
                                            (WRK-TENSU-KEISAN2 *
                                            (WRK-HYOKETU-TEN   / 100)))
                                                               + 0.5
                                   ELSE
                                     COMPUTE WRK-TENSU-KEISAN  =
                                             WRK-TENSU-KEISAN2 + 0.5
                                   END-IF
                                   COMPUTE  SYU-HKNTEN(15)  =
                                            SYU-HKNTEN(15)  +
                                            WRK-TENSU-KEISAN
                                   COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                            SDAY-HKNTEN(SYU-IDX) +
                                            WRK-TENSU-KEISAN
      *                          ¸º³Û
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  17
                                   PERFORM   CHUKSNCD-CHECK-SEC
                                   EVALUATE  TBL-GAIHAKU(SYU-IDX)
                                    WHEN       8
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.92)
                                    WHEN       9
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.92)
      ***                           WHEN      11
      ***                             COMPUTE WRK-TENSU-KEISAN2 =
      ***                                    (WRK-SENTEI-KHNTEN * 0.92
      ***                                                       * 0.7 )
                                    WHEN      13
                                     IF  SYU-SRYYMD(1:6) >= "201604"
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.92
                                                                * 0.9 )
                                     ELSE
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.92
                                                                * 0.7 )
                                     END-IF
                                    WHEN      17
                                      COMPUTE WRK-TENSU-KEISAN2 =
                                             (WRK-SENTEI-KHNTEN * 0.92
                                                                * 0.95)
                                   END-EVALUATE
                                   COMPUTE   WRK-TENSU-KEISAN2 =
                                             WRK-TENSU-KEISAN2 +
                                             WRK-KSNTEN
                                   IF  WRK-HYOKETU-TEN  NOT =  ZERO
                                     COMPUTE WRK-TENSU-KEISAN  =
                                           ( WRK-TENSU-KEISAN2 -
                                            (WRK-TENSU-KEISAN2 *
                                            (WRK-HYOKETU-TEN   / 100)))
                                                               + 0.5
                                   ELSE
                                     COMPUTE WRK-TENSU-KEISAN  =
                                             WRK-TENSU-KEISAN2 + 0.5
                                   END-IF
                                   COMPUTE  SYU-HKNTEN(15)  =
                                            SYU-HKNTEN(15)  +
                                            WRK-TENSU-KEISAN
                                   COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                            SDAY-HKNTEN(SYU-IDX) +
                                            WRK-TENSU-KEISAN
                               END-EVALUATE
      *                        ÁªÄê´ü´Ö¤Î´µ¼Ô¼«¸ÊÉéÃ´³Û»»Äê
                               PERFORM    SENTEI-PTFTN-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        Æþ±¡ÎÁ²Ã»»¤Î½¸·×
               IF   NACCT-ZAISKBKBN      =   "6"
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  ((SYU-DAY(SYU-IDX)                =   "1") OR
                            (WRK-NYUINDAY-KBN(IDX SYU-IDX)   =   "1"))
                       AND ( NACCT-DAY(SYU-IDX)          NOT =  ZERO )
                       AND ((TBL-GAIHAKU(SYU-IDX)            =  ZERO) OR
                            (TBL-GAIHAKU(SYU-IDX)           >=    4 ))
                       AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1    )
                         IF   NACCT-SRYCDTOTAL  =  190182590
                                               OR  190182690
                           COMPUTE  SYU-HKNTEN(15) =
                                    SYU-HKNTEN(15) -
                                    NACCT-ZAITEN
                           COMPUTE  SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) -
                                    NACCT-ZAITEN
                         ELSE
                           ADD   NACCT-ZAITEN   TO
                                             SYU-HKNTEN(15)
                                             SDAY-HKNTEN(SYU-IDX)      
                         END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        ÎÅÍÜÃ´Åö¼êÅö¡ÊËÌ³¤Æ»¡Ë¤Î½¸·×
               IF   NACCT-ZAISKBKBN      =   "7"
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  ((SYU-DAY(SYU-IDX)                =   "1") OR
                            (WRK-NYUINDAY-KBN(IDX SYU-IDX)   =   "1"))
                       AND ( NACCT-DAY(SYU-IDX)          NOT =  ZERO )
                       AND ((TBL-GAIHAKU(SYU-IDX)            =  ZERO) OR
                            (TBL-GAIHAKU(SYU-IDX)           >=    4 ))
                       AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1    )
                           ADD   NACCT-ZAITEN    TO
                                             SYU-HKNTEN(16)
                                             SDAY-HKNTEN(SYU-IDX)      
                           IF    SYU-SYUHKNNUM   =    "975"
                             ADD NACCT-ZAITEN    TO
                                        WRK-JIBAI-RYOYO-NACCT(IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           PERFORM   NYUINACCT-GET-ETC-SEC
      *
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           PERFORM NYUINACCT-READ-SEC
      *
           .
       NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ¡Ê¤½¤ÎÂ¾¡Ë
      **************************************************************
       NYUINACCT-GET-ETC-SEC           SECTION.
      *
           IF     NACCT-SRYYM         =   SYU-SRYYMD(1:6)
      *        ¼¼ÎÁº¹³Û¤Î½¸·×
               IF   NACCT-ZAISKBKBN       =   "3"
      *
      *            ¥·¥¹¥Æ¥à´ÉÍý¤«¤éÆþ±¡ÎÁÌ¤»»Äê»þ¤Î¼¼ÎÁº¹³Û¤È¿©»ö¤Î
      *            ¼è°·¤¤¤ò¼èÆÀ
                   PERFORM    SYSKANRI-RMSGK-GET-SEC
      *
      *            ¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤«¤éÀßÄê¼¼ÎÁº¹³Û¤Î¼è¤ê¹þ¤ß
                   MOVE    ZERO                TO   FLG-SYSKANRI
                   INITIALIZE                       SYS-5113-REC
                   MOVE    LNK-S02-HOSPNUM     TO   SYS-5113-HOSPNUM
                   MOVE    "5113"              TO   SYS-5113-KANRICD
                   MOVE    SYU-SRYYMD          TO   SYS-5113-STYUKYMD
                                                    SYS-5113-EDYUKYMD
                   MOVE    SYS-5113-REC        TO   MCPDATA-REC
                   MOVE    "DBSELECT"          TO   MCP-FUNC
                   MOVE    "tbl_syskanri"      TO   MCP-TABLE
                   MOVE    "key2"              TO   MCP-PATHNAME
grpsys             CALL "ORCDBMAIN" USING MCPAREA
                                          MCPDATA-REC
grpsys                                    SPA-AREA
                   IF      MCP-RC              =    ZERO
                        MOVE    "tbl_syskanri" TO   MCP-TABLE
                        MOVE    "key2"         TO   MCP-PATHNAME
                        PERFORM  SYSKANRI-READ-SEC
                        PERFORM   UNTIL   FLG-SYSKANRI   =   1
                            MOVE   SYSKANRI-REC   TO  SYS-5113-REC
                            MOVE   SYS-5113-KBNCD(1:2)  TO  WRK-KBNCD
                            MOVE   SYS-5113-KBNCD(3:6)  TO  WRK-KBNCD2
                            IF     WRK-KBNCD  IS  NUMERIC
                            AND    WRK-KBNCD  NOT = "00"
                            AND    WRK-KBNCD2     =  SPACE
                                MOVE   SYS-5113-BRM-SAGAKU-NM
                                      TO  TBL-RMSAGAKU(WRK-KBNCD-X)
                            END-IF
                            MOVE    "tbl_syskanri" TO   MCP-TABLE
                            MOVE    "key2"         TO   MCP-PATHNAME
                            PERFORM  SYSKANRI-READ-SEC
                        END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_syskanri" TO   MCP-TABLE
                   MOVE    "key2"         TO   MCP-PATHNAME
                   PERFORM   CSR-CLOSE-SEC
      *
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       EVALUATE  TBL-5000-NACCT-BRMSGKKBN(SYU-IDX)
                         WHEN  SPACE
                         WHEN  "1"
                           IF (( SYU-DAY(SYU-IDX)         = "1"   ) AND
                               ( NACCT-DAY(SYU-IDX)   NOT =  ZERO ) AND
                               ( SYU-SRYYMD(1:6)      <= "201403" ))
                           OR (((SYU-DAY(SYU-IDX)         = "1"  ) OR
                                (WRK-NYUINDAY-KBN(IDX SYU-IDX)
                                                          = "1"  )) AND
                               ( NACCT-DAY(SYU-IDX)   NOT =  ZERO ) AND
                               ( SYU-SRYYMD(1:6)      >= "201404" ))
                             MOVE  NACCT-DAY(SYU-IDX)  TO  RMSAGAKU-IDX
                             IF  (WRK-BRMSGK-TAXKBN    =  "1"    )
                             AND (SYU-SRYYMD(1:6)     >= "200704")
                               COMPUTE WRK-KEISAN  =
                                      (TBL-RMSAGAKU(RMSAGAKU-IDX)
                                                   * WRK-S4) + WRK-S3
                               ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                   TO   SYU-RMSAGAKU
                               ADD   WRK-KEISAN    TO   SYU-RMSAGAKU
                             ELSE
                               ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                   TO   SYU-RMSAGAKU
                             END-IF
                           END-IF
                         WHEN  "2"
                           IF  SYU-SRYYMD(1:6)   =  WRK-SKYYM(1)
                             IF  (SYU-HKNCOMBINUM =
                                             WRK-DAY5(1 SYU-IDX))   AND
                                 (NACCT-DAY(SYU-IDX) NOT =  ZERO)   AND
                                 (WRK-DOJITU-FLG(IDX SYU-IDX)
                                                        = 0 OR 1)
                               MOVE  NACCT-DAY(SYU-IDX) TO RMSAGAKU-IDX
                               IF  (WRK-BRMSGK-TAXKBN    =  "1"    )
                               AND (SYU-SRYYMD(1:6)     >= "200704")
                                 COMPUTE WRK-KEISAN  =
                                        (TBL-RMSAGAKU(RMSAGAKU-IDX)
                                                     * WRK-S4) + WRK-S3
                                 ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                     TO   SYU-RMSAGAKU
                                 ADD   WRK-KEISAN    TO   SYU-RMSAGAKU
                               ELSE
                                 ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                     TO   SYU-RMSAGAKU
                               END-IF
                             END-IF
                           ELSE
                             IF  (SYU-SRYYMD(1:6)  =  WRK-SKYYM(2)) AND
                                 (SYU-HKNCOMBINUM  =
                                             WRK-DAY5(2 SYU-IDX))   AND
                                 (NACCT-DAY(SYU-IDX) NOT =  ZERO)   AND
                                 (WRK-DOJITU-FLG(IDX SYU-IDX)
                                                        = 0 OR 1)
                               MOVE  NACCT-DAY(SYU-IDX) TO RMSAGAKU-IDX
                               IF  (WRK-BRMSGK-TAXKBN    =  "1"    )
                               AND (SYU-SRYYMD(1:6)     >= "200704")
                                 COMPUTE WRK-KEISAN  =
                                        (TBL-RMSAGAKU(RMSAGAKU-IDX)
                                                     * WRK-S4) + WRK-S3
                                 ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                     TO   SYU-RMSAGAKU
                                 ADD   WRK-KEISAN    TO   SYU-RMSAGAKU
                               ELSE
                                 ADD   TBL-RMSAGAKU (RMSAGAKU-IDX)
                                                     TO   SYU-RMSAGAKU
                               END-IF
                             END-IF
                           END-IF
                       END-EVALUATE
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       NYUINACCT-GET-ETC-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç¥Á¥§¥Ã¥¯
      **************************************************************
       NYUKHNKSNKBN-CHECK-SEC           SECTION.
      *
           IF      SYU-SRYYMD(1:6)  >=  "201804"
                  PERFORM   NYUKHNKSNKBN-CHECK3-SEC
           ELSE
             IF    SYU-SRYYMD(1:6)  >=  "201604"
                  PERFORM   NYUKHNKSNKBN-CHECK2-SEC
             ELSE
                  PERFORM   NYUKHNKSNKBN-CHECK1-SEC
             END-IF
           END-IF
      *
           .
      *
       NYUKHNKSNKBN-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç¥Á¥§¥Ã¥¯£±
      **************************************************************
       NYUKHNKSNKBN-CHECK1-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN
      *      ÆÃÄêÆþ±¡´ðËÜÎÁ
             WHEN   099  THRU  100
               IF    TBL-GAIHAKU(SYU-IDX)  =    6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      ÆÃÄêÆþ±¡ÎÁ
             WHEN   201  THRU  219
             WHEN   221  THRU  222
             WHEN   224
             WHEN   227  THRU  229
             WHEN   230  THRU  239
             WHEN   241  THRU  249
             WHEN   250  THRU  255
      *      (H26-ADD-ST)
             WHEN   256
             WHEN   257  THRU  259
             WHEN   260  THRU  261
             WHEN   262  THRU  265
             WHEN   266  THRU  267
      *      (H26-ADD-ED)
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ
               IF    WRK-NYUKHNKSNKBN = 221 OR 232
      *          ¡Ê£¶£±Æü°Ê¾å¡Ë
                 IF   WRK-CHK-SRYCD = "190740010" OR "190813510" OR
                                      "190722510" OR "190722810"
                   IF    TBL-GAIHAKU(SYU-IDX)  =    7
                      MOVE    1    TO    WRK-TAIJYUSIN-ERR
                   END-IF
                 END-IF
      *          ¡Ê£¶£°Æü°ÊÆâ¡Ë
                 IF   WRK-CHK-SRYCD = "190151610" OR "190739910" OR
                                      "190151710" OR "190813410" OR
                                      "190722310" OR "190722410" OR
                                      "190722610" OR "190722710"
                   IF    TBL-GAIHAKU(SYU-IDX)  =    6
                      MOVE    1    TO    WRK-TAIJYUSIN-ERR
                   END-IF
                 END-IF
               ELSE
      *          Àº¿À²ÊµßµÞÆþ±¡ÎÁ¡¢Àº¿À²ÊµÞÀ­´ü¼£ÎÅÉÂÅïÆþ±¡ÎÁ
      *          Àº¿À²ÊµßµÞ¡¦¹çÊ»¾ÉÆþ±¡ÎÁ¡¢Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
                 IF  WRK-NYUKHNKSNKBN = 229 OR 238 OR 215 OR 216 OR
                                        217 OR 239 OR 252
                   IF    TBL-GAIHAKU(SYU-IDX)  =    7
                      MOVE    1    TO    WRK-TAIJYUSIN-ERR
                   END-IF
                 ELSE
                   IF    TBL-GAIHAKU(SYU-IDX)  =    6 OR 7
                      MOVE    1    TO    WRK-TAIJYUSIN-ERR
                   END-IF
                 END-IF
               END-IF
      *      ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
             WHEN   013  THRU  021
             WHEN   101  THRU  109
             WHEN   147  THRU  152
             WHEN   173  THRU  190
             WHEN   196
      *      (H26-ADD-ST)
             WHEN   199
             WHEN   004  THRU  008
             WHEN   010  THRU  012
      *      (H26-ADD-ED)
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
               IF    TBL-GAIHAKU(SYU-IDX)  =    6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *
             WHEN   075  THRU  080
                  CONTINUE
      *      Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
             WHEN   085  THRU  086
             WHEN   132  THRU  133
             WHEN   153  THRU  158
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
               IF  WRK-NYUKHNKSNKBN = 153 OR 154 OR 155 OR 156 OR
                                      157 OR 158
                 IF    TBL-GAIHAKU(SYU-IDX)  =    6
                    MOVE    1    TO    WRK-TAIJYUSIN-ERR
                 END-IF
               ELSE
                 IF    TBL-GAIHAKU(SYU-IDX)  =    6 OR 7
                    MOVE    1    TO    WRK-TAIJYUSIN-ERR
                 END-IF
               END-IF
      *      Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
             WHEN   226
             WHEN   240
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
      *      Àº¿ÀÉÂÅïÆþ±¡´ðËÜÎÁ
             WHEN   031
             WHEN   033
             WHEN   034
             WHEN   035
             WHEN   038
             WHEN   170
             WHEN   191
      *      (H26-ADD-ST)
             WHEN   017  THRU  020
      *      (H26-ADD-ED)
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      ·ë³ËÉÂÅïÆþ±¡´ðËÜÎÁ
             WHEN   022
             WHEN   023
             WHEN   024
             WHEN   025
             WHEN   026
             WHEN   029
             WHEN   136
             WHEN   168
             WHEN   169
      *      (H26-ADD-ST)
             WHEN   013  THRU  016
      *      (H26-ADD-ED)
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ
             WHEN   081
             WHEN   083
             WHEN   172
      *      (H26-ADD-ST)
             WHEN   032  THRU  037
      *      (H26-ADD-ED)
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Â¾
             WHEN   OTHER
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6 OR 7
                  MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
           END-EVALUATE
      *
           .
      *
       NYUKHNKSNKBN-CHECK1-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç¥Á¥§¥Ã¥¯£²  201604
      **************************************************************
       NYUKHNKSNKBN-CHECK2-SEC           SECTION.
      *
           IF   TBL-GAIHAKU(SYU-IDX)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN
      *      ÆÃÄêÆþ±¡ÎÁ
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
      *      --- H28.4 TUIKA ---
             WHEN   268  THRU  270
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ¡¢Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
      *        ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ
               IF  WRK-NYUKHNKSNKBN = 221 OR 232 OR 217 OR 268
                 CONTINUE
               ELSE
                 IF  TBL-GAIHAKU(SYU-IDX) =  6
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
                 END-IF
               END-IF
      *      ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
             WHEN   004  THRU  008
             WHEN   010  THRU  012
             WHEN   152
             WHEN   173  THRU  190
             WHEN   196
             WHEN   199
      *      --- H28.4 TUIKA ---
             WHEN   048  THRU  049
             WHEN   051  THRU  054
             WHEN   057  THRU  068
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
               IF    TBL-GAIHAKU(SYU-IDX) =  6
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
             WHEN   153  THRU  158
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
             WHEN   226
             WHEN   240
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
      *      Â¾
             WHEN   OTHER
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
           END-EVALUATE
      *
           .
      *
       NYUKHNKSNKBN-CHECK2-EXT.
           EXIT.
      *
      **************************************************************
      *    Â¾°åÎÅµ¡´Ø¼õ¿Ç¥Á¥§¥Ã¥¯£³  201804
      **************************************************************
       NYUKHNKSNKBN-CHECK3-SEC           SECTION.
      *
           IF   TBL-GAIHAKU(SYU-IDX)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN
      *      ÆÃÄêÆþ±¡ÎÁ
             WHEN   200  THRU  257
             WHEN   260  THRU  263
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *        Ç§ÃÎ¾É¼£ÎÅÉÂÅïÆþ±¡ÎÁ¡¢Àº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ
      *        ÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ
               IF  WRK-NYUKHNKSNKBN = 255 OR 254 OR 257
                 CONTINUE
               ELSE
                 IF  TBL-GAIHAKU(SYU-IDX) =  6 OR 16
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
                 END-IF
               END-IF
      *      ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
             WHEN    11  THRU   14
             WHEN    51
             WHEN   901
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
               IF    TBL-GAIHAKU(SYU-IDX) =  6 OR 16
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
             WHEN    26
             WHEN    53
             WHEN   909
               IF    TBL-SENTEI(SYU-IDX)  NOT  =   ZERO
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
      *      Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
             WHEN   258  THRU  259
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
      *      Â¾
             WHEN   OTHER
               IF    TBL-GAIHAKU(SYU-IDX)  =    4 OR 6 OR 15 OR 16
                   MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
           END-EVALUATE
      *
           .
      *
       NYUKHNKSNKBN-CHECK3-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸º»»¥Á¥§¥Ã¥¯
      **************************************************************
       CHUKSNCD-CHECK-SEC           SECTION.
      *
           IF      SYU-SRYYMD(1:6)  >=  "201804"
                  PERFORM   CHUKSNCD-CHECK3-SEC
           ELSE
             IF    SYU-SRYYMD(1:6)  >=  "201604"
                  PERFORM   CHUKSNCD-CHECK2-SEC
             ELSE
                  PERFORM   CHUKSNCD-CHECK1-SEC
             END-IF
           END-IF
      *
           .
      *
       CHUKSNCD-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸º»»¥Á¥§¥Ã¥¯£±
      **************************************************************
       CHUKSNCD-CHECK1-SEC           SECTION.
      *
           EVALUATE   TBL-GAIHAKU(SYU-IDX)
             WHEN        8
      ***    WHEN       11
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9018"
                 WHEN   "9026"
                 WHEN   "9030"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR
               END-EVALUATE
             WHEN        9
             WHEN       13
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9018"
                 WHEN   "9026"
                 WHEN   "9030"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR2
               END-EVALUATE
           END-EVALUATE
      *
           .
      *
       CHUKSNCD-CHECK1-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸º»»¥Á¥§¥Ã¥¯£²  201604
      **************************************************************
       CHUKSNCD-CHECK2-SEC           SECTION.
      *
           EVALUATE   TBL-GAIHAKU(SYU-IDX)
             WHEN        8
      ***    WHEN       11
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9026"
                 WHEN   "9030"
                 WHEN   "9090"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR
               END-EVALUATE
             WHEN        9
             WHEN       13
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9026"
                 WHEN   "9030"
                 WHEN   "9090"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR2
               END-EVALUATE
           END-EVALUATE
      *
           .
      *
       CHUKSNCD-CHECK2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸º»»¥Á¥§¥Ã¥¯£³  201804
      **************************************************************
       CHUKSNCD-CHECK3-SEC           SECTION.
      *
           EVALUATE   TBL-GAIHAKU(SYU-IDX)
             WHEN        8
      ***    WHEN       11
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9026"
                 WHEN   "9030"
                 WHEN   "9090"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR
               END-EVALUATE
             WHEN        9
             WHEN       13
             WHEN       17
               EVALUATE   WRK-CHUKSNCD
                 WHEN   "9017"
                 WHEN   "9026"
                 WHEN   "9030"
                 WHEN   "9090"
                   CONTINUE
                 WHEN    OTHER
                   MOVE    1    TO    WRK-GENSAN-ERR2
               END-EVALUATE
           END-EVALUATE
      *
           .
      *
       CHUKSNCD-CHECK3-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ¡ÊÏ«ºÒ¡¦¼«ÇåÀÕÍÑ¡Ë
      **************************************************************
       NYUINACCT-GET2-SEC           SECTION.
      *
           IF    (NACCT-ZAITEN               NOT = ZERO  )  AND
                 (NACCT-SRYYM(1:6)    =   SYU-SRYYMD(1:6))
      *
      *        ÎÅÍÜÃ´Åö¼êÅö¡ÊËÌ³¤Æ»¡Ë¤Î½¸·×
               IF   NACCT-ZAISKBKBN      =   "7"
                   PERFORM   VARYING  SYU-IDX  FROM  1  BY  1
                             UNTIL    SYU-IDX  >   31
                       IF  ((SYU-DAY(SYU-IDX)                =   "1") OR
                            (WRK-NYUINDAY-KBN(IDX SYU-IDX)   =   "1"))
                       AND ( NACCT-DAY(SYU-IDX)          NOT =  ZERO )
                       AND ((TBL-GAIHAKU(SYU-IDX)            =  ZERO) OR
                            (TBL-GAIHAKU(SYU-IDX)           >=    4 ))
                       AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1    )
                           ADD   NACCT-ZAITEN    TO   SYU-HKNTEN(16)
                           IF    SYU-SYUHKNNUM   =    "973"
                              ADD  NACCT-ZAITEN
                                    TO   WRK-JIBAI-RYOYO-NACCT(IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           PERFORM   NYUINACCT-GET-ETC-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key9"                  TO  MCP-PATHNAME
           PERFORM NYUINACCT-READ-SEC
      *
           .
       NYUINACCT-GET2-EXT.
           EXIT.
      *
      *
      **************************************************************
      *    Æþ±¡ÎÁ´Ø·¸¤Î¼èÆÀ¡Ê¤½¤ÎÂ¾¡Ë
      **************************************************************
       SYSKANRI-RMSGK-GET-SEC           SECTION.
      *
           PERFORM   SYS-5000-CHK-SEC
      *
           MOVE      SPACE               TO  WRK-BRMSGK-TAXKBN
           IF        WRK-SYS5000-COUNT    =     ZERO
             INITIALIZE   TABLE-5000-NACCT
             MOVE    ZERO                TO  FLG-SYSKANRI
             INITIALIZE                      SYSKANRI-REC
             MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                             SYS-EDYUKYMD
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       CALL    "ORCDBMAIN" USING   MCPAREA
                                         MCPDATA-REC
grpsys                                   SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  VARYING SYU-IDX FROM 1 BY 1    
                  UNTIL    SYU-IDX  >   31      
                      MOVE   SYS-5000-NACCT-BRMSGKKBN
                                  TO  TBL-5000-NACCT-BRMSGKKBN(SYU-IDX)
               END-PERFORM
               MOVE   SYS-5000-BRMSGK-TAXKBN
                                  TO  WRK-BRMSGK-TAXKBN
             END-IF
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
           ELSE
      *
             INITIALIZE                      TABLE-5000-NACCT
      *
             MOVE    ZERO                TO  FLG-SYSKANRI
             INITIALIZE                      SYSKANRI-REC
             MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                             SYS-EDYUKYMD
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       CALL "ORCDBMAIN" USING MCPAREA
                                    MCPDATA-REC
grpsys                              SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key10"           TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               PERFORM VARYING SYU-IDX FROM 1 BY 1    
                  UNTIL    SYU-IDX  >   31      
                      MOVE   SYS-5000-NACCT-BRMSGKKBN
                                TO  TBL-5000-NACCT-BRMSGKKBN(SYU-IDX)
               END-PERFORM
               MOVE   SYS-5000-BRMSGK-TAXKBN
                                TO  WRK-BRMSGK-TAXKBN
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
      *
             MOVE    ZERO                TO  FLG-SYSKANRI
             INITIALIZE                      SYSKANRI-REC
             MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    SYU-SRYYMD(1:6)     TO  SYS-STYUKYMD(1:6)
             MOVE    "__"                TO  SYS-STYUKYMD(7:2)
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key11"             TO  MCP-PATHNAME
grpsys       CALL "ORCDBMAIN" USING MCPAREA
                                    MCPDATA-REC
grpsys                              SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE   SYS-5000-STYUKYMD(7:2)  TO  KIRIKAE-IDXX
                   PERFORM    VARYING  SYU-IDX  FROM KIRIKAE-IDX BY 1
                              UNTIL    SYU-IDX  >   31
                       MOVE   SYS-5000-NACCT-BRMSGKKBN
                                  TO  TBL-5000-NACCT-BRMSGKKBN(SYU-IDX)
                   END-PERFORM
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key11"          TO  MCP-PATHNAME
                   PERFORM  SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key11"          TO  MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
      *
      *
           END-IF
      *
           .
       SYSKANRI-RMSGK-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡Ï«ºÒ²ñ·×ºîÀ®¥µ¥Ö¤è¤êÆþ±¡ÎÁ¡¦³°Çñ¤ò¼èÆÀ¡ÊÏ«ºÒ¡¦¼«ÇåÀÕÍÑ¡Ë
      **************************************************************
       ROSAI-JIBAI-NYUINRYO-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-CHECK-STDAY
           MOVE    ZERO                TO  WRK-CHECK-EDDAY
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    SYU-HOSPNUM          TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    SYU-PTID            TO  LNK-SROUNYUIN-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    SYU-HKNCOMBINUM     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   SYU-IDX   FROM   1   BY   1
                          UNTIL    SYU-IDX    >    31
      *
                IF  ( LNK-SROUNYUIN-NYUIN-FLG(SYU-IDX)    =   "1" )
                AND ((SYU-DAY(SYU-IDX)                    =   "1") OR
                     (WRK-NYUINDAY-KBN(IDX SYU-IDX)       =   "1"))
                AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1        )
                     ADD   LNK-SROUNYUIN-ZAITEN(SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                     ADD   LNK-SROUNYUIN-ZAITEN-NACCT(SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                     EVALUATE  WRK-DOJITU-FLG(IDX SYU-IDX)
                       WHEN      0
                         ADD  LNK-SROUNYUIN-ZAITEN-ACCT (SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                       WHEN      1
                         ADD  LNK-SROUNYUIN-ZAITEN-ACCT2(SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                     END-EVALUATE
      **>            ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC(SYU-IDX)
      **>                                        TO  SYU-HKNTEN(15)
                     ADD   LNK-SROUNYUIN-GENTEN(SYU-IDX)
                                       TO  WRK-SYUG-NYN-HKNTEN(IDX)
                END-IF
      *
                MOVE   SYU-SKYSTYMD(7:2)  TO  WRK-CHECK-STDAY
                MOVE   SYU-SKYEDYMD(7:2)  TO  WRK-CHECK-EDDAY
                IF  ( WRK-CHECK-STDAY  <=  SYU-IDX         )
                AND ( SYU-IDX          <=  WRK-CHECK-EDDAY )
                 IF  WRK-DOJITU-FLG2(IDX SYU-IDX) = 1
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC (SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                 ELSE
                  EVALUATE  WRK-DOJITU-FLG(IDX SYU-IDX)
                    WHEN      0
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC (SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                    WHEN      1
                    WHEN      7
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC2(SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                    WHEN      8
                    WHEN      9
                     ADD   LNK-SROUNYUIN-ZAITEN-ACCTETC3(SYU-IDX)
                                                 TO  SYU-HKNTEN(15)
                  END-EVALUATE
                 END-IF
                END-IF
      *
              END-PERFORM
      *
           ELSE
      *
              IF   LNK-SROUNYUIN-RC   =   7
                   MOVE    1    TO   WRK-NYUINRYO-ERR
              END-IF
              IF   LNK-SROUNYUIN-RC   =   8
                   MOVE    1    TO   WRK-NYUINRYO-ERR2
              END-IF
              IF   LNK-SROUNYUIN-RC   =   9
                   MOVE    1    TO   WRK-TAIJYUSIN-ERR
              END-IF
              IF   LNK-SROUNYUIN-RC   =  10
                   MOVE    1    TO   WRK-GENSAN-ERR
              END-IF
              IF   LNK-SROUNYUIN-RC   =  11
                   MOVE    1    TO   WRK-GENSAN-ERR2
              END-IF
      *
           END-IF
      *
           .
       ROSAI-JIBAI-NYUINRYO-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡´ðËÜÎÁ¤È²Ã¸º»»ÅÀ¿ô¤Î¼èÆÀ
      **************************************************************
       NYUINKHN-GET-SEC           SECTION.
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    ZERO               TO   WRK-KHNTEN-MOTO
           MOVE    ZERO               TO   WRK-KHNTEN-GEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-MOTO
           MOVE    ZERO               TO   WRK-KHN-ZAITEN-GEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN-MOTO
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN-GEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN-MOTO
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN-GEN
           MOVE    ZERO               TO   WRK-NYUKHNKSNKBN
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    ZERO               TO   WRK-HYOKETU-TEN
           MOVE    ZERO               TO   SENTEI-IDX
           MOVE    SPACE              TO   WRK-CHK-SRYCD
           MOVE    SPACE              TO   WRK-CHUKSNCD
      *
           MOVE    SYU-HOSPNUM        TO   NSRY-HOSPNUM
           MOVE    "1"                TO   NSRY-NYUGAIKBN
           MOVE    SYU-PTID           TO   NSRY-PTID
           MOVE    "00"               TO   NSRY-SRYKA
           MOVE    SYU-SRYYMD(1:6)    TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    1                  TO   NSRY-RENNUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC             =    ZERO
               MOVE    "tbl_nyuinact" TO   MCP-TABLE
               MOVE    "key2"         TO   MCP-PATHNAME
               PERFORM NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
           IF      FLG-NYUINACT        =   ZERO
               MOVE      ZERO         TO   FLG-NYUKHN
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE     LNK-S02-HOSPNUM  TO  TNS-HOSPNUM
                     MOVE     NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE     SYU-SRYYMD(1:6)  TO  TNS-YUKOSTYMD
                                                   TNS-YUKOEDYMD
                     MOVE     "01"             TO  TNS-YUKOSTYMD(7:2)
                                                   TNS-YUKOEDYMD(7:2)
                     PERFORM  TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              IF  (SYU-SRYYMD(1:6) >= "202004")
                              AND (TNS-SRYCD    NOT = SPACE   )
                              AND (TNS-NYUKHNKSNKBN = ZERO    )
                                INITIALIZE              SNTENSU-AREA
                                MOVE  TNS-SRYCD     TO  SNTENSU-SRYCD
                                MOVE  SYU-SRYYMD(1:6)
                                             TO   SNTENSU-BASEYMD(1:6)
                                MOVE  "01"   TO   SNTENSU-BASEYMD(7:2)
                                CALL  "ORCSNTENSU"  USING
                                                    SNTENSU-AREA
                                                    TNS-TENSU-REC
                                                    SPA-AREA
                              END-IF
                              MOVE   TNS-TEN   TO   WRK-KHNTEN-MOTO
      *                       Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»
                              COMPUTE  WRK-KHNTEN-GEN   =
                                      (WRK-KHNTEN-MOTO  * 0.95)
                              MOVE   TNS-NYUKHNKSNKBN
                                               TO   WRK-NYUKHNKSNKBN
      *                       Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÃÖ¤­´¹¤¨
                              IF  SYU-SRYYMD(1:6) >= "201804"
      *                         ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
      *                                       ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
                                IF   (TNS-NYUKHNKSNKBN = 27 )
                                AND  (TNS-CDKBN-KBNNUM = 101)
                                  MOVE   901   TO   WRK-NYUKHNKSNKBN
                                END-IF
      *                         Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
      *                                       ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
                                IF   (TNS-NYUKHNKSNKBN = 27 )
                                AND  (TNS-CDKBN-KBNNUM = 109)
                                  MOVE   909   TO   WRK-NYUKHNKSNKBN
                                END-IF
                              END-IF
                              MOVE   TNS-CHUKSNCD
                                               TO   WRK-CHUKSNCD
                              MOVE   NSRY-SRYCD(NYUINACT-IDX)
                                               TO   WRK-CHK-SRYCD
      ***                     MOVE   1         TO   FLG-NYUKHN
                              MOVE   NYUINACT-IDX  TO  SENTEI-IDX
                         ELSE
                              EVALUATE  TNS-TENSIKIBETU
                                WHEN     3
                                    ADD  TNS-TEN  TO  WRK-KSNTEN
                                WHEN     8
                                    COMPUTE  WRK-KSNTEN = WRK-KSNTEN +
                                                        (TNS-TEN * -1)
                                WHEN     6
                                    MOVE  TNS-TEN  TO  WRK-HYOKETU-TEN
                              END-EVALUATE
                         END-IF
                     ELSE
                         IF  NYUINACT-IDX    =    1
                              MOVE    1    TO   WRK-NYUINRYO-ERR
                         END-IF
                     END-IF                  
               END-PERFORM
           END-IF
      *
           COMPUTE  WRK-KHN-ZAITEN-MOTO     =
                    NACCT-ZAITEN
           COMPUTE  WRK-KHN-ZAITEN-GEN      =
                    WRK-KHNTEN-GEN          +
                    WRK-KSNTEN
           IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *      É¸·ç¡§¡ó¸º»»
             COMPUTE    WRK-KEISAN2             =
                      ( WRK-KHN-ZAITEN-GEN      -
                       (WRK-KHN-ZAITEN-GEN      *
                        (WRK-HYOKETU-TEN   /  100)))  +  0.5
             MOVE   WRK-KEISAN2   TO   WRK-KHN-ZAITEN-GEN
           END-IF
      *
      *    Æþ±¡ÎÁºÞÅÀ¥Á¥§¥Ã¥¯
           COMPUTE  WRK-HIKAKU-ZAITEN = WRK-KHNTEN-MOTO + WRK-KSNTEN
           IF     WRK-HYOKETU-TEN    NOT =   ZERO
             COMPUTE WRK-HIKAKU-ZAITEN =
                   ( WRK-HIKAKU-ZAITEN -
                    (WRK-HIKAKU-ZAITEN * (WRK-HYOKETU-TEN / 100)) )
                                                                + 0.5
           END-IF
           IF   SYU-SRYYMD(1:6) >= "201004"
             IF   NACCT-ZAITEN   =  WRK-HIKAKU-ZAITEN
               CONTINUE
             ELSE
               MOVE    1    TO   WRK-NYUINRYO-ERR2
             END-IF
           END-IF
      *
      *    ´Ç¸îÈæÎ¨´ð½àÌ¤Ã£»þ¤ÎÆþ±¡´ðËÜÅÀ¿ô¤Î¼èÆÀ
           MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
           IF   SYU-SRYYMD(1:6) <= "201203"
             PERFORM  GAIHAKU-NYUINKHN-SET-SEC
           END-IF
           IF    WRK-GAIHAKU-SRYCD   NOT =   SPACE
                 MOVE  WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN-MOTO
           END-IF
      *
           MOVE    "tbl_nyuinact" TO  MCP-TABLE
           MOVE    "key2"         TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
      *    ÁªÄêÎÅÍÜ¤ÎÆþ±¡ºÞÅÀ¿ô¤È´ðËÜÅÀ¿ô¤Î¼èÆÀ
           IF        FLG-SENTEI-SET    =    1
               MOVE    ZERO            TO  FLG-SENTEICDCHG
               INITIALIZE  SENTEICDCHG-REC
               MOVE    NSRY-SRYCD(SENTEI-IDX)
                                       TO  SENCHG-IPNSRYCD
               MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
               IF      MCP-RC             =    ZERO
                   MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM  SENTEICDCHG-READ-SEC
               ELSE
                   MOVE    1              TO   FLG-SENTEICDCHG
               END-IF
      *
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  CSR-CLOSE-SEC
      *
               IF   FLG-SENTEICDCHG = ZERO
                IF    SENCHG-IPNSRYCD  =  SENCHG-SENTEISRYCD
      *           Ìë¶Ð»þ´Ö·Ð²á¸º»»¤ÎÁªÄêÂÐ±þ
      *           "190131410","190165810","190165910","190131310"
                  IF   SYU-SRYYMD(1:6) >= "201404"
                    COMPUTE  WRK-SENTEI-KHNTEN-MOTO  =
                            (WRK-KHNTEN-MOTO * 0.85) +  0.5
      *             Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»
                    COMPUTE  WRK-SENTEI-KHNTEN-GEN   =
                            (WRK-SENTEI-KHNTEN-MOTO  * 0.95)
                    COMPUTE  WRK-SENTEI-ZAITEN-MOTO  =
                             WRK-SENTEI-KHNTEN-MOTO  +
                             WRK-KSNTEN
                    COMPUTE  WRK-SENTEI-ZAITEN-GEN   =
                             WRK-SENTEI-KHNTEN-GEN   +
                             WRK-KSNTEN
                    IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *               É¸·ç¡§¡ó¸º»»
                      COMPUTE    WRK-SENTEI-ZAITEN-MOTO  =
                               ( WRK-SENTEI-ZAITEN-MOTO  -
                                (WRK-SENTEI-ZAITEN-MOTO  *
                                 (WRK-HYOKETU-TEN   /  100)))  +  0.5
                      COMPUTE    WRK-KEISAN2             =
                               ( WRK-SENTEI-ZAITEN-GEN   -
                                (WRK-SENTEI-ZAITEN-GEN   *
                                 (WRK-HYOKETU-TEN   /  100)))  +  0.5
                      MOVE   WRK-KEISAN2   TO   WRK-SENTEI-ZAITEN-GEN
                    END-IF
                  END-IF
                ELSE
                 MOVE    SPACE         TO   TNS-TENSU-REC
                 INITIALIZE                 TNS-TENSU-REC
                 MOVE     LNK-S02-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    SENCHG-SENTEISRYCD TO  TNS-SRYCD
                 MOVE     SYU-SRYYMD(1:6)   TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                 MOVE     "01"              TO  TNS-YUKOSTYMD(7:2)
                                                TNS-YUKOEDYMD(7:2)
                 PERFORM  TENSU-READ-SEC
                 IF       FLG-TENSU      =    ZERO
                   MOVE   TNS-TEN      TO   WRK-SENTEI-KHNTEN-MOTO
      *            Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»
                   COMPUTE  WRK-SENTEI-KHNTEN-GEN   =
                           (WRK-SENTEI-KHNTEN-MOTO  * 0.95)
                   COMPUTE  WRK-SENTEI-ZAITEN-MOTO  =
                            WRK-SENTEI-KHNTEN-MOTO  +
                            WRK-KSNTEN
                   COMPUTE  WRK-SENTEI-ZAITEN-GEN   =
                            WRK-SENTEI-KHNTEN-GEN   +
                            WRK-KSNTEN
                   IF     WRK-HYOKETU-TEN    NOT =   ZERO
      *              É¸·ç¡§¡ó¸º»»
                     COMPUTE    WRK-SENTEI-ZAITEN-MOTO  =
                              ( WRK-SENTEI-ZAITEN-MOTO  -
                               (WRK-SENTEI-ZAITEN-MOTO  *
                                (WRK-HYOKETU-TEN   /  100)))  +  0.5
                     COMPUTE    WRK-KEISAN2             =
                              ( WRK-SENTEI-ZAITEN-GEN   -
                               (WRK-SENTEI-ZAITEN-GEN   *
                                (WRK-HYOKETU-TEN   /  100)))  +  0.5
                      MOVE   WRK-KEISAN2   TO   WRK-SENTEI-ZAITEN-GEN
                   END-IF
                 END-IF
                END-IF
               END-IF
      *
           END-IF
      *
           .
       NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           MOVE    ZERO     TO   WRK-GAIHAKU-KHNTEN
      *
      ***  EVALUATE  TNS-SRYCD
           EVALUATE  NSRY-SRYCD(SENTEI-IDX)
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
           IF   (SYU-SRYYMD(1:6)   >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE     LNK-S02-HOSPNUM   TO  TNS-HOSPNUM
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    SYU-SRYYMD(1:6)    TO  TNS-YUKOSTYMD
                                              TNS-YUKOEDYMD
               MOVE    "01"               TO  TNS-YUKOSTYMD(7:2)
                                              TNS-YUKOEDYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN   TO   WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄê´ü´Ö¤Î´µ¼Ô¼«¸ÊÉéÃ´³Û»»Äê
      **************************************************************
       SENTEI-PTFTN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM   CSR-CLOSE-SEC
               EVALUATE   SYS-5000-SENTEI-FTNKBN
      *            °åÎÅµ¡´ØÄêÎ¨ÀßÄê
                   WHEN   "1"
                       IF     SYS-5000-SENTEI-TANKA-X   =   SPACE
                           MOVE   10       TO   SYS-5000-SENTEI-TANKA
                       END-IF
                       EVALUATE   SYS-5000-SENTEI-TAXKBN
      *                   ¾ÃÈñÀÇ¤Ê¤·
                           WHEN   SPACE
                           WHEN   "0"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE  WRK-KEISAN2 =
                                             (WRK-KHNTEN  -
                                              WRK-SENTEI-KHNTEN) + 0.5
                                     COMPUTE  NYUIN-TGMONEY =
                                              NYUIN-TGMONEY +
                                             (WRK-KEISAN2 *
                                              SYS-5000-SENTEI-TANKA)
      *                          ³°Çñ¡Ê£±£µ¡ó¤ÈÁªÄê³°Çñ¤Îº¹³Û¡Ë
                                 WHEN   1
                                 WHEN   3
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                             (WRK-KHNTEN * 
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                                 + 0.5
                                     COMPUTE  NYUIN-TGMONEY  =
                                              NYUIN-TGMONEY  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                 WHEN   4
                                   IF  SYU-SRYYMD(1:6) >= "201004"
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  NYUIN-TGMONEY  =
                                              NYUIN-TGMONEY  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                   ELSE
                                     IF  SYU-SRYYMD(1:6) >= "200404"
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                    (WRK-KHNTEN * 0.30) + 0.5
                                              (WRK-KHNTEN * 0.00) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY  =
                                               NYUIN-TGMONEY  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     ELSE
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                              (WRK-KHNTEN * 0.15) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY  =
                                               NYUIN-TGMONEY  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     END-IF
                                   END-IF
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                 WHEN  17
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  NYUIN-TGMONEY  =
                                              NYUIN-TGMONEY  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                               END-EVALUATE
      *                   ¾ÃÈñÀÇ¤¢¤ê
                           WHEN   "1"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                     COMPUTE  WRK-KEISAN2 =
                                             (WRK-KHNTEN  -
                                              WRK-SENTEI-KHNTEN) + 0.5
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                             (WRK-KEISAN2 *
                                              SYS-5000-SENTEI-TANKA)
                                     IF  SYU-SRYYMD(1:6) >= "200704"
                                      COMPUTE  WRK-KEISAN =
                                              (WRK-KEISAN2           *
                                               SYS-5000-SENTEI-TANKA *
                                               WRK-S4) + WRK-S3
                                      COMPUTE  NYUIN-TGMONEY-TAX =
                                               NYUIN-TGMONEY-TAX +
                                               WRK-KEISAN
                                     END-IF
      *                          ³°Çñ¡Ê£±£µ¡ó¤ÈÁªÄê³°Çñ¤Îº¹³Û¡Ë
                                 WHEN   1
                                 WHEN   3
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                             (WRK-KHNTEN * 
                                              WRK-SENTEI-GAIHAKU-KEISU)
                                                                 + 0.5
                                     COMPUTE  NYUIN-TGMONEY-TAX  =
                                              NYUIN-TGMONEY-TAX  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                     IF  SYU-SRYYMD(1:6) >= "200704"
                                      COMPUTE  WRK-KEISAN =
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)  *
                                               SYS-5000-SENTEI-TANKA *
                                               WRK-S4) + WRK-S3
                                      COMPUTE  NYUIN-TGMONEY-TAX =
                                               NYUIN-TGMONEY-TAX +
                                               WRK-KEISAN
                                     END-IF
                                 WHEN   4
                                   IF  SYU-SRYYMD(1:6) >= "201004"
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  NYUIN-TGMONEY-TAX  =
                                              NYUIN-TGMONEY-TAX  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                     COMPUTE  WRK-KEISAN =
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)  *
                                              SYS-5000-SENTEI-TANKA *
                                              WRK-S4) + WRK-S3
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                              WRK-KEISAN
                                   ELSE
                                     IF  SYU-SRYYMD(1:6) >= "200404"
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                    (WRK-KHNTEN * 0.30) + 0.5
                                              (WRK-KHNTEN * 0.00) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY-TAX  =
                                               NYUIN-TGMONEY-TAX  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                      IF  SYU-SRYYMD(1:6) >= "200704"
                                       COMPUTE  WRK-KEISAN =
                                              ((WRK-SENTEI-GAIHAKU1 -
                                                WRK-SENTEI-GAIHAKU2)  *
                                                SYS-5000-SENTEI-TANKA *
                                                WRK-S4) + WRK-S3
                                       COMPUTE  NYUIN-TGMONEY-TAX =
                                                NYUIN-TGMONEY-TAX +
                                                WRK-KEISAN
                                      END-IF
                                     ELSE
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                      MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                      COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                              (WRK-KHNTEN * 0.15) + 0.5
                                      COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                              (WRK-KHNTEN * 
                                               WRK-SENTEI-GAIHAKU-KEISU)
                                                                  + 0.5
                                      COMPUTE  NYUIN-TGMONEY-TAX  =
                                               NYUIN-TGMONEY-TAX  +
                                             ((WRK-SENTEI-GAIHAKU1 -
                                               WRK-SENTEI-GAIHAKU2)
                                               *  SYS-5000-SENTEI-TANKA)
                                     END-IF
                                   END-IF
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                 WHEN  17
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                     MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                     COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                             (WRK-KHNTEN * 0.15) + 0.5
                                     COMPUTE  NYUIN-TGMONEY-TAX  =
                                              NYUIN-TGMONEY-TAX  +
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)
                                              *  SYS-5000-SENTEI-TANKA)
                                     COMPUTE  WRK-KEISAN =
                                            ((WRK-SENTEI-GAIHAKU1 -
                                              WRK-SENTEI-GAIHAKU2)  *
                                              SYS-5000-SENTEI-TANKA *
                                              WRK-S4) + WRK-S3
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                              WRK-KEISAN
                               END-EVALUATE
                       END-EVALUATE
      *            °åÎÅµ¡´ØÄê³ÛÀßÄê
                   WHEN   "2"
      *                 IF     SYS-5000-SENTEI-KINGAKU-X   =   SPACE
      *                     MOVE   ZERO    TO  SYS-5000-SENTEI-KINGAKU
      *                 END-IF
      *-------------------------------------------------------
                       IF  (SYS-5000-SENTEI-KINGAKU-X NOT = SPACE) AND
                           (SYS-5000-SENTEI-KINGAKU-X NOT = "0000")
                           CONTINUE
                       ELSE
                         IF   AGECHK-ROUJIN2    =   0
                           IF  SYS-5000-SENTEI-IPN-KINGAKU-X  =  SPACE
                               MOVE   ZERO TO  SYS-5000-SENTEI-KINGAKU
                           ELSE
                               MOVE   SYS-5000-SENTEI-IPN-KINGAKU
                                           TO  SYS-5000-SENTEI-KINGAKU
                           END-IF
                         ELSE
                           IF  SYS-5000-SENTEI-RJN-KINGAKU-X  =  SPACE
                               MOVE  ZERO  TO  SYS-5000-SENTEI-KINGAKU
                           ELSE
                               MOVE  SYS-5000-SENTEI-RJN-KINGAKU
                                           TO  SYS-5000-SENTEI-KINGAKU
                           END-IF
                         END-IF
                       END-IF
      *-------------------------------------------------------
                       EVALUATE   SYS-5000-SENTEI-TAXKBN
      *                   ¾ÃÈñÀÇ¤Ê¤·
                           WHEN   SPACE
                           WHEN   "0"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                 WHEN   1
                                 WHEN   3
                                 WHEN   4
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                 WHEN  17
                                     COMPUTE  NYUIN-TGMONEY =
                                              NYUIN-TGMONEY +
                                              SYS-5000-SENTEI-KINGAKU
                               END-EVALUATE
      *                   ¾ÃÈñÀÇ¤¢¤ê
                           WHEN   "1"
                               EVALUATE   TBL-GAIHAKU(SYU-IDX)
                                 WHEN   ZERO
                                 WHEN   1
                                 WHEN   3
                                 WHEN   4
                                 WHEN   5
                                 WHEN   6
                                 WHEN   7
                                 WHEN   8
                                 WHEN   9
      ***                        WHEN  11
                                 WHEN  13
                                 WHEN  14
                                 WHEN  15
                                 WHEN  16
                                 WHEN  17
                                     COMPUTE  NYUIN-TGMONEY-TAX =
                                              NYUIN-TGMONEY-TAX +
                                              SYS-5000-SENTEI-KINGAKU
                                     IF  SYU-SRYYMD(1:6) >= "200704"
                                      COMPUTE WRK-KEISAN =
                                             (SYS-5000-SENTEI-KINGAKU *
                                              WRK-S4) + WRK-S3
                                      COMPUTE  NYUIN-TGMONEY-TAX =
                                               NYUIN-TGMONEY-TAX +
                                               WRK-KEISAN
                                     END-IF
                               END-EVALUATE
                       END-EVALUATE
      *            ÉÂÅïÀßÄê
                   WHEN   "3"
                       PERFORM   SENTEI-PTFTN-BYOTO-SEC
               END-EVALUATE
           END-IF
           .
       SENTEI-PTFTN-EXT.
           EXIT.
      *
      **************************************************************
      *    ÁªÄê´ü´Ö¤Î´µ¼Ô¼«¸ÊÉéÃ´³Û»»Äê¡ÊÉÂÅïÀßÄê¡Ë
      **************************************************************
       SENTEI-PTFTN-BYOTO-SEC           SECTION.
      *
           EVALUATE   SBTUSENTEI-DAY-FTNKBN(SYU-IDX)
      *        ÉÂÅïÄêÎ¨ÀßÄê
               WHEN   "1"
                   EVALUATE   SBTUSENTEI-DAY-TAXKBN(SYU-IDX)
      *              ¾ÃÈñÀÇ¤Ê¤·
                       WHEN   SPACE
                       WHEN   "0"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                                   COMPUTE  WRK-KEISAN2 =
                                           (WRK-KHNTEN  -
                                            WRK-SENTEI-KHNTEN) + 0.5
                                   COMPUTE  NYUIN-TGMONEY =
                                            NYUIN-TGMONEY +
                                           (WRK-KEISAN2 *
                                            SBTUSENTEI-DAY-TANKA
                                                            (SYU-IDX))
      *                      ³°Çñ¡Ê£±£µ¡ó¤ÈÁªÄê³°Çñ¤Îº¹³Û¡Ë
                             WHEN   1
                             WHEN   3
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                         (WRK-KHNTEN * 
                                          WRK-SENTEI-GAIHAKU-KEISU)
                                                             + 0.5
                                 COMPUTE  NYUIN-TGMONEY  =
                                          NYUIN-TGMONEY  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)  *
                                         SBTUSENTEI-DAY-TANKA(SYU-IDX))
                             WHEN   4
                               IF  SYU-SRYYMD(1:6) >= "201004"
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  NYUIN-TGMONEY  =
                                          NYUIN-TGMONEY  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)  *
                                         SBTUSENTEI-DAY-TANKA(SYU-IDX))
                               ELSE
                                 IF  SYU-SRYYMD(1:6) >= "200404"
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                (WRK-KHNTEN * 0.30) + 0.5
                                          (WRK-KHNTEN * 0.00) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY  =
                                           NYUIN-TGMONEY  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)  *
                                          SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 ELSE
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                          (WRK-KHNTEN * 0.15) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY  =
                                           NYUIN-TGMONEY  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)  *
                                          SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 END-IF
                               END-IF
                             WHEN   5
                             WHEN   6
                             WHEN   7
                             WHEN   8
                             WHEN   9
      ***                    WHEN  11
                             WHEN  13
                             WHEN  14
                             WHEN  15
                             WHEN  16
                             WHEN  17
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  NYUIN-TGMONEY  =
                                          NYUIN-TGMONEY  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)  *
                                         SBTUSENTEI-DAY-TANKA(SYU-IDX))
                           END-EVALUATE
      *               ¾ÃÈñÀÇ¤¢¤ê
                       WHEN   "1"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                                 COMPUTE  WRK-KEISAN2 =
                                         (WRK-KHNTEN  -
                                          WRK-SENTEI-KHNTEN) + 0.5
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                         (WRK-KEISAN2 *
                                          SBTUSENTEI-DAY-TANKA
                                                          (SYU-IDX))
                                 IF  SYU-SRYYMD(1:6) >= "200704"
                                  COMPUTE  WRK-KEISAN =
                                          (WRK-KEISAN2               *
                                           SBTUSENTEI-DAY-TANKA
                                                           (SYU-IDX) *
                                           WRK-S4) + WRK-S3
                                  COMPUTE  NYUIN-TGMONEY-TAX =
                                           NYUIN-TGMONEY-TAX +
                                           WRK-KEISAN
                                 END-IF
      *                      ³°Çñ¡Ê£±£µ¡ó¤ÈÁªÄê³°Çñ¤Îº¹³Û¡Ë
                             WHEN   1
                             WHEN   3
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                         (WRK-KHNTEN * 
                                          WRK-SENTEI-GAIHAKU-KEISU)
                                                             + 0.5
                                 COMPUTE  NYUIN-TGMONEY-TAX  =
                                          NYUIN-TGMONEY-TAX  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)
                                      *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 IF  SYU-SRYYMD(1:6) >= "200704"
                                  COMPUTE  WRK-KEISAN =
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)      *
                                       SBTUSENTEI-DAY-TANKA(SYU-IDX) *
                                           WRK-S4) + WRK-S3
                                  COMPUTE  NYUIN-TGMONEY-TAX =
                                           NYUIN-TGMONEY-TAX +
                                           WRK-KEISAN
                                 END-IF
                             WHEN   4
                               IF  SYU-SRYYMD(1:6) >= "201004"
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  NYUIN-TGMONEY-TAX  =
                                          NYUIN-TGMONEY-TAX  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)
                                      *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 COMPUTE  WRK-KEISAN =
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)      *
                                      SBTUSENTEI-DAY-TANKA(SYU-IDX) *
                                          WRK-S4) + WRK-S3
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                          WRK-KEISAN
                               ELSE
                                 IF  SYU-SRYYMD(1:6) >= "200404"
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
      ****                                (WRK-KHNTEN * 0.30) + 0.5
                                          (WRK-KHNTEN * 0.00) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY-TAX  =
                                           NYUIN-TGMONEY-TAX  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)
                                       *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                  IF  SYU-SRYYMD(1:6) >= "200704"
                                   COMPUTE  WRK-KEISAN =
                                          ((WRK-SENTEI-GAIHAKU1 -
                                            WRK-SENTEI-GAIHAKU2)      *
                                        SBTUSENTEI-DAY-TANKA(SYU-IDX) *
                                            WRK-S4) + WRK-S3
                                   COMPUTE  NYUIN-TGMONEY-TAX =
                                            NYUIN-TGMONEY-TAX +
                                            WRK-KEISAN
                                  END-IF
                                 ELSE
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                  MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                  COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                          (WRK-KHNTEN * 0.15) + 0.5
                                  COMPUTE  WRK-SENTEI-GAIHAKU2 =
                                          (WRK-KHNTEN * 
                                           WRK-SENTEI-GAIHAKU-KEISU)
                                                              + 0.5
                                  COMPUTE  NYUIN-TGMONEY-TAX  =
                                           NYUIN-TGMONEY-TAX  +
                                         ((WRK-SENTEI-GAIHAKU1 -
                                           WRK-SENTEI-GAIHAKU2)
                                       *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 END-IF
                               END-IF
                             WHEN   5
                             WHEN   6
                             WHEN   7
                             WHEN   8
                             WHEN   9
      ***                    WHEN  11
                             WHEN  13
                             WHEN  14
                             WHEN  15
                             WHEN  16
                             WHEN  17
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU1
                                 MOVE  ZERO TO WRK-SENTEI-GAIHAKU2
                                 COMPUTE  WRK-SENTEI-GAIHAKU1 =
                                         (WRK-KHNTEN * 0.15) + 0.5
                                 COMPUTE  NYUIN-TGMONEY-TAX  =
                                          NYUIN-TGMONEY-TAX  +
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)
                                      *  SBTUSENTEI-DAY-TANKA(SYU-IDX))
                                 COMPUTE  WRK-KEISAN =
                                        ((WRK-SENTEI-GAIHAKU1 -
                                          WRK-SENTEI-GAIHAKU2)      *
                                      SBTUSENTEI-DAY-TANKA(SYU-IDX) *
                                          WRK-S4) + WRK-S3
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                          WRK-KEISAN
                           END-EVALUATE
                   END-EVALUATE
      *        ÉÂÅïÄê³ÛÀßÄê
               WHEN   "2"
                   EVALUATE   SBTUSENTEI-DAY-TAXKBN(SYU-IDX)
      *               ¾ÃÈñÀÇ¤Ê¤·
                       WHEN   SPACE
                       WHEN   "0"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                             WHEN   1
                             WHEN   3
                             WHEN   4
                             WHEN   5
                             WHEN   6
                             WHEN   7
                             WHEN   8
                             WHEN   9
      ***                    WHEN  11
                             WHEN  13
                             WHEN  14
                             WHEN  15
                             WHEN  16
                             WHEN  17
                                 COMPUTE  NYUIN-TGMONEY =
                                          NYUIN-TGMONEY +
                                       SBTUSENTEI-DAY-KINGAKU(SYU-IDX)
                           END-EVALUATE
      *               ¾ÃÈñÀÇ¤¢¤ê
                       WHEN   "1"
                           EVALUATE   TBL-GAIHAKU(SYU-IDX)
                             WHEN   ZERO
                             WHEN   1
                             WHEN   3
                             WHEN   4
                             WHEN   5
                             WHEN   6
                             WHEN   7
                             WHEN   8
                             WHEN   9
      ***                    WHEN  11
                             WHEN  13
                             WHEN  14
                             WHEN  15
                             WHEN  16
                             WHEN  17
                                 COMPUTE  NYUIN-TGMONEY-TAX =
                                          NYUIN-TGMONEY-TAX +
                                       SBTUSENTEI-DAY-KINGAKU(SYU-IDX)
                                 IF  SYU-SRYYMD(1:6) >= "200704"
                                  COMPUTE  WRK-KEISAN =
                                     (SBTUSENTEI-DAY-KINGAKU(SYU-IDX) *
                                           WRK-S4) + WRK-S3
                                  COMPUTE  NYUIN-TGMONEY-TAX =
                                           NYUIN-TGMONEY-TAX +
                                           WRK-KEISAN
                                 END-IF
                           END-EVALUATE
                   END-EVALUATE
           END-EVALUATE
           .
       SENTEI-PTFTN-BYOTO-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ²ñ·×¥»¥Ã¥È½èÍý                                *
      ******************************************************************
       SINRYO-KAIKEI-SET-SEC         SECTION.
      *
           MOVE    LNK-SYUNOU-REC(IDX) TO  SYUNOU-REC
           MOVE    LNK-SYUDAY-REC(IDX) TO  SYUDAY-REC                  
      *
           PERFORM      GAIHAKU-HENSYU-SEC
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý£´£°£°£±¤«¤é¼«ÇåÀÕ¸ÇÄêÂÓ²Ã»»Åù¼è°·¤òÆÉ¤ß¹þ¤à
           MOVE    SPACE     TO    WRK-4001-KOTEIKBN
           IF   SYU-SYUHKNNUM    =    "973"
              INITIALIZE                         SYS-4001-REC
              MOVE    LNK-S02-HOSPNUM      TO    SYS-4001-HOSPNUM
              MOVE    "4001"               TO    SYS-4001-KANRICD
              MOVE    "*"                  TO    SYS-4001-KBNCD
              MOVE    SYU-SRYYMD           TO    SYS-4001-STYUKYMD
                                                 SYS-4001-EDYUKYMD
              MOVE    SYS-4001-REC         TO    MCPDATA-REC
              MOVE    "DBSELECT"           TO    MCP-FUNC
              MOVE    "tbl_syskanri"       TO    MCP-TABLE
              MOVE    "key10"              TO    MCP-PATHNAME
grpsys        CALL "ORCDBMAIN" USING MCPAREA
                                     MCPDATA-REC
grpsys                               SPA-AREA
              IF      MCP-RC               =     ZERO
                  MOVE    "tbl_syskanri"   TO    MCP-TABLE
                  MOVE    "key10"          TO    MCP-PATHNAME
                  PERFORM   SYSKANRI-READ-SEC
                  MOVE      SYS-4001-KOTEIKBN
                                           TO    WRK-4001-KOTEIKBN
              END-IF
      *
              MOVE    "tbl_syskanri"   TO    MCP-TABLE
              MOVE    "key10"          TO    MCP-PATHNAME
              PERFORM   CSR-CLOSE-SEC
           END-IF
      *
           MOVE    SPACE               TO  HKNTEKGAI-TABLE
           INITIALIZE                      HKNTEKGAI-TABLE
           MOVE    SPACE               TO  JIHI-TABLE
           INITIALIZE                      JIHI-TABLE
           MOVE    SPACE               TO  WRK-SKYTEN-TABLE
           INITIALIZE                      WRK-SKYTEN-TABLE
      *
           INITIALIZE                      WRK-ROSAI-AREA
      *
           MOVE    ZERO                TO  IDX3
      *
      *
      *Ê¡²¬¸©ÂÐ±þ¤ÎÍ×¡¿ÉÔÍ×¤òÈ½Äê
           PERFORM     FUKUOKA-HANTEI-SEC
      *
      *¿ÇÎÅ²ñ·×¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    SYU-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  ACCT-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key30"         TO  MCP-PATHNAME
               PERFORM    SRYACCT-READ-SEC
      *
               PERFORM    SRYACCT-SET-SEC
                      UNTIL   FLG-SRYACCT  =  1
      *
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key30"         TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
      **   DISPLAY  "SYU-KYUFUGAI-SHOSHIN-TEN = " 
      **             SYU-KYUFUGAI-SHOSHIN-TEN
      **   DISPLAY  "SYU-KYUFUGAI-GOKEI-TEN = " 
      **             SYU-KYUFUGAI-GOKEI-TEN
      *
      *    ÅÀÅ©ÌôºÞ¡Ê´ïºà¡ËÅÀ¿ô¤Þ¤È¤á½èÍý
           PERFORM   TENTEK-MATOME-SEC
      *
           EVALUATE   SYU-SYUHKNNUM
             WHEN      "971"
             WHEN      "973"
      *        Ï«ºÒ¡¦¼«ÇåÀÕÅ¾Á÷½èÍý
               PERFORM   ROSAI-JIBAI-TENSO-SEC
             WHEN      "975"
      *        ¸ø³²Å¾Á÷½èÍý
               PERFORM   KOGAI-TENSO-SEC
             WHEN       OTHER
      *        ¤½¤ÎÂ¾ÊÝ¸±Å¾Á÷½èÍý
               PERFORM   ETCHKN-TENSO-SEC
           END-EVALUATE
      *
      *    ÊÝ¸±ÅÀ¿ô½¸·×
           MOVE     ZERO    TO   WRK-SKYCHK-KEISAN
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL   IDZ    >  16
             ADD    SYU-HKNTEN(IDZ)   TO  SYU-HKNTEN(17)
                                          WRK-SKYCHK-KEISAN
           END-PERFORM
           IF   WRK-SKYCHK-KEISAN  >  9999999
             MOVE   1       TO   FLG-SKYCHK-KEISAN
           END-IF
           IF   WRK-SKYTEN(07)     >  9999999
             MOVE   1       TO   FLG-SKYCHK-KEISAN
           END-IF
      *
      *    ÊÝ¸±Å¬ÍÑ³°ÂàÈò¥Æ¡¼¥Ö¥ë¤È¼«ÈñÂàÈòÍÑ¥Æ¡¼¥Ö¥ë¤«¤é
      *    ¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Ø¤Î¥»¥Ã¥È
           PERFORM   HKNTEKGAI-SET-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU-REC(IDX)
           MOVE    SYUDAY-REC          TO  LNK-SYUDAY-REC(IDX)         
      *
           .
      *
       SINRYO-KAIKEI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    Ê¡²¬¸©ÂÐ±þ¤ÎÍ×¡¿ÉÔÍ×¤òÈ½Äê                                  *
      ******************************************************************
       FUKUOKA-HANTEI-SEC                     SECTION.
      *
           INITIALIZE                         FLG-KYUFUGAI
           INITIALIZE                         TBL-FUKUOKA
      *
      *    ¸©ÈÖ¹æ¤Î¼èÆÀ
           INITIALIZE                         SYS-1001-REC
           MOVE    LNK-S02-HOSPNUM      TO    SYS-1001-HOSPNUM
           MOVE    "1001"               TO    SYS-1001-KANRICD
           MOVE    "*"                  TO    SYS-1001-KBNCD
           MOVE    SYU-SRYYMD           TO    SYS-1001-STYUKYMD
                                              SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE    "tbl_syskanri"   TO    MCP-TABLE
               MOVE    "key10"          TO    MCP-PATHNAME
               PERFORM   SYSKANRI-READ-SEC
      *         MOVE      SYS-1001-PREFNUM
      *                                  TO    WK-1001-PREFNUM
      *         MOVE      SYS-1001-HOSPSBT
      *                                  TO    WK-1001-HOSPSBT
           END-IF
      *
           MOVE    "tbl_syskanri"   TO    MCP-TABLE
           MOVE    "key10"          TO    MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           IF   SYS-1001-PREFNUM        =   40
      **       DISPLAY   "FUKUOKA TAISHOU !!"
      **       DISPLAY   "SYU-HKNCOMBINUM = " SYU-HKNCOMBINUM
      **       DISPLAY   "SYU-KOH1HKNNUM = " SYU-KOH1HKNNUM
      *        ¸øÈñÉÕ²Ã¾ðÊó¤«¤éµëÉÕ³°ÂÐ¾Ý¤Î¸øÈñ¤ò»ý¤Ä´µ¼Ô¤Ç¤¢¤ë¤«È½Äê
               INITIALIZE                   ORCSKOHPLUS-AREA
               MOVE    SYS-1001-HOSPNUM  TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM TO  LNK-SKOHPLUS-PREFNUM
               MOVE    SYU-PTID         TO  LNK-SKOHPLUS-PTID
               MOVE    SYU-HKNCOMBINUM  TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    SYU-SRYYMD       TO  LNK-SKOHPLUS-SRYYMD
               CALL    "ORCSKOHPLUS"        USING
                                            ORCSKOHPLUS-AREA
                                            SPA-AREA
      *           
               IF    ( LNK-SKOHPLUS-RC     =   ZERO )
               AND   ( LNK-SKOHPLUS-FTNKBN =   "1"  )
                   MOVE    1            TO  FLG-KYUFUGAI
      **           DISPLAY "FLG-KYUFUGAI = " FLG-KYUFUGAI
               END-IF
      *
      *        Åö³º´µ¼Ô¤Î½é¿Ç»»ÄêÍúÎò¤ò¥ï¡¼¥¯¥Æ¡¼¥Ö¥ë¤ËÀßÄê
               IF   FLG-KYUFUGAI     =     1
                   PERFORM   VARYING  FUKU-IDX    FROM   1  BY  1
                             UNTIL    FUKU-IDX    >   TBL-SYOSIN2-MAX
                       MOVE        SPACE        TO  SANTEI-REC
                       INITIALIZE                   SANTEI-REC
                       MOVE    SYU-HOSPNUM       TO  SANTEI-HOSPNUM
                       MOVE    SYU-PTID         TO  SANTEI-PTID
                       MOVE    SYU-SRYYMD(1:6)  TO  SANTEI-SRYYM
                       MOVE    TBL-SYOSIN2(FUKU-IDX)
                                                TO  SANTEI-SRYCD
                       MOVE    ZERO             TO  SANTEI-HKNCOMBINUM
                       MOVE    SANTEI-REC       TO  MCPDATA-REC
                       MOVE    "DBSELECT"       TO  MCP-FUNC
                       MOVE    "tbl_santei"     TO  MCP-TABLE
                       MOVE    "key7"           TO  MCP-PATHNAME
grpsys                 CALL "ORCDBMAIN" USING MCPAREA
                                              MCPDATA-REC
grpsys                                        SPA-AREA
                       IF      MCP-RC           =   ZERO
                         MOVE    "tbl_santei"     TO  MCP-TABLE
                         MOVE    "key7"           TO  MCP-PATHNAME
                         PERFORM  SANTEI-READ-SEC
                         IF    FLG-SANTEI             =  ZERO
                          IF   SANTEI-MSANTEICNT  NOT =  ZERO
                           PERFORM VARYING FUKU-IDX2 FROM 1 BY 1
                                    UNTIL  FUKU-IDX2  >  31
                            IF  SANTEI-DAY(FUKU-IDX2) NOT =  ZERO
      ***                     ADD  SANTEI-DAY(FUKU-IDX2)
      ***                          TO  TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                              IF TBL-SYOSIN2(FUKU-IDX) = "111011810"
                                                      OR "111012610"
                                                      OR "111012810"
                                 EVALUATE TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    0
                                     MOVE  2  TO
                                          TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    1
                                     MOVE  3  TO
                                          TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    2
                                   WHEN    3
                                     CONTINUE
                                 END-EVALUATE
                              ELSE
                                 EVALUATE TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    0
                                     MOVE  1  TO
                                          TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    2
                                     MOVE  3  TO
                                          TBL-FUKUOKA-SYOSIN(FUKU-IDX2)
                                   WHEN    1
                                   WHEN    3
                                     CONTINUE
                                 END-EVALUATE
                              END-IF
                            END-IF
                           END-PERFORM
                          END-IF
                         END-IF
                       END-IF
                       MOVE      "tbl_santei"     TO  MCP-TABLE
                       MOVE      "key7"           TO  MCP-PATHNAME
                       PERFORM   CSR-CLOSE-SEC
                   END-PERFORM
               END-IF
      **       PERFORM   VARYING  FUKU-IDX   FROM 1  BY  1
      **                 UNTIL    FUKU-IDX   >  31
      **           DISPLAY  "FUKUOKA SHOSHIN = " 
      **                     TBL-FUKUOKA-SYOSIN(FUKU-IDX)
      **                    " IDX = " FUKU-IDX 
      **       END-PERFORM
           END-IF
           .
      *
       FUKUOKA-HANTEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ²ñ·×¥»¥Ã¥È½èÍý                                *
      ******************************************************************
       SRYACCT-SET-SEC         SECTION.
      *
      *    Æþ±¡Æü¤È¿ÇÎÅÆü¡Ê¿ÇÎÅ²ñ·×¥Æ¡¼¥Ö¥ë¤Î¥«¥ì¥ó¥À¡¼¡Ë¤òÈæ³Ó¤·¤Æ
      *    »»ÄêÂÐ¾ÝÆü¿ô¤òµá¤á¤ë
           MOVE      ZERO          TO     SINRYO-COUNT
           INITIALIZE                     TABLE-ZAIKAISU               
           MOVE   ZERO             TO     WRK-CHECK-STDAY
           MOVE   ZERO             TO     WRK-CHECK-EDDAY
           MOVE   ZERO             TO     FLG-NYUINRYO-KASAN-OK
           MOVE   ZERO             TO     FLG-NYUINRYO-KASAN2-OK
           MOVE   ZERO             TO     FLG-NYUINRYO-KASAN-CHK
           IF   ACCT-SRYKBN  =  "90"  OR  "92"
             PERFORM   VARYING   SYU-IDX   FROM   1   BY   1
                     UNTIL      SYU-IDX   >   31
                 IF  ((SYU-DAY(SYU-IDX)                =   "1") OR
                      (WRK-NYUINDAY-KBN(IDX SYU-IDX)   =   "1"))
                 AND ( ACCT-DAY(1 SYU-IDX)         NOT =  ZERO )
                 AND ( SYU-HKNCOMBINUM       =   ACCT-HKNCOMBI )
                 AND ((TBL-GAIHAKU(SYU-IDX)            =  ZERO) OR
                      (TBL-GAIHAKU(SYU-IDX)           >=    4 ))
                 AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1    )
                     EVALUATE  WRK-DOJITU-FLG(IDX SYU-IDX)
                       WHEN      0
                         ADD    ACCT-DAY(1 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(1 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                       WHEN      1
                         ADD    ACCT-DAY(2 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(2 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                     END-EVALUATE
                 ELSE
                   IF  ( ACCT-DAY(1 SYU-IDX)         NOT =  ZERO )
                   AND ( SYU-HKNCOMBINUM       =   ACCT-HKNCOMBI )
                   AND ((TBL-GAIHAKU(SYU-IDX)            =  ZERO) OR
                        (TBL-GAIHAKU(SYU-IDX)           >=    4 ))
                   AND ( WRK-DOJITU-FLG(IDX SYU-IDX) = 0 OR 1    )
                     MOVE   SYU-SKYSTYMD(7:2)  TO  WRK-CHECK-STDAY
                     MOVE   SYU-SKYEDYMD(7:2)  TO  WRK-CHECK-EDDAY
                     IF     (WRK-CHECK-STDAY  <=  SYU-IDX        )
                     AND    (SYU-IDX          <=  WRK-CHECK-EDDAY)
                       EVALUATE  WRK-DOJITU-FLG(IDX SYU-IDX)
                         WHEN      0
                           ADD    ACCT-DAY(1 SYU-IDX) TO  SINRYO-COUNT
                           IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             MOVE ACCT-DAY(1 SYU-IDX)
                                              TO  TBL-ZAIKAISU(SYU-IDX)
                           END-IF
                         WHEN      1
                           ADD    ACCT-DAY(2 SYU-IDX) TO  SINRYO-COUNT
                           IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             MOVE ACCT-DAY(2 SYU-IDX)
                                              TO  TBL-ZAIKAISU(SYU-IDX)
                           END-IF
                       END-EVALUATE
                     END-IF
                   END-IF
                 END-IF
                 IF   FLG-NYUINRYO-KASAN-CHK =  ZERO
                    MOVE   ACCT-HKNCOMBI   TO   WRK-HKNCOMBINUM
                    PERFORM  HKNCOMBI-KENSAKU-SEC
                    IF  COMB-HKNNUM  =  "971" OR "973" OR "975"
                      IF  COMB-HKNNUM  =  "971" OR "973"
                         PERFORM   NYUINRYO-KASAN-CHECK2-SEC
                      END-IF
                    ELSE
                      PERFORM   NYUINRYO-KASAN-CHECK-SEC
                    END-IF
                    MOVE    1    TO   FLG-NYUINRYO-KASAN-CHK
                 END-IF
                 IF  ((WRK-DOJITU-FLG(IDX SYU-IDX) =  0 OR 1 OR 7) AND
                      (ACCT-DAY(2 SYU-IDX)     NOT =  ZERO       ))
                 OR  ((WRK-DOJITU-FLG(IDX SYU-IDX) =  8 OR 9     ) AND
                      (ACCT-DAY(3 SYU-IDX)     NOT =  ZERO       ))
                   IF  (SYU-DAY(SYU-IDX)                =   "1")
                   OR  (WRK-NYUINDAY-KBN(IDX SYU-IDX)   =   "1")
                     IF  (TBL-GAIHAKU(SYU-IDX)  =  ZERO)
                     OR  (TBL-GAIHAKU(SYU-IDX) >=    4 )
                       CONTINUE
                     ELSE
                       IF  ACCT-ZAITEN  NOT = ZERO
                            MOVE    2     TO    WRK-KAIKEING(IDX)
                       END-IF
                     END-IF
                     IF  SYU-HKNCOMBINUM = ACCT-HKNCOMBI
                       CONTINUE
                     ELSE
                       IF  ACCT-ZAITEN  NOT = ZERO
                         IF  (WRK-KAIKEING(IDX)  = ZERO OR 3)
                         AND (FLG-NYUINRYO-KASAN-OK  =  ZERO)
                         AND (FLG-NYUINRYO-KASAN2-OK =  ZERO)
                            MOVE    1     TO    WRK-KAIKEING(IDX)
                         END-IF
                       END-IF
                     END-IF
                   ELSE
                     MOVE   SYU-SKYSTYMD(7:2)  TO  WRK-CHECK-STDAY
                     MOVE   SYU-SKYEDYMD(7:2)  TO  WRK-CHECK-EDDAY
                     IF     (WRK-CHECK-STDAY  <=  SYU-IDX        )
                     AND    (SYU-IDX          <=  WRK-CHECK-EDDAY)
                      IF  ( SYU-HKNCOMBINUM      =  ACCT-HKNCOMBI )
                      OR  ((SYU-HKNCOMBINUM  NOT =  ACCT-HKNCOMBI) AND
                           (WRK-HANBETU-DAY-NACCT(IDX SYU-IDX)
                                                      NOT = SPACE))
                      OR  ((SYU-HKNCOMBINUM  NOT =  ACCT-HKNCOMBI) AND
                           (WRK-HANBETU-DAY-NACCT(IDX SYU-IDX)
                                                          = SPACE) AND
                           (WRK-DOJITU-FLG(IDX SYU-IDX) =  9     ))
                       IF  ACCT-ZAITEN  NOT = ZERO
                         IF  (( FLG-NYUINRYO-KASAN-OK  =  ZERO    )  OR
                              ( WRK-HANBETU-DAY-NACCT(IDX SYU-IDX)
                                                NOT = SPACE       )  OR
                              ((WRK-HANBETU-DAY-NACCT(IDX SYU-IDX)
                                                    = SPACE      ) AND
                               (WRK-DOJITU-FLG(IDX SYU-IDX) =  9 )))
                         AND (  FLG-NYUINRYO-KASAN2-OK =  ZERO     )
                           IF   WRK-DOJITU-FLG(IDX SYU-IDX) =  8 OR 9
                             IF  WRK-KAIKEING(IDX)  =  ZERO OR 1 OR 3
                               MOVE    4     TO    WRK-KAIKEING(IDX)
                             END-IF
                           ELSE
                             IF  WRK-KAIKEING(IDX)  =  ZERO
                               MOVE    3     TO    WRK-KAIKEING(IDX)
                             END-IF
                           END-IF
                         END-IF
                       END-IF
                      END-IF
                     END-IF
                   END-IF
                 END-IF
                 IF   (WRK-DOJITU-FLG(IDX SYU-IDX) =  0          )
                 AND  (ACCT-DAY(3 SYU-IDX)     NOT =  ZERO       )
                   MOVE   SYU-SKYSTYMD(7:2)  TO  WRK-CHECK-STDAY
                   MOVE   SYU-SKYEDYMD(7:2)  TO  WRK-CHECK-EDDAY
                   IF     (WRK-CHECK-STDAY  <=  SYU-IDX        )
                   AND    (SYU-IDX          <=  WRK-CHECK-EDDAY)
                     IF  ACCT-ZAITEN  NOT = ZERO
                       IF  (WRK-KAIKEING(IDX)      =  ZERO OR 1 OR 3)
                       AND (FLG-NYUINRYO-KASAN2-OK =  ZERO          )
                          MOVE    4     TO    WRK-KAIKEING(IDX)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE   SYU-SKYSTYMD(7:2)  TO  WRK-CHECK-STDAY
                 MOVE   SYU-SKYEDYMD(7:2)  TO  WRK-CHECK-EDDAY
                 IF   (WRK-NYUINDAY-KYUKYU-KBN(IDX SYU-IDX)  =  "1")
                 AND  (ACCT-DAY(1 SYU-IDX)          NOT =  ZERO    )
                 AND  (ACCT-ZAITEN                  NOT =  ZERO    )
                 AND  (WRK-CHECK-STDAY        <=    SYU-IDX        )
                 AND  (SYU-IDX                <=    WRK-CHECK-EDDAY)
                        MOVE    1    TO    WRK-KYUKYUHANSO-ERR
                 END-IF
             END-PERFORM
           ELSE
             PERFORM   VARYING   SYU-IDX   FROM   1   BY   1
                     UNTIL      SYU-IDX   >   31
                 IF  (SYU-DAY(SYU-IDX) = "1" OR "2" OR "8" OR "9")
                 AND (ACCT-DAY(1 SYU-IDX)    NOT =  ZERO         )
                 AND (SYU-HKNCOMBINUM        =    ACCT-HKNCOMBI  )
                    IF  WRK-DOJITU-FLG2(IDX SYU-IDX) = 1
                         ADD    ACCT-DAY(1 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(1 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                    ELSE
                     EVALUATE  WRK-DOJITU-FLG(IDX SYU-IDX)
                       WHEN      0
                         ADD    ACCT-DAY(1 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(1 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                       WHEN      1
                       WHEN      7
                         ADD    ACCT-DAY(2 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(2 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                       WHEN      8
                       WHEN      9
                         ADD    ACCT-DAY(3 SYU-IDX) TO  SINRYO-COUNT
                         IF  SYU-SYUHKNNUM  = "971" OR "973" OR "975"
                           CONTINUE
                         ELSE
                           MOVE ACCT-DAY(3 SYU-IDX)
                                            TO  TBL-ZAIKAISU(SYU-IDX)
                         END-IF
                     END-EVALUATE
                    END-IF
                 END-IF
             END-PERFORM
           END-IF
      *
           IF  SINRYO-COUNT     NOT =     ZERO
      *
               EVALUATE   SYU-SYUHKNNUM
                 WHEN      "971"
                 WHEN      "973"
      *              Ï«ºÒ¡¦¼«ÇåÀÕÍÑ¤ÎÊÔ½¸½èÍý
                     PERFORM     ROSAI-JIBAI-HENSYU-SEC
                 WHEN      "975"
      *              ¸ø³²ÍÑ¤ÎÊÔ½¸½èÍý
                     PERFORM     KOGAI-HENSYU-SEC
                 WHEN       OTHER
                     IF   SYU-KOH1HKNNUM   =   "970"
                        PERFORM  DAISANSYA-HENSYU-SEC
                     END-IF
      *              ½é¿ÇÎÁ»»Äê¥Õ¥é¥°¡ÊÃÏÊý¸øÈñÍÑ¡Ë¤Î¥»¥Ã¥È
      *              ½é¿ÇÎÁ»»Äê¥Õ¥é¥°£²¡ÊÃÏÊý¸øÈñÍÑ¡Ë¤Î¥»¥Ã¥È
                     PERFORM     SYOSINFLG-SET-SEC
               END-EVALUATE
      *
               EVALUATE    ACCT-SRYKBN
      *            ¿ÇÎÅ
                   WHEN    "11"
                   WHEN    "12"
                    COMPUTE SYU-HKNTEN (01) =  SYU-HKNTEN (01)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
      *               Ê¡²¬µëÉÕ³°ÅÀ¿ô¤ÎÈ½Äê
                      IF    (FLG-KYUFUGAI     =     1)   AND
                            (ACCT-SRYKBN      =   "11")
                        IF   TBL-ZAIKAISU(SYU-IDX)  NOT = ZERO
                          IF  SYU-IDX       =     1
                            PERFORM   FUKUOKA-KYUFUGAI-HENSYU-SEC
                          ELSE
                            MOVE    ZERO    TO   FUKU-IDX
                            COMPUTE  FUKU-IDX  =  SYU-IDX  - 1
                            IF   TBL-FUKUOKA(1:FUKU-IDX)  =  ALL"0"
                               PERFORM   FUKUOKA-KYUFUGAI-HENSYU-SEC
                            END-IF
                          END-IF
                        END-IF
                      END-IF
                    END-PERFORM                                        
                    IF   ACCT-SRYKBN                =  "11"             
                    AND  ACCT-ZAITEN           NOT  =  ZERO             
                    AND  WRK-HANBETU-SENINFLG(IDX)  =   "1"             
                         MOVE   "1"   TO   WRK-HANBETU-SENINFLG2(IDX)   
                    END-IF                                              
      *            »ØÆ³
                   WHEN    "13"
                    IF   ACCT-ZAIKBN  =  2
                      COMPUTE WRK-SYUG-SDO-HKNTEN(IDX) =
                              WRK-SYUG-SDO-HKNTEN(IDX) +
                             (ACCT-ZAITEN  *   SINRYO-COUNT)
                      PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                 UNTIL    SYU-IDX   >    31
                         COMPUTE WRK-SDAYG-HKNTEN(IDX SYU-IDX) =
                                 WRK-SDAYG-HKNTEN(IDX SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                      END-PERFORM
                    ELSE
                      COMPUTE SYU-HKNTEN (02) =  SYU-HKNTEN (02)  +
                             (ACCT-ZAITEN  *   SINRYO-COUNT)
                      PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                                 UNTIL    SYU-IDX   >    31              
                         COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                                 SDAY-HKNTEN(SYU-IDX) +                  
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                      END-PERFORM                                        
                    END-IF
      *            ºßÂð
                   WHEN    "14"
                    IF   ACCT-ZAIKBN  =  2
                      COMPUTE WRK-SYUG-ZTK-HKNTEN(IDX) =
                              WRK-SYUG-ZTK-HKNTEN(IDX) +
                             (ACCT-ZAITEN  *   SINRYO-COUNT)
                      PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                 UNTIL    SYU-IDX   >    31
                         COMPUTE WRK-SDAYG-HKNTEN(IDX SYU-IDX) =
                                 WRK-SDAYG-HKNTEN(IDX SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                      END-PERFORM
                    ELSE
                      COMPUTE SYU-HKNTEN (03) =  SYU-HKNTEN (03)  +
                             (ACCT-ZAITEN  *   SINRYO-COUNT)
                      PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                                 UNTIL    SYU-IDX   >    31              
                         COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                                 SDAY-HKNTEN(SYU-IDX) +                  
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                      END-PERFORM                                        
                    END-IF
      *            ÅêÌô
                   WHEN    "21"   THRU  "27"
                   COMPUTE SYU-HKNTEN (04) =  SYU-HKNTEN (04)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            Ãí¼Í
                   WHEN    "31"
                   WHEN    "32"
                    COMPUTE SYU-HKNTEN (05) =  SYU-HKNTEN (05)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    COMPUTE WRK-CSY-YAKTEN(IDX) =
                            WRK-CSY-YAKTEN(IDX) +
                           (ACCT-YKZTEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            ÅÀÅ©Ãí¼ÍËô¤ÏÃæ¿´ÀÅÌ®Ãí¼Í
                   WHEN    "33"
                       PERFORM   TENTEK-GET-SEC
                       IF   FLG-SRYACT   =    ZERO
                           EVALUATE  SRY-SRYSYUKBN
                               WHEN  "330"
                               WHEN  "331"
                               WHEN  "332"
                                  COMPUTE SYU-HKNTEN (05) = 
                                          SYU-HKNTEN (05) + 
                                         (ACCT-SYUTEN1 * SINRYO-COUNT)
                                  PERFORM VARYING SYU-IDX FROM 1 BY 1  
                                          UNTIL    SYU-IDX  >  31      
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) +     
                                           (ACCT-SYUTEN1 *             
                                            TBL-ZAIKAISU(SYU-IDX))     
                                  END-PERFORM                          
                               WHEN  "340"
                                 IF   ACCT-ZAIKBN  =  2
                                  COMPUTE WRK-SYUG-CSY-HKNTEN(IDX) =
                                          WRK-SYUG-CSY-HKNTEN(IDX) +
                                         (ACCT-ZAITEN * SINRYO-COUNT)
                                  PERFORM VARYING SYU-IDX FROM 1 BY 1
                                          UNTIL    SYU-IDX  >  31
                                    COMPUTE WRK-SDAYG-HKNTEN
                                                     (IDX SYU-IDX) =
                                            WRK-SDAYG-HKNTEN
                                                     (IDX SYU-IDX) +
                                           (ACCT-ZAITEN  *
                                            TBL-ZAIKAISU(SYU-IDX))
                                  END-PERFORM
                                 ELSE
                                  COMPUTE SYU-HKNTEN (05) = 
                                          SYU-HKNTEN (05) + 
                                         (ACCT-ZAITEN * SINRYO-COUNT)
                                  COMPUTE WRK-CSY-YAKTEN(IDX) =
                                          WRK-CSY-YAKTEN(IDX) +
                                         (ACCT-YKZTEN * SINRYO-COUNT)
                                  PERFORM VARYING SYU-IDX FROM 1 BY 1  
                                          UNTIL    SYU-IDX  >  31      
                                    COMPUTE SDAY-HKNTEN(SYU-IDX) =     
                                            SDAY-HKNTEN(SYU-IDX) +     
                                           (ACCT-ZAITEN  *             
                                            TBL-ZAIKAISU(SYU-IDX))     
                                  END-PERFORM                          
                                 END-IF
                           END-EVALUATE
                       END-IF
      *            ½èÃÖ
                   WHEN    "40"
                    COMPUTE SYU-HKNTEN (06)   =  SYU-HKNTEN (06)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            ¼ê½Ñ
                   WHEN    "50"
                    COMPUTE SYU-HKNTEN (07)   =  SYU-HKNTEN (07)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    COMPUTE WRK-SKYTEN (07)   =  WRK-SKYTEN (07)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM
      *            Ëã¿ì
                   WHEN    "54"
                    COMPUTE SYU-HKNTEN (08)   =  SYU-HKNTEN (08)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            ¸¡ºº
                   WHEN    "60"
                    COMPUTE SYU-HKNTEN (09)   =  SYU-HKNTEN (09)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            ÉÂÍý¿ÇÃÇ
                   WHEN    "64"
                    COMPUTE SYU-HKNTEN (14)   =  SYU-HKNTEN (14)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            £ØÀþ
                   WHEN    "70"
                    COMPUTE SYU-HKNTEN (10)   =  SYU-HKNTEN (10)  +
                           (ACCT-ZAITEN  *   SINRYO-COUNT)
                    PERFORM  VARYING  SYU-IDX  FROM  1  BY  1          
                               UNTIL    SYU-IDX   >    31              
                       COMPUTE SDAY-HKNTEN(SYU-IDX) =                  
                               SDAY-HKNTEN(SYU-IDX) +                  
                              (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))  
                    END-PERFORM                                        
      *            ¤½¤ÎÂ¾
                   WHEN    "80"
                    INITIALIZE                      SRYACT-REC
                    MOVE    ACCT-HOSPNUM       TO   SRY-HOSPNUM
                    MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
                    MOVE    ACCT-PTID          TO   SRY-PTID
                    MOVE    ACCT-SRYKA         TO   SRY-SRYKA
                    MOVE    ACCT-SRYYM         TO   SRY-SRYYM
                    MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
                    MOVE    SRYACT-REC         TO   MCPDATA-REC
                    MOVE    "DBSELECT"         TO   MCP-FUNC
                    MOVE    "tbl_sryact"       TO   MCP-TABLE
                    MOVE    "key2"             TO   MCP-PATHNAME
grpsys              CALL "ORCDBMAIN" USING MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
                    IF      MCP-RC             =    ZERO
                       MOVE    "tbl_sryact"    TO   MCP-TABLE
                       MOVE    "key2"          TO   MCP-PATHNAME
                      PERFORM  SRYACT-READ-SEC
                    ELSE
                      MOVE    1                TO   FLG-SRYACT
                    END-IF
                    IF      FLG-SRYACT          =   ZERO
                      EVALUATE   SRY-SRYSYUKBN
                       WHEN        "800"
                       WHEN        "809"
      ***              WHEN        "810"
                         COMPUTE SYU-HKNTEN (11) =  SYU-HKNTEN (11)  +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                       WHEN        "820"
                         COMPUTE SYU-HKNTEN (4)  =  SYU-HKNTEN (4)   +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                       WHEN        "830"
                         COMPUTE SYU-HKNTEN (12) =  SYU-HKNTEN (12)  +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                       WHEN        "840"
                         COMPUTE SYU-HKNTEN (13) =  SYU-HKNTEN (13)  +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                       WHEN        "850"
                         COMPUTE SYU-HKNTEN (16) =  SYU-HKNTEN (16)  +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                       WHEN        "890"
                         COMPUTE SYU-HKNTEN (15) =  SYU-HKNTEN (15)  +
                                (ACCT-ZAITEN  *   SINRYO-COUNT)
                         PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                    UNTIL    SYU-IDX   >    31
                            COMPUTE SDAY-HKNTEN(SYU-IDX) =
                                    SDAY-HKNTEN(SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                         END-PERFORM
                      END-EVALUATE
                    END-IF
                    MOVE    "tbl_sryact"    TO   MCP-TABLE
                    MOVE    "key2"          TO   MCP-PATHNAME
                    PERFORM   CSR-CLOSE-SEC
      *            Æþ±¡ÎÁ²Ã»»
                   WHEN    "90"
                   WHEN    "92"
                    IF   SYU-SYUHKNNUM  =  "971"  OR  "973"
                      CONTINUE
                    ELSE
                     IF   ACCT-ZAIKBN  =  2
                      COMPUTE WRK-SYUG-NYN-HKNTEN(IDX) =
                              WRK-SYUG-NYN-HKNTEN(IDX) +
                             (ACCT-ZAITEN  *   SINRYO-COUNT)
                      PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                 UNTIL    SYU-IDX   >    31
                         COMPUTE WRK-SDAYG-HKNTEN(IDX SYU-IDX) =
                                 WRK-SDAYG-HKNTEN(IDX SYU-IDX) +
                                (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                      END-PERFORM
                     ELSE
                      IF    (ACCT-SRYCDTOTAL    =   190080470  OR
                                                    190799670  OR
                                                    190080570  OR
                                                    190799770  OR
                                                    190111690  OR
                                                    190809090  OR
                                                    190111790  OR
                                                    190809190)
                      OR    (ACCT-SRYCDTOTAL    =   190182590  OR
                                                    190182690)
                        COMPUTE SYU-HKNTEN (15)   =  SYU-HKNTEN (15) -
                               (ACCT-ZAITEN  *   SINRYO-COUNT)
                        PERFORM  VARYING  SYU-IDX  FROM  1  BY  1      
                                   UNTIL    SYU-IDX   >    31          
                           COMPUTE SDAY-HKNTEN(SYU-IDX) =              
                                   SDAY-HKNTEN(SYU-IDX) -              
                                  (ACCT-ZAITEN  *                      
                                   TBL-ZAIKAISU(SYU-IDX))              
                        END-PERFORM                                    
                      ELSE
                        COMPUTE SYU-HKNTEN (15)   =  SYU-HKNTEN (15) +
                               (ACCT-ZAITEN  *   SINRYO-COUNT)
                        PERFORM  VARYING  SYU-IDX  FROM  1  BY  1      
                                   UNTIL    SYU-IDX   >    31          
                           COMPUTE SDAY-HKNTEN(SYU-IDX) =              
                                   SDAY-HKNTEN(SYU-IDX) +              
                                  (ACCT-ZAITEN  *                      
                                   TBL-ZAIKAISU(SYU-IDX))              
                        END-PERFORM                                    
                      END-IF
                     END-IF
                    END-IF
      *            ÊÝ¸±Å¬ÍÑ³°µÚ¤Ó¼«Èñ
                   WHEN    "95"
                   WHEN    "96"
                       INITIALIZE                      SRYACT-REC
                       MOVE    ACCT-HOSPNUM        TO   SRY-HOSPNUM
                       MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
                       MOVE    ACCT-PTID          TO   SRY-PTID
                       MOVE    ACCT-SRYKA         TO   SRY-SRYKA
                       MOVE    ACCT-SRYYM         TO   SRY-SRYYM
                       MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
                       MOVE    SRYACT-REC         TO   MCPDATA-REC
                       MOVE    "DBSELECT"         TO   MCP-FUNC
                       MOVE    "tbl_sryact"       TO   MCP-TABLE
                       MOVE    "key2"             TO   MCP-PATHNAME
grpsys                 CALL "ORCDBMAIN" USING MCPAREA
                                              MCPDATA-REC
grpsys                                        SPA-AREA
                       IF      MCP-RC             =    ZERO
                         MOVE    "tbl_sryact"     TO   MCP-TABLE
                         MOVE    "key2"           TO   MCP-PATHNAME
                         PERFORM  SRYACT-READ-SEC
                       ELSE
                         MOVE    1                TO   FLG-SRYACT
                       END-IF
                       IF      FLG-SRYACT          =   ZERO
                           PERFORM  UNTIL
                               (FLG-SRYACT         =  1)            OR
                               (ACCT-HOSPNUM  NOT = SRY-HOSPNUM)      OR
                               (ACCT-NYUGAIKBN  NOT = SRY-NYUGAIKBN) OR
                               (ACCT-PTID     NOT = SRY-PTID)       OR
                               (ACCT-SRYKA    NOT = SRY-SRYKA)      OR
                               (ACCT-SRYKBN   NOT = SRY-SRYKBN)     OR
                               (ACCT-ZAINUM   NOT = SRY-ZAINUM)
      *                        ÊÝ¸±Å¬ÍÑ³°¶â³Û¥»¥Ã¥È
                               PERFORM    HKNTEKGAI-TBL-SEC
                               MOVE    "tbl_sryact"   TO   MCP-TABLE
                               MOVE    "key2"         TO   MCP-PATHNAME
                               PERFORM    SRYACT-READ-SEC
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_sryact"     TO   MCP-TABLE
                       MOVE    "key2"           TO   MCP-PATHNAME
                       PERFORM   CSR-CLOSE-SEC
      *
               END-EVALUATE
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key30"         TO  MCP-PATHNAME
           PERFORM SRYACCT-READ-SEC
      *
           .
      *
       SRYACCT-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
      **************************************************************
       NYUINRYO-KASAN-CHECK-SEC          SECTION.
      *
      ***  PERFORM   VARYING   IDX6   FROM    1   BY  1
      ***             UNTIL   (IDX6    >   TBL-NYUINRYO-KASAN-MAX)
      ***              OR     (FLG-NYUINRYO-KASAN-OK =  1        )
      *
      ***    INITIALIZE                      SRYACT-REC
      ***    MOVE    ACCT-HOSPNUM     TO     SRY-HOSPNUM
      ***    MOVE    ACCT-NYUGAIKBN   TO     SRY-NYUGAIKBN
      ***    MOVE    ACCT-PTID        TO     SRY-PTID
      ***    MOVE    ACCT-SRYYM       TO     SRY-SRYYM
      ***    MOVE    ACCT-ZAINUM      TO     SRY-ZAINUM
      ***    MOVE    TBL-NYUINRYO-KASAN(IDX6)   TO   SRY-SRYCD(1)
      ***    MOVE    TBL-NYUINRYO-KASAN(IDX6)   TO   SRY-SRYCD(2)
      ***    MOVE    TBL-NYUINRYO-KASAN(IDX6)   TO   SRY-SRYCD(3)
      ***    MOVE    TBL-NYUINRYO-KASAN(IDX6)   TO   SRY-SRYCD(4)
      ***    MOVE    TBL-NYUINRYO-KASAN(IDX6)   TO   SRY-SRYCD(5)
      ***    MOVE    SRYACT-REC       TO     MCPDATA-REC
      ***    MOVE    "DBSELECT"       TO     MCP-FUNC
      ***    MOVE    "tbl_sryact"     TO     MCP-TABLE
      ***    MOVE    "key8"           TO     MCP-PATHNAME
      ***    CALL    "ORCDBMAIN"      USING  MCPAREA
      ***                                    MCPDATA-REC
      ***                                    SPA-AREA
      ***    IF      MCP-RC           =      ZERO
      ***      MOVE  "tbl_sryact"     TO     MCP-TABLE
      ***      MOVE  "key8"           TO     MCP-PATHNAME
      ***      PERFORM   SRYACT-READ-SEC
      ***      IF    FLG-SRYACT       =      ZERO
      ***          MOVE       1       TO     FLG-NYUINRYO-KASAN-OK
      ***      END-IF
      ***    END-IF
      ***    MOVE    "tbl_sryact"     TO     MCP-TABLE
      ***    MOVE    "key8"           TO     MCP-PATHNAME
      ***    PERFORM     CSR-CLOSE-SEC
      *
      ***  END-PERFORM
      *
      *    ¡ãÅÀ¿ô¥Þ¥¹¥¿¤Î¥³¡¼¥ÉÉ½ÍÑÈÖ¹æ¡ä
      *    ¶èÊ¬¤¬¡Ö£Á¡×¤Ç¡¢ÈÖ¹æ¤¬£²£°£°ÈÖÂæ¤«¥Á¥§¥Ã¥¯¤¹¤ë
           INITIALIZE                       SRYACT-REC
           MOVE    ACCT-HOSPNUM     TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID        TO      SRY-PTID
           MOVE    ACCT-SRYYM       TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM      TO      SRY-ZAINUM
           MOVE    SRYACT-REC       TO      MCPDATA-REC
           MOVE    "DBSELECT"       TO      MCP-FUNC
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING   MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC = ZERO
              MOVE  "tbl_sryact"    TO      MCP-TABLE
              MOVE  "key9"          TO      MCP-PATHNAME
              PERFORM   SRYACT-READ-SEC
              IF   FLG-SRYACT = ZERO
                 IF   SRY-SRYCD(1) NOT = SPACE
                   MOVE     SPACE         TO  TNS-TENSU-REC
                   INITIALIZE                 TNS-TENSU-REC
                   MOVE     SRY-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE     SRY-SRYCD(1)  TO  TNS-SRYCD
                   MOVE     SRY-SRYYM     TO  TNS-YUKOSTYMD(1:6)
                                              TNS-YUKOEDYMD(1:6)
                   MOVE     "01"          TO  TNS-YUKOSTYMD(7:2)
                                              TNS-YUKOEDYMD(7:2)
                   PERFORM  TENSU-READ-SEC
                   IF  FLG-TENSU = ZERO
                     IF  (TNS-CDKBN-KBN         = "A")
                     AND (TNS-CDKBN-KBNNUM(1:1) = "2")
                       MOVE       1       TO  FLG-NYUINRYO-KASAN-OK
                     END-IF
                   END-IF
                 END-IF
              END-IF
           END-IF
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key9"           TO      MCP-PATHNAME
           PERFORM     CSR-CLOSE-SEC
      *
      *    ËýÀ­°Ý»ýÆ©ÀÏ´ÉÍý²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡ÎÁ£±¡Ë¤Ï¥Á¥§¥Ã¥¯ÂÐ¾Ý³°
      *    ¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡¦¸ø³²¤ÇÆþ±¡ÎÁ»»Äê»þ¤Ë¸Â¤ë¡Ë
           IF   (FLG-NYUINRYO-KASAN-OK  =  ZERO       )
           AND  (SRY-SRYCD(1)           =  "190167970")
             MOVE      1     TO    FLG-NYUINRYO-KASAN-OK
             IF  SRY-SRYYM  =  WRK-SKYYM(1)
               MOVE    1     TO    IDX12
             ELSE
               MOVE    2     TO    IDX12
             END-IF
             PERFORM  VARYING  IDX6  FROM  1  BY  1
                       UNTIL  (IDX6                  >  31 )
                         OR   (FLG-NYUINRYO-KASAN-OK = ZERO)
               IF  (ACCT-DAY(1 IDX6)     NOT =  ZERO)
               AND (WRK-DAY5(IDX12 IDX6) NOT =  ZERO)
                 MOVE  WRK-DAY5(IDX12 IDX6)  TO  WRK-HKNCOMBINUM
                 PERFORM  HKNCOMBI-KENSAKU-SEC
                 IF  COMB-HKNNUM  =  "971" OR "973" OR "975"
                   CONTINUE
                 ELSE
                   MOVE     ZERO   TO    FLG-NYUINRYO-KASAN-OK
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       NYUINRYO-KASAN-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯£²
      **************************************************************
       NYUINRYO-KASAN-CHECK2-SEC          SECTION.
      *
           PERFORM   VARYING   IDX6   FROM    1   BY  1
                      UNTIL   (IDX6    >   TBL-NYUINRYO-KASAN2-MAX)
                       OR     (FLG-NYUINRYO-KASAN2-OK =  1        )
      *
             INITIALIZE                      SRYACT-REC
             MOVE    ACCT-HOSPNUM     TO     SRY-HOSPNUM
             MOVE    ACCT-NYUGAIKBN   TO     SRY-NYUGAIKBN
             MOVE    ACCT-PTID        TO     SRY-PTID
             MOVE    ACCT-SRYYM       TO     SRY-SRYYM
             MOVE    ACCT-ZAINUM      TO     SRY-ZAINUM
             MOVE    TBL-NYUINRYO-KASAN2(IDX6)  TO   SRY-SRYCD(1)
             MOVE    TBL-NYUINRYO-KASAN2(IDX6)  TO   SRY-SRYCD(2)
             MOVE    TBL-NYUINRYO-KASAN2(IDX6)  TO   SRY-SRYCD(3)
             MOVE    TBL-NYUINRYO-KASAN2(IDX6)  TO   SRY-SRYCD(4)
             MOVE    TBL-NYUINRYO-KASAN2(IDX6)  TO   SRY-SRYCD(5)
             MOVE    SRYACT-REC       TO     MCPDATA-REC
             MOVE    "DBSELECT"       TO     MCP-FUNC
             MOVE    "tbl_sryact"     TO     MCP-TABLE
             MOVE    "key8"           TO     MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING  MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
             IF      MCP-RC           =      ZERO
               MOVE  "tbl_sryact"     TO     MCP-TABLE
               MOVE  "key8"           TO     MCP-PATHNAME
               PERFORM   SRYACT-READ-SEC
               IF    FLG-SRYACT       =      ZERO
                   MOVE       1       TO     FLG-NYUINRYO-KASAN2-OK
               END-IF
             END-IF
             MOVE    "tbl_sryact"     TO     MCP-TABLE
             MOVE    "key8"           TO     MCP-PATHNAME
             PERFORM     CSR-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       NYUINRYO-KASAN-CHECK2-EXT.
           EXIT.
      *
      **************************************************************
      *    Ê¡²¬¡¦µëÉÕ³°ÅÀ¿ôÊÔ½¸
      **************************************************************
       FUKUOKA-KYUFUGAI-HENSYU-SEC          SECTION.
      *
           IF   TBL-FUKUOKA-SYOSIN(SYU-IDX)  =   3
              INITIALIZE                      SRYACT-REC
              MOVE    ACCT-HOSPNUM        TO   SRY-HOSPNUM
              MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
              MOVE    ACCT-PTID          TO   SRY-PTID
              MOVE    ACCT-SRYKA         TO   SRY-SRYKA
              MOVE    ACCT-SRYYM         TO   SRY-SRYYM
              MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
              MOVE    SRYACT-REC         TO   MCPDATA-REC
              MOVE    "DBSELECT"         TO   MCP-FUNC
              MOVE    "tbl_sryact"       TO   MCP-TABLE
              MOVE    "key2"             TO   MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
              IF      MCP-RC             =    ZERO
                MOVE    "tbl_sryact"     TO   MCP-TABLE
                MOVE    "key2"           TO   MCP-PATHNAME
                PERFORM  SRYACT-READ-SEC
              ELSE
                MOVE    1                TO   FLG-SRYACT
              END-IF
              IF      FLG-SRYACT          =   ZERO
                 IF   SRY-SRYCD(1)  =  "111011810" OR "111012610"
                                                   OR "111012810"
                    CONTINUE
                 ELSE
                    COMPUTE  SYU-KYUFUGAI-SHOSHIN-TEN =
                             SYU-KYUFUGAI-SHOSHIN-TEN +
                            (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                    COMPUTE  SYU-KYUFUGAI-GOKEI-TEN =
                             SYU-KYUFUGAI-GOKEI-TEN +
                            (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
                 END-IF
              END-IF
              MOVE    "tbl_sryact"     TO   MCP-TABLE
              MOVE    "key2"           TO   MCP-PATHNAME
              PERFORM   CSR-CLOSE-SEC
           ELSE
              COMPUTE  SYU-KYUFUGAI-SHOSHIN-TEN =
                       SYU-KYUFUGAI-SHOSHIN-TEN +
                      (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
              COMPUTE  SYU-KYUFUGAI-GOKEI-TEN =
                       SYU-KYUFUGAI-GOKEI-TEN +
                      (ACCT-ZAITEN  *  TBL-ZAIKAISU(SYU-IDX))
           END-IF
      *
           .
       FUKUOKA-KYUFUGAI-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    Ï«ºÒ¡¦¼«ÇåÀÕÍÑ¤ÎÊÔ½¸½èÍý
      **************************************************************
       ROSAI-JIBAI-HENSYU-SEC          SECTION.
      *
           IF  (ACCT-SRYKBN  =  "90" OR "92" OR "95" OR "96" OR "97")
           OR  (ACCT-ZAIKBN  =   2                                  )
             CONTINUE
           ELSE
      *
             IF     SYU-SYUHKNNUM   =   "973"
                COMPUTE     WRK-SYUTEN  =  WRK-SYUTEN
                                 +  ( ACCT-SYUTEN1 * SINRYO-COUNT )
                IF     ACCT-SRYKBN   =   "70"
                   COMPUTE  WRK-SYUTEN  =  WRK-SYUTEN
                                 -  ( ACCT-SYUTEN2 * SINRYO-COUNT )
                END-IF
             END-IF
      *
             MOVE          ZERO    TO      WRK-TAIHI-DAY
             PERFORM   VARYING   IDX8   FROM    1    BY   1
                        UNTIL    IDX8     >    31
                IF   ACCT-DAY(1 IDX8)  NOT =   ZERO
                  MOVE     IDX8    TO      WRK-TAIHI-DAY
                  MOVE     31      TO      IDX8
                END-IF
             END-PERFORM
             IF   WRK-TAIHI-DAY  =  ZERO
                MOVE       01      TO      WRK-TAIHI-DAY
             END-IF
      *
             INITIALIZE                      SRYACT-REC
             MOVE    ACCT-HOSPNUM        TO   SRY-HOSPNUM
             MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
             MOVE    ACCT-PTID          TO   SRY-PTID
             MOVE    ACCT-SRYKA         TO   SRY-SRYKA
             MOVE    ACCT-SRYYM         TO   SRY-SRYYM
             MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
             MOVE    SRYACT-REC         TO   MCPDATA-REC
             MOVE    "DBSELECT"         TO   MCP-FUNC
             MOVE    "tbl_sryact"       TO   MCP-TABLE
             MOVE    "key2"             TO   MCP-PATHNAME
grpsys       CALL "ORCDBMAIN" USING MCPAREA
                                    MCPDATA-REC
grpsys                              SPA-AREA
             IF      MCP-RC             =    ZERO
                MOVE    "tbl_sryact"    TO   MCP-TABLE
                MOVE    "key2"          TO   MCP-PATHNAME
                PERFORM  SRYACT-READ-SEC
                PERFORM  UNTIL  FLG-SRYACT    =    1
      *
                  PERFORM   VARYING   IDZ   FROM   1   BY   1
                       UNTIL    (IDZ            >       5)  OR
                                (SRY-SRYCD(IDZ) =   SPACE)
      *
                    MOVE    SPACE             TO   TNS-TENSU-REC
                    INITIALIZE                     TNS-TENSU-REC
                    MOVE     LNK-S02-HOSPNUM  TO   TNS-HOSPNUM
                    MOVE    SRY-SRYCD(IDZ)    TO   TNS-SRYCD
                    MOVE     SYU-SRYYMD(1:6)  TO   TNS-YUKOSTYMD
                                                   TNS-YUKOEDYMD
                    MOVE     WRK-TAIHI-DAY    TO   TNS-YUKOSTYMD(7:2)
                                                   TNS-YUKOEDYMD(7:2)
                    PERFORM    TENSU-READ-SEC
      *
                    IF       FLG-TENSU      =    ZERO
      *
                      COMPUTE  WRK-TEN     =   TNS-TEN  /  10
      *
                      IF   SYU-SRYYMD(1:6)   >=   "201307"
                        IF   SRY-SRYCD(IDZ)   =
                             "101110010" OR "101110040"
                           COMPUTE WRK-RSISHOSHIN = WRK-RSISHOSHIN +
                                  (WRK-TEN  *   SINRYO-COUNT)
                        END-IF
                        EVALUATE  SRY-SRYCD(IDZ)
                          WHEN    "101800390"  THRU  "101800460"
                          WHEN    "101800470"  THRU  "101800540"
                          WHEN    "101800870"  THRU  "101800880"
                          WHEN    "101800960"  THRU  "101800980"
                          WHEN    "101801040"
                          WHEN    "101801150"  THRU  "101801180"
                           COMPUTE WRK-RSIETC  = WRK-RSIETC  +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                        END-EVALUATE
                        EVALUATE  SRY-SRYCD(IDZ)
                          WHEN    "101800570"  THRU  "101800860"
                          WHEN    "101801010"  THRU  "101801020"
                          WHEN    "101801190"  THRU  "101801420"
                           COMPUTE WRK-RSIETC  = WRK-RSIETC  +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
      *
                           COMPUTE WRK-TEN-KG     = WRK-TEN
                           COMPUTE WRK-TEN-KG2    = TNS-TEN
                        END-EVALUATE
                        IF   SRY-SRYCD(IDZ)   = "101800550" OR
                                 "101800560" OR "101800990" OR
                                 "101801030"
                           COMPUTE WRK-FUTANWARI  = TNS-TEN / 100
      *
                           COMPUTE WRK-KEISAN     =
                                  (WRK-TEN-KG     * WRK-FUTANWARI)
                                                  + 0.5
                           COMPUTE WRK-KEISAN     =
                                  (WRK-KEISAN     * SINRYO-COUNT)
                           COMPUTE WRK-RSIETC     =
                                   WRK-RSIETC     + WRK-KEISAN
      *
                           COMPUTE WRK-KEISAN     =
                                  (WRK-TEN-KG2    * WRK-FUTANWARI)
                           COMPUTE WRK-KEISAN     =
                                  (WRK-KEISAN     * SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2    =
                                   WRK-RSIETC2    + WRK-KEISAN
                        END-IF
                      ELSE
                        IF   SRY-SRYCD(IDZ)   =
                             "101110010" OR "101110020" OR
                             "101110040"
                           COMPUTE WRK-RSISHOSHIN = WRK-RSISHOSHIN +
                                  (WRK-TEN  *   SINRYO-COUNT)
                        END-IF
                        IF ( SRY-SRYCD(IDZ)   =
                             "101130020" OR "101130030" OR
                             "101130040" OR "101130050" OR
                             "101130060" OR "101130070" OR
                             "101130080" OR "101130090" OR
                             "101130100"                        )
                        OR ((SYU-SRYYMD(1:6) <=   "200803"     ) AND
                            (SRY-SRYCD(IDZ)   =   "101130010"  ))
                           COMPUTE WRK-RSISDO = WRK-RSISDO +
                                  (WRK-TEN  *   SINRYO-COUNT)
                        END-IF
                        IF   SRY-SRYCD(IDZ)   =
                             "101910010" OR "101910020" OR
                             "101910030" OR "101910040" OR
                             "101910070" OR "101910080" OR
                             "101910090" OR "101910100"
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                        END-IF
                        EVALUATE  SRY-SRYCD(IDZ)
                          WHEN    "101910110"  THRU  "101910400"
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                        END-EVALUATE
                      END-IF
      *
                      IF     SYU-SYUHKNNUM   =   "971"
      *
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09593"
                                                    OR    "09594"
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-TEN    = SRY-JIHIMONEY(IDZ)
                                              / 10
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (SRY-JIHIMONEY(IDZ)
                                            *   SINRYO-COUNT)
                          ELSE
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
      *
                      END-IF
      *
                      IF     SYU-SYUHKNNUM   =   "973"
      *
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09591"
                                                    OR    "09592"
                                                    OR    "09593"
                                                    OR    "09594"
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-TEN    = SRY-JIHIMONEY(IDZ)
                                              / 10
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (SRY-JIHIMONEY(IDZ)
                                            *   SINRYO-COUNT)
                          ELSE
                           COMPUTE WRK-RSIETC = WRK-RSIETC +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09591"
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-SINDAN = WRK-SINDAN +
                                  (SRY-JIHIMONEY(IDZ)
                                            *   SINRYO-COUNT)
                          ELSE
                           COMPUTE WRK-SINDAN = WRK-SINDAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09592"
                          IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-MEISAI = WRK-MEISAI +
                                  (SRY-JIHIMONEY(IDZ)
                                            *   SINRYO-COUNT)
                          ELSE
                           COMPUTE WRK-MEISAI = WRK-MEISAI +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
      *
                         IF    SRY-SRYCD (IDZ)  =   "140037490"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WRK-YOUBUKASAN = WRK-YOUBUKASAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDZ)  =   "140040110"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WRK-KYOBUKASAN = WRK-KYOBUKASAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDZ)  =   "140052610"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WRK-KEIBUKASAN = WRK-KEIBUKASAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
                         IF    SRY-SRYCD (IDZ)  =   "150266970"
                          IF  WRK-4001-KOTEIKBN  =  SPACE OR "2"
                           COMPUTE WRK-SOGAIKASAN = WRK-SOGAIKASAN +
                                  (TNS-TEN  *   SINRYO-COUNT)
                          END-IF
                         END-IF
      *
                         IF    SRY-SRYCD (IDZ)  =   "199000510"
                           COMPUTE WRK-RYOYOTEATE = WRK-RYOYOTEATE +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                         IF    SRY-SRYCD (IDZ)(1:5)   =   "09591"
                                                    OR    "09592"
                                                    OR    "09593"
                           IF   IDX3  >= 50
                             DISPLAY "TABLE OVER"
                           ELSE
                             ADD    1   TO  IDX3
                             MOVE   SRY-SRYCD(IDZ)
                                TO  WRK-JIBAI-SONOTA-SRYCD(IDX IDX3)
                             MOVE   SINRYO-COUNT
                                TO  WRK-JIBAI-SONOTA-ZAIKAISU(IDX IDX3)
                             IF  SRY-SRYSYUKBN = "809"
                               MOVE SRY-JIHIMONEY(IDZ)
                                TO  WRK-JIBAI-SONOTA-TANKA(IDX IDX3)
                             ELSE
                               MOVE TNS-TEN
                                TO  WRK-JIBAI-SONOTA-TANKA(IDX IDX3)
                             END-IF
                           END-IF
                         END-IF
      *
                         IF   SYU-SRYYMD(1:6)  >=   "201307"
                         AND  SRY-SRYCD (IDZ)   =   "101800880"
                           COMPUTE WRK-KYUKYU = WRK-KYUKYU +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                      END-IF
      *
                    END-IF
      *
                  END-PERFORM
      *
                  MOVE    "tbl_sryact"    TO   MCP-TABLE
                  MOVE    "key2"          TO   MCP-PATHNAME
                  PERFORM  SRYACT-READ-SEC
                END-PERFORM
             END-IF
      *
             MOVE    "tbl_sryact"    TO   MCP-TABLE
             MOVE    "key2"          TO   MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
      *
           END-IF
      *
           .
       ROSAI-JIBAI-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    Âè»°¼Ô¹Ô°Ù¤ÎÊÔ½¸½èÍý
      **************************************************************
       DAISANSYA-HENSYU-SEC          SECTION.
      *
           IF   (ACCT-SRYKBN  =  "90" OR "92" OR "95" OR "96" OR "97")
           OR   (ACCT-ZAIKBN  =   2                                  )
             CONTINUE
           ELSE
             IF  ACCT-SRYKBN  =  "80"
               INITIALIZE                      SRYACT-REC
               MOVE    ACCT-HOSPNUM       TO   SRY-HOSPNUM
               MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
               MOVE    ACCT-PTID          TO   SRY-PTID
               MOVE    ACCT-SRYKA         TO   SRY-SRYKA
               MOVE    ACCT-SRYYM         TO   SRY-SRYYM
               MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
               MOVE    SRYACT-REC         TO   MCPDATA-REC
               MOVE    "DBSELECT"         TO   MCP-FUNC
               MOVE    "tbl_sryact"       TO   MCP-TABLE
               MOVE    "key2"             TO   MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
               IF      MCP-RC             =    ZERO
                 MOVE    "tbl_sryact"    TO   MCP-TABLE
                 MOVE    "key2"          TO   MCP-PATHNAME
                 PERFORM  SRYACT-READ-SEC
                 PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   IDZ   FROM   1   BY   1
                              UNTIL   (IDZ            >       5)
                                OR    (SRY-SRYCD(IDZ) =   SPACE)
                     IF   SRY-SRYCD(IDZ)(1:5) = "09591" OR "09592" OR
                                                "09593" OR "09594"
                       MOVE    SPACE            TO  TNS-TENSU-REC
                       INITIALIZE                   TNS-TENSU-REC
                       MOVE    LNK-S02-HOSPNUM  TO  TNS-HOSPNUM
                       MOVE    SRY-SRYCD(IDZ)   TO  TNS-SRYCD
                       MOVE    SYU-SRYYMD(1:6)  TO  TNS-YUKOSTYMD
                                                    TNS-YUKOEDYMD
                       MOVE    "01"             TO  TNS-YUKOSTYMD(7:2)
                                                    TNS-YUKOEDYMD(7:2)
                       PERFORM    TENSU-READ-SEC
                       IF      FLG-TENSU    =    ZERO
      *
                         COMPUTE  WRK-TEN  =    TNS-TEN  /  10
      *
                         IF  SRY-SRYSYUKBN = "809"
                           COMPUTE WRK-TEN     = SRY-JIHIMONEY(IDZ)
                                               / 10
                           COMPUTE WRK-RSIETC  = WRK-RSIETC  +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (SRY-JIHIMONEY(IDZ)
                                            *   SINRYO-COUNT)
                         ELSE
                           COMPUTE WRK-RSIETC  = WRK-RSIETC  +
                                  (WRK-TEN  *   SINRYO-COUNT)
                           COMPUTE WRK-RSIETC2 = WRK-RSIETC2 +
                                  (TNS-TEN  *   SINRYO-COUNT)
                         END-IF
      *
                         IF  SRY-SRYCD(IDZ)(1:5) =
                                    "09591" OR "09592" OR "09593"
                           IF   IDX3  >= 50
                             DISPLAY "TABLE OVER"
                           ELSE
                             ADD    1   TO  IDX3
                             MOVE   SRY-SRYCD(IDZ)
                                TO  WRK-JIBAI-SONOTA-SRYCD(IDX IDX3)
                             MOVE   SINRYO-COUNT
                                TO  WRK-JIBAI-SONOTA-ZAIKAISU(IDX IDX3)
                             IF  SRY-SRYSYUKBN = "809"
                               MOVE SRY-JIHIMONEY(IDZ)
                                TO  WRK-JIBAI-SONOTA-TANKA(IDX IDX3)
                             ELSE
                               MOVE TNS-TEN
                                TO  WRK-JIBAI-SONOTA-TANKA(IDX IDX3)
                             END-IF
                           END-IF
                         END-IF
      *
                       END-IF
                     END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"    TO   MCP-TABLE
                   MOVE    "key2"          TO   MCP-PATHNAME
                   PERFORM  SRYACT-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_sryact"    TO   MCP-TABLE
               MOVE    "key2"          TO   MCP-PATHNAME
               PERFORM  CSR-CLOSE-SEC
             END-IF
           END-IF
      *
           .
       DAISANSYA-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸ø³²ÍÑ¤ÎÊÔ½¸½èÍý
      **************************************************************
       KOGAI-HENSYU-SEC          SECTION.
      *
           IF  (ACCT-SRYKBN  =  "90" OR "92" OR "95" OR "96" OR "97")
           OR  (ACCT-ZAIKBN  =   2                                  )
             CONTINUE
           ELSE
             COMPUTE     WRK-SYUTEN  =  WRK-SYUTEN
                              +  ( ACCT-SYUTEN1 * SINRYO-COUNT )
             IF     ACCT-SRYKBN   =   "70"
                COMPUTE  WRK-SYUTEN  =  WRK-SYUTEN
                              -  ( ACCT-SYUTEN2 * SINRYO-COUNT )
             END-IF
           END-IF
      *
           IF  (ACCT-SRYKBN  =  "90" OR "92" OR "80")
           AND (ACCT-ZAIKBN   NOT =   2             )
             INITIALIZE                      SRYACT-REC
             MOVE    ACCT-HOSPNUM       TO   SRY-HOSPNUM
             MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
             MOVE    ACCT-PTID          TO   SRY-PTID
             MOVE    ACCT-SRYKA         TO   SRY-SRYKA
             MOVE    ACCT-SRYYM         TO   SRY-SRYYM
             MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
             MOVE    SRYACT-REC         TO   MCPDATA-REC
             MOVE    "DBSELECT"         TO   MCP-FUNC
             MOVE    "tbl_sryact"       TO   MCP-TABLE
             MOVE    "key2"             TO   MCP-PATHNAME
grpsys       CALL "ORCDBMAIN" USING MCPAREA
                                    MCPDATA-REC
grpsys                              SPA-AREA
             IF      MCP-RC             =    ZERO
                MOVE    "tbl_sryact"    TO   MCP-TABLE
                MOVE    "key2"          TO   MCP-PATHNAME
                PERFORM  SRYACT-READ-SEC
                PERFORM  UNTIL  FLG-SRYACT    =    1
                  PERFORM   VARYING   IDZ   FROM   1   BY   1
                       UNTIL    (IDZ            >       5)  OR
                                (SRY-SRYCD(IDZ) =   SPACE)
      *
                    MOVE    SPACE             TO   TNS-TENSU-REC
                    INITIALIZE                     TNS-TENSU-REC
                    MOVE     LNK-S02-HOSPNUM  TO   TNS-HOSPNUM
                    MOVE    SRY-SRYCD(IDZ)    TO   TNS-SRYCD
                    MOVE     SYU-SRYYMD(1:6)  TO   TNS-YUKOSTYMD
                                                   TNS-YUKOEDYMD
                    MOVE     "01"             TO   TNS-YUKOSTYMD(7:2)
                                                   TNS-YUKOEDYMD(7:2)
                    PERFORM    TENSU-READ-SEC
                    IF       FLG-TENSU      =    ZERO
                      IF   SRY-SRYCD(IDZ) =  "102900010"  OR
                                             "102900020"  OR
                                             "102900030"  OR
                                             "102900040"
                         COMPUTE  WRK-KOGAI-TEN = WRK-KOGAI-TEN +
                                 (TNS-TEN  *  SINRYO-COUNT)
                      END-IF
      *
                      IF   SRY-SRYCD(IDZ) =  "199000510"
                         COMPUTE  WRK-RYOYOTEATE = WRK-RYOYOTEATE +
                                 (TNS-TEN  *  SINRYO-COUNT)
                      END-IF
                    END-IF
      *
                  END-PERFORM
                  MOVE    "tbl_sryact"    TO   MCP-TABLE
                  MOVE    "key2"          TO   MCP-PATHNAME
                  PERFORM  SRYACT-READ-SEC
                END-PERFORM
             END-IF
             MOVE    "tbl_sryact"    TO   MCP-TABLE
             MOVE    "key2"          TO   MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
           END-IF
      *
           .
       KOGAI-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°¡ÊÃÏÊý¸øÈñÍÑ¡Ë¤Î¥»¥Ã¥È
      *    ½é¿ÇÎÁ»»Äê¥Õ¥é¥°£²¡ÊÃÏÊý¸øÈñÍÑ¡Ë¤Î¥»¥Ã¥È
      **************************************************************
       SYOSINFLG-SET-SEC          SECTION.
      *
           IF  ACCT-SRYKBN  =   "11"
      *
               PERFORM   VARYING   IDX6   FROM    1   BY  1
                          UNTIL  ( IDX6    >   TBL-SYOSIN-MAX  )
      *
                   INITIALIZE                        SRYACT-REC
                   MOVE    ACCT-HOSPNUM          TO   SRY-HOSPNUM
                   MOVE    ACCT-NYUGAIKBN       TO   SRY-NYUGAIKBN
                   MOVE    ACCT-PTID            TO   SRY-PTID
                   MOVE    ACCT-SRYYM           TO   SRY-SRYYM
                   MOVE    ACCT-ZAINUM          TO   SRY-ZAINUM
      *
                   MOVE    TBL-SYOSIN(IDX6)     TO   SRY-SRYCD (1)
                   MOVE    TBL-SYOSIN(IDX6)     TO   SRY-SRYCD (2)
                   MOVE    TBL-SYOSIN(IDX6)     TO   SRY-SRYCD (3)
                   MOVE    TBL-SYOSIN(IDX6)     TO   SRY-SRYCD (4)
                   MOVE    TBL-SYOSIN(IDX6)     TO   SRY-SRYCD (5)
                   MOVE    SRYACT-REC           TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_sryact"         TO   MCP-TABLE
                   MOVE    "key8"               TO   MCP-PATHNAME
grpsys             CALL "ORCDBMAIN" USING MCPAREA
                                          MCPDATA-REC
grpsys                                    SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE    "tbl_sryact"       TO   MCP-TABLE
                     MOVE    "key8"             TO   MCP-PATHNAME
                     PERFORM   SRYACT-READ-SEC
                     IF     FLG-SRYACT   =    ZERO
                       MOVE      "1"  TO  WRK-SYOSIN(IDX)
                       PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
                                 UNTIL   SYU-IDX   >   31
                         IF   TBL-ZAIKAISU(SYU-IDX) NOT = ZERO
                           MOVE  "1"  TO  WRK-SYOSIN-DAY(IDX SYU-IDX)
                         END-IF
                       END-PERFORM
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_sryact"       TO   MCP-TABLE
                   MOVE    "key8"             TO   MCP-PATHNAME
                   PERFORM   CSR-CLOSE-SEC
      *
               END-PERFORM
      *
           END-IF
      *
           IF  ACCT-SRYKBN  =   "99"
      *
                   INITIALIZE                        SRYACT-REC
                   MOVE    ACCT-HOSPNUM         TO   SRY-HOSPNUM
                   MOVE    ACCT-NYUGAIKBN       TO   SRY-NYUGAIKBN
                   MOVE    ACCT-PTID            TO   SRY-PTID
                   MOVE    ACCT-SRYYM           TO   SRY-SRYYM
                   MOVE    ACCT-ZAINUM          TO   SRY-ZAINUM
      *
                   MOVE    "099409906"          TO   SRY-SRYCD (1)
                   MOVE    "099409906"          TO   SRY-SRYCD (2)
                   MOVE    "099409906"          TO   SRY-SRYCD (3)
                   MOVE    "099409906"          TO   SRY-SRYCD (4)
                   MOVE    "099409906"          TO   SRY-SRYCD (5)
                   MOVE    SRYACT-REC           TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "tbl_sryact"         TO   MCP-TABLE
                   MOVE    "key8"               TO   MCP-PATHNAME
                   CALL "ORCDBMAIN" USING MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                   IF      MCP-RC             =       ZERO
                     MOVE    "tbl_sryact"       TO   MCP-TABLE
                     MOVE    "key8"             TO   MCP-PATHNAME
                     PERFORM   SRYACT-READ-SEC
                     IF     FLG-SRYACT   =    ZERO
                       MOVE      "1"  TO  WRK-SYOSIN2(IDX)
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_sryact"       TO   MCP-TABLE
                   MOVE    "key8"             TO   MCP-PATHNAME
                   PERFORM   CSR-CLOSE-SEC
      *
           END-IF
      *
           .
      *
       SYOSINFLG-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Ï«ºÒ¡¦¼«ÇåÀÕÍÑ¤ÎÅ¾Á÷½èÍý
      **************************************************************
       ROSAI-JIBAI-TENSO-SEC          SECTION.
      *
           IF      WRK-RSISHOSHIN     NOT =    ZERO
                   COMPUTE SYU-HKNTEN (01)  =  SYU-HKNTEN (01)
                                            -  WRK-RSISHOSHIN
                   COMPUTE SYU-RSISHOSHIN-MONEY  =  WRK-RSISHOSHIN
                                                 *  10
           END-IF
           IF      WRK-RSISDO         NOT =    ZERO
                   COMPUTE SYU-HKNTEN (02)  =  SYU-HKNTEN (02)
                                            -  WRK-RSISDO
                   COMPUTE SYU-RSISDO-MONEY      =  WRK-RSISDO
                                                 *  10
           END-IF
           IF      WRK-RSIETC         NOT =    ZERO
                   COMPUTE SYU-HKNTEN (11)  =  SYU-HKNTEN (11)
                                            -  WRK-RSIETC
      ****         COMPUTE SYU-RSIETC-MONEY      =  WRK-RSIETC
      ****                                       *  10
           END-IF
           MOVE    WRK-RSIETC2    TO   SYU-RSIETC-MONEY
      *
           IF   SYU-HKNTEN(2)   >= WRK-SYUG-SDO-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(2)  =  SYU-HKNTEN(2)
                                       -  WRK-SYUG-SDO-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(2)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(3)   >= WRK-SYUG-ZTK-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(3)  =  SYU-HKNTEN(3)
                                       -  WRK-SYUG-ZTK-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(3)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(15)  >= WRK-SYUG-NYN-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(15) =  SYU-HKNTEN(15)
                                       -  WRK-SYUG-NYN-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(15)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(5)   >= WRK-SYUG-CSY-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(5)  =  SYU-HKNTEN(5)
                                       -  WRK-SYUG-CSY-HKNTEN(IDX)
              IF  WRK-CSY-YAKTEN(IDX)
                                <  WRK-SYUG-CSY-HKNTEN(IDX)
                MOVE    1       TO    WRK-GENTEN-ERR2
              END-IF
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(5)
              MOVE      1       TO    WRK-GENTEN-ERR2
           END-IF
      *
           IF  SYU-SYUHKNNUM       =   "973"
      *
             COMPUTE   WRK-SYUTEN   =  WRK-SYUTEN
                                    -  WRK-SYUG-SDO-HKNTEN(IDX)
                                    -  WRK-SYUG-ZTK-HKNTEN(IDX)
      *
             COMPUTE   WRK-JIBAI-SYUTEN(IDX)
                                        =    WRK-SYUTEN
                                        -    WRK-RSISHOSHIN
                                        -    WRK-RSISDO
                                        -    WRK-RSIETC
                                        -    WRK-YOUBUKASAN
                                        -    WRK-KYOBUKASAN
                                        -    WRK-KEIBUKASAN
                                        -    WRK-SOGAIKASAN
                                        -    WRK-RYOYOTEATE
      *
             ADD    SYU-HKNTEN(15)     TO    WRK-JIBAI-SYUTEN(IDX)
      *
             MOVE      WRK-SINDAN      TO    WRK-JIBAI-SINDAN(IDX)
             MOVE      WRK-MEISAI      TO    WRK-JIBAI-MEISAI(IDX)
      *
             MOVE      WRK-RYOYOTEATE  TO    WRK-JIBAI-RYOYO(IDX)
             COMPUTE   WRK-JIBAI-RYOYO(IDX)
                                  =  WRK-JIBAI-RYOYO(IDX)
                                  +  WRK-JIBAI-RYOYO-NACCT(IDX)
      *
             MOVE      WRK-KYUKYU      TO    WRK-JIBAI-KYUKYU(IDX)
      *
           END-IF
      *
           .
       ROSAI-JIBAI-TENSO-EXT.
           EXIT.
      *
      **************************************************************
      *    ¸ø³²ÍÑ¤ÎÅ¾Á÷½èÍý
      **************************************************************
       KOGAI-TENSO-SEC          SECTION.
      *
           IF   SYU-HKNTEN(2)   >= WRK-SYUG-SDO-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(2)  =  SYU-HKNTEN(2)
                                       -  WRK-SYUG-SDO-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(2)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(3)   >= WRK-SYUG-ZTK-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(3)  =  SYU-HKNTEN(3)
                                       -  WRK-SYUG-ZTK-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(3)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(15)  >= WRK-SYUG-NYN-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(15) =  SYU-HKNTEN(15)
                                       -  WRK-SYUG-NYN-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(15)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(5)   >= WRK-SYUG-CSY-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(5)  =  SYU-HKNTEN(5)
                                       -  WRK-SYUG-CSY-HKNTEN(IDX)
              IF  WRK-CSY-YAKTEN(IDX)
                                <  WRK-SYUG-CSY-HKNTEN(IDX)
                MOVE    1       TO    WRK-GENTEN-ERR2
              END-IF
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(5)
              MOVE      1       TO    WRK-GENTEN-ERR2
           END-IF
      *
           COMPUTE   WRK-SYUTEN     =  WRK-SYUTEN
                                    -  WRK-SYUG-SDO-HKNTEN(IDX)
                                    -  WRK-SYUG-ZTK-HKNTEN(IDX)
      *
           COMPUTE   WRK-JIBAI-SYUTEN(IDX) =  WRK-SYUTEN
                                           -  WRK-KOGAI-TEN
                                           -  WRK-RYOYOTEATE
      *
           ADD    SYU-HKNTEN(15)     TO    WRK-JIBAI-SYUTEN(IDX)
      *
           MOVE      WRK-RYOYOTEATE  TO    WRK-JIBAI-RYOYO(IDX)
           COMPUTE   WRK-JIBAI-RYOYO(IDX)
                                =  WRK-JIBAI-RYOYO(IDX)
                                +  WRK-JIBAI-RYOYO-NACCT(IDX)
      *
           .
       KOGAI-TENSO-EXT.
           EXIT.
      *
      **************************************************************
      *    ¤½¤ÎÂ¾ÊÝ¸±ÍÑ¤ÎÅ¾Á÷½èÍý
      **************************************************************
       ETCHKN-TENSO-SEC          SECTION.
      *
           IF   SYU-KOH1HKNNUM   =   "970"
              IF    WRK-RSIETC   NOT =   ZERO
                COMPUTE SYU-HKNTEN(11) =  SYU-HKNTEN(11)
                                       -  WRK-RSIETC
              END-IF
              MOVE    WRK-RSIETC2   TO    SYU-RSIETC-MONEY
           END-IF
      *
           IF   SYU-HKNTEN(2)   >= WRK-SYUG-SDO-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(2)  =  SYU-HKNTEN(2)
                                       -  WRK-SYUG-SDO-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(2)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(3)   >= WRK-SYUG-ZTK-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(3)  =  SYU-HKNTEN(3)
                                       -  WRK-SYUG-ZTK-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(3)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(15)  >= WRK-SYUG-NYN-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(15) =  SYU-HKNTEN(15)
                                       -  WRK-SYUG-NYN-HKNTEN(IDX)
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(15)
              MOVE      1       TO    WRK-GENTEN-ERR
           END-IF
           IF   SYU-HKNTEN(5)   >= WRK-SYUG-CSY-HKNTEN(IDX)
              COMPUTE   SYU-HKNTEN(5)  =  SYU-HKNTEN(5)
                                       -  WRK-SYUG-CSY-HKNTEN(IDX)
              IF  WRK-CSY-YAKTEN(IDX)
                                <  WRK-SYUG-CSY-HKNTEN(IDX)
                MOVE    1       TO    WRK-GENTEN-ERR2
              END-IF
           ELSE
              MOVE      ZERO    TO    SYU-HKNTEN(5)
              MOVE      1       TO    WRK-GENTEN-ERR2
           END-IF
      *
      ***  PERFORM  VARYING  SYU-IDX  FROM  1  BY  1
      ***            UNTIL   SYU-IDX   >    31
      ***    COMPUTE   SDAY-HKNTEN(SYU-IDX) =
      ***              SDAY-HKNTEN(SYU-IDX) -
      ***              WRK-SDAYG-HKNTEN(IDX SYU-IDX)
      ***  END-PERFORM
      *
           .
       ETCHKN-TENSO-EXT.
           EXIT.
      *
      **************************************************************
      *    ÊÝ¸±Å¬ÍÑ³°¤Î¥Æ¡¼¥Ö¥ëÎÎ°è¥»¥Ã¥È
      **************************************************************
       HKNTEKGAI-TBL-SEC          SECTION.
      *
           EVALUATE    SRY-SRYKBN
               WHEN    "95"
                   PERFORM     VARYING    IDZ    FROM    1     BY    1
                                 UNTIL   (IDZ         >   5   )   OR
                                         (SRY-SRYCD(IDZ)  =   SPACE)
                       IF    SRY-SRYCD (IDZ) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDZ) (4:2)    =   "00"
                                                     OR       "91"     
                                                     OR       "92"     
                                                     OR       "93"     
                                                     OR       "94"     
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE     LNK-S02-HOSPNUM
                                                      TO  TNS-HOSPNUM
                               MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                               MOVE     SYU-SRYYMD(1:6)
                                                     TO   TNS-YUKOSTYMD
                                                          TNS-YUKOEDYMD
                               MOVE     "01"     TO   TNS-YUKOSTYMD(7:2)
                                                      TNS-YUKOEDYMD(7:2)
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-NYUTENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXNASI(1)
                                               =   TBL-JIHI-TAXNASI(1)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXNASI(2)
                                               =   TBL-JIHI-TAXNASI(2)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXNASI(3)
                                               =   TBL-JIHI-TAXNASI(3)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXNASI(4)
                                               =   TBL-JIHI-TAXNASI(4)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXNASI(5)
                                               =   TBL-JIHI-TAXNASI(5)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    6
                                          COMPUTE  TBL-JIHI-TAXNASI(6)
                                               =   TBL-JIHI-TAXNASI(6)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    7
                                          COMPUTE  TBL-JIHI-TAXNASI(7)
                                               =   TBL-JIHI-TAXNASI(7)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    8
                                          COMPUTE  TBL-JIHI-TAXNASI(8)
                                               =   TBL-JIHI-TAXNASI(8)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    9
                                          COMPUTE  TBL-JIHI-TAXNASI(9)
                                               =   TBL-JIHI-TAXNASI(9)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    10
                                          COMPUTE  TBL-JIHI-TAXNASI(10)
                                               =   TBL-JIHI-TAXNASI(10)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDZ) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)  =
                                                TBL-TGMONEY(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)  =
                                                TBL-TGMONEY(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)  =
                                                TBL-TGMONEY(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)  =
                                                TBL-TGMONEY(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)  =
                                                TBL-TGMONEY(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY(7)  =
                                                TBL-TGMONEY(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(8)  =
                                                TBL-TGMONEY(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(9)  =
                                                TBL-TGMONEY(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY(14)  =
                                                TBL-TGMONEY(14)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(10)  =
                                                TBL-TGMONEY(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
      ***                          WHEN   "81"
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "82"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "83"
                                       COMPUTE  TBL-TGMONEY(12)  =
                                                TBL-TGMONEY(12)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "84"
                                       COMPUTE  TBL-TGMONEY(13)  =
                                                TBL-TGMONEY(13)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "85"
                                       COMPUTE  TBL-TGMONEY(16)  =
                                                TBL-TGMONEY(16)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "89"
                                       COMPUTE  TBL-TGMONEY(15)  =
                                                TBL-TGMONEY(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY(15)  =
                                                TBL-TGMONEY(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE     LNK-S02-HOSPNUM
                                                  TO  TNS-HOSPNUM
                           MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                           MOVE     SYU-SRYYMD(1:6)
                                                  TO  TNS-YUKOSTYMD
                                                      TNS-YUKOEDYMD
                           MOVE     "01"          TO  TNS-YUKOSTYMD(7:2)
                                                      TNS-YUKOEDYMD(7:2)
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY(1)  =
                                                TBL-TGMONEY(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY(2)  =
                                                TBL-TGMONEY(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY(3)  =
                                                TBL-TGMONEY(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY(5)  =
                                                TBL-TGMONEY(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY(6)  =
                                                TBL-TGMONEY(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY(7)  =
                                                TBL-TGMONEY(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY(8)  =
                                                TBL-TGMONEY(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY(9)  =
                                                TBL-TGMONEY(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY(14)  =
                                                TBL-TGMONEY(14)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY(10)  =
                                                TBL-TGMONEY(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                    EVALUATE   TNS-SRYSYUKBN
                                     WHEN       "800"
      ***                            WHEN       "810"
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "820"
                                       COMPUTE  TBL-TGMONEY(4)  =
                                                TBL-TGMONEY(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "830"
                                       COMPUTE  TBL-TGMONEY(12)  =
                                                TBL-TGMONEY(12)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "840"
                                       COMPUTE  TBL-TGMONEY(13)  =
                                                TBL-TGMONEY(13)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "850"
                                       COMPUTE  TBL-TGMONEY(16)  =
                                                TBL-TGMONEY(16)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "890"
                                       COMPUTE  TBL-TGMONEY(15)  =
                                                TBL-TGMONEY(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                    END-EVALUATE
                                   WHEN   "90"
                                   WHEN   "92"
                                       COMPUTE  TBL-TGMONEY(15)  =
                                                TBL-TGMONEY(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                         COMPUTE  TBL-TGMONEY(5)  =
                                                  TBL-TGMONEY(5)  +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       ELSE
                                         COMPUTE  TBL-TGMONEY(4)  =
                                                  TBL-TGMONEY(4)  +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                     OR   TNS-SRYCD(1:3)    =   "059"
                                       COMPUTE  TBL-TGMONEY(11)  =
                                                TBL-TGMONEY(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
               WHEN    "96"
                   PERFORM     VARYING    IDZ    FROM   1    BY   1
                                 UNTIL   (IDZ        >   5   )   OR
                                         (SRY-SRYCD(IDZ)  =   SPACE)
                       IF    SRY-SRYCD (IDZ) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDZ) (4:2)    =   "00"
                                                     OR       "91"      
                                                     OR       "92"      
                                                     OR       "93"      
                                                     OR       "94"      
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE     LNK-S02-HOSPNUM 
                                                      TO  TNS-HOSPNUM
                               MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                               MOVE     SYU-SRYYMD(1:6)
                                                 TO  TNS-YUKOSTYMD
                                                     TNS-YUKOEDYMD
                               MOVE     "01"     TO  TNS-YUKOSTYMD(7:2)
                                                     TNS-YUKOEDYMD(7:2)
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-NYUTENTTLKBN
                                       WHEN    1
                                          COMPUTE  TBL-JIHI-TAXARI(1)
                                               =   TBL-JIHI-TAXARI(1)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    2
                                          COMPUTE  TBL-JIHI-TAXARI(2)
                                               =   TBL-JIHI-TAXARI(2)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    3
                                          COMPUTE  TBL-JIHI-TAXARI(3)
                                               =   TBL-JIHI-TAXARI(3)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    4
                                          COMPUTE  TBL-JIHI-TAXARI(4)
                                               =   TBL-JIHI-TAXARI(4)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    5
                                          COMPUTE  TBL-JIHI-TAXARI(5)
                                               =   TBL-JIHI-TAXARI(5)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    6
                                          COMPUTE  TBL-JIHI-TAXARI(6)
                                               =   TBL-JIHI-TAXARI(6)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    7
                                          COMPUTE  TBL-JIHI-TAXARI(7)
                                               =   TBL-JIHI-TAXARI(7)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    8
                                          COMPUTE  TBL-JIHI-TAXARI(8)
                                               =   TBL-JIHI-TAXARI(8)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    9
                                          COMPUTE  TBL-JIHI-TAXARI(9)
                                               =   TBL-JIHI-TAXARI(9)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                       WHEN    10
                                          COMPUTE  TBL-JIHI-TAXARI(10)
                                               =   TBL-JIHI-TAXARI(10)
                                               +  (SRY-JIHIMONEY(IDZ)
                                                   *  SINRYO-COUNT) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDZ) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)  =
                                                TBL-TGMONEY-TAX(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)  =
                                                TBL-TGMONEY-TAX(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)  =
                                                TBL-TGMONEY-TAX(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)  =
                                                TBL-TGMONEY-TAX(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)  =
                                                TBL-TGMONEY-TAX(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY-TAX(7)  =
                                                TBL-TGMONEY-TAX(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(8)  =
                                                TBL-TGMONEY-TAX(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(9)  =
                                                TBL-TGMONEY-TAX(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY-TAX(14)  =
                                                TBL-TGMONEY-TAX(14)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(10)  =
                                                TBL-TGMONEY-TAX(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
      ***                          WHEN   "81"
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "82"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "83"
                                       COMPUTE  TBL-TGMONEY-TAX(12)  =
                                                TBL-TGMONEY-TAX(12)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "84"
                                       COMPUTE  TBL-TGMONEY-TAX(13)  =
                                                TBL-TGMONEY-TAX(13)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "85"
                                       COMPUTE  TBL-TGMONEY-TAX(16)  =
                                                TBL-TGMONEY-TAX(16)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "89"
                                       COMPUTE  TBL-TGMONEY-TAX(15)  =
                                                TBL-TGMONEY-TAX(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "90"
                                       COMPUTE  TBL-TGMONEY-TAX(15)  =
                                                TBL-TGMONEY-TAX(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE    LNK-S02-HOSPNUM TO  TNS-HOSPNUM
                           MOVE    SRY-SRYCD(IDZ) TO   TNS-SRYCD
                           MOVE     SYU-SRYYMD(1:6)  TO  TNS-YUKOSTYMD
                                                         TNS-YUKOEDYMD
                           MOVE     "01"         TO  TNS-YUKOSTYMD(7:2)
                                                     TNS-YUKOEDYMD(7:2)
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
                                       COMPUTE  TBL-TGMONEY-TAX(1)  =
                                                TBL-TGMONEY-TAX(1)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "13"
                                       COMPUTE  TBL-TGMONEY-TAX(2)  =
                                                TBL-TGMONEY-TAX(2)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "14"
                                       COMPUTE  TBL-TGMONEY-TAX(3)  =
                                                TBL-TGMONEY-TAX(3)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "21"   THRU  "27"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
                                       COMPUTE  TBL-TGMONEY-TAX(5)  =
                                                TBL-TGMONEY-TAX(5)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "40"
                                       COMPUTE  TBL-TGMONEY-TAX(6)  =
                                                TBL-TGMONEY-TAX(6)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "50"
                                       COMPUTE  TBL-TGMONEY-TAX(7)  =
                                                TBL-TGMONEY-TAX(7)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "54"
                                       COMPUTE  TBL-TGMONEY-TAX(8)  =
                                                TBL-TGMONEY-TAX(8)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "60"
                                       COMPUTE  TBL-TGMONEY-TAX(9)  =
                                                TBL-TGMONEY-TAX(9)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "64"
                                       COMPUTE  TBL-TGMONEY-TAX(14)  =
                                                TBL-TGMONEY-TAX(14)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "70"
                                       COMPUTE  TBL-TGMONEY-TAX(10)  =
                                                TBL-TGMONEY-TAX(10)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   "80"
                                    EVALUATE   TNS-SRYSYUKBN
                                     WHEN       "800"
      ***                            WHEN       "810"
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "820"
                                       COMPUTE  TBL-TGMONEY-TAX(4)  =
                                                TBL-TGMONEY-TAX(4)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "830"
                                       COMPUTE  TBL-TGMONEY-TAX(12)  =
                                                TBL-TGMONEY-TAX(12)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "840"
                                       COMPUTE  TBL-TGMONEY-TAX(13)  =
                                                TBL-TGMONEY-TAX(13)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "850"
                                       COMPUTE  TBL-TGMONEY-TAX(16)  =
                                                TBL-TGMONEY-TAX(16)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     WHEN       "890"
                                       COMPUTE  TBL-TGMONEY-TAX(15)  =
                                                TBL-TGMONEY-TAX(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                    END-EVALUATE
                                   WHEN   "90"
                                   WHEN   "92"
                                       COMPUTE  TBL-TGMONEY-TAX(15)  =
                                                TBL-TGMONEY-TAX(15)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
                                       IF    TNS-YKZKBN     =    4
                                         COMPUTE  TBL-TGMONEY-TAX(5) =
                                                  TBL-TGMONEY-TAX(5) +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       ELSE
                                         COMPUTE  TBL-TGMONEY-TAX(4) =
                                                  TBL-TGMONEY-TAX(4) +
                                                 (SRY-JIHIMONEY(IDZ) *
                                                  SINRYO-COUNT)
                                       END-IF
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
                                     OR   TNS-SRYCD(1:3)    =   "059"
                                       COMPUTE  TBL-TGMONEY-TAX(11)  =
                                                TBL-TGMONEY-TAX(11)  +
                                               (SRY-JIHIMONEY(IDZ) *
                                                SINRYO-COUNT)
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
       HKNTEKGAI-TBL-EXT.
           EXIT.
      *
      **************************************************************
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û¤Î½¸·×¥Æ¡¼¥Ö¥ë¤«¤é¼ýÇ¼¥Æ¡¼¥Ö¥ë¤Ø¤Î¥»¥Ã¥È
      **************************************************************
       HKNTEKGAI-SET-SEC          SECTION.
      *
      *    ·×»»¶â³Û·å¿ô¥Á¥§¥Ã¥¯
           MOVE   ZERO                  TO  WRK-SKYCHK-KEISAN2
           MOVE   ZERO                  TO  WRK-SKYCHK-KEISAN3
           ADD    SYU-TGMONEY(15)       TO  WRK-SKYCHK-KEISAN2
           ADD    SYU-TGMONEY-TAX(15)   TO  WRK-SKYCHK-KEISAN3
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL   IDZ    >  16
             ADD  TBL-TGMONEY(IDZ)      TO  WRK-SKYCHK-KEISAN2
             ADD  TBL-TGMONEY-TAX(IDZ)  TO  WRK-SKYCHK-KEISAN3
           END-PERFORM
           IF   WRK-SKYCHK-KEISAN2  >  9999999
           OR   WRK-SKYCHK-KEISAN3  >  9999999
             MOVE      1     TO   FLG-SKYCHK-KEISAN2
           END-IF
      *
           MOVE   ZERO                  TO  WRK-SKYCHK-KEISAN2
           MOVE   ZERO                  TO  WRK-SKYCHK-KEISAN3
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL   IDZ    >  10
             ADD  TBL-JIHI-TAXNASI(IDZ) TO  WRK-SKYCHK-KEISAN2
             ADD  TBL-JIHI-TAXARI(IDZ)  TO  WRK-SKYCHK-KEISAN3
           END-PERFORM
           IF   WRK-SKYCHK-KEISAN2  >  9999999
           OR   WRK-SKYCHK-KEISAN3  >  9999999
             MOVE      1     TO   FLG-SKYCHK-KEISAN2
           END-IF
      *
      *    ÊÝ¸±Å¬ÍÑ³°
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL   IDZ    >  16
             ADD   TBL-TGMONEY(IDZ)      TO  SYU-TGMONEY(IDZ)
             ADD   TBL-TGMONEY-TAX(IDZ)  TO  SYU-TGMONEY-TAX(IDZ)
             ADD   TBL-TGMONEY(IDZ)      TO  SYU-TGMONEY(17)
             ADD   TBL-TGMONEY-TAX(IDZ)  TO  SYU-TGMONEY-TAX(17)
           END-PERFORM
      *
      *    ¼«Èñ£±¤«¤é£±£°
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL   IDZ    >  10
             MOVE  TBL-JIHI-TAXNASI(IDZ) TO  SYU-JIHI-TAXNASI(IDZ)
             MOVE  TBL-JIHI-TAXARI(IDZ)  TO  SYU-JIHI-TAXARI(IDZ)
             ADD   TBL-JIHI-TAXNASI(IDZ) TO  SYU-JIHI-TOTAL
             ADD   TBL-JIHI-TAXARI(IDZ)  TO  SYU-JIHI-TOTAL-TAX
           END-PERFORM
      *
           .
       HKNTEKGAI-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÅÀÅ©ÌôºÞÅÀ¿ô¤Þ¤È¤áÍÑ¤Ë¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß            *
      ******************************************************************
       TENTEK-GET-SEC                 SECTION.
      *
      *¿ÇÎÅ¹Ô°Ù¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO               TO   FLG-SRYACT
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO   SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
           MOVE    ACCT-SRYKA         TO   SRY-SRYKA
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
          IF      MCP-RC             =    ZERO
                MOVE    "tbl_sryact"  TO  MCP-TABLE
                MOVE    "key2"        TO   MCP-PATHNAME
                PERFORM  SRYACT-READ-SEC
          ELSE
                MOVE    1                TO   FLG-SRYACT
          END-IF
          MOVE    "tbl_sryact"      TO  MCP-TABLE
          MOVE    "key2"            TO   MCP-PATHNAME
          PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       TENTEK-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÅÀÅ©ÌôºÞ¡Ê´ïºà¡ËÅÀ¿ô¤Þ¤È¤á½èÍý                              *
      ******************************************************************
       TENTEK-MATOME-SEC                 SECTION.
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    SYU-PTID            TO  ORCSTEN-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  ORCSTEN-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  ORCSTEN-HKNCOMBI
           MOVE    SYU-HOSPNUM          TO  SPA-HOSPNUM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    ÌôºÞÅÀ¿ô¤È´ïºàÅÀ¿ô
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       31
      *
               IF   SYU-DAY(IDX1)   =   "1"  OR  "2"  OR  "8"  OR  "9"
                 IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                  IF  WRK-DOJITU-FLG2(IDX IDX1) = 1
                      ADD    ORCSTEN-ZAITEN(IDX1)  TO  SYU-HKNTEN (05)
                      ADD    ORCSTEN-ZAITEN(IDX1)
                                               TO  WRK-CSY-YAKTEN(IDX)
                      IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                        CONTINUE
                      ELSE
                        ADD  ORCSTEN-ZAITEN(IDX1)  TO  SDAY-HKNTEN(IDX1)
                      END-IF
                  ELSE
                   EVALUATE  WRK-DOJITU-FLG(IDX IDX1)
                     WHEN      0
                      ADD    ORCSTEN-ZAITEN(IDX1)  TO  SYU-HKNTEN (05)
                      ADD    ORCSTEN-ZAITEN(IDX1)
                                               TO  WRK-CSY-YAKTEN(IDX)
                      IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                        CONTINUE
                      ELSE
                        ADD  ORCSTEN-ZAITEN(IDX1)  TO  SDAY-HKNTEN(IDX1)
                      END-IF
                     WHEN      1
                     WHEN      7
                      ADD    ORCSTEN-DOU-ZAITEN(IDX1 1)
                                                   TO  SYU-HKNTEN (05)
                      ADD    ORCSTEN-DOU-ZAITEN(IDX1 1)
                                               TO  WRK-CSY-YAKTEN(IDX)
                      IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                        CONTINUE
                      ELSE
                        ADD  ORCSTEN-DOU-ZAITEN(IDX1 1)
                                                   TO  SDAY-HKNTEN(IDX1)
                      END-IF
                     WHEN      8
                     WHEN      9
                      IF   ORCSTEN-ZAITEN(IDX1) =
                                            ORCSTEN-DOU-ZAITEN(IDX1 2)
                        ADD    ORCSTEN-DOU-ZAITEN(IDX1 2)
                                                   TO  SYU-HKNTEN (05)
                        ADD    ORCSTEN-DOU-ZAITEN(IDX1 2)
                                               TO  WRK-CSY-YAKTEN(IDX)
                        IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                          CONTINUE
                        ELSE
                          ADD  ORCSTEN-DOU-ZAITEN(IDX1 2)
                                                   TO  SDAY-HKNTEN(IDX1)
                        END-IF
                      ELSE
                        COMPUTE   WRK-DOJITU-TENTEK    =
                                  ORCSTEN-ZAITEN(IDX1) -
                                  ORCSTEN-DOU-ZAITEN(IDX1 1)
                        ADD    WRK-DOJITU-TENTEK   TO  SYU-HKNTEN (05)
                        ADD    WRK-DOJITU-TENTEK
                                               TO  WRK-CSY-YAKTEN(IDX)
                        IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                          CONTINUE
                        ELSE
                          ADD  WRK-DOJITU-TENTEK   TO  SDAY-HKNTEN(IDX1)
                        END-IF
                      END-IF
                   END-EVALUATE
                  END-IF
                 END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM  VARYING  IDX1    FROM    1   BY  1
                      UNTIL  IDX1     >       50
                       OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                    MOVE    ZERO   TO   WRK-ZAIKAISU
                    PERFORM  VARYING  IDX2    FROM    1   BY  1
                          UNTIL   IDX2     >       31
                      IF   SYU-DAY(IDX2) = "1" OR "2" OR "8" OR "9"
                       IF  WRK-DOJITU-FLG2(IDX IDX2) = 1
                           IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             COMPUTE SDAY-HKNTEN(IDX2) =
                                     SDAY-HKNTEN(IDX2) +
                                   ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                     ORCSTEN-KIZ-DAY (IDX1 IDX2) )
                           END-IF
                           ADD   ORCSTEN-KIZ-DAY (IDX1 IDX2)
                                                 TO   WRK-ZAIKAISU
                       ELSE
                        EVALUATE  WRK-DOJITU-FLG(IDX IDX2)
                          WHEN      0
                           IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             COMPUTE SDAY-HKNTEN(IDX2) =
                                     SDAY-HKNTEN(IDX2) +
                                   ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                     ORCSTEN-KIZ-DAY (IDX1 IDX2) )
                           END-IF
                           ADD   ORCSTEN-KIZ-DAY (IDX1 IDX2)
                                                 TO   WRK-ZAIKAISU
                          WHEN      1
                          WHEN      7
                           IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             COMPUTE SDAY-HKNTEN(IDX2) =
                                     SDAY-HKNTEN(IDX2) +
                                   ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                     ORCSTEN-KIZ-DAY2(IDX1 IDX2 1) )
                           END-IF
                           ADD   ORCSTEN-KIZ-DAY2(IDX1 IDX2 1)
                                                 TO   WRK-ZAIKAISU
                          WHEN      8
                          WHEN      9
                           IF  SYU-SYUHKNNUM = "971" OR "973" OR "975"
                             CONTINUE
                           ELSE
                             COMPUTE SDAY-HKNTEN(IDX2) =
                                     SDAY-HKNTEN(IDX2) +
                                   ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                     ORCSTEN-KIZ-DAY2(IDX1 IDX2 2) )
                           END-IF
                           ADD   ORCSTEN-KIZ-DAY2(IDX1 IDX2 2)
                                                 TO   WRK-ZAIKAISU
                        END-EVALUATE
                       END-IF
                      END-IF
                    END-PERFORM
      *
                    COMPUTE  SYU-HKNTEN (05)  =
                             SYU-HKNTEN (05)  +
                             ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                               WRK-ZAIKAISU )
      *
           END-PERFORM
      *
      ***  ¿ÇÎÅ¼ïÊÌ¡Ö¡¥£³£´£°¡×¤Î¿ÇÎÅ¹Ô°ÙÆþÎÏ¥Á¥§¥Ã¥¯¡Ê¥ì¥»¥×¥È½èÍý»þ¡Ë
      *
           IF   LNK-S02-TEIKI-KBN   =   "X"
      *
             INITIALIZE                   ORCSTENTEK2AREA
             MOVE    SYU-HOSPNUM      TO  LNK-ORCSTEN2-HOSPNUM
             MOVE    SYU-PTID         TO  LNK-ORCSTEN2-PTID
             MOVE    SYU-SRYYMD(1:6)  TO  LNK-ORCSTEN2-SRYYM
             MOVE    SYU-HKNCOMBINUM  TO  LNK-ORCSTEN2-HKNCOMBI
             CALL    "ORCSTENTEK2"  USING
                                    ORCSTENTEK2AREA
                                    SPA-AREA
             IF      LNK-ORCSTEN2-RC  =   ZERO
               IF   (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
               OR   (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1    >  40
                   IF  LNK-ORCSTEN2-ZAINUM(IDX1) NOT = ZERO
                     PERFORM  TENTEK-MATOME-CHECK-SEC
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
           END-IF
      *
           .
      *
       TENTEK-MATOME-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¿ÇÎÅ¼ïÊÌ¡Ö¡¥£³£´£°¡×¤Î¿ÇÎÅ¹Ô°ÙÆþÎÏ¥Á¥§¥Ã¥¯¡Ê¥ì¥»¥×¥È½èÍý»þ¡Ë*
      ******************************************************************
       TENTEK-MATOME-CHECK-SEC                 SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SYU-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SYU-PTID            TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-ZAINUM(IDX1)
                                       TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF      MCP-RC  =   ZERO
             MOVE  "tbl_sryacct"       TO  MCP-TABLE
             MOVE  "key57"             TO  MCP-PATHNAME
             PERFORM  SRYACCT-READ-SEC
             IF  FLG-SRYACCT = ZERO
               IF   ACCT-YKZTEN     NOT = ZERO
               OR   ACCT-KIZAITEN   NOT = ZERO
                 INITIALIZE                   SRYACT-REC
                 MOVE    ACCT-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE    ACCT-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE    ACCT-PTID        TO  SRY-PTID
                 MOVE    ACCT-SRYYM       TO  SRY-SRYYM
                 MOVE    ACCT-ZAINUM      TO  SRY-ZAINUM
                 MOVE    SRYACT-REC       TO  MCPDATA-REC
                 MOVE    "DBSELECT"       TO  MCP-FUNC
                 MOVE    "tbl_sryact"     TO  MCP-TABLE
                 MOVE    "key9"           TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"      USING MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                 IF      MCP-RC  =   ZERO
                   MOVE  "tbl_sryact"     TO  MCP-TABLE
                   MOVE  "key9"           TO  MCP-PATHNAME
                   PERFORM  SRYACT-READ-SEC
                   IF  FLG-SRYACT = ZERO
                     IF  SRY-SRYSYUKBN = "340"
                       MOVE     1         TO  WRK-TENTEKI-ERR
                     END-IF
                   END-IF
                 END-IF
                 MOVE    "tbl_sryact"     TO  MCP-TABLE
                 MOVE    "key9"           TO  MCP-PATHNAME
                 PERFORM    CSR-CLOSE-SEC
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM    CSR-CLOSE-SEC
      *
           .
      *
       TENTEK-MATOME-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD(1:6)     TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               MOVE     ZERO     TO        WRK-SYS5000-COUNT
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM  SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      TABLE-GENSAN
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             PERFORM  SYSKANRI-READ-SEC
             PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                      UNTIL     GENSAN-IDX   >   4
                  PERFORM   VARYING   SYU-IDX   FROM  1  BY  1
                            UNTIL     SYU-IDX   >  31
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              MOVE   5   TO  TBL-INNAI-KANSEN(SYU-IDX)
                            WHEN  3
                              MOVE   10  TO  TBL-IRYOU-ANZEN(SYU-IDX)
                            WHEN  4
                              MOVE   5   TO  TBL-JOKUSOU(SYU-IDX)
                        END-EVALUATE
                    END-IF
                  END-PERFORM
             END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   CSR-CLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-S02-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SYU-SRYYMD(1:6)     TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE   SYS-5000-STYUKYMD(7:2)  TO  KIRIKAE-IDXX
                   PERFORM    VARYING  SYU-IDX  FROM KIRIKAE-IDX BY 1
                              UNTIL    SYU-IDX  >   31
                       IF  SYS-5000-GENKBN(2)   =   "2"
                           MOVE   5      TO  TBL-INNAI-KANSEN
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-INNAI-KANSEN
                                                       (SYU-IDX)
                       END-IF
                       IF  SYS-5000-GENKBN(3)   =   "2"
                           MOVE   10     TO  TBL-IRYOU-ANZEN
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-IRYOU-ANZEN
                                                       (SYU-IDX)
                       END-IF
                       IF  SYS-5000-GENKBN(4)   =   "2"
                           MOVE   5      TO  TBL-JOKUSOU
                                                       (SYU-IDX)
                       ELSE
                           MOVE   ZERO   TO  TBL-JOKUSOU
                                                       (SYU-IDX)
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key11"             TO  MCP-PATHNAME
                   PERFORM  SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key11"             TO  MCP-PATHNAME
             PERFORM   CSR-CLOSE-SEC
      *
           .
      *
       SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       NYUINACT-ERAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÄêÎÅÍÜ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SENTEICDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       SENTEICDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       SANTEI-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß½èÍý
      **************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-4001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
                                           SYS-5113-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-1001-REC
                                           SYS-4001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
                                           SYS-5113-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
grpsys                                SPA-AREA
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
           PERFORM  CSR-CLOSE-SEC
      *
           .
       TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
      *    INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           .
       CSR-CLOSE-EXT.
           EXIT.
