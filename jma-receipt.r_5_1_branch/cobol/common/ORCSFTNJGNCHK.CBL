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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSFTNJGNCHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ´µ¼Ô¼«¸ÊÉéÃ´¶â¥ì¥»¥×¥Èµ­ºÜÈ½Äê
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/11/15  ¸øÈñ·î¾å¸Â³Û¼èÆÀ½¤Àµ
      *  01.00.02    NACL-ÌçÏÆ    04/11/30  Æþ±¡ÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    05/06/01  Ä¹´ü¹â³ÛÎÅÍÜ¥ì¥»µ­ºÜÈ½ÄêÄÉ²Ã
      *  01.00.04    NACL-ÌçÏÆ    05/08/12  ¼¢²ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *  01.00.05    NACL-ÌçÏÆ    05/10/04  Åçº¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/09/05  Ä¹´ü£¹£·£´ÂÐ±þ
      *  03.03.01    NACL-ÌçÏÆ    06/11/02  ¸º³Û¡¦ÌÈ½ü¡¦»ÙÊ§Í±Í½ÂÐ±þ
      *  03.04.01    NACL-ÌçÏÆ    07/02/14  Ê¡Åç¸©ÈÓ´ÛÂ¼ÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/03/22  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *                                     ¡ÊÀî¸ý¡¦½ÕÆüÉô»Ô¡Ë
      *  03.04.03    NACL-ÌçÏÆ    07/06/04  Ä¹ºê»ÔÃÏÊý¸øÈñÂÐ±þ
      *
      *  03.05.01    NACL-Æ£¸¶    07/04/19  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-ÌçÏÆ    07/08/01  ½êÆÀ¾ðÊó¼è°·¤¤µ¡Ç½¶¯²½ÂÐ±þ
      *  03.05.03    NACL-ÌçÏÆ    07/08/01  ·îÅÓÃæ¤Î½êÆÀ¾ðÊóÊÑ¹¹ÂÐ±þ
      *  04.01.01    NACL-ÌçÏÆ    08/01/18  ºë¶Ì¸©ÀîÅçÄ®ÃÏÊý¸øÈñÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    08/01/31  ¼¢²ì¸©£´£´£±ÄÉ²Ã¤ËÈ¼¤¦½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/03/19  À¸ÊÝ¤Î£·£¹Ê»ÍÑ»þ¤ÎÂÐ±þ
      *  04.02.02    NACL-ÌçÏÆ    08/04/24  ºë¶Ì¸©»ÖÌÚ»ÔÃÏÊý¸øÈñÂÐ±þ
      *  04.02.03    NACL-ÌçÏÆ    08/07/02  ÂçºåÉÜ£´£±Ï·¿Í¡Ü£±£¸£¸ÅùÂÐ±þ
      *  04.02.04    NACL-ÌçÏÆ    08/08/07  Àº¿ÀÄÌ±¡¤ÎÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *  04.02.05    NACL-ÌçÏÆ    08/09/04  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.02.06    NACL-ÌçÏÆ    08/10/02  ËÌ³¤Æ»ÆÃÄê¼À´µÂÐ±þ
      *  04.02.07    NACL-ÌçÏÆ    08/09/02  ÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *  04.02.08    NACL-ÌçÏÆ    08/11/05  £±£°±ßÌ¤Ëþ»Í¼Î¸ÞÆþ¤·¤Ê¤¤¾ì¹ç¤Î
      *                                     È½Äê½¤Àµ
      *  04.03.01    NACL-ÌçÏÆ    08/11/28  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.03.02    NACL-ÌçÏÆ    08/12/08  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ½¤Àµ
      *  04.03.03    NACL-ÌçÏÆ    08/12/10  ÀéÍÕ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.03.04    NACL-ÌçÏÆ    08/12/17  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *  04.03.05    NACL-ÌçÏÆ    09/01/13  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *  04.03.06    NACL-ÌçÏÆ    09/03/10  È½Äê¾ò·ï½¤Àµ
      *  04.03.07    NACL-ÌçÏÆ    09/03/13  Á°´ü¹âÎð¼Ô¤Î¹â³Û·×»»ÂÐ±þ
      *
      *  04.04.01    NACL-ÌçÏÆ    09/06/11  £·£°ºÐÌ¤Ëþ¤Î¹â³Û·×»»ÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/07/02  Ä¹ºê¸©£±£¸£°³°ÍèÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/07/16  ¸øÈñ·î¾å¸Â³ÛÈ½Äê
      *  04.04.04    NACL-ÌçÏÆ    09/09/07  °¦ÃÎ¸©Ã±ÆÈ£µ£±ÂÐ±þ
      *  04.04.05    NACL-ÌçÏÆ    09/09/17  ÅìµþÅÔ¥Þ¥ë»ÒÂÐ±þ
      *                                     ¡ÊÊ¿À®£²£±Ç¯£±£°·î¿ÇÎÅ¡Á¡Ë
      *  04.04.06    NACL-ÌçÏÆ    09/12/14  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *                                     ¡Ê¿·ºÂ¡¦Ä«²â¡¦ÏÂ¸÷¡¦»ÖÌÚ»Ô¡Ë
      *  04.04.07    NACL-ÌçÏÆ    09/12/25  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡ÊÄ«²â»Ô¡Ë
      *  04.04.08    NACL-ÌçÏÆ    10/04/14  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡Ê¹ÔÅÄ»Ô¡Ë
      *  04.04.09    NACL-ÌçÏÆ    10/05/21  °ñ¾ë¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.10    NACL-ÌçÏÆ    10/09/14  Ê¡²¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.11    NACL-ÌçÏÆ    10/11/12  Ä¹ºê¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.12    NACL-ÌçÏÆ    10/12/06  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.04.13    NACL-ÌçÏÆ    11/02/17  Ä¹ºê¸©ÃÏÊý¸øÈñÂÐ±þ
      *
      *  04.05.01    NACL-ÌçÏÆ    09/12/04  ¿©»ö¡ÊÆÃÊÌ¿©¡Ë¤ÎÂ¾ÊÝ¸±ÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    11/03/08  Ä¹ºê¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    11/11/17  ºë¶Ì¸©ÃÏÊý¸øÈñÂÐ±þ¡Ê¾¾ÉúÄ®¡Ë
      *  04.05.04    NACL-ÌçÏÆ    12/04/27  ÂçºåÉÜÃÏÊý¸øÈñ¡ÜÄ¹´üÂÐ±þ
      *  04.05.05    NACL-ÌçÏÆ    12/06/04  À¸ÊÝ¤Î£²£´Ê»ÍÑ»þ¤ÎÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    12/06/11  Ä¹´ü¤ÎÈ½Äê½¤Àµ
      *  04.05.07    NACL-ÌçÏÆ    12/07/31  Ä¹´ü¤ÎÈ½Äê½¤Àµ
      *  04.05.08    NACL-ÌçÏÆ    12/09/18  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.05.09    NACL-ÌçÏÆ    12/09/19  Ê¡Åç¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.05.10    NACL-ÌçÏÆ    13/02/18  Åçº¬¸©ÃÏÊý¸øÈñÂÐ±þ
      *
      *  04.06.01    NACL-ÌçÏÆ    11/12/13  ±¡³°½èÊý¤Î¤ß¤ÎÅÀ¸¡ÍÑ¥ì¥»¥×¥ÈÂÐ±þ
      *                                     ¡ÊÀÁµáÅÀ¿ô£°ÅÀ¸øÈñÊ¬¡Ë
      *  04.06.02    NACL-ÌçÏÆ    13/03/01  £·£°ºÐÂåÁ°È¾¤ÎÆÃÎãÁ¼ÃÖ±äÄ¹ÂÐ±þ
      *  04.06.03    NACL-ÌçÏÆ    13/06/14  À¸ÊÝ¤Î£·£¹¡¦£²£´Ê»ÍÑ»þ¤ÎÂÐ±þ½¤Àµ
      *  04.06.04    NACL-ÌçÏÆ    13/10/09  Ä¹ºê¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.06.05    NACL-ÌçÏÆ    13/11/28  ¸å´ü¹âÎð¤Î·îÅÓÃæ¸©³°Å¾½ÐÂÐ±þ
      *
      *  04.07.01    NACL-ÌçÏÆ    11/09/01  ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°µ¡Ç½ÄÉ²ÃÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    12/08/27  ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°µ¡Ç½ÄÉ²ÃÂÐ±þ
      *  04.07.03    NACL-ÌçÏÆ    13/07/01  ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°µ¡Ç½ÄÉ²ÃÂÐ±þ
      *  04.07.04    NACL-ÌçÏÆ    13/07/10  ¼«ÇåÀÕ¡ÊÂè»°¼Ô¹Ô°Ù¡ËÂÐ±þ
      *  04.07.05    NACL-ÌçÏÆ    13/11/11  ¸º³Û¡Ê±ß¡Ë¤ÎÂÐ±þ
      *  04.07.06    NACL-ÌçÏÆ    14/03/04  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.07    NACL-ÌçÏÆ    14/08/29  Åçº¬¡¦ÆàÎÉ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.07.08    NACL-ÌçÏÆ    15/03/17  ÀÁµáÅÀ¿ô£°ÅÀ¸øÈñ¥ì¥»µ­ºÜÂÐ±þ
      *  04.07.09    NACL-ÌçÏÆ    15/05/18  ÀÁµáÅÀ¿ô£°ÅÀ¸øÈñ¥ì¥»µ­ºÜÂÐ±þ
      *
      *  04.08.01    NACL-ÌçÏÆ    15/10/13  ÀÁµá¥¼¥í¥Õ¥é¥°ÊÔ½¸
      *  04.08.02    NACL-ÌçÏÆ    15/12/02  £°ÅÀÃÏ°è¸øÈñµ­ºÜÂÐ±þ
      *  04.08.03    NACL-ÌçÏÆ    17/06/13  ¸ºÌÈ»þ¤ÎÈ½Äê½¤Àµ
      *  04.08.04    NACL-ÌçÏÆ    17/09/15  Ê¡Åç¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.08.05    NACL-ÌçÏÆ    17/10/18  Æþ±¡ÎÁ¤È°Û¤Ê¤ëÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
      *                                     ¤Ç¤Î¿©»ö»»ÄêÂÐ±þ
      *  04.08.06    NACL-ÌçÏÆ    17/12/07  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.08.07    NACL-ÌçÏÆ    18/01/09  ·§ËÜ¸©ÃÏÊý¸øÈñÂÐ±þ
      *  04.08.08    NACL-ÌçÏÆ    18/01/25  £°ÅÀ¸øÈñ¥ì¥»µ­ºÜ½¤Àµ
      *                                     ¡ÊÊñ³çºÞ¤Î¤ß¤Î¾ì¹ç¤âÂÐ¾Ý¤È¤¹¤ë¡Ë
      *
      *  05.00.01    NACL-ÌçÏÆ    18/11/27  ÀÁµáÅÀ¿ô¥Á¥§¥Ã¥¯½¤Àµ
      *  05.00.02    NACL-ÌçÏÆ    18/12/07  ´Î¤¬¤ó¡¦½ÅÅÙ´Î¹ÅÊÑ¼£ÎÅ¸¦µæÂ¥¿Ê
      *                                     »ö¶È°åÎÅÂÐ±þ
      *  05.00.03    NACL-ÌçÏÆ    19/04/12  ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°¥Ç¥Õ¥©¥ë¥È
      *                                     ÀßÄêÊÑ¹¹ÂÐ±þ
      *  05.00.04    NACL-ÌçÏÆ    20/03/05  ¿··¿¥³¥í¥Ê¥¦¥¤¥ë¥¹ÂÐ±þ
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-CHECK           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-GENMEN          PIC 9(01).
           03  FLG-SAIGAI-ARI      PIC 9(01).
           03  FLG-SAIGAI-ARI2     PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-TOKU75          PIC 9(01).
           03  FLG-SKJRYOYOHI-ARI  PIC 9(01).
           03  FLG-KOREI           PIC 9(01).
           03  FLG-SAIGAI-TAISYO   PIC 9(01).
           03  FLG-TENKEN          PIC 9(01).
           03  FLG-ZENKI-TOKUREI   PIC 9(01).
           03  FLG-KOREI-HIGAITO   PIC 9(01).
           03  FLG-SEIHO-CHECK     PIC 9(01).
           03  FLG-HKNCOMBI-CHECK  PIC 9(01).
           03  FLG-SKJ-CHECK       PIC 9(01).
           03  FLG-SRYACCT-CHECK   PIC 9(01).
           03  FLG-SRYACCT-CHECK3  PIC 9(01).
           03  FLG-SRYACCT-CHECK4  PIC 9(01).
           03  FLG-SRYACT-CHECK    PIC 9(01).
           03  FLG-GENMEN-ARI      PIC 9(01).
           03  FLG-GENMEN-NASI     PIC 9(01).
           03  FLG-CHECK-METHOD    PIC 9(01).
           03  FLG-KANGAN-CHECK    PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  SYOTOKU-IDX         PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-HKNMST-JGNGAK  PIC 9(06).
           03  WRK-HKNMST-JGNGAK2 PIC 9(06).
           03  WRK-TAIHI-JGNGAK   PIC 9(06).
           03  WRK-KOHFTN         PIC 9(10).
           03  WRK-KOHFTN-EN      PIC 9(10)V9(01).
           03  WRK-KOHFTN-H       PIC 9(10)V9(01).
           03  WRK-KOHFTN9-H      PIC 9(10)V9(01).
           03  WRK-KOHFTN9-X      PIC 9(10).
           03  WRK-KOHFTN-X       PIC 9(10).
           03  WRK-KOHFTN-SKJ     PIC 9(10).
           03  WRK-KOHFTN-TOKU75  PIC 9(10).
           03  WRK-KOHFTN-RITU    PIC 9(03).
           03  WRK-KOHFTN-HKNFTNMONEY
                                  PIC 9(10).
           03  WRK-KOHFTN-JYO-HKNFTNMONEY
                                  PIC 9(10).
           03  WRK-SEIKYU         PIC 9(10).
           03  WRK-SEIKYU-TOTAL   PIC 9(10).
           03  WRK-KYUFUGAITEN    PIC 9(10).
           03  WRK-SKJ-FTN        PIC 9(10).
           03  WRK-PREFNUM        PIC 9(02).
           03  WRK-CHIHOUNUMKBN   PIC X(01).
      *
           03  WRK-TEISYOTOKU     PIC X(01).
           03  WRK-TEISYOTOKU2    PIC X(01).
           03  WRK-RECEJGNDSPKBN  PIC X(01).
           03  WRK-RECEJGNDSPKBN2 PIC X(01).
      *
           03  WRK-HKNCOMBINUM-TBL.
               05  WRK-HKNCOMBINUM-OCC      OCCURS 50.
                 07  WRK-HKNCOMBINUM        PIC 9(04).
                 07  WRK-HKNCOMBINUM-HKNID  PIC 9(10).
                 07  WRK-HKNCOMBINUM-FLG    PIC 9(01).
                 07  WRK-HKNCOMBINUM-KYUFUGAI
                                            PIC 9(01).
                 07  WRK-HKNCOMBINUM-ZEROFLG
                                            PIC 9(01).
      *
           03  WRK-HKNNUM                   PIC X(03).
           03  WRK-HKNNUM2                  PIC X(03).
      *
           03  WRK-HKNJANUM-TBL.
               05  WRK-HKNJANUM             PIC X(08).
               05  WRK-HONKZKKBN            PIC X(01).
               05  WRK-KIGO                 PIC X(80).
               05  WRK-NUM                  PIC X(80).
           03  WRK-HKNJANUM2-TBL.
               05  WRK-HKNJANUM2            PIC X(08).
               05  WRK-HONKZKKBN2           PIC X(01).
               05  WRK-KIGO2                PIC X(80).
               05  WRK-NUM2                 PIC X(80).
      *
           03  WRK-CONTKBN                  PIC X(01).
           03  WRK-CONTKBN2                 PIC X(01).
      *
           03  WRK-TEKSTYMD                 PIC X(08).
           03  WRK-TEKSTYMD2                PIC X(08).
      *
           03  WRK-HKNID                    PIC 9(10).
           03  WRK-SRYYMD                   PIC X(08).
           03  WRK-COMPFTN                  PIC 9(07).
      *
           03  WRK-SYUSKJ-FTN               PIC 9(07).
           03  WRK-SKYMONEY-SKJ             PIC 9(07).
      *
           03  WRK-SYS2010-TAIHI-TBL.
               05  WRK-RECEKSIFLG-G         PIC X(01).
               05  WRK-RECEKSIFLG-N         PIC X(01).
               05  WRK-CHKJGNFLG-G          PIC X(01).
               05  WRK-CHKJGNFLG-N          PIC X(01).
               05  WRK-KOHSKYFLG-G          PIC X(01).
               05  WRK-KOHSKYFLG-N          PIC X(01).
               05  WRK-IRYOKEIKBN-G         PIC X(01).
               05  WRK-IRYOKEIKBN-N         PIC X(01).
      *
           03  WRK-KYURATE                  PIC 9(03).
      *
           03  WRK-BIRTHDAY                 PIC X(08).
      *
           03  WRK-ZERO-KOH-PRTKBN          PIC X(01).
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK2010.INC".
      *
           COPY    "CPSK200501.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSFTNJGNCHK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSFTNJGNCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-FTNJGNCHK-RC
                                           LNK-FTNJGNCHK-TENKEN-RC
                                           LNK-FTNJGNCHK-KOHFTN
                                           LNK-FTNJGNCHK-KOHFTNSKJ
      *
           MOVE    SPACE               TO  LNK-FTNJGNCHK-KOHSKYFLG
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-FTNJGNCHK-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC      =   ZERO
             IF  LNK-FTNJGNCHK-KOHNUM  =   "970" OR "940"
                MOVE    01    TO   LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC      =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           MOVE    LNK-FTNJGNCHK-RC   TO   LNK-FTNJGNCHK-TENKEN-RC
           IF   (LNK-FTNJGNCHK-RC      =   1     )
           AND  (FLG-TENKEN            =   1     )
      ***  AND  (LNK-FTNJGNCHK-HKNID   =  ZERO   )
           AND  (LNK-FTNJGNCHK-SRYYM  >= "200804")
              MOVE     ZERO           TO   LNK-FTNJGNCHK-TENKEN-RC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    PTINF-BIRTHDAY          TO  WRK-BIRTHDAY
      *
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHNUM   = SPACE
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHID    =  ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           MOVE        ZERO      TO     FLG-CHECK
           MOVE        ZERO      TO     FLG-OK
           MOVE        SPACE     TO     WRK-TEISYOTOKU
           MOVE        SPACE     TO     WRK-TEISYOTOKU2
           MOVE        SPACE     TO     WRK-RECEJGNDSPKBN
           MOVE        SPACE     TO     WRK-RECEJGNDSPKBN2
           MOVE        ZERO      TO     WRK-KOHFTN-RITU
           MOVE        ZERO      TO     FLG-ZENKI-TOKUREI
           MOVE        ZERO      TO     FLG-KOREI-HIGAITO
           MOVE        999       TO     WRK-KYURATE
           MOVE        ZERO      TO     WRK-TAIHI-JGNGAK
      *
      *    Á°´ü¹âÎð¼Ô¡Ê£·£°ºÐ¡Á£·£´ºÐ¡ËÆÃÎãÂÐ¾Ý¼Ô¥Á¥§¥Ã¥¯
           PERFORM    2006-ZENKI-TOKUREI-HANTEI-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤«¤é¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¤ÈÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
           PERFORM    2001-HKNNUM-GET-SEC
      *
           IF   FLG-HKNNUM     =    ZERO
      *
             IF ((LNK-FTNJGNCHK-PREFNUM =  32            ) AND
                 (LNK-FTNJGNCHK-KOHNUM  = "191"          ) AND
                 (LNK-FTNJGNCHK-SRYYM  >= "200510"       ))
             OR ((LNK-FTNJGNCHK-PREFNUM =  11            ) AND
                 (LNK-FTNJGNCHK-KOHNUM  = "483"          ) AND
                 (LNK-FTNJGNCHK-SRYYM  >= "200804"       ))
             OR ((LNK-FTNJGNCHK-PREFNUM =  11            ) AND
                 (LNK-FTNJGNCHK-KOHNUM  = "344"          ) AND
                 (LNK-FTNJGNCHK-SRYYM  >= "200507"       ))
             OR ((LNK-FTNJGNCHK-KOHNUM(1:1)
                                       >= "1" AND <= "8" ) AND
                 (LNK-FTNJGNCHK-SRYYM  >= "200810"       ))
                 PERFORM    2005-SYOTOKU-HENSYU-SEC
             END-IF
      *
      *      ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°ÊÔ½¸
             PERFORM      2002-SYS2010-HENSYU-SEC
      *
      *      ÀßÄê³Û°Ê¾åµ­ºÜ¤Ê¤·¡ÊÀßÄê³ÛÌ¤Ëþ¡¦°Ê¾å¤Ç°Û¤Ê¤ëÉéÃ´¶â·×»»¡Ë
      *      ÂÐ¾Ý¥Á¥§¥Ã¥¯
             IF   (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8")
             AND  (HKN-RECEJGNDSPKBN          = "0"           )
               IF    LNK-FTNJGNCHK-NYUGAIKBN  = "1"
                 IF   (WRK-IRYOKEIKBN-N   NOT =  SPACE)
                 AND  (WRK-RECEKSIFLG-N       =  "3"  )
                   MOVE   WRK-IRYOKEIKBN-N   TO   WRK-RECEJGNDSPKBN2
                 END-IF
               ELSE
                 IF   (WRK-IRYOKEIKBN-G   NOT =  SPACE)
                 AND  (WRK-RECEKSIFLG-G       =  "3"  )
                   MOVE   WRK-IRYOKEIKBN-G   TO   WRK-RECEJGNDSPKBN2
                 END-IF
               END-IF
             END-IF
      *
             IF   (HKN-RECEJGNDSPKBN    =   "1"           )
             OR   (HKN-HKNNUM           =   "972" OR "974")
             OR   (WRK-RECEJGNDSPKBN    =   "1"           )
      *        ÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
               PERFORM    2003-PTKOHFTN-GET-SEC
             END-IF
      *
      *      ÀßÄê¤È°Û¤Ê¤ëµ­ºÜÀßÄê¤ÎÊÔ½¸
             MOVE   WRK-HKNMST-JGNGAK   TO  WRK-TAIHI-JGNGAK
             IF    LNK-FTNJGNCHK-NYUGAIKBN   =   "1"
               EVALUATE  WRK-RECEKSIFLG-N
                 WHEN      "1"
                    MOVE    999999      TO  WRK-HKNMST-JGNGAK
                 WHEN      "2"
                    MOVE    999998      TO  WRK-HKNMST-JGNGAK
               END-EVALUATE
             ELSE
               EVALUATE  WRK-RECEKSIFLG-G
                 WHEN      "1"
                    MOVE    999999      TO  WRK-HKNMST-JGNGAK
                 WHEN      "2"
                    MOVE    999998      TO  WRK-HKNMST-JGNGAK
               END-EVALUATE
             END-IF
      *
      *      ÊÝ¸±¤Î¾å¸Â³Û¤È¼ýÇ¼¤ÎÉéÃ´³Û¤òÈæ³ÓÈ½Äê
             PERFORM      2004-HANTEI-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¤«¤é¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¤ÈÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
      *****************************************************************
       2001-HKNNUM-GET-SEC       SECTION.
      *
           IF ((LNK-FTNJGNCHK-PREFNUM =  32            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "191" OR "291"
                                     OR "190" OR "290" ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200510"       ))
           OR ((LNK-FTNJGNCHK-PREFNUM =  27            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "141" OR "241" ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200804"       ))
               MOVE     "1"      TO     WRK-RECEJGNDSPKBN
           END-IF
      *
           INITIALIZE                           HKNNUM-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM  TO    HKN-HOSPNUM
           MOVE    LNK-FTNJGNCHK-KOHNUM   TO    HKN-HKNNUM
           MOVE   "tbl_hknnum"            TO    MCP-TABLE
           MOVE   "key2"                  TO    MCP-PATHNAME
           PERFORM  910-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM           =       ZERO
               MOVE    "tbl_hknnum"       TO    MCP-TABLE
               MOVE    "key2"             TO    MCP-PATHNAME
               PERFORM  910-HKNNUM-READ-SEC
               PERFORM  UNTIL
                   ( FLG-HKNNUM          =   1   )   OR
                   ((HKN-TEKSTYMD(1:6)
                                  <=   LNK-FTNJGNCHK-SRYYM) AND
                    (LNK-FTNJGNCHK-SRYYM
                                  <=   HKN-TEKEDYMD(1:6)))
                      MOVE   "tbl_hknnum" TO    MCP-TABLE
                      MOVE   "key2"       TO    MCP-PATHNAME
                      PERFORM 910-HKNNUM-READ-SEC
               END-PERFORM
           END-IF
      *    ÊÝ¸±ÈÖ¹æ¥¯¥í¡¼¥º
           MOVE     "tbl_hknnum" TO    MCP-TABLE
           MOVE     "key2"       TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    FLG-HKNNUM     =    ZERO
             IF    LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8"
                CONTINUE
             ELSE
                MOVE    "0"      TO    HKN-RECEJGNDSPKBN
             END-IF
           END-IF
      *
           .
       2001-HKNNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°ÊÔ½¸
      *****************************************************************
       2002-SYS2010-HENSYU-SEC       SECTION.
      *
           MOVE     SPACE    TO     WRK-SYS2010-TAIHI-TBL
      *
           IF   LNK-FTNJGNCHK-KOHNUM(1:1) >=  "1" AND <=  "8"
      *
             MOVE     LNK-FTNJGNCHK-HKNID  TO  WRK-HKNID
             PERFORM  200411111-HKNINF-KENSAKU-SEC
      *
             IF   LNK-FTNJGNCHK-SRYYM     >=  "200804"
                INITIALIZE                       SYS-2010-REC
                MOVE  "2010"                 TO  SYS-2010-KANRICD
                MOVE  LNK-FTNJGNCHK-KOHNUM   TO  SYS-2010-KBNCD
                MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  SYS-2010-HOSPNUM
                MOVE  LNK-FTNJGNCHK-SRYYM    TO  SYS-2010-STYUKYMD(1:6)
                MOVE  "01"                   TO  SYS-2010-STYUKYMD(7:2)
                MOVE  LNK-FTNJGNCHK-SRYYM    TO  SYS-2010-EDYUKYMD(1:6)
                MOVE  "01"                   TO  SYS-2010-EDYUKYMD(7:2)
                MOVE  SYS-2010-REC           TO  MCPDATA-REC
                MOVE  "DBSELECT"             TO  MCP-FUNC
                MOVE  "tbl_syskanri"         TO  MCP-TABLE
                MOVE  "key10"                TO  MCP-PATHNAME
                CALL  "ORCDBMAIN"            USING   MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                IF     MCP-RC   =   ZERO
                   MOVE     "tbl_syskanri"   TO  MCP-TABLE
                   MOVE     "key10"          TO  MCP-PATHNAME
                   PERFORM   SYSKANRI-READ-SEC
                ELSE
                   INITIALIZE                    SYS-2010-REC
                END-IF
                PERFORM      20021-SYS2010-SET-SEC
                MOVE        "tbl_syskanri"   TO  MCP-TABLE
                MOVE        "key10"          TO  MCP-PATHNAME
                PERFORM      910-DB-CLOSE-SEC
             ELSE
                EVALUATE  LNK-FTNJGNCHK-PREFNUM
                  WHEN      42
                    IF  (LNK-FTNJGNCHK-KOHNUM  = "287" OR "380"
                                              OR "281" OR "282"       )
                    AND (LNK-FTNJGNCHK-SRYYM  >= "200704"             )
                    AND (PTHKN-HKNNUM  NOT = "060" AND "067" AND "039")
                      MOVE  "2"  TO WRK-RECEKSIFLG-G
                      MOVE  "2"  TO WRK-RECEKSIFLG-N
                    END-IF
                END-EVALUATE
                EVALUATE  LNK-FTNJGNCHK-PREFNUM
      **>         WHEN      12
      **>             MOVE  "2"  TO WRK-CHKJGNFLG-G
      **>             MOVE  "1"  TO WRK-CHKJGNFLG-N
                  WHEN      25
                    IF   LNK-FTNJGNCHK-SRYYM   >= "200508"
                      MOVE  "1"  TO WRK-CHKJGNFLG-N
                    END-IF
                  WHEN      40
                      MOVE  "1"  TO WRK-CHKJGNFLG-N
                END-EVALUATE
                EVALUATE  LNK-FTNJGNCHK-PREFNUM
                  WHEN      15
                    IF  (HKN-HBTNUM    = "80" OR "82" OR "84" OR
                                         "86" OR "00"           )
                    AND (PTHKN-HKNNUM  = "060" OR "067" OR "039") 
                      MOVE  "1"  TO WRK-KOHSKYFLG-G
                    END-IF
                  WHEN      29
                    IF  (HKN-HBTNUM    = "71" OR "81" OR "91"   )
                    AND (PTHKN-HKNNUM  = "060" OR "067" OR "039") 
                      MOVE  "1"  TO WRK-KOHSKYFLG-G
                      MOVE  "1"  TO WRK-KOHSKYFLG-N
                    END-IF
                  WHEN      40
                    IF  (HKN-HBTNUM    NOT = "41"      )
                    AND (PTHKN-HKNNUM  = "060" OR "067") 
                      MOVE  "1"  TO WRK-KOHSKYFLG-G
                      MOVE  "1"  TO WRK-KOHSKYFLG-N
                    END-IF
                END-EVALUATE
                PERFORM   200211-SYS2010-SET2-SEC
                PERFORM   200212-SYS2010-SET3-SEC
             END-IF
             PERFORM      20021A-SYS2010-KURIA-SEC
      *
      *      ¸øÈñÀÁµá¥Æ¡¼¥Ö¥ëºîÀ®¥Õ¥é¥°¤ÎÊÔ½¸
             IF    LNK-FTNJGNCHK-NYUGAIKBN   =   "1"
               MOVE   WRK-KOHSKYFLG-N   TO  LNK-FTNJGNCHK-KOHSKYFLG
             ELSE
               MOVE   WRK-KOHSKYFLG-G   TO  LNK-FTNJGNCHK-KOHSKYFLG
             END-IF
      *
           END-IF
      *
           .
       2002-SYS2010-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°¥»¥Ã¥È
      *****************************************************************
       20021-SYS2010-SET-SEC       SECTION.
      *
           EVALUATE  SYS-2010-RECEKSIKBN-G
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
                 WHEN      42
                  IF ((LNK-FTNJGNCHK-KOHNUM  = "287" OR "380"
                                            OR "281" OR "282"      ) AND
                      (PTHKN-HKNNUM NOT = "060" AND "067" AND "039"))
                  OR ((LNK-FTNJGNCHK-KOHNUM  = "188"               ) AND
                      (PTHKN-HKNNUM NOT = "060" AND "067" AND "039") AND
                      (LNK-FTNJGNCHK-SRYYM  >= "201310"            ))
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-RECEKSIFLG-G
             WHEN      "1"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "1"  TO WRK-RECEKSIFLG-G
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "1"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-G
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-G
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "3"  TO WRK-RECEKSIFLG-G
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "3"  TO WRK-RECEKSIFLG-G
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-G
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-RECEKSIKBN-N
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
                 WHEN      11
                  IF   LNK-FTNJGNCHK-KOHNUM  = "481" OR "482" OR "483"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
                  IF   LNK-FTNJGNCHK-KOHNUM  = "383"
                  AND  LNK-FTNJGNCHK-SRYYM  >= "201001"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN      42
                  IF  (LNK-FTNJGNCHK-KOHNUM  = "287" OR "380"
                                            OR "281" OR "282"       )
                  AND (PTHKN-HKNNUM  NOT = "060" AND "067" AND "039")
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-RECEKSIFLG-N
             WHEN      "1"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "1"  TO WRK-RECEKSIFLG-N
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "2"  TO WRK-RECEKSIFLG-N
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-RECEKSI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "3"  TO WRK-RECEKSIFLG-N
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "3"  TO WRK-RECEKSIFLG-N
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "3"  TO WRK-RECEKSIFLG-N
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-CHKJGNKBN-G
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
      **>        WHEN      12
      **>           MOVE  "2"  TO WRK-CHKJGNFLG-G
                 WHEN      13
                  IF   LNK-FTNJGNCHK-KOHNUM  = "588"
                  AND  LNK-FTNJGNCHK-SRYYM  >= "200910"
                    MOVE  "1"  TO WRK-CHKJGNFLG-G
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-CHKJGNFLG-G
             WHEN      "1"
                    MOVE  "1"  TO WRK-CHKJGNFLG-G
             WHEN      "2"
                    MOVE  "2"  TO WRK-CHKJGNFLG-G
           END-EVALUATE
      *
           EVALUATE  SYS-2010-CHKJGNKBN-N
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
                 WHEN      11
                  IF ((LNK-FTNJGNCHK-KOHNUM  = "783"   ) AND
                      (LNK-FTNJGNCHK-SRYYM  >= "201004"))
                  OR ((LNK-FTNJGNCHK-KOHNUM  = "683"   ) AND
                      (LNK-FTNJGNCHK-SRYYM  >= "200904"))
                    MOVE  "1"  TO WRK-CHKJGNFLG-N
                  END-IF
      **>        WHEN      12
                 WHEN      25
                 WHEN      40
                    MOVE  "1"  TO WRK-CHKJGNFLG-N
                 WHEN      42
                  IF  (LNK-FTNJGNCHK-KOHNUM  = "182" OR "282")
                  AND (LNK-FTNJGNCHK-SRYYM  >= "201012"      )
                    MOVE  "1"  TO WRK-CHKJGNFLG-N
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-CHKJGNFLG-N
             WHEN      "1"
                    MOVE  "1"  TO WRK-CHKJGNFLG-N
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KOHSKYKBN-G
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
                 WHEN      15
                  IF  (HKN-HBTNUM    = "80" OR "82" OR "84" OR
                                       "86" OR "00"           )
                  AND (PTHKN-HKNNUM  = "060" OR "067" OR "039") 
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
                 WHEN      29
                  IF  (HKN-HBTNUM    = "71" OR "81" OR "91" OR
                                       "80"                   )
                  AND (PTHKN-HKNNUM  = "060" OR "067" OR "039") 
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
                 WHEN      40
                  IF  (HKN-HBTNUM    NOT = "41"      )
                  AND (PTHKN-HKNNUM  = "060" OR "067") 
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-KOHSKYFLG-G
             WHEN      "1"
               EVALUATE  SYS-2010-KOHSKY-TAIKBN-G
                 WHEN        "0"
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-G
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-KOHSKYKBN-N
             WHEN      SPACE
               EVALUATE   LNK-FTNJGNCHK-PREFNUM
                 WHEN      29
                  IF  (HKN-HBTNUM    = "71" OR "81" OR "91" OR
                                       "80"                   )
                  AND (PTHKN-HKNNUM  = "060" OR "067" OR "039") 
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
                 WHEN      40
                  IF  (HKN-HBTNUM    NOT = "41"      )
                  AND (PTHKN-HKNNUM  = "060" OR "067") 
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
               END-EVALUATE
             WHEN      "0"
                    MOVE SPACE TO WRK-KOHSKYFLG-N
             WHEN      "1"
               EVALUATE  SYS-2010-KOHSKY-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                 WHEN        "1"
                  IF   PTHKN-HKNNUM  = "060" OR "067" OR "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
                 WHEN        "2"
                  IF   PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
                 WHEN        "3"
                  IF   PTHKN-HKNNUM  = "060" OR "067"
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
                 WHEN        "4"
                  IF   PTHKN-HKNNUM  = "039"
                    MOVE  "1"  TO WRK-KOHSKYFLG-N
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-IRYOKEIKBN-G
             WHEN      SPACE
               PERFORM   200211-SYS2010-SET2-SEC
             WHEN      "0"
               IF  (LNK-FTNJGNCHK-PREFNUM  =   11     )
               AND (LNK-FTNJGNCHK-KOHNUM   =  "281"   )
               AND (LNK-FTNJGNCHK-SRYYM   >=  "200704")
               AND (LNK-FTNJGNCHK-SRYYM   <=  "201904")
                    MOVE  "1"  TO WRK-IRYOKEIKBN-G
               END-IF
             WHEN      "1"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "1"  TO WRK-IRYOKEIKBN-G
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "1"  TO WRK-IRYOKEIKBN-G
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "2"  TO WRK-IRYOKEIKBN-G
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "2"  TO WRK-IRYOKEIKBN-G
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "3"  TO WRK-IRYOKEIKBN-G
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "3"  TO WRK-IRYOKEIKBN-G
                  END-IF
               END-EVALUATE
             WHEN      "4"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-G
                 WHEN        "0"
                    MOVE  "4"  TO WRK-IRYOKEIKBN-G
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "4"  TO WRK-IRYOKEIKBN-G
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           EVALUATE  SYS-2010-IRYOKEIKBN-N
             WHEN      SPACE
               PERFORM   200212-SYS2010-SET3-SEC
             WHEN      "0"
               IF  (LNK-FTNJGNCHK-PREFNUM  =   11     )
               AND (LNK-FTNJGNCHK-KOHNUM   =  "281"   )
               AND (LNK-FTNJGNCHK-SRYYM   >=  "200704")
               AND (LNK-FTNJGNCHK-SRYYM   <=  "201904")
                    MOVE  "1"  TO WRK-IRYOKEIKBN-N
               END-IF
             WHEN      "1"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "1"  TO WRK-IRYOKEIKBN-N
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "1"  TO WRK-IRYOKEIKBN-N
                  END-IF
               END-EVALUATE
             WHEN      "2"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "2"  TO WRK-IRYOKEIKBN-N
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "2"  TO WRK-IRYOKEIKBN-N
                  END-IF
               END-EVALUATE
             WHEN      "3"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "3"  TO WRK-IRYOKEIKBN-N
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "3"  TO WRK-IRYOKEIKBN-N
                  END-IF
               END-EVALUATE
             WHEN      "4"
               EVALUATE  SYS-2010-IRYOKEI-TAIKBN-N
                 WHEN        "0"
                    MOVE  "4"  TO WRK-IRYOKEIKBN-N
                 WHEN        "1"
                  IF   WRK-TEISYOTOKU2  NOT =  SPACE
                    MOVE  "4"  TO WRK-IRYOKEIKBN-N
                  END-IF
               END-EVALUATE
           END-EVALUATE
      *
           .
       20021-SYS2010-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°¥»¥Ã¥È£²
      *****************************************************************
       200211-SYS2010-SET2-SEC       SECTION.
      *
           EVALUATE   LNK-FTNJGNCHK-PREFNUM
             WHEN       07
               IF ((LNK-FTNJGNCHK-KOHNUM    =  "245"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200412" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "246"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "445" OR "545" OR "645"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201210" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-G
               END-IF
             WHEN       11
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "343" OR "342"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200507" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "344"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200507" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "181" OR "182" OR "183"            ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200704" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "543" OR "582"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200710" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "381"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200801" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "481" OR "482" OR "743" OR "442"
                                            OR "542"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "483"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "383"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201001" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "681" OR "682"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "683"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "781"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "783"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201004" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "372" OR "373"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201101" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "462" OR "562" OR "662" OR "762"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201112" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "672"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201210" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "281"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200704" ) AND
                   (LNK-FTNJGNCHK-SRYYM    <= "201904" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-G
               END-IF
             WHEN       43
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "341" OR "442"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200912" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201210" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-G
               END-IF
           END-EVALUATE
      *
           .
       200211-SYS2010-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°¥»¥Ã¥È£³
      *****************************************************************
       200212-SYS2010-SET3-SEC       SECTION.
      *
           EVALUATE   LNK-FTNJGNCHK-PREFNUM
             WHEN       07
               IF ((LNK-FTNJGNCHK-KOHNUM    =  "245"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200412" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "246"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "445" OR "545" OR "645"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201210" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-N
               END-IF
             WHEN       11
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                    "181" OR "182" OR "183"            ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200704" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "543" OR "582"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200710" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "381"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200801" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "743" OR "442" OR "542"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "681" OR "682"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "683"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "781"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "783"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201004" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "372" OR "373"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201101" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "462" OR "762"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201112" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "672"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "201210" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "281"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200704" ) AND
                   (LNK-FTNJGNCHK-SRYYM    <= "201904" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-N
               END-IF
             WHEN       43
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "341" OR "442"   ) AND
                   (LNK-FTNJGNCHK-SRYYM    >= "200912" ))
                 MOVE    "1"    TO  WRK-IRYOKEIKBN-N
               END-IF
           END-EVALUATE
      *
           .
       200212-SYS2010-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£±£°¥»¥Ã¥È¥¯¥ê¥¢
      *****************************************************************
       20021A-SYS2010-KURIA-SEC       SECTION.
      *
           EVALUATE  LNK-FTNJGNCHK-PREFNUM
             WHEN      42
               IF ((LNK-FTNJGNCHK-KOHNUM  = "287" OR "380"
                                         OR "281" OR "282"       ) AND
                   (LNK-FTNJGNCHK-SRYYM   < "200704"             ) AND
                   (PTHKN-HKNNUM  NOT = "060" AND "067" AND "039"))
               OR ((LNK-FTNJGNCHK-KOHNUM  = "188"                ) AND
                   (PTHKN-HKNNUM  NOT = "060" AND "067" AND "039") AND
                   (LNK-FTNJGNCHK-SRYYM   < "201310"             ))
                 MOVE  SPACE  TO  WRK-RECEKSIFLG-G
               END-IF
           END-EVALUATE
      *
           EVALUATE  LNK-FTNJGNCHK-PREFNUM
             WHEN      11
               IF  (LNK-FTNJGNCHK-KOHNUM  = "383"   )
               AND (LNK-FTNJGNCHK-SRYYM   < "201001")
                 MOVE  SPACE  TO  WRK-RECEKSIFLG-N
               END-IF
             WHEN      42
               IF  (LNK-FTNJGNCHK-KOHNUM  = "287" OR "380"
                                         OR "281" OR "282"       )
               AND (LNK-FTNJGNCHK-SRYYM   < "200704"             )
               AND (PTHKN-HKNNUM  NOT = "060" AND "067" AND "039")
                 MOVE  SPACE  TO  WRK-RECEKSIFLG-N
               END-IF
           END-EVALUATE
      *
           EVALUATE  LNK-FTNJGNCHK-PREFNUM
             WHEN      13
               IF  (LNK-FTNJGNCHK-KOHNUM  = "588"   )
               AND (LNK-FTNJGNCHK-SRYYM   < "200910")
                 MOVE  SPACE  TO  WRK-CHKJGNFLG-G
               END-IF
           END-EVALUATE
      *
           EVALUATE  LNK-FTNJGNCHK-PREFNUM
             WHEN      11
               IF ((LNK-FTNJGNCHK-KOHNUM  = "783"   ) AND
                   (LNK-FTNJGNCHK-SRYYM   < "201004"))
               OR ((LNK-FTNJGNCHK-KOHNUM  = "683"   ) AND
                   (LNK-FTNJGNCHK-SRYYM   < "200904"))
                 MOVE  SPACE  TO  WRK-CHKJGNFLG-N
               END-IF
            WHEN      25
               IF   LNK-FTNJGNCHK-SRYYM   < "200508"
                 MOVE  SPACE  TO  WRK-CHKJGNFLG-N
               END-IF
             WHEN      42
               IF  (LNK-FTNJGNCHK-KOHNUM  = "182" OR "282")
               AND (LNK-FTNJGNCHK-SRYYM   < "201012"      )
                 MOVE  SPACE  TO  WRK-CHKJGNFLG-N
               END-IF
           END-EVALUATE
      *
           EVALUATE  LNK-FTNJGNCHK-PREFNUM
             WHEN      29
               IF  (HKN-HBTNUM            = "80"           )
               AND (PTHKN-HKNNUM  = "060" OR "067" OR "039")
               AND (LNK-FTNJGNCHK-SRYYM   < "201410"       )
                 MOVE  SPACE  TO  WRK-KOHSKYFLG-G
                 MOVE  SPACE  TO  WRK-KOHSKYFLG-N
               END-IF
           END-EVALUATE
      *
           EVALUATE   LNK-FTNJGNCHK-PREFNUM
             WHEN       07
               IF ((LNK-FTNJGNCHK-KOHNUM    =  "245"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200412" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "246"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "445" OR "545" OR "645"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201210" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-G
               END-IF
             WHEN       11
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "343" OR "342"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200507" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "344"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200507" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "181" OR "182" OR "183" OR "281"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200704" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "543" OR "582"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200710" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "381"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200801" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "481" OR "482" OR "743" OR "442"
                                            OR "542"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "483"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "383"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201001" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "681" OR "682"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "683"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "781"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "783"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201004" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "372" OR "373"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201101" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                    "462" OR "562" OR "662" OR "762"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201112" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "672"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201210" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-G
               END-IF
             WHEN       43
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "341" OR "442"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200912" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201210" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-G
               END-IF
           END-EVALUATE
      *
           EVALUATE   LNK-FTNJGNCHK-PREFNUM
             WHEN       07
               IF ((LNK-FTNJGNCHK-KOHNUM    =  "245"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200412" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "246"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "445" OR "545" OR "645"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200604" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "480"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201210" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-N
               END-IF
             WHEN       11
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                    "181" OR "182" OR "183" OR "281"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200704" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "543" OR "582"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200710" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "381"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200801" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                             "743" OR "442" OR "542"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200804" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "681" OR "682"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "683"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "781"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200904" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "783"   ) AND
                   (WRK-TEISYOTOKU2     NOT =  SPACE   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201004" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "372" OR "373"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201101" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =
                                      "462" OR "762"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201112" ))
               OR ((LNK-FTNJGNCHK-KOHNUM    =  "672"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "201210" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-N
               END-IF
             WHEN       43
               IF ((LNK-FTNJGNCHK-KOHNUM    =
                                      "341" OR "442"   ) AND
                   (LNK-FTNJGNCHK-SRYYM     < "200912" ))
                 MOVE  SPACE  TO  WRK-IRYOKEIKBN-N
               END-IF
           END-EVALUATE
      *
           .
       20021A-SYS2010-KURIA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñÉéÃ´¥Þ¥¹¥¿¤«¤éÉéÃ´¾å¸Â³Û¤Î¼èÆÀ
      *****************************************************************
       2003-PTKOHFTN-GET-SEC       SECTION.
      *
           IF    LNK-FTNJGNCHK-NYUGAIKBN     =   "1"
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   MOVE   HKN-HON-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-N
                    WHEN      "1"
                      IF  HKN-HON-NYUDJGNGAK  NOT = ZERO
                        MOVE  HKN-HON-NYUDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE   HKN-HON-NYUKFTNRATE    TO  WRK-KOHFTN-RITU
                 WHEN     "1"
                 WHEN     "4"
                   MOVE   HKN-TNK-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-N
                    WHEN      "1"
                      IF  HKN-TNK-NYUDJGNGAK  NOT = ZERO
                        MOVE  HKN-TNK-NYUDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE   HKN-TNK-NYUKFTNRATE    TO  WRK-KOHFTN-RITU
                 WHEN     "2"
                 WHEN     "3"
                   MOVE   HKN-TSY-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-N
                    WHEN      "1"
                      IF  HKN-TSY-NYUDJGNGAK  NOT = ZERO
                        MOVE  HKN-TSY-NYUDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE   HKN-TSY-NYUKFTNRATE    TO  WRK-KOHFTN-RITU
               END-EVALUATE
           ELSE
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   IF    HKN-HON-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-HON-GAIMGAIJGNGAK TO  WRK-HKNMST-JGNGAK
                   END-IF
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-G
                    WHEN      "1"
                      IF  HKN-HON-GAIDJGNGAK  NOT = ZERO
                        MOVE  HKN-HON-GAIDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                    WHEN      "2"
                      IF  HKN-HON-GAIKJGNGAK  NOT = ZERO
                        MOVE  HKN-HON-GAIKJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE    HKN-HON-GAIKFTNRATE   TO  WRK-KOHFTN-RITU
                 WHEN     "1"
                 WHEN     "4"
                   IF    HKN-TNK-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TNK-GAIMGAIJGNGAK TO  WRK-HKNMST-JGNGAK
                   END-IF
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-G
                    WHEN      "1"
                      IF  HKN-TNK-GAIDJGNGAK  NOT = ZERO
                        MOVE  HKN-TNK-GAIDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                    WHEN      "2"
                      IF  HKN-TNK-GAIKJGNGAK  NOT = ZERO
                        MOVE  HKN-TNK-GAIKJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE    HKN-TNK-GAIKFTNRATE   TO  WRK-KOHFTN-RITU
                 WHEN     "2"
                   IF    HKN-TSY-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TSY-GAIMGAIJGNGAK TO  WRK-HKNMST-JGNGAK
                   END-IF
      *            ¥Á¥§¥Ã¥¯ÂÐ¾Ý¾å¸Â³ÛÈ½Äê
                   EVALUATE  WRK-CHKJGNFLG-G
                    WHEN      "1"
                      IF  HKN-TSY-GAIDJGNGAK  NOT = ZERO
                        MOVE  HKN-TSY-GAIDJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                    WHEN      "2"
                      IF  HKN-TSY-GAIKJGNGAK  NOT = ZERO
                        MOVE  HKN-TSY-GAIKJGNGAK TO  WRK-HKNMST-JGNGAK
                      END-IF
                   END-EVALUATE
                   MOVE    HKN-TSY-GAIKFTNRATE   TO  WRK-KOHFTN-RITU
               END-EVALUATE
           END-IF
      *
      *@@@ IF   (LNK-FTNJGNCHK-PREFNUM   =  11                    )
      *@@@ AND  (LNK-FTNJGNCHK-KOHNUM    = "343" OR "342" OR "344")
      *@@@ AND  (LNK-FTNJGNCHK-NYUGAIKBN = "1"                    )
      *@@@ AND  (LNK-FTNJGNCHK-SRYYM    >= "200507"               )
      *@@@          MOVE    999999      TO  WRK-HKNMST-JGNGAK
      *@@@ END-IF
      *@@@ IF   (LNK-FTNJGNCHK-PREFNUM   =  11                    )
      *@@@ AND  (LNK-FTNJGNCHK-KOHNUM    = "562" OR "662"         )
      *@@@ AND  (LNK-FTNJGNCHK-NYUGAIKBN = "1"                    )
      *@@@ AND  (LNK-FTNJGNCHK-SRYYM    >= "201112"               )
      *@@@          MOVE    999999      TO  WRK-HKNMST-JGNGAK
      *@@@ END-IF
      *
           .
       2003-PTKOHFTN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±¤Î¾å¸Â³Û¤È¼ýÇ¼¤ÎÉéÃ´³Û¤òÈæ³ÓÈ½Äê
      *****************************************************************
       2004-HANTEI-SEC            SECTION.
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ßÂÐ¾Ý¤ÎÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¼èÆÀ
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  COMB-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  COMB-PTID
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(1)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(2)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(3)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(4)
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  2004-HANTEI-EXT
           ELSE
               MOVE   "tbl_hkncombi"       TO  MCP-TABLE
               MOVE   "key4"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
      *
               MOVE      ZERO      TO      IDX
               PERFORM
                 UNTIL   (IDX              >=    50 )
                   OR    (FLG-HKNCOMBI  NOT =   ZERO)
      *
                 IF    COMB-DELKBN          =   SPACE
                 AND   COMB-TEKSTYMD(1:6)  <=   LNK-FTNJGNCHK-SRYYM
                 AND   LNK-FTNJGNCHK-SRYYM <=   COMB-TEKEDYMD(1:6)
      *
                   ADD     1       TO      IDX
                   MOVE COMB-HKNCOMBINUM TO WRK-HKNCOMBINUM(IDX)
                   MOVE COMB-HKNID       TO WRK-HKNCOMBINUM-HKNID(IDX)
      *
                   IF  (LNK-FTNJGNCHK-PREFNUM  =   40            )
                   AND (LNK-FTNJGNCHK-KOHNUM(1:1)
                                              >=  "1" AND <=  "8")
                   AND (LNK-FTNJGNCHK-SRYYM   >=  "200810"       )
                     INITIALIZE            ORCSKOHPLUS-AREA
                     MOVE  LNK-FTNJGNCHK-HOSPNUM
                                        TO LNK-SKOHPLUS-HOSPNUM
                     MOVE  LNK-FTNJGNCHK-PREFNUM
                                        TO LNK-SKOHPLUS-PREFNUM
                     MOVE  LNK-FTNJGNCHK-PTID
                                        TO LNK-SKOHPLUS-PTID
                     MOVE  WRK-HKNCOMBINUM(IDX)
                                        TO LNK-SKOHPLUS-HKNCOMBI
                     MOVE  LNK-FTNJGNCHK-SRYYM
                                        TO LNK-SKOHPLUS-SRYYMD(1:6)
                     MOVE  "01"         TO LNK-SKOHPLUS-SRYYMD(7:2)
                     CALL  "ORCSKOHPLUS"   USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
                     IF    (LNK-SKOHPLUS-RC     =   ZERO)
                     AND   (LNK-SKOHPLUS-FTNKBN =   "1" )
                        MOVE    1    TO    WRK-HKNCOMBINUM-KYUFUGAI(IDX)
                     END-IF
                   END-IF
      *
                   IF    WRK-RECEJGNDSPKBN      =   "1"
                   AND   LNK-FTNJGNCHK-PREFNUM  =   27
      *
                     IF ((COMB-KOH1HKNNUM = LNK-FTNJGNCHK-KOHNUM) AND
                         (COMB-KOH2HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ))
                     OR ((COMB-KOH2HKNNUM = LNK-FTNJGNCHK-KOHNUM) AND
                         (COMB-KOH3HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ))
                     OR ((COMB-KOH1HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ) AND
                         (COMB-KOH2HKNNUM = LNK-FTNJGNCHK-KOHNUM))
                     OR ((COMB-KOH2HKNNUM = "187" OR "188"
                                         OR "189" OR "190"      ) AND
                         (COMB-KOH3HKNNUM = LNK-FTNJGNCHK-KOHNUM))
                         MOVE  1   TO   WRK-HKNCOMBINUM-FLG(IDX)
                     END-IF
      *
                   END-IF
      *
                   IF    WRK-RECEJGNDSPKBN      =   "1"
                   AND   LNK-FTNJGNCHK-PREFNUM  =   32
                   AND   FLG-CHECK              =   ZERO
      *
                     IF   COMB-HKNNUM     NOT =  SPACE
                       IF   COMB-KOH1HKNNUM   =  "027"
                          IF  COMB-KOH1PAYKBN  =  "01"
                             CONTINUE
                          ELSE
                             MOVE    1    TO   FLG-CHECK
                          END-IF
                       ELSE
                          EVALUATE  LNK-FTNJGNCHK-NYUGAIKBN
                            WHEN       "1"
                              IF  COMB-HOJOKBN  =  "1" OR "5" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                            WHEN       "2"
                              IF  COMB-HOJOKBN  =  "1" OR "4" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                          END-EVALUATE
                       END-IF
      *
                       IF    FLG-CHECK                =   ZERO
                       AND   LNK-FTNJGNCHK-KOHNUM     =  "191"
                       AND   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND   WRK-TEISYOTOKU           =   SPACE
                       AND   FLG-OK                   =   ZERO
                       AND   LNK-FTNJGNCHK-SRYYM      <  "200610"
      *
                         MOVE    1           TO  FLG-OK
      *
                         INITIALIZE              ORCSAGECHKAREA
                         MOVE    LNK-FTNJGNCHK-HOSPNUM
                                             TO  AGECHK-HOSPNUM
                         MOVE    LNK-FTNJGNCHK-PTID
                                             TO  AGECHK-PTID
                         MOVE    LNK-FTNJGNCHK-SRYYM
                                             TO  AGECHK-SRYYMD(1:6)
                         MOVE    "01"        TO  AGECHK-SRYYMD(7:2)
                         MOVE    PTINF-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                         CALL    "ORCSAGECHK"    USING
                                                 ORCSAGECHKAREA
                                                 SPA-AREA
                         IF      AGECHK-RC       =   ZERO
                           IF  ( AGECHK-NENREI   >=  70      )
                           OR  ( AGECHK-NENREI   >=  65  AND
                                 COMB-KOH1HKNNUM  = "027"    )
                              MOVE    01      TO  LNK-FTNJGNCHK-RC
                              MOVE     1      TO  FLG-CHECK
                           END-IF
                         END-IF
                       END-IF
      *
                       IF   (FLG-CHECK                =   ZERO   )
                       AND  (LNK-FTNJGNCHK-KOHNUM     =  "191"   )
                       AND  (FLG-OK                   =   ZERO   )
                       AND  (LNK-FTNJGNCHK-SRYYM     >=  "200708")
                         MOVE     1           TO  FLG-OK
                         PERFORM 20042-FTNJANUM-CHK-SEC
                       END-IF
      *
                     END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE   "tbl_hkncombi"      TO  MCP-TABLE
                 MOVE   "key4"              TO  MCP-PATHNAME
                 PERFORM 910-HKNCOMBI-READ-SEC
      *
               END-PERFORM
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE     "tbl_hkncombi"      TO  MCP-TABLE
           MOVE     "key4"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ¼ýÇ¼¥Þ¥¹¥¿¤«¤é³ºÅö¸øÈñ¤Î´µ¼ÔÉéÃ´³Û¥Á¥§¥Ã¥¯
           IF   WRK-RECEJGNDSPKBN =  "1"
           AND  FLG-CHECK         =   1
           AND  LNK-FTNJGNCHK-RC  =  ZERO
              MOVE     ZERO    TO    FLG-CHECK
              MOVE     SPACE   TO    WRK-RECEJGNDSPKBN
           END-IF
      *
           IF   FLG-CHECK  =  ZERO
              PERFORM      20041-SYUKEI-SEC
           END-IF
      *
           .
       2004-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³ºÅö¸øÈñ¤Î´µ¼ÔÉéÃ´³Û½¸·×
      *****************************************************************
       20041-SYUKEI-SEC            SECTION.
      *
           MOVE   ZERO   TO   FLG-GENMEN
           MOVE   ZERO   TO   FLG-TOKU75
           MOVE   ZERO   TO   FLG-SKJRYOYOHI-ARI
           MOVE   ZERO   TO   FLG-ARI2
           MOVE   ZERO   TO   FLG-SAIGAI-ARI
           MOVE   ZERO   TO   FLG-SAIGAI-ARI2
           MOVE   ZERO   TO   FLG-SEIHO-CHECK
           MOVE   ZERO   TO   FLG-HKNCOMBI-CHECK
           MOVE   ZERO   TO   FLG-SKJ-CHECK
           MOVE   ZERO   TO   FLG-GENMEN-ARI
           MOVE   ZERO   TO   FLG-GENMEN-NASI
           MOVE   ZERO   TO   FLG-KANGAN-CHECK
      *
           MOVE   ZERO   TO   WRK-KOHFTN
           MOVE   ZERO   TO   WRK-KOHFTN-EN
           MOVE   ZERO   TO   WRK-KOHFTN-H
           MOVE   ZERO   TO   WRK-KOHFTN9-H
           MOVE   ZERO   TO   WRK-KOHFTN9-X
           MOVE   ZERO   TO   WRK-KOHFTN-X
           MOVE   ZERO   TO   WRK-KOHFTN-SKJ
           MOVE   ZERO   TO   WRK-KOHFTN-TOKU75
           MOVE   ZERO   TO   WRK-KOHFTN-HKNFTNMONEY
           MOVE   ZERO   TO   WRK-KOHFTN-JYO-HKNFTNMONEY
           MOVE   ZERO   TO   WRK-SEIKYU
           MOVE   ZERO   TO   WRK-SEIKYU-TOTAL
           MOVE   ZERO   TO   WRK-KYUFUGAITEN
           MOVE   ZERO   TO   WRK-SKJ-FTN
           MOVE   ZERO   TO   WRK-SYUSKJ-FTN
           MOVE   ZERO   TO   WRK-SKYMONEY-SKJ
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  SYU-PTID
           MOVE    LNK-FTNJGNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNK-FTNJGNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                    TO  SYU-SRYYMD(7:2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(1)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(3)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(4)
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key33"              TO  MCP-PATHNAME
           ELSE
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key25"              TO  MCP-PATHNAME
           END-IF
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  20041-SYUKEI-EXT
           ELSE
               IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key33"          TO  MCP-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key25"          TO  MCP-PATHNAME
               END-IF
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             =   ZERO
                 IF   (HKN-RECEJGNDSPKBN       =  "1"           )
                 OR   (HKN-HKNNUM              =  "972" OR "974"
                                                        OR "012")
                 OR   (WRK-RECEJGNDSPKBN       =  "1"           )
                 OR   (WRK-RECEJGNDSPKBN2  NOT =  SPACE         )
                     PERFORM 200411-SYUKEI-HENSYU1-SEC
                 ELSE
                     PERFORM 200412-SYUKEI-HENSYU2-SEC
                 END-IF
               END-IF
           END-IF
      *    ¼ýÇ¼¥¯¥í¡¼¥º
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key33"          TO  MCP-PATHNAME
           ELSE
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key25"          TO  MCP-PATHNAME
           END-IF
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       20041-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉéÃ´¼ÔÈÖ¹æÅù¥Á¥§¥Ã¥¯
      *****************************************************************
       20042-FTNJANUM-CHK-SEC            SECTION.
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM  TO  PTKOH-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID     TO  PTKOH-PTID
           MOVE    LNK-FTNJGNCHK-KOHID    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key8"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC   =    ZERO
             MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
             MOVE    "key8"           TO  MCP-PATHNAME
             PERFORM  910-PTKOHINF-READ-SEC
             IF       FLG-PTKOHINF    =   ZERO
               IF  (PTKOH-TEKSTYMD(1:6) <= LNK-FTNJGNCHK-SRYYM)
               AND (LNK-FTNJGNCHK-SRYYM <= PTKOH-TEKEDYMD(1:6))
      *
                  IF  (WRK-TEISYOTOKU      NOT  =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "200708")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "001"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE   4001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   3001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU      NOT  =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "200904")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "005"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE   3001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   2001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU      NOT  =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "200912")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "002"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE   4001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   2001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU           =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "200912")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "002"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE  30001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   9001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU      NOT  =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "201302")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "003"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE   5001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   3001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU           =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "201302")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201409")
                  AND (PTKOH-FTNJANUM(5:3)      =  "003"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE  10001  TO   WRK-HKNMST-JGNGAK2
                    ELSE
                       MOVE   6001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
                  IF  (WRK-TEISYOTOKU           =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "201410")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201509")
                  AND (PTKOH-FTNJANUM(5:3)      =  "003"   )
                    IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       MOVE  10001  TO   WRK-HKNMST-JGNGAK2
                    END-IF
                  END-IF
      *
                  IF  (WRK-TEISYOTOKU           =   SPACE  )
                  AND (LNK-FTNJGNCHK-SRYYM     >=  "200804")
                  AND (LNK-FTNJGNCHK-SRYYM     <=  "201301")
                  AND (PTKOH-FTNJANUM(5:3)      =  "003"   )
                  AND (LNK-FTNJGNCHK-NYUGAIKBN  =  "2"     )
                    MOVE    01      TO   LNK-FTNJGNCHK-RC
                    MOVE     1      TO   FLG-CHECK
                  END-IF
      *
               END-IF
             END-IF
           END-IF
           MOVE     "tbl_ptkohinf"    TO  MCP-TABLE
           MOVE     "key8"            TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       20042-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý£±
      *****************************************************************
       200411-SYUKEI-HENSYU1-SEC            SECTION.
      *
      *    ´µ¼ÔÉéÃ´¶â¤Î½¸·×
           PERFORM   UNTIL    FLG-SYUNOU  =   1
             MOVE    ZERO          TO      FLG-ARI
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL   (IDX1                  >  50 )
                          OR    (WRK-HKNCOMBINUM(IDX1) = ZERO)
                          OR    (FLG-ARI               =   1 )
               IF   SYU-HKNCOMBINUM  =  WRK-HKNCOMBINUM(IDX1)
                  MOVE    1    TO    FLG-ARI
                  EVALUATE   LNK-FTNJGNCHK-KOHNUM
                    WHEN   SYU-KOH-HKNNUM(1)
                       IF  (LNK-FTNJGNCHK-KOHNUM    = "972" OR "974")
                       AND (WRK-KYURATE        NOT  =  999          )
                         COMPUTE  SYU-KOH-COMPFTN(1) =
                                  SYU-TOTAL-HKNTEN * 10 * WRK-KYURATE
                                                        / 100
                         MOVE     SYU-KOH-COMPFTN(1)  TO
                                           SYU-KOH-COMPFTN-ENTANI(1)
                         COMPUTE  SYU-KOH-COMPFTN(1)  =
                                  SYU-KOH-COMPFTN(1)  + 5
                         COMPUTE  SYU-KOH-COMPFTN(1)  =
                                  SYU-KOH-COMPFTN(1)  / 10
                         COMPUTE  SYU-KOH-COMPFTN(1)  =
                                  SYU-KOH-COMPFTN(1)  * 10
                       END-IF
                       ADD  SYU-KOH-HKNFTNMONEY(1)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                       ADD  SYU-JYO-HKNFTNMONEY
                                       TO  WRK-KOHFTN-JYO-HKNFTNMONEY
                       ADD  SYU-KOH-HKNFTNMONEY(1) TO  WRK-KOHFTN
                       ADD  SYU-KOH-COMPFTN-ENTANI(1) TO WRK-KOHFTN-EN
                       IF  (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8")
                       AND (WRK-HKNCOMBINUM-HKNID(IDX1)
                                                  = LNK-FTNJGNCHK-HKNID)
                         ADD SYU-KOH-COMPFTN-ENTANI(1) TO WRK-KOHFTN-H
                         ADD SYU-KOH-HKNFTNMONEY(1)    TO WRK-KOHFTN-X
                         ADD SYU-SYUCOMPFTN-ENTANI     TO WRK-KOHFTN9-H
                         ADD SYU-SYUCOMPFTN            TO WRK-KOHFTN9-X
                         IF  (LNK-FTNJGNCHK-NYUGAIKBN  =  "1"      )
                         AND (SYU-SYUSKJ-RYOYOHI   NOT = ZERO      )
                         AND (SYU-SYUSKJ-FTN  NOT = SYU-KOH1SKJ-FTN)
                             MOVE    1    TO   FLG-SKJ-CHECK
                         END-IF
                       END-IF
                       IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND  SYU-SKJ-FTNKBN(1)        =  "2"
                          ADD  SYU-SKJ-FTN(1)      TO  WRK-KOHFTN-SKJ
                       END-IF
                       IF  (LNK-FTNJGNCHK-KOHNUM = "972" OR "974")
                       AND (SYU-SKJ-FTNDAY(1)  NOT = ZERO        )
                          MOVE SYU-KOH-COMPFTN-ENTANI(1) TO WRK-COMPFTN
                          PERFORM  2004111-CYOKI-HENSYU-SEC
                       END-IF
                    WHEN   SYU-KOH-HKNNUM(2)
                       IF  (LNK-FTNJGNCHK-KOHNUM    = "972" OR "974")
                       AND (WRK-KYURATE        NOT  =  999          )
                         COMPUTE  SYU-KOH-COMPFTN(2) =
                                  SYU-TOTAL-HKNTEN * 10 * WRK-KYURATE
                                                        / 100
                         MOVE     SYU-KOH-COMPFTN(2)  TO
                                           SYU-KOH-COMPFTN-ENTANI(2)
                         COMPUTE  SYU-KOH-COMPFTN(2)  =
                                  SYU-KOH-COMPFTN(2)  + 5
                         COMPUTE  SYU-KOH-COMPFTN(2)  =
                                  SYU-KOH-COMPFTN(2)  / 10
                         COMPUTE  SYU-KOH-COMPFTN(2)  =
                                  SYU-KOH-COMPFTN(2)  * 10
                       END-IF
                       ADD  SYU-KOH-HKNFTNMONEY(2)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                       ADD  SYU-JYO-HKNFTNMONEY
                                       TO  WRK-KOHFTN-JYO-HKNFTNMONEY
                       ADD  SYU-KOH-HKNFTNMONEY(2) TO  WRK-KOHFTN
                       ADD  SYU-KOH-COMPFTN-ENTANI(2) TO WRK-KOHFTN-EN
                       IF  (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8")
                       AND (WRK-HKNCOMBINUM-HKNID(IDX1)
                                                  = LNK-FTNJGNCHK-HKNID)
                         ADD SYU-KOH-COMPFTN-ENTANI(2) TO WRK-KOHFTN-H
                         ADD SYU-KOH-HKNFTNMONEY(2)    TO WRK-KOHFTN-X
                         ADD SYU-KOH-COMPFTN-ENTANI(1) TO WRK-KOHFTN9-H
                         ADD SYU-KOH-HKNFTNMONEY(1)    TO WRK-KOHFTN9-X
                         IF  (LNK-FTNJGNCHK-NYUGAIKBN  =  "1"      )
                         AND (SYU-SYUSKJ-RYOYOHI   NOT = ZERO      )
                         AND (SYU-SYUSKJ-FTN  NOT = SYU-KOH2SKJ-FTN)
                             MOVE    1    TO   FLG-SKJ-CHECK
                         END-IF
                       END-IF
                       IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND  SYU-SKJ-FTNKBN(2)        =  "2"
                          ADD  SYU-SKJ-FTN(2)      TO  WRK-KOHFTN-SKJ
                       END-IF
                       IF  (LNK-FTNJGNCHK-KOHNUM = "972" OR "974")
                       AND (SYU-SKJ-FTNDAY(1)  NOT = ZERO        )
                          MOVE SYU-KOH-COMPFTN-ENTANI(2) TO WRK-COMPFTN
                          PERFORM  2004111-CYOKI-HENSYU-SEC
                       END-IF
                    WHEN   SYU-KOH-HKNNUM(3)
                       IF  (LNK-FTNJGNCHK-KOHNUM    = "972" OR "974")
                       AND (WRK-KYURATE        NOT  =  999          )
                         COMPUTE  SYU-KOH-COMPFTN(3) =
                                  SYU-TOTAL-HKNTEN * 10 * WRK-KYURATE
                                                        / 100
                         MOVE     SYU-KOH-COMPFTN(3)  TO
                                           SYU-KOH-COMPFTN-ENTANI(3)
                         COMPUTE  SYU-KOH-COMPFTN(3)  =
                                  SYU-KOH-COMPFTN(3)  + 5
                         COMPUTE  SYU-KOH-COMPFTN(3)  =
                                  SYU-KOH-COMPFTN(3)  / 10
                         COMPUTE  SYU-KOH-COMPFTN(3)  =
                                  SYU-KOH-COMPFTN(3)  * 10
                       END-IF
                       ADD  SYU-KOH-HKNFTNMONEY(3)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                       ADD  SYU-JYO-HKNFTNMONEY
                                       TO  WRK-KOHFTN-JYO-HKNFTNMONEY
                       ADD  SYU-KOH-HKNFTNMONEY(3) TO  WRK-KOHFTN
                       ADD  SYU-KOH-COMPFTN-ENTANI(3) TO WRK-KOHFTN-EN
                       IF  (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8")
                       AND (WRK-HKNCOMBINUM-HKNID(IDX1)
                                                  = LNK-FTNJGNCHK-HKNID)
                         ADD SYU-KOH-COMPFTN-ENTANI(3) TO WRK-KOHFTN-H
                         ADD SYU-KOH-HKNFTNMONEY(3)    TO WRK-KOHFTN-X
                         ADD SYU-KOH-COMPFTN-ENTANI(2) TO WRK-KOHFTN9-H
                         ADD SYU-KOH-HKNFTNMONEY(2)    TO WRK-KOHFTN9-X
                         IF  (LNK-FTNJGNCHK-NYUGAIKBN  =  "1"      )
                         AND (SYU-SYUSKJ-RYOYOHI   NOT = ZERO      )
                         AND (SYU-SYUSKJ-FTN  NOT = SYU-KOH3SKJ-FTN)
                             MOVE    1    TO   FLG-SKJ-CHECK
                         END-IF
                       END-IF
                       IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND  SYU-SKJ-FTNKBN(3)        =  "2"
                          ADD  SYU-SKJ-FTN(3)      TO  WRK-KOHFTN-SKJ
                       END-IF
                       IF  (LNK-FTNJGNCHK-KOHNUM = "972" OR "974")
                       AND (SYU-SKJ-FTNDAY(1)  NOT = ZERO        )
                          MOVE SYU-KOH-COMPFTN-ENTANI(3) TO WRK-COMPFTN
                          PERFORM  2004111-CYOKI-HENSYU-SEC
                       END-IF
                    WHEN   SYU-KOH-HKNNUM(4)
                       IF  (LNK-FTNJGNCHK-KOHNUM    = "972" OR "974")
                       AND (WRK-KYURATE        NOT  =  999          )
                         COMPUTE  SYU-KOH-COMPFTN(4) =
                                  SYU-TOTAL-HKNTEN * 10 * WRK-KYURATE
                                                        / 100
                         MOVE     SYU-KOH-COMPFTN(4)  TO
                                           SYU-KOH-COMPFTN-ENTANI(4)
                         COMPUTE  SYU-KOH-COMPFTN(4)  =
                                  SYU-KOH-COMPFTN(4)  + 5
                         COMPUTE  SYU-KOH-COMPFTN(4)  =
                                  SYU-KOH-COMPFTN(4)  / 10
                         COMPUTE  SYU-KOH-COMPFTN(4)  =
                                  SYU-KOH-COMPFTN(4)  * 10
                       END-IF
                       ADD  SYU-KOH-HKNFTNMONEY(4)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                       ADD  SYU-JYO-HKNFTNMONEY
                                       TO  WRK-KOHFTN-JYO-HKNFTNMONEY
                       ADD  SYU-KOH-HKNFTNMONEY(4) TO  WRK-KOHFTN
                       ADD  SYU-KOH-COMPFTN-ENTANI(4) TO WRK-KOHFTN-EN
                       IF  (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8")
                       AND (WRK-HKNCOMBINUM-HKNID(IDX1)
                                                  = LNK-FTNJGNCHK-HKNID)
                         ADD SYU-KOH-COMPFTN-ENTANI(4) TO WRK-KOHFTN-H
                         ADD SYU-KOH-HKNFTNMONEY(4)    TO WRK-KOHFTN-X
                         ADD SYU-KOH-COMPFTN-ENTANI(3) TO WRK-KOHFTN9-H
                         ADD SYU-KOH-HKNFTNMONEY(3)    TO WRK-KOHFTN9-X
                         IF  (LNK-FTNJGNCHK-NYUGAIKBN  =  "1"      )
                         AND (SYU-SYUSKJ-RYOYOHI   NOT = ZERO      )
                         AND (SYU-SYUSKJ-FTN  NOT = SYU-KOH4SKJ-FTN)
                             MOVE    1    TO   FLG-SKJ-CHECK
                         END-IF
                       END-IF
                       IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND  SYU-SKJ-FTNKBN(4)        =  "2"
                          ADD  SYU-SKJ-FTN(4)      TO  WRK-KOHFTN-SKJ
                       END-IF
                       IF  (LNK-FTNJGNCHK-KOHNUM = "972" OR "974")
                       AND (SYU-SKJ-FTNDAY(1)  NOT = ZERO        )
                          MOVE SYU-KOH-COMPFTN-ENTANI(4) TO WRK-COMPFTN
                          PERFORM  2004111-CYOKI-HENSYU-SEC
                       END-IF
                  END-EVALUATE
                  ADD       SYU-TOTAL-HKNTEN       TO  WRK-SEIKYU-TOTAL
      *           IF  WRK-HKNCOMBINUM(IDX1) = LNK-FTNJGNCHK-HKNCOMBINUM
                  IF  WRK-HKNCOMBINUM-HKNID(IDX1)
                                            = LNK-FTNJGNCHK-HKNID
                     MOVE       1    TO    FLG-ARI2
                     ADD    SYU-TOTAL-HKNTEN       TO  WRK-SEIKYU
                     IF  SYU-SYUSKJ-RYOYOHI   NOT = ZERO
                     OR  SYU-KOH1SKJ-RYOYOHI  NOT = ZERO
                        MOVE    1    TO    FLG-SKJRYOYOHI-ARI
                     END-IF
                     PERFORM     2004113-SAIGAI-HENSYU2-SEC
                     IF  SYU-TOTAL-HKNTEN = ZERO
                        MOVE    1    TO    WRK-HKNCOMBINUM-ZEROFLG(IDX1)
                     END-IF
                  ELSE
                     PERFORM     2004112-SAIGAI-HENSYU-SEC
                     IF  FLG-SAIGAI-TAISYO  =  1
                        PERFORM  2004113-SAIGAI-HENSYU2-SEC
                     END-IF
                  END-IF
                  IF  WRK-HKNCOMBINUM-KYUFUGAI(IDX1) =  1
                    ADD  SYU-KYUFUGAI-GOKEI-TEN    TO  WRK-KYUFUGAITEN
                  END-IF
      ***         IF    (SYU-YKZKENNUM  NOT = ZERO )
      ***         AND   (SYU-SYUHKNNUM  NOT = SPACE)
      ***              MOVE    1    TO   FLG-GENMEN
      ***         END-IF
                  IF  (LNK-FTNJGNCHK-NYUGAIKBN  = "1"     )
                  AND (LNK-FTNJGNCHK-KOHNUM     = "012"   )
                  AND (LNK-FTNJGNCHK-SRYYM     >= "200804")
                    IF  (SYU-SYUHKNNUM     =   SPACE        )
                    AND (SYU-KOH-HKNNUM(1) =  "079" OR "024")
                    AND (SYU-KOH-HKNNUM(2) =  "012"         )
                    AND (SYU-KOH-HKNNUM(3) =   SPACE        )
                    AND (SYU-KOH-HKNNUM(4) =   SPACE        )
                    AND (SYU-HKNCOMBINUM
                               =   LNK-FTNJGNCHK-HKNCOMBINUM)
                        ADD  SYU-SKJ-FTN-RECE(1) TO  WRK-SKJ-FTN
                    ELSE
                        MOVE    1                TO  FLG-SEIHO-CHECK
                    END-IF
                  END-IF
                  IF  (LNK-FTNJGNCHK-PREFNUM    =  42    )
                  AND (WRK-HKNMST-JGNGAK        =  999998)
                    IF  (SYU-SYUHKNNUM     NOT =   SPACE              )
                    AND (SYU-KOH-HKNNUM(1) = "015" OR "016" OR "021"  )
                    AND (SYU-KOH-HKNNUM(2) = "188"                    )
                    AND (SYU-HKNCOMBINUM   = LNK-FTNJGNCHK-HKNCOMBINUM)
                        MOVE    1                TO  FLG-HKNCOMBI-CHECK
                    END-IF
                  END-IF
                  IF     WRK-HKNCOMBINUM-FLG(IDX1) =  1
                        MOVE   01                TO  LNK-FTNJGNCHK-RC
                  END-IF
               END-IF
             END-PERFORM
             IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                MOVE    "tbl_syunou"     TO  MCP-TABLE
                MOVE    "key33"          TO  MCP-PATHNAME
             ELSE
                MOVE    "tbl_syunou"     TO  MCP-TABLE
                MOVE    "key25"          TO  MCP-PATHNAME
             END-IF
             PERFORM  910-SYUNOU-READ-SEC
           END-PERFORM
      *
           IF  (LNK-FTNJGNCHK-PREFNUM    =  42    )
           AND (WRK-HKNMST-JGNGAK        =  999998)
             IF   FLG-HKNCOMBI-CHECK  =  ZERO
                MOVE   WRK-TAIHI-JGNGAK  TO  WRK-HKNMST-JGNGAK
             END-IF
           END-IF
      *
           IF  ((LNK-FTNJGNCHK-PREFNUM    =  27           ) AND
                (LNK-FTNJGNCHK-KOHNUM     = "141" OR "241") AND
                (LNK-FTNJGNCHK-SRYYM     >= "200804"      ))
              MOVE   9999999999     TO  WRK-KOHFTN
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
              IF    WRK-HKNMST-JGNGAK  =  999999
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
              ELSE
                IF  WRK-HKNMST-JGNGAK  =  999998
                   CONTINUE
                ELSE
                   IF    WRK-RECEJGNDSPKBN2  NOT =  SPACE
                     PERFORM  2004114X-FTNJGNCHK-HENSYU2-SEC
                   ELSE
                     PERFORM  2004114-FTNJGNCHK-HENSYU-SEC
                   END-IF
                END-IF
              END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
           AND   FLG-ARI2          =    1
             IF  (LNK-FTNJGNCHK-SRYYM   >= "201101")
             AND (FLG-SAIGAI-ARI2        =    1    )
             AND (WRK-SYUSKJ-FTN = WRK-SKYMONEY-SKJ)
                       MOVE    1        TO  FLG-SAIGAI-ARI
             END-IF
             IF  (LNK-FTNJGNCHK-SRYYM   >= "201101")
             AND (FLG-SAIGAI-ARI         =    1    )
             AND (FLG-GENMEN-NASI        =  ZERO   )
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
           AND   FLG-ARI2          =    1
             IF   (LNK-FTNJGNCHK-NYUGAIKBN  =  "2" )
             AND  (WRK-SEIKYU               =  ZERO)
                MOVE     ZERO    TO   FLG-SRYACCT-CHECK4
                PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL  (IDX2               >  50)
                           OR    (FLG-SRYACCT-CHECK4 =  1 )
                  IF  WRK-HKNCOMBINUM-HKNID(IDX2)
                                      = LNK-FTNJGNCHK-HKNID
                    PERFORM  2004119-SRYACCT-CHECK4-SEC
                  END-IF
                END-PERFORM
                IF    FLG-SRYACCT-CHECK4  =  1
                  MOVE   99999   TO   WRK-SEIKYU
                END-IF
             END-IF
      *
             IF  WRK-SEIKYU         =  ZERO
                  MOVE        "1"       TO  LNK-FTNJGNCHK-SKYZEROFLG
             END-IF
      *
             IF  WRK-SEIKYU         =  ZERO
             AND FLG-SKJRYOYOHI-ARI =  ZERO
                MOVE     ZERO    TO   FLG-SRYACCT-CHECK
                PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL  (IDX2               >  50)
                           OR    (FLG-SRYACCT-CHECK  =  1 )
                  IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                    PERFORM  2004115-SRYACCT-CHECK-SEC
                  END-IF
                END-PERFORM
                IF    FLG-SRYACCT-CHECK  =  1
                  IF  LNK-FTNJGNCHK-KOHNUM(1:1) = "0"
      *              ¥·¥¹¥Æ¥à´ÉÍý£²£°£°£µÊÔ½¸
                     PERFORM  2004116-SYS2005-CHECK-SEC
                     IF  WRK-ZERO-KOH-PRTKBN = "1"
                       CONTINUE
                     ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                     END-IF
                  ELSE
                     IF  LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8"
                       MOVE     ZERO    TO   FLG-SRYACT-CHECK
                       PERFORM  VARYING  IDX2  FROM  1  BY  1
                                 UNTIL  (IDX2               >  50)
                                  OR    (FLG-SRYACT-CHECK   =  1 )
                         IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                           PERFORM  2004117-SRYACCT-CHECK2-SEC
                         END-IF
                       END-PERFORM
                       IF   FLG-SRYACT-CHECK = 1
                         CONTINUE
                       ELSE
                         MOVE 01        TO  LNK-FTNJGNCHK-RC
                         MOVE  1        TO  FLG-TENKEN
                       END-IF
                     ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                     END-IF
                  END-IF
                ELSE
                  MOVE     ZERO    TO   FLG-SRYACCT-CHECK3
                  PERFORM  VARYING  IDX2  FROM  1  BY  1
                            UNTIL  (IDX2               >  50)
                             OR    (FLG-SRYACCT-CHECK3 =  1 )
                    IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                      PERFORM  2004118-SRYACCT-CHECK3-SEC
                    END-IF
                  END-PERFORM
                  IF  FLG-SRYACCT-CHECK3 =  1
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                  ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                  END-IF
                END-IF
             END-IF
           END-IF
      *
           COMPUTE LNK-FTNJGNCHK-KOHFTN = WRK-KOHFTN-HKNFTNMONEY
           COMPUTE LNK-FTNJGNCHK-KOHFTNSKJ
                                        = WRK-KOHFTN-HKNFTNMONEY
                                        + WRK-KOHFTN-SKJ
      *
           .
       200411-SYUKEI-HENSYU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Þ¥ëÄ¹¤ÎÊÔ½¸
      *****************************************************************
       2004111-CYOKI-HENSYU-SEC            SECTION.
      *
           PERFORM  20041111-HKNINF-HENSYU-SEC
      *
           IF  (WRK-CONTKBN   NOT = SPACE)
           OR  (WRK-CONTKBN2  NOT = SPACE)
              IF  (WRK-HKNJANUM2  =  WRK-HKNJANUM )
              AND (WRK-HONKZKKBN2 =  WRK-HONKZKKBN)
                MOVE  WRK-KIGO       TO  WRK-KIGO2
                MOVE  WRK-NUM        TO  WRK-NUM2
              END-IF
           END-IF
      *
           IF    (WRK-HKNNUM        =  "039")
           AND   (WRK-HKNNUM2       =  "039")
             IF    LNK-FTNJGNCHK-NYUGAIKBN   =   "1"
               MOVE     WRK-HKNJANUM-TBL  TO  WRK-HKNJANUM2-TBL
             ELSE
               IF   LNK-FTNJGNCHK-SRYYM >= "201204"
                 IF   WRK-HKNJANUM2(1:4)  =  WRK-HKNJANUM(1:4)
                   MOVE WRK-HKNJANUM-TBL  TO  WRK-HKNJANUM2-TBL
                 END-IF
               ELSE
                 MOVE   WRK-HKNJANUM-TBL  TO  WRK-HKNJANUM2-TBL
               END-IF
             END-IF
           ELSE
             IF  (SYU-SKJ-FTNDAY(1) =  1  OR  2)
             OR  (WRK-HKNNUM        =  "060"   )
                IF   WRK-HKNJANUM2  =  WRK-HKNJANUM
                  MOVE  WRK-HONKZKKBN  TO  WRK-HONKZKKBN2
                  MOVE  WRK-KIGO       TO  WRK-KIGO2
                  MOVE  WRK-NUM        TO  WRK-NUM2
                END-IF
             END-IF
           END-IF
      *
           EVALUATE  SYU-SKJ-FTNDAY(1)
             WHEN       1
             WHEN       2
                 IF  WRK-HKNNUM     =  "039"
                    IF  WRK-HKNNUM2   =  "039"
                      IF    WRK-HKNJANUM-TBL  =  WRK-HKNJANUM2-TBL
                         MOVE   1             TO  FLG-TOKU75
                         ADD  WRK-COMPFTN     TO  WRK-KOHFTN-TOKU75
                      END-IF
                    END-IF
                 ELSE
                    IF  SYU-SKJ-FTNDAY(1)  =   1
                      IF    WRK-HKNJANUM-TBL  =  WRK-HKNJANUM2-TBL
                         MOVE   1             TO  FLG-TOKU75
                         ADD  WRK-COMPFTN     TO  WRK-KOHFTN-TOKU75
                      END-IF
                    ELSE
                      IF    WRK-HKNJANUM-TBL  =  WRK-HKNJANUM2-TBL
                        MOVE  LNK-FTNJGNCHK-SRYYM TO  WRK-SRYYMD(1:6)
                        MOVE  SYU-SKJ-FTNDAY(2)   TO  WRK-SRYYMD(7:2)
                        IF ((WRK-SRYYMD <= WRK-TEKSTYMD ) AND
                            (WRK-SRYYMD <= WRK-TEKSTYMD2))
                        OR ((WRK-SRYYMD >  WRK-TEKSTYMD ) AND
                            (WRK-SRYYMD >  WRK-TEKSTYMD2))
                          MOVE   1            TO  FLG-TOKU75
                          ADD  WRK-COMPFTN    TO  WRK-KOHFTN-TOKU75
                        END-IF
                      END-IF
                    END-IF
                 END-IF
             WHEN       3
                 IF    WRK-HKNJANUM-TBL  =  WRK-HKNJANUM2-TBL
                   MOVE  LNK-FTNJGNCHK-SRYYM TO  WRK-SRYYMD(1:6)
                   MOVE  SYU-SKJ-FTNDAY(2)   TO  WRK-SRYYMD(7:2)
                   IF ((WRK-SRYYMD <= WRK-TEKSTYMD ) AND
                       (WRK-SRYYMD <= WRK-TEKSTYMD2))
                   OR ((WRK-SRYYMD >  WRK-TEKSTYMD ) AND
                       (WRK-SRYYMD >  WRK-TEKSTYMD2))
                     MOVE   1            TO  FLG-TOKU75
                     ADD  WRK-COMPFTN    TO  WRK-KOHFTN-TOKU75
                   END-IF
                 END-IF
           END-EVALUATE
      *
           .
       2004111-CYOKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊóÊÔ½¸
      *****************************************************************
       20041111-HKNINF-HENSYU-SEC            SECTION.
      *
           MOVE     LNK-FTNJGNCHK-HKNID  TO  WRK-HKNID
           PERFORM  200411111-HKNINF-KENSAKU-SEC
           MOVE     SPACE                TO  WRK-HKNJANUM-TBL
           MOVE     PTHKN-HKNNUM         TO  WRK-HKNNUM
           MOVE     PTHKN-HKNJANUM       TO  WRK-HKNJANUM
           MOVE     PTHKN-HONKZKKBN      TO  WRK-HONKZKKBN
           MOVE     PTHKN-CONTKBN        TO  WRK-CONTKBN
           MOVE     PTHKN-TEKSTYMD       TO  WRK-TEKSTYMD
      *
           MOVE     WRK-HKNCOMBINUM-HKNID(IDX1)
                                         TO  WRK-HKNID
           PERFORM  200411111-HKNINF-KENSAKU-SEC
           MOVE     SPACE                TO  WRK-HKNJANUM2-TBL
           MOVE     PTHKN-HKNNUM         TO  WRK-HKNNUM2
           MOVE     PTHKN-HKNJANUM       TO  WRK-HKNJANUM2
           MOVE     PTHKN-HONKZKKBN      TO  WRK-HONKZKKBN2
           MOVE     PTHKN-CONTKBN        TO  WRK-CONTKBN2
           MOVE     PTHKN-TEKSTYMD       TO  WRK-TEKSTYMD2
      *
           .
       20041111-HKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊó¸¡º÷
      *****************************************************************
       200411111-HKNINF-KENSAKU-SEC            SECTION.
      *
           IF     WRK-HKNID  NOT = ZERO
              INITIALIZE                        PTHKNINF-REC
              MOVE   LNK-FTNJGNCHK-HOSPNUM  TO  PTHKN-HOSPNUM
              MOVE   LNK-FTNJGNCHK-PTID     TO  PTHKN-PTID
              MOVE   WRK-HKNID              TO  PTHKN-HKNID
              MOVE   PTHKNINF-REC           TO  MCPDATA-REC
              MOVE   "DBSELECT"             TO  MCP-FUNC
              MOVE   "tbl_pthkninf"         TO  MCP-TABLE
              MOVE   "key"                  TO  MCP-PATHNAME
              CALL   "ORCDBMAIN"            USING MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
              IF      MCP-RC   =   ZERO
                 MOVE    "tbl_pthkninf"     TO  MCP-TABLE
                 MOVE    "key"              TO  MCP-PATHNAME
                 MOVE    "DBFETCH"          TO  MCP-FUNC
                 CALL    "ORCDBMAIN"        USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
                 IF      MCP-RC             =   ZERO
                     MOVE    MCPDATA-REC    TO  PTHKNINF-REC
                 ELSE
                     INITIALIZE                 PTHKNINF-REC
                 END-IF
              ELSE
                 INITIALIZE                     PTHKNINF-REC
              END-IF
              MOVE      "tbl_pthkninf"      TO  MCP-TABLE
              MOVE      "key"               TO  MCP-PATHNAME
              PERFORM    910-DB-CLOSE-SEC
           ELSE
              INITIALIZE                        PTHKNINF-REC
           END-IF
      *
           .
       200411111-HKNINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÒ³²ÍÑ¤ÎÊÔ½¸
      *****************************************************************
       2004112-SAIGAI-HENSYU-SEC            SECTION.
      *
           MOVE       ZERO     TO     FLG-SAIGAI-TAISYO
      *
           PERFORM  20041111-HKNINF-HENSYU-SEC
      *
           IF   (WRK-HKNJANUM      =  WRK-HKNJANUM2)
           AND  (WRK-HKNJANUM  NOT =  SPACE        )
      *
              MOVE    ZERO                 TO  FLG-KOREI
              INITIALIZE                       ORCSAGECHKAREA
              MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  AGECHK-HOSPNUM
              MOVE  LNK-FTNJGNCHK-PTID     TO  AGECHK-PTID
              MOVE  LNK-FTNJGNCHK-SRYYM    TO  AGECHK-SRYYMD(1:6)
              MOVE  "01"                   TO  AGECHK-SRYYMD(7:2)
              MOVE  PTINF-BIRTHDAY         TO  AGECHK-BIRTHDAY
              CALL  "ORCSAGECHK"        USING  ORCSAGECHKAREA
                                               SPA-AREA
              IF    AGECHK-RC  =   ZERO
                 IF  AGECHK-NENREI   >=  70
                    MOVE     1         TO   FLG-KOREI
                 END-IF
              END-IF
      *
              IF  (WRK-HKNNUM        =  "039")
              OR  (WRK-HKNNUM        =  "060")
              OR  (FLG-KOREI         =    1  )
                 MOVE  WRK-HONKZKKBN   TO   WRK-HONKZKKBN2
              END-IF
      *
              IF   WRK-HKNJANUM-TBL  =   WRK-HKNJANUM2-TBL
                 MOVE      1           TO   FLG-SAIGAI-TAISYO
              END-IF
      *
           END-IF
      *
           .
       2004112-SAIGAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÒ³²ÍÑ¤ÎÊÔ½¸£²
      *****************************************************************
       2004113-SAIGAI-HENSYU2-SEC            SECTION.
      *
           IF    SYU-YKZKENNUM  NOT = ZERO
              MOVE    1     TO   FLG-GENMEN-ARI
           ELSE
              MOVE    1     TO   FLG-GENMEN-NASI
           END-IF
      *
           IF  LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
             IF  (SYU-YKZKENNUM  =  7 OR 8  )
             AND (SYU-SYUHKNNUM  NOT = SPACE)
               MOVE   1    TO    FLG-SAIGAI-ARI
             END-IF
             IF  (SYU-YKZKENNUM  =  3 OR 4  )
             AND (SYU-SYUHKNNUM  NOT = SPACE)
               ADD   SYU-SYUSKJ-FTN    TO  WRK-SYUSKJ-FTN
               ADD   SYU-SKYMONEY-SKJ  TO  WRK-SKYMONEY-SKJ
               ADD   SYU-SKYMONEY-LIFE TO  WRK-SKYMONEY-SKJ
               MOVE   1    TO    FLG-SAIGAI-ARI2
             END-IF
           ELSE
             IF  (SYU-YKZKENNUM  =  3 OR 4  )
             AND (SYU-SYUHKNNUM  NOT = SPACE)
               MOVE   1    TO    FLG-SAIGAI-ARI
             END-IF
           END-IF
      *
           .
       2004113-SAIGAI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉéÃ´¾å¸Â¥Á¥§¥Ã¥¯ÊÔ½¸
      *****************************************************************
       2004114-FTNJGNCHK-HENSYU-SEC            SECTION.
      *
           MOVE      ZERO     TO     FLG-CHECK-METHOD
           IF  ((LNK-FTNJGNCHK-PREFNUM      =  42           ) AND
                (LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8") AND
                (LNK-FTNJGNCHK-SRYYM       >= "200704"      ))
             MOVE     1       TO     FLG-CHECK-METHOD
           END-IF
      *
           IF ((LNK-FTNJGNCHK-NYUGAIKBN = "2"       ) AND
               (WRK-IRYOKEIKBN-G        = "1" OR "2"))
           OR ((LNK-FTNJGNCHK-NYUGAIKBN = "1"       ) AND
               (WRK-IRYOKEIKBN-N        = "1" OR "2"))
               IF   WRK-KOHFTN-HKNFTNMONEY   NOT =   ZERO
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
               END-IF
               IF    LNK-FTNJGNCHK-RC       =    1
               AND   FLG-SKJ-CHECK          =    1
                       MOVE   ZERO      TO  LNK-FTNJGNCHK-RC
               END-IF
           ELSE
               EVALUATE   LNK-FTNJGNCHK-KOHNUM
                 WHEN       "012"
                   IF  (WRK-SKJ-FTN     >=  14880)
                   AND (FLG-SEIHO-CHECK  =  ZERO )
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                   END-IF
                 WHEN       "972"
                 WHEN       "974"
                   IF   FLG-GENMEN   =   1
                     CONTINUE
                   ELSE
                    IF   LNK-FTNJGNCHK-SRYYM    >=   "201204"
                     IF   FLG-TOKU75  =  1
                       IF   WRK-HKNMST-JGNGAK   >=   WRK-KOHFTN-EN
                         EVALUATE  LNK-FTNJGNCHK-KOHNUM
                           WHEN      "972"
                              IF  WRK-KOHFTN-TOKU75  <= 5000
                                 MOVE   01   TO  LNK-FTNJGNCHK-RC
                              END-IF
                           WHEN      "974"
                              IF  WRK-KOHFTN-TOKU75  <= 10000
                                 MOVE   01   TO  LNK-FTNJGNCHK-RC
                              END-IF
                         END-EVALUATE
                       END-IF
                     ELSE
                       IF   WRK-HKNMST-JGNGAK   >=  WRK-KOHFTN-EN
                         MOVE   01        TO  LNK-FTNJGNCHK-RC
                       END-IF
                     END-IF
                    ELSE
                     IF   FLG-TOKU75  =  1
                       IF   WRK-HKNMST-JGNGAK   >   WRK-KOHFTN-EN
                         EVALUATE  LNK-FTNJGNCHK-KOHNUM
                           WHEN      "972"
                              IF  WRK-KOHFTN-TOKU75  <  5000
                                 MOVE   01   TO  LNK-FTNJGNCHK-RC
                              END-IF
                           WHEN      "974"
                              IF  WRK-KOHFTN-TOKU75  <  10000
                                 MOVE   01   TO  LNK-FTNJGNCHK-RC
                              END-IF
                         END-EVALUATE
                       END-IF
                     ELSE
                       IF   WRK-HKNMST-JGNGAK   >   WRK-KOHFTN-EN
                         MOVE   01        TO  LNK-FTNJGNCHK-RC
                       END-IF
                     END-IF
                    END-IF
                   END-IF
                 WHEN       OTHER
                   IF   LNK-FTNJGNCHK-RC      = ZERO
                   AND  WRK-HKNMST-JGNGAK NOT = ZERO
                   AND  WRK-KOHFTN        NOT = 9999999999
                     IF   WRK-HKNMST-JGNGAK2  NOT =  ZERO
                        IF     LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                          IF   (WRK-KOHFTN9-H      = WRK-KOHFTN-H)
                          AND  (WRK-HKNMST-JGNGAK2 > WRK-KOHFTN-X)
                              MOVE   01   TO  LNK-FTNJGNCHK-RC
                          END-IF
                        ELSE
                          IF   (WRK-KOHFTN9-H      = WRK-KOHFTN-H)
                          AND  (WRK-HKNMST-JGNGAK2 > WRK-KOHFTN-H)
                              MOVE   01   TO  LNK-FTNJGNCHK-RC
                          END-IF
                        END-IF
                     ELSE
                        IF     FLG-CHECK-METHOD   =   1
                          IF   (WRK-KOHFTN9-H  =  WRK-KOHFTN-H)
                          OR   (WRK-KOHFTN9-X  =  WRK-KOHFTN-X)
                              MOVE   01   TO  LNK-FTNJGNCHK-RC
                          END-IF
                        ELSE
                          IF   LNK-FTNJGNCHK-SRYYM  >= "200804"
                            IF  WRK-KOHFTN9-H  =  WRK-KOHFTN-H
                              MOVE   01   TO  LNK-FTNJGNCHK-RC
                            END-IF
                          ELSE
                            IF  WRK-HKNMST-JGNGAK  >  WRK-KOHFTN
                              MOVE   01   TO  LNK-FTNJGNCHK-RC
                            END-IF
                          END-IF
                        END-IF
                     END-IF
      *
                     IF    LNK-FTNJGNCHK-RC       =    1
                     AND   FLG-SKJ-CHECK          =    1
                             MOVE   ZERO       TO  LNK-FTNJGNCHK-RC
                     END-IF
                   END-IF
      *
                   IF   LNK-FTNJGNCHK-RC      = ZERO
                   AND  WRK-HKNMST-JGNGAK     = ZERO
                   AND  WRK-KOHFTN        NOT = 9999999999
                   AND  LNK-FTNJGNCHK-SRYYM  >= "200804"
                     IF   WRK-KOHFTN9-H  =  WRK-KOHFTN-H
                             MOVE   01         TO  LNK-FTNJGNCHK-RC
                     END-IF
      *
                     IF    LNK-FTNJGNCHK-RC       =    1
                     AND   FLG-SKJ-CHECK          =    1
                             MOVE   ZERO       TO  LNK-FTNJGNCHK-RC
                     END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
             IF  (WRK-KYUFUGAITEN  NOT =  ZERO            )
             AND (WRK-KYUFUGAITEN      =  WRK-SEIKYU-TOTAL)
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           .
       2004114-FTNJGNCHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉéÃ´¾å¸Â¥Á¥§¥Ã¥¯ÊÔ½¸
      *****************************************************************
       2004114X-FTNJGNCHK-HENSYU2-SEC            SECTION.
      *
           IF   WRK-RECEJGNDSPKBN2  =  "1" OR "2"
                 IF   WRK-KOHFTN-HKNFTNMONEY  NOT =  ZERO
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
                 END-IF
           ELSE
             EVALUATE  WRK-RECEJGNDSPKBN2
               WHEN     "3"
                 IF   WRK-KOHFTN-JYO-HKNFTNMONEY  >=  21000
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
                 END-IF
               WHEN     "4"
                 IF   WRK-KOHFTN-JYO-HKNFTNMONEY  >=  20000
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
                 END-IF
             END-EVALUATE
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC       =    1
           AND   FLG-SKJ-CHECK          =    1
                   MOVE   ZERO      TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           .
       2004114X-FTNJGNCHK-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯
      *****************************************************************
       2004115-SRYACCT-CHECK-SEC            SECTION.
      *
           INITIALIZE                          SRYACCT-REC
           MOVE   LNK-FTNJGNCHK-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE   LNK-FTNJGNCHK-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE   LNK-FTNJGNCHK-PTID       TO  ACCT-PTID
           MOVE   LNK-FTNJGNCHK-SRYYM      TO  ACCT-SRYYM
           MOVE   WRK-HKNCOMBINUM(IDX2)    TO  ACCT-HKNCOMBI
           MOVE   SRYACCT-REC              TO  MCPDATA-REC
           MOVE   "DBSELECT"               TO  MCP-FUNC
           MOVE   "tbl_sryacct"            TO  MCP-TABLE
           MOVE   "key64"                  TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC   =   ZERO
             MOVE   "tbl_sryacct"          TO  MCP-TABLE
             MOVE   "key64"                TO  MCP-PATHNAME
             PERFORM 910-SRYACCT-READ-SEC
             IF    FLG-SRYACCT  =  ZERO
                MOVE    1                  TO  FLG-SRYACCT-CHECK
             END-IF
           END-IF
           MOVE     "tbl_sryacct"          TO  MCP-TABLE
           MOVE     "key64"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2004115-SRYACCT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý£²£°£°£µ¥Á¥§¥Ã¥¯
      *****************************************************************
       2004116-SYS2005-CHECK-SEC            SECTION.
      *
           MOVE      SPACE              TO  WRK-ZERO-KOH-PRTKBN
      *
           INITIALIZE                       SYS-200501-REC
           MOVE  "2005"                 TO  SYS-200501-KANRICD
           MOVE  "01"                   TO  SYS-200501-KBNCD
           MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  SYS-200501-HOSPNUM
           MOVE  LNK-FTNJGNCHK-SRYYM    TO  SYS-200501-STYUKYMD(1:6)
           MOVE  "01"                   TO  SYS-200501-STYUKYMD(7:2)
           MOVE  LNK-FTNJGNCHK-SRYYM    TO  SYS-200501-EDYUKYMD(1:6)
           MOVE  "01"                   TO  SYS-200501-EDYUKYMD(7:2)
           MOVE  SYS-200501-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"             TO  MCP-FUNC
           MOVE  "tbl_syskanri"         TO  MCP-TABLE
           MOVE  "key10"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF     MCP-RC   =   ZERO
              MOVE     "tbl_syskanri"   TO  MCP-TABLE
              MOVE     "key10"          TO  MCP-PATHNAME
              PERFORM   SYSKANRI-READ-SEC
              MOVE      SYS-200501-ZERO-KOH-PRTKBN
                                        TO  WRK-ZERO-KOH-PRTKBN
           END-IF
           MOVE        "tbl_syskanri"   TO  MCP-TABLE
           MOVE        "key10"          TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           IF      WRK-ZERO-KOH-PRTKBN  =   SPACE
               MOVE    "1"              TO  WRK-ZERO-KOH-PRTKBN
           END-IF
      *
           .
       2004116-SYS2005-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯£²
      *****************************************************************
       2004117-SRYACCT-CHECK2-SEC            SECTION.
      *
           INITIALIZE                          SRYACCT-REC
           MOVE   LNK-FTNJGNCHK-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE   LNK-FTNJGNCHK-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE   LNK-FTNJGNCHK-PTID       TO  ACCT-PTID
           MOVE   LNK-FTNJGNCHK-SRYYM      TO  ACCT-SRYYM
           MOVE   WRK-HKNCOMBINUM(IDX2)    TO  ACCT-HKNCOMBI
           MOVE   SRYACCT-REC              TO  MCPDATA-REC
           MOVE   "DBSELECT"               TO  MCP-FUNC
           MOVE   "tbl_sryacct"            TO  MCP-TABLE
           MOVE   "key67"                  TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC   =   ZERO
             MOVE   "tbl_sryacct"          TO  MCP-TABLE
             MOVE   "key67"                TO  MCP-PATHNAME
             PERFORM 910-SRYACCT-READ-SEC
             PERFORM  UNTIL  (FLG-SRYACCT      =  1)
                       OR    (FLG-SRYACT-CHECK =  1)
               INITIALIZE                      SRYACT-REC
               MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
               MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
               MOVE    ACCT-PTID           TO  SRY-PTID
               MOVE    ACCT-SRYYM          TO  SRY-SRYYM
               MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
               MOVE    "099999925"         TO  SRY-SRYCD(1)
               MOVE    "099999925"         TO  SRY-SRYCD(2)
               MOVE    "099999925"         TO  SRY-SRYCD(3)
               MOVE    "099999925"         TO  SRY-SRYCD(4)
               MOVE    "099999925"         TO  SRY-SRYCD(5)
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
               IF      MCP-RC   =   ZERO
                 MOVE   "tbl_sryact"       TO  MCP-TABLE
                 MOVE   "key8"             TO  MCP-PATHNAME
                 PERFORM 910-SRYACT-READ-SEC
                 IF    FLG-SRYACT  =  ZERO
                    MOVE    1              TO  FLG-SRYACT-CHECK
                 END-IF
               END-IF
               MOVE     "tbl_sryact"       TO  MCP-TABLE
               MOVE     "key8"             TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
      *
               MOVE     "tbl_sryacct"      TO  MCP-TABLE
               MOVE     "key67"            TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
             END-PERFORM
           END-IF
           MOVE     "tbl_sryacct"          TO  MCP-TABLE
           MOVE     "key67"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2004117-SRYACCT-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯£³
      *****************************************************************
       2004118-SRYACCT-CHECK3-SEC            SECTION.
      *
           INITIALIZE                          SRYACCT-REC
           MOVE   LNK-FTNJGNCHK-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE   LNK-FTNJGNCHK-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE   LNK-FTNJGNCHK-PTID       TO  ACCT-PTID
           MOVE   LNK-FTNJGNCHK-SRYYM      TO  ACCT-SRYYM
           MOVE   WRK-HKNCOMBINUM(IDX2)    TO  ACCT-HKNCOMBI
           MOVE   SRYACCT-REC              TO  MCPDATA-REC
           MOVE   "DBSELECT"               TO  MCP-FUNC
           MOVE   "tbl_sryacct"            TO  MCP-TABLE
           MOVE   "key71"                  TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC   =   ZERO
             MOVE   "tbl_sryacct"          TO  MCP-TABLE
             MOVE   "key71"                TO  MCP-PATHNAME
             PERFORM 910-SRYACCT-READ-SEC
             IF    FLG-SRYACCT  =  ZERO
                MOVE    1                  TO  FLG-SRYACCT-CHECK3
             END-IF
           END-IF
           MOVE     "tbl_sryacct"          TO  MCP-TABLE
           MOVE     "key71"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2004118-SRYACCT-CHECK3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯£´
      *****************************************************************
       2004119-SRYACCT-CHECK4-SEC            SECTION.
      *
           INITIALIZE                          SRYACCT-REC
           MOVE   LNK-FTNJGNCHK-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE   LNK-FTNJGNCHK-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE   LNK-FTNJGNCHK-PTID       TO  ACCT-PTID
           MOVE   LNK-FTNJGNCHK-SRYYM      TO  ACCT-SRYYM
           MOVE   WRK-HKNCOMBINUM(IDX2)    TO  ACCT-HKNCOMBI
           MOVE   SRYACCT-REC              TO  MCPDATA-REC
           MOVE   "DBSELECT"               TO  MCP-FUNC
           MOVE   "tbl_sryacct"            TO  MCP-TABLE
           MOVE   "key72"                  TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC   =   ZERO
             MOVE   "tbl_sryacct"          TO  MCP-TABLE
             MOVE   "key72"                TO  MCP-PATHNAME
             PERFORM 910-SRYACCT-READ-SEC
             IF    FLG-SRYACCT  =  ZERO
               MOVE      1                 TO  FLG-SRYACCT-CHECK4
             END-IF
           END-IF
           MOVE     "tbl_sryacct"          TO  MCP-TABLE
           MOVE     "key72"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2004119-SRYACCT-CHECK4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý£²
      *****************************************************************
       200412-SYUKEI-HENSYU2-SEC            SECTION.
      *
      *    ´µ¼ÔÉéÃ´¶â¤Î½¸·×
           PERFORM   UNTIL    FLG-SYUNOU     =  1
             MOVE    ZERO       TO      FLG-ARI
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL   (IDX1                  >  50 )
                          OR    (WRK-HKNCOMBINUM(IDX1) = ZERO)
                          OR    (FLG-ARI               =   1 )
               IF   SYU-HKNCOMBINUM  =  WRK-HKNCOMBINUM(IDX1)
                  MOVE      1                 TO  FLG-ARI
                  EVALUATE   LNK-FTNJGNCHK-KOHNUM
                    WHEN   SYU-KOH-HKNNUM(1)
                     ADD  SYU-KOH-HKNFTNMONEY(1)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                     IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                     AND  SYU-SKJ-FTNKBN(1)        =  "2"
                       ADD  SYU-SKJ-FTN(1)    TO  WRK-KOHFTN-SKJ
                     END-IF
                     IF  (LNK-FTNJGNCHK-KOHNUM  =  "092"   )
                     AND (LNK-FTNJGNCHK-SRYYM  >=  "201812")
                       IF   SYU-NYNSHOKAISU     =   7
                         MOVE     1           TO  FLG-KANGAN-CHECK
                       ELSE
                         MOVE     2           TO  FLG-KANGAN-CHECK
                       END-IF
                     END-IF
                    WHEN   SYU-KOH-HKNNUM(2)
                     ADD  SYU-KOH-HKNFTNMONEY(2)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                     IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                     AND  SYU-SKJ-FTNKBN(2)        =  "2"
                       ADD  SYU-SKJ-FTN(2)    TO  WRK-KOHFTN-SKJ
                     END-IF
                    WHEN   SYU-KOH-HKNNUM(3)
                     ADD  SYU-KOH-HKNFTNMONEY(3)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                     IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                     AND  SYU-SKJ-FTNKBN(3)        =  "2"
                       ADD  SYU-SKJ-FTN(3)    TO  WRK-KOHFTN-SKJ
                     END-IF
                    WHEN   SYU-KOH-HKNNUM(4)
                     ADD  SYU-KOH-HKNFTNMONEY(4)
                                       TO  WRK-KOHFTN-HKNFTNMONEY
                     IF   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                     AND  SYU-SKJ-FTNKBN(4)        =  "2"
                       ADD  SYU-SKJ-FTN(4)    TO  WRK-KOHFTN-SKJ
                     END-IF
                  END-EVALUATE
      *           IF  WRK-HKNCOMBINUM(IDX1) = LNK-FTNJGNCHK-HKNCOMBINUM
                  IF  WRK-HKNCOMBINUM-HKNID(IDX1)
                                            = LNK-FTNJGNCHK-HKNID
                     MOVE       1    TO    FLG-ARI2
                     ADD    SYU-TOTAL-HKNTEN  TO  WRK-SEIKYU
                     IF  SYU-SYUSKJ-RYOYOHI   NOT = ZERO
                     OR  SYU-KOH1SKJ-RYOYOHI  NOT = ZERO
                        MOVE    1    TO    FLG-SKJRYOYOHI-ARI
                     END-IF
                     PERFORM     2004113-SAIGAI-HENSYU2-SEC
                     IF  SYU-TOTAL-HKNTEN = ZERO
                        MOVE    1    TO    WRK-HKNCOMBINUM-ZEROFLG(IDX1)
                     END-IF
                  ELSE
                     PERFORM     2004112-SAIGAI-HENSYU-SEC
                     IF  FLG-SAIGAI-TAISYO  =  1
                        PERFORM  2004113-SAIGAI-HENSYU2-SEC
                     END-IF
                  END-IF
               END-IF
             END-PERFORM
      *
             IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key33"          TO  MCP-PATHNAME
             ELSE
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key25"          TO  MCP-PATHNAME
             END-IF
             PERFORM  910-SYUNOU-READ-SEC
           END-PERFORM
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
              IF    WRK-HKNMST-JGNGAK  =  999999
                   MOVE   01        TO  LNK-FTNJGNCHK-RC
              END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
           AND   FLG-ARI2          =    1
             IF  (LNK-FTNJGNCHK-SRYYM   >= "201101")
             AND (FLG-SAIGAI-ARI2        =    1    )
             AND (WRK-SYUSKJ-FTN = WRK-SKYMONEY-SKJ)
                       MOVE    1        TO  FLG-SAIGAI-ARI
             END-IF
             IF  (LNK-FTNJGNCHK-SRYYM   >= "201101")
             AND (FLG-SAIGAI-ARI         =    1    )
             AND (FLG-GENMEN-NASI        =  ZERO   )
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
             END-IF
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC  =   ZERO
           AND   FLG-ARI2          =    1
             IF   (LNK-FTNJGNCHK-NYUGAIKBN  =  "2" )
             AND  (WRK-SEIKYU               =  ZERO)
                MOVE     ZERO    TO   FLG-SRYACCT-CHECK4
                PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL  (IDX2               >  50)
                           OR    (FLG-SRYACCT-CHECK4 =  1 )
                  IF  WRK-HKNCOMBINUM-HKNID(IDX2)
                                      = LNK-FTNJGNCHK-HKNID
                    PERFORM  2004119-SRYACCT-CHECK4-SEC
                  END-IF
                END-PERFORM
                IF    FLG-SRYACCT-CHECK4  =  1
                  MOVE   99999   TO   WRK-SEIKYU
                END-IF
             END-IF
      *
             IF  WRK-SEIKYU         =  ZERO
                  MOVE        "1"       TO  LNK-FTNJGNCHK-SKYZEROFLG
             END-IF
      *
             IF  WRK-SEIKYU         =  ZERO
             AND FLG-SKJRYOYOHI-ARI =  ZERO
                MOVE     ZERO    TO   FLG-SRYACCT-CHECK
                PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL  (IDX2               >  50)
                           OR    (FLG-SRYACCT-CHECK  =  1 )
                  IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                    PERFORM  2004115-SRYACCT-CHECK-SEC
                  END-IF
                END-PERFORM
                IF    FLG-SRYACCT-CHECK  =  1
                  IF  LNK-FTNJGNCHK-KOHNUM(1:1) = "0"
      *              ¥·¥¹¥Æ¥à´ÉÍý£²£°£°£µÊÔ½¸
                     PERFORM  2004116-SYS2005-CHECK-SEC
                     IF  WRK-ZERO-KOH-PRTKBN = "1"
                       CONTINUE
                     ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                     END-IF
                  ELSE
                     IF  LNK-FTNJGNCHK-KOHNUM(1:1) >= "1" AND <= "8"
                       MOVE     ZERO    TO   FLG-SRYACT-CHECK
                       PERFORM  VARYING  IDX2  FROM  1  BY  1
                                 UNTIL  (IDX2               >  50)
                                  OR    (FLG-SRYACT-CHECK   =  1 )
                         IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                           PERFORM  2004117-SRYACCT-CHECK2-SEC
                         END-IF
                       END-PERFORM
                       IF   FLG-SRYACT-CHECK = 1
                         CONTINUE
                       ELSE
                         MOVE 01        TO  LNK-FTNJGNCHK-RC
                         MOVE  1        TO  FLG-TENKEN
                       END-IF
                     ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                     END-IF
                  END-IF
                ELSE
                  MOVE     ZERO    TO   FLG-SRYACCT-CHECK3
                  PERFORM  VARYING  IDX2  FROM  1  BY  1
                            UNTIL  (IDX2               >  50)
                             OR    (FLG-SRYACCT-CHECK3 =  1 )
                    IF  WRK-HKNCOMBINUM-ZEROFLG(IDX2)  =  1
                      PERFORM  2004118-SRYACCT-CHECK3-SEC
                    END-IF
                  END-PERFORM
                  IF  FLG-SRYACCT-CHECK3 =  1
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE    1        TO  FLG-TENKEN
                  ELSE
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                  END-IF
                END-IF
      *
                EVALUATE    FLG-KANGAN-CHECK
                  WHEN         2
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE   ZERO      TO  FLG-TENKEN
                END-EVALUATE
             END-IF
           END-IF
      *
           EVALUATE    FLG-KANGAN-CHECK
             WHEN         1
                       MOVE   01        TO  LNK-FTNJGNCHK-RC
                       MOVE   ZERO      TO  FLG-TENKEN
           END-EVALUATE
      *
           COMPUTE LNK-FTNJGNCHK-KOHFTN = WRK-KOHFTN-HKNFTNMONEY
           COMPUTE LNK-FTNJGNCHK-KOHFTNSKJ
                                        = WRK-KOHFTN-HKNFTNMONEY
                                        + WRK-KOHFTN-SKJ
      *
           .
       200412-SYUKEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Äã½êÆÀ¡¦ÄãÇ¯¶â¾ðÊóÊÔ½¸
      *****************************************************************
       2005-SYOTOKU-HENSYU-SEC       SECTION.
      *
           MOVE       1       TO      SYOTOKU-IDX
      *
           IF   LNK-FTNJGNCHK-SRYYM   >=  "200604"
      *
              INITIALIZE                         PTKOHINF-REC
              MOVE    LNK-FTNJGNCHK-HOSPNUM  TO  PTKOH-HOSPNUM
              MOVE    LNK-FTNJGNCHK-PTID     TO  PTKOH-PTID
              MOVE    LNK-FTNJGNCHK-KOHID    TO  PTKOH-KOHID
              MOVE    PTKOHINF-REC           TO  MCPDATA-REC
              MOVE    "DBSELECT"             TO  MCP-FUNC
              MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
              MOVE    "key8"                 TO  MCP-PATHNAME
              CALL    "ORCDBMAIN"            USING  MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
              IF      MCP-RC   =    ZERO
                MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                MOVE    "key8"           TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
                IF       FLG-PTKOHINF    =   ZERO
                  IF   PTKOH-TEKSTYMD(1:6)  >=  LNK-FTNJGNCHK-SRYYM
                      MOVE   PTKOH-TEKSTYMD(7:2)  TO   SYOTOKU-IDX
                  END-IF
                END-IF
              END-IF
              MOVE     "tbl_ptkohinf"    TO   MCP-TABLE
              MOVE     "key8"            TO   MCP-PATHNAME
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                       ORCSSYOTOKUAREA
           MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE  LNK-FTNJGNCHK-PTID     TO  LNK-SYOTOKU-PTID
           MOVE  LNK-FTNJGNCHK-SRYYM    TO  LNK-SYOTOKU-SRYYM
           MOVE  SPACE                  TO  LNK-SYOTOKU-SKJCHKFLG
      *
           CALL    "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                          SPA-AREA
      *
           IF    LNK-SYOTOKU-RC    =   ZERO
                 MOVE   LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX)
                                        TO  WRK-TEISYOTOKU
                                            WRK-TEISYOTOKU2
           END-IF
      *
           IF ((LNK-FTNJGNCHK-PREFNUM =  32            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "191"          ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200510"       ))
           OR ((LNK-FTNJGNCHK-PREFNUM =  11            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "483"          ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200804"       ))
           OR ((LNK-FTNJGNCHK-PREFNUM =  11            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "344"          ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200507"       ))
             CONTINUE
           ELSE
             PERFORM  20051-SYSKAN-HENSYU-SEC
             IF       WRK-CHIHOUNUMKBN         =   "1"
               IF     LNK-FTNJGNCHK-NYUGAIKBN  =   "1"
                 EVALUATE    WRK-TEISYOTOKU
                   WHEN    "2"
                   WHEN    "3"
                     IF    HKN-TSY-NYUFTNNBK  =  "0"
                       MOVE   SPACE   TO  WRK-TEISYOTOKU
                     END-IF
                   WHEN    "1"
                   WHEN    "4"
                     IF    HKN-TNK-NYUFTNKBN  =  "0"
                       MOVE   SPACE   TO  WRK-TEISYOTOKU
                     END-IF
                 END-EVALUATE
               ELSE
                 EVALUATE    WRK-TEISYOTOKU
                   WHEN    "2"
                     IF    HKN-TSY-GAIFTNKBN  =  "0"
                       MOVE   SPACE   TO  WRK-TEISYOTOKU
                     END-IF
                   WHEN    "1"
                   WHEN    "4"
                     IF    HKN-TNK-GAIFTNKBN  =  "0"
                       MOVE   SPACE   TO  WRK-TEISYOTOKU
                     END-IF
                 END-EVALUATE
               END-IF
             ELSE
                       MOVE   SPACE   TO  WRK-TEISYOTOKU
             END-IF
           END-IF
      *
           .
       2005-SYOTOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýÊÔ½¸
      *****************************************************************
       20051-SYSKAN-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CHIHOUNUMKBN
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-FTNJGNCHK-HOSPNUM
                                       TO  SYS-1001-HOSPNUM
           MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-EDYUKYMD(7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC               =  ZERO
               MOVE     "tbl_syskanri" TO  MCP-TABLE
               MOVE     "key10"        TO  MCP-PATHNAME
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-CHIHOUNUMKBN
                                       TO  WRK-CHIHOUNUMKBN
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE     "tbl_syskanri"     TO    MCP-TABLE
           MOVE     "key10"            TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       20051-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á°´ü¹âÎð¼Ô¡Ê£·£°ºÐ¡Á£·£´ºÐ¡ËÆÃÎãÂÐ¾Ý¼Ô¥Á¥§¥Ã¥¯
      *****************************************************************
       2006-ZENKI-TOKUREI-HANTEI-SEC            SECTION.
      *
           INITIALIZE                            HKNCOMBI-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID        TO  COMB-PTID
           MOVE    LNK-FTNJGNCHK-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE     "tbl_hkncombi"           TO  MCP-TABLE
           MOVE     "key"                    TO  MCP-PATHNAME
           PERFORM   910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI   =   ZERO
             MOVE   "tbl_hkncombi"           TO  MCP-TABLE
             MOVE   "key"                    TO  MCP-PATHNAME
             PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
             INITIALIZE                          HKNCOMBI-REC
           END-IF
           MOVE     "tbl_hkncombi"           TO  MCP-TABLE
           MOVE     "key"                    TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           INITIALIZE                            ORCSAGECHKAREA
           MOVE  LNK-FTNJGNCHK-HOSPNUM       TO  AGECHK-HOSPNUM
           MOVE  LNK-FTNJGNCHK-PTID          TO  AGECHK-PTID
           MOVE  LNK-FTNJGNCHK-SRYYM         TO  AGECHK-SRYYMD(1:6)
           MOVE  "01"                        TO  AGECHK-SRYYMD(7:2)
           MOVE  PTINF-BIRTHDAY              TO  AGECHK-BIRTHDAY
           CALL  "ORCSAGECHK"                USING  ORCSAGECHKAREA
                                                    SPA-AREA
           IF    AGECHK-RC  =   ZERO
              CONTINUE
           END-IF
      *
      *****
      *
           IF   (LNK-FTNJGNCHK-SRYYM      >=   "200804")
           AND  (LNK-FTNJGNCHK-SRYYM      <=   "201903")
      ***  AND  (LNK-FTNJGNCHK-PREFNUM     =    27     )
      ***  AND  (LNK-FTNJGNCHK-NYUGAIKBN   =   "2"     )
      *
             IF    AGECHK-NENREI   >=  70
               IF  (COMB-HKNNUM                =   SPACE   )
               OR  (COMB-SYU-HKNKOHSBTKBN  = "8" OR "9"    )
               OR  (COMB-HKNNUM  =  "971" OR "973" OR "975"
                                          OR "040" OR "039"
                                          OR "068" OR "069")
                CONTINUE
               ELSE
                IF ( COMB-HKNNUM      = "060"           )
                OR ((COMB-HKNNUM  NOT = "060"          ) AND
                    (COMB-HOJOKBN     = SPACE          ))
                  INITIALIZE                         PTKOHINF-REC
                  MOVE    LNK-FTNJGNCHK-HOSPNUM  TO  PTKOH-HOSPNUM
                  MOVE    LNK-FTNJGNCHK-PTID     TO  PTKOH-PTID
                  MOVE    "976"                  TO  PTKOH-KOHNUM
                  MOVE    PTKOHINF-REC           TO  MCPDATA-REC
                  MOVE    "DBSELECT"             TO  MCP-FUNC
                  MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
                  MOVE    "key4"                 TO  MCP-PATHNAME
                  CALL    "ORCDBMAIN"            USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                  IF      MCP-RC   =    ZERO
                    MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                    MOVE    "key4"               TO  MCP-PATHNAME
                    PERFORM  910-PTKOHINF-READ-SEC
                    PERFORM  UNTIL   FLG-PTKOHINF  =  1
                      MOVE   LNK-FTNJGNCHK-SRYYM TO  WRK-SRYYMD(1:6)
                      IF     COMB-TEKSTYMD(1:6)   >= LNK-FTNJGNCHK-SRYYM
                        MOVE COMB-TEKSTYMD(7:2)  TO  WRK-SRYYMD(7:2)
                      ELSE
                        MOVE  "01"               TO  WRK-SRYYMD(7:2)
                      END-IF
                      IF  (PTKOH-TEKSTYMD  <=  WRK-SRYYMD    )
                      AND (WRK-SRYYMD      <=  PTKOH-TEKEDYMD)
                      AND (PTKOH-SAKJOKBN   =  SPACE         )
                         MOVE    1               TO  FLG-KOREI-HIGAITO
                         MOVE    1               TO  FLG-PTKOHINF
                      ELSE
                         MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                         MOVE    "key4"          TO  MCP-PATHNAME
                         PERFORM  910-PTKOHINF-READ-SEC
                      END-IF
                    END-PERFORM
                  END-IF
                  MOVE     "tbl_ptkohinf"        TO   MCP-TABLE
                  MOVE     "key4"                TO   MCP-PATHNAME
                  PERFORM   910-DB-CLOSE-SEC
                END-IF
               END-IF
             END-IF
      *
             IF  (COMB-HKNNUM                =   SPACE   )
             OR  (COMB-SYU-HKNKOHSBTKBN  = "8" OR "9"    )
             OR  (COMB-HKNNUM  =  "971" OR "973" OR "975"
                               OR "068" OR "069" OR "040"
                                                 OR "039")
             OR  (AGECHK-NENREI         <     70         )
      ***    OR  (AGECHK-NENREI         >     74         )
             OR  (FLG-KOREI-HIGAITO     =      1         )
               CONTINUE
             ELSE
               IF  (COMB-HOJOKBN  =  "1"  OR  "9")
               AND (WRK-BIRTHDAY  <  "19440402"  )
                  MOVE    1     TO    FLG-ZENKI-TOKUREI
               END-IF
             END-IF
      *
           END-IF
      *
      *****
      *
           IF    FLG-ZENKI-TOKUREI       =   1
                 MOVE    20      TO     WRK-KYURATE
           ELSE
             IF  LNK-FTNJGNCHK-SRYYM    >=  "201204"
               IF  COMB-HKNNUM   =   SPACE
                 MOVE   100      TO     WRK-KYURATE
               ELSE
                 MOVE    30      TO     WRK-KYURATE
                 EVALUATE   LNK-FTNJGNCHK-NYUGAIKBN
                   WHEN       "1"
                      EVALUATE   COMB-HOJOKBN
                        WHEN   "0"
                        WHEN   "4"
                          MOVE   ZERO   TO   WRK-KYURATE
                        WHEN   "1"
                        WHEN   "5"
                        WHEN   "9"
                          MOVE   10     TO   WRK-KYURATE
                        WHEN   "2"
                        WHEN   "6"
                        WHEN   "8"
                          MOVE   20     TO   WRK-KYURATE
                        WHEN   "3"
                        WHEN   "7"
                          MOVE   30     TO   WRK-KYURATE
                      END-EVALUATE
                      IF   AGECHK-SYUGAKUKBN   =   1
                        IF    WRK-KYURATE  >   20
                          MOVE   20     TO   WRK-KYURATE
                        END-IF
                      END-IF
                   WHEN       "2"
                      EVALUATE   COMB-HOJOKBN
                        WHEN   "0"
                          MOVE   ZERO   TO   WRK-KYURATE
                        WHEN   "1"
                        WHEN   "4"
                        WHEN   "9"
                          MOVE   10     TO   WRK-KYURATE
                        WHEN   "2"
                        WHEN   "5"
                        WHEN   "8"
                          MOVE   20     TO   WRK-KYURATE
                        WHEN   "3"
                        WHEN   "6"
                        WHEN   "7"
                          MOVE   30     TO   WRK-KYURATE
                      END-EVALUATE
                      IF   AGECHK-SYUGAKUKBN   =   1
                        IF    WRK-KYURATE  >   20
                          MOVE   20     TO   WRK-KYURATE
                        END-IF
                      END-IF
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
           .
       2006-ZENKI-TOKUREI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-2010-REC
                                           SYS-200501-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-2010-REC
                                           SYS-200501-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_ptinf"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  PTINF-REC
                   MOVE    ZERO         TO  FLG-PTINF
               ELSE
                   INITIALIZE               PTINF-REC
                   MOVE    1            TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                   PTINF-REC
               MOVE    1                TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"         TO  MCP-TABLE
           MOVE     "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNNUM-SELECT-SEC        SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNNUM-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
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
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       910-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
