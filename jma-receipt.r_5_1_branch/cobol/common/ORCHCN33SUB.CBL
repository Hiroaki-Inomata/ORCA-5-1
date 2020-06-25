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
       PROGRAM-ID.                 ORCHCN33SUB.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥«¥ë¥Æ£³¹æ»æÍÑÆþ±¡²ñ·×¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  04/12/15    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  00.00.01    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.02    NACL-¾®Æ¦Âô  06/01/27  É¸·çÂÐ±þ
      *  01.00.03    NACL-¾®Æ¦Âô  06/02/01  ¼ç²Ê»»ÄêÍÑ¤ÎÊÖµÑÃÍÂÐ±þ
      *  01.00.04    NACL-¾®Æ¦Âô  06/02/03  ÎÅÍÜÃ´Åö¼êÅö¤ÆÊÖµÑÃÍÂÐ±þ
      *  01.00.05    NACL-¾®Æ¦Âô  06/02/02  MONFUNC ÂÐ±þ
      *  01.00.06    NACL-¾®Æ¦Âô  06/03/22  ÆÃÄêÆþ±¡ÎÁÄÉ²ÃÊ¬¡ÊÇ¾Â´Ãæ¥±¥¢¥æ¥Ë¡Ë
      *                                     ³°ÇñÃÖ¤­´¹¤¨½èÍý
      *  01.00.07    NACL-ÌçÏÆ    06/03/27  ¿©»ö²þÀµÂÐ±þ¡Ê£²£°£°£¶Ç¯£´·î¡Ë
      *  01.00.08    NACL-¾®Æ¦Âô  06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  01.00.09    NACL-¾®Æ¦Âô  06/10/24  Æþ±¡»þÀ¸³èÎÅÍÜÂÐ±þ
      *  03.04.01    NACL-Æ£¸¶    07/04/18  £Ä£Â£Ã£Ì£Ï£Ó£Å£Ã£Õ£Ò£Ó£Ï£ÒÂÐ±þ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.01.00    NACL-¾®Æ¦Âô  07/12/18  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡ÍâÆü¤Î
      *                                     ¿©»ö¤ÈÆþ±¡ÎÁ²Ã»»ÂÐ±þ
      *  04.04.01    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    10/08/09  Â¾°å¼õ¿Ç¡Ê³°Çñ£´¡Ë¤ÎÁªÄê»þÂÐ±þ
      *  04.05.01    NACL-¾®Æ¦Âô  10/01/14  ¼£¸³ÂÐ±þ
      *  04.05.02    NACL-¾®Æ¦Âô  10/01/19  ¿ÇÎÅÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤ÎÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    12/03/13  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¶¡¤£·¤ÎÂÐ±þ¡Ë
      *  04.05.04    NACL-ÌçÏÆ    12/09/18  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¸¡¤£¹¤ÎÂÐ±þ¡Ë
      *  04.07.01    NACL-ÌçÏÆ    14/03/14  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.02    NACL-ÌçÏÆ    14/03/26  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÌë¶Ð»þ´Ö·Ð²á¸º»»¤ÎÁªÄêÂÐ±þ¡Ë
      *  04.08.01    NACL-ÌçÏÆ    16/03/01  Ê¿À®£²£¸Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.02    NACL-ÌçÏÆ    18/03/01  Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.03    NACL-ÌçÏÆ    18/03/27  Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *
      *  05.00.01    NACL-ÌçÏÆ    20/03/09  ÎáÏÂ£²Ç¯£´·î²þÀµ
      *                                     Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÊÔ½¸
      *  05.00.02    NACL-ÌçÏÆ    20/03/16  Â¾°åÎÅµ¡´Ø¼õ¿Ç¡ÜÉ¸·ç¤Î·×»»½¤Àµ
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
           03  FLG-TENSUWRK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBI-MATCH          PIC 9(01).
           03  FLG-NYUINKA-MATCH           PIC 9(01).
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
           03  IDX8                        PIC 9(04).
           03  KSN-IDX                     PIC 9(04).
           03  GENSAN-IDX                  PIC 9(04).
           03  GENSAN-IDX2                 PIC 9(04).
           03  IDV                         PIC 9(02).
           03  TSY90-IDX                   PIC 9(02).
           03  ETC-IDX                     PIC 9(02).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-NYUINKA                 PIC 9(02).
           03  IDX-HKNCOMBI                PIC 9(02).
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
      *    ÃÔÊò²Ã»»
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *    ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
           03  CONS-TOKUSEIYAKU-KSN        PIC X(09) VALUE "190117570".
      *    É¸·ç¸º³Û¥³¡¼¥É
           03  CONS-HYOUKETSU1             PIC X(09) VALUE "190107890".
           03  CONS-HYOUKETSU2             PIC X(09) VALUE "190107990".
           03  CONS-HYOUKETSU3             PIC X(09) VALUE "190108090".
           03  CONS-HYOUKETSU4             PIC X(09) VALUE "190108190".
           03  CONS-HYOUKETSU5             PIC X(09) VALUE "190108290".
           03  CONS-HYOUKETSU6             PIC X(09) VALUE "190108390".
           03  CONS-HYOUKETSU7             PIC X(09) VALUE "190108490".
           03  CONS-HYOUKETSU8             PIC X(09) VALUE "190108590".
           03  CONS-HYOUKETSU9             PIC X(09) VALUE "190108690".
      *    ¤½¤ÎÂ¾¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤·¤¿Æþ±¡ÎÁ²Ã»»
           03  CONS-ETCNYUIN-TBL.
             05  CONS-ETCNYUIN-KSN         PIC X(09)  OCCURS 10.
      *    ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß¥ï¡¼¥¯ÍÑ
           03  TTNS-CONST-MAX              PIC 9(03) VALUE 200.
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CDKBN-KBNNUM            PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
           03  WRK-NYUKHNKBN-XXX           PIC 9(03).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    Æþ±¡²ñ·×¥ï¡¼¥¯
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-SRYKA-DAY       PIC X(01).
                   07  WRK-SRYKBN          PIC X(02).
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOUKETSU       PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-GAIHAKU2        PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
                   07  WRK-GAIHAKU-KBN     PIC 9(03).
                   07  WRK-CHUKSNCD        PIC X(04).
                   07  WRK-NYUKHNKBN       PIC 9(03).
                   07  WRK-KANGOGEN        PIC X(09).
      *
      *    Æþ±¡²ñ·×ÊÂ¤ÓÂØ¤¨¥ï¡¼¥¯
           03  WRK-NYUIN-SORT-TABLE.
               05  WRK-NYUIN-SORT-TBL      OCCURS  31.
                   07  WRK-SO-MEISAISU     PIC 9(02).
                   07  WRK-SO-SRYKA-DAY    PIC X(01).
                   07  WRK-SO-SRYKBN       PIC X(02).
                   07  WRK-SO-SRYCD        PIC X(09)  OCCURS 20.
                   07  WRK-SO-SRYCDTOTAL   PIC 9(14).
                   07  WRK-SO-TEN          PIC S9(08).
                   07  WRK-SO-TEN2         PIC S9(08)V9(5).
                   07  WRK-SO-KAISU        PIC 9(02).
      *
      *    Æ±°ìºÞ¤Þ¤È¤áÍÑÈæ³Ó¥­¡¼
           03  WRK-OLD-KEY.
               05  WRK-OLD-MEISAISU        PIC 9(02).
               05  WRK-OLD-SRYKA-DAY       PIC X(01).
               05  WRK-OLD-SRYCDTOTAL      PIC 9(14).
               05  WRK-OLD-TEN             PIC 9(07).
           03  WRK-NEW-KEY.
               05  WRK-NEW-MEISAISU        PIC 9(02).
               05  WRK-NEW-SRYKA-DAY       PIC X(01).
               05  WRK-NEW-SRYCDTOTAL      PIC 9(14).
               05  WRK-NEW-TEN             PIC 9(07).
      *
      *    Æþ±¡ÎÁ²Ã»»ÂàÈòÎÎ°è
           03  WRK-NYUKSN-TABLE.
               05  WRK-NYUKSN-TBL          OCCURS  20.
                   07  WRK-NYUKSN          PIC X(09).
                   07  WRK-NYUKSN-ZAITEN   PIC 9(08).
                   07  WRK-NYUKSN-DAY      PIC 9(03)  OCCURS 31.
      *
      *    ¼£¸³¸ºÅÀÂàÈòÎÎ°è
           03  WRK-CHIKEN-TABLE.
               05  WRK-CHIKEN-ZAITEN       PIC 9(08)  OCCURS 31.
      *
      *    °åÎÅ´Ñ»¡ÅÀ¿ôÂàÈòÎÎ°è
           03  WRK-IRYOKNS-TABLE.
               05  WRK-IRYOKNS-ZAITEN      PIC 9(08)  OCCURS 31.
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
      *    ÃÔÊò²Ã»»ÂàÈò
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
      *
      *    ¿©»ö¥ï¡¼¥¯
           03  WRK-SKJ-TABLE.
               05  WRK-SKJ-TBL           OCCURS  31.
                   07  WRK-SKJ-SRYKA-DAY       PIC X(01).
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
                   07  WRK-SKJ2-SRYKA-DAY      PIC X(01).
                   07  WRK-SKJ2-FUKUSU         PIC X(09).
                   07  WRK-SKJ2-KAISU          PIC 9(02).
      *    ¿©»ö¥ï¡¼¥¯£³¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           03  WRK-SKJ3-TABLE.
               05  WRK-SKJ3-TBL          OCCURS  31.
                   07  WRK-SKJ3-SRYKA-DAY      PIC X(01).
                   07  WRK-SKJ3-TOKU-SHOKU     PIC X(09).
                   07  WRK-SKJ3-KAISU          PIC 9(02).
      *
      *    ¿©»ö¥ï¡¼¥¯£²£°£°£¶£°£´
           03  WRK-SKJ200604-TABLE.
               05  WRK-SKJ200604-TBL     OCCURS  31.
                   07  WRK-SKJ200604-NYUINJI   PIC 9(04).
                   07  WRK-SKJ200604-TOKUSYO   PIC 9(04).
               05  WRK-SKJ200604-TBL2    OCCURS   6.
                   07  WRK-SKJ200604-KINGAKU   PIC 9(07).
      *
           03  WRK-SKJ200604X-TABLE.
               05  WRK-SKJ200604X-TBL    OCCURS  31.
                   07  WRK-SKJ200604-ASA       PIC 9(04).
                   07  WRK-SKJ200604-HIRU      PIC 9(04).
                   07  WRK-SKJ200604-YORU      PIC 9(04).
           03  WRK-SKJ200604X-TABLE-R  REDEFINES  WRK-SKJ200604X-TABLE
                                         OCCURS  31.
                   07  WRK-SKJ200604-AHY       PIC 9(04)   OCCURS   3.
      *
           03  WRK-KEISAN                      PIC 9(07).
           03  WRK-KEISAN2                     PIC S9(08).
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           03  WRK-TENSU-OCC                   OCCURS  200.
               05  WRK-TENSU-SRYCD             PIC X(09).
               05  WRK-TENSU-DATA              PIC X(2000).
           03  WRK-TENSU-CD                    PIC X(09).
           03  TTNS-IDX                        PIC 9(03).
           03  TTNS-MAX                        PIC 9(03).
      *
      *    ÁªÄêÎÅÍÜÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-SENTEI.
               05  TBL-SENTEI                  PIC 9(01)   OCCURS  31.
      *
      *    Æþ±¡²Ê°ìÃ×È½ÄêÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-KAMATCH.
               05  TBL-KAMATCH                 PIC X(01)   OCCURS  31.
      *
      *    Æþ±¡²ÊÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-NYUINKA.
               05  TBL-NYUINKA                 PIC X(02)   OCCURS  31.
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÂàÈòÍÑ¥Æ¡¼¥Ö¥ë
           03  TABLE-HKNCOMBI.
               05  TBL-HKNCOMBI                PIC 9(04)   OCCURS  31.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
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
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ÁªÄêÎÅÍÜÈñ
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö£²
           COPY    "CPORCSNYULIFE2.INC".
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
      *    Æþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCHCN33SUB.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
            ORCHCN33SUB-AREA
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
           INITIALIZE                  LNK-ORCHCN33SUB-HENKYAKU-AREA
           INITIALIZE                  CONS-ETCNYUIN-TBL
           MOVE    ZERO                TO  LNK-ORCHCN33SUB-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCHCN33SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-ORCHCN33SUB-RC      =    ZERO
               PERFORM 200-MAIN-SEC
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
           IF    ( LNK-ORCHCN33SUB-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-ORCHCN33SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-ORCHCN33SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ¿ÇÎÅ²Ê¥Á¥§¥Ã¥¯
           IF     LNK-ORCHCN33SUB-SRYKA    NOT =   SPACE
               CONTINUE
           ELSE
               MOVE    3               TO  LNK-ORCHCN33SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡ÝÊÝ¸±ÁÈ¹ç¤»¤ÎºÞ¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4               TO  LNK-ORCHCN33SUB-RC
      *********GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-ORCHCN33SUB-RC
               ELSE
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        IF     NACCT-DAY(IDX1)  NOT =  ZERO
                            MOVE  "1"     TO    WRK-SRYKA-DAY(IDX1)
                            MOVE  NACCT-DAY(IDX1) TO TBL-HKNCOMBI(IDX1)
                        END-IF
                    END-PERFORM
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    ¥Æ¥¹¥ÈÍÑ
      *     MOVE   "00"     TO    LNK-ORCHCN33SUB-SRYKA
      *     MOVE   "2"     TO    LNK-ORCHCN33SUB-SHORIKBN
      *ÂÐ¾ÝÆþ±¡²Ê¤Î¼èÆÀ
           INITIALIZE                           TABLE-NYUINKA
           MOVE    ZERO                     TO  IDX-ST
                                                IDX-ED
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    "key8"                   TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE    "key8"               TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK    =   1)
                   IF   PTNYUINRRK-JTIKBN    =    "5"
                       CONTINUE
                   ELSE
                       IF   (PTNYUINRRK-TENNYUYMD(1:6) <=
                               LNK-ORCHCN33SUB-SANTEIYM)   AND
                            (PTNYUINRRK-TENSTUYMD(1:6) >=
                               LNK-ORCHCN33SUB-SANTEIYM)
                           IF   LNK-ORCHCN33SUB-SRYKA = "00"
                               IF LNK-ORCHCN33SUB-SHORIKBN = "1" OR "2"
                                   IF  PTNYUINRRK-TENNYUYMD(1:6)   <
                                             LNK-ORCHCN33SUB-SANTEIYM
                                       MOVE  1       TO   IDX-ST
                                   ELSE
                                       MOVE  PTNYUINRRK-TENNYUYMD(7:2)
                                                     TO   IDX-ST
                                   END-IF
                                   IF  PTNYUINRRK-TENSTUYMD(1:6)   >
                                             LNK-ORCHCN33SUB-SANTEIYM
                                       MOVE  31      TO   IDX-ED
                                   ELSE
                                       MOVE  PTNYUINRRK-TENSTUYMD(7:2)
                                                     TO   IDX-ED
                                       IF  PTNYUINRRK-TENSTUYMD NOT =
                                           PTNYUINRRK-TAIINYMD 
                                         COMPUTE   IDX-ED = IDX-ED - 1
                                       END-IF
                                   END-IF
                                   PERFORM  VARYING  IDX1  
                                            FROM IDX-ST   BY  1
                                            UNTIL (IDX1  >  IDX-ED)
      **                                  IF   TBL-NYUINKA(IDX1) = SPACE
      **                                     MOVE PTNYUINRRK-NYUINKA  TO
      **                                               TBL-NYUINKA(IDX1)
      **                                  END-IF
      *
                                           MOVE PTNYUINRRK-NYUINKA  TO
                                                     TBL-NYUINKA(IDX1)
                                   END-PERFORM
                               END-IF
                           ELSE
                               IF     LNK-ORCHCN33SUB-SRYKA   =
                                                PTNYUINRRK-NYUINKA
                                   IF  PTNYUINRRK-TENNYUYMD(1:6)   <
                                             LNK-ORCHCN33SUB-SANTEIYM
                                       MOVE  1       TO   IDX-ST
                                   ELSE
                                       MOVE  PTNYUINRRK-TENNYUYMD(7:2)
                                                     TO   IDX-ST
                                   END-IF
                                   IF  PTNYUINRRK-TENSTUYMD(1:6)   >
                                             LNK-ORCHCN33SUB-SANTEIYM
                                       MOVE  31      TO   IDX-ED
                                   ELSE
                                       MOVE  PTNYUINRRK-TENSTUYMD(7:2)
                                                     TO   IDX-ED
                                       IF  PTNYUINRRK-TENSTUYMD NOT =
                                           PTNYUINRRK-TAIINYMD 
                                         COMPUTE   IDX-ED = IDX-ED - 1
                                       END-IF
                                   END-IF
                                   PERFORM  VARYING  IDX1  
                                            FROM IDX-ST   BY  1
                                            UNTIL (IDX1  >  IDX-ED)
                                        MOVE  "1"   TO
                                                     TBL-KAMATCH(IDX1)
                                   END-PERFORM
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "key8"                 TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE     5      TO      LNK-ORCHCN33SUB-RC
      *********GO   TO    200-MAIN-EXT
           END-IF
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key8"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF    ( LNK-ORCHCN33SUB-RC       NOT =   ZERO )
               GO   TO    200-MAIN-EXT
           END-IF
      *
           IF      LNK-ORCHCN33SUB-SRYKA   NOT =    "00"
               PERFORM    VARYING  IDX1   FROM  1  BY  1
                          UNTIL    IDX1   >    31
                   IF    (WRK-SRYKA-DAY(IDX1)   =   "1")   AND
                         (TBL-KAMATCH(IDX1)     =   "1")
                       CONTINUE
                   ELSE
                       MOVE   SPACE   TO   WRK-SRYKA-DAY(IDX1)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æþ±¡ÅÀ¿ô¼èÆÀ½èÍý
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   SPACE  OR  "1"
               PERFORM    210-NYUINTEN-GET-SEC
           END-IF
      *
      *    Æþ±¡ÅÀ¿ôÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   "1"
               PERFORM    210-NYUINTEN-SET-SEC
           END-IF
      *
      *    Æþ±¡Æü¿ôÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   "2"
               PERFORM    210-NYUINDAY-SET-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÅÀ¿ô¼èÆÀ
      *****************************************************************
       210-NYUINTEN-GET-SEC            SECTION.
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»¼èÆÀ
           IF      LNK-ORCHCN33SUB-SANTEIYM   >=   "201804"
             INITIALIZE                         ORCSKANGOGEN-AREA
             MOVE  LNK-ORCHCN33SUB-HOSPNUM  TO  LNK-KANGOGEN-HOSPNUM
             MOVE  LNK-ORCHCN33SUB-PTID     TO  LNK-KANGOGEN-PTID
             MOVE  LNK-ORCHCN33SUB-SANTEIYM TO  LNK-KANGOGEN-SANTEIYM
             CALL  "ORCSKANGOGEN"           USING  ORCSKANGOGEN-AREA
                                                   SPA-AREA
             IF      LNK-KANGOGEN-RC  =   ZERO
               PERFORM    VARYING   IDX1  FROM   1   BY   1
                           UNTIL    IDX1   >    31
                 MOVE  LNK-KANGOGEN-SRYCD(IDX1) TO  WRK-KANGOGEN(IDX1)
               END-PERFORM
             END-IF
           END-IF
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                                              WRK-CDKBN-KBNNUM
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
                           IF    (WRK-SRYKA-DAY(IDX1) NOT = SPACE)
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
      *                        °åÎÅ´Ñ»¡Ë¡¤ÎÆþ±¡ÎÁ»»Äê»þ¤Ï¥ï¡¼¥¯ÎÎ°èÂàÈò
                               IF   NSRY-SRYCD(1)(1:3)  =  "188"
                                   MOVE  NACCT-ZAITEN  TO
                                               WRK-IRYOKNS-ZAITEN(IDX1)
                               END-IF
      *
                               IF   NSRY-SRYCD(2)  NOT =   SPACE
                                   IF (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU1) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU2) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU3) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU4) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU5) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU6) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU7) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU8) AND
                                      (NSRY-SRYCD(2) NOT =
                                                    CONS-HYOUKETSU9)
                                       MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-SHOKIKSN(IDX1)
                                   END-IF
                               END-IF
      *                        É¸·ç¥³¡¼¥É
                               IF   NSRY-SRYCD(2)  NOT =   SPACE
                                   IF (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU1) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU2) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU3) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU4) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU5) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU6) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU7) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU8) OR
                                      (NSRY-SRYCD(2) =
                                                    CONS-HYOUKETSU9)
                                       MOVE   NSRY-SRYCD(2)  TO
                                                   WRK-HYOUKETSU(IDX1)
                                   END-IF
                               END-IF
                               IF   NSRY-SRYCD(3)  NOT =   SPACE
                                   IF (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU1) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU2) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU3) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU4) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU5) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU6) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU7) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU8) OR
                                      (NSRY-SRYCD(3) =
                                                    CONS-HYOUKETSU9)
                                       MOVE   NSRY-SRYCD(3)  TO
                                                   WRK-HYOUKETSU(IDX1)
                                   END-IF
                               END-IF
      *                        ¿ÇÎÅ¶èÊ¬
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
      *                        Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬ÃÖ¤­´¹¤¨
                               IF  LNK-ORCHCN33SUB-SANTEIYM >= "201804"
      *                          ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ
      *                                        ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
                                 IF   (WRK-KSNKBN       = 27 )
                                 AND  (WRK-CDKBN-KBNNUM = 101)
                                   MOVE  901 TO WRK-NYUKHNKSNKBN(IDX1)
                                 END-IF
      *                          Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
      *                                        ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
                                 IF   (WRK-KSNKBN       = 27 )
                                 AND  (WRK-CDKBN-KBNNUM = 109)
                                   MOVE  909 TO WRK-NYUKHNKSNKBN(IDX1)
                                 END-IF
                               END-IF
                               MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                               MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "key17"        TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
               IF   FLG-OK     =   ZERO
                   MOVE     7      TO      LNK-ORCHCN33SUB-RC
               END-IF
           ELSE
               MOVE     6      TO      LNK-ORCHCN33SUB-RC
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤Ë¤Ä¤¤¤Æ¼èÆÀ
           INITIALIZE                         ORCSNYUINDAY-AREA
           MOVE LNK-ORCHCN33SUB-HOSPNUM   TO  LNK-NYUINDAY-HOSPNUM
           MOVE LNK-ORCHCN33SUB-PTID      TO  LNK-NYUINDAY-PTID
           MOVE LNK-ORCHCN33SUB-SANTEIYM  TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING  ORCSNYUINDAY-AREA
                                              SPA-AREA
           IF      LNK-NYUINDAY-TBL    NOT = SPACE
               MOVE    ZERO        TO   LNK-ORCHCN33SUB-RC
           END-IF
      *
           IF    ( LNK-ORCHCN33SUB-RC       NOT =   ZERO )
                GO   TO    210-NYUINTEN-GET-EXT
          END-IF
      *
      *Æþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
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
                   MOVE    "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *ÁªÄêÎÅÍÜ¾ðÊó¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-SENTEI-SET
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "key22"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key22"            TO  MCP-PATHNAME
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
                   MOVE    "key22"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key22"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯
           PERFORM   210-RJN-TAISYO-CHK-SEC
      *
      *·îÆâ¤ËÊ£¿ô¤ÎÆþ±¡´ðËÜ¾ðÊó¡Ê£µ£°£°£°¡Ë¤¬¤Ê¤¤¤«¸¡º÷
           PERFORM   210-SYS-5000-CHK-SEC
      *
      *°åÎÅµ¡´Ø¸º»»¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*%"                TO  SYS-KBNCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
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
      *        ¿©»ö¾ðÊó¤ÎÂàÈò
               PERFORM    VARYING   IDX  FROM  1  BY  1
                          UNTIL     IDX   >    31
                   IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                       MOVE  SYS-5000-SHOKUJIRYOYOCD
                                          TO WRK-SYS5000-SHOKUCD(1 IDX)
                   END-IF
                   IF     SYS-5000-SHOKUCD(1)  NOT =  SPACE
                       IF  SYS-5000-SHOKUKBN(1) =     "2"
                           MOVE  SYS-5000-SHOKUCD(1)
                                          TO WRK-SYS5000-SHOKUCD(2 IDX)
                       END-IF
                   END-IF
      *             IF     SYS-5000-SHOKUCD(2)  NOT =  SPACE
      *                 IF  SYS-5000-SHOKUKBN(2) =     "2"
      *                     MOVE  SYS-5000-SHOKUCD(2)
      *                                    TO WRK-SYS5000-SHOKUCD(3 IDX)
      *                 END-IF
      *             END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
                 CONTINUE
           ELSE
                 PERFORM  210-SYS-5000-GET-SEC
           END-IF
      *
      *¿©Æ²²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL     =   197000570
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    (WRK-SRYKA-DAY(IDX1) NOT = SPACE)
                       AND   ((WRK-GAIHAKU-KBN(IDX1)  = ZERO) OR
                              (WRK-GAIHAKU-KBN(IDX1) >=  4  ))
                       AND   (NACCT-DAY(IDX1)    NOT =  ZERO )
                           MOVE  "197000570"
                                       TO WRK-SYS5000-SHOKUCD(3 IDX1)
                       END-IF
                   END-PERFORM
                 END-IF
                 MOVE    "key17"                 TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)    OR
                             (LNK-NYUINDAY-KBN(IDX1)  =  "1"))
                       AND  ((WRK-GAIHAKU-KBN(IDX1)  = ZERO )    OR
                             (WRK-GAIHAKU-KBN(IDX1) >=  4   ))
                       AND  ( NACCT-DAY(IDX1)    NOT =  ZERO )
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
      *
                       END-IF
                   END-PERFORM
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *ËÌ³¤Æ»¤ÎÎÅÍÜÃ´Åö¼êÅö¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "7"                     TO  NACCT-ZAISKBKBN
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                             (LNK-NYUINDAY-KBN(IDX1)  =  "1"))
                       AND  ((WRK-GAIHAKU-KBN(IDX1)  = ZERO )  OR
                             (WRK-GAIHAKU-KBN(IDX1) >=  4   ))
                       AND  ( NACCT-DAY(IDX1)    NOT =  ZERO )
                           MOVE   NACCT-ZAITEN  
                                        TO  LNK-ORCHCN33SUB-ETCTEN
                                                             (IDX1)
      *
                       END-IF
                   END-PERFORM
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ¿ÇÎÅ²ñ·×¥Æ¡¼¥Ö¥ë¤è¤êÆþ±¡ÎÁ²Ã»»¤Î¼èÆÀ
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   SPACE  OR  "1"
               PERFORM    210-NYUKSN-SRYACCT-GET-SEC
           END-IF
      *
      *    ¼èÆÀ¤·¤¿Æþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *        ¿ÇÎÅ¶èÊ¬
               MOVE   WRK-SRYKBN(IDX1)  TO  WRK-SO-SRYKBN(IDX1)
      *        ÊÝ¸±ÁÈ¹ç¤»
               MOVE   WRK-SRYKA-DAY(IDX1)
                                        TO  WRK-SO-SRYKA-DAY(IDX1)
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
                           MOVE    "key2"         TO  MCP-PATHNAME
                           PERFORM   910-SENTEICDCHG-SELECT-SEC
                           IF      FLG-SENTEICDCHG  =    ZERO
                               MOVE    "key2"   TO  MCP-PATHNAME
                               PERFORM  910-SENTEICDCHG-READ-SEC
                               IF    FLG-SENTEICDCHG  =   ZERO
                                 IF  SENCHG-IPNSRYCD  =
                                                  SENCHG-SENTEISRYCD
      *                            Ìë¶Ð»þ´Ö·Ð²á¸º»»¤ÎÁªÄêÂÐ±þ
      *                            "190131410","190165810","190165910",
      *                            "190131310"
                                   IF  LNK-ORCHCN33SUB-SANTEIYM
                                                  >= "201404"
                                    ADD    1    TO  WRK-MEISAISU
                                    MOVE  "190122890"
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                                   END-IF
                                 ELSE
                                   MOVE  SENCHG-SENTEISRYCD
                                                TO  WRK-SO-SRYCD
                                                   (IDX1 WRK-MEISAISU)
                                 END-IF
                               END-IF
                           END-IF
                           MOVE   "tbl_senteicdchg"    TO  MCP-TABLE
                           MOVE   "key2"               TO  MCP-PATHNAME
                         PERFORM   910-DB-CLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *        Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¸º»»
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-KANGOGEN(IDX1)  NOT =        SPACE)
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-KANGOGEN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ
               IF         WRK-GAIHAKU(IDX1)   NOT =        SPACE
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ£²
               IF         WRK-GAIHAKU2(IDX1)  NOT =        SPACE
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU2(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )
               AND     (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        É¸·ç
               IF      WRK-HYOUKETSU(IDX1)   NOT =         SPACE
                 IF   (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )
                 AND  (WRK-NYUINKHN(IDX1)   NOT =        SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOUKETSU(IDX1) TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOUKETSU(IDX1)
                 END-IF
               END-IF
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
      *        Æü¾ïÀ¸³è¾ã³²²Ã»»
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
      *        ÃÔÊò²Ã»»
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
      *        êóáì´µ¼Ô´ÉÍý²Ã»»
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
      *        ÆÃÄê¹³Àº¿ÀÉÂÌô¼£ÎÅ´ÉÍý²Ã»»
               IF         WRK-TOKUSEIYAKU-KSN(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-TOKUSEIYAKU-KSN TO  WRK-SO-SRYCD
                                           (IDX1 WRK-MEISAISU)
               END-IF
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
                       WHEN   1
                       WHEN   3
                           INITIALIZE   WRK-NYUIN-SORT-TBL(IDX1)
      *                Â¾°åÎÅµ¡´Ø¼õ¿Ç
                       WHEN   4
                         IF   LNK-ORCHCN33SUB-SANTEIYM  >=  "201004"
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
                            OR (IDX2    >      20)
                   PERFORM    210-TENSU-KEISAN-SEC
               END-PERFORM
      *        ¼£¸³Ê¬¤Î¸ºÅÀ
               COMPUTE  WRK-SO-TEN2(IDX1) = WRK-SO-TEN2(IDX1)
                                          - WRK-CHIKEN-ZAITEN(IDX1)
               COMPUTE  WRK-SO-TEN (IDX1) = WRK-SO-TEN2(IDX1)
                                          + 0.5
           END-PERFORM
      *
      **     PERFORM    VARYING   IDX1   FROM  1  BY  1
      **                UNTIL     (IDX1   >     31)
      **          DISPLAY  "IDX1 = " IDX1
      **          DISPLAY  "WRK-SO-SRYKA-DAY = " WRK-SO-SRYKA-DAY
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
      *    ¥«¥ë¥Æ£³¹æÍÑ»æ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤ØÆþ±¡ÅÀ¿ô¥»¥Ã¥È
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   SPACE
               PERFORM    VARYING   IDX1   FROM  1  BY  1
                          UNTIL     (IDX1   >     31)
                   IF     WRK-SO-TEN(IDX1)       NOT =   ZERO
                        MOVE   WRK-SO-TEN(IDX1)   TO
                                       LNK-ORCHCN33SUB-TEN(IDX1)
                   END-IF
      *            °åÎÅ´Ñ»¡Ë¡¤ÎÆþ±¡»þÅÀ¿ôÃÖ¤­´¹¤¨
                   IF    (WRK-SO-TEN(IDX1)       NOT =   ZERO)  AND
                         (WRK-IRYOKNS-ZAITEN(IDX1)  NOT =  ZERO)
                        MOVE   WRK-IRYOKNS-ZAITEN(IDX1)   TO
                                       LNK-ORCHCN33SUB-TEN(IDX1)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ¥«¥ë¥Æ£³¹æÍÑ»æ¥µ¥ÖÊÖµÑ¥¨¥ê¥¢¤Ø¿©»ö¾ðÊó¤Î¥»¥Ã¥È
           IF   LNK-ORCHCN33SUB-SHORIKBN       =   SPACE
               IF   LNK-ORCHCN33SUB-SANTEIYM  >=  "200604"
                  PERFORM    210-SKJ-SET200604-SEC
               ELSE
                  PERFORM    210-SKJ-SET-SEC
               END-IF
           END-IF
      *
      *
           .
       210-NYUINTEN-GET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE     "key9"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
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
           MOVE    NSRY-SRYCD(1)       TO  TNS-SRYCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           IF  (LNK-ORCHCN33SUB-SANTEIYM >= "202004")
           AND (TNS-SRYCD             NOT = SPACE   )
           AND (TNS-NYUKHNKSNKBN          = ZERO    )
             INITIALIZE                          SNTENSU-AREA
             MOVE  TNS-SRYCD                 TO  SNTENSU-SRYCD
             MOVE  LNK-ORCHCN33SUB-SANTEIYM  TO  SNTENSU-BASEYMD(1:6)
             MOVE  "01"                      TO  SNTENSU-BASEYMD(7:2)
             CALL  "ORCSNTENSU"              USING
                                             SNTENSU-AREA
                                             TNS-TENSU-REC
                                             SPA-AREA
           END-IF
           MOVE    TNS-NYUKHNKSNKBN    TO  WRK-KSNKBN
           MOVE    TNS-CDKBN-KBNNUM    TO  WRK-CDKBN-KBNNUM
           MOVE    TNS-CHUKSNCD        TO  WRK-CHUKSNCD-XXX
           MOVE    TNS-NYUKHNKBN       TO  WRK-NYUKHNKBN-XXX
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
                    WHEN     3
                      MOVE   "190111590"   TO  WRK-GAIHAKU(IDX1)
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
      ***           WHEN    11
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
                    WHEN     3
                      MOVE   "190111590"   TO  WRK-GAIHAKU(IDX1)
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
      ***           WHEN    11
                    WHEN    13
                    WHEN    17
                      PERFORM   2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
           END-EVALUATE
      *
           IF   WRK-GAIHAKU(IDX1)  =  SPACE
                 MOVE   ZERO    TO    WRK-GAIHAKU-KBN(IDX1)
           END-IF
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
           IF       LNK-ORCHCN33SUB-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-TAKIKAN3-SEC
           ELSE
             IF     LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
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
                 IF   LNK-ORCHCN33SUB-SANTEIYM  >=  "201004"
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
           IF       LNK-ORCHCN33SUB-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-GENSAN3-SEC
           ELSE
             IF     LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
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
                      IF   LNK-ORCHCN33SUB-SANTEIYM  >=  "201404"
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
                      IF   LNK-ORCHCN33SUB-SANTEIYM  >=  "201404"
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
           MOVE    LNK-ORCHCN33SUB-HOSPNUM TO  COMB-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID  TO   COMB-PTID
           PERFORM   VARYING   IDX   FROM  1  BY  1
                     UNTIL     (WRK-SRYKA-DAY(IDX)  NOT =  ZERO) OR
                               (IDX     >   31)
               IF   WRK-SRYKA-DAY(IDX)   NOT =   SPACE
                  MOVE    WRK-SRYKA-DAY(IDX)
                                       TO   COMB-HKNCOMBINUM
               END-IF
           END-PERFORM
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE     "key"          TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    LNK-ORCHCN33SUB-HOSPNUM
                                       TO  AGECHK-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID
                                       TO  AGECHK-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
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
      *    ¿ÇÎÅ²ñ·×¥Æ¡¼¥Ö¥ë¤è¤êÆþ±¡ÎÁ²Ã»»¡Ê¼£¸³¤Î¸ºÅÀ¡Ë¤Î¼èÆÀ½èÍý
      *****************************************************************
       210-NYUKSN-SRYACCT-GET-SEC            SECTION.
      *
      *Æþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                             SRYACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                       TO   ACCT-NYUGAIKBN
           MOVE    LNK-ORCHCN33SUB-PTID      TO   ACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM  TO   ACCT-SRYYM
           MOVE    "key30"                   TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "key30"            TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
                    IF   ACCT-ZAIKBN    NOT =    2
                     IF   ACCT-SRYCDTOTAL(6:9)  =   
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
                                        OR  CONS-TOKUSEIYAKU-KSN
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                 (LNK-NYUINDAY-KBN(IDX1)  =  "1"))
                           AND  ((WRK-GAIHAKU-KBN(IDX1)  = ZERO )  OR
                                 (WRK-GAIHAKU-KBN(IDX1) >=  4   ))
                           AND  ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
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
      *                         ÃÔÊò²Ã»»
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
      *
                           END-IF
                       END-PERFORM
                     ELSE
      *                ¤½¤ÎÂ¾Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
      *                ¿ÇÎÅ²ñ·×¤«¤é¿ÇÎÅ¹Ô°Ù¤ò¸¡º÷¤·¡¢Æþ±¡ÎÁ²Ã»»¤Î¿ÇÎÅ
      *                ¥³¡¼¥É¤òÆÃÄê¤¹¤ë¡Ê¼£¸³¸ºÅÀÊ¬¤Ï½ü¤¯¡Ë
                       IF        ACCT-MEISAISU    =      1
                             CONTINUE
                       ELSE
                          INITIALIZE      SRYACT-REC
                          MOVE    ACCT-HOSPNUM    TO   SRY-HOSPNUM
                          MOVE    ACCT-NYUGAIKBN TO   SRY-NYUGAIKBN
                          MOVE    ACCT-PTID      TO   SRY-PTID
                          MOVE    ACCT-SRYKA     TO   SRY-SRYKA
                          MOVE    ACCT-SRYYM     TO   SRY-SRYYM
                          MOVE    ACCT-ZAINUM    TO   SRY-ZAINUM
                          MOVE    1              TO   SRY-RENNUM
                          MOVE    "key"          TO   MCP-PATHNAME
                          PERFORM 910-SRYACT-SELECT-SEC
                          IF      FLG-SRYACT     =    ZERO
                              MOVE     "key"          TO  MCP-PATHNAME
                              PERFORM   910-SRYACT-READ-SEC
                              IF  FLG-SRYACT    =    ZERO
                                  MOVE  SRY-SRYCD(1)   TO  
                                                  ACCT-SRYCDTOTAL(6:9)   
                              END-IF
                          END-IF
      *                   ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                          MOVE    "tbl_sryact"   TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM   910-DB-CLOSE-SEC
                       END-IF
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                 (LNK-NYUINDAY-KBN(IDX1)  =  "1"))
                           AND  ((WRK-GAIHAKU-KBN(IDX1)  = ZERO )  OR
                                 (WRK-GAIHAKU-KBN(IDX1) >=  4   ))
                           AND  ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
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
      *
      *             ¼£¸³¸ºÅÀ¿ô¤ÎÂàÈò
                    IF      (ACCT-ZAIKBN      =      2)   AND
                            (ACCT-ZAITEN      NOT =  ZERO)
                        PERFORM    VARYING   IDX1   FROM   1   BY   1
                                   UNTIL     IDX1   >   31
                           IF   ((WRK-NYUINKHN(IDX1) NOT = SPACE)  OR
                                 (LNK-NYUINDAY-KBN(IDX1)  =  "1")) AND
                                (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                             ADD  ACCT-ZAITEN    TO 
                                              WRK-CHIKEN-ZAITEN(IDX1)
                          END-IF
                        END-PERFORM
                    END-IF
      *
                   END-IF
                 END-IF
                 MOVE     "key30"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key30"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUKSN-SRYACCT-GET-EXT.
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
               MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
               IF    LNK-ORCHCN33SUB-SANTEIYM  <=  "201203"
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
           MOVE    LNK-ORCHCN33SUB-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                      TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD             TO  TNS-YUKOEDYMD
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
                   COMPUTE    WRK-SO-TEN2(IDX1)  =
                              WRK-SO-TEN2(IDX1)  +
                              TNS-TEN
      *        ¡ó²Ã»»
               WHEN    5
                   COMPUTE    WRK-SO-TEN2(IDX1)  =
                              WRK-SO-TEN2(IDX1)  +
                             (WRK-SO-TEN2(IDX1)  *
                             (TNS-TEN / 100))
      *        ¡ó¸º»»
               WHEN    6
                 IF    WRK-SO-SRYCD(IDX1 IDX2) = "190122890"
                   COMPUTE    WRK-KEISAN2           =
                            ( WRK-SO-TEN2(IDX1)     -
                             (WRK-SO-TEN2(IDX1)     *
                             (TNS-TEN   /  100)))   +  0.5
                   MOVE       WRK-KEISAN2  TO   WRK-SO-TEN2(IDX1)
                 ELSE
                   COMPUTE    WRK-SO-TEN2(IDX1)  =
                              WRK-SO-TEN2(IDX1)  -
                             (WRK-SO-TEN2(IDX1)  *
                             (TNS-TEN / 100))
                 END-IF
      *        ÅÀ¿ô¡Ê¥Þ¥¤¥Ê¥¹¡Ë
               WHEN    8
                   COMPUTE    WRK-SO-TEN2(IDX1)  =
                              WRK-SO-TEN2(IDX1)  -
                              TNS-TEN
      *        WHEN  OTHER
      *            COMPUTE    WRK-SO-TEN2(IDX1)  =
      *                       WRK-SO-TEN2(IDX1)  +
      *                       TNS-TEN
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
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
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
           IF   (LNK-ORCHCN33SUB-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
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
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "key11"           TO  MCP-PATHNAME
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
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
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
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
                 MOVE    "key11"           TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
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
      *    ¿©»ö¾ðÊó¼èÆÀ½èÍý¡ÊÊ¿À®£±£¸Ç¯£³·î°ÊÁ°¡Ë
      *****************************************************************
       210-SKJ-SET-SEC            SECTION.
      *
      *¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                           NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "key28"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "key28"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   VARYING   IDX1   FROM  1   BY  1
                         UNTIL     IDX1   >    31
                   IF    (WRK-SRYKA-DAY(IDX1) NOT = SPACE)    AND
                         (NACCT-DAY(IDX1)    NOT =  ZERO)     AND
                         (WRK-NYUINKHN(IDX1) NOT =  SPACE)    AND
                         ((WRK-GAIHAKU-KBN(IDX1)  = ZERO) OR
                          (WRK-GAIHAKU-KBN(IDX1) >=  4  ))
                       MOVE   WRK-SRYKA-DAY(IDX1)
                                       TO  WRK-SKJ-SRYKA-DAY(IDX1)
                                           WRK-SKJ2-SRYKA-DAY(IDX1)
                                           WRK-SKJ3-SRYKA-DAY(IDX1)
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
                           WHEN   2
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                           WHEN   3
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                           WHEN   4
                               MOVE   WRK-SYS5000-SHOKUCD(1 IDX1)
                                               TO  WRK-SKJ-RYOYO(IDX1)
                               MOVE   "197000470"
                                         TO  WRK-SKJ3-TOKU-SHOKU(IDX1)
                               MOVE   "197000670"
                                          TO  WRK-SKJ2-FUKUSU(IDX1)
                               EVALUATE  WRK-SYS5000-SHOKUCD(1 IDX1)
                                   WHEN  "197000110"
                                      ADD  1920 TO WRK-SKJ-MONEY(IDX1)
                                   WHEN  "197000710"
                                      ADD  1520 TO WRK-SKJ-MONEY(IDX1)
                               END-EVALUATE
                       END-EVALUATE
      *                ÆÃÊÌ´ÉÍý²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(1 IDX1) = "197000110"
                        IF    WRK-SYS5000-SHOKUCD(2 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(2 IDX1)
                                          TO  WRK-SKJ-TOKU-KANRI(IDX1)
                           ADD   200          TO  WRK-SKJ-MONEY(IDX1)
                        END-IF
                       END-IF
      *                ¿©Æ²²Ã»»
                       IF    WRK-SYS5000-SHOKUCD(1 IDX1) = "197000110"
                        IF    WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                           MOVE  WRK-SYS5000-SHOKUCD(3 IDX1)
                                          TO  WRK-SKJ-SHOKUDOU(IDX1)
                           ADD    50          TO  WRK-SKJ-MONEY(IDX1)
                        END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
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
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
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
      *    ÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È
           MOVE       ZERO    TO   IDX2
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ-SRYCD(IDX1 1)   NOT =  SPACE
                    ADD  WRK-SKJ-MONEY(IDX1)  TO
                                LNK-ORCHCN33SUB-SKJ(IDX1)
                END-IF
           END-PERFORM
      *    ÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÆÃÊÌ¿©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ3-TOKU-SHOKU(IDX1)   NOT =  SPACE
                    ADD  350  TO
                                LNK-ORCHCN33SUB-SKJ(IDX1)
                END-IF
           END-PERFORM
      *    ÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¿©»ö¥»¥Ã¥È¡ÊÊ£¿ô¸¥Î©²Ã»»¡Ë
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
                IF    WRK-SKJ2-FUKUSU(IDX1)   NOT =  SPACE
                    ADD   50  TO
                                LNK-ORCHCN33SUB-SKJ(IDX1)
                END-IF
           END-PERFORM
      *
           .
       210-SKJ-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊó¼èÆÀ½èÍý¡ÊÊ¿À®£±£¸Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       210-SKJ-SET200604-SEC            SECTION.
      *
      *    ¿©»ö¾ðÊó¤Î¼èÆÀ¡ÊÆþ±¡²ñ·×¡Ë
           INITIALIZE                           NYUINACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  NACCT-SRYYM
           MOVE    "key33"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT            =   ZERO
               MOVE     "key33"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   FLG-NYUINACCT   =    1
      *
                 EVALUATE   NACCT-SRYCDTOTAL
                    WHEN      099999918
                      PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL    IDX1    >    31
                        IF       NACCT-DAY(IDX1)   NOT =  ZERO
                          MOVE   NACCT-DAY(IDX1) TO
                                               WRK-SKJ200604-ASA (IDX1)
                        END-IF
                      END-PERFORM
                    WHEN      099999919
                      PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL    IDX1    >    31
                        IF       NACCT-DAY(IDX1)   NOT =  ZERO
                          MOVE   NACCT-DAY(IDX1) TO
                                               WRK-SKJ200604-HIRU(IDX1)
                        END-IF
                      END-PERFORM
                    WHEN      099999920
                      PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL    IDX1    >    31
                        IF       NACCT-DAY(IDX1)   NOT =  ZERO
                          MOVE   NACCT-DAY(IDX1) TO
                                               WRK-SKJ200604-YORU(IDX1)
                        END-IF
                      END-PERFORM
                 END-EVALUATE
      *
                 MOVE     "key33"             TO  MCP-PATHNAME
                 PERFORM   910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key33"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¤ÈÆÃÊÌ¿©²Ã»»¤Î¿¶¤êÊ¬¤±
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                       UNTIL    IDX1    >    31
              EVALUATE   WRK-SKJ200604-ASA (IDX1)
                WHEN        1
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                WHEN        2
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                                     WRK-SKJ200604-TOKUSYO(IDX1)
                WHEN        3
                  IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                  END-IF
              END-EVALUATE
              EVALUATE   WRK-SKJ200604-HIRU(IDX1)
                WHEN        1
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                WHEN        2
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                                     WRK-SKJ200604-TOKUSYO(IDX1)
                WHEN        3
                  IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                  END-IF
              END-EVALUATE
              EVALUATE   WRK-SKJ200604-YORU(IDX1)
                WHEN        1
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                WHEN        2
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                                     WRK-SKJ200604-TOKUSYO(IDX1)
                WHEN        3
                  IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
                    ADD   1     TO   WRK-SKJ200604-NYUINJI(IDX1)
                  END-IF
              END-EVALUATE
           END-PERFORM
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
           INITIALIZE                      TNS-TENSU-REC
           MOVE    "197000110"         TO  TNS-SRYCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  WRK-SKJ200604-KINGAKU(1)
      *
      *    Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
           INITIALIZE                      TNS-TENSU-REC
           MOVE    "197000710"         TO  TNS-SRYCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  WRK-SKJ200604-KINGAKU(2)
      *
      *    ÆÃÊÌ¿©²Ã»»
           INITIALIZE                      TNS-TENSU-REC
           MOVE    "197000470"         TO  TNS-SRYCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  WRK-SKJ200604-KINGAKU(3)
      *
      *    ¿©Æ²²Ã»»
           INITIALIZE                      TNS-TENSU-REC
           MOVE    "197000570"         TO  TNS-SRYCD
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  WRK-SKJ200604-KINGAKU(4)
      *
           IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
      *      Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë¡Ê£±¿©¤Ë¤Ä¤­¡Ë¡ÊÎ®Æ°¿©¤Î¤ß¤òÄó¶¡¡Ë
             INITIALIZE                    TNS-TENSU-REC
             MOVE    "197003110"       TO  TNS-SRYCD
             MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
             MOVE    "01"              TO  TNS-YUKOSTYMD(7:2)
             MOVE    TNS-YUKOSTYMD     TO  TNS-YUKOEDYMD
             PERFORM 910-TENSU-READ-SEC
             MOVE    TNS-TEN           TO  WRK-SKJ200604-KINGAKU(5)
      *      Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë¡Ê£±¿©¤Ë¤Ä¤­¡Ë¡ÊÎ®Æ°¿©¤Î¤ß¤òÄó¶¡¡Ë
             INITIALIZE                    TNS-TENSU-REC
             MOVE    "197003210"       TO  TNS-SRYCD
             MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  TNS-YUKOSTYMD(1:6)
             MOVE    "01"              TO  TNS-YUKOSTYMD(7:2)
             MOVE    TNS-YUKOSTYMD     TO  TNS-YUKOEDYMD
             PERFORM 910-TENSU-READ-SEC
             MOVE    TNS-TEN           TO  WRK-SKJ200604-KINGAKU(6)
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜ¡Ê¿©»öÎÅÍÜ¡¦´Ä¶­ÎÅÍÜ¡Ë¤Î¼èÆÀ
           IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
             INITIALIZE                           ORCSNYULIFE2-AREA
             MOVE    LNK-ORCHCN33SUB-HOSPNUM  TO  LNK-NYULIFE2-HOSPNUM
             MOVE    LNK-ORCHCN33SUB-PTID     TO  LNK-NYULIFE2-PTID
             MOVE    LNK-ORCHCN33SUB-SANTEIYM TO  LNK-NYULIFE2-SANTEIYM
             CALL    "ORCSNYULIFE2"   USING  ORCSNYULIFE2-AREA
                                             SPA-AREA
           ELSE
             IF  LNK-ORCHCN33SUB-SANTEIYM  >=  "200610"
               INITIALIZE                         ORCSNYULIFE-AREA
               MOVE  LNK-ORCHCN33SUB-HOSPNUM  TO  LNK-NYULIFE-HOSPNUM
               MOVE  LNK-ORCHCN33SUB-PTID     TO  LNK-NYULIFE-PTID
               MOVE  LNK-ORCHCN33SUB-SANTEIYM TO  LNK-NYULIFE-SANTEIYM
               CALL  "ORCSNYULIFE"    USING  ORCSNYULIFE-AREA
                                             SPA-AREA
             END-IF
           END-IF
      *    
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü¤Ë¤Ä¤¤¤Æ¼èÆÀ
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-ORCHCN33SUB-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
      *    ¿©»öÎÅÍÜÈñ·×»»
           IF    LNK-ORCHCN33SUB-SANTEIYM  >=  "201604"
              PERFORM  2101-SKJ-HEN201604-SEC
           ELSE
              PERFORM  2102-SKJ-HEN200604-SEC
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°ÙÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤Î¶â³Û»»Äê
           PERFORM     210-TOKU-SHOKU-HENSHU-SEC
      *
           .
       210-SKJ-SET200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆþÎÏ¤µ¤ì¤¿ÆÃÊÌ¿©²Ã»»¤Î¶â³Û»»Äê
      *****************************************************************
       210-TOKU-SHOKU-HENSHU-SEC        SECTION.
      *
      *ÆÃÊÌ¿©²Ã»»¡Ê¿©»öÎÅÍÜ¡Ë¤Î¼èÆÀ
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCHCN33SUB-PTID    TO  ACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                           TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key54"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key54"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT   =  1)
      *
                   PERFORM    VARYING   IDX1   FROM   1   BY  1
                     UNTIL   (IDX1         >   31)
                       IF     ACCT-DAY(1 IDX1)   NOT =   ZERO
                       AND    ACCT-SRYKBN            =   "97"
                           MOVE    ZERO        TO  WRK-KEISAN
                           COMPUTE WRK-KEISAN  =   ACCT-ZAITEN *
                                                   ACCT-DAY(1 IDX1)
                           ADD     WRK-KEISAN  TO  
                                            LNK-ORCHCN33SUB-SKJ(IDX1)
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key54"         TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key54"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *ÆÃÊÌ¿©²Ã»»¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î¼èÆÀ
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCHCN33SUB-HOSPNUM TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCHCN33SUB-PTID    TO  ACCT-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                           TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key55"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key55"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT   =  1)
      *
                   PERFORM    VARYING   IDX1   FROM   1   BY  1
                     UNTIL   (IDX1         >   31)
                       IF     ACCT-DAY(1 IDX1)   NOT =   ZERO
                       AND    ACCT-SRYKBN            =   "97"
                           MOVE    ZERO        TO  WRK-KEISAN
                           COMPUTE WRK-KEISAN  =   ACCT-ZAITEN *
                                                   ACCT-DAY(1 IDX1)
                           ADD     WRK-KEISAN  TO  
                                            LNK-ORCHCN33SUB-SKJ(IDX1)
                       END-IF
                   END-PERFORM
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
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-TOKU-SHOKU-HENSHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊóÊÔ½¸½èÍý¡ÊÊ¿À®£²£¸Ç¯£´·î¡Á¡Ë 
      *****************************************************************
       2101-SKJ-HEN201604-SEC            SECTION.
      *
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                       UNTIL    IDX1     >   31
      *
             IF   ( WRK-SRYKA-DAY(IDX1)          NOT = SPACE )
             AND  ( WRK-SKJ200604-NYUINJI(IDX1)  NOT =  ZERO )
             AND  ((WRK-NYUINKHN(IDX1)           NOT = SPACE)  OR
                   (LNK-NYUINDAY-KBN(IDX1)       =     "1"  ))
             AND  ((WRK-GAIHAKU-KBN(IDX1)        =     ZERO )  OR
                   (WRK-GAIHAKU-KBN(IDX1)       >=      4   ))
      *
               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *
                 WHEN     "197000110"
      *
                   PERFORM  VARYING  IDX8  FROM  1  BY  1
                             UNTIL   IDX8   >    3
                     EVALUATE  WRK-SKJ200604-AHY(IDX1 IDX8)
                       WHEN       1
                       WHEN       2
                         MOVE  WRK-SKJ200604-KINGAKU(1)
                                                   TO  WRK-KEISAN
                         IF LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8) NOT = ZERO
                           MOVE  LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8)
                                                   TO  WRK-KEISAN
                         END-IF
                         ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                         IF    WRK-SKJ200604-AHY(IDX1 IDX8) =  2
                           MOVE  WRK-SKJ200604-KINGAKU(3)
                                                   TO  WRK-KEISAN
                           ADD WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                         END-IF
                       WHEN       3
                         MOVE  WRK-SKJ200604-KINGAKU(5)
                                                   TO  WRK-KEISAN
                         IF LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8) NOT = ZERO
                           MOVE  LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8)
                                                   TO  WRK-KEISAN
                         END-IF
                         ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                     END-EVALUATE
                   END-PERFORM
                   IF   WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                         MOVE  WRK-SKJ200604-KINGAKU(4)
                                                   TO  WRK-KEISAN
                         ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                   END-IF
      *
                 WHEN     "197000710"
      *
                   PERFORM  VARYING  IDX8  FROM  1  BY  1
                             UNTIL   IDX8   >    3
                     EVALUATE  WRK-SKJ200604-AHY(IDX1 IDX8)
                       WHEN       1
                       WHEN       2
                         MOVE  WRK-SKJ200604-KINGAKU(2)
                                                   TO  WRK-KEISAN
                         IF LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8) NOT = ZERO
                           MOVE  LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8)
                                                   TO  WRK-KEISAN
                         END-IF
                         ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                       WHEN       3
                         MOVE  WRK-SKJ200604-KINGAKU(6)
                                                   TO  WRK-KEISAN
                         IF LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8) NOT = ZERO
                           MOVE  LNK-NYULIFE2-SKJ-MONEY(IDX1 IDX8)
                                                   TO  WRK-KEISAN
                         END-IF
                         ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
                     END-EVALUATE
                   END-PERFORM
      *
               END-EVALUATE
      *
             END-IF
      *
      *      ´Ä¶­ÎÅÍÜ·×»»
             IF   ( WRK-SRYKA-DAY(IDX1)          NOT = SPACE )
             AND  ((WRK-NYUINKHN(IDX1)           NOT = SPACE)  OR
                   (LNK-NYUINDAY-KBN(IDX1)       =     "1"  ))
             AND  ((WRK-GAIHAKU-KBN(IDX1)        =     ZERO )  OR
                   (WRK-GAIHAKU-KBN(IDX1)       >=      4   ))
      *
                 MOVE    ZERO              TO  WRK-KEISAN
                 IF LNK-NYULIFE2-KAN-MONEY(IDX1) NOT = ZERO
                   MOVE  LNK-NYULIFE2-KAN-MONEY(IDX1)
                                           TO  WRK-KEISAN
                 END-IF
                 ADD   WRK-KEISAN  TO  LNK-ORCHCN33SUB-SKJ(IDX1)
      *
             END-IF
      *
           END-PERFORM
      *
           .
       2101-SKJ-HEN201604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»ö¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       2102-SKJ-HEN200604-SEC            SECTION.
      *
           PERFORM    VARYING   IDX1   FROM   1   BY   1
                       UNTIL    IDX1     >   31
      *
             IF   ( WRK-SRYKA-DAY(IDX1)          NOT = SPACE )
             AND  ( WRK-SKJ200604-NYUINJI(IDX1)  NOT =  ZERO )
             AND  ((WRK-NYUINKHN(IDX1)           NOT = SPACE)  OR
                   (LNK-NYUINDAY-KBN(IDX1)       =     "1"  ))
             AND  ((WRK-GAIHAKU-KBN(IDX1)        =     ZERO )  OR
                   (WRK-GAIHAKU-KBN(IDX1)       >=      4   ))
      *
               EVALUATE   WRK-SYS5000-SHOKUCD(1 IDX1)
      *
                 WHEN     "197000110"
      *
                   COMPUTE WRK-KEISAN   =  WRK-SKJ200604-KINGAKU(1)
                                        *  WRK-SKJ200604-NYUINJI(IDX1)
                   IF   LNK-NYULIFE-SKJ-MONEY(IDX1) NOT = ZERO
                   AND  LNK-ORCHCN33SUB-SANTEIYM  >=  "200610"
                        COMPUTE  WRK-KEISAN  =
                                         LNK-NYULIFE-SKJ-MONEY(IDX1) *
                                         WRK-SKJ200604-NYUINJI(IDX1)
                   END-IF
                   ADD     WRK-KEISAN   TO LNK-ORCHCN33SUB-SKJ(IDX1)
      *
                   IF   WRK-SKJ200604-TOKUSYO(IDX1)  NOT = ZERO
                     COMPUTE WRK-KEISAN =  WRK-SKJ200604-KINGAKU(3)
                                        *  WRK-SKJ200604-TOKUSYO(IDX1)
                     ADD     WRK-KEISAN TO LNK-ORCHCN33SUB-SKJ(IDX1)
                   END-IF
      *
                   IF   WRK-SYS5000-SHOKUCD(3 IDX1)  NOT = SPACE
                     COMPUTE WRK-KEISAN =  WRK-SKJ200604-KINGAKU(4)
                     ADD     WRK-KEISAN TO LNK-ORCHCN33SUB-SKJ(IDX1)
                   END-IF
      *
                 WHEN     "197000710"
      *
                   COMPUTE WRK-KEISAN   =  WRK-SKJ200604-KINGAKU(2)
                                        *  WRK-SKJ200604-NYUINJI(IDX1)
                   IF   LNK-NYULIFE-SKJ-MONEY(IDX1) NOT = ZERO
                   AND  LNK-ORCHCN33SUB-SANTEIYM  >=  "200610"
                        COMPUTE  WRK-KEISAN  =
                                         LNK-NYULIFE-SKJ-MONEY(IDX1) *
                                         WRK-SKJ200604-NYUINJI(IDX1)
                   END-IF
                   ADD     WRK-KEISAN   TO LNK-ORCHCN33SUB-SKJ(IDX1)
      *
               END-EVALUATE
      *
             END-IF
      *
      *      ´Ä¶­ÎÅÍÜ·×»»
             IF   ( WRK-SRYKA-DAY(IDX1)          NOT = SPACE )
             AND  ((WRK-NYUINKHN(IDX1)           NOT = SPACE)  OR
                   (LNK-NYUINDAY-KBN(IDX1)       =     "1"  ))
             AND  ((WRK-GAIHAKU-KBN(IDX1)        =     ZERO )  OR
                   (WRK-GAIHAKU-KBN(IDX1)       >=      4   ))
      *
                 MOVE    ZERO    TO   WRK-KEISAN
                 IF   LNK-NYULIFE-KAN-MONEY(IDX1) NOT = ZERO
                 AND  LNK-ORCHCN33SUB-SANTEIYM  >=  "200610"
                      MOVE  LNK-NYULIFE-KAN-MONEY(IDX1)
                                        TO   WRK-KEISAN
                 END-IF
                 ADD     WRK-KEISAN   TO LNK-ORCHCN33SUB-SKJ(IDX1)
      *
             END-IF
      *
           END-PERFORM
      *
           .
       2102-SKJ-HEN200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÅÀ¿ôÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È½èÍý¡Ê¼ç²Ê½èÍýÍÑ¡Ë
      *****************************************************************
       210-NYUINTEN-SET-SEC            SECTION.
      *
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
               ADD    LNK-ORCHCN33SUB-ETCTEN(IDX1)  TO  WRK-SO-TEN(IDX1)
               IF     WRK-SO-TEN(IDX1)       NOT =   ZERO
                    MOVE   ZERO    TO   FLG-HKNCOMBI-MATCH
                    MOVE   ZERO    TO   FLG-NYUINKA-MATCH
                    PERFORM   VARYING  IDX-HKNCOMBI  FROM  1  BY  1
                      UNTIL  (IDX-HKNCOMBI         >  10)    OR
                             (FLG-HKNCOMBI-MATCH   =   1)    OR
                             (LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                 (IDX-HKNCOMBI)   =   ZERO)
                        IF  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                              (IDX-HKNCOMBI)    =   TBL-HKNCOMBI(IDX1)
                             MOVE   1    TO   FLG-HKNCOMBI-MATCH
                             PERFORM  VARYING  IDX-NYUINKA FROM 1 BY 1
                               UNTIL (IDX-NYUINKA   >   50)     OR
                                     (FLG-NYUINKA-MATCH  =  1)  OR
                                     (LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA) = SPACE)
                                 IF  LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA)
                                                   = TBL-NYUINKA(IDX1)
                                     MOVE   1    TO  FLG-NYUINKA-MATCH
                                     ADD  WRK-SO-TEN(IDX1)  TO
                                        LNK-ORCHCN33SUB-TENSU
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 END-IF
                             END-PERFORM
                             IF      FLG-NYUINKA-MATCH   =   ZERO
                                 MOVE  TBL-NYUINKA(IDX1)    TO
                                    LNK-ORCHCN33SUB-NYUINKA
                                      (IDX-HKNCOMBI  IDX-NYUINKA)
                                 MOVE  WRK-SO-TEN(IDX1)  TO
                                        LNK-ORCHCN33SUB-TENSU
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                             END-IF
                        END-IF
                    END-PERFORM
                    IF      FLG-HKNCOMBI-MATCH   =   ZERO
                        MOVE  TBL-HKNCOMBI(IDX1)   TO
                              LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                      (IDX-HKNCOMBI)
                        MOVE  TBL-NYUINKA(IDX1)    TO
                              LNK-ORCHCN33SUB-NYUINKA
                                 (IDX-HKNCOMBI  1)
                        MOVE  WRK-SO-TEN(IDX1)  TO
                                  LNK-ORCHCN33SUB-TENSU
                                           (IDX-HKNCOMBI 1)
                    END-IF
               END-IF
           END-PERFORM
      *    
      *    ·îËöÆü¤ÎÆ±ÆüºÆÆþ±¡È½Äê¤Ë¤Ä¤¤¤Æ¼èÆÀ
      *    ·îËöÆü¤ò¼èÆÀ
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-ORCHCN33SUB-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
           IF   LNK-NYUINDAY-DOUJITSU-KBN(WRK-GETSUMATSU-DD)  =   "1"
             PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *         IF     WRK-SO-TEN(IDX1)       NOT =   ZERO
                    MOVE   ZERO    TO   FLG-HKNCOMBI-MATCH
                    MOVE   ZERO    TO   FLG-NYUINKA-MATCH
                    PERFORM   VARYING  IDX-HKNCOMBI  FROM  1  BY  1
                      UNTIL  (IDX-HKNCOMBI         >  10)    OR
                             (FLG-HKNCOMBI-MATCH   =   1)    OR
                             (LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                 (IDX-HKNCOMBI)   =   ZERO)
                        IF  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                              (IDX-HKNCOMBI)    =   
                                   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                             MOVE   1    TO   FLG-HKNCOMBI-MATCH
                             PERFORM  VARYING  IDX-NYUINKA FROM 1 BY 1
                               UNTIL (IDX-NYUINKA   >   50)     OR
                                     (FLG-NYUINKA-MATCH  =  1)  OR
                                     (LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA) = SPACE)
                                 IF  LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA)
                                                   = TBL-NYUINKA(IDX1)
                                     MOVE   1    TO  FLG-NYUINKA-MATCH
                                     MOVE  IDX1  TO
                                             LNK-ORCHCN33SUB-EDDAY
                                             (IDX-HKNCOMBI IDX-NYUINKA)
                                     ADD  1  TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 END-IF
                             END-PERFORM
                             IF      FLG-NYUINKA-MATCH   =   ZERO
                                 MOVE  TBL-NYUINKA(IDX1)    TO
                                    LNK-ORCHCN33SUB-NYUINKA
                                      (IDX-HKNCOMBI  IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-STDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-EDDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 ADD   1     TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                             END-IF
                        END-IF
                    END-PERFORM
                    IF      FLG-HKNCOMBI-MATCH   =   ZERO
                      IF LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1) NOT = ZERO
                        MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)   TO
                              LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                      (IDX-HKNCOMBI)
                        MOVE  TBL-NYUINKA(IDX1)    TO
                              LNK-ORCHCN33SUB-NYUINKA
                                 (IDX-HKNCOMBI  1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-STDAY
                                                 (IDX-HKNCOMBI 1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-EDDAY
                                                 (IDX-HKNCOMBI 1)
                        ADD   1        TO   LNK-ORCHCN33SUB-DAY
                                                 (IDX-HKNCOMBI 1)
                      END-IF
                    END-IF
      *         END-IF
             END-PERFORM
           END-IF
      *
      *     DISPLAY   "ÊÖµÑ¾ðÊóÉ½¼¨"
      *     DISPLAY   "ÊÝ¸±ÁÈ¹ç¤»£± = " 
      *                LNK-ORCHCN33SUB-HENKYAKU-HKNCOM (1)
      *     DISPLAY   "Æþ±¡²Ê£±¡¡£± = "
      *                LNK-ORCHCN33SUB-NYUINKA (1 1)
      *     DISPLAY   "ÅÀ¿ô£±¡¡£± = "
      *                LNK-ORCHCN33SUB-TENSU (1 1)
      *     DISPLAY   "Æþ±¡²Ê£±¡¡£² = "
      *                LNK-ORCHCN33SUB-NYUINKA (1 2)
      *     DISPLAY   "ÅÀ¿ô£±¡¡£² = "
      *                LNK-ORCHCN33SUB-TENSU (1 2)
      *     DISPLAY   "ÊÝ¸±ÁÈ¹ç¤»£² = " 
      *                LNK-ORCHCN33SUB-HENKYAKU-HKNCOM (2)
      *     DISPLAY   "Æþ±¡²Ê£²¡¡£± = "
      *                LNK-ORCHCN33SUB-NYUINKA (2 1)
      *     DISPLAY   "ÅÀ¿ô£²¡¡£± = "
      *                LNK-ORCHCN33SUB-TENSU (2 1)
      *     DISPLAY   "Æþ±¡²Ê£²¡¡£² = "
      *                LNK-ORCHCN33SUB-NYUINKA (2 2)
      *     DISPLAY   "ÅÀ¿ô£²¡¡£² = "
      *                LNK-ORCHCN33SUB-TENSU (2 2)
           .
       210-NYUINTEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡Æü¿ôÊÖµÑ¥¨¥ê¥¢¤Ø¤Î¥»¥Ã¥È½èÍý¡Ê¼ç²Ê½èÍýÍÑ¡Ë
      *****************************************************************
       210-NYUINDAY-SET-SEC            SECTION.
      *
           PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *         IF     WRK-SO-TEN(IDX1)       NOT =   ZERO
                    MOVE   ZERO    TO   FLG-HKNCOMBI-MATCH
                    MOVE   ZERO    TO   FLG-NYUINKA-MATCH
                    PERFORM   VARYING  IDX-HKNCOMBI  FROM  1  BY  1
                      UNTIL  (IDX-HKNCOMBI         >  10)    OR
                             (FLG-HKNCOMBI-MATCH   =   1)    OR
                             (LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                 (IDX-HKNCOMBI)   =   ZERO)
                        IF  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                              (IDX-HKNCOMBI)    =   TBL-HKNCOMBI(IDX1)
                             MOVE   1    TO   FLG-HKNCOMBI-MATCH
                             PERFORM  VARYING  IDX-NYUINKA FROM 1 BY 1
                               UNTIL (IDX-NYUINKA   >   50)     OR
                                     (FLG-NYUINKA-MATCH  =  1)  OR
                                     (LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA) = SPACE)
                                 IF  LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA)
                                                   = TBL-NYUINKA(IDX1)
                                     MOVE   1    TO  FLG-NYUINKA-MATCH
                                     MOVE  IDX1  TO
                                             LNK-ORCHCN33SUB-EDDAY
                                             (IDX-HKNCOMBI IDX-NYUINKA)
                                     ADD  1  TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 END-IF
                             END-PERFORM
                             IF      FLG-NYUINKA-MATCH   =   ZERO
                                 MOVE  TBL-NYUINKA(IDX1)    TO
                                    LNK-ORCHCN33SUB-NYUINKA
                                      (IDX-HKNCOMBI  IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-STDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-EDDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 ADD   1     TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                             END-IF
                        END-IF
                    END-PERFORM
                    IF      FLG-HKNCOMBI-MATCH   =   ZERO
                      IF  TBL-HKNCOMBI(IDX1)   NOT =   ZERO
                        MOVE  TBL-HKNCOMBI(IDX1)   TO
                              LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                      (IDX-HKNCOMBI)
                        MOVE  TBL-NYUINKA(IDX1)    TO
                              LNK-ORCHCN33SUB-NYUINKA
                                 (IDX-HKNCOMBI  1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-STDAY
                                                 (IDX-HKNCOMBI 1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-EDDAY
                                                 (IDX-HKNCOMBI 1)
                        ADD   1        TO   LNK-ORCHCN33SUB-DAY
                                                 (IDX-HKNCOMBI 1)
                      END-IF
                    END-IF
      *         END-IF
           END-PERFORM
      *    
      *    ·îËöÆü¤ÎÆ±ÆüºÆÆþ±¡È½Äê¤Ë¤Ä¤¤¤Æ¼èÆÀ
      *    ·îËöÆü¤ò¼èÆÀ
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-ORCHCN33SUB-SANTEIYM
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-GETSUMATSU-YMD
           END-IF
      *
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-ORCHCN33SUB-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-ORCHCN33SUB-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-ORCHCN33SUB-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
           IF   LNK-NYUINDAY-DOUJITSU-KBN(WRK-GETSUMATSU-DD)  =   "1"
             PERFORM    VARYING   IDX1   FROM  1  BY  1
                      UNTIL     (IDX1   >     31)
      *         IF     WRK-SO-TEN(IDX1)       NOT =   ZERO
                    MOVE   ZERO    TO   FLG-HKNCOMBI-MATCH
                    MOVE   ZERO    TO   FLG-NYUINKA-MATCH
                    PERFORM   VARYING  IDX-HKNCOMBI  FROM  1  BY  1
                      UNTIL  (IDX-HKNCOMBI         >  10)    OR
                             (FLG-HKNCOMBI-MATCH   =   1)    OR
                             (LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                 (IDX-HKNCOMBI)   =   ZERO)
                        IF  LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                              (IDX-HKNCOMBI)    =   
                                   LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)
                             MOVE   1    TO   FLG-HKNCOMBI-MATCH
                             PERFORM  VARYING  IDX-NYUINKA FROM 1 BY 1
                               UNTIL (IDX-NYUINKA   >   50)     OR
                                     (FLG-NYUINKA-MATCH  =  1)  OR
                                     (LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA) = SPACE)
                                 IF  LNK-ORCHCN33SUB-NYUINKA
                                     (IDX-HKNCOMBI IDX-NYUINKA)
                                                   = TBL-NYUINKA(IDX1)
                                     MOVE   1    TO  FLG-NYUINKA-MATCH
                                     MOVE  IDX1  TO
                                             LNK-ORCHCN33SUB-EDDAY
                                             (IDX-HKNCOMBI IDX-NYUINKA)
                                     ADD  1  TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 END-IF
                             END-PERFORM
                             IF      FLG-NYUINKA-MATCH   =   ZERO
                                 MOVE  TBL-NYUINKA(IDX1)    TO
                                    LNK-ORCHCN33SUB-NYUINKA
                                      (IDX-HKNCOMBI  IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-STDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 MOVE  IDX1  TO  LNK-ORCHCN33SUB-EDDAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                                 ADD   1     TO  LNK-ORCHCN33SUB-DAY
                                           (IDX-HKNCOMBI IDX-NYUINKA)
                             END-IF
                        END-IF
                    END-PERFORM
                    IF      FLG-HKNCOMBI-MATCH   =   ZERO
                      IF LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1) NOT = ZERO
                        MOVE  LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX1)   TO
                              LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                      (IDX-HKNCOMBI)
                        MOVE  TBL-NYUINKA(IDX1)    TO
                              LNK-ORCHCN33SUB-NYUINKA
                                 (IDX-HKNCOMBI  1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-STDAY
                                                 (IDX-HKNCOMBI 1)
                        MOVE  IDX1     TO   LNK-ORCHCN33SUB-EDDAY
                                                 (IDX-HKNCOMBI 1)
                        ADD   1        TO   LNK-ORCHCN33SUB-DAY
                                                 (IDX-HKNCOMBI 1)
                      END-IF
                    END-IF
      *         END-IF
             END-PERFORM
           END-IF
      *
      *     DISPLAY   "ÊÖµÑ¾ðÊóÉ½¼¨"
      *     DISPLAY   "ÊÝ¸±ÁÈ¹ç¤»£± = " 
      *                LNK-ORCHCN33SUB-HENKYAKU-HKNCOM (1)
      *     DISPLAY   "Æþ±¡²Ê£±¡¡£± = "
      *                LNK-ORCHCN33SUB-NYUINKA (1 1)
      *     DISPLAY   "³«»ÏÆü£±¡¡£± = "
      *                LNK-ORCHCN33SUB-STDAY (1 1)
      *     DISPLAY   "½ªÎ»Æü£±¡¡£± = "
      *                LNK-ORCHCN33SUB-EDDAY (1 1)
      *     DISPLAY   "Æü¿ô£±¡¡£± = "
      *                LNK-ORCHCN33SUB-DAY (1 1)
      *     DISPLAY   "Æþ±¡²Ê£±¡¡£² = "
      *                LNK-ORCHCN33SUB-NYUINKA (1 2)
      *     DISPLAY   "³«»ÏÆü£±¡¡£² = "
      *                LNK-ORCHCN33SUB-STDAY (1 2)
      *     DISPLAY   "½ªÎ»Æü£±¡¡£² = "
      *                LNK-ORCHCN33SUB-EDDAY (1 2)
      *     DISPLAY   "Æü¿ô£±¡¡£² = "
      *                LNK-ORCHCN33SUB-DAY (1 2)
      *     DISPLAY   "ÊÝ¸±ÁÈ¹ç¤»£² = " 
      *                LNK-ORCHCN33SUB-HENKYAKU-HKNCOM (2)
      *     DISPLAY   "Æþ±¡²Ê£²¡¡£± = "
      *                LNK-ORCHCN33SUB-NYUINKA (2 1)
      *     DISPLAY   "³«»ÏÆü£²¡¡£± = "
      *                LNK-ORCHCN33SUB-STDAY (2 1)
      *     DISPLAY   "½ªÎ»Æü£²¡¡£± = "
      *                LNK-ORCHCN33SUB-EDDAY (2 1)
      *     DISPLAY   "Æü¿ô£²¡¡£± = "
      *                LNK-ORCHCN33SUB-DAY (2 1)
      *     DISPLAY   "Æþ±¡²Ê£²¡¡£² = "
      *                LNK-ORCHCN33SUB-NYUINKA (2 2)
      *     DISPLAY   "³«»ÏÆü£²¡¡£² = "
      *                LNK-ORCHCN33SUB-STDAY (2 2)
      *     DISPLAY   "½ªÎ»Æü£²¡¡£² = "
      *                LNK-ORCHCN33SUB-EDDAY (2 2)
      *     DISPLAY   "Æü¿ô£²¡¡£² = "
      *                LNK-ORCHCN33SUB-DAY (2 2)
           .
       210-NYUINDAY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "tbl_tensu"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
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
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
