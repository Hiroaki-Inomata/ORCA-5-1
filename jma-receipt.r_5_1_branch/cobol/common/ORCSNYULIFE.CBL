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
       PROGRAM-ID.                 ORCSNYULIFE.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
      *                      ¡ÊÊ¿À®£±£¸Ç¯£±£°·î²þÀµÂÐ±þ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/09/12    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-ÂÀÅÄ    07/05/10  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *              NACL-¾®Æ¦Âô  07/07/12  ´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤ò¥µ¥Ö
      *                                     ¥ë¡¼¥Á¥ó¤ËÊÑ¹¹
      *  04.01.00    NACL-¾®Æ¦Âô  07/12/17  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤ÎÆþ±¡
      *                                     ÍâÆü¿©»ö»»ÄêÂÐ±þ
      *Ê¿À®£²£°Ç¯£´·î²þÄêÂÐ±þ
      *  04.02.00    NACL-¾®Æ¦Âô  08/03/05  ¸å´ü¹âÎð¼Ô¤ÎÈ½ÄêÊÑ¹¹
      *                                     À¸³èÎÅÍÜ¤ÎÆþ±¡ÎÁÈ½ÄêÄÉ²Ã
      *                                       Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
      *                                       ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±
      *                                       ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²
      *  04.02.00    NACL-¾®Æ¦Âô  08/04/11  ¸å´ü¹âÎð¼Ô¤ÎÈ½Äê¤ò¥µ¥Ö¥ë¡¼¥Á¥ó»ÈÍÑ
      *                                     ¤ËÊÑ¹¹
      *  04.02.00    NACL-¾®Æ¦Âô  08/05/02  ÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Ë£Á£Ä£ÌÆþÎÏ¤«¤é
      *                                     °åÎÅÅÙ¤Î¹â¤¤´µ¼Ô¤òÈ½Äê
      *Ê¿À®£²£·Ç¯£±·îÆñÉÂÂÐ±þ
      *  04.07.00    NACL-¾®Æ¦Âô  15/06/02  ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁµÚ¤Ó
      *                                     ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ¤Î
      *                                     »»Äê¤ÇÆñÉÂ¸øÈñ¤Î»»Äê»þ¤Ë
      *                                     °åÎÅÅÙ¤Î¹â¤¤´µ¼Ô¤È¤¹¤ëÈ½ÄêÄÉ²Ã
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-TSYRRK                  PIC 9(01).
           03  FLG-TNKRRK                  PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-SEI                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-NANBYO                  PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  TSY90-IDX                   PIC 9(02).
           03  IDX-RJN                     PIC 9(02).
           03  IDX-SEI                     PIC 9(04).
           03  IDX-SEI2                    PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
      *    À¸³èÎÅÍÜ¤Ë´Ø¤¹¤ëÆþ±¡¿ÇÎÅ¥³¡¼¥É
       01  CONSTANT-AREA.
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²
           03   CONS-RYOYO2A               PIC X(09) VALUE "190123710".
           03   CONS-RYOYO2B               PIC X(09) VALUE "190123810".
           03   CONS-RYOYO2C               PIC X(09) VALUE "190123910".
           03   CONS-RYOYO2D               PIC X(09) VALUE "190124010".
           03   CONS-RYOYO2E               PIC X(09) VALUE "190124110".
           03   CONS-RYOYO2TOKU            PIC X(09) VALUE "190124210".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£²
           03   CONS-SRYOYO2A              PIC X(09) VALUE "190124310".
           03   CONS-SRYOYO2B              PIC X(09) VALUE "190124410".
           03   CONS-SRYOYO2C              PIC X(09) VALUE "190124510".
           03   CONS-SRYOYO2D              PIC X(09) VALUE "190124610".
           03   CONS-SRYOYO2E              PIC X(09) VALUE "190124710".
           03   CONS-SRYOYO2TOKU           PIC X(09) VALUE "190124810".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ
           03   CONS-KAIFUKU               PIC X(09) VALUE "190124910".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÊ¿À®£²£²Ç¯°Ê¹ß¤Ï´ð½àÌ¤Å¬¹ç¡Ë
           03   CONS-KAIFUKU1              PIC X(09) VALUE "190129510".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÊ¿À®£²£²Ç¯°Ê¹ß¤Ï´ð½àÌ¤Å¬¹ç¡Ë
           03   CONS-KAIFUKU2              PIC X(09) VALUE "190129710".
      *    ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
           03   CONS-ROUKAN1               PIC X(09) VALUE "190125010".
           03   CONS-ROUKAN2               PIC X(09) VALUE "190125110".
           03   CONS-ROUKAN3               PIC X(09) VALUE "190125210".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²
           03   CONS-TANKI2                PIC X(09) VALUE "190125310".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
           03   CONS-TANKI3                PIC X(09) VALUE "190130510".
      *
      *    Ê¿À®£²£²Ç¯ÅÙ²þÄêÄÉ²ÃÊ¬
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¤«¤é£Ç¤ÎÀ¸³èÎÅÍÜ¡Ë
           03   CONS-H22-RYOYO1F           PIC X(09) VALUE "190131710".
           03   CONS-H22-RYOYO1G           PIC X(09) VALUE "190131910".
           03   CONS-H22-RYOYO1H           PIC X(09) VALUE "190132110".
           03   CONS-H22-RYOYO1I           PIC X(09) VALUE "190132310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡Ê£Á¤«¤é£É¤ÎÀ¸³èÎÅÍÜ¡Ë
           03   CONS-H22-RYOYO2A           PIC X(09) VALUE "190132510".
           03   CONS-H22-RYOYO2B           PIC X(09) VALUE "190132710".
           03   CONS-H22-RYOYO2C           PIC X(09) VALUE "190132910".
           03   CONS-H22-RYOYO2D           PIC X(09) VALUE "190133110".
           03   CONS-H22-RYOYO2E           PIC X(09) VALUE "190133310".
           03   CONS-H22-RYOYO2F           PIC X(09) VALUE "190133510".
           03   CONS-H22-RYOYO2G           PIC X(09) VALUE "190133710".
           03   CONS-H22-RYOYO2H           PIC X(09) VALUE "190133910".
           03   CONS-H22-RYOYO2I           PIC X(09) VALUE "190134110".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   CONS-H22-KAIFUKU1          PIC X(09) VALUE "190141710".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   CONS-H22-KAIFUKU2          PIC X(09) VALUE "190141910".
      *
      *    Ê¿À®£²£´Ç¯ÅÙ²þÄêÀ¸³èÎÅÍÜÈ½ÄêÍÑ
      *  ¡Ê°åÎÅÅÙ¤Î¹â¤¤À¸³èÎÅÍÜ¤ò¾å°Ì¤Ë¤Þ¤È¤á¤Æ¤¤¤ë¤Î¤Ç¡¢¥Ç¡¼¥¿¤ÎÊÂ¤Ó¤ËÃí°Õ¤¹¤ë¤³¤È¡Ë
      *  ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¤ÏÆÃÊÌ¤ÊÈ½Äê¤¬¤¢¤ë¤³¤È¤«¤é¥Ç¡¼¥¿¤ÎºÇ¸å¤È¤¹¤ë¡Ë
       01  TBL-H24-SEIKATSU-AREA.
      *  °åÎÅÅÙ¤Î¹â¤¤À¸³èÎÅÍÜ
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190150210".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190719610".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190141710".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190719810".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190141910".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190720010".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190125310".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190130510".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124310".
      *10  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711510".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124410".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711710".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124510".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711910".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155410".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á¡ÊÀ¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155510".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155810".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â¡ÊÀ¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155910".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156210".
      *20  ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã¡ÊÀ¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701610".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124110".
      *30  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190131710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133110".
      *40  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703610".
      *   ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703810".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157410".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157510".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157810".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157910".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158210".
      *50  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158310".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158610".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158710".
      *   ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159010".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159110".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159410".
      *56  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159510".
      *
      *  °åÎÅÅÙ¤Î¹â¤¯¤Ê¤¤À¸³èÎÅÍÜ
      *57  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124610".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190712110".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124710".
      *60  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190712310".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156610".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä¡ÊÀ¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156710".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å¡ÊÀ¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157110".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190131910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132310".
      *70  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702610".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190704010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190704210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190134110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190704410".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159810".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159910".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160210".
      *80  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160310".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160610".
      *82  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡¦À¸³èÎÅÍÜ¡¦±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160710".
      *
      *83  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124810".
      *84  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124210".
      *
       01  TBL-H24-SEIKATSU-R     REDEFINES  TBL-H24-SEIKATSU-AREA.
           03   TBL-H24-SEISRYCD           PIC X(09)  OCCURS  84.
      *
      *    Ê¿À®£²£¶Ç¯ÅÙ²þÄêÀ¸³èÎÅÍÜÈ½ÄêÍÑ
      *  ¡Ê°åÎÅÅÙ¤Î¹â¤¤À¸³èÎÅÍÜ¤ò¾å°Ì¤Ë¤Þ¤È¤á¤Æ¤¤¤ë¤Î¤Ç¡¢¥Ç¡¼¥¿¤ÎÊÂ¤Ó¤ËÃí°Õ¤¹¤ë¤³¤È¡Ë
      *  ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¤ÏÆÃÊÌ¤ÊÈ½Äê¤¬¤¢¤ë¤³¤È¤«¤é¥Ç¡¼¥¿¤ÎºÇ¸å¤È¤¹¤ë¡Ë
       01  TBL-H26-SEIKATSU-AREA.
      *  °åÎÅÅÙ¤Î¹â¤¤À¸³èÎÅÍÜ
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190150210".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190141710".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190141910".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190125310".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124310".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124410".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124510".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155410".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155810".
      *10  ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190123910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190131710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132910".
      *20  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166610".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167010".
      *28  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167210".
      *
      *  °åÎÅÅÙ¤Î¹â¤¯¤Ê¤¤À¸³èÎÅÍÜ
      *29  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124610".
      *30  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124710".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156610".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190131910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190132310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190134110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167410".
      *40  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167610".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167810".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190177710".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190177910".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190178110".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190178310".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190178510".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190178710".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190178910".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190179110".
      *50  Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190179310".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190179510".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190179710".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190179910".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190180110".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190180310".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190180510".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190180710".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190180910".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190181110".
      *60  Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190181310".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190181510".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190181710".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190181910".
      *    Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190182110".
      *65  Ã»¼ê£³
           03   FILLER                     PIC X(09) VALUE "190182850".
      *66  ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190175410".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190175610".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190175810".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190176010".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176210".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176410".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176610".
      *73  ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176810".
      *
      *74  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124810".
      *75  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   FILLER                     PIC X(09) VALUE "190124210".
      *
       01  TBL-H26-SEIKATSU-R     REDEFINES  TBL-H26-SEIKATSU-AREA.
           03   TBL-H26-SEISRYCD           PIC X(09)  OCCURS  75.
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-YMD.
               05  WRK-YYMM                PIC X(06).
               05  WRK-DD                  PIC 9(02).
           03  WRK-RJN-TBL.
               05  WRK-RJN                 PIC 9(01)   OCCURS   31.
      *    µßµÞÈÂÁ÷¿ÇÎÅÎÁ¤ò»»Äê¤·¡¢¤«¤Ä¡¢Æþ±¡ÎÁÌ¤»»Äê¤ÎÆü
           03  WRK-KYUKYU-DAY              PIC 9(02).
           03  WRK-KYUKYU-YMD              PIC X(08).
           03  WRK-KYUKYU-NYUINCD          PIC X(09).
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÂàÈò
           03  WRK-NYUIN-TABLE.
               05  WRK-NYUIN-TBL           OCCURS  31.
                   07  WRK-HKNCOMBI-DAY    PIC 9(03).
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
      *    ´µ¼Ô½êÆÀ¾ðÊóÊÔ½¸¥µ¥Ö
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    °åÎÅ¶èÊ¬¼èÆÀ¥µ¥Ö
           COPY    "CPORCSS008.INC".
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    Æþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSNYULIFE.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYULIFE-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-NYULIFE-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-NYULIFE-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-NYULIFE-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-NYULIFE-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     PERFORM    VARYING IDX   FROM   1   BY  1
      *                UNTIL IDX   >   31
      *       DISPLAY "IDX = " IDX
      *       DISPLAY "LNK-NYULIFE-KBN = " LNK-NYULIFE-KBN(IDX)
      *       DISPLAY "LNK-NYULIFE-FTNKBN2 = " LNK-NYULIFE-FTNKBN2(IDX)
      *     END-PERFORM
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
           IF     (LNK-NYULIFE-SANTEIYM    =   SPACE)  OR
                  (LNK-NYULIFE-SANTEIYM    >=  "201604")
               MOVE    1               TO  LNK-NYULIFE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                       PTINF-REC
           MOVE    LNK-NYULIFE-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-NYULIFE-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF            NOT =   ZERO
               MOVE    2                TO  LNK-NYULIFE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-NYULIFE-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYULIFE-PTID     TO  NACCT-PTID
           MOVE    LNK-NYULIFE-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                  TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT        NOT =   ZERO
               MOVE    3                TO  LNK-NYULIFE-RC
           ELSE
               MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT       =   ZERO
      *             ÆñÉÂ¸øÈñÈ½ÄêÍÑ¤ËÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ¤òÂàÈò
                    PERFORM    VARYING   IDX1   FROM   1   BY   1
                               UNTIL     IDX1   >      31
                        MOVE   NACCT-DAY (IDX1) TO 
                                            WRK-HKNCOMBI-DAY(IDX1)
                    END-PERFORM
               ELSE
                    MOVE    3           TO  LNK-NYULIFE-RC
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
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
      *    °åÎÅ¶èÊ¬¼èÆÀ¥µ¥Ö¤Î¸Æ½Ð¤·¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Î°åÎÅ¶èÊ¬È½ÄêÍÑ¡Ë
           INITIALIZE                      SS008-AREA
           MOVE    LNK-NYULIFE-HOSPNUM     TO  SPA-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  SPA-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  SS008-SRYYM
           CALL   "ORCSS008"       USING   SS008-AREA
                                           SPA-AREA
      *
      *    µßµÞÈÂÁ÷¿ÇÎÅÎÁ¤ò»»Äê¤·¡¢¤«¤Ä¡¢Æþ±¡ÎÁÌ¤»»Äê¤ÎÆü¤òÆÃÄê¤·¡¢
      *    ³ºÅöÆü¤Î¿©»ö¤¬À¸³èÎÅÍÜ¤Ë³ºÅö¤¹¤ë¤«È½Äê¡Ê¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ¡Ë
           MOVE    ZERO                    TO  WRK-KYUKYU-DAY
           MOVE    SPACE                   TO  WRK-KYUKYU-YMD
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-NYULIFE-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC         =   ZERO
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL   IDX1   >   31
                         IF   (LNK-NYUINDAY-KBN(IDX1)   =   "1")   AND
                              (LNK-NYUINDAY-KYUKYU-KBN(IDX1)   =   "1")
                             MOVE    IDX1  TO  WRK-KYUKYU-DAY
                             MOVE    LNK-NYULIFE-SANTEIYM
                                           TO  WRK-KYUKYU-YMD(1:6)
                             MOVE    WRK-KYUKYU-DAY
                                           TO  WRK-KYUKYU-YMD(7:2)
                         END-IF
                   END-PERFORM
           END-IF
           IF   WRK-KYUKYU-DAY             NOT =   ZERO
      *      ´µ¼ÔÆþ±¡ÍúÎò¤«¤é»»ÄêÆþ±¡ÎÁ¤Î¼èÆÀ
             INITIALIZE                          PTNYUINRRK-REC
             MOVE   LNK-NYULIFE-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
             MOVE   LNK-NYULIFE-PTID       TO  PTNYUINRRK-PTID
             MOVE   LNK-NYULIFE-SANTEIYM   TO  PTNYUINRRK-TENNYUYMD(1:6)
             MOVE   "99"                   TO  PTNYUINRRK-TENNYUYMD(7:2)
             MOVE   LNK-NYULIFE-SANTEIYM   TO  PTNYUINRRK-TENSTUYMD(1:6)
             MOVE   "01"                   TO  PTNYUINRRK-TENSTUYMD(7:2)
             MOVE   "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE   "key44"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK        =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               PERFORM  UNTIL  
                  ( FLG-PTNYUINRRK         =   1  )
      *
                   IF  (PTNYUINRRK-TENNYUYMD  <=  WRK-KYUKYU-YMD) AND
                       (WRK-KYUKYU-YMD        <=  PTNYUINRRK-TENSTUYMD)
      *               ÆÃÄêÆþ±¡ÎÁ¥³¡¼¥É¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ
                      INITIALIZE             ORCSTOKNYUIN-AREA
                      MOVE  PTNYUINRRK-TOKUTEI-KBN
                                           TO  LNK-TOKNYUIN-KBN
                      MOVE  PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  LNK-TOKNYUIN-NYUIN
                      MOVE  PTNYUINRRK-BTUNUM
                                           TO  LNK-TOKNYUIN-BTUNUM
                      MOVE  PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                       TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
                      MOVE  WRK-KYUKYU-YMD(1:6)
                                           TO  LNK-TOKNYUIN-SANTEIYM
                      CALL  "ORCSTOKNYUIN"   USING    ORCSTOKNYUIN-AREA
                                                      SPA-AREA
                      IF    LNK-TOKNYUIN-RC   =    ZERO
                         IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                              MOVE   LNK-TOKNYUIN-SRYCD
                                           TO  WRK-KYUKYU-NYUINCD
                         ELSE
                              MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                           TO  WRK-KYUKYU-NYUINCD
                         END-IF
                   END-IF
      *
                   END-IF
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key44"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
      *
               END-PERFORM
             END-IF
      *
             MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
             MOVE    "key44"               TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
           END-IF
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñ¤Î³ºÅöÆü¤È·Ú¸º¶èÊ¬£²¡ÊÆþ±¡ÎÁÈ½Äê¡Ë¤ò¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYULIFE-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  NACCT-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *
      *            ¡ÊÊ¿À®£²£²Ç¯£³·î°ÊÁ°¡Ë
                     IF   LNK-NYULIFE-SANTEIYM <=  "201003"
      *                À¸³èÎÅÍÜ¤Ë³ºÅö¤¹¤ëÆþ±¡ÎÁ¤òÈ½Äê¤·¡¢
      *                ÊÖµÑÃÍ¤ÎÂÐ¾Ý¶èÊ¬¤òÀßÄê¤¹¤ë
                       PERFORM    210-NYUINACT-GET-SEC
                       IF   NSRY-SRYCD(1)   =  CONS-RYOYO2A     OR
                                               CONS-RYOYO2B     OR
                                               CONS-RYOYO2C     OR
                                               CONS-RYOYO2D     OR
                                               CONS-RYOYO2E     OR
                                               CONS-RYOYO2TOKU  OR
                                               CONS-SRYOYO2A    OR
                                               CONS-SRYOYO2B    OR
                                               CONS-SRYOYO2C    OR
                                               CONS-SRYOYO2D    OR
                                               CONS-SRYOYO2E    OR
                                               CONS-SRYOYO2TOKU OR
                                               CONS-KAIFUKU     OR
                                               CONS-KAIFUKU1    OR
                                               CONS-KAIFUKU2    OR
                                               CONS-ROUKAN1     OR
                                               CONS-ROUKAN2     OR
                                               CONS-ROUKAN3     OR
                                               CONS-TANKI2      OR
                                               CONS-TANKI3
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆþ±¡ÎÁÈ½ÃÇ¡Ë
                               IF  NSRY-SRYCD(1)  =  CONS-RYOYO2A  OR
                                                     CONS-RYOYO2B  OR
                                                     CONS-RYOYO2C  OR
                                                     CONS-SRYOYO2A OR
                                                     CONS-SRYOYO2B OR
                                                     CONS-SRYOYO2C OR
                                                     CONS-KAIFUKU  OR
                                                     CONS-KAIFUKU1 OR
                                                     CONS-KAIFUKU2 OR
                                                     CONS-ROUKAN1  OR
                                                     CONS-TANKI2   OR
                                                     CONS-TANKI3
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Î
      *                                                    £Á£Ä£ÌÆþÎÏÈ½ÃÇ¡Ë
                               IF (NSRY-SRYCD(1)  =  CONS-RYOYO2TOKU OR
                                                     CONS-SRYOYO2TOKU)
                                                 AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
      *            ¡ÊÊ¿À®£²£²Ç¯£´·î¤«¤éÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¡Ë
                     IF  (LNK-NYULIFE-SANTEIYM >=  "201004")    AND
                         (LNK-NYULIFE-SANTEIYM <=  "201203")
      *                À¸³èÎÅÍÜ¤Ë³ºÅö¤¹¤ëÆþ±¡ÎÁ¤òÈ½Äê¤·¡¢
      *                ÊÖµÑÃÍ¤ÎÂÐ¾Ý¶èÊ¬¤òÀßÄê¤¹¤ë
                       PERFORM    210-NYUINACT-GET-SEC
                       IF   NSRY-SRYCD(1)   =  CONS-RYOYO2A     OR
                                               CONS-RYOYO2B     OR
                                               CONS-RYOYO2C     OR
                                               CONS-RYOYO2D     OR
                                               CONS-RYOYO2E     OR
                                               CONS-RYOYO2TOKU  OR
                                               CONS-SRYOYO2A    OR
                                               CONS-SRYOYO2B    OR
                                               CONS-SRYOYO2C    OR
                                               CONS-SRYOYO2D    OR
                                               CONS-SRYOYO2E    OR
                                               CONS-SRYOYO2TOKU OR
                                               CONS-KAIFUKU1    OR
                                               CONS-KAIFUKU2    OR
                                               CONS-TANKI2      OR
                                               CONS-TANKI3      OR
                                               CONS-H22-RYOYO1F OR
                                               CONS-H22-RYOYO1G OR
                                               CONS-H22-RYOYO1H OR
                                               CONS-H22-RYOYO1I OR
                                               CONS-H22-RYOYO2A OR
                                               CONS-H22-RYOYO2B OR
                                               CONS-H22-RYOYO2C OR
                                               CONS-H22-RYOYO2D OR
                                               CONS-H22-RYOYO2E OR
                                               CONS-H22-RYOYO2F OR
                                               CONS-H22-RYOYO2G OR
                                               CONS-H22-RYOYO2H OR
                                               CONS-H22-RYOYO2I OR
                                               CONS-H22-KAIFUKU1 OR
                                               CONS-H22-KAIFUKU2
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆþ±¡ÎÁÈ½ÃÇ¡Ë
                               IF  NSRY-SRYCD(1)  =  CONS-RYOYO2A  OR
                                                     CONS-RYOYO2B  OR
                                                     CONS-RYOYO2C  OR
                                                     CONS-RYOYO2D  OR
                                                     CONS-RYOYO2E  OR
                                                     CONS-H22-RYOYO1F OR
                                                     CONS-SRYOYO2A OR
                                                     CONS-SRYOYO2B OR
                                                     CONS-SRYOYO2C OR
                                                     CONS-KAIFUKU1 OR
                                                     CONS-KAIFUKU2 OR
                                                     CONS-TANKI2   OR
                                                     CONS-TANKI3   OR
                                                     CONS-H22-RYOYO2A OR
                                                     CONS-H22-RYOYO2B OR
                                                     CONS-H22-RYOYO2C OR
                                                     CONS-H22-RYOYO2D OR
                                                     CONS-H22-RYOYO2E OR
                                                     CONS-H22-RYOYO2F OR
                                                    CONS-H22-KAIFUKU1 OR
                                                    CONS-H22-KAIFUKU2
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Î
      *                                                    £Á£Ä£ÌÆþÎÏÈ½ÃÇ¡Ë
                               IF (NSRY-SRYCD(1)  =  CONS-RYOYO2TOKU OR
                                                     CONS-SRYOYO2TOKU)
                                                 AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
      *            ¡ÊÊ¿À®£²£´Ç¯£´·î¤«¤éÊ¿À®£²£¶Ç¯£³·î¤Þ¤Ç¡Ë
                     IF  (LNK-NYULIFE-SANTEIYM >=  "201204")  AND
                         (LNK-NYULIFE-SANTEIYM <=  "201403")
      *                À¸³èÎÅÍÜ¤Ë³ºÅö¤¹¤ëÆþ±¡ÎÁ¤òÈ½Äê¤·¡¢
      *                ÊÖµÑÃÍ¤ÎÂÐ¾Ý¶èÊ¬¤òÀßÄê¤¹¤ë
                       PERFORM    210-NYUINACT-GET-SEC
                       MOVE       ZERO        TO   FLG-SEI
                                                   IDX-SEI2
                       PERFORM    VARYING   IDX-SEI  FROM 1  BY  1
                         UNTIL   (IDX-SEI   >   84)    OR
                                 (FLG-SEI   =    1)
                             IF  NSRY-SRYCD(1) = TBL-H24-SEISRYCD
                                                        (IDX-SEI)
                                 MOVE   1        TO   FLG-SEI
                                 MOVE   IDX-SEI  TO   IDX-SEI2
                             END-IF
                       END-PERFORM
                       IF   FLG-SEI    =    1
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆþ±¡ÎÁÈ½ÃÇ¡Ë
                               IF  (IDX-SEI2   >=   1 )   AND
                                   (IDX-SEI2   <=   56)
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Î
      *                                                    £Á£Ä£ÌÆþÎÏÈ½ÃÇ¡Ë
                               IF (IDX-SEI2   =   83  OR  84)    AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
      *            ¡ÊÊ¿À®£²£¶Ç¯£´·î¤«¤é¡Ë
                     IF  LNK-NYULIFE-SANTEIYM >=  "201404"
      *                À¸³èÎÅÍÜ¤Ë³ºÅö¤¹¤ëÆþ±¡ÎÁ¤òÈ½Äê¤·¡¢
      *                ÊÖµÑÃÍ¤ÎÂÐ¾Ý¶èÊ¬¤òÀßÄê¤¹¤ë
                       PERFORM    210-NYUINACT-GET-SEC
                       MOVE       ZERO        TO   FLG-SEI
                                                   IDX-SEI2
                       PERFORM    VARYING   IDX-SEI  FROM 1  BY  1
                         UNTIL   (IDX-SEI   >   75)    OR
                                 (FLG-SEI   =    1)
                             IF  NSRY-SRYCD(1) = TBL-H26-SEISRYCD
                                                        (IDX-SEI)
                                 MOVE   1        TO   FLG-SEI
                                 MOVE   IDX-SEI  TO   IDX-SEI2
                             END-IF
                       END-PERFORM
                       IF   FLG-SEI    =    1
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆþ±¡ÎÁÈ½ÃÇ¡Ë
                               IF  (IDX-SEI2   >=   1 )   AND
                                   (IDX-SEI2   <=   28)
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        ÆñÉÂ¸øÈñ¤ÎÈ½ÄêÄÉ²Ã¡Ê°Ê²¼Æþ±¡ÎÁ¤Î¾ì¹ç¤Î¤ß¡Ë
      *                        ¡¦ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ
      *                        ¡¦ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ
                               IF  (IDX-SEI2   >=   66)   AND
                                   (IDX-SEI2   <=   73)
      *                          ÆñÉÂ¸øÈñ¤ò»ÈÍÑ¤µ¤ì¤Æ¤¤¤ë¤«È½Äê
                                 MOVE   ZERO       TO   FLG-NANBYO
                                 IF  WRK-HKNCOMBI-DAY(IDX1) NOT =  ZERO
                                     INITIALIZE            HKNCOMBI-REC
                                     MOVE   LNK-NYULIFE-HOSPNUM
                                                   TO  COMB-HOSPNUM
                                     MOVE   LNK-NYULIFE-PTID 
                                                   TO  COMB-PTID
                                     MOVE   WRK-HKNCOMBI-DAY(IDX1)
                                                   TO  COMB-HKNCOMBINUM
                                     MOVE   "tbl_hkncombi"
                                                   TO  MCP-TABLE
                                     MOVE    "key" TO  MCP-PATHNAME
                                     PERFORM 910-HKNCOMBI-SELECT-SEC
                                     IF   FLG-HKNCOMBI      =   ZERO
                                       MOVE  "tbl_hkncombi"
                                                   TO  MCP-TABLE
                                       MOVE  "key"  TO  MCP-PATHNAME
                                       PERFORM   910-HKNCOMBI-READ-SEC
                                       IF  FLG-HKNCOMBI         =  ZERO
                                         IF  (COMB-KOHHKNNUM(1) = "054")
                                          OR (COMB-KOHHKNNUM(2) = "054")
                                          OR (COMB-KOHHKNNUM(3) = "054")
                                          OR (COMB-KOHHKNNUM(4) = "054")
                                             MOVE  1    TO   FLG-NANBYO
                                         END-IF
                                       END-IF
                                     END-IF
      *                              ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
                                     MOVE    "tbl_hkncombi"
                                                        TO  MCP-TABLE
                                     MOVE    "key"      TO  MCP-PATHNAME
                                     PERFORM   900-CLOSE-SEC
                                 END-IF
      *
                                 IF   FLG-NANBYO  =   1
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                                 END-IF
                               END-IF
      *                        ÉÂ¾õ¤ÎÄøÅÙ¡¢¼£ÎÅ¤ÎÆâÍÆ¤ò¤·¤ó¼à¤·¤ÆÉ¸½à
      *                        ÉéÃ´³Û¤ò·Ú¸º¤¹¤ë´µ¼Ô¤ÎÈ½Äê¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ»»Äê»þ¤Î
      *                                                    £Á£Ä£ÌÆþÎÏÈ½ÃÇ¡Ë
                               IF (IDX-SEI2   =   74  OR  75)    AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                   MOVE    "key17"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡Ë¤ÎÆþ±¡ÎÁÌ¤»»ÄêÆü
      *    ¤Ë¤Ä¤¤¤ÆÀ¸³èÎÅÍÜ¤Î³ºÅöÆü¤È·Ú¸º¶èÊ¬£²¤Î¾ðÊó¤ò¼èÆÀ¡Ê¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ¡Ë
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-NYULIFE-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC         =   ZERO
               IF    LNK-NYUINDAY-TANKI2-TBL   NOT =    SPACE
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL   IDX1   >   31
                         IF    LNK-NYUINDAY-TANKI2-KBN(IDX1)   =   "2"
                               MOVE   "1"   TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE   "1"   TO
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               IF   LNK-NYUINDAY-TANKI3-KBN(IDX1) = "2"
                                    MOVE   SPACE  TO
                                              LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                               MOVE    1    TO   FLG-OK
                         END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *¡Ý¡Ý¡Ý¡Ý°åÎÅµ¡´Ø¿©»ö¾ðÊó¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý
      *°åÎÅµ¡´Ø¿©»ö¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-NYULIFE-HOSPNUM TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYULIFE-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        ¿©»ö¾ðÊó¤ÎÂàÈò
               IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                   PERFORM    VARYING   IDX1  FROM  1  BY  1
                          UNTIL     IDX1   >    31
                       IF    LNK-NYULIFE-KBN(IDX1)   =   "1"
      *                   Æþ±¡»þ¿©»öÎÅÍÜÈñ
                           IF   SYS-5000-SHOKUJIRYOYOCD  = "197000110"
                                MOVE   "1"     TO  
                                          LNK-NYULIFE-SKJKBN(IDX1)
                           ELSE
                                MOVE   "2"     TO  
                                          LNK-NYULIFE-SKJKBN(IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *¡Ý¡Ý¡Ý¡Ý°åÎÅµ¡´Ø¿©»ö¾ðÊó¼èÆÀ¼èÆÀ¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý¡Ý
      *
      *
           IF    FLG-OK   =    1
      *
      *        Ï·¿ÍÅ¬ÍÑ¤Î¥Á¥§¥Ã¥¯
               IF      LNK-NYULIFE-SANTEIYM <=  "200803"
      *          ¡ÊÊ¿À®£²£°Ç¯£³·î°ÊÁ°¤ÏÏ·¿ÍÊÝ·ò¤ÎÈ½Äê¥Á¥§¥Ã¥¯¡Ë
                   MOVE    ZERO                 TO  FLG-RJN 
                   INITIALIZE                       PTKOHINF-REC
                   MOVE    LNK-NYULIFE-HOSPNUM  TO  PTKOH-HOSPNUM
                   MOVE    LNK-NYULIFE-PTID     TO  PTKOH-PTID
                   MOVE    "027"                TO  PTKOH-KOHNUM
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM 910-PTKOHINF-SELECT-SEC
                   IF      FLG-PTKOHINF         =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                                 MOVE   1           TO   FLG-RJN
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
                   END-IF
      *            ´µ¼Ô¸øÈñ¾ðÊó¥¯¥í¡¼¥º
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               ELSE
      *          ¡ÊÊ¿À®£²£°Ç¯£´·î°Ê¹ß¤Ï¥µ¥Ö¥ë¡¼¥Á¥ó¤Ë¤è¤ê¥Á¥§¥Ã¥¯¡Ë
                   PERFORM  VARYING    IDX-RJN    FROM   1   BY  1
                     UNTIL  IDX-RJN    >   31
                       INITIALIZE                   ORCSAGECHKAREA
                       MOVE    LNK-NYULIFE-HOSPNUM  TO  AGECHK-HOSPNUM
                       MOVE    LNK-NYULIFE-PTID     TO  AGECHK-PTID
                       MOVE    LNK-NYULIFE-SANTEIYM TO  WRK-YYMM
                       MOVE    IDX-RJN              TO  WRK-DD
                       MOVE    WRK-YMD              TO  AGECHK-SRYYMD
                       CALL   "ORCSAGECHK"          USING
                                                    ORCSAGECHKAREA
                                                    SPA-AREA
                       IF      AGECHK-ROUJIN2    =   1
                           MOVE   1              TO  WRK-RJN(IDX-RJN)
                       END-IF
                   END-PERFORM
               END-IF
      *
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
               INITIALIZE                          ORCSSYOTOKUAREA
               MOVE    LNK-NYULIFE-HOSPNUM     TO  LNK-SYOTOKU-HOSPNUM
               MOVE    LNK-NYULIFE-PTID        TO  LNK-SYOTOKU-PTID
               MOVE    LNK-NYULIFE-SANTEIYM    TO  LNK-SYOTOKU-SRYYM
               MOVE    "1"                     TO  LNK-SYOTOKU-SKJCHKFLG
               CALL   "ORCSSYOTOKU"    USING    ORCSSYOTOKUAREA
                                                SPA-AREA
               IF    LNK-SYOTOKU-RC    =        ZERO
                    PERFORM   VARYING   IDX1   FROM   1  BY  1
                              UNTIL     IDX1   >    31
                        EVALUATE   LNK-SYOTOKU-SKJKBN(IDX1)
                            WHEN  "1"
                                MOVE  "3"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "2"
                                MOVE  "1"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "3"
                                MOVE  "2"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "4"
                                MOVE  "4"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                        END-EVALUATE
                    END-PERFORM
               ELSE
                    DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
               END-IF
      *¡Ý¡Ý¡Ý¡Ý¥µ¥Ö¥ë¡¼¥Á¥ó¤Ç¤Î´µ¼Ô½êÆÀ¾ðÊó¤Î¼èÆÀ¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý¡Ý¡Ý
      *
      *
      *¡Ý¡Ý¡Ý¡Ý¿©»öÎÅÍÜ¡¢´Ä¶­ÎÅÍÜ¥³¡¼¥É¡¢Ã±²ÁÀßÄê¤³¤³¤«¤é¡Ý¡Ý¡Ý¡Ý¡Ý
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-SKJKBN(IDX1)
                        WHEN  "1"
                            MOVE  "197001310"   TO
                                            LNK-NYULIFE-SKJ-SRYCD(IDX1)
                            MOVE   554          TO
                                            LNK-NYULIFE-SKJ-MONEY(IDX1)
                            MOVE  "197001410"   TO
                                            LNK-NYULIFE-KAN-SRYCD(IDX1)
                            MOVE   398          TO
                                            LNK-NYULIFE-KAN-MONEY(IDX1)
                        WHEN  "2"
                            MOVE  "197001710"   TO
                                            LNK-NYULIFE-SKJ-SRYCD(IDX1)
                            MOVE   420          TO
                                            LNK-NYULIFE-SKJ-MONEY(IDX1)
                            MOVE  "197001810"   TO
                                            LNK-NYULIFE-KAN-SRYCD(IDX1)
                            MOVE   398          TO
                                            LNK-NYULIFE-KAN-MONEY(IDX1)
                    END-EVALUATE
                END-IF
             END-PERFORM
      *¡Ý¡Ý¡Ý¡Ý¿©»öÎÅÍÜ¡¢´Ä¶­ÎÅÍÜ¥³¡¼¥É¡¢Ã±²ÁÀßÄê¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý¡Ý¡Ý
      *
      *¡Ý¡Ý¡Ý¡Ý¿©»öÉéÃ´¡¢´Ä¶­ÎÅÍÜÉéÃ´¥³¡¼¥É¡¢Ã±²ÁÀßÄê¤³¤³¤«¤é¡Ý¡Ý¡Ý
      *      À¸³èÎÅÍÜ¿©»öÉ¸½àÉéÃ´³ÛÀßÄê¡Ê°ìÈÌ¡Ë
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-SKJKBN(IDX1)
                        WHEN  "1"
                            MOVE  "197002010"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                            MOVE   460          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                        WHEN  "2"
                            MOVE  "197002110"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                            MOVE   420          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                    END-EVALUATE
                END-IF
             END-PERFORM
      *
      *      À¸³èÎÅÍÜ¿©»öÉ¸½àÉéÃ´³ÛÀßÄê¡Ê·Ú¸º¤¢¤ê¤Î´µ¼Ô¡Ë
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-FTNKBN2(IDX1)
                        WHEN  SPACE
      *                   ½êÆÀ¤Î¾õ¶·¤Ë¤è¤ë·Ú¸º
                            EVALUATE   LNK-NYULIFE-FTNKBN1(IDX1)
                                WHEN   "1"
                                WHEN   "2"
      *                           ¡ÊÄã£²¡Ë
                                    MOVE  "197002210"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   210          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "3"
      *                           ¡ÊÄã£±¡Ë
                                    IF  (FLG-RJN         =  0)   AND
                                        (WRK-RJN(IDX1)   =  0)                                        
                                       MOVE  "197002310"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       MOVE   130          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                    ELSE
                                       MOVE  "197002410"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       MOVE   130          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                    END-IF
                                WHEN   "4"
      *                           ¡ÊÄã£±¡ÝÏ·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¡Ë
                                    MOVE  "197002510"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   100          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                            END-EVALUATE
                        WHEN  "1"
      *                   ÉÂ¾õ¤ÎÄøÅÙ¤Ë¤è¤ë·Ú¸º
                            EVALUATE   LNK-NYULIFE-FTNKBN1(IDX1)
                                WHEN   SPACE
      *                           ¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡Ê°ìÈÌ¡Ë¡Ë
                                    MOVE  "197002710"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   260          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "1"
      *                           ¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¡£¹£°Æü°Ê²¼¡Ë¡Ë
                                    MOVE  "197002810"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   210          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "2"
      *                           ¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£²¡¡£¹£°ÆüÄ¶¡Ë¡Ë
                                    MOVE  "197002910"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   160          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "3"
                                WHEN   "4"
      *                           ¡Ê°åÎÅ¶èÊ¬£²¡¢£³¡ÊÄã£±¡Ë¡Ë
                                    MOVE  "197003010"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   100          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                            END-EVALUATE
                    END-EVALUATE
                END-IF
             END-PERFORM
      *
      *      À¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÉ¸½àÉéÃ´³ÛÀßÄê
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    IF  LNK-NYULIFE-FTNKBN2(IDX1)   =   SPACE
                        MOVE  "197001910"       TO
                                        LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                        MOVE  320       TO
                                        LNK-NYULIFE-KANFTN-MONEY(IDX1)
                    END-IF
      *             Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¤Ï´Ä¶­ÎÅÍÜ´µ¼ÔÉéÃ´Ìµ¤·¤ËÃÖ´¹¤¨
                    IF  LNK-NYULIFE-FTNKBN1(IDX1)   =   "4"
                        MOVE  SPACE  TO   LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                        MOVE  ZERO   TO   LNK-NYULIFE-KANFTN-MONEY(IDX1)
                    END-IF
                END-IF
             END-PERFORM
      *
      *¡Ý¡Ý¡Ý¡Ý¿©»öÉéÃ´¡¢´Ä¶­ÎÅÍÜÉéÃ´¥³¡¼¥É¡¢Ã±²ÁÀßÄê¤³¤³¤Þ¤Ç¡Ý¡Ý¡Ý
      *
           END-IF
      *
           IF   FLG-OK   =   ZERO
               MOVE   4      TO   LNK-NYULIFE-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY  1
      *               UNTIL  IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "LNK-NYULIFE-FTNKBN1(IDX1) = " 
      *                   LNK-NYULIFE-FTNKBN1(IDX1)
      *          DISPLAY "LNK-NYULIFE-FTNKBN2(IDX1) = " 
      *                   LNK-NYULIFE-FTNKBN2(IDX1)
      *          DISPLAY  "LNK-NYULIFE-SKJ-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-SKJ-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-SKJ-MONEY(IDX1) = "
      *                   LNK-NYULIFE-SKJ-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-SKJFTN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-SKJFTN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-SKJFTN-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-KAN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-KAN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-KAN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-KAN-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-KANFTN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-KANFTN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-KANFTN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-KANFTN-MONEY(IDX1)
      *     END-PERFORM
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
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿¡¼¸¡º÷
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
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿¡¼ÆÉ¹þ
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
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
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
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Þ¥¹¥¿¡¼ÆÉ¹þ
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
               IF      WRK-KYUKYU-DAY     NOT =   ZERO
                   IF   WRK-KYUKYU-NYUINCD   = NACCT-SRYCDTOTAL(6:9)
                        MOVE   1          TO  NACCT-DAY(WRK-KYUKYU-DAY)  
                   END-IF
               END-IF
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
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-SEC-EXT.
           EXIT.
