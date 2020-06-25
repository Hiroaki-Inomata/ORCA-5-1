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
       PROGRAM-ID.             ORCBC010.
      *****************************************************************
      *  ¡ú½¤Àµ»þ¤Ë¤ÏÉ¬¤º¥×¥í¥°¥é¥à¼±ÊÌÈÖ¹æ¤ò¥ê¥»¥Ã¥È¤¹¤ë¤³¤È
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥Ç¡¼¥¿¼ý½¸
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥Ç¡¼¥¿¼ý½¸ÍÑ´ðËÜ¾ðÊóºîÀ®
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/08/17    NACL¡Ý°ËÆ£    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.03.01    NACL-Æ£¸¶    06/10/27  ¥ì¥»¥×¥È¤ÇÀ¸³èÊÝ¸î¤ÎÀÁµá¤¬¤Ê
      *                                     ¤¤¾ì¹çµ­ºÜ¤ÎÂÐ¾Ý¤È¤·¤Ê¤¤
      *  04.01.01    NACL-¾®Æ¦Âô  07/11/16  ¸øÌ³ºÒ³²¡¢¸ø³²¤Î¼ïÊÌÈ½ÄêÄÉ²Ã
      *  04.02.01    NACL-°ËÆ£    08/ 4/22  ²þÄêÂÐ±þ
      *  04.02.02    NACL-ÌçÏÆ    08/08/07  Àº¿ÀÄÌ±¡¤ÎÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *
      *  04.03.01    NACL-ÌçÏÆ    08/09/02  ÀÁµáÅÀÈ½ÄêÄÉ²Ã
      *  04.03.02    NACL-°ËÆ£    08/10/31  ¸øÈñÃ±ÆÈ¤Î¼¼ÎÁº¹³Û
      *                                     ËÜ¿Í¡¦²ÈÂ²¶èÊ¬¤Î¸«Ä¾¤·
      *                                     £°·ï¤Î¾ì¹çÀµ¾ï½ªÎ»¤È¤¹¤ë
      *  04.03.03    NACL-ÌçÏÆ    08/12/17  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *
      *  04.04.01    NACL-Æ£¸¶    09/02/26  Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤Ë£·£µºÐÅþ
      *                                     Ã£»þ¤Ë³¤³°¤Ë½»½ê¤òÍ­¤¹¤ë¾ì¹ç
      *                                     ¤ÎÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/04/13  Ê¿À®£²£±Ç¯£µ·î²þÀµÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    10/03/23  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *  04.04.04    NACL-Æ£¸¶    10/08/23  ¸å´ü¹âÎðÈó³ºÅöÂÐ±þ
      *                                     ¡Ê¹âÎð¼õµë¼ÔÉ¼¤¬¤Ê¤¤¾ì¹ç¡Ë 
      *
      *  04.05.01    NACL-°ËÆ£    10/01/28  ¾®¿ôÅÀ£µ·åÂÐ±þCOPY¶ç¤Î¤ß
      *              NACL-¾®Æ¦Âô  10/02/16  ¼£¸³ÂÐ±þ
      *  04.05.02    NACL-Æ£¸¶    10/03/12  Ê¿À®£²£²Ç¯£´·î²þÀµÂÐ±þ
      *                                     Ê¿À®£²£²Ç¯£´·îÊ¬°Ê¹ß¤ÎÀ¯ÉÜ´É
      *                                     ¾¸¤ò¥¨¥é¡¼¤È¤¹¤ë
      *  04.05.03    NACL-¾®Æ¦Âô  10/05/27  ºÞÈÖ¹æ¤Î½ÅÊ£¤ËÂÐ½è
      *                                     Æþ±¡ÎÁ¤ÎºÞÈÖ¹æ¤ÏºÇÂçºÞÈÖ¹æ¡Ü£±
      *                                     ¤«¤é¿¶¤ê½Ð¤¹
      *  04.05.04    NACL-ÂÀÅÄ    10/06/04  ³°Çñ¤ÎºÞ¤ËÆþ±¡ÎÁ¤òÊÔ½¸¤¹¤ë
      *                                     ÁªÄêÆþ±¡Ãæ¤Î³°ÇñºÞ¤òÆÉ¤ßÈô¤Ð¤¹
      *  04.05.05    NACL-ÌçÏÆ    12/04/02  Ê¿À®£²£´Ç¯£´·î²þÀµ
      *                                     ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÂÐ±þ
      *                                     ³°Çñ¶èÊ¬£¶¡¥£·¤ÎÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    12/09/18  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¸¡¤£¹¤ÎÂÐ±þ¡Ë
      *
      *  04.06.01    NACL-Æ£¸¶    11/02/01  ¼çÊÝ¸±¤¬¤Ê¤¤¾ì¹ç¡¢»ùÆ¸ÊÝ¸î(53)
      *                                     ¤Ï¸øÈñÃ±ÆÈ¤Î¥ì¥»¥×¥È¤È¤¹¤ë
      *  04.06.02    NACL-ÌçÏÆ    13/02/18  Ï«ºÒ¥ì¥»ÅÅÂÐ±þ
      *                                     ¡Ê¿·¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÂÐ±þ¡Ë
      *  04.06.03    NACL-ÌçÏÆ    13/02/27  Ê¿À®£²£µÇ¯£´·î²þÀµÂÐ±þ
      *  04.06.04    NACL-ÌçÏÆ    13/09/25  Ï«ºÒ¥Þ¥¹¥¿ÄÉ²ÃÂÐ±þ
      *                                     ¡ÊÅÅÏÃºÆ¿ÇÎÁ¡¡Åù¡Ë
      *
      *  04.07.01    NACL-ÌçÏÆ    14/03/28  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.02    NACL-Æ£¸¶    14/03/30  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.03    NACL-Æ£¸¶    14/08/18  £·£µºÐ°Ê¾å¤Î³¤³°µï½»¼Ô£²³äÂÐ±þ
      *  04.07.04    NACL-ÌçÏÆ    14/09/01  Ê¿À®£²£¶Ç¯£±£°·î²þÀµÂÐ±þ
      *  04.07.05    NACL-ÌçÏÆ    14/12/15  Ê¿À®£²£·Ç¯£±·î²þÀµÂÐ±þ
      *                                     ¡Ê½éºÆ¿ÇÎÁ¡¦ÄãÂÅ·ëÎ¨ÂÐ±þ¡Ë
      *  04.07.06    NACL-ÌçÏÆ    15/03/16  Ê¿À®£²£·Ç¯£´·î²þÀµÂÐ±þ
      *
      *  04.08.01    NACL-ÌçÏÆ    16/03/28  Ê¿À®£²£¸Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.02    NACL-¾®Æ¦Âô  16/08/25  Ê¿À®£²£¸Ç¯£±£°·î»ý»²ÌôÂÐ±þ
      *  04.08.03    NACL-¾®Æ¦Âô  17/08/17  À¸³èÎÅÍÜÉ¸½àÉéÃ´³Û¥³¡¼¥ÉÄÉ²ÃÂÐ±þ
      *  04.08.04    NACL-ÌçÏÆ    18/03/27  Ê¿À®£³£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.08.05    NACL-ÌçÏÆ    18/06/11  Äþ¸ºÅÀ¿ô½¸·×¤Ë´Ø¤¹¤ë½¤Àµ
      *  05.00.01    NACL-ÌçÏÆ    20/03/30  ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ¼ý½¸¥Ç¡¼¥¿¡ÊÉÂ±¡´ðËÜ¾ðÊó¡Ë
           SELECT  COLDAT91-FILE   ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT91-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
           SELECT  COLDAT911-FILE  ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT911-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
      *    ¼ý½¸¥Ç¡¼¥¿¡Ê´µ¼Ô´ðËÜ¾ðÊó¡Ë
           SELECT  COLDAT01-FILE   ASSIGN  COLDAT01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT01-KEY
                                   FILE    STATUS  IS  STS-COLDAT01.
      *
           SELECT  COLDAT011-FILE  ASSIGN  COLDAT01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT011-KEY
                                   FILE    STATUS  IS  STS-COLDAT01.
      *
      *    ¼ý½¸¥Ç¡¼¥¿¡Ê¿ÇÎÅ¾ðÊó¡Ë
           SELECT  COLDAT02-FILE   ASSIGN  COLDAT02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT02-KEY
                                   FILE    STATUS  IS  STS-COLDAT02.
      *
           SELECT  COLDAT021-FILE  ASSIGN  COLDAT02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT021-KEY
                                   FILE    STATUS  IS  STS-COLDAT02.
      *
      *    ¼ý½¸¥Ç¡¼¥¿¡ÊÉÂÌ¾¾ðÊó¡Ë
           SELECT  COLDAT03-FILE   ASSIGN  COLDAT03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT03-KEY
                                   FILE    STATUS  IS  STS-COLDAT03.
      *
           SELECT  COLDAT031-FILE  ASSIGN  COLDAT03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT031-KEY
                                   FILE    STATUS  IS  STS-COLDAT03.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  COLDATERR-FILE  ASSIGN  WRK-PARA-ERRFILENM
                                   FILE    STATUS  IS  STS-COLDATERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ÉÂ±¡´ðËÜ¾ðÊó¥Ç¡¼¥¿
       FD  COLDAT91-FILE.
       01  COLDAT91-REC.
           COPY    "CPCDF901.INC".
      *
       FD  COLDAT911-FILE.
       01  COLDAT911-REC.
           COPY    "CPCDF901.INC"  REPLACING  //COLDAT91//
                                   BY         //COLDAT911//.
      *
      *    ´µ¼Ô´ðËÜ¾ðÊó¥Ç¡¼¥¿
       FD  COLDAT01-FILE.
       01  COLDAT01-REC.
           COPY    "CPCDF001.INC".
      *
       FD  COLDAT011-FILE.
       01  COLDAT011-REC.
           COPY    "CPCDF001.INC"  REPLACING  //COLDAT01//
                                   BY         //COLDAT011//.
      *
      *    ¿ÇÎÅ¾ðÊó¥Ç¡¼¥¿
       FD  COLDAT02-FILE.
       01  COLDAT02-REC.
           COPY    "CPCDF002.INC".
      *
       FD  COLDAT021-FILE.
       01  COLDAT021-REC.
           COPY    "CPCDF002.INC"  REPLACING  //COLDAT02//
                                   BY         //COLDAT021//.
      *
      *    ÉÂÌ¾¾ðÊó¥Ç¡¼¥¿
       FD  COLDAT03-FILE.
       01  COLDAT03-REC.
           COPY    "CPCDF003.INC".
      *
       FD  COLDAT031-FILE.
       01  COLDAT031-REC.
           COPY    "CPCDF003.INC"  REPLACING  //COLDAT03//
                                   BY         //COLDAT031//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  COLDATERR-FILE.
       01  COLDATERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20071029".
      *01  PRGSKBNO                PIC X(08)  VALUE "20080428".
      *01  PRGSKBNO                PIC X(08)  VALUE "20080826".
      *01  PRGSKBNO                PIC X(08)  VALUE "20081031".
      *01  PRGSKBNO                PIC X(08)  VALUE "20100128".
      *01  PRGSKBNO                PIC X(08)  VALUE "20100527".
      *01  PRGSKBNO                PIC X(08)  VALUE "20100604".
       01  PRGSKBNO                PIC X(08)  VALUE "20160401".
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT91//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT01//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT02//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT03//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-COLDAT91                    PIC X(02).
           03  STS-COLDAT01                    PIC X(02).
           03  STS-COLDAT02                    PIC X(02).
           03  STS-COLDAT03                    PIC X(02).
           03  STS-COLDATERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01).
           03  FLG-HKNNUM                      PIC 9(01).
           03  FLG-COLDAT01                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-PTCOM                       PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
      *
           03  FLG-KOH                         PIC 9(01).
           03  FLG-CONTKBN                     PIC 9(01).
           03  FLG-SET                         PIC 9(01).
           03  FLG-LASTHKNID                   PIC 9(01).
           03  FLG-SHOSAISIN                   PIC 9(01).
           03  FLG-SHOSAISIN-GEN               PIC 9(01).
           03  FLG-CHK                         PIC 9(01).
           03  FLG-EDIT                        PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-COLDAT91                    PIC 9(06).
           03  CNT-COLDAT01                    PIC 9(06).
           03  CNT-COLDAT02                    PIC 9(06).
           03  CNT-COLDAT03                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03).
           03  IDX1                            PIC 9(03).
           03  IDX2                            PIC 9(03).
           03  IDX3                            PIC 9(03).
           03  IDX4                            PIC 9(03).
           03  IDX5                            PIC 9(03).
           03  IDX6                            PIC 9(03).
           03  IDY                             PIC 9(03).
           03  IDZ                             PIC 9(03).
           03  IDZ1                            PIC 9(03).
           03  IDZ2                            PIC 9(03).
           03  IDZ3                            PIC 9(03).
           03  IDXZ                            PIC 9(03).
           03  IDXX                            PIC 9(03).
           03  JN-IDX                          PIC 9(03).
           03  HC-IDX                          PIC 9(03).
           03  KH-IDX                          PIC 9(03).
           03  ACCT-IDX                        PIC 9(02).
           03  IDX-DAY                         PIC 9(02).
      **** 03  SYSKAN-IDX                      PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-HKNCOMBI              PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-HKNCOMBI              PIC 9(04).
      *
      *    °ì»þÎÎ°è
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(16).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-PREFNUM            PIC X(02).
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-ERRFILENM          PIC X(100).
      *
           03  WRK-HOSPID                  PIC X(24).
      *
           03  WRK-COLDATERR               PIC X(200).
      *
           03  WRK-PATH                    PIC X(64).
      *
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      *
           03  WRK-DATAKBN             PIC X(01).
      *
           03  WRK-CHGNUM-P.
               05  WRK-CHGNUM          PIC 9(01).
      *
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ZAINUM              PIC 9(08).
      *
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
      *
      *    ½ýÉÂÌ¾ÊÔ½¸
           03  WRK-BYOMEI          PIC X(150).
           03  WRK-BYOMEIW         PIC X(150).
      *    ½ýÉÂÌ¾ÊÔ½¸¥Õ¥é¥°¡Êµ¿¤¤¡Ë
           03  WRK-HENFLG1         PIC X(01).
      *    ½ýÉÂÌ¾ÊÔ½¸¥Õ¥é¥°¡ÊµÞÀ­¡Ë
           03  WRK-HENFLG2         PIC X(01).
      *    ÉÂÌ¾Ê¸»ú¿ô         
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-YM              PIC 9(06).
      *
           03  WRK-MSG-NYUGAI      PIC X(04).
      *
      *    ¸å´ü¹âÎð¼ÔÈó³ºÅö¥Õ¥é¥°
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *    ¥ì¥»¥×¥È¼ïÊÌÈ½ÄêÍÑÎÎ°è
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
      *    ¿ÇÎÅ²ó¿ô¥Æ¡¼¥Ö¥ë¡ÊÆþ±¡²ñ·×ÍÑ  £±¡§´ðËÜ  £²¡§ÁÈ¹ç¤»
      *                                  £³¡§¿©»ö  £´¡§³°Çñ¡Ë
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  4.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
      *    ¥ï¡¼¥¯ÊÝ¸±ÁÈ¹ç¤»¡ÊÆþ±¡²ñ·×ÍÑ¡Ë
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).
      *
       01  WRK-COLDAT01-TABLE.
           02  WRK-COLDAT01-OCC    OCCURS  100.
           COPY    "CPCDF001.INC"  REPLACING  //COLDAT01//
                                   BY         //WRK-COLDAT01//.
       01  WRK-COLDAT01-TABLE-MAX      PIC 9(03)   VALUE    100.
      *
      *    ¥ï¡¼¥¯´µ¼ÔÉÂÌ¾
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO//
                                   BY         //WRK-PTBYO//.
      *
      *    ¥¹¥ÑÎÎ°è ¥ï¡¼¥¯
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
           COPY    "MCPAREA".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           COPY    "CPTTNS.INC".
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
           03  WRK-CHK-PAYKBN                  PIC X(02).
           03  WRK-CHK-TEKSTYMD                PIC X(08).
           03  WRK-CHK-TEKEDYMD                PIC X(08).
       01  TABLE-HKNNUM-AREA.
         02  TBL-HKNNUM-REC                    OCCURS  2000.
           COPY    "CPHKNNUM.INC"  REPLACING   //HKN-//
                                   BY          //TBL-HKN-//.
       01  TBL-HKNNUM-CONST-MAX                PIC 9(05)
                                               VALUE   2000.
      *
      *    ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÍÑÎÎ°è
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN                          OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *    ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÍÑÂàÈòÎÎ°è
       01  TBL-JNISSU-S.
           03  TBL-JNISSU-HKN-S                        OCCURS   5.
               05  TBL-JNISSU-DAY-S                    PIC X(01)
                                                       OCCURS   31.
      *    ¼ÂÆü¿ô¸º¥«¥¦¥ó¥ÈÍÑÎÎ°è
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-HKN-GEN                      OCCURS   5.
               05  TBL-JNISSU-DAY-GEN                  PIC X(01)
                                                       OCCURS   31.
      *    ¹ç·×ÅÀ¿ôÎÎ°è
       01  TBL-TOTALTENSU-INF.
           03  TBL-TOTALTENSU                          PIC S9(08)
                                                       OCCURS   5.
      *    ¼£¸³¸ºÅÀÍÑÅÀ¿ôÎÎ°è
       01  WRK-TOTALTENSU-INF.
           03  WRK-TOTALTENSU                          PIC 9(08)
                                                       OCCURS   5.
      *    ¿©»öÎÅÍÜ²ó¿ô¥«¥¦¥ó¥ÈÍÑÎÎ°è
       01  TBL-SKJNISSU-INF.
           03  TBL-SKJNISSU-OCC                        OCCURS   5.
               05  TBL-SKJNISSU                        PIC 9(02)
                                                       OCCURS   31.
      *    ¿©»öÎÅÍÜÈñÎÎ°è
       01  TBL-SKJRYOYOHI-INF.
           03  TBL-SKJRYOYOHI                          PIC 9(08)
                                                       OCCURS   5.
      *    ¿©»öÎÅÍÜÈñÉ¸½àÉéÃ´³ÛÎÎ°è
       01  TBL-SKJFTN-INF.
           03  TBL-SKJFTN                              PIC 9(08)
                                                       OCCURS   5.
      *    ¥ì¥»¥×¥È¼ïÊÌÊÖµÑ¥µ¥Ö
           COPY    "CPORCSSYUBETUGET.INC".
      *
       01  TBL-NYUINKA-G.
           03  TBL-NYUINKA                 PIC X(02)   OCCURS  31.
           03  TBL-STIDX                   PIC 9(02).
           03  TBL-EDIDX                   PIC 9(02).
      *
      *    É¸½àÉéÃ´³Û¥Æ¡¼¥Ö¥ë
           03  SKJFTN-DAY-AREA.
               05  SKJFTN-MAX              PIC  9(02).
               05  SKJFTN-KAISUINF         OCCURS  31.
                   07  SKJFTN-SRYKA        PIC  X(02).
                   07  SKJFTN-KAISU        PIC  9(02).
      *
       01  COLDATWRK-REC.
           COPY    "CPCDF002.INC"  REPLACING  //COLDAT02//
                                   BY         //COLDATWRK//.
      *
       01  WRK-NYUTAIIN.
           03  WRK-NYUINYMD                PIC  X(08).
           03  WRK-TAIINYMD                PIC  X(08).
      *
       01  WRK-CONS-AREA.
      *
      *    À¸³èÊÝ¸îÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012".
      *    Ãæ¹ñ»ÄÎ±Ë®¿ÍÅùÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-ZANRYUU-HKNNUM PIC X(03)   VALUE   "025". 
      *    ¼«ÈñÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    ¼«ÇåÀÕÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    ¸¶ÇúÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".
      *    ¸ø³²ÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-KOUGAI-HKNNUM PIC X(03)    VALUE   "975".
      *    µ¿¤¤
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "¤Îµ¿¤¤".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".
      *    µÞÀ­
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "µÞÀ­".
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.
      *
      *    É¸½àÉéÃ´³Û
           03  WRK-CONS-HYOJYUN-FUTAN-G.
      *        É¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN1
                                      PIC X(09)   VALUE   "197000810".
      *        É¸½àÉéÃ´³Û¡ÊÄã½êÆÀ£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN2
                                      PIC X(09)   VALUE   "197001110".
      *        É¸½àÉéÃ´³Û¡ÊÄã½êÆÀ£²¡¦£¹£°Æü°Ê²¼¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN3
                                      PIC X(09)   VALUE   "197000910".
      *        É¸½àÉéÃ´³Û¡ÊÄã½êÆÀ£²¡¦£¹£°ÆüÄ¶¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN4
                                      PIC X(09)   VALUE   "197001010".
      *        ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ«ºÒÆþ±¡Ãæ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN5
                                      PIC X(09)   VALUE   "197001210".
      *        À¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÉ¸½àÉéÃ´³Û
               05  WRK-CONS-HYOJYUN-FUTAN6
                                      PIC X(09)   VALUE   "197001910".
      *        À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN7
                                      PIC X(09)   VALUE   "197002010".
      *        À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN8
                                      PIC X(09)   VALUE   "197002110".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£²¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN9
                                      PIC X(09)   VALUE   "197002210".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN10
                                      PIC X(09)   VALUE   "197002310".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡°Ê³°¡Ë¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN11
                                      PIC X(09)   VALUE   "197002410".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÏ·¿ÍÄã£±¡ÊÏ·Ê¡¡Ë¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN12
                                      PIC X(09)   VALUE   "197002510".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN13
                                      PIC X(09)   VALUE   "197002710".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN14
                                      PIC X(09)   VALUE   "197002810".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN15
                                      PIC X(09)   VALUE   "197002910".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN16
                                      PIC X(09)   VALUE   "197003010".
      *        Ê¿À®£²£¸Ç¯ÅÙ²þÄê¤ÎÄÉ²ÃÊ¬
      *        ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡¦¾®»ùËýÀ­ÆÃÄê¼ÀÉÂ´µ¼Ô¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN17
                                      PIC X(09)   VALUE   "197003310".
      *        ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN18
                                      PIC X(09)   VALUE   "197003410".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN19
                                      PIC X(09)   VALUE   "197003610".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÀº¿À£±Ç¯Ä¶¡¦°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN20
                                      PIC X(09)   VALUE   "197003710".
      *        Ê¿À®£²£¹Ç¯£±£°·î²þÄê¤ÎÄÉ²ÃÊ¬
      *        À¸³èÎÅÍÜ´Ä¶­ÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN21
                                      PIC X(09)   VALUE   "197003810".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡ÊÄã¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN22
                                      PIC X(09)   VALUE   "197003910".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN23
                                      PIC X(09)   VALUE   "197004010".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡ÊÄã£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN24
                                      PIC X(09)   VALUE   "197004110".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN25
                                      PIC X(09)   VALUE   "197004210".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°Æü°Ê²¼¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN26
                                      PIC X(09)   VALUE   "197004310".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN27
                                      PIC X(09)   VALUE   "197004410".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡¦£¹£°ÆüÄ¶¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN28
                                      PIC X(09)   VALUE   "197004510".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN29
                                      PIC X(09)   VALUE   "197004610".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°Æü°Ê²¼¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN30
                                      PIC X(09)   VALUE   "197004710".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£²¡¦£¹£°ÆüÄ¶¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN31
                                      PIC X(09)   VALUE   "197004810".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê»ØÄêÆñÉÂ¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN32
                                      PIC X(09)   VALUE   "197004910".
      *        À¸³èÎÅÍÜ¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê¶­³¦ÁØ³ºÅö¡Ë¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡ÊÄã£±¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN33
                                      PIC X(09)   VALUE   "197005010".
      *        Ê¿À®£³£°Ç¯£´·î²þÄê¤ÎÄÉ²ÃÊ¬
      *        À¸³èÎÅÍÜ¡Ê£±¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN34
                                      PIC X(09)   VALUE   "197005110".
      *        À¸³èÎÅÍÜ¡Ê£²¡Ë¿©»öÎÅÍÜÉ¸½àÉéÃ´³Û¡Ê°åÎÅ¶èÊ¬£²¡¦£³¡Ë¡Ê°ìÈÌ¡Ë
               05  WRK-CONS-HYOJYUN-FUTAN35
                                      PIC X(09)   VALUE   "197005210".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó
           COPY  "CPSK1001.INC".
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó
           COPY    "CPSK1009.INC".
      *
      *    ¸©Æâ°·¤¤ÊÝ¸±¼Ô¾ðÊó
           COPY    "CPSK1033.INC".
      *
      *    ¿ÇÎÅ²Ê¾ðÊó
           COPY    "CPSK1005.INC".
      *    »ÜÀß´ð½à¾ðÊó
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK2005.INC".
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
      *    ´µ¼ÔÊÝ¸±¾ðÊó
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ´µ¼Ô¸øÈñ¾ðÊó
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ÊÝ¸±¾ðÊó
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ´µ¼ÔÉÂÌ¾
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    Æþ±¡²ñ·×ÊÔ½¸¥µ¥Ö
           COPY    "CPORCHCM34S01.INC".
      *    ¸øÈñÉÕ²Ã¾ðÊó¼èÆÀ¥µ¥Ö
           COPY    "CPORCSKOHPLUS.INC".
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *   ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *    ´µ¼Ô¼«¸ÊÉéÃ´¶â¥ì¥»¥×¥Èµ­ºÜÈ½Äê
           COPY    "CPORCSFTNJGNCHK.INC".
      *    ÉÂÌ¾ÁÈÎ©¥µ¥Ö
           COPY    "CPORCSMKBYOMEI.INC".
      *    Æþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSNYURECEDEN.INC".
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       WRK-COLDAT01-TABLE
                                       TTNS-AREA
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-PREFNUM
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-ERRFILENM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBC010"      TO  JOB-PGID
           IF      WRK-PARA-NYUGAIKBN  =  "1"
               MOVE    "Æþ±¡"          TO  WRK-MSG-NYUGAI
           ELSE
               MOVE    "³°Íè"          TO  WRK-MSG-NYUGAI
           END-IF
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           STRING  LNK-DAY2-EDTYMD3(5:12)     DELIMITED BY SIZE
                   WRK-MSG-NYUGAI             DELIMITED BY SIZE
                   "Ê¬¼ý½¸¥Ç¡¼¥¿ºîÀ®¡ÊÃê½Ð¡Ë" DELIMITED BY SIZE
                                 INTO  JOB-SHELLMSG
           END-STRING
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    ¿ÇÎÅÇ¯·îÊÔ½¸
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
      *
           MOVE    WRK-SYMD        TO  WRK-STYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *    °åÎÅµ¡´Ø£É£ÄÊÔ½¸
           INITIALIZE                  SYS-1001-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
           MOVE    WRK-SYMD            TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  WRK-HOSPID
           ELSE
               MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                           TO  WRK-COLDATERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊóÊÔ½¸
           INITIALIZE                  SYS-1009-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
           MOVE    WRK-SYMD            TO  SYS-1009-EDYUKYMD
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-COLDATERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ¥ì¥»¥×¥ÈÊÔ½¸¶èÊ¬¾ðÊó
           INITIALIZE                  SYS-2005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
      *    ¸©Æâ°·¤¤ÊÝ¸±¼Ô¾ðÊó
           MOVE    SPACE               TO  WRK-1033-TABLE
      *
           MOVE    SPACE               TO  SYS-1033-REC
           INITIALIZE                      SYS-1033-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1033-HOSPNUM
           MOVE    "1033"              TO  SYS-1033-KANRICD
           MOVE    "*"                 TO  SYS-1033-KBNCD
           MOVE    SYS-1033-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT91-HOSPNUM
           MOVE    "COLDT910"          TO  COLDAT91-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT91-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT01-HOSPNUM
           MOVE    "COLDT010"          TO  COLDAT01-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT01-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT02-HOSPNUM
           MOVE    "COLDT020"          TO  COLDAT02-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT02-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT03-HOSPNUM
           MOVE    "COLDT030"          TO  COLDAT03-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT03-TERMID
      *
           OPEN    OUTPUT              COLDAT911-FILE
           CLOSE                       COLDAT911-FILE
           OPEN    OUTPUT              COLDAT011-FILE
           CLOSE                       COLDAT011-FILE
           OPEN    OUTPUT              COLDAT021-FILE
           CLOSE                       COLDAT021-FILE
           OPEN    OUTPUT              COLDAT031-FILE
           CLOSE                       COLDAT031-FILE
      *
           OPEN    I-O                 COLDAT91-FILE
                                       COLDAT01-FILE
                                       COLDAT02-FILE
                                       COLDAT03-FILE
      *
           PERFORM 1001-FILE-SYORI-SEC
           PERFORM 10011-FILE-SYORI-SEC
           PERFORM 10012-FILE-SYORI-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂ±¡´ðËÜ¥Õ¥¡¥¤¥ë½èÍý
      *****************************************************************
       1001-FILE-SYORI-SEC              SECTION.
      *
      *    °åÎÅµ¡´Ø¾ðÊó½èÍý
           INITIALIZE                      COLDAT91-REC
           MOVE    "1"                 TO  COLDAT91-RECKBN
           MOVE    1                   TO  COLDAT91-RECSEQ
           MOVE    WRK-HOSPID          TO  COLDAT91-HOSPID
           MOVE    SYS-1001-PREFNUM    TO  COLDAT91-PREFNUM
           MOVE    SYS-1001-HOSPSBT    TO  COLDAT91-HOSPSBT
           MOVE    SYS-1001-HOSPSBT1   TO  COLDAT91-HOSPSBT1
           MOVE    SYS-1001-BEDSU      TO  COLDAT91-BEDSU
           MOVE    SYS-1001-BEDSUIPN   TO  COLDAT91-BEDSUIPN
           MOVE    WRK-PARA-NYUGAIKBN  TO  COLDAT91-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM      TO  COLDAT91-DATAYM
           MOVE    PRGSKBNO            TO  COLDAT91-PRGSKBNO
           WRITE   COLDAT91-REC
           ADD     1                   TO  CNT-COLDAT91
           .
       1001-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂ±¡´ðËÜ¥Õ¥¡¥¤¥ë½èÍý¡Ê¿ÇÎÅ²Ê¾ðÊó¡Ë
      *****************************************************************
       10011-FILE-SYORI-SEC        SECTION.
      *
      *    ¿ÇÎÅ²Ê¾ðÊó
           INITIALIZE                      SYS-1005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    "*"                 TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ADD     1                   TO  IDX
               INITIALIZE                      COLDAT91-REC
               MOVE    "2"                 TO  COLDAT91-RECKBN
               MOVE    IDX                 TO  COLDAT91-RECSEQ
               MOVE    SYS-1005-KBNCD(1:2) TO  COLDAT91-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT91-RECEKA
               MOVE    SYS-1005-SRYKANAME  TO  COLDAT91-SRYKANAME
               WRITE   COLDAT91-REC
               ADD     1                   TO  CNT-COLDAT91
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       10011-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂ±¡´ðËÜ¥Õ¥¡¥¤¥ë½èÍý¡Ê»ÜÀß´ð½à¾ðÊó¡Ë
      *****************************************************************
       10012-FILE-SYORI-SEC        SECTION.
      *
      *    »ÜÀß´ð½à¾ðÊó
           INITIALIZE                      SYS-1006-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "*"                 TO  SYS-1006-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1006-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1006-STYUKYMD(7:2)
           MOVE    SYS-1006-STYUKYMD   TO  SYS-1006-EDYUKYMD
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               MOVE    SYS-1006-KBNCD(1:1) TO  WRK-CHGNUM-P
               PERFORM VARYING  IDX1    FROM  1  BY  1
                                UNTIL   IDX1  >  500
                   IF  SYS-1006-SSTKIJUN (IDX1)  =  1
                       ADD     1        TO  IDX
                       INITIALIZE           COLDAT91-REC
                       MOVE    "3"      TO  COLDAT91-RECKBN
                       MOVE    IDX      TO  COLDAT91-RECSEQ
                       COMPUTE COLDAT91-SSTKIJUNCD
                                        =   (WRK-CHGNUM  -  1)
                                        *   500
                                        +   IDX1
                       WRITE   COLDAT91-REC
                       ADD     1        TO  CNT-COLDAT91
                   END-IF
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       10012-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂ±¡´ðËÜ¥Õ¥¡¥¤¥ë½èÍý¡ÊÊÝ¸±ÈÖ¹æ¾ðÊó¡Ë
      *****************************************************************
       10013-FILE-SYORI-SEC        SECTION.
      *
      *    ÊÝ¸±ÈÖ¹æ¾ðÊó
           MOVE    ZERO                TO  IDX
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
               MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               IF      HKN-HKNNUM      >   "099"
                 ADD     1                 TO  IDX
                 INITIALIZE                    COLDAT91-REC
                 MOVE    "4"               TO  COLDAT91-RECKBN
                 MOVE    IDX               TO  COLDAT91-RECSEQ
                 MOVE    HKN-HKNNUM        TO  COLDAT91-HKNNUM
                 MOVE    HKN-PAYKBN        TO  COLDAT91-PAYKBN
                 MOVE    HKN-HBTNUM        TO  COLDAT91-HBTNUM
                 MOVE    HKN-SEIDONAME     TO  COLDAT91-SEIDONAME
                 MOVE    HKN-HKNKOHSBTKBN  TO  COLDAT91-HKNKOHSBTKBN
                 MOVE    HKN-HKNKOHKBN     TO  COLDAT91-HKNKOHKBN
                 MOVE    HKN-JOKEN-STYEAR  TO  COLDAT91-JOKEN-STYEAR
                 MOVE    HKN-JOKEN-EDYEAR  TO  COLDAT91-JOKEN-EDYEAR
                 MOVE    HKN-TENTNK        TO  COLDAT91-TENTNK
                 MOVE    HKN-RECESKYKBN    TO  COLDAT91-RECESKYKBN
                 WRITE   COLDAT91-REC
                 ADD     1                 TO  CNT-COLDAT91
               END-IF
           END-PERFORM
           .
       10013-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "1"
      *            Æþ±¡²ñ·×ÆÉ¤ß¹þ¤ß¡Ê´µ¼Ô£É£Ä¡¢¿ÇÎÅ²Ê¡¢ºÞ¼±ÊÌ¶èÊ¬½ç¡Ë
                   INITIALIZE                  NYUINACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  NACCT-NYUGAIKBN
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key18"             TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key18"         TO  MCP-PATHNAME
                       PERFORM 900-NYUINACCT-READ-SEC
                   ELSE
                       INITIALIZE                  NYUINACCT-REC
                       MOVE    1               TO  FLG-NYUINACCT
                   END-IF
      *
                   PERFORM 2001-NYUIN-HENSYU-SEC
                                       UNTIL   FLG-NYUINACCT   =   1
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key18"             TO  MCP-PATHNAME
                   PERFORM 900-DBCLOSE-SEC
               WHEN    "2"
      *            ¿ÇÎÅ²ñ·×ÆÉ¤ß¹þ¤ß¡Ê´µ¼Ô£É£Ä¡¢ÊÝ¸±ÁÈ¹ç¤»½ç¡Ë
                   INITIALIZE                  SRYACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"   TO  MCP-TABLE
                       MOVE    "key8"          TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1               TO  FLG-SRYACCT
                   END-IF
      *
                   PERFORM 2001-NYUINGAI-HENSYU-SEC
                                       UNTIL   FLG-SRYACCT     =   1
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-DBCLOSE-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡½èÍý
      *****************************************************************
       2001-NYUIN-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  WRK-COLDAT01-TABLE
           INITIALIZE                  TBL-JNISSU
           INITIALIZE                  TBL-JNISSU-S
           INITIALIZE                  TBL-JNISSU-GEN
           INITIALIZE                  TBL-TOTALTENSU-INF
           INITIALIZE                  WRK-TOTALTENSU-INF
           INITIALIZE                  TBL-SKJNISSU-INF
           INITIALIZE                  TBL-SKJRYOYOHI-INF
           INITIALIZE                  TBL-SKJFTN-INF
           MOVE    SPACE               TO  WRK-NYUTAIIN
           MOVE    ZERO                TO  WRK-ZAINUM
      *
      *    ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
grpsys     MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    NACCT-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           DISPLAY "NYUINACCT PTID=" NACCT-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF    ( FLG-PTINF           =   ZERO )
           AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key18"                 TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
                 IF    NACCT-ZAISKBKBN    =  "1"  OR  "2"  OR
                                             "4"  OR  "5"
      *            ºÞ¼±ÊÌ¶èÊ¬ÊÌ¥Æ¡¼¥Ö¥ëÊÔ½¸
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "2"
                           MOVE    4               TO  IDX
                       WHEN    "4"
                           MOVE    3               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                         IF    IDX         =       3
                           IF  NACCT-SRYYM >       "200603"
                               ADD   1     TO      WRK-DAY (IDX IDX1)
                           ELSE
                               MOVE  1     TO      WRK-DAY (IDX IDX1)
                           END-IF
                         ELSE
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                         END-IF
                       END-IF
                   END-PERFORM
      **             IF      WRK-ZAINUM              <   NACCT-ZAINUM
      **                 MOVE    NACCT-ZAINUM        TO  WRK-ZAINUM
      **             END-IF
                 END-IF
                 MOVE    "tbl_nyuinacct"           TO  MCP-TABLE
                 MOVE    "key18"                   TO  MCP-PATHNAME
                 PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
      *        Æþ±¡²ñ·×¤ÎÊÝ¸±ÁÈ¹ç¤»¤ò¤Þ¤È¤á¤ë
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      WRK-DAY (1 IDX)     >   ZERO    AND
                           WRK-DAY (2 IDX)     >   ZERO
                       PERFORM 20011-NYUINACCT-HENSYU-SEC
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC
      *
                   MOVE    "1"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
      *
      *            Æþ±¡´ðËÜÎÁÅù½èÍý
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  NOT =   "1"
                       PERFORM 20013-NYUIN-KIHON-SEC
                   END-IF
      *ito 20080425  END-PERFORM ¤ò15¹Ô¤¯¤é¤¤¸å¤Ë°ÜÆ°¤µ¤»¤¿
      *
                   PERFORM 20011-SKJFTN-SEC
                   PERFORM 200111-NYUIN-JNISSU-TAIHI-SEC
                   PERFORM 200112-SKJNISSU-TAIHI-SEC
      *            ¼ÂÆü¿ô¡¢ÅÀ¿ôÊÔ½¸
                   PERFORM 20012-JNISSU-SET-SEC
                   PERFORM 20013-TOTALTENSU-SET-SEC
                   MOVE    TBL-JNISSU      TO  TBL-JNISSU-S
                   INITIALIZE                  TBL-JNISSU
                   INITIALIZE                  TBL-JNISSU-GEN
                   INITIALIZE                  TBL-TOTALTENSU-INF
                   INITIALIZE                  WRK-TOTALTENSU-INF
                   INITIALIZE                  TBL-SKJNISSU-INF
                   INITIALIZE                  TBL-SKJRYOYOHI-INF
                   INITIALIZE                  TBL-SKJFTN-INF
               END-PERFORM
      *        ¿ÇÎÅ²ñ·×ÊÔ½¸½èÍý
               PERFORM 20012-NYUIN-SRYACCT-HENSYU-SEC
      *
      *        ´ðËÜ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
               PERFORM 2002-KIHON-WRITE-SEC
           END-IF
           .
       2001-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20011-NYUINACCT-HENSYU-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        ÊÝ¸±ÁÈ¹ç¤»ÄÉ²Ã
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                   ELSE
                       MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "1"     TO  WRK-HKNCOMBI-SYOKBN  (IDX1)
                   END-IF
               END-IF
      *
      *        Å¬ÍÑÈÏ°Ï¤«¥Á¥§¥Ã¥¯¡ÊÈÏ°Ï³°¤Î¤È¤­£²¤ò¥»¥Ã¥È¡Ë
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
           .
       20011-NYUINACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    É¸½àÉéÃ´ºîÀ®
      *****************************************************************
       20011-SKJFTN-SEC            SECTION.
      *
           INITIALIZE                      ORCSNYURECEDEN-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYURECEDEN-HOSPNUM
           MOVE    PTINF-PTID          TO  LNK-NYURECEDEN-PTID
           MOVE    WRK-PARA-SRYYM      TO  LNK-NYURECEDEN-SANTEIYM
           IF      COLDAT01-HKNNUM     =   "060" OR "067" OR
                                           "068" OR "069"
               MOVE    "2"             TO  LNK-NYURECEDEN-HKNKBN
           ELSE
               IF      COLDAT01-HKNNUM     =   "039" OR "040"
                   MOVE    "3"             TO  LNK-NYURECEDEN-HKNKBN
               ELSE
                   MOVE    "1"             TO  LNK-NYURECEDEN-HKNKBN
               END-IF
           END-IF
           PERFORM  VARYING   IDZ1    FROM   1   BY  1
             UNTIL  IDZ1    >    10
               MOVE   COLDAT01-HKNCOMBI(IDZ1)
                                  TO  LNK-NYURECEDEN-SET-HKNCOMBI(IDZ1)
           END-PERFORM
           CALL    "ORCSNYURECEDEN"    USING   ORCSNYURECEDEN-AREA
                                               SPA-AREA
           IF      LNK-NYURECEDEN-RC   NOT =   ZERO
                   DISPLAY "LNK-NYURECEDEN-RC=" LNK-NYURECEDEN-RC
                   GO    TO    20011-SKJFTN-EXT
           END-IF
      *
           PERFORM VARYING IDZ1    FROM    3   BY  1
                   UNTIL   IDZ1    >       5
             PERFORM VARYING IDZ2    FROM    1   BY  1
                     UNTIL   IDZ2    >       31
                     OR      LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                               =   ZERO
               MOVE    ZERO                    TO  FLG-CHK
               PERFORM VARYING IDXZ    FROM    1   BY  1
                       UNTIL   IDXZ    >       10
                       OR      COLDAT01-HKNCOMBI (IDXZ)  =   ZERO
                       IF      LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                           =   COLDAT01-HKNCOMBI (IDXZ)
                           MOVE    1           TO  FLG-CHK
                           MOVE    10          TO  IDXZ
                       END-IF
               END-PERFORM
               PERFORM VARYING IDXZ    FROM    1   BY  1
                       UNTIL   IDXZ    >
                                   LNK-NYURECEDEN-MEISAISU (IDZ1 IDZ2)
                       OR      FLG-CHK =       ZERO
                 IF  ( LNK-NYURECEDEN-SRYCD (IDZ1 IDZ2 IDXZ)
                                       =   WRK-CONS-HYOJYUN-FUTAN1
                                       OR  WRK-CONS-HYOJYUN-FUTAN2
                                       OR  WRK-CONS-HYOJYUN-FUTAN3
                                       OR  WRK-CONS-HYOJYUN-FUTAN4
                                       OR  WRK-CONS-HYOJYUN-FUTAN5
                                       OR  WRK-CONS-HYOJYUN-FUTAN6
                                       OR  WRK-CONS-HYOJYUN-FUTAN7
                                       OR  WRK-CONS-HYOJYUN-FUTAN8
                                       OR  WRK-CONS-HYOJYUN-FUTAN9
                                       OR  WRK-CONS-HYOJYUN-FUTAN10
                                       OR  WRK-CONS-HYOJYUN-FUTAN11
                                       OR  WRK-CONS-HYOJYUN-FUTAN12
                                       OR  WRK-CONS-HYOJYUN-FUTAN13
                                       OR  WRK-CONS-HYOJYUN-FUTAN14
                                       OR  WRK-CONS-HYOJYUN-FUTAN15
                                       OR  WRK-CONS-HYOJYUN-FUTAN16
                                       OR  WRK-CONS-HYOJYUN-FUTAN17
                                       OR  WRK-CONS-HYOJYUN-FUTAN18
                                       OR  WRK-CONS-HYOJYUN-FUTAN19
                                       OR  WRK-CONS-HYOJYUN-FUTAN20
                                       OR  WRK-CONS-HYOJYUN-FUTAN21
                                       OR  WRK-CONS-HYOJYUN-FUTAN22
                                       OR  WRK-CONS-HYOJYUN-FUTAN23
                                       OR  WRK-CONS-HYOJYUN-FUTAN24
                                       OR  WRK-CONS-HYOJYUN-FUTAN25
                                       OR  WRK-CONS-HYOJYUN-FUTAN26
                                       OR  WRK-CONS-HYOJYUN-FUTAN27
                                       OR  WRK-CONS-HYOJYUN-FUTAN28
                                       OR  WRK-CONS-HYOJYUN-FUTAN29
                                       OR  WRK-CONS-HYOJYUN-FUTAN30
                                       OR  WRK-CONS-HYOJYUN-FUTAN31
                                       OR  WRK-CONS-HYOJYUN-FUTAN32
                                       OR  WRK-CONS-HYOJYUN-FUTAN33
                                       OR  WRK-CONS-HYOJYUN-FUTAN34
                                       OR  WRK-CONS-HYOJYUN-FUTAN35 )
                   MOVE    SPACE           TO  COLDAT02-REC
                   INITIALIZE                  COLDAT02-REC
      *            ¿ÇÎÅ²ñ·×¥Ç¡¼¥¿ÊÔ½¸
                   MOVE    COLDAT01-KEY    TO  COLDAT02-KEY
                   COMPUTE WRK-ZAINUM      =   WRK-ZAINUM  +  1
                   MOVE    WRK-ZAINUM      TO  COLDAT02-ZAINUM
                   MOVE    1               TO  COLDAT02-SEQNO
                   MOVE    COMB-HKNID      TO  COLDAT02-HKNID
                   PERFORM VARYING IDZ3    FROM    1   BY  1
                           UNTIL   IDZ3    >   4
                     MOVE  COMB-KOHID (IDZ3)
                                           TO  COLDAT02-TKOHID(IDZ3)
                   END-PERFORM
                   MOVE    "97"            TO  COLDAT02-SRYKBN
                   MOVE    "970"           TO  COLDAT02-SRYSYUKBN
                   MOVE    LNK-NYURECEDEN-TEN (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-ZAITEN
                                               COLDAT02-SYUTEN1
                   MOVE    LNK-NYURECEDEN-KAISU (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-ZAIKAISU
                   MOVE    LNK-NYURECEDEN-HKNCOMBI (IDZ1 IDZ2)
                                           TO  COLDAT02-HKNCOMBI
                   PERFORM VARYING IDZ3    FROM    1   BY  1
                           UNTIL   IDZ3    >   31
                     MOVE  SKJFTN-KAISU (IDZ3)
                                           TO  COLDAT02-KAISU (IDZ3)
                   END-PERFORM
                   MOVE    LNK-NYURECEDEN-SRYCD (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-SRYCD
                   MOVE    LNK-NYURECEDEN-SURYO (IDZ1 IDZ2 IDXZ)
                                           TO  COLDAT02-SRYSURYO
                   IF      COLDAT02-SRYSURYO   =   ZERO
                       MOVE  1             TO  COLDAT02-SRYSURYO
                   END-IF
      *
                   INITIALIZE                  TNS-TENSU-REC
grpsys             MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
                   MOVE    COLDAT02-SRYCD  TO  TNS-SRYCD
                   MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-TANICD      TO  COLDAT02-TANICD
                   MOVE    TNS-TANINAME    TO  COLDAT02-TANINAME
                   MOVE    TNS-FORMALNAME  TO  COLDAT02-FORMALNAME
                   MOVE    TNS-TENSIKIBETU TO  COLDAT02-TENSIKIBETU
                   MOVE    TNS-TEN         TO  COLDAT02-TEN
                   IF      TNS-TENSIKIBETU     =   1   OR   3
                       MOVE  TNS-NYUTENTTLKBN
                                           TO  COLDAT02-TENTTLKBN
                   END-IF
                   MOVE    TNS-NAME        TO  COLDAT02-NAME
                   IF   SKJFTN-MAX         =   1
                        MOVE  SKJFTN-SRYKA(1)
                                           TO  COLDAT02-SRYKA
                   ELSE
                     PERFORM VARYING IDZ3  FROM  1  BY  1
                             UNTIL  (IDZ3  >   31)
                             OR     (SKJFTN-SRYKA (IDZ3)
                                           =   SPACE)
                       IF  COLDAT02-ZAIKAISU = SKJFTN-KAISU(IDZ3)
                           MOVE SKJFTN-SRYKA(IDZ3)
                                           TO  COLDAT02-SRYKA
                           MOVE 31         TO  IDZ3
                       END-IF
                     END-PERFORM
                   END-IF
                   IF      COLDAT02-SRYKA  =   SPACE
                       MOVE  SKJFTN-SRYKA(1)
                                           TO  COLDAT02-SRYKA
                   END-IF
                   PERFORM 200313-SKJFTN-TAIHI-SEC
                   WRITE   COLDAT02-REC
                   ADD     1               TO  CNT-COLDAT02
                 END-IF
               END-PERFORM
             END-PERFORM
           END-PERFORM
           .
       20011-SKJFTN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ÂÆü¿ôÂàÈò
      *****************************************************************
       200111-NYUIN-JNISSU-TAIHI-SEC     SECTION.
      *
           PERFORM VARYING    IDX6   FROM  1  BY  1
                   UNTIL      IDX6     >   31
               IF  WRK-DAY (1 IDX6)    >   ZERO    AND
                   WRK-DAY (2 IDX6)    >   ZERO
                 IF  (WRK-DAY(2 IDX6)  =   WRK-COMB-HKNCOMBINUM (IDX2))
      *ito 20080425 IF  (COLDAT01-HKNNUM      NOT =   SPACE)   OR
      *ito              (COLDAT01-RJNHKNNUM   NOT =   SPACE)
                     MOVE    "1"       TO  TBL-JNISSU-DAY(1 IDX6)
                 END-IF
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    10
                     IF  COLDAT01-HKNCOMBI(IDX3) = WRK-DAY(2 IDX6)
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(2 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(3 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(4 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                          MOVE    "1"       TO  TBL-JNISSU-DAY(5 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
               END-IF
           END-PERFORM
           .
       200111-NYUIN-JNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÆü¿ôÂàÈò
      *****************************************************************
       200112-SKJNISSU-TAIHI-SEC     SECTION.
      *
           PERFORM VARYING    IDX6   FROM  1  BY  1
                   UNTIL      IDX6     >   31
               IF  (WRK-DAY (3 IDX6)    >   ZERO                    )
               AND (WRK-DAY (2 IDX6)    NOT =   ZERO                )
               AND ((WRK-DAY(4 IDX6)        =   ZERO               ) OR
                    (WRK-DAY(4 IDX6)       >=    4                 ))
                 IF  (WRK-DAY(2 IDX6)  =   WRK-COMB-HKNCOMBINUM (IDX2))
      *ito 20080425 IF  (COLDAT01-HKNNUM      NOT =   SPACE)   OR
      *ito              (COLDAT01-RJNHKNNUM   NOT =   SPACE)
                     MOVE WRK-DAY (3 IDX6)  TO  TBL-SKJNISSU(1 IDX6)
                 END-IF
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    10
                     IF  COLDAT01-HKNCOMBI(IDX3) = WRK-DAY(2 IDX6)
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(2 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(3 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(4 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
                 PERFORM VARYING  IDX3   FROM   1   BY   1
                         UNTIL    IDX3     >    4
                     IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                          MOVE WRK-DAY (3 IDX6)
                                            TO  TBL-SKJNISSU(5 IDX6)
                      END-IF
                     ELSE
                         MOVE  10      TO     IDX3
                     END-IF
                 END-PERFORM
               END-IF
           END-PERFORM
           .
       200112-SKJNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÊÔ½¸½èÍý¡ÊÆþ±¡¡Ë
      *****************************************************************
       20012-NYUIN-SRYACCT-HENSYU-SEC           SECTION.
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
      *
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    PTINF-PTID          TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
      *        Ì¤ÄûÀµÊ¬¤¬¤¢¤ë¤«
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      KEY-N-HKNCOMBI      =   9999
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    ZERO                TO  FLG-HKNCOMBI 
               ELSE
      *            ÊÝ¸±ÁÈ¹ç¤»ÆÉ¹þ¤ß
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                        ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE
                      MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF
               END-IF
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
                   MOVE    "2"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
      *
                   IF      WRK-UPDKBN          =   SPACE
      *                Æ±¤¸´µ¼Ô£É£Ä¡¦¿ÇÎÅ²Ê¡¦ÊÝ¸±ÁÈ¹ç¤»¤ÏÆÉ¤ßÈô¤Ð¤¹
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *                    »ý»²Ìô°Ê³°¤Î»þ¡¢ÊÔ½¸½èÍý¤ò¹Ô¤¦
                           IF    ACCT-ZAIKBN   NOT =   3  AND  4
                               PERFORM 2003-SRYACCT-HENSYU-SEC
                           END-IF
                           MOVE    "tbl_sryacct"      TO  MCP-TABLE
                           MOVE    "key9"             TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-PERFORM
                   ELSE
      *                »ý»²Ìô°Ê³°¤Î»þ¡¢ÊÔ½¸½èÍý¤ò¹Ô¤¦
                       IF    ACCT-ZAIKBN   NOT =   3  AND  4
                           PERFORM 2003-SRYACCT-HENSYU-SEC
                       END-IF
                       MOVE    "tbl_sryacct"      TO  MCP-TABLE
                       MOVE    "key9"             TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM
      *        ¼ÂÆü¿ôÊÔ½¸
               PERFORM   20012-JNISSU-SET-SEC
               PERFORM   20013-TOTALTENSU-SET-SEC
               INITIALIZE                  TBL-JNISSU
               INITIALIZE                  TBL-JNISSU-GEN
               INITIALIZE                  TBL-TOTALTENSU-INF
               INITIALIZE                  TBL-SKJNISSU-INF
               INITIALIZE                  TBL-SKJRYOYOHI-INF
               INITIALIZE                  TBL-SKJFTN-INF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    "key9"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       20012-NYUIN-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¡¦ÆÃÄêÆþ±¡ÎÁ¡¦¿©»öÎÅÍÜÊÔ½¸½èÍý
      *****************************************************************
       20013-NYUIN-KIHON-SEC           SECTION.
      *
           INITIALIZE                      HCM34S01-AREA
                                           WRK-SPA-AREA
                                           SKJFTN-DAY-AREA
      *
           PERFORM 200131-NYUINKA-HEN-SEC
      *
           MOVE    SPA-HOSPNUM         TO  HCM34S01-HOSPNUM
                                           WRK-SPA-HOSPNUM
           MOVE    PTINF-PTID          TO  HCM34S01-PTID
           MOVE    WRK-PARA-SRYYM      TO  HCM34S01-SANTEIYM
           MOVE    "1"                 TO  HCM34S01-EXGAIHAKU
           MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
           MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *        Ï«ºÒ¼«ÇåÆþ±¡²ñ·×ÊÔ½¸¥µ¥Ö
               CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           ELSE
      *        °åÎÅÆþ±¡²ñ·×ÊÔ½¸¥µ¥Ö
               CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL   IDX4    >       HCM34S01-HENKYAKU-MAX
      *
             MOVE    1             TO      FLG-EDIT
      *
             PERFORM VARYING IDX5    FROM    1   BY  1
                     UNTIL ( IDX5                >   20    )
                     OR    ( HCM34S01-SRYCD (IDX4 IDX5)
                                                 =   SPACE )
                     OR    ( FLG-EDIT            =   ZERO  )
      *
                 IF  ( HCM34S01-SRYCD (IDX4 IDX5)    =   "099999907" )
                     MOVE    ZERO  TO      FLG-EDIT
                 END-IF
      *
             END-PERFORM
      *
            IF   ( FLG-EDIT                =   1 )
      *
             COMPUTE WRK-ZAINUM            =   WRK-ZAINUM  +  1
             MOVE    ZERO                  TO  WRK-RENNUM
             PERFORM VARYING IDX5    FROM    1   BY  1
                     UNTIL ( IDX5                >   20    )
                     OR    ( HCM34S01-SRYCD (IDX4 IDX5)
                                                 =   SPACE )
      *
               MOVE    SPACE               TO  COLDAT02-REC
               INITIALIZE                      COLDAT02-REC
      *
      *        ¿ÇÎÅ²ñ·×¥Ç¡¼¥¿ÊÔ½¸
               MOVE    COLDAT01-KEY        TO  COLDAT02-KEY
               MOVE    WRK-ZAINUM          TO  COLDAT02-ZAINUM
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  COLDAT02-SEQNO
               MOVE    COMB-HKNID          TO  COLDAT02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  COLDAT02-TKOHID(IDX3)
               END-PERFORM
               MOVE    HCM34S01-SRYKBN  (IDX4) TO  COLDAT02-SRYKBN
               MOVE    HCM34S01-ZAITEN  (IDX4) TO  COLDAT02-ZAITEN
                                                   COLDAT02-SYUTEN1
               MOVE    HCM34S01-ZAIKAISU(IDX4) TO  COLDAT02-ZAIKAISU
               MOVE    HCM34S01-HKNCOMBI       TO  COLDAT02-HKNCOMBI
               MOVE    HCM34S01-DAY-TBL (IDX4) TO  COLDAT02-DAY-AREA
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  COLDAT02-SRYCD
               MOVE    HCM34S01-SURYO   (IDX4 IDX5)
                                               TO  COLDAT02-SRYSURYO
      *
               INITIALIZE                      TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-SRYSYUKBN           TO  COLDAT02-SRYSYUKBN
               MOVE    TNS-TANICD              TO  COLDAT02-TANICD
               MOVE    TNS-TANINAME            TO  COLDAT02-TANINAME
               MOVE    TNS-FORMALNAME          TO  COLDAT02-FORMALNAME
               MOVE    TNS-TENSIKIBETU         TO  COLDAT02-TENSIKIBETU
               MOVE    TNS-TEN                 TO  COLDAT02-TEN
               IF      TNS-TENSIKIBETU     =   1   OR   3
                       MOVE  TNS-NYUTENTTLKBN  TO  COLDAT02-TENTTLKBN
               END-IF
               MOVE    TNS-NAME                TO  COLDAT02-NAME
               MOVE    COLDAT02-REC            TO  COLDATWRK-REC
      *
               IF      COLDAT02-SEQNO          =   1
                 INITIALIZE                        SRYACCT-REC
                 MOVE  COLDAT02-SRYKBN         TO  ACCT-SRYKBN
                 MOVE  ZERO                    TO  ACCT-SRYCDTOTAL
                 MOVE  COLDAT02-ZAITEN         TO  ACCT-ZAITEN
                 MOVE  COLDAT02-ZAIKAISU       TO  ACCT-ZAIKAISU
                 PERFORM 200312-TOTALTEN-TAIHI-SEC
               END-IF
               PERFORM UNTIL   COLDATWRK-ZAIKAISU  =   ZERO
                   MOVE    COLDATWRK-REC       TO  COLDAT02-REC
                   MOVE    ZERO                TO  COLDAT02-ZAIKAISU
                   MOVE    ZERO                TO  COLDAT02-DAY-AREA
                   PERFORM VARYING IDX-DAY   FROM  1  BY  1
                                   UNTIL     IDX-DAY  >   31
                     IF    COLDATWRK-KAISU(IDX-DAY)   >   ZERO
                       IF (COLDAT02-SRYKA   =   SPACE)  OR
                          (COLDAT02-SRYKA   =   TBL-NYUINKA(IDX-DAY))
                           MOVE    TBL-NYUINKA(IDX-DAY)
                                   TO  COLDAT02-SRYKA
                           COMPUTE COLDAT02-ZAIKAISU
                                   =   COLDAT02-ZAIKAISU
                                   +   COLDATWRK-KAISU(IDX-DAY)
                           MOVE    COLDATWRK-KAISU(IDX-DAY)
                                   TO  COLDAT02-KAISU(IDX-DAY)
                           COMPUTE COLDATWRK-ZAIKAISU
                                   =   COLDATWRK-ZAIKAISU
                                   -   COLDATWRK-KAISU(IDX-DAY)
                           MOVE    ZERO
                                   TO  COLDATWRK-KAISU(IDX-DAY)
                       END-IF
                     END-IF
                   END-PERFORM
                   IF   (COLDAT02-SEQNO        =   1)    AND
                        (COLDAT02-SRYKBN       =   "97")
                     PERFORM VARYING IDZ1      FROM    1   BY  1
                             UNTIL   IDZ1      >   31
                       IF    SKJFTN-SRYKA (IDZ1)   =   SPACE
                         MOVE COLDAT02-SRYKA   TO  SKJFTN-SRYKA(IDZ1)
                         MOVE COLDAT02-ZAIKAISU
                                               TO  SKJFTN-KAISU(IDZ1)
                         IF   IDZ1             >   SKJFTN-MAX
                              MOVE  IDZ1       TO  SKJFTN-MAX
                         END-IF
                         MOVE 31               TO  IDZ1
                       ELSE
                         IF  SKJFTN-SRYKA (IDZ1)   =   COLDAT02-SRYKA
                             COMPUTE SKJFTN-KAISU(IDZ1)
                                   = SKJFTN-KAISU(IDZ1)
                                   + COLDAT02-ZAIKAISU
                             IF   IDZ1         >   SKJFTN-MAX
                                  MOVE  IDZ1   TO  SKJFTN-MAX
                             END-IF
                             MOVE 31           TO  IDZ1
                         END-IF
                       END-IF
                     END-PERFORM
                   END-IF
                   WRITE   COLDAT02-REC
                   ADD     1               TO  CNT-COLDAT02
               END-PERFORM
             END-PERFORM
      *
            END-IF
      *
           END-PERFORM
           .
       20013-NYUIN-KIHON-EXT.
           EXIT. 
      *
      *****************************************************************
      *    Æþ±¡ÆüÊÌ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       200131-NYUINKA-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  TBL-NYUINKA-G
           INITIALIZE                      TBL-NYUINKA-G
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
             IF    ( PTNYUINRRK-TENNYUYMD (1:6)    <=  WRK-PARA-SRYYM )
             AND   ( PTNYUINRRK-TENSTUYMD (1:6)    >=  WRK-PARA-SRYYM )
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   WRK-PARA-SRYYM )
                   MOVE    1           TO  TBL-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  TBL-STIDX
               END-IF
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   WRK-PARA-SRYYM )
                   MOVE    31          TO  TBL-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  TBL-EDIDX
               END-IF
               PERFORM VARYING IDX-DAY     FROM    TBL-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       TBL-EDIDX )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  TBL-NYUINKA (IDX-DAY)
               END-PERFORM
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD
                                       TO  WRK-TAIINYMD
             END-IF
             PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       200131-NYUINKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë½èÍý
      *****************************************************************
       2001-NYUINGAI-HENSYU-SEC    SECTION.
      *
           INITIALIZE                  WRK-COLDAT01-TABLE
           INITIALIZE                  TBL-JNISSU
           INITIALIZE                  TBL-JNISSU-S
           INITIALIZE                  TBL-JNISSU-GEN
           INITIALIZE                  TBL-TOTALTENSU-INF
           INITIALIZE                  WRK-TOTALTENSU-INF
           INITIALIZE                  TBL-SKJNISSU-INF
           INITIALIZE                  TBL-SKJRYOYOHI-INF
           INITIALIZE                  TBL-SKJFTN-INF
      *
      *    ¥Æ¥¹¥È´µ¼Ô¤Î¤È¤­¤ÏÆÉ¤ßÈô¤Ð¤¹
grpsys     MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"           TO  MCP-TABLE
                   MOVE    "key8"                  TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            Ì¤ÄûÀµÊ¬¤¬¤¢¤ë¤«
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      KEY-N-HKNCOMBI      =   9999
                       INITIALIZE                  HKNCOMBI-REC
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM
                       MOVE    ZERO                TO  FLG-HKNCOMBI 
                   ELSE
      *                ÊÝ¸±ÁÈ¹ç¤»ÆÉ¹þ¤ß
                       INITIALIZE                  HKNCOMBI-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM 910-HKNCOMBI-INV-SEC
      *
                       IF      FLG-HKNCOMBI    =   ZERO
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                            ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       ELSE
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-IF
      *
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       MOVE    "2"                 TO  WRK-DATAKBN
                       PERFORM 2001-FILE-SYORI-SEC
      *
                       IF      WRK-UPDKBN          =   SPACE
      *                    Æ±¤¸´µ¼Ô£É£Ä¡¦¿ÇÎÅ²Ê¡¦ÊÝ¸±ÁÈ¹ç¤»¤ÏÆÉ¤ßÈô¤Ð¤¹
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               PERFORM 2003-SRYACCT-HENSYU-SEC
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    "key8"          TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           PERFORM 2003-SRYACCT-HENSYU-SEC
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM
      *            ¼ÂÆü¿ôÊÔ½¸
                   PERFORM   20012-JNISSU-SET-SEC
      *            ¹ç·×ÅÀ¿ôÊÔ½¸
                   PERFORM   20013-TOTALTENSU-SET-SEC
                   INITIALIZE                  TBL-JNISSU
                   INITIALIZE                  TBL-JNISSU-GEN
                   INITIALIZE                  TBL-TOTALTENSU-INF
                   INITIALIZE                  WRK-TOTALTENSU-INF
                   INITIALIZE                  TBL-SKJNISSU-INF
                   INITIALIZE                  TBL-SKJRYOYOHI-INF
                   INITIALIZE                  TBL-SKJFTN-INF
               END-PERFORM
      *
      *        ´ðËÜ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
               PERFORM   2002-KIHON-WRITE-SEC
           END-IF
           .
       2001-NYUINGAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë½èÍý
      *****************************************************************
       2001-FILE-SYORI-SEC              SECTION.
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "1"
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                   END-IF
               WHEN    "2"
                   IF      WRK-UPDKBN          =   "1"
      *                ÊÝ¸±¤ÎÄûÀµ¤¬¤µ¤ì¤Æ¤¤¤ë¤«
                       IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                           MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX             >=  WRK-DD
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKSTYMD(1:6)
                                                   >   WRK-STYMD (1:6)
                               MOVE    "1"     TO  WRK-TEISEIKBN
                           END-IF
                       END-IF
                       IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                           MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                           ADD     1                   TO  WRK-DD
                           PERFORM VARYING IDX FROM    WRK-DD  BY  1
                                   UNTIL   IDX             >   31
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKEDYMD(1:6)
                                                   <   WRK-EDYMD (1:6)
                               MOVE    "1"         TO  WRK-TEISEIKBN
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    Ç¯Îð
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    COMB-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    COLDAT01-PTID   TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ËÏ·¿ÍÊÝ¸±»ÈÍÑ¤Î¤È¤­
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *    Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ËÂà¿¦¹ñÊÝ¤Ç£¶£µºÐ°Ê¾å¤Î¤È¤­
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
               IF      AGECHK-NENREI       >=  65
                   MOVE    "1"                 TO  WRK-TEISEIKBN
               END-IF
          END-IF
      *
      *    Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤ËÀ¯ÉÜ´É¾¸¤ò»ÈÍÑ¤Î¤È¤­
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *   ¼çÊÝ¸±¾ðÊóÆÉ¤ß½Ð¤·
           INITIALIZE                      PTHKNINF-REC
                                           PTRSIINF-REC
           IF      COMB-HKNID          NOT =   ZERO
               EVALUATE    COMB-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       INITIALIZE                  PTRSIINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
                       MOVE    COMB-HKNID          TO  PTRSI-HKNID
                       MOVE    COMB-PTID           TO  PTRSI-PTID
                       MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTRSIINF-INV-SEC
                   WHEN    OTHER
                       INITIALIZE                  PTHKNINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                       MOVE    COMB-PTID           TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                       MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
               END-EVALUATE
           END-IF
      *
      *    ¥Õ¥¡¥¤¥ëÊÔ½¸½èÍý
           INITIALIZE                      COLDAT01-REC
           MOVE    WRK-HOSPID          TO  COLDAT01-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  COLDAT01-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  COLDAT01-NYUGAIKBN
           MOVE    COMB-PTID           TO  COLDAT01-PTID
           MOVE    COMB-HKNNUM         TO  COLDAT01-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  COLDAT01-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  COLDAT01-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  COLDAT01-KOHID
                   MOVE    "0"                 TO  WK-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  COLDAT01-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  WK-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "0" TO  WK-HONKZKKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "0" TO  WK-HONKZKKBN
                              END-EVALUATE
                      WHEN    "040"
                      WHEN    "039"
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0"   TO  WK-HONKZKKBN
                          END-EVALUATE
                      WHEN    OTHER
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WK-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WK-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  WK-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  COLDAT01-HKNJANUM
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOH1HKNNUM     =   "020"
                                       OR  "030" )
               MOVE    COMB-KOH1HKNNUM     TO  COLDAT01-HKNJANUM
           END-IF
           IF      WRK-PARA-SRYYM      <=  "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTHKN-HOJOKBN   TO
                                               COLDAT01-HOJOKBN
                   END-IF
               WHEN    "060"
               WHEN    "068"
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2"             TO
                                                   COLDAT01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2"             TO
                                                   COLDAT01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3"             TO
                                                   COLDAT01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    OTHER
                   EVALUATE   PTHKN-HOJOKBN
                      WHEN   "9"
                      WHEN   "8"
                          MOVE    PTHKN-HOJOKBN   TO
                                               COLDAT01-HOJOKBN
                   END-EVALUATE
             END-EVALUATE
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )
                           MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                   END-IF
               WHEN    "060"
               WHEN    "068"
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  COLDAT01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN   "3"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  COLDAT01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  COLDAT01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  COLDAT01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    "040"
               WHEN    "039"
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO  COLDAT01-HOJOKBN
                   END-EVALUATE
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN
                                               TO  COLDAT01-HOJOKBN
                   END-EVALUATE
             END-EVALUATE
           END-IF
      *
           IF      COMB-HKNCOMBINUM    =    9999
               MOVE    "9999999"            TO  COLDAT01-HKNJANUM
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-COLDAT01-TABLE-MAX
               IF      WRK-COLDAT01-HOSPID (IDXX)  =   SPACE
                   PERFORM 20011-COLDAT01-WRITE-SEC
                   MOVE    WRK-COLDAT01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-COLDAT01-KEY (IDXX) =
                                               COLDAT01-KEY
                       PERFORM 20011-COLDAT01-REWRITE-SEC
                       MOVE    WRK-COLDAT01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-FILE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄÉ²Ã½èÍý
      *****************************************************************
       20011-COLDAT01-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
           IF      COLDAT01-HKNNUM NOT =   SPACE
               EVALUATE    COLDAT01-HKNNUM
                 WHEN    WRK-CONS-ROUSAI-HKNNUM
                 WHEN    WRK-CONS-JIBAI-HKNNUM
      *            Ï«ºÒ¾ðÊó 
                   MOVE   PTRSI-HKNID      TO  COLDAT01-HKNID
                   MOVE   PTRSI-HKNKBN     TO  COLDAT01-HKNKBN
                   MOVE   PTRSI-SAIGAIKBN  TO  COLDAT01-SAIGAIKBN
                 WHEN    OTHER
      *            ¼çÊÝ¸±¾ðÊó 
                   MOVE    PTHKN-HKNID     TO  COLDAT01-HKNID
                                               COLDAT01-THKNID     (1)
                   MOVE    PTHKN-TEKSTYMD  TO  COLDAT01-TTEKSTYMD  (1)
                   MOVE    PTHKN-TEKEDYMD  TO  COLDAT01-TTEKEDYMD  (1)
                   MOVE    PTHKN-CONTKBN   TO  COLDAT01-TCONTKBN   (1)
                   MOVE    PTHKN-HOJOKBN   TO  COLDAT01-THOJOKBN   (1)
                   IF      PTHKN-HKNNUM    =   "002"
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )
                           CONTINUE
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  COLDAT01-THOJOKBN (1)
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  COLDAT01-THOJOKBN (1)
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  COLDAT01-THOJOKBN (1)
                           END-EVALUATE
                       END-IF
                   END-IF
                   MOVE    PTHKN-HONKZKKBN TO  COLDAT01-THONKZKKBN (1)
      *            Ë¡ÊÌÈÖ¹æ
                   INITIALIZE                      HKNNUM-REC
grpsys             MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    ZERO                TO  HKN-PAYKBN
                   MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       MOVE    HKN-HBTNUM      TO  COLDAT01-HKNHBTNUM
                       MOVE    HKN-HKNKOHSBTKBN
                                               TO  COLDAT01-HKNKOHSBTKBN
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    Ï·¿ÍÊÝ¸±¾ðÊó
           IF      COLDAT01-KOHID  >   ZERO
               MOVE    COMB-KOH1HKNNUM TO  COLDAT01-RJNHKNNUM
      *
grpsys         MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
               MOVE    PTINF-PTID      TO  PTKOH-PTID
               MOVE    COLDAT01-KOHID  TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   MOVE    PTKOH-PAYKBN        TO  COLDAT01-RJNPAYKBN
                   MOVE    PTKOH-TEKSTYMD      TO  COLDAT01-RJNTEKSTYMD
                   MOVE    PTKOH-TEKEDYMD      TO  COLDAT01-RJNTEKEDYMD
      *            Ë¡ÊÌÈÖ¹æ¤Î¥»¥Ã¥È
                   INITIALIZE                  HKNNUM-REC
grpsys             MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
                   MOVE    PTKOH-KOHNUM    TO  HKN-HKNNUM
                   MOVE    PTKOH-PAYKBN    TO  HKN-PAYKBN
                   MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM      =   ZERO
                       MOVE   HKN-HBTNUM       TO COLDAT01-RJNHBTNUM
                   END-IF
               END-IF
           END-IF
      *
      *    ´µ¼Ô¾ðÊó
           MOVE    PTINF-SEX       TO  COLDAT01-SEX
           MOVE    PTINF-BIRTHDAY  TO  COLDAT01-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  COLDAT01-HOME-POST
      *
      *    Ç¯Îð
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    COLDAT01-PTID   TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC       =   ZERO
               MOVE  AGECHK-NENREI     TO  COLDAT01-AGE
               MOVE  AGECHK-SYUGAKUKBN TO  COLDAT01-SYUGAKUKBN
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM    TO  COLDAT01-HKNCOMBI (1)
           MOVE    COMB-HKNNUM         TO  COLDAT01-HKNNUMHC (1)
           MOVE    COMB-KOHHKNNUM (1)  TO  COLDAT01-KOHNUMHC (1 1)
           MOVE    COMB-KOHID (1)      TO  COLDAT01-KOHIDHC  (1 1)
           MOVE    COMB-KOHHKNNUM (2)  TO  COLDAT01-KOHNUMHC (1 2)
           MOVE    COMB-KOHID (2)      TO  COLDAT01-KOHIDHC  (1 2)
           MOVE    COMB-KOHHKNNUM (3)  TO  COLDAT01-KOHNUMHC (1 3)
           MOVE    COMB-KOHID (3)      TO  COLDAT01-KOHIDHC  (1 3)
           MOVE    COMB-KOHHKNNUM (4)  TO  COLDAT01-KOHNUMHC (1 4)
           MOVE    COMB-KOHID (4)      TO  COLDAT01-KOHIDHC  (1 4)
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"
                   MOVE    ACCT-SRYKA      TO  COLDAT01-SRYKA      (1)
           END-EVALUATE
      *
           MOVE    WRK-TEISEIKBN   TO  COLDAT01-TEISEIKBN
      *
           MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
           .
       20011-COLDAT01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÑ¹¹½èÍý
      *****************************************************************
       20011-COLDAT01-REWRITE-SEC            SECTION.
      *
           MOVE    WRK-COLDAT01-OCC (IDXX) TO  COLDAT01-REC
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
      *    ¼çÊÝ¸±¾ðÊó 
           IF      COLDAT01-HKNNUM     NOT =   SPACE
             EVALUATE    COLDAT01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                 CONTINUE
               WHEN    OTHER
                 MOVE    1                       TO  FLG-LASTHKNID
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       5
                     IF    ( COLDAT01-TTEKSTYMD (IDX)
                                       =   PTHKN-TEKSTYMD )
                     AND   ( COLDAT01-TTEKEDYMD (IDX)
                                       =   PTHKN-TEKEDYMD )
                         MOVE    5              TO  IDX
                     ELSE
                         IF      COLDAT01-TTEKSTYMD (IDX)  =   SPACE
                             MOVE    PTHKN-HKNID
                                           TO  COLDAT01-THKNID    (IDX)
                             MOVE    PTHKN-TEKSTYMD
                                           TO  COLDAT01-TTEKSTYMD (IDX)
                             MOVE    PTHKN-TEKEDYMD
                                           TO  COLDAT01-TTEKEDYMD (IDX)
                             MOVE    PTHKN-CONTKBN
                                           TO  COLDAT01-TCONTKBN  (IDX)
                             MOVE    PTHKN-HOJOKBN
                                           TO  COLDAT01-THOJOKBN  (IDX)
                             IF      PTHKN-HKNNUM    =   "002"
                                 IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                                 OR    ( AGECHK-NENREI <=  74          )
                                     CONTINUE
                                 ELSE
                                     EVALUATE    PTHKN-HOJOKBN
                                       WHEN    "A"
                                       WHEN    "G"
                                       WHEN    "D"
                                         MOVE    "1"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                       WHEN    "B"
                                       WHEN    "H"
                                       WHEN    "E"
                                         MOVE    "2"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                       WHEN    "C"
                                       WHEN    "I"
                                       WHEN    "F"
                                         MOVE    "3"
                                           TO  COLDAT01-THOJOKBN (IDX)
                                     END-EVALUATE
                                 END-IF
                             END-IF
                             MOVE    PTHKN-HONKZKKBN
                                           TO  COLDAT01-THONKZKKBN(IDX)
                             MOVE    5     TO  IDX
                         ELSE
                             IF      COLDAT01-TTEKSTYMD (IDX)
                                                   >=  PTHKN-TEKSTYMD
                                 MOVE    ZERO   TO  FLG-LASTHKNID
                             END-IF
                         END-IF
                     END-IF
                 END-PERFORM
      *
                 IF      FLG-LASTHKNID       =   1
                     MOVE    PTHKN-HKNID         TO  COLDAT01-HKNID
                 END-IF
             END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      COLDAT01-HKNCOMBI    (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       COLDAT01-HKNCOMBI   (IDX)
                   MOVE    COMB-HKNNUM         TO
                                       COLDAT01-HKNNUMHC   (IDX)
                   MOVE    COMB-KOHHKNNUM (1)  TO
                                       COLDAT01-KOHNUMHC (IDX 1)
                   MOVE    COMB-KOHID (1)      TO
                                       COLDAT01-KOHIDHC  (IDX 1)
                   MOVE    COMB-KOHHKNNUM (2)  TO
                                       COLDAT01-KOHNUMHC (IDX 2)
                   MOVE    COMB-KOHID (2)      TO
                                       COLDAT01-KOHIDHC  (IDX 2)
                   MOVE    COMB-KOHHKNNUM (3)  TO
                                       COLDAT01-KOHNUMHC (IDX 3)
                   MOVE    COMB-KOHID (3)      TO
                                       COLDAT01-KOHIDHC  (IDX 3)
                   MOVE    COMB-KOHHKNNUM (4)  TO
                                       COLDAT01-KOHNUMHC (IDX 4)
                   MOVE    COMB-KOHID (4)      TO
                                       COLDAT01-KOHIDHC  (IDX 4)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       COLDAT01-HKNCOMBI   (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       20
                       IF      COLDAT01-SRYKA    (IDX) =   SPACE
                           MOVE    ACCT-SRYKA  TO  COLDAT01-SRYKA (IDX)
                           MOVE    20          TO  IDX
                       ELSE
                           IF      COLDAT01-SRYKA  (IDX)
                                                       =   ACCT-SRYKA
                               MOVE    20          TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  COLDAT01-TEISEIKBN
           END-IF
      *
           MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
           .
       20011-COLDAT01-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÈÖ¹æ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20012-KOHID-CHECK-SEC              SECTION.
      *
      *    Ï·¿ÍÊÝ¸±°Ê³°¤Î¸øÈñ¾ðÊó¤òÊÔ½¸
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               IF      COMB-KOHHKNNUM (IDX)    =   "027"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY         >   10
                       IF      COMB-KOHID (IDX)
                                           =   COLDAT01-TKOHID (IDY)
                           MOVE    10              TO  IDY
                       ELSE
                           IF      COLDAT01-TKOHID (IDY)   =   ZERO
                               PERFORM 200121-KOHID-HENSYU-SEC
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20012-KOHID-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÈÖ¹æÊÔ½¸½èÍý
      *****************************************************************
       200121-KOHID-HENSYU-SEC                SECTION.
      *
           MOVE    COMB-KOHID (IDX)    TO  COLDAT01-TKOHID    (IDY)
           MOVE    COMB-KOHHKNNUM(IDX) TO  COLDAT01-TKOHHKNNUM(IDY)
           MOVE    COMB-KOHPAYKBN(IDX) TO  COLDAT01-TKOHPAYKBN(IDY)
      *
grpsys     MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    COLDAT01-PTID       TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           IF       FLG-PTKOHINF       =   ZERO
               MOVE   PTKOH-TEKSTYMD   TO  COLDAT01-TKOHTEKSTYMD (IDY)
               MOVE   PTKOH-TEKEDYMD   TO  COLDAT01-TKOHTEKEDYMD (IDY)
      *        Ë¡ÊÌÈÖ¹æ¤Î¥»¥Ã¥È
               INITIALIZE                 HKNNUM-REC
grpsys         MOVE    SPA-HOSPNUM     TO HKN-HOSPNUM
               MOVE    PTKOH-KOHNUM    TO HKN-HKNNUM
               MOVE    PTKOH-PAYKBN    TO HKN-PAYKBN   
               MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM      =   ZERO
                    MOVE   HKN-HBTNUM      TO  COLDAT01-TKOHHBTNUM (IDY)
               END-IF
      *        ÃÏÊý¸øÈñ¤Î¤È¤­¥ì¥»¥×¥ÈÀÁµá¶èÊ¬¤ò¼èÆÀ
               IF    ( COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8" )
               OR    ( COMB-KOHHKNNUM (IDX)(1:2)
                                           =  "99"             )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "060"
                       WHEN    "067"
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "5"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    "039"
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "4"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    OTHER
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                   END-EVALUATE
                   IF     COLDAT01-TRECESKYKBN (IDY)   =   1
      *                ÃÏÊý¸øÈñ¥ì¥»µ­ºÜÈ½Äê
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                ¿ÇÎÅÇ¯·î
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                ¸øÈñÈÖ¹æ
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                ¸øÈñ£É£Ä
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                ÅÔÆ»ÉÜ¸©ÈÖ¹æ
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                ¼çÊÝ¸±¡Ý£É£Ä
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       "2"  TO  COLDAT01-TRECESKYKBN(IDY)
                       END-IF
                   END-IF
               ELSE
      *            Á´¹ñ¸øÈñ¤Î¤È¤­¡ÊÄ¹´ü¡Ë
                   IF      HKN-HKNKOHSBTKBN    =   "2" 
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            Á´¹ñ¸øÈñ¤Î¤È¤­¡ÊÀ¸ÊÝ¡Ë
      **>          IF   COMB-KOHHKNNUM (IDX)   =   "012" OR "021"
      *                ¥ì¥»µ­ºÜÈ½Äê
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                ¿ÇÎÅÇ¯·î
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                ¸øÈñÈÖ¹æ
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                ¸øÈñ£É£Ä
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                ÅÔÆ»ÉÜ¸©ÈÖ¹æ
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                ¼çÊÝ¸±¡Ý£É£Ä
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE    "2"     TO  COLDAT01-TRECESKYKBN(IDY)
                       ELSE
                           MOVE    "1"     TO  COLDAT01-TRECESKYKBN(IDY)
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
      **>          END-IF
      *            Á´¹ñ¸øÈñ¤Î¤È¤­¡ÊÏ·¿Í¸¶Çú¡Ë
                   IF    ( COLDAT01-RJNHKNNUM
                                       NOT =   SPACE                   )
                   AND   ( COMB-KOHHKNNUM (IDX)
                                           =   WRK-CONS-GENBAKU-HKNNUM )
                   AND   ( COLDAT01-TRECESKYKBN(IDY)  =  "1"           )
                       IF      COLDAT01-HKNNUM =   "060"   OR  "067"
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "2" TO  COLDAT01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            Á´¹ñ¸øÈñ¤Î¤È¤­¡ÊÄ¹´ü¡¦Ï·¿Í¸¶Çú¡¦À¸ÊÝ¡¦Àº¿À°Ê³°¡Ë
      **>          MOVE    "1"         TO  COLDAT01-TRECESKYKBN(IDY)
               END-IF
           END-IF
           .
       200121-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¡²¬¸©¸øÈñ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    ¸©³°°·¤¤¤ÎÊÝ¸±¼Ô¤Ï¥ì¥»¥×¥È¤Øµ­ºÜ¤·¤Ê¤¤
           IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
           ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
           END-IF
           IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"
                           IF      PTHKN-HKNJANUM =
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ¸©Æâ°·¤¤¤Î¾ì¹ç¡¢¸øÈñÉÕ²Ã¾ðÊó¤è¤ê¥ì¥»¥×¥ÈÀÁµá¶èÊ¬¤ò¼èÆÀ
           IF      WRK-PREFKBN         =   SPACE
               MOVE    "2"                 TO  COLDAT01-TRECESKYKBN(IDY)
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    WRK-SYMD            TO  LNK-SKOHPLUS-SRYYMD
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
               IF      LNK-SKOHPLUS-RC     =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    "2"         TO  COLDAT01-TRECESKYKBN(IDY)
                   ELSE
                       MOVE    "1"         TO  COLDAT01-TRECESKYKBN(IDY)
                   END-IF
               ELSE
                   MOVE    "1"         TO  COLDAT01-TRECESKYKBN (IDY)
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ÂÆü¿ôÊÔ½¸
      *****************************************************************
       20012-JNISSU-SET-SEC        SECTION.
      *
      *¼ÂÆü¿ô¤«¤é¸º¤¹¤ë
           PERFORM    VARYING    JN-IDX    FROM   1   BY   1
                       UNTIL     JN-IDX    >      5
             PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                         UNTIL     ACCT-IDX    >      31
                IF  (TBL-JNISSU-DAY(JN-IDX ACCT-IDX)      NOT =  SPACE)
                AND (TBL-JNISSU-DAY-GEN(JN-IDX ACCT-IDX)  NOT =  SPACE)
                    MOVE  SPACE  TO  TBL-JNISSU-DAY(JN-IDX ACCT-IDX)
                END-IF
             END-PERFORM
           END-PERFORM
      *
      *¼ÂÆü¿ôÊÔ½¸
      *    ¼çÊÝ¸±
           PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                      UNTIL      ACCT-IDX    >     31
                    IF  ( TBL-JNISSU-DAY(1 ACCT-IDX)
                                        NOT =  SPACE )
                    AND ( TBL-JNISSU-DAY-S(1 ACCT-IDX)
                                            =  SPACE )
                          ADD    1      TO    COLDAT01-HKNJNISSU
                    END-IF
                    ADD   TBL-SKJNISSU(1 ACCT-IDX)
                                        TO    COLDAT01-HKNSKJNISSU
           END-PERFORM
      *    ¸øÈñ
           PERFORM    VARYING    JN-IDX    FROM   2   BY   1
                      UNTIL      JN-IDX    >      5
               PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                          UNTIL      ACCT-IDX    >     31
                    IF  ( TBL-JNISSU-DAY(JN-IDX ACCT-IDX)
                                        NOT =  SPACE )
                    AND ( TBL-JNISSU-DAY-S(JN-IDX ACCT-IDX)
                                            =  SPACE )
                          ADD    1      TO
                                       COLDAT01-TKOHJNISSU(JN-IDX - 1)
                    END-IF
                    ADD   TBL-SKJNISSU(JN-IDX ACCT-IDX)
                                        TO
                                     COLDAT01-TKOHSKJNISSU(JN-IDX - 1)
               END-PERFORM
           END-PERFORM
           .
       20012-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¹ç·×ÅÀ¿ôÊÔ½¸
      *****************************************************************
       20013-TOTALTENSU-SET-SEC    SECTION.
      *
      *    ¼çÊÝ¸±
           COMPUTE COLDAT01-HKNTOTALTENSU  =   COLDAT01-HKNTOTALTENSU
                                           +   TBL-TOTALTENSU (1)
           IF    WRK-TOTALTENSU (1)        NOT =   ZERO
               COMPUTE COLDAT01-HKNTOTALTENSU = COLDAT01-HKNTOTALTENSU
                                           -   WRK-TOTALTENSU (1)
           END-IF
           COMPUTE COLDAT01-HKNSKJRYOYOHI  =   COLDAT01-HKNSKJRYOYOHI
                                           +   TBL-SKJRYOYOHI (1)
           COMPUTE COLDAT01-HKNSKJFTN      =   COLDAT01-HKNSKJFTN
                                           +   TBL-SKJFTN     (1)
      *    ¸øÈñ
           PERFORM    VARYING    JN-IDX    FROM   2   BY   1
                      UNTIL      JN-IDX    >      5
               COMPUTE COLDAT01-TKOHTOTALTENSU(JN-IDX - 1) =
                       COLDAT01-TKOHTOTALTENSU(JN-IDX - 1)
                                           +   TBL-TOTALTENSU (JN-IDX)
               IF    WRK-TOTALTENSU (JN-IDX)    NOT =   ZERO
                 COMPUTE COLDAT01-TKOHTOTALTENSU(JN-IDX - 1) =
                         COLDAT01-TKOHTOTALTENSU(JN-IDX - 1)
                                           -  WRK-TOTALTENSU (JN-IDX)
               END-IF
               COMPUTE COLDAT01-TKOHSKJRYOYOHI(JN-IDX - 1) =
                       COLDAT01-TKOHSKJRYOYOHI(JN-IDX - 1)
                                           +   TBL-SKJRYOYOHI (JN-IDX)
               COMPUTE COLDAT01-TKOHSKJFTN(JN-IDX - 1)     =
                       COLDAT01-TKOHSKJFTN(JN-IDX - 1)
                                           +   TBL-SKJFTN (JN-IDX)
           END-PERFORM
      *    ¥Æ¡¼¥Ö¥ë¥ì¥³¡¼¥ÉÆþ¤ìÂØ¤¨
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
               IF  WRK-COLDAT01-KEY (IDXX) =
                                           COLDAT01-KEY
                   MOVE    COLDAT01-REC    TO  WRK-COLDAT01-OCC (IDXX)
                   MOVE    100             TO  IDXX
               END-IF
           END-PERFORM
           .
       20013-TOTALTENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ðËÜ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       2002-KIHON-WRITE-SEC              SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-COLDAT01-TABLE-MAX
                   OR      WRK-COLDAT01-HOSPID (IDXX)  =   SPACE
               MOVE    WRK-COLDAT01-OCC (IDXX)     TO  COLDAT01-REC
      *
      *        ¿³ºº»ÙÊ§µ¡´Ø
               IF      COLDAT01-HKNNUM =  "060" OR "067" OR
                                          "068" OR "069"
                   MOVE    2            TO  COLDAT01-TEISYUTUSAKI
               ELSE
                   IF      COLDAT01-HKNNUM =  "039" OR "040"
                       MOVE    3            TO  COLDAT01-TEISYUTUSAKI
                   ELSE
                       MOVE    1            TO  COLDAT01-TEISYUTUSAKI
                   END-IF
               END-IF
      *        ¥ì¥»¥×¥È¼ïÊÌ
               PERFORM 20021-RECESYUBETU-SET-SEC
      *
               IF      WRK-PARA-NYUGAIKBN   =  "1"
                   MOVE    WRK-NYUINYMD TO  COLDAT01-NYUINYMD
                   MOVE    WRK-TAIINYMD TO  COLDAT01-TAIINYMD
               ELSE
                   MOVE    SPACE        TO  COLDAT01-NYUINYMD
                   MOVE    SPACE        TO  COLDAT01-TAIINYMD
               END-IF
      *
               WRITE   COLDAT01-REC
               ADD     1                    TO  CNT-COLDAT01
           END-PERFORM
      *
      *    ÉÂÌ¾¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
           PERFORM 20022-BYOMEI-HENSYU-SEC
           .
       2002-RECESYUBETU-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¼ïÊÌ¥»¥Ã¥È½èÍý
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    ÊÝ¸±ÊÑ¹¹¸å¡¢ÄûÀµ¤·¤Æ¤¤¤Ê¤¤¤È¤­
      *    ÊÝ¸±ÁÈ¹ç¤»¤¬¤Ê¤¤¤È¤­
           IF      COLDAT01-TEISEIKBN  =   "1"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *    Êñ³ç¤Î¤È¤­
           IF      COLDAT01-HKNJANUM   =   "9999999"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤Î¤È¤­
           IF      COLDAT01-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               EVALUATE    COLDAT01-HKNKBN
                                       ALSO    COLDAT01-SAIGAIKBN
                                           ALSO    WRK-PARA-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"
                       MOVE    7081        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"
                       MOVE    7071        TO    COLDAT01-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092        TO    COLDAT01-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021        TO    COLDAT01-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY
                       MOVE    7001        TO    COLDAT01-RECESYUBETU
      *            ¸øÌ³ºÒ³²
                   WHEN    "5"         ALSO    ANY     ALSO    "1"
                       MOVE    7061        TO    COLDAT01-RECESYUBETU
                   WHEN    "5"         ALSO    ANY     ALSO    "2"
                       MOVE    7051        TO    COLDAT01-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    ¼«Èñ¤Î¤È¤­
           IF     COLDAT01-HKNKOHSBTKBN
                                       =   "8"
               MOVE    8888                TO  COLDAT01-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    ¼£¸³¤Î¤È¤­
           IF     (COLDAT01-HKNNUM    >=  "900")    AND
                  (COLDAT01-HKNNUM    <=  "919")
               MOVE    8888                TO  COLDAT01-RECESYUBETU
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    ¸ø³²ÊÝ¸±¤Î¤È¤­
           IF      COLDAT01-HKNNUM     =   WRK-CONS-KOUGAI-HKNNUM
               EVALUATE    WRK-PARA-NYUGAIKBN
                   WHEN    "1"
                       MOVE    7210        TO    COLDAT01-RECESYUBETU
                   WHEN    "2"
                       MOVE    7200        TO    COLDAT01-RECESYUBETU
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
           IF      COLDAT01-HKNNUM     NOT =   SPACE
               EVALUATE    COLDAT01-HKNNUM
                   WHEN    "060"
                   WHEN    "068"
                       MOVE   "1"          TO  WK-KOKUHO
                   WHEN    "067"
                   WHEN    "069"
                       MOVE   "2"          TO  WK-KOKUHO
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           IF      WRK-PARA-SRYYM     <      "200210"
             IF      COLDAT01-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
      *****MOVE    COLDAT01-HONKZKKBN  TO  WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      COLDAT01-TKOHHKNNUM (IDX)   =   SPACE
               IF      COLDAT01-TRECESKYKBN (IDX)  =   "1"
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
           IF  COLDAT01-HKNNUM     NOT =   SPACE
      *
      *        ¼çÊÝ¸±ºÇ¿·¾ðÊó
               MOVE    1               TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      COLDAT01-TTEKSTYMD (IDX)  =   SPACE
                   IF      COLDAT01-TTEKSTYMD (IDX)
                                           >   COLDAT01-TTEKSTYMD (IDY)
                       MOVE    IDX             TO  IDY
                   END-IF
               END-PERFORM
               MOVE    COLDAT01-THONKZKKBN (IDY)   TO  WK-HONKZKKBN
      *
               IF    ( WRK-PARA-SRYYM      >=  "200210"              )
               AND   ( WRK-PARA-SRYYM      <   WRK-CONS-SRYYM-200804 )
      *            £³ºÐÌ¤Ëþ¤Î¾ì¹ç¡¢£×£Ë¡Ý£È£Ï£Î£Ë£Ú£Ë£Ë£Â£Î¤Ë"£³"¤ò¥»¥Ã¥È
                   IF      COLDAT01-AGE         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *        Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
               IF      WRK-PARA-SRYYM      >=  WRK-CONS-SRYYM-200804
                   IF      COLDAT01-SYUGAKUKBN =   1
                       MOVE   "3"          TO  WK-HONKZKKBN
                   END-IF
               END-IF
      *
      *        ¸å´ü¹âÎðÈó³ºÅö¤ÎÍ­ÌµÀßÄê
               MOVE    ZERO            TO  WRK-KOUKI-FLG
      * 
               IF    ( COLDAT01-AGE    >=  70                    )
               AND   ( WRK-PARA-SRYYM  >=  WRK-CONS-SRYYM-200804 )
                   IF    ( COLDAT01-HKNNUM     =   SPACE   OR  "040"   
                                               OR  "039"   OR  "067"
                                               OR  "069"             )
                   OR    ( COLDAT01-HKNNUM (1:2)
                                               =   "98"    OR  "90"
                                               OR  "91"              )
                       CONTINUE
                   ELSE
                       IF   (  COLDAT01-HKNNUM =   "060"   OR  "068" )
                       OR   (( COLDAT01-HKNNUM
                                           NOT =   "060"   AND "068" )
                               AND
                             ( COLDAT01-THOJOKBN (IDY)
                                               =   SPACE             ))
                           MOVE    WRK-PARA-SRYYM  TO  WRK-TEKSTYYMM
                           IF      COLDAT01-TTEKSTYMD (IDY) (1:6)  >=
                                               WRK-PARA-SRYYM
                                MOVE  COLDAT01-TTEKSTYMD (IDY)(7:2)
                                                       TO  WRK-TEKSTDD
                           ELSE
                                MOVE  01               TO  WRK-TEKSTDD
                           END-IF
      *
                           INITIALIZE                      PTKOHINF-REC
                           MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
                           MOVE    COLDAT01-PTID       TO  PTKOH-PTID
                           MOVE    "976"               TO  PTKOH-KOHNUM
                           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-DBSELECT-SEC
                           IF      MCP-RC              =   ZERO
                               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                               MOVE    "key4"          TO  MCP-PATHNAME
                               PERFORM 900-PTKOHINF-READ-N-SEC
                               PERFORM UNTIL   FLG-PTKOHINF    =   1
                                   IF    (PTKOH-TEKSTYMD
                                                       <=  WRK-TEKSTYMD)
                                   AND   (WRK-TEKSTYMD <=
                                                       PTKOH-TEKEDYMD  )
                                       MOVE    1       TO  WRK-KOUKI-FLG
                                       MOVE    1       TO  FLG-PTKOHINF
                                   ELSE
                                       MOVE    "tbl_ptkohinf"
                                                        TO  MCP-TABLE
                                       MOVE    "key4"   TO  MCP-PATHNAME
                                       PERFORM  900-PTKOHINF-READ-N-SEC
                                   END-IF
                               END-PERFORM
                           END-IF
                           MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                           MOVE    "key4"               TO  MCP-PATHNAME
                           PERFORM  900-DBCLOSE-SEC
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-PARA-SRYYM  >=  WRK-CONS-SRYYM-200804
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
      *********************IF  (COLDAT01-AGE  >=   70)  AND
      *********************    (COLDAT01-AGE  <=   74)
                           IF  (COLDAT01-AGE  >=   70)
                           AND (WRK-KOUKI-FLG =    0 )
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF
                       WHEN    "039"
                       WHEN    "040"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067"
                       WHEN    "069"
                           IF      COLDAT01-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   COLDAT01-THOJOKBN (IDY)
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
               END-IF
      *
               IF    ( WRK-PARA-SRYYM      >=  "200610" )
               AND   ( WRK-PARA-SRYYM      <   WRK-CONS-SRYYM-200804 )
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060"
                       WHEN    "068"
                           IF  (COLDAT01-AGE  >=   70)  AND
                               (COLDAT01-AGE  <=   74)
                             EVALUATE    WRK-PARA-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   COLDAT01-THOJOKBN (IDY)
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF
                       WHEN    OTHER
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COLDAT01-RJNHKNNUM      =    "027"
                       EVALUATE   COLDAT01-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( WRK-PARA-SRYYM      >=  "200210" )
               AND   ( WRK-PARA-SRYYM      <=  "200609" )
      *
      *            £±³ä¡¢£²³äÉéÃ´¤Î¹âÎð¼Ô¤Î¾ì¹ç£×£Ë¡Ý£È£Ï£Î£Ë£Ú£Ë£Ë£Â£Î¤Ë
      *            "£¹"¤Þ¤¿¤Ï"£¸"¤ò¥»¥Ã¥È
      *            Ï·¿ÍÊÝ¸±¤Î¾ì¹ç¤Ï£×£Ë¡Ý£Ò£Ê£Î¤Ë¥»¥Ã¥È¤¹¤ë	
                   EVALUATE    COLDAT01-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF  (COLDAT01-AGE  >=   70)  AND
                               (COLDAT01-AGE  <=   74)
                               EVALUATE   COLDAT01-THOJOKBN (IDY)
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COLDAT01-THOJOKBN (IDY)
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      COLDAT01-RJNHKNNUM      =    "027"
                       EVALUATE   COLDAT01-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( COLDAT01-THONKZKKBN (IDY)
                                       =   "1"   )
               IF      COLDAT01-HKNNUM =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
      *     DISPLAY "HONKZKKBN : " WK-HONKZKKBN 
      *     DISPLAY "NYUGAIKBN : " COLDAT01-NYUGAIKBN 
      *     DISPLAY "IHO       : " WK-IHO 
      *     DISPLAY "KOKUHO    : " WK-KOKUHO 
      *     DISPLAY "RJN       : " WK-RJN 
      *     DISPLAY "KOHI-CNT  : " WK-KOHI-CNT 
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    COLDAT01-NYUGAIKBN  TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM      TO  LNK-SYUBETUGET-SRYYM
           MOVE    COLDAT01-HKNNUM     TO  LNK-SYUBETUGET-HKNNUM
           MOVE    COLDAT01-BIRTHDAY   TO  LNK-SYUBETUGET-BIRTHDAY 
      *
      *    ¹ñÊÝ¤Î¤È¤­£¹£·£·¤Î¸øÈñ¤ÎÍ­Ìµ
           IF    ( LNK-SYUBETUGET-BIRTHDAY
                                       <   19440402 )
           AND   ( LNK-SYUBETUGET-HKNNUM
                                       =   "060"    )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >   "201404" )
               MOVE    "99"                TO  LNK-SYUBETUGET-RJNPAYKBN
      *
               MOVE    LNK-SYUBETUGET-SRYYM
                                           TO  WRK-TEKSTYYMM
               IF      COLDAT01-TTEKSTYMD (IDY) (1:6)
                                           >=  WRK-PARA-SRYYM
                   MOVE    COLDAT01-TTEKSTYMD (IDY)(7:2)
                                               TO  WRK-TEKSTDD
               ELSE
                   MOVE    01                  TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
               MOVE    COLDAT01-PTID       TO  PTKOH-PTID
               MOVE    "977"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
                   PERFORM UNTIL   FLG-PTKOHINF    =   1
                       IF    ( PTKOH-TEKSTYMD  <=  WRK-TEKSTYMD   )
                       AND   ( WRK-TEKSTYMD    <=  PTKOH-TEKEDYMD )
                           MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                           MOVE    1               TO  FLG-PTKOHINF
                       ELSE
                           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                           MOVE    "key4"          TO  MCP-PATHNAME
                           PERFORM  900-PTKOHINF-READ-N-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  900-DBCLOSE-SEC
           END-IF
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  COLDAT01-RECESYUBETU
      *
           .
        20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÌ¾¾ðÊó½ÐÎÏ½èÍý
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC              SECTION.
      *
      *    ´µ¼ÔÉÂÌ¾ÆÉ¤ß¹þ¤ß
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI     =   1
               PERFORM 200221-BYOMEI-WRITE-SEC
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾½ÐÎÏ½èÍý
      *****************************************************************
       200221-BYOMEI-WRITE-SEC         SECTION.
      *
      *    ¿ÇÎÅ³«»ÏÆü
           IF      PTBYO-SRYYMD(1:6)    =   WRK-PARA-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   WRK-PARA-SRYYM
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  WRK-PARA-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    Æþ³°¶èÊ¬
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   WRK-PARA-NYUGAIKBN
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    "999999"¤Î¥³¡¼¥É¤òÆÉ¤ßÈô¤Ð¤¹
           INITIALIZE                  WRK-PTBYOMEI-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               IF      PTBYO-BYOMEICD (IDX)    =   ALL "9"
                   CONTINUE
               ELSE
                   ADD     1               TO  WRK-PTBYO-BYOMEICDSU
                                               IDY
                   MOVE    PTBYO-BYOMEICD (IDX)
                                           TO  WRK-PTBYO-BYOMEICD (IDY)
               END-IF
           END-PERFORM
           MOVE    WRK-PTBYO-BYOMEICD-G
                                       TO  PTBYO-BYOMEICD-G
           MOVE    WRK-PTBYO-BYOMEICDSU
                                       TO  PTBYO-BYOMEICDSU    
      *
      *    µ¿¤¤¥Õ¥é¥°¤è¤êÉÂÌ¾¤òÊÔ½¸
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
               MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
               MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               COMPUTE PTBYO-BYOMEIMOJI    =   SMKBYO-LENGTH   /   2
               MOVE    SMKBYO-BYOMEI       TO  PTBYO-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  PTBYO-BYOMEICD-G
           END-IF
      *
           INITIALIZE                      COLDAT03-REC
           MOVE    COLDAT01-HOSPID     TO  COLDAT03-HOSPID
           MOVE    COLDAT01-SRYYM      TO  COLDAT03-SRYYM
           MOVE    COLDAT01-NYUGAIKBN  TO  COLDAT03-NYUGAIKBN
           MOVE    COLDAT01-PTID       TO  COLDAT03-PTID
           MOVE    COLDAT01-RECEKA     TO  COLDAT03-RECEKA
           MOVE    PTBYO-SRYKA         TO  COLDAT03-SRYKA
           MOVE    PTBYO-SRYYMD        TO  COLDAT03-SRYYMD
           MOVE    PTBYO-RENNUM        TO  COLDAT03-RENNUM
           MOVE    PTBYO-BYOMEI        TO  COLDAT03-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI    TO  COLDAT03-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG     TO  COLDAT03-UTAGAIFLG
           MOVE    PTBYO-SYUBYOFLG     TO  COLDAT03-SYUBYOFLG   
           MOVE    PTBYO-MANSEIKBN     TO  COLDAT03-MANSEIKBN
           MOVE    PTBYO-HKNCOMBI      TO  COLDAT03-HKNCOMBI
           MOVE    PTBYO-REZEFLG       TO  COLDAT03-REZEFLG
           IF      WRK-PARA-SRYYM       <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE
               MOVE    PTBYO-TENKIKBN      TO  COLDAT03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  COLDAT03-TENKIYMD
           END-IF
           MOVE    PTBYO-HKNBYOMEIFLG  TO  COLDAT03-HKNBYOMEIFLG
           MOVE    PTBYO-BYOMEIHENFLG  TO  COLDAT03-BYOMEIHENFLG
           MOVE    PTBYO-BYOMEICD-G    TO  COLDAT03-BYOMEICD-G
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               MOVE    IDX                 TO  COLDAT03-BYOMEICDSU
           END-PERFORM
      *    ¥ì¥»¥×¥È¿ÇÎÅ²Ê
           IF      PTBYO-SRYKA     NOT =   SPACE
               INITIALIZE                      SYS-1005-REC
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE     PTBYO-SRYKA        TO  SYS-1005-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
               MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT03-RECESRYKA
           END-IF
      *
           WRITE   COLDAT03-REC
           ADD     1                   TO  CNT-COLDAT03
           .
       200221-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°Ù¥Ç¡¼¥¿ÊÔ½¸½èÍý
      *****************************************************************
       2003-SRYACCT-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  FLG-SHOSAISIN
           MOVE    ZERO                TO  FLG-SHOSAISIN-GEN
      *
      *    ¥ì¥»¥×¥È¿ÇÎÅ²Ê
           INITIALIZE                      SYS-1005-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Ç¡¼¥¿ÆÉ¤ß¹þ¤ß
           INITIALIZE                      SRYACT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 20031-SRYACT-HENSYU-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           PERFORM 200312-TOTALTEN-TAIHI-SEC
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       2003-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¾ðÊó½ÐÎÏ½èÍý
      *****************************************************************
       20031-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   5
                   OR      SRY-SRYCD (IDX2)    =   SPACE
      *
               MOVE    SPACE               TO  COLDAT02-REC
               INITIALIZE                      COLDAT02-REC
      *
      *        ¿ÇÎÅ²ñ·×¥Ç¡¼¥¿ÊÔ½¸
               MOVE    COLDAT01-KEY        TO  COLDAT02-KEY
               MOVE    ACCT-ZAINUM         TO  COLDAT02-ZAINUM
      *
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  COLDAT02-SEQNO
               MOVE    COMB-HKNID          TO  COLDAT02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  COLDAT02-TKOHID(IDX3)
               END-PERFORM
               MOVE    ACCT-SRYKA          TO  COLDAT02-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  COLDAT02-RECESRYKA
               MOVE    ACCT-SRYKBN         TO  COLDAT02-SRYKBN
               MOVE    ACCT-JIHOKBN        TO  COLDAT02-JIHOKBN
               MOVE    ACCT-ZAITEN         TO  COLDAT02-ZAITEN
               MOVE    ACCT-ZAIKAISU       TO  COLDAT02-ZAIKAISU
               MOVE    ACCT-MEISAISU       TO  COLDAT02-MEISAISU
               MOVE    ACCT-SYUTEN1        TO  COLDAT02-SYUTEN1
               MOVE    ACCT-SYUTEN2        TO  COLDAT02-SYUTEN2
               MOVE    ACCT-YKZTEN         TO  COLDAT02-YKZTEN
               MOVE    ACCT-KIZAITEN       TO  COLDAT02-KIZAITEN
               MOVE    ACCT-HKNCOMBI       TO  COLDAT02-HKNCOMBI
               MOVE    ACCT-DAY-TBL (1)    TO  COLDAT02-DAY-AREA
      *
      *        ¿ÇÎÅ¹Ô°Ù¥Ç¡¼¥¿ÊÔ½¸
               MOVE    SRY-RENNUM          TO  COLDAT02-RENNUM
               IF      SRY-SRYSYUKBN       =   SPACE
                   MOVE    ACCT-SRYKBN     TO  COLDAT02-SRYSYUKBN (1:2)
                   MOVE    "0"             TO  COLDAT02-SRYSYUKBN (3:1)
               ELSE
                   MOVE    SRY-SRYSYUKBN   TO  COLDAT02-SRYSYUKBN
               END-IF
               MOVE    SRY-JIHIMONEY(IDX2) TO  COLDAT02-JIHIMONEY
               MOVE    SRY-SRYCD    (IDX2) TO  COLDAT02-SRYCD
               MOVE    SRY-SRYSURYO (IDX2) TO  COLDAT02-SRYSURYO
               MOVE    SRY-SRYKAISU (IDX2) TO  COLDAT02-SRYKAISU
               MOVE    SRY-INPUTNUM (IDX2) TO  COLDAT02-INPUTNUM
      *
               INITIALIZE                      TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(IDX2)     TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANICD          TO  COLDAT02-TANICD
               MOVE    TNS-TANINAME        TO  COLDAT02-TANINAME
               MOVE    TNS-FORMALNAME      TO  COLDAT02-FORMALNAME
               MOVE    TNS-TENSIKIBETU     TO  COLDAT02-TENSIKIBETU
               MOVE    TNS-TEN             TO  COLDAT02-TEN
               IF      TNS-SRYCD (1:1)     =   "6"
                   MOVE    TNS-YAKKAKJNCD      TO  COLDAT02-YAKKAKJNCD
               END-IF
               IF      TNS-TENSIKIBETU     =   1   OR   3
                   IF      ACCT-NYUGAIKBN      =   "1"
                       MOVE  TNS-NYUTENTTLKBN  TO  COLDAT02-TENTTLKBN
                   ELSE
                       MOVE  TNS-GAITENTTLKBN  TO  COLDAT02-TENTTLKBN
                   END-IF
               END-IF
      *
               IF (   (SRY-SRYCD    (IDX2)(1:1)
                                               =   "8"  )
                   OR (SRY-SRYCD    (IDX2)(1:3)
                                               =   "008") ) 
               AND (   SRY-INPUTNUM (IDX2) NOT =   ZERO   )
grpsys             MOVE    SPA-HOSPNUM             TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID                TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM              TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD   (IDX2)      TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)      TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT       TO  COLDAT02-NAME
               ELSE
                   MOVE    TNS-NAME                TO  COLDAT02-NAME
               END-IF
      *
               PERFORM 200311-JNISSU-TAIHI-SEC
      *
               WRITE   COLDAT02-REC
               ADD     1               TO  CNT-COLDAT02
           END-PERFORM
           .
       20031-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ÂÆü¿ôÂàÈò
      *****************************************************************
       200311-JNISSU-TAIHI-SEC     SECTION.
      *
           IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                "6" OR  "7" OR
                                                "8"
      *        ¿ÇÎÅ¼ÂÆü¿ô¤Î¥«¥¦¥ó¥È¤ò¹Ô¤¨¤ëºÞ¤Ç¤¢¤ë¤«¤ÎÈ½Äê
               IF  ((ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                           "13"  OR  "14"     ))
               OR  ((ACCT-ZAITEN        =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                           "99"               ))
      *
                       IF ( (TNS-JITUDAY    =     2          ) AND
                            (TNS-DAYCNT     =     1   OR   2 )    )
                       OR (  SRY-SRYCD(IDX2)  =  "112700850"
                                              OR "112003350"
                                              OR "099409901"
                                              OR "099999902"
                                              OR "099110001"
                                              OR "099120001"
      *                              Æ±ÆüºÆ¿Ç
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112016550"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"
      *
                                              OR "111011810"
                                              OR "111012610"
                                              OR "113013910"
                                              OR "113014010"
                                              OR "113014110"
                                              OR "112015810"
                                              OR "112015950"
                                              OR "112016210"
                                              OR "112016410"
                                              OR "113015810"
                                              OR "101110040"
                                              OR "112016850"
                                              OR "112016950"
                                              OR "112017010"
                                              OR "112017150"
                                              OR "112017450"
      *
                                              OR "113018410"
                                              OR "111012810"
                                              OR "112017610"
                                              OR "113024810"
                                              OR "113025710"
                                              OR "112023210"
                                              OR "112023450"
                                              OR "112023750"
                                              OR "112023550"
                                              OR "112023850"
                                              OR "113031310"
                                              OR "113031410"      )
                       OR ( (SRY-SRYCD(IDX2)  =  "101120100" ) AND
                            (SRY-SRYYM       <=  "201306"    )    )
                       OR ( (SRY-SRYCD(IDX2)  =  "101120040"
                                              OR "101120050"
                                              OR "101120060"
                                              OR "101120070"
                                              OR "101120080" ) AND
                            (SRY-SRYYM       >=  "201307"    )    )
                             MOVE    1      TO    FLG-SHOSAISIN
                       END-IF
                       IF    SRY-SRYCD(IDX2)  =  "099409903"
                             MOVE    1      TO    FLG-SHOSAISIN-GEN
                       END-IF
               END-IF
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
      *    ¸øÈñ¤ÎÊ¬Î¥É½¼¨ÍÑ¥¤¥ó¥Ç¥Ã¥¯¥¹¤Î³ÎÄê
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                      IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                          MOVE    "1"       TO
                                        TBL-JNISSU-DAY(1 ACCT-IDX)
                      END-IF
                    END-PERFORM
               END-IF
               IF   FLG-SHOSAISIN-GEN    =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                      IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                          MOVE    "1"       TO
                                        TBL-JNISSU-DAY-GEN(1 ACCT-IDX)
                      END-IF
                    END-PERFORM
               END-IF
           END-IF
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£²
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(2 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(2 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£³
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(3 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(3 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£´
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(4 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(4 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹£µ
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      4)
                     IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                      IF   COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                      AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                           IF   FLG-SHOSAISIN      =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                            TBL-JNISSU-DAY(5 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                           IF   FLG-SHOSAISIN-GEN  =    1
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1
                                    UNTIL    ACCT-IDX   >   31
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO
                                     MOVE  "1"  TO
                                        TBL-JNISSU-DAY-GEN(5 ACCT-IDX)
                                 END-IF
                               END-PERFORM
                           END-IF
                      END-IF
                     ELSE
                      MOVE  10      TO     IDX3
                     END-IF
           END-PERFORM
           .
       200311-JNISSU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥È¡¼¥¿¥ëÅÀÊÔ½¸
      *****************************************************************
       200312-TOTALTEN-TAIHI-SEC   SECTION.
      *
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
             IF          ACCT-SRYKBN         =   "97"
                   COMPUTE  TBL-SKJRYOYOHI (1)
                                             =  TBL-SKJRYOYOHI (1)
                                             + (ACCT-ZAITEN
                                             *  ACCT-ZAIKAISU)
             ELSE
               IF      ( ACCT-SRYKBN         =   "95"   OR  "96" )
                OR     ( COLDAT02-SRYCD      =   "099999912" )
                   CONTINUE
               ELSE
                 IF  (ACCT-SRYCDTOTAL  =  630010002  OR  630010005
                                      OR  630010006  OR  630010007)
                 OR  (ACCT-ZAIKBN      =  2                       )
                   IF    ACCT-ZAIKBN        =    2
                         COMPUTE    WRK-TOTALTENSU(1)   =
                                                  WRK-TOTALTENSU(1) +
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                         MOVE     ZERO    TO   ACCT-ZAITEN
                         MOVE     ZERO    TO   ACCT-ZAIKAISU
                   END-IF                                              
                   COMPUTE  TBL-TOTALTENSU(1) =  TBL-TOTALTENSU(1) -
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                 ELSE
                   COMPUTE  TBL-TOTALTENSU(1) =  TBL-TOTALTENSU(1) +
                                                 (ACCT-ZAITEN  *
                                                  ACCT-ZAIKAISU)
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX3    FROM   1   BY   1
             UNTIL   (IDX3      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                  IF        ACCT-SRYKBN      =   "97"
                      COMPUTE  TBL-SKJRYOYOHI (KH-IDX)
                                             =  TBL-SKJRYOYOHI (KH-IDX)
                                             + (ACCT-ZAITEN
                                             *  ACCT-ZAIKAISU)
                  ELSE
                  IF      ( ACCT-SRYKBN         =   "95"   OR  "96" )
                   OR     ( COLDAT02-SRYCD      =   "099999912" )
                     CONTINUE
                  ELSE
                     IF  (ACCT-SRYCDTOTAL  =  630010002  OR  630010005
                                          OR  630010006  OR  630010007)
                     OR  (ACCT-ZAIKBN      =  2                       )
                         IF    ACCT-ZAIKBN        =    2
                             COMPUTE    WRK-TOTALTENSU(KH-IDX)   =
                                              WRK-TOTALTENSU(KH-IDX) +
                                              (ACCT-ZAITEN  *
                                               ACCT-ZAIKAISU)
                             MOVE     ZERO    TO   ACCT-ZAITEN
                             MOVE     ZERO    TO   ACCT-ZAIKAISU
                         END-IF
                         COMPUTE  TBL-TOTALTENSU(KH-IDX) =
                                        TBL-TOTALTENSU(KH-IDX) -
                                             (ACCT-ZAITEN *
                                              ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE  TBL-TOTALTENSU(KH-IDX) =
                                        TBL-TOTALTENSU(KH-IDX) +
                                             (ACCT-ZAITEN *
                                              ACCT-ZAIKAISU)
                     END-IF
                  END-IF
                  END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       200312-TOTALTEN-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÉ¸½àÉéÃ´³ÛÊÔ½¸
      *****************************************************************
       200313-SKJFTN-TAIHI-SEC     SECTION.
      *
           IF    (COLDAT01-HKNNUM        NOT =   SPACE)   OR
                 (COLDAT01-RJNHKNNUM     NOT =   SPACE)
                 COMPUTE  TBL-SKJFTN(1)      =   TBL-SKJFTN (1)
                                             +  (COLDAT02-ZAITEN
                                             *   COLDAT02-ZAIKAISU)
           END-IF
      *
           PERFORM    VARYING   IDX3    FROM   1   BY   1
              UNTIL   (IDX3      >      10)
                      IF   COLDAT01-HKNCOMBI(IDX3) = COLDAT02-HKNCOMBI
                           MOVE  IDX3       TO  HC-IDX
                           MOVE  10         TO  IDX3
                      END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX3    FROM   1   BY   1
             UNTIL   (IDX3      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF    COLDAT01-KOHNUMHC(HC-IDX IDX3) NOT =  SPACE
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(1)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(2)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(3)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF  COLDAT01-KOHNUMHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHHKNNUM(4)
                 AND  COLDAT01-KOHIDHC(HC-IDX IDX3) =
                                                COLDAT01-TKOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      COMPUTE  TBL-SKJFTN(KH-IDX)
                                             =   TBL-SKJFTN (KH-IDX)
                                             +  (COLDAT02-ZAITEN
                                             *   COLDAT02-ZAIKAISU)
                 END-IF
               END-IF  
           END-PERFORM
           .
       200313-SKJFTN-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   COLDATERR-FILE
           IF      STS-COLDATERR         =   ZERO
               CLOSE   COLDATERR-FILE
           ELSE
               OPEN    OUTPUT              COLDATERR-FILE
      *
               MOVE    WRK-COLDATERR         TO  COLDATERR-REC
               WRITE   COLDATERR-REC
               CLOSE   COLDATERR-FILE
      *
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-COLDATERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           PERFORM 10013-FILE-SYORI-SEC
      *
           CLOSE   COLDAT91-FILE
                   COLDAT01-FILE
                   COLDAT02-FILE
                   COLDAT03-FILE
      *
           IF      CNT-COLDAT01           =   ZERO  
               OPEN    OUTPUT              COLDAT911-FILE
               CLOSE                       COLDAT911-FILE
               MOVE    ZERO            TO  CNT-COLDAT91
           END-IF
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "COLDAT91  CNT-COLDAT91:" CNT-COLDAT91
           DISPLAY "COLDAT01  CNT-COLDAT01:" CNT-COLDAT01
           DISPLAY "COLDAT02  CNT-COLDAT02:" CNT-COLDAT02
           DISPLAY "COLDAT03  CNT-COLDAT03:" CNT-COLDAT03
           DISPLAY "*** ORCBC010 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¡£Ò£Å£Á£Ä
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-SYSKANRI
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿£Ò£Å£Á£Ä
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¡£Ò£Å£Á£Ä
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÍúÎò¸¡º÷½èÍý(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
grpsys     MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    PTINF-PTID          TO  PTNYUINRRK-PTID
           MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÍúÎòFETCH½èÍý(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           IF      COMB-HKNCOMBINUM =   9999
               MOVE    ZERO                TO  FLG-HKNCOMBI
               GO  TO  910-HKNCOMBI-INV-EXT
           END-IF
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC TO  HKNCOMBI-REC
                   IF      COMB-DELKBN =   "1"
                       MOVE    1       TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-HKNCOMBI
                   INITIALIZE              HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN  =   "1"
                       MOVE    1       TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTKOHINF-REC
                   MOVE    ZERO        TO  FLG-PTKOHINF
               ELSE
                   MOVE    1           TO  FLG-PTKOHINF
                   INITIALIZE              PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-HKNNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    HKN-HKNNUM          TO  WRK-CHK-HKNNUM
           MOVE    HKN-PAYKBN          TO  WRK-CHK-PAYKBN
           MOVE    HKN-TEKSTYMD        TO  WRK-CHK-TEKSTYMD
           MOVE    HKN-TEKEDYMD        TO  WRK-CHK-TEKEDYMD
      *
           INITIALIZE                      HKNNUM-REC
      *
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
      *
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
                   OR      HKN-HKNNUM        NOT =   SPACE
               IF    ( WRK-CHK-HKNNUM  =
                                       TBL-HKN-HKNNUM (WRK-HKNNUM-IDX) )
               AND   ( WRK-CHK-PAYKBN  =
                                       TBL-HKN-PAYKBN (WRK-HKNNUM-IDX) )
               AND   ( TBL-HKN-TEKSTYMD (WRK-HKNNUM-IDX)
                                       <=  WRK-CHK-TEKSTYMD            )
               AND   ( TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)
                                       >=  WRK-CHK-TEKEDYMD            )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
grpsys         MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    WRK-CHK-HKNNUM      TO  HKN-HKNNUM
               MOVE    WRK-CHK-PAYKBN      TO  HKN-PAYKBN
               MOVE    WRK-CHK-TEKSTYMD    TO  HKN-TEKSTYMD
               MOVE    WRK-CHK-TEKEDYMD    TO  HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_hknnum"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
      *            ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸
                   IF      TBL-HKNNUM-CONST-MAX
                                               >   WRK-HKNNUM-MAX
                       COMPUTE WRK-HKNNUM-MAX
                                       =   WRK-HKNNUM-MAX  +   1
                       MOVE    HKNNUM-REC      TO
                                   TBL-HKNNUM-REC (WRK-HKNNUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBCLOSE-SEC
           END-IF
           .
       900-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÈÖ¹æ¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   MOVE    1           TO  FLG-PTINF
                   INITIALIZE              PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCOM-REC
                   MOVE    ZERO        TO  FLG-PTCOM
               ELSE
                   MOVE    1           TO  FLG-PTCOM
                   INITIALIZE              PTCOM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-PARA-SRYYM      TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "DBFETCH"   TO  MCP-FUNC
                   MOVE    "tbl_tensu" TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBCLOSE-SEC
           END-IF
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÏ«ºÒÊÝ¸±¾ðÊó¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN =   "1"
                       MOVE    1           TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE             PTRSIINF-REC
                   MOVE    1           TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSE-SEC
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿£Ò£Å£Á£Ä
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊóÆÉ¹þ
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÌÀºÙ½ñÆÉ¹þ
      *****************************************************************
       900-COLDAT01-READ-SEC             SECTION.
      *
           READ    COLDAT01-FILE
               INVALID
                   MOVE    1           TO  FLG-COLDAT01
               NOT INVALID
                   MOVE    ZERO        TO  FLG-COLDAT01
           END-READ
           .
       900-COLDAT01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ã£Ì£Ï£Ó£Å½èÍý
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
