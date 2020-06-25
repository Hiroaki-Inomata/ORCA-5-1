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
      *******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC70200604.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯¡¢»»Äê¡Ê²èÁü¿ÇÃÇ¡Ë
      *                      ¥ì¥»ÅÅÂÐ±þ¡Ê£È£±£·Ç¯¡©·î¤«¤é¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/12/01    MCC¡ÝÂ¿¡¹Ç¼   ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     MCC-Â¿¡¹Ç¼   01/05/23  ¥Õ¥£¥ë¥àÎÁ¡¢ÌôºÞÎÁ·×»»½¤Àµ
      * 01.00.02     MCC-Â¿¡¹Ç¼   02/03/18  »ÜÀßÉÔÅ¬¹çÄÉ²Ã
      * 01.00.03     MCC-Â¿¡¹Ç¼   02/04/25  »£±ÆÎÁ¤Ë²ó¿ôÆþÎÏÄÉ²Ã
      * 01.00.04     MCC-Â¿¡¹Ç¼   02/04/**  Ï«ºÒÍÑÄÉ²Ã
      * 01.00.05     NACL-Â¿¡¹Ç¼  02/09/27  ¿ÇÃÇ¼«Æ°È¯À¸ÄÉ²Ã
      * 01.00.06     NACL-Â¿¡¹Ç¼  02/10/29  Æþ±¡ÂÐ±þ
      * 01.00.07     NACL¡ÝÂ¿¡¹Ç¼ 02/11/22  ÅÀÅ©Ãí¼ÍÆ±»þ½ÅÊ£¥¨¥é¡¼ÄÉ²Ã
      * 01.00.08     NACL¡ÝÂ¿¡¹Ç¼ 02/12/24  ¥Õ¥£¥ë¥àÎÁ¤ò¼êµ»ÎÁ¤Ø²Ã»»
      * 01.00.09     NACL-Â¿¡¹Ç¼  03/01/16  »£±ÆÎÌ²ó¿ôÄûÀµ»þ½¤Àµ
      * 01.00.11     NACL-Â¿¡¹Ç¼  03/02/18  ¥Õ¥£¥ë¥àÎÁ¤ò¼êµ»ÎÁ£²¤Ø
      * 01.00.12     NACL-Â¿¡¹Ç¼  03/02/26  ÅÀ¿ô¥¼¥í¤Î¤­¤¶¤ßÃÍ·×»»¤Î½¤Àµ
      * 01.00.13     NACL-Â¿¡¹Ç¼  03/04/25  Ã±½ã»£±Æ¤Ë´ÖÀÜ»£±Æ¤Ê¤É¤òÄÉ²Ã
      * 01.00.14     NACL-Â¿¡¹Ç¼  03/07/30  Æ±»þÊ»»ÜÅÀ¿ô·×»»½¤ÀµÂ¾
      * 01.00.15     NACL-Â¿¡¹Ç¼  03/09/26  £Ã£Ô¡¢£Í£Ò£ÉÆ±»þÆþÎÏÄÉ²Ã
      * 01.00.16     NACL-Â¿¡¹Ç¼  03/12/03  Æ±»þÊ»»Ü¤ÎÃ±ÆÈÆþÎÏ·Ù¹ð¤Ø
      * 01.00.17     NACL-Â¿¡¹Ç¼  04/12/17  £Ã£Ô¡¢£Í£Ò£É¤ÎÂ¤±ÆºÞÈ½Äê½¤Àµ
      * 01.00.18     NACL-Â¿¡¹Ç¼  05/01/27  »ÀÁÇÄÉ²Ã
      * 02.00.01     NACL-Â¿¡¹Ç¼  05/04/27  ¥ì¥»ÅÅÂÐ±þ
      * 02.00.02     NACL-Â¿¡¹Ç¼  05/12/14  MONFUNC ÂÐ±þ
      * 02.00.03     NACL-Â¿¡¹Ç¼  06/03/16  £È£±£¸Ç¯£´·î²þÀµÄÉ²Ã
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
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
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-BUIKBN          PIC 9(01).
           03  FLG-JIKASAN         PIC 9(01).
      *    ÆýÍÄ»ù²Ã»»Í­¤ê
           03  FLG-NYUYOJI         PIC 9(01).
      *    Â¤±ÆºÞ²Ã»»Í­¤ê
           03  FLG-ZOEIZAI         PIC 9(01).
      *    ¼«Æ°ÄÉ²Ã¼ïÊÌ
           03  FLG-SATU             PIC 9(01).
      *    £Ã£ÔÍ­¤ê
           03  FLG-XSEN             PIC 9(01).
           03  FLG-KAKUI            PIC 9(01).
           03  FLG-CTMRI            PIC 9(01).
      *
           03  FLG-SINDAN           PIC 9(01).
      *
           03  FLG-CHK              PIC 9(01).
      *
           03  FLG-SYORI            PIC 9(01).
      *    »ÜÀß´ð½àÉÔÅ¬¹ç
           03  FLG-KIJUNN          PIC 9(01).
      *    ±ó³Ö²èÁü¿ÇÃÇ²Ã»»
           03  FLG-ENKALL          PIC 9(01).
           03  FLG-ENK1            PIC 9(01).
           03  FLG-ENK2            PIC 9(01).
           03  FLG-ENK3            PIC 9(01).
      *    ÅÀÅ©¤Î¥Á¥§¥Ã¥¯
           03  FLG-TENTEKI         PIC 9(01).
      *    ¿ÇÃÇÎÁÅÀ¿ôÄþ¸º
           03  FLG-SATU-GEN        PIC 9(01).
      *
           03  FLG-NENREI          PIC 9(01).
           03  FLG-SYUGI           PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDXS1               PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SAT             PIC 9(04).
           03  IDX-SAT2            PIC 9(04).
           03  IDX-SAT3            PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-KAS             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
      *
           03  IDX-GEN             PIC 9(04).
           03  IDX-GEN2            PIC 9(04).
           03  IDX-NYU             PIC 9(04).
           03  IDX-GAI             PIC 9(04).
      *
           03  IDX-9               PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-CT              PIC 9(04).
           03  IDX-BUI             PIC 9(04).
      *    £Í£Á£Ø¹Ô
           03  IDX-MAX             PIC 9(04).
      *    £ØÀþ¤Î³«»Ï¹Ô
           03  IDX-HOZ             PIC 9(04).
           03  IDX-HOZ2            PIC 9(04).
      *    ¹Ôºï½ü
           03  IDX-D1              PIC 9(04).
           03  IDX-D2              PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
      *    Æ±»þÊ»»Ü
           03  IDX-S1              PIC 9(04).
           03  IDX-DOJI1           PIC 9(04).
           03  IDX-DOJI2           PIC 9(04).
      *
           03  IDX-XSEN             PIC 9(04).
           03  IDX-KAKUI            PIC 9(04).
           03  IDX-CTMRI            PIC 9(04).
      *
           03  IDX-STR              PIC 9(04).
           03  HZN-IDX              PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-MOJI            PIC X(01). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-SRYCD-2         PIC X(09). 
           03  WRK-HEN-SRYCD       PIC X(09). 
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSU           PIC 9(08)V99.
           03  WRK-TENSU1          PIC 9(08).
           03  WRK-IDX             PIC 9(04).
           03  WRK-TENSUKEI        PIC 9(08).
      *
           03  WRK-SRYCD1          PIC X(09). 
           03  WRK-SRYCD2          PIC X(09). 
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-SUM-TENSUKEI    PIC 9(08)V99.
               05  WRK-SUM-TENGAKEI    PIC 9(08)V99.
      *        »£±ÆÎÁ¤ÎÅÀ¿ô·×
               05  WRK-TENSUKEI1       PIC S9(08)V999.
      *        ¿ÇÃÇÎÁ¤ÎÅÀ¿ô·×
               05  WRK-TENSUKEI2       PIC S9(08)V999.
      *        ¤½¤ÎÂ¾¤ÎÅÀ¿ô·×
               05  WRK-TENSUKEI3       PIC S9(08)V999.
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V9999.
           03  WRK-SYOKEIS3        PIC 9(08)V9999.
           03  WRK-SYOKEI          PIC 9(08).
      *    ÅÀ¿ô·×»»ÍÑ
           03  WRK-KISOTEN-9           PIC 9(07)V9(03).
           03  WRK-KISOTEN             PIC 9(07).
           03  WRK-KISOTEN-G           PIC 9(07).
      *    Äþ¸ºÅÀ¿ô
           03  WRK-GEN-TEN             PIC 9(07)V9(03).
           03  WRK-GENTEN              PIC 9(07)V9(03).
      *    »£±ÆÎÁ²ó¿ô
           03  WRK-CNT-SATUEI          PIC 9(03).
      *
      *    ÆýÍÄ»ù²Ã»»ÍÑ¥ï¡¼¥¯
           03  WRK-NYUYOJI-AREA.
               05  WRK-NYU-BUI     PIC X(01).
               05  WRK-NYU-CD      PIC X(01).
               05  WRK-NYU-FIR     PIC X(01).
               05  WRK-NYU-IDX     PIC 9(04).
               05  WRK-NYU-TEN     PIC 9(08).
      *
               05  WRK-FIRTEN      PIC 9(08).
      *
           03  WRK-TENSUKEIS       PIC 9(08)V999.
           03  WRK-TENSUKEI-9      PIC S9(08).
           03  WRK-TENSUKEI-9G     PIC S9(08).
           03  WRK-TENSUKEI-G      PIC S9(08).
           03  WRK-TENSUKEI-K      PIC S9(08).
      *
           03  WRK-FIRTEN-KEI      PIC 9(08).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
      *
      *   ¤­¤¶¤ßÃÍ¹Ô°ÙºÆ·×»»ÍÑ
           03  WRK-SATU-TEN        PIC 9(08)V9(03).
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-KZMTEN          PIC 9(08)V9(03).
      *    ÆýÍÄ»ù²Ã»»³ä¹ç
           03  WRK-SATU-KASAN      PIC 9(03)V9(03).
      *    »£±ÆÎÁ¤Î¿ôÎÌ¡Ê¿ÇÃÇÎÁ¤Ø¡Ë
           03  WRK-SATUEI-SURYO    PIC 9(05)V9(03).
      *
      *    ¸º¥³¡¼¥É
           03 WRK-GEN-SRYCD        PIC X(09).
      *
      *    £ØÀþ¿ÇÃÇÎÁ
           03  WRK-XSEN-SRYCD-I     PIC X(09).
           03  WRK-XSEN-SRYCD-R     PIC X(09).
      *
      *    £Ã£Ô¡¤£Í£Ò£É²ó¿ô
           03  WRK-CTMRI-CNT-G.
               05  WRK-CTMRI-CNT-TBL            OCCURS  3.
                   07  WRK-CTMRI-CNT   PIC 9(04).
                   07  WRK-CTMRI-DCNT  PIC 9(04).
           03  WRK-CTMRI-KAI       PIC 9(03).
      *
      *    ·Ù¹ð¥á¥Ã¥»¡¼¥¸
           03  WRK-KIDCD           PIC X(04).
      *
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-SRYKA           PIC X(02).
           03  WRK-HKNCOMBI        PIC X(04).
      *!
      *    »ÀÁÇ·×»»ÍÑ
           03  WRK-SANSO               PIC 9(08).
           03  WRK-SANSOKIN            PIC 9(08)V99.
           03  WRK-SANKEI              PIC 9(08).
           03  WRK-SANTEN              PIC 9(08).
           03  WRK-SANSOKIN-SUM        PIC 9(08)V99.
           03  IDX-SANSO               PIC 9(04).
      *
      *    Æþ±¡Æü
       01  WRK-NYUINDAY-AREA.
           03  WRK-XSEN-DAY            PIC 9(02).
           03  WRK-CTMRI-DAY           PIC 9(02).
           03  WRK-KAKUI-DAY           PIC 9(02).
           03  WRK-NYU-DAY             PIC 9(02).
           03  IDN1                    PIC 9(02).
           03  IDN2                    PIC 9(02).
           03  FLG-XSEN-ARI            PIC 9(01).
           03  FLG-CTMRI-ARI           PIC 9(01).
           03  FLG-KAKUI-ARI           PIC 9(01).
      *
      *    ÊÝÂ¸ÍÑ
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *»þ´Ö³°¥³¥á¥ó¥È¡ÊÆü»þÊ¬¡Ë
       01  CONS-JIKAN                  PIC X(09)   VALUE   "840000114".
      *
      *   £ØÀþÆ±»þÊ»»Ü¡Ê¿ÇÃÇ¡¦»£±Æ¡Ë
       01  TBL-DOUJI-HEISI-AREA.
      *    ÆÃ¼ì»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170005530".
           03  FILLER              PIC X(09)  VALUE   "170005630".
      *    Æ°ÂÖ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170005730".
           03  FILLER              PIC X(09)  VALUE   "170005830".
      *    ½ÅÊ£»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006130".
           03  FILLER              PIC X(09)  VALUE   "170006230".
      *    Î©ÂÎ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006330".
           03  FILLER              PIC X(09)  VALUE   "170006430".
      *    ¥Ñ¥ó¥È¥â¥°¥é¥Õ¥£¡¼
           03  FILLER              PIC X(09)  VALUE   "170006530".
           03  FILLER              PIC X(09)  VALUE   "170006630".
      *    ¥Ñ¥Î¥é¥é¥Þ£ØÀþ
           03  FILLER              PIC X(09)  VALUE   "170006730".
           03  FILLER              PIC X(09)  VALUE   "170006830".
      *    ÃÇÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006930".
           03  FILLER              PIC X(09)  VALUE   "170007030".
      *    Æ±»þÂ¿ÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007130".
           03  FILLER              PIC X(09)  VALUE   "170007200".
      *    ²óÅ¾²£ÃÇ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007330".
           03  FILLER              PIC X(09)  VALUE   "170007430".
      *    ¥¹¥Ý¥Ã¥È»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007530".
           03  FILLER              PIC X(09)  VALUE   "170007630".
      *    ÁÀ·â»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007730".
           03  FILLER              PIC X(09)  VALUE   "170007830".
      *    ¶ÊÌÌÃÇÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007930".
           03  FILLER              PIC X(09)  VALUE   "170008030".
      *    »ùÆ¬¹üÈ×ÉÔ¶Ñ¹Õ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170008430".
           03  FILLER              PIC X(09)  VALUE   "170008530".
      *    ÆÃ¼ì»£±Æ¡Ê»£±Æ¡¦¿ÇÃÇ¡Ë¿·À¸»ù
           03  FILLER              PIC X(09)  VALUE   "170019130".
           03  FILLER              PIC X(09)  VALUE   "170019330".
      *    ÆÃ¼ì»£±Æ¡Ê»£±Æ¡¦¿ÇÃÇ¡ËÆýÍÄ»ù
           03  FILLER              PIC X(09)  VALUE   "170019230".
           03  FILLER              PIC X(09)  VALUE   "170019430".
      *****Ã±½ã»£±Æ¡¦ÆÃ¼ì»£±Æ
      *    03  FILLER              PIC X(09)  VALUE   "170001910".
      *****03  FILLER              PIC X(09)  VALUE   "170002010".
      *
       01  TBL-DOUJI-HEISI-R       REDEFINES  TBL-DOUJI-HEISI-AREA.
           03  TBL-DOUJI-OCC       OCCURS     15  INDEXED  TBL-IDX.
               05  TBL-DOUJI-1     PIC X(09).
               05  TBL-DOUJI-2     PIC X(09).
      *
      *    Ã±½ã»£±Æ
       01  TBL-XSEN-AREA.
      *    Ã±½ã»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170001910".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170000410".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R 170000510".
      *
      *    Ã±½ã»£±Æ¡Ê¼êÁ°¡Ë
           03  FILLER              PIC X(09)  VALUE   "170021750".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170021550".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R          ".
      *
      *    Ã±½ã»£±Æ¡ÊÃ»´ü¡Ë
           03  FILLER              PIC X(09)  VALUE   "170023950".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170023750".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R 170024250".
      *
      *    Ã±½ã´ÖÀÜ
           03  FILLER              PIC X(09)  VALUE   "170002410".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170000910".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R 170001010".
      *
      *    Ã±½ã´ÖÀÜ¡Ê¼êÁ°¡Ë
           03  FILLER              PIC X(09)  VALUE   "170021850".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170021650".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R          ".
      *
      *    Ã±½ã´ÖÀÜ¡ÊÃ»´ü¡Ë
           03  FILLER              PIC X(09)  VALUE   "170024050".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170023850".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R 170024350".
      *
      *    ÆýË¼»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170027010".
      *    ¡Ê¥¤¡Ë
           03  FILLER              PIC X(11)  VALUE   "I 170026910".
      *    ¡Ê¥í¡Ë
           03  FILLER              PIC X(11)  VALUE   "R 170026910".
      *
      *
       01  TBL-XSEN-AREA-R         REDEFINES  TBL-XSEN-AREA.
           03  TBL-XSEN-OCC        OCCURS   7   INDEXED  TBL-XIDX.
               05  TBL-XSEN-SRYCD      PIC X(09).
               05  TBL-XSEN-KBN1       PIC X(02).
               05  TBL-XSEN-SRYCD-I    PIC X(09).
               05  TBL-XSEN-KBN2       PIC X(02).
               05  TBL-XSEN-SRYCD-R    PIC X(09).
      *
      *    Ç¯ÎðÊÌ¥³¡¼¥É¡Ê¿ÇÃÇ¡¦»£±Æ¡Ë
       01  TBL-DOUJI-NEBREI-AREA.
      *    ÆÃ¼ì»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170005530".
           03  FILLER              PIC X(09)  VALUE   "170019130".
           03  FILLER              PIC X(09)  VALUE   "170019230".
      *    ÆÃ¼ì»£±Æ¡ÊÆ±»þÊ»»Ü¡Ë
           03  FILLER              PIC X(09)  VALUE   "170005630".
           03  FILLER              PIC X(09)  VALUE   "170019330".
           03  FILLER              PIC X(09)  VALUE   "170019430".
      *    £É¡¥£É´ÖÀÜ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170011430".
           03  FILLER              PIC X(09)  VALUE   "170019730".
           03  FILLER              PIC X(09)  VALUE   "170019830".
      *
       01  TBL-DOUJI-NEBREI-AREA-R   REDEFINES  TBL-DOUJI-NEBREI-AREA.
           03  TBL-DOUJI-NEBREI-OCC    OCCURS   3   INDEXED  TBL-NIDX.
      *        °ìÈÌ
               05  TBL-NEN-SRYCD0      PIC X(09).
      *        ¿·À¸»ù
               05  TBL-NEN-SRYCD1      PIC X(09).
      *        ÆýÍÄ»ù
               05  TBL-NEN-SRYCD2      PIC X(09).
      *
      *    »ÀÁÇ¥³¡¼¥É¥Æ¡¼¥Ö¥ë
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê»ÜÀß´ð½à¡Ë
           COPY    "CPSK1006.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÊÔ½¸¥Ñ¥é¥á¥¿
           COPY    "CPORCSC90TEN.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÎò¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSC91.INC".
      *
      *    ¿ÇÎÅ¹Ô°ÙÅÀ¿ôÊÔ½¸¡¦´ðËÜ¥Á¥§¥Ã¥¯
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *    ¿ÇÎÅ¹Ô°ÙÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "K01COMMON-SPA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
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
      *
           EVALUATE    LNK-ORCSC-KBN
      *        ºÞÊÌ¡¡¥Á¥§¥Ã¥¯¡¢ÊÔ½¸¡¢¼«Æ°»»Äê
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        Á´ÂÎ¤Î¥Á¥§¥Ã¥¯
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        ¼«Æ°»»Äê
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ºÞÊÌ¤Î¥Á¥§¥Ã¥¯¡¢ÊÔ½¸¡¢¼«Æ°»»Äê½èÍý
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»Äê
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    ÅÀ¿ô·×»»
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙÊÌÆâÍÆ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»ÄêÊÔ½¸½èÍý
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-BUI
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDX-SAT
           MOVE    ZERO                TO  IDX-GEN
           MOVE    ZERO                TO  IDX-GEN2
           MOVE    ZERO                TO  IDX-NYU
           MOVE    1                   TO  IDX-HOZ
      *
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  FLG-SATU
           MOVE    ZERO                TO  FLG-BUIKBN
           MOVE    ZERO                TO  FLG-ZOEIZAI
           MOVE    ZERO                TO  FLG-SINDAN
           MOVE    ZERO                TO  FLG-KIJUNN
           MOVE    ZERO                TO  WRK-KEIMAI
           INITIALIZE                  WRK-NYUYOJI-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )  OR
                              (WRK-ERRCD       NOT =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                       OR  "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                ÊÔ½¸¥Á¥§¥Ã¥¯¤Ø
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
           IF      IDX-SAT             >   ZERO
      *        ¼«Æ°²Ã»»¤òÄÉ²Ã¤¹¤ë
               PERFORM 1002-JIDO-KASAN-SEC
           END-IF
      *
      *    »ÀÁÇ¤ÎÊäÀµÎ¨¤ò¼«Æ°È¯À¸¤¹¤ë
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KIDCD           TO  WRK-ERRCD
           END-IF
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙÊÌÆâÍÆ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»ÄêÊÔ½¸½èÍý
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    ³Æ¥Á¥§¥Ã¥¯½èÍý
           EVALUATE    WRK-MOJI
               WHEN    "0"
      *            Éô°Ì¥³¡¼¥É¥Á¥§¥Ã¥¯
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3)   =   "002"
                       IF      FLG-BUIKBN      NOT =   ZERO
                           MOVE    "0011"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z)
                       ELSE
                           EVALUATE    WRK-COM-BUIKBN (IDX-Z)
      *                        Æ¬Éô
                               WHEN    "1"
      *                        ¶í´´
                               WHEN    "2"
                                   MOVE    2       TO  FLG-BUIKBN
      *                        ¶»Éô¡¦Ê¢Éô
                               WHEN    "5"
                               WHEN    "6"
                                   MOVE    2       TO  FLG-BUIKBN
                                   MOVE    "1"     TO  WRK-NYU-BUI
      *                        ÀÔ¿ñ
                               WHEN    "7"
                                   MOVE    2       TO  FLG-BUIKBN
      *                        ¾Ã²½´ï
                               WHEN    "8"
                                   MOVE    3       TO  FLG-BUIKBN
      *                        ¤½¤ÎÂ¾
                               WHEN    OTHER
                                   MOVE    1       TO  FLG-BUIKBN
                           END-EVALUATE
                       END-IF
                       MOVE    IDX-Z           TO  IDX-BUI
                   END-IF
      *
               WHEN    "1"
      *            »£±ÆÎÁ
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       PERFORM 10010-SYUGICHK-SEC
                   END-IF
      *            ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸ºÍ­¤ê¡Ê´ð½àÉÔÅ¬¹çÄþ¸º¶èÊ¬°Ê³°
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN     (IDX-Z) =   SPACE )
                       MOVE    IDX-Z               TO  IDX-GEN
                   END-IF
      *            ¼êµ»ÎÁ ²Ã»»¥Á¥§¥Ã¥¯
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 10011-KASAN-CHK-SEC
                   END-IF
      *            Ã±½ã»£±ÆÈ½Äê¡ÊÆýÍÄ»ù²Ã»»¡Ë¤ÎÍ­Ìµ
                   IF      WRK-ERRCD           =   SPACE
                       PERFORM 1008-XSEN-NYUYOUJI-CHK-SEC
                   END-IF
      *            ÆýÍÄ»ù²Ã»»¡ÊÅÀ¿ô)
                   IF     (WRK-COM-DATAKBN (IDX-Z) =   "1" )
                     AND  (WRK-ERRCD           =   SPACE   )
                       MOVE    IDX-Z               TO  IDX-SYU
                       PERFORM 1007-NYUYOJI-SET-SEC
                   END-IF
      *            »ÜÀß´ð½àÉÔÅ¬¹ç¥Á¥§¥Ã¥¯
                   IF     (WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                           NOT =   ZERO  )
                     AND  (WRK-ERRCD           =   SPACE )
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *        ÌôºÞ
               WHEN    "6"
      *            Â¤±ÆºÞ
                   IF      WRK-COM-ZOEIZAIKBN(IDX-Z)   =   1
      *                ÆâÉþÌôºÞ¤ÏÂÐ¾Ý³°¤È¤¹¤ë
                       IF      WRK-COM-YKZKBN (IDX-Z)  =   "1"
                           CONTINUE
                       ELSE
                           MOVE   1                TO  FLG-ZOEIZAI
                       END-IF
                   END-IF
      *        ´ïºà
               WHEN    "7"
      *            ¥Õ¥£¥ë¥àÈ½Äê
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "3"
      *                ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬¤ÎÈ½Äê
                       IF      WRK-COM-TOKUKIZAIAGEKSNKBN (IDX-Z)
                                                       =   1
                           MOVE    "1"                 TO  WRK-NYU-FIR
                       END-IF
      *                £ØÀþ¤Î¿ôÎÌÊÑ¹¹¤Î°Ù¤ÎËç¿ô½¸·×
                       IF      WRK-COM-BUNGSU (IDX-Z)  =  ZERO
                           MOVE    1               TO  WRK-COM-BUNGSU
                                                               (IDX-Z)
                       END-IF
                       COMPUTE WRK-KEIMAI      =   WRK-KEIMAI
                                               +  (WRK-COM-SURYO(IDX-Z)
                                               *   WRK-COM-BUNGSU
                                                               (IDX-Z))
                   END-IF
      *            »ÀÁÇ¶èÊ¬È½Äê
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                »ÀÁÇ¤Î»þ¡¢»ÀÁÇÊäÀµÎ¨¤ò¼«Æ°²Ã»»
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  WRK-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¼êµ»ÎÁÈ½Äê½èÍý
      *****************************************************************
       10010-SYUGICHK-SEC           SECTION.
      *
           EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)
               WHEN    "721"
               WHEN    "720"
      *            £ØÀþ»£±ÆÎÁ
      *             Éô°Ì¶èÊ¬¤¬¤¢¤ë¤³¤È
                   IF      WRK-COM-TNSSRYSYUKBN(IDX-Z)   =   "721"
                     IF      FLG-BUIKBN      =   ZERO
                       MOVE    "0036"      TO  WRK-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                     END-IF
                   ELSE
                       MOVE    1           TO  FLG-BUIKBN
                   END-IF
      *
                   IF      FLG-SINDAN          =   1
                       PERFORM 1002-XSEN-END-SEC
                   ELSE
                       IF      FLG-SINDAN      =   ZERO
      *                    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯Åù
                           PERFORM 10013-DOUJI-CHK-SEC
                       ELSE
      *                    Â¾¤Î»£±Æ
                           MOVE    "0071"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                                (IDX-Z)
                       END-IF
                   END-IF
                   MOVE    1               TO  FLG-SINDAN
                   MOVE    IDX-Z           TO  IDX-SAT
                   ADD     1               TO  WRK-CNT-SATUEI
               WHEN    "722"
      *            ³Ë°å³Ø¿ÇÃÇÎÁ
                   IF      FLG-SINDAN      NOT =   ZERO
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       MOVE    "0071"          TO  WRK-ERRCD
                   ELSE
                       MOVE    2               TO  FLG-SINDAN
                       MOVE    IDX-Z           TO  IDX-SAT
                   END-IF
               WHEN    "723"
      *            £Ã£Ô»£±Æ
                   IF      FLG-SINDAN      NOT =   ZERO
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                       MOVE    "0071"          TO  WRK-ERRCD
                   ELSE
                       MOVE    3               TO  FLG-SINDAN
                       MOVE    IDX-Z           TO  IDX-SAT
                       MOVE    1               TO  FLG-CTMRI
                   END-IF
                WHEN    "724"
      *            £Í£Ò£É»£±Æ
                   IF      FLG-SINDAN      NOT =   ZERO
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                       MOVE    "0071"          TO  WRK-ERRCD
                   ELSE
                       MOVE    3               TO  FLG-SINDAN
                       MOVE    IDX-Z           TO  IDX-SAT
                       MOVE    2               TO  FLG-CTMRI
                   END-IF
               WHEN    "711"
               WHEN    "710"
      *            Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯Åù
                   PERFORM 10015-SIDAN-DOUJI-CHK-SEC
               WHEN    "712"
               WHEN    "713"
               WHEN    "714"
      *            £ØÀþ¿ÇÃÇÎÁ
                   CONTINUE
               WHEN    "700"
      *            ¤½¤ÎÂ¾¡Ê¤É¤Î¿ÇÃÇÎÁ¤Ë¤âÉÕ¿ï¤¹¤ë¡Ë
                   IF      FLG-SINDAN          =   4
                       CONTINUE
                   ELSE
                       IF      FLG-SINDAN  NOT =   ZERO
                           MOVE    "0071"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
                   END-IF
      *            Æ©»ë¿ÇÃÇ¤Ï£ØÀþ¤È¤¹¤ë
                   IF     WRK-COM-INPUTCD (IDX-Z)
                                           =   "170000310"
                       CONTINUE
                   ELSE
                       MOVE    4               TO  FLG-SINDAN
                       MOVE    IDX-Z           TO  IDX-SAT
                  END-IF
           END-EVALUATE
      *
           .
       10010-SYUGICHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÀÁÇÈ½Äê½èÍý
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO           TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    WRK-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH 
      *
           .
       100115-SANSO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÀÁÇÊäÀµÎ¨¼«Æ°²Ã»»½èÍý
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE  )  OR
                              (WRK-COM-INPUTCD(IDX1)   =   SPACE AND
                               WRK-GMN-INPUTCD(IDX1)   =   SPACE )
               IF     (WRK-COM-INPUTCD  (IDX1)(1:1)    =   "7" ) AND
                      (WRK-COM-SANSOKBN (IDX1) =   1           )
                   IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD OR
                                                       "770030070"
                       CONTINUE
                   ELSE
                       IF      IDX-SANSO       NOT =   IDX1
                           MOVE    IDX1                TO  IDX-SANSO
                       END-IF
                   END-IF
               END-IF
               IF      WRK-COM-INPUTCD (IDX1)  =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               MOVE    IDX-SANSO           TO  IDX-S
           END-IF
      *    »ÀÁÇÊäÀµÎ¨¼«Æ°²Ã»»
           MOVE    ZERO                TO  FLG-NENREI
           PERFORM 1002-JIDOUSET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *!
      *
      *****************************************************************
      *    »ÜÀß´ð½àÉÔÅ¬¹ç¥Á¥§¥Ã¥¯²Ã»»½èÍý
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
      *    ¼êµ»¤Î»þ¡¢Å¬¹ç¥Á¥§¥Ã¥¯
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        »ÜÀß´ð½àÅ¬±þ
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-KIJUNN
                   GO      TO      100111-KIJUNN-CHK-EXT
               END-IF
      *        »ÜÀß´ð½àÉÔÅ¬±þ
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    2                   TO  FLG-KIJUNN
      *            »ÜÀß´ð½àÉÔÅ¬¹ç¡Ê²èÁü¿ÇÃÇ¡Ë
                   MOVE    "170025170"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       COMPUTE IDX-GEN2            =   IDX-Z  +  1
                   ELSE
                       MOVE    IDX-Z               TO  IDX-S
                       MOVE    ZERO                TO  FLG-ARI
                       PERFORM 1002-JIDOUSET-SEC
                       MOVE    IDX-S               TO  IDX-GEN2
                  END-IF
              END-IF
           END-IF
      *
      *    ²Ã»»¤Î»þ¡¢ÂÐ¾Ý¤Î¼êµ»¤¬Í­¤ë¤³¤È
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0073"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100111-KIJUNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÜÀß´ð½àÉÔÅ¬±þ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       1001111-SSTKIJUN-CHK-SEC        SECTION.
      *
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
      *
           MOVE    ZERO                TO  FLG-ERR
      *    »ÜÀß´ð½à¾ðÊó¸¡º÷
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           IF      IDY                 <=  500
               MOVE    "1"                 TO  SYS-1006-KBNCD
           ELSE
               MOVE    "2"                 TO  SYS-1006-KBNCD
           END-IF
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    ´ð½àÉÔÅ¬¹çÄþ¸ºÂÐ¾Ý»ÜÀß´ð½à
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
           IF      IDY                 <=  500
               IF      SYS-1006-SSTKIJUN (IDY) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           ELSE
               IF      SYS-1006-SSTKIJUN (IDY - 500) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°²Ã»»ÄÉ²Ã½èÍý
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
      *    £ØÀþ¤Î»þ¡¢¥¨¥é¡¼¤Ê¤é¿ÇÃÇÎÁ¤Î¼«Æ°È¯À¸¤ò¤ª¤³¤Ê¤¦
           IF     (FLG-SINDAN          =   1     )
      *??? AND    (WRK-ERRCD       NOT =   SPACE )
               PERFORM 10021-XSEN-SINDAN-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HEN-SRYCD
           IF      FLG-SINDAN          =   3
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»ÄêÍÑ¥³¡¼¥ÉÊÔ½¸
               MOVE    IDX-SAT             TO  IDX-Z
               PERFORM 10023-CTMRIHEN-SEC
           END-IF
           IF     (FLG-SINDAN          =   3     )  AND
                  (LNK-ORCSC-PG        =   "K02" )  AND
                  (LNK-ORCSC-SYORIFLG  =   ZERO  )  AND
                  (WRK-HEN-SRYCD   NOT =   SPACE )
      *        £Ã£Ô¡¥£Í£Ò£É¤Î¼êµ»ÎÁÊÑ¹¹¥Á¥§¥Ã¥¯
               PERFORM 10012-SYUGI-CHK-SEC
           END-IF
      *
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»ÄêÍÑ¥³¡¼¥ÉÊÑ´¹½èÍý
      *****************************************************************
       10023-CTMRIHEN-SEC                  SECTION.
      *
      *
           MOVE    SPACE               TO  WRK-HEN-SRYCD
      *    Êñ³çÄþ¸º¶èÊ¬È½Äê¡Ê£Ë£°£³¤Ç¤âÆ±ÍÍ¤ÎÈ½Äê¤ò¤·¤Æ¤¤¤ë¡Ë
           EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *        Æ¬Éô
               WHEN    "201"
                   MOVE    "099700101"         TO  WRK-HEN-SRYCD
      *        ¶í´´
               WHEN    "202"
                   MOVE    "099700102"         TO  WRK-HEN-SRYCD
      *        »Í»è
               WHEN    "203"
                   MOVE    "099700103"         TO  WRK-HEN-SRYCD
           END-EVALUATE
           IF      WRK-HEN-SRYCD       =   SPACE
               MOVE    SPACE           TO  WRK-COM-CHK-SRYCD (IDX-Z)
           ELSE
               MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD (IDX-Z)
           END-IF
      *    £Ã£Ô¡¦£Í£Ò£É¿ÇÃÇÎÁ
           PERFORM 100231-CTMRI-SINDAN-SEC
      *
           .
       10023-CTMRIHEN-EXT.
           EXIT.
      *****************************************************************
      *    £Ã£Ô¡¦£Í£Ò£É¿ÇÃÇÎÁºîÀ®½èÍý
      *****************************************************************
       100231-CTMRI-SINDAN-SEC           SECTION.
      *
      *    Â¤±ÆºÞ»ÈÍÑ²Ã»»ÎÁ
           IF      FLG-CTMRI           =   1
      *            £Ã£Ô²Ã»»
               MOVE    "170012070"         TO  WRK-SRYCD
               MOVE    "170020470"         TO  WRK-SRYCD-2
           ELSE
      *        £Í£É£Ò²Ã»»
               MOVE    "170020470"         TO  WRK-SRYCD
               MOVE    "170012070"         TO  WRK-SRYCD-2
           END-IF
           MOVE    IDX-SAT             TO  IDX-S
           MOVE    ZERO                TO  FLG-SATU
           MOVE    ZERO                TO  FLG-NENREI
           PERFORM 1003-JIDOU-CHK-SEC
           IF      FLG-ZOEIZAI         =   1
      *        Â¤±ÆºÞ»ÈÍÑ²Ã»»ÎÁ¤¢¤ê
               IF      FLG-ARI             =   ZERO
                   PERFORM 1002-JIDOUSET-SEC
               END-IF
           ELSE
               IF      FLG-ARI             =   1
                   IF      WRK-COM-AUTOKBN (IDX-T) =   "2"
                       PERFORM 10023-JIDOU-DEL-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       100231-CTMRI-SINDAN-EXT.
           EXIT.
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î¼êµ»ÎÁÊÑ¹¹¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10012-SYUGI-CHK-SEC                  SECTION.
      *
      *    Ï«ºÒ¤Î»þ¡¢ÊÑ¹¹¤Ê¤·
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               GO      TO      10012-SYUGI-CHK-EXT
           END-IF
      *
      *    Êñ³ç¤Þ¤È¤áÆþÎÏ¤ÏÂÐ¾Ý³°
           IF      SPA-HKNCOMBINUM     =   "9999"
             OR   (WRK-COM-HKNCOMBI (IDX-Z) =   "9999")
               GO      TO      10012-SYUGI-CHK-EXT
           END-IF
      *
           MOVE    IDX-SAT             TO  IDX-Z
      *
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯
           INITIALIZE                      ORCSC91AREA
      *****MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    "4"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-HEN-SRYCD       TO  LNK-SC91-SRYCD
           MOVE    IDX-Z               TO  LNK-SC91-GMN-IDX
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC91-KAISU
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "0001"
               MOVE    LNK-SC91-ERRCD      TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      10012-SYUGI-CHK-EXT
           END-IF
      *
      *    Åö·î»»Äê¤Ê¤·
           IF      LNK-SC91-CNT        =   ZERO
               CONTINUE
           ELSE
      *        Êñ³çÄþ¸º¶èÊ¬È½Äê
               EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *            Æ¬Éô
                   WHEN    "201"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022290"
                                                       OR  "170701470"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022290"     TO  WRK-COM-INPUTCD
                                                               (IDX-Z)
                       END-IF
      *            ¶í´´
                   WHEN    "202"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022390" 
                                                       OR  "170701570"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022390"     TO  WRK-COM-INPUTCD
                                                                (IDX-Z)
                       END-IF
      *            »Í»è
                   WHEN    "203"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022490" 
                                                       OR  "170701670"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022490"     TO  WRK-COM-INPUTCD
                                                               (IDX-Z)
                       END-IF
               END-EVALUATE
      *        ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
               MOVE    1                   TO  FLG-ARI
               MOVE    IDX-Z               TO  IDX-T
               MOVE    WRK-COM-INPUTCD(IDX-Z)
                                           TO  WRK-SRYCD
               PERFORM 1002-JIDOUSET-SEC
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-Z)
               IF      FLG-ADD             =   1
      *            ÊÑ¹¹¤Î¿ÇÎÅ¥³¡¼¥É¤ò¥»¥Ã¥È
                   MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD(IDX-S)
               END-IF
           END-IF
           .
       10012-SYUGI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10011-KASAN-CHK-SEC                  SECTION.
      *
      *    ²Ã»»¥Ç¡¼¥¿¤Î»þ¡¢Á°¤Ë¼êµ»ÎÁ¤¬¤¢¤ë¤³¤È
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
      *    ²èÁü¿ÇÃÇ¤Î²Ã»»¤Î¤ß
           IF     (WRK-COM-SRYSYUKBN(IDX-Z)    =   "704" )  AND
                  (WRK-COM-SRYKBN (IDX-Z)      =   "70"  )
               MOVE    1                   TO  FLG-ARI
           END-IF
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       10011-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ã±½ã»£±ÆÈ½Äê¡ÊÆýÍÄ»ù²Ã»»¡Ë¤ÎÍ­Ìµ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       1008-XSEN-NYUYOUJI-CHK-SEC           SECTION.
      *
      *    £ØÀþ¥Õ¥£¥ë¥à¥µ¥Ö¥È¥é¥¯¥·¥ç¥ó¤ÏÂÐ¾Ý
           IF      WRK-COM-INPUTCD (IDX-Z) =   "170011650"
               MOVE    "1"             TO  WRK-NYU-CD
           END-IF
      *    Ã±½ã»£±Æ¤Ç¤¢¤ë¤³¤È
           SET     TBL-XIDX            TO  1
           SEARCH      TBL-XSEN-OCC    VARYING   TBL-XIDX
                   AT  END
                       GO      TO      1008-XSEN-NYUYOUJI-CHK-EXT
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-XSEN-SRYCD
                                                           (TBL-XIDX)
                       MOVE    "1"             TO  WRK-NYU-CD
           END-SEARCH
      *
           .
       1008-XSEN-NYUYOUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10013-DOUJI-CHK-SEC           SECTION.
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-DOUJI-2
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-1 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
      *
      *    ÆÃ¼ì»£±Æ°Ê³°¥¨¥é¡¼
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "170002010" )  OR
                  (WRK-SRYCD               =   SPACE )
               CONTINUE
           ELSE
      ******   MOVE    "0070"          TO  WRK-ERRCD
      *****    MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *        ·Ù¹ð¤ØÊÑ¹¹¤¹¤ë
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE ) AND
                      (LNK-ORCSC-KEI-IDX       =   ZERO  )
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG(IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "K070"              TO  WRK-ERRCD
                   GO      TO      10013-DOUJI-CHK-EXT
               END-IF
           END-IF
      *
      *    Ç¯ÎðÊÌ¤Ë¤¢¤ë¾ì¹ç¤Ï¡¢Ç¯ÎðËè¤Î¥³¡¼¥É¤Ø
           IF      LNK-ORCSC-NENREI-YY <=  3
               MOVE    SPACE               TO  WRK-SRYCD
               PERFORM 10023-DOUJI-NENREI-SEC
               IF      WRK-SRYCD       NOT =   SPACE
      *            ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
                   MOVE    1                   TO  FLG-ARI
                   MOVE    IDX-Z               TO  IDX-T
                   PERFORM 1002-JIDOUSET-SEC
                   MOVE    SPACE               TO  WRK-COM-AUTOKBN
                                                       (IDX-S)
               END-IF
           END-IF
           .
      *
       10013-DOUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÃÇÎÁÆ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10015-SIDAN-DOUJI-CHK-SEC           SECTION.
      *
      *    »£±ÆÎÁ¤¬¤¢¤ë¤³¤È
           IF      FLG-SINDAN          =   1
               CONTINUE
           ELSE
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               MOVE    "0079"          TO  WRK-ERRCD
               GO      TO      10015-SIDAN-DOUJI-CHK-EXT
           END-IF
      *
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
               WHEN    "170000730"
      *            ÆÃ¼ì»£±Æ¤Î¼Ì¿¿¿ÇÃÇ¡ÊÆ±»þ¡Ë
                   IF     (WRK-CNT-SATUEI      >   1   )  AND
                          (WRK-COM-INPUTCD(IDX-SAT)
                                               =   "170002010")
                       CONTINUE
                   ELSE
      ***********      MOVE    "0071"          TO  WRK-ERRCD
      ***********      MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *                ·Ù¹ð¤ØÊÑ¹¹¤¹¤ë
                       IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE ) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO  )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                           MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                           MOVE    "K070"          TO  WRK-ERRCD
                       END-IF
                   END-IF
               WHEN    "170022930"
      *            Â¤±ÆºÞ»ÈÍÑ¼Ì¿¿¿ÇÃÇ¡ÊÆ±»þ¡Ë
                   IF     (WRK-CNT-SATUEI      >   1   )  AND
                          (WRK-COM-INPUTCD(IDX-SAT)
                                               =   "170002110")
                       CONTINUE
                   ELSE
      ***************  MOVE    "0071"          TO  WRK-ERRCD
      **************   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *                ·Ù¹ð¤ØÊÑ¹¹¤¹¤ë
                       IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE ) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO  )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                           MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                           MOVE    "K070"          TO  WRK-ERRCD
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    Æ±»þÊ»»Ü¤Î¿ÇÃÇÎÁ¤Ç»£±Æ¤¬£±²ó¤Î¾ì¹ç·Ù¹ð
           IF     (WRK-CNT-SATUEI      =   1   )  AND
                  (WRK-COM-TNSSRYSYUKBN(IDX-Z) =   "710")
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE ) AND
                      (LNK-ORCSC-KEI-IDX       =   ZERO  )
                   MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"             TO  WRK-COM-KZMFLG (IDX-Z)
                   MOVE    "1"             TO  WRK-COM-CUR   (IDX-Z)
                   MOVE    "K070"          TO  WRK-ERRCD
               END-IF
           END-IF
           .
      *
       10015-SIDAN-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ØÀþ»£±ÆÎÁ½ªÎ»½èÍý
      *****************************************************************
       1002-XSEN-END-SEC           SECTION.
      *
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-DOUJI-1
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-2 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
      *    Ç¯ÎðÊÌ¤Ë¤¢¤ë¾ì¹ç¤Ï¡¢Ç¯ÎðËè¤Î¥³¡¼¥É¤Ø
           IF      LNK-ORCSC-NENREI-YY <=  3
               PERFORM 10023-DOUJI-NENREI-SEC
           END-IF
      *
      *    Ã±½ã»£±ÆÎÁ¤Î£²²óÌÜ¤òÆÃ¼ì»£±ÆÎÁ¤ØÊÑ´¹
           IF      WRK-SRYCD       NOT =   SPACE
      *        ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
               MOVE    1                   TO  FLG-ARI
               MOVE    IDX-Z               TO  IDX-T
               PERFORM 1002-JIDOUSET-SEC
           END-IF
      *    »£±Æ¤ÎÀÚ¤êÂØ¤¨¥Õ¥é¥°
           MOVE    "K"                 TO  WRK-COM-AUTOKBN (IDX-Z)
           MOVE    IDX-Z               TO  IDX-HOZ2
      *
      *    ¿ÇÃÇÎÁ²Ã»»ÄÉ²Ã
           PERFORM 10021-XSEN-SINDAN-SEC
      *
      *    ¸½ºß¤Î¥Á¥§¥Ã¥¯°ÌÃÖ¤ËÌá¤¹
           PERFORM VARYING     IDX     FROM    IDX-HOZ2  BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-COM-AUTOKBN (IDX)   =   "K"
                   MOVE    IDX             TO  IDX-Z
                   MOVE    SPA-MAX-LINE    TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDX-SAT
           MOVE    IDX-Z               TO  IDX-HOZ
           .
      *
       1002-XSEN-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ØÀþ»£±ÆÎÁ½ªÎ»½èÍý
      *****************************************************************
       10021-XSEN-SINDAN-SEC           SECTION.
      *    ¥Õ¥£¥ë¥àËç¿ô¤ÎÀßÄê
      *    ¤­¤¶¤ßÃÍÈ½Äê
           IF     (IDX-SAT             >   ZERO   )  AND
                  (WRK-COM-KZMCOMPSIKIBETU (IDX-SAT) NOT =   ZERO)
               PERFORM 100211-KZMSURYO-SET-SEC
           END-IF
      *    ÆýÍÄ»ù²Ã»»¤¢¤ê
           IF      IDX-NYU             >   ZERO
               MOVE    IDX-NYU         TO  IDX-SAT2
           ELSE
               MOVE    IDX-SAT         TO  IDX-SAT2
           END-IF
      *
           MOVE    IDX-SAT2            TO  IDX-S
           PERFORM 100012-SINDAN-SET-SEC
      *    ¥Õ¥£¥ë¥àÆýÍÄ»ù²Ã»»¤Î»»ÄêÍ­Ìµ
           IF     (WRK-NYU-BUI         =   "1" )  AND
                  (WRK-NYU-CD          =   "1" )  AND
                  (WRK-NYU-FIR         =   "1" )  AND
                  (LNK-ORCSC-NENREI-YY <   6   )
               PERFORM 100211-FIRTEN-SET-SEC
           END-IF
           MOVE    ZERO                TO  IDX-NYU
           MOVE    ZERO                TO  IDX-GEN
      *    ¥Õ¥£¥ë¥àÆýÍÄ»ù²Ã»»ÎÎ°è¥¯¥ê¥¢
           INITIALIZE                  WRK-NYUYOJI-AREA
           MOVE    ZERO                TO  WRK-KEIMAI
           MOVE    IDX-Z               TO  IDX-HOZ
      *
           .
       10021-XSEN-SINDAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÃÇÎÁ¼«Æ°²Ã»»ºîÀ®½èÍý
      *****************************************************************
       100211-KZMSURYO-SET-SEC           SECTION.
      *
      *    ¿ôÎÌÆþÎÏ¤Î¤Ê¤¤»þ,¿ôÎÌ¤òËç¿ô¤È¤¹¤ë
           IF     (WRK-COM-SURFLG (IDX-SAT)   =   SPACE )
               IF      WRK-KEIMAI          >   WRK-COM-KZMJGN(IDX-SAT)
                   MOVE    WRK-COM-KZMJGN (IDX-SAT)
                                               TO  WRK-COM-SURYO
                                                           (IDX-SAT)
               ELSE
                   IF      WRK-KEIMAI          =   ZERO
                       MOVE    1               TO  WRK-COM-SURYO
                                                           (IDX-SAT)
                   ELSE
                       MOVE    WRK-KEIMAI      TO  WRK-COM-SURYO
                                                           (IDX-SAT)
                   END-IF
               END-IF
           END-IF
      *
           .
       100211-KZMSURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÃÇÎÁ¼«Æ°²Ã»»ºîÀ®½èÍý
      *****************************************************************
       100012-SINDAN-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    ¿ÇÃÇ¡¦»£±ÆÆ±»þ¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-SAT) =   TBL-DOUJI-1
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-2 (TBL-IDX)
                                                TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-SAT) =   TBL-DOUJI-2
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-1 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
           IF     (WRK-SRYCD           =   SPACE      )  OR
                  (WRK-SRYCD           =   "170002010"  OR 
                                           "170001910" )
               MOVE    ZERO                TO  FLG-SYORI
           ELSE
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    1                   TO  FLG-SYORI
               GO      TO      100012-SINDAN-SET-EXT
           END-IF
      *
      *    Ã±½ã»£±Æ¤ÎÈ½Äê
           SET     TBL-XIDX            TO  1
           SEARCH      TBL-XSEN-OCC    VARYING   TBL-XIDX
                   AT  END
                       MOVE    SPACE           TO  WRK-XSEN-SRYCD-I
                       MOVE    SPACE           TO  WRK-XSEN-SRYCD-R
                   WHEN    WRK-COM-INPUTCD (IDX-SAT)
                                           =   TBL-XSEN-SRYCD
                                                           (TBL-XIDX)
                       MOVE    TBL-XSEN-SRYCD-I (TBL-XIDX)
                                                   TO  WRK-XSEN-SRYCD-I
                       MOVE    TBL-XSEN-SRYCD-R (TBL-XIDX)
                                                   TO  WRK-XSEN-SRYCD-R
           END-SEARCH
      *
      *    Æ±»þÊ»»Ü¤Î¿ÇÃÇÎÁ¤ÎÈ½Äê
           MOVE    ZERO                TO  IDX-DOJI1
           MOVE    ZERO                TO  IDX-DOJI2
           PERFORM VARYING     IDX-S1  FROM    IDX-SAT   BY  1
                   UNTIL      (IDX-S1      >   SPA-MAX-LINE )  OR
                              (IDX-DOJI1   >   ZERO )    OR
                              (IDX-DOJI2   >   ZERO )    OR
                              (WRK-COM-INPUTCD (IDX-S1)  =   SPACE AND
                               WRK-GMN-INPUTCD (IDX-S1)  =   SPACE )
      *        Æ±»þÊ»»Ü¤Î¿ÇÃÇÎÁÈ½Äê
               IF      WRK-COM-TNSSRYSYUKBN(IDX-S1)    =   "710"
                   MOVE    IDX-S1              TO  IDX-DOJI1
               END-IF
               IF      WRK-COM-TNSSRYSYUKBN(IDX-S1)    =   "711"
                   MOVE    IDX-S1              TO  IDX-DOJI2
               END-IF
      *
               IF     (IDX-S1          NOT =   IDX-SAT)  AND
                      (WRK-COM-TNSSRYSYUKBN (IDX-S1)    =   "721"
                                                        OR  "720")
                   MOVE   SPA-MAX-LINE         TO  IDX-S1
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-COM-INPUTCD (IDX-SAT)
               WHEN    "170002010"
      *            ÆÃ¼ì»£±Æ¤Î¼Ì¿¿¿ÇÃÇ
                   IF      WRK-CNT-SATUEI      =   1
                       MOVE    "170000610"         TO  WRK-SRYCD
                       MOVE    "170000730"         TO  WRK-SRYCD-2
                   ELSE
                       MOVE    "170000730"         TO  WRK-SRYCD
                       MOVE    "170000610"         TO  WRK-SRYCD-2
                   END-IF
      ************ MOVE    SPACE               TO  WRK-SRYCD-2
                   IF     (IDX-DOJI1           >   ZERO )  AND
                          (WRK-COM-INPUTCD (IDX-DOJI1)
                                               =   "170000730")
      *                Æ±»þÊ»»Ü¤Î¥³¡¼¥É¤¢¤ê
                       MOVE    SPACE               TO  WRK-SRYCD
                   END-IF
               WHEN    "170002110"
      *            Â¤±ÆºÞ»ÈÍÑ¤Î¿ÇÃÇÎÁ
                   IF      WRK-CNT-SATUEI      =   1
                       MOVE    "170000810"         TO  WRK-SRYCD
                       MOVE    "170022930"         TO  WRK-SRYCD-2
                   ELSE
                       MOVE    "170022930"         TO  WRK-SRYCD
                       MOVE    "170000810"         TO  WRK-SRYCD-2
                   END-IF
      **********   MOVE    SPACE               TO  WRK-SRYCD-2
                   IF     (IDX-DOJI1           >   ZERO )  AND
                          (WRK-COM-INPUTCD (IDX-DOJI1)
                                               =   "170022930")
      *                Æ±»þÊ»»Ü¤Î¥³¡¼¥É¤¢¤ê
                       MOVE    SPACE               TO  WRK-SRYCD
                   END-IF
               WHEN    "170001910"
      *            Ã±½ã»£±Æ¤Î¿ÇÃÇÎÁ
                   EVALUATE    FLG-BUIKBN
                       WHEN    1
                       WHEN    3
      *                    ¤½¤ÎÂ¾¡Ê¥í¡Ë
                           MOVE    WRK-XSEN-SRYCD-R    TO  WRK-SRYCD
                           MOVE    WRK-XSEN-SRYCD-I    TO  WRK-SRYCD-2
      *                    Æ±»þÊ»»Ü¤Î¾ì¹ç
                           IF      WRK-CNT-SATUEI      >   1
                               MOVE    "170022830"     TO  WRK-SRYCD
                               MOVE    "170022730"     TO  WRK-SRYCD-2
                           END-IF
                           IF     (IDX-DOJI1           >   ZERO )  AND
                                  (WRK-COM-INPUTCD (IDX-DOJI1)
                                                       =   "170022830")
      *                        Æ±»þÊ»»Ü¤Î¥³¡¼¥É¤¢¤ê
                               MOVE    SPACE               TO  WRK-SRYCD
                           END-IF
                       WHEN    2
      *                    Æ¬¡¦¶»¡¦Ê¢¡¦ÀÔ¿ñ¡Ê¥¤¡Ë
                           MOVE    WRK-XSEN-SRYCD-I    TO  WRK-SRYCD
                           MOVE    WRK-XSEN-SRYCD-R    TO  WRK-SRYCD-2
      *                    Æ±»þÊ»»Ü¤Î¾ì¹ç
                           IF      WRK-CNT-SATUEI      >   1
                               MOVE    "170022730"     TO  WRK-SRYCD
                               MOVE    "170022830"     TO  WRK-SRYCD-2
                           END-IF
                           IF     (IDX-DOJI1           >   ZERO )  AND
                                  (WRK-COM-INPUTCD (IDX-DOJI1)
                                                       =   "170022730")
      *                        Æ±»þÊ»»Ü¤Î¥³¡¼¥É¤¢¤ê
                               MOVE    SPACE               TO  WRK-SRYCD
                           END-IF
                   END-EVALUATE
               WHEN    OTHER
      *            ¤½¤ÎÂ¾¤ÎÃ±½ã»£±Æ¤Î¿ÇÃÇÎÁ
                   EVALUATE    FLG-BUIKBN
                       WHEN    1
                       WHEN    3
      *                    ¤½¤ÎÂ¾¡Ê¥í¡Ë
                           MOVE    WRK-XSEN-SRYCD-R    TO  WRK-SRYCD
                           MOVE    WRK-XSEN-SRYCD-I    TO  WRK-SRYCD-2
                       WHEN    2
      *                    Æ¬¡¦¶»¡¦Ê¢¡¦ÀÔ¿ñ¡Ê¥¤¡Ë
                           MOVE    WRK-XSEN-SRYCD-I    TO  WRK-SRYCD
                           MOVE    WRK-XSEN-SRYCD-R    TO  WRK-SRYCD-2
                   END-EVALUATE
           END-EVALUATE
           IF      FLG-SYORI           =   1
               MOVE    SPACE               TO  WRK-SRYCD
           END-IF
      *    ¿ÇÃÇÎÁ¼«Æ°ÄÉ²Ã
           MOVE    IDX-SAT             TO  IDX-SIN
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    1                   TO  FLG-SATU
               MOVE    IDX-SAT2            TO  IDX-S
               MOVE    ZERO                TO  FLG-NENREI
               PERFORM 1003-JIDOU-CHK-SEC
               PERFORM 1002-JIDOUSET-SEC
               IF      FLG-ADD             =   1
                   MOVE    IDX-S               TO  IDX-SIN
               END-IF
      *
      ******   £±²óÌÜ¤Ç´ÖÀÜ»£±Æ¥³¡¼¥É¤ÎÆþÎÏ¤¬¤¢¤ë»þ¡¢¿ÇÃÇÎÁ¤Î¸å¤Ë¤âÄÉ²Ã
      *        IF      (FLG-ADD            =   1    )  AND
      *                (IDX-GEN            >   ZERO )  AND
      *                (WRK-CNT-SATUEI     =   1    )
      *            MOVE    ZERO            TO  FLG-ARI
      **           MOVE    WRK-COM-INPUTCD (IDX-GEN)
      *            MOVE    WRK-GEN-SRYCD   TO  WRK-SRYCD
      *            PERFORM 1002-JIDOUSET-SEC
      ******   END-IF
           END-IF
      *
      *    ¾Ã²½´É¤ÇÂ¤±ÆºÞ»£±Æ¤Î»þ¡¢Æ©»ë¿ÇÃÇÎÁ¤ÎÄÉ²Ã¡Ê»£±ÆÎÁ¤Î¾å¤ËÄÉ²Ã¡Ë
           IF     (FLG-BUIKBN          =   3  )   AND
                  (WRK-COM-INPUTCD (IDX-SAT)   =   "170002110" )
               MOVE    "170000310"         TO  WRK-SRYCD
               COMPUTE IDX-S               =   IDX-SAT    -    1
               MOVE    ZERO                TO  FLG-SATU
               MOVE    SPACE               TO  WRK-SRYCD-2
               MOVE    ZERO                TO   FLG-NENREI
               PERFORM 1003-JIDOU-CHK-SEC
               PERFORM 1002-JIDOUSET-SEC
               IF      FLG-ADD             =   1
                   IF      WRK-COM-INPUTCD (IDX-SAT + 1)
                                               =   "170002110"
                       COMPUTE IDX-SAT         =   IDX-SAT    +   1
                   END-IF
               END-IF
           END-IF
      *
           .
       100012-SINDAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁÄÉ²Ã½èÍý
      *****************************************************************
       1002-JIDOUSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÊÔ½¸¡¦´ðËÜ¥Á¥§¥Ã¥¯
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           IF      FLG-NENREI          =   1
               MOVE    "1"                 TO  LNK-SC92-CHUKSN
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    ¥¨¥é¡¼¤Î»þ¡¢ÄÉ²Ã¤Ê¤·
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-JIDOUSET-EXT
           END-IF
      *
           IF      IDX-S               >   1
               MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
           END-IF
      *
      *    ¼«Æ°ÄÉ²ÃÊ¬¡ÊÆþÎÏ»þ¤ËÄÉ²Ã¡Ë
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *****ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           .
       1002-JIDOUSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°²Ã»»¡¡Â¸ºß¥Á¥§¥Ã¥¯²Ã½èÍý
      *****************************************************************
       1003-JIDOU-CHK-SEC        SECTION.
      *
      *    Â¸ºß¥Á¥§¥Ã¥¯
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           IF      FLG-NENREI          =   1
               MOVE    IDX-SYU             TO  IDXS1
           ELSE
               MOVE    1                   TO  IDXS1
      *        ¿ÇÃÇÎÁ¤Î»þ¡¢»£±ÆÎÁ°Ê²¼
               IF      FLG-SATU            =   1
                   MOVE    IDX-SAT             TO  IDXS1
               END-IF
           END-IF
           PERFORM VARYING     IDX1    FROM    IDXS1  BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ) OR
                              (WRK-GMN-INPUTCD(IDX1)(1:1)  =   "*")
              IF      (WRK-COM-INPUTCD (IDX1) NOT =   SPACE )  AND
                     ((WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD ) OR
                      (WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD-2))
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
      *!!      IF     (WRK-SRYCD               =    "170000510"   OR
      *                                             "170000410" )  AND
      *               (WRK-COM-INPUTCD (IDX1)  =    "170000510"   OR
      *                                             "170000410" )
      *             MOVE    IDX1           TO  IDX-T
      *!            MOVE    1              TO  FLG-ARI
      *!!      END-IF
           END-PERFORM
           .
       1003-JIDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¯Îð²Ã»»ÄÉ²Ã½èÍý¡Ê¶¦ÄÌ¡Ë
      *****************************************************************
       1007-NYUYOJI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-SYU IDX2)
                                          =   SPACE  OR   ZERO )
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-SYU IDX2)
                                               TO  WRK-SRYCD
               MOVE    IDX-SYU            TO   IDX-T
               MOVE    IDX-SYU            TO   IDX-S
               MOVE    SPACE              TO   WRK-SRYCD-2
               MOVE    1                  TO   FLG-NENREI
               MOVE    3                   TO  FLG-SATU
               PERFORM 1003-JIDOU-CHK-SEC
      *        ²Ã»»ÄÉ²Ã
               PERFORM 1002-JIDOUSET-SEC
               IF      FLG-ADD             =   1
                   MOVE    2                   TO  WRK-COM-SANFLG 
                                                           (IDX-S)
      *            »£±ÆÎÁ¤ÎÆýÍÄ»ù²Ã»»¤Î»þ¡¢°ÌÃÖÊÝÂ¸
                   IF      IDX-SYU             =   IDX-SAT
                       MOVE    IDX-S           TO  IDX-NYU
                   END-IF
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¥Õ¥£¥ë¥àÆýÍÄ»ù²Ã»»½èÍý
      *****************************************************************
       100211-FIRTEN-SET-SEC             SECTION.
      *
      *    ÆýÍÄ»ù²Ã»»¤¢¤ê
           MOVE    "799990070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  IDX-S
      *
      *    ¼«Æ°²Ã»»¡¡Â¸ºß¥Á¥§¥Ã¥¯¡õÁÞÆþ°ÌÃÖ¥»¥Ã¥È
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-NYU-IDX
           PERFORM VARYING     IDX1    FROM    IDX-HOZ  BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
                          OR  (WRK-COM-AUTOKBN (IDX1)  =   "K"   )
      *            ¥Õ¥£¥ë¥àÈ½Äê
               IF    ((WRK-COM-INPUTCD (IDX1)(1:1) =   "7" ) OR
                      (WRK-COM-INPUTCD (IDX1)(1:3) =   "059"))  AND
                      (WRK-COM-DATAKBN (IDX1)      =   "3" )   AND
                      (WRK-COM-INPUTCD (IDX1)  NOT =   WRK-SRYCD )
      *            ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬¤ÎÈ½Äê
                   IF      WRK-COM-TOKUKIZAIAGEKSNKBN (IDX1)
                                               =   1
                       IF      WRK-COM-INPUTCD (IDX1 + 1)
                                               =   WRK-SRYCD
                           CONTINUE
                       ELSE
                           MOVE    IDX1                TO  IDX-S
                           MOVE    1                   TO  FLG-NENREI
                           PERFORM 1002-JIDOUSET-SEC
                           IF      FLG-ADD             =   1
                               MOVE    IDX-S           TO  WRK-NYU-IDX
                               ADD     1               TO  IDX-MAX
                               ADD     1               TO  IDX1
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      ***  IF      WRK-NYU-IDX         >   ZERO
      *        MOVE    WRK-NYU-IDX         TO  IDX-S
      *    END-IF
      *    MOVE    1                   TO  FLG-NENREI
      **** PERFORM 1002-JIDOUSET-SEC
      *
           .
       100211-FIRTEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¯ÎðÊÌ¥³¡¼¥ÉÊÑ´¹½èÍý
      *****************************************************************
       10023-DOUJI-NENREI-SEC           SECTION.
      *
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯
           SET     TBL-NIDX            TO  1
           SEARCH      TBL-DOUJI-NEBREI-OCC    VARYING   TBL-NIDX
                   AT   END
                       MOVE    SPACE            TO  WRK-SRYCD1
                       MOVE    SPACE            TO  WRK-SRYCD2
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-NEN-SRYCD0
                                                           (TBL-NIDX)
                       MOVE    TBL-NEN-SRYCD1 (TBL-NIDX)
                                                TO  WRK-SRYCD1
                       MOVE    TBL-NEN-SRYCD2 (TBL-NIDX)
                                                TO  WRK-SRYCD2
           END-SEARCH
           IF      WRK-SRYCD1          =   SPACE
               GO      TO      10023-DOUJI-NENREI-EXT
           END-IF
      *
      *    ¿·À¸»ùÈ½Äê
           IF     (LNK-ORCSC-NENREI-YY     =   ZERO )  AND
                  (LNK-ORCSC-NENREI-MM     =   ZERO )  AND
                  (LNK-ORCSC-NENREI-DD     <   28   )
               MOVE    WRK-SRYCD1          TO  WRK-SRYCD
           ELSE
      *        ÆýÍÄ»ù¡Ê£³ºÐÌ¤Ëþ¡Ë
               IF      LNK-ORCSC-NENREI-YY     <   3
                   MOVE    WRK-SRYCD2          TO  WRK-SRYCD
               END-IF
           END-IF
           .
      *
       10023-DOUJI-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»ÊÔ½¸½èÍý
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *    »£±ÆÎÁ¤ÎºÆ·×»»
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KISOTEN-G
      *    Ëç¿ô·×»»
           MOVE    ZERO                TO  WRK-KEIMAI
      *
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  IDX-MAX
           MOVE    1                   TO  IDX-STR
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *        ¼êµ»Ëè
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                       OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                ÊÔ½¸¥Á¥§¥Ã¥¯¤Ø
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    ºÇ½ª¹Ô
      *    ³ä¹ç·×»»²Ã»»
           IF     (IDX-SYU             =   ZERO ) AND
                  (FLG-SYUGI           =   1    )
               MOVE    1                   TO  IDX-SYU
           END-IF
           IF      IDX-SYU             >   ZERO
               COMPUTE IDX-Z               =   IDX-MAX   +  1
               PERFORM 10021-TENSUKEI-SEC
           END-IF
      *    ¹ç·×ÅÀ¿ô·×»»¡¦ÊÔ½¸
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    ·×
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
      *    AUTKBN¥¯¥ê¥¢
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL       IDX-Z       >   IDX-MAX
               IF      WRK-COM-AUTOKBN(IDX-Z)  =   "K"
                   MOVE    SPACE           TO  WRK-COM-AUTOKBN(IDX-Z)
               END-IF
           END-PERFORM
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
      *    ÅÀ¿ôµ­ºÜÈ½Äê
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               IF      FLG-SINDAN          =   1
      *        £ØÀþ»£±ÆÎÁ
                   IF      WRK-COM-AUTOKBN(IDX-Z)  =   "K"
                       IF      IDX-SYU             >   ZERO
      *                    ¼êµ»ÎÁ¤Î·×»»
                           PERFORM 10021-TENSUKEI-SEC
      *                    ÅÀ¿ô·×»»
                           PERFORM 1007-TENSU-SUM-SEC
                       END-IF
                       MOVE    IDX-Z               TO  IDX-SYU
                   END-IF
               ELSE
                   IF      IDX-SYU             >   ZERO
      *                ¼êµ»ÎÁ¤Î·×»»
                       PERFORM 10021-TENSUKEI-SEC
      *                ÅÀ¿ô·×»»
                       PERFORM 1007-TENSU-SUM-SEC
                   END-IF
                   MOVE    IDX-Z               TO  IDX-SYU
               END-IF
           END-IF
      *
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            ¼êµ»ÎÁ½¸·×
      *            ¡ó²Ã»»¡¦¸º»»¤Ï½¸·×¤·¤Ê¤¤
                   IF      WRK-COM-TENSIKIBETU (IDX-Z)
                                               =   5
                                               OR  6
                       CONTINUE
                   ELSE
                       PERFORM 100212-SYUGI-KEI-SEC
                   END-IF
           END-EVALUATE
      *    ²ó¿ô
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼êµ»ÎÁ·×»»½èÍý
      *****************************************************************
       100212-SYUGI-KEI-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-SYUGI
           EVALUATE    FLG-SINDAN
               WHEN    1
      *            £ØÀþ»£±ÆÎÁ
                   PERFORM 1002121-XSEN-KEI-SEC
               WHEN    OTHER
      *            ¤½¤ÎÂ¾
      *            ÄÌÂ§²Ã»»ÂÐ¾Ý³°¶èÊ¬È½Äê
                   IF      WRK-COM-TUSOKUGAIKBN (IDX-Z)  =   "1"
                       ADD     WRK-COM-KISOTEN (IDX-Z)
                                                   TO  WRK-SUM-TENGAKEI
                   ELSE
                       ADD     WRK-COM-KISOTEN(IDX-Z)
                                                   TO  WRK-SUM-TENSUKEI
                   END-IF
           END-EVALUATE
      *
           .
       100212-SYUGI-KEI-EXT.
           EXIT.
      *****************************************************************
      *    £ØÀþ»£±ÆÎÁ·×»»½èÍý
      *****************************************************************
       1002121-XSEN-KEI-SEC             SECTION.
      *
           EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)
               WHEN    "72"
      *            »£±ÆÎÁÊÔ½¸
                   MOVE    ZERO                    TO  FLG-SATU
      *            »£±ÆÎÁ¤òÄþ¸º¡¢ÆýÍÄ»ù²Ã»»¸å¤Ë¤­¤¶¤ßÃÍ·×»»¤ò¤¹¤ë
                   PERFORM 1000131-SATUEI-KEI-SEC
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSUKEI1
                   ADD     1                   TO  WRK-CNT-SATUEI
                   MOVE    IDX-Z               TO  IDX-SYU
               WHEN    "71"
      *            ¿ÇÃÇÎÁ
                   MOVE    1                      TO  FLG-SATU
                   PERFORM 1000133-SINDAN-KEISAN-SEC
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSUKEI2
      *        ¤½¤ÎÂ¾
                WHEN    OTHER
                   ADD     WRK-COM-KISOTEN(IDX-Z)
                                                   TO  WRK-TENSUKEI3
                   IF    WRK-COM-DATAKBN (IDX-Z)   =   "1"
                       MOVE    IDX-Z               TO  IDX-SYU
                   END-IF
           END-EVALUATE
      *
           .
       1002121-XSEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     »£±ÆÎÁÊÔ½¸½èÍý
      *****************************************************************
       1000131-SATUEI-KEI-SEC          SECTION.
      *
           MOVE    WRK-COM-TEN (IDX-Z) TO  WRK-SATU-TEN
           MOVE    ZERO                TO  WRK-SATUEI-SURYO
      *
      *    ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»
           PERFORM VARYING     IDX-Z2  FROM    IDX-STR BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE )
               IF      (IDX-Z          NOT =   IDX-Z2   )  AND
                       (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
                   MOVE    SPA-MAX-LINE          TO  IDX-Z2
               ELSE
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z2) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z2)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN    (IDX-Z2) =   SPACE )
      *                ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»¡Ê¿ÇÃÇÎÁ¡Ë
                       COMPUTE WRK-GENTEN      =   WRK-SATU-TEN
                                           *  (WRK-COM-TEN  (IDX-Z2)
                                           /   100        )
                       COMPUTE WRK-SATU-TEN    =   WRK-SATU-TEN
                                               -   WRK-GENTEN
                       MOVE    1               TO  WRK-COM-SANFLG
                                                          (IDX-Z2)
                       MOVE    SPA-MAX-LINE    TO  IDX-Z2
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-SATU-KASAN
      *    ÆýÍÄ»ù²Ã»»¡Ê¡ó²Ã»»¡Ë
      **** PERFORM VARYING     IDX-Z2  FROM    1    BY  1
           PERFORM VARYING     IDX-Z2  FROM    IDX-STR  BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
              IF     (WRK-COM-SANFLG(IDX-Z2)  =   2 OR 5 )  AND
                     (WRK-COM-TENSIKIBETU(IDX-Z2)  =   5  )
                   MOVE    ZERO                TO  WRK-TENSUKEIS
                   COMPUTE WRK-TENSUKEIS       =   WRK-SATU-TEN
                                               *   WRK-COM-TEN (IDX-Z2)
                                               /   100
                   MOVE    5               TO  WRK-COM-SANFLG (IDX-Z2)
                   COMPUTE WRK-SATU-TEN        =   WRK-SATU-TEN
                                               +   WRK-TENSUKEIS
      *            ²Ã»»¤Î³ä¹ç¤òÊÔ½¸¤¹¤ë
                   COMPUTE WRK-SATU-KASAN      =   WRK-COM-TEN (IDX-Z2)
                                               /   100
      *
                   MOVE    SPA-MAX-LINE        TO  IDX-Z2
               END-IF
           END-PERFORM
      *
      *    ¤­¤¶¤ßÃÍÈ½Äê
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                  (WRK-COM-DATAKBN (IDX-Z)         =   "1" ) AND
                  (WRK-COM-KZMCOMPSIKIBETU (IDX-Z) NOT =   ZERO)
      *        ¤­¤¶¤ßÃÍ·×»»
               MOVE    ZERO                TO  FLG-SATU-GEN
               MOVE    ZERO                TO  FLG-SATU
               PERFORM 1000132-TENSU-KEISAN-SEC
      *        »£±ÆÎÁ¤Î¿ôÎÌ¤òÊÝÂ¸
               MOVE    WRK-COM-SURYO (IDX-Z)   TO  WRK-SATUEI-SURYO
           ELSE
      *        »£±ÆÎÁ
               COMPUTE WRK-TENSUKEI-9      =   WRK-SATU-TEN  + 0.5
               MOVE    WRK-TENSUKEI-9      TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
           .
       1000131-SATUEI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     ¿ÇÃÇÎÁ¤½¤ÎÂ¾½èÍý
      *****************************************************************
       1000133-SINDAN-KEISAN-SEC          SECTION.
      *
           MOVE    WRK-COM-TEN  (IDX-Z)    TO  WRK-SATU-TEN
      *    ÆýÍÄ»ù²Ã»»¥¯¥ê¥¢
           MOVE    ZERO                TO  WRK-SATU-KASAN
      *
      *    ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»
           IF     (WRK-CNT-SATUEI      =   1   )  AND
                  (WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)  =  "71" )
               PERFORM VARYING     IDX-Z2  FROM    IDX-STR BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z2) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z2)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN    (IDX-Z2) =   SPACE )
      *                ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»¡Ê¿ÇÃÇÎÁ¡Ë
                       COMPUTE WRK-GENTEN      =   WRK-SATU-TEN
                                           *  (WRK-COM-TEN  (IDX-Z2)
                                           /   100        )
                       COMPUTE WRK-SATU-TEN    =   WRK-SATU-TEN
                                               -   WRK-GENTEN
                       MOVE    1               TO  WRK-COM-SANFLG
                                                          (IDX-Z2)
                       MOVE    SPA-MAX-LINE    TO  IDX-Z2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    Æ±»þ»£±Æ¤Ë¤è¤ëÄþ¸º
           IF    ((WRK-CNT-SATUEI      >   1   )  AND
                  (WRK-COM-TNSSRYSYUKBN(IDX-Z) =  "711"  ))
      ***                                      OR "710"  )
              OR  (WRK-COM-TNSSRYSYUKBN(IDX-Z) =  "710"  )
               IF      WRK-COM-TNSSRYSYUKBN(IDX-Z) =  "711"
                   COMPUTE WRK-SATU-TEN        =   WRK-SATU-TEN
                                               /   2
               END-IF
               MOVE    1                   TO  FLG-SATU-GEN
           ELSE
               MOVE    ZERO                TO  FLG-SATU-GEN
           END-IF
      *
      *    ¤­¤¶¤ßÃÍÈ½Äê
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                  (WRK-COM-DATAKBN (IDX-Z)         =   "1" ) AND
                  (WRK-COM-KZMCOMPSIKIBETU (IDX-Z) NOT =   ZERO)
      *        »£±ÆÎÁ¤Î¿ôÎÌ¤ò¿ôÎÌ¤Ø
               IF      WRK-SATUEI-SURYO    >   ZERO
      ************ MOVE    "1"             TO  WRK-COM-SURFLG (IDX-Z)
                   MOVE    WRK-SATUEI-SURYO    TO  WRK-COM-SURYO (IDX-Z)
                   MOVE    WRK-SATUEI-SURYO    TO  WRK-KEIMAI
                   MOVE    1                   TO  FLG-SATU
               ELSE
                   MOVE    ZERO                TO  FLG-SATU
               END-IF
      *        ¤­¤¶¤ßÃÍ·×»»
               PERFORM 1000132-TENSU-KEISAN-SEC
           ELSE
               COMPUTE WRK-TENSUKEI-9      =   WRK-SATU-TEN  + 0.5
               MOVE    WRK-TENSUKEI-9      TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
      *
           .
       1000133-SINDAN-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ¤­¤¶¤ßÃÍ·×»»½èÍý
      *****************************************************************
       1000132-TENSU-KEISAN-SEC          SECTION.
      *
           MOVE    WRK-COM-INPUTCD (IDX-Z)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
      *    ¤­¤¶¤ßÃÍ·×»»
           MOVE    WRK-COM-SURYO (IDX-Z)   TO  WRK-SURYO
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    ¤­¤¶¤ßÅÀ¿ô¤ò»»ÄêÅÀ¿ô¤Î£±¡¿£²¤È¤¹¤ë
           IF      WRK-SATU-TEN        =   ZERO
               MOVE    TNS-KZMTEN          TO  WRK-KZMTEN
      *        ÆýÍÄ»ù²Ã»»·×»»
               IF      WRK-SATU-KASAN      >   ZERO
                   COMPUTE WRK-KZMTEN      =   (TNS-KZMTEN
                                           *    WRK-SATU-KASAN )
                                           +    TNS-KZMTEN
               END-IF
           ELSE
               IF      FLG-SATU-GEN        =   ZERO
                   COMPUTE WRK-KZMTEN          =   WRK-SATU-TEN   /  2
               ELSE
                   MOVE    WRK-SATU-TEN        TO  WRK-KZMTEN
               END-IF
           END-IF
      *    ¤­¤¶¤ßÃÍÈ½Äê
           EVALUATE    TRUE
      *            ¥Ç¡¼¥¿¡å¡Ê²¼¸ÂÃÍ¡Ý¤­¤¶¤ßÃÍ¡Ë
               WHEN    WRK-SURYO       <=  (TNS-KZMKGN  -  TNS-KZM)
                   IF      TNS-KZMERR          =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            ²¼¸ÂÃÍ¡ã¥Ç¡¼¥¿¡å¾å¸ÂÃÍ
               WHEN     (TNS-KZMKGN        <   WRK-SURYO )   AND
                        (TNS-KZMJGN        >=  WRK-SURYO )
      *            ÅÀ¿ô·×»»¼°£±
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO -  TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
      *            ÅÀ¿ô¤¬¥¼¥í¤Î»þ¡¢¤³¤ÎÅÀ¿ô¤È¤¹¤ë
                   COMPUTE WRK-KEI-TEN     =   WRK-SATU-TEN
                                               + ( WRK-KEI-TEN2
                                               *   WRK-KZMTEN )
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            ¾å¸ÂÃÍ¡ã¥Ç¡¼¥¿
               WHEN    TNS-KZMJGN        <   WRK-SURYO
                   IF      TNS-KZMERR        =   0  OR  2
      *            ÅÀ¿ô·×»»¼°£±¤ò¤»¤º¡¢¥¨¥é¡¼¤È¤¹¤ë
      *************    MOVE    "0022"        TO  SPA-ERRCD
      *            ·Ù¹ð¥¨¥é¡¼¤È¤¹¤ë
                       MOVE    "K030"            TO  SPA-ERRCD
      *            ÅÀ¿ô·×»»¼°£±
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -  TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                               + ( WRK-KEI-TEN2
                                               *   TNS-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            ÅÀ¿ô·×»»¼°£²
                       COMPUTE WRK-KEI-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-SATU-TEN
                                                   + ( WRK-KEI-TEN2
                                                   *   WRK-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-KEI-TEN2        >   ZERO
                   MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-Z)
               ELSE
                   COMPUTE WRK-TENSUKEI-9  =   WRK-SATU-TEN  + 0.5
                   MOVE    WRK-TENSUKEI-9  TO  WRK-COM-KISOTEN(IDX-Z)
               END-IF
           END-IF
      *
           .
       1000132-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼êµ»ÎÁ¤Î·×»»½èÍý
      *****************************************************************
       10021-TENSUKEI-SEC             SECTION.
      *
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
           EVALUATE    FLG-SINDAN
               WHEN    1
      *            £ØÀþ»£±Æ
                   COMPUTE LNK-SC90-TENSU  =   WRK-TENSUKEI1
                                           +   WRK-TENSUKEI2
                                           +   WRK-TENSUKEI3
                                           +   WRK-TENSUKEI
               WHEN    OTHER
                   COMPUTE LNK-SC90-TENSU  =  (WRK-SUM-TENSUKEI
                                           +   WRK-SUM-TENGAKEI )
                                           +   0.5
           END-EVALUATE
      *
           .
       10021-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³ä¹ç²Ã»»·×»»½èÍý
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
      *    ¼êµ»ÎÁ¤ÎÅÀ¿ô¤ò½êÄêÅÀ¿ô¤È¤¹¤ë
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL      (IDX         >=  HZN-IDX     )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *      ´ÖÀÜ»£±ÆÄþ¸º¤ÏÂÐ¾Ý³°
             IF       (WRK-COM-TENSIKIBETU (IDX)   =   6 )  AND
                      (WRK-COM-KIJUNTEIGENKBN(IDX)
                                                   =   ZERO ) AND
                      (WRK-COM-AUTOKBN    (IDX)    =   SPACE ) AND
                      (WRK-COM-SANFLG     (IDX)    =   1    )
               CONTINUE
             ELSE
      *        ¡ó²Ã»»
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO  OR  2 )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
                   MOVE    ZERO                TO  WRK-TENSUKEI
               END-IF
      *        ¡ó¸º»»
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO OR 2 )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
                   MOVE    ZERO                TO  WRK-TENSUKEI
               END-IF
             END-IF
           END-PERFORM
      *
           COMPUTE WRK-SUM-TENSUKEI    =   WRK-SUM-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼êµ»Ëè¤Î·×»»½èÍý
      *****************************************************************
       1007-TENSU-SUM-SEC            SECTION.
      *
      *    ¼êµ»Ëè¤Î·×»»½èÍý
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           COMPUTE LNK-SC90-IDX-END    =   IDX-Z   -   1
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
           MOVE    IDX-Z               TO  IDX-STR
           INITIALIZE                  WRK-TENSU-KEISAN
           MOVE    ZERO                TO  LNK-SC90-TENSU
      *
           .
       1007-TENSU-SUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    Êñ³ç¤Þ¤È¤áÆþÎÏ¤ÏÂÐ¾Ý³°
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-JIKASAN
           MOVE    ZERO                TO  IDX-GAI
           MOVE    ZERO                TO  IDX-CT
      *
           MOVE    ZERO                TO  FLG-TENTEKI
      *    £Ã£Ô¡¢£Í£Ò£É¥Á¥§¥Ã¥¯
      *        Ï«ºÒ¤Î»þ¡¢ÊÑ¹¹¤Ê¤·
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               INITIALIZE                  WRK-CTMRI-CNT-G
           ELSE
               PERFORM 2002-CTMRI-CNT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-TENTEKI
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
             IF   (WRK-COM-HKNCOMBI (IDX-Z) NOT =   "9999")
      *        »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "170016010"
                   MOVE    IDX-Z           TO  IDX-GAI
               END-IF
      *
      *        »þ´Ö³°²Ã»»¥Á¥§¥Ã¥¯¡Ê¿Ç»¡¡¢²Ã»»¡¢»þ´Ö²Ã»»¤Î¤â¤Î¡Ë
               IF     (WRK-COM-SRYKBN  (IDX-Z)     =   "11"  OR
                                                       "12"     )
                   IF     (WRK-COM-DATAKBN   (IDX-Z)   =   "2"  ) AND
                          (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =   ZERO)
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
                   IF     (WRK-COM-DATAKBN (IDX-Z)   =   "1"  ) AND
                          (WRK-COM-TIMEFLG (IDX-Z)   NOT =   ZERO )
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
               END-IF
      *
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯
               IF      WRK-COM-KNSJISGRPKBN (IDX-Z)    =  32
                   PERFORM 2001-CTMRI-CHK-SEC
               END-IF
      *        ÅÀÅ©¤Ï£±²ó¤Î¤ß¤È¤¹¤ë
      *        ³°Íè¤Î»þ¤Î¤ß¡ÊÆþ±¡¤ÏÅÀÅ©¤Ç¥Á¥§¥Ã¥¯¡Ë
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
                      (WRK-COM-SRYSYUKBN(IDX-Z) NOT =  "950"  AND
                                                       "960") AND
                      (WRK-COM-INPUTCD  (IDX-Z)    =   "130009310" OR
                                                       "130003710" OR
                                                       "130003810")
                   IF      FLG-TENTEKI         =   1
                       MOVE    "0057"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
                   MOVE    1               TO  FLG-TENTEKI
               END-IF
      *
             END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»Í­¤ê
           IF      IDX-GAI             >   ZERO
               MOVE    IDX-GAI             TO  IDX-Z
               IF     (FLG-JIKASAN         =   ZERO         )  AND
                      (WRK-COM-KZMFLG (IDX-Z)  =   SPACE    )  AND
                      (LNK-ORCSC-TOROKU    NOT =   "2"   )
      *            ³°Íè¤Î¾ì¹ç¤Î¤ß
                 AND  (SPA-NYUGAIKBN       =   "2"       )
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
      *        »þ´ÖÆþÎÏÄÉ²Ã
               MOVE    "840000071"             TO  WRK-SRYCD
               IF      WRK-COM-INPUTCD(IDX-Z + 1)  =   WRK-SRYCD
                                                   OR  CONS-JIKAN
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-SATU
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    SPACE               TO  WRK-SRYCD-2
                   MOVE    ZERO                TO  FLG-NENREI
                   PERFORM 1003-JIDOU-CHK-SEC
                   PERFORM 1002-JIDOUSET-SEC
                   IF      FLG-ADD             =   1
                       MOVE    "1"             TO  WRK-COM-IN  (IDX-S)
                       MOVE    "2"             TO  WRK-COM-IN2 (IDX-S)
                   END-IF
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê²ó¿ôÊÔ½¸½èÍý
      *****************************************************************
       2002-CTMRI-CNT-SEC                SECTION.
      *
           INITIALIZE                      WRK-CTMRI-CNT-G
      *    Æ¬Éô
           MOVE    "099700101"         TO  WRK-SRYCD
           PERFORM 20021-CTMRI-SANTEI-SEC
           MOVE    LNK-SC91-CNT        TO  WRK-CTMRI-CNT (1)
           MOVE    LNK-SC91-DAYCNT     TO  WRK-CTMRI-DCNT(1)
      *    ¶í´´
           MOVE    "099700102"         TO  WRK-SRYCD
           PERFORM 20021-CTMRI-SANTEI-SEC
           MOVE    LNK-SC91-CNT        TO  WRK-CTMRI-CNT (2)
           MOVE    LNK-SC91-DAYCNT     TO  WRK-CTMRI-DCNT (2)
      *        »Í»è
           MOVE    "099700103"         TO  WRK-SRYCD
           PERFORM 20021-CTMRI-SANTEI-SEC
           MOVE    LNK-SC91-CNT        TO  WRK-CTMRI-CNT (3)
           MOVE    LNK-SC91-DAYCNT     TO  WRK-CTMRI-DCNT (3)
      *
           .
       2002-CTMRI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20021-CTMRI-SANTEI-SEC                SECTION.
      *
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯
           INITIALIZE                      ORCSC91AREA
      *****MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    "4"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    ZERO                TO  LNK-SC91-GMN-IDX
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
      *    Åö·î»»Äê¤Ê¤·
           IF      LNK-SC91-ATO-CNT    <=  LNK-SC91-CNT
               COMPUTE LNK-SC91-CNT    =   LNK-SC91-CNT
                                       -   LNK-SC91-ATO-CNT
           ELSE
               MOVE    ZERO            TO  LNK-SC91-CNT
           END-IF
      *    ÄûÀµ»þ
           IF     LNK-ORCSC-SYORIFLG   =   "1"
               IF      LNK-SC91-DAYCNT     <=  LNK-SC91-CNT
                   COMPUTE LNK-SC91-CNT    =   LNK-SC91-CNT
                                           -   LNK-SC91-DAYCNT
               END-IF
           ELSE
               MOVE    ZERO                TO  LNK-SC91-DAYCNT
           END-IF
      *
           .
       20021-CTMRI-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CTMRI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDCD
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    Ï«ºÒ¤Î»þ¡¢·Ù¹ð¥Á¥§¥Ã¥¯
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               PERFORM 20012-RSI-CTMRI-CHK-SEC
               GO      TO      2001-CTMRI-CHK-EXT
           END-IF
      *
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    WRK-COM-KAISU (IDX-Z)   TO  WRK-CTMRI-KAI
           ELSE
               MOVE    WRK-COM-NYU-KAI(IDX-Z)  TO  WRK-CTMRI-KAI
           END-IF
      *
      *    Êñ³çÄþ¸º¶èÊ¬È½Äê
           EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *        Æ¬Éô
               WHEN    "201"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022290"
                       IF     (WRK-CTMRI-CNT (1)   =   ZERO )  AND
                              (WRK-CTMRI-DCNT(1)   =   ZERO )
                            MOVE    "K003"             TO  WRK-KIDCD
                       END-IF
                   ELSE
                       IF      WRK-CTMRI-CNT (1)   >   ZERO
                           MOVE    "170022290"     TO  WRK-SRYCD
                       END-IF
                   END-IF
                   ADD     WRK-CTMRI-KAI       TO  WRK-CTMRI-CNT (1)
      *        ¶í´´
               WHEN    "202"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022390"
                       IF     (WRK-CTMRI-CNT (2)   =   ZERO )  AND
                              (WRK-CTMRI-DCNT(2)   =   ZERO )
                            MOVE    "K003"              TO  WRK-KIDCD
                       END-IF
                   ELSE
                       IF      WRK-CTMRI-CNT (2)   >   ZERO
                           MOVE    "170022390"     TO  WRK-SRYCD
                       END-IF
                   END-IF
                   ADD     WRK-CTMRI-KAI       TO  WRK-CTMRI-CNT (2)
      *        »Í»è
               WHEN    "203"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022490"
                       IF     (WRK-CTMRI-CNT (3)   =   ZERO )  AND
                              (WRK-CTMRI-DCNT (3)   =   ZERO )
                            MOVE    "K003"              TO  WRK-KIDCD
                       END-IF
                   ELSE
                       IF      WRK-CTMRI-CNT (3)   >   ZERO
                           MOVE    "170022490"     TO  WRK-SRYCD
                       END-IF
                   END-IF
                   ADD     WRK-CTMRI-KAI       TO  WRK-CTMRI-CNT (3)
           END-EVALUATE
      *
           IF      WRK-SRYCD       NOT =   SPACE
      *        ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
               MOVE    WRK-COM-CHK-SRYCD(IDX-Z)
                                           TO  WRK-HEN-SRYCD
               MOVE    1                   TO  FLG-ARI
               MOVE    IDX-Z               TO  IDX-T
               PERFORM 1002-JIDOUSET-SEC
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-Z)
               IF      FLG-ADD             =   1
      *            ÊÑ¹¹¤Î¿ÇÎÅ¥³¡¼¥É¤ò¥»¥Ã¥È
                   MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD(IDX-S)
               END-IF
           END-IF
      *
           IF      WRK-KIDCD       NOT =  SPACE
      *        £Ã£Ô¡¥£Í£Ò£É¤Î·Ù¹ð¥Á¥§¥Ã¥¯
               IF      WRK-COM-KZMFLG (IDX-Z)  =   SPACE
                   MOVE    "K003"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR
                                                               (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
               END-IF
           END-IF
           .
      *
       2001-CTMRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   Ï«ºÒ¤Î»þ¡¢·Ù¹ð¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       20012-RSI-CTMRI-CHK-SEC                SECTION.
      *
      *    Êñ³çÄþ¸º¶èÊ¬È½Äê
           EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *        Æ¬Éô
               WHEN    "201"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022290"
                        MOVE    "K003"              TO  WRK-KIDCD
                   END-IF
      *        ¶í´´
               WHEN    "202"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022390"
                        MOVE    "K003"              TO  WRK-KIDCD
                   END-IF
      *        »Í»è
               WHEN    "203"
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170022490"
                        MOVE    "K003"              TO  WRK-KIDCD
                   END-IF
           END-EVALUATE
           IF      WRK-KIDCD       NOT =  SPACE
      *        £Ã£Ô¡¥£Í£Ò£É¤Î·Ù¹ð¥Á¥§¥Ã¥¯
               IF      WRK-COM-KZMFLG (IDX-Z)  =   SPACE
                   MOVE    "K003"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR
                                                               (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
               END-IF
           END-IF
      *
           .
      *
       20012-RSI-CTMRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°ºï½ü½èÍý
      *****************************************************************
       10023-JIDOU-DEL-SEC                SECTION.
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                      WRK-SCR-REC
           MOVE    ZERO                TO  IDX-D2
           PERFORM VARYING     IDX-D1  FROM    1   BY  1
                   UNTIL       IDX-D1      >   SPA-MAX-LINE
               IF      IDX-T           =   IDX-D1
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-D2
                   MOVE    HOZ-GMN-TBL (IDX-D1)
                                           TO  WRK-GMN-TBL(IDX-D2)
               END-IF
           END-PERFORM
           .
      *
       10023-JIDOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°»»Äê½èÍý
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
      *    Êñ³ç¤Þ¤È¤áÆþÎÏ¤ÏÂÐ¾Ý³°
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *    ÄûÀµ¤Ç¼«Æ°È¯À¸¤Ê¤·¤Î¾ì¹ç
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           MOVE    ZERO                TO  FLG-CTMRI
           MOVE    ZERO                TO  FLG-XSEN
           MOVE    ZERO                TO  FLG-KAKUI
           MOVE    ZERO                TO  FLG-ENKALL
           MOVE    ZERO                TO  FLG-ENK1
           MOVE    ZERO                TO  FLG-ENK2
           MOVE    ZERO                TO  FLG-ENK3
           MOVE    ZERO                TO  FLG-XSEN-ARI
           MOVE    ZERO                TO  FLG-CTMRI-ARI
           MOVE    ZERO                TO  FLG-KAKUI-ARI
           INITIALIZE                  WRK-NYUINDAY-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
             IF   (WRK-COM-HKNCOMBI (IDX-Z) NOT =   "9999")
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "70")  AND
                      (WRK-COM-INPUTCD(IDX-Z)(1:1)
                                               =   "1"  )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"  )
      *
                   EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)
      *                £ØÀþÍ­¤ê
                       WHEN    "721"
                       WHEN    "720"
                           MOVE    1                   TO  FLG-XSEN
                           MOVE    1                   TO  FLG-XSEN-ARI
                           IF      IDX-XSEN            =   ZERO
                               MOVE    IDX-Z           TO  IDX-XSEN
                           END-IF
                   END-EVALUATE
      *
      *            ¸¡ººÅù¼Â»ÜÈ½Äê¥°¥ë¡¼¥×
                   EVALUATE    WRK-COM-KNSJISGRPKBN (IDX-Z)
      *                ³Ë°å³Ø
                       WHEN    31
                           MOVE    1                   TO  FLG-KAKUI
                           MOVE    1                   TO  FLG-KAKUI-ARI
                           IF      IDX-KAKUI           =   ZERO
                               MOVE    IDX-Z           TO  IDX-KAKUI
                           END-IF
      *                ¥³¥ó¥Ô¥å¡¼¥¿
                       WHEN    32
                           MOVE    1                   TO  FLG-CTMRI
                           MOVE    1                   TO  FLG-CTMRI-ARI
                           IF      IDX-CTMRI           =   ZERO
                               MOVE    IDX-Z           TO  IDX-CTMRI
                           END-IF
                   END-EVALUATE
      *
      *            ±ó³Ö²èÁü¿ÇÃÇ´ÉÍý²Ã»»¥Á¥§¥Ã¥¯
                   EVALUATE    WRK-COM-INPUTCD (IDX-Z)
                       WHEN    "170024910"
                       WHEN    "170025010"
                           MOVE    1                   TO  FLG-ENKALL
                       WHEN    "170025910"
      *                    £ØÀþ
                           MOVE    1                   TO  FLG-ENK1
                       WHEN    "170026100"
                       WHEN    "170026210"
      *                    ³Ë°å³Ø
                           MOVE    1                   TO  FLG-ENK2
                       WHEN    "170026110"
                       WHEN    "170026310"
      *                    ¥³¥ó¥Ô¥å¡¼¥¿
                           MOVE    1                   TO  FLG-ENK3
                   END-EVALUATE
               END-IF
      *
      *        ²èÁü¿ÇÃÇ¤ÇÆþ±¡²ó¿ô¤Î»þ¡¢²ó¿ô½¸·×
               IF     (SPA-NYUGAIKBN               =   "1" )  AND
                      (WRK-COM-SRYKBN (IDX-Z)(1:1) =   "7" )  AND
                     ((WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "*" ) OR
                      (WRK-COM-KAIFLG (IDX-Z)      =   "1" ) OR
                      (WRK-COM-ZAIEND (IDX-Z)      =   "1" ))
                   PERFORM 3009-NYUINKAISU-SEC
               END-IF
               IF      WRK-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-XSEN-ARI
                   MOVE    ZERO                TO  FLG-CTMRI-ARI
                   MOVE    ZERO                TO  FLG-KAKUI-ARI
               END-IF
      *
             END-IF
           END-PERFORM
      *    ²èÁü¿ÇÃÇ´ÉÍý²Ã»»ÄÉ²Ã
           PERFORM 3001-GAZOUKSN-SET-SEC
      *
           .
      *
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÁü¿ÇÃÇ´ÉÍý²Ã»»ÄÉ²Ã½èÍý
      *****************************************************************
       3001-GAZOUKSN-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDXS
      *
      *    ²èÁü¿ÇÃÇ´ÉÍý²Ã»»ÄÉ²Ã¡Ê¼Ì¿¿¿ÇÃÇ¡Ë
           IF     (FLG-XSEN            =   1    )  AND
                  (FLG-ENKALL          =   ZERO )  AND
                  (FLG-ENK1            =   ZERO )
      *        ¥·¥¹¥Æ¥à´ÉÍý¤Î¶èÊ¬È½Äê
             AND  (SPA-GAZOUKSN1       =   1   )
      *        ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±
               MOVE    "170025210"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               MOVE    WRK-XSEN-DAY        TO  WRK-NYU-DAY
               MOVE    SPACE               TO  WRK-SRYKA
               MOVE    SPACE               TO  WRK-HKNCOMBI
               IF      IDX-XSEN            >   ZERO
                   MOVE    WRK-COM-SRYKA (IDX-XSEN)    TO  WRK-SRYKA
                   MOVE    WRK-COM-HKNCOMBI(IDX-XSEN)  TO  WRK-HKNCOMBI
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
           END-IF
      *
      *    ³Ë°å³Ø¿ÇÃÇÎÁ²Ã»»Í­¤ê
           IF      FLG-KAKUI           =   1
      *        ³Ë°å³Ø¿ÇÃÇÎÁ²Ã»»
               MOVE    "170015310"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               MOVE    WRK-KAKUI-DAY       TO  WRK-NYU-DAY
               MOVE    SPACE               TO  WRK-SRYKA
               MOVE    SPACE               TO  WRK-HKNCOMBI
               IF      IDX-KAKUI           >   ZERO
                   MOVE    WRK-COM-SRYKA (IDX-KAKUI)   TO  WRK-SRYKA
                   MOVE    WRK-COM-HKNCOMBI(IDX-KAKUI) TO  WRK-HKNCOMBI
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               IF     (FLG-ERR             =   ZERO )  AND
                      (FLG-ENKALL          =   ZERO )  AND
                      (FLG-ENK2            =   ZERO )
      *        ¥·¥¹¥Æ¥à´ÉÍý¤Î¶èÊ¬È½Äê
                 AND  (SPA-GAZOUKSN1       =   1   )
      *            ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£²¡Ê³Ë°å³Ø¡Ë
                   MOVE    "170025610"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
                   MOVE    WRK-KAKUI-DAY       TO  WRK-NYU-DAY
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       CONTINUE
                   ELSE
      *                ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê³Ë°å³Ø¡Ë
                       IF      FLG-ARI         =   ZERO
                           MOVE    "170025410"         TO  WRK-SRYCD
                           MOVE    SPACE               TO  WRK-SRYCD-2
                           MOVE    WRK-KAKUI-DAY       TO  WRK-NYU-DAY
                           PERFORM 3002-KASAN-SYORI-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    ¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇÎÁ²Ã»»Í­¤ê
           IF      FLG-CTMRI           =   1
      *        ¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ
               MOVE    "170015410"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               MOVE    WRK-CTMRI-DAY       TO  WRK-NYU-DAY
               MOVE    SPACE               TO  WRK-SRYKA
               MOVE    SPACE               TO  WRK-HKNCOMBI
               IF      IDX-CTMRI           >   ZERO
                   MOVE    WRK-COM-SRYKA (IDX-CTMRI)   TO  WRK-SRYKA
                   MOVE    WRK-COM-HKNCOMBI(IDX-CTMRI) TO  WRK-HKNCOMBI
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               IF     (FLG-ERR             =   ZERO )  AND
                      (FLG-ENKALL          =   ZERO )  AND
                      (FLG-ENK3            =   ZERO )
      *            ¥·¥¹¥Æ¥à´ÉÍý¤Î¶èÊ¬È½Äê
                 AND  (SPA-GAZOUKSN1       =   1   )
      *            ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£²¡Ê¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¡Ë
                   MOVE    "170025710"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
                   MOVE    WRK-CTMRI-DAY       TO  WRK-NYU-DAY
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       CONTINUE
                   ELSE
      *                ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¡Ë
                       IF      FLG-ARI         =   ZERO
                           MOVE    "170025510"         TO  WRK-SRYCD
                           MOVE    SPACE               TO  WRK-SRYCD-2
                           MOVE    WRK-CTMRI-DAY       TO  WRK-NYU-DAY
                           PERFORM 3002-KASAN-SYORI-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       3001-GAZOUKSN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡Æü½èÍý
      *****************************************************************
       3009-NYUINKAISU-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDN1
           IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)  NOT =   "*"
               MOVE    SPA-SRYYMD (7:2)    TO  IDN1
           ELSE
               PERFORM VARYING     IDN2    FROM    1   BY  1
                       UNTIL       IDN2    >   31
                   IF      WRK-COM-NYUINDAY (IDX-Z IDN2) >   ZERO
                       MOVE    IDN2            TO  IDN1
                       MOVE    31              TO  IDN2
                   END-IF
               END-PERFORM
           END-IF
      *    £ØÀþ
           IF      FLG-XSEN-ARI        =   1
      *        Æþ±¡»þ¤Î½éÆü
               IF     (WRK-XSEN-DAY    =   ZERO )  OR
                      (WRK-XSEN-DAY    >   IDN1 )
                   MOVE    IDN1            TO  WRK-XSEN-DAY
               END-IF
           END-IF
      *    ³Ë°å³Ø
           IF      FLG-KAKUI-ARI       =   1
      *        Æþ±¡»þ¤Î½éÆü
               IF     (WRK-KAKUI-DAY   =   ZERO )  OR
                      (WRK-KAKUI-DAY   >   IDN1 )
                   MOVE    IDN1            TO  WRK-KAKUI-DAY
               END-IF
           END-IF
      *    ¥³¥ó¥Ô¥å¡¼¥¿
           IF      FLG-CTMRI-ARI       =   1
      *        Æþ±¡»þ¤Î½éÆü
               IF     (WRK-CTMRI-DAY   =   ZERO )  OR
                      (WRK-CTMRI-DAY   >   IDN1 )
                   MOVE    IDN1            TO  WRK-CTMRI-DAY
               END-IF
           END-IF
      *
           .
       3009-NYUINKAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁ»»Äê½èÍý
      *****************************************************************
       3002-KASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    º£²óÆþÎÏ¤Ë¤Ê¤¤¤³¤È
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF      WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
           IF      WRK-SRYKA           =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           END-IF
           IF      WRK-HKNCOMBI        =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           END-IF
      *    ÄÉ²Ã¤Î¥Á¥§¥Ã¥¯
           PERFORM 3001-SANTEI-CHK-SEC
           IF      FLG-ERR             =   ZERO
      *
               MOVE    WRK-SRYCD       TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU       =   ZERO
      *
                   ADD     1               TO  IDXS
                   MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                       (08 IDXS)
                   MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                       (08 IDXS)
                   MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                       (08 IDXS)
                   MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                       (08 IDXS)
                   MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                       (08 IDXS)
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                        (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                        (08 IDXS)
      *            ¼«Æ°È¯À¸Ê¬¤È¤·¤Ê¤¤
                   MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                         (08 IDXS)
      *            Æþ±¡²ó¿ô
                   IF      WRK-NYU-DAY     >   ZERO
                       MOVE    "*/"           TO  LNK-ORCSC-INPUTCD
                                                    (08 IDXS 01)
                       MOVE    WRK-NYU-DAY    TO  LNK-ORCSC-INPUTCD
                                                    (08 IDXS 01)(3:2)
                   END-IF
               END-IF
           END-IF
      *
           .
       3002-KASAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ARI
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           MOVE    WRK-GMN-MAX         TO  WRK-IDX
           ADD     1                   TO  WRK-IDX
           IF      WRK-IDX             >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE         TO  WRK-IDX
           END-IF
      *    ÅÀ¿ô¥Þ¥¹¥¿ÊÔ½¸¡¦´ðËÜ¥Á¥§¥Ã¥¯
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-IDX             TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       HOZ-SCR-REC
                                       SPA-AREA
      *
      *    ¥¨¥é¡¼¤Î»þ¡¢ÄÉ²Ã¤Ê¤·
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        »»ÄêÍúÎò¤¢¤ê¤Î»þ
               IF      LNK-SC92-ERRCD3 NOT =   SPACE
                   MOVE    1               TO  FLG-ARI
               END-IF
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       3001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
