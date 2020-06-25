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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW20.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÀßÄê
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÉÂÅï´ÉÍý
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/07/30    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 00.00.01     NACL¡ÝÂÀÅÄ   02/11/29  Í­¸ú³«»ÏÆü¤ò´ð½à¤Ë¥·¥¹´É¤Î
      *                                     ¾ðÊó¤ò¼èÆÀ¤¹¤ë¤è¤¦½¤Àµ
      *                                     ÉÂÅï²Ê¤òºÇÂç£¹£¹·ï¤ËÊÑ¹¹
      * 00.00.02     NACL¡ÝÂÀÅÄ   03/01/29  Í­¸ú³«»ÏÆü¤Î¥Á¥§¥Ã¥¯ÄÉ²Ã
      * 01.00.01     NACL-ÂÀÅÄ    03.11.17  ¹¹¿·ÆüÉÕÊÔ½¸ÂÐ±þ
      * 02.08.00     NACL-ÂÀÅÄ    06.03.09  Ê¿À®£±£¸Ç¯ÅÙ²þÀµÂÐ±þ
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/05/08  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóÀßÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "W01COMMON-SPA".
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
           COPY    "ENUM-VALUE".
      *
      *    ²èÌÌÍÑ£Ó£Ð£Á
           COPY    "W20SCR-SPA".
      *
      *    ¥Õ¥é¥°¥¨¥ê¥¢
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-BTU-NUM                         PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-BRM-SONZAI                      PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-NYUKHN                          PIC 9(01).
           03  FLG-SYS5001                         PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-SETKHNRYO                       PIC 9(01).
           03  FLG-BTULST-ROW                      PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-KSNERR                          PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
           03  FLG-RYOYOSANTEI                     PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXG                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-MAX                             PIC 9(05).
           03  IDX-LST                             PIC 9(05).
           03  IDX-BTULST                          PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
           03  IDX-GKSN                            PIC 9(05).
           03  IDX-SST                             PIC 9(05).
           03  IDX-KSN2                            PIC 9(05).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-STR                             PIC 9(05).
           03  CNT-SST                             PIC 9(05).
           03  CNT-KSN                             PIC 9(05).
           03  CNT-SANTEI                          PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-MAX                             PIC 9(05).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-MAE-INPUT               PIC X(400).
           03  WRK-KANACHK-OUT-INPUT               PIC X(400).
           03  WRK-BTUNUM                          PIC 9(02).
           03  WRK-ZZZ9                            PIC ZZZ9.
           03  WRK-ZZZZZZZZZ9                      PIC Z(09)9.
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
               05  WRK-CMB-ITM4                    PIC X(200).
               05  WRK-CMB-ITM5                    PIC X(200).
               05  WRK-CMB-ITM6                    PIC X(200).
           03  WRK-KIJUN-YMD                       PIC X(08).
           03  WRK-BTU-NINZU                       PIC 9(05).
           03  WRK-BTULST                          PIC 9(05).
           03  WRK-TNS-NAME                        PIC X(30).
           03  WRK-STYUKYMD                        PIC X(08).
           03  WRK-EDYUKYMD                        PIC X(08).
           03  WRK-CDKBNNUM                        PIC 9(03).
           03  WRK-CDKOUBAN                        PIC 9(02).
           03  WRK-TK-CDKBNNUM                     PIC 9(03).
           03  WRK-TK-CDKOUBAN                     PIC 9(02).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-KJNYMD                      PIC X(08).
           03  WRK-99                              PIC 9(02).
           03  WRK-BTULST-SEL                      PIC 9(02).
           03  WRK-STATE                           PIC S9(9) BINARY.
      *    ÆþÎÏ¹àÌÜÌ¾
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-YUKOSTYMD                       PIC X(08).
           03  WRK-YUKOEDYMD                       PIC X(08).
           03  WRK-KJN-YUKSTYMD                    PIC X(08).
           03  WRK-PATHNAME                        PIC X(64).
           03  WRK-BASE-CUR                        PIC 9(03).
           03  WRK-KSNCD                           PIC X(09).
           03  WRK-KSNCD-A104                      PIC X(09).
           03  WRK-KSNCD-A105                      PIC X(09).
           03  WRK-KSNCD-A312                      PIC X(09).
           03  WRK-KSNCD-A109                      PIC X(09).
           03  WRK-KSNIDX                          PIC 9(03).
           03  WRK-KSNKBN                          PIC 9(03).
           03  WRK-NYUINKBN                        PIC 9(03).
           03  WRK-SSTKJNCD-G.
               05  WRK-SSTKJNCD-OCC                OCCURS 10.
                   07  WRK-SSTKJNCDSEP             PIC X(01).
                   07  WRK-SSTKJNCD                PIC X(04).
                   07  WRK-SSTKJNCD-R              REDEFINES
                       WRK-SSTKJNCD.
                       09  WRK-SSTKJNCD9           PIC 9(04).
           03  WRK-KSNMEI                          PIC X(100).
           03  WRK-PNT                             PIC 9(03).
           03  WRK-STYMD                           PIC X(08).
           03  WRK-EDYMD                           PIC X(08).
           03  WRK-KBN1-G.
               05  WRK-KBN1                        PIC 9(01).
           03  WRK-KBN2-G.
               05  WRK-KBN2                        PIC 9(02).
           03  WRK-KBN4-G.
               05  WRK-KBN4                        PIC 9(04).
           03  WRK-TEN                             PIC 9(05).
           03  WRK-HYOUKACD                        PIC X(09).
           03  WRK-NUM02X.
               05  WRK-NUM02                       PIC 9(02).
           03  WRK-FROM                            PIC 9(05).
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-KSN-STATE                       PIC S9(9)   BINARY.
           03  WRK-KSN-PAGENO                      PIC 9(03).
           03  WRK-DUMMYCD                         PIC X(09).
           03  WRK-REALCD                          PIC X(09).
           03  WRK-TOKHOSPSBT                      PIC X(02).
           03  WRK-KHNCD                           PIC X(09).
           03  WRK-KSNNUM-G.
               05  WRK-KSNNUMFL                    PIC X(03).
               05  WRK-KSNNUM                      PIC 9(02).
           03  WRK-YKTTYMDNUM-G.
               05  WRK-YKTTYMDNUMDFL               PIC X(07).
               05  WRK-YKTTYMDNUM                  PIC 9(02).
      *
       01  WRK-ROW.
           03  WRK-BTULST-ROW                      PIC S9(9)   BINARY.
      *
       01  TKSN-AREA.
           03  TKSN-MAX                            PIC 9(03).
           03  TKSN-OCC                            OCCURS  48.
               05  TKSN-LBL                        PIC X(36).
               05  TKSN-IDX                        PIC 9(03).
               05  TKSN-SRYCD                      PIC X(09).
      *
       01  TGKSN-AREA.
           03  TGKSN-OCC                           OCCURS  46.
               05  TGKSN-LBLKSN                    PIC X(36).
               05  TGKSN-CMB-KSN-G.
                   07  TGKSN-KSN                   PIC X(30).
                   07  TGKSN-KSN-STATE             PIC S9(9) BINARY.
                   07  TGKSN-KSN-CNT               PIC S9(9) BINARY.
                   07  TGKSN-KSN-ITM               PIC X(30) OCCURS  10.
                   07  TGKSN-KSN-ITM-STATE         PIC S9(9) BINARY.
      *
       01  SSTKJN-AREA.
           03  SSTKJN-VAL.
               05  SSTKJN-VAL-OCC                  PIC X(500)
                                                   OCCURS  20.
           03  SSTKJN-R    REDEFINES       SSTKJN-VAL.
               05  SSTKJN-CD                       PIC X(01)
                                                   OCCURS  10000.
      *
      *    ¸ÇÄêÃÍÎÎ°è
       01  CONST-AREA.
           03  CONST-BTULST-MAX        PIC 9(03) VALUE 50.
           03  CONST-RIREKILST-MAX     PIC 9(03) VALUE 20.
           03  CONST-TOKNYUIN-MAX      PIC 9(03) VALUE 50.
           03  CONST-SRYKA-MAX         PIC 9(03) VALUE 99.
           03  CONST-SBT-MAX           PIC 9(03) VALUE 15.
           03  CONST-YKNKNG-MAX        PIC 9(03) VALUE 10.
           03  CONST-KHNRYO-MAX        PIC 9(03) VALUE 50.
           03  CONST-SYOUBTUBRM-MAX    PIC 9(03) VALUE 16.
           03  CONST-KSN-MAX           PIC 9(03) VALUE 48.
           03  CONST-GKSN-MAX          PIC 9(03) VALUE 46.
           03  CONST-R02-AREA.
      *        Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
               05  CONST-R02-A249-VAL.
                   07  FILLER              PIC X(09) VALUE "190218610".
                   07  FILLER              PIC X(09) VALUE "190194110".
                   07  FILLER              PIC X(09) VALUE "190218810".
               05  CONST-R02-A249-R
                               REDEFINES   CONST-R02-A249-VAL.
                   07  CONST-R02-A249      PIC X(09) OCCURS 3.
           03  CONST-H30-AREA.
      *        °ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¤ÏÇÑ»ß
      *        ´Ç¸îÉ¬Í×ÅÙ²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
               05  CONST-H30-A105C3-VAL.
                   07  FILLER              PIC X(09) VALUE "190189170".
                   07  FILLER              PIC X(09) VALUE "190189270".
                   07  FILLER              PIC X(09) VALUE "190189370".
               05  CONST-H30-A105C3-R
                               REDEFINES   CONST-H30-A105C3-VAL.
                   07  CONST-H30-A105C3    PIC X(09) OCCURS 3.
      *        °ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¤ÏÇÑ»ß
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
               05  CONST-H30-A105C04       PIC X(09) VALUE "190134970".
      *        Ìë´Ö´Ç¸î²Ã»»
               05  CONST-H30-A101C13       PIC X(09) VALUE "190204570".
      *        ´Ç¸îÊä½õ²Ã»»¡Ê¾ã³²¼ÔÉÂÅï¡Ë
               05  CONST-H30-A106C09       PIC X(09) VALUE "190205570".
      *        Ìë´Ö´Ç¸îÂÎÀ©²Ã»»
               05  CONST-H30-A106C10       PIC X(09) VALUE "190205770".
      *        ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
               05  CONST-H30-A207-04-VAL.
                   07  FILLER              PIC X(09) VALUE "190190810".
                   07  FILLER              PIC X(09) VALUE "190190910".
                   07  FILLER              PIC X(09) VALUE "190191010".
                   07  FILLER              PIC X(09) VALUE "190206210".
               05  CONST-H30-A207-04-R
                               REDEFINES CONST-H30-A207-04-VAL.
                   07  CONST-H30-A207-04   PIC X(09) OCCURS 4.
      *        £Á£³£°£¸¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
               05  CONST-H30-A308-03C07     PIC X(09) VALUE "190212570".
      *        £Á£³£±£±´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
               05  CONST-H30-A311C05        PIC X(09) VALUE "190213070".
      *        £Á£³£±£±¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
               05  CONST-H30-A311-03C05     PIC X(09) VALUE "190213170".
      *        £Á£³£±£´Ç§ÃÎ¾ÉÌë´ÖÂÐ±þ²Ã»»
               05  CONST-H30-A314C03        PIC X(09) VALUE "190151870".
           03  CONST-H28-AREA.
      *        ´Ç¸îÉ¬Í×ÅÙ²Ã»»¡Ê°ìÈÌ¡Ë
               05  CONST-H28-A100C6-VAL.
                   07  FILLER              PIC X(09) VALUE "190183070".
                   07  FILLER              PIC X(09) VALUE "190183170".
                   07  FILLER              PIC X(09) VALUE "190183270".
               05  CONST-H28-A100C6-R
                               REDEFINES CONST-H28-A100C6-VAL.
                   07  CONST-H28-A100C6    PIC X(09) OCCURS 3.
      *        ´Ç¸îÉ¬Í×ÅÙ²Ã»»¡ÊÆÃÄêµ¡Ç½¡Ë
               05  CONST-H28-A104C5-VAL.
                   07  FILLER              PIC X(09) VALUE "190188870".
                   07  FILLER              PIC X(09) VALUE "190188970".
                   07  FILLER              PIC X(09) VALUE "190189070".
               05  CONST-H28-A104C5-R
                               REDEFINES CONST-H28-A104C5-VAL.
                   07  CONST-H28-A104C5    PIC X(09) OCCURS 3.
      *        ´Ç¸îÉ¬Í×ÅÙ²Ã»»¡ÊÀìÌç¡Ë
               05  CONST-H28-A105C3-VAL.
                   07  FILLER              PIC X(09) VALUE "190189170".
                   07  FILLER              PIC X(09) VALUE "190189270".
                   07  FILLER              PIC X(09) VALUE "190189370".
               05  CONST-H28-A105C3-R
                               REDEFINES CONST-H28-A105C3-VAL.
                   07  CONST-H28-A105C3    PIC X(09) OCCURS 3.
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡Ê°ìÈÌ¡Ë
               05  CONST-H28-A100C07       PIC X(09) VALUE "190131570".
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌç¡Ë
               05  CONST-H28-A105C04       PIC X(09) VALUE "190134970".
      *        £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»¡Ê°ìÈÌ¡Ë
               05  CONST-H28-A100C12       PIC X(09) VALUE "190166070".
      *        £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»¡ÊÆÃÄêµ¡Ç½¡Ë
               05  CONST-H28-A104C10       PIC X(09) VALUE "190169070".
      *        £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»¡ÊÀìÌç¡Ë
               05  CONST-H28-A105C09       PIC X(09) VALUE "190169170".
      *        Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»¡ÊÀº¿À¡Ë
               05  CONST-H28-A103C07       PIC X(09) VALUE "190168970".
      *        Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»¡ÊÀº¿À²ÊÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
               05  CONST-H28-A312C06       PIC X(09) VALUE "190177570".
      *        ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
               05  CONST-H28-A101C10       PIC X(09) VALUE "190168070".
      *        Í­¾²¿ÇÎÅ½êºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ¡Ë
               05  CONST-H28-A108C11       PIC X(09) VALUE "190190270".
      *        Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
               05  CONST-H28-A109C11       PIC X(09) VALUE "190190370".
      *        ÂÎÀ©¶¯²½²Ã»»
               05  CONST-H28-A308C05-VAL.
                   07  FILLER              PIC X(09) VALUE "190194870".
                   07  FILLER              PIC X(09) VALUE "190194970".
               05  CONST-H28-A308C05-R
                               REDEFINES CONST-H28-A308C05-VAL.
                   07  CONST-H28-A308C05   PIC X(09) OCCURS 2.
      *        ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
               05  CONST-H28-A308-03C03    PIC X(09) VALUE "190176970".
      *        ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
               05  CONST-H28-A308-03C04    PIC X(09) VALUE "190177070".
      *        Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
               05  CONST-H28-A249          PIC X(09) VALUE "190194110".
      *        °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±
               05  CONST-H28-A207-0201-VAL.
                   07  FILLER              PIC X(09) VALUE "190172210".
                   07  FILLER              PIC X(09) VALUE "190172310".
                   07  FILLER              PIC X(09) VALUE "190172410".
                   07  FILLER              PIC X(09) VALUE "190172510".
                   07  FILLER              PIC X(09) VALUE "190172610".
                   07  FILLER              PIC X(09) VALUE "190172710".
                   07  FILLER              PIC X(09) VALUE "190172810".
                   07  FILLER              PIC X(09) VALUE "190172910".
               05  CONST-H28-A207-0201-R
                               REDEFINES CONST-H28-A207-0201-VAL.
                   07  CONST-H28-A207-0201 PIC X(09) OCCURS 8.
      *        °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²
               05  CONST-H28-A207-0202-VAL.
                   07  FILLER              PIC X(09) VALUE "190135710".
                   07  FILLER              PIC X(09) VALUE "190135810".
                   07  FILLER              PIC X(09) VALUE "190127110".
                   07  FILLER              PIC X(09) VALUE "190145510".
                   07  FILLER              PIC X(09) VALUE "190145610".
                   07  FILLER              PIC X(09) VALUE "190127210".
                   07  FILLER              PIC X(09) VALUE "190127310".
                   07  FILLER              PIC X(09) VALUE "190127410".
               05  CONST-H28-A207-0202-R
                               REDEFINES CONST-H28-A207-0202-VAL.
                   07  CONST-H28-A207-0202 PIC X(09) OCCURS 8.
      *        µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
               05  CONST-H28-A207-03-VAL.
                   07                      PIC X(09) VALUE "190145710".
                   07                      PIC X(09) VALUE "190145810".
                   07                      PIC X(09) VALUE "190135910".
                   07                      PIC X(09) VALUE "190136010".
               05  CONST-H28-A207-03-R
                               REDEFINES CONST-H28-A207-03-VAL.
                   07  CONST-H28-A207-03 PIC X(09) OCCURS 4.
      *        Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
               05  CONST-H28-A207-03C02-VAL.
                   07  FILLER              PIC X(09) VALUE "190173010".
                   07  FILLER              PIC X(09) VALUE "190145970".
                   07  FILLER              PIC X(09) VALUE "190146070".
               05  CONST-H28-A207-03C02-R
                               REDEFINES CONST-H28-A207-03C02-VAL.
                   07  CONST-H28-A207-03C02  PIC X(09) OCCURS 3.
      *        Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£°£·¡Ý£³¡Ë
               05  CONST-H28-A207-03C03    PIC X(09) VALUE "190190770".
      *        ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
               05  CONST-H28-A207-04-VAL.
                   07  FILLER              PIC X(09) VALUE "190190810".
                   07  FILLER              PIC X(09) VALUE "190190910".
                   07  FILLER              PIC X(09) VALUE "190191010".
               05  CONST-H28-A207-04-R
                               REDEFINES CONST-H28-A207-04-VAL.
                   07  CONST-H28-A207-04   PIC X(09) OCCURS 3.
      *        ÆñÉÂÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
               05  CONST-H28-A210          PIC X(09) VALUE "190101770".
      *        ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
               05  CONST-H28-A211          PIC X(09) VALUE "190101970".
      *        ´Ç¸îÇÛÃÖ²Ã»»
               05  CONST-H28-A213          PIC X(09) VALUE "190102070".
      *        Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»
               05  CONST-H28-A214C02       PIC X(09) VALUE "190191170".
      *        Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£±£´¡Ë
               05  CONST-H28-A214C03       PIC X(09) VALUE "190191270".
      *        Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
               05  CONST-H28-A228          PIC X(09) VALUE "190106970".
      *        Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
               05  CONST-H28-A230-02       PIC X(09) VALUE "190127810".
      *        ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ë
               05  CONST-H28-A307C02       PIC X(09) VALUE "190111370".
      *        µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ê²óÉü´ü¥ê¥ÏÆþ±¡ÎÁ£²Ëô¤Ï£³¡Ë
               05  CONST-H28-A308C02       PIC X(09) VALUE "190140870".
      *        ¿©Æ²²Ã»»
               05  CONST-H28-SYOKUDO       PIC X(09) VALUE "197000570".
      *        Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
               05  CONST-H28-A108C04       PIC X(09) VALUE "190126470".
      *        °å»ÕÇÛÃÖ²Ã»»£±
               05  CONST-H28-A108C0501     PIC X(09) VALUE "190135570".
      *        °å»ÕÇÛÃÖ²Ã»»£²
               05  CONST-H28-A108C0502     PIC X(09) VALUE "190117070".
      *        ´Ç¸îÇÛÃÖ²Ã»»£±
               05  CONST-H28-A108C0601     PIC X(09) VALUE "190109670".
      *        ´Ç¸îÇÛÃÖ²Ã»»£²
               05  CONST-H28-A108C0602     PIC X(09) VALUE "190126570".
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
               05  CONST-H28-A108C0603     PIC X(09) VALUE "190126770".
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
               05  CONST-H28-A108C0604     PIC X(09) VALUE "190126670".
      *        ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±
               05  CONST-H28-A108C0605     PIC X(09) VALUE "190170570".
      *        ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²
               05  CONST-H28-A108C0606     PIC X(09) VALUE "190170670".
      *        ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
               05  CONST-H28-A108C10       PIC X(09) VALUE "190170770".
               05  CONST-H28-A109C10       PIC X(09) VALUE "190171770".
           03  CONST-H26-AREA.
               05  CONST-H26-ZTKFUKKI      PIC X(09) VALUE "190168070".
               05  CONST-H26-DRHJO1-VAL.
                   07                      PIC X(09) VALUE "190172210".
                   07                      PIC X(09) VALUE "190172310".
                   07                      PIC X(09) VALUE "190172410".
                   07                      PIC X(09) VALUE "190172510".
                   07                      PIC X(09) VALUE "190172610".
                   07                      PIC X(09) VALUE "190172710".
                   07                      PIC X(09) VALUE "190172810".
                   07                      PIC X(09) VALUE "190172910".
               05  CONST-H26-DRHJO1-R
                               REDEFINES CONST-H26-DRHJO1-VAL.
                   07  CONST-H26-DRHJO1  PIC X(09) OCCURS 8.
               05  CONST-H26-DRHJO2-VAL.
                   07                      PIC X(09) VALUE "190135710".
                   07                      PIC X(09) VALUE "190135810".
                   07                      PIC X(09) VALUE "190127110".
                   07                      PIC X(09) VALUE "190145510".
                   07                      PIC X(09) VALUE "190145610".
                   07                      PIC X(09) VALUE "190127210".
                   07                      PIC X(09) VALUE "190127310".
                   07                      PIC X(09) VALUE "190127410".
               05  CONST-H26-DRHJO2-R
                               REDEFINES CONST-H26-DRHJO2-VAL.
                   07  CONST-H26-DRHJO2  PIC X(09) OCCURS 8.
               05  CONST-H26-YKNKYUKNGHJ-VAL.
                   07                      PIC X(09) VALUE "190173010".
                   07                      PIC X(09) VALUE "190145970".
                   07                      PIC X(09) VALUE "190146070".
               05  CONST-H26-YKNKYUKNGHJ-R
                               REDEFINES CONST-H26-YKNKYUKNGHJ-VAL.
                   07  CONST-H26-YKNKYUKNGHJ  PIC X(09) OCCURS 3.
               05  CONST-H26-ADLIJI-A100   PIC X(09) VALUE "190166070".
               05  CONST-H26-ADLIJI-A104   PIC X(09) VALUE "190169070".
               05  CONST-H26-ADLIJI-A105   PIC X(09) VALUE "190169170".
               05  CONST-H26-SSNKYUDR      PIC X(09) VALUE "190182270".
               05  CONST-H26-SSNHKNFUKU-A103
                                           PIC X(09) VALUE "190168970".
               05  CONST-H26-SSNHKNFUKU-A312
                                           PIC X(09) VALUE "190177570".
               05  CONST-H26-SNRHJO1       PIC X(09) VALUE "190170570".
               05  CONST-H26-SNRHJO2       PIC X(09) VALUE "190170670".
               05  CONST-H26-EIYO-A108     PIC X(09) VALUE "190170770".
               05  CONST-H26-EIYO-A109     PIC X(09) VALUE "190171770".
               05  CONST-H26-TAISEI        PIC X(09) VALUE "190175270".
               05  CONST-H26-KNGSKHICH     PIC X(09) VALUE "190176970".
               05  CONST-H26-KNGHJSHICH    PIC X(09) VALUE "190177070".
           03  CONST-H24-AREA.
               05  CONST-H2404-DRHJO-VAL.
                   07                      PIC X(09) VALUE "190135710".
                   07                      PIC X(09) VALUE "190135810".
                   07                      PIC X(09) VALUE "190127110".
                   07                      PIC X(09) VALUE "190145510".
                   07                      PIC X(09) VALUE "190145610".
                   07                      PIC X(09) VALUE "190127210".
                   07                      PIC X(09) VALUE "190127310".
                   07                      PIC X(09) VALUE "190127410".
               05  CONST-H2404-DRHJO-R
                               REDEFINES CONST-H2404-DRHJO-VAL.
                   07  CONST-H2404-DRHJO   PIC X(09) OCCURS 8.
               05  CONST-H2404-KYUKNGHJ-VAL.
                   07                      PIC X(09) VALUE "190145710".
                   07                      PIC X(09) VALUE "190145810".
                   07                      PIC X(09) VALUE "190135910".
                   07                      PIC X(09) VALUE "190136010".
               05  CONST-H2404-KYUKNGHJ-R
                               REDEFINES CONST-H2404-KYUKNGHJ-VAL.
                   07  CONST-H2404-KYUKNGHJ PIC X(09) OCCURS 4.
               05  CONST-H2404-YKNKYUKNGHJ1 PIC X(09) VALUE "190145970".
               05  CONST-H2404-YKNKYUKNGHJ2 PIC X(09) VALUE "190146070".
               05  CONST-H2404-KNGYKNHICHI  PIC X(09) VALUE "190146170".
               05  CONST-H2404-KNGHITU1     PIC X(09) VALUE "190143070".
               05  CONST-H2404-KNGHITU1-SEN PIC X(09) VALUE "190144570".
               05  CONST-H2404-KNGHITU1-TOK PIC X(09) VALUE "190143970".
               05  CONST-H2404-KNGHITU2     PIC X(09) VALUE "190143170".
               05  CONST-H2404-KNGHITU2-SEN PIC X(09) VALUE "190144670".
               05  CONST-H2404-KNGHITU2-TOK PIC X(09) VALUE "190144070".
               05  CONST-H2404-BTUYKZ       PIC X(09) VALUE "190149110".
               05  CONST-H2404-TOKIPN1      PIC X(09) VALUE "190151910".
               05  CONST-H2404-TOKIPN2      PIC X(09) VALUE "190152010".
           03  CONST-SYOUBTUBRMCD-VAL.
               05                      PIC X(09) VALUE "         ".
               05                      PIC X(09) VALUE "188001070".
               05                      PIC X(09) VALUE "188001170".
               05                      PIC X(09) VALUE "188001270".
               05                      PIC X(09) VALUE "188001370".
               05                      PIC X(09) VALUE "188001470".
               05                      PIC X(09) VALUE "188001570".
               05                      PIC X(09) VALUE "188001670".
               05                      PIC X(09) VALUE "188001770".
               05                      PIC X(09) VALUE "188001870".
               05                      PIC X(09) VALUE "188001970".
               05                      PIC X(09) VALUE "188002070".
               05                      PIC X(09) VALUE "188002170".
               05                      PIC X(09) VALUE "188002270".
               05                      PIC X(09) VALUE "188002370".
               05                      PIC X(09) VALUE "188002470".
           03  CONST-SYOUBTUBRMCD-R    REDEFINES CONST-SYOUBTUBRMCD-VAL.
               05  CONST-SYOUBTUBRMCD  PIC X(09) OCCURS      16.
           03  CONST-YKNKNGCD-VAL.
               05                      PIC X(09) VALUE "190109870".
               05                      PIC X(09) VALUE "190076070".
               05                      PIC X(09) VALUE "190076170".
               05                      PIC X(09) VALUE "190076370".
               05                      PIC X(09) VALUE "190076470".
           03  CONST-YKNKNGCD-R        REDEFINES CONST-YKNKNGCD-VAL.
               05  CONST-YKNKNGCD-OCC  OCCURS 5.
                   07  CONST-YKNKNGCD  PIC X(09).
           03  CONST-SYOKUDOCD         PIC X(09) VALUE "197000570".
           03  CONST-HOIKUCD           PIC X(09) VALUE "190111370".
           03  CONST-NANBYO            PIC X(09) VALUE "190101770".
           03  CONST-TOKUSYUCD         PIC X(09) VALUE "190101970".
           03  CONST-JIDOCD            PIC X(09) VALUE "190110170".
           03  CONST-SEISINCD          PIC X(09) VALUE "190106970".
           03  CONST-KANGOCD-VAL.
               05                      PIC X(10) VALUE "2190102070".
               05                      PIC X(10) VALUE "3190102170".
               05                      PIC X(10) VALUE "4190102270".
               05                      PIC X(10) VALUE "5190102370".
               05                      PIC X(10) VALUE "6190102470".
           03  CONST-KANGOCD-R         REDEFINES   CONST-KANGOCD-VAL.
               05  CONST-KANGOCD-OCC   OCCURS 5.
                   07  CONST-KANGOCDL  PIC X(01).
                   07  CONST-KANGOCD   PIC X(09).
           03  CONST-KNGTEKCD          PIC X(09) VALUE "190109670".
           03  CONST-DRTEKCD           PIC X(09) VALUE "190117070".
           03  CONST-H2204-DRTEKCD1    PIC X(09) VALUE "190135570".
           03  CONST-KNGHJO-VAL.
               05                      PIC X(11) VALUE "01190103070".
               05                      PIC X(11) VALUE "01190103170".
               05                      PIC X(11) VALUE "01190103270".
               05                      PIC X(11) VALUE "01190103370".
               05                      PIC X(11) VALUE "02190103470".
               05                      PIC X(11) VALUE "02190103570".
               05                      PIC X(11) VALUE "02190103670".
               05                      PIC X(11) VALUE "03190103770".
               05                      PIC X(11) VALUE "03190103870".
               05                      PIC X(11) VALUE "04190103970".
               05                      PIC X(11) VALUE "05190104070".
               05                      PIC X(11) VALUE "06190103770".
               05                      PIC X(11) VALUE "07190103870".
               05                      PIC X(11) VALUE "08190103970".
               05                      PIC X(11) VALUE "09190104070".
           03  CONST-KNGHJOCD-R        REDEFINES   CONST-KNGHJO-VAL.
               05  CONST-KNGHJOCD-OCC  OCCURS 15.
                   07  CONST-KNGHJOCDL PIC X(02).
                   07  CONST-KNGHJOCD  PIC X(09).
           03  CONST-H2204-DRHJO-VAL.
               05                      PIC X(09) VALUE "190135710".
               05                      PIC X(09) VALUE "190135810".
               05                      PIC X(09) VALUE "190127110".
               05                      PIC X(09) VALUE "190127210".
               05                      PIC X(09) VALUE "190127310".
               05                      PIC X(09) VALUE "190127410".
           03  CONST-H2204-DRHJO-R     REDEFINES CONST-H2204-DRHJO-VAL.
               05  CONST-H2204-DRHJO   PIC X(09) OCCURS 6.
           03  CONST-DRHJO-VAL.
               05                      PIC X(09) VALUE "190127110".
               05                      PIC X(09) VALUE "190127210".
               05                      PIC X(09) VALUE "190127310".
               05                      PIC X(09) VALUE "190127410".
           03  CONST-DRHJO-R           REDEFINES CONST-DRHJO-VAL.
               05  CONST-DRHJO         PIC X(09) OCCURS 4.
           03  CONST-JKAIFUKU          PIC X(09) VALUE "190129870".
           03  CONST-YKNKNKYU          PIC X(09) VALUE "190126470".
           03  CONST-SNRHICH1          PIC X(09) VALUE "190126570".
           03  CONST-SNRHICH2          PIC X(09) VALUE "190109670".
           03  CONST-YKNHICH1          PIC X(09) VALUE "190126670".
           03  CONST-YKNHICH2          PIC X(09) VALUE "190126770".
           03  CONST-KYUKNGHJ1         PIC X(09) VALUE "190135910".
           03  CONST-KYUKNGHJ2         PIC X(09) VALUE "190136010".
           03  CONST-H2204-SNRHICH1    PIC X(09) VALUE "190109670".
           03  CONST-H2204-SNRHICH2    PIC X(09) VALUE "190126570".
           03  CONST-H2204-YKNHICH1    PIC X(09) VALUE "190126770".
           03  CONST-H2204-YKNHICH2    PIC X(09) VALUE "190126670".
           03  CONST-SEICHIIKI         PIC X(09) VALUE "190127810".
           03  CONST-HYOUKA            PIC X(09) VALUE "190131570".
           03  CONST-HYOUKA-TOK        PIC X(09) VALUE "190134870".
           03  CONST-HYOUKA-SEN        PIC X(09) VALUE "190134970".
           03  CONST-KJTRIHA           PIC X(09) VALUE "190140870".
           03  CONST-BTUMIKJNCD        PIC X(09) VALUE "188000270".
           03  CONST-KNGHJOCD-MAX      PIC 9(03) VALUE 15.
           03  CONST-5001-KSN-MAX      PIC 9(03) VALUE 14.
           03  CONST-YKTTYMD-MAX       PIC 9(03) VALUE 12.
           03  CONST-KANSATU           PIC X(02) VALUE "10".
           03  CONST-H180401           PIC X(08) VALUE "20060401".
           03  CONST-H180701           PIC X(08) VALUE "20060701".
           03  CONST-H181001           PIC X(08) VALUE "20061001".
           03  CONST-H200401           PIC X(08) VALUE "20080401".
           03  CONST-H200701           PIC X(08) VALUE "20080701".
           03  CONST-H220401           PIC X(08) VALUE "20100401".
           03  CONST-H240401           PIC X(08) VALUE "20120401".
           03  CONST-H241001           PIC X(08) VALUE "20121001".
           03  CONST-H260401           PIC X(08) VALUE "20140401".
           03  CONST-H261001           PIC X(08) VALUE "20141001".
           03  CONST-H280401           PIC X(08) VALUE "20160401".
           03  CONST-H300401           PIC X(08) VALUE "20180401".
           03  CONST-R020401           PIC X(08) VALUE "20200401".
      *
           COPY    "CPSK5001.INC"  REPLACING  //SYS-//
                                   BY         //TMP-//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY  "CPSYSKANRI.INC".
      *    ÉÂÅï´ÉÍý
           COPY  "CPSK5001.INC".
           COPY  "CPSK5008.INC".
      *    ÉÂ¼¼´ÉÍý
           COPY  "CPSK5002.INC".
      *    °åÎÅµ¡´Ø¾ðÊó
           COPY  "CPSK1001.INC".
      *    »ÜÀß´ð½à
           COPY  "CPSK1006.INC".
      *    ¿ÇÎÅ²Ê¥³¡¼¥É
           COPY  "CPSK1005.INC".
      *    ÉÂÅï¼ïÊÌ
           COPY  "CPSK5101.INC".
      *    Ê¿¶Ñºß±¡Æü¿ô
           COPY  "CPSK5102.INC".
      *    ´Ç¸îÇÛÃÖ
           COPY  "CPSK5103.INC".
      *    ´Ç¸î»ÕÈæÎ¨
           COPY  "CPSK5104.INC".
      *    ´Ç¸îÊä½õ²Ã»»
           COPY  "CPSK5105.INC".
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
           COPY  "CPSK5106.INC".
      *    ÆÃÄêÆþ±¡ÎÁ¡ÊÉÂÅï¡Ë
           COPY  "CPSK5110.INC".
      *    ÆÃÄêÆþ±¡ÎÁ¡ÊÉÂ¼¼¡Ë
           COPY  "CPSK5111.INC".
      *    ´Ç¸îÊä½õÇÛÃÖ
           COPY  "CPSK5115.INC".
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    Æþ±¡ÎÁ»»Äê¥Þ¥¹¥¿
      *01  NYUINKHN-REC.
      *    COPY  "CPNYUINKHN.INC".
      *    ÅÀ¿ô¥Þ¥¹¥¿
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *
      *    Æþ±¡´ðËÜÎÁ»»Äê¥µ¥Ö
           COPY  "CPW20SUB01.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ²èÌÌÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSGDAY.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    Æþ±¡ÎÁÃí²Ã»»¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY   "CPORCSNCHUKSN.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
           WHEN        "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    ²èÌÌÁ«°Ü
           WHEN     "PUTG"             ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    ÆþÎÏ
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    ²èÌÌÁ«°Ü¤¬¤Ê¤¤»þ¡¢²èÌÌÉ½¼¨¤Ø
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W20FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ¥¨¥é¡¼²èÌÌ¤è¤ê
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ½é´ü²èÌÌ¥»¥Ã¥È
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        ²èÌÌÊÔ½¸
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W20    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é´ü²èÌÌ½èÍý
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    Â¾¤Î£Ì£Ä¤è¤ê
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "W21"
               IF    ( SPA-W20-W21-MOTOPG  =   "W20" )
      *
                   INITIALIZE  SPA-W20-W21-AREA
      *
      *            ÉÂÅï¥ê¥¹¥È¤òºÆÊÔ½¸¤¹¤ë
                   MOVE    SPA-GMN-W20-BTULST-SEL
                                       TO  WRK-BTULST-SEL
                   PERFORM 3101-EDIT-BTULST-SEC 
                   MOVE    WRK-BTULST-SEL
                                       TO  SPA-GMN-W20-BTULST-SEL
      *
                   PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
                   GO  TO  300-SCREEN-EXT
               ELSE
                   PERFORM 310-SPASET-SEC
      *            ÉÂÅï¾ðÊóÀßÄê½èÍý
                   PERFORM 310-BTU-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           WHEN    "W201"
      *            ÉÂÅï¥ê¥¹¥È¤òºÆÊÔ½¸¤¹¤ë
                   MOVE    SPA-GMN-W20-BTULST-SEL
                                       TO  WRK-BTULST-SEL
                   PERFORM 3101-EDIT-BTULST-SEC 
                   MOVE    WRK-BTULST-SEL
                                       TO  SPA-GMN-W20-BTULST-SEL
      *
                   PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
                   GO  TO  300-SCREEN-EXT
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¹¥Ñ½é´üÊÔ½¸½èÍý
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-W20-AREA
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W20-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W20-KJNYMD
      *
           MOVE    ZERO            TO  SPA-GMN-W20-KSN-PAGENO
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¹¥Ñ½é´üÊÔ½¸½èÍý
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-GET-SYS1001-SEC
      *
      *    ¥³¥ó¥Ü½é´ü½èÍý
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    SPA-GMN-W20-EDKBNLST (1)
                                   TO  SPA-GMN-W20-EDKBN-G
      *
      *    ÉÂÅï´ÉÍý¾ðÊó¡ÊÉÂÅï¥ê¥¹¥È¤ËÉ½¼¨¡Ë
           PERFORM 3101-EDIT-BTULST-SEC
      *
      *    ÆþÎÏ¾ðÊóÎÎ°è½é´ü²½½èÍý
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÀßÄê½èÍý¡ÊÉÂ¼¼²èÌÌ¤è¤êÁ«°Ü»þ¡Ë
      *****************************************************************
       310-BTU-SET-SEC                 SECTION.
      *
           MOVE    56              TO  SPA-GMN-W20-CUR
           MOVE    0               TO  SPA-GMN-W20-KSN-PAGENO
      *
      *    ÆþÎÏ¹àÌÜÎÎ°èÊÔ½¸½èÍý
           MOVE    SPA-W20-W21-BTUNUM
                                   TO  WRK-BTUNUM 
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KIJUN-YMD
      *
      *    ²èÌÌ½é´ü²½
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           IF    ( FLG-SYS5001     =   ZERO )
      *
               PERFORM 4100-BTUNUM-LSTSEL-SEC
               MOVE    IDX-BTULST  TO  SPA-GMN-W20-BTULST-SEL
      *
      *        ÍúÎò¥Ç¡¼¥¿ÊÔ½¸½èÍý
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
      *
               MOVE    2           TO  SPA-GMN-W20-CUR
      *
           END-IF
      *
           .
       310-BTU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-COMBO-SYOKI-SEC          SECTION.
      *
           INITIALIZE  SPA-GMN-W20-CMB-DAT-AREA
                       SPA-NAI-W20-CMB-DAT-AREA
      *
      *    ÆÃÄêÆþ±¡ÎÁ¼èÆÀ½èÍý
           PERFORM 3101-GET-SYS5110-SEC
      *
      *    ÉÂÅï²ÊÌ¾¼èÆÀ½èÍý
           PERFORM 3101-GET-SYS1005-SEC
      *
      *    ÉÂÅï¼ïÊÌ¼èÆÀ½èÍý
           PERFORM 3101-GET-SYS5101-SEC
      *
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
           PERFORM 3101-RECEDSPKBN-GET-SEC
      *
      *    ÉÂÅïµ¡Ç½
           PERFORM 3101-BTUTYPE-GET-SEC
      *
      *    ½ªÎ»¶èÊ¬
           MOVE    2       TO  SPA-GMN-W20-EDKBN-MAX
           MOVE    "1"     TO  SPA-GMN-W20-EDKBNL    (1)
           MOVE    "·ÑÂ³"  TO  SPA-GMN-W20-EDKBNMEIL (1)
           MOVE    "2"     TO  SPA-GMN-W20-EDKBNL    (2)
           MOVE    "ÇÑ»ß"  TO  SPA-GMN-W20-EDKBNMEIL (2)
      *
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
      *    ²Ã»»¶¦ÄÌ¥³¥ó¥Ü¥Ü¥Ã¥¯¥¹½é´ü½èÍý
           PERFORM 3101-KSNCOMMON-SET-SEC
      *
           IF    ( WRK-KJN-YUKSTYMD    >=   CONST-H200401 )
            AND  ( SPA-GMN-W20-SBT      =   CONST-KANSATU )
                   PERFORM 31014-CMB-KSN-SYOKI-SEC
           ELSE
               EVALUATE    TRUE
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H300401 )
                   PERFORM 31018-H30R02-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H280401 )
                   PERFORM 31017-H28-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H260401 )
                   PERFORM 31016-CMB-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H240401 )
                   PERFORM 31015-CMB-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H200401 )
                   PERFORM 31013-CMB-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H180401 )
                   PERFORM 31012-CMB-KSN-SYOKI-SEC
               WHEN    OTHER
                   PERFORM 31011-CMB-KSN-SYOKI-SEC
               END-EVALUATE
           END-IF
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H14.4¡ÁH18.3)
      *****************************************************************
       31011-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
           PERFORM 3101-GET-SYS5106-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           PERFORM 3101-JIDO-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
           .
       31011-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H18.4¡ÁH20.3)
      *****************************************************************
       31012-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
           PERFORM 3101-GET-SYS5106-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           PERFORM 3101-JIDO-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           PERFORM 3101-HICHTEK-SET-SEC
      *
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           PERFORM 3101-DRTEK-SET-SEC
      *
           .
       31012-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H20.4¡Á)
      *****************************************************************
       31013-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           PERFORM 3101-JIDO-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-DRHJO-SET-SEC
      *
      *    ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
           PERFORM 3101-JKAIFUKU-SET-SEC
      *
      *    °å»ÕÇÛÃÖÅ¬¹ç²Ã»»
           PERFORM 3101-DRTEK-SET-SEC
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           PERFORM 3101-YKNKNKYU-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH1-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH2-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           PERFORM 3101-YKNHICH1-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           PERFORM 3101-YKNHICH2-SET-SEC
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-KYUKNGHJ-SET-SEC
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           PERFORM 3101-SEICHIIKI-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           PERFORM 3101-DRTEK2-SET-SEC
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-HYOUKA-SET-SEC
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           PERFORM 3101-KJTRIHA-SET-SEC
      *
           .
       31013-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(°åÎÅ´Ñ»¡Ë¡)
      *****************************************************************
       31014-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»
           PERFORM 3101-SYOUBTUBRMKSN-SET-SEC
      *
      *    ÉÂÅïÌ¤´ð½à¸º»»
           PERFORM 3101-BTUMIKJN-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
           .
       31014-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H24.4¡ÁH26.3)
      *****************************************************************
       31015-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-DRHJO-SET-SEC
      *
      *    °å»ÕÇÛÃÖÅ¬¹ç²Ã»»
           PERFORM 3101-DRTEK-SET-SEC
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           PERFORM 3101-YKNKNKYU-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH1-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH2-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           PERFORM 3101-YKNHICH1-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           PERFORM 3101-YKNHICH2-SET-SEC
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-KYUKNGHJ-SET-SEC
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-YKNKYUKNGHJ-SET-SEC
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           PERFORM 3101-KNGYKNHICHI-SET-SEC
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           PERFORM 3101-SEICHIIKI-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           PERFORM 3101-DRTEK2-SET-SEC
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-HYOUKA-SET-SEC
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           PERFORM 3101-KJTRIHA-SET-SEC
      *
      *    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-KNGHITU-SET-SEC
      *
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
           PERFORM 3101-BTUYKZ-SET-SEC
      *
           .
       31015-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H26.4¡ÁH28.3)
      *****************************************************************
       31016-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-DRHJO-SET-SEC
      *
      *    °å»ÕÇÛÃÖÅ¬¹ç²Ã»»
           PERFORM 3101-DRTEK-SET-SEC
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           PERFORM 3101-YKNKNKYU-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH1-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH2-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           PERFORM 3101-YKNHICH1-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           PERFORM 3101-YKNHICH2-SET-SEC
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           PERFORM 3101-ZTKFUKKI-SET-SEC
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-KYUKNGHJ-SET-SEC
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-YKNKYUKNGHJ-SET-SEC
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           PERFORM 3101-KNGYKNHICHI-SET-SEC
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           PERFORM 3101-SEICHIIKI-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           PERFORM 3101-DRTEK2-SET-SEC
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-HYOUKA-SET-SEC
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           PERFORM 3101-KJTRIHA-SET-SEC
      *
      *    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-KNGHITU-SET-SEC
      *
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
           PERFORM 3101-BTUYKZ-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²
           PERFORM 3101-DRHJO2-SET-SEC
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           PERFORM 3101-ADLIJI-SET-SEC
      *
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           PERFORM 3101-SSNKYUDR-SET-SEC
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           PERFORM 3101-SSNHKNFUKU-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±
           PERFORM 3101-SNRHJO1-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²
           PERFORM 3101-SNRHJO2-SET-SEC
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           PERFORM 3101-EIYO-SET-SEC
      *
      *    ÂÎÀ©¶¯²½²Ã»»
           PERFORM 3101-TAISEI-SET-SEC
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»
           PERFORM 3101-KNGSKHICH-SET-SEC
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»
           PERFORM 3101-KNGHJSHICH-SET-SEC
      *
           .
       31016-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H28.4¡ÁH30.3)
      *****************************************************************
       31017-H28-KSN-SYOKI-SEC         SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
      *
NEW   *    Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-YKNKNGHJ-SET-SEC
      *
NEW   *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¡Ë
           PERFORM 3101-YKNKNG-A214-SET-SEC
      *
UPDATE*    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-KNGHITU-SET-SEC
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-HYOUKA-SET-SEC
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           PERFORM 3101-ADLIJI-SET-SEC
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           PERFORM 3101-SSNHKNFUKU-SET-SEC
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           PERFORM 3101-ZTKFUKKI-SET-SEC
      *
UPDATE*    ÂÎÀ©¶¯²½²Ã»»
           PERFORM 3101-TAISEI-SET-SEC
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
           PERFORM 3101-KNGSKHICH-SET-SEC
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
           PERFORM 3101-KNGHJSHICH-SET-SEC
      *
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           PERFORM 3101-SSNKYUDR-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±
           PERFORM 3101-DRHJO-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²
           PERFORM 3101-DRHJO2-SET-SEC
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-KYUKNGHJ-SET-SEC
      *
UPDATE*    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-YKNKYUKNGHJ-SET-SEC
      *
NEW   *    Ìë´Ö´Ç¸îÊä½õÂÎÀ©²Ã»»¡ÊµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»¡Ë
           PERFORM 3101-YKNKNG-A207-03-SET-SEC
      *
UPDATE*    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           PERFORM 3101-KNGYKNHICHI-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           PERFORM 3101-SEICHIIKI-SET-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           PERFORM 3101-KJTRIHA-SET-SEC
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           PERFORM 3101-YKNKNKYU-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£±
           PERFORM 3101-DRTEK-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           PERFORM 3101-DRTEK2-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH1-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH2-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±
           PERFORM 3101-SNRHJO1-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²
           PERFORM 3101-SNRHJO2-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           PERFORM 3101-YKNHICH1-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           PERFORM 3101-YKNHICH2-SET-SEC
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           PERFORM 3101-EIYO-SET-SEC
      *
           .
       31017-H28-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²Ã»»¥³¥ó¥Ü½é´ü½èÍý(H30.4¡ÁR02.3)
      *****************************************************************
       31018-H30R02-KSN-SYOKI-SEC      SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-KNGHJO-SET-SEC
H30   *
NEW   *    Ìë´Ö´Ç¸î²Ã»»
           PERFORM 3101-H30YKNKNG-SET-SEC
H30   *
NEW   *    ´Ç¸îÊä½õ²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
           PERFORM 3101-KNGHJO-A104-SET-SEC
H30   *
NEW   *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
           PERFORM 3101-YKNKNG-A104-SET-SEC
      *
      *    Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»
           PERFORM 3101-YKNKNGHJ-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¡Ë
           PERFORM 3101-YKNKNG-A214-SET-SEC
      *
      *    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-KNGHITU-SET-SEC
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           PERFORM 3101-HYOUKA-SET-SEC
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           PERFORM 3101-ADLIJI-SET-SEC
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           PERFORM 3101-SSNHKNFUKU-SET-SEC
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           PERFORM 3101-ZTKFUKKI-SET-SEC
      *
      *    ÂÎÀ©¶¯²½²Ã»»
           PERFORM 3101-TAISEI-SET-SEC
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
           PERFORM 3101-KNGSKHICH-SET-SEC
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»¡ÊÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡Ë
           PERFORM 3101-KNGHJSHICH-SET-SEC
R02   *
UPDATE*    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           IF    ( WRK-KJN-YUKSTYMD    >=   CONST-R020401 )
               PERFORM 3101-R02SSNKYUDR-SET-SEC
           ELSE
               PERFORM 3101-SSNKYUDR-SET-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±
           PERFORM 3101-DRHJO-SET-SEC
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²
           PERFORM 3101-DRHJO2-SET-SEC
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-KYUKNGHJ-SET-SEC
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           PERFORM 3101-YKNKYUKNGHJ-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÊä½õÂÎÀ©²Ã»»¡ÊµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»¡Ë
           PERFORM 3101-YKNKNG-A207-03-SET-SEC
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           PERFORM 3101-KNGYKNHICHI-SET-SEC
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           PERFORM 3101-NANBYO-SET-SEC
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-TOKUSYU-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           PERFORM 3101-HICH-SET-SEC
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           PERFORM 3101-SEISIN-SET-SEC
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           PERFORM 3101-SEICHIIKI-SET-SEC
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           PERFORM 3101-HOIKU-SET-SEC
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           PERFORM 3101-KJTRIHA-SET-SEC
H30   *
NEW   *    £Á£³£°£¸¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-KYH-A308-03LST-G
H30   *
NEW   *    £Á£³£±£±´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-KYH-A311LST-G
H30   *
NEW   *    £Á£³£±£±¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-KYH-A311-03LST-G
H30   *
NEW   *    £Á£³£±£´Ç§ÃÎ¾ÉÌë´ÖÂÐ±þ²Ã»»
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-NYTLST-G
      *
      *    ¿©Æ²²Ã»»
           PERFORM 3101-SYOKUDO-SET-SEC
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           PERFORM 3101-YKNKNKYU-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£±
           PERFORM 3101-DRTEK-SET-SEC
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           PERFORM 3101-DRTEK2-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH1-SET-SEC
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           PERFORM 3101-SNRHICH2-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±
           PERFORM 3101-SNRHJO1-SET-SEC
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²
           PERFORM 3101-SNRHJO2-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           PERFORM 3101-YKNHICH1-SET-SEC
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           PERFORM 3101-YKNHICH2-SET-SEC
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           PERFORM 3101-EIYO-SET-SEC
      *
           .
       31018-H30R02-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-SYOUBTUBRMKSN-SET-SEC  SECTION.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   CONST-SYOUBTUBRM-MAX )
      *
               IF    ( IDX-CMB     =   1 )
      *
                   MOVE    "00"    TO  SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                   MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-SYOUBTUBRMMEIL
                                                               (IDX-CMB)
      *
               ELSE
                   COMPUTE WRK-NUM02
                       =   IDX-CMB
                   MOVE    WRK-NUM02X
                                   TO  SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                   COMPUTE WRK-NUM02
                       =   IDX-CMB +   13
                   MOVE    WRK-NUM02X
                                   TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-SEC
                   STRING  "¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê"   DELIMITED BY SIZE
                           WRK-KANACHK-OUT-INPUT      DELIMITED BY SPACE
                           "¾²¡Ë"                     DELIMITED BY SIZE
                   INTO    SPA-GMN-W20-SYOUBTUBRMMEIL (IDX-CMB)
                   END-STRING
               END-IF
      *
               COMPUTE SPA-GMN-W20-SYOUBTUBRM-MAX
                   =   SPA-GMN-W20-SYOUBTUBRM-MAX  +   1
      *
           END-PERFORM
      *
           .
       3101-SYOUBTUBRMKSN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÌ¤´ð½à¸º»»
      *****************************************************************
       3101-BTUMIKJN-SET-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-BTUMIKJNL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-BTUMIKJNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-BTUMIKJNL(2)
           MOVE    "»»Äê¤¹¤ë"          TO  SPA-GMN-W20-BTUMIKJNMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-BTUMIKJN-MAX
      *
           .
       3101-BTUMIKJN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õ²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-KNGHJO-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H240401 )
      *
               MOVE    "00"                TO  SPA-GMN-W20-KNGHJOL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-KNGHJOMEIL(1)
      *
               MOVE    "06"                TO  SPA-GMN-W20-KNGHJOL(2)
               MOVE    "´Ç¸îÊä½õ²Ã»»£±"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(2)
      *
               MOVE    "08"                TO  SPA-GMN-W20-KNGHJOL(3)
               MOVE    "´Ç¸îÊä½õ²Ã»»£²"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(3)
      *
               MOVE    "09"                TO  SPA-GMN-W20-KNGHJOL(4)
               MOVE    "´Ç¸îÊä½õ²Ã»»£³"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(4)
      *
               MOVE    4                   TO  SPA-GMN-W20-KNGHJO-MAX
      *
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H180401 )
      *
               MOVE    "00"                TO  SPA-GMN-W20-KNGHJOL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-KNGHJOMEIL(1)
      *
               MOVE    "06"                TO  SPA-GMN-W20-KNGHJOL(2)
               MOVE    "´Ç¸îÊä½õ²Ã»»£±"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(2)
      *
               MOVE    "07"                TO  SPA-GMN-W20-KNGHJOL(3)
               MOVE    "´Ç¸îÊä½õ²Ã»»£±¡Ê£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(3)
      *
               MOVE    "08"                TO  SPA-GMN-W20-KNGHJOL(4)
               MOVE    "´Ç¸îÊä½õ²Ã»»£²"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(4)
      *
               MOVE    "09"                TO  SPA-GMN-W20-KNGHJOL(5)
               MOVE    "´Ç¸îÊä½õ²Ã»»£³"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(5)
      *
               MOVE    5                   TO  SPA-GMN-W20-KNGHJO-MAX
           WHEN    OTHER
      *
               MOVE    "00"                TO  SPA-GMN-W20-KNGHJOL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-KNGHJOMEIL(1)
      *
               MOVE    "01"                TO  SPA-GMN-W20-KNGHJOL(2)
               MOVE    "£´ÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(2)
      *
               MOVE    "02"                TO  SPA-GMN-W20-KNGHJOL(3)
               MOVE    "£µÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(3)
      *
               MOVE    "03"                TO  SPA-GMN-W20-KNGHJOL(4)
               MOVE    "£¶ÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(4)
      *
               MOVE    "04"                TO  SPA-GMN-W20-KNGHJOL(5)
               MOVE    "£±£°ÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(5)
      *
               MOVE    "05"                TO  SPA-GMN-W20-KNGHJOL(6)
               MOVE    "£±£µÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(6)
      *
               MOVE    6               TO  SPA-GMN-W20-KNGHJO-MAX
           END-EVALUATE
      *
           .
       3101-KNGHJO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ²Ã»»¶¦ÄÌ¥³¥ó¥Ü¥Ü¥Ã¥¯¥¹½é´ü½èÍý
      *****************************************************************
       3101-KSNCOMMON-SET-SEC          SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-KSNCOMMONL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-KSNCOMMONMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-KSNCOMMONL(2)
           MOVE    "»»Äê¤¹¤ë"          TO  SPA-GMN-W20-KSNCOMMONMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-KSNCOMMON-MAX
      *
           .
       3101-KSNCOMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-HOIKU-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-HOIKULST-G
      *
           .
       3101-HOIKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-SYOKUDO-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SYOKUDOLST-G
      *
           .
       3101-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-NANBYO-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-NANBYOLST-G
      *
           .
       3101-NANBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-TOKUSYU-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-TOKUSYULST-G
      *
           .
       3101-TOKUSYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-JIDO-SET-SEC        SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-JIDOL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-JIDOMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-JIDOL(2)
           MOVE    "»»Äê¤¹¤ë"          TO  SPA-GMN-W20-JIDOMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-JIDO-MAX
      *
           .
       3101-JIDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-SEISIN-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SEISINLST-G
      *
           .
       3101-SEISIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-HICH-SET-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H180401 )
      *
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                           TO  SPA-GMN-W20-HICHLST-G
      *
           WHEN    OTHER
      *
               MOVE    "1"                 TO  SPA-GMN-W20-HICHL (1)
               MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-HICHMEIL (1)
      *
               MOVE    "2"                 TO  SPA-GMN-W20-HICHL (2)
               MOVE    "»»Äê¤¹¤ë¡ÊÆþ±¡´ðËÜÎÁ£³¡Ë"
                                           TO  SPA-GMN-W20-HICHMEIL (2)
      *
               MOVE    "3"                 TO  SPA-GMN-W20-HICHL (3)
               MOVE    "»»Äê¤¹¤ë¡ÊÆþ±¡´ðËÜÎÁ£´¡Ë"
                                           TO  SPA-GMN-W20-HICHMEIL (3)
      *
               MOVE    "4"                 TO  SPA-GMN-W20-HICHL (4)
               MOVE    "»»Äê¤¹¤ë¡ÊÆþ±¡´ðËÜÎÁ£µ¡Ë"
                                           TO  SPA-GMN-W20-HICHMEIL (4)
      *
               MOVE    "5"                 TO  SPA-GMN-W20-HICHL (5)
               MOVE    "»»Äê¤¹¤ë¡ÊÆþ±¡´ðËÜÎÁ£¶¡Ë"
                                           TO  SPA-GMN-W20-HICHMEIL (5)
      *
               MOVE    "6"                 TO  SPA-GMN-W20-HICHL (6)
               MOVE    "»»Äê¤¹¤ë¡ÊÆþ±¡´ðËÜÎÁ£·¡Ë"
                                           TO  SPA-GMN-W20-HICHMEIL (6)
      *
               MOVE    6                   TO  SPA-GMN-W20-HICH-MAX
           END-EVALUATE
      *
           .
       3101-HICH-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-HICHTEK-SET-SEC        SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-HICHTEKL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"        TO  SPA-GMN-W20-HICHTEKMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-HICHTEKL(2)
           MOVE    "»»Äê¤¹¤ë"          TO  SPA-GMN-W20-HICHTEKMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-HICHTEK-MAX
      *
           .
       3101-HICHTEK-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-DRTEK-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-DRTEKLST-G
      *
           .
       3101-DRTEK-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-DRTEK2-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-DRTEK2LST-G
      *
           .
       3101-DRTEK2-SET-EXT.
           EXIT.
      *****************************************************************
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-DRHJO-SET-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H240401 )
               IF    ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-R020401 )
                AND  ( SPA-NAI-W20-HOSPSBT  =    2 )
                   MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
                   MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJOMEIL(1)
                   MOVE    "7"             TO  SPA-GMN-W20-DRHJOL(2)
                   MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(2)
                   MOVE    "8"             TO  SPA-GMN-W20-DRHJOL(3)
                   MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(3)
                   MOVE    "9"             TO  SPA-GMN-W20-DRHJOL(4)
                   MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJOMEIL(4)
                   MOVE    4               TO  SPA-GMN-W20-DRHJO-MAX
               ELSE
                   MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
                   MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJOMEIL(1)
                   MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
                   MOVE    "£±£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(2)
                   MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
                   MOVE    "£²£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(3)
                   MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
                   MOVE    "£²£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(4)
                   MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
                   MOVE    "£³£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(5)
                   MOVE    "6"             TO  SPA-GMN-W20-DRHJOL(6)
                   MOVE    "£´£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(6)
                   MOVE    "7"             TO  SPA-GMN-W20-DRHJOL(7)
                   MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(7)
                   MOVE    "8"             TO  SPA-GMN-W20-DRHJOL(8)
                   MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(8)
                   MOVE    "9"             TO  SPA-GMN-W20-DRHJOL(9)
                   MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJOMEIL(9)
                   MOVE    9               TO  SPA-GMN-W20-DRHJO-MAX
               END-IF
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H220401 )
               MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
               MOVE    "£±£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
               MOVE    "£²£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
               MOVE    "£²£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
               MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(5)
               MOVE    "6"             TO  SPA-GMN-W20-DRHJOL(6)
               MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(6)
               MOVE    "7"             TO  SPA-GMN-W20-DRHJOL(7)
               MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJOMEIL(7)
               MOVE    7               TO  SPA-GMN-W20-DRHJO-MAX
           WHEN    OTHER
               MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
               MOVE    "£²£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
               MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
               MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJOMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
               MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJOMEIL(5)
               MOVE    5               TO  SPA-GMN-W20-DRHJO-MAX
           END-EVALUATE
      *
      *
           .
       3101-DRHJO-SET-EXT.
           EXIT.
      *****************************************************************
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-DRHJO2-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H260401 )
               IF    ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-R020401 )
                AND  ( SPA-NAI-W20-HOSPSBT  =    2 )
                   MOVE    "1"             TO  SPA-GMN-W20-DRHJO2L(1)
                   MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJO2MEIL(1)
                   MOVE    "7"             TO  SPA-GMN-W20-DRHJO2L(2)
                   MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(2)
                   MOVE    "8"             TO  SPA-GMN-W20-DRHJO2L(3)
                   MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(3)
                   MOVE    "9"             TO  SPA-GMN-W20-DRHJO2L(4)
                   MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJO2MEIL(4)
                   MOVE    4               TO  SPA-GMN-W20-DRHJO2-MAX
               ELSE
                   MOVE    "1"             TO  SPA-GMN-W20-DRHJO2L(1)
                   MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-DRHJO2MEIL(1)
                   MOVE    "2"             TO  SPA-GMN-W20-DRHJO2L(2)
                   MOVE    "£±£µÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(2)
                   MOVE    "3"             TO  SPA-GMN-W20-DRHJO2L(3)
                   MOVE    "£²£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(3)
                   MOVE    "4"             TO  SPA-GMN-W20-DRHJO2L(4)
                   MOVE    "£²£µÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(4)
                   MOVE    "5"             TO  SPA-GMN-W20-DRHJO2L(5)
                   MOVE    "£³£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(5)
                   MOVE    "6"             TO  SPA-GMN-W20-DRHJO2L(6)
                   MOVE    "£´£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(6)
                   MOVE    "7"             TO  SPA-GMN-W20-DRHJO2L(7)
                   MOVE    "£µ£°ÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(7)
                   MOVE    "8"             TO  SPA-GMN-W20-DRHJO2L(8)
                   MOVE    "£·£µÂÐ£±"      TO  SPA-GMN-W20-DRHJO2MEIL(8)
                   MOVE    "9"             TO  SPA-GMN-W20-DRHJO2L(9)
                   MOVE    "£±£°£°ÂÐ£±"    TO  SPA-GMN-W20-DRHJO2MEIL(9)
                   MOVE    9               TO  SPA-GMN-W20-DRHJO2-MAX
               END-IF
           END-EVALUATE
      *
           .
       3101-DRHJO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-HYOUKA-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-HYOUKALST-G
      *
           .
       3101-HYOUKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-KJTRIHA-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-KJTRIHALST-G
      *
           .
       3101-KJTRIHA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÉ¬Í×ÅÙ²Ã»»
      *****************************************************************
       3101-KNGHITU-SET-SEC     SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    "1"             TO  SPA-GMN-W20-KNGHITUL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-KNGHITUMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KNGHITUL(2)
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»£±"
                                   TO  SPA-GMN-W20-KNGHITUMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KNGHITUL(3)
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»£²"
                                       TO  SPA-GMN-W20-KNGHITUMEIL(3)
      *
               MOVE    "4"             TO  SPA-GMN-W20-KNGHITUL(4)
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»£³"
                                       TO  SPA-GMN-W20-KNGHITUMEIL(4)
      *
               MOVE    4               TO  SPA-GMN-W20-KNGHITU-MAX
           WHEN    OTHER
               MOVE    "1"             TO  SPA-GMN-W20-KNGHITUL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-KNGHITUMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KNGHITUL(2)
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»£±"
                                   TO  SPA-GMN-W20-KNGHITUMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KNGHITUL(3)
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»£²"
                                       TO  SPA-GMN-W20-KNGHITUMEIL(3)
      *
               MOVE    3               TO  SPA-GMN-W20-KNGHITU-MAX
           END-EVALUATE
      *
           .
       3101-KNGHITU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
      *****************************************************************
       3101-BTUYKZ-SET-SEC     SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-BTUYKZLST-G
      *
           .
       3101-BTUYKZ-SET-EXT.
           EXIT.
      *****************************************************************
      *    ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-JKAIFUKU-SET-SEC          SECTION.
      *
           MOVE    "1"             TO  SPA-GMN-W20-JKAIFUKUL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-JKAIFUKUMEIL(1)
           MOVE    "2"             TO  SPA-GMN-W20-JKAIFUKUL(2)
           MOVE    "»»Äê¤¹¤ë"      TO  SPA-GMN-W20-JKAIFUKUMEIL(2)
           MOVE    2               TO  SPA-GMN-W20-JKAIFUKU-MAX
      *
           .
       3101-JKAIFUKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¥³¥ó¥Ü½é´ü½èÍý
      *****************************************************************
       3101-YKNKNKYU-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-YKNKNKYULST-G
      *
           .
       3101-YKNKNKYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
      *****************************************************************
       3101-SNRHICH1-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-SNRHICH1LST-G
      *
           .
       3101-SNRHICH1-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
      *****************************************************************
       3101-SNRHICH2-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-SNRHICH2LST-G
      *
           .
       3101-SNRHICH2-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *****************************************************************
       3101-YKNHICH1-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-YKNHICH1LST-G
      *
           .
       3101-YKNHICH1-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *****************************************************************
       3101-YKNHICH2-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-YKNHICH2LST-G
      *
           .
       3101-YKNHICH2-SET-EXT.
           EXIT.
      *****************************************************************
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
      *****************************************************************
       3101-ZTKFUKKI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-ZTKFUKKILST-G
      *
           .
       3101-ZTKFUKKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *****************************************************************
       3101-KYUKNGHJ-SET-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H240401 )
               MOVE    "1"             TO  SPA-GMN-W20-KYUKNGHJL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-KYUKNGHJMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KYUKNGHJL(2)
               MOVE    "£²£µÂÐ£±¡Ê´Ç¸îÊä½õ£µ³ä°Ê¾å¡Ë"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KYUKNGHJL(3)
               MOVE    "£²£µÂÐ£±¡Ê´Ç¸îÊä½õ£µ³äÌ¤Ëþ¡Ë"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(3)
      *
               MOVE    "4"             TO  SPA-GMN-W20-KYUKNGHJL(4)
               MOVE    "£µ£°ÂÐ£±"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(4)
      *
               MOVE    "5"             TO  SPA-GMN-W20-KYUKNGHJL(5)
               MOVE    "£·£µÂÐ£±"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(5)
      *
               MOVE    5               TO  SPA-GMN-W20-KYUKNGHJ-MAX
           WHEN   OTHER
               MOVE    "1"             TO  SPA-GMN-W20-KYUKNGHJL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"    TO  SPA-GMN-W20-KYUKNGHJMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KYUKNGHJL(2)
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»£±" 
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KYUKNGHJL(3)
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»£²" 
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(3)
      *
               MOVE    3               TO  SPA-GMN-W20-KYUKNGHJ-MAX
           END-EVALUATE
      *
           .
       3101-KYUKNGHJ-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *****************************************************************
       3101-YKNKYUKNGHJ-SET-SEC    SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    "1"         TO  SPA-GMN-W20-YKNKYUKNGHJL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJL(2)
               MOVE    "£³£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJL(3)
               MOVE    "£µ£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(3)
      *
               MOVE    "4"         TO  SPA-GMN-W20-YKNKYUKNGHJL(4)
               MOVE    "£±£°£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(4)
      *
               MOVE    4           TO  SPA-GMN-W20-YKNKYUKNGHJ-MAX
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H260401 )
               MOVE    "1"         TO  SPA-GMN-W20-YKNKYUKNGHJL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJL(2)
               MOVE    "£²£µÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJL(3)
               MOVE    "£µ£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(3)
      *
               MOVE    "4"         TO  SPA-GMN-W20-YKNKYUKNGHJL(4)
               MOVE    "£±£°£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(4)
      *
               MOVE    4           TO  SPA-GMN-W20-YKNKYUKNGHJ-MAX
           WHEN    OTHER
               MOVE    "1"         TO  SPA-GMN-W20-YKNKYUKNGHJL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJL(2)
               MOVE    "£µ£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJL(3)
               MOVE    "£±£°£°ÂÐ£±"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(3)
      *
               MOVE    3           TO  SPA-GMN-W20-YKNKYUKNGHJ-MAX
           END-EVALUATE
      *
           .
       3101-YKNKYUKNGHJ-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *****************************************************************
       3101-YKNKNG-A207-03-SET-SEC      SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                               TO  SPA-GMN-W20-YKNKNG-A207-03LST-G
           END-EVALUATE
      *
           .
       3101-YKNKNG-A207-03-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *****************************************************************
       3101-YKNKNGHJ-SET-SEC      SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                               TO  SPA-GMN-W20-YKNKNGHJLST-G
           END-EVALUATE
      *
           .
       3101-YKNKNGHJ-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¡Ë
      *****************************************************************
       3101-YKNKNG-A214-SET-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-YKNKNG-A214LST-G
           END-EVALUATE
      *
           .
       3101-YKNKNG-A214-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
      *****************************************************************
       3101-KNGYKNHICHI-SET-SEC    SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H300401 )
               MOVE    "1"         TO  SPA-GMN-W20-KNGYKNHICHIL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-KNGYKNHICHIL(2)
               MOVE    "£±£²ÂÐ£±ÇÛÃÖ²Ã»»£±"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-KNGYKNHICHIL(3)
               MOVE    "£±£²ÂÐ£±ÇÛÃÖ²Ã»»£²"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(3)
      *
               MOVE    "4"         TO  SPA-GMN-W20-KNGYKNHICHIL(4)
               MOVE    " £±£¶ÂÐ£±ÇÛÃÖ²Ã»»£±"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(4)
      *
               MOVE    "5"         TO  SPA-GMN-W20-KNGYKNHICHIL(5)
               MOVE    " £±£¶ÂÐ£±ÇÛÃÖ²Ã»»£²"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(5)
      *
               MOVE    5           TO  SPA-GMN-W20-KNGYKNHICHI-MAX
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    "1"         TO  SPA-GMN-W20-KNGYKNHICHIL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-KNGYKNHICHIL(2)
               MOVE    "£±£²ÂÐ£±ÇÛÃÖ²Ã»»£±"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-KNGYKNHICHIL(3)
               MOVE    "£±£²ÂÐ£±ÇÛÃÖ²Ã»»£²"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(3)
      *
               MOVE    "4"         TO  SPA-GMN-W20-KNGYKNHICHIL(4)
               MOVE    "£±£¶ÂÐ£±ÇÛÃÖ²Ã»»"
                                   TO  SPA-GMN-W20-KNGYKNHICHIMEIL(4)
      *
               MOVE    4           TO  SPA-GMN-W20-KNGYKNHICHI-MAX
           WHEN  OTHER
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-KNGYKNHICHILST-G
           END-EVALUATE
      *
           .
       3101-KNGYKNHICHI-SET-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
      *****************************************************************
       3101-SEICHIIKI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SEICHIIKILST-G
      *
           .
       3101-SEICHIIKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
      *****************************************************************
       3101-ADLIJI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-ADLIJILST-G
      *
           .
       3101-ADLIJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»(R02.4¡Á)
      *****************************************************************
       3101-R02SSNKYUDR-SET-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-W20-SSNKYUDRLST-G
      *
           MOVE    "1"                 TO  SPA-GMN-W20-SSNKYUDRL(1)
           MOVE    "»»Äê¤·¤Ê¤¤"
                                       TO  SPA-GMN-W20-SSNKYUDRMEIL(1)
      *
           MOVE    "2"                 TO  SPA-GMN-W20-SSNKYUDRL(2)
           MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-SSNKYUDRMEIL(2)
      *
           MOVE    "3"                 TO  SPA-GMN-W20-SSNKYUDRL(3)
           MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-SSNKYUDRMEIL(3)
      *
           MOVE    "4"                 TO  SPA-GMN-W20-SSNKYUDRL(4)
           MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»£³"
                                       TO  SPA-GMN-W20-SSNKYUDRMEIL(4)
      *
           MOVE    4                   TO  SPA-GMN-W20-SSNKYUDR-MAX
      *
           .
       3101-R02SSNKYUDR-SET-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
      *****************************************************************
       3101-SSNKYUDR-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SSNKYUDRLST-G
      *
           .
       3101-SSNKYUDR-SET-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
      *****************************************************************
       3101-SSNHKNFUKU-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SSNHKNFUKULST-G
      *
           .
       3101-SSNHKNFUKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±
      *****************************************************************
       3101-SNRHJO1-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SNRHJO1LST-G
      *
           .
       3101-SNRHJO1-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²
      *****************************************************************
       3101-SNRHJO2-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SNRHJO2LST-G
      *
           .
       3101-SNRHJO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
      *****************************************************************
       3101-EIYO-SET-SEC           SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-EIYOLST-G
      *
           .
       3101-EIYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÂÎÀ©¶¯²½²Ã»»
      *****************************************************************
       3101-TAISEI-SET-SEC        SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H280401 )
               MOVE    "1"         TO  SPA-GMN-W20-TAISEIL(1)
               MOVE    "»»Äê¤·¤Ê¤¤"
                                   TO  SPA-GMN-W20-TAISEIMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-TAISEIL(2)
               MOVE    "ÂÎÀ©¶¯²½²Ã»»£±"
                                   TO  SPA-GMN-W20-TAISEIMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-TAISEIL(3)
               MOVE    "ÂÎÀ©¶¯²½²Ã»»£²"
                                   TO  SPA-GMN-W20-TAISEIMEIL(3)
      *
               MOVE    3           TO  SPA-GMN-W20-TAISEI-MAX
           WHEN    OTHER
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-TAISEILST-G
           END-EVALUATE
      *
           .
       3101-TAISEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»
      *****************************************************************
       3101-KNGSKHICH-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-KNGSKHICHLST-G
      *
           .
       3101-KNGSKHICH-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»
      *****************************************************************
       3101-KNGHJSHICH-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-KNGHJSHICHLST-G
      *
           .
       3101-KNGHJSHICH-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸î²Ã»»
      *****************************************************************
       3101-H30YKNKNG-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-H30YKNKNGLST-G
      *
           .
       3101-H30YKNKNG-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õ²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
      *****************************************************************
       3101-KNGHJO-A104-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-KNGHJO-A104LST-G
      *
           .
       3101-KNGHJO-A104-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
      *****************************************************************
       3101-YKNKNG-A104-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-YKNKNG-A104LST-G
      *
           .
       3101-YKNKNG-A104-SET-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       3101-GET-SYS1001-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           MOVE    SYS-1001-HOSPSBT
                                   TO  SPA-NAI-W20-HOSPSBT
           MOVE    SYS-1001-BEDSU
                                   TO  SPA-NAI-W20-BEDSU
      *
           .
       3101-GET-SYS1001-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¼èÆÀ½èÍý
      *****************************************************************
       3101-GET-SYS5110-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5110"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-TOKNYUIN-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5110-REC
      *
               IF    ( SYS-5110-SEIKATU-RYOKBN NOT =   SPACE )
                OR   ( SYS-5110-KBNCD  =   "24" OR "25"
                                        OR "35" OR "36" OR "37" 
                                        OR "38" OR "39" OR "40" 
                                        OR "49" OR "50" OR "51"
                                        OR "52" )
                   CONTINUE
               ELSE
      *
                   COMPUTE IDX-CMB =   IDX-CMB +   1
                   MOVE    IDX-CMB     TO  SPA-GMN-W20-TOKNYUIN-MAX
      *
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                               TO  SPA-GMN-W20-TOKNYUINMEIL
                                                         (IDX-CMB)
      *
                   MOVE    SYS-5110-KBNCD
                               TO  SPA-GMN-W20-TOKNYUINL-KBNCD
                                                         (IDX-CMB)
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM2
                               TO  SPA-GMN-W20-TOKNYUINL-TANMEI
                                                         (IDX-CMB)
                   MOVE    SYS-5110-SRYCD
                               TO  SPA-GMN-W20-TOKNYUINL-SRYCD
                                                         (IDX-CMB)
                   MOVE    SYS-5110-SRYCD  TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-KOUHYOJYUNNUM
                                       TO  SPA-GMN-W20-TOKNYUINL-JUNNUM
                                                         (IDX-CMB)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE IDXS    =   CONST-TOKNYUIN-MAX  +   1
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-W20-TOKNYUIN-MAX )
      *
               COMPUTE WRK-FROM    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    WRK-FROM   BY  1
                       UNTIL ( IDXB    >   SPA-GMN-W20-TOKNYUIN-MAX )
                   IF    ( SPA-GMN-W20-TOKNYUINL-JUNNUM (IDXA)
                               >   SPA-GMN-W20-TOKNYUINL-JUNNUM (IDXB))
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXA)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXS)
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXB)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXA)
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXS)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXB)
                   END-IF
               END-PERFORM
      *
               COMPUTE WRK-NUM02   =   IDXA
               MOVE    WRK-NUM02X  TO  SPA-GMN-W20-TOKNYUINL (IDXA)
      *
           END-PERFORM
      *
           .
       3101-GET-SYS5110-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²Ê¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       3101-GET-SYS1005-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-SRYKA-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-SRYKA-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD
                           TO  SPA-GMN-W20-SRYKAL   (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME
                           TO  SPA-GMN-W20-SRYKAMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS1005-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¼ïÊÌ¼èÆÀ½èÍý
      *****************************************************************
       3101-GET-SYS5101-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5101"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-SBT-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-SBT-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5101-REC
      *
               MOVE    SYS-5101-KBNCD
                           TO  SPA-GMN-W20-SBTL   (IDX-CMB)
               MOVE    SYS-5101-BTU-SBT-NM
                           TO  SPA-GMN-W20-SBTMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS5101-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»¼èÆÀ½èÍý
      *****************************************************************
       3101-GET-SYS5106-SEC        SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5106"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
      *
           COMPUTE IDX-CMB =   IDX-CMB +   1
           MOVE    "0"             TO  SPA-GMN-W20-YKNKNGL   (IDX-CMB)
           MOVE    "»»Äê¤·¤Ê¤¤"    TO SPA-GMN-W20-YKNKNGMEIL (IDX-CMB)
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-YKNKNG-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-YKNKNG-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5106-REC
      *
               MOVE    SYS-5106-KBNCD(2:1)
                           TO  SPA-GMN-W20-YKNKNGL   (IDX-CMB)
               MOVE    SYS-5106-BTU-YAKANKNGKSN-NM
                           TO  SPA-GMN-W20-YKNKNGMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS5106-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï´ÉÍý¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       3101-EDIT-BTULST-SEC        SECTION.
      *
      *    ¥·¥¹¥Æ¥àÆüÉÕ¤ÎÆâÍÆ¤Ç¥³¥ó¥Ü¤òºÆÊÔ½¸
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           INITIALIZE                  SPA-GMN-W20-BTULST
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W20-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY7-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-LST        >=   CONST-BTULST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
               MOVE    IDX-LST TO  SPA-GMN-W20-BTULST-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *        ÉÂÅï¥ê¥¹¥ÈÊÔ½¸
               PERFORM 31011-EDIT-BTULST-SEC
      *
               PERFORM 900-SYSKANRI-KEY7-FET-SEC
      *
           END-PERFORM
      *
      **   ¼ýÍÆ¿Í¿ôÀßÄê½èÍý
           PERFORM 31011-GET-BTU-NINZU-SEC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1               TO  FLG-BTULST-ROW
      *
           .
       3101-EDIT-BTULST-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¼èÆÀ½èÍý
      *****************************************************************
       3101-RECEDSPKBN-GET-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-W20-RECEDSPKBNLST-G
                       SPA-NAI-W20-RECEDSPKBNLST-G
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H181001 )
               MOVE    "01"    TO  SPA-GMN-W20-RECEDSPKBNL (1)
               MOVE    "02"    TO  SPA-GMN-W20-RECEDSPKBNL (2)
               MOVE    "07"    TO  SPA-GMN-W20-RECEDSPKBNL (3)
      *
               MOVE    "Àº¿À"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(1)
               MOVE    "·ë³Ë"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(2)
               MOVE    "ÎÅÍÜ"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(3)
      *
               MOVE    "02"    TO  SPA-NAI-W20-RECEDSPKBNLST (1)
               MOVE    "03"    TO  SPA-NAI-W20-RECEDSPKBNLST (2)
               MOVE    "04"    TO  SPA-NAI-W20-RECEDSPKBNLST (3)
      *
               MOVE    3       TO  SPA-GMN-W20-RECEDSPKBN-MAX
           ELSE
      *
               MOVE    "01"    TO  SPA-GMN-W20-RECEDSPKBNL (1)
               MOVE    "02"    TO  SPA-GMN-W20-RECEDSPKBNL (2)
               MOVE    "03"    TO  SPA-GMN-W20-RECEDSPKBNL (3)
               MOVE    "05"    TO  SPA-GMN-W20-RECEDSPKBNL (4)
               MOVE    "07"    TO  SPA-GMN-W20-RECEDSPKBNL (5)
               MOVE    "08"    TO  SPA-GMN-W20-RECEDSPKBNL (6)
               MOVE    "09"    TO  SPA-GMN-W20-RECEDSPKBNL (7)
      *
               MOVE    "Àº¿À"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(1)
               MOVE    "·ë³Ë"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(2)
               MOVE    "ÆÃÎã"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(3)
               MOVE    "Ï·¿Í"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(4)
               MOVE    "ÎÅÍÜ"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(5)
               MOVE    "Ê£¹ç"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(6)
               MOVE    "Ê£ÎÅ"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(7)
      *
               MOVE    "02"    TO  SPA-NAI-W20-RECEDSPKBNLST (1)
               MOVE    "03"    TO  SPA-NAI-W20-RECEDSPKBNLST (2)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (3)
               MOVE    "07"    TO  SPA-NAI-W20-RECEDSPKBNLST (4)
               MOVE    "04"    TO  SPA-NAI-W20-RECEDSPKBNLST (5)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (6)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (7)
      *
               MOVE    7           TO  SPA-GMN-W20-RECEDSPKBN-MAX
           END-IF
      *
           .
       3101-RECEDSPKBN-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïµ¡Ç½¾ðÊó¼èÆÀ½èÍý
      *****************************************************************
       3101-BTUTYPE-GET-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-W20-BTUTYPELST-G
      *
           MOVE    "1"             TO  SPA-GMN-W20-BTUTYPEL (1)
           MOVE    "¹âÅÙµÞÀ­´ü"    TO  SPA-GMN-W20-BTUTYPEMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W20-BTUTYPEL (2)
           MOVE    "µÞÀ­´ü"        TO  SPA-GMN-W20-BTUTYPEMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-W20-BTUTYPEL (3)
           MOVE    "²óÉü´ü"        TO  SPA-GMN-W20-BTUTYPEMEIL (3)
      *
           MOVE    "4"             TO  SPA-GMN-W20-BTUTYPEL (4)
           MOVE    "ËýÀ­´ü"        TO  SPA-GMN-W20-BTUTYPEMEIL (4)
      *
           MOVE    4               TO  SPA-GMN-W20-BTUTYPE-MAX
      *
           .
       3101-BTUTYPE-GET-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥ê¥¹¥ÈÊÔ½¸½èÍý
      *****************************************************************
       31011-EDIT-BTULST-SEC           SECTION.
      *
      *    ÉÂÅïÈÖ¹æ
           MOVE    SYS-5001-BTU-NUM
                           TO  SPA-GMN-W20-TBTUNUM (IDX-LST)
      *    ÉÂÅïÌ¾¾Î
           MOVE    SYS-5001-BTU-NAME
                           TO  SPA-GMN-W20-TNAME (IDX-LST)
      *    Ã»½ÌÉÂÅïÌ¾¾Î
           MOVE    SYS-5001-BTU-TANNAME
                           TO  SPA-GMN-W20-TTANNAME (IDX-LST)
      *
      **   ÉÂÅï¼ïÊÌ
           MOVE    SYS-5001-BTU-SBT    TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           MOVE    WRK-CMB-ITM (4:)
                           TO  SPA-GMN-W20-TSBT (IDX-LST)
      *
      *    ÆÃÄêÆþ±¡ÎÁ
           MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           MOVE    WRK-CMB-ITM2
                           TO  SPA-GMN-W20-TTOKNYUIN (IDX-LST)
      *
      *    ÉÂÅï²ÊÌ¾
           MOVE    SYS-5001-BTU-KA     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM (4:)
                           TO  SPA-GMN-W20-TSRYKA (IDX-LST)
      *
           MOVE    SYS-5001-BTU-KHNSRYCD
                           TO  SPA-GMN-W20-TKHNSRYCD (IDX-LST)
      *
      *    Æþ±¡´ðËÜÅÀ¿ô¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ¤¹¤ë
           INITIALIZE                      W20SUB01-LNK
           MOVE    "1"                 TO  W20SUB01-I-KBN
           IF    ( SYS-5001-KHNSRYCD   NOT =   SPACE )
               MOVE    SYS-5001-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           ELSE
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           END-IF
           MOVE    SYS-5001-STYUKYMD   TO  W20SUB01-I-KJNYMD
      *
           CALL    "ORCGW20SUB01"      USING
                                       W20SUB01-LNK
                                       SPA-AREA
           IF    ( W20SUB01-O-RC   =   ZERO )
               MOVE    W20SUB01-O-NAME
                                   TO  SPA-GMN-W20-TKHNRYO (IDX-LST)
               MOVE    W20SUB01-O-KHNTEN
                                   TO  SPA-GMN-W20-TKHNTEN (IDX-LST)
           END-IF
      *
      *    Í­¸ú³«»ÏÆü
           MOVE    SYS-5001-STYUKYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W20-TYUKOSTYMD
                                                           (IDX-LST)
      *
      *    Í­¸ú½ªÎ»Æü
           MOVE    SYS-5001-EDYUKYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W20-TYUKOEDYMD
                                                           (IDX-LST)
      *
           .
       31011-EDIT-BTULST-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¿Í¿ô¼èÆÀ½èÍý
      *****************************************************************
       31011-GET-BTU-NINZU-SEC     SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST  >  SPA-GMN-W20-BTULST-MAX )
      *
               INITIALIZE                  WRK-BTU-NINZU
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5002"          TO  SYS-KANRICD
               MOVE    SPA-GMN-W20-TBTUNUM (IDX-LST)
                                       TO  SYS-KBNCD
               MOVE    "%"             TO  SYS-KBNCD(3:1)
               MOVE    SPA-NAI-W20-KJNYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
                   MOVE    SYSKANRI-REC
                                       TO  SYS-5002-REC
      *
                   COMPUTE WRK-BTU-NINZU
                       =   WRK-BTU-NINZU   +   SYS-5002-BRM-NINZU
      *
                   PERFORM 900-SYSKANRI-KEY-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               MOVE    WRK-BTU-NINZU
                                   TO  SPA-GMN-W20-TNINZU (IDX-LST)
      *
            END-PERFORM
      *
           .
       31011-GET-BTU-NINZU-EXT.
           EXIT.
      *****************************************************************
      *    ³ÎÇ§²èÌÌ¡Ê£Ã£É£Ä£±¡Ë£Ï£Ë½èÍý
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "2002"
                       PERFORM 4201-EDYUKYMD-UPDATE-SEC
                       IF    ( SPA-ERRCD   =   SPACE )
                           PERFORM 4201-TUIKA-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌÁ«°Ü½èÍý
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    Ìá¤ë
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    ¥¯¥ê¥¢
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
               MOVE    0               TO  SPA-GMN-W20-KSN-PAGENO
      *    ´ð½àÆü
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    101         TO      SPA-GMN-W20-CUR
      *    ¥¿¥ÖÀÚÂØ
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 440-TAB-SWITCH-SEC
      *    ÄÉ²Ã
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    ÊÑ¹¹
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    ºï½ü
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    ÉÂ¼¼´ÉÍý¾ðÊó²èÌÌ¤Ø
           WHEN    "CLICKED"       ALSO    "B09"
               IF  ( SPA-W20-W21-MOTOPG    =   SPACE )
                   PERFORM 420-BYOSITU-KANRI-SEC
               END-IF
      *    Æþ±¡ÎÁ²Ã»»ÀßÄê²èÌÌ¤Ø
           WHEN    "CLICKED"       ALSO    "B10"
               IF    ( SPA-NAI-W20-YUKOSTYMD
                                   >=  CONST-H240401 )
                   IF (SPA-GMN-W20-BTUNUM    NOT =   ZERO)   AND
                      (SPA-GMN-W20-SELNUM    NOT =   ZERO)
                       IF   SPA-GMN-W20-SBT  NOT =   "10"
                           PERFORM 420-W201-SYORI-SEC
                       ELSE
                           MOVE   "1109"   TO      SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE   "1108"   TO      SPA-ERRCD
                   END-IF
               END-IF
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ²ñÏÃ¡¡½èÍý
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    ÉÂÅïÈÖ¹æ
           WHEN    "ACTIVATE"      ALSO    "BTUSELNUM"
                   PERFORM 4100-BTUSELNUM-CHK-SEC
      *
      *    ÉÂÅï¼ïÊÌ
           WHEN    "ACTIVATE"      ALSO    "SBT"
                   PERFORM 4100-SBT-CHK-SEC
      *    ´ð½àÆü
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
                   PERFORM 4100-KJNYMD-CHK-SEC
      *    ÁªÂòÈÖ¹æ
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4100-SELNUM-CHK-SEC
      *    ÉÂÅï¥ê¥¹¥È
           WHEN    ANY             ALSO    "BTULST"
               PERFORM 4100-BTULST-CHK-SEC
      *    ÍúÎò¥ê¥¹¥È
           WHEN    ANY             ALSO    "RIREKILST"
                   PERFORM 4100-RIREKILST-CHK-SEC
      *    Æþ±¡´ðËÜÎÁ
           WHEN    "ACTIVATE"      ALSO    "KHNRYO"
      *
      *        Æþ±¡´ðËÜÎÁ¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4100-KHNRYO-CHK-SEC
           WHEN    "SWITCH"        ALSO   "nb1"
               PERFORM 440-TAB-CHANGE-SEC
      *
           WHEN    OTHER
      *
      *        ÆþÎÏ¸Ä½ê¤Î¥»¥Ã¥È
               PERFORM 400-GMN-INPUT-SEC
      *        ÆþÎÏ¥Á¥§¥Ã¥¯½èÍý
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¿¥ÖÀÚÂØ½èÍý¡Ê¥Õ¥¡¥ó¥¯¥·¥ç¥ó¥­¡¼¡Ë
      *****************************************************************
       440-TAB-SWITCH-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-W20-KSN-PAGENO
           WHEN    0
               MOVE    1           TO  W20-KSN-PAGENO
           WHEN    1
               MOVE    2           TO  W20-KSN-PAGENO
           WHEN    2
               MOVE    3           TO  W20-KSN-PAGENO
           WHEN    3
               MOVE    0           TO  W20-KSN-PAGENO
               MOVE    9           TO  SPA-GMN-W20-CUR
           END-EVALUATE
      *
           IF   (  W20-KSN-PAGENO  =   1 )
               IF    ( W20-LBLKSNP02-STATE =   WIDGET-INSENSITIVE )
                   MOVE    2       TO  W20-KSN-PAGENO
               ELSE
                   MOVE    315     TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF   (  W20-KSN-PAGENO  =   2 )
               IF    ( W20-LBLKSNP03-STATE =   WIDGET-INSENSITIVE )
                   MOVE    3       TO  W20-KSN-PAGENO
               ELSE
                   MOVE    331     TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF   (  W20-KSN-PAGENO  =   3 )
               IF    ( W20-LBLYKTTYMDP-STATE =   WIDGET-INSENSITIVE )
                   MOVE    0       TO  W20-KSN-PAGENO
                   MOVE    9       TO  SPA-GMN-W20-CUR
               ELSE
                   MOVE    901     TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
      *
           PERFORM 440-TAB-CHANGE-SEC
      *
           .
      *
       440-TAB-SWITCH-EXT.
           EXIT.
      *****************************************************************
      *    ¥¿¥ÖÀÚÂØ½èÍý
      *****************************************************************
       440-TAB-CHANGE-SEC              SECTION.
      *
      *    ²èÌÌ£Ó£Ð£ÁÊÔ½¸½èÍý
           PERFORM 4101-GMN-SPA-SET-SEC
           PERFORM 4101-KASAN-CHK-SEC
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-YKTTYMD-CHK-SEC
           END-IF
           IF    ( SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-GMN-W20-CUR
               WHEN    9
               WHEN    301 THRU    314
                   MOVE    0   TO  W20-KSN-PAGENO
               WHEN    315 THRU    330
                   MOVE    1   TO  W20-KSN-PAGENO
               WHEN    331 THRU    346
                   MOVE    2   TO  W20-KSN-PAGENO
               WHEN    901 THRU    912
                   MOVE    3   TO  W20-KSN-PAGENO
               END-EVALUATE
           END-IF
      *
           MOVE    W20-KSN-PAGENO  TO  SPA-GMN-W20-KSN-PAGENO
      *
           .
      *
       440-TAB-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆþÎÏ¸Ä½ê¤Î¥»¥Ã¥È½èÍý
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-W20-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆþÎÏ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ²èÌÌ¤ò£Ó£Ð£Á¥»¥Ã¥È
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ´ðËÜ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌ£Ó£Ð£ÁÊÔ½¸½èÍý
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    ÉÂÅïÈÖ¹æ
           MOVE    W20-BTUNUM      TO  SPA-GMN-W20-BTUNUM
      *
      *    ÉÂÅïÌ¾¾Î
           MOVE    W20-NAME        TO  SPA-GMN-W20-NAME
      *
      *    Ã»½ÌÉÂÅïÌ¾
           MOVE    W20-TANNAME     TO  SPA-GMN-W20-TANNAME
      *
      *    ÆÃÄêÆþ±¡ÎÁ
           MOVE    W20-TOKUTEINYUIN
                                   TO  SPA-GMN-W20-TOKNYUIN-G
      *
      *    ÉÂÅï²ÊÌ¾
           MOVE    W20-SRYKA       TO  SPA-GMN-W20-SRYKA-G
      *
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬
           MOVE    W20-YKNCHOKAKBN TO  SPA-GMN-W20-YKNCHOKAKBN-G
      *
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
           MOVE    W20-RECEDSPKBN  TO  SPA-GMN-W20-RECEDSPKBN-G
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë
           MOVE    W20-BTUTYPE     TO  SPA-GMN-W20-BTUTYPE-G
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë
           MOVE    W20-BTUCD       TO  SPA-GMN-W20-BTUCD
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           MOVE    W20-TBTUTYPE    TO  SPA-GMN-W20-TBTUTYPE-G
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           MOVE    W20-TBTUCD      TO  SPA-GMN-W20-TBTUCD
      *
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬
           MOVE    W20-RYOYOKBN    TO  SPA-GMN-W20-RYOYOKBN-G
      *
      *    Í­¸ú´ü´Ö³«»ÏÆü
           MOVE    W20-YUKOSTYMD   TO  SPA-GMN-W20-YUKOSTYMD
      *
      *    Í­¸ú´ü´Ö½ªÎ»Æü
           MOVE    W20-YUKOEDYMD   TO  SPA-GMN-W20-YUKOEDYMD
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬
           MOVE    W20-EDKBN       TO  SPA-GMN-W20-EDKBN-G
      *
           IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
      *        ¾®µ¬ÌÏÉÂÅïÉÂ¾²¿ô²Ã»»
               MOVE    W20-KNGHJO  TO  SPA-GMN-W20-SYOUBTUBRM-G
           ELSE
      *        ´Ç¸îÊä½õ²Ã»»
               MOVE    W20-KNGHJO  TO  SPA-GMN-W20-KNGHJO-G
           END-IF
      *
      *    Æþ±¡ÎÁ²Ã»»
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   TO  IDX-GKSN
               IF    ( IDX-GKSN    >   ZERO )
                   EVALUATE    IDX-GKSN
                   WHEN    1   THRU    14
                       MOVE    W20-KSN-P01 (IDX-GKSN)
                                   TO  SPA-GMN-W20-KSNCD-G
                                                      (IDX-KSN)
                   WHEN    15  THRU    30
                       COMPUTE IDX-GKSN  = IDX-GKSN  -  14
                       MOVE    W20-KSN-P02 (IDX-GKSN)
                                   TO  SPA-GMN-W20-KSNCD-G
                                                      (IDX-KSN)
                   WHEN    31  THRU    46
                       COMPUTE IDX-GKSN  = IDX-GKSN  -  30
                       MOVE    W20-KSN-P03 (IDX-GKSN)
                                   TO  SPA-GMN-W20-KSNCD-G
                                                      (IDX-KSN)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¥Á¥§¥Ã¥¯
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-YKTTYMD-MAX )
      *
               MOVE    W20-YKTTYMD (IDXA)
                                   TO  SPA-GMN-W20-YKTTYMD (IDXA)
      *
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ðËÜ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÁªÂòÉÂÅïÈÖ¹æ¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-BTUSELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Í­¸ú´ü´Ö³«»ÏÆü¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Í­¸ú´ü´Ö½ªÎ»Æü¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-W20-BTULST-SEL  =   ZERO )
      *            ÉÂÅïÈÖ¹æ¥Á¥§¥Ã¥¯½èÍý
                   PERFORM 4101-BTUNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅïÌ¾¾Î¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-NAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Ã»½ÌÉÂÅïÌ¾¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-TANNAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÆÃÄêÆþ±¡ÎÁ¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-TOKUTEINYUIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï²ÊÌ¾¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï¼ïÊÌ¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-SBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Ìë¶ÐÄ¶²á¸º»»¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-YKNCHOKAKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-RECEDSPKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-BTUTYPE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-BTUCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-TBTUTYPE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-TBTUCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-RYOYOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÉÂÅï·ÑÂ³¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
               PERFORM 4101-EDKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Æþ±¡´ðËÜÎÁ¥Á¥§¥Ã¥¯
               PERFORM 4101-KHNRYO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KASAN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¥Á¥§¥Ã¥¯
               PERFORM 4101-YKTTYMD-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-KASAN-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    TRUE
               WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
      *            ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»
                   PERFORM 4101-SYOUBTUBRM-CHK-SEC
               WHEN  ( SPA-NAI-W20-HOSPSBT     =   1 )
      *            ´Ç¸îÊä½õ²Ã»»
                   PERFORM 4101-KNGHJO-CHK-SEC
               END-EVALUATE
           END-IF
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    TRUE
               WHEN    ( SPA-GMN-W20-SBT   =   CONST-KANSATU )
                   PERFORM 41021-KNST-KSN-CHK-SEC
               WHEN    OTHER
                   PERFORM 41021-KSN-CHK-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
      *    IF    ( SPA-ERRCD           =   SPACE )
      *        IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
      *            CONTINUE
      *        ELSE
      *            Æþ±¡²Ã»»¾ðÊó¥Á¥§¥Ã¥¯½èÍý-»ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *            PERFORM 4901-SSTKIJUN-CHK-SEC
      *        END-IF
      *    END-IF
      *
           .
       4101-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ´Ñ»¡Ë¡Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       41021-KNST-KSN-CHK-SEC          SECTION.
      *
           IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
               EVALUATE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
               WHEN    CONST-BTUMIKJNCD
                   PERFORM 4101-BTUMIKJN-CHK-SEC
               WHEN    CONST-SYOKUDOCD
                   PERFORM 4101-SYOKUDO-CHK-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       41021-KNST-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       41021-KSN-CHK-SEC           SECTION.
      *
           IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
               MOVE    SPA-GMN-W20-KSNCD (IDX-KSN)
                                   TO  WRK-CMB-CD
      *
               MOVE    SPACE       TO  WRK-CMB-ITM-G
               MOVE    SPA-GMN-W20-KSNLST-MAX (IDX-KSN)
                                   TO  WRK-MAX
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   WRK-MAX )
                        OR   ( WRK-CMB-ITM-G   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-W20-KSNCDL (IDX-KSN IDXL)
                                   =   SPA-GMN-W20-KSNCD (IDX-KSN))
                       MOVE    SPA-GMN-W20-KSNLST (IDX-KSN IDXL)
                                   TO  WRK-CMB-ITM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CMB-ITM =   SPACE )
                   MOVE    SPA-GMN-W20-KSNLST (IDX-KSN 1)
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               ELSE
                   MOVE    WRK-CMB-ITM
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               END-IF
      *
           END-IF
      *
           .
       41021-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìá¤ë¡¡½èÍý
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF    ( SPA-W20-W21-MOTOPG  =   SPACE )
               MOVE    "W01"               TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-W20-W21-MOTOPG  TO  SPA-SAKIPG
           END-IF
           MOVE    "W20"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾ò·ï¥¯¥ê¥¢½èÍý
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    ¾ò·ï¥¯¥ê¥¢½èÍý¡Ê¥³¥é¥à¥ê¥¹¥È¤âºÆÊÔ½¸¡Ë
      *****************************************************************
       421-CLEAR-SEC               SECTION.
      *
           PERFORM 3101-EDIT-BTULST-SEC
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           .
       421-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    ÄÉ²Ã½èÍý
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
      *    ÆþÎÏ¹àÌÜÆþÎÏ»þ¥Á¥§¥Ã¥¯
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        ¶¦ÄÌ´ØÏ¢¥Á¥§¥Ã¥¯
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        ´ûÂ¸¤ÎÍúÎò¥Ç¡¼¥¿¤ò¥Á¥§¥Ã¥¯¤·¡¢´ü´Ö¤Î½ÅÊ£¤·¤¿¥Ç¡¼¥¿¤¬
      *        Â¸ºß¤·¤Ê¤¤¤³¤È
               MOVE    1                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               EVALUATE    FLG-KIKAN-DABURI
               WHEN    ZERO
                   PERFORM 4201-TUIKA-SEC
               WHEN    1
                   MOVE    "2002"      TO  SPA-WIDCD
               WHEN    OTHER
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-EVALUATE
           END-IF
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    ÄÉ²Ã½èÍý
      *****************************************************************
       4201-TUIKA-SEC               SECTION.
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬¤¬ÇÑ»ß¤Î¾ì¹ç¡¢
      *    ÉÂ¼¼´ÉÍý¾ðÊó¡ÊÍ­¸ú´ü´Ö½ªÎ»Æü¡Ë¤ò¹¹¿·¤¹¤ë
           IF    ( SPA-GMN-W20-EDKBN   =   "2" )
      *        ÉÂ¼¼´ÉÍý¾ðÊó¹¹¿·½èÍý
               PERFORM 4201-BRM-UPDATE-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4201-TUIKA-EXT
               END-IF
      *
           END-IF
      *
      *    ÉÂÅï¾ðÊóÄÉ²ÃÊÔ½¸½èÍý
           PERFORM 4201-BTU-INSERT-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4201-TUIKA-EXT
           END-IF
      *
           PERFORM 421-CLEAR-SEC
      *
      *    ÉÂÅï¥ê¥¹¥ÈºÇÂç·ï¿ô¥Á¥§¥Ã¥¯
           IF    ( SPA-GMN-W20-BTULST-MAX  >=  CONST-BTULST-MAX )
               MOVE    "1001"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-GMN-W20-BTULST-SEL  =   SPA-GMN-W20-BTULST-MAX )
               MOVE    2           TO  FLG-BTULST-ROW
           ELSE
               MOVE    ZERO        TO  FLG-BTULST-ROW
           END-IF
      *
           .
       4201-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    Í­¸ú´ü´ÖÊÑ¹¹½èÍý
      *****************************************************************
       4201-EDYUKYMD-UPDATE-SEC        SECTION.
      *
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
      *
               MOVE    SYS-5001-REC    TO  SYSKANRI-REC
               MOVE    SYS-KEY         TO  SYSUPD-KEY
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  SYS-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd3"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1205"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    "5008"          TO  SYS-KANRICD
                                           SYSUP-KANRICD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd3"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1209"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4201-EDYUKYMD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ÊÑ¹¹½èÍý
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
      *    ÆþÎÏ¹àÌÜÆþÎÏ»þ¥Á¥§¥Ã¥¯
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    ¥ê¥¹¥È¤Ë³ºÅöÈÖ¹æ¤ÎÉÂÅï¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           PERFORM 4100-BTUNUM-LSTSEL-SEC
           IF    ( IDX-BTULST      =   ZERO )
               MOVE    "1002"      TO  SPA-ERRCD
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    ¶¦ÄÌ´ØÏ¢¥Á¥§¥Ã¥¯
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    Í­¸ú´ü´Ö¤Î³«»ÏÆü¤È½ªÎ»Æü¤¬Æ±»þ¤ËÊÑ¹¹¤µ¤ì¤Æ¤¤¤Ê¤¤¤³¤È
           IF    ( SPA-NAI-W20-YUKOSTYMD
                                   NOT =   SPA-NAI-W20-YUKOSTYMD-OLD )
            AND
                 ( SPA-NAI-W20-YUKOEDYMD
                                   NOT =   SPA-NAI-W20-YUKOEDYMD-OLD )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    ´ûÂ¸¤ÎÍúÎò¥Ç¡¼¥¿¤ò¥Á¥§¥Ã¥¯¤·¡¢´ü´Ö¤Î½ÅÊ£¤·¤¿¥Ç¡¼¥¿¤¬
      *    Â¸ºß¤·¤Ê¤¤¤³¤È
           MOVE    2                   TO  FLG-UPD-KBN
           PERFORM 4100-RIREKI-CHK-SEC
           IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
               MOVE    "1104"          TO  SPA-ERRCD
               MOVE    51              TO  SPA-GMN-W20-CUR
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬¤¬ÇÑ»ß¤Î¾ì¹ç¡¢
      *    ÉÂ¼¼´ÉÍý¾ðÊó¡ÊÍ­¸ú´ü´Ö½ªÎ»Æü¡Ë¤ò¹¹¿·¤¹¤ë
           IF    ( SPA-GMN-W20-EDKBN   =   "2" )
      *        ÉÂ¼¼´ÉÍý¾ðÊó¹¹¿·½èÍý
               PERFORM 4201-BRM-UPDATE-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  420-HENKOU-EXT
               END-IF
      *
           END-IF
      *
      *    ÉÂÅï´ÉÍý¾ðÊó¹¹¿·½èÍý
           PERFORM 4201-BTU-UPDATE-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *
           PERFORM 421-CLEAR-SEC
      *
           MOVE    ZERO            TO  FLG-BTULST-ROW
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    ÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
      *
      *    ÉÂÅï¾ðÊó¸¡º÷½èÍý
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                    OR   ( FLG-KIKAN-DABURI    NOT =   ZERO )
      *
      *        Í­¸ú´ü´Ö¤¬½ÅÊ£¤¹¤ë¥Ç¡¼¥¿¤¬¤Ê¤¤¤«¥Á¥§¥Ã¥¯
      *        ¡ÊÊÑ¹¹½èÍý¤Î¾ì¹ç¡¢¸µ¥Ç¡¼¥¿¤ÏÂÐ¾Ý³°¤È¤¹¤ë¡Ë
               IF    ( FLG-UPD-KBN         =  2 )
                AND  ( SPA-NAI-W20-YUKOSTYMD-OLD 
                                           =   SYS-5001-STYUKYMD )
                AND  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                           =   SYS-5001-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-W20-YUKOEDYMD
                                           <   SYS-5001-STYUKYMD )
                     OR  ( SPA-NAI-W20-YUKOSTYMD
                                           >   SYS-5001-EDYUKYMD )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-W20-YUKOSTYMD
                                           >   SYS-5001-STYUKYMD )
                        AND  ( SPA-NAI-W20-YUKOSTYMD
                                          <=   SYS-5001-EDYUKYMD )
                           MOVE    1       TO  FLG-KIKAN-DABURI
                       ELSE
                           MOVE    2       TO  FLG-KIKAN-DABURI
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ºï½ü½èÍý
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
      *    ÉÂÅïÈÖ¹æ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 4101-BTUSELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *   ¥ê¥¹¥È¤Ë³ºÅöÈÖ¹æ¤ÎÉÂÅï¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           PERFORM 4100-BTUNUM-LSTSEL-SEC
           IF    ( IDX-BTULST      =   ZERO )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *    ºï½ü¤¹¤ëÉÂÅï¤ÎÍ­¸ú´ü´ÖÆâ¤ËÉÂ¼¼¤Î¾ðÊó¤¬Â¸ºß¤·¤Ê¤¤¤«¥Á¥§¥Ã¥¯
           PERFORM 4200-BRM-SONZAI-CHK-SEC
           IF    ( FLG-BRM-SONZAI  =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1006"      TO  SPA-ERRCD
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
           MOVE    "2001"          TO  SPA-WIDCD
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    ºï½ü½èÍý
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
      *    ÉÂÅï¾ðÊóºï½ü½èÍý
           PERFORM  4201-BTU-DELETE-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  421-SAKUJO-EXT
           END-IF
      *
           PERFORM 421-CLEAR-SEC
      *
           MOVE    ZERO            TO  FLG-BTULST-ROW
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂ¼¼´ÉÍý¾ðÊóÂ¸ºß¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-BRM-SONZAI-CHK-SEC         SECTION.
      *
      *    ºï½ü¤¹¤ëÉÂÅï¤ÎÍ­¸ú´ü´ÖÆâ¤ËÉÂ¼¼¤Î¾ðÊó¤¬Â¸ºß¤·¤Ê¤¤¤«¥Á¥§¥Ã¥¯
      *    ¤¹¤ë
           MOVE    ZERO            TO  FLG-BRM-SONZAI
      *
           PERFORM 900-SYSKANRI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-BRM-SONZAI      =   1 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD-OLD
                                      >=   SYS-5002-EDYUKYMD )
                 OR  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                      <=   SYS-5002-STYUKYMD )
                   CONTINUE
               ELSE
      *
                   MOVE    1       TO  FLG-BRM-SONZAI
      *
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4200-BRM-SONZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂ¼¼´ÉÍý¤Ø
      *****************************************************************
       420-BYOSITU-KANRI-SEC           SECTION.
      *
      *    ²èÌÌ¤ò£Ó£Ð£Á¥»¥Ã¥È
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE  SPA-W20-W21-AREA
      *
      *    ÉÂÅï¤¬ÁªÂò¤µ¤ì¤Æ¤¤¤ë¾ì¹ç¤Ï¡¢ÉÂ¼¼´ÉÍý²èÌÌ¤ËÉÂÅïÈÖ¹æ¤ò°ú·Ñ¤°
           IF    ( SPA-GMN-W20-BTULST-SEL
                                       NOT =   ZERO )
                 MOVE    SPA-GMN-W20-BTULST-SEL 
                                       TO  IDX1
                 MOVE    SPA-GMN-W20-TBTUNUM (IDX1)
                                       TO  SPA-W20-W21-BTUNUM
           END-IF
      *
           MOVE    "W21"               TO  SPA-SAKIPG
           MOVE    "W20"               TO  SPA-MOTOPG
                                           SPA-W20-W21-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       420-BYOSITU-KANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»ÀßÄê¤Ø
      *****************************************************************
       420-W201-SYORI-SEC           SECTION.
      *
      *    ²èÌÌ¤ò£Ó£Ð£Á¥»¥Ã¥È
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD
                                   >=  CONST-H240401 )
               CONTINUE
           ELSE
               MOVE    "1111"          TO  SPA-ERRCD
           END-IF
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-W20-YUKOSTYMD
                                   NOT =   SPA-NAI-W20-YUKOSTYMD-OLD )
                   MOVE    "1110"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4201-W201-SYORI-SEC
           END-IF
      *
           .
       420-W201-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»ÀßÄê¤Ø
      *****************************************************************
       4201-W201-SYORI-SEC           SECTION.
      *
      *    ½é´ü²èÌÌ¥»¥Ã¥È
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W20FREE         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW201"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-W20FREE
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
      *
           MOVE    "W201"              TO  SPA-SAKIPG
           MOVE    "W20"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      SPA-W201-TKSNCD2(1) =   SPACE
               MOVE    "KSNCD2"        TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"        TO  MCP-WIDGET
           END-IF
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W201"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       4201-W201-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂ¼¼´ÉÍý¾ðÊó¹¹¿·½èÍý
      *****************************************************************
       4201-BRM-UPDATE-SEC             SECTION.
      *
      *    ÉÂ¼¼´ÉÍý¾ðÊó¸¡º÷½èÍý
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    "%"             TO  SYS-KBNCD(3:1)
           MOVE    SPA-NAI-W20-YUKOEDYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
               MOVE    SYS-5002-EDYUKYMD
                                       TO  WRK-EDYUKYMD
      *
      *        ÊÑ¹¹¹àÌÜ
               MOVE    SPA-NAI-W20-YUKOEDYMD
                                       TO  SYS-5002-EDYUKYMD
      *
               MOVE    SYS-5002-REC    TO  SYSKANRI-REC
      *
      *        ¥­¡¼¹àÌÜ
               MOVE    SYS-5002-KANRICD
                                       TO  SYSUP-KANRICD
               MOVE    SYS-5002-KBNCD
                                       TO  SYSUP-KBNCD
               MOVE    SYS-5002-STYUKYMD
                                       TO  SYSUP-STYUKYMD
               MOVE    WRK-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1204"      TO  SPA-ERRCD
                   GO  TO  4201-BRM-UPDATE-EXT
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4201-BRM-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊó¹¹¿·½èÍý
      *****************************************************************
       4201-BTU-UPDATE-SEC             SECTION.
      *
      *    ¥¢¥Ã¥×¥Ç¡¼¥ÈÂÐ¾Ý¤Î¥Ç¡¼¥¿¤òÆÉ¤ßÄ¾¤¹
           INITIALIZE                  SYSKANRI-REC
      *
            MOVE   "5001"              TO  SYS-KANRICD
            MOVE   SPA-GMN-W20-BTUNUM  TO  SYS-KBNCD
            MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
               GO  TO  4201-BTU-UPDATE-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý
           PERFORM 4201-BTU-EDIT-SEC
      *
      *    ÉÂÅï²Ã»»¾ðÊóÊÔ½¸½èÍý
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD
                                   TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD  TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                   TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                   TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
           MOVE    SPA-OPID        TO  SYS-OPID
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "upd1"          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC      NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SYS-5008-TBL    =   SPACE )
               MOVE    SYSUPD-KEY      TO  SYS-KEY
               MOVE    "5008"          TO  SYS-KANRICD
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           ELSE
               MOVE    "5008"          TO  SYS-KANRICD
                                           SYSUP-KANRICD
               MOVE    SYS-5008-TBL    TO  SYS-KANRITBL
               MOVE    SYSKANRI-REC    TO  SYS-5008-REC
               MOVE    SYS-5008-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC          =   ZERO )
                   MOVE    SYS-5008-REC
                                       TO  SYSKANRI-REC
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =    ZERO )
                       CONTINUE
                   ELSE
                       MOVE    SYS-5008-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF   (  MCP-RC          =   ZERO )
                           CONTINUE
                       ELSE
                           MOVE    "1211"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "1210"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           .
      *
       4201-BTU-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÄÉ²Ã½èÍý
      *****************************************************************
       4201-BTU-INSERT-SEC             SECTION.
      *
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           MOVE    SYS-5001-REC    TO  TMP-5001-REC
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           IF    ( LNK-DAY6-KEISAN     NOT =   TMP-5001-EDYUKYMD )
               MOVE    SPACE       TO  TMP-5001-REC
               INITIALIZE              TMP-5001-REC
           END-IF
      *
           MOVE    SPACE           TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
      *
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý
           PERFORM 4201-BTU-EDIT-SEC
      *
      *    ÉÂÅï²Ã»»¾ðÊóÊÔ½¸½èÍý
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
      *    Æþ±¡ÎÁ²Ã»»¡Ê¥æ¡¼¥¶ÀßÄêÎÎ°è¡Ë
           MOVE    TMP-5001-KSN2   TO  SYS-5001-KSN2
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-5001-CREYMD
                                       SYS-5001-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-5001-UPHMS
           MOVE    SPA-OPID        TO  SYS-5001-OPID
      *
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    "1202"      TO  SPA-ERRCD
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYS-5001-HOSPNUM
               MOVE    SYS-5001-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1202"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SYS-5008-TBL    NOT =   SPACE )
               MOVE    SYS-5001-KEY    TO  SYS-5008-KEY
               MOVE    "5008"          TO  SYS-5008-KANRICD
      *
               MOVE    SYS-5008-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    "1207"      TO  SPA-ERRCD
               ELSE
                   MOVE    SYS-5008-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF   (  MCP-RC      NOT =   ZERO )
                       MOVE    "1207"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4201-BTU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóºï½ü½èÍý
      *****************************************************************
       4201-BTU-DELETE-SEC             SECTION.
      *
           INITIALIZE                     SYS-5001-REC
           MOVE    "5001"             TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM TO  SYS-5001-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                      TO  SYS-5001-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                      TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM        TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC       TO  MCPDATA-REC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC         NOT =   ZERO )
               MOVE    "1203"      TO  SPA-ERRCD
           END-IF
      *
           MOVE    SYS-5001-REC       TO  SYS-5008-REC
           MOVE    "5008"             TO  SYS-5008-KANRICD
           MOVE    SYS-5008-REC       TO  MCPDATA-REC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC         NOT =   ZERO )
               MOVE    "1208"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-BTU-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       4201-BTU-EDIT-SEC               SECTION.
      *
      *    MOVE    SPACE       TO  SYS-5001-REC
      *    INITIALIZE              SYS-5001-REC
      *
           MOVE    SPACE           TO  SYS-5008-REC
           INITIALIZE                  SYS-5008-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
      *
      *    ´ÉÍý¥³¡¼¥É
           MOVE    "5001"              TO  SYS-5001-KANRICD
      *
      *    ¶èÊ¬¥³¡¼¥É
           MOVE    SPA-GMN-W20-BTUNUM  TO  WRK-BTUNUM 
           MOVE    WRK-BTUNUM          TO  SYS-5001-KBNCD
      *
      *    Í­¸ú³«»ÏÇ¯·îÆü
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-5001-STYUKYMD
      *
      *    Í­¸ú½ªÎ»Ç¯·îÆü
           MOVE    SPA-NAI-W20-YUKOEDYMD
                                       TO  SYS-5001-EDYUKYMD
      *
      *    ÉÂÅïÈÖ¹æ
           MOVE    WRK-BTUNUM          TO  SYS-5001-BTU-NUM
      *
      *    ÉÂÅïÌ¾¾Î
           MOVE    SPA-GMN-W20-NAME    TO  SYS-5001-BTU-NAME
      *
      *    ÉÂÅïÃ»½ÌÌ¾¾Î
           MOVE    SPA-GMN-W20-TANNAME TO  SYS-5001-BTU-TANNAME
      *
      *    ÆÃÄêÆþ±¡ÎÁ
           MOVE    SPA-NAI-W20-TOKNYUIN
                                       TO  SYS-5001-BTU-TOKTEI-NYUIN
      *
      *    ÉÂÅï²Ê
           MOVE    SPA-GMN-W20-SRYKA   TO  SYS-5001-BTU-KA
      *
      *    ÉÂÅï¼ïÊÌ
           MOVE    SPA-GMN-W20-SBT     TO  SYS-5001-BTU-SBT
      *
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H260401 )
             AND ( SPA-NAI-W20-YKNCHOKA      =  "1" )
             AND ( SPA-GMN-W20-YKNCHOKAKBN   = "1" )
               MOVE    "1"             TO  SYS-5001-YKNCHOKAKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-YKNCHOKAKBN
           END-EVALUATE
      *
      *    Ìë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¶èÊ¬
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H280401 )
             AND ( SPA-NAI-W20-YKNTOKUBETSU  = "1" )
             AND ( SPA-GMN-W20-YKNCHOKAKBN   = "2" )
               MOVE    "1"             TO  SYS-5001-YKNTOKUKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-YKNTOKUKBN
           END-EVALUATE
      *
      *    ´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¶èÊ¬
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H280401 )
             AND ( SPA-NAI-W20-KNGKEIKASOCHI = "1" )
             AND ( SPA-GMN-W20-YKNCHOKAKBN   = "3" )
               MOVE    "1"             TO  SYS-5001-SYOKUINKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-SYOKUINKBN
           END-EVALUATE
      *
      *    ´Ç¸î¿¦°÷ÈæÎ¨²áÁ¼ÃÖ¶èÊ¬
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H300401 )
             AND ( SPA-NAI-W20-SYOKUINKBN2   = "1" )
             AND ( SPA-GMN-W20-YKNCHOKAKBN   = "4" )
               MOVE    "1"             TO  SYS-5001-SYOKUINKBN2
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-SYOKUINKBN2
           END-EVALUATE
      *
      *    ½Å¾É´µ¼Ô³ä¹çÆÃÊÌÆþ±¡´ðËÜÎÁ¶èÊ¬
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H300401 )
             AND ( SPA-NAI-W20-JUUSHOUKNGKBN = "1" )
             AND ( SPA-GMN-W20-YKNCHOKAKBN   = "5" )
               MOVE    "1"             TO  SYS-5001-JUUSHOUKNGKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-JUUSHOUKNGKBN
           END-EVALUATE
      *
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
           MOVE    SPA-GMN-W20-RECEDSPKBN
                                       TO  SYS-5001-RECEDSPKBN
      *    ÉÂÅï¥³¡¼¥É
           IF    ( SPA-GMN-W20-BTUTYPE NOT =   SPACE )
               MOVE    SPACE           TO  SYS-5001-SRYCD
               MOVE    "1906"          TO  SYS-5001-SRYCD
               MOVE    SPA-GMN-W20-BTUTYPE
                                       TO  SYS-5001-SRYCD(5:1)
               MOVE    SPA-GMN-W20-BTUCD
                                       TO  WRK-KBN4
               MOVE    WRK-KBN4-G      TO  SYS-5001-SRYCD(6:)
           ELSE
               MOVE    SPACE           TO  SYS-5001-SRYCD
           END-IF
           IF    ( SPA-GMN-W20-TBTUTYPE NOT =   SPACE )
               MOVE    SPACE           TO  SYS-5001-TSRYCD
               MOVE    "1906"          TO  SYS-5001-TSRYCD
               MOVE    SPA-GMN-W20-TBTUTYPE
                                       TO  SYS-5001-TSRYCD(5:1)
               MOVE    SPA-GMN-W20-TBTUCD
                                       TO  WRK-KBN4
               MOVE    WRK-KBN4-G      TO  SYS-5001-TSRYCD(6:)
           ELSE
               MOVE    SPACE           TO  SYS-5001-TSRYCD
           END-IF
      *
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬
           PERFORM 42011-RYOYOSANTEI-CHK-SEC
           EVALUATE    TRUE
           WHEN  ( FLG-RYOYOSANTEI =   1 )
               MOVE    SPA-GMN-W20-RYOYOKBN
                                       TO  SYS-5001-CRYOYOKBN
           WHEN    OTHER
               MOVE    "0"             TO  SYS-5001-CRYOYOKBN
           END-EVALUATE
      *
           PERFORM 42011-KHNRYO-EDIT-SEC
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬
           MOVE    SPA-GMN-W20-EDKBN   TO  SYS-5001-BTU-EDKBN
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü
           MOVE    ZERO                TO  IDXB
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-YKTTYMD-MAX )
      *
               IF    ( SPA-NAI-W20-YKTTYMD (IDXA)  NOT =   SPACE )
                   COMPUTE IDXB = IDXB +   1
                   MOVE    SPA-NAI-W20-YKTTYMD (IDXA)
                                       TO  SYS-5008-YAKANGENYMD (IDXB)
               END-IF
      *
           END-PERFORM
      *
           .
       4201-BTU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       42011-RYOYOSANTEI-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOYOSANTEI
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H261001 )
            AND  ( SPA-GMN-W20-SBT        =   "01" OR "06" )
           WHEN  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H241001 )
            AND  ( SPA-NAI-W20-YUKOSTYMD <   CONST-H300401 )
            AND  ( SPA-GMN-W20-SBT        =   "01" )
            AND  ( SPA-NAI-W20-NYUKHNKBN  =   56 OR 57 )
      *
               MOVE    1       TO  FLG-RYOYOSANTEI
      *
               IF    ( SPA-NAI-W20-NYUKHNKBN  =   79 )
                   MOVE    ZERO    TO  FLG-RYOYOSANTEI
               END-IF
      *
               IF    ( SPA-NAI-W20-KHNSRYCD   =   "190079010" )
                   MOVE    ZERO    TO  FLG-RYOYOSANTEI
               END-IF
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
            AND  ( SPA-GMN-W20-SBT          =  "08" OR "09" )
                   MOVE    1       TO  FLG-RYOYOSANTEI
           END-EVALUATE
      *
           .
       42011-RYOYOSANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       42011-KHNRYO-EDIT-SEC           SECTION.
      *
      *    Æþ±¡´ðËÜÎÁÅÀ¿ô¥³¡¼¥É
           IF    ( SPA-GMN-W20-SBT         =   "04" OR "09" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180701 )
      *
               MOVE    SPACE           TO  SYS-5001-BTU-KHNSRYCD
                                           SYS-5001-KHNSRYCD
                                           SYS-5001-KHNKBNCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY2-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
               PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                        OR   ( SYS-5001-KHNSRYCD   NOT =   SPACE )
      *
                   IF    ( SPA-NAI-W20-KHNSRYCD    =   SYS-5111-SRYCD )
                       MOVE    SPA-NAI-W20-KHNSRYCD
                                       TO  SYS-5001-KHNSRYCD
                       MOVE    "3"     TO  SYS-5001-KHNKBNCD (1:1)
                       MOVE    SYS-5111-KBNCD
                                       TO  SYS-5001-KHNKBNCD (2:2)
                   END-IF
      *
                   PERFORM 900-SYSKANRI-KEY2-FET-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           ELSE
               MOVE    SPA-NAI-W20-KHNSRYCD
                                       TO  SYS-5001-BTU-KHNSRYCD
               MOVE    SPACE           TO  SYS-5001-KHNSRYCD
                                           SYS-5001-KHNKBNCD
           END-IF
      *
           .
       42011-KHNRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       4201-BTU-KSN-EDIT-SEC           SECTION.
      *
           INITIALIZE                  TKSN-AREA
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               PERFORM 42013-BTU-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H300401 )
               PERFORM 42017-H30R02-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H280401 )
               PERFORM 42016-H28-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H260401 )
               PERFORM 42015-BTU-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
               PERFORM 42014-BTU-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H200401 )
               PERFORM 42012-BTU-KSN-EDIT-SEC
           WHEN    OTHER
               PERFORM 42011-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
           .
       4201-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(¡ÁH20.03)
      *****************************************************************
       42011-BTU-KSN-EDIT-SEC          SECTION.
      *
      *    --  ²Ã»»¾ðÊó¤Ë½é´üÃÍ¤òÀßÄê  --
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-YKNKNG  =   "0" )
               MOVE    SPACE           TO  SYS-5001-BTU-YAKANKNGKSN
           ELSE
               MOVE    "00"            TO  SYS-5001-BTU-YAKANKNGKSN
               MOVE    SPA-GMN-W20-YKNKNG
                                       TO  SYS-5001-BTU-YAKANKNGKSN(2:1)
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    ÊÝ°é»Î¿Í¿ô¶èÊ¬
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    ÆñÉÂ´µ¼ÔÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           MOVE    CONST-NANBYO    TO  SYS-5001-NANBYO-CD
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    "2"             TO  SYS-5001-NANBYO-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-NANBYO-KBN
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    CONST-TOKUSYUCD TO  SYS-5001-TOKUSYU-CD
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    "2"             TO  SYS-5001-TOKUSYU-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-TOKUSYU-KBN
           END-IF
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           MOVE    CONST-JIDOCD    TO  SYS-5001-JIDO-CD
           IF    ( SPA-GMN-W20-JIDO    =   "2" )
               MOVE    "2"             TO  SYS-5001-JIDO-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-JIDO-KBN
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    CONST-SEISINCD  TO  SYS-5001-SEISIN-CD
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    "2"             TO  SYS-5001-SEISIN-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-SEISIN-KBN
           END-IF
      *
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H180401 )
               MOVE    CONST-KANGOCD (1)
                                   TO  SYS-5001-KANGO-CD  (1)
               IF    ( SPA-GMN-W20-HICH    =   "2" )
                   MOVE    "2"         TO  SYS-5001-KANGO-KBN (1)
               ELSE
                   MOVE    "1"         TO  SYS-5001-KANGO-KBN (1)
               END-IF
           ELSE
      *        ´Ç¸îÇÛÃÖ²Ã»»
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   5 )
                   MOVE    CONST-KANGOCD (IDX1)
                                       TO  SYS-5001-KANGO-CD (IDX1)
      *
                   IF    ( SPA-GMN-W20-HICH    =   CONST-KANGOCDL(IDX1))
                       MOVE    "2"     TO  SYS-5001-KANGO-KBN (IDX1)
                   ELSE
                       MOVE    "1"     TO  SYS-5001-KANGO-KBN (IDX1)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    CONST-KNGTEKCD  TO  SYS-5001-KNGTEK-CD
           IF    ( SPA-GMN-W20-HICHTEK =   "2" )
               MOVE    "2"         TO  SYS-5001-KNGTEK-KBN
           ELSE
               MOVE    "1"         TO  SYS-5001-KNGTEK-KBN
           END-IF
      *
           MOVE    CONST-DRTEKCD   TO  SYS-5001-DRTEK-CD
           IF    ( SPA-GMN-W20-DRTEK   =   "2" )
               MOVE    "2"         TO  SYS-5001-DRTEK-KBN
           ELSE
               MOVE    "1"         TO  SYS-5001-DRTEK-KBN
           END-IF
      *
           .
       42011-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ´Ñ»¡Ë¡²Ã»»ÊÔ½¸½èÍý
      *****************************************************************
       42013-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»
           IF    ( SPA-GMN-W20-SYOUBTUBRM  NOT =   "00" )
               MOVE    SPA-NAI-W20-SYOUBTUBRMCD
                                       TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ì¤´ð½à¡ÊÉÂÅï¤Ë·¸¤ë´ð½à¤ò¤ß¤¿¤µ¤Ê¤¤¾ì¹ç¡Ë
           IF    ( SPA-GMN-W20-BTUMIKJN   =   "2" )
               MOVE    CONST-BTUMIKJNCD
                                   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
           .
       42013-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H20.04¡Á)
      *****************************************************************
       42012-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    ÊÝ°é»Î¿Í¿ô¶èÊ¬
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    ÆñÉÂ´µ¼ÔÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-NANBYO    TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-TOKUSYUCD TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-JIDO    =   "2" )
               MOVE    CONST-JIDOCD    TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-SEISINCD  TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-KANGOCD (1)
                                   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO   IS  NUMERIC )
               MOVE    SPA-GMN-W20-DRHJO   TO  WRK-KBN1-G
               IF    ( WRK-KBN1        >   1 )
                AND  ( WRK-KBN1        <=  SPA-GMN-W20-DRHJO-MAX )
                   COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                       MOVE    CONST-H2204-DRHJO (WRK-KBN1)
                                           TO  WRK-KSNCD
                       PERFORM 420121-BTU-KSN-EDIT-SEC
                   WHEN    OTHER
                       MOVE    CONST-DRHJO (WRK-KBN1)
                                           TO  WRK-KSNCD
                       PERFORM 420121-BTU-KSN-EDIT-SEC
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
           IF    ( SPA-GMN-W20-JKAIFUKU    =   "2" )
               MOVE    CONST-JKAIFUKU     TO   WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    CONST-H2204-DRTEKCD1    TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-YKNKNKYU      TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    CONST-H2204-SNRHICH1
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    CONST-SNRHICH1
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-SNRHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-H2204-SNRHICH2
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-SNRHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-SNRHICH2
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-YKNHICH1
                                                TO  WRK-KBN1-G
                   MOVE    CONST-H2204-YKNHICH1
                                                TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-YKNHICH1 TO  WRK-KBN1-G
                   MOVE    CONST-YKNHICH1       TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-YKNHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-H2204-YKNHICH2 TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-YKNHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-YKNHICH2       TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               EVALUATE    SPA-GMN-W20-KYUKNGHJ
      *        µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»£±
               WHEN    "2"
                   MOVE    CONST-KYUKNGHJ1         TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
      *        µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»£²
               WHEN    "3"
                   MOVE    CONST-KYUKNGHJ2         TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-EVALUATE
           END-IF
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-SEICHIIKI     =  "2" )
               MOVE    SPA-GMN-W20-SEICHIIKI TO  WRK-KBN1-G
               MOVE    CONST-SEICHIIKI       TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
                   MOVE    SPA-GMN-W20-HYOUKA    TO  WRK-KBN1-G
                   MOVE    CONST-HYOUKA          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
                   MOVE    SPA-GMN-W20-KJTRIHA    TO  WRK-KBN1-G
                   MOVE    CONST-KJTRIHA          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
           .
       42012-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H24.04¡ÁH26.03)
      *****************************************************************
       42014-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           EVALUATE    SPA-GMN-W20-KNGHITU
           WHEN    "2"
               MOVE    CONST-H2404-KNGHITU1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-KNGHITU2
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
               MOVE    CONST-HYOUKA        TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HYOUKA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-YKNKNKYU      TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNKYU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               MOVE    CONST-H2204-DRTEKCD1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               MOVE    CONST-H2204-SNRHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               MOVE    CONST-H2204-SNRHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               MOVE    CONST-H2204-YKNHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               MOVE    CONST-H2204-YKNHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H2404-DRHJO (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-KYUKNGHJ    <=   "5" )
               MOVE    SPA-GMN-W20-KYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H2404-KYUKNGHJ (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           EVALUATE    SPA-GMN-W20-YKNKYUKNGHJ
           WHEN    "2"
               MOVE    CONST-H2404-YKNKYUKNGHJ1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-YKNKYUKNGHJ2
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGYKNHICHI   =  "2" )
               MOVE    CONST-H2404-KNGYKNHICHI
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGYKNHICHI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆñÉÂ´µ¼ÔÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-NANBYO    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NANBYO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-TOKUSYUCD TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TOKUSYU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-KANGOCD (1)
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HICH-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-SEISINCD  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEISIN-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-SEICHIIKI   =  "2" )
               MOVE    CONST-SEICHIIKI     TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEICHIIKI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-BTUYKZ      =   "2" )
               MOVE    CONST-H2404-BTUYKZ  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-BTUYKZ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÊÝ°é»Î¿Í¿ô¶èÊ¬
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
               MOVE    CONST-HOIKUCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HOIKU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
               MOVE    CONST-KJTRIHA       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KJTRIHA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
           .
       42014-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H26.04¡ÁH28.03)
      *****************************************************************
       42015-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    ´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           EVALUATE    SPA-GMN-W20-KNGHITU
           WHEN    "2"
               MOVE    CONST-H2404-KNGHITU1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-KNGHITU2
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-ADLIJI  =   "2" )
               MOVE    CONST-H26-ADLIJI-A100 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-ADLIJI-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-SSNHKNFUKU   =  "2" )
               MOVE    CONST-H26-SSNHKNFUKU-A103 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SSNHKNFUKU-IDX
                                          TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
               MOVE    CONST-HYOUKA        TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HYOUKA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-YKNKNKYU      TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNKYU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               MOVE    CONST-H2204-DRTEKCD1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               MOVE    CONST-H2204-SNRHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               MOVE    CONST-H2204-SNRHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO1  =   "2" )
               MOVE    CONST-H26-SNRHJO1   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO1-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO2  =   "2" )
               MOVE    CONST-H26-SNRHJO2   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO2-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               MOVE    CONST-H2204-YKNHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               MOVE    CONST-H2204-YKNHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           IF    ( SPA-GMN-W20-ZTKFUKKI     =  "2" )
               MOVE    CONST-H26-ZTKFUKKI   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-ZTKFUKKI-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           IF    ( SPA-GMN-W20-ZTKFUKKI     =  "2" )
               MOVE    CONST-H26-ZTKFUKKI   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-ZTKFUKKI-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-EIYO  =   "2" )
               MOVE    CONST-H26-EIYO-A108 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-EIYO-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÂÎÀ©¶¯²½²Ã»»
           IF    ( SPA-GMN-W20-TAISEI  =   "2" )
               MOVE    CONST-H26-TAISEI    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TAISEI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGSKHICH  =   "2" )
               MOVE    CONST-H26-KNGSKHICH TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGSKHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGHJSHICH  =   "2" )
               MOVE    CONST-H26-KNGHJSHICH TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHJSHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-SSNKYUDR  =   "2" )
               MOVE    CONST-H26-SSNKYUDR  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SSNKYUDR-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H26-DRHJO1 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO2   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO2   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO2
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H26-DRHJO2 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-KYUKNGHJ    <=   "5" )
               MOVE    SPA-GMN-W20-KYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H2404-KYUKNGHJ (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-YKNKYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-YKNKYUKNGHJ    <=   "4" )
               MOVE    SPA-GMN-W20-YKNKYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H26-YKNKYUKNGHJ (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGYKNHICHI   =  "2" )
               MOVE    CONST-H2404-KNGYKNHICHI
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGYKNHICHI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆñÉÂ´µ¼ÔÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-NANBYO    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NANBYO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-TOKUSYUCD TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TOKUSYU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-KANGOCD (1)
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HICH-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-SEISINCD  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEISIN-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-SEICHIIKI   =  "2" )
               MOVE    CONST-SEICHIIKI     TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEICHIIKI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-BTUYKZ      =   "2" )
               MOVE    CONST-H2404-BTUYKZ  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-BTUYKZ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÊÝ°é»Î¿Í¿ô¶èÊ¬
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
               MOVE    CONST-HOIKUCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HOIKU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
               MOVE    CONST-KJTRIHA       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KJTRIHA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
           .
       42015-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H28.04¡ÁH30.03)
      *****************************************************************
       42016-H28-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    ´Ç¸îÉ¬Í×ÅÙ²Ã»»
           IF    ( SPA-GMN-W20-KNGHITU     >=  "2" )
            AND  ( SPA-GMN-W20-KNGHITU     <=  "4" )
               MOVE    SPA-GMN-W20-KNGHITU
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A100C6 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
               MOVE    CONST-H28-A100C07   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HYOUKA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-ADLIJI  =   "2" )
               MOVE    CONST-H28-A100C12   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-ADLIJI-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-SSNHKNFUKU   =  "2" )
               MOVE    CONST-H28-A103C07    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SSNHKNFUKU-IDX
                                          TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           IF    ( SPA-GMN-W20-ZTKFUKKI     =  "2" )
               IF    ( SPA-NAI-W20-HOSPSBT  =  "1" )
                   MOVE    CONST-H28-A101C10   TO  WRK-KSNCD
               ELSE
                   MOVE    CONST-H28-A109C11   TO  WRK-KSNCD
               END-IF
               MOVE    SPA-NAI-W20-ZTKFUKKI-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÂÎÀ©¶¯²½²Ã»»
           EVALUATE    SPA-GMN-W20-TAISEI
           WHEN    "2"
               MOVE    CONST-H28-A308C05 (1)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TAISEI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H28-A308C05 (2)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TAISEI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGSKHICH  =   "2" )
               MOVE    CONST-H28-A308-03C03 TO WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGSKHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGHJSHICH  =   "2" )
               MOVE    CONST-H28-A308-03C04 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHJSHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-SSNKYUDR  =   "2" )
               MOVE    CONST-H28-A249      TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SSNKYUDR-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-0201 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO2   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO2   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO2
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-0202 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-KYUKNGHJ    <=   "5" )
               MOVE    SPA-GMN-W20-KYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-03 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-YKNKYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-YKNKYUKNGHJ    <=   "4" )
               MOVE    SPA-GMN-W20-YKNKYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-03C02 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÊä½õÂÎÀ©²Ã»»¡ÊµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»¡Ë
           IF    ( SPA-GMN-W20-YKNKNG-A207-03  =   "2" )
               MOVE    CONST-H28-A207-03C03 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNG-A207-03-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGYKNHICHI  >=  "2" )
            AND  ( SPA-GMN-W20-KNGYKNHICHI  <=  "4" )
               MOVE    SPA-GMN-W20-KNGYKNHICHI
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-04 (WRK-KBN1)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGYKNHICHI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆñÉÂÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-H28-A210  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NANBYO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-H28-A211  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TOKUSYU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-H28-A213
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HICH-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNGHJ    =   "2" )
               MOVE    CONST-H28-A214C02
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¡Ë
           IF    ( SPA-GMN-W20-YKNKNG-A214    =   "2" )
               MOVE    CONST-H28-A214C03
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNG-A214-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-H28-A228  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEISIN-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-SEICHIIKI   =  "2" )
               MOVE    CONST-H28-A230-02   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEICHIIKI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ë
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
               MOVE    CONST-H28-A307C02
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HOIKU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
               MOVE    CONST-H28-A308C02   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KJTRIHA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-H28-A108C04   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNKYU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               MOVE    CONST-H28-A108C0501
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-H28-A108C0502
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK2-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               MOVE    CONST-H28-A108C0601  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               MOVE    CONST-H28-A108C0602  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               MOVE    CONST-H28-A108C0603  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               MOVE    CONST-H28-A108C0604  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO1  =   "2" )
               MOVE    CONST-H28-A108C0605 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO1-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO2  =   "2" )
               MOVE    CONST-H28-A108C0606 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO2-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-EIYO  =   "2" )
               MOVE    CONST-H28-A108C10   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-EIYO-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
           .
       42016-H28-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H30.04¡Á)
      *****************************************************************
       42017-H30R02-KSN-EDIT-SEC       SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    ´Ç¸îÊä½õ²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    £Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-ADLIJI  =   "2" )
               MOVE    CONST-H28-A100C12   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-ADLIJI-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-SSNHKNFUKU   =  "2" )
               MOVE    CONST-H28-A103C07    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SSNHKNFUKU-IDX
                                          TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»
           IF    ( SPA-GMN-W20-ZTKFUKKI     =  "2" )
               IF    ( SPA-NAI-W20-HOSPSBT  =  "1" )
                   MOVE    CONST-H28-A101C10   TO  WRK-KSNCD
               ELSE
                   MOVE    CONST-H28-A109C11   TO  WRK-KSNCD
               END-IF
               MOVE    SPA-NAI-W20-ZTKFUKKI-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÂÎÀ©¶¯²½²Ã»»
           EVALUATE    SPA-GMN-W20-TAISEI
           WHEN    "2"
               MOVE    CONST-H28-A308C05 (1)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TAISEI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H28-A308C05 (2)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TAISEI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    ´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGSKHICH  =   "2" )
               MOVE    CONST-H28-A308-03C03 TO WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGSKHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGHJSHICH  =   "2" )
               MOVE    CONST-H28-A308-03C04 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHJSHICH-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»
           IF    ( SPA-NAI-W20-YUKOSTYMD  >= CONST-R020401 )
               EVALUATE    SPA-GMN-W20-SSNKYUDR
               WHEN    "2" THRU    "4"
                   COMPUTE WRK-KBN1
                         = FUNCTION NUMVAL(SPA-GMN-W20-SSNKYUDR) - 1
                   MOVE    CONST-R02-A249 (WRK-KBN1)
                                       TO  WRK-KSNCD
                   MOVE    SPA-NAI-W20-SSNKYUDR-IDX
                                       TO  WRK-KSNIDX
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-EVALUATE
           ELSE
               IF    ( SPA-GMN-W20-SSNKYUDR    =   "2" )
                   MOVE    CONST-H28-A249  TO  WRK-KSNCD
                   MOVE    SPA-NAI-W20-SSNKYUDR-IDX
                                           TO  WRK-KSNIDX
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-0201 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-DRHJO2   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO2   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO2
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-0202 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-KYUKNGHJ    <=   "5" )
               MOVE    SPA-GMN-W20-KYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-03 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-YKNKYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-YKNKYUKNGHJ    <=   "4" )
               MOVE    SPA-GMN-W20-YKNKYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H28-A207-03C02 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÊä½õÂÎÀ©²Ã»»¡ÊµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»¡Ë
           IF    ( SPA-GMN-W20-YKNKNG-A207-03  =   "2" )
               MOVE    CONST-H28-A207-03C03 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNG-A207-03-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KNGYKNHICHI  >=  "2" )
            AND  ( SPA-GMN-W20-KNGYKNHICHI  <=  "5" )
               MOVE    SPA-GMN-W20-KNGYKNHICHI
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H30-A207-04 (WRK-KBN1)
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGYKNHICHI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆñÉÂÅùÆÃÊÌÆþ±¡¿ÇÎÅ²Ã»»
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-H28-A210  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NANBYO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-H28-A211  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TOKUSYU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-H28-A213
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HICH-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNGHJ    =   "2" )
               MOVE    CONST-H28-A214C02
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¡Ë
           IF    ( SPA-GMN-W20-YKNKNG-A214    =   "2" )
               MOVE    CONST-H28-A214C03
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNG-A214-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-H28-A228  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEISIN-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-SEICHIIKI   =  "2" )
               MOVE    CONST-H28-A230-02   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEICHIIKI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»¡Ê¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ¡Ë
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
               MOVE    CONST-H28-A307C02
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HOIKU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»
           IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
               MOVE    CONST-H28-A308C02   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KJTRIHA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ¿©Æ²²Ã»»¶èÊ¬
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-H28-A108C04   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNKYU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               MOVE    CONST-H28-A108C0501
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °å»ÕÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-H28-A108C0502
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK2-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               MOVE    CONST-H28-A108C0601  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               MOVE    CONST-H28-A108C0602  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               MOVE    CONST-H28-A108C0603  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               MOVE    CONST-H28-A108C0604  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO1  =   "2" )
               MOVE    CONST-H28-A108C0605 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO1-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë
           IF    ( SPA-GMN-W20-SNRHJO2  =   "2" )
               MOVE    CONST-H28-A108C0606 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHJO2-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»
           IF    ( SPA-GMN-W20-EIYO  =   "2" )
               MOVE    CONST-H28-A108C10   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-EIYO-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    Ìë´Ö´Ç¸î²Ã»»
           IF    ( SPA-GMN-W20-H30YKNKNG  =   "2" )
               MOVE    CONST-H30-A101C13   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-H30YKNKNG-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÊä½õ²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
           IF    ( SPA-GMN-W20-KNGHJO-A104  =   "2" )
               MOVE    CONST-H30-A106C09   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHJO-A104-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *    Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê¾ã³²¼Ô»ÜÀß¡Ë
           IF    ( SPA-GMN-W20-YKNKNG-A104  =   "2" )
               MOVE    CONST-H30-A106C10   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNG-A104-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    ´Ç¸îÉ¬Í×ÅÙ²Ã»»
           IF    ( SPA-GMN-W20-KNGHITU     >=  "2" )
            AND  ( SPA-GMN-W20-KNGHITU     <=  "4" )
               MOVE    SPA-GMN-W20-KNGHITU
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H30-A105C3 (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
               MOVE    CONST-H30-A105C04   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HYOUKA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    £Á£³£°£¸¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KYH-A308-03  =   "2" )
               MOVE    CONST-H30-A308-03C07 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYH-A308-03-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    £Á£³£°£¸¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KYH-A311     =   "2" )
               MOVE    CONST-H30-A311C05   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYH-A311-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    £Á£³£±£±¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    ( SPA-GMN-W20-KYH-A311-03     =   "2" )
               MOVE    CONST-H30-A311-03C05
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYH-A311-03-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    £Á£³£±£´Ç§ÃÎ¾ÉÌë´ÖÂÐ±þ²Ã»»
           IF    ( SPA-GMN-W20-NYT    =   "2" )
               MOVE    CONST-H30-A314C03
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NYT-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
           .
       42017-H30R02-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H20.04¡Á)
      *****************************************************************
       420121-BTU-KSN-EDIT-SEC         SECTION.
      *
           MOVE    WRK-KSNCD           TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( TNS-HOSPSRYKBN  =   ZERO )
            OR   ( TNS-HOSPSRYKBN  =   SPA-NAI-W20-HOSPSBT )
               PERFORM 4201211-BTU-KSN-EDIT-SEC
           END-IF
      *
           .
       420121-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾ðÊóÊÔ½¸½èÍý(H20.04¡Á)
      *****************************************************************
       4201211-BTU-KSN-EDIT-SEC         SECTION.
      *
           IF    ( WRK-KSNCD            =  CONST-HOIKUCD )
               CONTINUE
           ELSE
               IF    ( CONST-5001-KSN-MAX   >  CNT-KSN )
                   COMPUTE CNT-KSN =   CNT-KSN +   1
      *
                   MOVE    WRK-KSNCD   TO SYS-5001-KSNCD (CNT-KSN)
                   MOVE    "2"         TO SYS-5001-KSNKBN (CNT-KSN)
               END-IF
           END-IF
      *
           IF    ( CONST-KSN-MAX       >   TKSN-MAX )
               COMPUTE TKSN-MAX    =   TKSN-MAX    +   1
               MOVE    WRK-KSNCD   TO  TKSN-SRYCD (TKSN-MAX)
               MOVE    WRK-KSNIDX  TO  TKSN-IDX   (TKSN-MAX)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-KSN-MAX )
                        OR   ( TKSN-LBL (TKSN-MAX) NOT =   SPACE )
      *
                   IF    ( SPA-NAI-W20-KSNINF-IDX (IDX0)
                                       =   WRK-KSNIDX )
                       MOVE    SPA-GMN-W20-KSN-LBL (IDX0)
                                   TO  TKSN-LBL (TKSN-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       4201211-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÁªÂòÈÖ¹æ
      *****************************************************************
       4100-BTUSELNUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-BTUSELNUM   TO  SPA-GMN-W20-BTUSELNUM
      *
           IF    ( SPA-GMN-W20-BTUSELNUM   =   ZERO )
               PERFORM 420-CLEAR-SEC
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               MOVE    SPA-GMN-W20-EDKBNLST (1)
                                       TO  SPA-GMN-W20-EDKBN-G
               MOVE    1           TO  SPA-GMN-W20-CUR
           ELSE
               PERFORM 41001-BTUSELNUM-CHK-SEC
           END-IF
      *
           .
      *
       4100-BTUSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÁªÂòÈÖ¹æ
      *****************************************************************
       41001-BTUSELNUM-CHK-SEC          SECTION.
      *
      *    ÆþÎÏ¹àÌÜÎÎ°èÊÔ½¸½èÍý
           MOVE    SPA-GMN-W20-BTUSELNUM
                                       TO  WRK-BTUNUM
           MOVE    SPA-NAI-W20-KJNYMD  TO  WRK-KIJUN-YMD
      *
      *    ²èÌÌ½é´ü²½
           PERFORM 420-CLEAR-SEC
      *
           MOVE    WRK-BTUNUM          TO  SPA-GMN-W20-BTUSELNUM
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           IF    ( FLG-SYS5001      =   ZERO )
               PERFORM 4100-BTUNUM-LSTSEL-SEC
               MOVE    IDX-BTULST   TO  SPA-GMN-W20-BTULST-SEL
      *
      *        ÍúÎò¥Ç¡¼¥¿ÊÔ½¸½èÍý
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
      *
               MOVE    1               TO  SPA-GMN-W20-CUR
      *
           ELSE
               MOVE    "0015"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       41001-BTUSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÁªÂòÈÖ¹æ
      *****************************************************************
       4101-BTUSELNUM-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-W20-BTUSELNUM   NOT =   ZERO )
            AND  ( SPA-GMN-W20-BTULST-SEL      =   ZERO )
               MOVE    "0015"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-BTUSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¼ïÊÌ
      *****************************************************************
       4100-SBT-CHK-SEC                SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-SBT         TO  SPA-GMN-W20-SBT-G
      *
           IF    ( SPA-GMN-W20-SBT-G   =   SPACE )
      *
               INITIALIZE  SPA-NAI-W20-KHNRYOLST-G
                           SPA-GMN-W20-KHNRYOLST-G
                           SPA-GMN-W20-YKNCHOKAKBNLST-G
                           SPA-GMN-W20-RYOYOKBNLST-G
      *
               INITIALIZE  SPA-GMN-W20-KHNRYO-G
                           SPA-NAI-W20-KHNRYO-G
                           SPA-GMN-W20-RECEDSPKBN-G
                           SPA-GMN-W20-BTUTYPE-G
                           SPA-GMN-W20-BTUCD
                           SPA-GMN-W20-TBTUTYPE-G
                           SPA-GMN-W20-TBTUCD
                           SPA-GMN-W20-YKNCHOKAKBN-G
                           SPA-GMN-W20-RYOYOKBN-G
                           SPA-NAI-W20-SBT-MOT
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               PERFORM 4201-KSN-AREA-CLEAR-SEC
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           PERFORM 4101-SBT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   SPA-NAI-W20-SBT-MOT )
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   CONST-KANSATU )
            OR   ( SPA-NAI-W20-SBT-MOT =   CONST-KANSATU )
            OR   ( SPA-NAI-W20-SBT-MOT =   SPACE )
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT     TO  SPA-NAI-W20-SBT-MOT
      *
           INITIALIZE      SPA-NAI-W20-KHNRYOLST-G
                           SPA-GMN-W20-KHNRYOLST-G
                           SPA-GMN-W20-YKNCHOKAKBNLST-G
                           SPA-GMN-W20-RYOYOKBNLST-G
      *
           INITIALIZE      SPA-GMN-W20-KHNRYO-G
                           SPA-NAI-W20-KHNRYO-G
                           SPA-GMN-W20-YKNCHOKAKBN-G
                           SPA-GMN-W20-RECEDSPKBN-G
                           SPA-GMN-W20-BTUTYPE-G
                           SPA-GMN-W20-BTUCD
                           SPA-GMN-W20-TBTUTYPE-G
                           SPA-GMN-W20-TBTUCD
                           SPA-GMN-W20-RYOYOKBN-G
      *
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                   MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                       TO  SPA-GMN-W20-YKNKNG-G
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H240401 )
               IF    ( SPA-GMN-W20-SBT     =   "08" OR "09" )
                   MOVE  SPA-GMN-W20-RYOYOKBNLST (1)
                                       TO  SPA-GMN-W20-RYOYOKBN-G
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT     TO  WRK-CMB-CD
           PERFORM 4201-RECEDSPKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-RECEDSPKBN-G
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           .
      *
       4100-SBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÈÖ¹æ
      *****************************************************************
       4101-BTUNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W20-BTULST-MAX )
                    OR   ( IDX-LST >   ZERO )
      *
               IF    ( SPA-GMN-W20-TBTUNUM (IDX1)
                                   =   SPA-GMN-W20-BTUNUM )
                   MOVE    IDX1    TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-LST >       ZERO )
               MOVE    "0016"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-W20-CUR
           END-IF
      *
           .
      *
       4101-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÌ¾¾Î
      *****************************************************************
       4101-NAME-CHK-SEC               SECTION.
      *
      *    Á´³ÑÈ¾³Ñ¥Á¥§¥Ã¥¯
           MOVE    SPA-GMN-W20-NAME
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-W20-NAME
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    2           TO  SPA-GMN-W20-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ã»½ÌÉÂÅïÌ¾
      *****************************************************************
       4101-TANNAME-CHK-SEC        SECTION.
      *
      *    Á´³ÑÈ¾³Ñ¥Á¥§¥Ã¥¯
           MOVE    SPA-GMN-W20-TANNAME
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-W20-TANNAME
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    3           TO  SPA-GMN-W20-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-TANNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ
      *****************************************************************
       4101-TOKUTEINYUIN-CHK-SEC   SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-W20-TOKNYUIN
                                       SPA-NAI-W20-TOKSRYCD
      *
           IF    ( SPA-GMN-W20-TOKNYUIN-G  =   SPACE )
               GO  TO  4101-TOKUTEINYUIN-CHK-EXT
           END-IF
      *
      *    ³ºÅö¥³¡¼¥É¤Î¥Ç¡¼¥¿¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           MOVE    SPA-GMN-W20-TOKNYUIN    TO  WRK-CMB-CD
           PERFORM 4200-TOKNYUINLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    4           TO  SPA-GMN-W20-CUR
               MOVE    "0004"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKNYUIN-G
               MOVE    WRK-CMB-ITM3
                                   TO  SPA-NAI-W20-TOKSRYCD
               MOVE    WRK-CMB-ITM4
                                   TO  SPA-NAI-W20-TOKNYUIN
           END-IF
      *
           .
      *
       4101-TOKUTEINYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï²ÊÌ¾
      *****************************************************************
       4101-SRYKA-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W20-SRYKA-G =   SPACE )
               GO  TO  4101-SRYKA-CHK-EXT
           END-IF
      *
      *    ³ºÅö¥³¡¼¥É¤Î¥Ç¡¼¥¿¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           MOVE    SPA-GMN-W20-SRYKA   TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    5           TO  SPA-GMN-W20-CUR
               MOVE    "0005"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SRYKA-G
           END-IF
      *
           .
      *
       4101-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¼ïÊÌ
      *****************************************************************
       4101-SBT-CHK-SEC                SECTION.
      *
           IF    ( SPA-GMN-W20-SBT-G   =   SPACE )
               GO  TO  4101-SBT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    6           TO  SPA-GMN-W20-CUR
               MOVE    "0006"      TO  SPA-ERRCD
               GO  TO  4101-SBT-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SBT-G
           END-IF
      *
           .
      *
       4101-SBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       4100-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE  SPA-NAI-W20-KHNRYOLST-G
                       SPA-GMN-W20-KHNRYOLST-G
                       SPA-GMN-W20-YKNCHOKAKBNLST-G
                       SPA-GMN-W20-RYOYOKBNLST-G
      *
           MOVE    ZERO            TO  IDX-CMB
      *
           IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               CONTINUE
           ELSE
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
                AND  ( SPA-GMN-W20-SBT         =   "08" )
                   PERFORM 41001-KHNRYO-CMB-EDT-SEC
               ELSE
                   PERFORM 41002-KHNRYO-CMB-EDT-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H260401 )
            AND  ( SPA-NAI-W20-YKNCHOKACHK-G NOT =  SPACE )
               PERFORM 4100-YKNCHOKAKBN-CMB-EDT-SEC
           END-IF
      *
           PERFORM 42011-RYOYOSANTEI-CHK-SEC
           IF    ( FLG-RYOYOSANTEI =   1 )
               PERFORM 4100-RYOYOKBN-CMB-EDT-SEC
           END-IF
      *
           .
      *
       4100-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       41001-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H260401 )
      *
               MOVE    "190169610"     TO  WRK-REALCD
               MOVE    "099999955"     TO  WRK-DUMMYCD
               PERFORM 410012-KHNRYO-CMB-EDT-SEC
      *
               MOVE    "190169910"     TO  WRK-REALCD
               MOVE    "099999956"     TO  WRK-DUMMYCD
               PERFORM 410012-KHNRYO-CMB-EDT-SEC
      *
               MOVE    "190170210"     TO  WRK-REALCD
               MOVE    "099999957"     TO  WRK-DUMMYCD
               PERFORM 410012-KHNRYO-CMB-EDT-SEC
           END-IF
      *
           MOVE    "190097010"         TO  WRK-REALCD
           MOVE    "099999950"         TO  WRK-DUMMYCD
           PERFORM 410012-KHNRYO-CMB-EDT-SEC
      *
           MOVE    "190097210"         TO  WRK-REALCD
           MOVE    "099999951"         TO  WRK-DUMMYCD
           PERFORM 410012-KHNRYO-CMB-EDT-SEC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
      *
               MOVE    "190135110"     TO  WRK-REALCD
               MOVE    "099999952"     TO  WRK-DUMMYCD
               PERFORM 410012-KHNRYO-CMB-EDT-SEC
           END-IF
      *
           .
      *
       41001-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       410012-KHNRYO-CMB-EDT-SEC       SECTION.
      *
           MOVE    WRK-REALCD      TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-TEN         TO  WRK-TEN
      *
           MOVE    WRK-DUMMYCD     TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-TNS-KJNYMD
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       =   ZERO )
               MOVE    WRK-TEN     TO  TNS-TEN
               PERFORM 410011-KHNRYO-CMB-SET-SEC
           END-IF
      *
           .
       410012-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       410011-KHNRYO-CMB-SET-SEC       SECTION.
      *
           COMPUTE IDX-CMB     =   IDX-CMB    +   1
           MOVE    IDX-CMB     TO  SPA-GMN-W20-KHNRYO-MAX
      *
           MOVE    TNS-SRYCD   TO  SPA-NAI-W20-KHNSRYCDLST
                                               (IDX-CMB)
      *
           MOVE    TNS-NYUKHNKBN
                               TO  SPA-NAI-W20-NYUKHNKBNLST
                                               (IDX-CMB)
      *
           PERFORM 4100111-KHNRYO-CMB-SET-SEC
      *
           MOVE    IDX-CMB     TO  WRK-99
           MOVE    WRK-99      TO  SPA-GMN-W20-KHNRYOL
                                                   (IDX-CMB)
           MOVE    TNS-NAME    TO  SPA-GMN-W20-KHNRYOMEIL
                                                   (IDX-CMB)
           MOVE    TNS-TEN     TO  SPA-NAI-W20-KHNTENLST
                                                   (IDX-CMB)
           IF    ( TNS-CDKBN-KBN       =    "A" )
            AND  ( TNS-CDKBN-KBNNUM    =    "104" )
               MOVE    1       TO  SPA-NAI-W20-TOKHOSPLST
                                                   (IDX-CMB)
           END-IF
      *
           .
       410011-KHNRYO-CMB-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       4100111-KHNRYO-CMB-SET-SEC       SECTION.
      *
           IF    ( TNS-CDKBN-KBNNUM   =   100 )
            OR   ( TNS-CDKBN-KBNNUM   =   101 AND TNS-CDKBN-KOUBAN = 2 
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  <  CONST-H300401 )
            OR   ( TNS-CDKBN-KBNNUM   =   102 )
            OR   ( TNS-CDKBN-KBNNUM   =   103 )
            OR   ( TNS-CDKBN-KBNNUM   =   106 )
            OR   ( TNS-SRYCD          =   "099999954"
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  <  CONST-H300401 )
               MOVE    "1"     TO  SPA-NAI-W20-YKNCHOKAL
                                               (IDX-CMB)
           END-IF
      *
           IF    ( TNS-CDKBN-KBNNUM   =   100 )
            OR   ( TNS-CDKBN-KBNNUM   =   101 AND TNS-CDKBN-KOUBAN = 2 
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  <  CONST-H300401 )
            OR   ( TNS-CDKBN-KBNNUM   =   102 )
            OR   ( TNS-CDKBN-KBNNUM   =   103 )
            OR   ( TNS-SRYCD          =   "099999954"
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  <  CONST-H300401 )
               MOVE    "1"     TO  SPA-NAI-W20-YKNTOKUBETSUL
                                               (IDX-CMB)
           END-IF
      *
           IF    ( TNS-CDKBN-KBNNUM   =   101 AND TNS-CDKBN-KOUBAN = 2 )
            OR   ( TNS-SRYCD          =   "099999954" )
               MOVE    "1"     TO  SPA-NAI-W20-KNGKEIKASOCHIL
                                               (IDX-CMB)
           END-IF
      *
           IF    ( TNS-CDKBN-KBNNUM   =   101 AND TNS-CDKBN-KOUBAN = 2
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  >=  CONST-H300401 )
            OR   ( TNS-SRYCD          =   "099999954"
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  >=  CONST-H300401 )
               MOVE    "1"     TO  SPA-NAI-W20-SYOKUINKBN2L
                                               (IDX-CMB)
           END-IF
      *
           IF    ( TNS-CDKBN-KBNNUM   =   102 AND TNS-CDKBN-KOUBAN = 1
                                              AND SPA-NAI-W20-YUKOSTYMD
                                                  >=  CONST-H300401 )
               MOVE    "1"     TO  SPA-NAI-W20-JUUSHOUKNGKBNL
                                               (IDX-CMB)
           END-IF
      *
           .
       4100111-KHNRYO-CMB-SET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       41002-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
             AND ( SPA-GMN-W20-SBT         =   "04" )
               MOVE    "099999953"             TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
               MOVE    "099999954"             TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý
           PERFORM 900-TENSU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                OR       ( IDX-CMB        >=   CONST-KHNRYO-MAX  )
      *
               MOVE    1           TO  FLG-SETKHNRYO
      *
      *        ÁªÄêÆþ±¡ÎÁ¤ÏÆÉ¤ßÈô¤Ð¤¹
               MOVE    ZERO        TO  CNT-STR
               INSPECT TNS-NAME    TALLYING   CNT-STR
                       FOR     ALL "¡ÊÁª¡Ë",
                               ALL "¡ÊÁªÄêÎÅÍÜ¡Ë" ,
                               ALL "·Ð²áÁ¼ÃÖ" ,
                               ALL "ÎÅÍÜÉÂ¾²°Ê³°"
                               ALL "ÆÃÄêÆþ±¡´ðËÜÎÁ"
                               ALL "Ìë¶Ð»þ´ÖÄ¶²á¸º»»"
                               ALL "ÎÅÍÜÉÂ¾²¡¦"
                               ALL "Ìë¶Ð»þ´ÖÆÃÊÌ"
                               ALL "´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ"
                               ALL "°åÎÅ¶èÊ¬"
                               ALL "½Å¾É´µ¼Ô³ä¹ç"
                               ALL "ÉÂÅï¡¦"
      * 
               IF    ( CNT-STR     >   ZERO )
                   MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
               IF    ( TNS-ROUTEKKBN       =   "2" )
                   MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
      *        Ê¿À®£²£¶Ç¯£´·î°Ê¹ß
      *        °ìÈÌÉÂÅï¡¢ÆÃÄêµ¡Ç½ÉÂ±¡¡Ê°ìÈÌÉÂÅï¡Ë¡¢ÀìÌçÉÂ±¡
      *        £·ÂÐ£±¡Ê·Ð²áÁ¼ÃÖ¡Ë
               IF    ( SPA-NAI-W20-YUKOSTYMD
                                      >=   CONST-H260401 )
                   IF    ( TNS-SRYCD   =    "190700110"
                                        OR  "190700610"
                                        OR  "190700710"
                                        OR  "190752730"
                                        OR  "190752930"
                                        OR  "190705610"
                                        OR  "190705910"
                                        OR  "190753430"
                                        OR  "190753630"
                                        OR  "190706810"
                                        OR  "190707210"
                                        OR  "190753830"
                                        OR  "190754030" )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-W20-SBT =   "04"  OR "09" )
                AND  ( SPA-NAI-W20-YUKOSTYMD
                                      >=   CONST-H180701 )
                   IF    ( TNS-SRYCD   =   "190081310"
                                        OR "190081410"
                                        OR "190080610"
                                        OR "190080710"
                                        OR "190098210"
                                        OR "190098310"
      *°Ê²¼À¸³èÎÅÍÜ¥³¡¼¥É
                                        OR "190123710"
                                        OR "190123810"
                                        OR "190123910"
                                        OR "190124010"
                                        OR "190124110"
                                        OR "190124210"
                                        OR "190124310"
                                        OR "190124410"
                                        OR "190124510"
                                        OR "190124610"
                                        OR "190124710"
                                        OR "190124810"
                                        OR "190131710"
                                        OR "190131910"
                                        OR "190132110"
                                        OR "190132310"
                                        OR "190132510"
                                        OR "190132710"
                                        OR "190132910"
                                        OR "190133110"
                                        OR "190133310"
                                        OR "190133510"
                                        OR "190133710"
                                        OR "190133910"
                                        OR "190134110"
      *
                                                        )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
                   END-IF
               END-IF
      *
               IF    ( TNS-SRYCD   =   "190799410"
                                    OR "190799510"
                                    OR "190161210"
                                    OR "190161310"
                                    OR "190161010"
                                    OR "190161110"
                                    OR "190160810"
                                    OR "190160910"
                                                   )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD
                                          <   CONST-H300401 )
                   IF    ( TNS-NYUKHNKSNKBN   =   196     )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
                   END-IF
               END-IF
      *
               IF    ( FLG-SETKHNRYO   =   1 )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
      *
      *        ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¹þ½èÍý
               PERFORM 900-TENSU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       41002-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ¶èÊ¬¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       4100-RYOYOKBN-CMB-EDT-SEC       SECTION.
      *
           IF    ( SPA-GMN-W20-SBT     =   "08" OR "09" )
               EVALUATE    SPA-GMN-W20-SBT
               WHEN    "08"
                   MOVE    "0"    TO  SPA-GMN-W20-RYOYOKBNL  (1)
                   MOVE    "Áê¸ß»»Äê¤·¤Ê¤¤"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (1)
      *
                   MOVE    "1"    TO  SPA-GMN-W20-RYOYOKBNL  (2)
                   MOVE    "Áê¸ß»»Äê¤¹¤ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (2)
                   MOVE    2      TO  SPA-GMN-W20-RYOYOKBN-MAX
               WHEN    "09"
                   EVALUATE    TRUE
                   WHEN    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H260401 )
                       MOVE    "0"    TO  SPA-GMN-W20-RYOYOKBNL (1)
                       MOVE    "Áê¸ß»»Äê¤·¤Ê¤¤"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (1)
      *
                       MOVE    "1"    TO  SPA-GMN-W20-RYOYOKBNL (2)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£±¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (2)
      *
                       MOVE    "2"    TO  SPA-GMN-W20-RYOYOKBNL (3)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£²¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (3)
      *
                       MOVE    "3"    TO  SPA-GMN-W20-RYOYOKBNL (4)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£³¡Ë" 
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (4)
      *
                       MOVE    "4"    TO  SPA-GMN-W20-RYOYOKBNL (5)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£´¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (5)
      *
                       MOVE    "5"    TO  SPA-GMN-W20-RYOYOKBNL (6)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£µ¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (6)
      *
                       MOVE    "6"    TO  SPA-GMN-W20-RYOYOKBNL (7)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£¶¡Ë" 
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (7)
                       MOVE    7      TO  SPA-GMN-W20-RYOYOKBN-MAX
                   WHEN    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H240401 )
                       MOVE    "0"    TO  SPA-GMN-W20-RYOYOKBNL (1)
                       MOVE    "Áê¸ß»»Äê¤·¤Ê¤¤"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (1)
      *
                       MOVE    "1"    TO  SPA-GMN-W20-RYOYOKBNL (2)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£±¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (2)
      *
                       MOVE    "2"    TO  SPA-GMN-W20-RYOYOKBNL (3)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£²¡Ë"
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (3)
      *
                       MOVE    "3"    TO  SPA-GMN-W20-RYOYOKBNL (4)
                       MOVE    "Áê¸ß»»Äê¤¹¤ë¡Ê¿Ç£³¡Ë" 
                                  TO  SPA-GMN-W20-RYOYOKBNMEIL (4)
      *
                       MOVE    4      TO  SPA-GMN-W20-RYOYOKBN-MAX
                   END-EVALUATE
               END-EVALUATE
           ELSE
      *
               INITIALIZE                  SPA-GMN-W20-RYOYOKBNLST-G
      *
               MOVE    "0"            TO   SPA-GMN-W20-RYOYOKBNL  (1)
               MOVE    "°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¤ò»»Äê"
                                      TO   SPA-GMN-W20-RYOYOKBNMEIL (1)
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H300401 )
                   MOVE    "1"    TO  SPA-GMN-W20-RYOYOKBNL  (2)
                   MOVE    "ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£±¤ÎÎã¤Ë¤è¤ê»»Äê"
                                      TO   SPA-GMN-W20-RYOYOKBNMEIL (2)
               ELSE
                   MOVE    "1"    TO  SPA-GMN-W20-RYOYOKBNL  (2)
                   MOVE    "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¤ÎÎã¤Ë¤è¤ê»»Äê"
                                      TO   SPA-GMN-W20-RYOYOKBNMEIL (2)
               END-IF
               MOVE    2              TO   SPA-GMN-W20-RYOYOKBN-MAX
      *
           END-IF
      *
           MOVE    SPA-GMN-W20-RYOYOKBNLST (1)
                                      TO   SPA-GMN-W20-RYOYOKBN-G
      *
           .
       4100-RYOYOKBN-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬¥³¥ó¥ÜÊÔ½¸½èÍý
      *****************************************************************
       4100-YKNCHOKAKBN-CMB-EDT-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-W20-YKNCHOKAKBNLST-G
      *
           MOVE    1          TO   IDX-CMB
           MOVE    "0"        TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
           MOVE    "³ºÅö¤·¤Ê¤¤"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
      *
           IF    ( SPA-NAI-W20-YKNCHOKA    =   "1" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H260401 )
      *
               COMPUTE IDX-CMB =  IDX-CMB  +   1
      *
               MOVE    "1"    TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
               MOVE    "·îÊ¿¶ÑÌë¶Ð»þ´ÖÄ¶²á¸º»»¤Ë¤è¤ë»»Äê"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
           END-IF
      *
           IF    ( SPA-NAI-W20-YKNTOKUBETSU    =   "1" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H280401 )
      *
               COMPUTE IDX-CMB =  IDX-CMB  +   1
      *
               MOVE    "2"    TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
               MOVE    "Ìë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¤Ë¤è¤ë»»Äê"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
           END-IF
      *
           IF    ( SPA-NAI-W20-KNGKEIKASOCHI   =   "1" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H280401 )
      *
               COMPUTE IDX-CMB =  IDX-CMB  +   1
      *
               MOVE    "3"    TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
               MOVE    "´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¤Ë¤è¤ë»»Äê"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
           END-IF
      *
           IF    ( SPA-NAI-W20-SYOKUINKBN2     =   "1" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  <    CONST-R020401 )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H300401 )
      *
               COMPUTE IDX-CMB =  IDX-CMB  +   1
      *
               MOVE    "4"    TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
               MOVE    "´Ç¸îÈæÎ¨£³£°ÂÐ£±Åù·Ð²áÁ¼ÃÖ¤Ë¤è¤ë»»Äê"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
           END-IF
      *
           IF    ( SPA-NAI-W20-JUUSHOUKNGKBN   =   "1" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H300401 )
      *
               COMPUTE IDX-CMB =  IDX-CMB  +   1
      *
               MOVE    "5"    TO   SPA-GMN-W20-YKNCHOKAKBNL(IDX-CMB)
               MOVE    "½Å¾É´µ¼Ô³ä¹çÆÃÊÌÆþ±¡´ðËÜÎÁ¤Ë¤è¤ë»»Äê"
                              TO   SPA-GMN-W20-YKNCHOKAKBNMEIL (IDX-CMB)
           END-IF
      *
           MOVE    IDX-CMB    TO   SPA-GMN-W20-YKNCHOKAKBN-MAX
      *
           .
       4100-YKNCHOKAKBN-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-YKNCHOKAKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-W20-YKNCHOKAKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-YKNCHOKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-YKNCHOKAKBN-G
      *
           .
      *
       4101-YKNCHOKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
      *****************************************************************
       4101-RECEDSPKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-W20-RECEDSPKBN-G
                                   =   SPACE )
               GO  TO  4101-RECEDSPKBN-CHK-EXT
      *
           END-IF
      *
      *    ³ºÅö¥³¡¼¥É¤Î¥Ç¡¼¥¿¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
           MOVE    SPA-GMN-W20-RECEDSPKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-RECEDSPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    8           TO  SPA-GMN-W20-CUR
               MOVE    "0009"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-RECEDSPKBN-G
           END-IF
      *
           .
      *
       4101-RECEDSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-BTUTYPE-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-W20-BTUTYPE-G   =   SPACE )
               MOVE    ZERO            TO   SPA-GMN-W20-BTUCD
           ELSE
      *
      *        ³ºÅö¥³¡¼¥É¤Î¥Ç¡¼¥¿¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
               MOVE    SPA-GMN-W20-BTUTYPE
                                   TO  WRK-CMB-CD
               PERFORM 4200-BTUTYPELST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    57          TO  SPA-GMN-W20-CUR
                   MOVE    "0017"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-BTUTYPE-G
               END-IF
      *
           END-IF
      *
           .
      *
       4101-BTUTYPE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-BTUCD-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-W20-BTUCD   =   ZERO )
               CONTINUE
           ELSE
      *
               IF    ( SPA-GMN-W20-BTUCD   >=  1 )
                AND  ( SPA-GMN-W20-BTUCD   <=  50 )
                   CONTINUE
               ELSE
                   MOVE    58          TO  SPA-GMN-W20-CUR
                   MOVE    "0018"      TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
      *
       4101-BTUCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥³¡¼¥É¡ÊÉÂÅïµ¡Ç½¡Ë¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-TBTUTYPE-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W20-TBTUTYPE-G   =   SPACE )
               MOVE    ZERO            TO   SPA-GMN-W20-TBTUCD
           ELSE
      *
      *        ³ºÅö¥³¡¼¥É¤Î¥Ç¡¼¥¿¤¬Â¸ºß¤¹¤ë¤«¥Á¥§¥Ã¥¯
               MOVE    SPA-GMN-W20-TBTUTYPE
                                   TO  WRK-CMB-CD
               PERFORM 4200-BTUTYPELST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    59          TO  SPA-GMN-W20-CUR
                   MOVE    "0017"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TBTUTYPE-G
               END-IF
      *
           END-IF
      *
           .
      *
       4101-TBTUTYPE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥³¡¼¥É¡ÊÏ¢ÈÖ¡Ë¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-TBTUCD-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W20-TBTUCD   =   ZERO )
               CONTINUE
           ELSE
      *
               IF    ( SPA-GMN-W20-TBTUCD   >=  1 )
                AND  ( SPA-GMN-W20-TBTUCD   <=  50 )
                   CONTINUE
               ELSE
                   MOVE    60          TO  SPA-GMN-W20-CUR
                   MOVE    "0018"      TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
      *
       4101-TBTUCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101-RYOYOKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-W20-RYOYOKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-RYOYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-RYOYOKBN-G
      *
           .
      *
       4101-RYOYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Í­¸ú´ü´Ö³«»ÏÆü
      *****************************************************************
       4101-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-YUKOSTYMD
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD-MOT   =   SPACE )
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD-MOT
           END-IF
      *
           MOVE    SPACE           TO  SPA-NAI-W20-YUKOSTYMD
      *
      *    ÆüÉÕÂÅÅöÀ­¥Á¥§¥Ã¥¯¡õÊÔ½¸½èÍý
           IF    ( SPA-GMN-W20-YUKOSTYMD   =   SPACE )
               MOVE    SPA-GMN-W20-KJNYMD (1 : 6)
                                       TO  SPA-GMN-W20-YUKOSTYMD (1 : 6)
               MOVE    ". 1"           TO  SPA-GMN-W20-YUKOSTYMD (7 : 3)
               MOVE    SPA-NAI-W20-KJNYMD   (1 : 6)
                                       TO  SPA-NAI-W20-YUKOSTYMD (1 : 6)
               MOVE    "01"            TO  SPA-NAI-W20-YUKOSTYMD (7 : 2)
      *
      *
      *        ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóºÆÀßÄê
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               MOVE    SPACE           TO  SPA-NAI-W20-YUKOSTYMD-MOT
      *
           ELSE
               MOVE    SPA-GMN-W20-YUKOSTYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               IF  ( FLG-HIZUKE-ERR    =   ZERO )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOSTYMD
                   MOVE    "0010"      TO  SPA-ERRCD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-IF
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD   <   "20020401" )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    "H14. 4. 1" TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    "20020401"  TO  SPA-NAI-W20-YUKOSTYMD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD
                               NOT =   SPA-NAI-W20-YUKOSTYMD-MOT )
      *        ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóºÆÀßÄê
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
           END-IF
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD-MOT   TO  WRK-STYMD
           MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-EDYMD
           IF    ( SPA-NAI-W20-YUKOSTYMD-MOT
                               >    SPA-NAI-W20-YUKOSTYMD )
               MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-STYMD
               MOVE    SPA-NAI-W20-YUKOSTYMD-MOT   TO  WRK-EDYMD
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( WRK-EDYMD                  >=   CONST-H180401 )
             AND ( WRK-STYMD                  <    CONST-H180401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H200401 )
             AND ( WRK-STYMD                  <    CONST-H200401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H200701 )
             AND ( WRK-STYMD                   <   CONST-H200701 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H220401 )
             AND ( WRK-STYMD                   <   CONST-H220401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H240401 )
             AND ( WRK-STYMD                   <   CONST-H240401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H241001 )
             AND ( WRK-STYMD                   <   CONST-H241001 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H260401 )
             AND ( WRK-STYMD                   <   CONST-H260401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H261001 )
             AND ( WRK-STYMD                   <   CONST-H261001 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H280401 )
             AND ( WRK-STYMD                   <   CONST-H280401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H300401 )
             AND ( WRK-STYMD                   <   CONST-H300401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-R020401 )
             AND ( WRK-STYMD                   <   CONST-R020401 )
                   PERFORM 4101-NYUINRYO-CLEAR-SEC
           END-EVALUATE
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD   <   CONST-H300401 )
               INITIALIZE              SPA-GMN-W20-YKTTYMD-G
                                       SPA-NAI-W20-YKTTYMD-G
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                   MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                           TO  SPA-GMN-W20-YKNKNG-G
               END-IF
      *
           END-IF
      *
           IF    ( WRK-EDYMD      >=   CONST-H180701 )
               IF    ( WRK-STYMD   <   CONST-H180701 )
                   IF    ( SPA-GMN-W20-SBT =   "04" OR "09" )
                       PERFORM 4101-NYUINRYO-CLEAR-SEC
                   END-IF
      *
               END-IF
      *
               MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                           TO  SPA-GMN-W20-YKNKNG-G
           END-IF
      *
           INITIALIZE                     SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           IF    ( FLG-SYSKANRI       =   ZERO )
               EVALUATE    TRUE
               WHEN  ( WRK-EDYMD          >=   CONST-H220401 )
                 AND ( WRK-STYMD          <    CONST-H220401 )
               WHEN  ( WRK-EDYMD          >=   CONST-H240401 )
                 AND ( WRK-STYMD          <    CONST-H240401 )
               WHEN  ( WRK-EDYMD          >=   CONST-H241001 )
                 AND ( WRK-STYMD          <    CONST-H241001 )
               WHEN  ( WRK-EDYMD          >=   CONST-H260401 )
                 AND ( WRK-STYMD          <    CONST-H260401 )
               WHEN  ( WRK-EDYMD          >=   CONST-H261001 )
                 AND ( WRK-STYMD          <    CONST-H261001 )
               WHEN  ( WRK-EDYMD          >=   CONST-H280401 )
                 AND ( WRK-STYMD          <    CONST-H280401 )
               WHEN  ( WRK-EDYMD          >=   CONST-H300401 )
                 AND ( WRK-STYMD          <    CONST-H300401 )
               WHEN  ( WRK-EDYMD          >=   CONST-R020401 )
                 AND ( WRK-STYMD          <    CONST-R020401 )
                   PERFORM 41001-INPUT-AREA-EDIT-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD-MOT
      *
           .
      *
       4101-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ½é´ü²½½èÍý
      *****************************************************************
       4101-NYUINRYO-CLEAR-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-W20-KHNRYO-G
                                       SPA-NAI-W20-KHNRYO-G
                                       SPA-GMN-W20-YKNCHOKAKBN-G
                                       SPA-GMN-W20-RYOYOKBN-G
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           .
       4101-NYUINRYO-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    Í­¸ú´ü´Ö½ªÎ»Æü
      *****************************************************************
       4101-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-W20-YUKOEDYMD
                                   TO  WRK-YUKOEDYMD
           MOVE    SPACE           TO  SPA-NAI-W20-YUKOEDYMD
      *
      *    ÆüÉÕÂÅÅöÀ­¥Á¥§¥Ã¥¯¡õÊÔ½¸½èÍý
           IF    ( SPA-GMN-W20-YUKOEDYMD   =   SPACE )
               MOVE    "99999999"  TO  SPA-GMN-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD
               GO  TO  4101-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-YUKOEDYMD
                                   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF  ( FLG-HIZUKE-ERR    =   ZERO )
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W20-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOEDYMD
           ELSE
               MOVE    WRK-YUKOEDYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W20-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOEDYMD
               MOVE    "0011"      TO  SPA-ERRCD
               MOVE    52          TO  SPA-GMN-W20-CUR
               GO  TO  4101-YUKOEDYMD-CHK-EXT
           END-IF
      *
           .
      *
       4101-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï½ªÎ»¶èÊ¬
      *****************************************************************
       4101-EDKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-EDKBN   TO  WRK-CMB-CD
           PERFORM 4200-EDKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-EDKBNLST (1)
                                       TO  SPA-GMN-W20-EDKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-EDKBN-G
           END-IF
      *
           .
      *
       4101-EDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ
      *****************************************************************
       4100-KHNRYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    SPA-NAI-W20-KHNSRYCD
                                   TO  WRK-SRYCD
      *
           INITIALIZE              SPA-NAI-W20-KHNRYO-G
      *
           MOVE    W20-KHNRYO      TO  SPA-GMN-W20-KHNRYO-G
      *
      *    Æþ±¡´ðËÜÎÁ¥Á¥§¥Ã¥¯
           PERFORM 4101-KHNRYO-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-W20-KHNSRYCD
                               NOT =   WRK-SRYCD )
                   INITIALIZE      SPA-GMN-W20-RYOYOKBNLST-G
                                   SPA-GMN-W20-RYOYOKBN-G
      *
                   PERFORM 42011-RYOYOSANTEI-CHK-SEC
                   IF    ( FLG-RYOYOSANTEI =   1 )
                       PERFORM 4100-RYOYOKBN-CMB-EDT-SEC
                   END-IF
      *
                   INITIALIZE      SPA-GMN-W20-YKNCHOKAKBNLST-G
                                   SPA-GMN-W20-YKNCHOKAKBN-G
                   IF    ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H260401 )
                    AND  ( SPA-NAI-W20-YKNCHOKACHK-G NOT =  SPACE )
                       PERFORM 4100-YKNCHOKAKBN-CMB-EDT-SEC
                       MOVE    SPA-GMN-W20-YKNCHOKAKBNLST (1)
                                       TO  SPA-GMN-W20-YKNCHOKAKBN-G
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4100-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ðËÜÎÁ
      *****************************************************************
       4101-KHNRYO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W20-KHNRYO  =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-W20-KHNRYO  TO  WRK-CMB-CD
               PERFORM 4200-KHNRYOLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0012"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-W20-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KHNRYO-G
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W20-KHNSRYCD
                   MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W20-KHNTEN
                   MOVE    WRK-CMB-ITM4    TO  SPA-NAI-W20-NYUKHNKBN
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W20-YKNCHOKACHK-G
                   MOVE    WRK-CMB-ITM6    TO  SPA-NAI-W20-TOKHOSP
               END-IF
           END-IF
      *
           .
      *
       4101-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õ²Ã»»¥Á¥§¥Ã¥¯
      *****************************************************************
       4101-KNGHJO-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-W20-KNGHJO  TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                       TO  SPA-GMN-W20-KNGHJO-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KNGHJO-G
           END-IF
      *
           .
      *
       4101-KNGHJO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»
      *****************************************************************
       4101-SYOKUDO-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-W20-SYOKUDO TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           IF    ( WRK-CMB-CD          =   SPACE )
               MOVE    SPA-GMN-W20-SYOKUDOLST (1)
                                       TO  SPA-GMN-W20-SYOKUDO-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-SYOKUDO-G
           END-IF
      *
           .
      *
       4101-SYOKUDO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ì¤´ð½à¡ÊÉÂÅï¤Ë·¸¤ë´ð½à¤ò¤ß¤¿¤µ¤Ê¤¤¾ì¹ç¡Ë
      *****************************************************************
       4101-BTUMIKJN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-W20-BTUMIKJN    TO  WRK-CMB-CD
           PERFORM 4200-BTUMIKJNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-W20-BTUMIKJNLST (1)
                                   TO  SPA-GMN-W20-BTUMIKJN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-BTUMIKJN-G
           END-IF
      *
           .
      *
       4101-BTUMIKJN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»
      *****************************************************************
       4101-SYOUBTUBRM-CHK-SEC         SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-W20-SYOUBTUBRMCD
           MOVE    SPA-GMN-W20-SYOUBTUBRM    TO  WRK-CMB-CD
           PERFORM 4200-SYOUBTUBRMLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                   TO  SPA-GMN-W20-SYOUBTUBRM-G
           ELSE
               MOVE    WRK-CMB-ITM  TO SPA-GMN-W20-SYOUBTUBRM-G
               MOVE    WRK-CMB-ITM2 TO SPA-NAI-W20-SYOUBTUBRMCD
           END-IF
      *
           .
      *
       4101-SYOUBTUBRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¥Á¥§¥Ã¥¯
      *****************************************************************
       4101-YKTTYMD-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-YKTTYMD-MAX )
      *
               MOVE    SPACE           TO  SPA-NAI-W20-YKTTYMD (IDXA)
               IF    ( SPA-GMN-W20-YKTTYMD (IDXA)
                                   NOT =   SPACE )
                   MOVE    SPA-GMN-W20-YKTTYMD (IDXA)
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   IF  ( FLG-HIZUKE-ERR    =  ZERO )
                       MOVE    WRK-HENYMDG TO SPA-GMN-W20-YKTTYMD (IDXA)
                       MOVE    WRK-SYMD    TO SPA-NAI-W20-YKTTYMD (IDXA)
                   ELSE
                       MOVE    "1005"      TO  SPA-ERRCD
                       COMPUTE SPA-GMN-W20-CUR = IDXA + 900
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4101-YKTTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ð½àÆü
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-W20-CUR
           MOVE    W20-KJNYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF  ( FLG-HIZUKE-ERR    =   ZERO )
      *
               INITIALIZE              SPA-W20-AREA
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-W20-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    101         TO  SPA-GMN-W20-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÁªÂòÈÖ¹æ
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-SELNUM      TO  SPA-GMN-W20-SELNUM
      *
           IF  (   SPA-GMN-W20-SELNUM  =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
      *    ÈÏ°ÏÆâ¤ÎÈÖ¹æ¤¬ÁªÂò¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-GMN-W20-SELNUM  >   SPA-GMN-W20-RIREKILST-MAX )
               MOVE    100         TO  SPA-GMN-W20-CUR
               MOVE    "0013"      TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-SELNUM  TO  SPA-GMN-W20-RIREKILST-SEL
      *
      *    ÆþÎÏ¹àÌÜÎÎ°èÊÔ½¸½èÍý
           MOVE    SPA-NAI-W20-RIREKILST-BTUNUM 
                                   TO  WRK-BTUNUM 
           MOVE    SPA-NAI-W20-TRYUKOSTYMD (SPA-GMN-W20-SELNUM)
                                   TO  WRK-KIJUN-YMD
      *
      *    ÆþÎÏÎÎ°è½é´ü²½
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           MOVE    W20-SELNUM      TO  SPA-GMN-W20-SELNUM
      *
           MOVE    2               TO  SPA-GMN-W20-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¥ê¥¹¥È
      *****************************************************************
       4100-BTULST-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-W20-BTULST-SEL
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   CONST-BTULST-MAX )
                     OR      ( SPA-GMN-W20-BTULST-SEL
                                       >   ZERO )
      *
               IF      (   W20-BTULST-SEL  (IDX2)  =   "T" )
      *
                   MOVE    SPA-GMN-W20-TBTUNUM (IDX2)
                                       TO  W20-BTUSELNUM
                   PERFORM 4100-BTUSELNUM-CHK-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4100-BTULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÍúÎò¥ê¥¹¥È
      *****************************************************************
       4100-RIREKILST-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-RIREKILST-SEL
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   CONST-RIREKILST-MAX )
                    OR       ( SPA-GMN-W20-RIREKILST-SEL
                                       >   ZERO )
      *
               IF    ( W20-RIREKILST-SEL (IDX2)
                                       =   "T" )
      *
                   MOVE    IDX2        TO  W20-SELNUM
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¹àÌÜÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYS5001
      *
      *    ÉÂÅï¾ðÊó¸¡º÷½èÍý
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    WRK-BTUNUM      TO  SYS-5001-KBNCD
           MOVE    WRK-KIJUN-YMD   TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    1           TO  FLG-SYS5001
               GO  TO  4100-INPUT-AREA-EDIT-EXT
           END-IF
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    Í­¸ú³«»ÏÆü
           MOVE    SYS-5001-STYUKYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD
                                       SPA-NAI-W20-YUKOSTYMD-OLD
                                       SPA-NAI-W20-YUKOSTYMD-MOT
                                       WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W20-YUKOSTYMD
      *
      *    Í­¸ú½ªÎ»Æü
           MOVE    SYS-5001-EDYUKYMD
                                   TO  SPA-NAI-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD-OLD
                                       WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W20-YUKOEDYMD
      *
           PERFORM 41001-INPUT-AREA-EDIT-SEC
      *
      *    Æþ±¡ÎÁ²Ã»»ÀßÄê²èÌÌ¤Î°ú¤­·Ñ¤®ÎÎ°èÊÔ½¸
           PERFORM    VARYING   IDX-KSN2   FROM   1   BY  1
             UNTIL   (IDX-KSN2   >   10)    OR
                     (SYS-5001-KSNCD2(IDX-KSN2)  =  SPACE)
                 MOVE   SYS-5001-KSNCD2(IDX-KSN2)
                                   TO  SPA-W201-TKSNCD2 (IDX-KSN2)
           END-PERFORM
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¹àÌÜÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       41001-INPUT-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE                  SYS-5008-REC
           MOVE    "5008"          TO  SYS-5008-KANRICD
           MOVE    SYS-5001-KBNCD  TO  SYS-5008-KBNCD
           MOVE    SYS-5001-STYUKYMD
                                   TO  SYS-5008-STYUKYMD
                                       SYS-5008-EDYUKYMD
           MOVE    SYS-5008-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5008-REC
      *
      *    ÉÂÅï¼ïÊÌ
           MOVE    SYS-5001-BTU-SBT    TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SBT-G
           MOVE    SYS-5001-BTU-SBT    TO  SPA-NAI-W20-SBT-MOT
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóºÆÀßÄê
           PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
      *    ÉÂÅïÈÖ¹æ
           MOVE    SYS-5001-BTU-NUM    TO  SPA-GMN-W20-BTUNUM
                                           SPA-GMN-W20-BTUSELNUM
      *
      *    ÉÂÅïÌ¾¾Î
           MOVE    SYS-5001-BTU-NAME   TO  SPA-GMN-W20-NAME
      *
      *    Ã»½ÌÉÂÅïÌ¾¾Î
           MOVE    SYS-5001-BTU-TANNAME
                                       TO  SPA-GMN-W20-TANNAME
      *
      *    ÆÃÄêÆþ±¡ÎÁ
           MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKNYUIN-G
           MOVE    WRK-CMB-ITM3        TO  SPA-NAI-W20-TOKSRYCD
           MOVE    WRK-CMB-ITM4        TO  SPA-NAI-W20-TOKNYUIN
      *
      *    ÉÂÅï²ÊÌ¾
           MOVE    SYS-5001-BTU-KA     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SRYKA-G
      *
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
           MOVE    SYS-5001-RECEDSPKBN TO  WRK-CMB-CD
           PERFORM 4200-RECEDSPKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-RECEDSPKBN-G
      *
      *    ÉÂÅï¥³¡¼¥É
           IF    ( SYS-5001-SRYCD      NOT =   SPACE )
               MOVE    SYS-5001-SRYCD(5:1)
                                       TO  WRK-CMB-CD
               PERFORM 4200-BTUTYPELST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-BTUTYPE-G
               MOVE    SYS-5001-SRYCD(6:4)
                                       TO  WRK-KBN4-G
               MOVE    WRK-KBN4        TO  SPA-GMN-W20-BTUCD
           END-IF
           IF    ( SYS-5001-TSRYCD      NOT =   SPACE )
               MOVE    SYS-5001-TSRYCD(5:1)
                                       TO  WRK-CMB-CD
               PERFORM 4200-BTUTYPELST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-TBTUTYPE-G
               MOVE    SYS-5001-TSRYCD(6:4)
                                       TO  WRK-KBN4-G
               MOVE    WRK-KBN4        TO  SPA-GMN-W20-TBTUCD
           END-IF
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬
           MOVE    SYS-5001-BTU-EDKBN  TO  WRK-CMB-CD
           PERFORM 4200-EDKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-EDKBN-G
      *
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥ÜÊÔ½¸½èÍý
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
           MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-W20-KHNSRYCD
      *
      *    Æþ±¡´ðËÜÅÀ¿ô¤ò¥µ¥Ö¥×¥í¤è¤ê¼èÆÀ¤¹¤ë
           INITIALIZE W20SUB01-LNK
           MOVE    "1"             TO  W20SUB01-I-KBN
           IF    ( SYS-5001-KHNSRYCD   NOT =   SPACE )
               MOVE    SYS-5001-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           ELSE
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           END-IF
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  W20SUB01-I-KJNYMD
      *
           CALL    "ORCGW20SUB01"      USING
                                       W20SUB01-LNK
                                       SPA-AREA
           IF    ( W20SUB01-O-RC   =   ZERO )
               MOVE    W20SUB01-O-KHNSRYCD
                                   TO  WRK-CMB-CD
               PERFORM 4201-KHNRYOLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KHNRYO-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W20-KHNSRYCD
               MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W20-KHNTEN
               MOVE    WRK-CMB-ITM4    TO  SPA-NAI-W20-NYUKHNKBN
               MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W20-YKNCHOKACHK-G
               MOVE    WRK-CMB-ITM6    TO  SPA-NAI-W20-TOKHOSP
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H260401 )
            AND  ( SPA-NAI-W20-YKNCHOKACHK-G NOT = SPACE )
               PERFORM 4100-YKNCHOKAKBN-CMB-EDT-SEC
               MOVE    SPA-GMN-W20-YKNCHOKAKBNLST (1)
                                       TO  SPA-GMN-W20-YKNCHOKAKBN-G
           END-IF
      *
      *    Ìë¶ÐÄ¶²Ã¸º»»¶èÊ¬
           MOVE    SPACE               TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5001-YKNCHOKAKBN    =   "1" )
               MOVE    "1"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-YKNTOKUKBN     =   "1" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-SYOKUINKBN     =   "1" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-SYOKUINKBN     =   "1" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-SYOKUINKBN2    =   "1" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-JUUSHOUKNGKBN  =   "1" )
               MOVE    "5"             TO  WRK-CMB-CD
           END-EVALUATE
           IF    ( WRK-CMB-CD          NOT =   SPACE )
               PERFORM 4200-YKNCHOKAKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-YKNCHOKAKBN-G
           END-IF
      *
           PERFORM 42011-RYOYOSANTEI-CHK-SEC
           IF    ( FLG-RYOYOSANTEI =   1 )
               PERFORM 4100-RYOYOKBN-CMB-EDT-SEC
           END-IF
      *
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬
           MOVE    SYS-5001-CRYOYOKBN  TO  WRK-CMB-CD
           PERFORM 4200-RYOYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-RYOYOKBN-G
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT          =   CONST-KANSATU )
               PERFORM 41005-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H300401 )
               PERFORM 41008-H30R02-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H280401 )
               PERFORM 41008-H28-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H260401 )
               PERFORM 41007-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H240401 )
               PERFORM 41006-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H220401 )
               PERFORM 41004-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H200401 )
               PERFORM 41003-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H180401 )
               PERFORM 41002-KSN-EDIT-SEC
           WHEN    OTHER
               PERFORM 41001-KSN-EDIT-SEC
           END-EVALUATE
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-YKTTYMD-MAX )
      *
               IF    ( SYS-5008-YAKANGENYMD (IDXA)
                                   NOT =   SPACE )
                   MOVE    SYS-5008-YAKANGENYMD (IDXA)
                                       TO  WRK-YMD
                  PERFORM 800-SEIWA-HEN-SEC
                  MOVE    WRK-HENYMDG  TO  SPA-GMN-W20-YKTTYMD (IDXA)
                  MOVE    WRK-SYMD     TO  SPA-NAI-W20-YKTTYMD (IDXA)
               END-IF
      *
           END-PERFORM
      *
           .
       41001-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H14.4¡ÁH18.3)
      *****************************************************************
       41001-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
           MOVE    SYS-5001-BTU-YAKANKNGKSN(2:1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-YAKANKNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-YKNKNG-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           MOVE    SYS-5001-NANBYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NANBYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-NANBYO-G
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    SYS-5001-TOKUSYU-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOKUSYULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKUSYU-G
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           MOVE    SYS-5001-JIDO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-JIDO-G
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    SYS-5001-SEISIN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SEISINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SEISIN-G
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           MOVE    "0"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5001-KANGO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (3)  =   "2" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (4)  =   "2" )
               MOVE    "5"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (5)  =   "2" )
               MOVE    "6"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-HICHLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICH-G
      *
           .
       41001-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H18.4¡ÁH20.3)
      *****************************************************************
       41002-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
           MOVE    SYS-5001-BTU-YAKANKNGKSN(2:1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-YAKANKNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-YKNKNG-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»
           MOVE    SYS-5001-NANBYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NANBYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-NANBYO-G
      *
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    SYS-5001-TOKUSYU-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOKUSYULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKUSYU-G
      *
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»
           MOVE    SYS-5001-JIDO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-JIDO-G
      *
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»
           MOVE    SYS-5001-SEISIN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SEISINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SEISIN-G
      *
      *    ´Ç¸îÇÛÃÖ²Ã»»
           MOVE    SYS-5001-KANGO-KBN (1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-HICHLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICH-G
      *
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           MOVE    SYS-5001-KNGTEK-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HICHTEKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICHTEK-G
      *
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           MOVE    SYS-5001-DRTEK-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-DRTEKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-DRTEK-G
      *
           .
       41002-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H30.4¡Á)
      *****************************************************************
       41008-H30R02-EDIT-SEC            SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  SPA-GMN-W20-SYOKUDO
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  SPA-GMN-W20-HOIKU
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
      *
                   IF    ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-R020401 )
                       EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                       WHEN    CONST-R02-A249 (1)
                           MOVE    "2"     TO  SPA-GMN-W20-SSNKYUDR
                       WHEN    CONST-R02-A249 (2)
                           MOVE    "3"     TO  SPA-GMN-W20-SSNKYUDR
                       WHEN    CONST-R02-A249 (3)
                           MOVE    "4"     TO  SPA-GMN-W20-SSNKYUDR
                       END-EVALUATE
                   ELSE
                       EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                       WHEN    CONST-H28-A249
                           MOVE    "2"     TO  SPA-GMN-W20-SSNKYUDR
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-H30-A105C3 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H30-A105C3 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H30-A105C3 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-KNGHITU
      *
                   WHEN    CONST-H30-A105C04
                       MOVE    "2"     TO  SPA-GMN-W20-HYOUKA
      *
                   WHEN    CONST-H28-A100C12
                       MOVE    "2"     TO  SPA-GMN-W20-ADLIJI
      *
                   WHEN    CONST-H28-A103C07
                       MOVE    "2"     TO  SPA-GMN-W20-SSNHKNFUKU
      *
                   WHEN    CONST-H28-A101C10
                       MOVE    "2"     TO  SPA-GMN-W20-ZTKFUKKI
      *
                   WHEN    CONST-H28-A109C11
                       MOVE    "2"     TO  SPA-GMN-W20-ZTKFUKKI
      *
                   WHEN    CONST-H28-A308C05 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-TAISEI
                   WHEN    CONST-H28-A308C05 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-TAISEI
      *
                   WHEN    CONST-H28-A308-03C03
                       MOVE    "2"     TO  SPA-GMN-W20-KNGSKHICH
      *
                   WHEN    CONST-H28-A308-03C04
                       MOVE    "2"     TO  SPA-GMN-W20-KNGHJSHICH
      *
                   WHEN    CONST-H28-A207-0201(1)
                       MOVE    "2"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(2)
                       MOVE    "3"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(3)
                       MOVE    "4"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(4)
                       MOVE    "5"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(5)
                       MOVE    "6"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(6)
                       MOVE    "7"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(7)
                       MOVE    "8"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(8)
                       MOVE    "9"     TO  SPA-GMN-W20-DRHJO
      *
                   WHEN    CONST-H28-A207-0202(1)
                       MOVE    "2"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(2)
                       MOVE    "3"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(3)
                       MOVE    "4"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(4)
                       MOVE    "5"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(5)
                       MOVE    "6"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(6)
                       MOVE    "7"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(7)
                       MOVE    "8"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(8)
                       MOVE    "9"     TO  SPA-GMN-W20-DRHJO2
      *
                   WHEN    CONST-H28-A207-03(1)
                       MOVE    "2"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(2)
                       MOVE    "3"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(3)
                       MOVE    "4"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(4)
                       MOVE    "5"     TO  SPA-GMN-W20-KYUKNGHJ
      *
                   WHEN    CONST-H28-A207-03C02 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H28-A207-03C02 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H28-A207-03C02 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-YKNKYUKNGHJ
      *
                   WHEN    CONST-H28-A207-03C03
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNG-A207-03
      *
                   WHEN    CONST-H30-A207-04 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-H30-A207-04 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-H30-A207-04 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-H30-A207-04 (4)
                       MOVE    "5"     TO  SPA-GMN-W20-KNGYKNHICHI
      *
                   WHEN    CONST-H28-A210
                       MOVE    "2"     TO  SPA-GMN-W20-NANBYO
      *
                   WHEN    CONST-H28-A211
                       MOVE    "2"     TO  SPA-GMN-W20-TOKUSYU
      *
                   WHEN    CONST-H28-A213
                       MOVE    "2"     TO  SPA-GMN-W20-HICH
      *
                   WHEN    CONST-H28-A214C02
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNGHJ
      *
                   WHEN    CONST-H28-A214C03
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNG-A214
      *
                   WHEN    CONST-H28-A228
                       MOVE    "2"     TO  SPA-GMN-W20-SEISIN
      *
                   WHEN    CONST-H28-A230-02
                       MOVE    "2"     TO  SPA-GMN-W20-SEICHIIKI
      *
                   WHEN    CONST-H28-A307C02
                       MOVE    "2"     TO  SPA-GMN-W20-HOIKU
      *
                   WHEN    CONST-H28-A308C02
                       MOVE    "2"     TO  SPA-GMN-W20-KJTRIHA
      *
                   WHEN    CONST-H28-A108C04
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNKYU
      *
                   WHEN    CONST-H28-A108C0501
                       MOVE    "2"     TO  SPA-GMN-W20-DRTEK
      *
                   WHEN    CONST-H28-A108C0502
                       MOVE    "2"     TO  SPA-GMN-W20-DRTEK2
      *
                   WHEN    CONST-H28-A108C0601
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHICH1
      *
                   WHEN    CONST-H28-A108C0602
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHICH2
      *
                   WHEN    CONST-H28-A108C0603
                       MOVE    "2"     TO  SPA-GMN-W20-YKNHICH1
      *
                   WHEN    CONST-H28-A108C0604
                       MOVE    "2"     TO  SPA-GMN-W20-YKNHICH2
      *
                   WHEN    CONST-H28-A108C0605
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHJO1
      *
                   WHEN    CONST-H28-A108C0606
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHJO2
      *
                   WHEN    CONST-H28-A108C10
                       MOVE    "2"     TO  SPA-GMN-W20-EIYO
      *
      *            Ìë´Ö´Ç¸î²Ã»»
                   WHEN    CONST-H30-A101C13
                       MOVE    "2"     TO  SPA-GMN-W20-H30YKNKNG
      *            ´Ç¸îÊä½õ²Ã»»¡Ê¾ã³²¼ÔÉÂÅï¡Ë
                   WHEN    CONST-H30-A106C09
                       MOVE    "2"     TO  SPA-GMN-W20-KNGHJO-A104
      *            Ìë´Ö´Ç¸îÂÎÀ©²Ã»»
                   WHEN    CONST-H30-A106C10
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNG-A104
      *            £Á£³£°£¸¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
                   WHEN    CONST-H30-A308-03C07
                       MOVE    "2"     TO  SPA-GMN-W20-KYH-A308-03
      *            £Á£³£±£±´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
                   WHEN    CONST-H30-A311C05
                       MOVE    "2"     TO  SPA-GMN-W20-KYH-A311
      *            £Á£³£±£±¡Ý£³´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
                   WHEN    CONST-H30-A311-03C05
                       MOVE    "2"     TO  SPA-GMN-W20-KYH-A311-03
      *            £Á£³£±£´Ç§ÃÎ¾ÉÌë´ÖÂÐ±þ²Ã»»
                   WHEN    CONST-H30-A314C03
                       MOVE    "2"     TO  SPA-GMN-W20-NYT
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41008-H30R02-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H28.4¡ÁH30.3)
      *****************************************************************
       41008-H28-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  SPA-GMN-W20-SYOKUDO
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  SPA-GMN-W20-HOIKU
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-H28-A100C6 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H28-A100C6 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H28-A100C6 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-KNGHITU
      *
                   WHEN    CONST-H28-A100C07
                       MOVE    "2"     TO  SPA-GMN-W20-HYOUKA
      *
                   WHEN    CONST-H28-A100C12
                       MOVE    "2"     TO  SPA-GMN-W20-ADLIJI
      *
                   WHEN    CONST-H28-A103C07
                       MOVE    "2"     TO  SPA-GMN-W20-SSNHKNFUKU
      *
                   WHEN    CONST-H28-A101C10
                       MOVE    "2"     TO  SPA-GMN-W20-ZTKFUKKI
      *
                   WHEN    CONST-H28-A109C11
                       MOVE    "2"     TO  SPA-GMN-W20-ZTKFUKKI
      *
                   WHEN    CONST-H28-A308C05 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-TAISEI
                   WHEN    CONST-H28-A308C05 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-TAISEI
      *
                   WHEN    CONST-H28-A308-03C03
                       MOVE    "2"     TO  SPA-GMN-W20-KNGSKHICH
      *
                   WHEN    CONST-H28-A308-03C04
                       MOVE    "2"     TO  SPA-GMN-W20-KNGHJSHICH
      *
                   WHEN    CONST-H28-A249
                       MOVE    "2"     TO  SPA-GMN-W20-SSNKYUDR
      *
                   WHEN    CONST-H28-A207-0201(1)
                       MOVE    "2"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(2)
                       MOVE    "3"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(3)
                       MOVE    "4"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(4)
                       MOVE    "5"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(5)
                       MOVE    "6"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(6)
                       MOVE    "7"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(7)
                       MOVE    "8"     TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H28-A207-0201(8)
                       MOVE    "9"     TO  SPA-GMN-W20-DRHJO
      *
                   WHEN    CONST-H28-A207-0202(1)
                       MOVE    "2"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(2)
                       MOVE    "3"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(3)
                       MOVE    "4"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(4)
                       MOVE    "5"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(5)
                       MOVE    "6"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(6)
                       MOVE    "7"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(7)
                       MOVE    "8"     TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H28-A207-0202(8)
                       MOVE    "9"     TO  SPA-GMN-W20-DRHJO2
      *
                   WHEN    CONST-H28-A207-03(1)
                       MOVE    "2"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(2)
                       MOVE    "3"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(3)
                       MOVE    "4"     TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H28-A207-03(4)
                       MOVE    "5"     TO  SPA-GMN-W20-KYUKNGHJ
      *
                   WHEN    CONST-H28-A207-03C02 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H28-A207-03C02 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H28-A207-03C02 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-YKNKYUKNGHJ
      *
                   WHEN    CONST-H28-A207-03C03
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNG-A207-03
      *
                   WHEN    CONST-H28-A207-04 (1)
                       MOVE    "2"     TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-H28-A207-04 (2)
                       MOVE    "3"     TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-H28-A207-04 (3)
                       MOVE    "4"     TO  SPA-GMN-W20-KNGYKNHICHI
      *
                   WHEN    CONST-H28-A210
                       MOVE    "2"     TO  SPA-GMN-W20-NANBYO
      *
                   WHEN    CONST-H28-A211
                       MOVE    "2"     TO  SPA-GMN-W20-TOKUSYU
      *
                   WHEN    CONST-H28-A213
                       MOVE    "2"     TO  SPA-GMN-W20-HICH
      *
                   WHEN    CONST-H28-A214C02
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNGHJ
      *
                   WHEN    CONST-H28-A214C03
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNG-A214
      *
                   WHEN    CONST-H28-A228
                       MOVE    "2"     TO  SPA-GMN-W20-SEISIN
      *
                   WHEN    CONST-H28-A230-02
                       MOVE    "2"     TO  SPA-GMN-W20-SEICHIIKI
      *
                   WHEN    CONST-H28-A307C02
                       MOVE    "2"     TO  SPA-GMN-W20-HOIKU
      *
                   WHEN    CONST-H28-A308C02
                       MOVE    "2"     TO  SPA-GMN-W20-KJTRIHA
      *
                   WHEN    CONST-H28-A108C04
                       MOVE    "2"     TO  SPA-GMN-W20-YKNKNKYU
      *
                   WHEN    CONST-H28-A108C0501
                       MOVE    "2"     TO  SPA-GMN-W20-DRTEK
      *
                   WHEN    CONST-H28-A108C0502
                       MOVE    "2"     TO  SPA-GMN-W20-DRTEK2
      *
                   WHEN    CONST-H28-A108C0601
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHICH1
      *
                   WHEN    CONST-H28-A108C0602
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHICH2
      *
                   WHEN    CONST-H28-A108C0603
                       MOVE    "2"     TO  SPA-GMN-W20-YKNHICH1
      *
                   WHEN    CONST-H28-A108C0604
                       MOVE    "2"     TO  SPA-GMN-W20-YKNHICH2
      *
                   WHEN    CONST-H28-A108C0605
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHJO1
      *
                   WHEN    CONST-H28-A108C0606
                       MOVE    "2"     TO  SPA-GMN-W20-SNRHJO2
      *
                   WHEN    CONST-H28-A108C10
                       MOVE    "2"     TO  SPA-GMN-W20-EIYO
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41008-H28-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H26.4¡ÁH28.3)
      *****************************************************************
       41007-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  SPA-GMN-W20-SYOKUDO
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  SPA-GMN-W20-HOIKU
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  SPA-GMN-W20-NANBYO
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  SPA-GMN-W20-TOKUSYU
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  SPA-GMN-W20-SEISIN
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  SPA-GMN-W20-HICH
      *
                   WHEN    CONST-H26-DRHJO1 (1)
                       MOVE    "2"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (2)
                       MOVE    "3"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (3)
                       MOVE    "4"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (4)
                       MOVE    "5"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (5)
                       MOVE    "6"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (6)
                       MOVE    "7"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (7)
                       MOVE    "8"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H26-DRHJO1 (8)
                       MOVE    "9"         TO  SPA-GMN-W20-DRHJO
      *
                   WHEN    CONST-H26-DRHJO2 (1)
                       MOVE    "2"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (2)
                       MOVE    "3"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (3)
                       MOVE    "4"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (4)
                       MOVE    "5"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (5)
                       MOVE    "6"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (6)
                       MOVE    "7"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (7)
                       MOVE    "8"         TO  SPA-GMN-W20-DRHJO2
                   WHEN    CONST-H26-DRHJO2 (8)
                       MOVE    "9"         TO  SPA-GMN-W20-DRHJO2
      *
                   WHEN    CONST-H2404-KYUKNGHJ (1)
                       MOVE    "2"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (2)
                       MOVE    "3"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (3)
                       MOVE    "4"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (4)
                       MOVE    "5"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H26-YKNKYUKNGHJ (1)
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H26-YKNKYUKNGHJ (2)
                       MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H26-YKNKYUKNGHJ (3)
                       MOVE    "4"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H2404-KNGYKNHICHI
                       MOVE    "2"         TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  SPA-GMN-W20-SEICHIIKI
                   WHEN    CONST-HYOUKA
                       MOVE    "2"         TO  SPA-GMN-W20-HYOUKA
                   WHEN    CONST-KJTRIHA
                       MOVE    "2"         TO  SPA-GMN-W20-KJTRIHA
                   WHEN    CONST-H2404-KNGHITU1
                       MOVE    "2"         TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H2404-KNGHITU2
                       MOVE    "3"         TO  SPA-GMN-W20-KNGHITU
      *             WHEN    CONST-H2404-BTUYKZ
      *                MOVE    "2"         TO  SPA-GMN-W20-BTUYKZ
      *
                   WHEN    CONST-H2204-DRTEKCD1
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK2
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKNKYU
                   WHEN    CONST-H2204-SNRHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH1
                   WHEN    CONST-H2204-SNRHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH2
                   WHEN    CONST-H2204-YKNHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH1
                   WHEN    CONST-H2204-YKNHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH2
                   WHEN    CONST-H26-ZTKFUKKI
                       MOVE    "2"         TO  SPA-GMN-W20-ZTKFUKKI
                   WHEN    CONST-H26-ADLIJI-A100
                       MOVE    "2"         TO  SPA-GMN-W20-ADLIJI
                   WHEN    CONST-H26-SSNKYUDR
                       MOVE    "2"         TO  SPA-GMN-W20-SSNKYUDR
                   WHEN    CONST-H26-SSNHKNFUKU-A103
                       MOVE    "2"         TO  SPA-GMN-W20-SSNHKNFUKU
                   WHEN    CONST-H26-SNRHJO1
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHJO1
                   WHEN    CONST-H26-SNRHJO2
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHJO2
                   WHEN    CONST-H26-EIYO-A108
                       MOVE    "2"         TO  SPA-GMN-W20-EIYO
                   WHEN    CONST-H26-TAISEI
                       MOVE    "2"         TO  SPA-GMN-W20-TAISEI
                   WHEN    CONST-H26-KNGSKHICH
                       MOVE    "2"         TO  SPA-GMN-W20-KNGSKHICH
                   WHEN    CONST-H26-KNGHJSHICH
                       MOVE    "2"         TO  SPA-GMN-W20-KNGHJSHICH
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41007-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H24.4¡ÁH26.3)
      *****************************************************************
       41006-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  SPA-GMN-W20-SYOKUDO
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  SPA-GMN-W20-HOIKU
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  SPA-GMN-W20-NANBYO
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  SPA-GMN-W20-TOKUSYU
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  SPA-GMN-W20-SEISIN
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  SPA-GMN-W20-HICH
      *
                   WHEN    CONST-H2404-DRHJO (1)
                       MOVE    "2"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (2)
                       MOVE    "3"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (3)
                       MOVE    "4"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (4)
                       MOVE    "5"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (5)
                       MOVE    "6"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (6)
                       MOVE    "7"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (7)
                       MOVE    "8"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (8)
                       MOVE    "9"         TO  SPA-GMN-W20-DRHJO
      *
                   WHEN    CONST-H2404-KYUKNGHJ (1)
                       MOVE    "2"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (2)
                       MOVE    "3"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (3)
                       MOVE    "4"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (4)
                       MOVE    "5"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-YKNKYUKNGHJ1
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H2404-YKNKYUKNGHJ2
                       MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H2404-KNGYKNHICHI
                       MOVE    "2"         TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  SPA-GMN-W20-SEICHIIKI
                   WHEN    CONST-HYOUKA
                       MOVE    "2"         TO  SPA-GMN-W20-HYOUKA
                   WHEN    CONST-KJTRIHA
                       MOVE    "2"         TO  SPA-GMN-W20-KJTRIHA
                   WHEN    CONST-H2404-KNGHITU1
                       MOVE    "2"         TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H2404-KNGHITU2
                       MOVE    "3"         TO  SPA-GMN-W20-KNGHITU
      *             WHEN    CONST-H2404-BTUYKZ
      *                MOVE    "2"         TO  SPA-GMN-W20-BTUYKZ
      *
                   WHEN    CONST-H2204-DRTEKCD1
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK2
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKNKYU
                   WHEN    CONST-H2204-SNRHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH1
                   WHEN    CONST-H2204-SNRHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH2
                   WHEN    CONST-H2204-YKNHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH1
                   WHEN    CONST-H2204-YKNHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH2
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41006-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(°åÎÅ´Ñ»¡Ë¡)
      *****************************************************************
       41005-KSN-EDIT-SEC               SECTION.
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0 >  SPA-GMN-W20-SYOUBTUBRM-MAX )
      *
                       IF    ( SYS-5001-KSNCD (IDX-KSN)
                                          =    CONST-SYOUBTUBRMCD(IDX0))
                           MOVE    SPA-GMN-W20-SYOUBTUBRMLST(IDX0)
                                          TO   SPA-GMN-W20-SYOUBTUBRM-G
                       END-IF
      *
                   END-PERFORM
      *
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-BTUMIKJNCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-BTUMIKJNLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-BTUMIKJN-G
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
           .
       41005-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H22.4¡ÁH24.3)
      *****************************************************************
       41004-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-NANBYOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-NANBYO-G
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-TOKUSYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKUSYU-G
                   WHEN    CONST-JIDOCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JIDOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JIDO-G
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEISINLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEISIN-G
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HICHLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HICH-G
                   WHEN    CONST-H2204-DRHJO (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (2)
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (3)
                       MOVE    "4"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (4)
                       MOVE    "5"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (5)
                       MOVE    "6"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (6)
                       MOVE    "7"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-JKAIFUKU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JKAIFUKULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JKAIFUKU-G
                   WHEN    CONST-H2204-DRTEKCD1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEKLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK-G
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEK2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK2-G
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNKNKYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNKNKYU-G
                   WHEN    CONST-H2204-SNRHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH1-G
                   WHEN    CONST-H2204-SNRHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH2-G
                   WHEN    CONST-H2204-YKNHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH1-G
                   WHEN    CONST-H2204-YKNHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH2-G
                   WHEN    CONST-KYUKNGHJ1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-KYUKNGHJLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KYUKNGHJ-G
                   WHEN    CONST-KYUKNGHJ2
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-KYUKNGHJLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KYUKNGHJ-G
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEICHIIKILST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEICHIIKI-G
                   WHEN    CONST-HYOUKA
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HYOUKALST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HYOUKA-G
                   WHEN    CONST-KJTRIHA
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-KJTRIHALST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KJTRIHA-G
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       41004-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÊÌÀßÄê²Ã»»ÀßÄê½èÍý(H20.4¡ÁH22.3)
      *****************************************************************
       41003-KSN-EDIT-SEC               SECTION.
      *
      *    ´Ç¸îÊä½õ²Ã»»
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    ¿©Æ²²Ã»»
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-NANBYOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-NANBYO-G
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-TOKUSYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKUSYU-G
                   WHEN    CONST-JIDOCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JIDOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JIDO-G
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEISINLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEISIN-G
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HICHLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HICH-G
                   WHEN    CONST-DRHJO (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (2)
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (3)
                       MOVE    "4"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (4)
                       MOVE    "5"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-JKAIFUKU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JKAIFUKULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JKAIFUKU-G
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEKLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK-G
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNKNKYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNKNKYU-G
                   WHEN    CONST-SNRHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH1-G
                   WHEN    CONST-SNRHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH2-G
                   WHEN    CONST-YKNHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH1-G
                   WHEN    CONST-YKNHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH2-G
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEICHIIKILST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEICHIIKI-G
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       41003-KSN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÍúÎòÎÎ°èÊÔ½¸½èÍý
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-W20-RIREKILST
                           SPA-NAI-W20-RIREKILST
      *
           MOVE    SPA-GMN-W20-BTUNUM
                                   TO  SPA-NAI-W20-RIREKILST-BTUNUM 
      *
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX-LST            >=   CONST-RIREKILST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
               MOVE    IDX-LST    TO  SPA-GMN-W20-RIREKILST-MAX
      *
      *        ÁªÂòÈÖ¹æ
               MOVE    IDX-LST    TO  SPA-GMN-W20-TRSELNUM (IDX-LST)
      *
      *        Í­¸ú³«»ÏÆü
               MOVE    SYS-5001-STYUKYMD
                                  TO  SPA-NAI-W20-TRYUKOSTYMD (IDX-LST)
                                          WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG
                                  TO  SPA-GMN-W20-TRYUKOSTYMD (IDX-LST)
      *
      *        Í­¸ú½ªÎ»Æü
               MOVE    SYS-5001-EDYUKYMD
                                   TO  SPA-NAI-W20-TRYUKOEDYMD(IDX-LST)
                                           WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W20-TRYUKOEDYMD(IDX-LST)
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD-OLD
                                   =   SPA-NAI-W20-TRYUKOSTYMD(IDX-LST))
                AND  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                   =   SPA-NAI-W20-TRYUKOEDYMD(IDX-LST))
                   MOVE    IDX-LST    TO  SPA-GMN-W20-SELNUM
                   MOVE    IDX-LST    TO  SPA-GMN-W20-RIREKILST-SEL
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¾ðÊóÎÎ°è½é´ü²½½èÍý
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W20-INPUT-AREA
                       SPA-NAI-W20-INPUT-AREA
      *
           IF    ( SPA-W20-W21-MOTOPG  =    SPACE )
               MOVE    "ÉÂ¼¼"      TO  SPA-GMN-W20-B09
           END-IF
      *
      *    Í­¸ú´ü´Ö¤Î³«»ÏÆü¤È½ªÎ»Æü¤òÀßÄê
           MOVE    SPA-GMN-W20-KJNYMD (1 : 6)
                                   TO  SPA-GMN-W20-YUKOSTYMD (1 : 6)
           MOVE    ". 1"           TO  SPA-GMN-W20-YUKOSTYMD (7 : 3)
           MOVE    SPA-NAI-W20-KJNYMD   (1 : 6)
                                   TO  SPA-NAI-W20-YUKOSTYMD (1 : 6)
           MOVE    "01"            TO  SPA-NAI-W20-YUKOSTYMD (7 : 2)
      *
           MOVE    "99999999"      TO  SPA-GMN-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD
      *
           IF    ( SPA-GMN-W20-BTULST-MAX  >   ZERO )
               MOVE    56          TO  SPA-GMN-W20-CUR
           ELSE
               MOVE    1           TO  SPA-GMN-W20-CUR
           END-IF
      *
           PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»ÎÎ°è½é´ü²½½èÍý
      *****************************************************************
       4201-KSN-AREA-CLEAR-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                   TO  SPA-GMN-W20-KNGHJO-G
      *
           MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                   TO  SPA-GMN-W20-SYOUBTUBRM-G
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                      >    ZERO )
                   MOVE   SPA-GMN-W20-KSNLST (IDX-KSN 1)
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4201-KSN-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *   ÍúÎò¾ðÊóÎÎ°è½é´ü²½½èÍý
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-W20-RIREKILST
                       SPA-NAI-W20-RIREKILST
      *
           MOVE    ZERO        TO  SPA-GMN-W20-BTULST-SEL
      *
           .
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¾ðÊóºÆÀßÄê½èÍý
      *****************************************************************
       4100-SYS-KANRI-SAI-SET-SEC      SECTION.
      *
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ½èÍý
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-GET-SYS1001-SEC
      *
      *    ¥³¥ó¥Ü½é´ü½èÍý
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           INITIALIZE  SPA-NAI-W20-KSNINF-G
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               MOVE    SPACE               TO  SPA-GMN-W20-KSN-LBL
                                                           (IDX-KSN)
      *
           END-PERFORM
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               MOVE    "Ì¤´ð½à¡ÊÉÂÅï¤Ë·¸¤ë´ð½à¤òËþ¤¿¤µ¤Ê¤¤¡Ë"
                                           TO  SPA-GMN-W20-BTUMIKJN-LBL
               MOVE    CONST-BTUMIKJNCD    TO  SPA-NAI-W20-BTUMIKJN-CD
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H300401 )
      *
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»"
                                       TO  SPA-GMN-W20-KNGHITU-LBL
               MOVE    CONST-H30-A105C3 (1)
                                       TO  SPA-NAI-W20-KNGHITU-CD
      *
               MOVE    "°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»"
                                       TO  SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-H30-A105C04
                                       TO  SPA-NAI-W20-HYOUKA-CD
      *
               MOVE    "£Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-ADLIJI-LBL
               MOVE    CONST-H28-A100C12
                                       TO  SPA-NAI-W20-ADLIJI-CD
      *
               MOVE    "Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-SSNHKNFUKU-LBL
               MOVE    CONST-H28-A103C07
                                       TO  SPA-NAI-W20-SSNHKNFUKU-CD
      *
               IF    ( SPA-NAI-W20-HOSPSBT  =  "1" )
                   MOVE    "ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»"
                                       TO  SPA-GMN-W20-ZTKFUKKI-LBL
                   MOVE    CONST-H28-A101C10
                                       TO  SPA-NAI-W20-ZTKFUKKI-CD
               ELSE
                   MOVE    "ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»"
                                       TO  SPA-GMN-W20-ZTKFUKKI-LBL
                   MOVE    CONST-H28-A109C11
                                       TO  SPA-NAI-W20-ZTKFUKKI-CD
               END-IF
               MOVE    "ÂÎÀ©¶¯²½²Ã»»"  TO  SPA-GMN-W20-TAISEI-LBL
               MOVE    CONST-H28-A308C05 (1)
                                       TO  SPA-NAI-W20-TAISEI-CD
               MOVE    "´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»" 
                                       TO  SPA-GMN-W20-KNGSKHICH-LBL
               MOVE    CONST-H28-A308-03C03
                                       TO  SPA-NAI-W20-KNGSKHICH-CD
               MOVE    "´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-KNGHJSHICH-LBL
               MOVE    CONST-H28-A308-03C04
                                       TO  SPA-NAI-W20-KNGHJSHICH-CD

               MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-SSNKYUDR-LBL
               MOVE    CONST-H28-A249
                                       TO  SPA-NAI-W20-SSNKYUDR-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±"
                                       TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H28-A207-0201 (7)
                                       TO  SPA-NAI-W20-DRHJO-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²"
                                       TO  SPA-GMN-W20-DRHJO2-LBL
               MOVE    CONST-H28-A207-0202 (7)
                                       TO  SPA-NAI-W20-DRHJO2-CD
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-H28-A207-03 (1)
                                       TO  SPA-NAI-W20-KYUKNGHJ-CD
               MOVE    "Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-YKNKYUKNGHJ-LBL
               MOVE    CONST-H28-A207-03C02 (1)
                                       TO  SPA-NAI-W20-YKNKYUKNGHJ-CD
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£°£·¡Ý£³¡Ë"
                                   TO  SPA-GMN-W20-YKNKNG-A207-03-LBL
               MOVE    CONST-H28-A207-03C03
                                       TO  SPA-NAI-W20-YKNKNG-A207-03-CD
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»¡Ê£Á£²£°£·¡Ý£´¡Ë"
                                       TO SPA-GMN-W20-KNGYKNHICHI-LBL
               MOVE    CONST-H30-A207-04 (1)
                                       TO  SPA-NAI-W20-KNGYKNHICHI-CD
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                       TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-H28-A210  TO  SPA-NAI-W20-NANBYO-CD
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                       TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-H28-A211  TO  SPA-NAI-W20-TOKUSYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"  TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-H28-A213  TO  SPA-NAI-W20-HICH-CD
               MOVE    "Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                       TO  SPA-GMN-W20-YKNKNGHJ-LBL
               MOVE    CONST-H28-A214C02
                                       TO  SPA-NAI-W20-YKNKNGHJ-CD
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£±£´¡Ë"
                                       TO  SPA-GMN-W20-YKNKNG-A214-LBL
               MOVE    CONST-H28-A214C03
                                       TO  SPA-NAI-W20-YKNKNG-A214-CD
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                       TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-H28-A228  TO  SPA-NAI-W20-SEISIN-CD
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                       TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-H28-A230-02
                                       TO SPA-NAI-W20-SEICHIIKI-CD
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                       TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-H28-A307C02
                                       TO  SPA-NAI-W20-HOIKU-CD
               MOVE    "µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»"
                                       TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-H28-A308C02
                                       TO SPA-NAI-W20-KJTRIHA-CD
      *
               MOVE    "¿©Æ²²Ã»»"      TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-H28-SYOKUDO
                                       TO  SPA-NAI-W20-SYOKUDO-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                       TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-H28-A108C04
                                       TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H28-A108C0501
                                       TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-H28-A108C0502
                                       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H28-A108C0601
                                       TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H28-A108C0602
                                       TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H28-A108C0603
                                       TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H28-A108C0604
                                       TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-SNRHJO1-LBL
               MOVE    CONST-H28-A108C0605
                                       TO  SPA-NAI-W20-SNRHJO1-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-SNRHJO2-LBL
               MOVE    CONST-H28-A108C0606
                                       TO  SPA-NAI-W20-SNRHJO2-CD
               MOVE    "±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»"
                                       TO  SPA-GMN-W20-EIYO-LBL
               MOVE    CONST-H28-A108C10
                                       TO  SPA-NAI-W20-EIYO-CD
               MOVE    "Ìë´Ö´Ç¸î²Ã»»"
                                       TO  SPA-GMN-W20-H30YKNKNG-LBL
               MOVE    CONST-H30-A101C13
                                       TO  SPA-NAI-W20-H30YKNKNG-CD
               MOVE    "´Ç¸îÊä½õ²Ã»»¡Ê£Á£±£°£¶¡Ë"
                                       TO  SPA-GMN-W20-KNGHJO-A104-LBL
               MOVE    CONST-H30-A106C09
                                       TO  SPA-NAI-W20-KNGHJO-A104-CD
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£±£°£¶¡Ë"
                                       TO  SPA-GMN-W20-YKNKNG-A104-LBL
               MOVE    CONST-H30-A106C10
                                       TO  SPA-NAI-W20-YKNKNG-A104-CD
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»¡Ê£Á£³£°£¸¡Ý£³¡Ë"
                                       TO  SPA-GMN-W20-KYH-A308-03-LBL
               MOVE    CONST-H30-A308-03C07
                                       TO  SPA-NAI-W20-KYH-A308-03-CD
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»¡Ê£Á£³£±£±¡Ë"
                                       TO  SPA-GMN-W20-KYH-A311-LBL
               MOVE    CONST-H30-A311C05
                                       TO  SPA-NAI-W20-KYH-A311-CD
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»¡Ê£Á£³£±£±¡Ý£³¡Ë"
                                       TO  SPA-GMN-W20-KYH-A311-03-LBL
               MOVE    CONST-H30-A311-03C05
                                       TO  SPA-NAI-W20-KYH-A311-03-CD
               MOVE    "Ç§ÃÎ¾ÉÌë´ÖÂÐ±þ²Ã»»"
                                       TO  SPA-GMN-W20-NYT-LBL
               MOVE    CONST-H30-A314C03
                                       TO  SPA-NAI-W20-NYT-CD
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H280401 )
      *
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»"
                                       TO  SPA-GMN-W20-KNGHITU-LBL
               MOVE    CONST-H28-A100C6 (1)
                                       TO  SPA-NAI-W20-KNGHITU-CD
      *
               MOVE    "°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»"
                                       TO  SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-H28-A100C07
                                       TO  SPA-NAI-W20-HYOUKA-CD
      *
               MOVE    "£Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-ADLIJI-LBL
               MOVE    CONST-H28-A100C12
                                       TO  SPA-NAI-W20-ADLIJI-CD
      *
               MOVE    "Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-SSNHKNFUKU-LBL
               MOVE    CONST-H28-A103C07
                                       TO  SPA-NAI-W20-SSNHKNFUKU-CD
      *
               IF    ( SPA-NAI-W20-HOSPSBT  =  "1" )
                   MOVE    "ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»"
                                       TO  SPA-GMN-W20-ZTKFUKKI-LBL
                   MOVE    CONST-H28-A101C10
                                       TO  SPA-NAI-W20-ZTKFUKKI-CD
               ELSE
                   MOVE    "ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»"
                                       TO  SPA-GMN-W20-ZTKFUKKI-LBL
                   MOVE    CONST-H28-A109C11
                                       TO  SPA-NAI-W20-ZTKFUKKI-CD
               END-IF
               MOVE    "ÂÎÀ©¶¯²½²Ã»»"  TO  SPA-GMN-W20-TAISEI-LBL
               MOVE    CONST-H28-A308C05 (1)
                                       TO  SPA-NAI-W20-TAISEI-CD
               MOVE    "´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»" 
                                       TO  SPA-GMN-W20-KNGSKHICH-LBL
               MOVE    CONST-H28-A308-03C03
                                       TO  SPA-NAI-W20-KNGSKHICH-CD
               MOVE    "´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-KNGHJSHICH-LBL
               MOVE    CONST-H28-A308-03C04
                                       TO  SPA-NAI-W20-KNGHJSHICH-CD

               MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»"
                                       TO  SPA-GMN-W20-SSNKYUDR-LBL
               MOVE    CONST-H28-A249
                                       TO  SPA-NAI-W20-SSNKYUDR-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±"
                                       TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H28-A207-0201 (1)
                                       TO  SPA-NAI-W20-DRHJO-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²"
                                       TO  SPA-GMN-W20-DRHJO2-LBL
               MOVE    CONST-H28-A207-0202 (1)
                                       TO  SPA-NAI-W20-DRHJO2-CD
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-H28-A207-03 (1)
                                       TO  SPA-NAI-W20-KYUKNGHJ-CD
               MOVE    "Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO  SPA-GMN-W20-YKNKYUKNGHJ-LBL
               MOVE    CONST-H28-A207-03C02 (1)
                                       TO  SPA-NAI-W20-YKNKYUKNGHJ-CD
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£°£·¡Ý£³¡Ë"
                                   TO  SPA-GMN-W20-YKNKNG-A207-03-LBL
               MOVE    CONST-H28-A207-03C03
                                       TO  SPA-NAI-W20-YKNKNG-A207-03-CD
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»"
                                       TO SPA-GMN-W20-KNGYKNHICHI-LBL
               MOVE    CONST-H28-A207-04 (1)
                                       TO  SPA-NAI-W20-KNGYKNHICHI-CD
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                       TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-H28-A210  TO  SPA-NAI-W20-NANBYO-CD
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                       TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-H28-A211  TO  SPA-NAI-W20-TOKUSYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"  TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-H28-A213  TO  SPA-NAI-W20-HICH-CD
               MOVE    "Ìë´Ö£·£µÂÐ£±´Ç¸îÊä½õ²Ã»»"
                                       TO  SPA-GMN-W20-YKNKNGHJ-LBL
               MOVE    CONST-H28-A214C02
                                       TO  SPA-NAI-W20-YKNKNGHJ-CD
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©²Ã»»¡Ê£Á£²£±£´¡Ë"
                                       TO  SPA-GMN-W20-YKNKNG-A214-LBL
               MOVE    CONST-H28-A214C03
                                       TO  SPA-NAI-W20-YKNKNG-A214-CD
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                       TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-H28-A228  TO  SPA-NAI-W20-SEISIN-CD
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                       TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-H28-A230-02
                                       TO SPA-NAI-W20-SEICHIIKI-CD
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                       TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-H28-A307C02
                                       TO  SPA-NAI-W20-HOIKU-CD
               MOVE    "µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»"
                                       TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-H28-A308C02
                                       TO SPA-NAI-W20-KJTRIHA-CD
      *
               MOVE    "¿©Æ²²Ã»»"      TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-H28-SYOKUDO
                                       TO  SPA-NAI-W20-SYOKUDO-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                       TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-H28-A108C04
                                       TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H28-A108C0501
                                       TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-H28-A108C0502
                                       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H28-A108C0601
                                       TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H28-A108C0602
                                       TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H28-A108C0603
                                       TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H28-A108C0604
                                       TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£±"
                                       TO  SPA-GMN-W20-SNRHJO1-LBL
               MOVE    CONST-H28-A108C0605
                                       TO  SPA-NAI-W20-SNRHJO1-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£²"
                                       TO  SPA-GMN-W20-SNRHJO2-LBL
               MOVE    CONST-H28-A108C0606
                                       TO  SPA-NAI-W20-SNRHJO2-CD
               MOVE    "±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»"
                                       TO  SPA-GMN-W20-EIYO-LBL
               MOVE    CONST-H28-A108C10
                                       TO  SPA-NAI-W20-EIYO-CD
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H260401 )
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£±"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H26-DRHJO1 (1)
                                           TO  SPA-NAI-W20-DRHJO-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»£²"
                                           TO  SPA-GMN-W20-DRHJO2-LBL
               MOVE    CONST-H26-DRHJO2 (1)
                                           TO  SPA-NAI-W20-DRHJO2-CD
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                           TO SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-H2404-KYUKNGHJ(1)
                                           TO  SPA-NAI-W20-KYUKNGHJ-CD
      *
               MOVE    "Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO SPA-GMN-W20-YKNKYUKNGHJ-LBL
               MOVE    CONST-H26-YKNKYUKNGHJ (1)
                                       TO  SPA-NAI-W20-YKNKYUKNGHJ-CD
      *
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»"
                                       TO SPA-GMN-W20-KNGYKNHICHI-LBL
               MOVE    CONST-H2404-KNGYKNHICHI
                                       TO  SPA-NAI-W20-KNGYKNHICHI-CD
      *
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
      *
               MOVE    "°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»"
                                            TO SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-HYOUKA         TO SPA-NAI-W20-HYOUKA-CD
      *
               MOVE    "µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»"
                                            TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-KJTRIHA        TO SPA-NAI-W20-KJTRIHA-CD
      *
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»"
                                            TO SPA-GMN-W20-KNGHITU-LBL
               MOVE    CONST-H2404-KNGHITU1 TO SPA-NAI-W20-KNGHITU-CD
      *
      *        MOVE    "ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»"
      *                                     TO SPA-GMN-W20-BTUYKZ-LBL
      *        MOVE    CONST-H2404-BTUYKZ   TO SPA-NAI-W20-BTUYKZ-CD
      *
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£±"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H2204-DRTEKCD1  TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H2204-SNRHICH1
                                           TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H2204-SNRHICH2
                                           TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H2204-YKNHICH1
                                           TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H2204-YKNHICH2
                                        TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»"
                                           TO SPA-GMN-W20-ZTKFUKKI-LBL
               MOVE    CONST-H26-ZTKFUKKI
                                        TO  SPA-NAI-W20-ZTKFUKKI-CD
               MOVE    "£Á£Ä£Ì°Ý»ý¸þ¾åÅùÂÎÀ©²Ã»»"
                                        TO SPA-GMN-W20-ADLIJI-LBL
               MOVE    CONST-H26-ADLIJI-A100
                                        TO  SPA-NAI-W20-ADLIJI-CD
               MOVE    "Àº¿À²ÊµÞÀ­´ü°å»ÕÇÛÃÖ²Ã»»"
                                        TO SPA-GMN-W20-SSNKYUDR-LBL
               MOVE    CONST-H26-SSNKYUDR
                                        TO  SPA-NAI-W20-SSNKYUDR-CD
               MOVE    "Àº¿ÀÊÝ·òÊ¡»ã»ÎÇÛÃÖ²Ã»»"
                                        TO SPA-GMN-W20-SSNHKNFUKU-LBL
               MOVE    CONST-H26-SSNHKNFUKU-A103
                                        TO  SPA-NAI-W20-SSNHKNFUKU-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£±"
                                        TO SPA-GMN-W20-SNRHJO1-LBL
               MOVE    CONST-H26-SNRHJO1
                                        TO  SPA-NAI-W20-SNRHJO1-CD
               MOVE    "´Ç¸îÊä½õÇÛÃÖ²Ã»»£²"
                                        TO SPA-GMN-W20-SNRHJO2-LBL
               MOVE    CONST-H26-SNRHJO2
                                        TO  SPA-NAI-W20-SNRHJO2-CD
               MOVE    "±ÉÍÜ´ÉÍý¼Â»Ü²Ã»»"
                                        TO SPA-GMN-W20-EIYO-LBL
               MOVE    CONST-H26-EIYO-A108
                                        TO  SPA-NAI-W20-EIYO-CD
               MOVE    "ÂÎÀ©¶¯²½²Ã»»"   TO SPA-GMN-W20-TAISEI-LBL
               MOVE    CONST-H26-TAISEI
                                        TO  SPA-NAI-W20-TAISEI-CD
               MOVE    "´Ç¸î¿¦°÷ÇÛÃÖ²Ã»»" 
                                        TO SPA-GMN-W20-KNGSKHICH-LBL
               MOVE    CONST-H26-KNGSKHICH
                                        TO  SPA-NAI-W20-KNGSKHICH-CD
               MOVE    "´Ç¸îÊä½õ¼ÔÇÛÃÖ²Ã»»"
                                        TO SPA-GMN-W20-KNGHJSHICH-LBL
               MOVE    CONST-H26-KNGHJSHICH
                                        TO  SPA-NAI-W20-KNGHJSHICH-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H2404-DRHJO (1)
                                           TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                           TO SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-H2404-KYUKNGHJ(1)
                                           TO  SPA-NAI-W20-KYUKNGHJ-CD
      *
               MOVE    "Ìë´ÖµÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                       TO SPA-GMN-W20-YKNKYUKNGHJ-LBL
               MOVE    CONST-H2404-YKNKYUKNGHJ1
                                       TO  SPA-NAI-W20-YKNKYUKNGHJ-CD
      *
               MOVE    "´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»"
                                       TO SPA-GMN-W20-KNGYKNHICHI-LBL
               MOVE    CONST-H2404-KNGYKNHICHI
                                       TO  SPA-NAI-W20-KNGYKNHICHI-CD
      *
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
      *
               MOVE    "°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»"
                                            TO SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-HYOUKA         TO SPA-NAI-W20-HYOUKA-CD
      *
               MOVE    "µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»"
                                            TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-KJTRIHA        TO SPA-NAI-W20-KJTRIHA-CD
      *
               MOVE    "´Ç¸îÉ¬Í×ÅÙ²Ã»»"
                                            TO SPA-GMN-W20-KNGHITU-LBL
               MOVE    CONST-H2404-KNGHITU1 TO SPA-NAI-W20-KNGHITU-CD
      *
      *        MOVE    "ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»"
      *                                     TO SPA-GMN-W20-BTUYKZ-LBL
      *        MOVE    CONST-H2404-BTUYKZ   TO SPA-NAI-W20-BTUYKZ-CD
      *
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£±"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H2204-DRTEKCD1  TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H2204-SNRHICH1
                                           TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H2204-SNRHICH2
                                           TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H2204-YKNHICH1
                                           TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H2204-YKNHICH2
                                            TO  SPA-NAI-W20-YKNHICH2-CD
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H220401 )
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "»ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H2204-DRHJO (1)
                                           TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»"
                                           TO  SPA-GMN-W20-JKAIFUKU-LBL
               MOVE    CONST-JKAIFUKU      TO  SPA-NAI-W20-JKAIFUKU-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£±"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H2204-DRTEKCD1  TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "°å»ÕÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H2204-SNRHICH1
                                           TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H2204-SNRHICH2
                                           TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H2204-YKNHICH1
                                           TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H2204-YKNHICH2
                                            TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»"
                                           TO SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-KYUKNGHJ1     TO  SPA-NAI-W20-KYUKNGHJ-CD
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
               MOVE    "°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»"
                                            TO SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-HYOUKA         TO SPA-NAI-W20-HYOUKA-CD
               MOVE    "µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»"
                                            TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-KJTRIHA        TO SPA-NAI-W20-KJTRIHA-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H200401 )
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "»ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "°å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-DRHJO (1)     TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»"
                                           TO  SPA-GMN-W20-JKAIFUKU-LBL
               MOVE    CONST-JKAIFUKU      TO  SPA-NAI-W20-JKAIFUKU-CD
               MOVE    "°å»ÕÇÛÃÖÅ¬¹ç²Ã»»"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£±"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-SNRHICH1      TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»£²"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-SNRHICH2      TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-YKNHICH1      TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-YKNHICH2      TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H180401 )
      *
               MOVE    "Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»"  TO  SPA-GMN-W20-YKNKNG-LBL
               MOVE    CONST-YKNKNGCD(1)   TO  SPA-NAI-W20-YKNKNG-CD
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»ÎÅù²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "»ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»"  TO  SPA-GMN-W20-HICHTEK-LBL
               MOVE    CONST-KNGTEKCD      TO  SPA-NAI-W20-HICHTEK-CD
      *
               MOVE    "Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»"
                                           TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK-CD
           WHEN    OTHER
               MOVE    "Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»"  TO  SPA-GMN-W20-YKNKNG-LBL
               MOVE    CONST-YKNKNGCD(1)   TO  SPA-NAI-W20-YKNKNG-CD
      *
               MOVE    "¿©Æ²²Ã»»"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ²Ã»»"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "»ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "´Ç¸îÇÛÃÖ²Ã»»"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
           END-EVALUATE
      *
           MOVE    ZERO                    TO  IDX-GKSN
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                                      NOT  =   SPACE )
      *
                   MOVE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                                       TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       IF    ( TNS-HOSPSRYKBN  =   ZERO )
                        OR   ( TNS-HOSPSRYKBN  =   SPA-NAI-W20-HOSPSBT )
                           COMPUTE    IDX-GKSN =   IDX-GKSN +   1
                           MOVE   IDX-GKSN  TO  SPA-NAI-W20-KSNINF-IDX
                                                               (IDX-KSN)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-SYS-KANRI-SAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ¶¦ÄÌ´ØÏ¢¥Á¥§¥Ã¥¯
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    ÉÂÅïÈÖ¹æ¤¬ÆþÎÏ¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-GMN-W20-BTUNUM  =   ZERO )
               MOVE    "1101"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    ÉÂÅïÌ¾¾Î¤¬ÆþÎÏ¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-GMN-W20-NAME    NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1007"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    Ã»½ÌÉÂÅïÌ¾¤¬ÆþÎÏ¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-GMN-W20-TANNAME NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1008"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    ÉÂ±¡¤Î¾ì¹ç¡¢ÉÂ±¡¤Î¼ïÊÌ¤¬ÁªÂò¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-NAI-W20-HOSPSBT     =   1 )
               IF    ( SPA-GMN-W20-SBT     =   "01"
                                           OR  "02"
                                           OR  "03"
                                           OR  "04"
                                           OR  "05"
                                           OR  "06"
                                           OR  "07"
                                           OR  "10" )
                   CONTINUE
               ELSE
                   MOVE    "1106"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-W20-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    ¿ÇÎÅ½ê¤Î¾ì¹ç¡¢¿ÇÎÅ½ê¤Î¼ïÊÌ¤¬ÁªÂò¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-NAI-W20-HOSPSBT     =   2 )
               IF    ( SPA-GMN-W20-SBT     =   "08"
                                           OR  "09" )
                   CONTINUE
               ELSE
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-W20-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    Í­¸ú´ü´Ö³«»ÏÆü¤¬ÆþÎÏ¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-NAI-W20-YUKOSTYMD   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1102"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    Í­¸ú´ü´Ö½ªÎ»Æü¤¬ÆþÎÏ¤µ¤ì¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-NAI-W20-YUKOEDYMD   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1103"      TO  SPA-ERRCD
               MOVE    52          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    Í­¸ú´ü´Ö³«»ÏÆü¡åÍ­¸ú´ü´Ö½ªÎ»Æü¤È¤Ê¤Ã¤Æ¤¤¤ë¤³¤È
           IF    ( SPA-NAI-W20-YUKOSTYMD   <=  SPA-NAI-W20-YUKOEDYMD )
               CONTINUE
           ELSE
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   CONST-KANSATU )
               CONTINUE
           ELSE
      *
      *        ÆÃÄêÆþ±¡ÎÁ¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤Ê¤¤¾ì¹çÉÂÅï¤´¤È¤ËÉ¬¿Ü¹àÌÜ¤ò
      *        ¥Á¥§¥Ã¥¯¤¹¤ë
               IF    ( SPA-GMN-W20-TOKNYUIN-G      =   SPACE )
      *
      *            ÉÂÅï¼ïÊÌ¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤ë¤³¤È
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( SPA-GMN-W20-SBT     NOT =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "1009"  TO  SPA-ERRCD
                           MOVE    6       TO  SPA-GMN-W20-CUR
                       END-IF
                   END-IF
      *
      *            Æþ±¡´ðËÜÎÁ¤¬»»Äê¤µ¤ì¤Æ¤¤¤ë¤³¤È
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( SPA-GMN-W20-KHNRYO   =   SPACE )
                           IF    ( SPA-GMN-W20-SBT  =   "04" OR "09"  )
                            AND  ( SPA-NAI-W20-YUKOSTYMD
                                                   >=  CONST-H180701 )
                               CONTINUE
                           ELSE
                               MOVE    "1014"  TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁÁªÂò»þ¡¢ÉÂÅï¼ïÊÌ¤Ï°ìÈÌÉÂÅï¤Ç¤¢¤ë¤³¤È
               IF    ( SPA-NAI-W20-TOKNYUIN    =   "33" OR "34")
                   IF    ( SPA-GMN-W20-SBT NOT =   "01" )
                       MOVE    "1112"  TO  SPA-ERRCD
                       MOVE    6       TO  SPA-GMN-W20-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-W20-BTUTYPE    NOT =   SPACE )
                OR   ( SPA-GMN-W20-BTUCD      NOT =   ZERO )
                   IF    ( SPA-GMN-W20-BTUTYPE    =   SPACE )
                       MOVE    "1114"  TO  SPA-ERRCD
                       MOVE    57      TO  SPA-GMN-W20-CUR
                   END-IF
                   IF    ( SPA-GMN-W20-BTUCD      =   ZERO )
                       MOVE    "1114"  TO  SPA-ERRCD
                       MOVE    58      TO  SPA-GMN-W20-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-W20-TBTUTYPE    NOT =   SPACE )
                OR   ( SPA-GMN-W20-TBTUCD      NOT =   ZERO )
                   IF    ( SPA-GMN-W20-TBTUTYPE    =   SPACE )
                       MOVE    "1114"  TO  SPA-ERRCD
                       MOVE    59      TO  SPA-GMN-W20-CUR
                   END-IF
                   IF    ( SPA-GMN-W20-TBTUCD      =   ZERO )
                       MOVE    "1114"  TO  SPA-ERRCD
                       MOVE    60      TO  SPA-GMN-W20-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( SPA-NAI-W20-YUKOSTYMD
                                  >=  CONST-H260401 )
      *        H26.4°Ê¹ßÀßÄê²ÄÇ½¤Ê²Ã»»¤Î¿ô¤¬¥·¥¹¥Æ¥à´ÉÍý¤ÎÎÎ°è(14)¤òÄ¶¤¨¤¿
      *        ¤³¤È¤Ë¤è¤ëÂÐ±þ
               MOVE    ZERO            TO  CNT-SANTEI
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                       UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
                   IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                       >   ZERO )
                       IF    ( SPA-GMN-W20-KSNCD (IDX-KSN)
                                   NOT =   "1" AND SPACE )
                           COMPUTE CNT-SANTEI
                               =   CNT-SANTEI  +   1
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( CNT-SANTEI  >   CONST-5001-KSN-MAX )
                   MOVE    "1113"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4901-SSTKIJUN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-KSNMEI
      *
      *    »ÜÀß´ð½à¸¡º÷½èÍý
           PERFORM 4100-SYS-1006-SEL-SEC
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
           IF    ( SPA-GMN-W20-KNGHJO      =   "00" )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-LOOPEND
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                       UNTIL ( IDX-KSN >   CONST-KNGHJOCD-MAX )
                        OR   ( FLG-LOOPEND     =   1 )
                   IF    ( SPA-GMN-W20-KNGHJO
                                       =   CONST-KNGHJOCDL (IDX-KSN))
      *
                       MOVE    CONST-KNGHJOCD (IDX-KSN)
                                       TO  WRK-KSNCD
                       PERFORM 41011-SSTKIJUN-CHK-SEC
                       IF    ( FLG-KSNERR      =   ZERO )
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
      *
                   END-IF
               END-PERFORM
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-KSNERR      =   1 )
                       MOVE    "1011"      TO  SPA-ERRCD
                       MOVE    9           TO  SPA-GMN-W20-CUR
                       MOVE    SPA-GMN-W20-KNGHJOMEI
                                       TO  WRK-KSNMEI
      *
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H240401 )
                   PERFORM 49012-SSTKIJUN-CHK-SEC
               ELSE
                   PERFORM 49011-SSTKIJUN-CHK-SEC
               END-IF
           END-IF
      *
           .
       4901-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý (¡ÁH24.3)
      *****************************************************************
       49011-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
                   MOVE    SPACE   TO  WRK-KSNCD
      *
                   EVALUATE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                   WHEN    CONST-YKNKNGCD (1)
                       EVALUATE    SPA-GMN-W20-YKNKNG
                       WHEN    "1"
                           MOVE    CONST-YKNKNGCD (1)
                                           TO  WRK-KSNCD
                       WHEN    "2"
                           MOVE    CONST-YKNKNGCD (2)
                                           TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-YKNKNGCD (3)
                                           TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-YKNKNGCD (4)
                                           TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-YKNKNGCD (5)
                                           TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-SYOKUDOCD
                       CONTINUE
                   WHEN    CONST-HOIKUCD
                       IF    ( SPA-GMN-W20-HOIKU   =   "2" )
                           MOVE    CONST-HOIKUCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-NANBYO
                       IF    ( SPA-GMN-W20-NANBYO   =   "2" )
                           MOVE    CONST-NANBYO
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-TOKUSYUCD
                       IF    ( SPA-GMN-W20-TOKUSYU  =   "2" )
                           MOVE    CONST-TOKUSYUCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-JIDOCD
                       IF    ( SPA-GMN-W20-JIDO     =   "2" )
                           MOVE    CONST-JIDOCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-SEISINCD
                       IF    ( SPA-GMN-W20-SEISIN   =   "2" )
                           MOVE    CONST-SEISINCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-KANGOCD (1)
                       IF    ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H180401 )
                          IF    ( SPA-GMN-W20-HICH    =   "2" )
                              MOVE    CONST-KANGOCD (1)
                                                      TO  WRK-KSNCD
                          END-IF
                       ELSE
                           PERFORM VARYING IDX1    FROM    1   BY  1
                                   UNTIL ( IDX1    >   5 )
                                    OR   ( WRK-KSNCD   NOT =   SPACE )
      *
                               IF    ( SPA-GMN-W20-HICH
                                               =   CONST-KANGOCDL(IDX1))
                                   MOVE    CONST-KANGOCD (IDX1)
                                               TO  WRK-KSNCD
                               END-IF
      *
                           END-PERFORM
                       END-IF
                   WHEN    CONST-H2204-DRTEKCD1
                           IF    ( SPA-GMN-W20-DRTEK   =  "2" )
                               MOVE    CONST-H2204-DRTEKCD1
                                               TO  WRK-KSNCD
                           END-IF
                   WHEN    CONST-DRTEKCD
                       IF    ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H220401)
                           IF    ( SPA-GMN-W20-DRTEK2  =  "2" )
                               MOVE    CONST-DRTEKCD
                                               TO  WRK-KSNCD
                           END-IF
                       ELSE
                           IF    ( SPA-GMN-W20-DRTEK   =  "2" )
                               MOVE    CONST-DRTEKCD
                                               TO  WRK-KSNCD
                           END-IF
                       END-IF
                   WHEN    CONST-H2204-DRHJO (1)
                       EVALUATE    SPA-GMN-W20-DRHJO
                       WHEN    "2"
                           MOVE    CONST-H2204-DRHJO (1)
                                                   TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-H2204-DRHJO (2)
                                                   TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-H2204-DRHJO (3)
                                                   TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-H2204-DRHJO (4)
                                                   TO  WRK-KSNCD
                       WHEN    "6"
                           MOVE    CONST-H2204-DRHJO (5)
                                                   TO  WRK-KSNCD
                       WHEN    "7"
                           MOVE    CONST-H2204-DRHJO (6)
                                                   TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-DRHJO (1)
                       EVALUATE    SPA-GMN-W20-DRHJO
                       WHEN    "2"
                           MOVE    CONST-DRHJO (1) TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-DRHJO (2) TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-DRHJO (3) TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-DRHJO (4) TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-JKAIFUKU
                       IF   (  SPA-GMN-W20-JKAIFUKU    =   "2" )
                           MOVE    CONST-JKAIFUKU  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-YKNKNKYU
                       IF   (  SPA-GMN-W20-YKNKNKYU    =   "2" )
                           MOVE    CONST-YKNKNKYU  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-SNRHICH1
                   WHEN    CONST-H2204-SNRHICH2
                       IF    ( SPA-NAI-W20-YUKOSTYMD  >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-SNRHICH2    =   "2" )
                               MOVE    CONST-H2204-SNRHICH2
                                                       TO  WRK-KSNCD
                           END-IF
                       ELSE
                           IF    ( SPA-GMN-W20-SNRHICH1    =   "2" )
                               MOVE    CONST-SNRHICH1  TO  WRK-KSNCD
                           END-IF
                       END-IF
                   WHEN    CONST-H2204-SNRHICH1
                   WHEN    CONST-SNRHICH2
                   WHEN    CONST-KNGTEKCD
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-SNRHICH1    =   "2" )
                               MOVE    CONST-H2204-SNRHICH1
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H200401 )
                           IF    ( SPA-GMN-W20-SNRHICH2  =   "2" )
                               MOVE    CONST-SNRHICH2  TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-HICHTEK     =   "2" )
                               MOVE    CONST-KNGTEKCD  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-YKNHICH1
                   WHEN    CONST-H2204-YKNHICH2
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-YKNHICH2    =   "2" )
                               MOVE    CONST-H2204-YKNHICH2
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-YKNHICH1    =   "2" )
                               MOVE    CONST-YKNHICH1  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-YKNHICH2
                   WHEN    CONST-H2204-YKNHICH1
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-YKNHICH1    =   "2" )
                               MOVE    CONST-H2204-YKNHICH1
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-YKNHICH2    =   "2" )
                               MOVE    CONST-YKNHICH2  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-KYUKNGHJ1
                       EVALUATE    SPA-GMN-W20-KYUKNGHJ
                       WHEN    "2"
                           MOVE    CONST-KYUKNGHJ1  TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-KYUKNGHJ2  TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-SEICHIIKI
                       IF    ( SPA-GMN-W20-SEICHIIKI    =   "2" )
                           MOVE    CONST-SEICHIIKI  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-HYOUKA
                       IF    ( SPA-GMN-W20-HYOUKA   =   "2" )
                           MOVE    CONST-HYOUKA     TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-KJTRIHA
                       IF    ( SPA-GMN-W20-KJTRIHA   =   "2" )
                           MOVE    CONST-KJTRIHA     TO  WRK-KSNCD
                       END-IF
                   END-EVALUATE
      *
                   IF    ( WRK-KSNCD       NOT =   SPACE )
                       PERFORM 41011-SSTKIJUN-CHK-SEC
      *
                       IF    ( SPA-ERRCD           =   SPACE )
                           IF    ( FLG-KSNERR      =   1 )
                               MOVE    "1011"  TO  SPA-ERRCD
                               COMPUTE SPA-GMN-W20-CUR
                                   =   SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   +   9
                               MOVE    SPA-GMN-W20-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                           END-IF
                       END-IF
      *
                       IF    ( SPA-ERRCD           =   SPACE )
                           IF    ( FLG-SSTKIJUNERR     =   1 )
                               MOVE    "1010"          TO  SPA-ERRCD
                               COMPUTE SPA-GMN-W20-CUR
                                   =   SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   +   300
                               MOVE    SPA-GMN-W20-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       49011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý (H24.4¡Á)
      *****************************************************************
       49012-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   TKSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               MOVE    TKSN-SRYCD (IDX-KSN)    TO  WRK-KSNCD
      *
               PERFORM 41011-SSTKIJUN-CHK-SEC
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-KSNERR      =   1 )
                       MOVE    "1011"  TO  SPA-ERRCD
                       COMPUTE SPA-GMN-W20-CUR
                           =   TKSN-IDX (IDX-KSN)
                           +   300
                       MOVE    TKSN-LBL (IDX-KSN)
                                       TO  WRK-KSNMEI
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-SSTKIJUNERR     =   1 )
                       MOVE    "1010"      TO  SPA-ERRCD
                       COMPUTE SPA-GMN-W20-CUR
                           =   TKSN-IDX (IDX-KSN)
                           +   300
                       MOVE    TKSN-LBL (IDX-KSN)
                                       TO  WRK-KSNMEI
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       49012-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-SSTKJNCD-G
      *
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
                                       FLG-KSNERR
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD      >=   CONST-H280401 )
               PERFORM 410111-H28-SSTKIJUN-CHK-SEC
           WHEN  OTHER
               PERFORM 410111-SSTKIJUN-CHK-SEC
           END-EVALUATE
      *
      *
           MOVE    WRK-KSNCD                   TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           MOVE    ZERO            TO  FLG-HIT
                                       CNT-SST
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   10 )
                    OR   ( FLG-HIT     =   1 )
      *
               IF    ( TNS-SSTKIJUNCD (IDX5)   >   ZERO )
                   COMPUTE CNT-SST =   CNT-SST +   1
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  WRK-SSTKJNCD9(CNT-SST)
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  IDX-SST
                   IF    ( SSTKJN-CD (IDX-SST) =   "1" )
                       MOVE    1   TO  FLG-HIT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SST     >   ZERO )
               IF    ( FLG-HIT =   ZERO )
                   MOVE        1   TO  FLG-SSTKIJUNERR
               END-IF
           END-IF
      *
           .
       41011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       410111-H28-SSTKIJUN-CHK-SEC      SECTION.
      *
           IF    ( SPA-NAI-W20-TOKSRYCD    NOT =   SPACE )
      *
               MOVE    SPA-NAI-W20-TOKSRYCD
                                           TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               INITIALIZE                          SNCHUKSN-AREA
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  SNCHUKSN-BASEYMD
               MOVE    TNS-CDKBN-KBNNUM        TO  SNCHUKSN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA    TO  SNCHUKSN-EDANUM
               MOVE    WRK-KSNCD               TO  SNCHUKSN-KSNCD
               CALL    "ORCSNCHUKSN"               USING
                                                   SNCHUKSN-AREA
                                                   SPA-AREA
               IF    ( SNCHUKSN-KSNCD      NOT =   SPACE )
                   MOVE    SNCHUKSN-KSNCD      TO  WRK-KSNCD
               ELSE
                   PERFORM 4101111-H28-SSTKIJUN-CHK-SEC
               END-IF
           ELSE
               PERFORM 4101111-H28-SSTKIJUN-CHK-SEC
           END-IF
      *
           .
       410111-H28-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4101111-H28-SSTKIJUN-CHK-SEC        SECTION.
      *
           MOVE    SPA-NAI-W20-KHNSRYCD    TO  WRK-KHNCD
           PERFORM 800-DUMMYCD-CHG-SEC
           MOVE    WRK-KHNCD               TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           INITIALIZE                          SNCHUKSN-AREA
           MOVE    SPA-NAI-W20-YUKOSTYMD   TO  SNCHUKSN-BASEYMD
           MOVE    TNS-CDKBN-KBNNUM        TO  SNCHUKSN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA    TO  SNCHUKSN-EDANUM
           MOVE    WRK-KSNCD               TO  SNCHUKSN-KSNCD
           CALL    "ORCSNCHUKSN"               USING
                                               SNCHUKSN-AREA
                                               SPA-AREA
           IF    ( SNCHUKSN-KSNCD      NOT =   SPACE )
               MOVE    SNCHUKSN-KSNCD      TO  WRK-KSNCD
           END-IF
      *
           .
       4101111-H28-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       410111-SSTKIJUN-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-KSNCD-A105
                                       WRK-KSNCD-A104
           EVALUATE    WRK-KSNCD
           WHEN    CONST-HYOUKA
               MOVE    CONST-HYOUKA-SEN         TO  WRK-KSNCD-A105
               MOVE    CONST-HYOUKA-TOK         TO  WRK-KSNCD-A104
           WHEN    CONST-H2404-KNGHITU1
               MOVE    CONST-H2404-KNGHITU1-SEN TO  WRK-KSNCD-A105
               MOVE    CONST-H2404-KNGHITU1-TOK TO  WRK-KSNCD-A104
           WHEN    CONST-H2404-KNGHITU2
               MOVE    CONST-H2404-KNGHITU2-SEN TO  WRK-KSNCD-A105
               MOVE    CONST-H2404-KNGHITU2-TOK TO  WRK-KSNCD-A104
           WHEN    CONST-H26-ADLIJI-A100
               MOVE    CONST-H26-ADLIJI-A105    TO  WRK-KSNCD-A105
               MOVE    CONST-H26-ADLIJI-A104    TO  WRK-KSNCD-A104
           END-EVALUATE
      *
           IF    ( WRK-KSNCD-A105   NOT =   SPACE )
               IF    ( SPA-GMN-W20-SBT =   "06" )
                   MOVE    WRK-KSNCD-A105   TO  WRK-KSNCD
               ELSE
                   MOVE    SPA-NAI-W20-KHNSRYCD
                                           TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( TNS-CDKBN-KBN       =   "A" )
                     AND ( TNS-CDKBN-KBNNUM    =   104 )
                       MOVE    WRK-KSNCD-A104
                                           TO  WRK-KSNCD
                   END-IF
               END-IF
           END-IF
      *
      *    ÀìÌç¡¢ÆÃÄêµ¡Ç½ÉÂ±¡°Ê³°¤ÎÃí²Ã»»ÃÖ¤­´¹¤¨
      *    Æþ±¡´ðËÜÎÁ¤ÈÆÃÄêÆþ±¡ÎÁ¤ÎÎ¾Êý¤Ë³Ý¤«¤ëÃí²Ã»»¤Î¾ì¹ç¡¢
      *    ÆÃÄêÆþ±¡ÎÁ¤Ç¥Á¥§¥Ã¥¯¤¹¤ë¤³¤È¤Ë¤¹¤ë¡£
           EVALUATE    WRK-KSNCD
           WHEN    CONST-H26-SSNHKNFUKU-A103
               IF    ( SPA-NAI-W20-TOKNYUIN    =   "07" )
                   MOVE    CONST-H26-SSNHKNFUKU-A312
                                           TO  WRK-KSNCD
               END-IF
           WHEN    CONST-H26-EIYO-A108
               IF    ( SPA-GMN-W20-SBT =   "09" )
                   MOVE    CONST-H26-EIYO-A109
                                           TO  WRK-KSNCD
               END-IF
           END-EVALUATE
      *
           .
       410111-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ÜÀß´ð½à¸¡º÷½èÍý
      *****************************************************************
       4100-SYS-1006-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  SSTKJN-AREA
      *
      *    »ÜÀß´ð½à¾ðÊó¸¡º÷
           INITIALIZE  SYSKANRI-REC
           MOVE    "1006"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1006-REC
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1006-KBNCD (2:)         =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "1" )
                AND  ( SYS-1006-KBNCD  <=  "9" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN1-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN1)
               WHEN  ( SYS-1006-KBNCD (2:1)    NOT =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "10" )
                AND  ( SYS-1006-KBNCD  <=  "19" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN2-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN2)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-SYS-1006-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅïÈÖ¹æ¸¡º÷½èÍý
      *****************************************************************
       4100-BTUNUM-LSTSEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-BTULST
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX-BTULST  >   ZERO )
                OR       ( IDX1        >   SPA-GMN-W20-BTULST-MAX )
      *
      *
               IF    ( SPA-GMN-W20-TBTUNUM (IDX1)
                                       =   SPA-GMN-W20-BTUSELNUM )
                   MOVE    IDX1    TO  IDX-BTULST
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-BTUNUM-LSTSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKNYUINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-W20-TOKNYUINL-TANMEI (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W20-TOKNYUINL-SRYCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W20-TOKNYUINL-KBNCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKUTEINYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄêÆþ±¡ÎÁ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4201-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKNYUINL-KBNCD (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-W20-TOKNYUINL-TANMEI (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W20-TOKNYUINL-SRYCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W20-TOKNYUINL-KBNCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4201-TOKUTEINYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²Ê¥³¡¼¥É¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SRYKALST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SRYKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SRYKAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SRYKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¼ïÊÌ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SBTLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SBT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SBTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SBTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SBTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÊä½õ²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-KNGHJOLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KNGHJO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KNGHJOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KNGHJOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KNGHJOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-YAKANKNGLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNKNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNKNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNKNGLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-YKNKNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YAKANKNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¿©Æ²²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SYOKUDOLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOKUDO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SYOKUDOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOKUDOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOKUDOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKUDOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ°é»Î¡¦¥×¥ì¥¤¥ë¡¼¥à²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-HOIKULST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HOIKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HOIKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HOIKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HOIKULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HOIKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆñÉÂ´µ¼ÔÅùÆþ±¡¿ÇÎÅ²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-NANBYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-NANBYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-NANBYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-NANBYOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-NANBYOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NANBYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃ¼ì¼À´µÆþ±¡»ÜÀß´ÉÍý²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-TOKUSYULST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKUSYU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKUSYUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKUSYULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-TOKUSYULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TOKUSYULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    »ùÆ¸¡¦»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-JIDOLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-JIDO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-JIDOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-JIDOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-JIDOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JIDOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²Ê±þµÞÆþ±¡»ÜÀß´ÉÍý²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SEISINLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SEISIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SEISINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SEISINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SEISINLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SEISINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-HICHLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HICH-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HICHL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HICHLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HICHLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HICHLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-DRTEKLST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRTEK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRTEKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRTEKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRTEKLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRTEKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    °å»ÕÇÛÃÖ²Ã»»£²¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-DRTEK2LST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRTEK2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRTEK2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRTEK2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRTEK2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRTEK2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-HICHTEKLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HICHTEK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HICHTEKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HICHTEKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HICHTEKLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HICHTEKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    °å»Õ»öÌ³ºî¶ÈÊä½õÂÎÀ©²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-DRHJOLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRHJO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRHJOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRHJOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRHJOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRHJOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-JKAIFUKULST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-JKAIFUKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-JKAIFUKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-JKAIFUKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-JKAIFUKULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JKAIFUKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-YKNKNKYULST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNKNKYU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNKNKYUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNKNKYULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNKNKYULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNKNKYULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê¿ÇÎÅ½ê¡Ë¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SNRHICH1LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SNRHICH1-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SNRHICH1L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SNRHICH1LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SNRHICH1LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SNRHICH1LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê¿ÇÎÅ½ê¡Ë¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SNRHICH2LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SNRHICH2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SNRHICH2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SNRHICH2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SNRHICH2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SNRHICH2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-YKNHICH1LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNHICH1-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNHICH1L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNHICH1LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNHICH1LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNHICH1LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-YKNHICH2LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNHICH2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNHICH2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNHICH2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNHICH2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNHICH2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ºßÂðÉüµ¢µ¡Ç½¶¯²½²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-ZTKFUKKILST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-ZTKFUKKI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-ZTKFUKKIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-ZTKFUKKILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-ZTKFUKKILST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZTKFUKKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-KYUKNGHJLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KYUKNGHJ-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KYUKNGHJL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KYUKNGHJLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-KYUKNGHJLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KYUKNGHJLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Àº¿À²ÊÃÏ°è°Ü¹Ô¼Â»Ü²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SEICHIIKILST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SEICHIIKI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SEICHIIKIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SEICHIIKILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SEICHIIKILST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SEICHIIKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-HYOUKALST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HYOUKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HYOUKAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HYOUKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HYOUKALST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HYOUKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-KJTRIHALST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KJTRIHA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KJTRIHAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KJTRIHALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-KJTRIHALST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KJTRIHALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ì¤´ð½à¡ÊÉÂÅï¤Ë·¸¤ë´ð½à¤ò¤ß¤¿¤µ¤Ê¤¤¾ì¹ç¡Ë¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-BTUMIKJNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-BTUMIKJN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-BTUMIKJNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-BTUMIKJNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-BTUMIKJNLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-BTUMIKJNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-SYOUBTUBRMLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOUBTUBRM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    CONST-SYOUBTUBRMCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOUBTUBRMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4201-SYOUBTUBRMLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOUBTUBRM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( CONST-SYOUBTUBRMCD (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    CONST-SYOUBTUBRMCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4201-SYOUBTUBRMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-RECEDSPKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-RECEDSPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-RECEDSPKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4200-RECEDSPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÅï¾õÂÖ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-BTUTYPELST-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-BTUTYPE-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-BTUTYPEL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-BTUTYPELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BTUTYPELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-YKNCHOKAKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNCHOKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNCHOKAKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNCHOKAKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
            AND  ( SPA-NAI-W20-YKNCHOKACHK-G NOT =  SPACE )
               MOVE    SPA-GMN-W20-YKNCHOKAKBNL (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNCHOKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4201-RECEDSPKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-RECEDSPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4201-RECEDSPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-RYOYOKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-RYOYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-RYOYOKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-RYOYOKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-RYOYOKBNLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RYOYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»¶èÊ¬¶èÊ¬¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-EDKBNLST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-EDKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-EDKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-EDKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-EDKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ðËÜÎÁ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4200-KHNRYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KHNRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KHNRYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W20-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
      *
                   MOVE    SPA-NAI-W20-KHNTENLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM3
      *
                   MOVE    SPA-NAI-W20-NYUKHNKBNLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM4
      *
                   MOVE    SPA-NAI-W20-YKNCHOKACHKLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM5
                   MOVE    SPA-NAI-W20-TOKHOSPLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM6
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KHNRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ´ðËÜÎÁ¥³¥ó¥Ü¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       4201-KHNRYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KHNRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W20-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
      *
                   MOVE    SPA-NAI-W20-KHNTENLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM3
      *
                   MOVE    SPA-NAI-W20-NYUKHNKBNLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM4
      *
                   MOVE    SPA-NAI-W20-YKNCHOKACHKLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM5
                   MOVE    SPA-NAI-W20-TOKHOSPLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM6
               END-IF
      *
           END-PERFORM
      *
           .
       4201-KHNRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ¼«²èÌÌÊÔ½¸½èÍý
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W20    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÌÌÊÔ½¸½èÍý
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    W20-BTULST-ROW          TO  WRK-BTULST-ROW
      *
           MOVE    SPACE                   TO  W20
           INITIALIZE                          W20
           INITIALIZE                          TGKSN-AREA
      *
           MOVE    SPA-GMN-W20-KSN-PAGENO  TO  W20-KSN-PAGENO
      *
      *    ÉÂ¼¼¥Ü¥¿¥ó
           MOVE    SPA-GMN-W20-B09         TO  W20-B09
      *
      *    Æþ±¡ÎÁ²Ã»»¥Ü¥¿¥ó
           IF    ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
               MOVE    "Æþ±¡ÎÁ²Ã»»"        TO  W20-B10
           END-IF
      *
      *    ´ð½àÆü
           MOVE    SPA-GMN-W20-KJNYMD      TO  W20-KJNYMD
      *
      *    ÉÂÅïÁªÂòÈÖ¹æ
           MOVE    SPA-GMN-W20-BTUSELNUM   TO  W20-BTUSELNUM
      *
      *    ÉÂÅïÈÖ¹æ
           MOVE    SPA-GMN-W20-BTUNUM      TO  W20-BTUNUM
      *
      *    ÉÂÅïÌ¾¾Î
           MOVE    SPA-GMN-W20-NAME        TO  W20-NAME
      *
      *    Ã»½ÌÉÂÅïÌ¾
           MOVE    SPA-GMN-W20-TANNAME     TO  W20-TANNAME
      *
      *    ÆÃÄêÆþ±¡ÎÁ¥³¥ó¥Ü
           MOVE    SPA-GMN-W20-TOKNYUIN-G  TO  W20-TOKUTEINYUIN
           MOVE    SPA-GMN-W20-TOKNYUIN-MAX
                                           TO  W20-TOKUTEINYUIN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-TOKUTEINYUIN-CNT )
                MOVE   SPA-GMN-W20-TOKNYUINLST (IDX1)
                                   TO  W20-TOKUTEINYUIN-ITM (IDX1)
           END-PERFORM
      *
      *    ÉÂÅï²ÊÌ¾¥³¥ó¥Ü
           MOVE    SPA-GMN-W20-SRYKA-G     TO  W20-SRYKA
           MOVE    SPA-GMN-W20-SRYKA-MAX   TO  W20-SRYKA-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-SRYKA-CNT )
                MOVE   SPA-GMN-W20-SRYKALST (IDX1)
                                   TO  W20-SRYKA-ITM (IDX1)
           END-PERFORM
      *
      *    ÉÂÅï¼ïÊÌ¥³¥ó¥Ü
           MOVE    SPA-GMN-W20-SBT-G   TO  W20-SBT
           MOVE    SPA-GMN-W20-SBT-MAX TO  W20-SBT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-SBT-CNT )
                MOVE   SPA-GMN-W20-SBTLST (IDX1)
                                   TO  W20-SBT-ITM (IDX1)
           END-PERFORM
      *
      *    Ìë¶ÐÄ¶²á¸º»»¶èÊ¬
           IF    ( SPA-NAI-W20-YKNCHOKACHK-G NOT =  SPACE )
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H280401 )
                   MOVE    "Ìë¶Ð»þ´ÖÄ¶²á¸º»»Åù"
                                       TO W20-LBLYKNCHOKAKBN
               WHEN  ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H260401 )
                   MOVE    "Ìë¶Ð»þ´ÖÄ¶²á¸º»»"
                                       TO W20-LBLYKNCHOKAKBN
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-GMN-W20-YKNCHOKAKBN-G
                                       TO  W20-YKNCHOKAKBN
           MOVE    SPA-GMN-W20-YKNCHOKAKBN-MAX
                                       TO  W20-YKNCHOKAKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-YKNCHOKAKBN-CNT )
                MOVE   SPA-GMN-W20-YKNCHOKAKBNLST (IDX1)
                                       TO  W20-YKNCHOKAKBN-ITM (IDX1)
           END-PERFORM
      *
      *    ¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬
           MOVE    SPA-GMN-W20-RECEDSPKBN-G    TO  W20-RECEDSPKBN
           MOVE    SPA-GMN-W20-RECEDSPKBN-MAX
                                       TO  W20-RECEDSPKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-RECEDSPKBN-CNT )
                MOVE   SPA-GMN-W20-RECEDSPKBNLST (IDX1)
                                   TO  W20-RECEDSPKBN-ITM (IDX1)
           END-PERFORM
      *
      *    ÉÂÅïµ¡Ç½¡ÊÆþ±¡ÎÁ¡Ë
           MOVE    SPA-GMN-W20-BTUTYPE-G    TO  W20-BTUTYPE
           MOVE    SPA-GMN-W20-BTUTYPE-MAX
                                       TO  W20-BTUTYPE-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-BTUTYPE-CNT )
                MOVE   SPA-GMN-W20-BTUTYPELST (IDX1)
                                   TO  W20-BTUTYPE-ITM (IDX1)
           END-PERFORM
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÆþ±¡ÎÁ¡Ë
           MOVE    SPA-GMN-W20-BTUCD   TO  W20-BTUCD
      *
      *    ÉÂÅïµ¡Ç½¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           MOVE    SPA-GMN-W20-TBTUTYPE-G
                                       TO  W20-TBTUTYPE
           MOVE    W20-BTUTYPE-LST     TO  W20-TBTUTYPE-LST
      *
      *    ÉÂÅï¥³¡¼¥É¡ÊÆÃÄêÆþ±¡ÎÁ¡Ë
           MOVE    SPA-GMN-W20-TBTUCD  TO  W20-TBTUCD
      *
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ»»Äê¶èÊ¬
           PERFORM 42011-RYOYOSANTEI-CHK-SEC
           IF    ( FLG-RYOYOSANTEI =   1 )
               IF   ( SPA-GMN-W20-SBT      =  "08" OR "09" )
                   MOVE    "°ìÈÌ¡¦ÎÅÍÜÁê¸ß»»Äê"    TO W20-LBLRYOYOKBN
               ELSE
                   MOVE    "£¹£°ÆüÄ¶¤¨¤Î»»Äê"      TO W20-LBLRYOYOKBN
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-W20-RYOYOKBN-G
                                       TO  W20-RYOYOKBN
           MOVE    SPA-GMN-W20-RYOYOKBN-MAX
                                       TO  W20-RYOYOKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-RYOYOKBN-CNT )
                MOVE   SPA-GMN-W20-RYOYOKBNLST (IDX1)
                                       TO  W20-RYOYOKBN-ITM (IDX1)
           END-PERFORM
      *
      *    Í­¸ú´ü´Ö³«»ÏÆü
           MOVE    SPA-GMN-W20-YUKOSTYMD   TO  W20-YUKOSTYMD
      *
      *    Í­¸ú´ü´Ö½ªÎ»Æü
           MOVE    SPA-GMN-W20-YUKOEDYMD   TO  W20-YUKOEDYMD
      *
      *    ÉÂÅï½ªÎ»¶èÊ¬
           MOVE    SPA-GMN-W20-EDKBN-G     TO  W20-EDKBN
           MOVE    SPA-GMN-W20-EDKBN-MAX   TO  W20-EDKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-EDKBN-CNT )
                MOVE   SPA-GMN-W20-EDKBNLST (IDX1)
                                   TO  W20-EDKBN-ITM (IDX1)
           END-PERFORM
      *
      *    Æþ±¡´ðËÜÎÁ¥³¥ó¥Ü
           MOVE    SPA-GMN-W20-KHNRYO-G    TO  W20-KHNRYO
           MOVE    SPA-GMN-W20-KHNRYO-MAX  TO  W20-KHNRYO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-KHNRYO-CNT )
                MOVE   SPA-GMN-W20-KHNRYOLST (IDX1)
                                   TO  W20-KHNRYO-ITM (IDX1)
           END-PERFORM
      *
      *    Æþ±¡´ðËÜÅÀ¿ô
           MOVE    SPA-NAI-W20-KHNTEN      TO  W20-KHNTEN
      *
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT         =   CONST-KANSATU )
      *        ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê´Ç¸îÊä½õ²Ã»»¥³¥ó¥Ü¤ò¼ÚÍÑ¡Ë
               MOVE    "¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»"
                                       TO  W20-LBLKNGHJO
               MOVE    SPA-GMN-W20-SYOUBTUBRM-G
                                       TO  W20-KNGHJO
               MOVE    SPA-GMN-W20-SYOUBTUBRM-MAX
                                       TO  W20-KNGHJO-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   W20-KNGHJO-CNT )
                    MOVE   SPA-GMN-W20-SYOUBTUBRMLST (IDX1)
                                   TO  W20-KNGHJO-ITM (IDX1)
               END-PERFORM
           WHEN  ( SPA-NAI-W20-HOSPSBT     =   1 )
      *
               MOVE    "´Ç¸îÊä½õ²Ã»»"      TO  W20-LBLKNGHJO
      *
      *        ´Ç¸îÊä½õ²Ã»»¥³¥ó¥Ü
               MOVE    SPA-GMN-W20-KNGHJO-G
                                       TO  W20-KNGHJO
               MOVE    SPA-GMN-W20-KNGHJO-MAX
                                       TO  W20-KNGHJO-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   W20-KNGHJO-CNT )
                    MOVE   SPA-GMN-W20-KNGHJOLST (IDX1)
                                   TO  W20-KNGHJO-ITM (IDX1)
               END-PERFORM
           END-EVALUATE
      *
      *    Æþ±¡ÎÁ²Ã»»
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   TO  IDXG
      *
               IF    ( IDXG    >   ZERO )
      *
                   MOVE    SPA-GMN-W20-KSN-LBL    (IDX-KSN)
                                   TO  TGKSN-LBLKSN (IDXG)
                   MOVE    SPA-GMN-W20-KSNCD-G    (IDX-KSN)
                                   TO  TGKSN-KSN    (IDXG)
                   MOVE    SPA-GMN-W20-KSNLST-MAX (IDX-KSN)
                                   TO  TGKSN-KSN-CNT(IDXG)
                                       WRK-MAX
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-MAX )
                       MOVE   SPA-GMN-W20-KSNLST (IDX-KSN IDX1)
                                   TO  TGKSN-KSN-ITM (IDXG IDX1)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
      *    Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   CONST-YKTTYMD-MAX )
               MOVE    SPA-GMN-W20-YKTTYMD (IDX1)
                                   TO  W20-YKTTYMD (IDX1)
           END-PERFORM
      *
      *    ÁªÂòÈÖ¹æ
           MOVE    SPA-GMN-W20-SELNUM      TO  W20-SELNUM
      *
      *    ÉÂÅï¥ê¥¹¥È
           MOVE    SPA-GMN-W20-BTULST-MAX  TO  W20-BTULST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-BTULST-CNT )
      *
      *        ÉÂÅïÈÖ¹æ
               MOVE    SPA-GMN-W20-TBTUNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W20-TBTUNUM (IDX1)
      *
      *        ÉÂÅïÌ¾¾Î
               MOVE    SPA-GMN-W20-TNAME (IDX1)
                                       TO  W20-TNAME (IDX1)
      *
      *        ÉÂÅï¼ïÊÌ
               MOVE    SPA-GMN-W20-TSBT (IDX1)
                                       TO  W20-TSBT (IDX1)
      *
      *        ÆÃÄêÆþ±¡ÎÁ
               MOVE    SPA-GMN-W20-TTOKNYUIN (IDX1)
                                       TO  W20-TTOKUTEINYUIN (IDX1)
      *
      *        ÉÂÅï²ÊÌ¾
               MOVE    SPA-GMN-W20-TSRYKA (IDX1)
                                       TO  W20-TSRYKA (IDX1)
      *
      *        Æþ±¡´ðËÜÎÁ
               MOVE    SPA-GMN-W20-TKHNRYO (IDX1)
                                       TO  W20-TKHNRYO (IDX1)
      *
      *        Æþ±¡´ðËÜÅÀ¿ô
               MOVE    SPA-GMN-W20-TKHNTEN (IDX1)
                                       TO  W20-TKHNTEN (IDX1)
      *
      *        ¼ýÍÆ¿Í¿ô
               MOVE    SPA-GMN-W20-TNINZU (IDX1)
                                       TO  WRK-ZZZZZZZZZ9
               MOVE    WRK-ZZZZZZZZZ9
                                       TO  W20-TNINZU (IDX1)
      *
      *        Í­¸ú´ü´Ö³«»ÏÆü
               MOVE    SPA-GMN-W20-TYUKOSTYMD (IDX1)
                                       TO  W20-TYUKOSTYMD (IDX1)
      *
      *        Í­¸ú´ü´Ö½ªÎ»Æü
               MOVE    SPA-GMN-W20-TYUKOEDYMD (IDX1)
                                       TO  W20-TYUKOEDYMD (IDX1)
      *
               IF    ( SPA-GMN-W20-BTULST-SEL  =   IDX1 )
                   MOVE    "T"         TO  W20-BTULST-SEL (IDX1)
               ELSE
                   MOVE    "F"         TO  W20-BTULST-SEL (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    ÍúÎò¥ê¥¹¥È
           MOVE    SPA-GMN-W20-RIREKILST-MAX
                                       TO  W20-RIREKILST-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-RIREKILST-CNT )
      *
               MOVE    SPA-GMN-W20-TRSELNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W20-TSELNUM (IDX1)
      *
               STRING  SPA-GMN-W20-TRYUKOSTYMD (IDX1)
                       " ¡Á "
                       SPA-GMN-W20-TRYUKOEDYMD (IDX1)
                       DELIMITED   BY  SIZE
               INTO    W20-TRIREKI (IDX1)
               END-STRING
      *
               IF    ( IDX1    =   SPA-GMN-W20-RIREKILST-SEL )
                   MOVE    "T"         TO  W20-RIREKILST-SEL (IDX1)
               ELSE
                   MOVE    "F"         TO  W20-RIREKILST-SEL (IDX1)
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    FLG-BTULST-ROW
           WHEN    1
               MOVE    ZERO               TO  W20-BTULST-ROW
               MOVE    ZERO               TO  W20-BTULST-ROWATTR
           WHEN    2
               MOVE    W20-BTULST-CNT     TO  W20-BTULST-ROW
               MOVE    1                  TO  W20-BTULST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-BTULST-ROW     TO  W20-BTULST-ROW
               MOVE    ZERO               TO  W20-BTULST-ROWATTR
           END-EVALUATE
      *
      *    ¹àÌÜ¾õÂÖÊÔ½¸
           PERFORM 500-EDIT-GMNHEN-STATE-SEC
      *
      *
      *    Æþ±¡ÎÁ²Ã»»
           PERFORM VARYING IDX-GKSN    FROM    1   BY  1
                   UNTIL ( IDX-GKSN    >   CONST-GKSN-MAX )
      *
               EVALUATE    IDX-GKSN
               WHEN    1   THRU    14
      *
                   COMPUTE IDXG    =   IDX-GKSN
      *
                   MOVE    TGKSN-LBLKSN    (IDX-GKSN)
                               TO  W20-LBLKSN-P01 (IDXG)
                   MOVE    TGKSN-CMB-KSN-G (IDX-GKSN)
                               TO  W20-CMB-KSN-P01-OCC(IDXG)
               WHEN    15  THRU    30
      *
                   COMPUTE IDXG    =   IDX-GKSN - 14
      *
                   MOVE    TGKSN-LBLKSN    (IDX-GKSN)
                               TO  W20-LBLKSN-P02     (IDXG )
                   MOVE    TGKSN-CMB-KSN-G (IDX-GKSN)
                               TO  W20-CMB-KSN-P02-OCC(IDXG )
               WHEN    31  THRU    46
      *
                   COMPUTE IDXG    =   IDX-GKSN - 30
      *
                   MOVE    TGKSN-LBLKSN    (IDX-GKSN)
                               TO  W20-LBLKSN-P03     (IDXG )
                   MOVE    TGKSN-CMB-KSN-G (IDX-GKSN)
                               TO  W20-CMB-KSN-P03-OCC(IDXG )
               END-EVALUATE
      *
           END-PERFORM
      *
           IF    ( W20-KSN-P02-CNT (1) =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  W20-LBLKSNP02-STATE
           END-IF
      *
           IF    ( W20-KSN-P03-CNT (1) =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  W20-LBLKSNP03-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¹àÌÜ¾õÂÖÊÔ½¸½èÍý
      *****************************************************************
       500-EDIT-GMNHEN-STATE-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  WRK-STATE
      *
           MOVE    WRK-STATE       TO  W20-B02-STATE
                                       W20-B05-STATE
                                       W20-B06-STATE
                                       W20-B07-STATE
                                       W20-B08-STATE
                                       W20-B09-STATE
                                       W20-B10-STATE
                                       W20-B12-STATE
                                       W20-BTUNUM-STATE
                                       W20-NAME-STATE
                                       W20-TANNAME-STATE
                                       W20-TOKUTEINYUIN-STATE
                                       W20-TOKUTEINYUIN-ITM-STATE
                                       W20-SRYKA-STATE
                                       W20-SRYKA-ITM-STATE
                                       W20-SBT-STATE
                                       W20-SBT-ITM-STATE
                                       W20-KNGHJO-STATE
                                       W20-KNGHJO-ITM-STATE
                                       W20-RECEDSPKBN-STATE
                                       W20-RECEDSPKBN-ITM-STATE
                                       W20-YUKOSTYMD-STATE
                                       W20-YUKOEDYMD-STATE
                                       W20-EDKBN-STATE
                                       W20-EDKBN-ITM-STATE
                                       W20-KHNRYO-STATE
                                       W20-KHNRYO-ITM-STATE
                                       W20-KHNTEN-STATE
                                       W20-YKNCHOKAKBN-STATE
                                       W20-YKNCHOKAKBN-ITM-STATE
                                       W20-RYOYOKBN-STATE
                                       W20-RYOYOKBN-ITM-STATE
                                       W20-LBLKSNP02-STATE
                                       W20-LBLKSNP03-STATE
                                       W20-BTUTYPE-STATE
                                       W20-BTUTYPE-ITM-STATE
                                       W20-BTUCD-STATE
                                       W20-TBTUTYPE-STATE
                                       W20-TBTUTYPE-ITM-STATE
                                       W20-TBTUCD-STATE
                                       W20-LBLYKTTYMDP-STATE
      *
           IF   ( SPA-GMN-W20-BTULST-SEL    >  ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-BTUNUM-STATE
           END-IF
      *
           IF   ( SPA-NAI-W20-HOSPSBT  =    1 )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-KNGHJO-STATE
                                           W20-KNGHJO-ITM-STATE
           END-IF
      *
      *    IF    ( SPA-GMN-W20-KHNRYO-MAX  =   ZERO )
      *        MOVE    WIDGET-INSENSITIVE
      *                                TO  W20-KHNRYO-STATE
      *                                    W20-KHNRYO-ITM-STATE
      *    END-IF
      *
           PERFORM VARYING IDX-GKSN FROM    1   BY  1
                   UNTIL ( IDX-GKSN >  CONST-GKSN-MAX )
      *
               IF    ( TGKSN-KSN-CNT (IDX-GKSN)   >   ZERO )
                   MOVE    WIDGET-NORMAL
                                       TO  TGKSN-KSN-STATE (IDX-GKSN)
                                           TGKSN-KSN-ITM-STATE
                                                           (IDX-GKSN)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                       TO  TGKSN-KSN-STATE (IDX-GKSN)
                                           TGKSN-KSN-ITM-STATE
                                                           (IDX-GKSN)
               END-IF
      *
               IF    ( TGKSN-KSN-STATE (IDX-GKSN)  =   WIDGET-NORMAL )
                   PERFORM 5001-EDIT-GMNHEN-STATE-SEC
                   MOVE    WRK-KSN-STATE
                                       TO  TGKSN-KSN-STATE (IDX-GKSN)
                                           TGKSN-KSN-ITM-STATE
                                                           (IDX-GKSN)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-W20-BTULST-MAX >=   CONST-BTULST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-B06-STATE
           END-IF
      *
           IF    ( W20-LBLYKNCHOKAKBN     =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-YKNCHOKAKBN-STATE
                                           W20-YKNCHOKAKBN-ITM-STATE
           END-IF
      *
           IF    ( W20-LBLRYOYOKBN     =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-RYOYOKBN-STATE
                                           W20-RYOYOKBN-ITM-STATE
           END-IF
      *
           IF    ( SPA-NAI-W20-HOSPSBT =   2 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-BTUTYPE-STATE
                                           W20-BTUTYPE-ITM-STATE
                                           W20-BTUCD-STATE
                                           W20-TBTUTYPE-STATE
                                           W20-TBTUTYPE-ITM-STATE
                                           W20-TBTUCD-STATE
           END-IF
      *
           IF    ( W20-BTUTYPE         =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-BTUCD-STATE
           END-IF
      *
           IF    ( W20-TBTUTYPE        =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-TBTUCD-STATE
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  WRK-STATE
           IF    ( SPA-NAI-W20-HOSPSBT     =   1 )
            AND  ( SPA-NAI-W20-BEDSU       <   100 )
            AND  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H300401 )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  WRK-STATE
           END-IF
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-YKTTYMD-MAX )
               MOVE    WRK-STATE       TO  W20-YKTTYMD-STATE (IDX0)
           END-PERFORM
           MOVE    WRK-STATE           TO  W20-LBLYKTTYMDP-STATE
      *
           .
       500-EDIT-GMNHEN-STATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¹àÌÜ¾õÂÖÊÔ½¸½èÍý
      *****************************************************************
       5001-EDIT-GMNHEN-STATE-SEC      SECTION.
      *
           MOVE    TGKSN-KSN-STATE (IDX-GKSN)
                                       TO  WRK-KSN-STATE
      *
           IF    ( SPA-NAI-W20-YKNKNG-IDX  >   ZERO )
            AND  ( SPA-NAI-W20-YKNKNG-IDX  =   IDX-GKSN )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
                   IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  WRK-KSN-STATE
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180701 )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  WRK-KSN-STATE
               END-IF
           END-IF
      *
           .
       5001-EDIT-GMNHEN-STATE-EXT.
           EXIT.
      *****************************************************************
      *    ²èÌÌ¥«¡¼¥½¥ë¥»¥Ã¥È½èÍý
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-W20-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W20-CUR
           WHEN    001
               MOVE    "BTUNUM"            TO  MCP-WIDGET
           WHEN    002
               MOVE    "NAME"              TO  MCP-WIDGET
           WHEN    003
               MOVE    "TANNAME"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "TOKUTEINYUIN"      TO  MCP-WIDGET
           WHEN    005
               MOVE    "SRYKA"             TO  MCP-WIDGET
           WHEN    006
               MOVE    "SBT"               TO  MCP-WIDGET
           WHEN    007
               MOVE    "KHNRYO"            TO  MCP-WIDGET
           WHEN    008
               MOVE    "RECEDSPKBN"        TO  MCP-WIDGET
           WHEN    009
               MOVE    "KNGHJO"            TO  MCP-WIDGET
           WHEN    301 THRU  346
               MOVE    "KSN"               TO  WRK-KSNNUM-G
               COMPUTE WRK-KSNNUM = SPA-GMN-W20-CUR - 300
               MOVE    WRK-KSNNUM-G        TO  MCP-WIDGET
           WHEN    901 THRU  912
               MOVE    "YKTTYMD"           TO  WRK-YKTTYMDNUM-G
               COMPUTE WRK-YKTTYMDNUM = SPA-GMN-W20-CUR - 900
               MOVE    WRK-YKTTYMDNUM-G    TO  MCP-WIDGET
           WHEN    051
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    052
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    053
               MOVE    "EDKBN"             TO  MCP-WIDGET
           WHEN    054
               MOVE    "RYOYOKBN"          TO  MCP-WIDGET
           WHEN    055
               MOVE    "YKNCHOKAKBN"       TO  MCP-WIDGET
           WHEN    056
               MOVE    "BTUSELNUM"         TO  MCP-WIDGET
           WHEN    057
               MOVE    "BTUTYPE"           TO  MCP-WIDGET
           WHEN    058
               MOVE    "BTUCD"             TO  MCP-WIDGET
           WHEN    059
               MOVE    "TBTUTYPE"          TO  MCP-WIDGET
           WHEN    060
               MOVE    "TBTUCD"            TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM"            TO  MCP-WIDGET
           WHEN    101
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    207
               MOVE    "B07"               TO  MCP-WIDGET
           WHEN    208
               MOVE    "B08"               TO  MCP-WIDGET
           WHEN    209
               MOVE    "B09"               TO  MCP-WIDGET
           WHEN    210
               MOVE    "B10"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ë¥¹¥­¥Ã¥×½èÍý
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-W20-CUR     =   1 )
               IF    ( W20-BTUNUM-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    2           TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   9 )
               IF    ( W20-KNGHJO-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    301         TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   7 )
               IF    ( W20-KHNRYO-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   55 )
               IF    ( W20-YKNCHOKAKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    54          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   54 )
               IF    ( W20-RYOYOKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   57 )
               IF    ( W20-BTUTYPE-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    58          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   58 )
               IF    ( W20-BTUCD-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    59          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   59 )
               IF    ( W20-TBTUTYPE-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    60          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   60 )
               IF    ( W20-TBTUCD-STATE
                                       =   WIDGET-INSENSITIVE )
                   EVALUATE    SPA-GMN-W20-KSN-PAGENO
                   WHEN    0
                       MOVE    009     TO  SPA-GMN-W20-CUR
                   WHEN    1
                       MOVE    315     TO  SPA-GMN-W20-CUR
                   WHEN    2
                       MOVE    331     TO  SPA-GMN-W20-CUR
                   WHEN    3
                       MOVE    901     TO  SPA-GMN-W20-CUR
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   9 )
               IF    ( W20-KNGHJO-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    301         TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-GKSN    FROM    1   BY  1
                   UNTIL ( IDX-GKSN    >   CONST-GKSN-MAX )
      *
               IF    ( SPA-GMN-W20-CUR =   IDX-GKSN + 300 )
      *
                   IF    ( TGKSN-KSN-STATE (IDX-GKSN)
                                       =   WIDGET-INSENSITIVE )
                       IF    ( IDX-GKSN    =   14 OR 30 OR 46
                                             OR CONST-GKSN-MAX )
                           COMPUTE SPA-GMN-W20-CUR
                               =   51
                       ELSE
                           COMPUTE SPA-GMN-W20-CUR
                               =   IDX-GKSN
                               +   301
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-YKTTYMD-MAX )
      *
               IF    ( SPA-GMN-W20-CUR =   IDXA    +   900 )
                   IF    ( W20-YKTTYMD-STATE (IDXA)
                                       =   WIDGET-INSENSITIVE )
                       IF    ( IDXA    =   CONST-YKTTYMD-MAX )
                           COMPUTE SPA-GMN-W20-CUR
                               =   51
                       ELSE
                           COMPUTE SPA-GMN-W20-CUR
                               =   IDXA
                               +   901
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-W20-CUR     =   206 )
               IF    ( W20-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    207         TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¸Ä½ê¤Î¥»¥Ã¥È½èÍý
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BTUSELNUM"
               MOVE    001             TO  SPA-GMN-W20-CUR
           WHEN    "BTUNUM"
               MOVE    002             TO  SPA-GMN-W20-CUR
           WHEN    "NAME"
               MOVE    003             TO  SPA-GMN-W20-CUR
           WHEN    "TANNAME"
               MOVE    005             TO  SPA-GMN-W20-CUR
           WHEN    "SRYKA"
               MOVE    006             TO  SPA-GMN-W20-CUR
           WHEN    "TOKUTEINYUIN"
               MOVE    007             TO  SPA-GMN-W20-CUR
           WHEN    "SBT"
               MOVE    004             TO  SPA-GMN-W20-CUR
           WHEN    "KHNRYO"
               MOVE    055             TO  SPA-GMN-W20-CUR
           WHEN    "YKNCHOKAKBN"
               MOVE    054             TO  SPA-GMN-W20-CUR
           WHEN    "RYOYOKBN"
               MOVE    008             TO  SPA-GMN-W20-CUR
           WHEN    "RECEDSPKBN"
               MOVE    057             TO  SPA-GMN-W20-CUR
           WHEN    "BTUTYPE"
               MOVE    058             TO  SPA-GMN-W20-CUR
           WHEN    "BTUCD"
               MOVE    059             TO  SPA-GMN-W20-CUR
           WHEN    "TBTUTYPE"
               MOVE    060             TO  SPA-GMN-W20-CUR
           WHEN    "TBTUCD"
               EVALUATE    SPA-GMN-W20-KSN-PAGENO
               WHEN    0
                   MOVE    009     TO  SPA-GMN-W20-CUR
               WHEN    1
                   MOVE    315     TO  SPA-GMN-W20-CUR
               WHEN    2
                   MOVE    331     TO  SPA-GMN-W20-CUR
               WHEN    3
                   MOVE    901     TO  SPA-GMN-W20-CUR
               END-EVALUATE
           WHEN    "KNGHJO"
               MOVE    301             TO  SPA-GMN-W20-CUR
           WHEN    "KSN01"    THRU "KSN13"
           WHEN    "KSN15"    THRU "KSN29"
           WHEN    "KSN31"    THRU "KSN45"
               COMPUTE SPA-GMN-W20-CUR
                   =   FUNCTION NUMVAL (WRK-MCP-WIDGET(4:2))
                   +   301
           WHEN    "KSN14"
           WHEN    "KSN30"
           WHEN    "KSN46"
               MOVE    051             TO  SPA-GMN-W20-CUR
           WHEN    "YKTTYMD01" THRU    "YKTTYMD11"
               COMPUTE SPA-GMN-W20-CUR
                   =   FUNCTION NUMVAL (WRK-MCP-WIDGET(8:2))
                   +   901
           WHEN    "YKTTYMD12"
               MOVE    051             TO  SPA-GMN-W20-CUR
           WHEN    "YUKOSTYMD"
               MOVE    052             TO  SPA-GMN-W20-CUR
           WHEN    "YUKOEDYMD"
               MOVE    053             TO  SPA-GMN-W20-CUR
           WHEN    "EDKBN"
               MOVE    206             TO  SPA-GMN-W20-CUR
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-W20-CUR
           WHEN    "KJNYMD"
               MOVE    001             TO  SPA-GMN-W20-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸É½¼¨Íý
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
                   MOVE    "ÆþÎÏÆâÍÆ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "Á´³ÑÊ¸»ú¤Ç¤Î¤ßÆþÎÏ¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "Í­¸ú´ü´Ö³«»ÏÆü¤ÏÊ¿À®£±£´Ç¯£´·î£±Æü°Ê¹ß¤Ë¤·¤Æ"
                       "¤¯¤À¤µ¤¤"
                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0004"
                   MOVE    "ÆÃÄêÆþ±¡ÎÁ¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
                   MOVE    "¿ÇÎÅ²Ê¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
                   MOVE    "ÉÂÅï¼ïÊÌ¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
                   MOVE    "¥ì¥»¥×¥ÈÉ½¼¨¶èÊ¬¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
                   MOVE    "Í­¸ú³«»ÏÆü¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
                   MOVE    "Í­¸ú½ªÎ»Æü¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
                   MOVE    "Æþ±¡´ðËÜÎÁ¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
                   MOVE    "ÅïÎò¤ÎÁªÂòÈÖ¹æ¤òÀµ¤·¤¯ÆþÎÏ¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
                   MOVE    "´ð½àÆü¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
                   MOVE    "ÉÂÅï¤òÀµ¤·¤¯ÁªÂò¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "ÆþÎÏ¤µ¤ì¤¿ÉÂÅïÈÖ¹æ¤Ï´û¤ËÅÐÏ¿ºÑ¤ß¤Ç¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "ÉÂÅïµ¡Ç½¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "Ï¢ÈÖ¤Ï£±¤«¤é£µ£°¤ÎÈÏ°Ï¤ÇÀßÄê¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "ÄÉ²Ã²ÄÇ½ºÇÂç·ï¿ô¤ËÃ£¤·¤Þ¤·¤¿"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "ÊÑ¹¹ÂÐ¾Ý¤ÎÉÂÅï¤ÏÂ¸ºß¤·¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "ºï½üÂÐ¾Ý¤ÎÉÂÅï¤ÏÂ¸ºß¤·¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "Í­¸ú´ü´Ö³«»ÏÆü¡äÍ­¸ú´ü´Ö½ªÎ»Æü¤È¤Ê¤Ã¤Æ¤¤¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "Ìë´Ö´Ç¸îÂÎÀ©ÆÃÄêÆü¤ÎÆþÎÏ¤Ë¸í¤ê¤¬¤¢¤ê¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "ÉÂÅï¤ÎÍ­¸ú´ü´ÖÆâ¤ËÉÂ¼¼´ÉÍý¾ðÊó¤¬Â¸ºß¤¹¤ë¤¿¤á"
                       "ºï½ü¤Ç¤­¤Þ¤»¤ó"
                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1007"
               MOVE    "ÉÂÅïÌ¾¾Î¤òÆþÎÏ¤·¤Æ²¼¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "Ã»½ÌÉÂÅïÌ¾¤òÆþÎÏ¤·¤Æ²¼¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "ÉÂÅï¼ïÊÌ¤òÆþÎÏ¤·¤Æ²¼¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    1               TO  WRK-PNT
               IF    ( WRK-KSNMEI      =   SPACE )
                   STRING  "²Ã»»¤ËÉ¬Í×¤Ê»ÜÀß´ð½à¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤Þ¤»¤ó"
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               ELSE
                   STRING  "É¬Í×¤Ê»ÜÀß´ð½à¤¬ÀßÄê¤µ¤ì¤Æ¤¤¤Þ¤»¤ó¡¿"
                           DELIMITED   BY  SPACE
                           WRK-KSNMEI
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-IF
      *
               PERFORM VARYING IDX-SST FROM    1   BY  1
                       UNTIL ( IDX-SST >   CNT-SST )
      *
                   STRING  WRK-SSTKJNCD-OCC  (IDX-SST)
                           DELIMITED   BY  SIZE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-PERFORM
           WHEN    "1011"
               STRING  "Æþ±¡ÎÁ¤ËÂÐ¤·¤Æ"
                       WRK-KSNMEI      DELIMITED   BY  SPACE
                       "¤Ï»»Äê¤Ç¤­¤Þ¤»¤ó"
                                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1014"
               MOVE    "Æþ±¡´ðËÜÎÁ¤¬»»Äê¤µ¤ì¤Æ¤¤¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE     "ÉÂÅïÈÖ¹æ¤òÆþÎÏ¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "Í­¸ú´ü´Ö³«»ÏÆü¤òÆþÎÏ¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "Í­¸ú´ü´Ö½ªÎ»Æü¤òÆþÎÏ¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               MOVE    "Í­¸ú´ü´Ö¤Î½ÅÊ£¤¹¤ë¥Ç¡¼¥¿¤¬Â¸ºß¤·¤Þ¤¹"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "Í­¸ú´ü´Ö¤Î³«»ÏÆü¤È½ªÎ»Æü¤ÏÆ±»þ¤ËÊÑ¹¹¤Ç¤­¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "ÉÂ±¡¤ÎÉÂÅï¼ïÊÌ¤òÁªÂò¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "¿ÇÎÅ½ê¤ÎÉÂÅï¼ïÊÌ¤òÁªÂò¤·¤Æ¤¯¤À¤µ¤¤"
                                       TO  SPA-ERRMSG
           WHEN    "1108"
               MOVE    "ÅÐÏ¿ºÑ¤ß¤ÎÉÂÅï¤¬ÁªÂò¤µ¤ì¤Æ¤¤¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1109"
               MOVE   "°åÎÅ´Ñ»¡ÉÂÅï¤Ç¤ÏÆþ±¡ÎÁ²Ã»»¤ÎÄÉ²ÃÀßÄê¤Ï¤Ç¤­¤Þ¤»¤ó"
                                       TO  SPA-ERRMSG
           WHEN    "1110"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "ÉÂÅï¤ÎÍ­¸ú³«»ÏÆü¤¬ÊÑ¹¹¤µ¤ì¤Æ¤¤¤Þ¤¹¡£"
                                           DELIMITED   BY  SIZE
                       "Àè¤ËÊÑ¹¹¤Þ¤¿¤ÏÄÉ²Ã½èÍý¤ò¹Ô¤Ã¤Æ¤¯¤À¤µ¤¤"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1111"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "Í­¸ú´ü´Ö¤Î³«»ÏÆü¤¬Ê¿À®£²£´Ç¯£´·î£±Æü°Ê¹ß¤Î"
                                           DELIMITED   BY  SIZE
                       "ÉÂÅï¤Î¤ßÆþ±¡ÎÁ²Ã»»¤ÎÄÉ²ÃÀßÄê¤¬²ÄÇ½¤Ç¤¹"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1112"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "ÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ»»Äê»þ¤Ï"
                                           DELIMITED   BY  SIZE
                       "ÉÂÅï¼ïÊÌ¤ò°ìÈÌÉÂÅï¤Ë¤·¤Æ¤¯¤À¤µ¤¤"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1113"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "¤³¤Î²èÌÌ¤ÇÅÐÏ¿²ÄÇ½¤Ê²Ã»»¤Î¾å¸Â¿ô¡Ê£±£´¡Ë¤ò"
                                           DELIMITED   BY  SIZE
                       "Ä¶¤¨¤Æ¤¤¤Þ¤¹"      DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1114"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "¥ì¥»ÅÅÉÂÅï¾ðÊó¤òÀßÄê¤¹¤ë¾ì¹ç¡¢ÉÂÅïµ¡Ç½¤ÈÏ¢ÈÖ"
                                           DELIMITED   BY  SIZE
                       "¤ÎÎ¾Êý¤òÀßÄê¤·¤Æ¤¯¤À¤µ¤¤"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1201"    THRU    "1211"
               MOVE    "¹¹¿·½èÍý¤Ë¼ºÇÔ¤·¤Þ¤·¤¿"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W20"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¬¥¤¥É¥á¥Ã¥»¡¼¥¸É½¼¨
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "2001"
               MOVE    "³ºÅö¤ÎÉÂÅï¤òºï½ü¤·¤Þ¤¹¡£¤è¤í¤·¤¤¤Ç¤¹¤«"
                                        TO  WRK-WIDMSG
           WHEN    "2002"
               MOVE    SPACE            TO  WRK-WIDMSG
               STRING  "Í­¸ú´ü´Ö¤Î½ÅÊ£¤¹¤ë¥Ç¡¼¥¿¤¬Â¸ºß¤·¤Þ¤¹¡£"
                                            DELIMITED   BY  SIZE
                       "Á°¤Î´ü´Ö¤ò¶èÀÚ¤ê¤Þ¤¹¤«"
                                            DELIMITED   BY  SIZE
               INTO    WRK-WIDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W20"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥À¥ß¡¼¥³¡¼¥ÉÃÖ´¹½èÍý
      *****************************************************************
       800-DUMMYCD-CHG-SEC             SECTION.
      *
           EVALUATE    WRK-KHNCD
           WHEN    "099999955"
               MOVE    "190169610"     TO  WRK-KHNCD
           WHEN    "099999956"
               MOVE    "190169910"     TO  WRK-KHNCD
           WHEN    "099999957"
               MOVE    "190170210"     TO  WRK-KHNCD
           WHEN    "190097010"
               MOVE    "099999950"     TO  WRK-KHNCD
           WHEN    "190097210"
               MOVE    "099999951"     TO  WRK-KHNCD
           WHEN    "099999952"
               MOVE    "190135110"     TO  WRK-KHNCD
           WHEN    "099999953"
               MOVE    "190121310"     TO  WRK-KHNCD
           WHEN    "099999954"
               MOVE    "190132410"     TO  WRK-KHNCD
           END-EVALUATE
      *
           .
       800-DUMMYCD-CHG-EXT.
           EXIT.
      *****************************************************************
      *    Á´³ÑÈ¾³Ñº®ºß¥Á¥§¥Ã¥¯
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    Á´³Ñ¤Ç¤Ê¤¤¾ì¹ç¤Ï¥¨¥é¡¼¤È¤¹¤ë¡£
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Á´³ÑÊÑ´¹½èÍý
      *****************************************************************
       800-ZENKAKU-SEC             SECTION.
      *
           INITIALIZE                  WRK-KANACHK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
           EVALUATE    TRUE
           WHEN  ( WRK-YMD (1:3)   =   "000" )
               MOVE    "00000000"  TO  WRK-HENYMDG
                                       WRK-SYMD
           WHEN  ( WRK-YMD (1:3)   =   "999" )
               MOVE    "99999999"  TO  WRK-HENYMDG
                                       WRK-SYMD
           WHEN    OTHER
      *
      *        ÆüÉÕ²èÌÌ¥Á¥§¥Ã¥¯¥µ¥Ö
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKE-ERR
               END-IF
           END-EVALUATE
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýFETCH½èÍý(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-SYSKANRI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýFETCH½èÍý(KEY)
      *****************************************************************
       900-SYSKANRI-KEY-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýFETCH½èÍý(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýFETCH½èÍý(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5002-KBNCD
           MOVE    "%"                 TO  SYS-5002-KBNCD (3:1)
           MOVE    SPA-HOSPNUM         TO  SYS-5002-HOSPNUM
           MOVE    SYS-5002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5002-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýFETCH½èÍý(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5002-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-5001-STYUKYMD
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-NYUINKBN        TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSNKBN          TO  NYUKSNCHK-KSNKBN
           MOVE    WRK-YUKOSTYMD       TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUKSNCHK-REC
           ELSE
               INITIALIZE                  NYUKSNCHK-REC
               MOVE    1               TO  FLG-NYUKSNCHK
           END-IF
      *
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-TENSU-SEL-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               PERFORM 9002-EDIT-TENSU-PATH-SEC
           ELSE
               PERFORM 9001-EDIT-TENSU-PATH-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷¥Ñ¥¹ÊÔ½¸½èÍý¡ÊÊ¿À®£±£·Ç¯ÅÙ¤Þ¤Ç¡Ë
      *****************************************************************
       9001-EDIT-TENSU-PATH-SEC         SECTION.
      *
           IF    ( SPA-GMN-W20-SBT =   "07" )
               MOVE    "-"         TO  TNS-CDKBN-KBN
           ELSE
               MOVE    "A"         TO  TNS-CDKBN-KBN
           END-IF
      *
      *    ¶èÊ¬ÈÖ¹æÀßÄê½èÍý
           PERFORM 9001-CDKBNNUM-SET-SEC
           MOVE    WRK-CDKBNNUM    TO  TNS-CDKBN-KBNNUM
           MOVE    WRK-CDKOUBAN    TO  TNS-CDKBN-KOUBAN
           MOVE    WRK-TK-CDKBNNUM TO  TNSXX-TK-CDKBN-KBNNUM
           MOVE    WRK-TK-CDKOUBAN TO  TNSXX-TK-CDKBN-KOUBAN
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷
           EVALUATE    SPA-GMN-W20-SBT
           WHEN    "01"
           WHEN    "02"
           WHEN    "03"
               MOVE    "key15"     TO  WRK-PATHNAME
           WHEN    "07"
               MOVE    "key14"     TO  WRK-PATHNAME
           WHEN    OTHER
               MOVE    "key13"     TO  WRK-PATHNAME
           END-EVALUATE
      *
           .
       9001-EDIT-TENSU-PATH-EXT.
           EXIT.
      *****************************************************************
      *    ¶èÊ¬ÈÖ¹æÀßÄê½èÍý¡ÊÊ¿À®£±£·Ç¯ÅÙ¤Þ¤Ç¡Ë
      *****************************************************************
       9001-CDKBNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-CDKBNNUM
                                       WRK-CDKOUBAN
                                       WRK-TK-CDKBNNUM
                                       WRK-TK-CDKOUBAN
      *
           EVALUATE    SPA-GMN-W20-SBT
      *    °ìÈÌÉÂÅï
           WHEN    "01"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    1               TO  WRK-TK-CDKOUBAN
               MOVE    100             TO  WRK-CDKBNNUM
      *    Àº¿ÀÉÂÅï
           WHEN    "02"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    3               TO  WRK-TK-CDKOUBAN
               MOVE    103             TO  WRK-CDKBNNUM
      *    ·ë³ËÉÂÅï
           WHEN    "03"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    2               TO  WRK-TK-CDKOUBAN
               MOVE    102             TO  WRK-CDKBNNUM
      *    ÎÅÍÜÉÂÅï
           WHEN    "04"
               MOVE    101             TO  WRK-CDKBNNUM
      *    ¾ã³²¼Ô»ÜÀß
           WHEN    "05"
               MOVE    106             TO  WRK-CDKBNNUM
      *    ÀìÌçÉÂ±¡
           WHEN    "06"
               MOVE    105             TO  WRK-CDKBNNUM
      *    Ï·¿ÍÉÂÅï
           WHEN    "07"
               MOVE    ZERO            TO  WRK-CDKBNNUM
               MOVE    8               TO  WRK-CDKOUBAN
      *    Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ
           WHEN    "08"
               MOVE    108             TO  WRK-CDKBNNUM
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡ÎÁ
           WHEN    "09"
               MOVE    109             TO  WRK-CDKBNNUM
           END-EVALUATE
      *
           .
      *
       9001-CDKBNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷¥Ñ¥¹ÊÔ½¸½èÍý¡ÊÊ¿À®£±£¸Ç¯ÅÙ°Ê¹ß¡Ë
      *****************************************************************
       9002-EDIT-TENSU-PATH-SEC         SECTION.
      *
           MOVE    "A"             TO  TNS-CDKBN-KBN
      *
      *    ¶èÊ¬ÈÖ¹æÀßÄê½èÍý
           PERFORM 9002-CDKBNNUM-SET-SEC
           MOVE    WRK-CDKBNNUM    TO  TNS-CDKBN-KBNNUM
           MOVE    WRK-CDKOUBAN    TO  TNS-CDKBN-KOUBAN
           MOVE    WRK-TK-CDKBNNUM TO  TNSXX-TK-CDKBN-KBNNUM
           MOVE    WRK-TK-CDKOUBAN TO  TNSXX-TK-CDKBN-KOUBAN
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷
           MOVE    "key25"         TO  WRK-PATHNAME
      *
           .
       9002-EDIT-TENSU-PATH-EXT.
           EXIT.
      *****************************************************************
      *    ¶èÊ¬ÈÖ¹æÀßÄê½èÍý¡ÊÊ¿À®£±£¸Ç¯ÅÙ°Ê¹ß¡Ë
      *****************************************************************
       9002-CDKBNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-CDKBNNUM
                                       WRK-CDKOUBAN
                                       WRK-TK-CDKBNNUM
                                       WRK-TK-CDKOUBAN
      *
           EVALUATE    SPA-GMN-W20-SBT
      *    °ìÈÌÉÂÅï
           WHEN    "01"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    1               TO  WRK-TK-CDKOUBAN
               MOVE    100             TO  WRK-CDKBNNUM
      *    Àº¿ÀÉÂÅï
           WHEN    "02"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    3               TO  WRK-TK-CDKOUBAN
               MOVE    103             TO  WRK-CDKBNNUM
      *    ·ë³ËÉÂÅï
           WHEN    "03"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    2               TO  WRK-TK-CDKOUBAN
               MOVE    102             TO  WRK-CDKBNNUM
      *    ÎÅÍÜÉÂÅï
           WHEN    "04"
               MOVE    101             TO  WRK-CDKBNNUM
      *    ¾ã³²¼Ô»ÜÀß
           WHEN    "05"
               MOVE    106             TO  WRK-CDKBNNUM
      *    ÀìÌçÉÂ±¡
           WHEN    "06"
               MOVE    105             TO  WRK-CDKBNNUM
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡ÎÁ
           WHEN    "09"
               MOVE    109             TO  WRK-CDKBNNUM
           END-EVALUATE
      *
           .
      *
       9002-CDKBNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿FETCH½èÍý
      *****************************************************************
       900-TENSU-FET-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-FET-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤Ï¹Ô¤ï¤Ê¤¤)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Ä£Å£Ì£Å£Ô£Å½èÍý
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£É£Î£Ó£Å£Ò£Ô½èÍý
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Õ£Ð£Ä£Á£Ô£Å½èÍý
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    £Ð£Õ£Ô¡¡½èÍý
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

