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
      *-------------------------------------------------*
      *    Ã±°Ì¥³¡¼¥É¥Æ¡¼¥Ö¥ë   COPY (CMTANICD.INC)     *
      *-------------------------------------------------*
      *****************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     MCC-Â¿¡¹Ç¼   02/03/19  124 ¶Ý¼ïÄÉ²Ã(H14.4²þÀµ)
      * 01.00.02     NACL-Â¿¡¹Ç¼  02/12/13  060 ¥·¡¼¥ÈÄÉ²Ã(H14.12²þÀµ)
      * 01.00.03     NACL-Â¿¡¹Ç¼  04/12/22  000 Ã±°Ì¤Ê¤·ÄÉ²Ã
      * 01.00.04     NACL-Â¿¡¹Ç¼  06/03/15  125 ¤«¤é 128ÄÉ²Ã(H18.4²þÀµ¡Ë
      * 04.08.00     NACL-Â¿¡¹Ç¼  16/08/30  061 ¥«¥»¥Ã¥ÈÄÉ²Ã
      *****************************************************************
       01  MTANI-DATA-REC.
           03  FILLER          PIC X(03)  VALUE   "000".
           03  FILLER          PIC X(24)  VALUE   SPACE.
      *
           03  FILLER          PIC X(03)  VALUE   "001".
           03  FILLER          PIC X(24)  VALUE   "Ê¬".
      *
           03  FILLER          PIC X(03)  VALUE   "002".
           03  FILLER          PIC X(24)  VALUE   "²ó".
      *
           03  FILLER          PIC X(03)  VALUE   "003".
           03  FILLER          PIC X(24)  VALUE   "¼ï".
      *
           03  FILLER          PIC X(03)  VALUE   "004".
           03  FILLER          PIC X(24)  VALUE   "È¢".
      *
           03  FILLER          PIC X(03)  VALUE   "005".
           03  FILLER          PIC X(24)  VALUE   "´¬".
      *
           03  FILLER          PIC X(03)  VALUE   "006".
           03  FILLER          PIC X(24)  VALUE   "Ëç".
      *
           03  FILLER          PIC X(03)  VALUE   "007".
           03  FILLER          PIC X(24)  VALUE   "ËÜ".
      *
           03  FILLER          PIC X(03)  VALUE   "008".
           03  FILLER          PIC X(24)  VALUE   "ÁÈ".
      *
           03  FILLER          PIC X(03)  VALUE   "009".
           03  FILLER          PIC X(24)  VALUE   "¥»¥Ã¥È".
      *
           03  FILLER          PIC X(03)  VALUE   "010".
           03  FILLER          PIC X(24)  VALUE   "¸Ä".
      *
           03  FILLER          PIC X(03)  VALUE   "011".
           03  FILLER          PIC X(24)  VALUE   "Îö".
      *
           03  FILLER          PIC X(03)  VALUE   "012".
           03  FILLER          PIC X(24)  VALUE   "Êý¸þ".
      *
           03  FILLER          PIC X(03)  VALUE   "013".
           03  FILLER          PIC X(24)  VALUE   "¥È¥í¡¼¥Á".
      *
           03  FILLER          PIC X(03)  VALUE   "014".
           03  FILLER          PIC X(24)  VALUE   "¥¢¥ó¥×¥ë".
      *
           03  FILLER          PIC X(03)  VALUE   "015".
           03  FILLER          PIC X(24)  VALUE   "¥«¥×¥»¥ë".
      *
           03  FILLER          PIC X(03)  VALUE   "016".
           03  FILLER          PIC X(24)  VALUE   "¾û".
      *
           03  FILLER          PIC X(03)  VALUE   "017".
           03  FILLER          PIC X(24)  VALUE   "´Ý".
      *
           03  FILLER          PIC X(03)  VALUE   "018".
           03  FILLER          PIC X(24)  VALUE   "Êñ".
      *
           03  FILLER          PIC X(03)  VALUE   "019".
           03  FILLER          PIC X(24)  VALUE   "ÉÓ".
      *
           03  FILLER          PIC X(03)  VALUE   "020".
           03  FILLER          PIC X(24)  VALUE   "ÂÞ".
      *
           03  FILLER          PIC X(03)  VALUE   "021".
           03  FILLER          PIC X(24)  VALUE   "ÉÓ¡ÊÂÞ¡Ë".
      *
           03  FILLER          PIC X(03)  VALUE   "022".
           03  FILLER          PIC X(24)  VALUE   "´É".
      *
           03  FILLER          PIC X(03)  VALUE   "023".
           03  FILLER          PIC X(24)  VALUE   "¥·¥ê¥ó¥¸".
      *
           03  FILLER          PIC X(03)  VALUE   "024".
           03  FILLER          PIC X(24)  VALUE   "²óÊ¬".
      *
           03  FILLER          PIC X(03)  VALUE   "025".
           03  FILLER          PIC X(24)  VALUE   "¥Æ¥¹¥ÈÊ¬".
      *
           03  FILLER          PIC X(03)  VALUE   "026".
           03  FILLER          PIC X(24)  VALUE   "¥¬¥é¥¹Åû".
      *
           03  FILLER          PIC X(03)  VALUE   "027".
           03  FILLER          PIC X(24)  VALUE   "Ûå¾û".
      *
           03  FILLER          PIC X(03)  VALUE   "028".
           03  FILLER          PIC X(24)  VALUE   "Ã±°Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "029".
           03  FILLER          PIC X(24)  VALUE   "ËüÃ±°Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "030".
           03  FILLER          PIC X(24)  VALUE   "¥Õ¥£¡¼¥È".
      *
           03  FILLER          PIC X(03)  VALUE   "031".
           03  FILLER          PIC X(24)  VALUE   "Å©".
      *
           03  FILLER          PIC X(03)  VALUE   "032".
           03  FILLER          PIC X(24)  VALUE   "£í£ç".
      *
           03  FILLER          PIC X(03)  VALUE   "033".
           03  FILLER          PIC X(24)  VALUE   "£ç".
      *
           03  FILLER          PIC X(03)  VALUE   "034".
           03  FILLER          PIC X(24)  VALUE   "£Ë£ç".
      *
           03  FILLER          PIC X(03)  VALUE   "035".
           03  FILLER          PIC X(24)  VALUE   "£ã£ã".
      *
           03  FILLER          PIC X(03)  VALUE   "036".
           03  FILLER          PIC X(24)  VALUE   "£í£Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "037".
           03  FILLER          PIC X(24)  VALUE   "£Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "038".
           03  FILLER          PIC X(24)  VALUE   "£í£Ì£Ö".
      *
           03  FILLER          PIC X(03)  VALUE   "039".
           03  FILLER          PIC X(24)  VALUE   "¥Ð¥¤¥¢¥ë".
      *
           03  FILLER          PIC X(03)  VALUE   "040".
           03  FILLER          PIC X(24)  VALUE   "£ã£í".
      *
           03  FILLER          PIC X(03)  VALUE   "041".
           03  FILLER          PIC X(24)  VALUE   "£ã£í£²".
      *
           03  FILLER          PIC X(03)  VALUE   "042".
           03  FILLER          PIC X(24)  VALUE   "£í".
      *
           03  FILLER          PIC X(03)  VALUE   "043".
           03  FILLER          PIC X(24)  VALUE   "¦Ì£Ã£é".
      *
           03  FILLER          PIC X(03)  VALUE   "044".
           03  FILLER          PIC X(24)  VALUE   "£í£Ã£é".
      *
           03  FILLER          PIC X(03)  VALUE   "045".
           03  FILLER          PIC X(24)  VALUE   "¦Ì£ç".
      *
           03  FILLER          PIC X(03)  VALUE   "046".
           03  FILLER          PIC X(24)  VALUE   "´É¡ÊÉÓ¡Ë".
      *
           03  FILLER          PIC X(03)  VALUE   "047".
           03  FILLER          PIC X(24)  VALUE   "Åû".
      *
           03  FILLER          PIC X(03)  VALUE   "048".
           03  FILLER          PIC X(24)  VALUE   "£Ç£Â£ñ".
      *
           03  FILLER          PIC X(03)  VALUE   "049".
           03  FILLER          PIC X(24)  VALUE   "£Í£Â£ñ".
      *
           03  FILLER          PIC X(03)  VALUE   "050".
           03  FILLER          PIC X(24)  VALUE   "£Ë£Â£ñ".
      *
           03  FILLER          PIC X(03)  VALUE   "051".
           03  FILLER          PIC X(24)  VALUE   "¥­¥Ã¥È".
      *
           03  FILLER          PIC X(03)  VALUE   "052".
           03  FILLER          PIC X(24)  VALUE   "¹ñºÝÃ±°Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "053".
           03  FILLER          PIC X(24)  VALUE   "´µ¼ÔÅö¤ê".
      *
           03  FILLER          PIC X(03)  VALUE   "054".
           03  FILLER          PIC X(24)  VALUE   "µ¤°µ".
      *
           03  FILLER          PIC X(03)  VALUE   "055".
           03  FILLER          PIC X(24)  VALUE   "´Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "056".
           03  FILLER          PIC X(24)  VALUE   "¼ê½ÑÅö¤ê".
      *
           03  FILLER          PIC X(03)  VALUE   "057".
           03  FILLER          PIC X(24)  VALUE   "ÍÆ´ï".
      *
           03  FILLER          PIC X(03)  VALUE   "058".
           03  FILLER          PIC X(24)  VALUE   "£í£Ì¡Ê£ç¡Ë".
      *
           03  FILLER          PIC X(03)  VALUE   "059".
           03  FILLER          PIC X(24)  VALUE   "¥Ö¥ê¥¹¥¿¡¼".
      *
           03  FILLER          PIC X(03)  VALUE   "060".
           03  FILLER          PIC X(24)  VALUE   "¥·¡¼¥È".
      *H28.8
           03  FILLER          PIC X(03)  VALUE   "061".
           03  FILLER          PIC X(24)  VALUE   "¥«¥»¥Ã¥È".
      *
      *
           03  FILLER          PIC X(03)  VALUE   "101".
           03  FILLER          PIC X(24)  VALUE   "Ê¬²è".
      *
           03  FILLER          PIC X(03)  VALUE   "102".
           03  FILLER          PIC X(24)  VALUE   "À÷¿§".
      *
           03  FILLER          PIC X(03)  VALUE   "103".
           03  FILLER          PIC X(24)  VALUE   "¼ïÎà".
      *
           03  FILLER          PIC X(03)  VALUE   "104".
           03  FILLER          PIC X(24)  VALUE   "³ô".
      *
           03  FILLER          PIC X(03)  VALUE   "105".
           03  FILLER          PIC X(24)  VALUE   "¶Ý³ô".
      *
           03  FILLER          PIC X(03)  VALUE   "106".
           03  FILLER          PIC X(24)  VALUE   "¾È¼Í".
      *
           03  FILLER          PIC X(03)  VALUE   "107".
           03  FILLER          PIC X(24)  VALUE   "Â¡´ï".
      *
           03  FILLER          PIC X(03)  VALUE   "108".
           03  FILLER          PIC X(24)  VALUE   "·ï".
      *
           03  FILLER          PIC X(03)  VALUE   "109".
           03  FILLER          PIC X(24)  VALUE   "Éô°Ì".
      *
           03  FILLER          PIC X(03)  VALUE   "110".
           03  FILLER          PIC X(24)  VALUE   "»è".
      *
           03  FILLER          PIC X(03)  VALUE   "111".
           03  FILLER          PIC X(24)  VALUE   "¶É½ê".
      *
           03  FILLER          PIC X(03)  VALUE   "112".
           03  FILLER          PIC X(24)  VALUE   "¼ïÌÜ".
      *
           03  FILLER          PIC X(03)  VALUE   "113".
           03  FILLER          PIC X(24)  VALUE   "¥¹¥­¥ã¥ó".
      *
           03  FILLER          PIC X(03)  VALUE   "114".
           03  FILLER          PIC X(24)  VALUE   "¥³¥Þ".
      *
           03  FILLER          PIC X(03)  VALUE   "115".
           03  FILLER          PIC X(24)  VALUE   "½èÍý".
      *
           03  FILLER          PIC X(03)  VALUE   "116".
           03  FILLER          PIC X(24)  VALUE   "»Ø".
      *
           03  FILLER          PIC X(03)  VALUE   "117".
           03  FILLER          PIC X(24)  VALUE   "»õ".
      *
           03  FILLER          PIC X(03)  VALUE   "118".
           03  FILLER          PIC X(24)  VALUE   "ÌÌ".
      *
           03  FILLER          PIC X(03)  VALUE   "119".
           03  FILLER          PIC X(24)  VALUE   "Â¦".
      *
           03  FILLER          PIC X(03)  VALUE   "120".
           03  FILLER          PIC X(24)  VALUE   "¸Ä½ê".
      *
           03  FILLER          PIC X(03)  VALUE   "121".
           03  FILLER          PIC X(24)  VALUE   "Æü".
      *
           03  FILLER          PIC X(03)  VALUE   "122".
           03  FILLER          PIC X(24)  VALUE   "ÄÇ´Ö".
      *
           03  FILLER          PIC X(03)  VALUE   "123".
           03  FILLER          PIC X(24)  VALUE   "¶Ú".
      *
           03  FILLER          PIC X(03)  VALUE   "124".
           03  FILLER          PIC X(24)  VALUE   "¶Ý¼ï".
      *
           03  FILLER          PIC X(03)  VALUE   "125".
           03  FILLER          PIC X(24)  VALUE   "¹àÌÜ".
      *
           03  FILLER          PIC X(03)  VALUE   "126".
           03  FILLER          PIC X(24)  VALUE   "²Õ½ê".
      *
           03  FILLER          PIC X(03)  VALUE   "127".
           03  FILLER          PIC X(24)  VALUE   "ÄÇµÝ".
      *
           03  FILLER          PIC X(03)  VALUE   "128".
           03  FILLER          PIC X(24)  VALUE   "¿©".
      *H30.4
           03  FILLER          PIC X(03)  VALUE   "131".
           03  FILLER          PIC X(24)  VALUE   "·î".
      *
           03  FILLER          PIC X(03)  VALUE   "140".
           03  FILLER          PIC X(24)  VALUE   "¿À·Ð".
      *H24.4
           03  FILLER          PIC X(03)  VALUE   "158".
           03  FILLER          PIC X(24)  VALUE   "ÄÇÂÎ".
      *
      *
       01  MTANI-DATA-REC-R         REDEFINES   MTANI-DATA-REC.
      *****03  MTANI-TBL-OCC        OCCURS    59   INDEXED   MTANI-IDX.
      **** 03  MTANI-TBL-OCC        OCCURS    83   INDEXED   MTANI-IDX.
           03  MTANI-TBL-OCC        OCCURS    93   INDEXED   MTANI-IDX.
               05  MTANI-TANICD        PIC X(03).
               05  MTANI-TANINAME      PIC X(24).
      *
       01  MTANI-MAX                   PIC 9(04)   VALUE   93.
      *
