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
      ************************************************************
      *    Æþ±¡CLAIM¹àÌÜID  COPY (CPNCLAIMID.INC)                *
      ************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ     ÆâÍÆ
      ************************************************************
       01  NCLAIMID-AREA.
           03  NCLAIMID-CLASS          PIC X(20)   VALUE   "Claim010".
           03  NCLAIMID-ORDER          PIC X(20)   VALUE   "Claim011".
           03  NCLAIMID-ITEM-VAL.
             05  NCLAIMID-NYUKBN-G.
               07  NCLAIMID-NYUKBN
                                PIC X(20)   VALUE   "Claim012".
               07  NCLAIMID-NYUKBNJMA
                                PIC X(05)   VALUE   "00001".
               07  NCLAIMID-NYUKBNMES
                                PIC X(100)  VALUE   "Æþ±¡¶èÊ¬".
             05  NCLAIMID-SHOKAI-G.
               07  NCLAIMID-SHOKAI
                                PIC X(20)   VALUE   "Claim013".
               07  NCLAIMID-SHOKAIJMA
                                PIC X(05)   VALUE   "00002".
               07  NCLAIMID-SHOKAIMES
                                PIC X(100)  VALUE   "·ÑÂ³Æþ±¡¶èÊ¬".
             05  NCLAIMID-BTUNUM-G.
               07  NCLAIMID-BTUNUM
                                PIC X(20)   VALUE   "Claim014".
               07  NCLAIMID-BTUNUMJMA
                                PIC X(05)   VALUE   "00003".
               07  NCLAIMID-BTUNUMMES
                                PIC X(100)  VALUE   "ÉÂÅï".
             05  NCLAIMID-BRMNUM-G.
               07  NCLAIMID-BRMNUM
                                PIC X(20)   VALUE   "Claim015".
               07  NCLAIMID-BRMNUMJMA
                                PIC X(05)   VALUE   "00004".
               07  NCLAIMID-BRMNUMMES
                                PIC X(100)  VALUE   "ÉÂ¼¼".
             05  NCLAIMID-SAGAKU-G.
               07  NCLAIMID-SAGAKU
                                PIC X(20)   VALUE   "Claim016".
               07  NCLAIMID-SAGAKUJMA
                                PIC X(05)   VALUE   "00005".
               07  NCLAIMID-SAGAKUMES
                                PIC X(100)  VALUE   "¼¼ÎÁº¹³Û".
             05  NCLAIMID-DR-G.
               07  NCLAIMID-DR
                                PIC X(20)   VALUE   "Claim017".
               07  NCLAIMID-DRJMA
                                PIC X(05)   VALUE   "00006".
               07  NCLAIMID-DRMES
                                PIC X(100)  VALUE   "Ã´Åö°å".
             05  NCLAIMID-TAIINCD-G.
               07  NCLAIMID-TAIINCD
                                PIC X(20)   VALUE   "MML0016".
               07  NCLAIMID-TAIINCDJMA
                                PIC X(05)   VALUE   "00007".
               07  NCLAIMID-TAIINCDMES
                                PIC X(100)  VALUE   "Âà±¡»öÍ³".
             05  NCLAIMID-TOKNYUIN-G.
               07  NCLAIMID-TOKNYUIN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TOKNYUINJMA
                                PIC X(05)   VALUE  "00008".
               07  NCLAIMID-TOKNYUINMES
                                PIC X(100)  VALUE  "ÆÃÄêÆþ±¡ÎÁ".
             05  NCLAIMID-NYUINKA-G.
               07  NCLAIMID-NYUINKA
                                PIC X(20)   VALUE  "MML0028".
               07  NCLAIMID-NYUINKAJMA
                                PIC X(05)   VALUE  "00009".
               07  NCLAIMID-NYUINKAMES
                                PIC X(100)  VALUE  "Æþ±¡²Ê".
             05  NCLAIMID-HKNCOMBI-G.
               07  NCLAIMID-HKNCOMBI
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-HKNCOMBIJMA
                                PIC X(05)   VALUE  "00010".
               07  NCLAIMID-HKNCOMBIMES
                                PIC X(100)  VALUE  "ÊÝ¸±ÁÈ¹ç¤»".
             05  NCLAIMID-SKJTIME-G.
               07  NCLAIMID-SKJTIME
                                PIC X(20)   VALUE  "Claim018".
               07  NCLAIMID-SKJTIMEJMA
                                PIC X(05)   VALUE  "00011".
               07  NCLAIMID-SKJTIMEMES
                                PIC X(100)  VALUE  "¿©»ö»þ´Ö".
             05  NCLAIMID-NISSU-G.
               07  NCLAIMID-NISSU
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NISSUJMA
                                PIC X(05)   VALUE  "00012".
               07  NCLAIMID-NISSUMES
                                PIC X(100)  VALUE  "°ì³çÆþÎÏ".
             05  NCLAIMID-TEIKIKBN-G.
               07  NCLAIMID-TEIKIKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TEIKIKBNJMA
                                PIC X(05)   VALUE  "00013".
               07  NCLAIMID-TEIKIKBNMES
                                PIC X(100)  VALUE  "Äê´üÀÁµá¶èÊ¬".
             05  NCLAIMID-KAIGOKBN-G.
               07  NCLAIMID-KAIGOKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KAIGOKBNJMA
                                PIC X(05)   VALUE  "00014".
               07  NCLAIMID-KAIGOKBNMES
                                PIC X(100)  VALUE  "²ð¸î¤«¤é¤Î°ÛÆ°".
             05  NCLAIMID-NYUINYMD-G.
               07  NCLAIMID-NYUINYMD
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NYUINYMDJMA
                                PIC X(05)   VALUE  "00015".
               07  NCLAIMID-NYUINYMDMES
                                PIC X(100)  VALUE  "Æþ±¡Æü".
             05  NCLAIMID-GAIHAKUKBN-G.
               07  NCLAIMID-GAIHAKUKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-GAIHAKUKBNJMA
                                PIC X(05)   VALUE  "00016".
               07  NCLAIMID-GAIHAKUKBNMES
                                PIC X(100)  VALUE  "³°Çñ¶èÊ¬".
             05  NCLAIMID-SKJTYPE-G.
               07  NCLAIMID-SKJTYPE
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SKJTYPEJMA
                                PIC X(05)   VALUE  "00017".
               07  NCLAIMID-SKJTYPEMES
                                PIC X(100)  VALUE  "¿©»ö¤Î¼ïÎà".
             05  NCLAIMID-MEDID-G.
               07  NCLAIMID-MEDID
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDIDJMA
                                PIC X(05)   VALUE  "00018".
               07  NCLAIMID-MEDIDMES
                                PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜÈÖ¹æ".
             05  NCLAIMID-MEDDATA-G.
               07  NCLAIMID-MEDDATA
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDDATAJMA
                                PIC X(05)   VALUE  "00019".
               07  NCLAIMID-MEDDATAMES
                       PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜ¿ÇÎÅÆü¤Î¾õÂÖ".
             05  NCLAIMID-MEDALL-G.
               07  NCLAIMID-MEDALL
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-MEDALLJMA
                                PIC X(05)   VALUE  "00020".
               07  NCLAIMID-MEDALLMES
                       PIC X(100)  VALUE  "°åÎÅ¶èÊ¬¹àÌÜÊ£¿ôÆüÀßÄê".
             05  NCLAIMID-ADLID-G.
               07  NCLAIMID-ADLID
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLIDJMA
                                PIC X(05)   VALUE  "00021".
               07  NCLAIMID-ADLIDMES
                       PIC X(100)  VALUE  "£Á£Ä£Ì¹àÌÜÈÖ¹æ".
             05  NCLAIMID-ADLTEN-G.
               07  NCLAIMID-ADLTEN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLTENJMA
                                PIC X(05)   VALUE  "00022".
               07  NCLAIMID-ADLTENMES
                       PIC X(100)  VALUE  "£Á£Ä£Ì¹àÌÜ¿ÇÎÅÆü¤ÎÅÀ¿ô".
             05  NCLAIMID-ADLALL-G.
               07  NCLAIMID-ADLALL
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ADLALLJMA
                                PIC X(05)   VALUE  "00023".
               07  NCLAIMID-ADLALLMES
                       PIC X(100)  VALUE  "£Á£Ä£ÌÅÀ¿ôÊ£¿ôÆüÀßÄê".
             05  NCLAIMID-TOKHIGAITOKBN-G.
               07  NCLAIMID-TOKHIGAITOKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TOKHIGAITOKBNJMA
                                PIC X(05)   VALUE  "00024".
               07  NCLAIMID-TOKHIGAITOKBNMES
                                PIC X(100)  VALUE  "»»ÄêÍ×·ïÈó³ºÅö¶èÊ¬".
             05  NCLAIMID-SENTEIKBN-G.
               07  NCLAIMID-SENTEIKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SENTEIKBNJMA
                                PIC X(05)   VALUE  "00025".
               07  NCLAIMID-SENTEIKBNMES
                                PIC X(100)  VALUE  "ÁªÄêÆþ±¡".
             05  NCLAIMID-NACCTKBN-G.
               07  NCLAIMID-NACCTKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NACCTKBNJMA
                                PIC X(05)   VALUE  "00026".
               07  NCLAIMID-NACCTKBNMES
                                PIC X(100)  VALUE  "Æþ±¡²ñ·×".
             05  NCLAIMID-DISPKBN-G.
               07  NCLAIMID-DISPKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-DISPKBNJMA
                                PIC X(05)   VALUE  "00027".
               07  NCLAIMID-DISPKBNMES
                                PIC X(100)
                                    VALUE  "¸¡º÷»þ´µ¼ÔÉ½¼¨".
             05  NCLAIMID-KSNCD-G.
               07  NCLAIMID-KSNCD
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KSNCDJMA
                                PIC X(05)   VALUE  "00028".
               07  NCLAIMID-KSNCDMES
                                PIC X(100)
                                    VALUE  "Æþ±¡²Ã»»".
             05  NCLAIMID-ENDYMD-G.
               07  NCLAIMID-ENDYMD
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ENDYMDJMA
                                PIC X(05)   VALUE  "00029".
               07  NCLAIMID-ENDYMDMES
                                PIC X(100)
                                    VALUE  "½ªÎ»Æü".
             05  NCLAIMID-BUNBENKBN-G.
               07  NCLAIMID-BUNBENKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-BUNBENKBNJMA
                                PIC X(05)   VALUE  "00030".
               07  NCLAIMID-BUNBENKBNMES
                                PIC X(100)
                                    VALUE  "Ê¬ÊÚ¶èÊ¬".
             05  NCLAIMID-RIYOUKBN-G.
               07  NCLAIMID-RIYOUKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-RIYOUKBNJMA
                                PIC X(05)   VALUE  "00031".
               07  NCLAIMID-RIYOUKBNMES
                                PIC X(100)
                                    VALUE  "Ä¾ÀÜ»ÙÊ§À©ÅÙÍøÍÑ¶èÊ¬".
             05  NCLAIMID-HONKZKKBN-G.
               07  NCLAIMID-HONKZKKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-HONKZKKBNJMA
                                PIC X(05)   VALUE  "00032".
               07  NCLAIMID-HONKZKKBNMES
                                PIC X(100)
                                    VALUE  "ËÜ¿Í²ÈÂ²¶èÊ¬".
             05  NCLAIMID-HKNJANUM-G.
               07  NCLAIMID-HKNJANUM
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-HKNJANUMJMA
                                PIC X(05)   VALUE  "00033".
               07  NCLAIMID-HKNJANUMMES
                                PIC X(100)
                                    VALUE  "ÊÝ¸±¼ÔÈÖ¹æ".
             05  NCLAIMID-KIGO-G.
               07  NCLAIMID-KIGO
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KIGOJMA
                                PIC X(05)   VALUE  "00034".
               07  NCLAIMID-KIGOMES
                                PIC X(100)
                                    VALUE  "ÈïÊÝ¸±¼Ô¾Úµ­¹æ".
             05  NCLAIMID-NUM-G.
               07  NCLAIMID-NUM
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NUMJMA
                                PIC X(05)   VALUE  "00035".
               07  NCLAIMID-NUMMES
                                PIC X(100)
                                    VALUE  "ÈïÊÝ¸±¼Ô¾ÚÈÖ¹æ".
             05  NCLAIMID-SKYYM-G.
               07  NCLAIMID-SKYYM
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SKYYMJMA
                                PIC X(05)   VALUE  "00036".
               07  NCLAIMID-SKYYMMES
                                PIC X(100)
                                    VALUE  "ÀÁµáÇ¯·î".
             05  NCLAIMID-TEISYUTUBIKBN-G.
               07  NCLAIMID-TEISYUTUBIKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TEISYUTUBIKBNJMA
                                PIC X(05)   VALUE  "00037".
               07  NCLAIMID-TEISYUTUBIKBNMES
                                PIC X(100)
                                    VALUE  "Äó½ÐÆü¶èÊ¬".
             05  NCLAIMID-SKYKBN-G.
               07  NCLAIMID-SKYKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SKYKBNJMA
                                PIC X(05)   VALUE  "00038".
               07  NCLAIMID-SKYKBNMES
                                PIC X(100)
                                    VALUE  "ÀÁµá¶èÊ¬".
             05  NCLAIMID-TEISYUTUSAKI-G.
               07  NCLAIMID-TEISYUTUSAKI
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-TEISYUTUSAKIJMA
                                PIC X(05)   VALUE  "00039".
               07  NCLAIMID-TEISYUTUSAKIMES
                                PIC X(100)
                                    VALUE  "Äó½ÐÀè¶èÊ¬".
             05  NCLAIMID-ZAIWEEK-G.
               07  NCLAIMID-ZAIWEEK
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ZAIWEEKJMA
                                PIC X(05)   VALUE  "00040".
               07  NCLAIMID-ZAIWEEKMES
                                PIC X(100)
                                    VALUE  "ºßÂÛ½µ¿ô".
             05  NCLAIMID-BIRTHDAY-G.
               07  NCLAIMID-BIRTHDAY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-BIRTHDAYJMA
                                PIC X(05)   VALUE  "00041".
               07  NCLAIMID-BIRTHDAYMES
                                PIC X(100)
                                    VALUE  "½Ð»ºÇ¯·îÆü".
             05  NCLAIMID-SIZANKBN-G.
               07  NCLAIMID-SIZANKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SIZANKBNJMA
                                PIC X(05)   VALUE  "00042".
               07  NCLAIMID-SIZANKBNMES
                                PIC X(100)
                                    VALUE  "»à»ºÍ­Ìµ¶èÊ¬".
             05  NCLAIMID-BIRTHCNT-G.
               07  NCLAIMID-BIRTHCNT
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-BIRTHCNTJMA
                                PIC X(05)   VALUE  "00043".
               07  NCLAIMID-BIRTHCNTMES
                                PIC X(100)
                                    VALUE  "½Ð»º¿ô".
             05  NCLAIMID-NYUIN-DAY-G.
               07  NCLAIMID-NYUIN-DAY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NYUIN-DAYJMA
                                PIC X(05)   VALUE  "00044".
               07  NCLAIMID-NYUIN-DAYMES
                                PIC X(100)
                                    VALUE  "Æþ±¡Æü¿ô".
             05  NCLAIMID-SANKAKBN-G.
               07  NCLAIMID-SANKAKBN
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SANKAKBNJMA
                                PIC X(05)   VALUE  "00045".
               07  NCLAIMID-SANKAKBNMES
                                PIC X(100)
                                    VALUE  "»º²Ê°åÎÅÊä½þÀ©ÅÙÂÐ¾Ý¶èÊ¬".
             05  NCLAIMID-NYU-MONEY-G.
               07  NCLAIMID-NYU-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-NYU-MONEYJMA
                                PIC X(05)   VALUE  "00046".
               07  NCLAIMID-NYU-MONEYMES
                                PIC X(100)
                                    VALUE  "Æþ±¡ÎÁ".
             05  NCLAIMID-RMSGK-MONEY-G.
               07  NCLAIMID-RMSGK-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-RMSGK-MONEYJMA
                                PIC X(05)   VALUE  "00047".
               07  NCLAIMID-RMSGK-MONEYMES
                                PIC X(100)
                                    VALUE  "¼¼ÎÁº¹³Û".
             05  NCLAIMID-KAIJO-MONEY-G.
               07  NCLAIMID-KAIJO-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KAIJO-MONEYJMA
                                PIC X(05)   VALUE  "00048".
               07  NCLAIMID-KAIJO-MONEYMES
                                PIC X(100)
                                    VALUE  "Ê¬ÊÚ²ð½õÎÁ".
             05  NCLAIMID-BUNBEN-MONEY-G.
               07  NCLAIMID-BUNBEN-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-BUNBEN-MONEYJMA
                                PIC X(05)   VALUE  "00049".
               07  NCLAIMID-BUNBEN-MONEYMES
                                PIC X(100)
                                    VALUE  "Ê¬ÊÚÎÁ".
             05  NCLAIMID-SHIN-MONEY-G.
               07  NCLAIMID-SHIN-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SHIN-MONEYJMA
                                PIC X(05)   VALUE  "00050".
               07  NCLAIMID-SHIN-MONEYMES
                                PIC X(100)
                                    VALUE  "¿·À¸»ù´ÉÍýÊÝ°éÎÁ".
             05  NCLAIMID-KNS-MONEY-G.
               07  NCLAIMID-KNS-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-KNS-MONEYJMA
                                PIC X(05)   VALUE  "00051".
               07  NCLAIMID-KNS-MONEYMES
                                PIC X(100)
                                    VALUE  "¸¡ºº¡¦ÌôºÞÎÁ".
             05  NCLAIMID-SYC-MONEY-G.
               07  NCLAIMID-SYC-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SYC-MONEYJMA
                                PIC X(05)   VALUE  "00052".
               07  NCLAIMID-SYC-MONEYMES
                                PIC X(100)
                                    VALUE  "½èÃÖ¡¦¼êÅöÎÁ".
             05  NCLAIMID-SNK-MONEY-G.
               07  NCLAIMID-SNK-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-SNK-MONEYJMA
                                PIC X(05)   VALUE  "00053".
               07  NCLAIMID-SNK-MONEYMES
                                PIC X(100)
                                    VALUE  "»º²Ê°åÎÅÊä½þÀ©ÅÙ³Ý¶â".
             05  NCLAIMID-ETC-MONEY-G.
               07  NCLAIMID-ETC-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ETC-MONEYJMA
                                PIC X(05)   VALUE  "00054".
               07  NCLAIMID-ETC-MONEYMES
                                PIC X(100)
                                    VALUE  "¤½¤ÎÂ¾".
             05  NCLAIMID-ICBFTN-MONEY-G.
               07  NCLAIMID-ICBFTN-MONEY
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-ICBFTN-MONEYJMA
                                PIC X(05)   VALUE  "00055".
               07  NCLAIMID-ICBFTN-MONEYMES
                                PIC X(100)
                                    VALUE  "°ìÉôÉéÃ´¶âÅù".
             05  NCLAIMID-BIKOU-G.
               07  NCLAIMID-BIKOU
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-BIKOUJMA
                                PIC X(05)   VALUE  "00056".
               07  NCLAIMID-BIKOUMES
                                PIC X(100)
                                    VALUE  "È÷¹Í".
             05  NCLAIMID-AUTOSET-G.
               07  NCLAIMID-AUTOSET
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-AUTOSETJMA
                                PIC X(05)   VALUE  "00057".
               07  NCLAIMID-AUTOSETMES
                                PIC X(100)
                                    VALUE  "Æþ±¡ÎÁ½é´üÃÍÀßÄê¶èÊ¬".
             05  NCLAIMID-FORCE-UPDATE-G.
               07  NCLAIMID-FORCE-UPDATE
                                PIC X(20)  VALUE  "XXXXXXX".
               07  NCLAIMID-FORCE-UPDATEJMA
                                PIC X(05)  VALUE  "00058".
               07  NCLAIMID-FORCE-UPDATEMES
                                PIC X(100)
                                    VALUE  "¶¯À©¹¹¿·".
             05  NCLAIMID-SAME-AFTER-G.
               07  NCLAIMID-SAME-AFTER
                                PIC X(20)  VALUE  "XXXXXXX".
               07  NCLAIMID-SAME-AFTERJMA
                                PIC X(05)  VALUE  "00059".
               07  NCLAIMID-SAME-AFTERMES
                                PIC X(100)
                                    VALUE  "¿©»ö·ÑÂ³¶èÊ¬".
             05  NCLAIMID-TEMPSET-G.
               07  NCLAIMID-TEMPSET
                                PIC X(20)  VALUE  "XXXXXXX".
               07  NCLAIMID-TEMPSETJMA
                                PIC X(05)  VALUE  "00060".
               07  NCLAIMID-TEMPSETMES
                                PIC X(100)
                                    VALUE  "ÊÝ¸±ÁÈ¹ç¤»²¾ÈÖ¹æÀßÄê¶èÊ¬".
             05  NCLAIMID-DESIGNRALL-G.
               07  NCLAIMID-DESIGNRALL
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-DESIGNRALLJMA
                                PIC X(05)   VALUE  "00061".
               07  NCLAIMID-DESIGNRALLMES
                                PIC X(100)
                                    VALUE  "£Ä£Å£Ó£É£Ç£Î¡Ý£Ò¹ç·×ÅÀ".
             05  NCLAIMID-UNCHKECK-G.
               07  NCLAIMID-UNCHKECK
                                PIC X(20)   VALUE  "XXXXXXX".
               07  NCLAIMID-UNCHKECKJMA
                                PIC X(05)   VALUE  "99999".
               07  NCLAIMID-UNCHKECKMES
                                PIC X(100)  VALUE  "ÉÔÌÀ".
           03  NCLAIMID-ITEM-R          REDEFINES   NCLAIMID-ITEM-VAL.
             05  NCLAIMID-ITEM-OCC      OCCURS  62.
               07  NCLAIMID-ITEM        PIC X(20).
               07  NCLAIMID-ITEMJMA     PIC X(05).
               07  NCLAIMID-ITEMMES     PIC X(100).
           03  NCLAIMID-ITEM-MAX        PIC 9(03)   VALUE   62.
