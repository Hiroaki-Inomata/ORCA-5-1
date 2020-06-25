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
      *----------------------------------------------------------------*
      *   ¥ê¥Ï¥Ó¥ê¥³¡¼¥É¥Æ¡¼¥Ö¥ë¡ÊÏ«ºÒ³°¡Ë COPY (CMRIHATBL2016.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 04.08.00     NACL-Â¿¡¹Ç¼  16/09/14  £È£²£¸Ç¯£±£°·î²þÀµÂÐ±þ
      * 05.00.00     NACL-Â¿¡¹Ç¼  19/03/14  £È£³£±Ç¯£´·îÇÑ»ß¥³¡¼¥ÉÂÐ±þ
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¡¼¥É
       01  TBL-RIHABIRI2016-AREA.
      *    ¿´·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê
           03  FILLER              PIC X(13)   VALUE   "1 1 180027410".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "1 2 180027510".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê
           03  FILLER              PIC X(13)   VALUE   "2 1 180027610".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 2 180027710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 3 180030810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë
           03  FILLER              PIC X(13)   VALUE   "211 180033910".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212 180034110".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213 180034310".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç¡Ë
           03  FILLER              PIC X(13)   VALUE   "211 180043430".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212 180043630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213 180043830".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  FILLER              PIC X(13)   VALUE   "211 180044310".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "212 180044410".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "213 180044510".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê
           03  FILLER              PIC X(13)   VALUE   "3 1 180032710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2 180027810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2 180027910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë
           03  FILLER              PIC X(13)   VALUE   "311 180034510".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180034610".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180034710".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç
           03  FILLER              PIC X(13)   VALUE   "311 180044030".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180044130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312 180044230".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  FILLER              PIC X(13)   VALUE   "311 180045810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312 180045910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312 180046010".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê
           03  FILLER              PIC X(13)   VALUE   "4 1 180028010".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "4 2 180028110".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²
           03  FILLER              PIC X(13)   VALUE   "9 1 180044610".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2 180044710".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2 180044810".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  FILLER              PIC X(13)   VALUE   "911 180044910".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "912 180045110".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "913 180045310".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë
           03  FILLER              PIC X(13)   VALUE   "911 180045010".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912 180045210".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913 180045410".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç
           03  FILLER              PIC X(13)   VALUE   "911 180045530".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912 180045630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913 180045730".
           03  FILLER              PIC X(01)   VALUE   "1".
      *
      *H28.19 ¸ºÅÀ
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "2 1G180050330".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 2G180050430".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "2 3G180050530".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "211G180050730".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212G180050930".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213G180051130".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç¡¦¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "211G180051230".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "212G180051330".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "213G180051430".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    Ç¾·ì´É¼ÀÉÂ¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "211G180050630".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "212G180050830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "213G180051030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "3 1G180052730".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2G180052830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "3 2G180052930".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "311G180053130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053330".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053530".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç¡¦¥ê¸º
           03  FILLER              PIC X(13)   VALUE   "311G180053630".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053730".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "312G180053830".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "311G180053030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312G180053230".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "312G180053430".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "9 1G180051530".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2G180051630".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "9 2G180051730".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "911G180051830".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "912G180052030".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(13)   VALUE   "913G180052230".
           03  FILLER              PIC X(01)   VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡¦Æþ±¡³°¡Ë¡Ê¥ê¸º¡Ë
           03  FILLER              PIC X(13)   VALUE   "911G180051930".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912G180052130".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913G180052330".
           03  FILLER              PIC X(01)   VALUE   "1".
      *    ÇÑÍÑ¾É¸õ·²¡ÊÍ×²ð¸î¡Ë´ð½àÉÔÅ¬¹ç¡¦¥ê¸º
           03  FILLER              PIC X(13)   VALUE   "911G180052430".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "912G180052530".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(13)   VALUE   "913G180052630".
           03  FILLER              PIC X(01)   VALUE   "1".
      *
       01  TBL-RIHABIRI2016-AREA-R REDEFINES   TBL-RIHABIRI2016-AREA.
           03  TBL-RIHABIRI2016-OCC    OCCURS  76   INDEXED   TBL-RIH16.
      *        ¥ê¥Ï¥Ó¥ê¼ïÎà
               05  TBL-RIH16-KBN        PIC X(01).
      *        1:Í×²ð¸î
               05  TBL-RIH16-KIG        PIC X(01).
      *        1:(1),2:(2),3:(3)
               05  TBL-RIH16-KBN2       PIC X(01).
      *        G:¸ºÅÀ
               05  TBL-RIH16-YOBO       PIC X(01).
               05  TBL-RIH16-SRYCD      PIC X(09).
      *H31.3
      *        1:H31.4¤«¤é·Ð²áÁ¼ÃÖÂÐ±þ
               05  TBL-RIH16-EDCHK      PIC X(01).
      *
       01  TBL-RIHA2016-MAX             PIC 9(02)  VALUE    76.
      *
