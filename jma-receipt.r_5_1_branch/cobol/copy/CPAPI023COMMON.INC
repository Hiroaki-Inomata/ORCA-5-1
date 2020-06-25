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
      ******************************************************************
      *    ¼ýÇ¼API¶¦ÄÌ¥³¥Ô¡¼¶ç          COPY (CPAPI023COMMON.INC)
      ******************************************************************
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
      *    ÀÁµáÅÀ¿ôÌ¾¾Î
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(03)   VALUE   "A00".
           03  FILLER              PIC X(30)   VALUE   "½é¡¦ºÆ¿ÇÎÁ".
           03  FILLER              PIC X(03)   VALUE   "B00".
           03  FILLER              PIC X(30)   VALUE   "°å³Ø´ÉÍýÅù".
           03  FILLER              PIC X(03)   VALUE   "C00".
           03  FILLER              PIC X(30)   VALUE   "ºßÂðÎÅÍÜ".
           03  FILLER              PIC X(03)   VALUE   "F00".
           03  FILLER              PIC X(30)   VALUE   "ÅêÌô".
           03  FILLER              PIC X(03)   VALUE   "G00".
           03  FILLER              PIC X(30)   VALUE   "Ãí¼Í".
           03  FILLER              PIC X(03)   VALUE   "J00".
           03  FILLER              PIC X(30)   VALUE   "½èÃÖ".
           03  FILLER              PIC X(03)   VALUE   "K00".
           03  FILLER              PIC X(30)   VALUE   "¼ê½Ñ".
           03  FILLER              PIC X(03)   VALUE   "L00".
           03  FILLER              PIC X(30)   VALUE   "Ëã¿ì".
           03  FILLER              PIC X(03)   VALUE   "D00".
           03  FILLER              PIC X(30)   VALUE   "¸¡ºº".
           03  FILLER              PIC X(03)   VALUE   "E00".
           03  FILLER              PIC X(30)   VALUE   "²èÁü¿ÇÃÇ".
           03  FILLER              PIC X(03)   VALUE   "H00".
           03  FILLER              PIC X(30)   VALUE   "¥ê¥Ï¥Ó¥ê".
           03  FILLER              PIC X(03)   VALUE   "I00".
           03  FILLER              PIC X(30)   VALUE   "Àº¿À²ÊÀìÌç".
           03  FILLER              PIC X(03)   VALUE   "M00".
           03  FILLER              PIC X(30)   VALUE   "Êü¼ÍÀþ¼£ÎÅ".
           03  FILLER              PIC X(03)   VALUE   "N00".
           03  FILLER              PIC X(30)   VALUE   "ÉÂÍý¿ÇÃÇ".
           03  FILLER              PIC X(03)   VALUE   "A10".
           03  FILLER              PIC X(30)   VALUE   "Æþ±¡ÎÁ".
           03  FILLER              PIC X(03)   VALUE   "001".
           03  FILLER              PIC X(30)   VALUE   "ÎÅÍÜÃ´Åö¼êÅö".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI-OCC  OCCURS   16.
               05  TBL-HKNTEN-KBN  PIC X(03).
               05  TBL-HKNTEN-MEI  PIC X(30).
