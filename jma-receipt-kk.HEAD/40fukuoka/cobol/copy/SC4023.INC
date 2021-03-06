      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
      *    Áè¾aÃ{óÌØÛÒÌ
      *    ©ÈSªz\¿     iª§j   COPY (SC4023.INC)
      *
      *  ì¬út    ìÆÒ        Lq
      *  09/02/12    gì          VKì¬
      ********************************************************************
      *  Rs[åC³ð
      * Maj/Min/Rev  C³Ò  út        àe
      *  01.00.01    gì    2009/03/05  ûÀ¼`lÇÁÌ×Ì
      *                                  JÝÒ¼oCgÁ
      *  01.00.02    gì    2009/08/20  2ñÚÌü@úÔðÇÁ
      ********************************************************************
       01  SC23.
      *    ¿út
           03  SC23-SEIYMD               PIC X(22).
      *    s¬º¼
           03  SC23-CITYNAME             PIC X(30).
      *    s¬ºÔiöïSÒÔj
           03  SC23-CITYNUM              PIC X(16).
      *    fÃN
           03  SC23-SRYYM                PIC X(16).
      *    ãÃ@ÖR[h
           03  SC23-HOSPCDN              PIC X(20).
      *    ãÃ@ÖÝn
           03  SC23-ADRS                 PIC X(100).
      *    ãÃ@Ö¼
           03  SC23-HOSPNAME             PIC X(100).
      *    JÝÒ¼
           03  SC23-KAISETU              PIC X(100).
      *    âs¼
           03  SC23-GINKOU               PIC X(20).
      *    xX¼
           03  SC23-SITEN                PIC X(20).
      *    ûÀæª
           03  SC23-KOUZAKBN             PIC X(02)  OCCURS 2.
      *    ûÀÔ
           03  SC23-KOUZANUM             PIC X(10).
      *    v¿z
           03  SC23-GSEIKYUGK            PIC X(10).
      *    ¿GA
            03  SC23-SEIKYU-AREA  OCCURS 4.
      *    Û¯ÒÔ
                05  SC23-HKNJANUM        PIC X(16).
      *    LEÔ
                05  SC23-KIGONUM         PIC X(36).
      *    óÒÔ
                05  SC23-JKYSNUM         PIC X(14).
      *    ¼
                05  SC23-NAME            PIC X(20).
      *    ü@úÔ
                05  SC23-NYUINKIKAN      PIC X(14)  OCCURS 2.
      *    üOe[u
                05  SC23-NYUGAI-TBL  OCCURS 2.
      *    ú
                    07  SC23-NISSU       PIC X(02).
      *    tOàz
                    07  SC23-KYUFUGAIGK  PIC X(09).
      *    ¿àz
                    07  SC23-SEIKYUGK    PIC X(09).
