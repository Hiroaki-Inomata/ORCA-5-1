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
      ********************************************************************
      *    ãúî\i­§j
      *
      *  ì¬út    ìÆÒ  Lq
      *  08/04/27    å|    VKì¬(ãúîÒÎ)
      ********************************************************************
      *  Rs[åC³ð
      * Maj/Min/Rev  C³Ò  út        àe
      *  01.00.01    gì    11/06/17    HúÌðâ·
      *  01.00.02    åÔ    19/02/04    C³ðRgí
      ********************************************************************
        01  SKT4615.
      *    fÃN
            03  SKT4615-SRYYM             PIC X(16).
      *    ¿Nú
            03  SKT4615-SEIYMD            PIC X(22).
      *    Z
            03  SKT4615-ADRS              PIC X(100).
      *    ãÃ@Ö¼
            03  SKT4615-HOSPNAME          PIC X(100).
      *    JÝÒ¼
            03  SKT4615-KAISETUNAME       PIC X(40).
      *    ãÃ@ÖR[h
            03  SKT4615-HOSPCD            PIC X(20).
      *    Û¯Ò¼
            03  SKT4615-HKNJANAME         PIC X(20).
      *    Û¯ÒÔ
            03  SKT4615-HKNJANUM          PIC X(12).
      *    ÝfEÝã
            03  SKT4615-ZAI1           PIC X(02).
            03  SKT4615-ZAI2           PIC X(02).
      *    y[WÔ
            03  SKT4615-NO                PIC X(04).
      *    Vl¿e[u
            03  SKT4615-SEIKYU-TBL  OCCURS  2 TIMES.
      *    üOe[u
              05  SKT4615-NYUGAI-TBL  OCCURS  2 TIMES.
      *    
                07  SKT4615-KENSU         PIC X(05).
      *    fÃÀú
                07  SKT4615-NISSU         PIC X(05).
      *    _
                07  SKT4615-TENSU         PIC X(10).
      *    êSà
                07  SKT4615-ITBFTN        PIC X(10).
      *    HÃ{F
              05  SKT4615-S-KENSU         PIC X(05).
      *    HÃ{Fú
              05  SKT4615-S-NISSU         PIC X(06).
      *    HÃ{Fàz
              05  SKT4615-S-KINGK         PIC X(10).
      *    HÃ{FWSz
              05  SKT4615-S-FTNGK         PIC X(10).
      *    öï¿e[u
            03  SKT4615-KOUHI-TBL  OCCURS  4 TIMES.
      *    öï¼
              05  SKT4615-HBTNUM          PIC X(02).
              05  SKT4615-KOHNAME         PIC X(10).
      *    üOe[u
              05  SKT4615-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    
                07  SKT4615-KKENSU        PIC X(05).
      *    fÃÀú
                07  SKT4615-KNISSU        PIC X(05).
      *    _
                07  SKT4615-KTENSU        PIC X(10).
      *    êSà
                07  SKT4615-KITBFTN       PIC X(10).
      *    HÃ{F
              05  SKT4615-S-KKENSU        PIC X(05).
      *    HÃ{Fú
              05  SKT4615-S-KNISSU        PIC X(06).
      *    HÃ{Fàz
              05  SKT4615-S-KKINGK        PIC X(10).
      *    HÃ{FWSz
              05  SKT4615-S-KFTNGK        PIC X(10).
