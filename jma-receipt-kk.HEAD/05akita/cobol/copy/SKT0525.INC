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
      *    ãúî¿iHc§j
      *
      *  ì¬út    ìÆÒ        Lq
      *  08/04/27    å|          VKì¬(ãúîÒÎ)
      ********************************************************************
      *  Rs[åC³ð
      * Maj/Min/Rev  C³Ò       út      àe
      ********************************************************************
        01  SKT0525.
      *    fÃN
            03  SKT0525-SRYYM             PIC X(16).
      *    ¿Nú
            03  SKT0525-SEIYMD            PIC X(22).
      *    _\æª
            03  SKT0525-TENHYOKBN-TBL     OCCURS  2.
                05  SKT0525-TENHYOKBN     PIC X(02).
      *    Û¯Ò¼
            03  SKT0525-HKNJANAME         PIC X(30).
      *    Û¯ÒÔ
            03  SKT0525-HKNJANUM          PIC X(04).
      *    Z
            03  SKT0525-ADRS              PIC X(100).
      *    ãÃ@Ö¼
            03  SKT0525-HOSPNAME          PIC X(100).
      *    JÝÒ¼
            03  SKT0525-KAISETUNAME       PIC X(40).
      *    ãÃ@ÖR[h
            03  SKT0525-HOSPCD            PIC X(20).
      *    ÝfEÝã
            03  SKT0525-ZAI1           PIC X(02).
            03  SKT0525-ZAI2           PIC X(02).
      *    y[WÔ
            03  SKT0525-NO                PIC X(04).
      *    Vl¿e[u
            03  SKT0525-SEIKYU-TBL  OCCURS  2 TIMES.
      *    üOe[u
              05  SKT0525-NYUGAI-TBL  OCCURS  2 TIMES.
      *    
                07  SKT0525-KENSU         PIC X(05).
      *    fÃÀú
                07  SKT0525-NISSU         PIC X(05).
      *    _
                07  SKT0525-TENSU         PIC X(10).
      *    êSà
                07  SKT0525-ITBFTN        PIC X(10).
      *    HÃ{F
              05  SKT0525-S-KENSU         PIC X(05).
      *    HÃ{Fú
              05  SKT0525-S-NISSU         PIC X(05).
      *    HÃ{Fàz
              05  SKT0525-S-KINGK         PIC X(10).
      *    HÃ{FWSz
              05  SKT0525-S-FTNGK         PIC X(10).
      *    öï¿e[u
            03  SKT0525-KOUHI-TBL.
      *    öï¼
              05  SKT0525-HBTNUM          PIC X(02).
              05  SKT0525-KOHNAME         PIC X(10).
      *    üOe[u
              05  SKT0525-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    
                07  SKT0525-KKENSU        PIC X(05).
      *    fÃÀú
                07  SKT0525-KNISSU        PIC X(05).
      *    _
                07  SKT0525-KTENSU        PIC X(10).
      *    êSà
                07  SKT0525-KITBFTN       PIC X(10).
      *    HÃ{F
              05  SKT0525-S-KKENSU        PIC X(05).
      *    HÃ{Fú
              05  SKT0525-S-KNISSU        PIC X(05).
      *    HÃ{Fàz
              05  SKT0525-S-KKINGK        PIC X(10).
      *    HÃ{FWSz
              05  SKT0525-S-KFTNGK        PIC X(10).
