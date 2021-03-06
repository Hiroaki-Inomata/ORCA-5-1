      *******************************************************************
      * Project code name "ORCA"
      * ๚ใWZvg\tgiJMA standard receipt softwarej
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
      *    ใ๚๎\i้สงj     COPY (SKT1115.INC)               
      *
      *  ์ฌ๚t    ์ฦา        Lq
      *  08/04/11    g์          VK์ฌ(ใ๚๎าฮ)
      ********************************************************************
      *  Rs[ๅCณ๐
      * Maj/Min/Rev  Cณา       ๚t      เe
      ********************************************************************
        01  SKT1115.
            03  SKT1115-NO               PIC X(04).
            03  SKT1115-SRYYM            PIC X(16).
            03  SKT1115-SEIYMD           PIC X(22).
            03  SKT1115-HKNJANAME        PIC X(20).
            03  SKT1115-ADRS             PIC X(100).
            03  SKT1115-HOSPNAME         PIC X(100).
            03  SKT1115-KAISETUNAME      PIC X(40).
            03  SKT1115-TEL              PIC X(15).
            03  SKT1115-HKNJANUM         PIC X(04).
            03  SKT1115-HOSPCD           PIC X(14).
            03  SKT1115-ZAI1             PIC X(02).
            03  SKT1115-ZAI2             PIC X(02).
            03  SKT1115-SEIKYU-TBL  OCCURS  2 TIMES.
              05  SKT1115-NYUGAI-TBL  OCCURS  2 TIMES.
                07  SKT1115-KENSU        PIC X(05).
                07  SKT1115-NISSU        PIC X(05).
                07  SKT1115-TENSU        PIC X(09).
                07  SKT1115-ITBFTN       PIC X(09).
              05  SKT1115-S-KENSU        PIC X(05).
              05  SKT1115-S-NISSU        PIC X(05).
              05  SKT1115-S-KINGK        PIC X(09).
              05  SKT1115-S-FTNGK        PIC X(09).
            03  SKT1115-KOH-TBL  OCCURS  2 TIMES.
              05  SKT1115-KOHNAME        PIC X(10).
              05  SKT1115-HBTNUM         PIC X(02).
              05  SKT1115-KOHMARU        PIC X(02).
              05  SKT1115-KNYUGAI-TBL  OCCURS  2 TIMES.
                07  SKT1115-KKENSU       PIC X(05).
                07  SKT1115-KNISSU       PIC X(05).
                07  SKT1115-KTENSU       PIC X(09).
                07  SKT1115-KITBFTN      PIC X(09).
              05  SKT1115-S-KKENSU       PIC X(05).
              05  SKT1115-S-KNISSU       PIC X(05).
              05  SKT1115-S-KKINGK       PIC X(09).
              05  SKT1115-S-KFTNGK       PIC X(09).
            03  SKT1115-T-KENSU          PIC X(05).
            03  SKT1115-T-NISSU          PIC X(05).
            03  SKT1115-T-TENSU          PIC X(09).
