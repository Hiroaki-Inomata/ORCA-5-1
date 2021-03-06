      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  VXe¼        : nqb`
      *  TuVXe¼    :  [
      *  R|[lg¼  : @EÝ@óµªÍ\
      *  ÇÒ            : 
      *  Maj/Min/Rev  C³Ò  út        àe
      *  01.00.00     c    2011/03/23  VKì¬
      ******************************************************************
       01  TA041.
      *    fÃN
           03  TA041-SRYYM           PIC X(16).
      *    O[v^Cg
           03  TA041-GRPTITLE        PIC X(20).
      *    ì¬Nú
           03  TA041-CREATEYMD       PIC X(22).
      *    Å
           03  TA041-PAGE            PIC X(02).
      *    vÅ
           03  TA041-GPAGE           PIC X(02).
      *    üOæª
           03  TA041-NYUGAI          PIC X(04).
      *    ¾×e[u
           03  TA041-MEISAI-TBL      OCCURS 7 TIMES.
      *    È¼^ãt¼
             05  TA041-KANAME        PIC X(20).
      *    JzÝ@
             05  TA041-KZAIIN        PIC X(05).
      *    ü@
             05  TA041-NYUIN         PIC X(05).
      *    Þ@
             05  TA041-TAIIN         PIC X(05).
      *    [à]S
             05  TA041-SIBOU         PIC X(05).
      *    Ý@
             05  TA041-ZAIIN         PIC X(05).
      *    Ý@×
             05  TA041-NZAIIN        PIC X(05).
      *    [à]O×
             05  TA041-NGAIHAKU      PIC X(05).
      *    Pú½Ïü@
             05  TA041-NYUIN-AVG     PIC X(05).
      *    Pú½ÏÞ@
             05  TA041-TAIIN-AVG     PIC X(05).
      *    Pú½ÏÝ@
             05  TA041-ZAIIN-AVG     PIC X(05).
      *    ve[u
           03  TA041-GOKEI-TBL.
      *    v\JzÝ@
             05  TA041-GKZAIIN       PIC X(06).
      *    v\ü@
             05  TA041-GNYUIN        PIC X(06).
      *    v\Þ@
             05  TA041-GTAIIN        PIC X(06).
      *    v\[à]S
             05  TA041-GSIBOU        PIC X(06).
      *    v\Ý@
             05  TA041-GZAIIN        PIC X(06).
      *    v\Ý@×
             05  TA041-GNZAIIN       PIC X(06).
      *    v\[à]O×
             05  TA041-GNGAIHAKU     PIC X(06).
      *    v\Pú½Ïü@
             05  TA041-GNYUIN-AVG    PIC X(06).
      *    v\Pú½Ï¤ö
             05  TA041-GTAIIN-AVG    PIC X(06).
      *    v\Pú½ÏÝ@
             05  TA041-GZAIIN-AVG    PIC X(06).
