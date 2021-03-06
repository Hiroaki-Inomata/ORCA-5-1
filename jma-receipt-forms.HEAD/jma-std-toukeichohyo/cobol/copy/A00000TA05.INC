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
      *  VXe¼        : nqb`
      *  TuVXe¼    :  [
      *  R|[lg¼  : wèa¼EfÃs×³Òê
      *  ÇÒ            : 
      *  Maj/Min/Rev  C³Ò  út        àe
      *   01.00.00    c    2011/03/07  VKì¬
      *****************************************************************
       01  TA05.
      *    fÃÎÛJnú
           03  TA05-SRYSTYMD          PIC X(22).
      *    fÃÎÛI¹ú
           03  TA05-SRYEDMD           PIC X(12).
      *    üOæª
           03  TA05-NYUGAIKBN         PIC X(10).
      *    Å
           03  TA05-PAGE              PIC X(03).
      *    vÅ
           03  TA05-GPAGE             PIC X(03).
      *    ­sú
           03  TA05-PRTYMD            PIC X(22).
      *    õÎÛfÃs×Ea¼
           03  TA05-SEARCHNAME        PIC X(160).
      *
           03  TA05-NAIYO-TBL         OCCURS  22 TIMES.
      *    AÔ
             05  TA05-RENNUM          PIC X(03).
      *    ³ÒÔ
             05  TA05-PTNUM           PIC X(10).
      *    ³ÒÔ(11Èã)
             05  TA05-PTNUM2          PIC X(20).
      *    Ji¼
             05  TA05-KANA-NAME       PIC X(30).
      *    ¼
             05  TA05-NAME            PIC X(22).
      *    «Ê
             05  TA05-SEX             PIC X(02).
      *    Nî
             05  TA05-AGE             PIC X(03).
      *    a¼
             05  TA05-BYOMEI          PIC X(22).
      *    a¼(12¶Èã)
             05  TA05-BYOMEI2         PIC X(44).
      *    a¼Jnú
             05  TA05-BYOSTYMD        PIC X(18).
      *    ]Aú
             05  TA05-TENKIYMD        PIC X(18).
      *    ]A
             05  TA05-TENKI           PIC X(04).
      *    fÃs×¼
             05  TA05-SRYNAME         PIC X(22).
      *    fÃs×¼(12¶Èã)
             05  TA05-SRYNAME2        PIC X(44).
      *    ÅI@ú
             05  TA05-LASTYMD         PIC X(18).
