      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  VXeผ        : nqb`
      *  TuVXeผ    :  [
      *  R|[lgผ  : Jะฉำ๖ฑะQWv\
      *  วา            : 
      *  Maj/Min/Rev  Cณา  ๚t       เe
      *   01.00.01    ๅิ    12/11/30   VK์ฌ
      *******************************************************************
        01  TA07J.
            02  TA07J-TITLE       PIC X(22).
            02  TA07J-SRYYM       PIC X(16).
            02  TA07J-CREATEYMD   PIC X(22).
            02  TA07J-NYUGAI      PIC X(4).
            02  TA07J-PAGE        PIC X(3).
            02  TA07J-MEISAI-TBL    OCCURS  30 TIMES.
              03  TA07J-RENNUM      PIC X(3).
              03  TA07J-PTNUM10     PIC X(10).
              03  TA07J-PTNUM20     PIC X(20).
              03  TA07J-NAME24      PIC X(24).
              03  TA07J-NAME48      PIC X(48).
              03  TA07J-HKNKBN      PIC X(12).
              03  TA07J-SKYKBN      PIC X(24).
              03  TA07J-TENKI       PIC X(4).
              03  TA07J-SYMD        PIC X(9).
              03  TA07J-RYMDST      PIC X(9).
              03  TA07J-RYMDED      PIC X(9).
              03  TA07J-SKYTENSU    PIC X(9).
              03  TA07J-SKYMONEY    PIC X(11).
