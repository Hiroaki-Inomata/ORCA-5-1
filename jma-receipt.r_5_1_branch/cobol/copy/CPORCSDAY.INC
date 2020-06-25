      *******************************************************************
      * Project code name "ORCA"
      * 泣板筛洁レセプトソフト∈JMA standard receipt software∷
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
      *  コピ〖剁饯赖旺悟
      * Maj/Min/Rev  饯赖荚       泣烧      柒推
      ************************************************************
      ******************************************************
      * 务恃垂巴完挝拌                                     
      *        巴完コ〖ド   11 ˇˇˇ 谰务ⅹ下务           
      *                   12 ˇˇˇ 下务ⅹ谰务        
      ******************************************************
       01  LNK-DAY1-AREA.
           03  LNK-DAY1-IRAI               PIC X(02).
           03  LNK-DAY1-YMD                PIC 9(08).
           03  LNK-DAY1-YMD-X   REDEFINES  LNK-DAY1-YMD.
             05  LNK-DAY1-XXXX             PIC 9(01).
             05  LNK-DAY1-WYMD             PIC 9(07).
           03  LNK-DAY1-GENGO              PIC X(01).
           03  LNK-DAY1-GENKIGO            PIC X(01).
           03  LNK-DAY1-GENMEI             PIC N(02).
           03  LNK-DAY1-FILLER             PIC X(64).
      ******************************************************
      * 泣烧试礁巴完挝拌                                   
      *        巴完コ〖ド   21 ˇˇˇ 谰务による巴完        
      *                   22 ˇˇˇ 下务による巴完         
      ******************************************************
       01  LNK-DAY2-AREA.
           03  LNK-DAY2-IRAI               PIC X(02).
           03  LNK-DAY2-YMD                PIC 9(08).
           03  LNK-DAY2-EDTYMD1.
             05  LNK-DAY2-EDTYMD1-G        PIC X(01).
             05  LNK-DAY2-EDTYMD1-Y        PIC Z9.
             05  LNK-DAY2-EDTYMD1-P1       PIC X(01).
             05  LNK-DAY2-EDTYMD1-M        PIC Z9.
             05  LNK-DAY2-EDTYMD1-P2       PIC X(01).
             05  LNK-DAY2-EDTYMD1-D        PIC Z9.
           03  LNK-DAY2-EDTYMD2.
             05  LNK-DAY2-EDTYMD2-G        PIC N(02).
             05  LNK-DAY2-EDTYMD2-Y        PIC N(02).
             05  LNK-DAY2-EDTYMD2-P1       PIC N(01).
             05  LNK-DAY2-EDTYMD2-M        PIC N(02).
             05  LNK-DAY2-EDTYMD2-P2       PIC N(01).
             05  LNK-DAY2-EDTYMD2-D        PIC N(02).
           03  LNK-DAY2-EDTYMD3.
             05  LNK-DAY2-EDTYMD3-G        PIC N(02).
             05  LNK-DAY2-EDTYMD3-Y        PIC N(02).
             05  LNK-DAY2-EDTYMD3-NEN      PIC N(01).
             05  LNK-DAY2-EDTYMD3-M        PIC N(02).
             05  LNK-DAY2-EDTYMD3-TUKI     PIC N(01).
             05  LNK-DAY2-EDTYMD3-D        PIC N(02).
             05  LNK-DAY2-EDTYMD3-HI       PIC N(01).
           03  LNK-DAY2-FILLER             PIC X(19).
      ******************************************************
      * 退泣换叫巴完挝拌                                   *
      *        巴完コ〖ド   31 ˇˇˇ 谰务による巴完            *
      *                   32 ˇˇˇ 下务による巴完            *
      ******************************************************
       01  LNK-DAY3-AREA.
           03  LNK-DAY3-IRAI               PIC X(02).
           03  LNK-DAY3-YMD                PIC 9(08).
           03  LNK-DAY3-YOBI               PIC 9(01).
           03  LNK-DAY3-YOBIKIGO           PIC X(03).
           03  LNK-DAY3-YOBIMEI            PIC N(01).
           03  LNK-DAY3-SYUKU              PIC 9(01).
           03  LNK-DAY3-YASUMI             PIC 9(01).
           03  LNK-DAY3-FILLER             PIC X(62).
      ******************************************************
      * 剿泣换叫巴完挝拌                                   *
      *        巴完コ〖ド   41 ˇˇˇ 谰务による巴完            *
      *                   42 ˇˇˇ 下务による巴完            *
      ******************************************************
       01  LNK-DAY4-AREA.
           03  LNK-DAY4-IRAI               PIC X(02).
           03  LNK-DAY4-YM                 PIC 9(06).
           03  LNK-DAY4-COUNT              PIC 9(02).
           03  LNK-DAY4-SKJTDD-R.
             05  LNK-DAY4-SKJTDD           PIC 9(02)  OCCURS  5.
           03  LNK-DAY4-FILLER             PIC X(60).
      ******************************************************
      * 袋粗泣眶纷换巴完挝拌                               *
      *        巴完コ〖ド   51 ˇˇˇ 谰务による巴完            *
      *                   52 ˇˇˇ 下务による巴完            *
      ******************************************************
       01  LNK-DAY5-AREA.
           03  LNK-DAY5-IRAI               PIC X(02).
           03  LNK-DAY5-START              PIC 9(08).
           03  LNK-DAY5-END                PIC 9(08).
           03  LNK-DAY5-NISSU1             PIC 9(05).
           03  LNK-DAY5-NISSU2             PIC 9(05).
           03  LNK-DAY5-FILLER             PIC X(52).
      ******************************************************
      * 澈碰泣换叫巴完挝拌                                 *
      *        巴完コ〖ド   61 ˇˇˇ 谰务による巴完            *
      *                   62 ˇˇˇ 下务による巴完            *
      ******************************************************
       01  LNK-DAY6-AREA.
           03  LNK-DAY6-IRAI               PIC X(02).
           03  LNK-DAY6-KIJUN              PIC 9(08).
           03  LNK-DAY6-ZOGENPTN           PIC X(01).
      *****03  LNK-DAY6-ZOGEN              PIC S9(05)   PACKED-DECIMAL.
           03  LNK-DAY6-ZOGEN              PIC S9(05).
           03  LNK-DAY6-KOYOMI             PIC 9(08).
           03  LNK-DAY6-KOYOMIZEN          PIC 9(08).
           03  LNK-DAY6-KOYOMIGO           PIC 9(08).
           03  LNK-DAY6-KEISAN             PIC 9(08).
           03  LNK-DAY6-KEISANZEN          PIC 9(08).
           03  LNK-DAY6-KEISANGO           PIC 9(08).
           03  LNK-DAY6-FILLER             PIC X(18).
      ******************************************************
      * 奉琐泣换叫巴完挝拌                                 *
      *        巴完コ〖ド   71 ˇˇˇ 谰务による巴完            *
      *                   72 ˇˇˇ 下务による巴完            *
      ******************************************************
       01  LNK-DAY7-AREA.
           03  LNK-DAY7-IRAI               PIC X(02).
           03  LNK-DAY7-YM                 PIC 9(06).
           03  LNK-DAY7-KOYOMI             PIC 9(08).
           03  LNK-DAY7-EIGYO              PIC 9(08).
           03  LNK-DAY7-FILLER             PIC X(56).
