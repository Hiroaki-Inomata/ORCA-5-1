      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *    在宅の処方せん切替えコードテーブル
      *                            　平成２６年４月改正から            *
      *                         COPY (CMZISOTBL2014.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       01  TBL-ZISOSIN-AREA.
      *在医総管
      *    （機能強化在支診等）（病床有・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "0111 114023110".
      *    （機能強化在支診等）（病床有・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "0121 114023310".
      *    （機能強化在支診等）（病床有・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "0212 114023210".
      *    （機能強化在支診等）（病床有・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "0222 114023410".
      *
      *    （機能強化在支診等）（病床無・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "0311 114023510".
      *    （機能強化在支診等）（病床無・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "0321 114023710".
      *    （機能強化在支診等）（病床無・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "0412 114023610".
      *    （機能強化在支診等）（病床無・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "0422 114023810".
      *
      *    （在支診等）（処方せんあり・同一建物居住者以外）
           03  FILLER              PIC X(14)   VALUE   "0511 114023910".
      *    （在支診等）（処方せんなし・同一建物居住者以外）
           03  FILLER              PIC X(14)   VALUE   "0521 114024110".
      *    （在支診等）（処方せんあり・同一建物居住者）
           03  FILLER              PIC X(14)   VALUE   "0612 114024010".
      *    （在支診等）（処方せんなし・同一建物居住者）
           03  FILLER              PIC X(14)   VALUE   "0622 114024210".
      *
      *    （在支診等以外）（処方せんあり・同一建物居住者以外）
           03  FILLER              PIC X(14)   VALUE   "0711 114024310".
      *    （在支診等以外）（処方せんなし・同一建物居住者以外）
           03  FILLER              PIC X(14)   VALUE   "0721 114024510".
      *    （在支診等以外）（処方せんあり・同一建物居住者）
           03  FILLER              PIC X(14)   VALUE   "0812 114024410".
      *    （在支診等以外）（処方せんなし・同一建物居住者）
           03  FILLER              PIC X(14)   VALUE   "0822 114024610".
      *
      *特医総管
      *    （機能強化在支診等）（病床有・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1111 114024910".
      *    （機能強化在支診等）（病床有・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1121 114025110".
      *    （機能強化在支診等）（病床有・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1212 114025010".
      *    （機能強化在支診等）（病床有・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1222 114025210".
      *
      *    （機能強化在支診等）（病床無・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1311 114025310".
      *    （機能強化在支診等）（病床無・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1321 114025510".
      *    （機能強化在支診等）（病床無・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1412 114025410".
      *    （機能強化在支診等）（病床無・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1422 114025610".
      *
      *    （在支診等）（病床無・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1511 114025710".
      *    （在支診等）（病床無・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1521 114025910".
      *    （在支診等）（病床無・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1612 114025810".
      *    （在支診等）（病床無・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1622 114026010".
      *
      *    （在支診等以外）（病床無・処方せん有・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1711 114026110".
      *    （在支診等以外）（病床無・処方せん無・同一建物以外）
           03  FILLER              PIC X(14)   VALUE   "1721 114026310".
      *    （在支診等以外）（病床無・処方せん有・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1812 114026210".
      *    （在支診等以外）（病床無・処方せん無・同一建物居住）
           03  FILLER              PIC X(14)   VALUE   "1822 114026410".
      *在宅がん医総合
      *    （在支診等）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2110 114007610".
      *    （在支診等）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2120 114007710".
      *
      *    （機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2210 114019510".
      *    （機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2220 114019610".
      *
      *    （機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2310 114019710".
      *    （機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2320 114019810".
      *
      *
       01  TBL-ZISOSIN-AREA-R      REDEFINES   TBL-ZISOSIN-AREA.
           03  TBL-ZISOSIN-OCC     OCCURS   38
                                   INDEXED  TBL-ZSIDX.
      *        同一識別（１桁目　0：在医総管、1：特医総管）
      *                          2:在宅がん医総
               05  TBL-ZS-GRP          PIC X(02).
      *        1:処方せんあり、2:処方せんなし
               05  TBL-ZS-KBN          PIC X(01).
      *        1:同一建物以外、2:同一建物居住
               05  TBL-ZS-KBN2         PIC X(01).
      *
               05  TBL-ZS-YOBI         PIC X(01).
      *
               05  TBL-ZS-SRYCD        PIC X(09).
      *
      *
