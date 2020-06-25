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
       01  SC17.
      *請求日付
           03  SC17-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC17-HOSPCDN           PIC  X(20).
      *医療機関所在地
           03  SC17-ADRS              PIC  X(100).
      *医療機関名称
           03  SC17-HOSPNAME          PIC  X(100).
      *管理者名
           03  SC17-KAISETU           PIC  X(40).
      *市町村番号
           03  SC17-CITYNUM           PIC  X(04).
      *請求エリア
           03  SC17-SEIKYU-AREA OCCURS 5.
      *受給者番号
               05  SC17-JKYSNUM       PIC  X(14).
      *氏名
               05  SC17-NAME          PIC  X(20).
      *誕生年
               05  SC17-TANJOYY       PIC  X(04).
      *性別
               05  SC17-SEX           PIC  X(02) OCCURS 2.
      *診療年月
               05  SC17-SRYYY         PIC  X(04).
               05  SC17-SRYMM         PIC  X(04).
      *入外区分
               05  SC17-NYUGAIKBN     PIC  X(02) OCCURS 2.
      *日数
               05  SC17-NISSU         PIC  X(02).
      *継続本人・特例退職者区分
               05  SC17-KBN           PIC  X(02) OCCURS 2.
      *総医療費
               05  SC17-IRYOHI        PIC  X(09).
      *一部負担金
               05  SC17-ITBFTN        PIC  X(09).
      *請求額
               05  SC17-SEIKYUGK      PIC  X(09).
      *診療年号
           03  SC17-SRYGG             PIC  X(04) OCCURS 5.
