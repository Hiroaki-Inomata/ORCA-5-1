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
       01  SC15.
      *請求日付
           03  SC15-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC15-HOSPCDN           PIC  X(20).
      *医療機関所在地
           03  SC15-ADRS              PIC  X(100).
      *医療機関名称
           03  SC15-HOSPNAME          PIC  X(100).
      *管理者名
           03  SC15-KAISETU           PIC  X(40).
      *市町村番号
           03  SC15-CITYNUM           PIC  X(04).
      *請求エリア
           03  SC15-SEIKYU-AREA OCCURS 5.
      *受給者番号
               05  SC15-JKYSNUM       PIC  X(14).
      *氏名
               05  SC15-NAME          PIC  X(20).
      *誕生元年
               05  SC15-GENGO         PIC  X(02) OCCURS 3.
               05  SC15-TANJOYY       PIC  X(02).
      *性別
               05  SC15-SEX           PIC  X(02) OCCURS 2.
      *診療年月
               05  SC15-SRYYY         PIC  X(04).
               05  SC15-SRYMM         PIC  X(04).
      *入外区分
               05  SC15-NYUGAIKBN     PIC  X(02) OCCURS 4.
      *日数
               05  SC15-NISSU         PIC  X(02).
      *請求額
               05  SC15-SEIKYUGK      PIC  X(09).
