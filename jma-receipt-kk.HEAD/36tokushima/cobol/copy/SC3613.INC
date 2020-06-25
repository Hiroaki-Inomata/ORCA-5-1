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
       01  SC13.
      *表示年月
              03  SC13-PRTYY		PIC  X(04).
              03  SC13-PRTMM		PIC  X(04).
      *請求年月日
              03  SC13-SEIYY            PIC  X(04).
              03  SC13-SEIMM            PIC  X(04).
              03  SC13-SEIDD            PIC  X(04).
      *医療機関コード
              03  SC13-HOSPCDN		PIC  X(20).
      *郵便番号
              03  SC13-POST             PIC  X(08).
      *医療機関住所
              03  SC13-HOSPADRS         PIC  X(100).
      *医療機関名称
              03  SC13-HOSPNAME         PIC  X(100).
      *電話番号
              03  SC13-TEL              PIC  X(15).
      *開設者名
              03  SC13-KAISETU          PIC  X(40).
      *点数表区分
              03  SC13-TENSUHYO         PIC  X(02) OCCURS 3.
      *市町村名
              03  SC13-CITYNAME         PIC  X(20).
      *負担者番号
              03  SC13-FTNJANUM         PIC  X(16).
      *受給者氏名
              03  SC13-KANANAME1        PIC  X(30).
              03  SC13-NAME1            PIC  X(20).
      *乳幼児氏名
              03  SC13-KANANAME2        PIC  X(30).
              03  SC13-NAME2            PIC  X(20).
      *誕生年月日
              03  SC13-TANJOYY          PIC  X(04).
              03  SC13-TANJOMM          PIC  X(04).
              03  SC13-TANJODD          PIC  X(04).
      *有効期限
              03  SC13-YUKOEDYY         PIC  X(04).
              03  SC13-YUKOEDMM         PIC  X(04).
              03  SC13-YUKOEDDD         PIC  X(04).
      *受給者番号
              03  SC13-JKYSNUM          PIC  X(20).
      *記号・番号
              03  SC13-KIGO             PIC  X(20).
              03  SC13-BANGO            PIC  X(20).
      *保険者名
              03  SC13-HKNJANAME        PIC  X(20).
      *給付割合
              03  SC13-KYURATE          PIC  X(02) OCCURS 5.
      *高額療養費区分
              03  SC13-KOUGAKU          PIC  X(02) OCCURS 3.
      *請求明細
              03  SC13-SEIKYU-TBL OCCURS 2.
      *日数
                  05  SC13-NISSU        PIC  X(02).
      *点数
                  05  SC13-TENSU        PIC  X(07).
      *他法負担点数
                  05  SC13-TAHO         PIC  X(06).
      *重度心身障害者等医療費請求額
                  05  SC13-SEIKYUGK     PIC  X(09).
