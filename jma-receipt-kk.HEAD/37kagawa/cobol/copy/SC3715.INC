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
      *診療年月
           03  SC15-PRTYM               PIC  X(16).
      *請求年月日
           03  SC15-SEIYMD              PIC  X(22).
      *証明年月日
           03  SC15-SHOMEIYMD           PIC  X(22).
      *市町村名
           03  SC15-CITYNAME            PIC  X(22).
      *申請者氏名
           03  SC15-SEINAME             PIC  X(20).
      *医療機関住所
           03  SC15-HOSPADRS            PIC  X(100).
      *医療機関名称
           03  SC15-HOSPNAME            PIC  X(100).
      *開設者氏名
           03  SC15-KAISETU             PIC  X(40).
      *受給者資格証番号
           03  SC15-JKYSNUM             PIC  X(14).
      *老人保健受給者番号
           03  SC15-RJN-JKYSNUM         PIC  X(14).
      *受給者住所
           03  SC15-ADRS                PIC  X(80).
      *受給者氏名
           03  SC15-NAME                PIC  X(30).
      *生年月日
           03  SC15-TANJOYMD            PIC  X(22).
      *公費区分
           03  SC15-KOHKBN              PIC  X(02) OCCURS 2.
      *請求テーブル
           03  SC15-SEIKYU-TBL.
      *保険種別
               05  SC15-HKNSYUBETU OCCURS 6 PIC X(02).
      *保険区分(一般)
               05  SC15-HKNKBN             PIC X(02).
      *入院データ
               05  SC15-NYUIN-TBL.
      *入院−総医療費
                   07  SC15-N-IRYOGK       PIC X(18).
      *入院−一部負担額(本人負担額)
                   07  SC15-N-ITBFTNGK     PIC X(18).
      *入院−標準負担
                   07  SC15-N-HYJFTN       PIC X(18).
      *入院開始年月日
                   07  SC15-N-NYUINYMD     PIC X(22).
      *入院−継続
                   07  SC15-KEIZOKU        PIC X(02).
      *入院−入院日数
                   07  SC15-N-NISSU        PIC X(04).
      *外来データ
               05  SC15-GAIRAI-TBL.
      *外来−総医療費
                   07  SC15-G-IRYOGK       PIC X(18).
      *外来−一部負担額(本人負担額)
                   07  SC15-G-ITBFTNGK     PIC X(18).
      *外来−通院日数
                   07  SC15-G-NISSU        PIC X(04).
      *一部負担金の減額・免除(１、減額 ２、免除)
               05  SC15-GENMEN OCCURS 2    PIC X(02).
      *一部負担金区分
               05  SC15-ITBFTNKBN OCCURS 8 PIC X(02).
