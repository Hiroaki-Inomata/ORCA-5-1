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
      *請求年月
           03  SC13-SEIYM               PIC X(16).
      *医療機関名称
           03  SC13-HOSPNAME            PIC X(100).
      *医療機関コード
           03  SC13-HOSPCD-TBL.
               05  SC13-HOSPCD          PIC X(02)  OCCURS 10.
      *市町村名
           03  SC13-CITYNAME            PIC X(10).
      *市町村番号
           03  SC13-CITYNUM-TBL.
               05  SC13-CITYNUM         PIC X(02)  OCCURS 3.
      *明細テーブル
           03  SC13-MEISAI-TBL          OCCURS 5.
      *受給者証番号
               05  SC13-JKYSNUM-TBL.
                   07  SC13-JKYSNUM     PIC X(02)  OCCURS 11.
      *保険者番号
               05  SC13-HKNJANUM-TBL.
                   07  SC13-HKNJANUM    PIC X(02)  OCCURS 8.
      *記号
               05  SC13-KIGO            PIC X(20).
      *番号
               05  SC13-NUM             PIC X(20).
      *氏名
               05  SC13-NAME            PIC X(40).
      *性別
               05  SC13-SEX             PIC X(04).
      *生年月日
               05  SC13-BIRTHDAY        PIC X(16).
      *診療年月
               05  SC13-SRYYM           PIC X(16).
      *入外
               05  SC13-NYUGAIKBN       PIC X(06).
      *一部負担金
               05  SC13-ITBFTN          PIC X(09).
      *処方箋発行医療機関
               05  SC13-SYOHOSEN-TBL.
                   07  SC13-SYOHOSEN    PIC X(02)  OCCURS 10.
      *窓口負担額
               05  SC13-KINGAKU         PIC X(09).
      *市町村合計：件数
           03  SC13-GKENSU              PIC X(05).
      *市町村合計：一部負担金
           03  SC13-GITBFTN             PIC X(10).
      *市町村合計：窓口負担額
           03  SC13-GKINGAKU            PIC X(10).
      *市町村合計：頁
           03  SC13-PAGE                PIC X(03).
      *市町村合計：頁計
           03  SC13-GPAGE               PIC X(03).
