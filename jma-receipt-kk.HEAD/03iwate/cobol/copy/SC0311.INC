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
       01  SC11.
      *区分
           03  SC11-KBN                 PIC X(02)  OCCURS 3.
      *診療年月
           03  SC11-SRYYM               PIC X(16).
      *個別コード
           03  SC11-TENHYOKBN           PIC X(02)  OCCURS 4.
      *医療機関コード
           03  SC11-HOSPCD              PIC X(20).
      *市町村名
           03  SC11-CITYNAME            PIC X(10).
      *市町村番号
           03  SC11-CITYNUM-TBL.
               05  SC11-CITYNUM         PIC X(02)  OCCURS 2.
      *医療機関所在地
           03  SC11-HOSPADRS            PIC X(100).
      *医療機関名称
           03  SC11-HOSPNAME            PIC X(100).
      *医療機関電話番号
           03  SC11-HOSPTEL             PIC X(15).
      *請求テーブル
           03  SC11-SEIKYU-TBL          OCCURS 2.
      *件数
               05  SC11-KENSU           PIC X(05).
      *点数（金額）
               05  SC11-TENSU           PIC X(10).
      *負担金額
               05  SC11-ITBFTN          PIC X(09).
      *特記事項コード
           03  SC11-TOKKI-TBL           OCCURS 15.
               05  SC11-TOKKI-NYUGAI-TBL    OCCURS 2.
                   07  SC11-TOKKIKENSU  PIC X(03).
      *公費負担医療
           03  SC11-KOHI-TBL            OCCURS 15.
               05  SC11-KOHI-NYUGAI-TBL     OCCURS 2.
                   07  SC11-KOHIKENSU   PIC X(03).
