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
        01  SC03.
      *     表示年月
            03  SC03-PRTYM      PIC X(16).
      *     請求年月日
            03  SC03-SEIYMD     PIC X(22).
      *     医療機関コード
            03  SC03-HOSPCDN    PIC X(20).
      *     郵便番号
            03  SC03-POST       PIC X(08).
      *     医療機関住所
            03  SC03-HOSPADRS   PIC X(100).
      *     医療機関名称
            03  SC03-HOSPNAME   PIC X(100).
      *     電話番号
            03  SC03-TEL        PIC X(15).
      *     開設者名
            03  SC03-KAISETU    PIC X(40).
      *     請求件数(一般国保・退職国保・社保)
            03  SC03-GKENSU     PIC X(11)   OCCURS  3 TIMES.
      *     請求テーブル
            03  SC03-SEIKYU-TBL         OCCURS  20 TIMES.
      *     市町村番号
                05  SC03-CITYNUM    PIC X(06).
      *     給付割合
                05  SC03-KYURATE    PIC X(04).
      *     長期
                05  SC03-KOUGK      PIC X(02).
      *     入外テーブル
                05  SC03-NYUGAI-TBL         OCCURS  2 TIMES.
      *     件数
                    07  SC03-KENSU      PIC X(05).
      *     日数
                    07  SC03-NISSU      PIC X(05).
      *     点数
                    07  SC03-TENSU      PIC X(09).
      *     他法
                    07  SC03-TAHO       PIC X(09).
      *     薬剤一部負担金
                05  SC03-YKZFTN     PIC X(09).
