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
        01  SC07.
      *     表示年月
            03  SC07-PRTYM      PIC X(16).
      *     請求年月日
            03  SC07-SEIYMD     PIC X(22).
      *     医療機関コード
            03  SC07-HOSPCDN    PIC X(20).
      *     郵便番号
            03  SC07-POST       PIC X(08).
      *     医療機関住所
            03  SC07-HOSPADRS   PIC X(100).
      *     医療機関名称
            03  SC07-HOSPNAME   PIC X(100).
      *     電話番号
            03  SC07-TEL        PIC X(15).
      *     開設者名
            03  SC07-KAISETU    PIC X(40).
      *     請求件数(一般国保・退職国保・社保)
            03  SC07-GKENSU      PIC X(11)   OCCURS  2 TIMES.
            03  SC07-SEIKYU-TBL         OCCURS  15 TIMES.
      *     市町村番号
                05  SC07-CITYNUM    PIC X(06).
      *     給付割合
                05  SC07-KYURATE    PIC X(04).
                05  SC07-NYUGAI-TBL         OCCURS  2 TIMES.
      *     件数
                    07  SC07-KENSU  PIC X(05).
      *     日数
                    07  SC07-NISSU  PIC X(05).
      *     点数
                    07  SC07-TENSU  PIC X(09).
      *     他法負担点数
                    07  SC07-TAHO   PIC X(09).
