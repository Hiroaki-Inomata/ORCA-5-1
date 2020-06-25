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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SC13.
      *    請求年月日
           03  SC13-SEIYMD                     PIC  X(22).
      *    送付年月
           03  SC13-SEIYM                      PIC  X(16).
      *    郵便番号
           03  SC13-POST                       PIC  X(10).
      *    医療機関住所
           03  SC13-ADRS                       PIC  X(100).
      *    医療機関名称
           03  SC13-HOSPNAME                   PIC  X(100).
      *    電話番号
           03  SC13-TEL                        PIC  X(15).
      *    医療機関コード
           03  SC13-HOSPCD                     PIC  X(20).
      *    点数表区分（１：医科　２：歯科　３：調剤　４：訪問看護）
           03  SC13-TENHYOKBN                  PIC  X(02)  OCCURS  4.
      *    提出方法（１：領収証明書　２：領収証明一覧表）
           03  SC13-TEISYUTU                   PIC  X(02)  OCCURS  2.
      *    件数テーブル
           03  SC13-KENSU-TBL  OCCURS  29.
      *    証明件数
               05  SC13-KENSU                  PIC  X(05).
      *    合計件数
           03  SC13-GKENSU                     PIC  X(06).
