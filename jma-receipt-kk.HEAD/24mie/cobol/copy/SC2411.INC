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
       01  SC11.
      *    送付月
           03  SC11-SEIYM                      PIC  X(16).
      *    郵便番号
           03  SC11-POST                       PIC  X(18).
      *    医療機関住所
           03  SC11-ADRS                       PIC  X(100).
      *    医療機関名称
           03  SC11-HOSPNAME                   PIC  X(100).
      *    電話番号
           03  SC11-TEL                        PIC  X(24).
      *    医療機関コード
           03  SC11-HOSPCD                     PIC  X(14).
      *    点数表区分
           03  SC11-TENHYOKBN                  PIC  X(02)  OCCURS  4.
      *    件数テーブル
           03  SC11-KENSU-TBL  OCCURS  6.
      *    件数
               05  SC11-KENSU                  PIC  X(07).
