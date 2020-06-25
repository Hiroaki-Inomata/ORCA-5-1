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
      *  01.00.01    吉川         14/03/12  市町村コード変更
      ********************************************************************
       01  SC05.
      *    医療機関コード
           03  SC05-HOSPCD-G  OCCURS  7.
               05  SC05-HOSPCD                 PIC  X(02).
      *    発行年月日
           03  SC05-SEIYMD                     PIC  X(22).
      *    市町村名
           03  SC05-CITYNAME                   PIC  X(20)  OCCURS  2.
      *    郵便番号
           03  SC05-POST                       PIC  X(18).
      *    医療機関住所
           03  SC05-ADRS                       PIC  X(100).
      *    医療機関名称
           03  SC05-HOSPNAME                   PIC  X(100).
      *    開設者氏名
           03  SC05-KAISETU                    PIC  X(40).
      *    電話番号
           03  SC05-TEL                        PIC  X(24).
      *    市町村コード
           03  SC05-CITYCD                     PIC  X(01)  OCCURS  3.
      *    送付月
           03  SC05-SEIYM                      PIC  X(16).
      *    件数テーブル
           03  SC05-KENSU-TBL  OCCURS  6.
      *    件数
               05  SC05-KENSU                  PIC  X(05).
