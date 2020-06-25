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
      *  01.00.01    吉川         10/06/07  帳票様式の変更に伴う修正
      *  01.00.02    萩野         18/11/07  修正履歴コメント削除
      *  01.00.03    萩野         18/11/07  改元対応
      ********************************************************************
       01  SC19.
      *診療年月
           03  SC19-SRYGG                 PIC  X(04).
           03  SC19-SRYYY                 PIC  X(04).
           03  SC19-SRYMM                 PIC  X(04).
      *医療機関CD
           03  SC19-HOSPCD                PIC  X(14).
      *医療機関所在地
           03  SC19-ADRS                  PIC  X(100).
      *医療機関名称
           03  SC19-HOSPNAME              PIC  X(100).
      *開設者氏名
           03  SC19-KAISETU               PIC  X(30).
      *枚数
           03  SC19-MAISU                 PIC  X(05) OCCURS 6.
      *受領委任額
           03  SC19-KINGK                 PIC  X(09) OCCURS 6.
