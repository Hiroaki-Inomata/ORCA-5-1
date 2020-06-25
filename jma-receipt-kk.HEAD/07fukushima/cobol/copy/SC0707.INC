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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    門間    2019/05/21  改元対応
      ********************************************************************
       01  SC07.
      *診療年月
           03  SC07-SRYGG             PIC  X(04).
           03  SC07-SRYYY             PIC  X(04).
           03  SC07-SRYMM             PIC  X(04).
      *請求日付
           03  SC07-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC07-HOSPCD            PIC  X(14).
      *市町村番号
           03  SC07-CITYNUM           PIC  X(12).
      *市町村名
           03  SC07-CITYNAME          PIC  X(20).
      *医療機関住所
           03  SC07-ADRS              PIC  X(100).
      *医療機関名称
           03  SC07-HOSPNAME          PIC  X(100).
      *電話番号
           03  SC07-TEL               PIC  X(15).
      *開設者名
           03  SC07-KAISETU           PIC  X(40).
      *枚数
           03  SC07-MAISU             PIC  X(12).
      *件数
           03  SC07-KENSU             PIC  X(12).
      *総点数
           03  SC07-TENSU             PIC  X(16).
      *一部負担金
           03  SC07-ITBFTN            PIC  X(14).
      *標準負担額
           03  SC07-FTNGK             PIC  X(16).
