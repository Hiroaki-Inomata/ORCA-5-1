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
      *  01.00.02    吉川         10/06/22  帳票様式の変更に伴う修正
      *  01.00.03    萩野         18/11/07  修正履歴コメント削除
      ********************************************************************
       01  SC21.
      *診療年月
           03  SC21-SRYYY                 PIC  X(04).
           03  SC21-SRYMM                 PIC  X(04).
      *医療機関CD
           03  SC21-HOSPCD                PIC  X(14).
      *医療機関所在地
           03  SC21-ADRS                  PIC  X(100).
      *医療機関名称
           03  SC21-HOSPNAME              PIC  X(100).
      *開設者氏名
           03  SC21-KAISETU               PIC  X(50).
      *医療機関電話番号
           03  SC21-TEL                   PIC  X(15).
      *レセプト種別
           03  SC21-SYUBETU               PIC  X(02) OCCURS 8.
      *老人保健負担者番号(市町村番号)
           03  SC21-CITYNUM               PIC  X(16).
      *老人保健受給者番号
           03  SC21-JKYSNUM               PIC  X(14).
      *保険者番号
           03  SC21-HKNJANUM              PIC  X(16).
      *給付割合
           03  SC21-KYURATE               PIC  X(02) OCCURS 3.
      *市町村名
           03  SC21-CITYNAME              PIC  X(16).
      *記号
           03  SC21-KIGO                  PIC  X(16).
      *番号
           03  SC21-NUM                   PIC  X(16).
      *請求日付
           03  SC21-SEIYMD                PIC  X(22).
      *氏名
           03  SC21-NAME                  PIC  X(24).
      *カナ氏名
           03  SC21-KANANAME              PIC  X(40).
      *性別
           03  SC21-SEX                   PIC  X(02) OCCURS 2.
      *元号
           03  SC21-GENGO                 PIC  X(02) OCCURS 3.
      *誕生日
           03  SC21-TANJOYMD              PIC  X(18).
      *氏名２(前期高齢者のみ)
           03  SC21-NAME2                 PIC  X(24).
      *患者住所
           03  SC21-PTADRS                PIC  X(60).
      *患者電話番号
           03  SC21-PTTEL                 PIC  X(15).
      *公費区分
           03  SC21-KOHKBN                PIC  X(02) OCCURS 6.
      *実日数
           03  SC21-NISSU                 PIC  X(02).
      *点数
           03  SC21-TENSU                 PIC  X(07).
      *請求額
           03  SC21-SEIKYUGK              PIC  X(09).
      *科別
           03  SC21-SRYKANAME             PIC  X(20).
