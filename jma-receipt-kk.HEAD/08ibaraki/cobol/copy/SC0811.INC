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
      * Maj/Min/Rev  修正者  日付      内容
      *  01.00.01    吉川    09/02/06  開設者名バイト数増加
      ********************************************************************
       01  SC11.
      *請求年月日
           03  SC11-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC11-HOSPCDN           PIC  X(20).
      *点数表区分
           03  SC11-TENSUHYO          PIC  X(02) OCCURS 4.
      *有効期限
           03  SC11-YUKOYM            PIC  X(16).
      *市町村名
           03  SC11-CITYNAME          PIC  X(30).
      *医療機関所在地
           03  SC11-ADRS              PIC  X(100).
      *医療機関名称
           03  SC11-HOSPNAME          PIC  X(100).
      *開設者名
      *=== バイト数増加 by Yoshikawa(2009/02/06) start
      *     03  SC11-KAISETU           PIC  X(40).
           03  SC11-KAISETU           PIC  X(60).
      *=== バイト数増加 by Yoshikawa(2009/02/06) end
      *電話番号
           03  SC11-TEL               PIC  X(15).
      *診療年月
           03  SC11-SRYYM             PIC  X(16).
      *受給者番号
           03  SC11-JKYSNUM1          PIC  X(12).
           03  SC11-JKYSNUM2          PIC  X(14).
      *記号・番号
           03  SC11-KIGO              PIC  X(08).
           03  SC11-BANGO             PIC  X(16).
      *氏名
           03  SC11-NAME              PIC  X(26).
      *性別
           03  SC11-SEX               PIC  X(02) OCCURS 2.
      *誕生日
           03  SC11-TANJOYMD          PIC  X(22).
      *保険区分
           03  SC11-HKNKBN            PIC  X(02) OCCURS 7.
      *保険者番号
           03  SC11-HKNJANUM          PIC  X(16).
      *本人・家族区分
           03  SC11-HONKZKKBN         PIC  X(01).
      *保険請求点数
           03  SC11-HKNTEN            PIC  X(07).
      *保険請求金額
           03  SC11-HKNSEIKYUGK       PIC  X(07).
      *結核・精神点数
           03  SC11-KESETEN           PIC  X(07).
      *訪問看護金額
           03  SC11-HOUMONKINGK       PIC  X(07).
      *併用・在宅区分
           03  SC11-TOKUTEI           PIC  X(02).
           03  SC11-TYOKI             PIC  X(02).
           03  SC11-ZAITAKU           PIC  X(02).
      *所得区分
           03  SC11-SHOTOKU           PIC  X(02)  OCCURS 4.
      *負担割合
           03  SC11-FTNRATE           PIC  X(02)  OCCURS 3.
      *患者負担割合金額
           03  SC11-FTNKINGK          PIC  X(07).
      *入外区分
           03  SC11-NYUGAIKBN         PIC  X(02) OCCURS 3.
      *外来診療実日数
           03  SC11-NISSU             PIC  X(02).
      *５００円回数
           03  SC11-KAISU500          PIC  X(01).
      *一部負担金
           03  SC11-JIKOFTN           PIC  X(05).
      *高齢重度者請求点数
           03  SC11-KOREITEN          PIC  X(07).
      *高齢重度者請求金額
           03  SC11-KOREIKINGK        PIC  X(07).
      *長期入院該当・非該当
           03  SC11-GAITOU            PIC  X(02) OCCURS 2.
      *入院日数
           03  SC11-N-NISSU           PIC  X(02).
      *食事療養費
           03  SC11-RYOYOHI           PIC  X(07).
      *食事日数
           03  SC11-SHOKUJINISSU      PIC  X(02).
      *食事標準負担額
           03  SC11-SHOKUJIFTNGK      PIC  X(07). 
