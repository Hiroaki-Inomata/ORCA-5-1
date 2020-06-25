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
       01  SC21.
      *申請者記入エリア(帳票上部)
           03  SC21-SINSEI-AREA.
      *請求日付
               05  SC21-SEIYMD1                    PIC  X(22).
      *申請者住所
               05  SC21-SINSEIADRS                 PIC  X(80).
      *申請者氏名
               05  SC21-SINSEINAME                 PIC  X(40).
      *申請者電話番号
               05  SC21-SINSEITEL                  PIC  X(15).
      *受給者番号
               05  SC21-JKYSNUM                    PIC  X(14).
      *カナ氏名
               05  SC21-KANANAME                   PIC  X(30).
      *氏名
               05  SC21-NAME                       PIC  X(20).
      *生年月日
               05  SC21-TANJOYY                    PIC  X(02).
               05  SC21-TANJOMM                    PIC  X(02).
               05  SC21-TANJODD                    PIC  X(02).
      *保険者番号
               05  SC21-HKNJANUM                   PIC  X(16).
      *保険者名
               05  SC21-HKNJANAME                  PIC  X(20).
      *被保険者名
               05  SC21-HIHKNJANAME                PIC  X(20).
      *課税・非課税
               05  SC21-KAZEI1                     PIC  X(02).
               05  SC21-KAZEI2                     PIC  X(02).
      *医療機関記入エリア(帳票中央部)
           03  SC21-HOSP-AREA.
      *診療科名
               05  SC21-SRYKANAME                  PIC  X(20).
      *保険診療一部負担金(請求金額)
               05  SC21-SEIKYUGK                   PIC  X(09).
      *診療年月
               05  SC21-SRYYY                      PIC  X(04).
               05  SC21-SRYMM                      PIC  X(04).
      *入外区分
               05  SC21-NYUGAIKBN1                 PIC  X(02).
               05  SC21-NYUGAIKBN2                 PIC  X(02).
      *実日数
               05  SC21-NISSU                      PIC  X(02).
      *医療機関コード
               05  SC21-HOSPCD                     PIC  X(14).
      *総点数
               05  SC21-TENSU                      PIC  X(12).
      *他法分点数
               05  SC21-TAHOU                      PIC  X(12).
      *標準負担額
               05  SC21-FTNGK                      PIC  X(06).
      *食事日数
               05  SC21-SNISSU                     PIC  X(02).
      *請求日付
               05  SC21-SEIYMD2                    PIC  X(22).
      *申請者名２
               05  SC21-SINSEINAME2                PIC  X(30).
      *医療機関住所
               05  SC21-HOSPADRS                   PIC  X(100).
      *医療機関名称
               05  SC21-HOSPNAME                   PIC  X(100).
      *開設者名
               05  SC21-KAISETU                    PIC  X(40).
      *点(円)
           03  SC21-MOJI                           PIC  X(02).
