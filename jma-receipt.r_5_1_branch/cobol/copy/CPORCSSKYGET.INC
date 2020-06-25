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
      *******************************************************************
      *    保険者負担等の請求額取り込みサブパラメタ
      *    COPY  (CPORCSSKYGET.INC)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-門脇    13/10/15  自賠責（第三者行為）対応
      *  04.08.01    NACL-門脇    16/09/28  自賠責（第三者行為）
      *                                     保険別請求チェック表の機能追加
      ************************************************************
       01  ORCSSKYGETAREA.
      *
      *入力値
      *  区分
           03  LNK-SKYGET-KBN                           PIC X(01).
      *  診療年月
           03  LNK-SKYGET-SRYYM                         PIC 9(06).
      *  パラメタ：（第三者行為）医療費一部負担額・食事標準負担額の集計先
      *              ０：自賠責（第三者行為）、１：医保
           03  LNK-SKYGET-PARA                          PIC X(01).
      *
      *返却値
           03  LNK-SKYGET-OUT-AREA.
      *
      *  請求額（医療費）情報
               05  LNK-SKYGET-IRYO-INF.
      *    実日数
                   07  LNK-SKYGET-IRYO-NISSU            PIC 9(02).
      *    請求点
                   07  LNK-SKYGET-IRYO-TOTALTEN         PIC 9(08).
      *    患者負担
                   07  LNK-SKYGET-IRYO-ICHIBUFTN        PIC 9(09).
      *    保険者負担
                   07  LNK-SKYGET-IRYO-HOKENJA          PIC 9(09).
      *    公費負担
                   07  LNK-SKYGET-IRYO-KOHFTN           PIC 9(09).
      *
      *  請求額（食事）情報
               05  LNK-SKYGET-SKJ-INF.
      *    回数
                   07  LNK-SKYGET-SKJ-KAISU             PIC 9(02).
      *    請求円
                   07  LNK-SKYGET-SKJ-RYOYOHI           PIC 9(09).
      *    患者負担
                   07  LNK-SKYGET-SKJ-ICHIBUFTN         PIC 9(09).
      *    保険者負担
                   07  LNK-SKYGET-SKJ-HOKENJA           PIC 9(09).
      *    公費負担
                   07  LNK-SKYGET-SKJ-KOHFTN            PIC 9(09).
      *
      *  公費情報（レセプト請求分）
               05  LNK-SKYGET-KOH-INF.
                 06  LNK-SKYGET-KOH-OCC                 OCCURS 4.
                   07  LNK-SKYGET-KOH-HKNNUM            PIC X(03).
                   07  LNK-SKYGET-KOH-NISSU             PIC 9(02).
                   07  LNK-SKYGET-KOH-TOTALTEN          PIC 9(08).
                   07  LNK-SKYGET-KOH-FTNMONEY          PIC 9(09).
                   07  LNK-SKYGET-KOH-KAISU             PIC 9(02).
                   07  LNK-SKYGET-KOH-RYOYOHI           PIC 9(09).
                   07  LNK-SKYGET-KOH-SHOKUJIFTN        PIC 9(09).
      *
      *  公費情報（公費請求書請求分）
               05  LNK-SKYGET-KOHETC-INF.
                 06  LNK-SKYGET-KOHETC-OCC              OCCURS 4.
                   07  LNK-SKYGET-KOHETC-HKNNUM         PIC X(03).
                   07  LNK-SKYGET-KOHETC-NISSU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-TOTALTEN       PIC 9(08).
                   07  LNK-SKYGET-KOHETC-FTNMONEY       PIC 9(09).
                   07  LNK-SKYGET-KOHETC-KAISU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-RYOYOHI        PIC 9(09).
                   07  LNK-SKYGET-KOHETC-SHOKUJIFTN     PIC 9(09).
      *
      *  請求フラグ（レセプトと公費請求書の請求年月が異なる場合）
      *  （０：同じ又は公費請求書なし　１：レセプトのみ　２：公費請求書のみ）
               05  LNK-SKYGET-SEIKYUFLG                 PIC 9(01).
      *
      *返却値２
           03  LNK-SKYGET-OUT2-AREA.
      *      (1)社保の自費分以下
      *      (2)国保の自費分以下
      *      (3)後期高齢の自費分以下
      *      (4)特別療養費
      *      (5)自費保険
      *      (6)治験
      *      (7)労災
      *      (8)自賠責
      *      (9)公害
      *      (10)第三者行為の自費分以下
             04  LNK-SKYGET-OUT2-OCC                    OCCURS 10.
      *        (1)入院の仮収納(2)外来(3)入院の本収納
               05  LNK-SKYGET-OUT2-OCC2                 OCCURS 3.
      *
      *  件数
                 06  LNK-SKYGET-KENSU                   PIC 9(06).
      *
      *  請求額（医療費）情報
                 06  LNK-SKYGET-IRYO2-INF.
      *    実日数
                   07  LNK-SKYGET-IRYO2-NISSU           PIC 9(06).
      *    請求点
                   07  LNK-SKYGET-IRYO2-TOTALTEN        PIC 9(08).
      *    患者負担
                   07  LNK-SKYGET-IRYO2-ICHIBUFTN       PIC 9(09).
      *    保険者負担
                   07  LNK-SKYGET-IRYO2-HOKENJA         PIC 9(09).
      *    公費負担
                   07  LNK-SKYGET-IRYO2-KOHFTN          PIC 9(09).
      *
      *  請求額（食事）情報
                 06  LNK-SKYGET-SKJ2-INF.
      *    回数
                   07  LNK-SKYGET-SKJ2-KAISU            PIC 9(06).
      *    請求円
                   07  LNK-SKYGET-SKJ2-RYOYOHI          PIC 9(09).
      *    患者負担
                   07  LNK-SKYGET-SKJ2-ICHIBUFTN        PIC 9(09).
      *    保険者負担
                   07  LNK-SKYGET-SKJ2-HOKENJA          PIC 9(09).
      *    公費負担
                   07  LNK-SKYGET-SKJ2-KOHFTN           PIC 9(09).
      *
      *  自費分
                 06  LNK-SKYGET-JIHI                    PIC 9(09).
      *  減免金額
                 06  LNK-SKYGET-DISCOUNT-MONEY          PIC S9(09).
      *  調整金計
                 06  LNK-SKYGET-CHOSEI                  PIC S9(09).
      *  調整金１
                 06  LNK-SKYGET-CHOSEI1                 PIC S9(09).
      *  調整金２
                 06  LNK-SKYGET-CHOSEI2                 PIC S9(09).
      *  端数調整金額
                 06  LNK-SKYGET-HASU                    PIC S9(09).
      *
      *復帰コード
           03  LNK-SKYGET-RC                            PIC 9(02).
