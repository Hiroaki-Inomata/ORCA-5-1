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
      *  01.00.01    吉川         10/08/19  受給者番号の桁数増加
      *  01.00.02    吉川         14/03/05  医療機関コード、一部負担金の追加
      *  01.00.03    吉川         14/04/11  診療年月の桁数増加。コメント削除
      ********************************************************************
        01  SC07.
      *    市町村コード
           03  SC07-CITYCDUP             PIC X(03).
      *    市町村名
           03  SC07-CITYNAME             PIC X(26).
      *    請求年月日
           03  SC07-SEIYMD               PIC X(22).
      *    医療機関コード
           03  SC07-HOSPCD               PIC X(20).
      *    郵便番号
           03  SC07-POST                 PIC X(10).
      *    所在地
           03  SC07-ADRS                 PIC X(100).
      *    医療機関名称
           03  SC07-HOSPNAME             PIC X(100).
      *    開設者氏名
           03  SC07-KAISETU              PIC X(40).
      *    電話番号
           03  SC07-TEL                  PIC X(15).
      *    ページ
           03  SC07-PAGE                 PIC X(03).
      *    請求テーブル
           03  SC07-SEIKYU-TBL  OCCURS  15.
      *    市町村コード
               05  SC07-CITYCD           PIC X(03).
      *    助成種別
               05  SC07-KOHKBN           PIC X(01).
      *    受給者資格証番号
               05  SC07-JKYSNUM          PIC X(20).
      *    氏名
               05  SC07-NAME             PIC X(26).
      *    性別
               05  SC07-SEX              PIC X(01).
      *    生年月日
               05  SC07-BIRTHDAY         PIC X(07).
      *    診療年月
               05  SC07-SRYYM            PIC X(05).
      *    負担割合
               05  SC07-RATE             PIC X(02).
      *    診療科コード
               05  SC07-SRYKA            PIC X(02).
      *    入院
               05  SC07-NYUGAIKBN        PIC X(01).
      *    入院日数
               05  SC07-NISSU            PIC X(04).
      *    保険請求点数
               05  SC07-TENSU            PIC X(07).
      *    一部負担額
               05  SC07-ITBFTN           PIC X(09).
      *    公費・長区分
               05  SC07-TYOKI            PIC X(02).
      *    公費請求点数
               05  SC07-ETENSU           PIC X(07).
      *    公費・長一部負担額
               05  SC07-EFTNMONEY        PIC X(06).
      *    食事−保険請求分
               05  SC07-SHOKUJIRYOYOHI   PIC X(09).
      *    食事−標準負担分.
               05  SC07-SHOKUJIFTN       PIC X(09).
      *    食事−公費請求分
               05  SC07-ESHOKUJIRYOYOHI  PIC X(09).
      *    食事−公費標準負担分
               05  SC07-ESHOKUJIFTN      PIC X(09).
      *    処方せん発行区分
               05  SC07-SYOHO            PIC X(01).
      *    備考
               05  SC07-BIKO             PIC X(20).
