      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 総括表患者別
      *  管理者            : 吉川
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    吉川    2011/11/07  新規作成
      *   01.00.01    吉川    2014/05/22  実日数の桁数増加
      *   01.00.02    門間    2019/03/14  実人数及び延べ人数のオーバーフロー対応
      *******************************************************************
       01  TA01.
      *    サブタイトル
           03  TA01-SUBTITLE            PIC X(20).
      *    サブタイトル２
           03  TA01-SUBTITLE2           PIC X(30).
      *    診療年月
           03  TA01-SRYYM               PIC X(16).
      *    作成年月日
           03  TA01-CREATEYMD           PIC X(22).
      *    ページ数
           03  TA01-PAGE                PIC X(02).
      *    合計ページ数
           03  TA01-GPAGE               PIC X(02).
      *    入院（外来）
           03  TA01-NYUGAI              PIC X(06).
      *    明細テーブル
           03  TA01-MEISAI-TBL          OCCURS  34 TIMES.
      *    保険種別１(社保、国保)
               05  TA01-HKNSBT1         PIC X(04).
      *    保険種別２(単独、併用)
               05  TA01-HKNSBT2         PIC X(04).
      *    本人家族区分
               05  TA01-HONKZK          PIC X(04).
      *    患者番号(1桁〜10桁)
               05  TA01-PTNUM           PIC X(10).
      *    患者番号(11桁〜20桁)
               05  TA01-PTNUM2          PIC X(20).
      *    氏名
               05  TA01-NAME            PIC X(20).
      *    保険者番号
               05  TA01-HKNJANUM        PIC X(08).
      *    実日数
               05  TA01-NISSU           PIC X(03).
      *    点数
               05  TA01-TENSU           PIC X(09).
      *    一部負担
               05  TA01-ITBFTN          PIC X(09).
      *    食事日数
               05  TA01-SKJ-NISSU       PIC X(03).
      *    食事金額
               05  TA01-SKJ-RYOYOHI     PIC X(09).
      *    食事負担
               05  TA01-SKJ-FTN         PIC X(09).
      *    室料差額
               05  TA01-RMSAGAKU        PIC X(09).
      *    振込金額
               05  TA01-FURIKOMI        PIC X(09).
      *    公費−法別
               05  TA01-KOHNUM          PIC X(03).
      *    負担者番号
               05  TA01-FTNJANUM        PIC X(08).
      *    受給者番号
               05  TA01-JKSNUM          PIC X(08).
      *    公費−実日数
               05  TA01-K-NISSU         PIC X(03).
      *    公費−点数
               05  TA01-K-TENSU         PIC X(09).
      *    公費−一部負担
               05  TA01-K-ITBFTN        PIC X(09).
      *    公費−食事金額
               05  TA01-K-SKJ-RYOYOHI   PIC X(09).
      *    公費−食事負担
               05  TA01-K-SKJ-FTN       PIC X(09).
      *    診療月(返戻月)
               05  TA01-HENREIYM        PIC X(07).
      *    合計テーブル
           03  TA01-GOKEI-TBL.
      *    合計−実人数
               05  TA01-JNINZU          PIC X(04).
      *    合計−延べ人数
               05  TA01-NNINZU          PIC X(04).
      *    合計−実日数
               05  TA01-GNISSU          PIC X(04).
      *    合計−点数
               05  TA01-GTENSU          PIC X(10).
      *    合計−一部負担
               05  TA01-GITBFTN         PIC X(10).
      *    合計−食事日数
               05  TA01-GSKJ-NISSU      PIC X(04).
      *    合計−食事金額
               05  TA01-GSKJ-RYOYOHI    PIC X(10).
      *    合計−食事負担
               05  TA01-GSKJ-FTN        PIC X(10).
      *    合計−室料差額
               05  TA01-GRMSAGAKU       PIC X(10).
      *    合計−振込金額
               05  TA01-GFURIKOMI       PIC X(10).
      *    合計−公費−実日数
               05  TA01-GK-NISSU        PIC X(04).
      *    合計−公費−点数
               05  TA01-GK-TENSU        PIC X(10).
      *    合計−公費−一部負担
               05  TA01-GK-ITBFTN       PIC X(10).
      *    合計−公費−食事金額
               05  TA01-GK-SKJ-RYOYOHI  PIC X(10).
      *    合計−公費−食事負担
               05  TA01-GK-SKJ-FTN      PIC X(10).
      *    合計−実人数２
               05  TA01-JNINZU2         PIC X(05).
      *    合計−延べ人数２
               05  TA01-NNINZU2         PIC X(05).
