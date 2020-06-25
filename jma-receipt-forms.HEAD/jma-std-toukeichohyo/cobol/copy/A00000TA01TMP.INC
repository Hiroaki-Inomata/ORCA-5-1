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
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.01    吉川    11/11/07   新規作成
      *******************************************************************
       01  TA01TMP-REC.
      *    キー項目
           03  TA01TMP-KEY.
      *    診療年月
               05  TA01TMP-SRYYM             PIC X(06).
      *    県区分
               05  TA01TMP-PREFKBN           PIC X(01).
      *    入外区分
               05  TA01TMP-NYUGAIKBN         PIC X(01).
      *    保険種別
               05  TA01TMP-HKNSBT-KEY.
      *    保険種別１(社保、国保)
                   07  TA01TMP-HKNSBT1       PIC X(01).
      *    保険種別２(単独、併用)
                   07  TA01TMP-HKNSBT2       PIC X(01).
      *    本人家族区分
                   07  TA01TMP-HONKZK        PIC X(01).
      *    患者番号
               05  TA01TMP-PTNUM             PIC X(20).
      *    保険者番号
               05  TA01TMP-HKNJANUM          PIC X(08).
      *    診療月(返戻月)
               05  TA01TMP-HENREIYM          PIC X(06).
      *    データテーブル
           03  TA01TMP-DATA-TBL.
      *    氏名
               05  TA01TMP-NAME              PIC X(20).
      *    実日数
               05  TA01TMP-NISSU             PIC 9(05).
      *    点数
               05  TA01TMP-TENSU             PIC 9(09).
      *    一部負担
               05  TA01TMP-ITBFTN            PIC 9(09).
      *    食事日数
               05  TA01TMP-SKJ-NISSU         PIC 9(05).
      *    食事金額
               05  TA01TMP-SKJ-RYOYOHI       PIC 9(09).
      *    食事負担
               05  TA01TMP-SKJ-FTN           PIC 9(09).
      *    室料差額
               05  TA01TMP-RMSAGAKU          PIC 9(09).
      *    振込金額
               05  TA01TMP-FURIKOMI          PIC 9(09).
      *    診療科
               05  TA01TMP-SRYKA             PIC X(02).
      *    公費テーブル
               05  TA01TMP-KOH-TBL  OCCURS  4.
      *    公費−法別
                   07  TA01TMP-KOHNUM        PIC X(03).
      *    公費−負担者番号
                   07  TA01TMP-FTNJANUM      PIC X(08).
      *    公費−受給者番号
                   07  TA01TMP-JKYSNUM       PIC X(08).
      *    公費−実日数
                   07  TA01TMP-K-NISSU       PIC 9(05).
      *    公費−点数
                   07  TA01TMP-K-TENSU       PIC 9(09).
      *    公費−一部負担
                   07  TA01TMP-K-ITBFTN      PIC 9(09).
      *    公費−食事金額
                   07  TA01TMP-K-SKJ-RYOYOHI PIC 9(09).
      *    公費−食事負担
                   07  TA01TMP-K-SKJ-FTN     PIC 9(09).
