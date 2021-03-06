      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
               05  MF100-HOSPNUM         PIC  9(02).
      *診療年月
               05  MF100-SRYYM           PIC  9(06).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  MF100-PTID            PIC  9(10).
      *レセプト科
               05  MF100-RECEKA          PIC  X(02).
      *審査支払機関
               05  MF100-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  MF100-RECESYUBETU     PIC  X(04).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  MF100-HOJOKBN-KEY     PIC  X(01).
      *公費ＩＤ（公費単独のみ）
               05  MF100-KOHID-KEY       PIC  9(10).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *分割区分
               05  MF100-RECEIPT-KBN     PIC  X(10).
      *患者番号
           03  MF100-PTNUM               PIC  X(20).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *カナ氏名
           03  MF100-KANANAME            PIC  X(100).
      *年齢
           03  MF100-AGE                 PIC  9(03).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *性別
           03  MF100-SEX                 PIC  X(01).
      *主保険情報
           03  MF100-HKN.
      *主保険−保険ＩＤ
               05  MF100-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF100-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF100-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF100-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF100-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF100-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF100-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF100-KIGO            PIC  X(80).
      *主保険−番号
               05  MF100-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF100-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF100-CERTNUM         PIC  X(03).
      *公費情報
           03  MF100-KOHINF.
      *公費−保険ＩＤ
               05  MF100-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF100-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF100-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF100-KOHFTNJANUM     PIC  X(20).
      *公費−受給者番号
               05  MF100-KOHJKYSNUM      PIC  X(20).
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *食事療養費情報
           03  MF100-SHOKUJIINF.
      *食事日数
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF100-SHOKUJIFTN      PIC  9(09).
