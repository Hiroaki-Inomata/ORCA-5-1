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
      ************************************************************
      *          医療費明細書 COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF101-REC.
           03  MF101-KEY.
      *医療機関ＩＤ
               05  MF101-HOSPNUM         PIC  9(02).
      *点数表区分
               05  MF101-TENHYOKBN       PIC  X(01).
      *診療年月
               05  MF101-SRYYM           PIC  X(06).
      *公費−負担者番号
               05  MF101-FTNJANUM        PIC  X(08).
      *公費−受給者番号
               05  MF101-JKYSNUM         PIC  X(20).
      *患者番号
               05  MF101-PTNUM           PIC  X(20).
      *保険者番号
               05  MF101-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF101-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF101-SRYKA           PIC  X(02).
      *入外区分
               05  MF101-NYUGAIKBN       PIC  X(01).
      *審査支払機関
               05  MF101-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  MF101-RECESYUBETU     PIC  X(04).
      *補助区分（船員保険のときのみ）
               05  MF101-HOJOKBN-KEY     PIC  X(01).
      *患者ＩＤ
           03  MF101-PTID                PIC  9(10).
      *カナ氏名
           03  MF101-KANANAME            PIC  X(100).
      *漢字氏名
           03  MF101-NAME                PIC  X(100).
      *年齢
           03  MF101-AGE                 PIC  9(03).
      *生年月日
           03  MF101-BIRTHDAY            PIC  9(08).
      *性別
           03  MF101-SEX                 PIC  9(01).
      *請求年月
           03  MF101-SKYYM               PIC  X(06).
      *主保険情報
           03  MF101-HKN.
      *主保険−保険ＩＤ
               05  MF101-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF101-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF101-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF101-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF101-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF101-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF101-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF101-KIGO            PIC  X(80).
      *主保険−番号
               05  MF101-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF101-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF101-CERTNUM         PIC  X(03).
      *老人保健情報
           03  MF101-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF101-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF101-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF101-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF101-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF101-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  MF101-KOHINF.
      *公費−保険ＩＤ
               05  MF101-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF101-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF101-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF101-KOHFTNJANUM     PIC  X(20).
      *公費−受給者番号
               05  MF101-KOHJKYSNUM      PIC  X(20).
      *公費−任意給付区分
               05  MF101-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF101-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF101-RECESKYKBN      PIC  X(01).
      *公費−組合せ区分
               05  MF101-COMBIKBN        PIC  9(01).
      *公費−組合せ件数
               05  MF101-COMBIKENSU      PIC  9(03).
      *公費−組合せ負担金
               05  MF101-COMBIFTN        PIC  9(09).
      *実日数情報
           03  MF101-JNISSUINF.
               05  MF101-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF101-TOTALTENINF.
               05  MF101-TOTALTEN        PIC  9(08).
      *
           03  MF101-FTNMONEYINF.
               05  MF101-FTNMONEY        PIC  9(09).
           03  MF101-FTNMONEYINF2.
               05  MF101-FTNMONEY2       PIC  9(09).
      *食事療養費情報
           03  MF101-SHOKUJIINF.
      *食事日数
               05  MF101-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  MF101-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF101-SHOKUJIFTN      PIC  9(09).
      *負担金額情報
           03  MF101-KOHFTNINF.
               05  MF101-KOHFTN          PIC  9(09).
      *薬剤一部負担金情報
           03  MF101-YKZFTNINF.
               05  MF101-YKZFTN          PIC  9(05).
