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
      ************************************************************
      *          医療費請求書（福祉） COPY (CPRMF04.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF04-REC.
           03  MF04-KEY.
      *医療機関ＩＤ
               05  MF04-HOSPID          PIC  X(24).
      *公費−負担者番号
               05  MF04-FTNJANUM        PIC  X(08).
      *診療科
               05  MF04-SRYKA           PIC  X(02).
      *患者番号
               05  MF04-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF04-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF04-TEKSTYMD        PIC  X(08). 
      *入外区分
               05  MF04-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
           03  MF04-PTID                PIC  9(10).
      *漢字氏名
           03  MF04-NAME                PIC  X(100).
      *生年月日
           03  MF04-BIRTHDAY            PIC  9(08).
      *主保険情報
           03  MF04-HKN.
      *主保険−保険ＩＤ
               05  MF04-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF04-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF04-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF04-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF04-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF04-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF04-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF04-KIGO            PIC  X(80).
      *主保険−番号
               05  MF04-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF04-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF04-CERTNUM         PIC  X(03).
      *老人保健情報
           03  MF04-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF04-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF04-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF04-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF04-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF04-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  MF04-KOHINF.
      *公費−保険ＩＤ
               05  MF04-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF04-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF04-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF04-KOHFTNJANUM     PIC  X(08).
      *公費−受給者番号
               05  MF04-KOHJKYSNUM      PIC  X(20).
      *公費−任意給付区分
               05  MF04-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF04-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF04-RECESKYKBN      PIC  X(01).
      *実日数情報
           03  MF04-JNISSUINF.
      *実日数
               05  MF04-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF04-TOTALTENINF.
      *合計点数
               05  MF04-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF04-FTNMONEYINF.
      *負担金額
               05  MF04-FTNMONEY        PIC  9(09).
      *薬剤一部負担金情報
           03  MF04-YKZFTNINF.
      *薬剤一部負担金
               05  MF04-YKZFTN          PIC  9(05).
