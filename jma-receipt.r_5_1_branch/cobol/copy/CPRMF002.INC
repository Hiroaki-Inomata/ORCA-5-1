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
      *          医療費請求書（乳幼児・福老） COPY (CPRMF02.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF02-REC.
           03  MF02-KEY.
      *医療機関ＩＤ
               05  MF02-HOSPID          PIC  X(24).
      *公費−負担者番号
               05  MF02-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF02-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF02-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF02-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF02-SRYKA           PIC  X(02).
      *入外区分
               05  MF02-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
           03  MF02-PTID                PIC  9(10).
      *漢字氏名
           03  MF02-NAME                PIC  X(100).
      *生年月日
           03  MF02-BIRTHDAY            PIC  9(08).
      *主保険情報
           03  MF02-HKN.
      *主保険−保険ＩＤ
               05  MF02-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF02-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF02-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF02-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF02-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF02-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF02-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF02-KIGO            PIC  X(80).
      *主保険−番号
               05  MF02-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF02-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF02-CERTNUM         PIC  X(03).
      *老人保健情報
           03  MF02-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF02-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF02-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF02-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF02-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF02-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  MF02-KOHINF.
      *公費−保険ＩＤ
               05  MF02-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF02-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF02-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF02-KOHFTNJANUM     PIC  X(08).
      *公費−受給者番号
               05  MF02-KOHJKYSNUM      PIC  X(20).
      *公費−任意給付区分
               05  MF02-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF02-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF02-RECESKYKBN      PIC  X(01).
      *実日数情報
           03  MF02-JNISSUINF.
      *実日数
               05  MF02-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF02-TOTALTENINF.
      *合計点数
               05  MF02-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF02-FTNMONEYINF.
      *負担金額
               05  MF02-FTNMONEY        PIC  9(09).
      *薬剤一部負担金情報
           03  MF02-YKZFTNINF.
      *薬剤一部負担金
               05  MF02-YKZFTN          PIC  9(05).
