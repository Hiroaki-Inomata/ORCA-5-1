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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *公費−負担者番号
               05  MF100-FTNJANUM        PIC  X(08).
      *市町村番号
               05  MF100-CITYNUM         PIC  X(03).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *主保険−記号
               05  MF100-KIGO-KEY        PIC  X(80).
      *主保険−番号
               05  MF100-NUM-KEY         PIC  X(80).
      *受給者番号
               05  MF100-JKYSNUM         PIC  X(20).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *レセプト種別
           03  MF100-RECESYUBETU         PIC  X(04).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
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
      *老人保健情報
           03  MF100-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF100-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF100-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF100-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF100-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF100-RJNJKYSNUM      PIC  X(07).
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
      *公費−任意給付区分
               05  MF100-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF100-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF100-RECESKYKBN      PIC  X(01).
      *公費−組合せ区分
               05  MF100-COMBIKBN        PIC  9(01).
      *公費−組合せ件数
               05  MF100-COMBIKENSU      PIC  9(03).
      *公費−組合せ負担金
               05  MF100-COMBIFTN        PIC  9(09).
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *負担金額情報
           03  MF100-KOHFTNINF.
               05  MF100-KOHFTN          PIC  9(09).
      *長期併用フラグ
           03  MF100-TYOKI               PIC  9(01).
      *経過措置フラグ
           03  MF100-KEIKA               PIC  9(01).
      *高半フラグ
           03  MF100-KOUHAN              PIC  9(01).
      
      *他法公費情報
           03  MF100-TAHO-TBL.
      *他法公費番号
               05  MF100-T-KOHNUM        PIC  X(03).
      *他法公費ID
               05  MF100-T-KOHID         PIC  9(10).
      *他法負担者番号
               05  MF100-T-FTNJANUM      PIC  X(20).
      *他法受給者番号
               05  MF100-T-JKYSNUM       PIC  X(20).
      *他法点数
               05  MF100-T-TOTALTEN      PIC  9(09).
      *他法一部負担金
               05  MF100-T-FTNMONEY      PIC  9(09).
      *その他一部負担金(長期・在宅等)
           03  MF100-ETC-FTNMONEY        PIC  9(09).
      *所得フラグ
           03  MF100-SYOTOKU             PIC  X(01).
      *経過措置チェック用一部負担金
           03  MF100-KEIKAITBFTN         PIC  9(09).
      *訪問看護
           03  MF100-HOUMON              PIC  9(02).
