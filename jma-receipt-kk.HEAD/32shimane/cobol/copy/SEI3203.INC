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
      *              劉丹     2006/01/07    第１公費情報と総点数と第１公費点数情報と年齢
      *  04.07.01    吉川     2015/07/13    KEYに診療年月追加
      *  04.07.02    井上     2019/01/10    修正履歴コメント削除
      *  05.01.01    門間     2019/06/06    帳票様式の変更
      *  05.01.02    井上     2019/11/14    第１公費負担金額を追加
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
               05  MF100-HOSPNUM         PIC  9(02).
      *点数表区分
               05  MF100-TENHYOKBN       PIC  X(01).
      *公費−負担者番号
               05  MF100-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *診療年月
               05  MF100-SRYYM           PIC  9(06).
      *レセプト種別
           03  MF100-RECESYUBETU         PIC  X(04).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *年齢
           03  MF100-AGE                PIC  9(03).
      *=== KEYに診療年月追加 by Yoshikawa(2015/07/13) start
      **診療年月
      *     03  MF100-SRYYM              PIC  9(06).
      *=== KEYに診療年月追加 by Yoshikawa(2015/07/13) end
           03  MF100-ZAITAKU              PIC  9(01).
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
      *第１公費情報
           03  MF100-KOHINF1.
      *第１公費−保険ＩＤ
               05  MF100-KOHID1           PIC  9(10).
      *第１公費−保険番号
               05  MF100-KOHNUM1          PIC  X(03).
      *第２公費−保険番号
               05  MF100-KOHNUM2          PIC  X(03).
      *第３公費−保険番号
               05  MF100-KOHNUM3          PIC  X(03).
      *第１公費−法別番号
               05  MF100-KOHHBTNUM1       PIC  X(02).
      *第１公費−負担者番号
               05  MF100-KOHFTNJANUM1     PIC  X(20).
      *第１公費−受給者番号
               05  MF100-KOHJKYSNUM1      PIC  X(20).
      *患者負担金額
               05  MF100-KJFTNMONEY       PIC  9(09).
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
      *実日数情報
           03  MF100-JNISSUINF.
      *実日数
               05  MF100-JNISSU          PIC  9(02).
      *総点数情報
           03  MF100-GTOTALTENINF.
      *総点数
               05  MF100-GTOTALTEN       PIC  9(07).
      *第１公費点数情報
           03  MF100-TOTALTENINF1.
      *第１公費点数
               05  MF100-TOTALTEN1       PIC  9(07).
      *公費 10/21 区分
           03  MF100-KOUHIKUBUN          PIC  9(01).
      *第１公費点数情報
           03  MF100-TAHOTEN        PIC  9(07).
      *第１公費点数情報
           03  MF100-TAHOFTN        PIC  9(07).
      *公費区分
           03  MF100-TAHO           PIC  9(01).
      *公費ー負担者番号
           03  MF100-TAHOFTNJANUM   PIC  X(20).
      *公費ー受給者番号
           03  MF100-TAHOJKYJANUM   PIC  X(20).
 
      *合計点数情報
           03  MF100-TOTALTENINF.
      *合計点数
               05  MF100-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF100-FTNMONEYINF.
      *負担金額
               05  MF100-FTNMONEY        PIC  9(09).
      *第１公費負担金額
               05  MF100-FTNMONEY1       PIC  9(09).
      *その他負担金額
           03  MF100-ETCFTNMONEYINF.
               05  MF100-ETCFTNSU        PIC  9(03).
               05  MF100-ETCFTNMONEY     PIC  9(09).
      *食事療養費情報
           03  MF100-SHOKUJIINF.
      *食事日数
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *薬剤一部負担金情報
           03  MF100-YKZFTNINF.
      *薬剤一部負担金
               05  MF100-YKZFTN         PIC  9(05).
      *控除額
           03  MF100-KOUJO              PIC  9(06).
      *結核・精神判定
           03  MF100-KESEINF.
               05  MF100-KESEFLG        PIC  9(01).
               05  MF100-KESENUM        PIC  9(02).
               05  MF100-KESETENSU      PIC  9(07).
      *長期療養
           03  MF100-TYOKI              PIC  9(01).
      *給付額
           03  MF100-KYUFUGAKU          PIC  9(06).
      *老人一部負担金
           03  MF100-RJNFTN             PIC  9(06).
      *特記事項
           03  MF100-TOKKI              PIC  X(02).
