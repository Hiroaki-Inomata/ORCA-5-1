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
      *          医療費請求書 COPY (CPRMF100.INC) *
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         12/08/17  記載順をINIファイルで設定
      *                                     KEY内容変更
      *  01.00.02    井上         18/12/12  修正履歴コメント削除
      *  01.00.02    門間         20/03/11  岐阜県の記載仕様に対応
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
               05  MF100-HOSPNUM         PIC  9(02).
      *審査支払機関
               05  MF100-TEISYUTUSAKI    PIC  9(01).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *記載順（INIファイルで患者番号かカナ氏名か）
               05  MF100-SORT            PIC  X(100).
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *レセプト種別
               05  MF100-RECESYUBETU     PIC  X(04).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *患者番号
           03  MF100-PTNUM               PIC  X(20).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *性別
           03  MF100-SEX                 PIC  X(01).
      *年齢
           03  MF100-AGE                 PIC  9(03).
      *県番号
           03  MF100-PREFNUM             PIC  X(02).
      *公費−負担者番号
           03  MF100-FTNJANUM            PIC  X(08).
      *公費受給者番号
           03  MF100-JKYSNUM             PIC  X(08).
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
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *薬剤一部負担金情報
           03  MF100-YKZFTNINF.
               05  MF100-YKZFTN          PIC  9(05).
      *主保険負担金
           03  MF100-SYUCOMPFTN          PIC  9(08).
      *全国公費：実日数
           03  MF100-TAHOJNISSU          PIC  9(02).
      *全国公費：公費点数
           03  MF100-KOHTENSU            PIC  9(08).
      *全国公費：負担金額
           03  MF100-KOHFTNGK            PIC  9(08).
      *全国公費：薬剤負担
           03  MF100-TAHOYKZFTN          PIC  9(06).
      *全国公費：公費番号
           03  MF100-TAHONUM             PIC  X(03).
      *全国公費：負担者番号
           03  MF100-TAHOFTNJANUM        PIC  X(08).
      *全国公費：受給者番号
           03  MF100-TAHOJKYSNUM         PIC  X(08).
      *全国公費：公費ID
           03  MF100-TAHOKOHID           PIC  9(10).
      *長期高額併用フラグ
           03  MF100-TYOKI               PIC  9(01).
      *低所得１フラグ
           03  MF100-TEISHO1             PIC  X(01).
      *低所得２フラグ
           03  MF100-TEISHO2             PIC  X(01).
      *保険情報
           03  MF100-HKNINF.
               05  MF100-H-JNISSU        PIC  9(03).
               05  MF100-H-TOTALTEN      PIC  9(08).
               05  MF100-H-FTNMONEY      PIC  9(09).
               05  MF100-H-YKZFTN        PIC  9(05).
      *保険食事情報
           03  MF100-H-SHOKUJIINF.
               05  MF100-H-SHOKUJINISSU    PIC  9(02).
               05  MF100-H-SHOKUJIRYOYOHI  PIC  9(09).
               05  MF100-H-SHOKUJIFTN      PIC  9(09).
      *第１公費食事情報
           03  MF100-K-SHOKUJIINF.
               05  MF100-K-SHOKUJINISSU    PIC  9(02).
               05  MF100-K-SHOKUJIRYOYOHI  PIC  9(09).
               05  MF100-K-SHOKUJIFTN      PIC  9(09).
      *在宅再掲
           03  MF100-ZAITAKU-SAIKEI        PIC  9(09).
