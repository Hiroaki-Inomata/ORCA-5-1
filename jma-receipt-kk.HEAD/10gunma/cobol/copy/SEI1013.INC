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
      *  01.00.01    吉川         15/01/06  平成２７年１月改正対応
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
               05  MF100-HOSPNUM         PIC  9(02).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *市町村番号
               05  MF100-CITYNUM         PIC  X(02).
      *ソートキー
               05  MF100-SORTKEY.
                   07  MF100-RATE        PIC  9(01).
                   07  MF100-KBN         PIC  9(01).
      *組合せ区分
               05  MF100-COMBIKBN        PIC  X(02).
      *公費−受給者番号
               05  MF100-JKYSNUM         PIC  X(20).
      *主保険−給付割合
               05  MF100-KYURATE-KEY     PIC  9(03).
      *公費−負担者番号
               05  MF100-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *保険者番号
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *レセプト種別
           03  MF100-RECESYUBETU         PIC  X(04).
      *審査支払機関
           03  MF100-TEISYUTUSAKI        PIC  9(01).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *カナ氏名
           03  MF100-KANANAME            PIC  X(50).
      *市町村番号２
           03  MF100-CITYNUM2            PIC  X(06).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *性別
           03  MF100-SEX                 PIC  9(01).
      *年齢
           03  MF100-AGE                 PIC  9(03).
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
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(08).
      *薬剤一部負担金情報
           03  MF100-YKZFTNINF.
               05  MF100-YKZFTN          PIC  9(05).
      *食事療養費情報
           03  MF100-SHOKUJIINF.
      *食事日数
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  FM100-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *公費分薬剤一部負担金
           03  MF100-KOHYKZFTN           PIC  9(05).
      *公費適用開始日
           03  MF100-KOHTEKSTYMD         PIC  X(08).
      *老人一部負担金
           03  MF100-RJNFTN              PIC  9(07).
      *請求金額
           03  MF100-SEIKYUGK            PIC  9(09).
      *割合区分
           03  MF100-WARIAIKBN           PIC  9(01).
      *長期
           03  MF100-TYOKI               PIC  9(01).
      *在宅
           03  MF100-ZAITAKU             PIC  9(01).
      *総点数２
           03  MF100-TOTALTEN2           PIC  9(09).
      *
           03  MF100-SEIKYU-FLG          PIC  9(01).
      *経過措置
           03  MF100-KEIKA               PIC  9(01).
      *低所得
           03  MF100-TEISHO              PIC  9(01).
      *高額療養費４回目以降
           03  MF100-KOGAKU4             PIC  9(01).
      *高半
           03  MF100-KOUHAN              PIC  9(01).
      *特欄コード
           03  MF100-TOKURAN             PIC  X(02).
      *備考コード
           03  MF100-BIKO                PIC  X(02).
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
      *その他の公費情報
           03  MF100-ETCKOHINF OCCURS 2.
               05  MF100-ENISSU          PIC  9(02).
               05  MF100-EJNISSU         PIC  9(02).
               05  MF100-EKOHNUM         PIC  X(03).
               05  MF100-EKOHFTNJANUM    PIC  X(08).
               05  MF100-EKOHJKYSNUM     PIC  X(07).
               05  MF100-ETENSU          PIC  9(08).
               05  MF100-ETOTALTEN       PIC  9(08).
               05  MF100-EITBFTN         PIC  9(08).
               05  MF100-EETCFTN         PIC  9(07).
               05  MF100-EYKZFTN         PIC  9(07).
               05  MF100-E-SHOKUJIINF.
                   07  MF100-E-SHOKUJINISSU    PIC  9(02).
                   07  MF100-E-SHOKUJIRYOYOHI  PIC  9(09).
                   07  MF100-E-SHOKUJIFTN      PIC  9(09).
      *高額療養費
           03  MF100-KOGAKU              PIC  X(01).
      *高額療養現物給付該当
           03  MF100-GENBUTU             PIC  9(01).
