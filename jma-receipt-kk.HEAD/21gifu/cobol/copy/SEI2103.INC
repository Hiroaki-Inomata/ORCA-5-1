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
      *  01.00.00    有村      03/09/26   ソート順変更
      *                                   月後れ請求用項目追加
      *  01.00.01    吉川      10/02/02   公費の種類毎に改頁しないように修正
      *  01.00.02    門間      17/04/10   備考欄の記載追加
      *  01.00.03    萩野      18/10/11   修正履歴コメント削除
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *点数表区分
               05  MF100-TENHYOKBN       PIC  X(01).
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *公費番号
               05  MF100-KOHNUM-KEY      PIC  X(03).
      *公費−受給社番号
               05  MF100-JKYSNUM-KEY     PIC  X(20).
      *保険区分
               05  MF100-HKNKBN          PIC  X(01).
      *公費−負担者番号
               05  MF100-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *月後れ請求用診療年月
               05  MF100-SRYYM           PIC  X(06).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *年齢
           03  MF100-AGE                 PIC  9(03).
      *性別
           03  MF100-SEX                 PIC  X(01).
      *在医総フラグ
           03  MF100-ZAITAKU             PIC  X(01).
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
      *実日数
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
      *合計点数
               05  MF100-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF100-FTNMONEYINF.
      *負担金額
               05  MF100-FTNMONEY        PIC  9(09).
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
      *結予・精神公費対象点数
           03  MF100-KOHTENSU		PIC  9(06).
      *公費対象薬剤負担金
           03  MF100-KOHYKZFTN		PIC  9(06).
      *長期フラグ
           03  MF100-TYOKI              PIC  9(01).
      *患者負担額
           03  MF100-ITBFTN             PIC  9(06).
      *特定疾患用患者負担額
           03  MF100-TOKUTEIFTN         PIC  9(06).
      *高額療養費フラグ
           03  MF100-KOGAKU             PIC  9(01).
      *特記事項
           03  MF100-TOKKI              PIC  X(02).
