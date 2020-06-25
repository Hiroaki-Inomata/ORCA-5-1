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
      *  01.00.01    吉川         10/02/23  特記事項追加
      *  01.00.02    吉川         12/02/10  公費適用開始年月日追加
      *  01.00.03    門間         18/05/02  公費欄−一部負担金金額置換対応
      *                           18/05/10  保険欄−一部負担金記載置換対応
      *  01.00.04    門間         19/02/01  修正履歴コメント削除
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *出力順
               05  MF100-DSPSEQ          PIC  9(03).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *カナ氏名(ソート用)
               05  MF100-KANANAME-KEY    PIC  X(50).
      *患者番号(ソート用)
               05  MF100-PTNUM-KEY       PIC  X(20).
      *提出先
               05  MF100-TEISYUTUSAKI    PIC  X(01).
      *負担者番号
               05  MF100-FTNJANUM        PIC  X(20).
      *公費受給者番号
               05  MF100-JKYSNUM         PIC  X(20).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *保険者番号KEY２
               05  MF100-HKNJANUM-KEY2   PIC  X(08).
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *レセプト種別
               05  MF100-RECESYUBETU     PIC  X(04).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *カナ氏名
           03  MF100-KANANAME            PIC  X(100).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *性別
           03  MF100-SEX                 PIC  X(01).
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
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *一部負担金
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
      *公費−適用開始年月日
           03  MF100-KOHTEKSTYMD         PIC  X(08).
      *総合計
           03  MF100-TOTALTEN-G          PIC  9(09).
           03  MF100-JNISSU-G            PIC  9(09).
           03  MF100-FTNMONEY-G          PIC  9(09).
           03  MF100-SHOKUJIINF-G.
               05  MF100-SHOKUJINISSU-G      PIC  9(02).
               05  MF100-SHOKUJIRYOYOHI-G    PIC  9(09).
               05  MF100-SHOKUJIFTN-G        PIC  9(09).
      *他法公費(月途中の地方公費市町村変更データ含む)
           03  MF100-T-KOHNUM            PIC  X(03).
           03  MF100-T-FTNJANUM          PIC  X(20).
           03  MF100-T-JKYSNUM           PIC  X(20).
           03  MF100-T-JNISSU            PIC  9(02).
           03  MF100-T-TOTALTEN          PIC  9(09).
           03  MF100-T-FTNMONEY          PIC  9(09).
           03  MF100-T-KOHTEKSTYMD       PIC  X(08).
      *
           03  MF100-T-SHOKUJIINF.
               05  MF100-T-SHOKUJINISSU      PIC  9(02).
               05  MF100-T-SHOKUJIRYOYOHI    PIC  9(09).
               05  MF100-T-SHOKUJIFTN        PIC  9(09).
      *特記事項
           03  MF100-TOKKI                   PIC  X(02)  OCCURS 4.
      *「保険負担金：」置換金額
           03  MF101-OKIKAE-HKN          PIC 9(07).
      *「（再掲下）：」置換金額
           03  MF101-OKIKAE-SAIKEI1      PIC 9(07).
      *「（再掲上）：」置換金額
           03  MF101-OKIKAE-SAIKEI2      PIC 9(07).
      *「（再掲３）：」置換金額
           03  MF101-OKIKAE-SAIKEI3      PIC 9(07).
      *「（再掲４）：」置換金額
           03  MF101-OKIKAE-SAIKEI4      PIC 9(07).
      *公費一部負担金置換
           03  MF100-OKIKAE-TBL        OCCURS 4.
      *一部負担金
               05  MF100-OKIKAE-FTN      PIC 9(07).
