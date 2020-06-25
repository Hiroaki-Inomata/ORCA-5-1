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
      * 01.00.00     有村       03/08/27  市町村番号を５→８バイトに変更
      *                                   請求額追加 
      * 01.00.01     有村       05/02/02  SORT1,2,3とSRYDAYの属性を9タイプに変更
      * 01.00.02     吉川       11/07/13  傷病名追加
      *                                   公費負担額追加
      *                                   公費対象点数追加
      * 01.00.03     吉川       11/09/22  備考２追加(診療日数ゼロ理由)
      * 01.00.04     吉川       12/07/05  傷病名バイト数増加
      * 01.00.05     吉川       14/02/04  複数の入退院日印字対応
      * 01.00.06     吉川       15/01/07  平成２７年１月改正対応
      * 01.00.07     吉川       15/12/10  備考４追加（３歳児健診対応）
      * 01.00.08     吉川       16/04/11  備考５追加
      * 01.00.09     門間       18/09/05  不要コメントを削除
      *                                   備考欄の記載修正
      * 01.00.10     萩野       19/06/10  特定疾病区分の項目を追加
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *点数表区分
               05  MF100-TENHYOKBN       PIC  X(01).
      *ソートKEY
               05  MF100-SORT1           PIC  9(02).
               05  MF100-SORT2           PIC  9(01).
      *
      *市町村番号
               05  MF100-CITYNUM         PIC  X(08).
      *公費−負担者番号
               05  MF100-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF100-SRYKA           PIC  X(02).
      *診療年月
               05  MF100-SRYYM           PIC  X(08).
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
      *長期フラグ
           03  MF100-TYOKI              PIC  9(02).
           03  MF100-TYOKI2             PIC  9(02).
      *患者負担月額
           03  MF100-JIKOFTNGK          PIC  9(08).
      *請求額
           03  MF100-SEIKYUGK           PIC  9(09).
      *入院日
           03  MF100-NYUINYMD  OCCURS 5.
               05  MF100-NYUINYY         PIC  9(04).
               05  MF100-NYUINMM         PIC  9(02).
               05  MF100-NYUINDD         PIC  9(02).
      *退院日
           03  MF100-TAIINYMD  OCCURS 5.
               05  MF100-TAIINYY         PIC  9(04).
               05  MF100-TAIINMM         PIC  9(02).
               05  MF100-TAIINDD         PIC  9(02).
      *全国共通公費区分
           03  MF100-KOHKBN-TBL.
               05  MF100-KOHKBN   PIC 9(01) OCCURS 6.
      *外来診療日情報
           03  MF100-SRYDAY-TBL.
               05  MF100-SRYDAY OCCURS 31 PIC 9(01).
      *備考
           03  MF100-BIKO               PIC  X(20).
      *公費負担額
           03  MF100-KOHFTNGK           PIC  9(08).
      *傷病名
           03  MF100-BYOMEI             PIC  X(160).
      *傷病名開始年月日
           03  MF100-BYOYMD             PIC  X(08).
      *公費対象点数
           03  MF100-TAHOTEN            PIC  9(08).
      *備考２
           03  MF100-BIKO2              PIC  X(40).
      *備考３
           03  MF100-BIKO3              PIC  X(208).
      *備考４
           03  MF100-BIKO4              PIC  X(60).
      *備考５
           03  MF100-BIKO5              PIC  X(90).
