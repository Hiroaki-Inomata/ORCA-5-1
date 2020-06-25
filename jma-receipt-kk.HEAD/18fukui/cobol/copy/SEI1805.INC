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
      *          医療費請求書 COPY (SEI1805.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2007/10/05   日レセver4.0対応
      *  01.00.02    吉川      2007/10/24   キー変更
      *  01.00.03    吉川      2008/02/26   キー変更
      *  01.00.04    吉川      2008/05/16   H20年4月改正対応
      *  01.00.05    吉川      2011/12/16   同月内で受給者番号が変わっても
      *                                     帳票に反映されないのを修正
      *  01.00.06    萩野      2012/10/25   明細書ファイルを新規作成のため項目追加
      *  01.00.07    吉川      2015/11/11   請求食事負担追加
      *  01.00.08    萩野      2018/11/07   修正履歴コメント削除
      ************************************************************
      *01  MF110-REC.
           03  MF110-KEY.
      *医療機関ＩＤ
               05  MF110-HOSPNUM         PIC  9(02).
      *点数表区分
               05  MF110-TENHYOKBN       PIC  X(01).
      *市町村コード
               05  MF110-CITYCD          PIC  X(02).
      *市町村名称
               05  MF110-CITYNAME        PIC  X(10).
      *入外区分
               05  MF110-NYUGAIKBN       PIC  X(01).
      *請求年月
               05  MF110-SKYYM           PIC  X(06).
      *公費−受給者番号
               05  MF110-JKYSNUM         PIC  X(20).
      *公費−保険番号
               05  MF110-KOHHKNNUM       PIC  X(03).
      *公費−負担者番号
               05  MF110-FTNJANUM        PIC  X(08).
      *患者番号
               05  MF110-PTNUM           PIC  X(20).
      *診療年月
               05  MF110-SRYYM           PIC  X(06).
      *保険者番号        
               05  MF110-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF110-TEKSTYMD        PIC  X(08). 
      *診療科
               05  MF110-SRYKA           PIC  X(02).
      *患者ＩＤ
               05  MF110-PTID            PIC  9(10).
      *長期区分
               05  MF110-CHOKIKBN        PIC  9(01).
      *漢字氏名
           03  MF110-NAME                PIC  X(100).
      *カナ氏名
           03  MF110-KANANAME            PIC  X(50).
      *性別
           03  MF110-SEX                 PIC  X(01).
      *被保区分
           03  MF110-HONHUYOKBN          PIC  X(01).
      *生年月日
           03  MF110-BIRTHDAY            PIC  9(08).
      *レセプト本人家族区分
           03  MF110-RECEHONKZKKBN       PIC  X(01).
      *主保険情報
           03  MF110-HKN.
      *主保険−保険ＩＤ
               05  MF110-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF110-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF110-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF110-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF110-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF110-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF110-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF110-KIGO            PIC  X(80).
      *主保険−番号
               05  MF110-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF110-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF110-CERTNUM         PIC  X(03).
      *老人保健情報
           03  MF110-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF110-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF110-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF110-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF110-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF110-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  MF110-KOHINF.
      *公費−保険ＩＤ
               05  MF110-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF110-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF110-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF110-KOHFTNJANUM     PIC  X(20).
      *公費−受給者番号
               05  MF110-KOHJKYSNUM      PIC  X(20).
      *公費−任意給付区分
               05  MF110-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF110-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF110-RECESKYKBN      PIC  X(01).
      *実日数情報
           03  MF110-JNISSUINF.
      *実日数
               05  MF110-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF110-TOTALTENINF.
      *合計点数
               05  MF110-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF110-FTNMONEYINF.
      *負担金額
               05  MF110-FTNMONEY        PIC  9(09).
      *その他負担金額
           03  MF110-ETCFTNMONEYINF.
               05  MF110-ETCFTNSU        PIC  9(03).
               05  MF110-ETCFTNMONEY     PIC  9(09).
      *食事療養費情報
           03  MF110-SHOKUJIINF.
      *食事回数
               05  MF110-SHOKUJIKAISU    PIC  9(02).
      *食事療養費
               05  MF110-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF110-SHOKUJIFTN      PIC  9(09).
      *薬剤一部負担金情報
           03  MF110-YKZFTNINF           OCCURS  2.
      *薬剤一部負担金
               05  MF110-YKZFTN         PIC  9(05).
      *一部負担金
           03  MF110-ITBFTN		PIC  9(06).
      *公費負担金
           03  MF110-KOHFTN		PIC  9(06).
      *結核・精神点数
           03  MF110-KESE		PIC  9(06).
      *福祉医療一部負担
           03  MF110-FKSFTN             PIC  9(06).
      *国　公費情報
           03  MF110-RECEKOHINF           OCCURS  2.
      *国　公費−保険ＩＤ
               05  MF110-RECEKOHID           PIC  9(10).
      *国　公費−保険番号
               05  MF110-RECEKOHNUM          PIC  X(03).
      *国　公費−法別番号
               05  MF110-RECEKOHHBTNUM       PIC  X(02).
      *国　公費−負担者番号
               05  MF110-RECEKOHFTNJANUM     PIC  X(20).
      *国　公費−受給者番号
               05  MF110-RECEKOHJKYSNUM      PIC  X(20).
      *国　公費−件数
               05  MF110-RECEKOHKENSU        PIC  9(03).
      *国　公費−レセプト請求区分
               05  MF110-RECERECESKYKBN      PIC  X(01).
      *国　実日数情報
           03  MF110-RECEJNISSUINF           OCCURS  2.
      *国　実日数
               05  MF110-RECEJNISSU          PIC  9(02).
      *国　合計点数情報
           03  MF110-RECETOTALTENINF           OCCURS  2.
      *国　合計点数
               05  MF110-RECETOTALTEN        PIC  9(08).
      *国　負担金額情報
           03  MF110-RECEFTNMONEYINF           OCCURS  2.
      *国　負担金額
               05  MF110-RECEFTNMONEY        PIC  9(09).
      *国　食事療養費情報
           03  MF110-RECESHOKUJIINF           OCCURS  2.
      *国　食事回数
               05  MF110-RECESHOKUJIKAISU    PIC  9(02).
      *国　食事療養費
               05  MF110-RECESHOKUJIRYOYOHI  PIC  9(09).
      *国　標準負担額
               05  MF110-RECESHOKUJIFTN      PIC  9(09).
      *フラグ情報
           03  MF110-FLGINF                  OCCURS  5.
      *フラグ
               05  MF110-FLG                 PIC  9(01).
      *食事請求額
           03  MF110-SKY-SHOKUJIFTN          PIC  9(09).
