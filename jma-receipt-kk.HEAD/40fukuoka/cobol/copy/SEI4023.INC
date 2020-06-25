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
      *          医療費請求書 COPY (SEI4023.INC) *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/08/20  2回目の入退院日追加
      *                                  MF100-KEY変更
      *  01.00.02    吉川    2011/02/08  区分追加(改ページ用)
      *  01.00.03    吉川    2011/05/17  月遅れ分は別帳票とする
      *  01.00.04    門間    2019/01/10  修正履歴コメント削除
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *        05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *診療年月
               05  MF100-SRYYM           PIC  X(06).
      *区分
               05  MF100-KBN             PIC  X(01).
      *市町村番号
               05  MF100-CITYNUM         PIC  X(16).
      *公費受給者番号
               05  MF100-JKYSNUM         PIC  X(07).
      *患者番号
               05  MF100-PTNUM           PIC  X(20).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *診療年月
      *         05  MF100-SRYYM           PIC  X(06).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *入外区分
           03  MF100-NYUGAIKBN           PIC  X(01).
      *診療科
           03  MF100-SRYKA               PIC  X(02).
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
           03  MF100-JNISSUINF  OCCURS 2.
               05  MF100-JNISSU          PIC  9(02).
      *負担金額情報
           03  MF100-FTNMONEYINF  OCCURS 2.
               05  MF100-FTNMONEY        PIC  9(09).
      *入院日
           03  MF100-NYUINYMD  OCCURS 2.
               05  MF100-NYUINYY         PIC  9(04).
               05  MF100-NYUINMM         PIC  9(02).
               05  MF100-NYUINDD         PIC  9(02).
      *退院日
           03  MF100-TAIINYMD  OCCURS 2.
               05  MF100-TAIINYY         PIC  9(04).
               05  MF100-TAIINMM         PIC  9(02).
               05  MF100-TAIINDD         PIC  9(02).
      *請求金額情報
           03  MF100-SEIKYUGKINF  OCCURS 2.
               05  MF100-SEIKYUGK        PIC  9(09).
