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
      *    地方公費請求書     COPY (CPKOHSKY.INC流用)            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      ************************************************************
      * 01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
               05  MF100-HOSPNUM         PIC  9(02).
      *診療年月
               05  MF100-SRYYM           PIC  9(06).
      *審査支払機関
               05  MF100-TEISYUTUSAKI    PIC  9(01).
      *出力順
               05  MF100-DSPSEQ          PIC  9(03).
      *保険者番号        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *カナ氏名(ソート用)
               05  MF100-KANANAME-KEY    PIC  X(50).
      *患者番号(ソート用)
               05  MF100-PTNUM-KEY       PIC  X(20).
      *入外区分
               05  MF100-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  MF100-PTID            PIC  9(10).
      *保険者番号２
               05  MF100-HKNJANUM2       PIC  X(02).
      *レセプト科
               05  MF100-RECEKA          PIC  X(02).
      *レセプト種別
               05  MF100-RECESYUBETU     PIC  X(04).
      *補助区分（船員保険のときのみ）
               05  MF100-HOJOKBN-KEY     PIC  X(01).
      *適用開始年月日
               05  MF100-TEKSTYMD        PIC  X(08). 
      *負担者番号(KEY)
               05  MF100-FTNJANUM-KEY    PIC  X(20).
      *受給者番号
               05  MF100-JKYSNUM-KEY     PIC  X(20).
      *公費ID
               05  MF100-KOHID-KEY       PIC  9(10).
      *患者番号
           03  MF100-PTNUM               PIC  X(20).
      *カナ氏名
           03  MF100-KANANAME            PIC  X(50).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *性別
           03  MF100-SEX                 PIC  X(01).
      *生年月日
           03  MF100-BIRTHDAY            PIC  9(08).
      *年齢
           03  MF100-AGE                 PIC  9(03).
      *死亡区分
           03  MF100-DEATHKBN            PIC  X(01).
      *公費適用開始日
           03  MF100-KOHTEKSTYMD         PIC  X(08).
      *公費適用終了日
           03  MF100-KOHTEKEDYMD         PIC  X(08).
      *上限負担額
           03  MF100-JGNGAK              PIC  9(07).
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
      *保険組合せ番号情報
           03  MF100-HKNCOMBIINF         OCCURS  10.
      *保険組合せ番号
               05  MF100-HKNCOMBI        PIC  9(04).
      *主保険番号
               05  MF100-HKNNUMHC        PIC  X(03).
      *組合せ公費保険番号情報
               05  MF100-KOHNUMHCINF     OCCURS  4.
      *組合せ公費保険番号
                   07  MF100-KOHNUMHC    PIC  X(03).
      *診療区分別点数情報
           03  MF100-SRYKBNINF           OCCURS  45.
      *基本点数
               05  MF100-KHNTEN          PIC  9(08).
      *回数
               05  MF100-KAISU           PIC  9(05).
      *明細点数情報
               05  MF100-TENSUINF        OCCURS  5.
      *明細点数
                   07  MF100-TENSU       PIC  9(08).
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
      *薬剤一部負担金情報
           03  MF100-YKZFTNINF.
      *薬剤一部負担金
               05  MF100-YKZFTN          PIC  9(05).
      *食事療養費情報
           03  MF100-SHOKUJIINF.
      *食事日数
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *標準負担区分
           03  MF100-SHOKUJIFTNKBN       PIC  X(01).
      *初診時間区分
           03  MF100-SHOSHINTIME         PIC  X(01).
      *在宅総合・在宅末期フラグ
           03  MF100-ZAITAKU             PIC  X(01).
      *在宅総合・在宅末期一部負担金再掲
           03  MF100-ZAITAKU-SAIKEI      PIC  9(09).
      *給付外点数
           03  MF100-KYUFUGAITEN         PIC  9(09).
      *総請求額
           03  MF100-SOUSEIKYUGK         PIC  9(09).
      *公費負担額
           03  MF100-KOHFTNGK            PIC  9(09).
      *他法負担額
           03  MF100-TAHOFTNGK           PIC  9(09).
      *差引請求額
           03  MF100-SEIKYUGK            PIC  9(09).
