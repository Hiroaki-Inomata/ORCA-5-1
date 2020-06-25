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
      *    地方公費請求書     COPY (CPKOHSKY.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-藤原    09/03/12  フラグ、データエリア追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      ************************************************************
       01  KOHSKY-REC.
           03  KOHSKY-KEY.
      *医療機関ＩＤ
               05  KOHSKY-HOSPNUM          PIC 9(2).
      *診療年月
               05  KOHSKY-SRYYM           PIC  9(06).
      *入外区分
               05  KOHSKY-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  KOHSKY-PTID            PIC  9(10).
      *レセプト科
               05  KOHSKY-RECEKA          PIC  X(02).
      *審査支払機関
               05  KOHSKY-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  KOHSKY-RECESYUBETU     PIC  X(04).
      *保険者番号        
               05  KOHSKY-HKNJANUM-KEY    PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  KOHSKY-HOJOKBN-KEY     PIC  X(01).
      *公費ＩＤ（公費単独のみ）
               05  KOHSKY-KOHID-KEY       PIC  9(10).
      *適用開始年月日
               05  KOHSKY-TEKSTYMD        PIC  X(08). 
      *分割区分
               05  KOHSKY-RECEIPT-KBN     PIC  X(10).
      *患者番号
           03  KOHSKY-PTNUM               PIC  X(20).
      *カナ氏名
           03  KOHSKY-KANANAME            PIC  X(50).
      *漢字氏名
           03  KOHSKY-NAME                PIC  X(100).
      *性別
           03  KOHSKY-SEX                 PIC  X(01).
      *生年月日
           03  KOHSKY-BIRTHDAY            PIC  9(08).
      *年齢
           03  KOHSKY-AGE                 PIC  9(03).
      *死亡区分
           03  KOHSKY-DEATHKBN            PIC  X(01).
      *診療科情報
           03  KOHSKY-SRYKAINF            OCCURS  20.
      *診療科
               05  KOHSKY-SRYKA           PIC  X(02).
      *主保険情報
           03  KOHSKY-HKN.
      *主保険−保険ＩＤ
               05  KOHSKY-HKNID           PIC  9(10).
      *主保険−保険番号
               05  KOHSKY-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  KOHSKY-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  KOHSKY-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  KOHSKY-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  KOHSKY-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  KOHSKY-CONTKBN         PIC  X(01).
      *主保険−記号
               05  KOHSKY-KIGO            PIC  X(80).
      *主保険−番号
               05  KOHSKY-NUM             PIC  X(80).
      *主保険−給付割合
               05  KOHSKY-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  KOHSKY-CERTNUM         PIC  X(03).
      *老人保健情報
           03  KOHSKY-RJNHKN.
      *老人保険−保険ＩＤ
               05  KOHSKY-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  KOHSKY-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  KOHSKY-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  KOHSKY-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  KOHSKY-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  KOHSKY-KOHINF              OCCURS  4.
      *公費−保険ＩＤ
               05  KOHSKY-KOHID           PIC  9(10).
      *公費−保険番号
               05  KOHSKY-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  KOHSKY-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  KOHSKY-KOHFTNJANUM     PIC  X(20).
      *公費−受給者番号
               05  KOHSKY-KOHJKYSNUM      PIC  X(20).
      *保険組合せ番号情報
           03  KOHSKY-HKNCOMBIINF         OCCURS  10.
      *保険組合せ番号
               05  KOHSKY-HKNCOMBI        PIC  9(04).
      *主保険番号
               05  KOHSKY-HKNNUMHC        PIC  X(03).
      *組合せ公費保険番号情報
               05  KOHSKY-KOHNUMHCINF     OCCURS  4.
      *組合せ公費保険番号
                   07  KOHSKY-KOHNUMHC    PIC  X(03).
      *診療区分別点数情報
           03  KOHSKY-SRYKBNINF           OCCURS  50.
      *基本点数
               05  KOHSKY-KHNTEN          PIC  9(08).
      *回数
               05  KOHSKY-KAISU           PIC  9(05).
      *明細点数情報
               05  KOHSKY-TENSUINF        OCCURS  5.
      *明細点数
                   07  KOHSKY-TENSU       PIC S9(08).
      *実日数情報
           03  KOHSKY-JNISSUINF           OCCURS  5.
      *実日数
               05  KOHSKY-JNISSU          PIC  9(02).
      *合計点数情報
           03  KOHSKY-TOTALTENINF         OCCURS  5.
      *合計点数
               05  KOHSKY-TOTALTEN        PIC S9(08).
      *負担金額情報
           03  KOHSKY-FTNMONEYINF         OCCURS  5.
      *負担金額
               05  KOHSKY-FTNMONEY        PIC S9(09).
      *薬剤一部負担金情報
           03  KOHSKY-YKZFTNINF           OCCURS  5.
      *薬剤一部負担金
               05  KOHSKY-YKZFTN          PIC  9(05).
      *食事療養費情報
           03  KOHSKY-SHOKUJIINF          OCCURS  5.
      *食事日数
               05  KOHSKY-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  KOHSKY-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  KOHSKY-SHOKUJIFTN      PIC  9(09).
      *一部負担金・食事療養費標準負担額区分
           03  KOHSKY-SHOKUJIFTNKBN       PIC  X(01).
      *初診時間区分
           03  KOHSKY-SHOSHINTIME         PIC  X(01).
      *在宅総合・在宅末期フラグ
           03  KOHSKY-ZAITAKU             PIC  X(01).
      *在宅総合・在宅末期一部負担金再掲
           03  KOHSKY-ZAITAKU-SAIKEI      PIC S9(09).
      *請求年月
           03  KOHSKY-SKYYM               PIC  X(06).
      *返戻年月
           03  KOHSKY-HENREIYM            PIC  X(06).
      *請求区分
           03  KOHSKY-SKYKBN              PIC  X(01).
      *訂正区分
           03  KOHSKY-TEISEIKBN           PIC  X(01).
      *
      *フラグ
           03  KOHSKY-FLGINF.
               05  KOHSKY-FLG             PIC  9(01)   OCCURS  5.
      *登録年月日
           03  KOHSKY-CREYMD              PIC  X(08).
      *更新年月日
           03  KOHSKY-UPDYMD              PIC  X(08).
      *公費−保険番号検索用項目
           03  KOHSKY-XXKOHNUM-G.  
               05  KOHSKY-XXKOHNUM        PIC X(03)   OCCURS  4.  
