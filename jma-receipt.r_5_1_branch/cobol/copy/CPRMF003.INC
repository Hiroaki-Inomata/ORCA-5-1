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
      *    国保・総括表集計用ファイル  COPY (CPRMF003.INC)       *
      *          KEY 以外はCPRCF002 と同じ
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF03-REC.
           03  MF03-KEY.
      *保険者番号        
               05  MF03-HKNJANUM-KEY    PIC  X(08).
      *医療機関ＩＤ
               05  MF03-HOSPID          PIC  X(24).
      *診療年月
               05  MF03-SRYYM           PIC  9(06).
      *入外区分
               05  MF03-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  MF03-PTID            PIC  9(10).
      *レセプト科
               05  MF03-RECEKA          PIC  X(02).
      *審査支払機関
               05  MF03-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  MF03-RECESYUBETU     PIC  9(04).
      *適用開始年月日
               05  MF03-TEKSTYMD        PIC  X(08). 
      *患者番号
           03  MF03-PTNUM               PIC  X(20).
      *診療科情報
           03  MF03-SRYKAINF            OCCURS  10.
      *診療科
               05  MF03-SRYKA           PIC  X(02).
      *カナ氏名
           03  MF03-KANANAME            PIC  X(100).
      *漢字氏名
           03  MF03-NAME                PIC  X(100).
      *性別
           03  MF03-SEX                 PIC  X(01).
      *生年月日
           03  MF03-BIRTHDAY            PIC  9(08).
      *死亡区分
           03  MF03-DEATHKBN            PIC  X(01).
      *主保険情報
           03  MF03-HKN.
      *主保険−保険ＩＤ
               05  MF03-HKNID           PIC  9(10).
      *主保険−保険番号
               05  MF03-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  MF03-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  MF03-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  MF03-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  MF03-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  MF03-CONTKBN         PIC  X(01).
      *主保険−記号
               05  MF03-KIGO            PIC  X(80).
      *主保険−番号
               05  MF03-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF03-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  MF03-CERTNUM         PIC  X(03).
      *老人保健情報
           03  MF03-RJNHKN.
      *老人保険−保険ＩＤ
               05  MF03-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  MF03-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  MF03-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  MF03-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  MF03-RJNJKYSNUM      PIC  X(07).
      *公費情報
           03  MF03-KOHINF              OCCURS  4.
      *公費−保険ＩＤ
               05  MF03-KOHID           PIC  9(10).
      *公費−保険番号
               05  MF03-KOHNUM          PIC  X(03).
      *公費−法別番号
               05  MF03-KOHHBTNUM       PIC  X(02).
      *公費−負担者番号
               05  MF03-KOHFTNJANUM     PIC  X(08).
      *公費−受給者番号
               05  MF03-KOHJKYSNUM      PIC  X(20).
      *公費−任意給付区分
               05  MF03-KOHNINIKYUKBN   PIC  X(01).
      *公費−件数
               05  MF03-KOHKENSU        PIC  9(03).
      *公費−レセプト請求区分
               05  MF03-RECESKYKBN      PIC  X(01).
      *保険組合せ番号情報
           03  MF03-HKNCOMBIINF         OCCURS  3.
      *保険組合せ番号
               05  MF03-HKNCOMBI        PIC  9(04).
      *主保険番号
               05  MF03-HKNNUMHC        PIC  X(03).
      *組合せ公費保険番号情報
               05  MF03-KOHNUMHCINF     OCCURS  4.
      *組合せ公費保険番号
                   07  MF03-KOHNUMHC    PIC  X(03).
      *特記事項情報
           03  MF03-TOKKIINF            OCCURS  5.
      *特記事項
               05  MF03-TOKKI           PIC  X(02).
      *病棟区分情報
           03  MF03-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  MF03-BYOTOKBN        PIC  X(02).
      *実日数情報
           03  MF03-JNISSUINF           OCCURS  5.
      *実日数
               05  MF03-JNISSU          PIC  9(02).
      *診療区分別点数情報
           03  MF03-SRYKBNINF           OCCURS  50.
      *基本点数
               05  MF03-KHNTEN          PIC  9(08).
      *回数
               05  MF03-KAISU           PIC  9(05).
      *明細点数情報
               05  MF03-TENSUINF        OCCURS  5.
      *明細点数
                   07  MF03-TENSU       PIC  9(08).
      *合計点数情報
           03  MF03-TOTALTENINF         OCCURS  5.
      *合計点数
               05  MF03-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF03-FTNMONEYINF         OCCURS  5.
      *負担金額
               05  MF03-FTNMONEY        PIC  9(09).
      *老人減免区分
           03  MF03-RJNGENMENKBN        PIC  X(01).
      *老人外来金額
           03  MF03-RJNGAIFTN           PIC  9(09).
      *老人入院金額
           03  MF03-RJNNYUFTN           PIC  9(09).
      *国保減免区分
           03  MF03-KKHGENMENKBN        PIC  X(01).
      *国保減額割合
           03  MF03-KKHGENGAKURATE      PIC  X(03).
      *国保減額金額
           03  MF03-KKHGENGAKU          PIC  X(03).
      *割引点数単価
           03  MF03-DISCOUNTTENTNK      PIC  X(02).
      *食事療養費情報
           03  MF03-SHOKUJIINF          OCCURS  5.
      *食事日数
               05  MF03-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  MF03-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  MF03-SHOKUJIFTN      PIC  9(09).
      *標準負担区分
           03  MF03-SHOKUJIFTNKBN       PIC  X(01).
      *老人公費負担金額情報
           03  MF03-RJNKOHFTNINF        OCCURS  3.
      *老人外来公費負担額
               05  MF03-RJNGAIKOHFTN    PIC  9(09).
      *老人入院公費負担額
               05  MF03-RJNNYUKOHFTN    PIC  9(09).
      *薬剤一部負担金情報
           03  MF03-YKZFTNINF           OCCURS  5.
      *薬剤一部負担金
               05  MF03-YKZFTN          PIC  9(05).
      *
