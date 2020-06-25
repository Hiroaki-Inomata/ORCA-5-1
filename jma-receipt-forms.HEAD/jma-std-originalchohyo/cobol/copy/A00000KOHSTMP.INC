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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      * 01.00.02     @@@@-門間    19/08/30  月途中で負担者及び受給者番号が変わった場合、
      *                                     分かれて印字されるように修正
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *医療機関ＩＤ
      *         05  MF100-HOSPID           PIC  X(24).
      *医療機関番号
               05  MF100-HOSPNUM          PIC  9(02).
      *公費番号(会社用)
               05  MF100-KOHNUM-KEY       PIC  X(03).
      *患者番号
               05  MF100-PTNUM            PIC  X(20).
      *診療年月
               05  MF100-SRYYM            PIC  9(06).
      *入外区分
               05  MF100-NYUGAIKBN        PIC  X(01).
      *レセプト科（空白の時ゼロをセット）
               05  MF100-SRYKA            PIC  X(02).
      *審査支払機関
               05  MF100-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
               05  MF100-RECESYUBETU      PIC  X(04).
      *保険者番号
               05  MF100-HKNJANUM         PIC  X(08).
      *適用開始年月日
               05  MF100-TEKSTYMD         PIC  X(08).
      *負担者番号
               05  MF100-FTNJANUM         PIC  X(20).
      *受給者番号
               05  MF100-JKYSNUM          PIC  X(20).
      *患者ＩＤ
           03  MF100-PTID                PIC  9(10).
      *漢字氏名
           03  MF100-NAME                PIC  X(100).
      *カナ氏名
           03  MF100-FURIGANA            PIC  X(100).
      *性別
           03  MF100-SEX                 PIC  9(01).
      *主保険情報
           03  MF100-HKN.
      *主保険−記号
               05  MF100-KIGO            PIC  X(80).
      *主保険−番号
               05  MF100-NUM             PIC  X(80).
      *主保険−給付割合
               05  MF100-KYURATE         PIC  9(03).
      *老人保険番号
           03  MF100-RJNHKNNUM           PIC  X(03).
      *公費番号
           03  MF100-KOHNUM              PIC  X(03).
      *公費名
           03  MF100-KOHNAME             PIC  X(20).
      *実日数情報
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *合計点数情報
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *負担金額情報
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *食事療養費-回数情報
           03  MF100-SKJKAISUINF.
               05  MF100-SKJKAISU        PIC  9(02).
      *食事療養費-請求情報
           03  MF100-SKJSKYINF.
               05  MF100-SKJSKY          PIC  9(09).
      *食事療養費-標準負担情報
           03  MF100-SKJFTNINF.
               05  MF100-SKJFTN          PIC  9(09).
      *他法公費(２７含む)
           03  MF100-TAHO-TBL.
               05  MF100-TAHO            PIC  X(03) OCCURS 4.
      *長期
           03  MF100-TYOKI               PIC  9(01).
