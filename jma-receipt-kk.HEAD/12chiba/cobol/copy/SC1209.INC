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
      *  01.00.01    吉川         10/12/03  帳票様式の変更に伴う修正
      ********************************************************************
       01  SC09.
      *医療機関コード
           03  SC09-HOSPCD            PIC  X(14).
      *印刷日付
           03  SC09-PRTYY             PIC  X(02).
           03  SC09-PRTMM             PIC  X(02).
      *請求日付
           03  SC09-SEIYY             PIC  X(02).
           03  SC09-SEIMM             PIC  X(02).
           03  SC09-SEIDD             PIC  X(02).
           03  SC09-ADRS              PIC  X(100).
      *医療機関名称
           03  SC09-HOSPNAME          PIC  X(100).
      *医療機関電話番号
           03  SC09-TEL               PIC  X(15).
      *開設者名
           03  SC09-KAISETUNAME       PIC  X(40).
      *市町村名称
           03  SC09-CITYNAME          PIC  X(20).
      *点数表区分
           03  SC09-TENSUKBN-TBL OCCURS 4.
               05  SC09-TENSUKBN      PIC  X(02).
      *特給区分
           03  SC09-TOKKYUKBN         PIC  X(03).
      *
           03  SC09-MEISAI-TBL OCCURS 10.
      *入外区分１
               05  SC09-NYUGAIKBN1     PIC  X(02).
      *入外区分２
               05  SC09-NYUGAIKBN2     PIC  X(04).
      *負担者番号
               05  SC09-FTNJANUM       PIC  X(16).
      *受給者番号
               05  SC09-JKYSNUM        PIC  X(14).
      *保険者番号
               05  SC09-HKNJANUM       PIC  X(08).
      *受給者氏名
               05  SC09-NAME           PIC  X(40).
      *誕生年月
               05  SC09-TANJOGYM       PIC  X(08).
      *性別
               05  SC09-SEX            PIC  X(02).
      *診療日数
               05  SC09-SRYNISSU       PIC  X(03).
      *請求点数
               05  SC09-SEIKYUTEN      PIC  X(11).
      *請求額
               05  SC09-SEIKYUGAKU     PIC  X(10).
      *負担金額
               05  SC09-FTNMONEY       PIC  X(07).
      *食事保険請求額
               05  SC09-SHOKUJIMONEY   PIC  X(11).
      *食事標準負担額
               05  SC09-SHOKUJIFTN     PIC  X(10).
      *入院年月日
               05  SC09-NYUINYMD       PIC  X(09).
      *公費法別番号
               05  SC09-KOHNUM         PIC  X(02).
      *=== 帳票様式の変更 by Yoshikawa(2010/12/03) start
      *特記事項
               05  SC09-TOKKI          PIC  X(02).
      *=== 帳票様式の変更 by Yoshikawa(2010/12/03) end
      *合計
          03  SC09-GOKEI-TBL.
      *合計件数
               05  SC09-G-KENSU        PIC  X(03).
      *合計請求点数
               05  SC09-G-SEIKYUTEN    PIC  X(11).
      *合計食事保険請求額
               05  SC09-G-SHOKUJIMONEY PIC  X(11).
      *合計請求額
               05  SC09-G-SEIKYUGAKU   PIC  X(10).
      *合計食事標準負担額
               05  SC09-G-SHOKUJIFTN   PIC  X(10).
      *合計負担金額
               05  SC09-G-FTNMONEY     PIC  X(07).

