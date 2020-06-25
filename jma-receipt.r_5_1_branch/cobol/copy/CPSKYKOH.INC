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
      *    請求管理(公費情報)   COPY (CPSKYKOH.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分検索用項目の追加
      ************************************************************
       01  SKYKOH-REC.
      *ＫＥＹ
           03  SKYKOH-KEY.
               05  SKYKOH-KEY1.
      *医療機関ＩＤ
                   07  SKYKOH-HOSPNUM          PIC 9(2).
      *診療年月
                   07  SKYKOH-SRYYM            PIC  9(06).
      *入外区分
                   07  SKYKOH-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
                   07  SKYKOH-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
                   07  SKYKOH-RECEKA           PIC  X(02).
      *審査支払機関
                   07  SKYKOH-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
                   07  SKYKOH-RECESYUBETU      PIC  X(04).
      *保険者番号
                   07  SKYKOH-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
                   07  SKYKOH-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（公費単独のみ）
                   07  SKYKOH-KOHID-KEY        PIC  9(10).
      *適用開始年月日
                   07  SKYKOH-TEKSTYMD         PIC  X(08).
      *分割区分
                   07  SKYKOH-RECEIPT-KBN      PIC  X(10).
      *連番号(1〜4)
               05  SKYKOH-RENNUM           PIC  9(02).
      *公費情報
           03  SKYKOH-KOHINF.
      *公費−保険番号
               05  SKYKOH-KOHNUM           PIC  X(03).
      *公費情報
           03  SKYKOH-KOHIDINF.
      *公費−公費ＩＤ
               05  SKYKOH-KOHID            PIC  9(10).
      *実日数情報
           03  SKYKOH-JNISSUINF.
      *実日数
               05  SKYKOH-JNISSU           PIC  9(02).
      *診療区分別点数情報
           03  SKYKOH-SRYKBNINF           OCCURS  45.
      *明細点数情報
               05  SKYKOH-TENSUINF.
      *明細点数
                   07  SKYKOH-TENSU       PIC  S9(08).
      *合計点数情報
           03  SKYKOH-TOTALTENINF.
      *合計点数
               05  SKYKOH-TOTALTEN        PIC S9(08).
      *負担金額情報
           03  SKYKOH-FTNMONEYINF.
      *負担金額
               05  SKYKOH-FTNMONEY        PIC S9(09).
      *負担金額（一部負担額）
               05  SKYKOH-FTNMONEY-FTN    PIC S9(09).
      *負担金額（控除額）
               05  SKYKOH-FTNMONEY-KOJ    PIC S9(09).
      *
      *食事療養費情報
           03  SKYKOH-SHOKUJIINF.
      *食事日数
               05  SKYKOH-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  SKYKOH-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  SKYKOH-SHOKUJIFTN      PIC  9(09).
      *
      *薬剤一部負担金情報
           03  SKYKOH-YKZFTNINF.
      *薬剤一部負担金
               05  SKYKOH-YKZFTN          PIC  9(05).
      *審査支払機関検索用項目
           03  SKYKOH-XXTEISYUTUSAKI      PIC 9(01).
      *保険番号検索用項目
           03  SKYKOH-XXHKNNUM            PIC X(03).
      *保険区分検索用項目
           03  SKYKOH-XXHKNKBN-RSI        PIC X(01).
