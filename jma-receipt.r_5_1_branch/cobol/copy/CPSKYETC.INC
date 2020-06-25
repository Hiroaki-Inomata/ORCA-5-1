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
      *    請求管理(レセプト記載外公費情報) COPY (CPSKYETC.INC)  *
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
       01  SKYETC-REC.
      *ＫＥＹ
           03  SKYETC-KEY.
      *医療機関ＩＤ
               05  SKYETC-HOSPNUM          PIC 9(2).
      *診療年月
               05  SKYETC-SRYYM            PIC  9(06).
      *入外区分
               05  SKYETC-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
               05  SKYETC-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
               05  SKYETC-RECEKA           PIC  X(02).
      *審査支払機関
               05  SKYETC-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
               05  SKYETC-RECESYUBETU      PIC  X(04).
      *保険者番号
               05  SKYETC-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  SKYETC-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（公費単独のみ）
               05  SKYETC-KOHID-KEY        PIC  9(10).
      *適用開始年月日
               05  SKYETC-TEKSTYMD         PIC  X(08).
      *分割区分
               05  SKYETC-RECEIPT-KBN      PIC X(10).
      *レセプト記載外公費情報
           03  SKYETC-ETC-KOH-G.
      *記載外公費−保険番号
               05  SKYETC-ETC-KOHINF.
                   07  SKYETC-ETC-KOHNUM   PIC  X(03)   OCCURS  4.
      *記載外公費−実日数
               05  SKYETC-ETC-JNISSUINF.
                   07  SKYETC-ETC-JNISSU   PIC  9(02)   OCCURS  4.
      *記載外公費−合計点数
               05  SKYETC-ETC-TOTALTENINF.
                   07  SKYETC-ETC-TOTALTEN PIC S9(08)   OCCURS  4.
      *記載外公費−負担金額
               05  SKYETC-ETC-FTNMONEYINF               OCCURS  4.
                   07  SKYETC-ETC-FTNMONEY     PIC S9(09).
                   07  SKYETC-ETC-FTNMONEY-FTN PIC S9(09).
                   07  SKYETC-ETC-FTNMONEY-KOJ PIC S9(09).
      *記載外公費−薬剤一部負担金情報
               05  SKYETC-ETC-YKZFTNINF.
                   07  SKYETC-ETC-YKZFTN   PIC  9(05)   OCCURS  4.
      *記載外公費−食事療養費情報
               05  SKYETC-ETC-SHOKUJIINF       OCCURS  4.
      *食事日数
                   07  SKYETC-ETC-SHOKUJINISSU PIC  9(02).
      *食事療養費
                   07  SKYETC-ETC-SHOKUJIRYOYOHI
                                               PIC  9(09).
      *標準負担額
                   07  SKYETC-ETC-SHOKUJIFTN   PIC  9(09).
      *審査支払機関検索用項目
           03  SKYETC-XXTEISYUTUSAKI      PIC 9(01).
      *保険番号検索用項目
           03  SKYETC-XXHKNNUM            PIC X(03).
      *保険区分検索用項目
           03  SKYETC-XXHKNKBN-RSI        PIC X(01).
