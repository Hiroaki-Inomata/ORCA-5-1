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
      *    請求管理(労災情報)   COPY (CPSKYRSI.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.07.02    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分検索用項目の追加
      ************************************************************
       01  SKYRSI-REC.
      *ＫＥＹ
           03  SKYRSI-KEY.
      *医療機関番号
               05  SKYRSI-HOSPNUM          PIC 9(2).
      *診療年月
               05  SKYRSI-SRYYM            PIC 9(06).
      *入外区分
               05  SKYRSI-NYUGAIKBN        PIC X(01).
      *患者ＩＤ
               05  SKYRSI-PTID             PIC 9(10).
      *レセプト科（空白の時ゼロをセット）
               05  SKYRSI-RECEKA           PIC X(02).
      *審査支払機関
               05  SKYRSI-TEISYUTUSAKI     PIC 9(01).
      *レセプト種別
               05  SKYRSI-RECESYUBETU      PIC X(04).
      *保険者番号
               05  SKYRSI-HKNJANUM         PIC X(08).
      *補助区分（船員保険のときのみ）
               05  SKYRSI-HOJOKBN-KEY      PIC X(01).
      *公費ＩＤ（公費単独のみ）
               05  SKYRSI-KOHID-KEY        PIC 9(10).
      *適用開始年月日
               05  SKYRSI-TEKSTYMD         PIC X(08).
      *分割区分
               05  SKYRSI-RECEIPT-KBN      PIC X(10).
      *労災情報
           03  SKYRSI-RSIINF.
      *保険区分（１：短期、２：傷病、３：アフターケア、４：公務災害）
               05  SKYRSI-HKNKBN           PIC X(01).
      *様式区分（４：新様式、５：従来様式）
               05  SKYRSI-RECEKBN          PIC X(01).
      *労災初回請求分フラグ（１：初回請求分）
               05  SKYRSI-SYOKAIFLG        PIC X(01).
      *労働基準監督署コード
               05  SKYRSI-SYOCD            PIC X(05).
      *患者交付番号
               05  SKYRSI-KOFUNUM          PIC X(14).
      *災害区分（１：業務、２：通勤）
               05  SKYRSI-SAIGAIKBN        PIC X(01).
      *労災コメント
               05  SKYRSI-COMMENT          PIC X(200).
      *審査支払機関検索用項目
           03  SKYRSI-XXTEISYUTUSAKI       PIC 9(01).
      *保険番号検索用項目
           03  SKYRSI-XXHKNNUM             PIC X(03).
      *保険区分検索用項目
           03  SKYRSI-XXHKNKBN-RSI         PIC X(01).
