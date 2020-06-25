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
      *    レセプト種別編集パラメタ      COPY (CPORCSSYUBETU.INC)
      *                                  使用プログラム (ORCSSYUBETU)
      *    設定項目
      *      医療機関ＩＤ
      *      県番号
      *      患者ＩＤ
      *      診療年月
      *      入外区分
      *      年齢
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ）
      *                                     適用開始年月日の追加
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *                                     レセプト種別（紙レセプト）の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      ************************************************************
      *
       01  ORCSSYUBETUAREA.
      *
           03  LNK-SYUBETU-IN.
      *        医療機関ＩＤ
               05  LNK-SYUBETU-HOSPNUM     PIC 9(2).
      *        県番号
               05  LNK-SYUBETU-PREFNUM     PIC 9(02).
      *        診療年月
               05  LNK-SYUBETU-SRYYM       PIC 9(06).
      *        入外区分
               05  LNK-SYUBETU-NYUGAIKBN   PIC X(01).
      *        患者ＩＤ
               05  LNK-SYUBETU-PTID        PIC 9(10).
      *        生年月日
               05  LNK-SYUBETU-BIRTHDAY    PIC X(08).
      *
           03  LNK-SYUBETU-OT.
      *        レセプト情報
               05  LNK-SYUBETU-OCC         OCCURS  100.
      *            審査支払機関
                   07  LNK-SYUBETU-TEISYUTUSAKI
                                       PIC 9(01).
      *            レセプト種別
                   07  LNK-SYUBETU-RECESYUBETU
                                       PIC X(04).
      *            レセプト種別（紙レセプト）
                   07  LNK-SYUBETU-RECESYUBETU-Y
                                       PIC 9(04).
      *            保険者番号
                   07  LNK-SYUBETU-HKNJANUM
                                       PIC X(08).
      *            補助区分
                   07  LNK-SYUBETU-HOJOKBN
                                       PIC X(01).
      *            公費ＩＤ（公費単独のみ）
                   07  LNK-SYUBETU-KOHID
                                       PIC 9(10).
      *            適用開始年月日
                   07  LNK-SYUBETU-TEKSTYMD
                                       PIC X(08).
      *            分割区分
                   07  LNK-SYUBETU-RECEIPT-KBN
                                       PIC X(10).
      *            保険番号（主保険）
                   07  LNK-SYUBETU-HKNNUM
                                       PIC X(03).
      *            保険組合せ番号
                   07  LNK-SYUBETU-HKNCOMBI-AREA.
                       09  LNK-SYUBETU-HKNCOMBI
                                       PIC 9(04)   OCCURS  10.
      *
      *    復帰コード  １：設定項目エラー
      *                ２：診療年月エラー（平成１４年９月以前）
      *                ３：年齢取得エラー
      *                ９：対象レセプトなし
           03  LNK-SYUBETU-RC              PIC S9(9) BINARY.
