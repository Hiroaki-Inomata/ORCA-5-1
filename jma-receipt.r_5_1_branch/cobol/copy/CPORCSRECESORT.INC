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
      *    レセ器材再編集パラメタ　　COPY (CPORCSRECESORT.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/07/10  器材点数追加
      *
      *  04.05.01    NACL-藤原    09/11/27  レセ電コメント拡張対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      ************************************************************
       01  ORCSRECESORTAREA.
      *
      *入力
           03  ORCSRECESORT-IN-AREA.
      *医療機関ＩＤ
               05  ORCSRECESORT-HOSPNUM         PIC  9(02).
      *入外区分
               05  ORCSRECESORT-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  ORCSRECESORT-PTID            PIC  9(10).
      *診療年月
               05  ORCSRECESORT-SRYYM           PIC  X(06).
      *診療区分
               05  ORCSRECESORT-SRYKBN          PIC  X(02).
      *剤番号
               05  ORCSRECESORT-ZAINUM          PIC  9(08).
      *器材点数
               05  ORCSRECESORT-KIZAITEN        PIC  9(07).
      *剤内容
               05  ORCSRECESORT-IN-TBL.
                   07  ORCSRECESORT-IN-SRYCD-G  OCCURS   50.
      *                関連コメント検索用ＫＥＹ
                       09  ORCSRECESORT-IN-KEY.
                           11  ORCSRECESORT-IN-KEY-HOSPNUM   PIC 9(2).
                           11  ORCSRECESORT-IN-KEY-SRYYM     PIC 9(06).
                           11  ORCSRECESORT-IN-KEY-NYUGAIKBN PIC X(01).
                           11  ORCSRECESORT-IN-KEY-PTID      PIC 9(10).
                           11  ORCSRECESORT-IN-KEY-RECEKA    PIC X(02).
                           11  ORCSRECESORT-IN-KEY-TEISYUTUSAKI
                                                             PIC 9(01).
                           11  ORCSRECESORT-IN-KEY-RECESYUBETU
                                                             PIC 9(04).
                           11  ORCSRECESORT-IN-KEY-HKNJANUM  PIC X(08).
                           11  ORCSRECESORT-IN-KEY-HOJOKBN   PIC X(01).
                           11  ORCSRECESORT-IN-KEY-KOHID     PIC 9(10).
                           11  ORCSRECESORT-IN-KEY-TEKSTYMD  PIC X(08). 
                           11  ORCSRECESORT-IN-KEY-RECEIPT-KBN
                                                             PIC X(10).
                           11  ORCSRECESORT-IN-KEY-ZAINUM    PIC 9(08).
                           11  ORCSRECESORT-IN-KEY-ZAIREN    PIC 9(02).
                           11  ORCSRECESORT-IN-KEY-RENNUM    PIC 9(02).
                       09  ORCSRECESORT-IN-SHORI-RENNUM
                                                    PIC 9(01).
      *                診療コード
                       09  ORCSRECESORT-IN-SRYCD    PIC X(09).
      *                データ区分
                       09  ORCSRECESORT-IN-DATAKBN  PIC X(01).
      *                数量
                       09  ORCSRECESORT-IN-SRYSURYO PIC  9(05)V9(05).
      *                回数
                       09  ORCSRECESORT-IN-SRYKAISU PIC  9(03).
      *                単位コード
                       09  ORCSRECESORT-IN-TANICD   PIC  9(03).
      *                点数識別区分
                       09  ORCSRECESORT-IN-TENSIKIBETU
                                                    PIC  9(01).
      *                点数
                       09  ORCSRECESORT-IN-TEN      PIC  9(09)V9(02).
      *                漢字名称
                       09  ORCSRECESORT-IN-NAME     PIC  X(200).
      *                特定器材種別１
                       09  ORCSRECESORT-IN-TOKUKIZAISBT1 
                                                    PIC X(01).
      *                酸素等区分
                       09  ORCSRECESORT-IN-SANSOKBN PIC X(01).
      *                レセ電用特定器材商品名
                       09  ORCSRECESORT-IN-RECEDENNAME
                                                    PIC X(300).
      *出力
           03  ORCSRECESORT-OUT-AREA.
      *        器材テーブル（１：フィルム、２：酸素、３：窒素、４：その他器材）
      *                     ５：フィルム２〜８：フィルム５）
               05  ORCSRECESORT-DATE-TBL       OCCURS   8.
      *            点数
                   07  ORCSRECESORT-ZAITEN     PIC  9(08).
      *            ＭＡＸ
                   07  ORCSRECESORT-TBLMAX     PIC  9(02).
      *
                   07  ORCSRECESORT-SRYCD-G         OCCURS   50.
      *                関連コメント検索用ＫＥＹ
                       09  ORCSRECESORT-KEY.
                           11  ORCSRECESORT-KEY-HOSPNUM    PIC 9(2).
                           11  ORCSRECESORT-KEY-SRYYM      PIC 9(06).
                           11  ORCSRECESORT-KEY-NYUGAIKBN  PIC X(01).
                           11  ORCSRECESORT-KEY-PTID       PIC 9(10).
                           11  ORCSRECESORT-KEY-RECEKA     PIC X(02).
                           11  ORCSRECESORT-KEY-TEISYUTUSAKI
                                                       PIC 9(01).
                           11  ORCSRECESORT-KEY-RECESYUBETU
                                                       PIC 9(04).
                           11  ORCSRECESORT-KEY-HKNJANUM   PIC X(08).
                           11  ORCSRECESORT-KEY-HOJOKBN    PIC X(01).
                           11  ORCSRECESORT-KEY-KOHID      PIC 9(10).
                           11  ORCSRECESORT-KEY-TEKSTYMD   PIC X(08). 
                           11  ORCSRECESORT-KEY-RECEIPT-KBN
                                                           PIC X(10).
                           11  ORCSRECESORT-KEY-ZAINUM     PIC 9(08).
                           11  ORCSRECESORT-KEY-ZAIREN     PIC 9(02).
                           11  ORCSRECESORT-KEY-RENNUM     PIC 9(02).
                       09  ORCSRECESORT-SHORI-RENNUM
                                                    PIC 9(01).
      *                診療コード
                       09  ORCSRECESORT-SRYCD       PIC X(09).
      *                データ区分
                       09  ORCSRECESORT-DATAKBN     PIC X(01).
      *                数量
                       09  ORCSRECESORT-SRYSURYO    PIC  9(05)V9(05).
      *                回数
                       09  ORCSRECESORT-SRYKAISU    PIC  9(03).
      *                単位コード
                       09  ORCSRECESORT-TANICD      PIC  9(03).
      *                点数識別区分
                       09  ORCSRECESORT-TENSIKIBETU PIC  9(01).
      *                点数
                       09  ORCSRECESORT-TEN         PIC  9(09)V9(02).
      *                漢字名称
                       09  ORCSRECESORT-NAME        PIC  X(200).
      *                レセ電用特定器材商品名
                       09  ORCSRECESORT-RECEDENNAME PIC  X(300).
      *エラーコード（１：剤なし）
           03  ORCSRECESORT-ERRCD           PIC 9(01).
      *
