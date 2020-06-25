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
      *    月次統計データ（基本情報）     COPY (CPTCF002.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-藤原    12/11/15  剤点数等のマイナス表記対応
      *  04.05.02    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.06.01    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      ************************************************************
      *01  TOUKEI02-REC.
           03  TOUKEI02-KEY.
               05  TOUKEI02-KEY1.
      *            医療機関ＩＤ
                   07  TOUKEI02-HOSPNUM    PIC 9(2).
      *            診療年月
                   07  TOUKEI02-SRYYM      PIC  9(06).
      *            入外区分
                   07  TOUKEI02-NYUGAIKBN  PIC  X(01).
      *            患者ＩＤ
                   07  TOUKEI02-PTID       PIC  9(10).
      *            入所中区分
                   07  TOUKEI02-RECEIPT-KBN
                                           PIC  X(10).
      *            レセプト診療科
                   07  TOUKEI02-RECEKA     PIC  X(02).
      *            主保険保険番号
                   07  TOUKEI02-HKNNUM     PIC  X(03).
      *            公費ＩＤ
                   07  TOUKEI02-KOHID      PIC  9(10).
      *            保険者番号        
                   07  TOUKEI02-HKNJANUM   PIC  X(08).
      *            本人家族区分
                   07  TOUKEI02-HONKZKKBN  PIC  X(01).
      *            補助区分
                   07  TOUKEI02-HOJOKBN    PIC  X(01).
      *            公費ＩＤ（公費単独時使用）
                   07  TOUKEI02-KOH-KOHID  PIC  9(10).
      *        診療科
               05  TOUKEI02-SRYKA          PIC  X(02).
      *        診療区分
               05  TOUKEI02-SRYKBN         PIC  X(02).
      *        剤番号
               05  TOUKEI02-ZAINUM         PIC  9(08).
      *        連番
               05  TOUKEI02-SEQNO          PIC  9(03).
      *連番号
           03  TOUKEI02-RENNUM             PIC  9(02).
      *保険組合せ
           03  TOUKEI02-HKNCOMBI           PIC  9(04).
      *
      *主保険−保険ＩＤ
           03  TOUKEI02-HKNID              PIC  9(10).
      *公費ＩＤ
           03  TOUKEI02-KOHID-AREA.
               05  TOUKEI02-TKOHID         PIC  9(10)   OCCURS  4.
      *レセ電診療科
           03  TOUKEI02-RECESRYKA          PIC  X(02).
      *自保区分
           03  TOUKEI02-JIHOKBN            PIC  X(01).
      *剤点数
           03  TOUKEI02-ZAITEN             PIC  S9(08).
      *剤回数
           03  TOUKEI02-ZAIKAISU           PIC  9(04).
      *明細数
           03  TOUKEI02-MEISAISU           PIC  9(04).
      *手技点数１
           03  TOUKEI02-SYUTEN1            PIC  S9(07).
      *手技点数２
           03  TOUKEI02-SYUTEN2            PIC  S9(07).
      *薬剤点数
           03  TOUKEI02-YKZTEN             PIC  S9(07).
      *器材点数
           03  TOUKEI02-KIZAITEN           PIC  S9(07).
      *ドクターコード
           03  TOUKEI02-DRCD               PIC  X(05).
      *日数回数テーブル
           03  TOUKEI02-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
               05  TOUKEI02-KAISUINF       OCCURS  31.
                   07  TOUKEI02-KAISU      PIC  9(03).
      *診療行為情報
           03  TOUKEI02-SRYACTINF.
      *診療種別区分
               05  TOUKEI02-SRYSYUKBN      PIC  X(03).
      *診療コード
               05  TOUKEI02-SRYCD          PIC  X(09).
      *数量
               05  TOUKEI02-SRYSURYO       PIC  9(05)V9(05).
      *回数
               05  TOUKEI02-SRYKAISU       PIC  9(03).
      *点数識別区分
               05  TOUKEI02-TENSIKIBETU    PIC  9(01).
      *点数
               05  TOUKEI02-TEN            PIC  9(09)V9(02).
      *名称入力番号
               05  TOUKEI02-INPUTNUM       PIC  9(03).
      *自費金額
               05  TOUKEI02-JIHIMONEY      PIC  9(07).
      *単位コード
               05  TOUKEI02-TANICD         PIC  X(03).
      *単位名称
               05  TOUKEI02-TANINAME       PIC  X(24).
      *漢字名称
               05  TOUKEI02-NAME           PIC  X(200).
      *正式名称
               05  TOUKEI02-FORMALNAME     PIC  X(400).
      *薬価基準コード
               05  TOUKEI02-YAKKAKJNCD     PIC  X(12).
      *点数欄集計先識別
               05  TOUKEI02-TENTTLKBN      PIC  9(03).
