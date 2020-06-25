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
      *    収集データ（診療情報）         COPY (CPCDF002.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-伊藤    10/01/28  数量小数点５桁
      ************************************************************
      *01  COLDAT02-REC.
           03  COLDAT02-KEY.
               05  COLDAT02-KEY1.
      *            医療機関ＩＤ
                   07  COLDAT02-HOSPID     PIC  X(24).
      *            診療年月
                   07  COLDAT02-SRYYM      PIC  9(06).
      *            入外区分
                   07  COLDAT02-NYUGAIKBN  PIC  X(01).
      *            患者ＩＤ
                   07  COLDAT02-PTID       PIC  9(10).
      *            レセプト診療科
                   07  COLDAT02-RECEKA     PIC  X(02).
      *            主保険保険番号
                   07  COLDAT02-HKNNUM     PIC  X(03).
      *            公費ＩＤ
                   07  COLDAT02-KOHID      PIC  9(10).
      *            保険者番号        
                   07  COLDAT02-HKNJANUM   PIC  X(08).
      *            本人家族区分
                   07  COLDAT02-HONKZKKBN  PIC  X(01).
      *            補助区分
                   07  COLDAT02-HOJOKBN    PIC  X(01).
      *        診療科
               05  COLDAT02-SRYKA          PIC  X(02).
      *        診療区分
               05  COLDAT02-SRYKBN         PIC  X(02).
      *        剤番号
               05  COLDAT02-ZAINUM         PIC  9(08).
      *        連番
               05  COLDAT02-SEQNO          PIC  9(03).
      *連番号
           03  COLDAT02-RENNUM             PIC  9(02).
      *保険組合せ
           03  COLDAT02-HKNCOMBI           PIC  9(04).
      *
      *主保険−保険ＩＤ
           03  COLDAT02-HKNID              PIC  9(10).
      *公費ＩＤ
           03  COLDAT02-KOHID-AREA.
               05  COLDAT02-TKOHID         PIC  9(10)   OCCURS  4.
      *レセ電診療科
           03  COLDAT02-RECESRYKA          PIC  X(02).
      *自保区分
           03  COLDAT02-JIHOKBN            PIC  X(01).
      *剤点数
           03  COLDAT02-ZAITEN             PIC  9(08).
      *剤回数
           03  COLDAT02-ZAIKAISU           PIC  9(04).
      *明細数
           03  COLDAT02-MEISAISU           PIC  9(04).
      *手技点数１
           03  COLDAT02-SYUTEN1            PIC  9(07).
      *手技点数２
           03  COLDAT02-SYUTEN2            PIC  9(07).
      *薬剤点数
           03  COLDAT02-YKZTEN             PIC  9(07).
      *器材点数
           03  COLDAT02-KIZAITEN           PIC  9(07).
      *日数回数テーブル
           03  COLDAT02-DAY-AREA.
      *        診療回数テーブル
               05  COLDAT02-KAISUINF       OCCURS  31.
                   07  COLDAT02-KAISU      PIC  9(03).
      *診療行為情報
           03  COLDAT02-SRYACTINF.
      *診療種別区分
               05  COLDAT02-SRYSYUKBN      PIC  X(03).
      *診療コード
               05  COLDAT02-SRYCD          PIC  X(09).
      *数量
               05  COLDAT02-SRYSURYO       PIC  9(05)V9(05).
      *回数
               05  COLDAT02-SRYKAISU       PIC  9(03).
      *点数識別区分
               05  COLDAT02-TENSIKIBETU    PIC  9(01).
      *点数
               05  COLDAT02-TEN            PIC  9(09)V9(02).
      *名称入力番号
               05  COLDAT02-INPUTNUM       PIC  9(03).
      *自費金額
               05  COLDAT02-JIHIMONEY      PIC  9(07).
      *単位コード
               05  COLDAT02-TANICD         PIC  X(03).
      *単位名称
               05  COLDAT02-TANINAME       PIC  X(24).
      *漢字名称
               05  COLDAT02-NAME           PIC  X(200).
      *正式名称
               05  COLDAT02-FORMALNAME     PIC  X(400).
      *薬価基準コード
               05  COLDAT02-YAKKAKJNCD     PIC  X(12).
      *点数欄集計先識別
               05  COLDAT02-TENTTLKBN      PIC  9(03).
