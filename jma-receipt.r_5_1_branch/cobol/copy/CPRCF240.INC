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
      *    公害レセプト（入院）・適用欄明細情報  COPY (CPRCF240.INC) 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-門脇    10/03/29  診療日２の追加
      *  04.05.01    NACL-門脇    10/01/19  剤区分追加
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      ************************************************************
      *01  RECE240-REC.
           03  RECE240-KEY.
               05  RECE240-KEY1.
      *医療機関番号
                   07  RECE240-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE240-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE240-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE240-PTID        PIC 9(10).
      *レセプト診療科
                   07  RECE240-RECEKA      PIC X(02).
      *公費ＩＤ
                   07  RECE240-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE240-TEKSTYMD    PIC X(08). 
      *診療区分
               05  RECE240-SRYKBN          PIC X(02).
      *自保区分
               05  RECE240-JIHOKBN         PIC X(01).
      *剤区分
               05  RECE240-ZAIKBN          PIC 9(03).
      *剤識別区分
               05  RECE240-ZAISKBKBN       PIC X(01).
      *剤識別内番号
               05  RECE240-ZAISKBBAN       PIC 9(08).
      *点滴出力区分
               05  RECE240-TENTEKIKBN      PIC 9(02).
      *点滴出力区分別内番号
               05  RECE240-TENTEKIBAN      PIC 9(08).
      *検査実施区分
               05  RECE240-KNSJISKBN       PIC 9(01).
      *診療種別区分
               05  RECE240-SRYSYUKBN       PIC X(03).
      *診療日
               05  RECE240-DAY             PIC 9(02).
      *表用番号
               05  RECE240-CDKBN-SYO       PIC 9(01).
               05  RECE240-CDKBN-BU        PIC 9(02).
               05  RECE240-CDKBN-KBN       PIC X(01).
               05  RECE240-CDKBN-KBNNUM    PIC 9(03).
               05  RECE240-CDKBN-KBNNUM-EDA
                                           PIC 9(02).
               05  RECE240-CDKBN-KOUBAN    PIC 9(02).
               05  RECE240-KOUHYOJUNNUM    PIC 9(09).
      *診療日２
               05  RECE240-DAY2            PIC 9(02).
      *剤点数
               05  RECE240-ZAITEN          PIC 9(08).
      *剤番号
               05  RECE240-ZAINUM          PIC 9(08).
      *連番号
               05  RECE240-LINENUM         PIC 9(05).
      *剤明細数
           03  RECE240-ZAIMEISAISU         PIC 9(03).
      *患者番号
           03  RECE240-PTNUM               PIC X(20).
      *適要欄情報
           03  RECE240-TEKIYO-INF.
      *剤区切（＊）
               05  RECE240-ASTER           PIC X(02).
      *摘要
               05  RECE240-TEKIYO          PIC X(100).
      *（サ）フラグ
           03  RECE240-SAFLG               PIC X(01).
