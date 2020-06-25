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
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      ********************************************************************
       01  L200.
           03  L200-PAGE                PIC  X(03).
           03  L200-SEIYMD              PIC  X(22).
           03  L200-SRYYM               PIC  X(16).
           03  L200-NYUGAI              PIC  X(04).
           03  L200-SRYKANAME           PIC  X(20).
           03  L200-GHKNJANUM           PIC  X(16).
           03  L200-GFTNJANUM           PIC  X(20).
      *    エラーメッセージ
           03  L200-ERR-MSG             PIC  X(60).
           03  L200-KANJA-TBL.
      *    NO
               05  L200-NO-TBL.
                   07  L200-NO          PIC  X(04) OCCURS 40.
      *    患者番号
               05  L200-PTNUM-TBL.
                   07  L200-PTNUM       PIC  X(10) OCCURS 40.
      *    患者氏名
               05  L200-NAME-TBL.
                   07  L200-NAME        PIC  X(20) OCCURS 40.
      *    保険者番号
               05  L200-HKNJANUM-TBL.
                   07  L200-HKNJANUM    PIC  X(16) OCCURS 40.
      *    保険者名
               05  L200-HKNJANAME-TBL.
                   07  L200-HKNJANAME   PIC  X(20) OCCURS 40.
      *    公費負担者番号
               05  L200-FTNJANUM-TBL.
                   07  L200-FTNJANUM1   PIC  X(20) OCCURS 40.
                   07  L200-FTNJANUM2   PIC  X(20) OCCURS 40.
      *    生年月日
               05  L200-TANJO-TBL.
                   07  L200-TANJO       PIC  X(10) OCCURS 40.
      *    年齢
               05  L200-AGE-TBL.
                   07  L200-AGE         PIC  X(03) OCCURS 40.
      *    本人・家族区分
               05  L200-HONKZKKBN-TBL.
                   07  L200-HONKZKKBN   PIC  X(02) OCCURS 40.
      *    負担割合
               05  L200-FUTANWARI-TBL.
                   07  L200-FUTANWARI   PIC  X(04) OCCURS 40.
      *    診療年月(月後れ時のみ)
               05  L200-SRYYM-TBL.
                   07  L200-SRYYM2      PIC  X(06) OCCURS 40.
      *    実日数
               05  L200-NISSU-TBL.
                   07  L200-NISSU       PIC  X(04) OCCURS 40.
      *    点数
               05  L200-TENSU-TBL.
                   07  L200-TENSU       PIC  X(14) OCCURS 40.
      *    一部負担金
               05  L200-ITBFTN-TBL.
                   07  L200-ITBFTN      PIC  X(14) OCCURS 40.
      *    食事日数
               05  L200-SNISSU-TBL.
                   07  L200-SNISSU      PIC  X(04) OCCURS 40.
      *    食事療養費
               05  L200-KINGK-TBL.
                   07  L200-KINGK       PIC  X(14) OCCURS 40.
      *    食事標準負担額
               05  L200-FTNGK-TBL.
                   07  L200-FTNGK       PIC  X(12) OCCURS 40.
      *    合計エリア
           03  L200-GOKEI-AREA.
               05  L200-GNISSU          PIC  X(04).
               05  L200-GTENSU          PIC  X(10).
               05  L200-GITBFTN         PIC  X(10).
               05  L200-GSNISSU         PIC  X(04).
               05  L200-GKINGK          PIC  X(10).
               05  L200-GFTNGK          PIC  X(09).
