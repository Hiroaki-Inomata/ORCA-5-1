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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.00     @@@@-田中    11/04/04  新規作成
      ********************************************************************
       01  HC17.
      *    システム日付
           03  HC17-SYSYMD              PIC X(22).
      *    診療日付
           03  HC17-SRYYMD              PIC X(22).
      *    頁数
           03  HC17-PAGE                PIC X(03).
      *    見出し
           03  HC17-MIDASI              PIC X(10).
      *    ジョブメッセージ
           03  HC17-JOBMSG              PIC X(60).
      *    入外区分
           03  HC17-NYUGAIKBN           PIC X(04).
      *    診療科
           03  HC17-SRYKA               PIC X(20).
      *    自費テーブル
           03  HC17-TJIHI-G             OCCURS 10 TIMES.
      *    自費名称
               05  HC17-TJIHI           PIC X(08).
      *    連番号テーブル
           03  HC17-RENNUM-G            OCCURS 21 TIMES.
      *    連番号
               05  HC17-RENNUM          PIC X(07).
      *    患者番号テーブル
           03  HC17-PTNUM-G             OCCURS 21 TIMES.
      *    患者番号（１０桁以下）
               05  HC17-PTNUM           PIC X(10).
      *    患者番号（１１桁以上）
               05  HC17-PTNUM2          PIC X(20).
      *    患者氏名テーブル
           03  HC17-NAME-G              OCCURS 21 TIMES.
      *    患者氏名
               05  HC17-NAME            PIC X(18).
      *    保険点数テーブル
           03  HC17-HKNTEN-G            OCCURS 21 TIMES.
      *    保険点数
               05  HC17-HKNTEN          PIC X(07).
      *    集計区分テーブル
           03  HC17-MONEY-G             OCCURS 20 TIMES.
      *    窓口請求額テーブル（集計区分別）
               05  HC17-MONEY-OCC       OCCURS 21 TIMES.
      *    窓口請求額（集計区分別）
                   07  HC17-MONEY       PIC X(07).
      *    医療収益テーブル
           03  HC17-SYUEKI-G            OCCURS 21 TIMES.
      *    医療収益
               05  HC17-SYUEKI          PIC X(08).
      *    窓口請求額テーブル
           03  HC17-SEIKYU-G            OCCURS 21 TIMES.
      *    窓口請求額
               05  HC17-SEIKYU          PIC X(08).
      *    窓口領収額テーブル
           03  HC17-RYOSYU-G            OCCURS 21 TIMES.
      *    窓口領収額
               05  HC17-RYOSYU          PIC X(08).
      *    保険点数―小計（７桁）
           03  HC17-G-HKNTEN            PIC X(07).
      *    保険点数―小計（８桁）
           03  HC17-GG-HKNTEN           PIC X(08).
      *    医療収益―小計（８桁）
           03  HC17-G-SYUEKI            PIC X(08).
      *    医療収益―小計（９桁）
           03  HC17-GG-SYUEKI           PIC X(09).
      *    集計区分テーブル―小計
           03  HC17-G-MONEY-TBL         OCCURS 20 TIMES.
      *    窓口請求額―小計（集計区分別）（７桁）
               05  HC17-G-MONEY         PIC X(07).
      *    集計区分テーブル―小計
           03  HC17-GG-MONEY-TBL        OCCURS 20 TIMES.
      *    窓口請求額―小計（集計区分別）（８桁）
               05  HC17-GG-MONEY        PIC X(08).
      *    窓口請求額―小計（８桁）
           03  HC17-G-SEIKYU            PIC X(08).
      *    窓口請求額―小計（９桁）
           03  HC17-GG-SEIKYU           PIC X(09).
      *    窓口領収額―小計（８桁）
           03  HC17-G-RYOSYU            PIC X(08).
      *    窓口領収額―小計（９桁）
           03  HC17-GG-RYOSYU           PIC X(09).
