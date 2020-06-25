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
      *    通算日数取得サブ          COPY (CPORCSTUSAN.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTUSANAREA.
      *  医療機関コード
           03  STUSAN-HOSPNUM              PIC 9(2).
      *  患者ＩＤ
           03  STUSAN-PTID                 PIC  9(10).
      *    基準日
           03  STUSAN-KJNYMD               PIC X(08).
      *    特定入院料コード（設定は任意）
           03  STUSAN-TOKUTEI-SRYCD        PIC X(09)
                                           OCCURS 10.
      *    履歴番号設定区分（設定は任意）
           03  STUSAN-RRKNUMKBN            PIC X(01).
      *    履歴番号（履歴番号設定区分が"1"の場合に設定）
           03  STUSAN-RRKNUM               PIC 9(03).
      *    入院会計検索省略
           03  STUSAN-NOT-SELECT-NACCT     PIC X(01).
      *
      *  出力領域
      *    通算日数
           03  STUSAN-OUT-AREA.
               05  STUSAN-NISSU-VAL.
      *            日数１：今回入院日から基準日までの通算日数
                   07  STUSAN-NISSU01      PIC 9(08).
      *            日数２：初回入院から基準日までの通算日数（他院分含む）
                   07  STUSAN-NISSU02      PIC 9(08).
      *            日数３：初回入院から基準日までの通算日数（他院分含めず）
                   07  STUSAN-NISSU03      PIC 9(08).
      *            日数４：初回入院から基準日までの通算日数
      *            （一般入院料で入院していた日数（他院分含む））
                   07  STUSAN-NISSU04      PIC 9(08).
      *            日数５：初回入院から基準日までの通算日数
      *            （一般入院料で入院していた日数（他院分含めず））
                   07  STUSAN-NISSU05      PIC 9(08).
      *            日数６：今回入院から基準日前日までの特定入院料通算日数
                   07  STUSAN-NISSU06      PIC 9(08).
      *            日数７：今回入院から基準日前日までの引数で指定した
      *            特定入院料での通算日数
                   07  STUSAN-NISSU07      PIC 9(08).
      *            日数８：初回入院から基準日前日までの特定入院料通算日数
      *            （自院歴分含む・他院歴分含めず）
                   07  STUSAN-NISSU08      PIC 9(08).
      *            日数９：初回入院から基準日前日までの引数で指定した
      *            特定入院料での通算日数
                   07  STUSAN-NISSU09      PIC 9(08).
               05  STUSAN-TUSAN-R  REDEFINES   STUSAN-NISSU-VAL.
                   07  STUSAN-TUSAN-OCC    OCCURS  9.
                       09  STUSAN-NISSU    PIC 9(08).
      *            基準日チェックフラグ　（0:対象外／1:対象）
      *            （基準日の入院料が通算日数の対象となるかチェック）
               05  STUSAN-KJNYMD-CHK-FLG   PIC 9(01).
      *        特別な関係にある医療機関に入院していた日数
               05  STUSAN-TAINREL-NISSU    PIC 9(08).
               05  STUSAN-RRK-VAL.
      *            テーブルオーバーフラグ
                   07  STUSAN-RRK-FLG-OVER         PIC 9(01).
                   07  STUSAN-RRK-CNT              PIC 9(05).
                   07  STUSAN-RRK-OCC              OCCURS  50.
                       09  STUSAN-RRK-JTIKBN       PIC X(01).
                       09  STUSAN-RRK-TAINRELKBN   PIC X(01).
                       09  STUSAN-RRK-NYUINYMD     PIC X(08).
                       09  STUSAN-RRK-TAIINYMD     PIC X(08).
                       09  STUSAN-RRK-DJKBN        PIC X(01).
      *                一般入院料で入院していた日数
      *                （歴作成データの場合は対象日数）
                       09  STUSAN-RRK-NISSU        PIC 9(08).
      *  リターンコード
           03  STUSAN-RC               PIC 9(02).
