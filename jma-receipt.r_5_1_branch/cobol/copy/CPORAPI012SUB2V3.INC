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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩＶ３　保険組合せ編集パラメタ
      *                                         (CPORAPI012SUB2V3.INC)
      ******************************************************************
       01  ORAPI012SUB2V3AREA.
      *
      *    処理区分（１：登録内容、２：チェック）
           03  AP12SUB2V3-SYORIKBN     PIC X(01).
      *    システム日付
           03  AP12SUB2V3-SYSYMD       PIC X(08).
      *
      *    保険組合せ情報
           03  AP12SUB2V3-COMBINATION-INF-G.
               05  AP12SUB2V3-COMBINATION-INF   OCCURS 200 TIMES.
      *            処理区分（新規・削除のみ）
                   07  AP12SUB2V3-COMB-MODE              PIC X(06).
      *            保険組合せ番号
                   07  AP12SUB2V3-COMBINATION-NUMBER     PIC X(04).
      *            保険組合せ　負担割合（入院）
                   07  AP12SUB2V3-COMBINATION-RATE-ADM   PIC X(04).
      *            保険組合せ　負担割合（外来）
                   07  AP12SUB2V3-COMBINATION-RATE-OUT   PIC X(04).
      *            保険組合せの有効開始日
                   07  AP12SUB2V3-COMB-START-DATE        PIC X(10).
      *            保険組合せの有効終了日
                   07  AP12SUB2V3-COMB-END-DATE          PIC X(10).
      *
      *            保険ＩＤ
                   07  AP12SUB2V3-COMB-MAIN-ID           PIC X(10).
      *            保険の種類
                   07  AP12SUB2V3-COMB-MAIN-CLASS        PIC X(03).
      *            保険者番号
                   07  AP12SUB2V3-COMB-MAIN-NUMBER       PIC X(08).
      *            保険の種類名称
                   07  AP12SUB2V3-COMB-MAIN-NAME         PIC X(20).
      *            記号
                   07  AP12SUB2V3-COMB-MARK              PIC X(80).
      *            番号
                   07  AP12SUB2V3-COMB-NUMBER            PIC X(80).
      *            枝番
                   07  AP12SUB2V3-COMB-EDABAN            PIC X(02).
      *            継続区分
                   07  AP12SUB2V3-COMB-CONTINUATION       PIC X(01).
      *            補助区分
                   07  AP12SUB2V3-COMB-ASSISTANCE         PIC X(01).
      *            補助区分名称
                   07  AP12SUB2V3-COMB-ASSI-NAME          PIC X(20).
      *            本人家族区分
                   07  AP12SUB2V3-COMB-FAMILY             PIC X(01).
      *            公費情報
                   07  AP12SUB2V3-COMB-PUBLIC-INS-INF
                                              OCCURS  4 TIMES.
      *                公費ＩＤ
                       09  AP12SUB2V3-INSURANCE-ID         PIC X(10).
      *                公費の種類
                       09  AP12SUB2V3-INSURANCE-CLASS      PIC X(03).
      *                公費の種類名称
                       09  AP12SUB2V3-INSURANCE-NAME       PIC X(20).
      *                負担者番号
                       09  AP12SUB2V3-PROVIDER-NUMBER      PIC X(08).
      *                受給者番号
                       09  AP12SUB2V3-RECIPIENT-NUMBER     PIC X(20).
      *                入院−負担率（割）
                       09  AP12SUB2V3-RATE-ADMISSION       PIC X(04).
      *                入院−固定額
                       09  AP12SUB2V3-MONEY-ADMISSION      PIC X(06).
      *                外来−負担率（割）
                       09  AP12SUB2V3-RATE-OUTPATIENT      PIC X(04).
      *                外来−固定額
                       09  AP12SUB2V3-MONEY-OUTPATIENT     PIC X(06).
      *
      *    件数
           03  AP12SUB2V3-MAX              PIC 9(04).
      *    エラーコード
           03  AP12SUB2V3-ERRCD            PIC X(02).
      *
