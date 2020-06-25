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
      ******************************************************************
      *    ＡＰＩレスポンス診療行為情報一括取得
      *                                      COPY (CPORAPI052BR7.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  MDCRES-OUT-REC.
      *    患者情報
           03  MDCRES-PATIENT-INFOMATION.
      *        患者番号
               05  MDCRES-PATIENTID         PIC X(20).
      *        患者氏名
               05  MDCRES-NAME              PIC X(100).
      *        患者カナ氏名
               05  MDCRES-KANANAME          PIC X(100).
      *        誕生日
               05  MDCRES-BIRTHDAY          PIC X(10).
      *        性別
               05  MDCRES-SEX               PIC X(1).
      *    受診日付
           03  MDCRES-PERFORM-DATE        PIC X(10).
      *    診療科
           03  MDCRES-DEP-CODE            PIC X(2).
      *    診療科短縮名称
           03  MDCRES-DEP-NAME            PIC X(20).
      *    連番
           03  MDCRES-RENNUM              PIC X(01).
      *    伝票番号（外来のみ）
           03  MDCRES-INVOICD-NUMBER      PIC X(07).
      *    保険組合せ番号
           03  MDCRES-COMBINATION-NUMBER  PIC X(04).
      *    ドクター（外来のみ）
           03  MDCRES-DRCD                PIC X(5).
      *    ドクター氏名
           03  MDCRES-DRCD-NAME           PIC X(80).
      *    更新日付（受診履歴）
           03  MDCRES-UPD-DATE            PIC X(10).
      *    更新時間（受診履歴）
           03  MDCRES-UPD-TIME            PIC X(8).
      *
           03  MDCRES-MEDICAL-INF-OVERFLOW
                                          PIC X(5).
      *    診療内容情報
           03  MDCRES-MEDICAL-INF.
      *        診療行為内容
               05  MDCRES-MDC-INFO        OCCURS   50.
      *            診療種別区分
                   07  MDCRES-MDC-CLASS           PIC X(03).
      *            診療種別区分名称
                   07  MDCRES-MDC-CLASS-NAME      PIC X(50).
      *            回数
                   07  MDCRES-MDC-CLASS-NUMBER    PIC X(03).
      *            点数
                   07  MDCRES-MDC-POINT           PIC X(08).
      *            金額（自費・労災円）
                   07  MDCRES-MDC-MOENY           PIC X(08).
      *            剤区分
                   07  MDCRES-MDC-CODE            PIC X(01).
      *            包括区分（True：包括剤）
                   07  MDCRES-INCLUSION-CLASS     PIC X(05).
      *            包括検査の検査数
                   07  MDCRES-HKTKNSA-CNT         PIC X(02).
      *            剤内容
                   07  MDCRES-PRESCRIPTION-INFO   OCCURS  50.
      *                診療コード
                       09  MDCRES-PRSCRPT-CODE         PIC X(9).
      *                診療名称（コメント）
                       09  MDCRES-PRSCRPT-NAME         PIC X(140).
      *                数量
                       09  MDCRES-PRSCRPT-NUMBER       PIC X(11).
      *                単位
                       09  MDCRES-UNIT-CODE            PIC X(03).
      *                単位名称
                       09  MDCRES-UNIT-NAME            PIC X(24).
      *
