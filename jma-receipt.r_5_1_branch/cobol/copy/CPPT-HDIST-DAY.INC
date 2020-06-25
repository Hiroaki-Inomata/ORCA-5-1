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
      *****************************************************************
      *  患者レセプト分割テーブル       (CPPT-HDIST-DAY.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  PT-HDIST-DAY-REC.
           03  PT-HDIST-DAY-KEY.
      *医療機関ＩＤ
               05  PT-HDIST-DAY-HOSPNUM    PIC 9(2).
      *患者ＩＤ
               05  PT-HDIST-DAY-PTID       PIC 9(10).
      *入外区分
               05  PT-HDIST-DAY-NYUGAIKBN  PIC X(01).
      *診療年月
               05  PT-HDIST-DAY-SRYYM      PIC X(06).
      *振分日
               05  PT-HDIST-DAY-DAY        PIC X(02).
      *レセプト区分
               05  PT-HDIST-DAY-RECEIPT-KBN
                                           PIC X(01).
      *保険組合せ情報（１：前 ２：後）
           03  PT-HDIST-DAY-HKNCOMBI-INF.
               05  PT-HDIST-DAY-HKNCOMBI-OCC   OCCURS 2.
                   07  PT-HDIST-DAY-HKNCOMBI-T OCCURS 20.
      *保険組合せ
                       09  PT-HDIST-DAY-HKNCOMBI
                                           PIC 9(04).
      *削除フラグ
                       09  PT-HDIST-DAY-DLTFLG
                                           PIC X(01).
      *端末ＩＤ
           03  PT-HDIST-DAY-TERMID         PIC X(16).
      *オペーレータＩＤ
           03  PT-HDIST-DAY-OPID           PIC X(16).
      *作成年月日
           03  PT-HDIST-DAY-CREYMD         PIC X(08).
      *更新年月日
           03  PT-HDIST-DAY-UPYMD          PIC X(08).
      *更新時間
           03  PT-HDIST-DAY-UPHMS          PIC X(06).
