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
      *    療養レセプトコメントマスタ    COPY (CPRYOUYOCOM.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  RYOUYOCOM-REC.
           03  RYOCOM-KEY.
      *医療機関ＩＤ
               05  RYOCOM-HOSPNUM         PIC 9(2).
      *入外区分
               05  RYOCOM-NYUGAIKBN      PIC  X(01).
      *患者ＩＤ
               05  RYOCOM-PTID           PIC  9(10).
      *診療年月
               05  RYOCOM-SRYYM          PIC  X(06).
      *剤番号
               05  RYOCOM-ZAINUM         PIC  9(08).
      *剤識別区分（１：ＡＤＬ点数　２：医療区分）
           03  RYOCOM-ZAISKBKBN          PIC  X(01).
      *診療コード（コメントコード）
           03  RYOCOM-SRYCD              PIC  X(09).
      *月固定値
           03  RYOCOM-MONTH              PIC  9(02).
      *区分（点数）テーブル
           03  RYOCOM-DAY-AREA.
      *        区分点数テーブル
             04  RYOCOM-DAY-TBL.
      *        診療回数（１日から３１日）
               05  RYOCOM-DAY-1          PIC  9(02).
               05  RYOCOM-DAY-2          PIC  9(02).
               05  RYOCOM-DAY-3          PIC  9(02).
               05  RYOCOM-DAY-4          PIC  9(02).
               05  RYOCOM-DAY-5          PIC  9(02).
               05  RYOCOM-DAY-6          PIC  9(02).
               05  RYOCOM-DAY-7          PIC  9(02).
               05  RYOCOM-DAY-8          PIC  9(02).
               05  RYOCOM-DAY-9          PIC  9(02).
               05  RYOCOM-DAY-10         PIC  9(02).
               05  RYOCOM-DAY-11         PIC  9(02).
               05  RYOCOM-DAY-12         PIC  9(02).
               05  RYOCOM-DAY-13         PIC  9(02).
               05  RYOCOM-DAY-14         PIC  9(02).
               05  RYOCOM-DAY-15         PIC  9(02).
               05  RYOCOM-DAY-16         PIC  9(02).
               05  RYOCOM-DAY-17         PIC  9(02).
               05  RYOCOM-DAY-18         PIC  9(02).
               05  RYOCOM-DAY-19         PIC  9(02).
               05  RYOCOM-DAY-20         PIC  9(02).
               05  RYOCOM-DAY-21         PIC  9(02).
               05  RYOCOM-DAY-22         PIC  9(02).
               05  RYOCOM-DAY-23         PIC  9(02).
               05  RYOCOM-DAY-24         PIC  9(02).
               05  RYOCOM-DAY-25         PIC  9(02).
               05  RYOCOM-DAY-26         PIC  9(02).
               05  RYOCOM-DAY-27         PIC  9(02).
               05  RYOCOM-DAY-28         PIC  9(02).
               05  RYOCOM-DAY-29         PIC  9(02).
               05  RYOCOM-DAY-30         PIC  9(02).
               05  RYOCOM-DAY-31         PIC  9(02).
           03  RYOCOM-DAY-AREA-R         REDEFINES   RYOCOM-DAY-AREA.
      *        区分回数テーブル
               05  RYOCOM-DAY-TBL2.
                   07  RYOCOM-DAY        PIC  9(02)
                                         OCCURS   31.
      *端末ＩＤ
           03  RYOCOM-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  RYOCOM-OPID               PIC  X(16).
      *作成年月日
           03  RYOCOM-CREYMD             PIC  X(08).
      *更新年月日
           03  RYOCOM-UPYMD              PIC  X(08).
      *更新時間
           03  RYOCOM-UPHMS              PIC  X(06).
      *
