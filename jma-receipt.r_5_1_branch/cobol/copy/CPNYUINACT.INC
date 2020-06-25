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
      *-------------------------------------------------*
      *    入院行為マスタ     COPY (CPNYUINACT.INC)     *
      *-------------------------------------------------*
      *01  NYUIN-SRYACT-REC.
           03  NSRY-KEY.
      *医療機関ＩＤ
               05  NSRY-HOSPNUM          PIC 9(2).
      *入外区分
               05  NSRY-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  NSRY-PTID            PIC  9(10).
      *診療科
               05  NSRY-SRYKA           PIC  X(02).
      *診療年月
               05  NSRY-SRYYM           PIC  X(06).
      *剤番号
               05  NSRY-ZAINUM          PIC  9(08).
      *連番号
               05  NSRY-RENNUM          PIC  9(02).
      *診療種別区分
           03  NSRY-SRYSYUKBN           PIC  X(03).
      *診療区分
           03  NSRY-SRYKBN              PIC  X(02).
      *自費金額計
           03  NSRY-JIHIMONEYTOTAL      PIC  9(07).
      *診療行為テーブル
           03  NSRY-SRYACT-TBL          OCCURS   5.
      *診療コード
               05  NSRY-SRYCD           PIC  X(09).
      *数量
               05  NSRY-SRYSURYO        PIC  9(05)V9(03).
      *回数
               05  NSRY-SRYKAISU        PIC  9(03).
      *自動算定区分
               05  NSRY-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  NSRY-INPUTNUM        PIC  9(03).
      *自費金額
               05  NSRY-JIHIMONEY       PIC  9(07).
      *端末ＩＤ
           03  NSRY-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  NSRY-OPID                PIC  X(16).
      *作成年月日
           03  NSRY-CREYMD              PIC  X(08).
      *更新年月日
           03  NSRY-UPYMD               PIC  X(08).
      *更新時間
           03  NSRY-UPHMS               PIC  X(06).
      *
