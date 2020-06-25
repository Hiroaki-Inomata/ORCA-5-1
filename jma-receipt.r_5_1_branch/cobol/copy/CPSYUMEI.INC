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
      *    収納明細マスタ    COPY (CPSYUMEI.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SYUMEI-REC.
           03  SMEI-KEY.
      *医療機関ＩＤ
               05  SMEI-HOSPNUM          PIC 9(2).
      *入外区分
               05  SMEI-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  SMEI-PTID            PIC  9(10).
      *伝票番号
               05  SMEI-DENPNUM         PIC  9(07).
      *伝票番号枝番
               05  SMEI-DENPEDANUM      PIC  9(02).
      *入金連番
           03  SMEI-NYUKINRENNUM        PIC  9(03).
      *診療科
           03  SMEI-SRYKA               PIC  X(02).
      *明細状態区分
           03  SMEI-MEISAIJOUTAIKBN     PIC  X(01).
      *請求発行日
           03  SMEI-SKYPRINTYMD         PIC  X(08).
      *請求再発行日
           03  SMEI-SKYREPRINTYMD       PIC  X(08).
      *領収書発行日
           03  SMEI-RYOSYUPRINTYMD      PIC  X(08).
      *領収書再発行日
           03  SMEI-RYOSYUREPRINTYMD    PIC  X(08).
      *請求金額
           03  SMEI-SKYMONEY            PIC S9(07).
      *入返金区分
           03  SMEI-NYUHEN-KBN          PIC  X(01).
      *入返金額
           03  SMEI-NYUHEN-MONEY        PIC S9(07).
      *入返金日
           03  SMEI-NYUHEN-YMD          PIC  X(08).
      *入金方法
           03  SMEI-NYUKIN-HOHO         PIC  X(02).
      *状態区分
           03  SMEI-JOUTAIKBN           PIC  X(01).
      *日報キー
           03  SMEI-DAILYKEY            PIC  X(12).
      *収納履歴番号
           03  SMEI-SYURRKNUM           PIC 9(2).
      *収納履歴枝番号
           03  SMEI-SYUEDANUM           PIC 9(2).
      *収納履歴更新区分
           03  SMEI-SYURRKUPDKBN        PIC X(01).
      *端末ＩＤ
           03  SMEI-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  SMEI-OPID                PIC  X(16).
      *作成年月日
           03  SMEI-CREYMD              PIC  X(08).
      *作成時間
           03  SMEI-CREHMS              PIC  X(06).
      *更新年月日
           03  SMEI-UPYMD               PIC  X(08).
      *更新時間
           03  SMEI-UPHMS               PIC  X(06).
      *
      *診療年月日（検索用）
           03  SMEI-UPNYUHEN-YMD        PIC  X(08).
      *開始日報キー
           03  SMEI-STDAILYKEY          PIC  X(12).
      *終了日報キー
           03  SMEI-EDDAILYKEY          PIC  X(12).
      *開始作成年月日
           03  SMEI-STCREYMD            PIC  X(8).
      *終了作成年月日
           03  SMEI-EDCREYMD            PIC  X(8).
