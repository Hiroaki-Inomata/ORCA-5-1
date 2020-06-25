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
      *    患者定期請求履歴                   (CPPTTEIKIRRK.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTTEIKIRRK-REC.
      *医療機関ＩＤ
           03  PTTEIKIRRK-HOSPNUM           PIC 9(2).
      *患者ＩＤ
           03  PTTEIKIRRK-PTID             PIC  9(10).
      *診療年月
           03  PTTEIKIRRK-SRYYM            PIC  X(06).
      *作成区分（処理期）
      *    １〜３：各処理期／９：月末時のみ（入退院登録で指定）
           03  PTTEIKIRRK-SAKKBN           PIC  X(01).
      *保険組合せ
           03  PTTEIKIRRK-HKNCOMBINUM      PIC  9(04).
      *収納−伝票番号
           03  PTTEIKIRRK-DENPNUM          PIC  9(07).
      *削除フラグ
           03  PTTEIKIRRK-DELFLG           PIC  9(01).
      *処理区分
           03  PTTEIKIRRK-SYOKBN           PIC  X(01).
      *期別
           03  PTTEIKIRRK-KBT              PIC  X(01).
      *請求開始年月日
           03  PTTEIKIRRK-SKYSTYMD         PIC  X(08).
      *請求終了年月日
           03  PTTEIKIRRK-SKYEDYMD         PIC  X(08).
      *請求点数
           03  PTTEIKIRRK-SKYTEN           PIC  S9(09).
      *請求金額−消費税（再掲）
           03  PTTEIKIRRK-SKYMONEY-TAX-SAI PIC  S9(07).
      *請求金額
           03  PTTEIKIRRK-SKYMONEY         PIC  S9(09).
      *入院履歴−履歴連番
           03  PTTEIKIRRK-RRKNUM           PIC  9(03).
      *収納−伝票番号枝番
           03  PTTEIKIRRK-DENPEDANUM       PIC  9(02).
      *請求データ更新フラグ（０：新規／１：訂正）
           03  PTTEIKIRRK-SKYUPDFLG        PIC  9(01).
      *端末ＩＤ
           03  PTTEIKIRRK-TERMID           PIC  X(16).
      *オペーレータＩＤ
           03  PTTEIKIRRK-OPID             PIC  X(16).
      *作成年月日
           03  PTTEIKIRRK-CREYMD           PIC  X(08).
      *更新年月日
           03  PTTEIKIRRK-UPYMD            PIC  X(08).
      *更新時間
           03  PTTEIKIRRK-UPHMS            PIC  X(06).
