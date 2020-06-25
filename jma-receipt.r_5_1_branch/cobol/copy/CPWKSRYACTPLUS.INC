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
      *    ワーク診療行為マスタ付加  COPY (CPWKSRYACTPLUS.INC)
      *
      *-------------------------------------------------*
      *03  WKSRYACTPLUS-REC.
      *作成区分（１：あり）
           05  WKSRYP-ADDKBN             PIC  X(01).
      *点数区分（０：プラス、１：マイナス）
           05  WKSRYP-PLUSKBN            PIC  9(01).
      *労災の円
           05  WKSRYP-RSIKINGAKU         PIC  9(07).
      *手技毎の内容（診察・手術・画像診断・処置）
           05  WKSRYP-SYUGI-TBL-G.
               07  WKSRYP-SYUGI-TBL      OCCURS   5.
      *開始手技コード
                   09  WKSRYP-SRYCD          PIC  X(09).
      *手技点数
                   09  WKSRYP-SYUTEN         PIC  9(07).
      *薬剤点数
                   09  WKSRYP-YKZTEN         PIC  9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                   09  WKSRYP-KIZAITEN       PIC  9(07).
      *フィルム点数
                   09  WKSRYP-FIRTEN         PIC  9(07).
      *酸素点数
           05  WKSRYP-SANSOTEN       PIC  9(07).
      *窒素点数
           05  WKSRYP-CHISOTEN       PIC  9(07).
      *院外処方点数
           05  WKSRYP-INGAITEN       PIC  9(07).
      *
      *内分泌負荷試験用
      *当月算定フラグ
           05  WKSRYP-MSANTEITFLG       PIC  9(01).
      *当月算定点数
           05  WKSRYP-MSANTEITEN        PIC  9(08).
      *当月算定点数区分
           05  WKSRYP-MSANTEITENKBN     PIC  9(01).
      *
      *内服７種類逓減対象（１：逓減明細剤、２：逓減）
           05  WKSRYP-TEI-NKBN           PIC  9(01).
      *H27.4
      *３０日投薬逓減対象（１：逓減明細剤、２：逓減）
           05  WKSRYP-TEI-NKBN30         PIC  9(01).
      *
      *API用
      *労災点数
           05  WKSRYP-RSI-TENSU          PIC  9(07).
      *
