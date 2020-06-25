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
      *    診療会計マスタ付加  COPY (CPSRYACCTPLUS.INC)
      *
      *-------------------------------------------------*
      *01  SRYACCTPLUS-REC.
           03  ACCTP-KEY.
      *医療機関ＩＤ
               05  ACCTP-HOSPNUM         PIC 9(2).
      *入外区分
               05  ACCTP-NYUGAIKBN      PIC  X(01).
      *患者ＩＤ
               05  ACCTP-PTID           PIC  9(10).
      *診療科
               05  ACCTP-SRYKA          PIC  X(02).
      *診療年月
               05  ACCTP-SRYYM          PIC  X(06).
      *剤番号
               05  ACCTP-ZAINUM         PIC  9(08).
      *連番号（通常は１件のみ、内服薬の逓減の場合複数）
               05  ACCTP-RENNUM         PIC  9(02).
      *点数区分（０：プラス、１：マイナス）
           03  ACCTP-PLUSKBN            PIC  9(01).
      *労災の円
           03  ACCTP-RSIKINGAKU         PIC  9(07).
      *手技毎の内容（手術・画像診断・処置）
           03  ACCTP-SYUGI-TBL-G.
               05  ACCTP-SYUGI-TBL      OCCURS   5.
      *手技開始コード
                   07  ACCTP-SRYCD          PIC  X(09).
      *手技点数
                   07  ACCTP-SYUTEN         PIC  9(07).
      *薬剤点数
                   07  ACCTP-YKZTEN         PIC  9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                   07  ACCTP-KIZAITEN       PIC  9(07).
      *フィルム点数
                   07  ACCTP-FIRTEN         PIC  9(07).
      *酸素点数
           03  ACCTP-SANSOTEN       PIC  9(07).
      *窒素点数
           03  ACCTP-CHISOTEN       PIC  9(07).
      *院外処方剤点数
           03  ACCTP-INGAITEN          PIC  9(07).
      *
      *内服７種類逓減対象剤番号
           03  ACCTP-GRP-ZAINUM-G.
               05  ACCTP-GRP-ZAINUM        PIC 9(08)
                                        OCCURS   15.
      *端末ＩＤ
           03  ACCTP-TERMID            PIC  X(16).
      *オペーレータＩＤ
           03  ACCTP-OPID              PIC  X(16).
      *作成年月日
           03  ACCTP-CREYMD            PIC  X(08).
      *更新年月日
           03  ACCTP-UPYMD             PIC  X(08).
      *更新時間
           03  ACCTP-UPHMS             PIC  X(06).
      *
