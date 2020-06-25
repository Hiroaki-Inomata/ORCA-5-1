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
      *    診療行為マスタ     COPY (CPSRYACT.INC)       *
      *-------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/10/02  換算入力数量追加
      ************************************************************
      *01  SRYACT-REC.
           03  SRY-KEY.
      *医療機関ＩＤ
               05  SRY-HOSPNUM          PIC 9(2).
      *入外区分
               05  SRY-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  SRY-PTID            PIC  9(10).
      *診療科
               05  SRY-SRYKA           PIC  X(02).
      *診療年月
               05  SRY-SRYYM           PIC  X(06).
      *剤番号
               05  SRY-ZAINUM          PIC  9(08).
      *連番号
               05  SRY-RENNUM          PIC  9(02).
      *診療種別区分
           03  SRY-SRYSYUKBN           PIC  X(03).
      *診療区分
           03  SRY-SRYKBN              PIC  X(02).
      *自費金額
           03  SRY-JIHIMONEYTOTAL      PIC  9(07).
      *診療行為テーブル
           03  SRY-SRYACT-TBL          OCCURS   5.
      *診療コード
               05  SRY-SRYCD           PIC  X(09).
      *数量
               05  SRY-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  SRY-SRYKAISU        PIC  9(03).
      *明細請求区分
      *        1:商品名材料の特定器材、2:関係コメント指示、3:内服種類数指示
               05  SRY-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  SRY-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  SRY-INPUTNUM        PIC  9(03).
      *自費金額
               05  SRY-JIHIMONEY       PIC  9(07).
      *換算数量入力
               05  SRY-KANSURYO        PIC  9(05)V9(05).
      *端末ＩＤ
           03  SRY-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  SRY-OPID                PIC  X(16).
      *作成年月日
           03  SRY-CREYMD              PIC  X(08).
      *更新年月日
           03  SRY-UPYMD               PIC  X(08).
      *更新時間
           03  SRY-UPHMS               PIC  X(06).
      *
