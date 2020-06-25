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
      *----------------------------------------------------------------*
      *    改元コメントコードテーブル
      *                            COPY (CMCOMGENGO.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       01  TBL-GENGO-COM-AREA.
      *    救急医療管理入院
           03  FILLER              PIC X(11)   VALUE   "4 840000020".
           03  FILLER              PIC X(11)   VALUE   "5 840000520".
      *    救命救急入院
           03  FILLER              PIC X(11)   VALUE   "4 840000021".
           03  FILLER              PIC X(11)   VALUE   "5 840000521".
      *    骨髄移植実施日
           03  FILLER              PIC X(11)   VALUE   "4 840000034".
           03  FILLER              PIC X(11)   VALUE   "5 840000534".
      *    再入院
           03  FILLER              PIC X(11)   VALUE   "4 840000014".
           03  FILLER              PIC X(11)   VALUE   "5 840000514".
      *    算定開始
           03  FILLER              PIC X(11)   VALUE   "4 840000056".
           03  FILLER              PIC X(11)   VALUE   "5 840000556".
      *    小児特定第１回目カウンセリング
           03  FILLER              PIC X(11)   VALUE   "4 840000104".
           03  FILLER              PIC X(11)   VALUE   "5 840000604".
      *    傷病手当金意見書交付
           03  FILLER              PIC X(11)   VALUE   "4 840000011".
           03  FILLER              PIC X(11)   VALUE   "5 840000511".
      *    （相続）傷病手当金意見書交付
           03  FILLER              PIC X(11)   VALUE   "4 840000012".
           03  FILLER              PIC X(11)   VALUE   "5 840000512".
      *    歯科矯正管理料算定
           03  FILLER              PIC X(11)   VALUE   "4 840000023".
           03  FILLER              PIC X(11)   VALUE   "5 840000523".
      *    歯面清掃２回目以降
           03  FILLER              PIC X(11)   VALUE   "4 840000120".
           03  FILLER              PIC X(11)   VALUE   "5 840000620".
      *    人工腎臓導入期
           03  FILLER              PIC X(11)   VALUE   "4 840000007".
           03  FILLER              PIC X(11)   VALUE   "5 840000507".
      *    精神科急性期治療病棟入院料算定開始
           03  FILLER              PIC X(11)   VALUE   "4 840000112".
           03  FILLER              PIC X(11)   VALUE   "5 840000612".
      *    前回歯科矯正診断料算定
           03  FILLER              PIC X(11)   VALUE   "4 840000022".
           03  FILLER              PIC X(11)   VALUE   "5 840000522".
      *    造血幹細胞移植実施日
           03  FILLER              PIC X(11)   VALUE   "4 840000133".
           03  FILLER              PIC X(11)   VALUE   "5 840000633".
      *    退　院
           03  FILLER              PIC X(11)   VALUE   "4 840000013".
           03  FILLER              PIC X(11)   VALUE   "5 840000513".
      *    対象調剤
           03  FILLER              PIC X(11)   VALUE   "4 840000093".
           03  FILLER              PIC X(11)   VALUE   "5 840000593".
      *    特定薬剤治療管理料初回算定
           03  FILLER              PIC X(11)   VALUE   "4 840000038".
           03  FILLER              PIC X(11)   VALUE   "5 840000538".
      *    動的処置開始
           03  FILLER              PIC X(11)   VALUE   "4 840000025".
           03  FILLER              PIC X(11)   VALUE   "5 840000525".
      *    動的処置算定
           03  FILLER              PIC X(11)   VALUE   "4 840000024".
           03  FILLER              PIC X(11)   VALUE   "5 840000524".
      *    入　院
           03  FILLER              PIC X(11)   VALUE   "4 840000088".
           03  FILLER              PIC X(11)   VALUE   "5 840000588".
      *    入院時一部負担金減額認定証
           03  FILLER              PIC X(11)   VALUE   "4 840000033".
           03  FILLER              PIC X(11)   VALUE   "5 840000533".
      *    文書等提供
           03  FILLER              PIC X(11)   VALUE   "4 840000111".
           03  FILLER              PIC X(11)   VALUE   "5 840000611".
      *    年月日
           03  FILLER              PIC X(11)   VALUE   "4 840000061".
           03  FILLER              PIC X(11)   VALUE   "5 840000561".
      *    年月
           03  FILLER              PIC X(11)   VALUE   "4 840000060".
           03  FILLER              PIC X(11)   VALUE   "5 840000560".
      *    補管算定
           03  FILLER              PIC X(11)   VALUE   "4 840000117".
           03  FILLER              PIC X(11)   VALUE   "5 840000617".
      *    マルチブラケット法開始
           03  FILLER              PIC X(11)   VALUE   "4 840000026".
           03  FILLER              PIC X(11)   VALUE   "5 840000526".
      *    療養費同意書交付
           03  FILLER              PIC X(11)   VALUE   "4 840000078".
           03  FILLER              PIC X(11)   VALUE   "5 840000578".
      *    連続携行式腹膜灌流導入期
           03  FILLER              PIC X(11)   VALUE   "4 840000008".
           03  FILLER              PIC X(11)   VALUE   "5 840000508".
      *R01.5.7
      *    初　療
           03  FILLER              PIC X(11)   VALUE   "4 840800003".
           03  FILLER              PIC X(11)   VALUE   "5 840800503".
      *
       01  TBL-GENGO-COM-AREA-R    REDEFINES   TBL-GENGO-COM-AREA.
           03  TBL-GENGO-COM-OCC   OCCURS   58
                                   INDEXED  IDX-GENGO.
      *        4:平成、5：令和
               05  TBL-GENGO-KBN       PIC X(01).
               05  TBL-YOBI1           PIC X(01).
               05  TBL-GENGO-SRYCD     PIC X(09).
      *
      *
