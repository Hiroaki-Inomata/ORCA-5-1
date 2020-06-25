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
      *----------------------------------------------------------*
      *    向精神薬多剤投与サブルーチンパラメタ
      *                                   COPY (CPORCSPSYCHO.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSPSYCHOAREA.
      *    診療日付
           03  ORCSPSYCHO-SRYYMD       PIC X(08).
      *    医薬品テーブル
           03  ORCSPSYCHO-MEDICINE-TBL.
               05  ORCSPSYCHO-MEDICINE-OCC     OCCURS   50.
      *            薬剤コード
                   07  ORCSPSYCHO-SRYCD        PIC X(09).
      *            薬価基準コード
                   07  ORCSPSYCHO-YAKKAKJNCD   PIC X(12).
      *            診療種別
                   07  ORCSPSYCHO-SRYSYUKBN    PIC X(03).
      *            回数
                   07  ORCSPSYCHO-KAISU        PIC 9(03).
      *    返却
           03  ORCSPSYCHO-OUT-AREA.
      *        薬剤毎の返却
               05  ORCSPSYCHO-MED-OUT-OCC      OCCURS   50.
      *            薬剤の効能区分
                   07  ORCSPSYCHO-FFECT-KBN    PIC 9(01).
      *            種類区分
                   07  ORCSPSYCHO-GROUP-KBN    PIC X(07).
      *            臨時投薬区分
                   07  ORCSPSYCHO-RINJI-KBN    PIC X(01).
      *        向精神薬有
               05  ORCSPSYCHO-FFECT-FLG    PIC 9(01).
      *        多剤投与結果（効能件数）
      *          （1:抗不安薬、2:睡眠薬、3:抗うつ薬、4:抗精神病薬）
               05  ORCSPSYCHO-COUNT        PIC 9(02)   OCCURS   4.
      *        多剤投与有無（1:多剤投与に該当する)
               05  ORCSPSYCHO-TAZAI        PIC 9(01).
      *
