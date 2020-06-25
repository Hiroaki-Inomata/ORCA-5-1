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
      ******************************************************************
      *    ＡＰＩレスポンスドクター情報一覧
      *                           COPY (CPPHYSICIANV2RES.INC)
      ******************************************************************
        01  XML-PHYSICIANRES.
            02  PHYSRES-CONTEXT            PIC S9(9)   BINARY.
            02  PHYSRES-OBJECT             PIC X(36).
            02  PHYSRES-MODE               PIC S9(9)   BINARY.
            02  PHYSRES-RECORDNAME         PIC X(128).
            02  PHYSRES-APPOINTRES.
      *       実施日
              03  PHYSRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  PHYSRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  PHYSRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  PHYSRES-API-RESULT-MSG      PIC X(100).
      *
              03  PHYSRES-RESKEY              PIC X(100).
      *       基準日
              03  PHYSRES-BASE-DATE           PIC X(10).
      *       ドクター一覧
              03  PHYSRES-PHYSICIAN-INFOMATION.
                  05  PHYSRES-PHYSICIAN-INFO       OCCURS   100.
      *                ドクターコード
                       07  PHYSRES-DRCD            PIC X(5).
      *                氏名
                       07  PHYSRES-NAME            PIC X(80).
      *                カナ氏名
                       07  PHYSRES-KANANAME        PIC X(80).
      *                医籍登録番号
                       07  PHYSRES-ISEKINUM        PIC  X(08).
      *                麻薬ライセンス
                       07  PHYSRES-MAYAKULICENSE   PIC  X(40).
      *                専門科コード
                       07  PHYSRES-SEMONKACD       OCCURS  5.
                           09  PHYSRES-SEMONKACD-X    PIC  X(02).
      *
