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
      *    病名組立サブ  COPY (CPORCSMKBYOMEI.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSMKBYOMEIAREA.
           03  SMKBYO-IN.
      *        疑い病名フラグ
               05  SMKBYO-UTAGAIFLG      PIC X(01).
      *        補足コメント
               05  SMKBYO-HOSOKU-COMMENT PIC X(80).
           03  SMKBYO-IO.
      *        病名
               05  SMKBYO-BYOMEI         PIC X(240).
      *        病名コード
               05  SMKBYO-BYOMEICD-G.
                   07  SMKBYO-BYOMEICD   PIC X(07)
                                         OCCURS   21.
      *        編集病名フラグ
               05  SMKBYO-BYOMEIHENFLG   PIC X(01).
      *
           03  SMKBYO-OT.
      *        長さ（バイト数で返却）
               05  SMKBYO-LENGTH         PIC 9(05).
      *        返却ステータス−更新状態
      *        （0:変更なし／1:変更あり）
               05  SMKBYO-STS-MODIFY     PIC 9(01).
      *        返却ステータス−疑い
      *        （0:変更なし／1:"の疑い"付加）
               05  SMKBYO-STS-UTAGAI     PIC 9(01).
      *        返却ステータス−急性
      *        （0:変更なし／1:"急性"付加）
               05  SMKBYO-STS-KYUSEI     PIC 9(01).
