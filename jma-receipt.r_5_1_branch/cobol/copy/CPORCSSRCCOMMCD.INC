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
      *****************************************************************
      *    コメントコード検索サブ  COPY (CPORCSSRCCOMMCD.INC)          *
      *****************************************************************
      *****************************************************************
      * コピー句修正履歴
      * Maj/Min/Rev    修正者      日付        内容
      *****************************************************************
       01  ORCSSRCCOMMCD-AREA.
      *
      *引数
      *    コメント種類（ 1:服用方法 2:画像診断部位 8:コメント )
           03  SRCCOM-COMMENT-KBN  PIC 9(01).
      *    マスタ日付
           03  SRCCOM-MSTYMD       PIC X(08).
      *    コメント内容
           03  SRCCOM-COMMENT      PIC X(100).
      *
      *戻り値
      *    コメントコード
           03  SRCCOM-COMMENT-CODE PIC X(09).
      *    復帰コード ( 0:該当コードあり 1:該当コードなし 2:コメント種類不正 )
           03  SRCCOM-RC           PIC 9(01).
