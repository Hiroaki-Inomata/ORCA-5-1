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
      *    入院調剤料一括更新パラメタ　　COPY (CPORCSCNYUHEN.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCNYUHENAREA.
      *
      *入力
           03  ORCSCNYUHEN-IN-AREA.
      *処理区分
               05  ORCSCNYUHEN-KBN                 PIC  X(01).
      *        診療日（必須）
               05  ORCSCNYUHEN-SRYYMD              PIC  X(08).
      *処理区分
               05  ORCSCNYUHEN-KBN2                PIC  X(01).
      *出力
           03  ORCSCNYUHEN-OUT-AREA.
      *        １：入院あり、０：入院なし
               05  ORCSCNYUHEN-RRK-NYUIN           PIC 9(01).
      *        １：入院中、２：当月に入院あり
               05  ORCSCNYUHEN-NYUIN-KBN           PIC 9(01).
      *        診療日の入院情報
               05  ORCSCNYUHEN-NYUINYMD            PIC X(08).
               05  ORCSCNYUHEN-TAIINYMD            PIC X(08).
               05  ORCSCNYUHEN-NYUINKA             PIC X(02).
               05  ORCSCNYUHEN-HKNCOMBI            PIC X(04).
               05  ORCSCNYUHEN-DRCD                PIC X(05).
      *        診療月の入院日
               05  ORCSCNYUHEN-NYUIN-DAYG.
                   07  ORCSCNYUHEN-NYUDAY          PIC 9(01)
                                                   OCCURS   31.
      *        診療日以前の最終退院日
               05  ORCSCNYUHEN-MAE-TAIINYMD        PIC X(08).
      *        最終退院日
               05  ORCSCNYUHEN-LAST-TAIINYMD       PIC X(08).
      *        他医院退院日
               05  ORCSCNYUHEN-TAI-TAIINYMD        PIC X(08).
      *        同一再入院有無
               05  ORCSCNYUHEN-DOUNYUKBN           PIC 9(01).
               05  ORCSCNYUHEN-DOUNYUIN            PIC 9(02).
      *        継続初回入院日
               05  ORCSCNYUHEN-STR-NYUINYMD        PIC X(08).
      *        同一再入院有無（退院日）
               05  ORCSCNYUHEN-DOUTAIKBN           PIC 9(01).
      *
