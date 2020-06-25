      ******************************************************************
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
      ******************************************************************
      *    リハビリ開始日コメント編集サブ   COPY (CPORCSRIHASTDAY.INC)
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
       01  ORCSRIHASTDAYAREA.
      *
      *医療機関ＩＤ
           03  LNK-RIHASTDAY-HOSPNUM              PIC  9(02).
      *入外区分
           03  LNK-RIHASTDAY-NYUGAIKBN            PIC  X(01).
      *患者ＩＤ
           03  LNK-RIHASTDAY-PTID                 PIC  9(10).
      *診療科
           03  LNK-RIHASTDAY-SRYKA                PIC  X(02).
      *診療年月
           03  LNK-RIHASTDAY-SRYYM                PIC  X(06).
      *保険組合せ
           03  LNK-RIHASTDAY-HKNCOMBI             PIC  9(04).
      *
      *返却項目
           03  LNK-RIHASTDAY-OUT-AREA.
              04  LNK-RIHASTDAY-OUT-DAT.
      *           １：心大血管疾患
      *           ２：心大血管疾患の早期・初期リハ加算
      *           ３：脳血管疾患等
      *           ４：廃用症候群
      *           ５：廃用症候群の早期・初期リハ加算
      *           ６：運動器
      *           ７：呼吸器
      *           ８：呼吸器の早期・初期リハ加算
      *           ９：摂食
      *         １０：難病患者
      *         １１：障害児（者）
                 05  LNK-RIHASTDAY-OUT-OCC          OCCURS  11.
                   06  LNK-RIHASTDAY-OUT-OCC2       OCCURS 100.
                      07  LNK-RIHASTDAY-MARK        PIC  X(01).
                      07  LNK-RIHASTDAY-YUKOKETA    PIC  9(03).
                      07  LNK-RIHASTDAY-COM         PIC  X(200).
      *            該当月に入力があったリハビリ診療行為コード
                   06  LNK-RIHASTDAY-OUT-OCC3.
                      07  LNK-RIHASTDAY-SRYCD       PIC  X(09)
                                                  OCCURS  50.
      *            リハビリ開始日コメントの有効インデックス
                   06  LNK-RIHASTDAY-IDX            PIC  9(03).
      *       リハビリシステム予約コード一覧 (LNK-RIHASTDAY-HOSPNUM = ZERO の時返却)
              04  LNK-RIHASTDAY-SYSCDLST          PIC  X(09)
                                                  OCCURS  50.
              04  LNK-RIHASTDAY-SYSCDLST-IDX      PIC  9(03).
      *       リハビリコード一覧 (LNK-RIHASTDAY-HOSPNUM = ZERO の時返却)
              04  LNK-RIHASTDAY-SRYCDLST          PIC  X(09)
                                                  OCCURS  300.
              04  LNK-RIHASTDAY-SRYCDLST-IDX      PIC  9(03).
      *
      *復帰コード
           03  LNK-RIHASTDAY-RC                   PIC  9(02).
      *
