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
      *    お薬手帳（入院）パラメタ作成パラ   COPY (CPORCHCN62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-多々納  09/10/07  換算値数量追加
      * 04.06.00     NACL-多々納  10/03/09  用量追加
      * 04.07.00     NACL-多々納  12/09/0X  商品名コード追加
      * 04.07.00     NACL-多々納  13/07/XX  ＱＲ、ＣＳＶ区分追加
      * 05.00.00     NACL-多々納  17/05/16  病棟番号追加
      ************************************************************
      *
       01  ORCHCN62AREA.
      *    診療日
           03  ORCHCN62-SRYYMD          PIC X(08).
      *    処理回数
           03  ORCHCN62-CNT             PIC 9(03).
      *    ドクター
           03  ORCHCN62-DRCD            PIC X(05).
      *    病棟
           03  ORCHCN62-BTUNAME         PIC X(20).
      *    病室
           03  ORCHCN62-ROOMNO          PIC X(06).
      *H29.5
      *    病棟番号
           03  ORCHCN62-BTUNUM          PIC X(02).
      *    処方内容
           03  ORCHCN62-DATA-REC.
      *        最大件数
               05  ORCHCN62-ZAI-MAX         PIC 9(03).
      *        剤テーブル
               05  ORCHCN62-REC-TBL         OCCURS   100.
      *            剤回数
                   07  ORCHCN62-ZAIKAIKEI       PIC 9(03).
      *            診療区分
                   07  ORCHCN62-SRYKBN          PIC X(02).
      *            診療種別
                   07  ORCHCN62-SRYSYUKBN       PIC X(03).
      *            剤番号
                   07  ORCHCN62-ZAINUM          PIC  9(08).
      *            剤内容
                   07  ORCHCN62-NAI-TBL         OCCURS   50.
      *                コード
                       09  ORCHCN62-SRYCD           PIC  X(09).
      *                数量
                       09  ORCHCN62-SRYSURYO        PIC  9(05)V9(05).
      *                回数（外用薬）
                       09  ORCHCN62-SRYKAISU        PIC  9(03).
      *                コメント名称
      *R02.4           09  ORCHCN62-INPUTCOMENT     PIC  X(80).
                       09  ORCHCN62-INPUTCOMENT     PIC  X(140).
      *                商品名コード
                       09  ORCHCN62-SHO-SRYCD       PIC  X(09).
      *                換算値数量
                       09  ORCHCN62-KANSURYO        PIC  9(05)V9(05).
      *                用量（１：朝、２：昼、３：夜、４：寝、５：起床）
                       09  ORCHCN62-YOURYOU-TBL.
                           11  ORCHCN62-YOURYOU     PIC 9(05)V9(05)
                                                   OCCURS   5.
      *            用法コード（空白あり）
                   07  ORCHCN62-YSRYCD          PIC X(09).
      *
      *    ＱＲ出力区分
           03  ORCHCN62-QRKBN          PIC X(01).
      *    ＣＳＶ出力区分
           03  ORCHCN62-CSVKBN         PIC X(01).
      *    ＣＳＶ出力のみ区分
           03  ORCHCN62-PRTKBN         PIC X(01).
      *
      *
