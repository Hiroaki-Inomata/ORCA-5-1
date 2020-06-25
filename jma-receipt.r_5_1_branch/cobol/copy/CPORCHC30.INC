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
      *    薬剤情報出力パラメタ              COPY (CPORCHC30.INC)
      *                                          (ORCHC30 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  06/01/24  用量割合コード追加
      * 04.01.00     NACL-多々納  07/12/11  起床追加
      * 04.05.00     NACL-森脇    09/06/22  小数点以下５桁に変更
      * 04.05.00     NACL-多々納  09/07/23  在宅薬剤、コメント追加
      * 04.06.00     NACL-多々納  11/02/22  用法コメント対応追加
      * 04.08.00     NACL-多々納  16/10/XX  用法コード数量追加
      ************************************************************
      *
       01  ORCHC30AREA.
      *
      *    最大件数
           03  ORCHC30-MAX             PIC 9(04).
      *
           03  ORCHC30-DATA-TBL        OCCURS   99.
      *        薬剤コード
               05  ORCHC30-SRYCD       PIC X(09).
      *        用法コード（空白あり）
               05  ORCHC30-YSRYCD      PIC X(09).
      *        用法コメント名称
      *R02.4   05  ORCHC30-YSRYCD-NAME PIC X(80).
               05  ORCHC30-YSRYCD-NAME PIC X(140).
      *
      *        数量（１日量／外用は総量）
               05  ORCHC30-SURYO       PIC 9(05)V9(05).
      *        日数（回数）（外用は１回）
               05  ORCHC30-KAISU       PIC 9(03).
      *        （１：内服、２：頓服、３：外用、４：在宅）
               05  ORCHC30-ZAIKBN      PIC X(01).
      *        用法毎の用量（１：朝、２：昼、３：夜、４：寝、５：起床）
               05  ORCHC30-YOURYOU-TBL.
                   07  ORCHC30-YOURYOU     PIC 9(05)V9(05)
                                           OCCURS   5.
      *        外用日数（日数入力があった時のみ）
               05  ORCHC30-SRYKAISU        PIC 9(03).
      *        換算値数量（１日量）
               05  ORCHC30-KANSURYO        PIC 9(05)V9(05).
      *H28.10
      *        用法コードの数量
      *           （１：朝、２：昼、３：夜、４：寝、５：起床）
               05  ORCHC30-INPUTSURYOU-TBL.
                   07  ORCHC30-INPUTSURYOU     PIC 9(05)V9(05)
                                               OCCURS   5.
      *        用法コード２（表示のみ）
               05  ORCHC30-YOHOU-G.
                   07  ORCHC30-YOHOU-TBL   OCCURS  5.
                       09  ORCHC30-YOHOU       PIC X(09).
      *                用法コメント名称
      *R02.4           09  ORCHC30-YOHOU-NAME  PIC X(80).
                       09  ORCHC30-YOHOU-NAME  PIC X(140).
      *        コメントコード
               05  ORCHC30-COMMENT-G.
                   07  ORCHC30-COMMENT-OCCURS  OCCURS  10.
                       09  ORCHC30-COMMENT-CD   PIC X(09).
      *R02.4           09  ORCHC30-COMMENT-MEI  PIC X(80).
                       09  ORCHC30-COMMENT-MEI  PIC X(140).
      *H28.10
      *                1:用法コードの数量編集分
                       09  ORCHC30-COMHENKBN    PIC X(01).
      *
