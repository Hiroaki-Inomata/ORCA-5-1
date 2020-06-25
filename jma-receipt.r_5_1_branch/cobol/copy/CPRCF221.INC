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
      *    公害レセプト（入院）・点数情報   COPY (CPRCF221.INC) 
      *                       診療科違いの同一剤のまとめ用         
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.07.01    NACL-門脇    13/01/22  データ領域拡張
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-門脇    16/08/23  持参薬対応
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE221-REC.
           03  RECE221-KEY.
               05  RECE221-KEY1.
      *医療機関番号
                   07  RECE221-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE221-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE221-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE221-PTID        PIC 9(10).
      *レセプト診療科
                   07  RECE221-RECEKA      PIC X(02).
      *公費ＩＤ
                   07  RECE221-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE221-TEKSTYMD    PIC X(08). 
      *診療区分
               05  RECE221-SRYKBN          PIC X(02).
      *診療種別区分
               05  RECE221-SRYSYUKBN       PIC X(03).
      *剤点数
               05  RECE221-ZAITEN          PIC 9(08).
      *診療コード計
               05  RECE221-SRYCDTOTAL      PIC 9(14).
      *数量計
               05  RECE221-SURYOUTOTAL     PIC 9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE221-RUIKEITOTAL     PIC 9(13)V9(05).
      *明細数
               05  RECE221-MEISAISU        PIC 9(04).
      *自保区分
               05  RECE221-JIHOKBN         PIC X(01).
      *持参薬区分
               05  RECE221-JISANYAKKBN     PIC X(01).
      *コメント剤連番
               05  RECE221-COMRENNUM       PIC 9(05).
      *剤回数
           03  RECE221-ZAIKAISU            PIC 9(04).
      *日数回数テーブル
           03  RECE221-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
               05  RECE221-DAY-TBL         OCCURS  4.
                   07  RECE221-KAISUINF    OCCURS  31.
                       09  RECE221-KAISU   PIC 9(03).
      *データ
           03  RECE221-REC1                PIC X(1000).
      *コメントデータ
           03  RECE221-COM-AREA.
               05  RECE221-COM             PIC X(300)  OCCURS  10.
      *初診（同日複数診療科）算定科
           03  RECE221-SYODOU-SRYKA-AREA.
               05  RECE221-SYODOU-SRYKA    PIC X(02)   OCCURS  10.
