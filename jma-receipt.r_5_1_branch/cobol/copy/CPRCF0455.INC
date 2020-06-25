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
      *    レセプト明細書     COPY (CPRCF0455.INC)               *
      *                       労災レセ電の関連コメント（外来）   *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE455-REC.
           03  RECE455-KEY.
             05  RECE455-KEY1.
      *医療機関ＩＤ
               07  RECE455-HOSPNUM         PIC 9(02).
      *診療年月
               07  RECE455-SRYYM           PIC 9(06).
      *入外区分
               07  RECE455-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               07  RECE455-PTID            PIC 9(10).
      *レセプト科
               07  RECE455-RECEKA          PIC X(02).
      *保険ＩＤ        
               07  RECE455-HKNID           PIC 9(10).
      *適用開始年月日
               07  RECE455-TEKSTYMD        PIC X(08). 
      *剤番号
               07  RECE455-ZAINUM          PIC 9(08).
      *連番号
               07  RECE455-RENNUM          PIC 9(02).
      *
             05  RECE455-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE455-SEQNO             PIC 9(02).
      *コメント情報
           03  RECE455-SRYACTINF.
      *診療コード
               05  RECE455-SRYCD           PIC X(09).
      *漢字名称
               05  RECE455-NAME            PIC X(300).
      *入力コメント
               05  RECE455-INPUTCOMENT     PIC X(140).
