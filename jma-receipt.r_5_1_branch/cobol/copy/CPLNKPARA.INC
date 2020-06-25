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
      *    画面遷移用パラメタ　  COPY  (CPLNKPARA.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  LNK-PARA-AREA.
           03  LNKPAR-KEY.
      *端末ＩＤ
               05  LNKPAR-TANMATID       PIC X(15).
      *
           03  LNKPAR-KYOUTU.
      *医療機関コード
               05  LNKPAR-HOSPID          PIC X(24).
      *システム日付（西暦）
               05  LNKPAR-SYSDAY          PIC 9(08).
      *システム日付（和暦編集済）
               05  LNKPAR-SYSDAYH         PIC X(09).
      *元プログラム
               05  LNKPAR-MOTOPG          PIC X(08).
      *先プログラム
               05  LNKPAR-SAKIPG          PIC X(08).
      *患者番号
               05  LNKPAR-PTNUM           PIC X(20).
      *患者ＩＤ
               05  LNKPAR-PTID            PIC 9(10).
      *診療日
               05  LNKPAR-SRYDAY          PIC X(08).
      *診療日（和暦編集済）
               05  LNKPAR-SRYDAYW         PIC X(09).
      *入外区分    
               05  LNKPAR-NYUGAI          PIC X(01).
      *患者氏名かな
               05  LNKPAR-KANANAME        PIC X(50).
      *患者氏名
               05  LNKPAR-NAME            PIC X(50).
      *患者性別
               05  LNKPAR-SEX             PIC X(02).
      *患者生年月日
               05  LNKPAR-BIRTHDAY        PIC X(08).
      *患者生年月日（和暦編集済）
               05  LNKPAR-BIRTHDAYW       PIC X(09).
      *診療科
               05  LNKPAR-SRYKA           PIC X(02).
      *診療科名
               05  LNKPAR-SRYKAMEI        PIC X(10).
      *保険組合番号
               05  LNKPAR-KUMINO          PIC X(04).
      *保険組合せ内容
               05  LNKPAR-KUMIMEI         PIC X(36).
      *負担率
               05  LNKPAR-RITU            PIC 9(03).
      *負担率名称
               05  LNKPAR-RITUMEI         PIC X(10).
      *受付時間
               05  LNKPAR-UKETIME         PIC X(06).
      *元の元プログラム
               05  LNKPAR-MOTOPG2         PIC X(08).
      *
