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
      *  コピー句修正履歴
      * Maj/Min/Rev       修正者    日付          内容
      * 01.00.01          吉川      09/09/02      帳票様式の変更に伴う修正
      * 01.00.02          萩野      14/05/09      帳票様式の変更に伴う修正
      * 01.00.03          門間      18/12/04      修正履歴コメント削除
      *                                           合計件数を4桁へ増加
      ********************************************************************
       01  SC03.
      *印刷年月
         03  SC03-PRTYM            PIC  X(16).
      *請求年月日
         03  SC03-SEIYMD           PIC  X(22).
      *住所
         03  SC03-ADRS             PIC  X(100).
      *医療機関名
         03  SC03-HOSPNAME         PIC  X(100).
      *管理者名
         03  SC03-KAISETUNAME      PIC  X(60).
      *医療機関コード
         03  SC03-HOSPCD           PIC  X(07).
      *点数表区分
         03  SC3-TENSUKBN-TBL OCCURS 3.
             05  SC03-TENSUKBN     PIC X(02).
      *カナ氏名
         03  SC03-KANANAME-TBL  OCCURS 25.
             05  SC03-KANANAME     PIC  X(20).
      *氏名
         03  SC03-NAME-TBL   OCCURS  25.
             05  SC03-NAME         PIC  X(20).
      *生年月日
            03  SC03-BIRTHDAY-TBL    OCCURS 25 TIMES.
              05  SC03-BIRTHDAY-D      PIC X(2).
              05  SC03-BIRTHDAY-M      PIC X(2).
              05  SC03-BIRTHDAY-Y      PIC X(2).
      *保険種別１
         03  SC03-HKNKBN-TBL   OCCURS  25.
             05  SC03-HKNKBN-G OCCURS 6.
                 07  SC03-HKNKBN   PIC  X(02).
      *本人家族区分１
      *   03  SC03-HONKZKKBN1-TBL   OCCURS  20.
         03  SC03-HONKZKKBN1-TBL   OCCURS  25.
             05  SC03-HONKZKKBN    PIC  X(02).
      *公費負担者番号
         03  SC03-FTNJANUM1-TBL   OCCURS  25.
             05  SC03-FTNJANUM1    PIC  X(04).
      *公費負担者番号
         03  SC03-FTNJANUM2-TBL   OCCURS  25.
             05  SC03-FTNJANUM2    PIC  X(08).
      *受給者番号
         03  SC03-JKYSNUM-TBL   OCCURS  25.
             05  SC03-JKYSNUM      PIC  X(14).
      *診療年月
         03  SC03-SRYY-TBL   OCCURS  25.
             05  SC03-SRYYM        PIC  X(08).
      *入外区分１
         03  SC03-NYUGAIKBN1-TBL OCCURS  25.
             05  SC03-NYUGAIKBN1   PIC  X(02).
      *入外区分２
         03  SC03-NYUGAIKBN2-TBL OCCURS  25.
             05  SC03-NYUGAIKBN2   PIC  X(02).
      *保険総点数
         03  SC03-TENSU-TBL     OCCURS  25.
             05  SC03-TENSU        PIC  X(09).
      *薬剤負担文字
         03  SC03-YKZMOJI-TBL    OCCURS  25.
             05  SC03-YKZMOJI      PIC  X(02).
      *薬剤負担○
         03  SC03-YKZMARU-TBL    OCCURS  25.
             05  SC03-YKZMARU      PIC  X(02).
      *薬剤負担
         03  SC03-YKZFTN-TBL    OCCURS  25.
             05  SC03-YKZFTN       PIC  X(06).
      *結核・精神
         03  SC03-KSFTN-TBL    OCCURS  25.
             05  SC03-KSFTN        PIC  X(06).
      *その他
         03  SC03-ETCFTN-TBL    OCCURS  25.
             05  SC03-ETCFTN       PIC  X(06).
      *自己負担
         03  SC03-ITBFTN-TBL    OCCURS  25.
             05  SC03-ITBFTN       PIC  X(06).
      *精神文字
         03  SC03-SMOJI-TBL    OCCURS  25.
             05  SC03-SMOJI        PIC  X(02).
      *精神○
         03  SC03-SMARU-TBL    OCCURS  25.
             05  SC03-SMARU        PIC  X(02).
      *結核文字
         03  SC03-KMOJI-TBL    OCCURS  25.
             05  SC03-KMOJI        PIC  X(04).
      *結核○
         03  SC03-KMARU-TBL    OCCURS  25.
             05  SC03-KMARU        PIC  X(02).
      *合計
         03  SC03-GOKEI-TBL.
      *合計件数
             05  SC03-GKENSU       PIC  X(04).
      *合計点数
             05  SC03-GTENSU       PIC  X(09).
      *合計薬剤文字
             05  SC03-GYKZMOJI     PIC  X(02).
      *合計薬剤○
             05  SC03-GYKZMARU     PIC  X(02).
      *合計薬剤負担
             05  SC03-GYKZFTN      PIC  X(07).
      *合計結核・精神
             05  SC03-GKSFTN       PIC  X(07).
      *合計その他
             05  SC03-GETCFTN      PIC  X(07).
      *合計自己負担
             05  SC03-GITBFTN      PIC  X(07).
