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
      *    チェックマスタ２       COPY (CPCHKSND.INC)                   *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  CHKSND-REC.
           03  CHKSND-KEY.
               05  CHKSND-HOSPNUM        PIC 9(2).
      *チェック区分
               05  CHKSND-CHKKBN         PIC  X(01).
      *診療コード
               05  CHKSND-SRYCD          PIC  X(09).
      *有効開始年月日
               05  CHKSND-YUKOSTYMD      PIC  X(08).
      *有効終了年月日
               05  CHKSND-YUKOEDYMD      PIC  X(08).
      *コード区分
               05  CHKSND-CDKBN          PIC  X(01).
      *連番号
               05  CHKSND-RENNUM         PIC  9(05).
      *適用コード
           03  CHKSND-CD                 PIC  X(170).
      *病名
           03  CHKSND-BYOMEI             PIC  X(160).
      *条件区分
           03  CHKSND-COMPKBN            PIC  X(1).
      *疑い区分
           03  CHKSND-UTAGAIKBN          PIC  X(1).
      *病名編集フラグ
           03  CHKSND-BYOMEIHENFLG       PIC  X(1).
      *入外区分
           03  CHKSND-NYUGAIKBN          PIC  X(1).
      *グループチェック区分
           03  CHKSND-GRPCHKKBN          PIC  X(1).
      *グループＩＤ
           03  CHKSND-GRPID              PIC  X(1).
      *補足コメント
           03  CHKSND-HOSOKU             PIC  X(40).
      *補足コメント条件区分
           03  CHKSND-HOSOKU-COMPKBN     PIC  X(1).
      *適応病名チェック区分
           03  CHKSND-TEKBYOKBN          PIC  X(1).
      *端末ＩＤ
           03  CHKSND-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  CHKSND-OPID               PIC  X(16).
      *作成年月日
           03  CHKSND-CREYMD             PIC  X(08).
      *更新年月日
           03  CHKSND-UPYMD              PIC  X(08).
      *更新時間 
           03  CHKSND-UPHMS              PIC  X(06).
      *主キー更新用項目
           03  CHKSND-UPKEY.
               05  CHKSND-UPCHKSNDKBN      PIC  X(01).
               05  CHKSND-UPSRYCD        PIC  X(09).
               05  CHKSND-UPYUKOSTYMD    PIC  X(08).
               05  CHKSND-UPYUKOEDYMD    PIC  X(08).
               05  CHKSND-UPCDKBN        PIC  X(01).
               05  CHKSND-UPRENNUM       PIC  9(03).
      *
      *検索用項目
           03  CHKSND-SELNAME            PIC  X(200).
           03  CHKSND-SELCDSYU           PIC X(1).
           03  CHKSND-SELINPUTCD         PIC X(20).
