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
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 指定病名・診療行為患者一覧
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/03/07  新規作成
      *****************************************************************
       01  TA05.
      *    診療対象開始日
           03  TA05-SRYSTYMD          PIC X(22).
      *    診療対象終了日
           03  TA05-SRYEDMD           PIC X(12).
      *    入外区分
           03  TA05-NYUGAIKBN         PIC X(10).
      *    頁数
           03  TA05-PAGE              PIC X(03).
      *    合計頁数
           03  TA05-GPAGE             PIC X(03).
      *    発行日
           03  TA05-PRTYMD            PIC X(22).
      *    検索対象診療行為・病名
           03  TA05-SEARCHNAME        PIC X(160).
      *
           03  TA05-NAIYO-TBL         OCCURS  22 TIMES.
      *    連番
             05  TA05-RENNUM          PIC X(03).
      *    患者番号
             05  TA05-PTNUM           PIC X(10).
      *    患者番号(11桁以上)
             05  TA05-PTNUM2          PIC X(20).
      *    カナ氏名
             05  TA05-KANA-NAME       PIC X(30).
      *    氏名
             05  TA05-NAME            PIC X(22).
      *    性別
             05  TA05-SEX             PIC X(02).
      *    年齢
             05  TA05-AGE             PIC X(03).
      *    病名
             05  TA05-BYOMEI          PIC X(22).
      *    病名(12文字以上)
             05  TA05-BYOMEI2         PIC X(44).
      *    病名開始日
             05  TA05-BYOSTYMD        PIC X(18).
      *    転帰日
             05  TA05-TENKIYMD        PIC X(18).
      *    転帰
             05  TA05-TENKI           PIC X(04).
      *    診療行為名
             05  TA05-SRYNAME         PIC X(22).
      *    診療行為名(12文字以上)
             05  TA05-SRYNAME2        PIC X(44).
      *    最終来院日
             05  TA05-LASTYMD         PIC X(18).
