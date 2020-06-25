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
      *    保険組合せ編集パラメタ　　COPY (CPORCSHKNSET.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  05/08/17  保険者番号、公費ＩＤ返却追加
      * 04.01.00     NACL-多々納  07/11/12  処理区分３（非表示）追加
      * 05.00.00     NACL-多々納  19/02/08  処理区分４ 件数２００追加
      ************************************************************
       01  ORCSHKNSETAREA.
      *
      *入力
           03  ORCSHKN-IN-AREA.
      *処理区分（１：当日対象、２：全部）
               05  ORCSHKN-KBN                 PIC  X(01).
      *処理区分２（１：包括まとめ入力有り）
               05  ORCSHKN-KBN2                PIC  X(01).
      *処理区分３（１：非表示分有り）
               05  ORCSHKN-KBN3                PIC  X(01).
      *処理区分４（１：MAX 200件 ）
               05  ORCSHKN-KBN4                PIC  X(01).
      *
      *保険組合せ一覧
           03  ORCSHKN-OUT-AREA.
      *        最大件数
               05  ORCSHKN-HKN-MAX             PIC 9(04).
      *        初期表示位置
               05  ORCSHKN-IDX                 PIC 9(04).
      *
      *        一覧画面表示
               05  ORCSHKN-GMN-HKNCOMBI-LISTG.
                   07  ORCSHKN-GMN-HKN-LIST    OCCURS  200.
      *                保険組合番号
                       09  ORCSHKN-GMN-HKNCOMBINUM     PIC  X(04).
      *                保険名称（労災の部位は１２文字まで）
                       09  ORCSHKN-GMN-HKNCOMBIMEI     PIC  X(50).
      *                負担割合（ZZZ%）
                       09  ORCSHKN-GMN-FTNRATEMEI      PIC  X(05).
      *                適用期間
                       09  ORCSHKN-GMN-TEKSTYMD        PIC  X(09).
                       09  ORCSHKN-GMN-TEKEDYMD        PIC  X(09).
      *                内部表示
      *                適用期間
                       09  ORCSHKN-NAI-TEKSTYMD        PIC  X(08).
                       09  ORCSHKN-NAI-TEKEDYMD        PIC  X(08).
      *                負担割合
                       09  ORCSHKN-NAI-FTNRATE         PIC  9(03).
      *                保険ＩＤ
                       09  ORCSHKN-NAI-HKNID           PIC  9(10).
      *                保険種別
                       09  ORCSHKN-NAI-HKNNUM          PIC  X(03).
      *                補助区分
                       09  ORCSHKN-NAI-HOJOKBN         PIC  X(01).
      *                本人家族区分
                       09  ORCSHKN-NAI-HONKZKKBN       PIC  X(01).
      *                保険区分
                       09  ORCSHKN-NAI-HKNKBN          PIC  9(01).
      *                保険者番号
                       09  ORCSHKN-NAI-HKNJANUM        PIC  X(08).
      *                労災・四肢区分
                       09  ORCSHKN-NAI-RSI-SISIKBN     PIC  X(01).
      *                労災・傷病年月日
                       09  ORCSHKN-NAI-RSI-SHOBYOYMD   PIC  X(08).
      *                労災・保険区分
                       09  ORCSHKN-NAI-RSI-HKNKBN      PIC  X(01).
      *                特定疾患の受給者番号
                       09  ORCSHKN-NAI-JKYSNUM         PIC  X(20).
      *                治験区分
                       09  ORCSHKN-NAI-CHIKENKBN       PIC  X(01).
      *                公費種類・公費ＩＤ
                       09  ORCSHKN-NAI-KOH-G.
                           11  ORCSHKN-NAI-KOH1HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH1ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH2HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH2ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH3HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH3ID          PIC  9(10).
                           11  ORCSHKN-NAI-KOH4HKNNUM      PIC  X(03).
                           11  ORCSHKN-NAI-KOH4ID          PIC  9(10).
                       09  ORCSHKN-NAI-KOH-R       REDEFINES
                                                   ORCSHKN-NAI-KOH-G.
                           11  ORCSHKN-NAI-KOH-OCC     OCCURS   4.
                               13  ORCSHKN-NAI-KOHHKNNUM   PIC  X(03).
                               13  ORCSHKN-NAI-KOHID       PIC  9(10).
      *                非表示区分
                       09  ORCSHKN-NAI-HYOJIKBN        PIC  X(01).
      *エラーコード
           03  ORCSHKN-ERRCD                    PIC 9(01).
      *
