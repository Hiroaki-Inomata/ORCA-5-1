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
      *    受診履歴　    　　　 COPY  (CPJYURRK.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/10  労災追加
      *  01.00.02    NACL-多々納  02/10/21  入外区分追加
      *  01.00.03    NACL-小豆沢  04/07/01  最終区分削除
      *                                     まとめ入力伝票番号追加
      *                                     まとめ入力連番追加
      ************************************************************
      *01  JYURRK-REC.
      *受診履歴
           03  JYURRK-KEY.
      *  医療機関ＩＤ
               05  JYURRK-HOSPNUM           PIC 9(2).
      *  患者ＩＤ
               05  JYURRK-PTID             PIC  9(10).
      *  入外区分
               05  JYURRK-NYUGAIKBN        PIC  X(01).
      *  診療科
               05  JYURRK-SRYKA            PIC  X(02).
      *  診療日
               05  JYURRK-SRYYMD           PIC  X(08).
      *  連番（１からの同日連番）
               05  JYURRK-RENNUM           PIC  9(01).
      *  同時診療連番（０：継続なし、１からの連番：継続あり）
               05  JYURRK-DOUJI-RENNUM     PIC  9(01).
      *  会計連番（会計情報修正時の連番）
               05  JYURRK-KAIKEI-RENNUM    PIC  9(03).
      *  枝番（１診療１５剤以上の時に１からの連番）
               05  JYURRK-EDANUM           PIC  9(01).
      *ドクター
           03  JYURRK-DRCD                 PIC X(05).
      *保険組合番号
           03  JYURRK-HKNCOMBINUM          PIC X(04).
      *保険区分（０：一般、１：労災、２：自賠責、３：歯科（未使用）
      *          ４：自賠責（健保準拠）、５：公務災害（健保準拠）、
      *          ６：公害）
           03  JYURRK-HKNKBN               PIC X(01).
      *伝票番号
           03  JYURRK-DENPNUM              PIC 9(07).
      *まとめ入力伝票番号
           03  JYURRK-GRP-DENPNUM          PIC 9(07).
      *まとめ入力連番
           03  JYURRK-GRP-RENNUM           PIC 9(02).
      *収納更新区分
           03  JYURRK-SYU-UPDKBN           PIC  X(01).
      *診療区分
           03  JYURRK-SRYKBN-G.
      *  診療区分１
             05  JYURRK-SRYKBN1            PIC  X(02).
      *  診療区分２
             05  JYURRK-SRYKBN2            PIC  X(02).
      *  診療区分３
             05  JYURRK-SRYKBN3            PIC  X(02).
      *  診療区分４
             05  JYURRK-SRYKBN4            PIC  X(02).
      *  診療区分５
             05  JYURRK-SRYKBN5            PIC  X(02).
      *  診療区分６
             05  JYURRK-SRYKBN6            PIC  X(02).
      *  診療区分７
             05  JYURRK-SRYKBN7            PIC  X(02).
      *  診療区分８
             05  JYURRK-SRYKBN8            PIC  X(02).
      *  診療区分９
             05  JYURRK-SRYKBN9            PIC  X(02).
      *  診療区分１０
             05  JYURRK-SRYKBN10           PIC  X(02).
      *  診療区分１１
             05  JYURRK-SRYKBN11           PIC  X(02).
      *
           03  JYURRK-SRYKBN-R         REDEFINES   JYURRK-SRYKBN-G.
      * 診療区分
             05  JYURRK-SRYKBN             PIC  X(02)
                                           OCCURS   11.
      *剤番号
           03  JYURRK-ZAINUM-G.
      *  剤番号１
             05  JYURRK-ZAINUM1            PIC  9(08).
      *  剤番号２
             05  JYURRK-ZAINUM2            PIC  9(08).
      *  剤番号３
             05  JYURRK-ZAINUM3            PIC  9(08).
      *  剤番号４
             05  JYURRK-ZAINUM4            PIC  9(08).
      *  剤番号５
             05  JYURRK-ZAINUM5            PIC  9(08).
      *  剤番号６
             05  JYURRK-ZAINUM6            PIC  9(08).
      *  剤番号７
             05  JYURRK-ZAINUM7            PIC  9(08).
      *  剤番号８
             05  JYURRK-ZAINUM8            PIC  9(08).
      *  剤番号９
             05  JYURRK-ZAINUM9            PIC  9(08).
      *  剤番号１０
             05  JYURRK-ZAINUM10           PIC  9(08).
      *  剤番号１１
             05  JYURRK-ZAINUM11           PIC  9(08).
      *  剤番号１２
             05  JYURRK-ZAINUM12           PIC  9(08).
      *  剤番号１３
             05  JYURRK-ZAINUM13           PIC  9(08).
      *  剤番号１４
             05  JYURRK-ZAINUM14           PIC  9(08).
      *  剤番号１５
             05  JYURRK-ZAINUM15           PIC  9(08).
      *
           03  JYURRK-ZAINUM-R         REDEFINES   JYURRK-ZAINUM-G.
      *  剤番号
             05  JYURRK-ZAINUM             PIC  9(08)
                                           OCCURS 15.
      *端末ＩＤ
           03  JYURRK-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  JYURRK-OPID                 PIC  X(16).
      *作成年月日
           03  JYURRK-CREYMD               PIC  X(08).
      *更新年月日
           03  JYURRK-UPYMD                PIC  X(08).
      *更新時間
           03  JYURRK-UPHMS                PIC  X(06).
      *　　主キー更新用項目
           03  JYURRK-UPKEY.
               05  JYURRK-UPHOSPNUM           PIC 9(2).
               05  JYURRK-UPPTID             PIC  9(10).
               05  JYURRK-UNYUGAIKBN         PIC  X(01).
               05  JYURRK-UPSRYKA            PIC  X(02).
               05  JYURRK-UPSRYYMD           PIC  X(08).
               05  JYURRK-UPRENNUM           PIC  9(01).
               05  JYURRK-UPDOUJI-RENNUM     PIC  9(01).
               05  JYURRK-UPKAIKEI-RENNUM    PIC  9(03).
               05  JYURRK-UPEDANUM           PIC  9(01).
