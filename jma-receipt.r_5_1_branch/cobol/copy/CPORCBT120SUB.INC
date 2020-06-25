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
      *    日次統計用入院情報取得パラメタ　COPY (CPORCBT120SUB.INC)
      *                                使用プログラム (ORCBT120SUB.CBL)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月日
      *      診療科
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      ************************************************************
       01  ORCBT120SUB-AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCBT120SUB-HOSPNUM                 PIC  9(02).
      *患者ＩＤ
           03  LNK-ORCBT120SUB-PTID                    PIC  9(10).
      *算定年月日
           03  LNK-ORCBT120SUB-SANTEIYMD               PIC  X(08).
      *診療科（００：全科で固定セット）
           03  LNK-ORCBT120SUB-SRYKA                   PIC  X(02).
      *入院履歴番号
           03  LNK-ORCBT120SUB-RRKNUM                  PIC  9(03).
      *情報返却項目
           03  LNK-ORCBT120SUB-HENKYAKU-AREA.
      *医師コード
               05  LNK-ORCBT120SUB-DRCD                PIC  X(05).
      *入退院区分（１：当日入院　２：当日退院　３：入院中）
               05  LNK-ORCBT120SUB-NYUIN-KBN           PIC  9(01).
      *病棟番号
               05  LNK-ORCBT120SUB-BTUNUM              PIC  X(02).
      *病室番号
               05  LNK-ORCBT120SUB-BRMNUM              PIC  X(06).
      *前日病棟番号
               05  LNK-ORCBT120SUB-MAE-BTUNUM          PIC  X(02).
      *前日病室番号
               05  LNK-ORCBT120SUB-MAE-BRMNUM          PIC  X(06).
      *外泊区分
               05  LNK-ORCBT120SUB-GAIHAKU-KBN         PIC  9(01).
      *食事区分
               05  LNK-ORCBT120SUB-SKJ-KBN             PIC  9(01).
      *退院事由
               05  LNK-ORCBT120SUB-TAIINCD             PIC  X(02).
      *入院科
               05  LNK-ORCBT120SUB-NYUINKA             PIC  X(02).
      *レセ電入院科
               05  LNK-ORCBT120SUB-RECENYUINKA         PIC  X(02).
      *前日入院科
               05  LNK-ORCBT120SUB-MAE-NYUINKA         PIC  X(02).
      *前日レセ電入院科
               05  LNK-ORCBT120SUB-MAE-RECENYUINKA     PIC  X(02).
      *入院保険組合せ番号
               05  LNK-ORCBT120SUB-NYU-HKNCOMBINUM     PIC  9(04).
      *収納情報返却エリア
               05  LNK-ORCBT120SUB-SYU-AREA            OCCURS 10.
      *伝票番号
                   07  LNK-ORCBT120SUB-DENPNUM         PIC  9(07).
      *伝票状態区分
                   07  LNK-ORCBT120SUB-DENPJTIKBN      PIC  X(01).
      *伝票作成区分
                   07  LNK-ORCBT120SUB-CREATEKBN       PIC  X(01).
      *保険点数
                   07  LNK-ORCBT120SUB-HKNTEN          PIC  9(07).
      *保険負担金額
                   07  LNK-ORCBT120SUB-HKNMONEY        PIC  9(07).
      *保険適用外金額
                   07  LNK-ORCBT120SUB-TGMONEY         PIC  9(07).
      *保険適用外金税額
                   07  LNK-ORCBT120SUB-TGMONEY-TAX     PIC  9(07).
      *自費金額
                   07  LNK-ORCBT120SUB-JIHI            PIC  9(07).
      *自費税額
                   07  LNK-ORCBT120SUB-JIHI-TAX        PIC  9(07).
      *食事負担金額
                   07  LNK-ORCBT120SUB-SKJ             PIC  9(07).
      *消費税額
                   07  LNK-ORCBT120SUB-SKJ-TAX         PIC  9(07).
      *室料差額
                   07  LNK-ORCBT120SUB-RMSAGAKU        PIC  9(07).
      *室料差額税額
                   07  LNK-ORCBT120SUB-RMSAGAKU-TAX    PIC  9(07).
      *請求金額
                   07  LNK-ORCBT120SUB-SKYMONEY        PIC  9(07).
      *レセプト種別
                   07  LNK-ORCBT120SUB-RECESYUBETU     PIC  X(04).
      *保険組合せ番号
                   07  LNK-ORCBT120SUB-HKNCOMBINUM     PIC  9(04).
      *保険番号
                   07  LNK-ORCBT120SUB-HKNNUM          PIC  X(03).
      *保険ＩＤ
                   07  LNK-ORCBT120SUB-HKNID           PIC  9(10).
      *保険者番号
                   07  LNK-ORCBT120SUB-HKNJANUM        PIC  X(08).
      *本人家族区分
                   07  LNK-ORCBT120SUB-HONKZKKBN       PIC  X(01).
      *補助区分
                   07  LNK-ORCBT120SUB-HOJOKBN         PIC  X(01).
      *継続区分
                   07  LNK-ORCBT120SUB-CONTKBN         PIC  X(01).
      *公費−負担情報
                   07  LNK-ORCBT120SUB-KOHAREA.
      *公費１−公費番号
                     09  LNK-ORCBT120SUB-KOH1HKNNUM    PIC  X(03).
      *公費１−公費ＩＤ
                     09  LNK-ORCBT120SUB-KOH1ID        PIC  9(10).
      *公費１−支払区分
                     09  LNK-ORCBT120SUB-KOH1PAYKBN    PIC  X(02).
      *公費２−公費番号
                     09  LNK-ORCBT120SUB-KOH2HKNNUM    PIC  X(03).
      *公費２−公費ＩＤ
                     09  LNK-ORCBT120SUB-KOH2ID        PIC  9(10).
      *公費２−支払区分
                     09  LNK-ORCBT120SUB-KOH2PAYKBN    PIC  X(02).
      *公費３−公費番号
                     09  LNK-ORCBT120SUB-KOH3HKNNUM    PIC  X(03).
      *公費３−公費ＩＤ
                     09  LNK-ORCBT120SUB-KOH3ID        PIC  9(10).
      *公費３−支払区分
                     09  LNK-ORCBT120SUB-KOH3PAYKBN    PIC  X(02).
      *公費４−公費番号
                     09  LNK-ORCBT120SUB-KOH4HKNNUM    PIC  X(03).
      *公費４−公費ＩＤ
                     09  LNK-ORCBT120SUB-KOH4ID        PIC  9(10).
      *公費４−支払区分
                     09  LNK-ORCBT120SUB-KOH4PAYKBN    PIC  X(02).
      *公費−負担情報再定義
                   07  LNK-ORCBT120SUB-KOHFTN-TABLE  REDEFINES
                                        LNK-ORCBT120SUB-KOHAREA
                                                       OCCURS  4.
      *公費−公費番号
                     09  LNK-ORCBT120SUB-KOH-HKNNUM  PIC  X(03).
      *公費−公費ＩＤ
                     09  LNK-ORCBT120SUB-KOH-ID      PIC  9(10).
      *公費−支払区分
                     09  LNK-ORCBT120SUB-KOH-PAYKBN  PIC  X(02).
      *労災保険区分
                 07  LNK-ORCBT120SUB-RSIHKNKBN       PIC  X(01).
      *自賠責請求
                 07  LNK-ORCBT120SUB-JIBAIKBN        PIC  X(01).
      *災害区分（レセプト種別取得用）
                 07  LNK-ORCBT120SUB-SAIGAIKBN       PIC  X(01).
      *
      *復帰コード（１：算定日エラー　２：患者検索エラー　３：診療科エラー
      *            ４：未入院　５：情報取得エラー）
           03  LNK-ORCBT120SUB-RC                    PIC S9(9) BINARY.
