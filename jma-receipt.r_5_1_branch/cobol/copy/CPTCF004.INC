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
      *    日次統計データ          COPY (CPTCF004.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *                                     X(400) に変更
      ************************************************************
      *01  TOUKEI04-REC.
      *    レコード基本情報
           03  TOUKEI04-KIHON-AREA.
      *        医療機関ＩＤ
               05  TOUKEI04-HOSPID         PIC  X(24).
      *        医療機関コード
               05  TOUKEI04-HOSPCD         PIC  X(07).
      *        診療年月日
               05  TOUKEI04-SRYYMD         PIC  9(08).
      *        レコード識別
               05  TOUKEI04-RECSKB         PIC  9(01).
      *        レセプト種別コード
               05  TOUKEI04-RECESYUBETU    PIC  9(04).
      *        入外区分
               05  TOUKEI04-NYUGAIKBN      PIC  9(01).
      *        診療科
               05  TOUKEI04-SRYKA          PIC  X(02).
      *        レセ電診療科
               05  TOUKEI04-RECEKA         PIC  X(02).
      *        保険組合せ番号
               05  TOUKEI04-HKNCOMBINUM    PIC  9(04).
      *    患者基本情報
           03  TOUKEI04-PTKHN-AREA.
      *        患者番号
               05  TOUKEI04-PTNUM          PIC  X(20).
      *        患者ＩＤ
               05  TOUKEI04-PTID           PIC  X(10).
      *        カナ氏名
               05  TOUKEI04-KANANAME       PIC  X(50).
      *        漢字氏名
               05  TOUKEI04-NAME           PIC  X(100).
      *        性別
               05  TOUKEI04-SEX            PIC  X(01).
      *        生年月日
               05  TOUKEI04-BIRTHDAY       PIC  9(08).
      *    外来情報
           03  TOUKEI04-GAIRAIINF-AREA.
      *        基本料区分
               05  TOUKEI04-KIHONKBN       PIC  9(01).
      *        時間外区分
               05  TOUKEI04-TIMEKBN        PIC  9(01).
      *        往診区分
               05  TOUKEI04-OSINKBN        PIC  9(01).
      *        紹介区分
               05  TOUKEI04-SHOUKAIKBN     PIC  9(01).
      *        入院中外来識別
               05  TOUKEI04-NYUHAISKB      PIC  X(01).
      *        医師コード
               05  TOUKEI04-DRCD           PIC  X(05).
      *    入院情報
           03  TOUKEI04-NYUININF-AREA.
      *        入退院区分（１：当日入院　２：当日退院　３：入院中）
               05  TOUKEI04-NYUIN-KBN      PIC  9(01).
      *        病棟番号
               05  TOUKEI04-BTUNUM         PIC  X(02).
      *        病室番号
               05  TOUKEI04-BRMNUM         PIC  X(06).
      *        外泊区分
               05  TOUKEI04-GAIHAKU-KBN    PIC  9(01).
      *        食事区分
               05  TOUKEI04-SKJ-KBN        PIC  9(01).
      *        退院事由
               05  TOUKEI04-TAIINCD        PIC  X(02).
      *        前日診療科
               05  TOUKEI04-MAE-SRYKA      PIC  X(02).
      *        前日レセ電診療科
               05  TOUKEI04-MAE-RECEKA     PIC  X(02).
      *        前日病棟番号
               05  TOUKEI04-MAE-BTUNUM     PIC  X(02).
      *        前日病室番号
               05  TOUKEI04-MAE-BRMNUM     PIC  X(06).
      *    収納情報情報
           03  TOUKEI04-SYUNOU-AREA.
      *        伝票番号
               05  TOUKEI04-DENPNUM        PIC  9(07).
      *        伝票状態区分
               05  TOUKEI04-DENPJTIKBN     PIC  X(01).
      *        伝票作成区分
               05  TOUKEI04-CREATEKBN      PIC  X(01).
      *        保険点数
               05  TOUKEI04-HKNTEN         PIC  9(07).
      *        保険負担金額
               05  TOUKEI04-HKNMONEY       PIC  9(07).
      *        保険適用外金額
               05  TOUKEI04-TGMONEY        PIC  9(07).
      *        保険適用外金税額
               05  TOUKEI04-TGMONEY-TAX    PIC  9(07).
      *        自費金額
               05  TOUKEI04-JIHI           PIC  9(07).
      *        自費税額
               05  TOUKEI04-JIHI-TAX       PIC  9(07).
      *        食事負担金額
               05  TOUKEI04-SKJ            PIC  9(07).
      *        消費税額
               05  TOUKEI04-SKJ-TAX        PIC  9(07).
      *        室料差額
               05  TOUKEI04-RMSAGAKU       PIC  9(07).
      *        室料差額税額
               05  TOUKEI04-RMSAGAKU-TAX   PIC  9(07).
      *        請求金額
               05  TOUKEI04-SKYMONEY       PIC  9(07).
      *    保険情報
           03  TOUKEI04-HKN-AREA.
      *        保険番号
               05  TOUKEI04-HKNNUM         PIC  X(03).
      *        保険ＩＤ
               05  TOUKEI04-HKNID          PIC  9(10).
      *        保険者番号
               05  TOUKEI04-HKNJANUM       PIC  X(08).
      *        本人家族区分
               05  TOUKEI04-HONKZKKBN      PIC  X(01).
      *        補助区分
               05  TOUKEI04-HOJOKBN        PIC  X(01).
      *        継続区分
               05  TOUKEI04-CONTKBN        PIC  X(01).
      *    公費情報
           03  TOUKEI04-KOHI-AREA.
      *        公費１−公費番号
               05  TOUKEI04-KOH1HKNNUM     PIC  X(03).
      *        公費１−公費ＩＤ
               05  TOUKEI04-KOH1ID         PIC  9(10).
      *        公費１−支払区分
               05  TOUKEI04-KOH1PAYKBN     PIC  X(02).
      *        公費２−公費番号
               05  TOUKEI04-KOH2HKNNUM     PIC  X(03).
      *        公費２−公費ＩＤ
               05  TOUKEI04-KOH2ID         PIC  9(10).
      *        公費２−支払区分
               05  TOUKEI04-KOH2PAYKBN     PIC  X(02).
      *        公費３−公費番号
               05  TOUKEI04-KOH3HKNNUM     PIC  X(03).
      *        公費３−公費ＩＤ
               05  TOUKEI04-KOH3ID         PIC  9(10).
      *        公費３−支払区分
               05  TOUKEI04-KOH3PAYKBN     PIC  X(02).
      *        公費４−公費番号
               05  TOUKEI04-KOH4HKNNUM     PIC  X(03).
      *        公費４−公費ＩＤ
               05  TOUKEI04-KOH4ID         PIC  9(10).
      *        公費４−支払区分
               05  TOUKEI04-KOH4PAYKBN     PIC  X(02).
      *    公費−負担情報再定義
           03  TOUKEI04-KOHFTN-TABLE  REDEFINES  TOUKEI04-KOHI-AREA
                                                  OCCURS  4.
      *        公費−公費番号
               05  TOUKEI04-KOH-HKNNUM     PIC  X(03).
      *        公費−公費ＩＤ
               05  TOUKEI04-KOH-ID         PIC  9(10).
      *        公費−支払区分
               05  TOUKEI04-KOH-PAYKBN     PIC  X(02).
      *    労災情報
           03  TOUKEI04-HKN.
      *        労災−労災保険区分
               05  TOUKEI04-RSIHKNKBN      PIC  X(01).
      *        労災−自賠責請求区分
               05  TOUKEI04-JIBAISEIKBN    PIC  X(01).
      *    患者属性情報
           03  TOUKEI04-PTINF-AREA.
      *        郵便番号
               05  TOUKEI04-HOME-POST       PIC  X(07).
      *        地方公共団体コード
               05  TOUKEI04-LPUBCD          PIC  X(05).
      *        住所
               05  TOUKEI04-HOME-ADRS       PIC  X(400).
      *        電話番号
               05  TOUKEI04-HOME-TEL        PIC  X(15).
