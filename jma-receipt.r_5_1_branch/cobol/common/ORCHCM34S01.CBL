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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCHCM34S01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院会計カード情報編集サブ
      *                      （会計カードのサブＰＧ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   04/02/02  室料差額・食事の算定を
      *                                     暫定的に修正
      *  01.00.02    NACL−太田   04/03/03  医療機関減算対応
      *  01.00.03    NACL−太田   04/03/11  有床診療所１群入院基本料１
      *                                     （医師配置適合）対応
      *  01.00.04    NACL−太田   04/08/23  入院会計室料差額区分
      *  00.00.05    NACL-小豆沢  05/09/16  以下の特定入院料の外泊計算を修正
      *                                     診療所老人医療管理料
      *                                     老人性認知症疾患治療病棟入院料１
      *                                     老人性認知症疾患治療病棟入院料２
      *  01.00.06    NACL-小豆沢  06/03/22  特定入院料追加分（脳卒中ケアユニ）
      *                                     外泊置き換え処理
      *  01.00.07    NACL-小豆沢  06/05/23  外泊時の入院基本料取得修正
      *  03.01.01    NACL-門脇    06/06/30  療養病棟の他医療機関受診対応
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  04.01.00    NACL-太田    07/11/20  室料差額６桁対応
      *  04.01.00    NACL-太田    07/12/19  短期滞在手術基本料２の判定追加
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    点数マスタ
           SELECT  CTENSU-FILE     ASSIGN   ASGN-CTENSU
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  CTNS-KEY
                                   FILE    STATUS  IS  STS-CTENSU.
      *
      *    選定コード置換
           SELECT  CSENTEICDCHG-FILE
                                   ASSIGN   ASGN-CSENTEICDCHG
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  CSENCHG-KEY
                                   FILE    STATUS  IS  STS-CSENTEICDCHG.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  CTENSU-FILE.
           COPY "CPTENSU.INC"  REPLACING   //TNS-//   BY //CTNS-//
                                           //TNSUP-// BY //CTNSUP-//
                                           //TNSXX-// BY //CTNSXX-//.
      *
       FD  CSENTEICDCHG-FILE.
       01  CSENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC"
                               REPLACING   //SENCHG-// BY //CSENCHG-//.
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //ASGN-CTENSU//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                       BY        //ASGN-CSENTEICDCHG//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-CTENSU                  PIC X(02).
           03  STS-CSENTEICDCHG            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OVER                        PIC 9(01).
           03  FLG-HIT                         PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-NYUINACT                    PIC 9(01).
           03  FLG-SENTEICDCHG                 PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-TAISYO                      PIC 9(01).
           03  FLG-NYUINARI                    PIC 9(01).
           03  FLG-KIKAN-G .
               05  FLG-KIKANDAY                PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINDAY-G.
               05  FLG-NYUINDAY                PIC 9(01)
                                               OCCURS  31.
           03  FLG-SKJDAY-G.
               05  FLG-SKJDAY                  PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINRYODAY-G.
               05  FLG-NYUINRYODAY             PIC 9(01)
                                               OCCURS  31.
           03  FLG-SENTEIDAY-G.
               05  FLG-SENTEIDAY               PIC 9(01)
                                               OCCURS  31.
           03  FLG-TOKTEIDAY-G.
               05  FLG-TOKTEIDAY               PIC 9(01)
                                               OCCURS  31.
           03  FLG-GAIHAKUDAY-G.
               05  FLG-GAIHAKUDAY              PIC 9(02)
                                               OCCURS  31.
           03  FLG-SYOKUDODAY-G.
               05  FLG-SYOKUDODAY              PIC 9(01)
                                               OCCURS  31.
           03  FLG-NYUINGENDAY-G.
               05  FLG-NYUINGENDAY-OCC         OCCURS  4.
                   07  FLG-NYUINGENDAY         PIC 9(01)
                                               OCCURS  31.
           03  FLG-BRMSGKSANTEIDAY-G.
               05  FLG-BRMSGKSANTEIDAY         PIC 9(01)
                                               OCCURS  31.
           03  FLG-SANTEI                      PIC 9(01).
           03  FLG-SENTEI-OK                   PIC 9(01).
           03  FLG-CLEAR                       PIC 9(01).
           03  FLG-EXSYOKUJI                   PIC 9(01).
           03  FLG-SKJZAIPTN                   PIC 9(01).
           03  FLG-SKJFTN                      PIC 9(01).
           03  FLG-LOOPEND                     PIC 9(01).
           03  FLG-SET-DIR                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SYS5000                     PIC 9(05).
           03  CNT-KANKYO-DAY-G.
               05  CNT-KANKYO-DAY-TOTAL        PIC 9(05).
               05  CNT-KANKYO-DAY              PIC 9(01)
                                               OCCURS  31.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDX2                            PIC 9(05).
           03  IDX3                            PIC 9(05).
           03  IDX4                            PIC 9(05).
           03  IDX5                            PIC 9(05).
           03  IDX6                            PIC 9(05).
           03  IDX7                            PIC 9(05).
           03  IDX8                            PIC 9(05).
           03  IDX9                            PIC 9(05).
           03  IDXA                            PIC 9(05).
           03  IDXB                            PIC 9(05).
           03  IDXC                            PIC 9(05).
           03  IDXD                            PIC 9(05).
           03  IDXE                            PIC 9(05).
           03  IDXF                            PIC 9(05).
           03  IDXG                            PIC 9(05).
           03  IDXH                            PIC 9(05).
           03  IDXI                            PIC 9(05).
           03  IDXJ                            PIC 9(05).
           03  IDXK                            PIC 9(05).
           03  IDXM                            PIC 9(05).
           03  IDXN                            PIC 9(05).
           03  IDXS                            PIC 9(05).
           03  IDX-KHN                         PIC 9(05).
           03  IDX-TAX                         PIC 9(05).
           03  IDX-SKJ                         PIC 9(05).
           03  IDX-WSYOKUJI                    PIC 9(05).
           03  IDX-KANKYO                      PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YYMM                    PIC X(06).
               05  WRK-DD                      PIC 9(02).
           03  WRK-DBYMD                       PIC X(08).
           03  WRK-TNS-SRYCD                   PIC X(09).
           03  WRK-GAIHAKU-SRYCD-G.
               05  WRK-GAIHAKU-SRYCD           PIC X(09).
               05  WRK-TAKIKAN2-SRYCD          PIC X(09).
           03  WRK-GAIHAKU-SRYCD-R REDEFINES WRK-GAIHAKU-SRYCD-G.
               05  WRK-GAIHAKU-SRYCD-OCC       OCCURS 2.
                   07  WRK-GAIHAKU-SRYCDL      PIC X(09).
           03  WRK-GAIHAKU-KHNTEN              PIC 9(08).
           03  WRK-GAIHAKU-KSNTEN              PIC 9(08).
           03  WRK-NYUKHNKSNKBN                PIC 9(03).
           03  WRK-CHUKSNCD                    PIC X(04).
           03  WRK-NYUKHNKBN                   PIC 9(03).
           03  WRK-NACCT-ZAISKBKBN             PIC X(01).
           03  WRK-KHNTEN                      PIC S9(08).
           03  WRK-KHNSRYCD                    PIC X(09).
           03  WRK-KSNSRYCD-G.
               05  WRK-KSNSRYCD-MAX            PIC 9(03).
               05  WRK-KSNSRYCD                PIC X(09)
                                               OCCURS   5.
           03  WRK-HYOKETSU-TEN                 PIC S9(08).
           03  WRK-HYOKETSU-SRYCD               PIC X(09).
           03  WRK-SENTEI-TEN                  PIC S9(08).
           03  WRK-SENTEI-SRYCD                PIC X(09).
           03  WRK-SENTEI-IPNCD                PIC X(09).
           03  WRK-SENTEI-KHNTEN               PIC 9(08).
           03  WRK-SENTEISRYCD                 PIC X(09).
           03  WRK-SENTEIIPNCD                  PIC X(09).
           03  WRK-SRYCDTOTAL                  PIC 9(09).
           03  WRK-SENTEI-KEISU                PIC 9V99.
           03  WRK-BRMSAGAKU                   PIC 9(07).
           03  WRK-BRMSAGAKUCD                 PIC X(02).
           03  WRK-SKJ-TEN-G.
               05  WRK-SKJ-TEN-RYOYO1          PIC 9(08).
               05  WRK-SKJ-TEN-RYOYO1-RDS      PIC 9(08).
               05  WRK-SKJ-TEN-RYOYO2          PIC 9(08).
               05  WRK-SKJ-TEN-RYOYO2-RDS      PIC 9(08).
               05  WRK-SKJ-TEN-TOKKANRI        PIC 9(08).
               05  WRK-SKJ-TEN-TOKSYOKU        PIC 9(08).
               05  WRK-SKJ-TEN-SYOKUDO         PIC 9(08).
               05  WRK-SKJ-TEN-FKSKONDATE      PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-IPN      PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI2     PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI2-90  PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-TEI1     PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-NANBYO   PIC 9(08).
               03  WRK-SKJ-TEN-SKJFTN-SEISHIN  PIC 9(08).
           03  WRK-LIFE-TEN-G.
               05  WRK-LIFE-TEN-RYOYO1         PIC 9(08).
               05  WRK-LIFE-TEN-RYOYO1-RDS     PIC 9(08).
               05  WRK-LIFE-TEN-RYOYO2         PIC 9(08).
               05  WRK-LIFE-TEN-TOKSYOKU       PIC 9(08).
               05  WRK-LIFE-TEN-SYOKUDO        PIC 9(08).
               05  WRK-LIFE-TEN-KANKYO1        PIC 9(08).
               05  WRK-LIFE-TEN-KANKYO2        PIC 9(08).
           03  WRK-SYS-5113.
               05  WRK-SYS-5113-MAX            PIC 9(03).
               05  WRK-SYS-5113-OCC            OCCURS  99.
                   07  WRK-SYS-5113-STYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-EDYUKYMD   PIC X(08).
                   07  WRK-SYS-5113-KBNCD      PIC 9(02).
                   07  WRK-SYS-5113-BRMSAGAKU  PIC 9(07).
           03  WRK-SRYCD-NUM                   PIC 9(09).
           03  WRK-SKJ-SRYCD-G.
               05  WRK-SKJ-PTFTN               PIC 9(01).
               05  WRK-SKJ-ZAITEN              PIC 9(08).
               05  WRK-SKJ-SRYCDTOTAL          PIC 9(14).
               05  WRK-SKJ-SRYCD               PIC X(09)   OCCURS  5.
           03  WRK-SYS-MIJISI-G.
               05  WRK-SYS-MIJISI-OCC                      OCCURS  4.
                   07  WRK-SYS-MIJISI          PIC 9(01).
                   07  WRK-SYS-MIJISI-ZAITEN   PIC 9(08).
           03  WRK-SHOKUJIRYOYO                PIC 9(01).
           03  WRK-TOKUKANRIKASAN-G.
               05  WRK-TOKUKANRIKASAN          PIC 9(01)   OCCURS  31.
           03  WRK-SENTEI-PTFTN-G.
               05  WRK-SENTEI-PTFTN            PIC 9(08)   OCCURS  2.
           03  WRK-SENTEI-GAIHAKU              PIC 9(08).
           03  WRK-SENTEI-FTNKBN               PIC X(01).
           03  WRK-SENTEI-TANKA-X              PIC  X(02).
           03  WRK-SENTEI-TANKA-R              REDEFINES
               WRK-SENTEI-TANKA-X.
               05  WRK-SENTEI-TANKA            PIC  9(02).
           03  WRK-SENTEI-KINGAKU-X            PIC  X(04).
           03  WRK-SENTEI-KINGAKU-R            REDEFINES
               WRK-SENTEI-KINGAKU-X.
               05  WRK-SENTEI-KINGAKU          PIC  9(04).
           03  WRK-STIDX                       PIC  9(02).
           03  WRK-EDIDX                       PIC  9(02).
           03  WRK-SRYCD-G.
               05  WRK-SRYCD                   PIC  X(09).
               05  WRK-SRYCD-9                 REDEFINES WRK-SRYCD
                                               PIC  9(09).
           03  WRK-SRYYMD                      PIC  X(08).
           03  WRK-SKYSTYMD.
               05  WRK-SKYSTYM                 PIC  X(06).
               05  WRK-SKYSTDD                 PIC  9(02).
           03  WRK-SKYEDYMD.
               05  WRK-SKYEDYM                 PIC  X(06).
               05  WRK-SKYEDDD                 PIC  9(02).
           03  WRK-SKJCOMMENTCD                PIC  X(09).
           03  WRK-ZAI-G.
               05  WRK-ZAI-ZAINUM              PIC  9(02).
               05  WRK-ZAI-SRYKBN              PIC  X(02).
               05  WRK-ZAI-SRYCDTOTAL          PIC  9(14).
               05  WRK-ZAI-TEN                 PIC  9(08).
               05  WRK-ZAI-TEN-TEMP            PIC  9(08)V999999.
               05  WRK-ZAI-KHNSRYCD            PIC  X(09).
               05  WRK-ZAI-KHNTEN              PIC  9(08).
               05  WRK-ZAI-KHNIDX              PIC  9(05).
               05  WRK-ZAI-SENTEI-KHNTEN       PIC  9(08).
               05  WRK-ZAI-KSNTEN              PIC  9(08).
               05  WRK-ZAI-HYOKETSU-RATE       PIC  9(05).
               05  WRK-ZAI-CNT                 PIC  9(08).
               05  WRK-ZAI-SRYCD               PIC  X(09)
                                               OCCURS  20.
           03  WRK-DOJITU-G.
               05  WRK-DOJITU-OCC              OCCURS  31.
                   07  WRK-DOJITUST            PIC  X(01).
                   07  WRK-DOJITUED            PIC  X(01).
           03  WRK-RATE1                       PIC 999V99.
           03  WRK-RATE2                       PIC 999V99.
           03  WRK-RATE3                       PIC 999V99.
           03  WRK-RATE4                       PIC 999V99.
           03  WRK-HASU                        PIC 9V99999.
           03  WRK-ZAITEN                      PIC 9(08).
           03  WRK-HKNCOMBI-FST-G.
               05  WRK-HKNCOMBI-FST            PIC 9(04).
           03  WRK-HKNCOMBI-SND-G.
               05  WRK-HKNCOMBI-SND            PIC 9(04).
      *
      *    入院料
       01  WNYUINRYO-AREA.
           03  WNYUINRYO-OCC1                  OCCURS 02.
               05  WNYUINRYO-OCC2-MAX          PIC 9(05).
               05  WNYUINRYO-OCC2              OCCURS 20.
                   07  WNYUINRYO-ZAINUM        PIC 9(08).
      *            入院基本料診療区分
                   07  WNYUINRYO-SRYKBN        PIC X(02).
      *            剤点数
                   07  WNYUINRYO-ZAITEN        PIC 9(08).
      *            明細数
                   07  WNYUINRYO-SRYCDTOTAL    PIC 9(14).
      *            剤回数
                   07  WNYUINRYO-ZAIKAISU      PIC 9(07).
      *            明細数
                   07  WNYUINRYO-MEISAISU      PIC 9(03).
      *            診療コード
                   07  WNYUINRYO-SRYCD         PIC X(09)   OCCURS  20.
      *            診療回数テーブル
                   07  WNYUINRYO-DAY-TBL.
                       09  WNYUINRYO-DAY       PIC 9(03)   OCCURS  31.
      *
      *    外泊
       01  WGAIHAKU-AREA.
           03  WGAIHAKU-OCC1                   OCCURS 02.
               05  WGAIHAKU-OCC2-MAX           PIC 9(05).
               05  WGAIHAKU-OCC2               OCCURS 20.
      *            ソート用番号
                   07  WGAIHAKU-NUM            PIC 9(03).
      *                診療行為コード合計
                   07  WGAIHAKU-SRYCDTOTAL     PIC 9(14).
      *            入院基本料診療区分
                   07  WGAIHAKU-SRYKBN         PIC X(02).
      *            剤点数
                   07  WGAIHAKU-ZAITEN         PIC 9(08).
      *            剤回数
                   07  WGAIHAKU-ZAIKAISU       PIC 9(07).
      *            診療コード
                   07  WGAIHAKU-SRYCD          PIC X(09)
                                               OCCURS 20.
      *            診療回数テーブル
                   07  WGAIHAKU-DAY-TBL.
                       09  WGAIHAKU-DAY        PIC 9(03)   OCCURS  31.
      *
      *    室料差額
       01  WBRMSAGAKU-AREA.
           03  WBRMSAGAKU-OCC1-MAX             PIC 9(05).
           03  WBRMSAGAKU-OCC1                 OCCURS 20.
      *        ソート用番号
               05  WBRMSAGAKU-NUM              PIC 9(03).
      *        入院基本料診療区分
               05  WBRMSAGAKU-SRYKBN           PIC X(02).
      *        剤点数
               05  WBRMSAGAKU-ZAITEN           PIC 9(08).
      *        室料差額区分コード
               05  WBRMSAGAKU-SAGAKUCD         PIC X(02).
      *        剤回数
               05  WBRMSAGAKU-ZAIKAISU         PIC 9(07).
      *        診療コード
               05  WBRMSAGAKU-SRYCD            PIC X(09).
      *        診療回数テーブル
               05  WBRMSAGAKU-DAY-TBL.
                   07  WBRMSAGAKU-DAY          PIC 9(03)  OCCURS  31.
      *
      *    食事
       01  WSYOKUJI-AREA.
           03  WSYOKUJI-OCC1-MAX               PIC 9(05).
           03  WSYOKUJI-OCC1                   OCCURS 50.
      *        ソート用番号
               05  WSYOKUJI-NUM                PIC 9(03).
      *        環境療養剤フラグ
               05  WSYOKUJI-KANKYO             PIC 9(03).
      *        診療行為コード合計
               05  WSYOKUJI-SRYCDTOTAL         PIC 9(14).
      *        数量合計
               05  WSYOKUJI-SURYOTOTAL         PIC 9(10).
      *        入院基本料診療区分
               05  WSYOKUJI-SRYKBN             PIC X(02).
      *        剤点数
               05  WSYOKUJI-ZAITEN             PIC 9(08).
      *        剤回数
               05  WSYOKUJI-ZAIKAISU           PIC 9(07).
      *        明細数
               05  WSYOKUJI-MEISAISU           PIC 9(08).
               05  WSYOKUJI-SRYCD-OCC          OCCURS  20.
      *            数量
                   07  WSYOKUJI-SURYO          PIC 9(05).
      *            診療コード
                   07  WSYOKUJI-SRYCD          PIC X(09).
      *        診療回数テーブル
               05  WSYOKUJI-DAY-TBL.
                   07  WSYOKUJI-DAY            PIC 9(03)   OCCURS  31.
      *        生活療養フラグ
               05  WSYOKUJI-LIFE               PIC 9(01).
      *        食事（環境）負担額剤フラグ
               05  WSYOKUJI-PTFTN              PIC 9(01).
      *
      *    食事
       01  WSKJH1804-AREA.
      *    診療行為コード合計
           03  WSKJH1804-SRYCDTOTAL        PIC 9(14).
      *    数量合計
           03  WSKJH1804-SURYOTOTAL        PIC 9(10).
      *    剤点数
           03  WSKJH1804-ZAITEN            PIC 9(08).
      *    明細数
           03  WSKJH1804-MEISAISU          PIC 9(08).
           03  WSKJH1804-SRYCD-CNT-G.
               05  WSKJH1804-SRYCD-CNT-VAL.
                   07  WSKJH1804-CNT-ASA       PIC 9(05).
                   07  WSKJH1804-CNT-HIRU      PIC 9(05).
                   07  WSKJH1804-CNT-YUU       PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO1 PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO1-RDS
                                               PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO2 PIC 9(05).
                   07  WSKJH1804-CNT-SKJRYOYO2-RDS
                                               PIC 9(05).
                   07  WSKJH1804-CNT-TOKSYOKU  PIC 9(05).
                   07  WSKJH1804-CNT-SYOKUDO   PIC 9(05).
                   07  WSKJH1804-CNT-SKJFTN    PIC 9(05).
                   07  WSKJH1804-CNT-KANFTN    PIC 9(05).
               05  WSKJH1804-SRYCD-CNT-OCC     REDEFINES
                   WSKJH1804-SRYCD-CNT-VAL     OCCURS 11.
                   07  WSKJH1804-SRYCD-CNT     PIC 9(05).
      *
      *    入院料加算
       01  WNYUINKSN-AREA.
           03  WNYUINKSN-OCC1                  OCCURS 02.
               05  WNYUINKSN-OCC2-MAX          PIC 9(05).
               05  WNYUINKSN-OCC2              OCCURS 50.
      *            表用番号（ソート用）
                   07 WNYUINKSN-CDKBN-SYO      PIC 9(01).
                   07 WNYUINKSN-CDKBN-BU       PIC 9(02).
                   07 WNYUINKSN-CDKBN-KBN      PIC X(01).
                   07 WNYUINKSN-CDKBN-KBNNUM   PIC 9(03).
                   07 WNYUINKSN-CDKBN-KBNNUM-EDA
                                               PIC 9(02).
                   07 WNYUINKSN-CDKBN-KOUBAN   PIC 9(02).
                   07 WNYUINKSN-KOUHYOJYUNNUM
                                               PIC 9(09).
      *            入院基本料診療区分
                   07  WNYUINKSN-SRYKBN        PIC X(02).
      *            剤点数
                   07  WNYUINKSN-ZAITEN        PIC 9(08).
      *            剤回数
                   07  WNYUINKSN-ZAIKAISU      PIC 9(07).
      *            診療コード
                   07  WNYUINKSN-SRYCD         PIC X(09).
      *            診療回数テーブル
                   07  WNYUINKSN-DAY-TBL.
                       09  WNYUINKSN-DAY       PIC  9(03)  OCCURS  31.
      *
      *    療養担当手当
       01  WRYYTAN-AREA.
      *    診療区分
           03  WRYYTAN-SRYKBN                  PIC X(02).
      *    剤点数
           03  WRYYTAN-ZAITEN                  PIC 9(08).
      *    剤回数
           03  WRYYTAN-ZAIKAISU                PIC 9(07).
      *    診療コード
           03  WRYYTAN-SRYCD                   PIC X(09).
      *    診療回数テーブル
           03  WRYYTAN-DAY-TBL.
               05  WRYYTAN-DAY                 PIC 9(03)   OCCURS  31.
      *
      *    ソート用領域
       01  SRT-AREA.
           03  SRT-IDX1                        PIC 9(05).
           03  SRT-IDX2                        PIC 9(05).
           03  SRT-STIDX                       PIC 9(05).
           03  SRT-MAX                         PIC 9(05).
           03  SRT-OCC1                        OCCURS 101.
               05  SRT-KEY.
                   07  SRT-SRYKBN              PIC X(02).
                   07  SRT-PRIORITY            PIC 9(03).
                   07  SRT-PRIORITY2           PIC 9(03).
                   07  SRT-PRIORITY3           PIC 9(03).
                   07  SRT-CDKBN-SYO           PIC 9(01).
                   07  SRT-CDKBN-BU            PIC 9(02).
                   07  SRT-CDKBN-KBN           PIC X(01).
                   07  SRT-CDKBN-KBNNUM        PIC 9(03).
                   07  SRT-CDKBN-KBNNUM-EDA    PIC 9(02).
                   07  SRT-CDKBN-KOUBAN        PIC 9(02).
                   07  SRT-KOUHYOJYUNNUM       PIC 9(09).
               05  SRT-ZAISKBKBN               PIC X(01).
               05  SRT-ZAITEN                  PIC 9(08).
               05  SRT-ZAIKAISU                PIC 9(07).
               05  SRT-SAGAKUCD                PIC X(02).
               05  SRT-PTFTN                   PIC 9(01).
               05  SRT-SRYCD-OCC               OCCURS  20.
                   07  SRT-SURYO               PIC 9(05).
                   07  SRT-SRYCD               PIC X(09).
               05  SRT-DAY-TBL.
                   07  SRT-DAY                 PIC 9(03)   OCCURS  31.
      *
      *    コンスタント値
       01  CONST-AREA.
           03  CONST-R0110                 PIC X(08)   VALUE "201910".
           03  CONST-H3004                 PIC X(08)   VALUE "201804".
           03  CONST-H2910                 PIC X(08)   VALUE "201710".
           03  CONST-H2804                 PIC X(08)   VALUE "201604".
           03  CONST-H2604                 PIC X(08)   VALUE "201404".
           03  CONST-H2404                 PIC X(06)   VALUE "201204".
           03  CONST-H2204                 PIC X(06)   VALUE "201004".
           03  CONST-H1904                 PIC X(06)   VALUE "200704".
           03  CONST-H1804                 PIC X(06)   VALUE "200604".
           03  CONST-H1810                 PIC X(06)   VALUE "200610".
           03  CONST-H1604                 PIC X(06)   VALUE "200404".
      *    入院料編集作業領域最大件数
           03  CONST-WNYUINRYO-MAX             PIC 9(05)   VALUE 20.
      *    外泊編集作業領域最大件数
           03  CONST-WGAIHAKU-MAX              PIC 9(05)   VALUE 20.
      *    室料差額編集作業領域最大件数
           03  CONST-WBRMSAGAKU-MAX            PIC 9(05)   VALUE 20.
      *    食事編集作業領域最大件数
           03  CONST-WSYOKUJI-MAX              PIC 9(05)   VALUE 50.
      *    入院料加算編集作業領域最大件数
           03  CONST-WNYUINKSN-MAX             PIC 9(05)   VALUE 50.
      *    返却最大件数
           03  CONST-HENKYAKU-MAX              PIC 9(05)   VALUE 100.
      *    ソート作業領域最大件数
           03  CONST-SRT-MAX                   PIC 9(05)   VALUE 100.
      *    入院時食事療養（１）
           03  CONST-SKJ-RYOYO1                PIC X(09)
                                                   VALUE "197000110".
      *    入院時食事療養（２）
           03  CONST-SKJ-RYOYO2                PIC X(09)
                                                   VALUE "197000710".
      *    入院時食事療養（１）（流動食）
           03  CONST-SKJ-RYOYO1-RDS            PIC X(09)
                                                   VALUE "197003110".
      *    入院時食事療養（２）（流動食）
           03  CONST-SKJ-RYOYO2-RDS            PIC X(09)
                                                   VALUE "197003210".
      *    特別管理加算
           03  CONST-SKJ-TOKKANRI              PIC X(09)
                                                   VALUE "197000270".
      *    特別食加算
           03  CONST-SKJ-TOKSYOKU              PIC X(09)
                                                   VALUE "197000470".
      *    食堂加算
           03  CONST-SKJ-SYOKUDO               PIC X(09)
                                                   VALUE "197000570".
      *    複数献立加算
           03  CONST-SKJ-FKSKONDATE            PIC X(09)
                                                   VALUE "197000670".
      *    入院時生活療養（１）食事療養
           03  CONST-LIFE-RYOYO1               PIC X(09)
                                                   VALUE "197001310".
      *    入院時生活療養（１）食事療養（流動食）
           03  CONST-LIFE-RYOYO1-RDS           PIC X(09)
                                                   VALUE "197003510".
      *    入院時生活療養（１）環境療養
           03  CONST-LIFE-KANKYO1              PIC X(09)
                                                   VALUE "197001410".
      *    特別食加算（生活療養）
           03  CONST-LIFE-TOKSYOKU             PIC X(09)
                                                   VALUE "197001570".
      *    食堂加算（生活療養）
           03  CONST-LIFE-SYOKUDO              PIC X(09)
                                                   VALUE "197001670".
      *    入院時生活療養（２）食事療養
           03  CONST-LIFE-RYOYO2               PIC X(09)
                                                   VALUE "197001710".
      *    入院時生活療養（２）環境療養
           03  CONST-LIFE-KANKYO2              PIC X(09)
                                                   VALUE "197001810".
      *    食事療養標準負担額（一般）
           03  CONST-SKJFTN-IPN                PIC X(09)
                                                   VALUE "197000810".
      *    食事療養標準負担額（低２・９０日以下）
           03  CONST-SKJFTN-TEI2               PIC X(09)
                                                   VALUE "197000910".
      *    食事療養標準負担額（低２・９０日超）
           03  CONST-SKJFTN-TEI2-90            PIC X(09)
                                                   VALUE "197001010".
      *    食事療養標準負担額（低１）
           03  CONST-SKJFTN-TEI1               PIC X(09)
                                                   VALUE "197001110".
      *    食事療養標準負担額（指定難病・小児慢性特定疾病患者）（一般）
           03  CONST-SKJFTN-NANBYO               PIC X(09)
                                                   VALUE "197003310".
      *    食事療養標準負担額（精神１年超）（一般）
           03  CONST-SKJFTN-SEISHIN               PIC X(09)
                                                   VALUE "197003410".
      *    選定療養（入院１８０日超）減算（入院基本料）
           03  CONST-SENTEI                    PIC X(09)
                                                   VALUE "190122890".
      *
           03  CONST-SKJ-COMMENT-G.
               05  CONST-SKJ-COMMENT-VAL.
      *    食事朝
                   07  CONST-SKJ-ASA           PIC 9(09)
                                               VALUE 99999918.
      *    食事昼
                   07  CONST-SKJ-HIRU          PIC 9(09)
                                               VALUE 99999919.
      *    食事夕
                   07  CONST-SKJ-YUU           PIC 9(09)
                                               VALUE 99999920.
               05  CONST-SKJ-COMMENT-R REDEFINES CONST-SKJ-COMMENT-VAL.
                   07  CONST-SKJ-COMMENT       PIC 9(09)
                                               OCCURS 3.
           03  CONST-NOT-FOUND                 PIC X(09)
                                                   VALUE "NOT_FOUND".
      *
           COPY    "CPSK5000.INC"  REPLACING   //SYS-5000//
                                   BY          //WKSYS-5000//.
      *
       01  WSKJNACCT-AREA.
           02  WSKJNACCT-SYOKUDO-NG            PIC X(01)
                                               OCCURS  31.
           02  WSKJNACCT-OCC                   OCCURS  3.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT-//
                                       BY          //WSKJNACCT-//.
      *
       01  TSKJFTN-AREA.
           03  TSKJFTN-OCC                     OCCURS  31.
               05  TSKJFTN-SRYCD               PIC X(09).
               05  TSKJFTN-MONEY               PIC 9(10).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5113.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
           COPY "CPTENSU.INC"  REPLACING   //TNS-//   BY //TTNS-//
                                           //TNSUP-// BY //TTNSUP-//
                                           //TNSXX-// BY //TTNSXX-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
       01  UIDIO-AREA.
           03  UIDIO-RET               PIC X(2).
           03  UIDIO-UID               PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    病棟選定入院自己負担取得サブ
           COPY    "CPORCSBTUSENTEI.INC".
      *
      *    入院時生活療養費負担額取得サブ
           COPY    "CPORCSNYULIFE.INC".
      *
      *    入院時生活療養費負担額取得サブ２
           COPY    "CPORCSNYULIFE2.INC".
      *
      *    食事負担据え置き対象取得サブ
           COPY    "CPORCSSKJKEEP.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    入院点数マスタ付加情報サブ
           COPY    "CPORCSNTENSU.INC".
      *
      *    夜間看護体制特定日減算取得サブ
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    入院会計カード情報編集サブパラメタ
           COPY    "CPORCHCM34S01.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           HCM34S01-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
           INITIALIZE                      WNYUINRYO-AREA
                                           WGAIHAKU-AREA
                                           WBRMSAGAKU-AREA
                                           WSYOKUJI-AREA
                                           WSKJH1804-AREA
                                           WNYUINKSN-AREA
                                           WRYYTAN-AREA
                                           WSKJNACCT-AREA
                                           TSKJFTN-AREA
      *
           INITIALIZE                      HCM34S01-HENKYAKU-AREA
           MOVE    ZERO                TO  HCM34S01-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
      *    初期処理
           PERFORM 150-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月が設定されていること
           IF    ( HCM34S01-SANTEIYM       =   SPACE )
               MOVE    1                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                          PTINF-REC
           MOVE    HCM34S01-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    HCM34S01-PTID           TO  PTINF-PTID
           MOVE    PTINF-REC               TO  MCPDATA-REC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  NOT =   ZERO )
               MOVE    2                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    該当保険組み合わせの入院会計が存在するかチェック
           INITIALIZE                          FLG-NYUINDAY-G
      *
           MOVE    "5"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               MOVE    3                   TO  HCM34S01-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    ZERO                    TO  FLG-NYUINARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   31 )
                IF   ( HCM34S01-HKNCOMBI   =   NACCT-DAY(IDX1) )
                   MOVE    1               TO  FLG-NYUINARI
                   IF    ( HCM34S01-SRYKAKBN   =   "1" )
                       IF    ( HCM34S01-SRYKADAY (IDX1)    =   1 )
                           MOVE    1       TO  FLG-NYUINDAY (IDX1)
                       END-IF
                   ELSE
                           MOVE    1       TO  FLG-NYUINDAY (IDX1)
                   END-IF
                END-IF
           END-PERFORM
      *
           MOVE    HCM34S01-SANTEIYM       TO  WRK-SKYSTYMD
                                               WRK-SKYEDYMD
           MOVE    1                       TO  WRK-SKYSTDD
           MOVE    31                      TO  WRK-SKYEDDD
      *    期間指定区分が設定されている場合、期間が設定されていること
           IF    ( HCM34S01-KIKANKBN       =   "1" )
               IF    ( HCM34S01-SKYSTYMD   IS  NOT NUMERIC )
                   MOVE    12              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               IF    ( HCM34S01-SKYEDYMD   IS  NOT NUMERIC )
                   MOVE    13              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    HCM34S01-SKYSTYMD (7:2) TO  WRK-STIDX
               MOVE    HCM34S01-SKYEDYMD (7:2) TO  WRK-EDIDX
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   31 )
      *
                   IF    ( IDX1    >=  WRK-STIDX )
                    AND  ( IDX1    <=  WRK-EDIDX )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-NYUINDAY (IDX1)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    HCM34S01-SKYSTYMD   TO  WRK-SKYSTYMD
               MOVE    HCM34S01-SKYEDYMD   TO  WRK-SKYEDYMD
      *
               IF    ( HCM34S01-DOJITUFLG-ST   =   "1" )
                   MOVE    2           TO  FLG-NYUINDAY(WRK-SKYSTDD)
                   MOVE    HCM34S01-TIMEKBN-ST
                                       TO  WRK-DOJITUST(WRK-SKYSTDD)
               END-IF
      *
               IF    ( HCM34S01-DOJITUFLG-ED   =   "1" )
                   MOVE    HCM34S01-TIMEKBN-ED
                                       TO  WRK-DOJITUED(WRK-SKYEDDD)
               END-IF
      *
           END-IF
      *
           MOVE    HCM34S01-SANTEIYM       TO  WRK-SRYYMD
           MOVE    "01"                    TO  WRK-SRYYMD (7:2)
      *
           IF    ( HCM34S01-SANTEIYM       >=  CONST-H1804 )
            AND  ( HCM34S01-EXSYOKUJI      =   "1" )
               MOVE    1                   TO  FLG-EXSYOKUJI
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM       >=  CONST-H1804 )
            AND  ( HCM34S01-SKJFTN         =   "1" )
               MOVE    1                   TO  FLG-SKJFTN
           END-IF
      *
      *
           IF    ( HCM34S01-EXGAIHAKU      =   SPACE )
               MOVE       "1"              TO  HCM34S01-EXGAIHAKU
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-R0110 )
               MOVE    0.10            TO  WRK-RATE1
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H2604 )
               MOVE    0.08            TO  WRK-RATE1
           WHEN  OTHER
               MOVE    0.05            TO  WRK-RATE1
           END-EVALUATE
      *
           COMPUTE WRK-RATE2 = WRK-RATE1   +   1
      *
           COMPUTE WRK-RATE3 = WRK-RATE1   *   100
      *
           COMPUTE WRK-RATE4 = WRK-RATE2   *   100
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           MOVE    SPACE               TO  ASGN-CTENSU
           STRING  "CTENSU-"           DELIMITED BY SIZE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".tmp"              DELIMITED BY SIZE
           INTO    ASGN-CTENSU
           END-STRING
      *
           MOVE    SPACE               TO  ASGN-CSENTEICDCHG
           STRING  "CSENTEICDCHG-"     DELIMITED BY SIZE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".tmp"              DELIMITED BY SIZE
           INTO    ASGN-CSENTEICDCHG
           END-STRING
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           IF    ( FUNCTION TRIM(SGETTEMP-TEMPDIR) =   "/" )
               INITIALIZE                  STEMPDIR-AREA
               CALL    "ORCSTEMPDIR" USING STEMPDIR-AREA
               MOVE    1                TO FLG-SET-DIR
           END-IF
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASGN-CTENSU          TO SGETTEMP-BASENAMES (1)
           MOVE    ASGN-CSENTEICDCHG    TO SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGN-CTENSU
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ASGN-CSENTEICDCHG
      *
           OPEN    OUTPUT              CTENSU-FILE
           CLOSE   CTENSU-FILE
           OPEN    I-O                 CTENSU-FILE
      *
           OPEN    OUTPUT              CSENTEICDCHG-FILE
           CLOSE   CSENTEICDCHG-FILE
           OPEN    I-O                 CSENTEICDCHG-FILE
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計検索処理（ＫＥＹ１７）
      *****************************************************************
       1001-NYUINACCT-KEY17-SEL-SEC    SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
           INITIALIZE                          NYUINACCT-REC
           MOVE    HCM34S01-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    HCM34S01-PTID           TO  NACCT-PTID
           MOVE    HCM34S01-SANTEIYM       TO  NACCT-SRYYM
           MOVE    WRK-NACCT-ZAISKBKBN     TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1                   TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           END-IF
      *
           .
       1001-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計読み込み処理（ＫＥＹ１７）
      *****************************************************************
       1001-NYUINACCT-KEY17-READ-SEC    SECTION.
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           END-IF
      *
           .
       1001-NYUINACCT-KEY17-READ-EXT.
           EXIT.
      *****************************************************************
      *    初期処理
      *****************************************************************
       150-INIT-SEC                    SECTION.
      *
           INITIALIZE                      WRK-SYS-MIJISI-G
                                           WRK-SHOKUJIRYOYO
                                           WRK-TOKUKANRIKASAN-G
                                           WRK-SENTEI-PTFTN-G
                                           FLG-NYUINGENDAY-G
                                           FLG-BRMSGKSANTEIDAY-G
                                           ORCSNYULIFE-AREA
                                           ORCSNYULIFE2-AREA
                                           ORCSSYOTOKUAREA
                                           ORCSSKJKEEP-AREA
                                           ORCSSKJHKN-AREA
                                           ORCSKANGOGEN-AREA
      *
      *    医療機関情報取得処理
           PERFORM 1501-SYS1001-GET-SEC
      *
      *    入院基本情報取得
           PERFORM 1501-SYS5000-GET-SEC
      *
           EVALUATE    TRUE
           WHEN  (   HCM34S01-SANTEIYM   >=  CONST-H2804 )
      *
               INITIALIZE                      ORCSNYULIFE2-AREA
               MOVE    HCM34S01-HOSPNUM    TO  LNK-NYULIFE2-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-NYULIFE2-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-NYULIFE2-SANTEIYM
               CALL    "ORCSNYULIFE2"      USING
                                           ORCSNYULIFE2-AREA
                                           SPA-AREA
               IF    ( LNK-NYULIFE2-RC  NOT =   ZERO )
                   INITIALIZE              ORCSNYULIFE2-AREA
               END-IF
      *
               MOVE    LNK-NYULIFE2-HOSPNUM
                                       TO  LNK-NYULIFE-HOSPNUM
               MOVE    LNK-NYULIFE2-PTID
                                       TO  LNK-NYULIFE-PTID
               MOVE    LNK-NYULIFE2-SANTEIYM
                                       TO  LNK-NYULIFE-SANTEIYM
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    LNK-NYULIFE2-KBN (IDXD)
                                   TO  LNK-NYULIFE-KBN (IDXD)
                   MOVE    LNK-NYULIFE2-SKJKBN  (IDXD)
                                   TO  LNK-NYULIFE-SKJKBN (IDXD)
                   MOVE    LNK-NYULIFE2-FTNKBN1 (IDXD)
                                   TO  LNK-NYULIFE-FTNKBN1 (IDXD)
                   MOVE    LNK-NYULIFE2-FTNKBN2 (IDXD)
                                   TO  LNK-NYULIFE-FTNKBN2 (IDXD)
      *
                   PERFORM VARYING IDXK    FROM    1   BY  1
                           UNTIL ( IDXK    >   3 )
      *
                       IF    ( LNK-NYULIFE2-SKJFTN-SRYCD (IDXD IDXK)
                                       NOT =   SPACE )
                           MOVE    LNK-NYULIFE2-SKJFTN-SRYCD (IDXD IDXK)
                                   TO  LNK-NYULIFE-SKJFTN-SRYCD (IDXD)
                           MOVE    LNK-NYULIFE2-SKJFTN-MONEY (IDXD IDXK)
                                   TO  LNK-NYULIFE-SKJFTN-MONEY (IDXD)
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    LNK-NYULIFE2-KAN-SRYCD (IDXD)
                                   TO  LNK-NYULIFE-KAN-SRYCD (IDXD)
                   MOVE    LNK-NYULIFE2-KAN-MONEY (IDXD)
                                   TO  LNK-NYULIFE-KAN-MONEY (IDXD)
                   MOVE    LNK-NYULIFE2-KANFTN-SRYCD (IDXD)
                                   TO  LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                   MOVE    LNK-NYULIFE2-KANFTN-MONEY (IDXD)
                                   TO  LNK-NYULIFE-KANFTN-MONEY (IDXD)
               END-PERFORM
      *
               INITIALIZE                      ORCSSKJKEEP-AREA
               MOVE    HCM34S01-HOSPNUM    TO  LNK-SKJKEEP-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-SKJKEEP-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-SKJKEEP-SANTEIYM
               CALL    "ORCSSKJKEEP"       USING
                                           ORCSSKJKEEP-AREA
                                           SPA-AREA
               IF    ( LNK-SKJKEEP-RC  NOT =   ZERO )
                   INITIALIZE              ORCSSKJKEEP-AREA
               END-IF
           WHEN  (   HCM34S01-SANTEIYM   >=  CONST-H1810 )
      *
               INITIALIZE                      ORCSNYULIFE-AREA
               MOVE    HCM34S01-HOSPNUM     TO  LNK-NYULIFE-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-NYULIFE-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-NYULIFE-SANTEIYM
               CALL    "ORCSNYULIFE"       USING
                                           ORCSNYULIFE-AREA
                                           SPA-AREA
               IF    ( LNK-NYULIFE-RC  NOT =   ZERO )
      *            DISPLAY "ORCSNYULIFE ERR:" LNK-NYULIFE-RC
                   INITIALIZE              ORCSNYULIFE-AREA
               END-IF
           END-EVALUATE
      *
           IF    (   HCM34S01-SANTEIYM   >=  CONST-H1810 )
               INITIALIZE                  ORCSSYOTOKUAREA
               MOVE    HCM34S01-HOSPNUM    TO  LNK-SYOTOKU-HOSPNUM
               MOVE    HCM34S01-PTID       TO  LNK-SYOTOKU-PTID
               MOVE    HCM34S01-SANTEIYM   TO  LNK-SYOTOKU-SRYYM
               MOVE    "1"                 TO  LNK-SYOTOKU-SKJCHKFLG
               CALL    "ORCSSYOTOKU"       USING
                                           ORCSSYOTOKUAREA
                                           SPA-AREA
               IF    ( LNK-SYOTOKU-RC  NOT =         ZERO )
      *             DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
                    INITIALIZE                 ORCSSYOTOKUAREA
               END-IF
           END-IF
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    HCM34S01-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           MOVE    HCM34S01-PTID       TO  LNK-NYUINDAY-PTID
           MOVE    HCM34S01-SANTEIYM   TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
      *            DISPLAY "ORCSNYUINDAY ERR:" LNK-NYUINDAY-RC
                   INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
      *
           PERFORM VARYING     IDXD    FROM    WRK-SKYSTDD BY  1
                   UNTIL     ( IDXD    >       WRK-SKYEDDD )
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDXD) = "1" )
                   IF    ( LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDXD)
                                               =  HCM34S01-HKNCOMBI )
                     AND ( LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDXD)
                                           NOT =  HCM34S01-HKNCOMBI )
                       MOVE    2           TO  FLG-NYUINDAY(IDXD)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( HCM34S01-SANTEIYM  >= CONST-H2910 )
               INITIALIZE              ORCSSKJHKN-AREA
               MOVE    HCM34S01-HOSPNUM
                                       TO  LNK-SKJHKN-HOSPNUM
               MOVE    HCM34S01-PTID   TO  LNK-SKJHKN-PTID
               MOVE    HCM34S01-SANTEIYM
                                       TO  LNK-SKJHKN-SANTEIYM
               CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
               IF    ( LNK-SKJHKN-RC   =   ZERO )
                   CONTINUE
               ELSE
                   INITIALIZE          ORCSSKJHKN-AREA
               END-IF
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM  >=   CONST-H3004 )
               INITIALIZE                  ORCSKANGOGEN-AREA
               MOVE  HCM34S01-HOSPNUM  TO  LNK-KANGOGEN-HOSPNUM
               MOVE  HCM34S01-PTID     TO  LNK-KANGOGEN-PTID
               MOVE  HCM34S01-SANTEIYM TO  LNK-KANGOGEN-SANTEIYM
               CALL  "ORCSKANGOGEN"        USING
                                           ORCSKANGOGEN-AREA
                                           SPA-AREA
              IF     ( LNK-KANGOGEN-RC =   ZERO )
                  CONTINUE
              ELSE
                  INITIALIZE               ORCSKANGOGEN-AREA
              END-IF
           END-IF
      *
           PERFORM 1501-SHOKUJI-HOKEN-SEC
      *
           .
       150-INIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報取得処理
      *****************************************************************
       1501-SYS1001-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               INITIALIZE  SYS-1001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    0                   TO  WRK-HASU
           EVALUATE  SYS-1001-TAX-HASU-KBN
           WHEN    "1"
               MOVE    0.5             TO  WRK-HASU
           WHEN    "3"
               MOVE    0.99999         TO  WRK-HASU
           END-EVALUATE
      *
           .
       1501-SYS1001-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院基本情報取得
      *****************************************************************
       1501-SYS5000-GET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    ZERO                TO  CNT-SYS5000
      *
           INITIALIZE                      WKSYS-5000-REC
      *
           INITIALIZE                      SYS-5000-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-5000-HOSPNUM
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*%"                TO  SYS-5000-KBNCD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( HCM34S01-SANTEIYM  >=  SYS-5000-STYUKYMD (1:6))
                AND  ( HCM34S01-SANTEIYM  <=  SYS-5000-EDYUKYMD (1:6))
      *
                   PERFORM 1501-SYS5000-HEN-SEC
      *
                   MOVE    SYS-5000-REC
                                       TO  WKSYS-5000-REC
      *
               END-IF
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    WKSYS-5000-REC      TO  SYS-5000-REC
      *
           .
       1501-SYS5000-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院基本情報編集
      *****************************************************************
       1501-SYS5000-HEN-SEC            SECTION.
      *
           COMPUTE CNT-SYS5000 =   CNT-SYS5000 +   1
      *
           IF    ( CNT-SYS5000         =   1 )
               IF    ( SYS-5000-SHOKUJIRYOYOCD     =   "197000110" )
                   MOVE    1               TO   WRK-SHOKUJIRYOYO
               ELSE
                   MOVE    2               TO   WRK-SHOKUJIRYOYO
               END-IF
           END-IF
      *
           MOVE    01                      TO   WRK-STIDX
           MOVE    31                      TO   WRK-EDIDX
      *
           IF    ( HCM34S01-SANTEIYM       =    SYS-5000-STYUKYMD (1:6))
               MOVE    SYS-5000-STYUKYMD (7:2)
                                           TO   WRK-STIDX
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM       =    SYS-5000-EDYUKYMD (1:6))
               MOVE    SYS-5000-EDYUKYMD (7:2)
                                           TO   WRK-EDIDX
           END-IF
      *
           IF    ( SYS-5000-SHOKUJIRYOYOCD         =   "197000110" )
               IF    ( SYS-5000-SHOKUKBN(1)        =   "2" )
                   PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
                       MOVE    1           TO  WRK-TOKUKANRIKASAN (IDX0)
                   END-PERFORM
               END-IF
           END-IF
      *
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   4 )
      *
               IF   ( SYS-5000-GENKBN(IDX4)    =   "2" )
                   MOVE   1            TO  WRK-SYS-MIJISI(IDX4)
                   PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
      *
                       MOVE   1        TO  FLG-NYUINGENDAY (IDX4 IDX0)
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           IF   ( SYS-5000-NACCT-BRMSGKKBN     =   "2" )
               PERFORM VARYING IDX0    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX0        >   WRK-EDIDX )
      *
                   MOVE   1        TO  FLG-BRMSGKSANTEIDAY (IDX0)
      *
               END-PERFORM
           END-IF
      *
           .
       1501-SYS5000-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事保険設定処理
      *****************************************************************
       1501-SHOKUJI-HOKEN-SEC          SECTION.
      *
           PERFORM VARYING     IDXD    FROM    WRK-SKYSTDD BY  1
                   UNTIL     ( IDXD    >       WRK-SKYEDDD )
      *
               IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDXD 1)
                                       NOT =   SPACE )
                   MOVE    LNK-SKJHKN-HKNCOMBINUM  (IDXD 1)
                                       TO  WRK-HKNCOMBI-FST-G
                   IF    ( WRK-HKNCOMBI-FST =  HCM34S01-HKNCOMBI )
                       MOVE    1       TO  FLG-SKJDAY (IDXD)
                   ELSE
                       MOVE    ZERO    TO  FLG-SKJDAY (IDXD)
                   END-IF
               ELSE
                   MOVE    FLG-NYUINDAY (IDXD)
                                       TO  FLG-SKJDAY (IDXD)
               END-IF
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDXD) = "1" )
                   MOVE    LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDXD)
                                           TO  WRK-HKNCOMBI-FST
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDXD)
                                           TO  WRK-HKNCOMBI-SND
                   IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDXD 1)
                                   NOT =   SPACE )
                       MOVE    LNK-SKJHKN-HKNCOMBINUM  (IDXD 1)
                                           TO  WRK-HKNCOMBI-FST-G
                   END-IF
                   IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDXD 2)
                                   NOT =   SPACE )
                       MOVE    LNK-SKJHKN-HKNCOMBINUM  (IDXD 2)
                                           TO  WRK-HKNCOMBI-SND-G
                   END-IF
      *
                   EVALUATE    TRUE
                   WHEN  ( WRK-HKNCOMBI-SND        =  HCM34S01-HKNCOMBI)
                     AND ( WRK-HKNCOMBI-FST    NOT =  HCM34S01-HKNCOMBI)
                       MOVE    2           TO  FLG-SKJDAY(IDXD)
                       MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN(IDXD)
                                           TO  WRK-DOJITUST (IDXD)
                   WHEN  ( WRK-HKNCOMBI-SND    NOT =  HCM34S01-HKNCOMBI)
                     AND ( WRK-HKNCOMBI-FST        =  HCM34S01-HKNCOMBI)
                       MOVE    LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDXD)
                                           TO  WRK-DOJITUED (IDXD)
                   WHEN  ( WRK-HKNCOMBI-SND    NOT =  HCM34S01-HKNCOMBI)
                     AND ( WRK-HKNCOMBI-FST    NOT =  HCM34S01-HKNCOMBI)
                       MOVE    ZERO        TO  FLG-SKJDAY (IDXD)
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      D    DISPLAY "HKNCOMBI," HCM34S01-HKNCOMBI ","
      D            FLG-SKJDAY (01) ","
      D            FLG-SKJDAY (02) ","
      D            FLG-SKJDAY (03) ","
      D            FLG-SKJDAY (04) ","
      D            FLG-SKJDAY (05) ","
      D            FLG-SKJDAY (06) ","
      D            FLG-SKJDAY (07) ","
      D            FLG-SKJDAY (08) ","
      D            FLG-SKJDAY (09) ","
      D            FLG-SKJDAY (10) ","
      D            FLG-SKJDAY (11) ","
      D            FLG-SKJDAY (12) ","
      D            FLG-SKJDAY (13) ","
      D            FLG-SKJDAY (14) ","
      D            FLG-SKJDAY (15) ","
      D            FLG-SKJDAY (16) ","
      D            FLG-SKJDAY (17) ","
      D            FLG-SKJDAY (18) ","
      D            FLG-SKJDAY (19) ","
      D            FLG-SKJDAY (20) ","
      D            FLG-SKJDAY (21) ","
      D            FLG-SKJDAY (22) ","
      D            FLG-SKJDAY (23) ","
      D            FLG-SKJDAY (24) ","
      D            FLG-SKJDAY (25) ","
      D            FLG-SKJDAY (26) ","
      D            FLG-SKJDAY (27) ","
      D            FLG-SKJDAY (28) ","
      D            FLG-SKJDAY (29) ","
      D            FLG-SKJDAY (30) ","
      D            FLG-SKJDAY (31) ","
      *
           .
       1501-SHOKUJI-HOKEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    外泊チェック
           PERFORM 2001-GAIHAKU-CHK-SEC
      *
      *    入院料チェック処理
           PERFORM 2001-NYUINRYO-CHK-SEC
      *
      *    入院料編集処理（外泊も編集）
           PERFORM 2001-NYUINRYO-HEN-SEC
      *
      *    室料差額編集処理
           PERFORM 2001-BRMSAGAKU-HEN-SEC
      *
      *    食事編集処理
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1804 )
               PERFORM 2002-SYOKUJI-HEN-SEC
           ELSE
               PERFORM 2001-SYOKUJI-HEN-SEC
           END-IF
      *
      *    入院料加算編集処理
           PERFORM 2001-NYUINKSN-HEN-SEC
      *
      *    療養担当費編集処理
           PERFORM 2001-RYYTAN-HEN-SEC
      *
      *    剤ＳＯＲＴ処理
           PERFORM 2001-ZAISORT-SEC
      *
      *    返却値設定処理
           PERFORM 2001-HENKYAKU-SET-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊チェック処理
      *****************************************************************
       2001-GAIHAKU-CHK-SEC            SECTION.
      *
           INITIALIZE                          FLG-GAIHAKUDAY-G
      *
           MOVE    "2"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL     ( IDX1    >   31 )
                   IF   ( NACCT-DAY(IDX1)  >   ZERO )
                       MOVE    NACCT-DAY(IDX1)
                                           TO  FLG-GAIHAKUDAY (IDX1)
                   END-IF
               END-PERFORM
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-GAIHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       2001-NYUINRYO-CHK-SEC           SECTION.
      *
           INITIALIZE                          FLG-SENTEIDAY-G
                                               FLG-NYUINRYODAY-G
                                               FLG-TOKTEIDAY-G
      *
           PERFORM 900-NYUINACCT-KEY62-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM 20011-NYUINACT-GET-SEC
      *
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL     ( IDX1    >   31 )
                       IF   ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO   FLG-SENTEIDAY (IDX1)
                       END-IF
                   END-PERFORM
               ELSE
      *
                   PERFORM 20011-NYUINRYO-CHK-SEC
      *
      *            入院料を算定している日にちを退避
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
      *
                       IF   ( NACCT-DAY(IDX1)  >   ZERO )
      *
                           MOVE    1       TO  FLG-NYUINRYODAY (IDX1)
      *
                           IF   ( FLG-SENTEI-OK    =   ZERO )
                               MOVE    ZERO    TO  FLG-SENTEIDAY (IDX1)
                           END-IF
                       END-IF
      *
                   END-PERFORM
      *
      *            特定入院料を算定していた日にちを退避
                   IF    ( NACCT-SRYKBN        =    "92" )
                       PERFORM VARYING     IDX1    FROM    1   BY  1
                               UNTIL     ( IDX1    >   31 )
                           IF   ( NACCT-DAY(IDX1)  >   ZERO )
                            OR (( LNK-NYUINDAY-KBN(IDX1)   =   "1" )
                             AND( LNK-NYUINDAY-TANKI2-KBN
                                                (IDX1) NOT = SPACE))
                               MOVE    1   TO  FLG-TOKTEIDAY (IDX1)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY62-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key62"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUINRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       20011-NYUINRYO-CHK-SEC           SECTION.
      *
           MOVE    1               TO  FLG-SENTEI-OK
      *
           IF    ( HCM34S01-HKNNUM =   "975" )
               MOVE    ZERO        TO  FLG-SENTEI-OK
           ELSE
               PERFORM VARYING IDXN    FROM    1   BY  1
                       UNTIL ( IDXN    >   5 )
                        OR   ( NSRY-SRYCD (IDXN)   =   SPACE )
                   MOVE    NSRY-SRYCD (IDXN)
                                   TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( TNS-DATAKBN     =   1 )
                       MOVE    IDXN    TO  IDX-KHN
                       PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SENTEICDCHG =   ZERO )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-SENTEI-OK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-NYUINRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2001-NYUINRYO-HEN-SEC           SECTION.
      *
           MOVE    "1"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   CONTINUE
               ELSE
                   PERFORM 20011-NYUINRYO-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       20011-NYUINRYO-HEN-SEC          SECTION.
      *
           PERFORM 20011-NYUINACT-GET-SEC
      *
      *    入院料編集処理
           PERFORM 200110-NYUINRYO-HEN-SEC
      *
      *    外泊編集処理
           PERFORM 200111-GAIHAKU-HEN-SEC
      *
      *    選定入院料患者自己負担編集処理
           PERFORM 200111-SENTEIPTFTN-HEN-SEC
      *
           .
       20011-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       200110-NYUINRYO-HEN-SEC         SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   31 )
      *
               INITIALIZE                      WRK-ZAI-G
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-NYUINRYODAY (IDXD)  =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO )
      *
      *            入院料基本情報設定処理
                   PERFORM 2001111-NYUINKHNINF-SET-SEC
      *
                   IF    ( FLG-SENTEIDAY(IDXD) =   1 )
                       PERFORM 200111-SENTEI-HEN-SEC
                   END-IF
      *
                   MOVE    WRK-ZAI-KHNTEN
                                       TO  WRK-ZAI-TEN-TEMP
      *
                   IF    ( LNK-KANGOGEN-SRYCD (IDXD)
                                           NOT =   SPACE )
                       PERFORM 200111-YKTTYMD-HEN-SEC
                   END-IF
      *
                   COMPUTE WRK-ZAI-TEN-TEMP
                       =   WRK-ZAI-TEN-TEMP +  WRK-ZAI-KSNTEN
      *
                   IF    ( WRK-ZAI-HYOKETSU-RATE   NOT =   ZERO )
                       PERFORM 2001111-HYOKETSU-HEN-SEC
                   END-IF
      *
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN-TEMP    +   0.5
      *
                   PERFORM 200111-NYUINRYO-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200110-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       200111-NYUINRYO-HEN-SEC         SECTION.
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               MOVE    1           TO  IDXA
           ELSE
               MOVE    2           TO  IDXA
           END-IF
      *
           MOVE    ZERO            TO  WRK-ZAI-SRYCDTOTAL
                                       WRK-ZAI-CNT
           PERFORM VARYING IDXN    FROM    1   BY  1
                   UNTIL ( IDXN    >   20 )
                    OR   ( WRK-ZAI-SRYCD (IDXN)    =   SPACE )
      *
               MOVE    WRK-ZAI-SRYCD (IDXN)    TO  WRK-SRYCD
      *
               COMPUTE WRK-ZAI-SRYCDTOTAL
                   =   WRK-ZAI-SRYCDTOTAL
                   +   WRK-SRYCD-9
               COMPUTE WRK-ZAI-CNT = WRK-ZAI-CNT + 1
      *
           END-PERFORM
      *
           MOVE    ZERO            TO      IDXB
           PERFORM VARYING IDXN    FROM    1   BY  1
                   UNTIL ( IDXN    >   WNYUINRYO-OCC2-MAX (IDXA))
                    OR   ( IDXB    >   ZERO )
      *
               IF    ( WNYUINRYO-ZAITEN (IDXA IDXN)
                                   =   WRK-ZAI-TEN )
                AND  ( WNYUINRYO-SRYCDTOTAL (IDXA IDXN)
                                   =   WRK-ZAI-SRYCDTOTAL )
                AND  ( WNYUINRYO-MEISAISU (IDXA IDXN)
                                   =   WRK-ZAI-CNT )
                   MOVE    IDXN    TO  IDXB
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXB            =   ZERO )
               IF    ( WNYUINRYO-OCC2-MAX (IDXA)
                                   <   20 )
                   COMPUTE WNYUINRYO-OCC2-MAX (IDXA)
                       =   WNYUINRYO-OCC2-MAX (IDXA)
                       +   1
                   MOVE    WNYUINRYO-OCC2-MAX (IDXA)
                                   TO  IDXB
                   MOVE    NACCT-SRYKBN
                                   TO  WNYUINRYO-SRYKBN (IDXA IDXB)
                   MOVE    WRK-ZAI-ZAINUM
                                   TO  WNYUINRYO-ZAINUM (IDXA IDXB)
                   MOVE    WRK-ZAI-TEN
                                   TO  WNYUINRYO-ZAITEN (IDXA IDXB)
                   MOVE    WRK-ZAI-SRYCDTOTAL
                                   TO  WNYUINRYO-SRYCDTOTAL (IDXA IDXB)
                   MOVE    WRK-ZAI-CNT
                                   TO  WNYUINRYO-MEISAISU (IDXA IDXB)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   WRK-ZAI-CNT )
                       MOVE    WRK-ZAI-SRYCD (IDX0)
                                   TO  WNYUINRYO-SRYCD (IDXA IDXB IDX0)
                   END-PERFORM
               END-IF
           END-IF
      *
           IF    ( IDXB        >   ZERO )
               MOVE    1           TO  WNYUINRYO-DAY (IDXA IDXB IDXD)
               COMPUTE WNYUINRYO-ZAIKAISU (IDXA IDXB)
                   =   WNYUINRYO-ZAIKAISU (IDXA IDXB)
                   +   1
           END-IF
      *
           .
       200111-NYUINRYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料編集処理
      *****************************************************************
       200111-SENTEI-HEN-SEC           SECTION.
      *
           MOVE    WRK-ZAI-KHNIDX      TO  IDX-KHN
           PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
      *
           MOVE    SENCHG-IPNSRYCD     TO  WRK-SENTEIIPNCD
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-SENTEISRYCD
      *
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           IF    ( SENCHG-IPNSRYCD     =   SENCHG-SENTEISRYCD )
               COMPUTE WRK-ZAI-KHNTEN
                   = ( WRK-ZAI-KHNTEN  * 0.85 ) +  0.5
           ELSE
                MOVE   TNS-TEN         TO  WRK-ZAI-KHNTEN
           END-IF
      *
           MOVE    WRK-SENTEISRYCD     TO  WRK-ZAI-SRYCD
                                                   (IDX-KHN)
      *
           IF    ( WRK-SENTEISRYCD     =   WRK-SENTEIIPNCD )
               PERFORM 2001111-NYUINRYO-SHIFT-SEC
               COMPUTE WRK-ZAI-KHNIDX  =   WRK-ZAI-KHNIDX  +   1
               MOVE    CONST-SENTEI    TO  WRK-ZAI-SRYCD(WRK-ZAI-KHNIDX)
           END-IF
      *
           .
       200111-SENTEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    夜間看護体制特定日減算処理
      *****************************************************************
       200111-YKTTYMD-HEN-SEC         SECTION.
      *
           PERFORM 2001111-NYUINRYO-SHIFT-SEC
           MOVE    LNK-KANGOGEN-SRYCD (IDXD)
                           TO  WRK-ZAI-SRYCD (WRK-ZAI-KHNIDX + 1)
           COMPUTE WRK-ZAI-TEN-TEMP
               =   WRK-ZAI-TEN-TEMP
               * ( 100 - LNK-KANGOGEN-TEN (IDXD)) / 100
      *
           .
       200111-YKTTYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外泊編集処理
      *****************************************************************
       200111-GAIHAKU-HEN-SEC          SECTION.
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               MOVE    1               TO  IDXA
           ELSE
               MOVE    2               TO  IDXA
           END-IF
      *
      *    外泊基本点数取得
           PERFORM 200111-GAIHAKU-KHNTEN-GET-SEC
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-NYUINRYODAY (IDXD)  =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    >   ZERO )
      *
      *            外泊診療行為コード取得処理
                   PERFORM 200111-GAIHAKU-SRYCD-GET-SEC
      *
      *            外泊点数取得処理
                   EVALUATE    TRUE
                   WHEN  ( NACCT-SRYYM >=  CONST-H2804 )
                       PERFORM 2002-H28-GAIHAKU-TENSU-GET-SEC
                   WHEN    OTHER
                       PERFORM 2002-GAIHAKU-TENSU-GET-SEC
                   END-EVALUATE
      *
      *            外泊基本情報設定処理
                   PERFORM 200111-GAIHAKKHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WGAIHAKU-DAY
                                                   (IDXA IDXB IDXD)
                       COMPUTE WGAIHAKU-ZAIKAISU (IDXA IDXB)
                           =   WGAIHAKU-ZAIKAISU (IDXA IDXB)   +   1
                       IF    ( WGAIHAKU-NUM (IDXA IDXB)    =   ZERO )
                               MOVE    IDXD
                                       TO  WGAIHAKU-NUM (IDXA IDXB)
                       ELSE
                           IF    ( IDXD    <   WGAIHAKU-NUM (IDXA IDXB))
                               MOVE    IDXD
                                       TO  WGAIHAKU-NUM (IDXA IDXB)
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外泊基本点数取得処理
      *****************************************************************
       200111-GAIHAKU-KHNTEN-GET-SEC   SECTION.
      *
      *    入院基本料のコードを探す
           MOVE    ZERO                TO  WRK-KHNTEN
           MOVE    ZERO                TO  WRK-NYUKHNKSNKBN
           MOVE    ZERO                TO  WRK-NYUKHNKBN
           MOVE    SPACE               TO  WRK-CHUKSNCD
           MOVE    SPACE               TO  WRK-KHNSRYCD
           MOVE    ZERO                TO  WRK-HYOKETSU-TEN
           MOVE    SPACE               TO  WRK-HYOKETSU-SRYCD
           MOVE    ZERO                TO  IDX-KHN
           MOVE    ZERO                TO  WRK-GAIHAKU-KSNTEN
           MOVE    SPACE               TO  WRK-SENTEI-SRYCD
           MOVE    SPACE               TO  WRK-SENTEI-IPNCD
           MOVE    ZERO                TO  WRK-SENTEI-TEN
           INITIALIZE                      WRK-KSNSRYCD-G
           INITIALIZE                      SNTENSU-AREA
           INITIALIZE                      TTNS-TENSU-REC
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   5 )
                    OR   ( NSRY-SRYCD (IDX2)   =   SPACE )
               MOVE    NSRY-SRYCD (IDX2)
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   EVALUATE    TNS-DATAKBN
                   WHEN    1
      *
                       INITIALIZE                      SNTENSU-AREA
                       MOVE    TNS-SRYCD           TO  SNTENSU-SRYCD
                       MOVE    WRK-SRYYMD          TO  SNTENSU-BASEYMD
                       CALL    "ORCSNTENSU"        USING
                                                   SNTENSU-AREA
                                                   TNS-TENSU-REC
                                                   SPA-AREA
      *
                       MOVE    TNS-TENSU-REC       TO  TTNS-TENSU-REC
                       MOVE    TNS-TEN             TO  WRK-KHNTEN
                       MOVE    TNS-NYUKHNKSNKBN    TO  WRK-NYUKHNKSNKBN
                       MOVE    TNS-NYUKHNKBN       TO  WRK-NYUKHNKBN
                       MOVE    TNS-CHUKSNCD        TO  WRK-CHUKSNCD
                       MOVE    IDX2                TO  IDX-KHN
                   WHEN    2
                       EVALUATE    TNS-TENSIKIBETU
                       WHEN    3
                       WHEN    8
                           IF    ( TNS-TENSIKIBETU =   3 )
                               COMPUTE WRK-GAIHAKU-KSNTEN
                                   =   WRK-GAIHAKU-KSNTEN
                                   +   TNS-TEN
                           ELSE
                               COMPUTE WRK-GAIHAKU-KSNTEN
                                   =   WRK-GAIHAKU-KSNTEN
                                   -   TNS-TEN
                           END-IF
                           IF    ( WRK-KSNSRYCD-MAX    >=  5 )
                               CONTINUE
                           ELSE
                               COMPUTE WRK-KSNSRYCD-MAX
                                   =   WRK-KSNSRYCD-MAX
                                   +   1
                               MOVE    TNS-SRYCD
                                           TO  WRK-KSNSRYCD
                                              (WRK-KSNSRYCD-MAX)
                           END-IF
                       WHEN    6
                           MOVE  TNS-SRYCD TO  WRK-HYOKETSU-SRYCD
                           MOVE  TNS-TEN   TO  WRK-HYOKETSU-TEN
                       END-EVALUATE
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF    ( IDX-KHN             =   ZERO )
               DISPLAY "KHNRYO NOT FOUND[" NACCT-PTID "]"
                                       "[" NACCT-ZAINUM "]"
      *        MOVE    5               TO  HCM34S01-RC
      *        PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( IDX-KHN             =   ZERO )
               MOVE    SPACE           TO  WRK-TNS-SRYCD
                                           WRK-KHNSRYCD
           ELSE
               MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  WRK-TNS-SRYCD
                                           WRK-KHNSRYCD
           END-IF
      *
      *    看護比率基準未達のチェック
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H2404 )
               CONTINUE
           ELSE
               PERFORM 2001111-KNGMITATU-CHK-SEC
               IF    ( WRK-GAIHAKU-KHNTEN  NOT =   ZERO )
                   MOVE    WRK-GAIHAKU-KHNTEN  TO  WRK-KHNTEN
                   MOVE    WRK-TNS-SRYCD       TO  WRK-KHNSRYCD
               END-IF
           END-IF
      *
      *    選定入院時のコード取得
           IF    ( IDX-KHN               >   ZERO )
               PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
      *        特定入院基本料（障害者施設等入院基本料）（夜勤時間超過減算）
      *        は選定入院の対象外
               IF      NSRY-SRYCD (IDX-KHN)  =  "190161310"
                    MOVE   1                 TO  FLG-SENTEICDCHG
               END-IF
           END-IF
      *    一般病棟、特定機能病院、専門病院以外は選定入院の対象外
           IF    ( HCM34S01-SANTEIYM    >=   CONST-H3004 )
               IF    ( TTNS-CDKBN-KBNNUM =   100 OR 104 OR 105 )
                   CONTINUE
               ELSE
                   MOVE    1             TO  FLG-SENTEICDCHG
               END-IF
           END-IF
           IF    ( FLG-SENTEICDCHG     =   ZERO )
               MOVE    SENCHG-IPNSRYCD     TO  WRK-SENTEI-IPNCD
               MOVE    SENCHG-SENTEISRYCD
                                           TO  WRK-SENTEI-SRYCD
               MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    14              TO  HCM34S01-RC
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               IF    ( SENCHG-IPNSRYCD     =   SENCHG-SENTEISRYCD )
                   COMPUTE WRK-SENTEI-TEN
                       = ( WRK-KHNTEN * 0.85 ) +  0.5
               ELSE
                   MOVE    TNS-TEN         TO  WRK-SENTEI-TEN
               END-IF
           END-IF
      *
           .
       200111-GAIHAKU-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    看護比率基準未達チェック処理
      *****************************************************************
       2001111-KNGMITATU-CHK-SEC       SECTION.
      *
           INITIALIZE                      WRK-GAIHAKU-SRYCD-G
           MOVE    ZERO                TO  WRK-GAIHAKU-KHNTEN
      *
           EVALUATE  WRK-TNS-SRYCD
           WHEN  "190078210"
               MOVE  "190077410"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078310"
               MOVE  "190077510"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078410"
               MOVE  "190077610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078510"
               MOVE  "190077710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078610"
               MOVE  "190077810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078710"
               MOVE  "190077910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078810"
               MOVE  "190078010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190078910"
               MOVE  "190078110"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190082410"
               MOVE  "190081710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082510"
               MOVE  "190081810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082610"
               MOVE  "190081910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082710"
               MOVE  "190082010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082810"
               MOVE  "190082110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190082910"
               MOVE  "190082210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190083010"
               MOVE  "190082310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190084310"
               MOVE  "190083610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084410"
               MOVE  "190083710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084510"
               MOVE  "190083810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084610"
               MOVE  "190083910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084710"
               MOVE  "190084010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084810"
               MOVE  "190084110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190084910"
               MOVE  "190084210"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190087410"
               MOVE  "190085910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087510"
               MOVE  "190086010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087610"
               MOVE  "190086110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087710"
               MOVE  "190086210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087810"
               MOVE  "190086310"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190087910"
               MOVE  "190086410"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088010"
               MOVE  "190086510"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088110"
               MOVE  "190086610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088210"
               MOVE  "190086710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088310"
               MOVE  "190086810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088410"
               MOVE  "190086910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088510"
               MOVE  "190087010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088610"
               MOVE  "190087110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088710"
               MOVE  "190087210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190088810"
               MOVE  "190087310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190094710"
               MOVE  "190093710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190094810"
               MOVE  "190093810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190094910"
               MOVE  "190093910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190095010"
               MOVE  "190094010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190095110"
               MOVE  "190094110"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190797410"
               MOVE  "190796610"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797510"
               MOVE  "190796710"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797610"
               MOVE  "190796810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797710"
               MOVE  "190796910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797810"
               MOVE  "190797010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190797910"
               MOVE  "190797110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190798010"
               MOVE  "190797210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190798110"
               MOVE  "190797310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190803510"
               MOVE  "190802810"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803610"
               MOVE  "190802910"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803710"
               MOVE  "190803010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803810"
               MOVE  "190803110"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190803910"
               MOVE  "190803210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190804010"
               MOVE  "190803310"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190804110"
               MOVE  "190803410"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190091210"
               MOVE  "190090210"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190091310"
               MOVE  "190090310"       TO  WRK-GAIHAKU-SRYCD
      *
           WHEN  "190109670"
           WHEN  "190117070"
           WHEN  "190117670"
               MOVE  "190097010"       TO  WRK-GAIHAKU-SRYCD
           WHEN  "190808870"
           WHEN  "190813270"
           WHEN  "190813870"
               MOVE  "190805710"       TO  WRK-GAIHAKU-SRYCD
      *    診療所老人医療管理料
           WHEN  "190774210"
                 MOVE  "190774110"     TO  WRK-GAIHAKU-SRYCD
      *    老人性認知症疾患治療病棟入院料１
           WHEN  "190740010"
                 MOVE  "190739910"     TO  WRK-GAIHAKU-SRYCD
      *    老人性認知症疾患治療病棟入院料２
           WHEN  "190813510"
                 MOVE  "190813410"     TO  WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (NACCT-SRYYM      >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD  = "190774110" OR
                                      "190739910" OR
                                      "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           IF    ( WRK-GAIHAKU-SRYCD   NOT =   SPACE )
               MOVE    WRK-GAIHAKU-SRYCD   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   MOVE    TNS-TEN     TO  WRK-GAIHAKU-KHNTEN
               END-IF
           END-IF
      *
           .
       2001111-KNGMITATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外泊診療行為コード取得処理
      *****************************************************************
       200111-GAIHAKU-SRYCD-GET-SEC    SECTION.
      *
           INITIALIZE                      WRK-GAIHAKU-SRYCD-G
           INITIALIZE                      WRK-ZAI-G
      *
           IF    ( NACCT-SRYKBN            =   "90" )
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107290"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107390"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                       MOVE    "099999907" TO  WRK-GAIHAKU-SRYCD
                   ELSE
                       MOVE    "190111590" TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN    4
               WHEN    5
               WHEN    6
               WHEN    7
               WHEN    14
               WHEN    15
               WHEN    16
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               WHEN    8
               WHEN    9
               WHEN    10
               WHEN    11
               WHEN    12
               WHEN    13
               WHEN    17
                       PERFORM 2001111-GAIHAKU-GENSAN-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-GAIHAKUDAY (IDXD)
               WHEN    1
                   MOVE    "190107490"     TO  WRK-GAIHAKU-SRYCD
               WHEN    2
                   MOVE    "190107590"     TO  WRK-GAIHAKU-SRYCD
               WHEN    3
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                       MOVE    "099999907" TO  WRK-GAIHAKU-SRYCD
                   ELSE
                       MOVE    "190111590" TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN    4
               WHEN    5
               WHEN    6
               WHEN    7
               WHEN    14
               WHEN    15
               WHEN    16
                   IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
                       PERFORM 2001111-TAKIKAN-SEC
                   ELSE
                       PERFORM 2001111-SENTEI-TAKIKAN-SEC
                   END-IF
               WHEN    8
               WHEN    9
               WHEN    10
               WHEN    11
               WHEN    12
               WHEN    13
               WHEN    17
                       PERFORM 2001111-GAIHAKU-GENSAN-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-SENTEIDAY(IDXD) =   ZERO )
               MOVE    WRK-KHNSRYCD        TO  WRK-ZAI-KHNSRYCD
               MOVE    WRK-KHNTEN          TO  WRK-ZAI-KHNTEN
           ELSE
               MOVE    WRK-SENTEI-SRYCD    TO  WRK-ZAI-KHNSRYCD
               MOVE    WRK-SENTEI-TEN      TO  WRK-ZAI-KHNTEN
           END-IF
           IF    ( WRK-ZAI-KHNSRYCD   IS  NUMERIC )
               MOVE    WRK-ZAI-KHNSRYCD    TO  WRK-SRYCD
               COMPUTE WRK-ZAI-SRYCDTOTAL
                   =   WRK-ZAI-SRYCDTOTAL
                   +   WRK-SRYCD-9
      *
               IF    ( HCM34S01-EXGAIHAKU =    "1" )
                   COMPUTE WRK-ZAI-CNT    =    WRK-ZAI-CNT +   1
                   MOVE    WRK-ZAI-KHNSRYCD
                                          TO   WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
               END-IF
      *
               IF    ( FLG-SENTEIDAY(IDXD) NOT =   ZERO )
                AND  ( WRK-SENTEI-IPNCD        =   WRK-SENTEI-SRYCD )
                   MOVE    CONST-SENTEI   TO  WRK-SRYCD
                   COMPUTE WRK-ZAI-SRYCDTOTAL
                       =   WRK-ZAI-SRYCDTOTAL
                       +   WRK-SRYCD-9
      *
                   IF    ( HCM34S01-EXGAIHAKU  =   "1" )
                       COMPUTE WRK-ZAI-CNT     =   WRK-ZAI-CNT +   1
                       MOVE    CONST-SENTEI    TO  WRK-ZAI-SRYCD
                                                    (WRK-ZAI-CNT)
                   END-IF
               END-IF
      *
               IF    ( LNK-KANGOGEN-SRYCD (IDXD)   NOT =   SPACE )
                   MOVE    LNK-KANGOGEN-SRYCD (IDXD)
                                               TO  WRK-SRYCD
                   COMPUTE WRK-ZAI-SRYCDTOTAL
                       =   WRK-ZAI-SRYCDTOTAL
                       +   WRK-SRYCD-9
      *
                   COMPUTE WRK-ZAI-CNT     =   WRK-ZAI-CNT +   1
                   MOVE    LNK-KANGOGEN-SRYCD (IDXD)
                                               TO  WRK-ZAI-SRYCD
                                                     (WRK-ZAI-CNT)
               END-IF
      *
           END-IF
      *
           PERFORM VARYING   IDXI  FROM    1   BY  1
                   UNTIL   ( IDXI  >   2 )
               IF    ( WRK-GAIHAKU-SRYCDL (IDXI)   IS  NUMERIC )
                   MOVE    WRK-GAIHAKU-SRYCDL (IDXI)
                                               TO  WRK-SRYCD
                   COMPUTE WRK-ZAI-SRYCDTOTAL
                       =   WRK-ZAI-SRYCDTOTAL
                       +   WRK-SRYCD-9
      *
                   COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
                   MOVE    WRK-GAIHAKU-SRYCDL (IDXI)
                                               TO  WRK-ZAI-SRYCD
                                                    (WRK-ZAI-CNT)
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-GAIHAKUDAY (IDXD)  >=   4 )
      *
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   WRK-KSNSRYCD-MAX )
                   IF    ( WRK-KSNSRYCD (IDX8)   IS  NUMERIC )
                       MOVE    WRK-KSNSRYCD (IDX8)
                                       TO  WRK-SRYCD
                       COMPUTE WRK-ZAI-SRYCDTOTAL
                           =   WRK-ZAI-SRYCDTOTAL
                           +   WRK-SRYCD-9
      *
                       COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
                       MOVE    WRK-KSNSRYCD (IDX8)
                                           TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
      *
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-HYOKETSU-SRYCD       IS  NUMERIC )
                   MOVE    WRK-HYOKETSU-SRYCD   TO  WRK-SRYCD
                   COMPUTE WRK-ZAI-SRYCDTOTAL
                       =   WRK-ZAI-SRYCDTOTAL
                       +   WRK-SRYCD-9
      *
                   COMPUTE WRK-ZAI-CNT =   WRK-ZAI-CNT +   1
                   MOVE    WRK-HYOKETSU-SRYCD
                                           TO  WRK-ZAI-SRYCD
                                                (WRK-ZAI-CNT)
      *
               END-IF
           END-IF
      *
           .
       200111-GAIHAKU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理(H28.3迄)
      *****************************************************************
       2001111-TAKIKAN-SEC             SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H3004 )
               PERFORM 20013-TAKIKAN-SEC
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H2804 )
               PERFORM 20012-H28-TAKIKAN-SEC
           WHEN    OTHER
               PERFORM 20012-TAKIKAN-SEC
           END-EVALUATE
      *
           .
       2001111-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理(H30.04以降)
      *****************************************************************
       20013-TAKIKAN-SEC               SECTION.
      *
           IF    ( SNTENSU-DEKIDAKAKBN     =   "1" )
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN     5
                   MOVE   "190141590"    TO   WRK-GAIHAKU-SRYCD
               WHEN     14
                   MOVE   "190213790"    TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           ELSE
               IF    ( NACCT-SRYKBN    =   "90" )
                   EVALUATE    SNTENSU-KBNNUM
                   WHEN    109
                       EVALUATE   FLG-GAIHAKUDAY(IDXD)
                       WHEN     4
                           MOVE   "190108890"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     5
                           MOVE   "190142190"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     6
                           MOVE   "190109090"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     14
                           MOVE   "190214290"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     15
                           MOVE   "190213890"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     16
                           MOVE   "190214090"  TO   WRK-GAIHAKU-SRYCD
                       END-EVALUATE
                   WHEN    OTHER
                       EVALUATE   FLG-GAIHAKUDAY(IDXD)
                       WHEN     4
                           MOVE   "190108890"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     5
                           MOVE   "190142190"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     14
                           MOVE   "190214290"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     15
                           MOVE   "190213890"  TO   WRK-GAIHAKU-SRYCD
                       END-EVALUATE
                   END-EVALUATE
               ELSE
                   EVALUATE    SNTENSU-KBNNUM
                   WHEN    312
                   WHEN    314
                   WHEN    318
                       EVALUATE   FLG-GAIHAKUDAY(IDXD)
                       WHEN     4
                           MOVE   "190108790"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     5
                           MOVE   "190142090"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     6
                           MOVE   "190182990"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     14
                           MOVE   "190214390"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     15
                           MOVE   "190213990"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     16
                           MOVE   "190214190"  TO   WRK-GAIHAKU-SRYCD
                       END-EVALUATE
                   WHEN    OTHER
                       EVALUATE   FLG-GAIHAKUDAY(IDXD)
                       WHEN     4
                           MOVE   "190108790"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     5
                           MOVE   "190142090"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     14
                           MOVE   "190214390"  TO   WRK-GAIHAKU-SRYCD
                       WHEN     15
                           MOVE   "190213990"  TO   WRK-GAIHAKU-SRYCD
                       END-EVALUATE
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       20013-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理
      *    (H28.4 - H30.3)
      *****************************************************************
       20012-H28-TAKIKAN-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN
           WHEN   201  THRU  206
           WHEN   208  THRU  210
           WHEN   212  THRU  217
           WHEN   221
           WHEN   227  THRU  229
           WHEN   232  THRU  236
           WHEN   238  THRU  239
           WHEN   241  THRU  249
           WHEN   250  THRU  254
           WHEN   257  THRU  267
           WHEN   268  THRU  270
               IF    WRK-NYUKHNKSNKBN = 221 OR 232 OR 217
                   EVALUATE   FLG-GAIHAKUDAY(IDXD)
                   WHEN     4
                       MOVE   "190108790"    TO   WRK-GAIHAKU-SRYCD
                   WHEN     5
                       MOVE   "190142090"    TO   WRK-GAIHAKU-SRYCD
                   WHEN     6
                       MOVE   "190182990"    TO   WRK-GAIHAKU-SRYCD
                   END-EVALUATE
               ELSE
                   EVALUATE   FLG-GAIHAKUDAY(IDXD)
                   WHEN     4
                       MOVE   "190108790"    TO   WRK-GAIHAKU-SRYCD
                   WHEN     5
                       MOVE   "190142090"    TO   WRK-GAIHAKU-SRYCD
                   END-EVALUATE
               END-IF
           WHEN   004  THRU  008
           WHEN   010  THRU  012
           WHEN   152
           WHEN   173  THRU  190
           WHEN   196
           WHEN   199
           WHEN   048  THRU  049
           WHEN   051  THRU  054
           WHEN   057  THRU  068
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN     4
                   MOVE   "190108890"    TO   WRK-GAIHAKU-SRYCD
               WHEN     5
                   MOVE   "190142190"    TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN   153  THRU  158
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN     4
                   MOVE   "190108890"    TO   WRK-GAIHAKU-SRYCD
               WHEN     5
                   MOVE   "190142190"    TO   WRK-GAIHAKU-SRYCD
               WHEN     6
                   MOVE   "190109090"    TO   WRK-GAIHAKU-SRYCD
              END-EVALUATE
           WHEN   226
           WHEN   240
               CONTINUE
           WHEN   OTHER
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN     5
                   MOVE   "190141590"    TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       20012-H28-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理(H28.3迄)
      *****************************************************************
       20012-TAKIKAN-SEC               SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN
           WHEN    099 THRU    100
               IF    ( HCM34S01-SANTEIYM   >=  CONST-H2204 )
                    EVALUATE   FLG-GAIHAKUDAY (IDXD)
                    WHEN   4
                        MOVE   "190108890" TO  WRK-GAIHAKU-SRYCD
                    WHEN   5
                        MOVE   "190142190" TO  WRK-GAIHAKU-SRYCD
                    END-EVALUATE
               END-IF
           WHEN    201 THRU    219
           WHEN    221 THRU    222
           WHEN    224
           WHEN    227 THRU    229
           WHEN    230 THRU    239
           WHEN    241 THRU    249
           WHEN    250 THRU    255
           WHEN    256
           WHEN    257 THRU  259
           WHEN    260 THRU  261
           WHEN    262 THRU  265
           WHEN    266 THRU  267
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN   4
                   MOVE   "190108790" TO  WRK-GAIHAKU-SRYCD
               WHEN   5
                   MOVE   "190142090" TO  WRK-GAIHAKU-SRYCD
               WHEN   6
                   IF    ( WRK-NYUKHNKSNKBN
                       =   221 OR 232
                        OR 229 OR 238 OR 215 OR 216 OR 217 OR 239
                        OR 252 )
                       MOVE   "190153190" TO   WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   7
                   IF    ( WRK-NYUKHNKSNKBN
                       =   221 OR 232 )
                      MOVE   "190153390"  TO   WRK-GAIHAKU-SRYCD
                   END-IF
               END-EVALUATE
           WHEN    013 THRU    021
           WHEN    101 THRU    109
           WHEN    147 THRU    152
           WHEN    173 THRU    190
           WHEN    196
           WHEN    199
           WHEN    004  THRU  008
           WHEN    010  THRU  012
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN   4
                   MOVE   "190108890"  TO  WRK-GAIHAKU-SRYCD
               WHEN   5
                   MOVE   "190142190"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    075 THRU    080
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190108990" TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    085 THRU    086
           WHEN    132 THRU    133
           WHEN    153 THRU    158
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     4
                   MOVE   "190109090"  TO  WRK-GAIHAKU-SRYCD
               WHEN     5
                   MOVE   "190142290"  TO  WRK-GAIHAKU-SRYCD
               WHEN     7
                   IF    ( WRK-NYUKHNKSNKBN  >= 153 )
                     AND ( WRK-NYUKHNKSNKBN  <= 158 )
                       MOVE   "190153290"  TO  WRK-GAIHAKU-SRYCD
                   END-IF
               END-EVALUATE
           WHEN   226
           WHEN   240
               CONTINUE
           WHEN   031
           WHEN   033
           WHEN   034
           WHEN   035
           WHEN   038
           WHEN   170
           WHEN   191
           WHEN   017 THRU 020
      *
           WHEN   022
           WHEN   023
           WHEN   024
           WHEN   025
           WHEN   026
           WHEN   029
           WHEN   136
           WHEN   168
           WHEN   169
           WHEN   013 THRU 016
      *
           WHEN   081
           WHEN   083
           WHEN   172
           WHEN   032 THRU 037
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"      TO  WRK-GAIHAKU-SRYCD
               WHEN     7
                   MOVE   "190153090"      TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
      *
           WHEN   OTHER
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"  TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       20012-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理（選定入院時）
      *****************************************************************
       2001111-SENTEI-TAKIKAN-SEC      SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( HCM34S01-SANTEIYM  >=  CONST-H3004 )
               PERFORM 20011111-SENTEI-TAKIKAN-SEC
           WHEN  OTHER
               PERFORM 20011112-SENTEI-TAKIKAN-SEC
           END-EVALUATE
      *
           .
       2001111-SENTEI-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理（選定入院時）
      *    （H30.4.1以降）
      *****************************************************************
       20011111-SENTEI-TAKIKAN-SEC      SECTION.
      *
           EVALUATE    FLG-GAIHAKUDAY (IDXD)
           WHEN   5
               MOVE    "190141590"     TO  WRK-GAIHAKU-SRYCD
           WHEN   14
               MOVE    "190213790"     TO  WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           .
       20011111-SENTEI-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理（選定入院時）
      *    （H30.3.31迄）
      *****************************************************************
       20011112-SENTEI-TAKIKAN-SEC      SECTION.
      *
           EVALUATE    WRK-NYUKHNKSNKBN
           WHEN    099 THRU  100
               IF    ( HCM34S01-SANTEIYM   >=  CONST-H2204 )
                    EVALUATE   FLG-GAIHAKUDAY (IDXD)
                    WHEN   4
                        MOVE   "190108890" TO  WRK-GAIHAKU-SRYCD
                    WHEN   5
                        MOVE   "190142190" TO  WRK-GAIHAKU-SRYCD
                    END-EVALUATE
               END-IF
           WHEN    201 THRU  219
           WHEN    221 THRU  222
           WHEN    224
           WHEN    227 THRU  229
           WHEN    230 THRU  239
           WHEN    241 THRU  249
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116590"     TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    013 THRU    021
           WHEN    101 THRU    109
           WHEN    147 THRU    152
           WHEN    173 THRU    190
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116690"     TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    075 THRU  080
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN    4
                   MOVE    "190116890" TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN    085 THRU    086
           WHEN    132 THRU    133
           WHEN    153 THRU    158
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     4
                   MOVE   "190116790"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN   226
           WHEN   240
               CONTINUE
           WHEN   OTHER
               EVALUATE   FLG-GAIHAKUDAY (IDXD)
               WHEN     5
                   MOVE   "190141590"  TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           END-EVALUATE
      *
           .
       20011112-SENTEI-TAKIKAN-EXT.
           EXIT.
      *****************************************************************
      *    特定曜日・特定時間減算診療コード取得
      *****************************************************************
       2001111-GAIHAKU-GENSAN-SEC      SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H3004 )
               PERFORM 20021-GAIHAKU-GENSAN-SEC
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H2804 )
               PERFORM 2002-H28-GAIHAKU-GENSAN-SEC
           WHEN    OTHER
               PERFORM 2002-GAIHAKU-GENSAN-SEC
           END-EVALUATE
      *
           .
       2001111-GAIHAKU-GENSAN-EXT.
           EXIT.
      *****************************************************************
      *    特定曜日・特定時間減算診療コード取得(H30.4以降)
      *****************************************************************
       20021-GAIHAKU-GENSAN-SEC        SECTION.
      *
           EVALUATE   FLG-GAIHAKUDAY (IDXD)
           WHEN     8
               EVALUATE   TTNS-CDKBN-KBNNUM
               WHEN   100
                   IF   ( SNTENSU-KBNNUM       =   100 )
                       IF   ( SNTENSU-TOKUBETSUKBN NOT =   SPACE )
                           MOVE  "190161570"   TO  WRK-GAIHAKU-SRYCD
                       ELSE
                           MOVE  "190161470"   TO  WRK-GAIHAKU-SRYCD
                       END-IF
                   END-IF
               WHEN   101
                   IF   ( SNTENSU-KBNNUM   =    100 )
                       MOVE   "190198570"  TO   WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   104
                   MOVE   "190161870"      TO   WRK-GAIHAKU-SRYCD
                WHEN  105
                   MOVE   "190162070"      TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN     9
           WHEN    13
           WHEN    17
               EVALUATE   TTNS-CDKBN-KBNNUM
               WHEN   100
                   IF   ( SNTENSU-KBNNUM       =   100 )
                       IF   ( SNTENSU-TOKUBETSUKBN NOT =   SPACE )
                           MOVE  "190161770"   TO  WRK-GAIHAKU-SRYCD
                       ELSE
                           MOVE  "190161670"   TO  WRK-GAIHAKU-SRYCD
                       END-IF
                   END-IF
               WHEN   101
                   IF   ( SNTENSU-KBNNUM   =    100 )
                       MOVE   "190198670"  TO   WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   104
                   MOVE   "190161970"      TO   WRK-GAIHAKU-SRYCD
                WHEN  105
                   MOVE   "190162170"      TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
               IF    ( FLG-GAIHAKUDAY (IDXD) = 13 OR 17)
                   PERFORM   20021-TAKIKAN2-SEC
               END-IF
           END-EVALUATE
      *
           .
       20021-GAIHAKU-GENSAN-EXT.
           EXIT.
      *****************************************************************
      *    特定曜日・特定時間減算診療コード取得(H28.4 - H30.3)
      *****************************************************************
       2002-H28-GAIHAKU-GENSAN-SEC     SECTION.
      *
           EVALUATE   FLG-GAIHAKUDAY (IDXD)
           WHEN     8
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   EVALUATE   WRK-NYUKHNKSNKBN
                   WHEN     9
                   WHEN   166   THRU  167
                   WHEN   197   THRU  198
                       MOVE  "190161570" TO   WRK-GAIHAKU-SRYCD
                   WHEN   OTHER
                       MOVE  "190161470" TO   WRK-GAIHAKU-SRYCD
                   END-EVALUATE
               WHEN   "9026"
                   MOVE   "190161870"    TO   WRK-GAIHAKU-SRYCD
                WHEN   "9030"
                   MOVE   "190162070"    TO   WRK-GAIHAKU-SRYCD
               WHEN   "9090"
                   MOVE   "190198570"    TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
           WHEN     9
           WHEN    13
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   EVALUATE   WRK-NYUKHNKSNKBN
                   WHEN     9
                   WHEN   166   THRU  167
                   WHEN   197   THRU  198
                       MOVE  "190161770" TO   WRK-GAIHAKU-SRYCD
                   WHEN   OTHER
                       MOVE  "190161670" TO   WRK-GAIHAKU-SRYCD
                   END-EVALUATE
               WHEN   "9026"
                   MOVE   "190161970"    TO   WRK-GAIHAKU-SRYCD
               WHEN   "9030"
                   MOVE   "190162170"    TO   WRK-GAIHAKU-SRYCD
               WHEN   "9090"
                   MOVE   "190198670"    TO   WRK-GAIHAKU-SRYCD
               END-EVALUATE
               IF    ( FLG-GAIHAKUDAY (IDXD) = 13 )
                   PERFORM   2002-TAKIKAN2-SEC
               END-IF
           END-EVALUATE
      *
           .
       2002-H28-GAIHAKU-GENSAN-EXT.
           EXIT.
      *****************************************************************
      *    特定曜日・特定時間減算診療コード取得(H28.3迄)
      *****************************************************************
       2002-GAIHAKU-GENSAN-SEC      SECTION.
      *
           EVALUATE   FLG-GAIHAKUDAY (IDXD)
           WHEN     8
           WHEN     10
           WHEN     11
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   IF   ( HCM34S01-SANTEIYM   >=  CONST-H2604 )
                       IF  (  WRK-NYUKHNKBN    =  79 OR 88 )
                        OR (  WRK-KHNSRYCD     =  "190799510" )
                           MOVE   "190161570"  TO  WRK-GAIHAKU-SRYCD
                       ELSE
                           MOVE   "190161470"  TO  WRK-GAIHAKU-SRYCD
                       END-IF
                   ELSE
                       MOVE   "190161470"  TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   "9018"
                   MOVE   "190161570"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9026"
                   MOVE   "190161870"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9030"
                   MOVE   "190162070"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
               IF    ( FLG-GAIHAKUDAY (IDXD)   =   10 OR 11 )
                   PERFORM 2002-TAKIKAN2-SEC
               END-IF
           WHEN     9
           WHEN     12
           WHEN     13
               EVALUATE   WRK-CHUKSNCD
               WHEN   "9017"
                   IF   ( HCM34S01-SANTEIYM   >=  CONST-H2604 )
                       IF  (  WRK-NYUKHNKBN    =  79 OR 88 )
                        OR (  WRK-KHNSRYCD     =  "190799510" )
                           MOVE   "190161770"  TO  WRK-GAIHAKU-SRYCD
                       ELSE
                           MOVE   "190161670"  TO  WRK-GAIHAKU-SRYCD
                       END-IF
                   ELSE
                       MOVE   "190161670"  TO  WRK-GAIHAKU-SRYCD
                   END-IF
               WHEN   "9018"
                   MOVE   "190161770"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9026"
                   MOVE   "190161970"  TO  WRK-GAIHAKU-SRYCD
               WHEN   "9030"
                  MOVE    "190162170"  TO  WRK-GAIHAKU-SRYCD
               END-EVALUATE
               IF    ( FLG-GAIHAKUDAY (IDXD)   =   12 OR 13 )
                   PERFORM 2002-TAKIKAN2-SEC
               END-IF
           END-EVALUATE
      *
           .
       2002-GAIHAKU-GENSAN-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理(H30.4以降）
      *****************************************************************
       20021-TAKIKAN2-SEC               SECTION.
      *
           EVALUATE   FLG-GAIHAKUDAY (IDXD)
           WHEN   13
               MOVE   "190141590"      TO   WRK-TAKIKAN2-SRYCD
           WHEN   17
               MOVE   "190213790"      TO   WRK-TAKIKAN2-SRYCD
           END-EVALUATE
      *
           .
       20021-TAKIKAN2-EXT.
           EXIT.
      *****************************************************************
      *    他医療機関受診時外泊診療行為コード取得処理(H30.3迄）
      *****************************************************************
       2002-TAKIKAN2-SEC               SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( HCM34S01-SANTEIYM   >=  CONST-H2804 )
                EVALUATE   FLG-GAIHAKUDAY (IDXD)
                WHEN   13
                    MOVE   "190141590"    TO   WRK-TAKIKAN2-SRYCD
                END-EVALUATE
           WHEN    OTHER
               EVALUATE   WRK-NYUKHNKSNKBN
               WHEN   099  THRU  100
                   EVALUATE   FLG-GAIHAKUDAY (IDXD)
                   WHEN    10
                   WHEN    12
                       MOVE   "190108890"  TO  WRK-TAKIKAN2-SRYCD
                   WHEN    11
                   WHEN    13
                       MOVE   "190142190"  TO  WRK-TAKIKAN2-SRYCD
                   END-EVALUATE
               WHEN   OTHER
                   EVALUATE   FLG-GAIHAKUDAY (IDXD)
                   WHEN    11
                   WHEN    13
                       MOVE   "190141590"  TO  WRK-TAKIKAN2-SRYCD
                   END-EVALUATE
               END-EVALUATE
           END-EVALUATE
      *
           .
       2002-TAKIKAN2-EXT.
           EXIT.
      *****************************************************************
      *    外泊点数取得処理
      *****************************************************************
       2002-H28-GAIHAKU-TENSU-GET-SEC  SECTION.
      *
           MOVE    WRK-ZAI-KHNTEN      TO  WRK-ZAI-TEN-TEMP
      *
      D    DISPLAY "WRK-ZAI-TEN-TEMP=" WRK-ZAI-TEN-TEMP
      *
           IF    ( LNK-KANGOGEN-SRYCD (IDXD)   NOT =   SPACE )
               COMPUTE WRK-ZAI-TEN-TEMP
                   =   WRK-ZAI-TEN-TEMP
                   * ( 100 - LNK-KANGOGEN-TEN (IDXD) ) / 100
           END-IF
      *
      D    DISPLAY "WRK-ZAI-TEN-TEMP=" WRK-ZAI-TEN-TEMP
      *
           IF  ( FLG-SENTEIDAY (IDXD) NOT  =  ZERO )
      *
               PERFORM 200111-SENTEI-KEISU-GET-SEC
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    1
               WHEN    3
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP *   WRK-SENTEI-KEISU )
               WHEN    4
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.6 )
               WHEN    5
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.9 )
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
               WHEN    13
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP     *   0.9  )
               WHEN    14
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.95  )
               WHEN    15
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.65 )
               WHEN    17
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP     *   0.95 )
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)   >=   4 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       =   WRK-ZAI-TEN-TEMP
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN-TEMP
                       =   WRK-ZAI-TEN-TEMP
                       - ( WRK-ZAI-TEN-TEMP * (WRK-HYOKETSU-TEN / 100))
               END-IF
           ELSE
               EVALUATE  FLG-GAIHAKUDAY(IDXD)
               WHEN   1
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.15)
               WHEN   2
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.3 )
               WHEN   4
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.6 )
               WHEN   5
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.9 )
               WHEN   6
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.8 )
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
               WHEN    13
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.9  )
               WHEN    14
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.95  )
               WHEN    15
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.65 )
               WHEN    16
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.85 )
               WHEN    17
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.92 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       = ( WRK-ZAI-TEN-TEMP  *   0.95 )
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)   >=   4 )
                   COMPUTE WRK-ZAI-TEN-TEMP
                       =   WRK-ZAI-TEN-TEMP
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN-TEMP
                       =   WRK-ZAI-TEN-TEMP
                       - ( WRK-ZAI-TEN-TEMP * (WRK-HYOKETSU-TEN / 100))
               END-IF
           END-IF
      *
      D    DISPLAY "WRK-ZAI-TEN-TEMP=" WRK-ZAI-TEN-TEMP
      *
           COMPUTE WRK-ZAI-TEN = WRK-ZAI-TEN-TEMP + 0.5
      *
           .
       2002-H28-GAIHAKU-TENSU-GET-EXT.
           EXIT.
      *****************************************************************
      *    外泊点数取得処理
      *****************************************************************
       2002-GAIHAKU-TENSU-GET-SEC    SECTION.
      *
           IF  ( FLG-SENTEIDAY (IDXD) NOT  =  ZERO )
      *
               PERFORM 200111-SENTEI-KEISU-GET-SEC
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    1
               WHEN    3
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   WRK-SENTEI-KEISU )  + 0.5
               WHEN    4
                   IF    ( NACCT-SRYYM     >=  CONST-H2204 )
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
                   END-IF
               WHEN    5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.7 )   +  0.5
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
               WHEN    10
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.3  )   +  0.5
               WHEN    11
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.7  )   +  0.5
               WHEN    12
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.3  )   +  0.5
               WHEN    13
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.7  )   +  0.5
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)   >=   4 )
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       - ( WRK-ZAI-TEN * ( WRK-HYOKETSU-TEN / 100 ))
                       +   0.5
               END-IF
           ELSE
               EVALUATE  FLG-GAIHAKUDAY(IDXD)
               WHEN   1
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.15)   +  0.5
               WHEN   2
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
               WHEN   4
                   IF    ( NACCT-SRYYM     <=  "200403" )
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.15)   +  0.5
                   ELSE
                       COMPUTE WRK-ZAI-TEN
                           = ( WRK-ZAI-KHNTEN  *   0.3 )   +  0.5
                   END-IF
               WHEN   5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.7 )   +  0.5
               WHEN   6
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.45 )  +  0.5
               WHEN   7
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.85 )   +  0.5
               WHEN    8
               WHEN    9
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
               WHEN    10
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.3  )   +  0.5
               WHEN    11
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.7  )   +  0.5
               WHEN    12
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.3  )   +  0.5
               WHEN    13
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-KHNTEN  *   0.92 )   +  0.5
                   COMPUTE WRK-ZAI-TEN
                       = ( WRK-ZAI-TEN     *   0.7  )   +  0.5
               END-EVALUATE
      *
               IF    ( FLG-GAIHAKUDAY(IDXD)   >=   4 )
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       +   WRK-GAIHAKU-KSNTEN
                   COMPUTE WRK-ZAI-TEN
                       =   WRK-ZAI-TEN
                       - ( WRK-ZAI-TEN * ( WRK-HYOKETSU-TEN / 100 ))
                       +   0.5
               END-IF
           END-IF
      *
           .
       2002-GAIHAKU-TENSU-GET-EXT.
           EXIT.
      *****************************************************************
      *    選定外泊時の係数セット処理
      *****************************************************************
       200111-SENTEI-KEISU-GET-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-SENTEI-KEISU
      *
      *    選定入院の場合の係数を求めておく
           EVALUATE    TRUE
           WHEN (  NACCT-SRYYM         <=  "200303" )
               MOVE   0.1           TO     WRK-SENTEI-KEISU
           WHEN (  NACCT-SRYYM         >=    "200304" )
            AND (  NACCT-SRYYM         <=    "200403" )
               MOVE   0.05           TO    WRK-SENTEI-KEISU
           WHEN (  NACCT-SRYYM         >=    "200404" )
               MOVE   ZERO          TO     WRK-SENTEI-KEISU
           END-EVALUATE
           .
      *
       200111-SENTEI-KEISU-GET-EXT.
           EXIT.
      *****************************************************************
      *    外泊基本情報編集処理
      *****************************************************************
       200111-GAIHAKKHNINF-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WGAIHAKU-OCC2-MAX (IDXA) )
                     OR  ( IDXB    >   CONST-WGAIHAKU-MAX )
                     OR (( WRK-ZAI-SRYCDTOTAL
                                   =   WGAIHAKU-SRYCDTOTAL (IDXA IDXB)) 
                      AND( WRK-ZAI-TEN
                                   =   WGAIHAKU-ZAITEN(IDXA IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WGAIHAKU-OCC2-MAX (IDXA) )
               COMPUTE WGAIHAKU-OCC2-MAX (IDXA)
                   =   WGAIHAKU-OCC2-MAX (IDXA)    +   1
               IF    ( WGAIHAKU-OCC2-MAX (IDXA)
                                           >   CONST-WGAIHAKU-MAX )
                   MOVE    CONST-WGAIHAKU-MAX
                                       TO  WGAIHAKU-OCC2-MAX (IDXA)
                   MOVE    1           TO  FLG-OVER
               ELSE
                   MOVE    WGAIHAKU-OCC2-MAX (IDXA)    TO  IDXB
                   MOVE    NACCT-SRYKBN
                                       TO  WGAIHAKU-SRYKBN (IDXA IDXB)
                   MOVE    WRK-ZAI-TEN
                                       TO  WGAIHAKU-ZAITEN (IDXA IDXB)
                   MOVE    WRK-ZAI-SRYCDTOTAL
                                       TO  WGAIHAKU-SRYCDTOTAL
                                                           (IDXA IDXB)
      *
                   PERFORM VARYING IDX9    FROM    1   BY  1
                           UNTIL ( IDX9    >   WRK-ZAI-CNT )
      *
                       MOVE    WRK-ZAI-SRYCD (IDX9)
                                   TO  WGAIHAKU-SRYCD (IDXA IDXB IDX9)
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       200111-GAIHAKKHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料基本情報設定処理
      *****************************************************************
       2001111-NYUINKHNINF-SET-SEC     SECTION.
      *
           MOVE    IDXD                TO  WRK-ZAI-ZAINUM
           MOVE    NACCT-SRYKBN        TO  WRK-ZAI-SRYKBN
           MOVE    1                   TO  WRK-ZAI-KHNIDX
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC                >   5 )
                    OR   ( NSRY-SRYCD (IDXC)   =   SPACE )
      *
               MOVE    NSRY-SRYCD (IDXC)
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-DATAKBN =   1 )
                       MOVE    TNS-TEN     TO  WRK-ZAI-KHNTEN
                       MOVE    IDXC        TO  WRK-ZAI-KHNIDX
                   ELSE
                       EVALUATE  TNS-TENSIKIBETU
                       WHEN     3
                           COMPUTE WRK-ZAI-KSNTEN
                               =   WRK-ZAI-KSNTEN  +   TNS-TEN
                       WHEN     8
                           COMPUTE WRK-ZAI-KSNTEN
                               =   WRK-ZAI-KSNTEN
                               + ( TNS-TEN *   -1 )
                       WHEN     6
                           MOVE  TNS-TEN   TO  WRK-ZAI-HYOKETSU-RATE
                       END-EVALUATE
                   END-IF
                   MOVE    NSRY-SRYCD (IDXC)
                                           TO  WRK-ZAI-SRYCD (IDXC)
               END-IF
           END-PERFORM
      *
           .
       2001111-NYUINKHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為取得処理
      *****************************************************************
       20011-NYUINACT-GET-SEC          SECTION.
      *
      *    入院診療行為検索処理
           MOVE    ZERO                TO  FLG-NYUINACT
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM        TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACT
               INITIALIZE                  NYUINACT-REC
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           END-IF
      *
      *    入院行為クローズ
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYUINACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院料剤編集処理
      *****************************************************************
       2001111-NYUINRYO-SHIFT-SEC          SECTION.
      *
           PERFORM VARYING IDXS    FROM    19  BY  -1
                   UNTIL ( IDXS    <   WRK-ZAI-KHNIDX )
      *
               MOVE    WRK-ZAI-SRYCD (IDXS)
                                   TO  WRK-ZAI-SRYCD (IDXS + 1)
      *
           END-PERFORM
      *
           .
       2001111-NYUINRYO-SHIFT-EXT.
           EXIT.
      *****************************************************************
      *    標欠・定数超過編集処理
      *****************************************************************
       2001111-HYOKETSU-HEN-SEC          SECTION.
      *
           IF    ( WRK-ZAI-HYOKETSU-RATE NOT =   ZERO )
      *
               COMPUTE WRK-ZAI-TEN-TEMP
                   =   WRK-ZAI-TEN-TEMP
                   -   WRK-ZAI-TEN-TEMP * (WRK-ZAI-HYOKETSU-RATE / 100)
           END-IF
      *
           .
       2001111-HYOKETSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料患者自己負担編集処理
      *****************************************************************
       200111-SENTEIPTFTN-HEN-SEC      SECTION.
      *
      *    入院患者の老人判定
           INITIALIZE   ORCSAGECHKAREA
           MOVE    HCM34S01-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    HCM34S01-PTID       TO  AGECHK-PTID
      *
           MOVE    HCM34S01-SANTEIYM   TO  WRK-YYMM
           MOVE    1                   TO  WRK-DD
           PERFORM VARYING IDXD    FROM    1   BY  1
               UNTIL ( IDXD        >   31 )
               IF    ( FLG-NYUINDAY (IDXD) =   1 )
                   MOVE    IDXD        TO  WRK-DD
                   MOVE    31          TO  IDXD
               END-IF
           END-PERFORM
           MOVE    WRK-YMD             TO  AGECHK-SRYYMD
      *
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      ( AGECHK-RC    =    ZERO )
               CONTINUE
           ELSE
               INITIALIZE    ORCSAGECHKAREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-TAISYO
           PERFORM VARYING IDXD    FROM    1   BY  1
               UNTIL ( IDXD        >   31 )
      *
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                 AND ( FLG-NYUINDAY (IDXD)     =   1 )
                 AND ( FLG-NYUINRYODAY (IDXD)  =   1 )
                 AND ( FLG-SENTEIDAY (IDXD)    =   1 )
                   IF    ( FLG-TAISYO          =   ZERO )
                       MOVE    1       TO  FLG-TAISYO
                       PERFORM 2001111-SENTEI-FTN-TNS-SET-SEC
                   END-IF
      *
      *            選定入院料患者自己負担額編集処理
                   EVALUATE    SYS-5000-SENTEI-FTNKBN
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    SYS-5000-SENTEI-FTNKBN
                                           TO  WRK-SENTEI-FTNKBN
                       IF    ( SYS-5000-SENTEI-TAXKBN      =   "1" )
                           MOVE    2       TO  IDX-TAX
                       ELSE
                           MOVE    1       TO  IDX-TAX
                       END-IF
                       MOVE    SYS-5000-SENTEI-TANKA-X
                                           TO  WRK-SENTEI-TANKA-X
                       IF  (SYS-5000-SENTEI-KINGAKU-X NOT = SPACE) AND
                           (SYS-5000-SENTEI-KINGAKU-X NOT = "0000")
                           MOVE    SYS-5000-SENTEI-KINGAKU
                                              TO  WRK-SENTEI-KINGAKU
                       ELSE
                         IF   AGECHK-ROUJIN2    =   0
                           IF  SYS-5000-SENTEI-IPN-KINGAKU-X  =  SPACE
                               MOVE   ZERO TO  WRK-SENTEI-KINGAKU
                           ELSE
                               MOVE   SYS-5000-SENTEI-IPN-KINGAKU
                                           TO  WRK-SENTEI-KINGAKU
                           END-IF
                         ELSE
                           IF  SYS-5000-SENTEI-RJN-KINGAKU-X  =  SPACE
                               MOVE   ZERO  TO  WRK-SENTEI-KINGAKU
                           ELSE
                               MOVE   SYS-5000-SENTEI-RJN-KINGAKU
                                                TO  WRK-SENTEI-KINGAKU
                           END-IF
                         END-IF
                       END-IF
                       PERFORM 2001111-SENTEI-PTFTN-SEC
                   WHEN    "3"
                       MOVE    SBTUSENTEI-DAY-FTNKBN   (IDXD)
                                           TO  WRK-SENTEI-FTNKBN
                       IF    ( SBTUSENTEI-DAY-TAXKBN   (IDXD)  =   "1" )
                           MOVE    2       TO  IDX-TAX
                       ELSE
                           MOVE    1       TO  IDX-TAX
                       END-IF
                       MOVE    SBTUSENTEI-DAY-TANKA    (IDXD)
                                           TO  WRK-SENTEI-TANKA
                       MOVE    SBTUSENTEI-DAY-KINGAKU  (IDXD)
                                           TO  WRK-SENTEI-KINGAKU
                       PERFORM 2001111-SENTEI-PTFTN-SEC
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-SENTEIPTFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料患者自己負担点数情報設定処理
      *****************************************************************
       2001111-SENTEI-FTN-TNS-SET-SEC  SECTION.
      *
           INITIALIZE                      WRK-ZAI-G
      *
           MOVE    SPACE               TO  WRK-SENTEISRYCD
                                           WRK-SENTEIIPNCD
      *
      *    入院基本料のコードを探す
           MOVE    ZERO                TO  IDX-KHN
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
                    OR   ( NSRY-SRYCD (IDXC)   =   SPACE )
               MOVE    NSRY-SRYCD (IDXC)
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-DATAKBN =   1 )
                       MOVE    TNS-TEN TO  WRK-ZAI-KHNTEN
                       MOVE    IDXC    TO  IDX-KHN
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( IDX-KHN             =   ZERO )
               MOVE    1               TO  IDX-KHN
           END-IF
      *
           PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
      *
           MOVE    SENCHG-IPNSRYCD     TO  WRK-SENTEIIPNCD
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-SENTEISRYCD
      *
      *    点数も改めて求めなおす
           MOVE    SENCHG-SENTEISRYCD  TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           IF    ( SENCHG-IPNSRYCD     =   SENCHG-SENTEISRYCD )
               COMPUTE WRK-ZAI-SENTEI-KHNTEN
                   = ( WRK-ZAI-KHNTEN * 0.85 ) +  0.5
           ELSE
                MOVE    TNS-TEN         TO  WRK-ZAI-SENTEI-KHNTEN
           END-IF
      *
      *    看護比率基準未達のチェック
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H2404 )
               CONTINUE
           ELSE
               PERFORM 2001111-KNGMITATU-CHK-SEC
               IF    ( WRK-GAIHAKU-KHNTEN  NOT =   ZERO )
                   MOVE    WRK-GAIHAKU-KHNTEN  TO  WRK-ZAI-KHNTEN
               END-IF
           END-IF
      *
      *    病棟設定の選定自己負担情報の取得
           IF    SYS-5000-SENTEI-FTNKBN   =   "3"
               INITIALIZE                  ORCSBTUSENTEIAREA
               MOVE    HCM34S01-HOSPNUM    TO  SBTUSENTEI-HOSPNUM
               MOVE    HCM34S01-PTID       TO  SBTUSENTEI-PTID
               MOVE    HCM34S01-SANTEIYM   TO  SBTUSENTEI-SRYYM
               CALL    "ORCSBTUSENTEI"     USING
                                           ORCSBTUSENTEIAREA
                                           SPA-AREA
           END-IF
      *
           .
       2001111-SENTEI-FTN-TNS-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料患者自己負担設定処理
      *****************************************************************
       2001111-SENTEI-PTFTN-SEC        SECTION.
      *
           MOVE       ZERO             TO  WRK-SENTEI-GAIHAKU
                                           WRK-KHNTEN
                                           WRK-SENTEI-KHNTEN
      *
           IF    ( LNK-KANGOGEN-SRYCD (IDXD)   NOT =   SPACE )
               COMPUTE WRK-KHNTEN
                   =   WRK-ZAI-KHNTEN
                   * ( 100 - LNK-KANGOGEN-TEN (IDXD) ) / 100 + 0.5
               COMPUTE WRK-SENTEI-KHNTEN
                   =   WRK-ZAI-SENTEI-KHNTEN
                   * ( 100 - LNK-KANGOGEN-TEN (IDXD) ) / 100 + 0.5
      *
           ELSE
               MOVE    WRK-ZAI-KHNTEN         TO  WRK-KHNTEN
               MOVE    WRK-ZAI-SENTEI-KHNTEN  TO  WRK-SENTEI-KHNTEN
           END-IF
      *
           PERFORM     200111-SENTEI-KEISU-GET-SEC
      *
           EVALUATE    WRK-SENTEI-FTNKBN
      *    定率設定
           WHEN   "1"
               IF    ( WRK-SENTEI-TANKA-X      =   SPACE )
                   MOVE   10           TO  WRK-SENTEI-TANKA
               END-IF
      *
               EVALUATE    FLG-GAIHAKUDAY(IDXD)
               WHEN    ZERO
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                    AND  ( NACCT-SRYYM     <   CONST-H2204 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   4 )
                       CONTINUE
                   ELSE
                       IF    ( IDX-TAX             =   1 )
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               + ( WRK-KHNTEN  -   WRK-SENTEI-KHNTEN )
                               *   WRK-SENTEI-TANKA
                       ELSE
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               + ( WRK-KHNTEN - WRK-SENTEI-KHNTEN )
                               *   WRK-SENTEI-TANKA
                               *   WRK-RATE2
                               +   WRK-HASU
                       END-IF
                   END-IF
               WHEN    3   THRU 17
                   IF    ( NACCT-SRYYM     >=  CONST-H1604 )
                    AND  ( NACCT-SRYYM     <   CONST-H2204 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   4 )
                       CONTINUE
                   ELSE
                       COMPUTE WRK-SENTEI-GAIHAKU
                           =   WRK-KHNTEN * 0.15 + 0.5
                       IF    ( IDX-TAX             =   1 )
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               +   WRK-SENTEI-GAIHAKU
                               *   WRK-SENTEI-TANKA
                       ELSE
                           COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                               =   WRK-SENTEI-PTFTN (IDX-TAX)
                               +   WRK-SENTEI-GAIHAKU
                               *   WRK-SENTEI-TANKA
                               *   WRK-RATE2
                               +   WRK-HASU
                       END-IF
                   END-IF
               END-EVALUATE
      *    定額設定
           WHEN    "2"
      *
               IF    ( WRK-SENTEI-KINGAKU-X   =   SPACE )
                   MOVE    ZERO        TO  WRK-SENTEI-KINGAKU
               END-IF
      *
               EVALUATE   FLG-GAIHAKUDAY(IDXD)
               WHEN    ZERO
               WHEN    3   THRU   17
                   IF    ( IDX-TAX             =   1 )
                       COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                           =   WRK-SENTEI-PTFTN (IDX-TAX)
                           +   WRK-SENTEI-KINGAKU
                   ELSE
                       COMPUTE WRK-SENTEI-PTFTN (IDX-TAX)
                           =   WRK-SENTEI-PTFTN (IDX-TAX)
                           +   WRK-SENTEI-KINGAKU
                           *   WRK-RATE2
                           +   WRK-HASU
                   END-IF
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2001111-SENTEI-PTFTN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額編集処理
      *****************************************************************
       2001-BRMSAGAKU-HEN-SEC          SECTION.
      *
      *    室料差額情報取得処理
           PERFORM 20011-SYS-5113-GET-SEC
      *
           MOVE    "3"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  2001-BRMSAGAKU-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-NYUINDAY (IDXD)     =   1 )
      *
                    IF    (  FLG-BRMSGKSANTEIDAY (IDXD)    =   1 )
                            MOVE   1       TO  FLG-SANTEI
                    ELSE
                        IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                         OR (( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                          AND( HCM34S01-SANTEIYM   >=  CONST-H2604 ))
                            MOVE   1       TO  FLG-SANTEI
                        END-IF
                    END-IF
                END-IF
      *
               IF    ( FLG-SANTEI          =   1 )
      *
      *            室料差額基本情報設定処理
                   PERFORM 20011-BRMSAGAKU-KHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WBRMSAGAKU-DAY
                                                   (IDXB IDXD)
                       COMPUTE WBRMSAGAKU-ZAIKAISU (IDXB)
                           =   WBRMSAGAKU-ZAIKAISU (IDXB)   +   1
                       IF    ( WBRMSAGAKU-NUM (IDXB)    =   ZERO )
                               MOVE    IDXD
                                       TO  WBRMSAGAKU-NUM (IDXB)
                       ELSE
                           IF    ( IDXD    <   WBRMSAGAKU-NUM (IDXB))
                               MOVE    IDXD
                                       TO  WBRMSAGAKU-NUM (IDXB)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-BRMSAGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額情報取得処理
      *****************************************************************
       20011-SYS-5113-GET-SEC          SECTION.
      *
           INITIALIZE                      WRK-SYS-5113
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5113-REC
           MOVE    HCM34S01-HOSPNUM    TO  SYS-5113-HOSPNUM
           MOVE    "5113"              TO  SYS-5113-KANRICD
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5113-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( WRK-SYS-5113-MAX   >=   99 )
      *
               IF    ( SYS-5113-STYUKYMD (1:6)
                                       <=  HCM34S01-SANTEIYM )
                AND  ( SYS-5113-EDYUKYMD (1:6)
                                       >=  HCM34S01-SANTEIYM )
                   COMPUTE WRK-SYS-5113-MAX
                       =   WRK-SYS-5113-MAX    +   1
                   MOVE    WRK-SYS-5113-MAX    TO  IDX3
      *
                   MOVE    SYS-5113-STYUKYMD
                                       TO  WRK-SYS-5113-STYUKYMD (IDX3)
                   MOVE    SYS-5113-EDYUKYMD
                                       TO  WRK-SYS-5113-EDYUKYMD (IDX3)
                   MOVE    SYS-5113-KBNCD (1:2)
                                       TO  WRK-SYS-5113-KBNCD    (IDX3)
                   MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-SYS-5113-BRMSAGAKU(IDX3)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-5113-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SYS-5113-GET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額基本情報設定処理
      *****************************************************************
       20011-BRMSAGAKU-KHNINF-SET-SEC  SECTION.
      *
           MOVE    NACCT-SRYYM         TO  WRK-YYMM
           MOVE    IDXD                TO  WRK-DD
           MOVE    ZERO                TO  WRK-BRMSAGAKU
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           MOVE    ZERO                TO  FLG-HIT
           PERFORM VARYING IDX3    FROM    1   BY    1
                   UNTIL ( IDX3        >   WRK-SYS-5113-MAX )
                    OR   ( FLG-HIT NOT =   ZERO )
      *
               IF    ( WRK-SYS-5113-STYUKYMD (IDX3) <= WRK-YMD )
                AND  ( WRK-SYS-5113-EDYUKYMD (IDX3) >= WRK-YMD )
                AND  ( WRK-SYS-5113-KBNCD    (IDX3)  = NACCT-DAY (IDXD))
                   MOVE    1           TO  FLG-HIT
                   MOVE    WRK-SYS-5113-BRMSAGAKU(IDX3)
                                       TO  WRK-BRMSAGAKU
                   MOVE    WRK-SYS-5113-KBNCD    (IDX3)
                                       TO  WRK-BRMSAGAKUCD
                   IF    ( HCM34S01-SANTEIYM   >=  CONST-H1904 )
                       IF    ( SYS-5000-BRMSGK-TAXKBN  =   "1" )
                           COMPUTE WRK-BRMSAGAKU
                               =   WRK-BRMSAGAKU   *   WRK-RATE2
                               +   WRK-HASU
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-HIT             =   ZERO )
               GO  TO  20011-BRMSAGAKU-KHNINF-SET-EXT
           END-IF
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WBRMSAGAKU-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WBRMSAGAKU-MAX )
                     OR  ( WRK-BRMSAGAKUCD
                                   =   WBRMSAGAKU-SAGAKUCD(IDXB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WBRMSAGAKU-OCC1-MAX )
      *
               COMPUTE WBRMSAGAKU-OCC1-MAX
                   =   WBRMSAGAKU-OCC1-MAX +   1
               IF    ( WBRMSAGAKU-OCC1-MAX >   CONST-WBRMSAGAKU-MAX )
                   MOVE    CONST-WBRMSAGAKU-MAX
                                       TO  WBRMSAGAKU-OCC1-MAX
                   MOVE    1           TO  FLG-OVER
               ELSE
                   MOVE    WBRMSAGAKU-OCC1-MAX TO  IDXB
                   MOVE    NACCT-SRYKBN 
                                       TO  WBRMSAGAKU-SRYKBN (IDXB)
                   MOVE    WRK-BRMSAGAKU
                                       TO  WBRMSAGAKU-ZAITEN (IDXB)
                   MOVE    WRK-BRMSAGAKUCD
                                       TO  WBRMSAGAKU-SAGAKUCD(IDXB)
                   MOVE    NACCT-SRYCDTOTAL
                                       TO  WRK-SRYCDTOTAL
                   MOVE    WRK-SRYCDTOTAL
                                       TO  WBRMSAGAKU-SRYCD  (IDXB)
               END-IF
           END-IF
      *
           .
       20011-BRMSAGAKU-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理
      *****************************************************************
       2001-SYOKUJI-HEN-SEC            SECTION.
      *
      *    食事基本点数取得処理
           PERFORM 20011-SKJ-KHNTEN-GET-SEC
      *
      *    食堂加算チェック
           PERFORM 20011-SYOKUDOKSN-CHK-SEC
      *
           MOVE    "4"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =   197000570 )
                   CONTINUE
               ELSE
      *            食事編集処理
                   PERFORM 20011-SYOKUJI-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SYOKUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事基本点数取得処理
      *****************************************************************
       20011-SKJ-KHNTEN-GET-SEC        SECTION.
      *
           INITIALIZE                  WRK-SKJ-TEN-G
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
             IF    ( HCM34S01-HKNNUM(1:2)  NOT =   "98" )
               IF    ( FLG-SKJDAY (IDXD)     =   1 OR 2 )
                OR   ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                   PERFORM 200111-SKJ-KHNTEN-GET-SEC
               END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       20011-SKJ-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    食事基本点数取得処理
      *****************************************************************
       200111-SKJ-KHNTEN-GET-SEC       SECTION.
      *
           EVALUATE    LNK-SYOTOKU-SKJKBN(IDXD)
      *    低所得２
           WHEN    "2"
               MOVE   CONST-SKJFTN-TEI2
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
               IF    ( WRK-SKJ-TEN-SKJFTN-TEI2 =   ZERO )
      *            食事療養標準負担額（低２・９０日以下）
                   MOVE    CONST-SKJFTN-TEI2    TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       MOVE    TNS-TEN TO  WRK-SKJ-TEN-SKJFTN-TEI2
                   END-IF
               END-IF
      *
               MOVE   WRK-SKJ-TEN-SKJFTN-TEI2
                                 TO  TSKJFTN-MONEY(IDXD)
      *    低所得２（９０日越え）
           WHEN    "3"
               MOVE   CONST-SKJFTN-TEI2-90
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
               IF    ( WRK-SKJ-TEN-SKJFTN-TEI2-90  =   ZERO )
      *            食事療養標準負担額（低２・９０日超）
                   MOVE    CONST-SKJFTN-TEI2-90 TO WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU      =   ZERO )
                       MOVE    TNS-TEN    TO  WRK-SKJ-TEN-SKJFTN-TEI2-90
                   END-IF
               END-IF
      *
               MOVE   WRK-SKJ-TEN-SKJFTN-TEI2-90
                                 TO  TSKJFTN-MONEY(IDXD)
      *    低所得１
           WHEN    "1"
           WHEN    "4"
               MOVE   CONST-SKJFTN-TEI1
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
               IF    ( WRK-SKJ-TEN-SKJFTN-TEI1
                                 =   ZERO )
      *            食事療養標準負担額（低１）
                   MOVE    CONST-SKJFTN-TEI1   TO WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       MOVE    TNS-TEN TO  WRK-SKJ-TEN-SKJFTN-TEI1
                   END-IF
               END-IF
      *
               MOVE   WRK-SKJ-TEN-SKJFTN-TEI1
                                 TO  TSKJFTN-MONEY(IDXD)
           WHEN    OTHER
               EVALUATE    TRUE
               WHEN ( HCM34S01-SANTEIYM  >=   CONST-H2804 )
                   EVALUATE    LNK-SKJKEEP-KBN (IDXD)
                   WHEN   "1"
                       MOVE   CONST-SKJFTN-NANBYO
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
                       IF   ( WRK-SKJ-TEN-SKJFTN-NANBYO
                                           =   ZERO )
      *                    食事療養標準負担額（指定難病・小児慢性特定疾病患者）（一般）
                           MOVE    CONST-SKJFTN-NANBYO
                                       TO  WRK-TNS-SRYCD
                           PERFORM 900-TENSU-KEY-SEL-SEC
                           MOVE    TNS-TEN
                                       TO  WRK-SKJ-TEN-SKJFTN-NANBYO
                       END-IF
      *
                       MOVE   WRK-SKJ-TEN-SKJFTN-NANBYO
                                 TO  TSKJFTN-MONEY(IDXD)
                   WHEN   "2"
                       MOVE   CONST-SKJFTN-SEISHIN
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
                       IF   ( WRK-SKJ-TEN-SKJFTN-SEISHIN = ZERO )
      *                    食事療養標準負担額（精神１年超）（一般）
                           MOVE    CONST-SKJFTN-SEISHIN
                                       TO  WRK-TNS-SRYCD
                           PERFORM 900-TENSU-KEY-SEL-SEC
                           MOVE    TNS-TEN
                                       TO  WRK-SKJ-TEN-SKJFTN-SEISHIN
                       END-IF
      *
                       MOVE   WRK-SKJ-TEN-SKJFTN-SEISHIN
                                 TO  TSKJFTN-MONEY(IDXD)
                   WHEN   OTHER
                       MOVE   CONST-SKJFTN-IPN
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
                       IF    ( WRK-SKJ-TEN-SKJFTN-IPN
                                       =   ZERO )
      *                            食事療養標準負担額（一般）
                           MOVE    CONST-SKJFTN-IPN
                                           TO  WRK-TNS-SRYCD
                           PERFORM 900-TENSU-KEY-SEL-SEC
                           IF    ( FLG-TENSU           =   ZERO )
                               MOVE    TNS-TEN
                                           TO  WRK-SKJ-TEN-SKJFTN-IPN
                           END-IF
                       END-IF
      *
                       MOVE   WRK-SKJ-TEN-SKJFTN-IPN
                                 TO  TSKJFTN-MONEY(IDXD)
                   END-EVALUATE
               WHEN   OTHER
                           MOVE   CONST-SKJFTN-IPN
                                 TO  TSKJFTN-SRYCD(IDXD)
      *
                   IF    ( WRK-SKJ-TEN-SKJFTN-IPN
                                 =   ZERO )
      *                食事療養標準負担額（一般）
                       MOVE    CONST-SKJFTN-IPN
                                 TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU           =   ZERO )
                           MOVE  TNS-TEN   TO  WRK-SKJ-TEN-SKJFTN-IPN
                       END-IF
                   END-IF
      *
                   MOVE   WRK-SKJ-TEN-SKJFTN-IPN
                                 TO  TSKJFTN-MONEY(IDXD)
               END-EVALUATE
           END-EVALUATE
      *
           .
       200111-SKJ-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    生活基本点数取得処理
      *****************************************************************
       20011-LIFE-KHNTEN-GET-SEC        SECTION.
      *
           INITIALIZE                  WRK-LIFE-TEN-G
      *
           .
       20011-LIFE-KHNTEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算チェック
      *****************************************************************
       20011-SYOKUDOKSN-CHK-SEC        SECTION.
      *
           INITIALIZE                          FLG-SYOKUDODAY-G
      *
           MOVE    "4"                     TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF   ( NACCT-SRYCDTOTAL =   197000570 )
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL     ( IDX1    >   31 )
                       IF    ( NACCT-DAY(IDX1)  >   ZERO )
                           MOVE    1       TO  FLG-SYOKUDODAY (IDX1)
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SYOKUDOKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理
      *****************************************************************
       20011-SYOKUJI-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                AND  ( FLG-SKJDAY   (IDXD)     =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4  )
                    IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                     OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                        MOVE   1       TO  FLG-SANTEI
                    END-IF
               END-IF
      *
               IF    ( FLG-SANTEI          =   1 )
      *
      *            食事診療行為コード取得処理
                   PERFORM 200111-SYOKUJI-SRYCD-GET-SEC
      *
      *            食事基本情報設定処理
                   PERFORM 200111-SYOKUJI-KHNINF-SET-SEC
                   IF    ( IDXB        >   ZERO )
                     AND ( FLG-OVER    =   ZERO )
                       MOVE    1       TO  WSYOKUJI-DAY
                                                   (IDXB IDXD)
                       COMPUTE WSYOKUJI-ZAIKAISU (IDXB)
                           =   WSYOKUJI-ZAIKAISU (IDXB)    +   1
                       IF    ( WSYOKUJI-NUM (IDXB)     =   ZERO )
                               MOVE    IDXD
                                       TO  WSYOKUJI-NUM (IDXB)
                       ELSE
                           IF    ( IDXD    <   WSYOKUJI-NUM (IDXB))
                               MOVE    IDXD
                                       TO  WSYOKUJI-NUM (IDXB)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20011-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事療行為コード取得処理
      *****************************************************************
       200111-SYOKUJI-SRYCD-GET-SEC    SECTION.
      *
           INITIALIZE                      WRK-SKJ-SRYCD-G
           MOVE    ZERO                TO  IDX5
      *
           IF    ( HCM34S01-SANTEIYM  >=   CONST-H2804 )
               PERFORM 2002-H28-SYOKUJI-SRYCD-GET-SEC
           ELSE
               PERFORM 2002-SYOKUJI-SRYCD-GET-SEC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   5 )
               IF    ( WRK-SKJ-SRYCD (IDX5)    NOT =   SPACE )
                   MOVE    WRK-SKJ-SRYCD (IDX5)    TO  WRK-SRYCD-NUM
                   COMPUTE WRK-SKJ-SRYCDTOTAL  =   WRK-SKJ-SRYCDTOTAL
                                               +   WRK-SRYCD-NUM
               END-IF
           END-PERFORM
      *
           .
       200111-SYOKUJI-SRYCD-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事療行為コード取得処理
      *****************************************************************
       2002-H28-SYOKUJI-SRYCD-GET-SEC      SECTION.
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    入院時食事療養（１）
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    入院時食事療養（２）
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *                    入院時食事療養（１）
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                    特別食加算
                           PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                       END-EVALUATE
                   WHEN  3
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *                    入院時食事療養（１）（流動食）
                           PERFORM 2001111-SKJ-RYOYO1-RDS-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                       WHEN  2
      *                    入院時食事療養（２）（流動食）
                           PERFORM 2001111-SKJ-RYOYO2-RDS-SET-SEC
                       END-EVALUATE
      *
                   END-EVALUATE
               WHEN  2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 OR 3)
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
                           PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
                       END-EVALUATE
                   WHEN    3
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-SKJ-RYOYO1-RDS-SET-SEC
                       WHEN    2
                           PERFORM 2001111-SKJ-RYOYO2-RDS-SET-SEC
                       END-EVALUATE
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 OR 3 )
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           .
       2002-H28-SYOKUJI-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    食事療行為コード取得処理
      *****************************************************************
       2002-SYOKUJI-SRYCD-GET-SEC      SECTION.
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    入院時食事療養（１）
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                    特別管理加算
                           PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    入院時食事療養（２）
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
      *
      *                入院時食事療養（１）
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                特別管理加算
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                特別食加算
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *                食堂加算
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   WHEN  3
      *
      *                入院時食事療養（１）
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                特別管理加算
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                食堂加算
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
      *
      *                複数献立加算
                       PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
                   WHEN  4
      *
      *                入院時食事療養（１）
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
      *                特別管理加算
                       PERFORM 2001111-SKJ-TOKKANRI-SET-SEC
      *
      *                特別食加算
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
      *
      *                食堂加算
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
      *
      *                複数献立加算
                       PERFORM 2001111-SKJ-FKSKONDATE-SET-SEC
      *
                   END-EVALUATE
               WHEN  2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-SKJ-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-SKJ-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       PERFORM 2001111-SKJ-RYOYO1-SET-SEC
                       PERFORM 2001111-SKJ-TOKSYOKU-SET-SEC
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       PERFORM 2001111-SKJ-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2001111-SKJ-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           .
       2002-SYOKUJI-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    食事朝コメント設定処理
      *****************************************************************
       2001111-SKJ-ASA-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-ASA   TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-ASA-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事昼コメント設定処理
      *****************************************************************
       2001111-SKJ-HIRU-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-HIRU  TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-HIRU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事夕コメント設定処理
      *****************************************************************
       2001111-SKJ-YUU-SET-SEC         SECTION.
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-YUU   TO  WRK-SKJ-SRYCD (IDX5)
           END-IF
      *
           .
       2001111-SKJ-YUU-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養（１）設定処理
      *****************************************************************
       2001111-SKJ-RYOYO1-SET-SEC      SECTION.
      *
           IF    ( WRK-SKJ-TEN-RYOYO1  =   ZERO )
      *        入院時食事療養（１）
               MOVE    CONST-SKJ-RYOYO1    TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO1
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
      *
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO1    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO1
           END-IF
      *
           .
       2001111-SKJ-RYOYO1-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養（１）（流動食）
      *****************************************************************
       2001111-SKJ-RYOYO1-RDS-SET-SEC      SECTION.
      *
           IF    ( WRK-SKJ-TEN-RYOYO1-RDS    = ZERO )
      *        入院時食事療養（１）（流動食）
               MOVE    CONST-SKJ-RYOYO1-RDS
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO1-RDS
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO1-RDS    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO1-RDS
           END-IF
      *
           .
       2001111-SKJ-RYOYO1-RDS-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養（２）設定処理
      *****************************************************************
       2001111-SKJ-RYOYO2-SET-SEC      SECTION.
      *
           IF    ( WRK-SKJ-TEN-RYOYO2      =   ZERO )
      *        入院時食事療養（２）
               MOVE    CONST-SKJ-RYOYO2    TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO2
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO2    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO2
           END-IF
      *
           .
       2001111-SKJ-RYOYO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養（２）（流動食）
      *****************************************************************
       2001111-SKJ-RYOYO2-RDS-SET-SEC      SECTION.
      *
           IF    ( WRK-SKJ-TEN-RYOYO2-RDS  =   ZERO )
      *        入院時食事療養（２）（流動食）
               MOVE    CONST-SKJ-RYOYO2-RDS
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN         TO  WRK-SKJ-TEN-RYOYO2-RDS
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-RYOYO2-RDS    TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-RYOYO2-RDS
           END-IF
      *
           .
       2001111-SKJ-RYOYO2-RDS-SET-EXT.
           EXIT.
      *****************************************************************
      *    特別管理加算設定処理
      *****************************************************************
       2001111-SKJ-TOKKANRI-SET-SEC    SECTION.
      *
           IF    ( WRK-TOKUKANRIKASAN(IDXD)      =  1 )
               PERFORM 20011111-SKJ-TOKKANRI-SET-SEC
           END-IF
      *
           .
       2001111-SKJ-TOKKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    特別管理加算設定処理
      *****************************************************************
       20011111-SKJ-TOKKANRI-SET-SEC   SECTION.
      *
           IF    ( WRK-SKJ-TEN-TOKKANRI    =   ZERO )
      *        特別管理加算
               MOVE    CONST-SKJ-TOKKANRI  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKKANRI
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-TOKKANRI  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-TOKKANRI
           END-IF
      *
           .
       20011111-SKJ-TOKKANRI-SET-EXT.
           EXIT.
      *****************************************************************
      *    特別食加算設定処理
      *****************************************************************
       2001111-SKJ-TOKSYOKU-SET-SEC    SECTION.
      *
           IF    ( WRK-SKJ-TEN-TOKSYOKU    =   ZERO )
      *        特別食加算
               MOVE    CONST-SKJ-TOKSYOKU  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-TOKSYOKU
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-TOKSYOKU  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-TOKSYOKU
           END-IF
      *
           .
       2001111-SKJ-TOKSYOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算設定処理
      *****************************************************************
       2001111-SKJ-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
            AND  ( WSKJH1804-CNT-SYOKUDO       =   ZERO )
               PERFORM 20011111-SKJ-SYOKUDO-SET-SEC
           END-IF
      *
           .
       2001111-SKJ-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算設定処理
      *****************************************************************
       20011111-SKJ-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( WRK-SKJ-TEN-SYOKUDO =   ZERO )
      *        食堂加算
               MOVE    CONST-SKJ-SYOKUDO   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-SYOKUDO
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-SYOKUDO   TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-SYOKUDO
           END-IF
      *
           .
       20011111-SKJ-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    複数献立加算設定処理
      *****************************************************************
       2001111-SKJ-FKSKONDATE-SET-SEC  SECTION.
      *
           IF    ( WRK-SKJ-TEN-FKSKONDATE  =   ZERO )
      *        複数献立加算
               MOVE    CONST-SKJ-FKSKONDATE
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-SKJ-TEN-FKSKONDATE
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-SKJ-FKSKONDATE
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-SKJ-TEN-FKSKONDATE
           END-IF
      *
           .
       2001111-SKJ-FKSKONDATE-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者食事負担設定処理
      *****************************************************************
       2001111-SKJ-SKJFTN-SET-SEC      SECTION.
      *
           IF    ( TSKJFTN-SRYCD (IDXD) =   SPACE )
               CONTINUE
           ELSE
               MOVE    1               TO  WRK-SKJ-PTFTN
               IF    ( IDX5                <   5 )
                   COMPUTE IDX5    =   IDX5    +   1
                   MOVE    TSKJFTN-SRYCD (IDXD)
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
                   COMPUTE WRK-SKJ-ZAITEN
                       =   WRK-SKJ-ZAITEN
                       +   TSKJFTN-MONEY (IDXD)
               END-IF
           END-IF
      *
      *
           .
       2001111-SKJ-SKJFTN-SET-EXT.
           EXIT.
      *****************************************************************
      *    生活療行為コード取得処理
      *****************************************************************
       200111-SEIKATU-SRYCD-GET-SEC    SECTION.
      *
           INITIALIZE                      WRK-SKJ-SRYCD-G
           MOVE    ZERO                TO  IDX5
      *
           IF    ( HCM34S01-SANTEIYM  >=   CONST-H2804 )
               PERFORM 2002-H28-SEIKATU-SRYCD-GET-SEC
           ELSE
               PERFORM 2002-SEIKATU-SRYCD-GET-SEC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   5 )
               IF    ( WRK-SKJ-SRYCD (IDX5)    NOT =   SPACE )
                   MOVE    WRK-SKJ-SRYCD (IDX5)    TO  WRK-SRYCD-NUM
                   COMPUTE WRK-SKJ-SRYCDTOTAL  =   WRK-SKJ-SRYCDTOTAL
                                               +   WRK-SRYCD-NUM
               END-IF
           END-PERFORM
      *
           .
       200111-SEIKATU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    生活療行為コード取得処理
      *****************************************************************
       2002-H28-SEIKATU-SRYCD-GET-SEC      SECTION.
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    入院時生活療養（１）
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    入院時生活療養（２）
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *                    入院時生活療養（１）
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                    特別食加算
                           PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                       END-EVALUATE
                   WHEN  3
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *                    入院時生活療養（１）（流動食）
                           PERFORM 2001111-LIFE-RYOYO1-RDS-SET-SEC
      *                    食堂加算
                           PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                       END-EVALUATE
                   END-EVALUATE
               WHEN    2
                   PERFORM 2002-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 OR 3)
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
                           PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
                       END-EVALUATE
                   WHEN    3
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-LIFE-RYOYO1-RDS-SET-SEC
                       END-EVALUATE
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 OR 3)
                       PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2002-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           .
       2002-H28-SEIKATU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    生活療行為コード取得処理
      *****************************************************************
       2002-SEIKATU-SRYCD-GET-SEC      SECTION.
      *
           IF    ( FLG-EXSYOKUJI       =   ZERO )
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
      *
      *                    入院時生活療養（１）
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                    食堂加算
                           PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                       WHEN  2
      *
      *                    入院時生活療養（２）
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN  2
      *
      *                入院時生活療養（１）
                       PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
      *                特別食加算
                       PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
      *
      *                食堂加算
                       PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
      *
                   END-EVALUATE
               WHEN    2
                   PERFORM 2002-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SKJZAIPTN
               WHEN    ZERO
      *
                   IF    ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       EVALUATE    WRK-SKJCOMMENTCD
                       WHEN    CONST-SKJ-ASA
                           PERFORM 2001111-SKJ-ASA-SET-SEC
                       WHEN    CONST-SKJ-HIRU
                           PERFORM 2001111-SKJ-HIRU-SET-SEC
                       WHEN    CONST-SKJ-YUU
                           PERFORM 2001111-SKJ-YUU-SET-SEC
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    NACCT-DAY (IDXD)
                   WHEN  1
      *
                       EVALUATE    WRK-SHOKUJIRYOYO
                       WHEN    1
                           PERFORM 2001111-LIFE-RYOYO1-SET-SEC
      *
                       WHEN    2
                           PERFORM 2001111-LIFE-RYOYO2-SET-SEC
                       END-EVALUATE
                   WHEN    2
                       PERFORM 2001111-LIFE-RYOYO1-SET-SEC
                       PERFORM 2001111-LIFE-TOKSYOKU-SET-SEC
                   END-EVALUATE
               WHEN    1
                   IF    ( WRK-SHOKUJIRYOYO    =   1 )
                    AND  ( NACCT-DAY (IDXD)    =   1  OR 2 )
                       PERFORM 2001111-LIFE-SYOKUDO-SET-SEC
                   END-IF
               WHEN    2
                   PERFORM 2002-LIFE-SKJFTN-SET-SEC
               END-EVALUATE
           END-IF
      *
           .
       2002-SEIKATU-SRYCD-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院時生活療養（１）設定処理
      *****************************************************************
       2001111-LIFE-RYOYO1-SET-SEC     SECTION.
      *
           IF    ( WRK-LIFE-TEN-RYOYO1 =   ZERO )
      *        入院時生活療養（１）食事療養
               MOVE    CONST-LIFE-RYOYO1   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-LIFE-TEN-RYOYO1
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-RYOYO1   TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-RYOYO1
           END-IF
      *
           .
       2001111-LIFE-RYOYO1-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時生活療養（１）（流動食）設定処理
      *****************************************************************
       2001111-LIFE-RYOYO1-RDS-SET-SEC     SECTION.
      *
           IF    ( WRK-LIFE-TEN-RYOYO1-RDS =   ZERO )
      *        入院時生活療養（１）食事療養（流動食）
               MOVE    CONST-LIFE-RYOYO1-RDS
                                       TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN         TO  WRK-LIFE-TEN-RYOYO1-RDS
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-RYOYO1-RDS   TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-RYOYO1-RDS
           END-IF
      *
           .
       2001111-LIFE-RYOYO1-RDS-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時生活療養（２）設定処理
      *****************************************************************
       2001111-LIFE-RYOYO2-SET-SEC     SECTION.
      *
           IF    ( WRK-LIFE-TEN-RYOYO2     =   ZERO )
      *        入院時生活療養（２）食事療養
               MOVE    CONST-LIFE-RYOYO2   TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-LIFE-TEN-RYOYO2
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-RYOYO2   TO  WRK-SKJ-SRYCD (IDX5)
                                               WRK-SRYCD-NUM
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-RYOYO2
           END-IF
      *
           .
       2001111-LIFE-RYOYO2-SET-EXT.
           EXIT.
      *****************************************************************
      *    特別食加算設定処理
      *****************************************************************
       2001111-LIFE-TOKSYOKU-SET-SEC   SECTION.
      *
           IF    ( WRK-LIFE-TEN-TOKSYOKU   =   ZERO )
      *        特別食加算
               MOVE    CONST-LIFE-TOKSYOKU TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-LIFE-TEN-TOKSYOKU
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-TOKSYOKU TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-TOKSYOKU
           END-IF
      *
           .
       2001111-LIFE-TOKSYOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算設定処理
      *****************************************************************
       2001111-LIFE-SYOKUDO-SET-SEC     SECTION.
      *
           IF    ( FLG-SYOKUDODAY(IDXD)    NOT =   ZERO )
            AND  ( WSKJH1804-CNT-SYOKUDO       =   ZERO )
               PERFORM 20011111-LIFE-SYOKUDO-SET-SEC
           END-IF
      *
           .
       2001111-LIFE-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算設定処理
      *****************************************************************
       20011111-LIFE-SYOKUDO-SET-SEC   SECTION.
      *
           IF    ( WRK-LIFE-TEN-SYOKUDO    =   ZERO )
      *        食堂加算
               MOVE    CONST-LIFE-SYOKUDO  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-LIFE-TEN-SYOKUDO
               END-IF
           END-IF
      *
           IF    ( IDX5                <   5 )
               COMPUTE IDX5    =   IDX5    +   1
               MOVE    CONST-LIFE-SYOKUDO  TO  WRK-SKJ-SRYCD (IDX5)
      *
               COMPUTE WRK-SKJ-ZAITEN
                   =   WRK-SKJ-ZAITEN      +   WRK-LIFE-TEN-SYOKUDO
           END-IF
      *
           .
       20011111-LIFE-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者食事負担設定処理
      *****************************************************************
       2002-LIFE-SKJFTN-SET-SEC     SECTION.
      *
           IF    ( HCM34S01-HKNNUM(1:2)            =   "98" )
             OR  ( LNK-NYULIFE-SKJFTN-SRYCD (IDXD) =   SPACE )
               CONTINUE
           ELSE
               MOVE    1               TO  WRK-SKJ-PTFTN
               IF    ( IDX5                <   5 )
                   COMPUTE IDX5    =   IDX5    +   1
                   MOVE    LNK-NYULIFE-SKJFTN-SRYCD (IDXD)
                                           TO  WRK-SKJ-SRYCD (IDX5)
      *
                   COMPUTE WRK-SKJ-ZAITEN
                       =   WRK-SKJ-ZAITEN
                       +   LNK-NYULIFE-SKJFTN-MONEY (IDXD)
               END-IF
           END-IF
      *
      *
           .
       2002-LIFE-SKJFTN-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事基本情報設定処理
      *****************************************************************
       200111-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-OVER
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WSYOKUJI-MAX )
                     OR (( WRK-SKJ-SRYCDTOTAL
                                   =   WSYOKUJI-SRYCDTOTAL (IDXB))
                      AND( WRK-SKJ-ZAITEN
                                   =   WSYOKUJI-ZAITEN     (IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WSYOKUJI-OCC1-MAX )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX   +   1
               IF    ( WSYOKUJI-OCC1-MAX   >   CONST-WSYOKUJI-MAX )
                   MOVE    CONST-WSYOKUJI-MAX  TO  WSYOKUJI-OCC1-MAX
                   MOVE    1                   TO  FLG-OVER
               ELSE
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
                   MOVE    WRK-SKJ-SRYCDTOTAL
                                       TO  WSYOKUJI-SRYCDTOTAL (IDXB)
                   MOVE    NACCT-SRYKBN
                                       TO  WSYOKUJI-SRYKBN (IDXB)
                   MOVE    WRK-SKJ-PTFTN
                                       TO  WSYOKUJI-PTFTN (IDXB)
                   MOVE    WRK-SKJ-ZAITEN
                                       TO  WSYOKUJI-ZAITEN (IDXB)
                   PERFORM VARYING IDXC    FROM    1   BY  1
                           UNTIL ( IDXC    >   5 )
                            OR   ( WRK-SKJ-SRYCD (IDXC)    =   SPACE )
                       MOVE    WRK-SKJ-SRYCD (IDXC)
                                       TO  WSYOKUJI-SRYCD  (IDXB IDXC)
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       200111-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理
      *****************************************************************
       2002-SYOKUJI-HEN-SEC            SECTION.
      *
      *    食事基本点数取得処理
           PERFORM 20011-SKJ-KHNTEN-GET-SEC
      *
      *    生活基本点数取得処理
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1810 )
               PERFORM 20011-LIFE-KHNTEN-GET-SEC
           ELSE
               INITIALIZE              WRK-LIFE-TEN-G
           END-IF
      *
           MOVE    "4"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =   197000570 )
                   CONTINUE
               ELSE
      *
      *            食事退避領域編集処理
                   PERFORM 20021-WSKJNACCT-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    食堂加算チェック
           PERFORM 20021-SYOKUDOKSN-CHK-SEC
      *
           IF    ( FLG-EXSYOKUJI   =   ZERO )
               MOVE    ZERO            TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               IF    ( FLG-SKJFTN  =   1 )
                   MOVE    2           TO      FLG-SKJZAIPTN
                   PERFORM 20021-SYOKUJI-HEN-SEC
               END-IF
           ELSE
               MOVE    ZERO            TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               MOVE    1               TO      FLG-SKJZAIPTN
               PERFORM 20021-SYOKUJI-HEN-SEC
               IF    ( FLG-SKJFTN  =   1 )
                   MOVE    2           TO      FLG-SKJZAIPTN
                   PERFORM 20021-SYOKUJI-HEN-SEC
               END-IF
           END-IF
      *
           IF    ( HCM34S01-SANTEIYM   >=  CONST-H1810 )
            AND  ( WRK-SHOKUJIRYOYO     =  1 OR 2 )
            AND  ( CONST-WSYOKUJI-MAX  >   WSYOKUJI-OCC1-MAX )
      *            入院時生活療養環境療養編集処理
                   PERFORM 20021-KANKYO-HEN-SEC
           END-IF
      *
           .
       2002-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事診療コード合計編集処理
      *****************************************************************
       20021-SKJ-SRYCDTTL-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDXG    FROM    1   BY  1
                   UNTIL ( IDXG    >   11 )
      *
               IF    ( WSKJH1804-SRYCD-CNT (IDXG)
                                   >   ZERO )
                   COMPUTE WSKJH1804-SURYOTOTAL
                       =   WSKJH1804-SURYOTOTAL
                       +   WSKJH1804-SRYCD-CNT (IDXG)
                   COMPUTE WSKJH1804-MEISAISU
                       =   WSKJH1804-MEISAISU
                       +   1
               END-IF
           END-PERFORM
      *
           .
       20021-SKJ-SRYCDTTL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事診療コード合計編集処理
      *****************************************************************
       20021-SYOKUJI-ZAI-EDIT-SEC      SECTION.
      *
           IF    ( WSKJH1804-ZAITEN    >   ZERO )
               PERFORM 20021-SYOKUJI-KHNINF-SET-SEC
               IF    ( IDXB        >   ZERO )
                 AND ( FLG-OVER    =   ZERO )
                   MOVE    1       TO  WSYOKUJI-DAY (IDXB IDXD)
                   COMPUTE WSYOKUJI-ZAIKAISU (IDXB)
                       =   WSYOKUJI-ZAIKAISU (IDXB)    +   1
               END-IF
           END-IF
      *
           .
       20021-SYOKUJI-ZAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    食事退避領域編集処理
      *****************************************************************
       20021-WSKJNACCT-HEN-SEC         SECTION.
      *
           MOVE        ZERO   TO   IDX-SKJ
           EVALUATE    NACCT-SRYCDTOTAL
           WHEN    CONST-SKJ-ASA
               MOVE    1       TO  IDX-SKJ
           WHEN    CONST-SKJ-HIRU
               MOVE    2       TO  IDX-SKJ
           WHEN    CONST-SKJ-YUU
               MOVE    3       TO  IDX-SKJ
           END-EVALUATE
      *
           IF    ( IDX-SKJ     >   ZERO )
      *
               MOVE    NYUINACCT-REC
                                   TO  WSKJNACCT-OCC      (IDX-SKJ)
               INITIALIZE              WSKJNACCT-DAY-AREA (IDX-SKJ)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
                   MOVE    ZERO                TO  FLG-SANTEI
                   IF    ( NACCT-DAY    (IDXD)     >   ZERO )
                    AND  ( FLG-SKJDAY   (IDXD)     =   1 OR 2 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4  )
                        IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                         OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                            MOVE   1       TO  FLG-SANTEI
                        END-IF
                   END-IF
      *
                   IF    ( FLG-SANTEI          =   1 )
                       MOVE    NACCT-DAY    (IDXD)
                                       TO  WSKJNACCT-DAY (IDX-SKJ IDXD)
      *
                       MOVE    ZERO    TO  FLG-CLEAR
                       IF    ( WRK-DOJITUST(IDXD)  NOT =  SPACE  )
                           EVALUATE    WRK-DOJITUST(IDXD)
                           WHEN    "0"
                                   MOVE    1   TO  FLG-CLEAR
                           WHEN    "1"
                               CONTINUE
                           WHEN    "2"
                               IF    ( IDX-SKJ <   2 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "3"
                               IF    ( IDX-SKJ <   3 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           END-EVALUATE
                       END-IF
      *
                       IF    ( FLG-CLEAR   =   1 )
                           MOVE    ZERO    TO  WSKJNACCT-DAY
                                                         (IDX-SKJ IDXD)
                           MOVE    "1"     TO  WSKJNACCT-SYOKUDO-NG
                                                         (IDXD)
                       END-IF
      *
                       MOVE    ZERO    TO  FLG-CLEAR
                       IF    ( WRK-DOJITUED(IDXD)  NOT =  SPACE  )
                           EVALUATE    WRK-DOJITUED(IDXD)
                           WHEN    "0"
                                   MOVE    1   TO  FLG-CLEAR
                           WHEN    "1"
                               IF    ( IDX-SKJ >   1 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "2"
                               IF    ( IDX-SKJ >   2 )
                                   MOVE    1   TO  FLG-CLEAR
                               END-IF
                           WHEN    "3"
                                   CONTINUE
                           END-EVALUATE
                       END-IF
                       IF    ( FLG-CLEAR   =   1 )
                           MOVE    ZERO    TO  WSKJNACCT-DAY
                                                         (IDX-SKJ IDXD)
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20021-WSKJNACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算チェック
      *****************************************************************
       20021-SYOKUDOKSN-CHK-SEC        SECTION.
      *
           PERFORM 20011-SYOKUDOKSN-CHK-SEC
      *
           PERFORM VARYING IDXD    FROM    1 BY  1
                   UNTIL ( IDXD    >       31 )
               IF    ( WSKJNACCT-SYOKUDO-NG (IDXD) =   "1" )
                   MOVE    ZERO    TO  FLG-SYOKUDODAY(IDXD)
               END-IF
           END-PERFORM
      *
           .
       20021-SYOKUDOKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理
      *****************************************************************
       20021-SYOKUJI-HEN-SEC           SECTION.
      *
           IF    ( FLG-EXSYOKUJI   =   ZERO )
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   INITIALIZE              WSKJH1804-AREA
                   PERFORM VARYING IDX-SKJ FROM    1   BY  1
                           UNTIL ( IDX-SKJ >   3 )
      *
                       MOVE    CONST-SKJ-COMMENT (IDX-SKJ)
                                   TO      WRK-SKJCOMMENTCD
                       MOVE    WSKJNACCT-OCC (IDX-SKJ)
                                   TO      NYUINACCT-REC
      *
                       IF    ( NACCT-DAY    (IDXD)     >   ZERO )
      *
                           IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                               PERFORM 200211-SYOKUJI-HEN-SEC
                           ELSE
                               PERFORM 200212-SYOKUJI-HEN-SEC
                           END-IF
      *
                       END-IF
      *
      *
                   END-PERFORM
                   PERFORM 20021-SKJ-SRYCDTTL-HEN-SEC
                   PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
               END-PERFORM
           ELSE
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   PERFORM VARYING IDX-SKJ FROM    1   BY  1
                           UNTIL ( IDX-SKJ >   3 )
      *
                       INITIALIZE          WSKJH1804-AREA
                       MOVE    CONST-SKJ-COMMENT (IDX-SKJ)
                                   TO      WRK-SKJCOMMENTCD
                       MOVE    WSKJNACCT-OCC (IDX-SKJ)
                                   TO      NYUINACCT-REC
      *
                       IF    ( NACCT-DAY    (IDXD)     >   ZERO )
      *
                           IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                               PERFORM 200211-SYOKUJI-HEN-SEC
                           ELSE
                               PERFORM 200212-SYOKUJI-HEN-SEC
                           END-IF
      *
                       END-IF
      *
                       PERFORM 20021-SKJ-SRYCDTTL-HEN-SEC
                       PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
      *
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       20021-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理（食事療養）
      *****************************************************************
       200211-SYOKUJI-HEN-SEC          SECTION.
      *
           PERFORM 200111-SYOKUJI-SRYCD-GET-SEC
      *
           COMPUTE WSKJH1804-SRYCDTOTAL
               =   WSKJH1804-SRYCDTOTAL
               +   WRK-SKJ-SRYCDTOTAL
      *
           COMPUTE WSKJH1804-ZAITEN
               =   WSKJH1804-ZAITEN
               +   WRK-SKJ-ZAITEN
      *
           PERFORM VARYING IDXF    FROM    1   BY  1
                           UNTIL ( IDXF    >       5 )
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-ASA )
                   COMPUTE WSKJH1804-CNT-ASA
                       =   WSKJH1804-CNT-ASA
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-HIRU )
                   COMPUTE WSKJH1804-CNT-HIRU
                       =   WSKJH1804-CNT-HIRU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-YUU )
                   COMPUTE WSKJH1804-CNT-YUU
                       =   WSKJH1804-CNT-YUU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-RYOYO1 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1
                       =   WSKJH1804-CNT-SKJRYOYO1
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-RYOYO1-RDS )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1-RDS
                       =   WSKJH1804-CNT-SKJRYOYO1-RDS
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-RYOYO2 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO2
                       =   WSKJH1804-CNT-SKJRYOYO2
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-RYOYO2-RDS )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO2-RDS
                       =   WSKJH1804-CNT-SKJRYOYO2-RDS
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-SKJ-TOKSYOKU )
                   COMPUTE WSKJH1804-CNT-TOKSYOKU
                       =   WSKJH1804-CNT-TOKSYOKU
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-SKJ-SYOKUDO )
                   COMPUTE WSKJH1804-CNT-SYOKUDO
                       =   WSKJH1804-CNT-SYOKUDO
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                   NOT =   SPACE )
                   IF    ( WRK-SKJ-SRYCD (IDXF)
                                   =   TSKJFTN-SRYCD (IDXD))
                       COMPUTE WSKJH1804-CNT-SKJFTN
                           =   WSKJH1804-CNT-SKJFTN
                           +   1
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200211-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事編集処理（生活療養）
      *****************************************************************
       200212-SYOKUJI-HEN-SEC          SECTION.
      *
           PERFORM 200111-SEIKATU-SRYCD-GET-SEC
      *
           COMPUTE WSKJH1804-SRYCDTOTAL
               =   WSKJH1804-SRYCDTOTAL
               +   WRK-SKJ-SRYCDTOTAL
      *
           COMPUTE WSKJH1804-ZAITEN
               =   WSKJH1804-ZAITEN
               +   WRK-SKJ-ZAITEN
      *
           PERFORM VARYING IDXF    FROM    1   BY  1
                           UNTIL ( IDXF    >       5 )
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-ASA )
                   COMPUTE WSKJH1804-CNT-ASA
                       =   WSKJH1804-CNT-ASA
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-HIRU )
                   COMPUTE WSKJH1804-CNT-HIRU
                       =   WSKJH1804-CNT-HIRU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                           =   CONST-SKJ-YUU )
                   COMPUTE WSKJH1804-CNT-YUU
                       =   WSKJH1804-CNT-YUU
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-RYOYO1 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1
                       =   WSKJH1804-CNT-SKJRYOYO1
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-RYOYO1-RDS )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO1-RDS
                       =   WSKJH1804-CNT-SKJRYOYO1-RDS
                       +   1
               END-IF
      *
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-RYOYO2 )
                   COMPUTE WSKJH1804-CNT-SKJRYOYO2
                       =   WSKJH1804-CNT-SKJRYOYO2
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-TOKSYOKU )
                   COMPUTE WSKJH1804-CNT-TOKSYOKU
                       =   WSKJH1804-CNT-TOKSYOKU
                       +   1
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                       =   CONST-LIFE-SYOKUDO )
                   COMPUTE WSKJH1804-CNT-SYOKUDO
                       =   WSKJH1804-CNT-SYOKUDO
                       +   1
      *
               END-IF
               IF    ( WRK-SKJ-SRYCD (IDXF)
                                   NOT =   SPACE )
                   IF    ( WRK-SKJ-SRYCD (IDXF)
                                   =   LNK-NYULIFE-SKJFTN-SRYCD (IDXD))
                       COMPUTE WSKJH1804-CNT-SKJFTN
                           =   WSKJH1804-CNT-SKJFTN
                           +   1
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200212-SYOKUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    食事基本情報設定処理
      *****************************************************************
       20021-SYOKUJI-KHNINF-SET-SEC    SECTION.
      *
           MOVE    ZERO                TO  IDX-WSYOKUJI
      *
           MOVE    ZERO                TO  IDXB
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                     OR  ( IDXB    >   CONST-WSYOKUJI-MAX )
                     OR (( WSKJH1804-SRYCDTOTAL
                                   =   WSYOKUJI-SRYCDTOTAL (IDXB))
                      AND( WSKJH1804-SURYOTOTAL
                                   =   WSYOKUJI-SURYOTOTAL (IDXB))
                      AND( WSKJH1804-MEISAISU
                                   =   WSYOKUJI-MEISAISU   (IDXB))
                      AND( WSKJH1804-ZAITEN
                                   =   WSYOKUJI-ZAITEN     (IDXB)))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF   (    IDXB  >   WSYOKUJI-OCC1-MAX )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX   +   1
               IF    ( WSYOKUJI-OCC1-MAX   >   CONST-WSYOKUJI-MAX )
                   MOVE    CONST-WSYOKUJI-MAX  TO  WSYOKUJI-OCC1-MAX
                   MOVE    1                   TO  FLG-OVER
               ELSE
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDX-WSYOKUJI
                   MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
                   MOVE    IDXB                TO  WSYOKUJI-NUM(IDXB)
                   MOVE    WSKJH1804-SRYCDTOTAL
                                           TO  WSYOKUJI-SRYCDTOTAL(IDXB)
                   MOVE    WSKJH1804-SURYOTOTAL
                                           TO  WSYOKUJI-SURYOTOTAL(IDXB)
                   MOVE    WSKJH1804-MEISAISU
                                           TO  WSYOKUJI-MEISAISU  (IDXB)
                   MOVE    WSKJH1804-ZAITEN
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    "97"            TO  WSYOKUJI-SRYKBN (IDXB)
                   MOVE    ZERO            TO  IDXC
      *
                   IF    ( LNK-NYULIFE-KBN(IDXD)   =   SPACE )
                       PERFORM 200211-SYOKUJI-KHNINF-SET-SEC
                   ELSE
                       PERFORM 200212-SYOKUJI-KHNINF-SET-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       20021-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事基本情報設定処理（食事療養）
      *****************************************************************
       200211-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           IF    ( WSKJH1804-CNT-ASA     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-ASA
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-ASA
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-HIRU    >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-HIRU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-HIRU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-YUU     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-YUU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-YUU 
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO1     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO1
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJRYOYO2     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO2
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO2
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO1-RDS  >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1-RDS
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO1-RDS
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJRYOYO2-RDS     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO2-RDS
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-RYOYO2-RDS
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-TOKSYOKU      >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-TOKSYOKU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-TOKSYOKU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SYOKUDO       >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SYOKUDO
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-SYOKUDO
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    TSKJFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
               MOVE    1           TO  WSYOKUJI-PTFTN(IDXB)
           END-IF
      *
           .
       200211-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事基本情報設定処理（生活療養）
      *****************************************************************
       200212-SYOKUJI-KHNINF-SET-SEC   SECTION.
      *
           IF    ( WSKJH1804-CNT-ASA     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-ASA
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-ASA
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-HIRU    >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-HIRU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-HIRU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-YUU     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-YUU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-SKJ-YUU 
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO1     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-RYOYO1
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
      *
           IF    ( WSKJH1804-CNT-SKJRYOYO2     >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO2
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-RYOYO2
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJRYOYO1-RDS >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJRYOYO1-RDS
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-RYOYO1-RDS
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-TOKSYOKU      >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-TOKSYOKU
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-TOKSYOKU
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SYOKUDO       >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SYOKUDO
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    CONST-LIFE-SYOKUDO
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
           END-IF
           IF    ( WSKJH1804-CNT-SKJFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-SKJFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    LNK-NYULIFE-SKJFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
               MOVE    1           TO  WSYOKUJI-PTFTN (IDXB)
           END-IF
           IF    ( WSKJH1804-CNT-KANFTN        >   ZERO )
               COMPUTE IDXC    =   IDXC  +     1
               MOVE    WSKJH1804-CNT-KANFTN
                                   TO  WSYOKUJI-SURYO(IDXB IDXC)
               MOVE    LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                   TO  WSYOKUJI-SRYCD(IDXB IDXC)
               MOVE    1           TO  WSYOKUJI-PTFTN (IDXB)
           END-IF
      *
           MOVE    1               TO  WSYOKUJI-LIFE (IDXB)
      *
           .
       200212-SYOKUJI-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院時生活療養環境療養編集処理
      *****************************************************************
       20021-KANKYO-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-KANKYO-DAY-G
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    ZERO                TO  FLG-SANTEI
               IF    ( FLG-NYUINDAY (IDXD)     =   1 )
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4 )
                AND  ( LNK-NYULIFE-KBN(IDXD)   NOT =   SPACE )
                    IF   ( FLG-NYUINRYODAY (IDXD)  =   1 )
                     OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" )
                       COMPUTE CNT-KANKYO-DAY-TOTAL
                           =   CNT-KANKYO-DAY-TOTAL
                           +   1
                       MOVE    1           TO  CNT-KANKYO-DAY (IDXD)
                    END-IF
               END-IF
           END-PERFORM
      *
           IF    ( CNT-KANKYO-DAY-TOTAL    >   ZERO )
               COMPUTE WSYOKUJI-OCC1-MAX
                   =   WSYOKUJI-OCC1-MAX
                   +   1
               MOVE    WSYOKUJI-OCC1-MAX   TO  IDXB
      *
               PERFORM VARYING IDXH    FROM    1   BY  1
                       UNTIL ( IDXH    >   WSYOKUJI-OCC1-MAX - 1 )
                   IF    ( WSYOKUJI-LIFE (IDXH)    =   1 )
                       IF    ( WSYOKUJI-NUM (IDXB)
                                       =   ZERO )
                        OR   ( WSYOKUJI-NUM (IDXH)
                                       >   WSYOKUJI-NUM (IDXB) )
                           MOVE    WSYOKUJI-NUM (IDXH)
                                       TO  WSYOKUJI-NUM (IDXB)
                       END-IF
                   END-IF
               END-PERFORM
               IF    ( WSYOKUJI-NUM (IDXB) =   ZERO )
                   MOVE    999             TO  WSYOKUJI-NUM (IDXB)
               END-IF
      *
               MOVE    1                   TO  WSYOKUJI-KANKYO (IDXB)
               IF    ( WRK-SHOKUJIRYOYO    =   1 )
                   MOVE    CONST-LIFE-KANKYO1
                                           TO  WRK-SRYCD
                   COMPUTE WSYOKUJI-SRYCDTOTAL (IDXB)
                       =   WSYOKUJI-SRYCDTOTAL (IDXB)
                       +   WRK-SRYCD-9
      *
                   IF    ( WRK-LIFE-TEN-KANKYO1    =   ZERO )
      *                入院時生活療養（１）環境療養
                       MOVE    CONST-LIFE-KANKYO1  TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU           =   ZERO )
                           MOVE    TNS-TEN     TO  WRK-LIFE-TEN-KANKYO1
                       END-IF
                   END-IF
      *
                   MOVE    WRK-LIFE-TEN-KANKYO1
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    CONST-LIFE-KANKYO1
                                           TO  WSYOKUJI-SRYCD(IDXB 1)
               ELSE
                   MOVE    CONST-LIFE-KANKYO2
                                           TO  WRK-SRYCD
                   COMPUTE WSYOKUJI-SRYCDTOTAL (IDXB)
                       =   WSYOKUJI-SRYCDTOTAL (IDXB)
                       +   WRK-SRYCD-9
      *
                   IF    ( WRK-LIFE-TEN-KANKYO2    =   ZERO )
      *                入院時生活療養（２）環境療養
                       MOVE    CONST-LIFE-KANKYO2
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU   =   ZERO )
                           MOVE    TNS-TEN TO  WRK-LIFE-TEN-KANKYO2
                       END-IF
                   END-IF
      *
                   MOVE    WRK-LIFE-TEN-KANKYO2
                                           TO  WSYOKUJI-ZAITEN (IDXB)
                   MOVE    CONST-LIFE-KANKYO2
                                           TO  WSYOKUJI-SRYCD(IDXB 1)
               END-IF
               MOVE    "97"                TO  WSYOKUJI-SRYKBN (IDXB)
               MOVE    1                   TO  WSYOKUJI-SURYOTOTAL(IDXB)
               MOVE    CNT-KANKYO-DAY-TOTAL
                                           TO  WSYOKUJI-ZAIKAISU (IDXB)
               MOVE    1                   TO  WSYOKUJI-MEISAISU (IDXB)
               MOVE    1                   TO  WSYOKUJI-SURYO (IDXB 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    CNT-KANKYO-DAY (IDXD)
                                           TO  WSYOKUJI-DAY (IDXB IDXD)
               END-PERFORM
               MOVE    1                   TO  WSYOKUJI-LIFE(IDXB)
      *
               IF    ( HCM34S01-HKNNUM(1:2)            =   "98" )
                   CONTINUE
               ELSE
                   MOVE    IDXB                TO  IDX-KANKYO
                   PERFORM 200211-KANKYOFTN-HEN-SEC
               END-IF
      *
           END-IF
      *
           .
       20021-KANKYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院時生活療養環境療養標準負担額編集処理
      *****************************************************************
       200211-KANKYOFTN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
               IF    ( CNT-KANKYO-DAY (IDXD)   >   ZERO )
                   IF    ( LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                           =   SPACE )
                       CONTINUE
                   ELSE
                       INITIALIZE          WSKJH1804-AREA
                       MOVE    LNK-NYULIFE-KANFTN-SRYCD (IDXD)
                                       TO  WRK-SRYCD-NUM
                       MOVE    WRK-SRYCD-NUM
                                       TO  WSKJH1804-SRYCDTOTAL
                       MOVE    1       TO  WSKJH1804-SURYOTOTAL
                       MOVE    LNK-NYULIFE-KANFTN-MONEY (IDXD)
                                       TO  WSKJH1804-ZAITEN
                       MOVE    1       TO  WSKJH1804-MEISAISU
                       MOVE    1       TO  WSKJH1804-CNT-KANFTN
                       PERFORM 20021-SYOKUJI-ZAI-EDIT-SEC
                       IF    ( IDX-WSYOKUJI    >   ZERO )
                           MOVE    WSYOKUJI-NUM (IDX-KANKYO)
                                       TO   WSYOKUJI-NUM (IDX-WSYOKUJI)
                           COMPUTE WSYOKUJI-KANKYO (IDX-WSYOKUJI)
                               =   100 +   IDXD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200211-KANKYOFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算編集処理
      *****************************************************************
       2001-NYUINKSN-HEN-SEC           SECTION.
      *
      *    減算情報編集処理
           PERFORM 20011-NYUINGEN-HEN-SEC
      *
      *    加算情報編集処理
           PERFORM 20011-NYUINKSN-HEN-SEC
      *
           .
       2001-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    減算情報編集処理
      *****************************************************************
       20011-NYUINGEN-HEN-SEC          SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   4 )
      *
               IF    ( WRK-SYS-MIJISI (IDX4)   =   1 )
                   MOVE    SPACE           TO  WRK-TNS-SRYCD
      *
                   EVALUATE    IDX4
                   WHEN    2
                       MOVE  "190080570"   TO  WRK-TNS-SRYCD
                   WHEN    3
                       MOVE  "190111690"   TO  WRK-TNS-SRYCD
                   WHEN    4
                       MOVE  "190111790"   TO  WRK-TNS-SRYCD
                   END-EVALUATE
      *
                   IF    ( WRK-TNS-SRYCD   NOT =   SPACE )
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU       =   ZERO )
                           PERFORM 200111-NYUINGEN-HEN-SEC
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       20011-NYUINGEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    減算情報編集処理
      *****************************************************************
       200111-NYUINGEN-HEN-SEC         SECTION.
      *
          MOVE    1                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-NYUINGENDAY (IDX4 IDXD) =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO )
                    AND  ( FLG-TOKTEIDAY (IDXD)        =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           MOVE    TNS-TEN TO  WRK-ZAITEN
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
          MOVE    2                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                     OR  ( LNK-NYUINDAY-KBN(IDXD)   =   "1" ))
                    AND  ( FLG-NYUINGENDAY (IDX4 IDXD) =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)        =   ZERO )
                    AND  ( FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1       TO  FLG-TAISYO
                           MOVE    TNS-TEN TO  WRK-ZAITEN
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-NYUINGEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算基本情報設定処理
      *****************************************************************
       2001111-KSN-KHNINF-SET-SEC      SECTION.
      *
           COMPUTE WNYUINKSN-OCC2-MAX (IDXA)
               =   WNYUINKSN-OCC2-MAX (IDXA)   +   1
           MOVE    WNYUINKSN-OCC2-MAX (IDXA)   TO   IDXB
           MOVE    TNS-CDKBN-SYO       TO  WNYUINKSN-CDKBN-SYO
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-BU        TO  WNYUINKSN-CDKBN-BU
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBN       TO  WNYUINKSN-CDKBN-KBN
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBNNUM    TO  WNYUINKSN-CDKBN-KBNNUM
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  WNYUINKSN-CDKBN-KBNNUM-EDA
                                                           (IDXA IDXB)
           MOVE    TNS-CDKBN-KOUBAN    TO  WNYUINKSN-CDKBN-KOUBAN
                                                           (IDXA IDXB)
           MOVE    TNS-KOUHYOJYUNNUM   TO  WNYUINKSN-KOUHYOJYUNNUM
                                                           (IDXA IDXB)
           IF    ( IDXA                =   1 )
               MOVE    "90"            TO  WNYUINKSN-SRYKBN
                                                           (IDXA IDXB)
           ELSE
               MOVE    "92"            TO  WNYUINKSN-SRYKBN
                                                           (IDXA IDXB)
           END-IF
      *
           MOVE    WRK-ZAITEN          TO  WNYUINKSN-ZAITEN
                                                           (IDXA IDXB)
      *
           MOVE    TNS-SRYCD           TO  WNYUINKSN-SRYCD
                                                           (IDXA IDXB)
      *
           .
       2001111-KSN-KHNINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    加算情報編集処理
      *****************************************************************
       20011-NYUINKSN-HEN-SEC          SECTION.
      *
           MOVE    "6"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    NACCT-SRYCDTOTAL
                                       TO  WRK-SRYCDTOTAL
               MOVE    WRK-SRYCDTOTAL  TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   PERFORM 200111-NYUINKSN-HEN-SEC
               END-IF
      *
               PERFORM 1001-NYUINACCT-KEY17-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算情報編集処理
      *****************************************************************
       200111-NYUINKSN-HEN-SEC          SECTION.
      *
          MOVE    1                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( NACCT-DAY (IDXD)            >   ZERO )
                    AND  ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ( FLG-NYUINRYODAY (IDXD)      =   1 )
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4 )
                    AND  ( FLG-TOKTEIDAY (IDXD)        =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1               TO  FLG-TAISYO
                           MOVE    NACCT-ZAITEN    TO  WRK-ZAITEN
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
          MOVE    2                        TO  IDXA
          IF    ( WNYUINKSN-OCC2-MAX (IDXA)
                                   <   CONST-WNYUINKSN-MAX)
               MOVE    ZERO                TO FLG-TAISYO
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD        >   31 )
                   IF    ( NACCT-DAY (IDXD)            >   ZERO )
                    AND  ( FLG-NYUINDAY  (IDXD)        =   1  )
                    AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                      OR  ( LNK-NYUINDAY-KBN(IDXD)  =   "1" ))
                    AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4 )
                    AND  ( FLG-TOKTEIDAY (IDXD)    NOT =   ZERO )
                       IF    ( FLG-TAISYO              =   ZERO )
                           MOVE    1               TO  FLG-TAISYO
                           MOVE    NACCT-ZAITEN    TO  WRK-ZAITEN
                           PERFORM 2001111-KSN-KHNINF-SET-SEC
                       END-IF
                       MOVE    1           TO  WNYUINKSN-DAY
                                                       (IDXA IDXB IDXD)
                       COMPUTE WNYUINKSN-ZAIKAISU (IDXA IDXB)
                           =   WNYUINKSN-ZAIKAISU (IDXA IDXB)  +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-NYUINKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    療養担当手当編集処理
      *****************************************************************
       2001-RYYTAN-HEN-SEC             SECTION.
      *
           MOVE    "7"                 TO  WRK-NACCT-ZAISKBKBN
           PERFORM 1001-NYUINACCT-KEY17-SEL-SEC
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINACCT       NOT =   ZERO )
               GO  TO  2001-RYYTAN-HEN-EXT
           END-IF
      *
           MOVE    "199000510"         TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       =   ZERO )
               PERFORM 20011-RYYTAN-HEN-SEC
           END-IF
      *
           .
       2001-RYYTAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    療養担当手当編集処理
      *****************************************************************
       20011-RYYTAN-HEN-SEC            SECTION.
      *
           MOVE    "80"            TO  WRYYTAN-SRYKBN
           MOVE    TNS-TEN         TO  WRYYTAN-ZAITEN
           MOVE    TNS-SRYCD       TO  WRYYTAN-SRYCD
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( NACCT-DAY (IDXD)        >   ZERO )
                AND  ( FLG-NYUINDAY  (IDXD)    =   1  )
                AND  ((FLG-NYUINRYODAY (IDXD)      =   1 )
                  OR  ( LNK-NYUINDAY-KBN(IDXD)     =   "1" ))
                AND  ( FLG-GAIHAKUDAY(IDXD)    =   ZERO OR >= 4 )
                   MOVE    1       TO  WRYYTAN-DAY (IDXD)
                   COMPUTE WRYYTAN-ZAIKAISU
                       =   WRYYTAN-ZAIKAISU    +   1
               END-IF
      *
           END-PERFORM
      *
           .
       20011-RYYTAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    剤ＳＯＲＴ処理
      *****************************************************************
       2001-ZAISORT-SEC                SECTION.
      *
      *    ＳＯＲＴ作業領域編集処理
           PERFORM 20011-SORTAREA-HEN-SEC
      *
      *    ＳＯＲＴ作業領域ＳＯＲＴ処理
           PERFORM 20011-SORTAREA-SORT-SEC
      *
           .
       2001-ZAISORT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＯＲＴ作業領域編集処理
      *****************************************************************
       20011-SORTAREA-HEN-SEC          SECTION.
      *
           INITIALIZE                  SRT-AREA
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   2 )
      *
      *        入院料ＳＯＲＴ作業領域編集処理
               PERFORM 200111-NYUINRYO-SRT-SET-SEC
      *
      *        外泊ＳＯＲＴ作業領域編集処理
               PERFORM 200111-GAIHAKU-SRT-SET-SEC
      *
      *        入院料加算ＳＯＲＴ作業領域編集処理
               PERFORM 200111-NYUINKSN-SRT-SET-SEC
      *
           END-PERFORM
      *
      *    食事ＳＯＲＴ作業領域編集処理
           PERFORM 200111-SYOKUJI-SRT-SET-SEC
      *
      *    室料差額ＳＯＲＴ作業領域編集処理
           PERFORM 200111-BRMSAGAKU-SRT-SET-SEC
      *
           IF    ( WRYYTAN-SRYKBN      NOT =   SPACE )
      *        療養担当手当ＳＯＲＴ作業領域編集処理
               PERFORM 200111-RYYTAN-SRT-SET-SEC
           END-IF
      *
           .
       20011-SORTAREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-NYUINRYO-SRT-SET-SEC     SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WNYUINRYO-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WNYUINRYO-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAINUM (IDXA IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WNYUINRYO-ZAIKAISU(IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
               MOVE    "1"                 TO  SRT-ZAISKBKBN(SRT-MAX)
      *
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   20 )
                   MOVE    WNYUINRYO-SRYCD (IDXA IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WNYUINRYO-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-NYUINRYO-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    外泊ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-GAIHAKU-SRT-SET-SEC      SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WGAIHAKU-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WGAIHAKU-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    2                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WGAIHAKU-NUM (IDXA IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WGAIHAKU-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WGAIHAKU-ZAIKAISU(IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "2"                 TO  SRT-ZAISKBKBN(SRT-MAX)
      *
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   20 )
                   MOVE    WGAIHAKU-SRYCD (IDXA IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WGAIHAKU-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-GAIHAKU-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-NYUINKSN-SRT-SET-SEC     SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WNYUINKSN-OCC2-MAX (IDXA))
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WNYUINKSN-SRYKBN (IDXA IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    3                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WNYUINKSN-CDKBN-SYO (IDXA IDXB)
                                           TO  SRT-CDKBN-SYO(SRT-MAX)
      *
               MOVE    WNYUINKSN-CDKBN-BU  (IDXA IDXB)
                                           TO  SRT-CDKBN-BU (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBN (IDXA IDXB)
                                           TO  SRT-CDKBN-KBN(SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBNNUM (IDXA IDXB)
                                           TO  SRT-CDKBN-KBNNUM
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KBNNUM-EDA (IDXA IDXB)
                                           TO  SRT-CDKBN-KBNNUM-EDA
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-CDKBN-KOUBAN (IDXA IDXB)
                                           TO  SRT-CDKBN-KOUBAN
                                                            (SRT-MAX)
               MOVE    WNYUINKSN-KOUHYOJYUNNUM (IDXA IDXB)
                                           TO  SRT-KOUHYOJYUNNUM
                                                            (SRT-MAX)
      *
               MOVE    WNYUINKSN-ZAITEN (IDXA IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WNYUINKSN-ZAIKAISU (IDXA IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "6"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WNYUINKSN-SRYCD (IDXA IDXB)
                                           TO  SRT-SRYCD    (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WNYUINKSN-DAY (IDXA IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-NYUINKSN-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-SYOKUJI-SRT-SET-SEC      SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WSYOKUJI-OCC1-MAX )
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WSYOKUJI-SRYKBN (IDXB)
                                           TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WSYOKUJI-NUM (IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
               MOVE    WSYOKUJI-KANKYO (IDXB)
                                           TO  SRT-PRIORITY3(SRT-MAX)
      *
               MOVE    WSYOKUJI-ZAITEN (IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WSYOKUJI-ZAIKAISU(IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
               MOVE    WSYOKUJI-PTFTN   (IDXB)
                                           TO  SRT-PTFTN    (SRT-MAX)
      *
               MOVE    "4"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL ( IDXC    >   20 )
                   MOVE    WSYOKUJI-SURYO (IDXB IDXC)
                                           TO  SRT-SURYO (SRT-MAX IDXC)
                   MOVE    WSYOKUJI-SRYCD (IDXB IDXC)
                                           TO  SRT-SRYCD (SRT-MAX IDXC)
               END-PERFORM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WSYOKUJI-DAY (IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-SYOKUJI-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-BRMSAGAKU-SRT-SET-SEC    SECTION.
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WBRMSAGAKU-OCC1-MAX )
                    OR   ( SRT-MAX >=  CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    "95"                TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    1                   TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-NUM (IDXB)
                                           TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WBRMSAGAKU-ZAITEN (IDXB)
                                           TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-ZAIKAISU(IDXB)
                                           TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    WBRMSAGAKU-SAGAKUCD(IDXB)
                                           TO  SRT-SAGAKUCD (SRT-MAX)
      *
               MOVE    "3"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WBRMSAGAKU-SRYCD (IDXB)
                                           TO  SRT-SRYCD (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WBRMSAGAKU-DAY (IDXB IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-BRMSAGAKU-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養担当手当ＳＯＲＴ作業領域編集処理
      *****************************************************************
       200111-RYYTAN-SRT-SET-SEC       SECTION.
      *
           IF    ( SRT-MAX     <   CONST-SRT-MAX )
      *
               COMPUTE SRT-MAX =   SRT-MAX +   1
      *
               MOVE    WRYYTAN-SRYKBN      TO  SRT-SRYKBN   (SRT-MAX)
      *
               MOVE    ZERO                TO  SRT-PRIORITY (SRT-MAX)
      *
               MOVE    ZERO                TO  SRT-PRIORITY2(SRT-MAX)
      *
               MOVE    WRYYTAN-ZAITEN      TO  SRT-ZAITEN   (SRT-MAX)
      *
               MOVE    WRYYTAN-ZAIKAISU    TO  SRT-ZAIKAISU (SRT-MAX)
      *
               MOVE    "7"                 TO  SRT-ZAISKBKBN(SRT-MAX)
               MOVE    WRYYTAN-SRYCD
                                           TO  SRT-SRYCD (SRT-MAX 1)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    WRYYTAN-DAY (IDXD)
                                           TO  SRT-DAY (SRT-MAX IDXD)
               END-PERFORM
      *
           END-IF
      *
           .
       200111-RYYTAN-SRT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＳＯＲＴ作業領域ＳＯＲＴ処理
      *****************************************************************
       20011-SORTAREA-SORT-SEC         SECTION.
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING SRT-IDX1    FROM    2   BY  1
                   UNTIL ( SRT-IDX1    >   SRT-MAX )
               COMPUTE SRT-STIDX   =   SRT-IDX1    -   1
               MOVE    SRT-OCC1 (SRT-IDX1)
                                   TO  SRT-OCC1 (101)
               PERFORM VARYING SRT-IDX2    FROM    SRT-STIDX   BY  -1
                       UNTIL ( SRT-IDX2    <   1 )
                          OR ( SRT-KEY (101)
                                       >=  SRT-KEY (SRT-IDX2) )
                       MOVE    SRT-OCC1 (SRT-IDX2)
                                   TO  SRT-OCC1 (SRT-IDX2 + 1)
               END-PERFORM
               MOVE    SRT-OCC1 (101)
                                   TO  SRT-OCC1 (SRT-IDX2 + 1)
           END-PERFORM
      *
           .
       20011-SORTAREA-SORT-EXT.
           EXIT.
      *****************************************************************
      *    返却値設定処理
      *****************************************************************
       2001-HENKYAKU-SET-SEC           SECTION.
      *
           COMPUTE HCM34S01-SENTEI-PTFTN
               =   WRK-SENTEI-PTFTN (1)
               +   WRK-SENTEI-PTFTN (2)
      *
           MOVE    ZERO                TO  HCM34S01-HENKYAKU-MAX
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   SRT-MAX )
                    OR   ( IDXB    >   CONST-HENKYAKU-MAX )
      *
               IF    ( SRT-ZAIKAISU (IDXB) NOT =   ZERO )
      *
                   COMPUTE HCM34S01-HENKYAKU-MAX
                       =   HCM34S01-HENKYAKU-MAX   +   1
      *
                   MOVE    HCM34S01-HENKYAKU-MAX   TO  IDXE
      *
                   MOVE    SRT-SRYKBN   (IDXB) TO  HCM34S01-SRYKBN
                                                              (IDXE)
                   IF    ( SRT-ZAISKBKBN(IDXB) =   "7" )
                       MOVE    "850"           TO  HCM34S01-SRYSYUKBN
                                                              (IDXE)
                   END-IF
                   IF    ( SRT-SRYKBN  (IDXB) =   "97" )
                    AND  ( SRT-PTFTN   (IDXB) =   1  )
                       MOVE    "971"           TO  HCM34S01-SRYSYUKBN
                                                              (IDXE)
                   END-IF
                   MOVE    SRT-ZAITEN   (IDXB) TO  HCM34S01-ZAITEN
                                                              (IDXE)
                   MOVE    SRT-ZAIKAISU (IDXB) TO  HCM34S01-ZAIKAISU
                                                              (IDXE)
                   MOVE    SRT-ZAISKBKBN(IDXB) TO  HCM34S01-ZAISKBKBN
                                                              (IDXE)
                   MOVE    SRT-PRIORITY (IDXB) TO  HCM34S01-PRIORITY
                                                              (IDXE)
                   MOVE    SRT-PRIORITY2(IDXB) TO  HCM34S01-PRIORITY2
                                                              (IDXE)
                   MOVE    SRT-PRIORITY3(IDXB) TO  HCM34S01-PRIORITY3
                                                              (IDXE)
                   PERFORM VARYING IDXC    FROM    1   BY  1
                           UNTIL ( IDXC    >   20 )
                       IF    ( HCM34S01-SANTEIYM   >=  CONST-H1804 )
                        AND  ( SRT-SRYKBN (IDXB)   =   "97" )
                           MOVE    SRT-SURYO(IDXB IDXC)
                                               TO  HCM34S01-SURYO
                                                           (IDXE IDXC)
                       ELSE
                           MOVE    1           TO  HCM34S01-SURYO
                                                           (IDXE IDXC)
                       END-IF
                       MOVE    SRT-SRYCD(IDXB IDXC)
                                               TO  HCM34S01-SRYCD
                                                           (IDXE IDXC)
      *
                       MOVE    SRT-SRYCD(IDXB IDXC)
                                               TO  WRK-SRYCD
                       IF    ( WRK-SRYCD   NOT =   SPACE )
                           COMPUTE HCM34S01-MEISAISU (IDXE)
                               =   HCM34S01-MEISAISU (IDXE)
                               +   1
                           COMPUTE HCM34S01-SURYOUTOTAL (IDXE)
                               =   HCM34S01-SURYOUTOTAL (IDXE)
                               +   HCM34S01-SURYO (IDXE IDXC)
                           IF    ( WRK-SRYCD   IS  NUMERIC )
                               COMPUTE HCM34S01-SRYCDTOTAL (IDXE)
                                   =   HCM34S01-SRYCDTOTAL (IDXE)
                                   +   WRK-SRYCD-9
                               COMPUTE HCM34S01-RUIKEITOTAL(IDXE)
                                   =   HCM34S01-RUIKEITOTAL(IDXE)
                                   +   WRK-SRYCD-9 
                           END-IF
      *
                           MOVE    WRK-SRYCD   TO  WRK-TNS-SRYCD
                           PERFORM 900-TENSU-KEY-SEL-SEC
                           IF    ( TNS-TENSIKIBETU     =   7 OR 8 )
                               MOVE    1   TO  HCM34S01-ZAIMINUS (IDXE)
                           END-IF
      *
                       END-IF
      *
                   END-PERFORM
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       MOVE    SRT-DAY  (IDXB IDXD)
                                               TO  HCM34S01-DAY
                                                           (IDXE IDXD)
      *
                       IF    ( SRT-ZAISKBKBN(IDXB) =   "3" )
                           IF    ( SRT-DAY  (IDXB IDXD)    >   ZERO )
                               MOVE    SRT-SAGAKUCD (IDXB)
                                               TO  HCM34S01-SAGAKUCD
                                                           (IDXD)
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       2001-HENKYAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO                TO  HCM34S01-RC
      *
           CLOSE   CTENSU-FILE
                   CSENTEICDCHG-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-CTENSU
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-CSENTEICDCHG
      *
           IF    ( FLG-SET-DIR     NOT =  ZERO )
               INITIALIZE              STEMPDIR-AREA
               MOVE    "UNSET"         TO    STEMPDIR-MODE
               CALL    "ORCSTEMPDIR"   USING STEMPDIR-AREA
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理（主キー）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    HCM34S01-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           PERFORM 9001-CTENSU-START-SEC
           IF    ( FLG-TENSU           =   ZERO )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-TENSU
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                                               CTNS-TENSU-REC
                   WRITE   CTNS-TENSU-REC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ一時ファイルSTART処理
      *****************************************************************
       9001-CTENSU-START-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           MOVE    TNS-TENSU-REC       TO  CTNS-TENSU-REC
           MOVE    "00000000"          TO  CTNS-YUKOSTYMD
                                           CTNS-YUKOEDYMD
      *
           START   CTENSU-FILE
               KEY IS  >=   CTNS-KEY
           INVALID
               MOVE    1               TO  FLG-TENSU
           NOT INVALID
               PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                        OR   ( FLG-LOOPEND NOT =   ZERO )
      *
                   READ CTENSU-FILE    NEXT
                   AT END
                       MOVE    1       TO  FLG-TENSU
                   NOT AT END
                       IF    ( CTNS-HOSPNUM    =   TNS-HOSPNUM )
                        AND  ( CTNS-SRYCD      =   TNS-SRYCD )
                           IF    ( CTNS-YUKOSTYMD <= TNS-YUKOSTYMD )
                            AND  ( CTNS-YUKOEDYMD >= TNS-YUKOSTYMD )
                               MOVE    1   TO  FLG-LOOPEND
                               MOVE    CTNS-TENSU-REC
                                           TO  TNS-TENSU-REC
                           END-IF
                       ELSE
                           MOVE    1       TO  FLG-TENSU
                       END-IF
                   END-READ
                   IF    ( STS-CTENSU  >   10 )
                       MOVE    1           TO  FLG-TENSU
                   END-IF
               END-PERFORM
           END-START
      *
           .
       9001-CTENSU-START-EXT.
           EXIT.
      *****************************************************************
      *    選定療養コード置換検索処理
      *****************************************************************
       900-SENTEICDCHG-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    NSRY-SRYCD (IDX-KHN)
                                       TO  SENCHG-IPNSRYCD
           PERFORM 9001-CSENTEICDCHG-START-SEC
           IF    ( FLG-SENTEICDCHG         =   ZERO OR 2 )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-SENTEICDCHG
               MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO  )
                   MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
                                               CSENTEICDCHG-REC
                   WRITE   CSENTEICDCHG-REC
               ELSE
                   MOVE    1               TO  FLG-SENTEICDCHG
                   MOVE    SENTEICDCHG-REC TO  CSENTEICDCHG-REC
                   MOVE    CONST-NOT-FOUND TO  CSENCHG-SENTEISRYCD
                   WRITE   CSENTEICDCHG-REC
               END-IF
      *
               MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-SENTEICDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    選定コード置換一時ファイルSTART処理
      *****************************************************************
       9001-CSENTEICDCHG-START-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           MOVE    SENTEICDCHG-REC     TO  CSENTEICDCHG-REC
           MOVE    ZERO                TO  CSENCHG-SENTEISRYCD
      *
           START   CSENTEICDCHG-FILE
               KEY IS  >=   CSENCHG-KEY
           INVALID
               MOVE    1               TO  FLG-SENTEICDCHG
           NOT INVALID
      *
               READ CSENTEICDCHG-FILE  NEXT
               AT END
                   MOVE    1           TO  FLG-SENTEICDCHG
               NOT AT END
                   IF    ( CSENCHG-IPNSRYCD
                                   =   SENCHG-IPNSRYCD )
                       IF    ( CSENCHG-SENTEISRYCD
                                   =   CONST-NOT-FOUND )
                           MOVE    2   TO  FLG-SENTEICDCHG
                       ELSE
                           MOVE    CSENTEICDCHG-REC
                                       TO  SENTEICDCHG-REC
                       END-IF
                   ELSE
                       MOVE    1   TO  FLG-SENTEICDCHG
                   END-IF
               END-READ
               IF    ( STS-CSENTEICDCHG  >   10 )
                   MOVE    1       TO  FLG-SENTEICDCHG
               END-IF
           END-START
      *
           .
       9001-CSENTEICDCHG-START-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY62-SEL-SEC         SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
      *
           INITIALIZE                          NYUINACCT-REC
           MOVE    HCM34S01-HOSPNUM        TO  NACCT-HOSPNUM
           MOVE    HCM34S01-PTID           TO  NACCT-PTID
           MOVE    HCM34S01-SANTEIYM       TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    099999916               TO  NACCT-SRYCDTOTAL
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key62"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計読み込み処理
      *****************************************************************
       900-NYUINACCT-KEY62-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key62"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY62-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCHCM34S01 ERRCD [" HCM34S01-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
