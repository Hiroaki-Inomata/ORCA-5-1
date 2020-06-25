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
      *    データチェック（バッチ）共通コピー句
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  DTCHKCOMMON-AREA.
      *
      *    保険ＩＤ退避ワーク
         02  THKNID-AREA.
           03  THKNID-MAX                              PIC 9(05).
           03  THKNID-IDX                              PIC 9(05).
           03  THKNID-OCC                              OCCURS 20.
               05  THKNID-HKNID                        PIC 9(10).
               05  THKNID-HKNNUM                       PIC X(03).
               05  THKNID-HOJOKBN                      PIC X(01).
               05  THKNID-TEKSTYMD                     PIC X(08).
               05  THKNID-KAKUNINYMD                   PIC X(08).
               05  THKNID-HKNID-DAY-G.
                   07  THKNID-HKNID-DAY                PIC 9(01)
                                                       OCCURS  31.
      *
      *    公費ＩＤ退避ワーク
         02  TKOHID-AREA.
           03  TKOHID-MAX                              PIC 9(05).
           03  TKOHID-IDX                              PIC 9(05).
           03  TKOHID-OCC                              OCCURS 20.
               05  TKOHID-KOHID                        PIC 9(10).
               05  TKOHID-KOHNUM                       PIC X(03).
               05  TKOHID-KOHID-DAY-G.
                   07  TKOHID-KOHID-DAY                PIC 9(01)
                                                       OCCURS  31.
      *
      *    保険組合せ退避ワーク
      *    （THKNCOMBI-MAXは患者氏名のチェック対象の判定にも使用）
         02  THKNCOMBI-AREA.
           03  THKNCOMBI-MAX                           PIC 9(05).
           03  THKNCOMBI-OCC                           OCCURS 40.
               05  THKNCOMBI-HKNCOMBI                  PIC 9(04).
               05  THKNCOMBI-HKNNUM                    PIC X(03).
               05  THKNCOMBI-KOHNUM                    PIC X(03).
               05  THKNCOMBI-KOHID                     PIC 9(10).
               05  THKNCOMBI-ACCTKBN                   PIC X(01).
               05  THKNCOMBI-STYMD                     PIC X(08).
               05  THKNCOMBI-EDYMD                     PIC X(08).
               05  THKNCOMBI-DAY-G.
                   07  THKNCOMBI-DAY                   PIC 9(01)
                                                       OCCURS  31.
      *
         02  TNACCT-KKN-AREA.
           03  TNACCT-KKN-NODATA                       PIC 9(01).
           03  TNACCT-KKN-STYMD.
               05  TNACCT-KKN-STYM                     PIC X(06).
               05  TNACCT-KKN-STDD                     PIC 9(02).
           03  TNACCT-KKN-EDYMD.
               05  TNACCT-KKN-EDYM                     PIC X(06).
               05  TNACCT-KKN-EDDD                     PIC 9(02).
           03  TNACCT-KKN-DAY                          PIC 9(03)
                                                       OCCURS 31.
      *
         02  TNYUINRYO-AREA.
           03  TNYUINRYO-DAY                           PIC 9(03)
                                                       OCCURS 31.
         02  TNACCT-GAIHAKU-AREA.
           03  TNACCT-GAIHAKU-DAY                      PIC 9(03)
                                                       OCCURS 31.
      *
      *    診療科退避ワーク
         02  TSRYKA-AREA.
           03  TSRYKA-ALL-G.
               05  TSRYKA-ALL-SYOSIN-DAY               PIC 9(01)
                                                       OCCURS 31.
               05  TSRYKA-ALL-JNISSU-DAY               PIC 9(01)
                                                       OCCURS 31.
           03  TSRYKA-MAX                              PIC 9(05).
           03  TSRYKA-OCC                              OCCURS 30.
               05  TSRYKA-PTBYO-CNT1                   PIC 9(05).
               05  TSRYKA-PTBYO-CNT2                   PIC 9(05).
               05  TSRYKA-HKNTG-CNT                    PIC 9(05).
               05  TSRYKA-PTSYUBYO-CNT                 PIC 9(05).
               05  TSRYKA-SRYKA                        PIC X(02).
               05  TSRYKA-SYOSIN-DAY                   PIC 9(01)
                                                       OCCURS 31.
               05  TSRYKA-SYOSIN-YMD                   PIC X(08).
               05  TSRYKA-SAISIN-DAY-G.
                   07  TSRYKA-SAISIN-DAY               PIC 9(01)
                                                       OCCURS  31.
               05  TSRYKA-JNISSU-DAY-G.
                   07  TSRYKA-JNISSU-DAY               PIC 9(01)
                                                       OCCURS  31.
               05  TSRYKA-SYOSAIYMD-MAX                PIC 9(03).
               05  TSRYKA-SYOSAIYMD-OCC                OCCURS  50.
                   07  TSRYKA-TIYU-CHUSHI              PIC 9(01).
                   07  TSRYKA-BYO-KAISHI               PIC 9(01).
                   07  TSRYKA-BYO-KEIZOKU              PIC 9(01).
                   07  TSRYKA-SYOSAIYMD-SYOSINYMD      PIC X(08).
                   07  TSRYKA-SYOSAIYMD-SAISINYMD.
                       09  TSRYKA-SYOSAIYMD-SAISIN-YM  PIC X(06).
                       09  TSRYKA-SYOSAIYMD-SAISIN-DD  PIC 9(02).
               05  TSRYKA-SRYCD-MAX                    PIC 9(05).
               05  TSRYKA-SRYCD-OCC                    OCCURS 1000.
                   07  TSRYKA-SRYCD                    PIC X(09).
                   07  TSRYKA-SRYCD-DAY-G.
                       09  TSRYKA-SRYCD-DAY            PIC 9(01)
                                                       OCCURS  31.
               05  TSRYKA-PSRYCD-MAX                   PIC 9(05).
               05  TSRYKA-PSRYCD-OCC                   OCCURS 1000.
                   07  TSRYKA-PSRYCD                   PIC X(09).
                   07  TSRYKA-PSRYCD-DAY-G.
                       09  TSRYKA-PSRYCD-DAY           PIC 9(01)
                                                       OCCURS  31.
      *
      *    診療行為退避ワーク
         02  TSRYCD-AREA.
           03  TSRYCD-MAX                              PIC 9(05).
           03  TSRYCD-OCC                              OCCURS 2000.
               05  TSRYCD-TAISYO                       PIC 9(01).
      *        入外ともに同一の診療行為コードの設定があった場合は
      *        処理パラメタの入外区分を優先的に設定
               05  TSRYCD-NYUGAIKBN                    PIC X(01).
               05  TSRYCD-HKNFLG                       PIC 9(01).
               05  TSRYCD-SRYCD                        PIC X(09).
      *        当月の全算定回数
               05  TSRYCD-KAISU                        PIC 9(09).
      *        対象の保険、入外区分のみの算定回数
               05  TSRYCD-KAISU2                       PIC 9(09).
               05  TSRYCD-YKZKAISU                     PIC 9(09).
               05  TSRYCD-SURYOTOTAL                   PIC 9(05)V9(05).
               05  TSRYCD-TYPE                         PIC X(02).
               05  TSRYCD-FSANTEIYMD                   PIC X(08).
               05  TSRYCD-DAY-G.
                   07  TSRYCD-DAY                      PIC 9(01)
                                                       OCCURS  31.
               05  TSRYCD-DAY-KAISU-G.
                   07  TSRYCD-DAY-KAISU                PIC 9(09)
                                                       OCCURS  31.
               05  TSRYCD-DAY-KAISU2-G.
                   07  TSRYCD-DAY-KAISU2               PIC 9(09)
                                                       OCCURS  31.
               05  TSRYCD-DAY-NYUGAIKBN-G.
                   07  TSRYCD-DAY-NYUGAIKBN            PIC X(01)
                                                       OCCURS  31.
               05  TSRYCD-DAY-IHOKBN-G.
                   07  TSRYCD-DAY-IHOKBN               PIC X(01)
                                                       OCCURS  31.
               05  TSRYCD-DAY-HKNFLG-G.
                   07  TSRYCD-DAY-HKNFLG               PIC 9(01)
                                                       OCCURS  31.
               05  TSRYCD-GROUP-G.
                   07  TSRYCD-GROUP-OCC              OCCURS 100.
                       09  TSRYCD-GROUP.
                           11 TSRYCD-SRYKA           PIC X(02).
                           11 TSRYCD-DENPNUM         PIC 9(07).
                       09  TSRYCD-GROUP-HKNFLG       PIC 9(01).
                       09  TSRYCD-GROUP-KAISU        PIC 9(09).
                       09  TSRYCD-GROUP-DAY          PIC 9(02).
      *
      *    実日数退避ワーク（実日数チェック用）
         02  TJNISSU-AREA.
           03  TJNISSU-MAX                             PIC 9(05).
           03  TJNISSU-OCC                             OCCURS 20.
               05  TJNISSU-HKNNUM                      PIC X(03).
               05  TJNISSU-DAY-CNT                     PIC 9(03).
               05  TJNISSU-DAY                         PIC 9(01)
                                                       OCCURS 31.
      *
      *    算定回数退避ワーク（実日数チェック用）
         02  TKAISU-AREA.
           03  TKAISU-MAX                              PIC 9(05).
           03  TKAISU-OCC                              OCCURS 20.
               05  TKAISU-HKNNUM                       PIC X(03).
               05  TKAISU-SRYCD-G.
                   07  TKAISU-SRYCD-MAX                PIC 9(05).
                   07  TKAISU-SRYCD-OCC                OCCURS 2000.
                       09  TKAISU-SRYCD                PIC X(09).
                       09  TKAISU-SRYCD-CNT            PIC 9(03).
                       09  TKAISU-SRYCD-DAY            PIC 9(01)
                                                       OCCURS 31.
      *
      *    入院調剤料算定日退避ワーク
         02  TNYUIN-CHOZAI-AREA.
           03  TNYUIN-CHOZAI-DAY-OCC                   OCCURS 3.
               05  TNYUIN-CHOZAI-DAY                   PIC 9(01)
                                                       OCCURS 31.
      *
      *    入院診療行為日退避ワーク
         02  TNYUIN-ACCT-AREA.
           03  TNYUIN-ACCT-DAY-G.
               05  TNYUIN-ACCT-DAY                     PIC 9(01)
                                                       OCCURS 31.
      *
      *    併用禁忌（算定）退避ワーク
         02  THEIYOU-AREA.
           03  THEIYOU-MAX                             PIC 9(05).
           03  THEIYOU-OCC                             OCCURS 4000.
               05  THEIYOU-SRYCD-G.
                   07  THEIYOU-CHKKBN                  PIC X(01).
                   07  THEIYOU-INTERACT                PIC X(01).
                   07  THEIYOU-NYUGAIKBN1              PIC X(01).
                   07  THEIYOU-SRYCD1                  PIC X(09).
                   07  THEIYOU-DAY1-G.
                       09  THEIYOU-DAY1                PIC 9(01)
                                                       OCCURS 31.
                   07  THEIYOU-NYUGAIKBN2              PIC X(01).
                   07  THEIYOU-SRYCD2                  PIC X(09).
                   07  THEIYOU-DAY2-G.
                       09  THEIYOU-DAY2                PIC 9(01)
                                                       OCCURS 31.
      *
      *  器材単位チェック退避ワーク
         02  TKZI-AREA.
           03  TKZI-MAX                                PIC 9(05).
           03  TKZI-OCC                                OCCURS 30.
               05  TKZI-SRYCD                          PIC X(09).
               05  TKZI-058-SRYCD                      PIC X(09).
               05  TKZI-DAY-G.
                   07  TKZI-DAY                        PIC 9(01)
                                                       OCCURS 31.
           03  TKZI-MICD-MAX                           PIC 9(05).
           03  TKZI-MICD-OCC                           OCCURS 30.
               05  TKZI-MICD-SRYCD                     PIC X(09).
               05  TKZI-MICD-DAY-G.
                   07  TKZI-MICD-DAY                   PIC 9(01)
                                                       OCCURS 31.
      *
      *    チェックマスタ退避ワーク
         02  TCHK-AREA.
           03  TCHK-IDX                                PIC 9(08).
           03  TCHK-FLG                                PIC 9(01).
           03  TCHK-ACTIVE-G.
               05  TCHK-ACT-CHKKBN                     PIC X(01).
               05  TCHK-ACT-SRYCD                      PIC X(09).
               05  TCHK-ACT-ID                         PIC 9(09).
               05  TCHK-ACT-YUKOSTYMD                  PIC X(08).
               05  TCHK-ACT-YUKOEDYMD                  PIC X(08).
               05  TCHK-ACT-CDKBN                      PIC X(01).
               05  TCHK-ACT-BYOMEI                     PIC X(160).
               05  TCHK-ACT-CD-G.
                   07  TCHK-ACT-CD-MAX                 PIC 9(05).
                   07  TCHK-ACT-CD-OCC                 OCCURS  400.
                       09  TCHK-ACT-CD                 PIC X(09).
                       09  TCHK-ACT-DAYMONTHKBN        PIC X(01).
                       09  TCHK-ACT-NYUGAIKBN          PIC X(01).
                       09  TCHK-ACT-NYUGAIBNDKBN       PIC X(01).
                       09  TCHK-ACT-SYORIKBN           PIC X(01).
                       09  TCHK-ACT-CHKERR             PIC 9(01).
           03  TCHK-CHK-CHKKBN                         PIC X(01).
           03  TCHK-CHK-SRYCD                          PIC X(09).
           03  TCHK-CHK-ID                             PIC 9(09).
           03  TCHK-CHK-YMD                            PIC X(08).
           03  TCHK-CHK-CDKBN                          PIC X(01).
      *
           03  TCHK-STOCK-MAX                          PIC 9(05).
           03  TCHK-STOCK-G.
               05  TCHK-STOCK-OCC                      OCCURS  2000.
                   07  TCHK-STK-CHKKBN                 PIC X(01).
                   07  TCHK-STK-SRYCD                  PIC X(09).
                   07  TCHK-STK-ID                     PIC 9(09).
                   07  TCHK-STK-YUKOSTYMD              PIC X(08).
                   07  TCHK-STK-YUKOEDYMD              PIC X(08).
                   07  TCHK-STK-CDKBN                  PIC X(01).
                   07  TCHK-STK-BYOMEI                 PIC X(160).
                   07  TCHK-STK-CD-G.
                       09  TCHK-STK-CD-MAX             PIC 9(05).
                       09  TCHK-STK-CD-OCC             OCCURS  400.
                           11  TCHK-STK-CD             PIC X(09).
                           11  TCHK-STK-DAYMONTHKBN    PIC X(01).
                           11  TCHK-STK-NYUGAIKBN      PIC X(01).
                           11  TCHK-STK-NYUGAIBNDKBN   PIC X(01).
                           11  TCHK-STK-SYORIKBN       PIC X(01).
                           11  TCHK-STK-CHKERR         PIC 9(01).
      *
         02  TGENZAN-AREA.
           03  TGENZAN-VAL.
               05  TGENZAN-INNAI-G.
                   07  TGENZAN-INNAI-SRYCD         PIC X(09).
                   07  TGENZAN-INNAI-SRY-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-INNAI-NSRY-DAY      PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-INNAI-ERR-CNT       PIC 9(02).
                   07  TGENZAN-INNAI-ERR-DAY       PIC 9(01)
                                                   OCCURS  31.
               05  TGENZAN-ANZEN-G.
                   07  TGENZAN-ANZEN-SRYCD         PIC X(09).
                   07  TGENZAN-ANZEN-SRY-DAY       PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ANZEN-NSRY-DAY      PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ANZEN-ERR-CNT       PIC 9(02).
                   07  TGENZAN-ANZEN-ERR-DAY       PIC 9(01)
                                                   OCCURS  31.
               05  TGENZAN-JOKUSO-G.
                   07  TGENZAN-JOKUSO-SRYCD        PIC X(09).
                   07  TGENZAN-JOKUSO-SRY-DAY      PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-JOKUSO-NSRY-DAY     PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-JOKUSO-ERR-CNT      PIC 9(02).
                   07  TGENZAN-JOKUSO-ERR-DAY      PIC 9(01)
                                                   OCCURS  31.
           03  TGENZAN-R   REDEFINES   TGENZAN-VAL.
               05  TGENZAN-OCC                     OCCURS  3.
                   07  TGENZAN-SRYCD               PIC X(09).
                   07  TGENZAN-SRY-DAY             PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-NSRY-DAY            PIC 9(01)
                                                   OCCURS  31.
                   07  TGENZAN-ERR-CNT             PIC 9(02).
                   07  TGENZAN-ERR-DAY-G.
                       09  TGENZAN-ERR-DAY         PIC 9(01)
                                                   OCCURS  31.
      *
      *    入院料加算保険チェック用ワーク
         02  TNYUKSN-AREA.
           03  TNYUKSN-MAX                         PIC 9(05).
           03  TNYUKSN-OCC                         OCCURS  20.
               05  TNYUKSN-HKNCOMBI                PIC 9(04).
               05  TNYUKSN-HKNCOMBI-DAY            PIC 9(01)
                                                   OCCURS  31.
               05  TNYUKSN-ERR-OCC                 OCCURS  3.
                   07  TNYUKSN-ERR-CNT             PIC 9(02).
                   07  TNYUKSN-ERR-DAY-G.
                       09  TNYUKSN-ERR-DAY         PIC 9(01)
                                                   OCCURS  31.
      *
         02  TCOMERR-AREA.
           03  TCOMERR-FREE-DAY-G.
               05  TCOMERR-FREE-DAY                PIC 9(01)
                                                   OCCURS  31.
           03  TCOMERR-ZAI-MAX                     PIC 9(05).
           03  TCOMERR-ZAI-OCC                     OCCURS  30.
               05  TCOMERR-COM                     PIC X(80).
               05  TCOMERR-DAY-G.
                   07  TCOMERR-DAY                 PIC 9(01)
                                                   OCCURS  31.
      *
         02  TRSIRECEDEN-AREA.
           03  TRSIRECEDEN-MAX                     PIC 9(05).
           03  TRSIRECEDEN-OCC                     OCCURS  20.
               05  TRSIRECEDEN-SRYCD               PIC X(09).
               05  TRSIRECEDEN-KAISU               PIC 9(05).
               05  TRSIRECEDEN-DAY-G.
                   07  TRSIRECEDEN-DAY             PIC 9(01)
                                                   OCCURS  31.
      *
      *  院外処方領域
         02  TINGAI-AREA.
           03  TINGAI-MAX                          PIC 9(07).
           03  TINGAI-OCC                          OCCURS  20.
               05  TINGAI-HKNNUM                   PIC X(03).
               05  TINGAI-NYUGAIKBN                PIC X(01).
               05  TINGAI-TEN                      PIC 9(07).
      *
      *  介護突合チェック退避領域
         02  TKAIGO-AREA.
           03  TKAIGO-NINTEI-DAY-G.
               05  TKAIGO-NINTEI-DAY               PIC 9(01)
                                                   OCCURS  31.
           03  TKAIGO-ERR-DAY-G.
               05  TKAIGO-ERR-DAY                  PIC 9(01)
                                                   OCCURS  31.
           03  TKAIGO-IDX                          PIC 9(05).
           03  TKAIGO-MAX                          PIC 9(05).
           03  TKAIGO-OCC                          OCCURS  50.
               05  TKAIGO-SRYCD                    PIC X(09).
               05  TKAIGO-SRYCD-DAY                PIC 9(01)
                                                   OCCURS  31.
      *
         02  SRH-AREA.
           03  SRH-IDX                             PIC 9(05).
           03  SRH-HKNNUM-DAT.
               05  SRH-HKNNUM                      PIC X(03).
               05  SRH-HKNNUMMEI                   PIC X(10).
               05  SRH-HKNNUM-MAX                  PIC 9(05).
               05  SRH-HKNNUM-OCC                  OCCURS  50.
                   07  SRH-HKNNUML                 PIC X(03).
                   07  SRH-HKNNUMMEIL              PIC X(10).
           03  SRH-KOHNUM-DAT.
               05  SRH-KOHNUM                      PIC X(03).
               05  SRH-KOHNUMMEI                   PIC X(10).
               05  SRH-PAYKBN                      PIC X(02).
               05  SRH-KOHNUM-MAX                  PIC 9(05).
               05  SRH-KOHNUM-OCC                  OCCURS  150.
                   07  SRH-KOHNUML                 PIC X(03).
                   07  SRH-PAYKBNL                 PIC X(02).
                   07  SRH-KOHNUMMEIL              PIC X(10).
           03  SRH-1005-DAT.
               05  SRH-1005-SRYKA                  PIC X(02).
               05  SRH-1005-SRYKAMEI1              PIC X(20).
               05  SRH-1005-SRYKAMEI2              PIC X(12).
               05  SRH-1005-SRYKAMEI3              PIC X(04).
               05  SRH-1005-MAX                    PIC 9(05).
               05  SRH-1005-OCC                    OCCURS 100.
                   07  SRH-1005-SRYKAL             PIC X(02).
                   07  SRH-1005-SRYKAMEI1L         PIC X(20).
                   07  SRH-1005-SRYKAMEI2L         PIC X(12).
                   07  SRH-1005-SRYKAMEI3L         PIC X(04).
           03  SRH-1040-DAT.
               05  SRH-1040-KOHNUM                 PIC X(03).
               05  SRH-1040-FTNJANUMCHK            PIC X(01).
               05  SRH-1040-JKYSNUMCHK             PIC X(01).
               05  SRH-1040-FTNJHANKAKUCHK         PIC X(01).
               05  SRH-1040-FTNJZENKAKUCHK         PIC X(01).
               05  SRH-1040-FTNJNUMERICCHK         PIC X(01).
               05  SRH-1040-FTNJKETACHK            PIC X(01).
               05  SRH-1040-FTNJKETAMIN-X.
                   07  SRH-1040-FTNJKETAMIN        PIC 9(02).
               05  SRH-1040-FTNJKETAMAX-X.
                   07  SRH-1040-FTNJKETAMAX        PIC 9(02).
               05  SRH-1040-JKYSHANKAKUCHK         PIC X(01).
               05  SRH-1040-JKYSZENKAKUCHK         PIC X(01).
               05  SRH-1040-JKYSNUMERICCHK         PIC X(01).
               05  SRH-1040-JKYSKETACHK            PIC X(01).
               05  SRH-1040-JKYSKETAMIN-X.
                   07  SRH-1040-JKYSKETAMIN        PIC 9(02).
               05  SRH-1040-JKYSKETAMAX-X.
                   07  SRH-1040-JKYSKETAMAX        PIC 9(02).
               05  SRH-1040-MAX                    PIC 9(05).
               05  SRH-1040-OCC                    OCCURS  100.
                   07  SRH-1040-KOHNUML            PIC X(03).
                   07  SRH-1040-FTNJANUMCHKL       PIC X(01).
                   07  SRH-1040-JKYSNUMCHKL        PIC X(01).
                   07  SRH-1040-FTNJHANKAKUCHKL    PIC X(01).
                   07  SRH-1040-FTNJZENKAKUCHKL    PIC X(01).
                   07  SRH-1040-FTNJNUMERICCHKL    PIC X(01).
                   07  SRH-1040-FTNJKETACHKL       PIC X(01).
                   07  SRH-1040-FTNJKETAMINL       PIC 9(02).
                   07  SRH-1040-FTNJKETAMAXL       PIC 9(02).
                   07  SRH-1040-JKYSHANKAKUCHKL    PIC X(01).
                   07  SRH-1040-JKYSZENKAKUCHKL    PIC X(01).
                   07  SRH-1040-JKYSNUMERICCHKL    PIC X(01).
                   07  SRH-1040-JKYSKETACHKL       PIC X(01).
                   07  SRH-1040-JKYSKETAMINL       PIC 9(02).
                   07  SRH-1040-JKYSKETAMAXL       PIC 9(02).
           03  SRH-1102-DAT.
               05  SRH-1102-HKNJANUM               PIC X(08).
               05  SRH-1102-HKNNUM                 PIC X(03).
               05  SRH-1102-CHK-DAT.
                   07  SRH-1102-KIGOCHK            PIC X(01).
                   07  SRH-1102-KGCHKPTN           PIC X(01).
                   07  SRH-1102-KGSUJI             PIC X(01).
                   07  SRH-1102-KGKANJI            PIC X(01).
                   07  SRH-1102-KGABT              PIC X(01).
                   07  SRH-1102-KGHYPN             PIC X(01).
                   07  SRH-1102-KGKAKKO            PIC X(01).
                   07  SRH-1102-KGETC              PIC X(01).
                   07  SRH-1102-KGKANJINASHI       PIC X(01).
                   07  SRH-1102-KGKETAMIN          PIC X(02).
                   07  SRH-1102-KGKETAMAX          PIC X(02).
                   07  SRH-1102-NMSUJINASHI        PIC X(01).
                   07  SRH-1102-NMKETAMIN          PIC X(02).
                   07  SRH-1102-NMKETAMAX          PIC X(02).
                   07  SRH-1102-NM1CHKPTN          PIC X(01).
                   07  SRH-1102-NM1SUJI            PIC X(01).
                   07  SRH-1102-NM1KANJI           PIC X(01).
                   07  SRH-1102-NM1ABT             PIC X(01).
                   07  SRH-1102-NM1HYPN            PIC X(01).
                   07  SRH-1102-NM1KAKKO           PIC X(01).
                   07  SRH-1102-NM1ETC             PIC X(01).
                   07  SRH-1102-NM2CHKPTN          PIC X(01).
                   07  SRH-1102-NM2SUJI            PIC X(01).
                   07  SRH-1102-NM2KANJI           PIC X(01).
                   07  SRH-1102-NM2ABT             PIC X(01).
                   07  SRH-1102-NM2HYPN            PIC X(01).
                   07  SRH-1102-NM2KAKKO           PIC X(01).
                   07  SRH-1102-NM2ETC             PIC X(01).
                   07  SRH-1102-UPDDUMMY           PIC X(01).
               05  SRH-1102-MAX                    PIC 9(05).
               05  SRH-1102-OCC                    OCCURS  200.
                   07  SRH-1102-HKNJANUML          PIC X(08).
                   07  SRH-1102-HKNJANUMLENL       PIC 9(03).
                   07  SRH-1102-JYKNKBNL           PIC X(01).
                   07  SRH-1102-HKNNUML            PIC X(03).
                   07  SRH-1102-KIGOCHKL           PIC X(01).
                   07  SRH-1102-KGCHKPTNL          PIC X(01).
                   07  SRH-1102-KGSUJIL            PIC X(01).
                   07  SRH-1102-KGKANJIL           PIC X(01).
                   07  SRH-1102-KGABTL             PIC X(01).
                   07  SRH-1102-KGHYPNL            PIC X(01).
                   07  SRH-1102-KGKAKKOL           PIC X(01).
                   07  SRH-1102-KGETCL             PIC X(01).
                   07  SRH-1102-KGKANJINASHIL      PIC X(01).
                   07  SRH-1102-KGKETAMINL         PIC X(02).
                   07  SRH-1102-KGKETAMAXL         PIC X(02).
                   07  SRH-1102-NMSUJINASHIL       PIC X(01).
                   07  SRH-1102-NMKETAMINL         PIC X(02).
                   07  SRH-1102-NMKETAMAXL         PIC X(02).
                   07  SRH-1102-NM1CHKPTNL         PIC X(01).
                   07  SRH-1102-NM1SUJIL           PIC X(01).
                   07  SRH-1102-NM1KANJIL          PIC X(01).
                   07  SRH-1102-NM1ABTL            PIC X(01).
                   07  SRH-1102-NM1HYPNL           PIC X(01).
                   07  SRH-1102-NM1KAKKOL          PIC X(01).
                   07  SRH-1102-NM1ETCL            PIC X(01).
                   07  SRH-1102-NM2CHKPTNL         PIC X(01).
                   07  SRH-1102-NM2SUJIL           PIC X(01).
                   07  SRH-1102-NM2KANJIL          PIC X(01).
                   07  SRH-1102-NM2ABTL            PIC X(01).
                   07  SRH-1102-NM2HYPNL           PIC X(01).
                   07  SRH-1102-NM2KAKKOL          PIC X(01).
                   07  SRH-1102-NM2ETCL            PIC X(01).
                   07  SRH-1102-UPDDUMMYL          PIC X(01).
