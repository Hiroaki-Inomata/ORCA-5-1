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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR1010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 公害レセプト（入院外）
      *                      診療区分別点数集計
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-門脇    19/03/11  予約コード読み飛ばし修正
      *  05.00.02    NACL-門脇    19/04/03  コメントマスタの改元対応に
      *                                     伴う修正
      *  05.00.03    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.04    NACL-門脇    19/12/26  器材点数０点時のコメントコード
      *                                     の取り扱い修正
      *  05.00.05    NACL-門脇    20/02/10  薬剤・器材の単位編集（点数付加）
      *  05.00.06    NACL-門脇    20/03/30  令和２年４月改正対応
      *  05.00.07    NACL-門脇    20/04/16  器材の単位編集修正
      *  05.00.08    NACL-門脇    20/04/28  「他保険にて診察料算定済」の名称
      *                                     編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE100-FILE     ASSIGN  RECE100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE100-KEY
                                   FILE    STATUS  IS  STS-RECE100.
           SELECT  RECE120-FILE     ASSIGN  RECE120PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE120-KEY
                                   FILE    STATUS  IS  STS-RECE120.
           SELECT  RECE130-FILE     ASSIGN  RECE130PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE130-KEY
                                   FILE    STATUS  IS  STS-RECE130.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE100-FILE.
       01  RECE100-REC. 
           COPY    "CPRCF100.INC".
      *
       FD  RECE120-FILE.
       01  RECE120-REC. 
           COPY    "CPRCF120.INC".
      *
       FD  RECE130-FILE.
       01  RECE130-REC. 
           COPY    "CPRCF130.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE100//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE120//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE130//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE100         PIC X(02).
           03  STS-RECE120         PIC X(02).
           03  STS-RECE130         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-RECE100                             PIC 9(01).
           03  FLG-KANJI                               PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE130                             PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).       
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-ZAIHOU-COM                          PIC 9(01).
           03  FLG-SYOUNI-COM                          PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-SAIDOU-COM                          PIC 9(01).
           03  FLG-NIKOCHIN-COM                        PIC 9(01).
           03  FLG-ZAIKYO-COM                          PIC 9(01).
           03  FLG-BOKOHO-COM                          PIC 9(01).
           03  FLG-IZONSYO-COM                         PIC 9(01).
           03  FLG-NINRYO-COM                          PIC 9(01).
           03  FLG-ZENSOKU-COM                         PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-ZAIRIHA-COM                         PIC 9(01).
           03  FLG-NYUSEN-COM                          PIC 9(01).
           03  FLG-KIATU-COM                           PIC 9(01).
           03  FLG-GAIJIR-COM                          PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-WRITE05                             PIC 9(01).
           03  FLG-WRITE05X                            PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-NEBU                                PIC 9(01).
           03  FLG-HENSYU-NG                           PIC 9(01).
           03  FLG-RIHA-OKIKAE                         PIC 9(02).
           03  FLG-ZAITAKU-CHECK                       PIC 9(01).
           03  FLG-IRYORENKEI                          PIC 9(01).
           03  FLG-SIPPU-COM                           PIC 9(01).
           03  FLG-SIPPU-ARI                           PIC 9(01).
           03  FLG-YKZ-PRTKBN                          PIC 9(01).
           03  FLG-KIZ-PRTKBN                          PIC 9(01).
           03  FLG-ARI5                                PIC 9(01).
      *
      *    基本点数の非表示判定用フラグ
       01  FLG-KHNTEN-AREA.
           03  FLG-KHNTEN2                             PIC 9(01).
           03  FLG-KHNTEN3                             PIC 9(01).
           03  FLG-KHNTEN4                             PIC 9(01).
           03  FLG-KHNTEN5                             PIC 9(01).
           03  FLG-KHNTEN6                             PIC 9(01).
           03  FLG-KHNTEN17                            PIC 9(01).
           03  FLG-KHNTEN20                            PIC 9(01).
           03  FLG-KHNTEN21                            PIC 9(01).
      *
      *    再診料基本点数加算フラグ
       01  FLG-SAISIN-AREA.
           03  FLG-NYUYOJI                             PIC 9(01).
           03  FLG-YOJI                                PIC 9(01).
           03  FLG-YOJI-ARI                            PIC 9(01).
           03  FLG-SAISIN-ARI                          PIC 9(01).
           03  FLG-KOKENKSN-ARI                        PIC 9(01).
           03  FLG-MESAIKSN-ARI                        PIC 9(01).
           03  FLG-CHIHOKATU-ARI                       PIC 9(01).
           03  FLG-CHIHOKAT2-ARI                       PIC 9(01).
           03  FLG-JIKANTAI1-ARI                       PIC 9(01).
           03  FLG-JIKANTAI3-ARI                       PIC 9(01).
           03  FLG-CHIHOKAT3-ARI                       PIC 9(01).
           03  FLG-CHIHOKAT4-ARI                       PIC 9(01).
           03  FLG-YAKRENKEI-ARI                       PIC 9(01).
           03  FLG-NINPU-ARI                           PIC 9(01).
           03  FLG-NINPU2-ARI                          PIC 9(01).
      *
      *    乳幼児加算カウント
       01  CNT-SAISIN-AREA.
           03  CNT-NYUYOJI                             PIC 9(02).
           03  CNT-NYUYOJI2                            PIC 9(02).
           03  CNT-KOKENKSN                            PIC 9(02).
           03  CNT-MESAIKSN                            PIC 9(02).
           03  CNT-CHIHOKATU                           PIC 9(02).
           03  CNT-CHIHOKAT2                           PIC 9(02).
           03  CNT-JIKANTAI1                           PIC 9(02).
           03  CNT-JIKANTAI3                           PIC 9(02).
           03  CNT-CHIHOKAT3                           PIC 9(02).
           03  CNT-CHIHOKAT4                           PIC 9(02).
           03  CNT-YAKRENKEI                           PIC 9(02).
           03  CNT-NINPU                               PIC 9(02).
           03  CNT-NINPU2                              PIC 9(02).
           03  CNT-SAISIN-DOUJITU                      PIC 9(02).
           03  CNT-SAISIN-ONLINE                       PIC 9(02).
           03  CNT-SAISIN-ETC                          PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX15                                   PIC 9(03).
           03  IDX17                                   PIC 9(03).
           03  IDX19                                   PIC 9(03).
           03  IDX20                                   PIC 9(03).
           03  IDX21                                   PIC 9(03).
           03  IDX22                                   PIC 9(02).
           03  IDX33                                   PIC 9(03).
           03  IDX34                                   PIC 9(03).
           03  IDX40                                   PIC 9(02).
           03  IDX41                                   PIC 9(02).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDXX                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW2                                    PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  IDW9                                    PIC 9(02).
           03  IDV                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  IDZ1                                    PIC 9(03).
           03  IDZ2                                    PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  ACCT-IDX                                PIC 9(02).       
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    剤情報退避領域
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    点数テーブル検索用対象日領域
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
      *    点数マスタ情報退避領域
       01  WRK-TNS-TAIHI-AREA.
           03  WRK-TNS-TAIHI-TABLE                     OCCURS  5.
               05  WRK-TNS-CHUKSNTUBAN                 PIC X(01).
               05  WRK-TNS-DATAKBN                     PIC 9(01).
               05  WRK-TNS-TIMEKSNKBN                  PIC 9(01).
               05  WRK-TNS-TEN                         PIC 9(09)V99.    
               05  WRK-TNS-GAITENTTLKBN                PIC 9(03).
               05  WRK-TNS-OSINKBN                     PIC 9(01).
               05  WRK-TNS-KNSJISKBN                   PIC 9(01).
               05  WRK-TNS-SRYCD                       PIC X(09).
      *
      *    院外処方一般名編集情報退避領域
       01  WRK-ORCSGETGE-OUT-AREA.
           03  WRK-ORCSGETGE-OUT-TABLE                 OCCURS  50.
               05  WRK-ORCSGETGE-YAKKAKJNCD            PIC X(12).
               05  WRK-ORCSGETGE-GENERIC-KBN           PIC X(01).
               05  WRK-ORCSGETGE-GENERICNAME           PIC X(200).
      *
      *    実日数カウント用領域                                         
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC X(01)
                                                       OCCURS   31.
      *    実日数減カウント用領域                                         
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-DAY-GEN                      PIC X(01)
                                                       OCCURS   31.
      *    施設基準情報退避用領域
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *    帳票編集区分情報退避用領域
       01  TBL-SYS1030-T.
           03  TBL-SYS1030-OCC                         OCCURS  100.
               05  TBL-SYS1030                         PIC X(610).
      *
       01  TBL-ZAITAKU-KAISU-XXX.
           03  TBL-ZAITAKU-KAISU-XXX2                  OCCURS   6.
             05  TBL-ZAITAKU-KAISU-XXX3                OCCURS   9.
               07  TBL-ZAITAKU-KAISU-XXX4              OCCURS  31.
                 09  TBL-ZAITAKU-KAISU                 PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-ZENKAKU                             PIC X(80).
           03  WRK-NUM.
               05  WRK-NUM-X                           PIC X(10).
               05  FILLER                              PIC X(70).
           03  WRK-NUM-9                               PIC 9(10). 
           03  WRK-STIDX                               PIC 9(02).
           03  WRK-EDIDX                               PIC 9(02).
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
      *
           03  WRK-SRYCD                               PIC 9(09).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
           03  WRK-SISETU3055                          PIC 9(01).
           03  WRK-SISETU3056                          PIC 9(01).
           03  WRK-SISETU3057                          PIC 9(01).
           03  WRK-SISETU3058                          PIC 9(01).
           03  WRK-SISETU3168                          PIC 9(01).
           03  WRK-SISETU3169                          PIC 9(01).
      *
           03  WRK-FILMTEN                             PIC 9(07)V9(02).
           03  WRK-FILMTEN2                            PIC 9(07)V9(02).
      *
           03  WRK-RYOYOTEATE                          PIC 9(07).
      *
      *    数字半角全角変換用     
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *     
      *    単価編集用     
           03  WRK-TANKA                       PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      *
           03  WRK-SRYCDTOTAL9                 PIC 9(14).
           03  WRK-SRYCDTOTALX   REDEFINES     WRK-SRYCDTOTAL9
                                               PIC X(14).
           03  WRK-ZAINUM                      PIC 9(08).
      *
      ***  03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-KINGAKU                PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-TOTALTEN1                   PIC 9(08).
           03  WRK-TOTALTEN2                   PIC 9(08).
      *
           03  WRK-SURYOU-COUNT                PIC 9(09).
      *
           03  WRK-COL                         PIC 9(03).
      *
           03  WRK-RIHA-YMD                    PIC X(07).
           03  WRK-RIHA-YMD-SEI                PIC X(08).
      *
           03  WRK-1030-KOUHATUKA              PIC X(01).
      *
      *    数量編集用
           03  WRK-SURYOKETA                   PIC 9(02).
           03  WRK-SURYOKETA1                  PIC 9(02).
           03  WRK-SURYOKETA2                  PIC 9(02).
           03  WRK-SURYOST                     PIC 9(02).
           03  WRK-SURYO                       PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1                PIC 9(05).
               05  WRK-SURYO-S2                PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                PIC ZZZZZ.
               05  WRK-SURYO-Z2                PIC X(01).
               05  WRK-SURYO-Z3                PIC ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1                PIC X(10).
               05  WRK-SURYO-N2                PIC X(02).
               05  WRK-SURYO-N3                PIC X(10).
           03  WRK-SRYSURYOW                   PIC X(24).
      *
      *    パス名領域
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *---------------------------------------------------------------
      *  回数減算用退避領域
      *    処方減回数
           03  WRK-SYOHOU-KAISU                PIC 9(03).
      *    処方せん減回数
           03  WRK-SYOSEN-KAISU                PIC 9(03).
      *    処方料（麻・向・毒・覚）
           03  WRK-MADOKU-SHOHO                PIC 9(03).
      *---------------------------------------------------------------
      *
      *    剤の診療内容チェックテーブル
       01  TBL-ZAICHECK-AREA.
      *    （サ）
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    （検選）
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    （リハ選）
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    （精選）
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    酸素 
           COPY    "CMSANSOTBL.INC".
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
      *
      *    改行コード
       01  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
       01  WRK-RECE100-REC. 
           COPY    "CPRCF100.INC"  REPLACING  //RECE100//
                                   BY         //WRK-RECE100//.
      *
       01  WRK-RECE100G-REC. 
           COPY    "CPRCF100.INC"  REPLACING  //RECE100//
                                   BY         //WRK-RECE100G//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *    院外処方一般名編集サブ
           COPY    "CPORCSGETGENERIC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE900"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE100PARA
      *
           MOVE    "RECE920"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE120PARA
      *
           MOVE    "RECE930"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE130PARA
      *
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR1010"          TO  JOB-PGID
               MOVE    "診療区分別点数集計"
                                           TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       施設基準情報
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              MOVE     ZERO     TO     WRK-SISETU3055
              MOVE     ZERO     TO     WRK-SISETU3056
              MOVE     ZERO     TO     WRK-SISETU3057
              MOVE     ZERO     TO     WRK-SISETU3058
              MOVE     ZERO     TO     WRK-SISETU3168
              MOVE     ZERO     TO     WRK-SISETU3169
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
                 MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
                 MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                 MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                 MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
                 MOVE    "key10"          TO  WRK-MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
                 IF      FLG-SYSKANRI     =   ZERO
                   MOVE     MCPDATA-REC   TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
                     WHEN     7
                       MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                       MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                       MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                       MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                       MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                       MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
                   END-EVALUATE
                 END-IF
               END-IF
              END-PERFORM
      *       帳票編集区分情報
              MOVE    SPACE               TO  WRK-1030-KOUHATUKA
      **>     IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1030"              TO  SYS-KANRICD
                MOVE    "*"                 TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
                MOVE    "key10"             TO  WRK-MCP-PATHNAME
                PERFORM 960-KANRIMST-READ-SEC
                IF      FLG-SYSKANRI     =   ZERO
                  MOVE  MCPDATA-REC         TO  SYS-1030-REC
                  MOVE  SYS-1030-KOUHATUKA  TO  WRK-1030-KOUHATUKA
                END-IF
      **>     END-IF
           ELSE
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC              =   ZERO
                     MOVE    "tbl_syskanri"      TO  MCP-TABLE
                     MOVE    "key8"              TO  MCP-PATHNAME
                     PERFORM  970-KANRIMST-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC   TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 970-KANRIMST-READ-SEC
      *
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
              END-PERFORM
      *
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1030-T
              INITIALIZE                      TBL-SYS1030-T
      **>     IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1030"              TO  SYS-KANRICD
                MOVE    "*"                 TO  SYS-KBNCD
                MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"    TO  MCP-TABLE
                  MOVE    "key8"            TO  MCP-PATHNAME
                  PERFORM 970-KANRIMST-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC     TO  SYS-1030-REC
                    ADD     1               TO  SYSKAN-IDX
                    MOVE    SYS-1030-REC    TO  TBL-SYS1030(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 970-KANRIMST-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key8"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
      **>     END-IF
           END-IF
      *
           MOVE    SPACE                  TO  SYSKANRI-REC
           INITIALIZE                         SYSKANRI-REC
           MOVE    "2005"                 TO  SYS-KANRICD
           MOVE    "01"                   TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM         TO  SYS-STYUKYMD (1:6)
           MOVE    "01"                   TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD           TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"         TO  WRK-MCP-TABLE
           MOVE    "key10"                TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-200501-REC
           ELSE
               INITIALIZE                     SYS-200501-REC
           END-IF
           IF      SYS-200501-INSPECT-GENERAL-KBN   =   SPACE
               MOVE    "0"   TO  SYS-200501-INSPECT-GENERAL-KBN
           END-IF
           IF      SYS-200501-COMPRESS-KBN          =   SPACE
               MOVE    "0"   TO  SYS-200501-COMPRESS-KBN
           END-IF
           IF      SYS-200501-15-YKZ-PRTKBN         =   SPACE
               MOVE    "0"   TO  SYS-200501-15-YKZ-PRTKBN
           END-IF
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              RECE130-FILE
           CLOSE                       RECE130-FILE
      *
           OPEN    I-O                 RECE100-FILE
           OPEN    OUTPUT              RECE120-FILE
           OPEN    I-O                 RECE130-FILE
      *
           PERFORM    RECE100-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE100 =   1
                                       OR      FLG-END     =   1
      *        編集処理
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE100-READ-SEC
      *
               IF      CNT-IN-COMMIT   =   300
                   MOVE  ZERO              TO  CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE100-FILE
                   RECE120-FILE
                   RECE130-FILE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                   WRK-RECE100G-REC
      *
           INITIALIZE                   TBL-JNISSU                      
           INITIALIZE                   TBL-JNISSU-GEN
      *
           INITIALIZE                   FLG-KHNTEN-AREA
           INITIALIZE                   FLG-SAISIN-AREA
           INITIALIZE                   CNT-SAISIN-AREA
      *
           INITIALIZE                   TBL-ZAITAKU-KAISU-XXX
      *
           MOVE    88888880     TO      WRK-ZAINUM
      *
           MOVE    ZERO         TO      WRK-SURYOU-COUNT
      *
           PERFORM VARYING IDX    FROM    1   BY   1
                   UNTIL ( IDX    >       20    )
                   OR    ( RECE100-SRYKA(IDX)
                                  =       SPACE )
               PERFORM 2104-RECE120-HENSYU-SEC
           END-PERFORM
           PERFORM     2105-GENTEN-HENSYU-SEC
           PERFORM     2106-JNISSU-SET-SEC                                   
           PERFORM     2107-KHNTEN-CLEAR-SEC
           PERFORM     2110-NYUYOJI-COMENTFLG-SET-SEC
      *
      *    在宅回数の再編集
           PERFORM     2108-ZAITAKU-KAISU-HENSYU-SEC
      *
           PERFORM     RECE100-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅回数の再編集
      *****************************************************************
       2108-ZAITAKU-KAISU-HENSYU-SEC               SECTION.
      *
           IF    RECE100-SRYYM   >=  "201404"
      *
      *      患家診療時間加算（往診）に関する回数の再編集
             PERFORM  VARYING  IDX19  FROM  1  BY  1
                       UNTIL   IDX19   >    9
               PERFORM  VARYING  IDX20  FROM  1  BY  1
                         UNTIL   IDX20   >   31
                 IF  TBL-ZAITAKU-KAISU(1 IDX19 IDX20) NOT = ZERO
                 AND TBL-ZAITAKU-KAISU(2 IDX19 IDX20) NOT = ZERO
                   COMPUTE  RECE100-KAISU(10) =
                            RECE100-KAISU(10) -
                            TBL-ZAITAKU-KAISU(2 IDX19 IDX20)
                 END-IF
               END-PERFORM
             END-PERFORM
      *      患家診療時間加算（特別往診）に関する回数の再編集
             PERFORM  VARYING  IDX19  FROM  1  BY  1
                       UNTIL   IDX19   >    9
               PERFORM  VARYING  IDX20  FROM  1  BY  1
                         UNTIL   IDX20   >   31
                 IF  TBL-ZAITAKU-KAISU(3 IDX19 IDX20) NOT = ZERO
                 AND TBL-ZAITAKU-KAISU(4 IDX19 IDX20) NOT = ZERO
                   COMPUTE  RECE100-KAISU(10) =
                            RECE100-KAISU(10) -
                            TBL-ZAITAKU-KAISU(4 IDX19 IDX20)
                 END-IF
               END-PERFORM
             END-PERFORM
      *      患家診療時間加算（在宅患者訪問診療料）に関する回数の再編集
             PERFORM  VARYING  IDX19  FROM  1  BY  1
                       UNTIL   IDX19   >    9
               PERFORM  VARYING  IDX20  FROM  1  BY  1
                         UNTIL   IDX20   >   31
                 IF  TBL-ZAITAKU-KAISU(5 IDX19 IDX20) NOT = ZERO
                 AND TBL-ZAITAKU-KAISU(6 IDX19 IDX20) NOT = ZERO
                   COMPUTE  RECE100-KAISU(13) =
                            RECE100-KAISU(13) -
                            TBL-ZAITAKU-KAISU(6 IDX19 IDX20)
                 END-IF
               END-PERFORM
             END-PERFORM
      *
           END-IF
      *
           .
       2108-ZAITAKU-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００４）
      *****************************************************************
       2104-RECE120-HENSYU-SEC                SECTION.
      *
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    RECE100-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    RECE100-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    RECE100-PTID        TO  ACCT-PTID
           MOVE    RECE100-SRYKA(IDX)  TO  ACCT-SRYKA
           MOVE    RECE100-SRYYM       TO  ACCT-SRYYM
           MOVE    RECE100-HKNCOMBI    TO  ACCT-HKNCOMBI
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO   MCP-TABLE
               MOVE    "key24"             TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                   MOVE   ZERO           TO  FLG-RINYAK
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                    "1" OR "2" OR "3" ) AND
                        (ACCT-JIHOKBN       =    SPACE))
                   OR  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (WRK-PARA-PRTKBN    =
                                           "4" OR "5" ))
                               MOVE  ZERO  TO   FLG-SHOSAISIN           
                               MOVE  ZERO  TO   FLG-SHOSAISIN-GEN
                               MOVE  ZERO  TO   WRK-SRYSURYO
                               MOVE  ZERO  TO   WRK-SRYSURYO2
                               MOVE  ZERO  TO   WRK-SRYSURYO3
                               MOVE  ZERO  TO   WRK-TANICD
                               MOVE  ZERO  TO   WRK-TANIMOJISU
                               MOVE  SPACE TO   WRK-TANINAME
                               MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                FLG-HOUTEN-COM
                                                FLG-SYOBYO-COM
                                                FLG-RYOYO-COM
                                                FLG-SYODOU-COM
                                                FLG-SAIDOU-COM
                                                FLG-NIKOCHIN-COM
                                                FLG-ZENSOKU-COM
                                                FLG-OSIN-COM
                                                FLG-ZAIHOU-COM
                                                FLG-TAHOKEN
                                                FLG-SYOUNI-COM
                                                FLG-KENSA-COM
                                                FLG-ZAIRIHA-COM
                                                FLG-ZAIKYO-COM
                                                FLG-BOKOHO-COM
                                                FLG-IZONSYO-COM
                                                FLG-NINRYO-COM
                                                FLG-NYUSEN-COM
                                                FLG-KIATU-COM
                                                FLG-GAIJIR-COM
      *
                               MOVE  ZERO  TO   WRK-FILMTEN
                               MOVE  ZERO  TO   FLG-NEBU
      *
                               MOVE  ZERO  TO
                                           WRK-KZI058-TEN
                                           WRK-KZI058-TANICD
                                           WRK-KZI058-TANIMOJISU
                               MOVE  SPACE TO
                                           WRK-KZI058-TANINAME
      *
                               MOVE  ZERO  TO   WRK-RYOYOTEATE
      *
                               MOVE  ZERO  TO   FLG-WRITE05
                               MOVE  ZERO  TO   FLG-RIHA-OKIKAE
                               MOVE  ZERO  TO   FLG-HENSYU-NG
                               MOVE  ZERO  TO   FLG-ZAITAKU-CHECK
                               MOVE  ZERO  TO   FLG-SIPPU-COM
                               MOVE  ZERO  TO   FLG-SIPPU-ARI
                               MOVE  ZERO  TO   FLG-YKZ-PRTKBN
                               MOVE  ZERO  TO   FLG-KIZ-PRTKBN
                               PERFORM 21043-RECE120-TENSOU-SEC
                               PERFORM 21044-RECE130-HENSYU-SEC
                               IF    FLG-WRITE05  =   1
                                 PERFORM RECE120-WRITE-SEC
                               END-IF
                   ELSE
                     IF    ACCT-ZAIKAISU  NOT =    ZERO
                     AND   ACCT-JIHOKBN       =    SPACE
      **>            IF  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                 (WRK-PARA-PRTKBN    =
      **>                             "1" OR "2" OR "3" ) AND
      **>                 (ACCT-JIHOKBN       =    SPACE))
      **>            OR  ((ACCT-ZAIKAISU  NOT =    ZERO ) AND
      **>                 (WRK-PARA-PRTKBN    =
      **>                                    "4" OR "5" ))
                               IF ( ACCT-SRYKBN  NOT =
                                    "24" AND "90" AND "92" AND
                                    "93" AND "97"                 )
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ZERO  TO   FLG-SHOSAISIN    
                                      MOVE  ZERO  TO   FLG-SHOSAISIN-GEN
                                      MOVE  ZERO  TO   WRK-SRYSURYO
                                      MOVE  ZERO  TO   WRK-SRYSURYO2
                                      MOVE  ZERO  TO   WRK-SRYSURYO3
                                      MOVE  ZERO  TO   WRK-TANICD
                                      MOVE  ZERO  TO   WRK-TANIMOJISU
                                      MOVE  SPACE TO   WRK-TANINAME
                                      MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
                                                       FLG-SYOBYO-COM
                                                       FLG-RYOYO-COM
                                                       FLG-SYODOU-COM
                                                       FLG-SAIDOU-COM
                                                       FLG-NIKOCHIN-COM
                                                       FLG-ZENSOKU-COM
                                                       FLG-OSIN-COM
                                                       FLG-ZAIHOU-COM
                                                       FLG-TAHOKEN
                                                       FLG-SYOUNI-COM
                                                       FLG-KENSA-COM
                                                       FLG-ZAIRIHA-COM
                                                       FLG-ZAIKYO-COM
                                                       FLG-BOKOHO-COM
                                                       FLG-IZONSYO-COM
                                                       FLG-NINRYO-COM
                                                       FLG-NYUSEN-COM
                                                       FLG-KIATU-COM
                                                       FLG-GAIJIR-COM
      *
                                      MOVE  ZERO  TO   WRK-FILMTEN
                                      MOVE  ZERO  TO   FLG-NEBU
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO   WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO   FLG-WRITE05
                                      MOVE  ZERO  TO   FLG-RIHA-OKIKAE
                                      MOVE  ZERO  TO   FLG-HENSYU-NG
                                      MOVE  ZERO  TO   FLG-ZAITAKU-CHECK
                                      MOVE  ZERO  TO   FLG-SIPPU-COM
                                      MOVE  ZERO  TO   FLG-SIPPU-ARI
                                      MOVE  ZERO  TO   FLG-YKZ-PRTKBN
                                      MOVE  ZERO  TO   FLG-KIZ-PRTKBN
                                      IF    ACCT-SRYKBN  =
                                            "60" OR "64" OR "80"
                                        PERFORM 21046-SINRYO-CHECK-SEC
                                      END-IF
                                      PERFORM   21043-RECE120-TENSOU-SEC
                                      PERFORM   21044-RECE130-HENSYU-SEC
                                      IF    FLG-WRITE05  =   1
                                        PERFORM RECE120-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"       TO   MCP-TABLE
                   MOVE    "key24"             TO   MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO   MCP-TABLE
           MOVE    "key24"             TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104-RECE120-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報ファイル項目転送処理
      *****************************************************************
       21043-RECE120-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE               TO  RECE120-REC
           INITIALIZE                      RECE120-REC
           MOVE    RECE100-KEY         TO  RECE120-KEY1
           MOVE    ACCT-SRYKBN         TO  RECE120-SRYKBN
           MOVE    ACCT-ZAINUM         TO  RECE120-ZAINUM
           MOVE    ACCT-SRYCDTOTAL     TO  WRK-SRYCDTOTAL9
           IF      WRK-SRYCDTOTALX(6:9)
                                       =   "112011010"    OR
                                           "112705110"    OR
                                           "112705310"
               ADD     1               TO  WRK-ZAINUM
               MOVE    WRK-ZAINUM      TO  RECE120-ZAINUM
           END-IF
           MOVE    RECE100-PTNUM       TO  RECE120-PTNUM
      *
           MOVE    ACCT-SRYKA           TO  RECE120-SRYKA
           MOVE    ACCT-HKNCOMBI        TO  RECE120-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO  RECE120-JIHOKBN
           MOVE    ACCT-ZAITEN          TO  RECE120-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO  RECE120-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO  RECE120-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO  RECE120-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO  RECE120-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO  RECE120-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO  RECE120-SYUTEN2
           MOVE    ACCT-YKZTEN          TO  RECE120-YKZTEN
           MOVE    ACCT-KIZAITEN        TO  RECE120-KIZAITEN
           IF      ACCT-SRYKBN    =  "70"
             COMPUTE  RECE120-SYUTEN1   =   RECE120-SYUTEN1
                                        -   RECE120-SYUTEN2
             COMPUTE  RECE120-KIZAITEN  =   RECE120-KIZAITEN
                                        +   RECE120-SYUTEN2
           END-IF
           MOVE    ACCT-DRCD            TO  RECE120-DRCD
           MOVE    ACCT-ZAIREQFLG       TO  RECE120-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO  RECE120-TEIGENRATE
           MOVE        ZERO             TO  WK-TAIHI-DAY
           MOVE        ZERO             TO  WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >      10
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE120-KAISU (IDX3 IDX2)
                   IF    (WK-TAIHI-FLG               =  ZERO)
                   AND   (RECE120-KAISU(1 IDX2)  NOT =  ZERO)
                       MOVE     IDX2    TO  WK-TAIHI-DAY
                       MOVE     1       TO  WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK      TO  RECE120-SAFLG
           END-IF
      *
           .
       21043-RECE120-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄情報ファイル項目転送処理
      *****************************************************************
       21044-RECE130-HENSYU-SEC               SECTION.
      *
           IF    (RECE100-SRYYM           >=  "201604")
           AND   (ACCT-SRYKBN              =  "23"    )
           AND   (ACCT-JIHOKBN             =   SPACE  )
             PERFORM 210444-SIPPU-CHECK-SEC
           END-IF
      *
           INITIALIZE     WRK-ORCSGETGE-OUT-AREA
           MOVE    ZERO                TO      FLG-IRYORENKEI
           IF    ((WRK-PARA-PRTKBN  =  "2" OR "3" OR "4" OR "5") AND
                  (SYS-200501-INSPECT-GENERAL-KBN  =   "1"     ) AND
                  (RECE100-SRYYM        >=   "201204"          ))
           OR    ( FLG-SIPPU-ARI         =     1                )
             PERFORM 210443-IRYORENKEI-HENSYU-SEC
           END-IF
           MOVE    ZERO                TO      IDX17
      *
           INITIALIZE                      SRYACT-REC
grpsys     MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                                     FLG-FORMAL-KISAI
                   MOVE   ZERO   TO  WRK-RENNUM
                                     WRK-RENNUM2
                   MOVE   ZERO   TO  FLG-WRITE05X
                   PERFORM   210441-RECE130-TENSOU-SEC
                   IF  ACCT-JIHOKBN  =  SPACE
                     PERFORM 210442-RECE100-HENSYU-SEC
                   END-IF
                   IF  FLG-WRITE05X  =  1
                      PERFORM  RECE130-WRITE-SEC
                      MOVE  1    TO  FLG-WRITE05
                   END-IF
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  RECE130-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  RECE130-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACT SELECT ERR" 
           END-IF
      *
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF     WRK-FILMTEN  NOT =  ZERO
                COMPUTE    RECE120-SYUTEN2  =  WRK-FILMTEN + 0.5
           END-IF
      *
           .
       21044-RECE130-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００５）
      *****************************************************************
       210441-RECE130-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO  RECE130-REC
           INITIALIZE                       RECE130-REC
           MOVE    RECE100-KEY          TO  RECE130-KEY1
           MOVE    SRY-ZAINUM           TO  RECE130-ZAINUM
           IF      RECE120-ZAINUM       >       88888880
               MOVE   RECE120-ZAINUM    TO  RECE130-ZAINUM
           END-IF
           MOVE    SRY-RENNUM           TO  RECE130-RENNUM
           MOVE    RECE100-PTNUM        TO  RECE130-PTNUM
      *
           MOVE    SRY-SRYKA            TO  RECE130-SRYKA
           MOVE    SRY-SRYKBN           TO  RECE130-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO  RECE130-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO  RECE130-JIHIMONEY
           MOVE    ZERO                 TO  FLG-OSINKBN
                                                IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
             ADD     1                  TO  IDX17
      *
             PERFORM 900-TENSU-TBL-SEC
      **     点数マスタ情報退避
             MOVE    TNS-CHUKSNTUBAN    TO  WRK-TNS-CHUKSNTUBAN(IDX2)
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
             MOVE    TNS-TEN            TO  WRK-TNS-TEN(IDX2)
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE120-MEISAISU  =  RECE120-MEISAISU
                                             -  1
             END-IF
      *
             IF       SRY-SRYCD(IDX2)  =  "099999909" OR "099999910" OR
                                          "099999921" OR "099999922" OR
                                          "099999923" OR "099999924" OR
                                          "099999925" OR "099999929" OR
                                          "099999931"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
      *
             IF       SRY-SRYCD(IDX2)(1:5)  =
                      "09960" OR "09970" OR "09980" OR "09983"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
             IF       SRY-SRYCD(IDX2)  =  "099999935" OR "099999936" OR
                                          "099999937" OR "099999938" OR
                                          "099999939" OR "099999940" OR
                                          "099999941" OR "099999942" OR
                                          "099999943"
               IF     ACCT-JIHOKBN     =   SPACE
                  EVALUATE  SRY-SRYCD(IDX2)
                    WHEN     "099999935"
                      IF  RECE100-RIHA2-SINDAI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    1     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999936"
                      IF  RECE100-RIHA2-NOUKE    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    2     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999937"
                      IF  RECE100-RIHA2-UNDOKI   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    3     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999938"
                      IF  RECE100-RIHA2-KOKYUKI  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    4     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999939"
                      IF  RECE100-RIHA2-SESSYOKU = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    5     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999940"
                      IF  RECE100-RIHA2-HAIYO    = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    6     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999941"
                      IF  RECE100-RIHA2-SINDAI9  = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    7     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999942"
                      IF  RECE100-RIHA2-KOKYUKI9 = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    8     TO    FLG-RIHA-OKIKAE
                      END-IF
                    WHEN     "099999943"
                      IF  RECE100-RIHA2-HAIYO9   = 1
                        MOVE    1     TO    FLG-HENSYU-NG
                      ELSE
                        MOVE    9     TO    FLG-RIHA-OKIKAE
                      END-IF
                  END-EVALUATE
               ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
               END-IF
             END-IF
             IF       SRY-SRYCD(IDX2)  =  "099140004" OR "099400001"
                                       OR "099400002" OR "099400003"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
             IF      (SRY-SRYCD(IDX2)  =  "099140011" OR "099140012")
             AND     (RECE100-SRYYM   >=  "201804"                  )
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
      *
             IF     ACCT-SRYKBN      =  "98"
               IF  (SRY-RENNUM       =   1         )
               AND (IDX2             =   1         )
               AND (SRY-SRYCD(IDX2)  =  "830000052")
                  MOVE    1     TO    FLG-SIPPU-COM
               ELSE
                 IF  FLG-SIPPU-COM   =   1
                  CONTINUE
                 ELSE
                  MOVE    1     TO    FLG-HENSYU-NG
                 END-IF
               END-IF
               IF  (SRY-RENNUM       =   1         )
               AND (IDX2             =   1         )
               AND (SRY-SRYCD(IDX2)  =  "830000052"
                                    OR  "099200201")
                 PERFORM  VARYING  IDX33  FROM  1  BY  1
                           UNTIL   IDX33   >   10
                   PERFORM  VARYING  IDX34  FROM  1  BY  1
                             UNTIL   IDX34   >   31
                     IF   ACCT-DAY(IDX33 IDX34)  NOT =  ZERO
                       MOVE    1    TO   RECE100-SIPPUCHKFLG
                       MOVE    1    TO   RECE100-SIPPUCHK(IDX33 IDX34)
                     END-IF
                   END-PERFORM
                 END-PERFORM
               END-IF
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                     (FLG-SIPPU-ARI         =  ZERO )       )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                     (FLG-SIPPU-ARI         =   1   ) AND
                     (SYS-200501-COMPRESS-KBN
                                        NOT =  "1"  )       )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
             OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
             OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                            OR "0086"       )
             OR     ( FLG-RIHA-OKIKAE   NOT =  ZERO         )
             OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
             OR     ( SRY-SRYCD(IDX2)       =
                      "099209902" OR "099209903" OR
                      "099209904" OR "099209905" OR
                      "099209906" OR "099209907" OR
                      "099209908" OR
                      "099999903" OR "099999904" OR
                      "099999905" OR "099999906" OR
                      "099999907" OR "099999908" OR
                      "099200101" OR "099999926" OR
                      "099999927" OR "099999928" OR
                      "099999934" OR
                      "099208101" OR
                      "840000136" OR "840000140" OR
                      "099114001" OR "099140001" OR
                      "099140003" OR "099409905" OR
                      "099409906" OR "099409907" OR
                      "099409908" OR "099409909" OR
                      "099200301"                           )
      *
                  PERFORM   2104413-MEISAISU-HENSYU-SEC
      *
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                      (FLG-SIPPU-ARI         =  ZERO )       )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "001") AND
                      (FLG-SIPPU-ARI         =   1   ) AND
                      (SYS-200501-COMPRESS-KBN
                                         NOT =  "1"  )       )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ( FLG-RIHA-OKIKAE   NOT =  ZERO         )
              OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
              OR     ( SRY-SRYCD(IDX2)       =
                       "099209902" OR "099209903" OR
                       "099209904" OR "099209905" OR
                       "099209906" OR "099209907" OR
                       "099209908" OR
                       "099999903" OR "099999904" OR
                       "099999905" OR "099999906" OR
                       "099999907" OR "099999908" OR
                       "099200101" OR "099999926" OR
                       "099999927" OR "099999928" OR
                       "099999934" OR
                       "099208101" OR
                       "840000136" OR "840000140" OR
                       "099114001" OR "099140001" OR
                       "099140003" OR "099409905" OR
                       "099409906" OR "099409907" OR
                       "099409908" OR "099409909" OR
                       "099200301"                           )
      *
               IF   FLG-RIHA-OKIKAE  NOT =  ZERO
                     PERFORM   2104415-RIHAYMD-HENSYU-SEC
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-KIZAITEN =  ZERO                        )
                 IF    SRY-SRYCD(IDX2)(1:3)  =  "059"
                 AND   TNS-TEN               =  ZERO
                   MOVE    1    TO    FLG-KIZ-PRTKBN
                 END-IF
               END-IF
      *
              ELSE
      *
               ADD     1                  TO    IDX4
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE130-SRYCD(IDX4)
      *
               IF     SRY-SRYCD(IDX2)      =    "160177770"
                  MOVE   "1"              TO    RECE100-KENTAIKASANFLG
                  MOVE   99               TO    RECE120-KNSJISGRPKBN
               END-IF
               IF     RECE120-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE120-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =     "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =      2           )
                   MOVE    TNS-KNSJISKBN        TO  RECE120-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE120-ZAITEN    =     ZERO                 )
               AND   ( RECE120-SAFLG     =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE120-SAFLG
                  END-IF
               END-IF
      *
               IF      ACCT-JIHOKBN  NOT =     SPACE
                  IF     RECE120-SAFLG   NOT =  SPACE
                        MOVE    "3"       TO    RECE120-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   RECE120-TOYAKUFLG = SPACE
                          MOVE  "4"       TO    RECE120-TOYAKUFLG
                        END-IF
                    ELSE
                        MOVE    "3"       TO    RECE120-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE120-ZAITEN    =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
               AND   ( ACCT-JIHOKBN      =     SPACE        )
                IF   ACCT-SRYCDTOTAL(6:9) = "630010002"
                                         OR "630010005"
                                         OR "630010006"
                                         OR "630010007"
                  CONTINUE
                ELSE
                  IF     RECE120-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE120-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232" OR
                                               "295"
                          IF   RECE120-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE120-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE120-TOYAKUFLG
                        IF   (RECE130-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                   NOT =  SPACE)
                          IF  (WRK-PARA-PRTKBN  =
                                           "2" OR "3" OR "4" OR "5")
                          AND (SYS-200501-INSPECT-GENERAL-KBN = "1")
                             MOVE  "1"  TO  RECE130-INGAIFLG-RECE(IDX4)
                          END-IF
                          IF   FLG-SIPPU-ARI    =  1
                             MOVE  "1"  TO  RECE130-INGAIFLG-SIPPU(IDX4)
                          END-IF
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO  RECE130-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO  RECE130-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO  RECE130-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE130-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE130-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
                END-IF
               END-IF
      *
               IF     FLG-SIPPU-ARI  =  1
      **********  １：院外分　２：院内分
                  IF   RECE120-ZAITEN  =   ZERO
                      MOVE  "1"         TO    RECE120-SIPPUFLG
                  ELSE
                      MOVE  "2"         TO    RECE120-SIPPUFLG
                  END-IF
                  IF  (SRY-SRYCD(IDX2)(1:1)  =  "6")
                  AND (TNS-MEIUSESKB         =   1 )
                      MOVE  "1"         TO    RECE130-SIPPUFLG(IDX4)
                  END-IF
                  IF   SRY-SRYCD(IDX2)(1:3)  =  "001"
                      MOVE  "810000001" TO    RECE130-SRYCD(IDX4)
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14"  )
               AND   ( RECE120-ZAITEN    =     ZERO  )
               AND   ( ACCT-ZAIKBN   NOT =     1     )
               AND   ( ACCT-JIHOKBN      =     SPACE )
                  IF     RECE120-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE120-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE120-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE120-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE120-TOYAKUFLG
                        IF   (RECE130-SRYCD(IDX4)(1:1) = "6"   )
                        AND  (WRK-ORCSGETGE-GENERICNAME(IDX17)
                                                   NOT =  SPACE)
                          MOVE    "1"  TO  RECE130-INGAIFLG-RECE(IDX4)
                          MOVE  WRK-ORCSGETGE-YAKKAKJNCD(IDX17)
                                          TO  RECE130-YAKKAKJNCD(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERIC-KBN(IDX17)
                                          TO  RECE130-GENERIC-KBN(IDX4)
                          MOVE  WRK-ORCSGETGE-GENERICNAME(IDX17)
                                          TO  RECE130-GENERICNAME(IDX4)
                          PERFORM  VARYING  IDX15  FROM  200  BY  -1
                                    UNTIL   IDX15   <     1
                            IF  RECE130-GENERICNAME(IDX4)(IDX15:1)
                                                        NOT =   SPACE
                               COMPUTE RECE130-GENERICYUKOKETA(IDX4) =
                                       IDX15  /  2
                               MOVE    1   TO  IDX15
                            END-IF
                          END-PERFORM
                        END-IF
                    END-IF
                  END-IF
               END-IF
      *
               IF      SRY-SRYKBN   =   "99"
                  IF      SRY-SRYCD(IDX2)     =   "099140011"
                      MOVE     "1"        TO    RECE120-HOMONCOMFLG
                      MOVE  "810000001"   TO    RECE130-SRYCD(IDX4)
                  END-IF
                  IF      SRY-SRYCD(IDX2)     =   "099140012"
                      MOVE     "2"        TO    RECE120-HOMONCOMFLG
                      MOVE  "810000001"   TO    RECE130-SRYCD(IDX4)
                  END-IF
                  IF      RECE120-HOMONCOMFLG =   "2"
                      MOVE     "1"        TO    RECE100-COMCNTFLG
                      MOVE     "1"        TO    RECE130-RECEPTFLG(IDX4)
                  END-IF
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-YKZTEN =  ZERO                          )
                 IF  ( SYS-200501-15-YKZ-PRTKBN  =  "0" )
                 OR  ((SYS-200501-15-YKZ-PRTKBN  =  "1") AND
                      (RECE100-SRYYM  <  "201204"      ))
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE130-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-YKZ-PRTKBN
                         PERFORM   2104413-MEISAISU-HENSYU-SEC
                   ELSE
                     IF  FLG-YKZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE130-RECEPTFLG(IDX4)
                         PERFORM   2104413-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-YKZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
                 ELSE
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "6"
                         MOVE   "1"    TO  RECE120-YKZKISAIFLG
                   END-IF
                 END-IF
               END-IF
      *
               IF   (ACCT-SRYKBN = "40" OR "50" OR "54" OR "60" OR
                                   "64" OR "70" OR "80"           )
               AND  (ACCT-KIZAITEN =  ZERO                        )
                   IF    SRY-SRYCD(IDX2)(1:1)  =  "7"
                   OR    SRY-SRYCD(IDX2)(1:3)  =  "058"
                     IF  ((ACCT-SRYKBN     = "70"                 ) AND
                          (TNS-DATAKBN     = "3"                  ))
                     OR  ((ACCT-SRYKBN     = "70"                 ) AND
                          (SRY-SRYCD(IDX2) = "799990070"          ))
                         MOVE   ZERO   TO  FLG-KIZ-PRTKBN
                     ELSE
                         MOVE   "1"    TO  RECE130-RECEPTFLG(IDX4)
                         MOVE    1     TO  FLG-KIZ-PRTKBN
                         PERFORM   2104413-MEISAISU-HENSYU-SEC
                     END-IF
                   ELSE
                     IF  FLG-KIZ-PRTKBN  =  1
                       IF (( ACCT-ZAITEN           =  ZERO  ) AND
                           ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                            (SRY-SRYCD(IDX2)(1:3)  =  "008")))
                       OR (  SRY-MEISKYFLG(IDX2)   =  "2"    )
                         MOVE   "1"    TO  RECE130-RECEPTFLG(IDX4)
                         PERFORM   2104413-MEISAISU-HENSYU-SEC
                       ELSE
                         MOVE   ZERO   TO  FLG-KIZ-PRTKBN
                       END-IF
                     END-IF
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
               AND    FLG-YKZ-PRTKBN   =   ZERO
               AND    FLG-KIZ-PRTKBN   =   ZERO
      *          IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)   TO  WRK-SRYCD
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >    9
                     MOVE   SRY-SRYCD(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  RECE120-RUIKEITOTAL  =
                                RECE120-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       COMPUTE  WRK-FUKA-KINGAKU    =
                                WRK-FUKA-KINGAKU    +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE120-RUIKEITOTAL  =
                            RECE120-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >   10
                     MOVE   SRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE120-RUIKEITOTAL  =
                            RECE120-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               IF      SRY-SRYKBN = "40" OR "50" OR "54" OR "60" OR "64"
                  IF   SRY-SRYCD(IDX2)(1:1)  =  "7"
                  AND  TNS-DATAKBN           =  "3"
                         COMPUTE WRK-FILMTEN2   =
                                (TNS-TEN  * SRY-SRYSURYO(IDX2) / 10)
                         IF   TNS-JGNTEN  NOT =  ZERO
                         AND  TNS-JGNTEN   <  WRK-FILMTEN2
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      TNS-JGNTEN
                         ELSE
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      WRK-FILMTEN2
                         END-IF
                  END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE130-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE130-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE130-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE130-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE130-INPUTNUM(IDX4)
      *
      **診療実日数のカウントを行える剤であるかの判定
               IF      ACCT-JIHOKBN       =    SPACE
                 IF  ((ACCT-ZAITEN    NOT =    ZERO             ) AND
                      (ACCT-ZAIKAISU  NOT =    ZERO             ) AND
                      (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                             "13"  OR  "14"     ))
                 OR  ((ACCT-ZAITEN        =    ZERO             ) AND
                      (ACCT-ZAIKAISU  NOT =    ZERO             ) AND
                      (ACCT-SRYKBN        =  "11"  OR  "12"  OR
                                             "99"               ))
      *
                       IF ( (TNS-JITUDAY    =     2          ) AND      
                            (TNS-DAYCNT     =     1   OR   2 )    )     
                       OR (  SRY-SRYCD(IDX2)  =  "112700850"            
                                              OR "112003350"            
                                              OR "099409901"            
                                              OR "099999902"            
                                              OR "099110001"            
                                              OR "099120001"            
      *                              同日再診
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112016550"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"
      *
                                              OR "111011810"
                                              OR "111012610"
                                              OR "113013910"
                                              OR "113014010"
                                              OR "113014110"
                                              OR "112015810"
                                              OR "112015950"
                                              OR "112016210"
                                              OR "112016410"
                                              OR "113015810"
                                              OR "112016850"
                                              OR "112016950"
                                              OR "112017010"
                                              OR "112017150"
                                              OR "112017450"
      *
                                              OR "113018410"
                                              OR "111012810"
                                              OR "112017610"
                                              OR "113024810"
                                              OR "113025710"
                                              OR "112023210"
                                              OR "112023450"
                                              OR "112023750"
                                              OR "112023550"
                                              OR "112023850"
                                              OR "113031310"
                                              OR "113031410"      )
      *
                             MOVE    1      TO    FLG-SHOSAISIN         
      *
                       END-IF                                           
      *
                       IF    SRY-SRYCD(IDX2)  =  "099409903"
                             MOVE    1      TO    FLG-SHOSAISIN-GEN
                       END-IF
      *
                 END-IF
               END-IF
      *
      *        療養担当手当
               IF      SRY-SRYCD(IDX2)  =  "199000610"
                  COMPUTE  WRK-RYOYOTEATE    =  WRK-RYOYOTEATE
                                             +  TNS-TEN
                  COMPUTE  RECE120-SYUTEN1   =  RECE120-SYUTEN1
                                             -  TNS-TEN
                  COMPUTE  RECE120-KIZAITEN  =  RECE120-KIZAITEN
                                             +  TNS-TEN
               END-IF
      *
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-GAITENTTLKBN   =     130
                       MOVE   "13"        TO    RECE120-SRYKBN
                                                RECE130-SRYKBN
                   END-IF
               END-IF
      *
               IF      FLG-SIPPU-COM      =     1
                       MOVE   "1"         TO    RECE120-SIPPUCOMFLG
                       MOVE   "80"        TO    RECE120-SRYKBN
                                                RECE130-SRYKBN
                       MOVE   "800"       TO    RECE130-SRYSYUKBN
               END-IF
      *
               IF      ACCT-JIHOKBN       =     SPACE
                  PERFORM  2104411-RECE130-COMCNT-HEN-SEC
               ELSE
                  IF   RECE120-SRYKBN     =     "50"
                     MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
                  END-IF
                  IF   RECE120-SRYKBN     =     "54"
                     MOVE   "1"   TO    RECE120-FLG-MASUI-COMMENT
                  END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE130-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE130-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE130-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE130-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE130-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE130-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE130-TANINAME(IDX4)   
                    IF    RECE130-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE130-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE130-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE130-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE130-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE130-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE130-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
      *********IF      TNS-TENSIKIBETU    =     1   OR   3
      *********    MOVE  TNS-TENSIKIBETU  TO   RECE130-TENSIKIBETU(IDX4)
      *********    MOVE  TNS-TEN          TO    RECE130-TEN(IDX4)
      *********END-IF
               MOVE  TNS-TENSIKIBETU  TO   RECE130-TENSIKIBETU(IDX4)
               MOVE  TNS-TEN          TO   RECE130-TEN(IDX4)
               IF      RECE100-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE130-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE130-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE130-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE130-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE130-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE130-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE130-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE130-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE130-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE130-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE130-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE130-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE130-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE130-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE130-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1  
                                      TO RECE130-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE130-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE130-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE130-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE130-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE130-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE130-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE130-SANSOKBN       (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE100-SRYYM  <=  "201003"   )
                      MOVE   202      TO  RECE130-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE130-CDKBN-KOUBAN(IDX4)
               END-IF
               IF     TNS-SRYCD       =  "170016010"  OR
                                         "170020270"  OR  "170024470" OR
                                         "170024910"  OR  "170025010" OR
                                         "170025210"  OR  "170025310" OR
                                         "170025410"  OR  "170025510" OR
                                         "170025610"  OR  "170025710" OR
                                         "170025810"  OR  "170025910" OR
                                         "170026010"  OR  "170026110" OR
                                         "170026210"  OR  "170026310" OR
                                         "170026810"  OR
                                         "170000210"  OR  "170016910" OR
                                         "170017010"  OR  "170026710" OR
                                         "170028810"  OR
                                         "170035710"  OR  "170035810" OR
                                         "170035910"  OR  "170036010"
                      MOVE   203      TO  RECE130-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE130-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               MOVE   TNS-SRYKBN      TO  RECE130-TNSSRYKBN(IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE100-SRYYM  >=  "201004"   )
                      MOVE   "1"      TO  RECE120-GAZOGEN
               END-IF
      *
               IF   (((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008")) AND
                     ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  ))
               OR   (( FLG-SIPPU-ARI                =    1    ) AND
                     ( SRY-SRYCD    (IDX2)(1:3)     =   "001" ) AND
                     ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  ))
grpsys             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT    TO  RECE130-NAME (IDX4)
                                                    RECE130-INPUTCOMENT
                                                                (IDX4)
      *
                   IF       SRY-SRYCD (IDX2)(1:1) = "8"
                       IF  (SRY-SRYCD (IDX2)(1:2) = "84" OR "85")
                       OR  (SRY-SRYCD (IDX2)(1:4) = "8311"      )
                         IF  PTCOM-INPUTCHI1  NOT =  SPACE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           END-IF
                         ELSE
                               MOVE    SPACE       TO
                                           RECE130-INPUTCOMENT   (IDX4)
                         END-IF
                       ELSE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          IF    RECE120-COMCDFLG   NOT =   "2"
      ***             MOVE   "1"           TO    RECE120-COMCDFLG
      ***          END-IF
               ELSE
                   MOVE    TNS-NAME        TO    RECE130-NAME    (IDX4)
                                                 RECE130-INPUTCOMENT
                                                                 (IDX4)
                   MOVE    TNS-YUKOKETA    TO    RECE130-YUKOKETA(IDX4)
               END-IF
      *
               IF  SRY-SRYCD(IDX2)        =     "099309901"
                  IF  RECE130-SRYSURYO(IDX4)  NOT = ZERO
                    PERFORM  2104414-RECE130-SURYO-HEN-SEC
      *
                    MOVE    SPACE      TO  RECE130-NAME(IDX4)
                    MOVE    SPACE      TO  RECE130-INPUTCOMENT(IDX4)
      ***           STRING  TNS-NAME          DELIMITED  BY  " "
      ***                   "　"              DELIMITED  BY  SIZE
      ***                   WRK-SRYSURYOW     DELIMITED  BY  " "
      ***                   RECE130-TANINAME(IDX4)
      ***                                     DELIMITED  BY  " "
      ***                   INTO              RECE130-NAME(IDX4)
      ***           END-STRING
                    STRING  "（使用量　"      DELIMITED  BY  SIZE
                            WRK-SRYSURYOW     DELIMITED  BY  " "
                            RECE130-TANINAME(IDX4)
                                              DELIMITED  BY  " "
                            "　残量廃棄）"    DELIMITED  BY  SIZE
                            INTO              RECE130-NAME(IDX4)
                    END-STRING
                    MOVE  RECE130-NAME(IDX4)
                                       TO  RECE130-INPUTCOMENT(IDX4)
      *
      ***           MOVE  "810000001"  TO  RECE130-SRYCD(IDX4)
                    MOVE  ZERO         TO  RECE130-SRYSURYO(IDX4)
                    MOVE  ZERO         TO  RECE130-TANICD(IDX4)
                    MOVE  ZERO         TO  RECE130-TANIMOJISU(IDX4)
                    MOVE  SPACE        TO  RECE130-TANINAME(IDX4)
                    MOVE  ZERO         TO  RECE130-YUKOKETA(IDX4)
                  END-IF
               END-IF
      *
               IF          SRY-SRYKBN       =  "21" OR "22" OR "23"
                    IF    SRY-SRYCD(IDX2)  =  "630010002"
                                           OR "630010005"
                                           OR "630010006"
                                           OR "630010007"
                                           OR "820000047"
                                           OR "820000166"
                        MOVE     "2"       TO    RECE120-COMCDFLG
                    END-IF
               END-IF
               IF   ( SRY-SRYCD(IDX2)(1:1)  =   "8"   )
               OR   ( SRY-SRYCD(IDX2)(1:3)  =   "008" )
               OR   ((FLG-SIPPU-ARI         =    1   ) AND
                     (SRY-SRYCD(IDX2)(1:3)  =   "001"))
                    IF    RECE120-COMCDFLG   NOT =   "2"
                        MOVE     "1"       TO    RECE120-COMCDFLG
                    END-IF
               END-IF
      *
      ***      IF    RECE120-COMCDFLG       =   "2"
      ***        IF    SRY-SRYCD(IDX2)  =  "630010002"
      ***                               OR "630010005"
      ***                               OR "630010006"
      ***                               OR "630010007"
      ***                               OR "820000047"
      ***                               OR "820000166"
      ***          CONTINUE
      ***        ELSE
      ***          ADD   999               TO  WRK-SURYOU-COUNT
      ***          ADD   WRK-SURYOU-COUNT  TO  RECE120-SURYOUTOTAL
      ***        END-IF
      ***      END-IF
      *
               IF   (SRY-SRYKBN       =  "31" OR "32")
               AND  (ACCT-JIHOKBN     =  SPACE       )
                 IF        SRY-SRYCD(IDX2)  =    "130000510"
      ***                                  OR    "130000110"
      ***                                  OR    "130000310"
                                           OR    "130003510"
      ***                                  OR    "130003670"
                   MOVE    SPACE           TO    RECE130-NAME(IDX4)
                 END-IF
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   300 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE130-NAME(IDX4)(IDV:2)   =   "訪"
                         MOVE   SPACE  TO    RECE120-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV            >   300)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE130-NAME(IDX4)(IDV:2)   =  "日"
                        MOVE   SPACE  TO  RECE120-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  ( SRY-SRYKBN       =  "14"       )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-ZAIRIHA-COM  =   ZERO      )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV              >   300)
                               OR    (FLG-ZAIRIHA-COM  =    1 )
                      IF  RECE130-NAME(IDX4)(IDV:2)   =  "日"
                        MOVE   SPACE  TO  RECE120-FLG-ZAIRIHA-COMMENT
                        MOVE     1    TO  FLG-ZAIRIHA-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN = "12" OR "13" OR
                                  "14" OR "80"      )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
                 IF (RECE130-NAME(IDX4)(1:20) = "管理料等算定開始年月")
                 OR (RECE130-NAME(IDX4)(1:18) = "管理料算定開始年月"  )
                   MOVE   "1"   TO   RECE120-FLG-ONLINE3-COMMENT
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE130-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE130-NAME(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE130-TEN(IDX4)
                 MOVE     ZERO            TO  RECE130-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE130-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE130-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE130-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1      TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE130-RENNUM  -  1
                    ELSE
                      MOVE  "1"      TO  RECE130-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"        TO  RECE130-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                     TO  RECE130-TEN(IDX4)
                    END-IF
                    IF   (WRK-KZI058-TANICD  NOT = ZERO)
                    AND  (RECE130-TANICD(IDX4)   = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                     TO  RECE130-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                     TO  RECE130-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                     TO  RECE130-TANINAME(IDX4)
                    END-IF
                 END-IF
               END-IF
      *
               IF  (SRY-SRYKBN         =  "99"       )
               AND (SRY-RENNUM         =   1         )
               AND (IDX2               =   1         )
               AND (SRY-SRYCD(IDX2)    =  "099999902")
                    MOVE      1         TO  FLG-TAHOKEN
               END-IF
               IF  (FLG-TAHOKEN           =   1              )
               AND (SRY-RENNUM            =   1              )
               AND (IDX2                  =   2              )
               AND (SRY-SRYCD(IDX2)(1:4)  =  "0082"          )
               AND (RECE130-NAME(IDX4)(1:14)
                                          =  "保険名称変更：")
                  MOVE     "1"          TO  RECE130-RECEPTFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   300   BY  -1
                             UNTIL    IDW     <    15
                       IF  RECE130-NAME(IDX4)(IDW:1)  NOT =  SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE130-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  RECE130-NAME(IDX4)(15:IDW4)
                                TO RECE130-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "にて診察料算定済"
                                TO RECE130-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE130-YUKOKETA(IDX4 - 1) =
                              RECE130-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE130-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   300   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE130-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE130-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        特定器材（画像診断のフィルム以外）のとき単価を編集
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
                   IF   (  SRY-SRYCD (IDX2)         =
                                    "799990070"            OR
                                    "770020070"            OR
                                    "770030070"               )
      ***          OR   (  TNS-DATAKBN              =   "3"   )
                       CONTINUE
                   ELSE
                     IF      RECE130-TEN(IDX4)  =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2104412-RECE130-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE130-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE130-NAME(IDX4)(IDW:)   
                       COMPUTE RECE130-YUKOKETA(IDX4)
                                              =   RECE130-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *----------------------------------------------------------------
      *        在宅部分のレセプト編集用に往診（特別往診）・夜間・
      *        深夜・緊急の剤判定を追加
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
      *
                 IF  SRY-SRYCD(IDX2) =  "114001110"
                                    OR  "114012910"
                                    OR  "114018010"
                                    OR  "114030310"
                                    OR  "114042110"
                                    OR  "114042210"
                                    OR  "114042810"
                                    OR  "114046310"
                                    OR  "114001470"
                    MOVE        9         TO    FLG-OSINKBN
                 END-IF
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *往診（特別往診）
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
      ***          WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3  AND  9
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *夜間
                   WHEN    "114000470"
                   WHEN    "114001970"
                   WHEN    "114011670"
                   WHEN    "114011970"
                   WHEN    "114017570"
                   WHEN    "114017870"
      *
                   WHEN    "114029270"
                   WHEN    "114029370"
                   WHEN    "114029470"
                   WHEN    "114029570"
                   WHEN    "114029970"
                   WHEN    "114030070"
                   WHEN    "114030170"
                   WHEN    "114030270"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     2         TO    FLG-OSINKBN
                   END-IF
      *深夜・緊急
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                   WHEN    "114011570"
                   WHEN    "114011770"
                   WHEN    "114011870"
                   WHEN    "114012070"
                   WHEN    "114017470"
                   WHEN    "114017770"
                   WHEN    "114017670"
                   WHEN    "114017970"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
      *
                 IF    RECE100-SRYYM   >=  "201404"
                  EVALUATE    SRY-SRYCD(IDX2)
      *            往診
                   WHEN    "114000110"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(1 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            患家診療時間加算（往診）
                   WHEN    "114000970"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(2 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
      *            特別往診
                   WHEN    "114001610"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(3 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            患家診療時間加算（特別往診）
                   WHEN    "114002470"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(4 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
      *            在宅患者訪問診療料
                   WHEN    "114001110"
                   WHEN    "114012910"
                   WHEN    "114018010"
                   WHEN    "114030310"
                   WHEN    "114042110"
                   WHEN    "114042210"
                   WHEN    "114042810"
                   WHEN    "114046310"
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(5 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                     MOVE  1  TO  FLG-ZAITAKU-CHECK
      *            患家診療時間加算（在宅患者訪問診療料）
                   WHEN    "114001470"
                    IF  FLG-ZAITAKU-CHECK = ZERO
                     PERFORM  VARYING  IDX19  FROM  2  BY  1
                               UNTIL   IDX19   >   10
                       COMPUTE  IDX21 = IDX19 - 1
                       PERFORM  VARYING  IDX20  FROM  1  BY  1
                                 UNTIL   IDX20   >   31
                        IF  ACCT-DAY(IDX19 IDX20) NOT = ZERO
                          ADD    ACCT-DAY(IDX19 IDX20)
                                   TO  TBL-ZAITAKU-KAISU(6 IDX21 IDX20)
                        END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF
                  END-EVALUATE
                 END-IF
      *
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE120-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE130-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE130-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE130-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE130-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE120-COMCDFLG
                     MOVE  "810000001"  TO  RECE130-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE130-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE130-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE130-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE130-TANINAME(IDX4)
               END-IF
      *
               IF   (RECE100-SRYYM           >= "201810")
               AND  (RECE130-SRYCD(IDX4)(1:4) = "8201"  )
                 MOVE   "1"         TO  RECE120-SENTAKUCOMFLG
               END-IF
      *
              END-IF
             END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE05X
           END-IF
      *
           .
       210441-RECE130-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントフラグ編集処理
      *****************************************************************
       2104411-RECE130-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE120-SRYKBN      =     "11" OR "12" OR "13"
              IF   SRY-SRYCD(IDX2)    =  "111012970" OR "111013070"
                                      OR "111013170" OR "111013270"
                                      OR "111013370" OR "111013470"
                                      OR "111013570" OR "111013670"
                                      OR "112021370" OR "112021470"
                                      OR "112021570" OR "112021670"
                                      OR "112022070" OR "112022170"
                                      OR "112022270" OR "112022370"
                                      OR "112022870" OR "112022470"
                                      OR "112022570" OR "112022670"
                                      OR "112022770" OR "112022970"
                                      OR "112023070" OR "112023170"
                                      OR "113024970" OR "113025070"
                                      OR "113025170" OR "113025270"
                                      OR "113025370" OR "113025470"
                                      OR "113025570" OR "113025870"
                                      OR "113025970" OR "113026070"
                                      OR "113026170" OR "113026270"
                                      OR "113026370" OR "113026470"
                 EVALUATE   RECE100-NINPUFLG
                    WHEN        0
                            MOVE    1    TO    RECE100-NINPUFLG
                    WHEN        2
                            MOVE    3    TO    RECE100-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
           IF      RECE120-SRYKBN      =     "99"
              IF   SRY-SRYCD(IDX2)    =     "820100348"
                 EVALUATE   RECE100-NINPUFLG
                    WHEN        0
                            MOVE    2    TO    RECE100-NINPUFLG
                    WHEN        1
                            MOVE    3    TO    RECE100-NINPUFLG
                 END-EVALUATE
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "11"
      *       初診料算定科：
              IF ( SRY-SRYCD(IDX2)    =     "830000020"          )
              OR ((SRY-SRYCD(IDX2)    =     "111011810"         ) AND
                  (RECE100-SRYYM     >=     "201204"            ))
              OR ( SRY-SRYCD(IDX2)    =     "111012610"
                                     OR     "111012810"          )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SYOSANTEIKA-COMMENT
              END-IF
      *       初診（同日複数診療科）
              IF  (SRY-SRYCD(IDX2)    =     "111011810"
                                      OR    "111012610"
                                      OR    "111012810"         )
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE120-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000024"
                   MOVE  SPACE  TO    RECE120-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE120-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "12"
      *       再診料算定科：
              IF   SRY-SRYCD(IDX2)    =     "830000021"
                                      OR    "112015810"
                                      OR    "112017010"
                                      OR    "112015950"
                                      OR    "112017150"
                                      OR    "112023550"
                                      OR    "112023850"
                                      OR    "112016210"
                                      OR    "112016410"
                                      OR    "112017610"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SAISANTEIKA-COMMENT
              END-IF
      *       電話再診
              IF  (SRY-SRYCD(IDX2)    =     "112009750"   OR
                                            "112009850"   OR
                                            "112009950"   OR
                                            "112010050"   OR
                                            "112012750"   OR
                                            "112704150"   OR
                                            "112704250"   OR
                                            "112704350"   OR
                                            "112706750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112007950"   OR
                                            "112008050"   OR
                                            "112008150"   OR
                                            "112008250"   OR
                                            "112012450"   OR
                                            "112702750"   OR
                                            "112702850"   OR
                                            "112702950"   OR
                                            "112706450"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"     )
              AND (RECE100-SRYYM      <     "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DENWASAI-COMMENT
              END-IF
              IF ((SRY-SRYCD(IDX2)    =     "112007950"     ) AND
                  (RECE100-SRYYM      >=    "201004"        ))
              OR ((SRY-SRYCD(IDX2)    =     "112016750"     ) AND
                  (RECE100-SRYYM      >=    "201501"        ))
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DENWASAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "112023350"
                                         OR "112023650"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DENWASAI2-COMMENT
              END-IF
      *       同日再診
              IF  (SRY-SRYCD(IDX2)    =     "112010150"   OR
                                            "112010250"   OR
                                            "112010350"   OR
                                            "112010450"   OR
                                            "112010550"   OR
                                            "112704450"   OR
                                            "112704550"   OR
                                            "112704650"   OR
                                            "112704750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"   OR
                                            "112008350"   OR
                                            "112008450"   OR
                                            "112008550"   OR
                                            "112008650"   OR
                                            "112008750"   OR
                                            "112703050"   OR
                                            "112703150"   OR
                                            "112703250"   OR
                                            "112703350"   OR
      *
                                            "112011710"   OR
                                            "112705850"     )
              AND (RECE100-SRYYM      <     "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUJITUSAI-COMMENT
              END-IF
              IF ((SRY-SRYCD(IDX2)    =     "112008350"   OR
                                            "112011710"   OR
                                            "112016550"     ) AND
                  (RECE100-SRYYM      >=    "201004"        ))
              OR ((SRY-SRYCD(IDX2)    =     "112016850"     ) AND
                  (RECE100-SRYYM      >=    "201501"        ))
              OR ((SRY-SRYCD(IDX2)    =     "112017450"     ) AND
                  (RECE100-SRYYM      >=    "201501"        ))
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUJITUSAI-COMMENT
              END-IF
      *       同日電話再診
              IF ((SRY-SRYCD(IDX2)    =     "112008850"     ) AND
                  (RECE100-SRYYM      >=    "201004"        ))
              OR ((SRY-SRYCD(IDX2)    =     "112016950"     ) AND
                  (RECE100-SRYYM      >=    "201501"        ))
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUDENSAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "112023450"
                                         OR "112023750"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUDENSAI2-COMMENT
              END-IF
      *
      *       再診（同一日２科目）
              IF  (SRY-SRYCD(IDX2)    =     "112015810"
                                      OR    "112017010"
                                      OR    "112015950"
                                      OR    "112017150"
                                      OR    "112023550"
                                      OR    "112023850"
                                      OR    "112016210"
                                      OR    "112016410"
                                      OR    "112017610"      )
              AND (FLG-SAIDOU-COM     =     ZERO             )
                 MOVE   "1"        TO   RECE120-FLG-SAIDOU-COMMENT
                 MOVE   SRY-SRYKA  TO   RECE120-SAIDOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "830000051"
                 MOVE   SPACE      TO   RECE120-FLG-SAIDOU-COMMENT
                 MOVE   SPACE      TO   RECE120-SAIDOU-SRYKA(1)
                 MOVE    1         TO   FLG-SAIDOU-COM
              END-IF
      *
      *       オンライン診療料
              IF   SRY-SRYCD(IDX2)    =     "112023210"
                 MOVE   "1"        TO   RECE120-FLG-ONLINE-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100006"
                 MOVE    1         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100007"
                 MOVE    2         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100008"
                 MOVE    3         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100009"
                 MOVE    4         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100010"
                 MOVE    5         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100011"
                 MOVE    6         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100012"
                 MOVE    7         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100013"
                 MOVE    8         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100014"
                 MOVE    9         TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "820100015"
                 MOVE    10        TO   RECE120-FLG-ONLINE2-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "40"
      *       介達牽引（発症日記載）
              IF   RECE100-SRYYM      >=    "200604"
              AND  RECE100-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     "140048010"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201004"
      *         人工腎臓等
                IF   SRY-SRYCD(IDX2)  =  "140007710" OR "140036710"
                                      OR "140051010" OR "140051110"
                                      OR "140008510" OR "140029850"
                                      OR "140008210" OR "140039210"
                                      OR "140052810" OR "140057810"
                                      OR "140057910" OR "140058010"
                                      OR "140058110" OR "140058210"
                                      OR "140058310" OR "140058410"
                                      OR "140058510" OR "140058610"
                                      OR "140059310" OR "140059410"
                                      OR "140059510"
                                      OR "140060210" OR "140060310"
                                      OR "140060410" OR "140060510"
                                      OR "140060610" OR "140060710"
                                      OR "140060810" OR "140060910"
                                      OR "140061010"
                        MOVE   "1"    TO    RECE120-FLG-JINZOU-COMMENT
                END-IF
      *         局所陰圧閉鎖処置
                IF    RECE100-SRYYM     >=    "201404"
                  IF   SRY-SRYCD(IDX2)  =     "140054270"
                                          OR  "140054370"
                                          OR  "140054470"
                        MOVE   "1"    TO    RECE120-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                 IF   RECE100-SRYYM     >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     "140052170"
                                          OR  "140052270"
                                          OR  "140052370"
                        MOVE   "1"    TO    RECE120-FLG-KYOKUIN-COMMENT
                  END-IF
                 ELSE
                  IF   SRY-SRYCD(IDX2)  =     "140052410"
                                          OR  "140051810"
                                          OR  "140051910"
                                          OR  "140052010"
                        MOVE   "1"    TO    RECE120-FLG-KYOKUIN-COMMENT
                  END-IF
                 END-IF
                END-IF
              END-IF
      *       磁気による膀胱等刺激法
              IF   RECE100-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "140055110"         )
                AND (FLG-BOKOHO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-BOKOHO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000085"
                   MOVE  SPACE  TO    RECE120-FLG-BOKOHO-COMMENT
                   MOVE    1    TO    FLG-BOKOHO-COM
                END-IF
              END-IF
      *       高気圧酸素治療
              IF  (SRY-SRYCD(IDX2)    =     "140057510"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "1"   TO    RECE120-FLG-KIATU-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "140057610"        )
              AND (FLG-KIATU-COM      =     ZERO               )
                   MOVE   "2"   TO    RECE120-FLG-KIATU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000085"
                                      OR    "840000086"
                                      OR    "840000045"
                   MOVE  SPACE  TO    RECE120-FLG-KIATU-COMMENT
                   MOVE    1    TO    FLG-KIATU-COM
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "80"
      *       リハビリ
              IF   SRY-SRYCD(IDX2)    =     "180706910"   OR
                                            "180022210"   OR
                                            "180706810"   OR
                                            "180022110"   OR
                                            "180707110"   OR
                                            "180022410"   OR
                                            "180707010"   OR
                                            "180022310"   OR
                                            "180707310"   OR
                                            "180022610"   OR
                                            "180707210"   OR
                                            "180022510"   OR
                                            "180707510"   OR
                                            "180022810"   OR
                                            "180707410"   OR
                                            "180022710"   OR
                                            "180708210"   OR
                                            "180023710"   OR
                                            "180708410"   OR
                                            "180023910"   OR
                                            "180708110"   OR
                                            "180023610"   OR
                                            "180708310"   OR
                                            "180023810"   OR
                                            "180024910"   OR
                                            "180024810"   OR
                                            "180025110"   OR
                                            "180025010"   OR
                                            "180025510"   OR
                                            "180025410"
      ****              MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "180027410"           OR
                                            "180027510"           OR
                                            "180029110"           OR
                                            "180029210"           OR
                                            "180028010"           OR
                                            "180028110"           OR
                                            "180029710"           OR
                                            "180029810"           OR
                                            "180027610"           OR
                                            "180027710"           OR
                                            "180029310"           OR
                                            "180029410"           OR
                                            "180030810"           OR
                                            "180032410"           OR
                                            "180032510"           OR
                                            "180032610"           OR
                                            "180033910"           OR
                                            "180034010"           OR
                                            "180034110"           OR
                                            "180034210"           OR
                                            "180034310"           OR
                                            "180034410"           OR
                                            "180043430"           OR
                                            "180043530"           OR
                                            "180043630"           OR
                                            "180043730"           OR
                                            "180043830"           OR
                                            "180043930"           OR
                                            "180050330"           OR
                                            "180050430"           OR
                                            "180050530"           OR
                                            "180050730"           OR
                                            "180050930"           OR
                                            "180051130"           OR
                                            "180051230"           OR
                                            "180051330"           OR
                                            "180051430"           OR
                                            "180027810"           OR
                                            "180027910"           OR
                                            "180029510"           OR
                                            "180029610"           OR
                                            "180032710"           OR
                                            "180034510"           OR
                                            "180034610"           OR
                                            "180034710"           OR
                                            "180044030"           OR
                                            "180044130"           OR
                                            "180044230"           OR
                                            "180052730"           OR
                                            "180052830"           OR
                                            "180052930"           OR
                                            "180053130"           OR
                                            "180053330"           OR
                                            "180053530"           OR
                                            "180053630"           OR
                                            "180053730"           OR
                                            "180053830"           OR
                                            "180028210"           OR
                                            "180028310"           OR
                                            "180028410"           OR
                                            "180044610"           OR
                                            "180044710"           OR
                                            "180044810"           OR
                                            "180045010"           OR
                                            "180045210"           OR
                                            "180045410"           OR
                                            "180045530"           OR
                                            "180045630"           OR
                                            "180045730"           OR
                                            "180051530"           OR
                                            "180051630"           OR
                                            "180051730"           OR
                                            "180051930"           OR
                                            "180052130"           OR
                                            "180052330"           OR
                                            "180052430"           OR
                                            "180052530"           OR
                                            "180052630"           OR
                                            "180033110"
      ****              MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     "180029910"
                        MOVE   "2"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030010"
                        MOVE   "3"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030510"
                        MOVE   "4"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030610"
                        MOVE   "5"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030110"
                        MOVE   "6"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030210"
                        MOVE   "7"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030310"
                        MOVE   "8"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     "180030410"
                        MOVE   "9"    TO    RECE120-RIHABIRI-COMMENT
              END-EVALUATE
      *       リハビリ（発症日記載）
              IF   RECE100-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     "180027410"
                                      OR    "180027510"
                                      OR    "180029110"
                                      OR    "180029210"
                                      OR    "180029910"
                                      OR    "180030010"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180028010"
                                      OR    "180028110"
                                      OR    "180029710"
                                      OR    "180029810"
                                      OR    "180030510"
                                      OR    "180030610"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-KOKYUKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180027610"
                                      OR    "180027710"
                                      OR    "180029310"
                                      OR    "180029410"
                                      OR    "180030110"
                                      OR    "180030210"
                                      OR    "180030810"
                                      OR    "180032410"
                                      OR    "180032510"
                                      OR    "180032610"
                                      OR    "180033910"
                                      OR    "180034010"
                                      OR    "180034110"
                                      OR    "180034210"
                                      OR    "180034310"
                                      OR    "180034410"
                                      OR    "180043430"
                                      OR    "180043530"
                                      OR    "180043630"
                                      OR    "180043730"
                                      OR    "180043830"
                                      OR    "180043930"
                                      OR    "180050330"
                                      OR    "180050430"
                                      OR    "180050530"
                                      OR    "180050730"
                                      OR    "180050930"
                                      OR    "180051130"
                                      OR    "180051230"
                                      OR    "180051330"
                                      OR    "180051430"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180027810"
                                      OR    "180027910"
                                      OR    "180029510"
                                      OR    "180029610"
                                      OR    "180030310"
                                      OR    "180030410"
                                      OR    "180032710"
                                      OR    "180034510"
                                      OR    "180034610"
                                      OR    "180034710"
                                      OR    "180044030"
                                      OR    "180044130"
                                      OR    "180044230"
                                      OR    "180052730"
                                      OR    "180052830"
                                      OR    "180052930"
                                      OR    "180053130"
                                      OR    "180053330"
                                      OR    "180053530"
                                      OR    "180053630"
                                      OR    "180053730"
                                      OR    "180053830"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-UNDOKI-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180017910"        )
                AND (RECE100-SRYYM   <=     "201803"           )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-NANBYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180028210"
                                      OR    "180028310"
                                      OR    "180028410"         )
                AND (RECE100-SRYYM   <=     "201803"            )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180016610"
                                      OR    "180054310"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SESSYOKU-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "180044610"
                                      OR    "180044710"
                                      OR    "180044810"
                                      OR    "180045010"
                                      OR    "180045210"
                                      OR    "180045410"
                                      OR    "180045530"
                                      OR    "180045630"
                                      OR    "180045730"
                                      OR    "180051530"
                                      OR    "180051630"
                                      OR    "180051730"
                                      OR    "180051930"
                                      OR    "180052130"
                                      OR    "180052330"
                                      OR    "180052430"
                                      OR    "180052530"
                                      OR    "180052630"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-HAIYO-COMMENT
                END-IF
              END-IF
      *
              IF   (RECE100-SRYYM   >=   "201604"             )
              AND  (SRY-SRYCD(IDX2)  =   "180033870"
                                     OR  "180030770"          )
                IF   RECE120-SINDAI-COMMENT = "1"
                    MOVE   "1"    TO    RECE120-SINDAI9-COMMENT
                END-IF
                IF   RECE120-KOKYUKI-COMMENT = "1"
                    MOVE   "1"    TO    RECE120-KOKYUKI9-COMMENT
                END-IF
                IF   RECE120-HAIYO-COMMENT = "1"
                    MOVE   "1"    TO    RECE120-HAIYO9-COMMENT
                END-IF
              END-IF
      *
      *       認知療法・認知行動療法
              IF   RECE100-SRYYM     >=    "201004"
                IF  (SRY-SRYCD(IDX2)  =     "180033210"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "1"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180035910"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "2"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180047810"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "5"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180056410"         )
                AND (FLG-NINRYO-COM   =     ZERO                )
                        MOVE   "8"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
              END-IF
      *       精神科重症患者早期集中支援管理料
      *       精神科在宅患者支援管理料
              IF   RECE100-SRYYM     >=    "201604"
                IF  (SRY-SRYCD(IDX2)  =     "180057210"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "A"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057310"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "B"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057810"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "G"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180057910"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "H"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF   RECE100-SRYYM   >=    "202004"
                  IF  (SRY-SRYCD(IDX2)  =     "180057410"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "C"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057510"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "D"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057610"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "E"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180057710"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "F"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180058010"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "I"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180058110"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "J"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180064010"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "P"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                  IF  (SRY-SRYCD(IDX2)  =     "180064110"          )
                  AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "Q"    TO    RECE120-FLG-NINRYO-COMMENT
                  END-IF
                END-IF
      *
                IF  (SRY-SRYCD(IDX2)  =     "180042110"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "K"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180042410"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "L"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180048710"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "M"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =     "180048810"          )
                AND (FLG-NINRYO-COM   =     ZERO                 )
                        MOVE   "N"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
              END-IF
              IF     SRY-SRYCD(IDX2)  =     "840000097"
                        MOVE  SPACE   TO    RECE120-FLG-NINRYO-COMMENT
                        MOVE    1     TO    FLG-NINRYO-COM
              END-IF
      *
      *       通院・在宅精神療法（２０歳未満）加算
              IF   RECE100-SRYYM     >=    "201404"
      ***       IF   SRY-SRYCD(IDX2)  =     "180020570"
                IF    (TNS-DATAKBN             =    1 )
                AND   (TNS-CDKBN-KBN           =   "I")
                AND   (TNS-CDKBN-KBNNUM        =    2 )
                AND   (TNS-CDKBN-KBNNUM-EDA    =    0 )
                AND   (RECE100-AGE             <    20)
                  IF    (RECE100-AGE           =      19            )
                  AND   (RECE100-BIRTHDAY(5:2) =  RECE100-SRYYM(5:2))
                  AND   (RECE100-BIRTHDAY(7:2) NOT = "01"           )
                    PERFORM VARYING IDX22 FROM 1 BY 1
                             UNTIL (IDX22                       >  31)
                              OR   (RECE120-FLG-TUINZAI-COMMENT = "1")
                      IF   ACCT-DAY(1 IDX22)  NOT = ZERO
                        IF   IDX22 < RECE100-BIRTHDAY(7:2)
                          MOVE "1"    TO    RECE120-FLG-TUINZAI-COMMENT
                        END-IF
                      END-IF
                    END-PERFORM
                  ELSE
                        MOVE   "1"    TO    RECE120-FLG-TUINZAI-COMMENT
                  END-IF
                END-IF
              END-IF
              IF   RECE100-SRYYM     >=    "201604"
                IF   SRY-SRYCD(IDX2)  =     "180020570"
                                      OR    "180047270"
                                      OR    "180047370"
                        MOVE   "1"    TO    RECE120-FLG-TUINZAI2-COMMENT
                END-IF
              END-IF
      *       依存症集団療法
      *       疾患別等専門プログラム加算
      *       経頭蓋磁気刺激療法
              IF   RECE100-SRYYM     >=    "201604"
                IF  (SRY-SRYCD(IDX2)  =    "180047910")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "1"    TO    RECE120-FLG-IZONSYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =    "180056570")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "2"    TO    RECE120-FLG-IZONSYO-COMMENT
                END-IF
              END-IF
              IF   RECE100-SRYYM     >=    "202004"
                IF  (SRY-SRYCD(IDX2)  =    "180058710")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "3"    TO    RECE120-FLG-IZONSYO-COMMENT
                END-IF
                IF  (SRY-SRYCD(IDX2)  =    "180058650")
                AND (FLG-IZONSYO-COM  =    ZERO       )
                        MOVE   "4"    TO    RECE120-FLG-IZONSYO-COMMENT
                END-IF
              END-IF
              IF   SRY-SRYCD(IDX2)  =     "840000076"
                        MOVE  SPACE   TO    RECE120-FLG-IZONSYO-COMMENT
                        MOVE    1     TO    FLG-IZONSYO-COM
              END-IF
      *
      *       精神科オンライン在宅管理料
              IF   SRY-SRYCD(IDX2)    =     "180058270"
                 MOVE   "4"        TO   RECE120-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     "113006510"
                   MOVE   "1"   TO    RECE120-FLG-SHIDOU-COMMENT
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "180000710"
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000011"
                                      OR    "840000012"
                                      OR    "840000511"
                                      OR    "840000512"
                   MOVE  SPACE  TO    RECE120-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "113004310"
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000078"
                                      OR    "840000578"
                   MOVE  SPACE  TO    RECE120-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "113008310" OR "113008410"
                                      OR    "113008510" OR "113031610")
              AND (FLG-NIKOCHIN-COM   =     ZERO                      )
                   MOVE   "1"   TO    RECE120-FLG-NIKOCHIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE  TO    RECE120-FLG-NIKOCHIN-COMMENT
                   MOVE    1    TO    FLG-NIKOCHIN-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "113008070"
                                      OR    "113008170"          )
              AND (FLG-ZENSOKU-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE120-FLG-ZENSOKU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000098"
                   MOVE  SPACE  TO    RECE120-FLG-ZENSOKU-COMMENT
                   MOVE    1    TO    FLG-ZENSOKU-COM
              END-IF
      *
              IF   RECE100-SRYYM    >=     "200804"
                IF   (SRY-SRYCD(IDX2)   =     "113000810"         )
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-SYOUNI-COMMENT
                   MOVE   "1"   TO    RECE100-SYOUNIFLG
                END-IF
                IF   (SRY-SRYCD(IDX2)   =     "113009910"
                                        OR    "113029410"         )
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "2"   TO    RECE120-FLG-SYOUNI-COMMENT
                END-IF
                IF    SRY-SRYCD(IDX2)   =     "840000104"
                                        OR    "840000604"
                   MOVE  SPACE  TO    RECE120-FLG-SYOUNI-COMMENT
                   MOVE    1    TO    FLG-SYOUNI-COM
                END-IF
              END-IF
      *
              IF  (RECE100-SRYYM     >=    "201004")
              AND (RECE100-SRYYM     <=    "202003")
                IF   RECE100-SRYYM   >=    "201604"
      *           遠隔モニタリング加算
                  IF   SRY-SRYCD(IDX2) =  "113018070"
                        MOVE   "1"    TO    RECE120-FLG-SINPESU-COMMENT
                  END-IF
                ELSE
      *           心臓ペースメーカー指導管理料（遠隔モニタリング）
                  IF   SRY-SRYCD(IDX2) =  "113001510"
                        MOVE   "1"    TO    RECE120-FLG-SINPESU-COMMENT
                  END-IF
                END-IF
              END-IF
              IF   RECE100-SRYYM     >=    "202004"
      *           遠隔モニタリング加算（ペースメーカー）
                  IF   SRY-SRYCD(IDX2) =  "113030470"
                        MOVE   "4"    TO    RECE120-FLG-SINPESU-COMMENT
                  END-IF
      *           遠隔モニタリング加算（植込型除細動器等）
                  IF   SRY-SRYCD(IDX2) =  "113030570"
                        MOVE   "5"    TO    RECE120-FLG-SINPESU-COMMENT
                  END-IF
              END-IF
              IF   RECE100-SRYYM     >=    "201004"
              AND  RECE100-SRYYM     <=    "201803"
      *         集団栄養食事指導料
                IF   SRY-SRYCD(IDX2)  =     "113003410"
                        MOVE   "1"    TO    RECE120-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201204"
                IF   SRY-SRYCD(IDX2)  =     "113014610"
                        MOVE   "1"    TO    RECE120-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113024310"
                        MOVE   "2"    TO    RECE120-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113027910"
                        MOVE   "3"    TO    RECE120-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113028310"
                        MOVE   "4"    TO    RECE120-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113008710"
                                      OR    "113008610"
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-FLG-TAKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113014370"
                        MOVE   "1"    TO    RECE120-FLG-TAKYOKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     "113013910"
                                      OR    "113014010"
                                      OR    "113014110"
                        MOVE   "1"    TO    RECE120-FLG-GAIRIHA-COMMENT
                END-IF
              END-IF
      *       人工内耳機器調整加算
              IF   SRY-SRYCD(IDX2)   =  "113030670"
                   MOVE   "1"   TO    RECE120-FLG-JINNAI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "113030770"
                   MOVE   "2"   TO    RECE120-FLG-JINNAI-COMMENT
              END-IF
      *       外来排尿自立指導料
              IF  (SRY-SRYCD(IDX2)   =  "113032810"        )
              AND (FLG-GAIJIR-COM    =  ZERO               )
                   MOVE   "1"   TO    RECE120-FLG-GAIJIR-COMMENT
                   PERFORM  VARYING  IDX40  FROM  1  BY  1
                             UNTIL   IDX40    >  31
                     IF  ACCT-DAY(1 IDX40) NOT = ZERO
                       IF  RECE100-GAIJIRYMD  =  SPACE
                         MOVE  RECE100-SRYYM TO  RECE100-GAIJIRYMD(1:6)
                         MOVE  IDX40         TO  RECE100-GAIJIRYMD(7:2)
                       ELSE
                         MOVE  RECE100-GAIJIRYMD(7:2)  TO  IDX41
                         IF    IDX40  <  IDX41
                           MOVE  IDX40       TO  RECE100-GAIJIRYMD(7:2)
                         END-IF
                       END-IF
                       MOVE    31    TO   IDX40
                     END-IF
                   END-PERFORM
              END-IF
              IF   SRY-SRYCD(IDX2)   =  "840000097"
                                     OR "840000086"
                   MOVE  SPACE  TO    RECE120-FLG-GAIJIR-COMMENT
                   MOVE    1    TO    FLG-GAIJIR-COM
              END-IF
      *       心臓ペースメーカー指導管理料（着用型自動除細動器）
      *       心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
      *       がん患者指導管理料２・３
              IF   RECE100-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2) =  "113015010"           )
                AND (RECE100-SRYYM  <=  "201803"              )
                   MOVE   "1"   TO    RECE120-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015130"
                   MOVE   "2"   TO    RECE120-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015310"
                   MOVE   "1"   TO    RECE120-FLG-GANSIDO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113015410"
                   MOVE   "2"   TO    RECE120-FLG-GANSIDO-COMMENT
                END-IF
              END-IF
              IF  (RECE100-SRYYM    >=     "201604")
              AND (RECE100-SRYYM    <=     "202003")
                IF   SRY-SRYCD(IDX2) =  "113001610"
                   MOVE   "3"   TO    RECE120-FLG-SINPESUR-COMMENT
                END-IF
              END-IF
              IF   RECE100-SRYYM    >=     "202004"
                IF   SRY-SRYCD(IDX2) =  "113030210"
                   MOVE   "4"   TO    RECE120-FLG-SINPESUR-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2) =  "113030310"
                   MOVE   "5"   TO    RECE120-FLG-SINPESUR-COMMENT
                END-IF
              END-IF
      *       乳腺炎重症化予防ケア・指導料（初回）
      *       乳腺炎重症化予防ケア・指導料（２回目から４回目まで）
              IF   SRY-SRYCD(IDX2)    =     "113024410"
              AND  FLG-NYUSEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-NYUSEN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "113024510"
              AND  FLG-NYUSEN-COM     =     ZERO
                   MOVE   "2"   TO    RECE120-FLG-NYUSEN-COMMENT
                   MOVE   "1"   TO    RECE100-NYUSENFLG
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000086"
                   MOVE  SPACE  TO    RECE120-FLG-NYUSEN-COMMENT
                   MOVE    1    TO    FLG-NYUSEN-COM
              END-IF
      *
      *       オンライン医学管理料
              IF   SRY-SRYCD(IDX2)    =     "113023890"
                 MOVE   "2"        TO   RECE120-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     "114008010"
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000073"
                   MOVE  SPACE  TO    RECE120-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     "114009410"
                   MOVE   "1"   TO    RECE120-FLG-HINKAI-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     "114000110"
                                      OR    "114001610"       )
              AND (FLG-OSIN-COM       =     ZERO              )
                   MOVE   "1"   TO    RECE120-FLG-OSIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000039"
                   MOVE  SPACE  TO    RECE120-FLG-OSIN-COMMENT
                   MOVE    1    TO    FLG-OSIN-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =  "114012210" OR "114012310"
                                      OR "114012410" OR "114007510"
                                      OR "114013010" OR "114013110"
                                      OR "114013210" OR "114013310"
                                      OR "114018710" OR "114018810"
                                      OR "114018910" OR "114019010"
                                      OR "114019110" OR "114019210"
                                      OR "114019310" OR "114019410"
                                      OR "114023110" OR "114023210"
                                      OR "114023310" OR "114023410"
                                      OR "114023510" OR "114023610"
                                      OR "114023710" OR "114023810"
                                      OR "114023910" OR "114024010"
                                      OR "114024110" OR "114024210"
                                      OR "114024310" OR "114024410"
                                      OR "114024510" OR "114024610"
                                      OR "114024910" OR "114025010"
                                      OR "114025110" OR "114025210"
                                      OR "114025310" OR "114025410"
                                      OR "114025510" OR "114025610"
                                      OR "114025710" OR "114025810"
                                      OR "114025910" OR "114026010"
                                      OR "114026110" OR "114026210"
                                      OR "114026310" OR "114026410"
                                      OR "114030710" OR "114030810"
                                      OR "114030910" OR "114031010"
                                      OR "114031110" OR "114031210"
                                      OR "114031310" OR "114031410"
                                      OR "114031510" OR "114031610"
                                      OR "114031710" OR "114031810"
                                      OR "114031910" OR "114032010"
                                      OR "114032110" OR "114032210"
                                      OR "114032310" OR "114032410"
                                      OR "114032510" OR "114032610"
                                      OR "114032710" OR "114032810"
                                      OR "114032910" OR "114033010"
                                      OR "114033110" OR "114033210"
                                      OR "114033310" OR "114033410"
                                      OR "114033510" OR "114033610"
                                      OR "114033710" OR "114033810"
                                      OR "114033910" OR "114034010"
                                      OR "114034110" OR "114034210"
      *
                                      OR "114035510" OR "114035610"
                                      OR "114035710" OR "114035810"
                                      OR "114035910" OR "114036010"
                                      OR "114036110" OR "114036210"
                                      OR "114036310" OR "114036410"
                                      OR "114036510" OR "114036610"
                                      OR "114036710" OR "114036810"
                                      OR "114036910" OR "114037010"
                                      OR "114037110" OR "114037210"
                                      OR "114037310" OR "114037410"
                                      OR "114037510" OR "114037610"
                                      OR "114037710" OR "114037810"
                                      OR "114037910" OR "114038010"
                                      OR "114038110" OR "114038210"
                                      OR "114038310" OR "114038410"
                                      OR "114038510" OR "114038610"
                                      OR "114038710" OR "114038810"
                                      OR "114038910" OR "114039010"
      *
                                      OR "114001110" OR "114012910"
                                      OR "114018010" OR "114030310"
                                      OR "114042110" OR "114042210"
                                      OR "114042810" OR "114046310"
                   MOVE   "1"   TO    RECE100-COMCNTFLG
                   MOVE   "2"   TO    RECE120-FLG-OSIN-COMMENT
              END-IF
      *
              IF   (SRY-SRYCD(IDX2)   =     "114001110"
                                      OR    "114012910"
                                      OR    "114018010"
                                      OR    "114030310"
                                      OR    "114042110"
                                      OR    "114042210"
                                      OR    "114042810"
                                      OR    "114046310"         )
              AND  (FLG-ZAIHOU-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-ZAIHOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000066"
                   MOVE  SPACE  TO    RECE120-FLG-ZAIHOU-COMMENT
                   MOVE    1    TO    FLG-ZAIHOU-COM
              END-IF
      *
              IF   RECE100-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114000110"
                                    OR  "114001610"
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE100-COMCNTFLG
                       MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE120-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE100-COMCNTFLG
                       MOVE   "2"   TO    RECE120-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE100-SRYYM   >=  "200604"
                  AND  RECE100-SRYYM   <=  "200803"
                      IF SRY-SRYCD(IDX2) =  "114001110"         OR
                                            "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                  IF   RECE100-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE100-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  "114011570"         OR
                                            "114011670"         OR
                                            "114011770"
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "2"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  "114020910"
                   MOVE   "1"   TO    RECE120-FLG-KAISYOKU-COMMENT
                END-IF
      ***       IF   SRY-SRYCD(IDX2) =  "114009710"
      ***          MOVE   "1"   TO    RECE120-FLG-YOUATU-COMMENT
      ***       END-IF
              END-IF
      *       在宅患者共同診療料
              IF   RECE100-SRYYM    >=     "201404"
                IF  (SRY-SRYCD(IDX2)    =     "114027610"
                                        OR    "114027710"
                                        OR    "114027810"
                                        OR    "114027910"         )
                AND (FLG-ZAIKYO-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-ZAIKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)    =     "840000097"
                                        OR    "840000086"
                   MOVE  SPACE  TO    RECE120-FLG-ZAIKYO-COMMENT
                   MOVE    1    TO    FLG-ZAIKYO-COM
                END-IF
              END-IF
      *       在宅患者訪問リハビリテーション指導管理料
              IF   RECE100-SRYYM    >=     "201611"
                IF  (SRY-SRYCD(IDX2)    =     "114006410"
                                        OR    "114015010"          )
                AND (FLG-ZAIRIHA-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE120-FLG-ZAIRIHA-COMMENT
                END-IF
              END-IF
      *       在宅患者訪問薬剤管理指導料
              IF   SRY-SRYCD(IDX2)    =     "114044710"
                                      OR    "114044810"
                                      OR    "114044910"
                   MOVE   "1"   TO    RECE120-FLG-ZAIYAK-COMMENT
              END-IF
      *       遠隔モニタリング加算（在宅酸素療法指導管理料（その他））
      *       遠隔モニタリング加算（在宅持続陽圧呼吸療法指導管理料２）
              IF   SRY-SRYCD(IDX2)    =     "114045470"
                   MOVE   "A"   TO    RECE120-FLG-SINPESU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114045670"
                   MOVE   "B"   TO    RECE120-FLG-SINPESU-COMMENT
              END-IF
      *       在宅酸素療法指導管理料（その他）
      *       在宅持続陽圧呼吸療法指導管理料２
              IF   SRY-SRYCD(IDX2)    =     "114003710"
                   MOVE   "A"   TO    RECE120-FLG-SINPESUR-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "114040810"
                   MOVE   "B"   TO    RECE120-FLG-SINPESUR-COMMENT
              END-IF
      *       在宅半固形栄養経管栄養法指導管理料
              IF  (SRY-SRYCD(IDX2)    =     "114045510"          )
              AND (FLG-NINRYO-COM     =     ZERO                 )
                   MOVE   "9"   TO    RECE120-FLG-NINRYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000097"
                   MOVE  SPACE  TO    RECE120-FLG-NINRYO-COMMENT
                   MOVE    1    TO    FLG-NINRYO-COM
              END-IF
      *
      *       オンライン在宅管理料（在医総管（月１回訪問診療））
              IF   SRY-SRYCD(IDX2)    =     "114043970"
                 MOVE   "3"        TO   RECE120-FLG-ONLINE-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "50"
              IF   RECE120-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     "840000042"
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE100-SRYYM     >=    "201004"
            IF     RECE120-SRYKBN     =     "54"
              IF   RECE120-SYUTEN1    =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170011910" OR "170012110"
                                      OR "170015550" OR "170015210"
                                      OR "170020110" OR "170022110"
                                      OR "170022290" OR "170022390"
                                      OR "170022490" OR "170023010"
                                      OR "170023110" OR "170023210"
                                      OR "170023310" OR "170023410"
                                      OR "170023510"               )
              AND (RECE100-SRYYM      <     "201004"               )
                   MOVE   "1"   TO    RECE120-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =  "170011710" OR "170011810"
                                      OR "170012110" OR "170015210"
                                      OR "170020110" OR "170028610"
                                      OR "170033410" OR "170033510"
                                      OR "170034910" OR "170035010")
              AND (RECE100-SRYYM      >=    "201004"               )
              AND (RECE100-SRYYM      <=    "202003"               )
                   MOVE   "1"   TO    RECE120-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     "160162350"
                                      OR    "160181250"
                                      OR    "160116310"        )
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE120-FLG-KENSA-COMMENT
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160162350"
                   MOVE    1    TO    RECE100-KENSA-BNPFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160181250"
                   MOVE    1    TO    RECE100-KENSA-NTPROFLG
              END-IF
              IF    SRY-SRYCD(IDX2)   =     "160116310"
                   MOVE    1    TO    RECE100-KENSA-ANPFLG
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                    OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO  1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE120-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2104411-RECE130-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の単価編集処理
      *****************************************************************
       2104412-RECE130-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    数字表示用編集（半角）
           MOVE    RECE130-TEN(IDX4)
                                   TO  WRK-TANKA
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    2   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-TANKA-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY:1)
                                         TO  WRK-TANKA-Z3(IDY:1)
             END-IF
           END-PERFORM
           IF  WRK-TANKA-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-TANKA-Z2
           END-IF
      *        
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-TANKA-X         TO  WRK-CHAR-X
           MOVE    12                  TO  WRK-CHAR-CNT
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   WRK-CHAR-CNT
               IF      WRK-CHAR-X (IDW:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDW:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDW:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDW:1)  TO  NUM-IDX
                           MOVE    TBL-SUJI (NUM-IDX)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       IF    WRK-CHAR-X (IDW:1)  =   "."
                           MOVE    "．"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE    
                           MOVE    "  "   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   END-IF    
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           IF      WRK-CHAR-N-CNT      >   ZERO
                   MOVE    "円"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF                             
      *
           .
       2104412-RECE130-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細数等の編集
      *****************************************************************
       2104413-MEISAISU-HENSYU-SEC                 SECTION.
      *
           COMPUTE   RECE120-MEISAISU    =  RECE120-MEISAISU
                                         -  1
           MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
           COMPUTE   RECE120-SRYCDTOTAL  =  RECE120-SRYCDTOTAL
                                         -  WRK-SRYCD
           COMPUTE   RECE120-SURYOUTOTAL =  RECE120-SURYOUTOTAL
                                         -  SRY-SRYSURYO(IDX2)
      *
           .
       2104413-MEISAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の数量編集処理
      *****************************************************************
       2104414-RECE130-SURYO-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      *
      *    数字表示用編集（半角）
      ***  MOVE    RECE130-SRYSURYO(IDX4) TO  WRK-SURYO
           MOVE    WRK-SRYSURYO           TO  WRK-SURYO
           MOVE    WRK-SURYO-S1           TO  WRK-SURYO-Z1
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDW   FROM    5   BY  -1
                      UNTIL    IDW   =   ZERO
             IF  WRK-SURYO-S2(IDW:1) NOT =   ZERO
                 MOVE    1         TO  FLG-SP
             END-IF
             IF  FLG-SP  =   1
                 MOVE    WRK-SURYO-S2(IDW:1)
                                   TO  WRK-SURYO-Z3(IDW:1)
             END-IF
           END-PERFORM
           IF  WRK-SURYO-Z3   NOT =  SPACE
               MOVE    "."         TO  WRK-SURYO-Z2
           END-IF
      *    表示文字数カウント
           PERFORM   VARYING   IDW   FROM    1   BY  1
                      UNTIL    IDW     >    11
             IF  WRK-SURYO-X(IDW:1)  NOT =  SPACE
                 IF  WRK-SURYOST           =   ZERO
                     MOVE    IDW   TO  WRK-SURYOST
                 END-IF
      *          数量桁数カウント(６桁目：ピリオド)
                 ADD     1         TO  WRK-SURYOKETA
                 IF  IDW    <   6
                     ADD     1     TO  WRK-SURYOKETA1
                 END-IF
                 IF  IDW    >   6
                     ADD     1     TO  WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST  =   6
               MOVE   "0"          TO  WRK-SURYO-Z1(5:1)
               MOVE    5           TO  WRK-SURYOST
               MOVE    1           TO  WRK-SURYOKETA1
               ADD     1           TO  WRK-SURYOKETA
           END-IF
      *    全角変換
           IF  WRK-SURYOKETA1   >   ZERO
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z1(WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2   >   ZERO
               MOVE    "．"            TO  WRK-SURYO-N2
      *
               INITIALIZE              WRK-CHAR-HENKAN-AREA
               MOVE    WRK-SURYO-Z3(1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR-CNT
      *
               MOVE    1               TO  WRK-CHAR-IDX
               PERFORM  VARYING  IDW   FROM    1   BY  1
                         UNTIL   IDW     >   WRK-CHAR-CNT
                   IF    WRK-CHAR-X(IDW:1)  IS  NUMERIC
                     IF    WRK-CHAR-X(IDW:1)  =   "0"
                        MOVE  TBL-SUJI(10)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     ELSE
                        MOVE  WRK-CHAR-X(IDW:1)  TO  NUM-IDX
                        MOVE  TBL-SUJI(NUM-IDX)
                                       TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                     END-IF
                   ELSE
                       MOVE    "  "    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                   END-IF
                   ADD     2           TO  WRK-CHAR-IDX
               END-PERFORM
      *
               MOVE    WRK-CHAR-N      TO  WRK-SURYO-N3
           END-IF
      *
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           .
       2104414-RECE130-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハ開始日編集
      *****************************************************************
       2104415-RIHAYMD-HENSYU-SEC                 SECTION.
      *
           MOVE      SPACE   TO  WRK-RIHA-YMD
           MOVE      SPACE   TO  WRK-RIHA-YMD-SEI
      *
           IF    SRY-SRYCD(IDX2) = "840000061" OR "840000561"
             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
             MOVE    SRY-PTID             TO  PTCOM-PTID
             MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
             MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
             MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
             PERFORM 950-PTCOM-READ-SEC
      *
             IF    SRY-SRYCD(IDX2) = "840000061"
               MOVE     "4"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             IF    SRY-SRYCD(IDX2) = "840000561"
               MOVE     "5"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             MOVE  PTCOM-INPUTCHI1(1:2)  TO  WRK-RIHA-YMD(2:2)
             MOVE  PTCOM-INPUTCHI2(1:2)  TO  WRK-RIHA-YMD(4:2)
             MOVE  PTCOM-INPUTCHI3(1:2)  TO  WRK-RIHA-YMD(6:2)
      *
             IF    WRK-RIHA-YMD  IS  NUMERIC
               INITIALIZE                   LNK-DAY1-AREA
               INITIALIZE                   STS-AREA-DAY
               MOVE    "12"           TO    LNK-DAY1-IRAI
               MOVE    WRK-RIHA-YMD   TO    LNK-DAY1-WYMD
               CALL    "ORCSDAY"      USING STS-AREA-DAY
                                            LNK-DAY1-AREA
               IF      STS-DAY-RC1    =     ZERO
                 MOVE  LNK-DAY1-YMD   TO  WRK-RIHA-YMD-SEI
               ELSE
                 MOVE  SPACE          TO  WRK-RIHA-YMD
               END-IF
             ELSE
               MOVE    SPACE          TO  WRK-RIHA-YMD
             END-IF
           END-IF
      *
           EVALUATE    FLG-RIHA-OKIKAE
             WHEN         1
               MOVE       1     TO  RECE100-RIHA2-SINDAI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-SINYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-SINYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-SINYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-SINYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         2
               MOVE       1     TO  RECE100-RIHA2-NOUKE
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-NOUYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-NOUYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-NOUYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-NOUYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         3
               MOVE       1     TO  RECE100-RIHA2-UNDOKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-UNDYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-UNDYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-UNDYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-UNDYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         4
               MOVE       1     TO  RECE100-RIHA2-KOKYUKI
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-KOKYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-KOKYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-KOKYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-KOKYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         5
               MOVE       1     TO  RECE100-RIHA2-SESSYOKU
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-SESYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-SESYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-SESYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-SESYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         6
               MOVE       1     TO  RECE100-RIHA2-HAIYO
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-HAIYMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                     >   2  )
                              OR    (RECE100-RIHA2-HAIYMD1(IDW9)
                                                              = SPACE)
                              OR    (FLG-ARI5                 =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-HAIYMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-HAIYMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         7
               MOVE       1     TO  RECE100-RIHA2-SINDAI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-SIN9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE100-RIHA2-SIN9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-SIN9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-SIN9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         8
               MOVE       1     TO  RECE100-RIHA2-KOKYUKI9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-KOK9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE100-RIHA2-KOK9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-KOK9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-KOK9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
             WHEN         9
               MOVE       1     TO  RECE100-RIHA2-HAIYO9
               IF   WRK-RIHA-YMD-SEI  NOT = SPACE
                 IF   WRK-RIHA-YMD-SEI(1:6) = RECE100-SRYYM
                   MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW9
                   MOVE   1     TO  RECE100-RIHA2-HAI9YMD2(IDW9)
                 ELSE
                   MOVE   ZERO  TO  FLG-ARI5
                   PERFORM  VARYING  IDW9  FROM  1  BY  1
                             UNTIL  (IDW9                      >   2  )
                              OR    (RECE100-RIHA2-HAI9YMD1(IDW9)
                                                               = SPACE)
                              OR    (FLG-ARI5                  =   1  )
                     IF  WRK-RIHA-YMD-SEI =
                                    RECE100-RIHA2-HAI9YMD1(IDW9)
                       MOVE  1  TO  FLG-ARI5
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI5 = ZERO
                   AND   IDW9    <=  2
                     MOVE  WRK-RIHA-YMD-SEI
                                TO  RECE100-RIHA2-HAI9YMD1(IDW9)
                   END-IF
                 END-IF
               END-IF
           END-EVALUATE
      *
           .
       2104415-RIHAYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）
      *****************************************************************
       210442-RECE100-HENSYU-SEC               SECTION.
      *
      *    処方減回数の初期化
           MOVE   ZERO              TO   WRK-SYOHOU-KAISU
      *    処方せん減回数の初期化
           MOVE   ZERO              TO   WRK-SYOSEN-KAISU
      *    処方料（麻・向・毒・覚）の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-MADOKU-SHOHO
      *
      *    公費の分離表示用インデックスの確定
           IF   FLG-SHOSAISIN        =       1                      
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1  
                           UNTIL    ACCT-IDX   >      31            
                  IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO          
                      MOVE    "1"       TO                      
                                        TBL-JNISSU-DAY(ACCT-IDX)      
                  END-IF                                            
                END-PERFORM                                         
           END-IF
           IF   FLG-SHOSAISIN-GEN    =       1
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                           UNTIL    ACCT-IDX   >      31
                  IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                      MOVE    "1"       TO
                                    TBL-JNISSU-DAY-GEN(ACCT-IDX)
                  END-IF
                END-PERFORM
           END-IF
      *
           MOVE      ZERO       TO     WK-ZAIKAISU
           MOVE      ZERO       TO     WK-ZAITEN
           MOVE      ZERO       TO     WK-SYUTEN
           MOVE      ZERO       TO     WK-KIZAITEN
           MOVE      ZERO       TO     WK-YKZTEN
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                            "40" OR "50" OR "54" OR
                                            "60" OR "70" OR "80" OR
                                            "64"
                           MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                           COMPUTE  WK-YKZTEN  =  ACCT-KIZAITEN
                                               +  ACCT-YKZTEN
                           IF  SRY-SRYKBN  =  "70"
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  ACCT-SYUTEN2
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  ACCT-SYUTEN2
                           END-IF
                           IF  WRK-RYOYOTEATE  NOT =  ZERO
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  WRK-RYOYOTEATE
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  WRK-RYOYOTEATE
                           END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                       END-IF
                   ELSE
                       IF ( SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64" OR
                                            "25"                     )
                       OR ((ACCT-ZAIKBN  =   2  ) AND
                           (SRY-SRYKBN   =  "12"))
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
      ***                  MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
      ***            MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                     PERFORM   2104421-RECE-LEFT-SET-SEC
               END-IF
               IF  ( SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "50"   OR   "54"   OR   "60"   OR
                                      "70"   OR   "64"     )
               OR  ((ACCT-ZAIKBN  =    2                  ) AND
                    (SRY-SRYKBN   =   "12" OR "13" OR "14"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
      *    処方の算定回数から減算
           COMPUTE    RECE100-KAISU(21)  =    RECE100-KAISU(21)
                                         -    WRK-SYOHOU-KAISU
      *    処方せんの算定回数から減算
           COMPUTE    RECE100-KAISU(36)  =    RECE100-KAISU(36)
                                         -    WRK-SYOSEN-KAISU
      *    麻毒の算定回数から処方料（麻・向・毒・覚）の減算
           COMPUTE    RECE100-KAISU(22)  =    RECE100-KAISU(22)
                                         -    WRK-MADOKU-SHOHO
      *
           .
      *
       210442-RECE100-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）
      *****************************************************************
       2104421-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   WRK-TNS-CHUKSNTUBAN(IDX3)   =   "0"
                   AND  SRY-SRYCD(IDX3)(1:1)        =   "1"
                        ADD   WK-ZAIKAISU  TO   RECE100-KAISU(1)
                   END-IF
      *
      *************１：時間外　２：深夜　３：休日　４：時間外・深夜
      *************５：時間外・休日　６：休日・深夜
      *************７：時間外・休日・深夜
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "1"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  CONTINUE
                               WHEN   "2"
                                  MOVE  "4"     TO   RECE100-SHOSHINTIME
                               WHEN   "3"
                                  MOVE  "5"     TO   RECE100-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    4
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "2"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "4"     TO   RECE100-SHOSHINTIME
                               WHEN   "2"
                                  CONTINUE
                               WHEN   "3"
                                  MOVE  "6"     TO   RECE100-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    3
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "3"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "5"     TO   RECE100-SHOSHINTIME
                               WHEN   "2"
                                  MOVE  "6"     TO   RECE100-SHOSHINTIME
                               WHEN   "3"
                                  CONTINUE
                               WHEN   "4"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                     END-EVALUATE
                   END-IF
      *
                   COMPUTE    RECE100-TENSU(1 1)  =
                              RECE100-TENSU(1 1)  +
                             (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                   WHEN  120
                     IF    RECE100-SRYYM  >= "201804"
                       PERFORM  21044211-RECE-LEFT12-HEN-SEC
                     ELSE
                       IF ( SRY-SRYCD(IDX3)      =   "112007170"  OR
                                                     "112702270"   )
                       OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"   )
                       OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1"   )
                           CONTINUE
                       ELSE
                           ADD   WK-ZAIKAISU    TO   RECE100-KAISU(2)
                           IF    SRY-SRYCD(IDX3)   =   "112015810"
                                                  OR   "112017010"
                                                  OR   "112015950"
                                                  OR   "112017150"
                                                  OR   "112016210"
                                                  OR   "112016410"
                                                  OR   "112017610"
                             ADD  WK-ZAIKAISU   TO   CNT-SAISIN-DOUJITU
                           ELSE
                             ADD  WK-ZAIKAISU   TO   CNT-SAISIN-ETC
                           END-IF
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112000970"
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   "112000970"
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112006970"
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   "112006970"
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112001070"
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   "112001070"
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112007070"
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   "112007070"
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112006270"
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   "112006270"
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112007270"
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   "112007270"
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112006370"
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   "112006370"
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   "112007370"
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   "112007370"
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF  SRY-SRYCD(IDX3)  =  "112000970"  OR
                                               "112006270"
                            COMPUTE  CNT-NYUYOJI  =  CNT-NYUYOJI
                                                  +  WK-ZAIKAISU
                            COMPUTE  CNT-NYUYOJI2 =  CNT-NYUYOJI2
                                                  +  WK-ZAIKAISU
                       END-IF
                       IF   RECE100-SRYYM  >= "201004"
                         IF  SRY-SRYCD(IDX3)  =  "112016070"
                            COMPUTE  CNT-JIKANTAI1 = CNT-JIKANTAI1
                                                   + WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  "112015670"
                            COMPUTE  CNT-KOKENKSN =  CNT-KOKENKSN
                                                  +  WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  "112016170"
                            COMPUTE  CNT-JIKANTAI3 = CNT-JIKANTAI3
                                                   + WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  "112015770"
                            COMPUTE  CNT-MESAIKSN =  CNT-MESAIKSN
                                                  +  WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  "112017270"
                            COMPUTE  CNT-CHIHOKATU = CNT-CHIHOKATU
                                                   + WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  "112017570"
                            COMPUTE  CNT-CHIHOKAT2 = CNT-CHIHOKAT2
                                                   + WK-ZAIKAISU
                         END-IF
                       END-IF
      **
                       IF ( SRY-SRYCD(IDX3)      =   "112007170"  OR
                                                     "112702270"   )
                       OR ( FLG-NYUYOJI          =   9             )
                       OR ( FLG-YOJI             =   9             )
                            MOVE   ZERO       TO    RECE100-KHNTEN(2)
                            MOVE   1          TO    FLG-KHNTEN2
                       ELSE
                         IF  SRY-SRYCD(IDX3)  =  "112000970"         OR
                                                 "112006970"         OR
                                                 "112001070"         OR
                                                 "112007070"         OR
                                                 "112006270"         OR
                                                 "112007270"         OR
                                                 "112006370"         OR
                                                 "112007370"
                            IF   FLG-YOJI-ARI  =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO
                                                     RECE100-KHNTEN(2)
                              MOVE     1       TO    FLG-YOJI-ARI
                            END-IF
                         ELSE
                          IF  (RECE100-SRYYM   >=  "201004"       )
                          AND (SRY-SRYCD(IDX3)  =  "112015670"    OR
                                                   "112015770"    OR
                                                   "112017270"    OR
                                                   "112017570"    OR
                                                   "112016070"    OR
                                                   "112016170"    )
                            EVALUATE  SRY-SRYCD(IDX3)
                              WHEN     "112016070"
                                IF   FLG-JIKANTAI1-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-JIKANTAI1-ARI
                                END-IF
                              WHEN     "112015670"
                                IF   FLG-KOKENKSN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-KOKENKSN-ARI
                                END-IF
                              WHEN     "112016170"
                                IF   FLG-JIKANTAI3-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-JIKANTAI3-ARI
                                END-IF
                              WHEN     "112015770"
                                IF   FLG-MESAIKSN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-MESAIKSN-ARI
                                END-IF
                              WHEN     "112017270"
                                IF   FLG-CHIHOKATU-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-CHIHOKATU-ARI
                                END-IF
                              WHEN     "112017570"
                                IF   FLG-CHIHOKAT2-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-CHIHOKAT2-ARI
                                END-IF
                            END-EVALUATE
                          ELSE
                            IF   FLG-SAISIN-ARI   =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO
                                                     RECE100-KHNTEN(2)
                              MOVE     1       TO    FLG-SAISIN-ARI
                            END-IF
                          END-IF
                         END-IF
                       END-IF
      **
                       COMPUTE  RECE100-TENSU(2 1) =
                                RECE100-TENSU(2 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
                     END-IF
      *      （外来管理加算）
                   WHEN  122
                        IF    RECE100-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO  RECE100-KHNTEN(3)
                        ELSE
                           IF RECE100-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE100-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE100-KAISU(3)
                        COMPUTE RECE100-TENSU(3 1) =
                                RECE100-TENSU(3 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      （時間外）
                   WHEN  123
                        IF    RECE100-KHNTEN(4)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(4)
                        ELSE
                          IF RECE100-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(4)
                             MOVE   1     TO  FLG-KHNTEN4
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(4)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(4 1) =
                                RECE100-TENSU(4 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      （休日）
                   WHEN  124
                        IF    RECE100-KHNTEN(5)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(5)
                        ELSE
                          IF RECE100-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(5)
                             MOVE   1     TO  FLG-KHNTEN5
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(5)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(5 1) =
                                RECE100-TENSU(5 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      （深夜）
                   WHEN  125
                        IF    RECE100-KHNTEN(6)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(6)
                        ELSE
                          IF RECE100-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(6)
                             MOVE   1     TO  FLG-KHNTEN6
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(6)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(6 1) =
                                RECE100-TENSU(6 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      （その他）
                   WHEN  OTHER 
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO       TO    RECE100-KHNTEN(2)
                           MOVE   1          TO    FLG-KHNTEN2
                           COMPUTE  WRK-RECE100G-TENSU(2 1) =
                                    WRK-RECE100G-TENSU(2 1) +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                 IF  ACCT-ZAIKBN   =    2
                  COMPUTE  WRK-RECE100G-TENSU(9 1)  =
                           WRK-RECE100G-TENSU(9 1)  +
                          (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                  EVALUATE   SRY-SRYCD(IDX3)
      *            公害疾患相談料
                   WHEN     "102130010"
                      ADD   WK-ZAIKAISU TO  RECE100-KAISU(7)
                      COMPUTE    RECE100-TENSU(7 2)  =
                                 RECE100-TENSU(7 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     1    TO    FLG-NEBU
                      END-IF
      *            公害外来療養指導料
                   WHEN     "102130020"
      ***             ADD   WK-ZAIKAISU TO  RECE100-KAISU(8)
                      COMPUTE    RECE100-TENSU(8 2)  =
                                 RECE100-TENSU(8 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     2    TO    FLG-NEBU
                      END-IF
      *            ネブライザー加算
                   WHEN     "102400010"
                      EVALUATE  FLG-NEBU
                        WHEN       1
                          COMPUTE  RECE100-TENSU(8 2)  =
                                   RECE100-TENSU(8 2)  +
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                          COMPUTE  RECE100-TENSU(7 2) =
                                   RECE100-TENSU(7 2) -
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                        WHEN       2
                          CONTINUE
                        WHEN       3
                          COMPUTE  RECE100-TENSU(8 2)  =
                                   RECE100-TENSU(8 2)  +
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                          COMPUTE  RECE100-TENSU(9 1) =
                                   RECE100-TENSU(9 1) -
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                      END-EVALUATE
                   WHEN      OTHER
                      ADD   WK-ZAIKAISU TO  RECE100-KAISU(9)
                      COMPUTE    RECE100-TENSU(9 1)  =
                                 RECE100-TENSU(9 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     3    TO    FLG-NEBU
                      END-IF
                  END-EVALUATE
                 END-IF
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE  WRK-RECE100G-TENSU(14 1)  =
                            WRK-RECE100G-TENSU(14 1)  +
                           (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(10)
                       COMPUTE   RECE100-TENSU(10 1)  =
                                 RECE100-TENSU(10 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ネブライザー加算
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(10 1) =
                                    RECE100-TENSU(10 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(11)
                       COMPUTE   RECE100-TENSU(11 1)  =
                                 RECE100-TENSU(11 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ネブライザー加算
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(11 1) =
                                    RECE100-TENSU(11 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(12)
                       COMPUTE   RECE100-TENSU(12 1)  =
                                 RECE100-TENSU(12 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ネブライザー加算
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(12 1) =
                                    RECE100-TENSU(12 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3)   =     1          )  AND
                       ((SRY-SRYCD(IDX3)     NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114029270")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114029370")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114029470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114029570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114029970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114030070")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114030170")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114030270")))
                   OR (  FLG-OSINKBN             =     9           )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(13)
                       COMPUTE   RECE100-TENSU(13 1)  =
                                 RECE100-TENSU(13 1)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
      *                ネブライザー加算
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(13 1) =
                                    RECE100-TENSU(13 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE100-TENSU(14 1)  =
                                 RECE100-TENSU(14 1)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
      *                ネブライザー加算
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(14 1) =
                                    RECE100-TENSU(14 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            薬剤
                   COMPUTE   RECE100-TENSU(15 2)  =
                             RECE100-TENSU(15 2)  +
                            (WK-KIZAITEN *   WK-ZAIKAISU)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010002"
                                            OR "630010005"
                                            OR "630010006"
                                            OR "630010007"
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU  TO   RECE100-KAISU(16)
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)  =    "099209901"
                           ADD   WK-ZAIKAISU  TO   RECE100-KAISU(16)
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       IF    RECE100-KHNTEN(17)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(17)
                       ELSE
                          IF   RECE100-KHNTEN(17)
                                          NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE100-KHNTEN(17)
                               MOVE   1       TO    FLG-KHNTEN17
                          END-IF
                       END-IF
                       ADD   WK-ZAIKAISU  TO   RECE100-KAISU(17)
                       COMPUTE   RECE100-TENSU(17 1)  =
                                 RECE100-TENSU(17 1)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                     END-IF
                   END-IF
               WHEN    "22"
                 IF    SRY-SRYCD(IDX3)      =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                   COMPUTE   RECE100-TENSU(18 2)  =
                             RECE100-TENSU(18 2)  -
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   ADD    WK-ZAIKAISU    TO   RECE100-KAISU(18)
                   COMPUTE   RECE100-TENSU(18 2)  =
                             RECE100-TENSU(18 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF    SRY-SRYCD(IDX3)  =  "630010005"
                                            OR "630010006"
                                            OR "630010007"
                       COMPUTE   RECE100-TENSU(19 2)  =
                                 RECE100-TENSU(19 2)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(19)
                       COMPUTE   RECE100-TENSU(19 2)  =
                                 RECE100-TENSU(19 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                       IF    RECE100-KHNTEN(20)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO  RECE100-KHNTEN(20)
                       ELSE
                          IF   RECE100-KHNTEN(20)
                                              NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE100-KHNTEN(20)
                               MOVE   1       TO    FLG-KHNTEN20
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(20)
                       COMPUTE   RECE100-TENSU(20 1)  =
                                 RECE100-TENSU(20 1)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF  SRY-SRYCD(IDX3) = "120002170" OR "120002270" OR
                                         "120003170" OR "120003370" OR
                                         "120003870" OR
                                         "120004070" OR "120004170" OR
                                         "120004370" OR "120004470"
                      COMPUTE  WRK-SYOHOU-KAISU  =  WRK-SYOHOU-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE100-KHNTEN(21)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE100-KHNTEN(21)
                     ELSE
                      IF   RECE100-KHNTEN(21) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE100-KHNTEN(21)
                            MOVE   1      TO   FLG-KHNTEN21
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    ACCT-ZAIKAISU   TO    RECE100-KAISU(21)
                   END-IF
                   COMPUTE   RECE100-TENSU(21 1)  =
                             RECE100-TENSU(21 1)  +
                            (WK-ZAITEN   *   ACCT-ZAIKAISU)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE100-KAISU(22)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE100-TENSU(22 1)  =
                             RECE100-TENSU(22 1)  +
                            (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
      *    調基のレセ点数欄の編集
               WHEN    "27"
      ***          IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
      ***          OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
      ***          OR  (SRY-SRYCD(IDX3)        = "099999903")
      ***             CONTINUE
      ***          ELSE
      ***             ADD    WK-ZAIKAISU     TO    RECE100-KAISU(23)
      ***          END-IF
                   COMPUTE   RECE100-TENSU(23 1)  =
                             RECE100-TENSU(23 1)  +
                            (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(24)
                   COMPUTE   RECE100-TENSU(24 1)  =
                             RECE100-TENSU(24 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(25)
                   COMPUTE   RECE100-TENSU(25 1)  =
                             RECE100-TENSU(25 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(26)
                   COMPUTE   RECE100-TENSU(26 1)  =
                             RECE100-TENSU(26 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(28)
                   END-IF
                   COMPUTE   RECE100-TENSU(28 1)  =
                             RECE100-TENSU(28 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(29 2)  =
                             RECE100-TENSU(29 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *
      *            ネブライザー加算
                   IF    SRY-SRYCD(IDX3)  =  "102400010"
                      COMPUTE    RECE100-TENSU(29 2)  =
                                 RECE100-TENSU(29 2)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                      COMPUTE    RECE100-TENSU(28 1)  =
                                 RECE100-TENSU(28 1)  -
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                   END-IF
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(30)
                   END-IF
                   COMPUTE   RECE100-TENSU(30 1)  =
                             RECE100-TENSU(30 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(31 2)  =
                             RECE100-TENSU(31 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(32)
                   END-IF
                   COMPUTE   RECE100-TENSU(32 1)  =
                             RECE100-TENSU(32 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE100-KAISU(32)  =
                                   RECE100-KAISU(32)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   "160101210"     OR
                                                  "160095710"     OR
                                                  "160095810"
                         IF   RECE100-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE100-KAISU(32)  =
                                        RECE100-KAISU(32)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE100-TENSU(33 2)  =
                             RECE100-TENSU(33 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(34)
                   END-IF
                   COMPUTE   RECE100-TENSU(34 1)  =
                             RECE100-TENSU(34 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(35 2)  =
                             RECE100-TENSU(35 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF  SRY-SRYCD(IDX3) =  "120002470"
                                       OR "120002570"
                                       OR "120003270"
                                       OR "120003470"
                                       OR "120003570"
                                       OR "120003970"
                                       OR "120004270"
                                       OR "120004570"
                      COMPUTE  WRK-SYOSEN-KAISU  =  WRK-SYOSEN-KAISU
                                                 +  ACCT-ZAIKAISU
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120002710"    OR
                                                "120002810"    OR
                                                "120002910"    OR
                                                "120003010"    OR
                                                "120003710"    OR
                                                "120002470"    OR
                                                "120002570"    OR
                                                "120003270"    OR
                                                "120003470"    OR
                                                "120003570"    OR
                                                "120003970"    OR
                                                "120004270"    OR
                                                "120004570"    OR
                                                "120004610"
                           ADD  ACCT-ZAIKAISU  TO  RECE100-KAISU(36)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120002710"    OR
                                                "120002810"    OR
                                                "120002910"    OR
                                                "120003010"    OR
                                                "120003710"    OR
                                                "120002470"    OR
                                                "120002570"    OR
                                                "120003270"    OR
                                                "120003470"    OR
                                                "120003570"    OR
                                                "120003970"    OR
                                                "120004270"    OR
                                                "120004570"    OR
                                                "120004610"
                       COMPUTE   RECE100-TENSU(36 1)  =
                                 RECE100-TENSU(36 1)  +
                                (WK-ZAITEN  *   ACCT-ZAIKAISU)
                   ELSE
                     COMPUTE     RECE100-TENSU(37 1)  =
                                 RECE100-TENSU(37 1)  +
                                (WK-SYUTEN  *   ACCT-ZAIKAISU)
                     IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                       COMPUTE   RECE100-TENSU(9 1)   =
                                 RECE100-TENSU(9 1)   +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       COMPUTE   RECE100-TENSU(37 1)  =
                                 RECE100-TENSU(37 1)  -
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                     END-IF
                   END-IF
                   COMPUTE   RECE100-TENSU(38 2)  =
                             RECE100-TENSU(38 2)  +
                            (WK-YKZTEN   *   ACCT-ZAIKAISU)
           END-EVALUATE
           .
      *
       2104421-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *
      *****************************************************************
       21044211-RECE-LEFT12-HEN-SEC               SECTION.
      *
           IF ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"   )
           OR ( SRY-SRYCD(IDX3)(1:1)       NOT = "1"   )
           OR ( SRY-SRYCD(IDX3) = "112021970"          )
             CONTINUE
           ELSE
             ADD   WK-ZAIKAISU      TO   RECE100-KAISU(2)
             IF    SRY-SRYCD(IDX3)   =   "112015810"
                                    OR   "112017010"
                                    OR   "112015950"
                                    OR   "112017150"
                                    OR   "112023550"
                                    OR   "112023850"
                                    OR   "112016210"
                                    OR   "112016410"
                                    OR   "112017610"
                 ADD  WK-ZAIKAISU   TO   CNT-SAISIN-DOUJITU
             ELSE
               IF  SRY-SRYCD(IDX3)   =   "112023210"
                 ADD  WK-ZAIKAISU   TO   CNT-SAISIN-ONLINE
               ELSE
                 ADD  WK-ZAIKAISU   TO   CNT-SAISIN-ETC
               END-IF
             END-IF
           END-IF
      *
           IF  SRY-SRYCD(IDX3)  =  "112000970"
                               OR  "112006270"
               COMPUTE  CNT-NYUYOJI   =  CNT-NYUYOJI
                                      +  WK-ZAIKAISU
               COMPUTE  CNT-NYUYOJI2  =  CNT-NYUYOJI2
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112016070"
               COMPUTE  CNT-JIKANTAI1 =  CNT-JIKANTAI1
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112015670"
               COMPUTE  CNT-KOKENKSN  =  CNT-KOKENKSN
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112016170"
               COMPUTE  CNT-JIKANTAI3 =  CNT-JIKANTAI3
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112015770"
               COMPUTE  CNT-MESAIKSN  =  CNT-MESAIKSN
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112017270"
               COMPUTE  CNT-CHIHOKATU =  CNT-CHIHOKATU
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112017570"
               COMPUTE  CNT-CHIHOKAT2 =  CNT-CHIHOKAT2
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112021770"
               COMPUTE  CNT-CHIHOKAT3 =  CNT-CHIHOKAT3
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112021870"
               COMPUTE  CNT-CHIHOKAT4 =  CNT-CHIHOKAT4
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112021970"
               COMPUTE  CNT-YAKRENKEI =  CNT-YAKRENKEI
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112022070"
               COMPUTE  CNT-NINPU     =  CNT-NINPU
                                      +  WK-ZAIKAISU
           END-IF
           IF  SRY-SRYCD(IDX3)  =  "112022870"
               COMPUTE  CNT-NINPU2    =  CNT-NINPU2
                                      +  WK-ZAIKAISU
           END-IF
      *
           IF  SRY-SRYCD(IDX3)  =  "112000970"
                               OR  "112006270"
              IF   FLG-YOJI-ARI  =  ZERO
                ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                MOVE     1             TO FLG-YOJI-ARI
              END-IF
           ELSE
              IF   SRY-SRYCD(IDX3)  =  "112015670"
                                   OR  "112015770"
                                   OR  "112017270"
                                   OR  "112017570"
                                   OR  "112016070"
                                   OR  "112016170"
                                   OR  "112021770"
                                   OR  "112021870"
                                   OR  "112021970"
                                   OR  "112022070"
                                   OR  "112022870"
                EVALUATE  SRY-SRYCD(IDX3)
                  WHEN     "112016070"
                    IF   FLG-JIKANTAI1-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-JIKANTAI1-ARI
                    END-IF
                  WHEN     "112015670"
                    IF   FLG-KOKENKSN-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-KOKENKSN-ARI
                    END-IF
                  WHEN     "112016170"
                    IF   FLG-JIKANTAI3-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-JIKANTAI3-ARI
                    END-IF
                  WHEN     "112015770"
                    IF   FLG-MESAIKSN-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-MESAIKSN-ARI
                    END-IF
                  WHEN     "112017270"
                    IF   FLG-CHIHOKATU-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKATU-ARI
                    END-IF
                  WHEN     "112017570"
                    IF   FLG-CHIHOKAT2-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT2-ARI
                    END-IF
                  WHEN     "112021770"
                    IF   FLG-CHIHOKAT3-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT3-ARI
                    END-IF
                  WHEN     "112021870"
                    IF   FLG-CHIHOKAT4-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-CHIHOKAT4-ARI
                    END-IF
                  WHEN     "112021970"
                    IF   FLG-YAKRENKEI-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-YAKRENKEI-ARI
                    END-IF
                  WHEN     "112022070"
                    IF   FLG-NINPU-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-NINPU-ARI
                    END-IF
                  WHEN     "112022870"
                    IF   FLG-NINPU2-ARI  =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-NINPU2-ARI
                    END-IF
                END-EVALUATE
              ELSE
                IF   FLG-SAISIN-ARI   =  ZERO
                      ADD  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(2)
                      MOVE     1             TO FLG-SAISIN-ARI
                END-IF
              END-IF
           END-IF
      *
           COMPUTE  RECE100-TENSU(2 1) =
                    RECE100-TENSU(2 1) +
                   (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *
           .
      *
       21044211-RECE-LEFT12-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療情報連携編集
      *****************************************************************
       210443-IRYORENKEI-HENSYU-SEC               SECTION.
      *
           IF   (ACCT-SRYKBN       =  "14" OR "21" OR "22" OR "23")
           AND  (ACCT-ZAITEN       =  ZERO                        )
           AND  (ACCT-ZAIKBN   NOT =   1                          )
           AND  (ACCT-JIHOKBN      =  SPACE                       )
      *
             INITIALIZE                   SRYACT-REC
             MOVE    ACCT-HOSPNUM     TO  SRY-HOSPNUM
             MOVE    ACCT-NYUGAIKBN   TO  SRY-NYUGAIKBN
             MOVE    ACCT-PTID        TO  SRY-PTID
             MOVE    ACCT-SRYYM       TO  SRY-SRYYM
             MOVE    ACCT-ZAINUM      TO  SRY-ZAINUM
             MOVE    "820000046"      TO  SRY-SRYCD(1)
             MOVE    "820000046"      TO  SRY-SRYCD(2)
             MOVE    "820000046"      TO  SRY-SRYCD(3)
             MOVE    "820000046"      TO  SRY-SRYCD(4)
             MOVE    "820000046"      TO  SRY-SRYCD(5)
             MOVE    SRYACT-REC       TO  MCPDATA-REC
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key8"           TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC   =   ZERO
               MOVE    "tbl_sryact"   TO  MCP-TABLE
               MOVE    "key8"         TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               IF   FLG-SRYACT  =  ZERO
                 CONTINUE
               ELSE
                 MOVE    1            TO  FLG-IRYORENKEI
               END-IF
             END-IF
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key8"           TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
           IF    FLG-IRYORENKEI   =   1
      *
             INITIALIZE                      ORCSGETGENERICAREA
             MOVE    RECE100-HOSPNUM     TO  ORCSGETGE-HOSPNUM
             MOVE    RECE100-PTID        TO  ORCSGETGE-PTID
             MOVE    RECE100-SRYYM       TO  ORCSGETGE-SRYYM
             MOVE    "2"                 TO  ORCSGETGE-NYUGAIKBN
             MOVE    ACCT-ZAINUM         TO  ORCSGETGE-ZAINUM
             MOVE    WRK-1030-KOUHATUKA  TO  ORCSGETGE-KOUHATUKA
             CALL    "ORCSGETGENERIC"    USING
                                         ORCSGETGENERICAREA
                                         SPA-AREA
             IF      ORCSGETGE-ERRCD     =   ZERO
               PERFORM   VARYING  IDX15    FROM    1   BY  1
                          UNTIL   IDX15     >     50
                 MOVE  ORCSGETGE-YAKKAKJNCD(IDX15)
                            TO  WRK-ORCSGETGE-YAKKAKJNCD(IDX15)
                 MOVE  ORCSGETGE-GENERIC-KBN(IDX15)
                            TO  WRK-ORCSGETGE-GENERIC-KBN(IDX15)
                 MOVE  ORCSGETGE-GENERICNAME(IDX15)
                            TO  WRK-ORCSGETGE-GENERICNAME(IDX15)
               END-PERFORM
             END-IF
      *
           END-IF
      *
           .
      *
       210443-IRYORENKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬チェック
      *****************************************************************
       210444-SIPPU-CHECK-SEC               SECTION.
      *
           INITIALIZE                     SRYACT-REC
           MOVE    ACCT-HOSPNUM       TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO  SRY-PTID
           MOVE    ACCT-SRYKA         TO  SRY-SRYKA
           MOVE    ACCT-SRYYM         TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO  SRY-ZAINUM
           MOVE    SRYACT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_sryact"     TO  MCP-TABLE
             MOVE    "key2"           TO  MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             PERFORM  UNTIL (FLG-SRYACT     =  1)
                       OR   (FLG-SIPPU-ARI  =  1)
      *
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                         UNTIL  (IDX2            >   5   )
                          OR    (SRY-SRYCD(IDX2) =  SPACE)
                          OR    (FLG-SIPPU-ARI   =   1   )
                  PERFORM    900-TENSU-TBL-SEC
                  IF  (SRY-SRYCD(IDX2)(1:1)  =  "6")
                  AND (TNS-MEIUSESKB         =   1 )
                     MOVE    1        TO  FLG-SIPPU-ARI
                  END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"   TO  MCP-TABLE
               MOVE    "key2"         TO  MCP-PATHNAME
               PERFORM   900-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       210444-SIPPU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤の診療内容チェック
      *****************************************************************
       21046-SINRYO-CHECK-SEC                 SECTION.
      *
      ***  PERFORM   VARYING   IDU   FROM   1   BY   1
           PERFORM   VARYING   IDU   FROM   2   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                   SRYACT-REC
grpsys           MOVE  RECE100-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE  RECE100-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE  RECE100-PTID        TO  SRY-PTID
                 MOVE  RECE100-SRYYM       TO  SRY-SRYYM
                 MOVE  ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE  SRYACT-REC         TO  MCPDATA-REC
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC               =       ZERO
                     MOVE    "tbl_sryact"         TO  MCP-TABLE
                     MOVE    "key8"               TO  MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =  ZERO
                         EVALUATE   IDU
                           WHEN      1
                             MOVE   1   TO  FLG-RINYAK
                           WHEN      2
                           WHEN      3
                           WHEN      4
                             MOVE   2   TO  FLG-RINYAK
                         END-EVALUATE
                     END-IF
                 END-IF
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21046-SINRYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括点数の治験減点分編集
      *****************************************************************
       2105-GENTEN-HENSYU-SEC      SECTION.
      *
           MOVE     ZERO    TO    WRK-TOTALTEN1
           MOVE     ZERO    TO    WRK-TOTALTEN2
           PERFORM VARYING ACCT-IDX    FROM    1   BY  1
                   UNTIL   ACCT-IDX    >       38
               ADD    RECE100-TENSU(ACCT-IDX 1)  TO  WRK-TOTALTEN1
               ADD    RECE100-TENSU(ACCT-IDX 2)  TO  WRK-TOTALTEN2
           END-PERFORM
           COMPUTE   WRK-TOTALTEN1  =  WRK-TOTALTEN1
                                    -  WRK-RECE100G-TENSU(2 1)
                                    -  WRK-RECE100G-TENSU(9 1)
                                    -  WRK-RECE100G-TENSU(14 1)
      *
      *    再診のレセ点数の編集（減点）
           IF   RECE100-TENSU(2 1)   >=  WRK-RECE100G-TENSU(2 1)
             COMPUTE  RECE100-TENSU(2 1)  =
                      RECE100-TENSU(2 1)  - WRK-RECE100G-TENSU(2 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(2 1)
           END-IF
      *
      *    指導のレセ点数の編集（減点）
           IF   RECE100-TENSU(9 1)   >=  WRK-RECE100G-TENSU(9 1)
             COMPUTE  RECE100-TENSU(9 1)  =
                      RECE100-TENSU(9 1)  - WRK-RECE100G-TENSU(9 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(9 1)
           END-IF
      *
      *    在宅のレセ点数の編集（減点）
           IF   RECE100-TENSU(14 1)  >=  WRK-RECE100G-TENSU(14 1)
             COMPUTE  RECE100-TENSU(14 1) =
                      RECE100-TENSU(14 1) - WRK-RECE100G-TENSU(14 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(14 1)
           END-IF
      *
           .
       2105-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数編集処理
      *****************************************************************
       2106-JNISSU-SET-SEC                    SECTION.                       
      *
           PERFORM VARYING ACCT-IDX    FROM    1   BY  1
                   UNTIL   ACCT-IDX    >       31
      *        実日数から減する
               IF    ( TBL-JNISSU-DAY     (ACCT-IDX)   NOT =   SPACE )
               AND   ( TBL-JNISSU-DAY-GEN (ACCT-IDX)   NOT =   SPACE )
                    MOVE  SPACE        TO  TBL-JNISSU-DAY (ACCT-IDX)
               END-IF
      *
               IF    TBL-JNISSU-DAY (ACCT-IDX)
                                       NOT =  SPACE
      *            実日数編集
                   ADD     1           TO  RECE100-JNISSU
      *            実日数テーブル編集
                   MOVE    "1"         TO  RECE100-JNISSUTBL(ACCT-IDX)
               END-IF
           END-PERFORM
      *
      *    合計点数集計
      ***  PERFORM VARYING ACCT-IDX    FROM    1   BY  1
      ***          UNTIL   ACCT-IDX    >       38
      ***      ADD    RECE100-TENSU(ACCT-IDX 1)  TO
      ***                                    RECE100-TENSU(41 1)
      ***      ADD    RECE100-TENSU(ACCT-IDX 2)  TO
      ***                                    RECE100-TENSU(41 2)
      ***  END-PERFORM
      *
           MOVE       WRK-TOTALTEN1     TO    RECE100-TENSU(41 1)
           MOVE       WRK-TOTALTEN2     TO    RECE100-TENSU(41 2)
      *
      *    合計（１５円×手技点）
           COMPUTE    RECE100-TOTALTEN(1)  =  RECE100-TENSU(41 1)
                                           *  15
      *    合計（１０円×薬剤等点）
           COMPUTE    RECE100-TOTALTEN(2)  =  RECE100-TENSU(41 2)
                                           *  10
      *    総合計
           COMPUTE    RECE100-TOTALTEN(3)  =  RECE100-TOTALTEN(1)
                                           +  RECE100-TOTALTEN(2)
           .
       2106-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの基本点数部分のクリア処理（Ｆ００２）
      *****************************************************************
       2107-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN17        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(17)
           END-IF
           IF        FLG-KHNTEN20        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(20)
           END-IF
           IF        FLG-KHNTEN21        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(21)
           END-IF
      *
           .
       2107-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳幼児・幼児のコメントフラグセット
      *****************************************************************
       2110-NYUYOJI-COMENTFLG-SET-SEC               SECTION.
      *
           IF   RECE100-SRYYM  >= "200604"
             IF   ((CNT-SAISIN-ETC      NOT =  ZERO) AND
                   (CNT-SAISIN-DOUJITU  NOT =  ZERO))
             OR   ((CNT-SAISIN-ETC      NOT =  ZERO) AND
                   (CNT-SAISIN-ONLINE   NOT =  ZERO))
             OR   ((CNT-SAISIN-DOUJITU  NOT =  ZERO) AND
                   (CNT-SAISIN-ONLINE   NOT =  ZERO))
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
             END-IF
             IF    (CNT-NYUYOJI         NOT =  ZERO        )
             AND   (CNT-SAISIN-ETC      NOT =  CNT-NYUYOJI )
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
             END-IF
             IF     RECE100-NYUYOJIFLG      =  SPACE
               IF  (CNT-NYUYOJI2        NOT =  ZERO        )
               AND (CNT-SAISIN-ETC      NOT =  CNT-NYUYOJI2)
                     MOVE     "1"        TO   RECE100-NYUYOJIFLG
               END-IF
             END-IF
             IF   RECE100-SRYYM  >= "201004"
               IF    (CNT-JIKANTAI1    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI1)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-KOKENKSN     NOT =  ZERO        )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-KOKENKSN)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-JIKANTAI3    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI3)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-MESAIKSN     NOT =  ZERO        )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-MESAIKSN)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKATU    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKATU)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT2    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT2)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT3    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT3)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-CHIHOKAT4    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-CHIHOKAT4)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-YAKRENKEI    NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-YAKRENKEI)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-NINPU        NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-NINPU    )
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-NINPU2       NOT =  ZERO         )
               AND   (CNT-SAISIN-ETC   NOT =  CNT-NINPU2   )
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
             END-IF
           END-IF
      *
           .
       2110-NYUYOJI-COMENTFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角→数値半角変換処理
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-STIDX
                                           WRK-EDIDX 
           MOVE    SPACE               TO  WRK-NUM
           MOVE    ZERO                TO  WRK-NUM-9
      *
      *    空白のときはゼロ 
           IF      WRK-ZENKAKU          =       SPACE
               MOVE    ZERO                TO  WRK-NUM
               MOVE    10                  TO  IDY   
           ELSE
      *
      *        文字の開始桁
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   80
                       OR      WRK-STIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-STIDX
                   END-IF     
               END-PERFORM
      *        文字の終了桁
               PERFORM VARYING IDX FROM    80  BY  -1
                       UNTIL   IDX         <   1
                       OR      WRK-EDIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-EDIDX
                   END-IF     
               END-PERFORM
      *
      *        全角数字のとき半角数字に変換（数字以外のときはゼロ）
               IF      WRK-ZENKAKU (WRK-STIDX:1)   >=  X"A1"
                   MOVE    ZERO                TO  IDX3
                   PERFORM VARYING     IDX     FROM    WRK-STIDX  BY  2
                           UNTIL       IDX     >       WRK-EDIDX
                       IF      WRK-ZENKAKU (IDX:2)  >= "０"  AND
                                                   <=  "９"
                           ADD     1               TO  IDX3
                           EVALUATE    WRK-ZENKAKU (IDX:2)
                               WHEN    "０"
                                   MOVE    "0"     TO  WRK-NUM (IDX3:1)
                               WHEN    "１"
                                   MOVE    "1"     TO  WRK-NUM (IDX3:1)
                               WHEN    "２"
                                   MOVE    "2"     TO  WRK-NUM (IDX3:1)
                               WHEN    "３"
                                   MOVE    "3"     TO  WRK-NUM (IDX3:1)
                               WHEN    "４"
                                   MOVE    "4"     TO  WRK-NUM (IDX3:1)
                               WHEN    "５"
                                   MOVE    "5"     TO  WRK-NUM (IDX3:1)
                               WHEN    "６"
                                   MOVE    "6"     TO  WRK-NUM (IDX3:1)
                               WHEN    "７"
                                   MOVE    "7"     TO  WRK-NUM (IDX3:1)
                               WHEN    "８"
                                   MOVE    "8"     TO  WRK-NUM (IDX3:1)
                               WHEN    "９"
                                   MOVE    "9"     TO  WRK-NUM (IDX3:1)
                           END-EVALUATE
      *****************ELSE
      *****************    MOVE    ZERO            TO  WRK-NUM
      *****************    MOVE    WRK-EDIDX       TO  IDX
      *****************    MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        半角数字に変換（数字以外のときはゼロ）
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)     NOT NUMERIC
                       PERFORM VARYING IDX     FROM    WRK-STIDX  BY  1
                               UNTIL   IDX     >       WRK-EDIDX
                           IF      WRK-ZENKAKU (IDX:1) >=  "0" AND
                                                       <=  "9"
                               ADD     1           TO  IDX3
                               MOVE    WRK-ZENKAKU (IDX:1)
                                                   TO  WRK-NUM (IDX3:1)
                           END-IF
                       END-PERFORM    
                       MOVE    IDX3                TO  IDY     
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-NUM-X           TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM         TO  WRK-NUM-9
           ELSE
               MOVE    ZERO                TO  WRK-NUM-9 
           END-IF
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE    ZERO                 TO  FLG-TENSU
           MOVE    SRY-SRYCD(IDX2)      TO  TTNS-CHK-SRYCD
           MOVE    SRY-SRYYM            TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           MOVE    WK-TAIHI-SRYYMD      TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                           TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM          TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(IDX2)      TO  TNS-SRYCD
               MOVE    WK-TAIHI-SRYYMD      TO  TNS-YUKOSTYMD
                                                    TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC        TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM  910-TENSU-READ-SEC
                   IF       FLG-TENSU       =       1
                       DISPLAY   "TENSU FETCH ERR"  
                   ELSE
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   END-IF
               ELSE
                   DISPLAY   "TENSU SELECT ERR" 
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT(TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF      SRY-SRYCD(IDX2)(1:1) = "6" OR "7"
               INITIALIZE                   TNSUP-KEY
               MOVE    WK-TAIHI-SRYYMD  TO  TNSUP-YUKOSTYMD
                                            TNSUP-YUKOEDYMD
               CALL    "ORCSSETTANI"    USING
                                        TNS-TENSU-REC
                                        SPA-AREA
           END-IF
      *
           IF      TNS-TANIMOJISU    =   ZERO
           AND     TNS-TANINAME  NOT =   SPACE
               PERFORM   VARYING   IDW2   FROM   24   BY  -1
                          UNTIL    IDW2    <      1
                  IF   TNS-TANINAME(IDW2:1)  NOT =   SPACE
                      COMPUTE  TNS-TANIMOJISU   =   IDW2   /   2
                      MOVE    1     TO    IDW2
                  END-IF
               END-PERFORM
           END-IF
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD(TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD(TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD(TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT(TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   GO  TO  900-SRYACCT-READ-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-SRYACCT
               END-IF
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      * 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       970-KANRIMST-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       970-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT   NOT =   SPACE
                   MOVE    ZERO            TO  WRK-COL
                                               IDZ1
                                               IDZ2 
                   INSPECT PTCOM-INPUTCOMENT   TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       DISPLAY "PTCOM=" PTCOM-KEY
                       PERFORM VARYING IDZ1    FROM    1   BY  1
                               UNTIL   IDZ1    >       140
                           IF      PTCOM-INPUTCOMENT(IDZ1:1)
                                               =   WRK-CONS-RETURN
                               COMPUTE IDZ2    =   IDZ1    -   1
                               MOVE    140     TO  IDZ1
                           END-IF
                       END-PERFORM
      *
                       CALL    "coblog" USING   "ptcom returncd err"
                       MOVE    SPACE               TO  WRK-RECEERR
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING
                        "コメント文字コード不正　患者番号="
                                                   DELIMITED  BY  SIZE
                               RECE100-PTNUM       DELIMITED  BY  SPACE
                               "　診療区分="       DELIMITED  BY  SIZE
                               SRY-SRYKBN          DELIMITED  BY  SPACE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       IF      IDZ2        >   1    
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                   "　コメント="   DELIMITED  BY  SIZE
                                   PTCOM-INPUTCOMENT(1:IDZ2)
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       ELSE
                           STRING   WRK-ERRMSG     DELIMITED  BY  SPACE
                                    "　コメント="  DELIMITED  BY  SIZE
                                    PTCOM-SRYCD    DELIMITED  BY  SPACE
                                                   INTO    WRK-RECEERR
                           END-STRING
                       END-IF
                       PERFORM 500-ERR-HENSYU-SEC
      *
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING  "ORCR1010 "         DELIMITED  BY  SIZE
                               WRK-RECEERR         DELIMITED  BY  SIZE
                               LOW-VALUE           DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       PERFORM 500-COBABORT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE100-READ-SEC            SECTION.
      *
           READ    RECE100-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE100
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE100 PTNUM=" RECE100-PTNUM     
      *
                   IF      WRK-PARA-SRYYM     =  ZERO
      *                 医療機関編集
                       MOVE  RECE100-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF
           END-READ
           .
       RECE100-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    施設基準情報
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                     UNTIL ( SYSKAN-IDX2               >  100  )
                      OR   ( TBL-SYS10062(SYSKAN-IDX2) = SPACE )
                      OR   ( FLG-SET                   =   1   )
               MOVE    TBL-SYS10062(SYSKAN-IDX2)  TO  SYS-1006-REC
               IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET    =   ZERO
               MOVE  ZERO                    TO  WRK-SISETU613
               MOVE  ZERO                    TO  WRK-SISETU695
           ELSE
               MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
               MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
           END-IF
      *
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                     UNTIL ( SYSKAN-IDX2               >  100  )
                      OR   ( TBL-SYS10067(SYSKAN-IDX2) = SPACE )
                      OR   ( FLG-SET                   =   1   )
               MOVE    TBL-SYS10067(SYSKAN-IDX2)  TO  SYS-1006-REC
               IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET    =   ZERO
               MOVE  ZERO                    TO  WRK-SISETU3055
               MOVE  ZERO                    TO  WRK-SISETU3056
               MOVE  ZERO                    TO  WRK-SISETU3057
               MOVE  ZERO                    TO  WRK-SISETU3058
               MOVE  ZERO                    TO  WRK-SISETU3168
               MOVE  ZERO                    TO  WRK-SISETU3169
           ELSE
               MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
               MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
               MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
               MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
               MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
               MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
           END-IF
      *
      *    帳票編集区分情報
           MOVE    ZERO                TO  FLG-SET
           MOVE    SPACE               TO  WRK-1030-KOUHATUKA
      *
      **>  IF      WRK-PARA-PRTKBN = "2" OR "3" OR "4" OR "5"
             PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                     UNTIL ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1030(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1030(SYSKAN-IDX) TO  SYS-1030-REC
               IF    ( SYS-1030-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1030-EDYUKYMD )
                   MOVE     1            TO  FLG-SET
                   MOVE    SYS-1030-KOUHATUKA
                                         TO  WRK-1030-KOUHATUKA
               END-IF
             END-PERFORM
      **>  END-IF
      *
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       RECE100-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE100-REC
      *
           ADD     1                   TO  CNT-OUT3
           .
       RECE100-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００４）
      *****************************************************************
       RECE120-WRITE-SEC                SECTION.
      *
           MOVE    RECE120-ZAITEN      TO  RECE120-HOKATU-ZAITEN
           MOVE    RECE120-SYUTEN1     TO  RECE120-HOKATU-SYUTEN1
           MOVE    RECE120-SYUTEN2     TO  RECE120-HOKATU-SYUTEN2
           MOVE    RECE120-YKZTEN      TO  RECE120-HOKATU-YKZTEN
           MOVE    RECE120-KIZAITEN    TO  RECE120-HOKATU-KIZAITEN
      *
           WRITE   RECE120-REC
      *
           ADD     1                   TO  CNT-OUT1
           .
       RECE120-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE130-WRITE-SEC                SECTION.
      *
           WRITE   RECE130-REC
      *
           ADD     1                   TO  CNT-OUT2
      *
           .
       RECE130-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE130-REWRITE-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE130-REC
              MOVE    RECE120-KEY1         TO  RECE130-KEY1
              MOVE    RECE120-ZAINUM       TO  RECE130-ZAINUM
              START   RECE130-FILE
                    KEY IS    >=   RECE130-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE130
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE130
                        READ    RECE130-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE130
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE120-KEY1         NOT =  RECE130-KEY1  ) OR
                  (RECE120-ZAINUM       NOT =  RECE130-ZAINUM) OR
                  (FLG-RECE130              =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE130-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE130-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE130-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE130-REC
                   END-IF
                   READ    RECE130-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE130
                   END-READ
              END-PERFORM
           .
       RECE130-REWRITE-EXT.
           EXIT.                                                        
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE130-REWRITE2-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE130-REC
              MOVE    RECE120-KEY1         TO  RECE130-KEY1
              MOVE    RECE120-ZAINUM       TO  RECE130-ZAINUM
              START   RECE130-FILE
                    KEY IS    >=   RECE130-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE130
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE130
                        READ    RECE130-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE130
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE120-KEY1         NOT =  RECE130-KEY1  ) OR
                  (RECE120-ZAINUM       NOT =  RECE130-ZAINUM) OR
                  (FLG-RECE130              =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE130-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE130-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE130-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE130-REC
                   END-IF
                   READ    RECE130-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE130
                   END-READ
              END-PERFORM
           .
       RECE130-REWRITE2-EXT.
           EXIT.                                                        
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
             IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
             END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE100  CNT-IN  :" CNT-IN 
           DISPLAY "RECE120  CNT-OUT :" CNT-OUT1  
           DISPLAY "RECE130  CNT-OUT :" CNT-OUT2  
           DISPLAY "*** ORCR1010 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
