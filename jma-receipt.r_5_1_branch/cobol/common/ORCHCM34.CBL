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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHCM34.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : オンライン帳票
      *  コンポーネント名  : 会計カード
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/15    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.09.19  自院歴より継続して入院している
      *                                     場合の入院日を修正
      *  01.00.02    NACL-太田    04.01.16  包括分の考慮を追加
      *  01.00.03    NACL-太田    04.01.19  室料差額消費税区分の考慮を追加
      *  01.00.04    NACL-太田    04.02.19  減算時の黒三角編集漏れを修正
      *  01.00.05    NACL-太田    04.04.01  褥瘡患者管理加算の対応
      *  01.00.06    NACL-門脇    06.06.02  褥瘡患者管理加算編集対応
      *  01.00.07    NACL-門脇    06.06.23  認知機能障害加算の編集対応
      *  01.00.08    NACL-小豆沢  06.07.04  労災入院料に含める加算がコメント
      *                                     と同時入力された場合の対応
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  03.05.01    NACL-門脇    07/10/24  自賠責特定器材等消費税対象外設定
      *                                     項目数拡張対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療会計ファイル
           SELECT  HCM34ACCT-FILE  ASSIGN   HCM34F01
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-HCM34F01.
      *
      *    診療行為ファイル
           SELECT  HCM34SRY-FILE   ASSIGN   HCM34F02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  HCM34SRY-ISM-KEY
                                   FILE    STATUS  IS  STS-HCM34F02.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療会計ファイル
       FD  HCM34ACCT-FILE.
       01  HCM34ACCT-REC.
         02  HCM34ACCT-DAT.
           COPY    "CPSRYACCT.INC"         REPLACING   //ACCT-//
                                           BY          //HCM34ACCT-//.
           03  HCM34ACCT-DATPLUS.
               05  HCM34ACCT-RUIKEITOTAL           PIC 9(15).
               05  HCM34ACCT-RUIKEITOTAL2          PIC 9(15).
               05  HCM34ACCT-JIHIMONEYTOTAL        PIC 9(07).
               05  HCM34ACCT-SRT-SRYCD             PIC X(09).
               05  HCM34ACCT-SRT-SRYSYUKBN         PIC X(03).
               05  HCM34ACCT-SRT-KOKUJISKBKBN2     PIC 9(01).
               05  HCM34ACCT-SRT-SRYCOMMENT        PIC 9(01).
               05  HCM34ACCT-ACCTKBN               PIC X(01).
               05  HCM34ACCT-ZAISKBKBN             PIC X(01).
               05  HCM34ACCT-PRIORITY              PIC 9(03).
               05  HCM34ACCT-PRIORITY2             PIC 9(03).
               05  HCM34ACCT-PRIORITY3             PIC 9(03).
               05  HCM34ACCT-TNSTEN                PIC 9(10).
               05  HCM34ACCT-SRYSTATUS             PIC 9(01).
               05  HCM34ACCT-MINUS                 PIC 9(01).
               05  HCM34ACCT-RIHA                  PIC 9(01).
               05  HCM34ACCT-RIHA-SYS              PIC 9(01).
               03  HCM34ACCT-TENTEKSBT             PIC X(01).
               03  HCM34ACCT-INGAIKBN              PIC X(01).
      *
      *    診療行為ファイル
       FD  HCM34SRY-FILE.
       01  HCM34SRY-REC.
         02  HCM34SRY-ISM-KEY.
           03  HCM34SRY-ISM-PTID                   PIC 9(10).
           03  HCM34SRY-ISM-ZAINUM                 PIC 9(08).
           03  HCM34SRY-ISM-RENNUM                 PIC 9(03).
         02  HCM34SRY-DAT.
           COPY    "CPSRYACT.INC"          REPLACING   //SRY-//
                                           BY          //HCM34SRY-//.
         02  HCM34SRY-PLUS.
           03  HCM34SRY-PLUS-OCC                   OCCURS 5.
               05  HCM34SRY-COMMENT                 PIC X(200).
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    処理対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01PARA//
                                           BY          //HCM34F01//.
      *
      *    処理対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01PARA//
                                           BY          //HCM34F02//.
      *
           COPY    "HCM341V01.INC"
                   REPLACING   //HCM341V01//  BY //HCM341//.
           COPY    "HCM342V01.INC"
                   REPLACING   //HCM342V01//  BY //HCM342//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA                            PIC X(02).
           03  STS-HCM34F01                        PIC X(02).
           03  STS-HCM34F02                        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-JYURRK                          PIC 9(01).
           03  FLG-HEAD                            PIC 9(01).
           03  FLG-OVER                            PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).
           03  FLG-PTHKNINF                        PIC 9(01).
           03  FLG-PTKOHINF                        PIC 9(01).
           03  FLG-PTBYOMEI                        PIC 9(01).
           03  FLG-ZENSP                           PIC 9(01).
           03  FLG-SRYACCT                         PIC 9(01).
           03  FLG-SRYACT                          PIC 9(01).
           03  FLG-NYUINACCT                       PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-SP                              PIC 9(01).
           03  FLG-LINE                            PIC 9(01).
           03  FLG-MINUS                           PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-NACCTCHK-EXT                    PIC 9(01).
           03  FLG-BRMSAGAKU-HEN                   PIC 9(01).
           03  FLG-BRMSAGAKU-TTL-HEN               PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-LINECHK                         PIC 9(01).
           03  FLG-RSIJBI-AREA-HEN                 PIC 9(01).
           03  FLG-JIBAI                           PIC 9(01).
           03  FLG-ROSAI                           PIC 9(01).
           03  FLG-KENPO                           PIC 9(01).
           03  FLG-DAISAN                          PIC 9(01).
           03  FLG-AFTER-CARE                      PIC 9(01).
           03  FLG-TBLCOMBI                        PIC 9(01).
           03  FLG-SRYACT-HEN5                     PIC 9(01).
           03  FLG-COMMENT-CHK                     PIC 9(01).
           03  FLG-PTCOM                           PIC 9(01).
           03  FLG-YAKUSOKU                        PIC 9(01).
           03  FLG-HOUKATU                         PIC 9(01).
           03  FLG-ZAIMATOME                       PIC 9(01).
           03  FLG-EDIT                            PIC 9(01).
           03  FLG-JBI-KZI-OUT                     PIC 9(01).
           03  FLG-HCM34ACCT                       PIC 9(01).
           03  FLG-HCM34SRY                        PIC 9(01).
           03  FLG-HKNCOMBI                        PIC 9(01).
           03  FLG-COMMENT-HEN                      PIC 9(01).
           03  FLG-JIHI-HEN                        PIC 9(01).
           03  FLG-RIHA                            PIC 9(01).
           03  FLG-RIHA-SYS                        PIC 9(01).
           03  FLG-RIHA-HEN                        PIC 9(01).
           03  FLG-SRYSTATUS                       PIC 9(01).
           03  FLG-TARGET                          PIC 9(01).
           03  FLG-ACCEPT                          PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
           03  FLG-ORCSHKNSET                      PIC 9(01)
                                                   OCCURS 100.
           03  FLG-HKNGENTEI                       PIC 9(01).
           03  FLG-PTRSIINF                        PIC 9(01).
           03  FLG-RECECOM                         PIC 9(01).
           03  FLG-TAJYUSHIN                       PIC 9(01).
           03  FLG-COMB-CHANGED                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(03).
           03  CNT-HKNCOMBI-LINE                   PIC 9(05).
           03  CNT-HKNKOH-LINE                     PIC 9(05).
           03  CNT-BYOMEI-LINE                     PIC 9(05).
           03  CNT-SRYACT-LINE                     PIC 9(05).
           03  CNT-YKZINGAI                        PIC 9(05).
           03  CNT-YKZHKT                          PIC 9(05).
           03  CNT-ZAIHKT                          PIC 9(05).
           03  CNT-JISANYAKU                       PIC 9(05).
           03  CNT-SPACE                           PIC 9(05).
           03  CNT-STR                             PIC 9(05).
           03  CNT-BYOMEIMOJI                      PIC 9(05).
           03  CNT-HOUSRYACT                       PIC 9(05).
           03  CNT-RENPAKU                         PIC 9(03).
           03  CNT-PTNYUINRRK                      PIC 9(05).
           03  CNT-RENNUM                          PIC 9(05).
           03  CNT-SRYACCT                         PIC 9(05).
           03  CNT-COMB-ACCT                       PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
           03  SYS-TIME                            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDX7                                PIC 9(05).
           03  IDX8                                PIC 9(05).
           03  IDX9                                PIC 9(05).
           03  IDX10                               PIC 9(05).
           03  IDX11                               PIC 9(05).
           03  IDX12                               PIC 9(05).
           03  IDX13                               PIC 9(05).
           03  IDX14                               PIC 9(05).
           03  IDX15                               PIC 9(05).
           03  IDX16                               PIC 9(05).
           03  IDX17                               PIC 9(05).
           03  IDX18                               PIC 9(05).
           03  IDX19                               PIC 9(05).
           03  IDX20                               PIC 9(05).
           03  IDX21                               PIC 9(05).
           03  IDX22                               PIC 9(05).
           03  IDX23                               PIC 9(05).
           03  IDX24                               PIC 9(05).
           03  IDX25                               PIC 9(05).
           03  IDX26                               PIC 9(05).
           03  IDX27                               PIC 9(05).
           03  IDX28                               PIC 9(05).
           03  IDX29                               PIC 9(05).
           03  IDX30                               PIC 9(05).
           03  IDX31                               PIC 9(05).
           03  IDX32                               PIC 9(05).
           03  IDX33                               PIC 9(05).
           03  IDX34                               PIC 9(05).
           03  IDX35                               PIC 9(05).
           03  IDX36                               PIC 9(05).
           03  IDX37                               PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXC                                PIC 9(05).
           03  IDXD                                PIC 9(05).
           03  IDXE                                PIC 9(05).
           03  IDXF                                PIC 9(05).
           03  IDXG                                PIC 9(05).
           03  IDXH                                PIC 9(05).
           03  IDXI                                PIC 9(05).
           03  IDXJ                                PIC 9(05).
           03  IDXK                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXM                                PIC 9(05).
           03  IDXR                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDXT                                PIC 9(05).
           03  IDXW                                PIC 9(05).
           03  IDXZ                                PIC 9(05).
           03  IDX-RRK                             PIC 9(05).
           03  IDX-RJ                              PIC 9(05).
           03  IDX-DAY                             PIC 9(05).
           03  IDX-PNT                             PIC 9(05).
           03  IDX-TBLCOMBI                        PIC 9(05).
           03  IDX-TRECECOM                        PIC 9(05).
      *
      *    同一セクション内のみ使用
           03  IDX0                                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME                        PIC X(64).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-HCM34-AREA.
               05  WRK-HCM34-SRYYM                 PIC X(16).
               05  WRK-HCM34-SAKYMD                PIC X(22).
               05  WRK-HCM34-PTNUM                 PIC X(10).
               05  WRK-HCM34-PTNUM2                PIC X(20).
               05  WRK-HCM34-BIRTHDAY              PIC X(22).
               05  WRK-HCM34-SRYKA-MAX             PIC 9(05).
               05  WRK-HCM34-SRYKA-G.
                   07  WRK-HCM34-SRYKA             PIC X(16)
                                                   OCCURS 2.
               05  WRK-HCM34-NYUINYMD              PIC X(22).
               05  WRK-HCM34-TAIINYMD              PIC X(22).
               05  WRK-HCM34-LBLSRYKA              PIC X(06).
               05  WRK-HCM34-LBLNYUINYMD           PIC X(06).
               05  WRK-HCM34-LBLTAIINYMD           PIC X(06).
               05  WRK-HCM34-BTUNAME               PIC X(30).
           03  WRK-STR                             PIC X(500).
           03  WRK-LONGSTR                         PIC X(5000).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-ZX99-G.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.99.
               05  WRK-STR-LEFT-FL                 PIC X(487).
           03  WRK-STRLEN                          PIC 9(05).
           03  WRK-STRLEN-MAX                      PIC 9(05).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-HKNCOMBINUM-N               PIC X(08).
               05  WRK-TEKSTYMD-N                  PIC X(18).
               05  WRK-TEKEDYMD-N                  PIC X(18).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-ZZ9                             PIC ZZ9.
           03  WRK-Z9                              PIC Z9.
           03  WRK-WIDTH                           PIC 9(05).
      *    診療行為欄に編集を行う項目を判断する区分
      *    1:保険組合せ／2:保険／3:公費／4:傷病名／5:診療行為
      *    空白:上記以外
           03  WRK-ITEM-KBN                        PIC X(01).
           03  WRK-SHO                             PIC 9(03).
           03  WRK-AMARI                           PIC 9(03).
           03  WRK-PTHKN-HKNNUM                    PIC X(03).
           03  WRK-PTHKN-HONKZKKBN                 PIC X(01).
           03  WRK-PTHKN-HONKZKKBNMEI              PIC X(10).
           03  WRK-PTHKN-HOJOKBN                   PIC X(01).
           03  WRK-PTHKN-HOJOKBNMEI                PIC X(10).
           03  WRK-PTHKN-CONTKBN                   PIC X(01).
           03  WRK-PTHKN-CONTKBNMEI                PIC X(10).
           03  WRK-PTKOH-FTNJANUM-N                PIC X(16).
           03  WRK-PTKOH-JKYSNUM-N                 PIC X(40).
           03  WRK-PTBYO-TENKIKBN                  PIC X(01).
           03  WRK-PTBYO-TENKIKBNMEI               PIC X(04).
           03  WRK-PTBYO-BYOMEI                    PIC X(240).
           03  WRK-PTBYO-BYOMEIMOJI                PIC 9(05).
           03  WRK-PTBYO-UTAGAIFLG                 PIC X(01).
           03  WRK-PTBYO-HOSOKU-COMMENT            PIC X(80).
           03  WRK-BASEYMD.
               05  WRK-BYY                         PIC 9(04).
               05  WRK-BMM                         PIC 9(02).
               05  WRK-BDD                         PIC 9(02).
           03  WRK-AGE70-YMD.
               05  WRK-AGE70-YY                    PIC 9(04).
               05  WRK-AGE70-MM                    PIC 9(02).
               05  WRK-AGE70-DD                    PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY                       PIC 9(04).
               05  WRK-SRYMM                       PIC 9(02).
               05  WRK-SRYDD                       PIC 9(02).
           03  WRK-PNT                             PIC 9(05).
           03  WRK-PNT-BF                          PIC 9(05).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
      *    数量表示用
           03  WRK-SURYO                           PIC 9(05)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1                    PIC 9(05).
               05  WRK-SURYO-S2                    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1                    PIC ZZZZZ.
               05  WRK-SURYO-Z19       REDEFINES   WRK-SURYO-Z1
                                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2                    PIC X(01).
               05  WRK-SURYO-Z3                    PIC ZZZZZ.
               05  WRK-SURYO-Z3-X      REDEFINES   WRK-SURYO-Z3
                                                   PIC X(05).
      *    分画数画面編集用
           03  WRK-BUNGSU                          PIC X(10).
           03  WRK-TENSUKAISU                      PIC X(20).
           03  WRK-KEY-HKNCOMBI                    PIC X(04).
           03  WRK-TNS-HOUKNSKBN                   PIC 9(02).
           03  WRK-KUGIRI                          PIC X(02).
           03  WRK-TTL-TEN                         PIC S9(11).
           03  WRK-TTL-KINGAKU                         PIC S9(11).
           03  WRK-TTL-JIHI                        PIC 9(09).
           03  WRK-TTL-JIHI-TAX                    PIC 9(09).
           03  WRK-TTL-BRMSAGAKU                   PIC 9(09).
           03  WRK-TTL-BRMSAGAKU-TAX               PIC 9(09).
           03  WRK-TENSU-LBL                    PIC X(100).
           03  WRK-TENSU                        PIC 9(09).
           03  WRK-GAIHAKU-ST                      PIC 9(02).
           03  WRK-GAIHAKU-ST-Z9                   PIC Z9.
           03  WRK-GAIHAKU-ED                      PIC 9(02).
           03  WRK-GAIHAKU-ED-Z9                   PIC Z9.
           03  WRK-HKNCOMBI-KEY-MAE                PIC X(04).
           03  WRK-HKNCOMBI-KEY                    PIC X(04).
           03  WRK-HKNCOMBINUM-G.
               05  WRK-HKNCOMBINUM                 PIC 9(04).
           03  WRK-HKNNUM                          PIC X(03).
           03  WRK-KOHNUM                          PIC X(03).
           03  WRK-SHONUM-G.
               05  WRK-SHONUM-MAX                  PIC 9(03).
               05  WRK-SHONUM                      PIC 9(03)
                                                   OCCURS  20.
           03  WRK-RRK-NYUINYMD                    PIC X(18).
           03  WRK-RRK-TAIINYMD                    PIC X(18).
           03  WRK-ZAITEN                          PIC S9(09).
           03  WRK-ZAIKAISU                        PIC 9(07).
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-SRYCD-9                         PIC 9(09).
           03  WRK-SRYSYUKBN                       PIC X(03).
           03  WRK-KOKUJISKBKBN2                   PIC 9(01).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TANINAME                        PIC X(24).
           03  WRK-JBI-SONOTA-G.
               05  WRK-JBI-SONOTA-KBN              PIC X(01).
               05  WRK-JBI-SONOTA-TEN              PIC 9(08).
               05  WRK-JBI-SONOTA-SRYCD            PIC X(09).
           03  WRK-RSI-SONOTA-G.
               05  WRK-RSI-SONOTA-KBN              PIC X(01).
               05  WRK-RSI-SONOTA-TEN              PIC 9(08).
           03  WRK-JBI-RYOYO-TEATE-KBN             PIC X(01).
           03  WRK-JIHIMONEYTOTAL                  PIC 9(07).
           03  WRK-RUIKEI                          PIC 9(15).
           03  WRK-RUIKEITOTAL                     PIC 9(15).
           03  WRK-RUIKEITOTAL2                    PIC 9(15).
           03  WRK-ZAINUM                          PIC 9(08).
           03  WRK-HBTSRYKA-G.
               05  WRK-HBTSRYKA-HBTKA              PIC X(02) OCCURS  31.
               05  WRK-HBTSRYKA-MAX                PIC 9(05).
               05  WRK-HBTSRYKA-OCC                OCCURS  100.
                   07  WRK-HBTSRYKA                PIC X(02).
                   07  WRK-HBTSRYKA-DAY            PIC 9(01) OCCURS  31.
           03  WRK-STIDX                           PIC 9(02).
           03  WRK-EDIDX                           PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                         PIC 9(03) OCCURS  31.
           03  WRK-ZAISKBKBN                       PIC X(01).
           03  WRK-CALCSRYCD-G.
               05  WRK-CALCSRYCD                   PIC 9(01) OCCURS  9.
           03  WRK-NUM                             PIC 9(01).
           03  WRK-DBYMD                           PIC X(08).
           03  WRK-HOKEN                           PIC X(01).
           03  WRK-PRT-STDAY                       PIC Z9.
           03  WRK-PRT-EDDAY                       PIC Z9.
           03  WRK-PRT-KIKAN                       PIC X(20).
           03  WRK-TNSTEN                          PIC 9(10).
           03  WRK-SJKBN-G.
               05  WRK-SJKBN                       PIC 9(2).
           03  WRK-DD-G.
               05  WRK-DD                          PIC 9(2).
           03  WRK-TOTAL-MAX                   PIC 9(06).
           03  WRK-TOTAL-G.
               05  WRK-TOTALZ                      PIC -------9.
               05  WRK-TOTAL-TANI                  PIC X(02).
               05  WRK-TOTAL-KAKKO                 PIC X(01).
           03  WRK-TNS-NAME                        PIC X(200).
           03  WRK-KZI-G.
               05  WRK-KZI-TANICD                  PIC X(03).
               05  WRK-KZI-TANINAME                PIC X(24).
               05  WRK-KZI-TEN                     PIC 9(08).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-BYOMEI-G.
               05  WRK-BYOMEI-MAX                  PIC 9(05).
               05  WRK-BYOMEI.
                   07  WRK-BYOMEI-LINE             PIC X(56)
                                                   OCCURS 3.
           03  WRK-RATE1                           PIC 999V99.
           03  WRK-RATE2                           PIC 999V99.
           03  WRK-RATE3                           PIC 999V99.
           03  WRK-RATE4                           PIC 999V99.
           03  WRK-HASU                            PIC 9V99999.
           03  WRK-RIHA-MAX                        PIC 9(05).
           03  WRK-COMMENT                         PIC X(200).
           03  WRK-MAXZAINUM                       PIC 9(08).
           03  WRK-MAX                             PIC 9(05).
           03  WRK-INGAIKBN                        PIC X(1).
      *
       01  COMMENT-CHK-AREA.
           03  COMMENT-IDX                         PIC 9(03).
           03  COMMENT-OCC                         OCCURS 2.
               05  COMMENT-ZAINUM                  PIC 9(08).
               05  COMMENT-MAX                     PIC 9(03).
               05  COMMENT-COMMENT-G.
                   07  COMMENT-COMMENT             PIC X(80)
                                                   OCCURS  10.
      *
       01  WRK-SRYKA-TBL.
           03  WRK-SRYKA-OCC                       OCCURS  20.
               05  WRK-SRYKA                       PIC X(02).
               05  WRK-SRYKA-SRYKAMEI1             PIC X(20).
               05  WRK-SRYKA-SRYKAMEI2             PIC X(12).
               05  WRK-SRYKA-SRYKAMEI3             PIC X(04).
      *
       01  WRK-SRYACT-AREA.
           03  WRK-SRYACT-STR                      PIC X(520).
           03  WRK-SRYACT-STR-PLUS                 PIC X(100).
           03  WRK-SRYACT-PNT                      PIC 9(05).
           03  WRK-SRYACT-MAX                      PIC 9(05).
           03  WRK-SRYACT-MAX-BF                   PIC 9(05).
           03  WRK-SRYACT-TBL.
               05  WRK-SRYACT-OCC                  OCCURS  500.
                   07  WRK-SRYACT                  PIC X(52).
           03  WRK-SRYACT-SRYKBN                   PIC X(02).
           03  WRK-SRYACT-SRYSYUKBN                PIC X(03).
           03  WRK-SRYACT-MARK                     PIC X(04).
           03  WRK-SRYACT-HKNCOMBI                 PIC X(04).
           03  WRK-SRYACT-JIHOKBN                  PIC X(01).
      *    TANI 1:TEN 2:EN
           03  WRK-SRYACT-TOTAL-G.
               05  WRK-SRYACT-TOTAL-OCC            OCCURS 2.
                   07  WRK-SRYACT-POS              PIC 9(05).
                   07  WRK-SRYACT-TANI             PIC 9(01).
                   07  WRK-SRYACT-TOTAL            PIC S9(11).
               05  WRK-SRYACT-DAY-OCC              OCCURS  31.
                   07  WRK-SRYACT-DAY-X            PIC X(03).
                   07  WRK-SRYACT-DAY-Z    REDEFINES   WRK-SRYACT-DAY-X
                                                   PIC ZZZ.
                   07  WRK-SRYACT-DAY-Z9   REDEFINES   WRK-SRYACT-DAY-X
                                                   PIC ZZ9.
      *
       01  TCOMBI-AREA.
           03  TCOMBI-MAX                          PIC 9(05).
           03  TCOMBI-HKNCOMBI-OCC                 OCCURS  50.
               05  TCOMBI-HKNCOMBI                 PIC 9(04).
               05  TCOMBI-HKNCOMBI-X       REDEFINES   TCOMBI-HKNCOMBI
                                                   PIC X(04).
               05  TCOMBI-HKNCOMBI-CNT             PIC 9(03).
               05  TCOMBI-HKNCOMBI-DAY             PIC 9(01)
                                                   OCCURS  31.
               05  TCOMBI-SENTEI-PTFTN             PIC 9(08).
      *
       01  TRECECOM-AREA.
           03  TRECECOM-AA                         PIC 9(01).
           03  TRECECOM-SRYDD                      PIC 9(02).
           03  TRECECOM-KARAGYO                    PIC 9(01).
           03  TRECECOM-PNT                        PIC 9(05).
           03  TRECECOM-SKJKBN                     PIC X(02).
           03  TRECECOM-OCC-G.
               05  TRECECOM-MAX                    PIC 9(05).
               05  TRECECOM-OCC                    OCCURS  500.
                   07  TRECECOM-COMMENT            PIC X(52).
      *
       01  SRH-AREA.
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
           03  SRH-SRYKA-DAT.
               05  SRH-SRYKA                       PIC X(02).
               05  SRH-SRYKAMEI1                   PIC X(20).
               05  SRH-SRYKAMEI2                   PIC X(12).
               05  SRH-SRYKAMEI3                   PIC X(04).
               05  SRH-SRYKA-MAX                   PIC 9(05).
               05  SRH-SRYKA-OCC                   OCCURS 100.
                   07  SRH-SRYKAL                  PIC X(02).
                   07  SRH-SRYKAMEI1L              PIC X(20).
                   07  SRH-SRYKAMEI2L              PIC X(12).
                   07  SRH-SRYKAMEI3L              PIC X(04).
      *
       01  SRT-AREA.
           03  SRT-IDX1                            PIC 9(05).
           03  SRT-IDX2                            PIC 9(05).
           03  SRT-STIDX                           PIC 9(05).
           03  SRT-SRYACCT.
               05  SRT-SRYACCT-OCC                 OCCURS  2001.
                   07  SRT-SRYACCT-IDX             PIC 9(05).
                   07  SRT-SRYACCT-HOUKNSKBN       PIC 9(02).
                   07  SRT-SRYACCT-KEY.
                       09 SRT-SRYACCT-HKNCOMBI     PIC X(04).
                       09 SRT-SRYACCT-JIHIKBN      PIC 9(01).
                       09 SRT-SRYACCT-SRYKBN       PIC X(02).
                       09 SRT-SRYACCT-SRYCOMMENT   PIC 9(01).
                       09 SRT-SRYACCT-TEMP         PIC X(05).
                       09 SRT-SRYACCT-ZAISKBKBN    PIC X(01).
                       09 SRT-SRYACCT-PRIORITY     PIC 9(03).
                       09 SRT-SRYACCT-PRIORITY2    PIC 9(03).
                       09 SRT-SRYACCT-PRIORITY3    PIC 9(03).
                       09 SRT-SRYACCT-ZAIHKTKBN    PIC 9(01).
                       09 SRT-SRYACCT-YKZHKTKBN    PIC 9(01).
                       09 SRT-SRYACCT-YKZINGAIKBN  PIC 9(01).
                       09 SRT-SRYACCT-KNSJISKBN    PIC 9(01).
                       09 SRT-SRYACCT-OPE-SRYSYUKBN PIC  X(03).
                       09 SRT-SRYACCT-OPE-DAY       PIC  9(02).
                       09 SRT-SRYACCT-CDKBN-KBN-ETC
                                                   PIC X(01).
                       09 SRT-SRYACCT-CDKBN-KBN-G.
                           11 SRT-SRYACCT-CDKBN-KBN
                                                   PIC X(01).
                       09 SRT-SRYACCT-CDKBN-SYO-G.
                           11 SRT-SRYACCT-CDKBN-SYO
                                                   PIC 9(01).
                       09 SRT-SRYACCT-CDKBN-BU-G.
                           11 SRT-SRYACCT-CDKBN-BU PIC 9(02).
                       09 SRT-SRYACCT-CDKBN-KBNNUM-G.
                           11 SRT-SRYACCT-CDKBN-KBNNUM
                                                   PIC 9(03).
                       09 SRT-SRYACCT-CDKBN-KBNNUM-EDA-G.
                           11 SRT-SRYACCT-CDKBN-KBNNUM-EDA
                                                   PIC 9(02).
                       09 SRT-SRYACCT-CDKBN-KOUBAN-G.
                           11 SRT-SRYACCT-CDKBN-KOUBAN
                                                   PIC 9(02).
                       09 SRT-SRYACCT-KOUHYOJYUNNUM-G.
                          11 SRT-SRYACCT-KOUHYOJYUNNUM
                                                   PIC 9(09).
                       09 SRT-SRYACCT-SRYSYUKBN    PIC X(03).
                       09 SRT-SRYACCT-ZAITEN       PIC 9(08).
                       09 SRT-SRYACCT-ZAINUM       PIC 9(08).
      *
       01  RSIJBI-AREA.
           03  RSIJBI-STIDX                        PIC 9(05).
           03  RSIJBI-IDX1                         PIC 9(05).
           03  RSIJBI-IDX2                         PIC 9(05).
           03  RSIJBI-MAX                          PIC 9(05).
           03  RSIJBI-COM-IDX                      PIC S9(05).
           03  RSIJBI-TEN-IDX                      PIC S9(05).
           03  RSIJBI-KIN-IDX                      PIC S9(05).
           03  RSIJBI-TEN                          PIC 9(08).
           03  RSIJBI-KINGAKU                      PIC 9(08).
           03  RSIJBI-ZAIKAISU                     PIC 9(07).
           03  RSIJBI-OCC                          OCCURS  20.
               05  RSIJBI-SRYCD                    PIC X(09).
               05  RSIJBI-INPUTNUM                 PIC 9(3).
               05  RSIJBI-NAME                     PIC X(200).
               05  RSIJBI-COMMENT                  PIC X(200).
           03  WRK-TAX-RATE                        PIC 9V99.
      *
       01  KEY-AREA.
           03  KEY-ACCT-HKNCOMBI                   PIC 9(4).
      *
       01  CONST-AREA.
           03  CONST-R0110             PIC X(08)   VALUE "201910".
           03  CONST-H2910             PIC X(08)   VALUE "201710".
           03  CONST-H2612             PIC X(08)   VALUE "201412".
           03  CONST-H2604             PIC X(08)   VALUE "201404".
           03  CONST-H2404             PIC X(08)   VALUE "201204".
           03  CONST-H190401           PIC X(08)   VALUE "20070401".
           03  CONST-H141001           PIC X(08)   VALUE "20021001".
           03  CONST-H181001           PIC X(08)   VALUE "20061001".
      *    労災自賠編集領域最大行
           03  CONST-RSIJBI-MAX        PIC 9(05)   VALUE 20.
      *    会計カード診療行為最大行
           03  CONST-LINE-MAX          PIC 9(05)   VALUE 28.
      *    会計カード診療行為最大行（続紙）
           03  CONST-ZKSI-LINE-MAX     PIC 9(05)   VALUE 39.
      *    保険組合せ最大行
           03  CONST-HKNCOMBI-LINE-MAX PIC 9(05)   VALUE 6.
      *    03  CONST-HKNCOMBI-LINE-MAX PIC 9(05)   VALUE 0.
      *    保険／公費最大行
           03  CONST-HKNKOH-LINE-MAX   PIC 9(05)   VALUE 6.
      *    03  CONST-HKNKOH-LINE-MAX   PIC 9(05)   VALUE 0.
      *    傷病名最大行
           03  CONST-BYOMEI-LINE-MAX   PIC 9(05)   VALUE 10.
      *    03  CONST-BYOMEI-LINE-MAX   PIC 9(05)   VALUE 0.
      *    診療行為桁数
           03  CONST-SRYACT-LEN        PIC 9(05)   VALUE 52.
      *
           03  CONST-KENSACOMMENT-VAL.
                05  FILLER                 PIC X(50)   VALUE
                "血液化学検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌学的検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "肝炎ウイルス関連検査".
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカーのうちα−フェトプロテイン等".
                05  FILLER                 PIC X(50)   VALUE
                "腫瘍マーカー".                        
                05  FILLER                 PIC X(50)   VALUE
                "出血・凝固検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "自己抗体検査".                        
                05  FILLER                 PIC X(50)   VALUE
                "内分泌負荷試験".
                05  FILLER                 PIC X(50)   VALUE
                "ウイルス抗体価".                        
                05  FILLER                 PIC X(50)   VALUE
                "ウイルス抗体価精密測定".
                05  FILLER                 PIC X(50)   VALUE
                "血漿蛋白免疫学的検査".                        
            03 CONST-KENSACOMMENT-R        REDEFINES
                                           CONST-KENSACOMMENT-VAL.
                05  CONST-KENSACOMMENT     PIC X(50)   OCCURS  11.
           03  CONST-RECECOM-VAL.
               05  FILLER                      PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状】".
               05  FILLER                      PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状の診察検査所見】".
               05  FILLER                       PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の必要性】".
               05  FILLER                       PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の経過】".
               05  FILLER                       PIC X(52)   VALUE
               "【合計点数が１００万点以上の薬剤に係る症状等】".
               05  FILLER                       PIC X(52)   VALUE
               "【合計点数が１００万点以上の処置に係る症状等】".
               05  FILLER                       PIC X(52)   VALUE
               "【その他】".
               05  FILLER                       PIC X(52)   VALUE
               "【薬事法に規定する治験に係る治験概要】".
               05  FILLER                       PIC X(52)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
               05  FILLER                       PIC X(52)   VALUE
               "【廃用症候群に係る評価表】".
               05  FILLER                       PIC X(52)   VALUE
               "【レセプト摘要欄コメント】".
               05  FILLER                       PIC X(52)   VALUE
               "【労災レセプト「傷病の経過」】".
               05  FILLER                       PIC X(52)   VALUE
               "【自賠責（第三者行為）レセプト摘要欄コメント】".
               05  FILLER                       PIC X(52)   VALUE
               "【上記以外の診療報酬明細書】".
               05  FILLER                       PIC X(52)   VALUE
               "【医薬品医療機器等法に規定する治験に係る治験概要】".
           03  CONST-RECECOM-R     REDEFINES   CONST-RECECOM-VAL.
               05  CONST-RECECOM   PIC X(52)   OCCURS   15.
      *    薬剤料逓減
           03  CONST-TEIGEN                PIC X(09)   VALUE
               "630010002".
      *    未実施減算
           03  CONST-GENZAN1               PIC X(09)   VALUE
               "190080470".
           03  CONST-GENZAN2               PIC X(09)   VALUE
               "190799670".
           03  CONST-GENZAN3               PIC X(09)   VALUE
               "190080570".
           03  CONST-GENZAN4               PIC X(09)   VALUE
               "190799770".
           03  CONST-GENZAN5               PIC X(09)   VALUE
               "190111690".
           03  CONST-GENZAN6               PIC X(09)   VALUE
               "190809090".
           03  CONST-GENZAN7               PIC X(09)   VALUE
               "190111790".
           03  CONST-GENZAN8               PIC X(09)   VALUE
               "190809190".
           03  CONST-GENZAN9               PIC X(09)   VALUE
               "190182590".
           03  CONST-GENZAN10              PIC X(09)   VALUE
               "190182690".
           03  CONST-SRYKAMEI-LEN          PIC 9(05)   VALUE   32.
           03  CONST-GAZOU-TANICD          PIC X(03)   VALUE   "006".
           03  CONST-SRYACCT-MAX           PIC 9(05)   VALUE   2000.
           03  CONST-RYOYO-TEATE-GAI       PIC X(09)   VALUE
               "199000610".
           03  CONST-RYOYO-TEATE-NYU       PIC X(09)   VALUE
               "199000510".
           03  CONST-HKNCOMBICD            PIC X(09) VALUE  "999999912".
           03  CONST-HBTSRYKA-MAX          PIC 9(05) VALUE  100.
           03  CONST-SRYCD-MAX             PIC 9(05) VALUE 20.
           03  CONST-TCOMBI-MAX            PIC 9(05) VALUE 50.
           03  CONST-RUIKEI2-G.
               05  CONST-RUIKEI2-VAL       PIC X(20) VALUE
               "01030507091113151719".
               05  CONST-RUIKEI2-R  REDEFINES   CONST-RUIKEI2-VAL.
                   07  CONST-RUIKEI2       PIC 9(02) OCCURS    10.
           03  CONST-TBLCOMBI-MAX      PIC 9(05)   VALUE   100.
           03  CONST-TRECECOM-MAX      PIC 9(05)   VALUE   500.
           03  CONST-TRECECOM-COMLEN   PIC 9(05)   VALUE   50.
           03  CONST-TRECECOM-MAXLEN
                                       PIC 9(05)   VALUE   52.
      *
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
      *    保険組み合わせワーク
       01  WKHKNCOMBI-AREA.
           02  WKHKNCOMBI-MAX          PIC 9(05).
           02  WKHKNCOMBI-OCC          OCCURS 50.
               03  WKHKNCOMBI-HKNCOMBINUM
                                       PIC  X(04).
               03  WKHKNCOMBI-HKNCOMBIMEI
                                       PIC  X(50).
               03  WKHKNCOMBI-TEKSTYMDS
                                       PIC  X(08).
               03  WKHKNCOMBI-TEKEDYMDS
                                       PIC  X(08).
               03  WKHKNCOMBI-TEKSTYMDW
                                       PIC  X(09).
               03  WKHKNCOMBI-TEKEDYMDW
                                       PIC  X(09).
      *
      *    保険マスタワーク
       01  WKPTHKNINF-REC.
           02  WKPTHKNINF-MAX          PIC 9(05).
           02  WKPTHKNINF-OCC          OCCURS 50.
           COPY    "CPPTHKNINF.INC"
                   REPLACING   //PTHKN-//  BY  //WKPTHKN-//.
      *
      *    公費マスタワーク
       01  WKPTKOHINF-REC.
           02  WKPTKOHINF-MAX          PIC 9(05).
           02  WKPTKOHINF-OCC          OCCURS 50.
           COPY    "CPPTKOHINF.INC"
                   REPLACING   //PTKOH-//  BY  //WKPTKOH-//.
      *
      *    患者病名マスタ−ワーク
       01  WKPTBYOMEI-REC.
           02  WKPTBYOMEI-MAX          PIC 9(05).
           02  WKPTBYOMEI-OCC          OCCURS 300.
           COPY    "CPPTBYOMEI.INC"
                   REPLACING   //PTBYO-//  BY //WKPTBYO-//.
      *
      *    診療会計ワーク
       01  WKSRYACCT-REC.
           02  WKSRYACCT-MAX           PIC 9(05).
           02  WKSRYACCT-OCC           OCCURS 2000
                                       INDEXED WKSRYACCT-IDX.
           COPY    "CPSRYACCT.INC"
                   REPLACING   //ACCT-//  BY //WKACCT-//.
               03 WKACCT-RUIKEITOTAL      PIC 9(15).
               03 WKACCT-RUIKEITOTAL2     PIC 9(15).
               03 WKACCT-JIHIMONEYTOTAL   PIC 9(07).
               03 WKACCT-SRT-SRYCD        PIC X(09).
               03 WKACCT-SRT-SRYSYUKBN    PIC X(03).
               03 WKACCT-SRT-KOKUJISKBKBN2 PIC 9(01).
               03 WKACCT-SRT-SRYCOMMENT   PIC 9(01).
               03 WKACCT-ACCTKBN          PIC X(01).
               03 WKACCT-ZAISKBKBN        PIC X(01).
               03 WKACCT-PRIORITY         PIC 9(03).
               03 WKACCT-PRIORITY2        PIC 9(03).
               03 WKACCT-PRIORITY3        PIC 9(03).
               03 WKACCT-TNSTEN           PIC 9(10).
               03 WKACCT-SRYSTATUS        PIC 9(01).
               03 WKACCT-MINUS            PIC 9(01).
               03 WKACCT-RIHA             PIC 9(01).
               03 WKACCT-RIHA-SYS         PIC 9(01).
               03 WKACCT-TENTEKSBT        PIC X(01).
               03 WKACCT-INGAIKBN         PIC X(01).
      *
      *    保険組合せ
       01  TBLCOMBI-AREA.
           02  TBLCOMBI-MAX            PIC 9(05).
           02  TBLCOMBI-OCC            OCCURS      100.
           COPY    "CPHKNCOMBI.INC"    REPLACING   //COMB-//
                                       BY          //TBLCOMBI-//.
      *
       01  WKCOMB-REC.
           COPY    "CPHKNCOMBI.INC"    REPLACING   //COMB-//
                                       BY          //WKCOMB-//.
      *
           COPY    "CPORCHCN68S01.INC" REPLACING   //HCN68S01-//
                                       BY          //THCN68S01-//.
      *
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者病名マスタ
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    労災自賠責医療機関情報設定
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK4003.INC".
      *
      *    医療機関情報−入院基本
           COPY    "CPSK5000.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    労災入院診療コードチェックサブ
           COPY    "CPORCHCM34S03.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    病名編集
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    リハビリ開始日コメント編集
           COPY    "CPORCSRIHASTDAY.INC".
      *
      *    入院点滴集計サブ
           COPY    "CPORCSTENTEK3.INC".
      *
      *    医療区分・ＡＤＬ点数取得サブ
           COPY    "CPORCHCN68S01.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHCM34.INC".
       COPY    "CPTTNS.INC".
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION    USING
                                           SPA-AREA
                                           ORCHCM34AREA
                                           TTNS-AREA
                                           TTNS-CONST-MAX.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
           .
      *
           EXIT PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SRH-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  ORCHCM34-OT
           INITIALIZE                  TBLCOMBI-AREA
           INITIALIZE                  WKPTBYOMEI-REC
           INITIALIZE                  HCN68S01-AREA
      *
      *    医療機関情報取得処理
           PERFORM 1001-SYS1001-GET-SEC
      *
      *    医療機関情報−入院基本取得処理
           PERFORM 1001-SYS5000-GET-SEC
      *
      *    労災自賠責医療機関情報取得処理
           PERFORM 1001-SYS4001-GET-SEC
      *
      *    自賠責保険　特定器材等消費税対象外コード情報取得処理
           PERFORM 1001-SYS4003-GET-SEC
      *
      *    保険の種類・公費の種類編集処理
           PERFORM 1001-HKNKOH-HEN-SEC
      *
      *    診療科編集処理
           PERFORM 1001-SRYKA-HEN-SEC
      *
      *    医療区分、ＡＤＬ点数取得処理
           IF    ( ORCHCM34-EDIT-MEDADL    =   "1" )
               PERFORM 1001-MEDADL-HEN-SEC
           END-IF
      *
      *    基本情報編集
           PERFORM 1001-KHN-EDIT-SEC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    ORCHCM34-NYUGAIKBN  TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
           MOVE    ORCHCM34-PRTAREA    TO  ORCSPRTAREA
      *
           INITIALIZE                      HCM34F01
           MOVE    SPA-HOSPNUM         TO  HCM34F01-HOSPNUM
           MOVE    "HCM3401"           TO  HCM34F01-FILE-ID
           MOVE     SPRT-TERMID        TO  HCM34F01-TERMID
      *
           INITIALIZE                      HCM34F02
           MOVE    SPA-HOSPNUM         TO  HCM34F02-HOSPNUM
           MOVE    "HCM3402"           TO  HCM34F02-FILE-ID
           MOVE    SPRT-TERMID         TO  HCM34F02-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HCM34F01-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE    HCM34F02-BASENAME   TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  HCM34F01-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  HCM34F02-FULLNAME
      *
           INITIALIZE                  HCM341
           INITIALIZE                  TCOMBI-AREA
      *
           MOVE    ZERO                TO  FLG-HEAD
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-SRYACT-LINE
           MOVE    ZERO                TO  CNT-YKZINGAI
           MOVE    ZERO                TO  CNT-YKZHKT
           MOVE    ZERO                TO  CNT-ZAIHKT
           MOVE    ZERO                TO  CNT-JISANYAKU
           MOVE    LOW-VALUE           TO  WRK-KEY-HKNCOMBI
           MOVE    ZERO                TO  WRK-TTL-TEN
           MOVE    ZERO                TO  WRK-TTL-KINGAKU
           MOVE    ZERO                TO  WRK-TTL-JIHI
           MOVE    ZERO                TO  WRK-TTL-JIHI-TAX
           MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU
           COMPUTE WRK-MAXZAINUM   =   PTINF-MAXZAINUM     +   1000
      *
           EVALUATE    TRUE
           WHEN  ( ORCHCM34-SRYYM      >=  CONST-R0110 )
               MOVE    0.10            TO  WRK-RATE1
           WHEN  ( ORCHCM34-SRYYM      >=  CONST-H2604 )
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
           MOVE    0                   TO  WRK-HASU
           EVALUATE  SYS-1001-TAX-HASU-KBN
           WHEN    "1"
               MOVE    0.5             TO  WRK-HASU
           WHEN    "3"
               MOVE    0.99999         TO  WRK-HASU
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報取得処理
      *****************************************************************
       1001-SYS1001-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    1               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       1001-SYS1001-GET-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報−入院基本取得処理
      *****************************************************************
       1001-SYS5000-GET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5000-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  SYS-5000-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2)
           MOVE    SYS-5000-STYUKYMD   TO  SYS-5000-EDYUKYMD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           ELSE
               INITIALIZE                  SYS-5000-REC
           END-IF
      *
           .
       1001-SYS5000-GET-EXT.
           EXIT.
      *****************************************************************
      *    労災医療機関情報
      *****************************************************************
       1001-SYS4001-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-4001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  SYS-4001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-4001-STYUKYMD(7:2)
           MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-4001-REC
               IF    ( SYS-4001-JIBAIKBN   =   SPACE )
                   MOVE    "1"         TO  SYS-4001-JIBAIKBN
               END-IF
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
           .
       1001-SYS4001-GET-EXT.
           EXIT.
      *****************************************************************
      *    自賠責保険　特定器材等消費税対象外コード
      *****************************************************************
       1001-SYS4003-GET-SEC                SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-4003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4003-HOSPNUM
           MOVE    "4003"              TO  SYS-4003-KANRICD
           MOVE    "*"                 TO  SYS-4003-KBNCD
           MOVE    ORCHCM34-SRYYM      TO  SYS-4003-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-4003-STYUKYMD(7:2)
           MOVE    SYS-4003-STYUKYMD   TO  SYS-4003-EDYUKYMD
           MOVE    SYS-4003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-4003-REC
           ELSE
               INITIALIZE                  SYS-4003-REC
           END-IF
      *
           .
       1001-SYS4003-GET-EXT.
           EXIT.
      *****************************************************************
      *    基本情報編集
      *****************************************************************
       1001-KHN-EDIT-SEC               SECTION.
      *
           INITIALIZE  WRK-HCM34-AREA
      *
      *    診療年月
           MOVE    ORCHCM34-SRYYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-HCM34-SRYYM
      *
      *    期間
           IF    ( ORCHCM34-KIKANKBN  =   "1" )
               MOVE    SPACE          TO   WRK-MAE-INPUT
               MOVE    ORCHCM34-STDAY TO   WRK-PRT-STDAY
               MOVE    ORCHCM34-EDDAY TO   WRK-PRT-EDDAY
               STRING  "（"           DELIMITED    BY  SIZE
                       WRK-PRT-STDAY  DELIMITED    BY  SIZE
                       "日"           DELIMITED    BY  SIZE
                       "〜"           DELIMITED    BY  SIZE
                       WRK-PRT-EDDAY  DELIMITED    BY  SIZE
                       "日"           DELIMITED    BY  SIZE
                       "）"           DELIMITED    BY  SIZE				
               INTO    WRK-MAE-INPUT
               END-STRING
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-PRT-KIKAN
           END-IF
      *
      *    作成日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-HCM34-SAKYMD
      *
      *    患者番号
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               INITIALIZE                  PTNUM-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               IF    ( PTNUM-PTNUM ( 11 : 1)   =   SPACE )
                   MOVE    PTNUM-PTNUM TO  WRK-HCM34-PTNUM
               ELSE
                   MOVE    PTNUM-PTNUM TO  WRK-HCM34-PTNUM2
               END-IF
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    患者情報取得
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               INITIALIZE                  PTINF-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-HCM34-BIRTHDAY
      *
      *    保険年齢で７０歳を迎える日を求める
           MOVE    PTINF-BIRTHDAY      TO  WRK-BASEYMD
           IF    ( WRK-BDD         NOT =   01 )
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF    ( WRK-BMM         >   12 )
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
           MOVE    WRK-BASEYMD         TO  WRK-AGE70-YMD
           COMPUTE WRK-AGE70-YY        =   WRK-AGE70-YY    +   70
      *
      *    入退院歴取得
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               INITIALIZE                  ORCSNTYMDAREA
               MOVE    PTINF-HOSPNUM        TO  SNTYMD-HOSPNUM
               MOVE    PTINF-PTID          TO  SNTYMD-PTID
               MOVE    "2"                 TO  SNTYMD-KBN
               CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                           SPA-AREA
           END-IF
      *
           EVALUATE    ORCHCM34-NYUGAIKBN
           WHEN    "1"
               MOVE    "入院科"        TO  WRK-HCM34-LBLSRYKA
               MOVE    "入院日"        TO  WRK-HCM34-LBLNYUINYMD
               MOVE    "退院日"        TO  WRK-HCM34-LBLTAIINYMD
      *        入院履歴より、入院科・入院日・退院日・病棟名を取得
               PERFORM 10011-PTNYUINRRK-HEN-SEC
           WHEN    "2"
               MOVE    "診療科"        TO  WRK-HCM34-LBLSRYKA
               MOVE    SPACE           TO  WRK-HCM34-LBLNYUINYMD
               MOVE    SPACE           TO  WRK-HCM34-LBLTAIINYMD
      *        受診履歴より、診療科を取得
               IF    ( ORCHCM34-SRYKAKBN   =   "1" )
                   MOVE    ORCHCM34-SRYKA  TO  SRH-SRYKA
                   PERFORM 200111-SRYKA-HEN-SEC
                   MOVE    SRH-SRYKAMEI1   TO  WRK-HCM34-SRYKA(2)
               ELSE
                   PERFORM 10011-SRYKA-HEN-SEC
               END-IF
               MOVE    SPACE           TO  WRK-HCM34-NYUINYMD
               MOVE    SPACE           TO  WRK-HCM34-TAIINYMD
               MOVE    SPACE           TO  WRK-HCM34-BTUNAME
           END-EVALUATE
      *
      *
           INITIALIZE                  ORCSHKNSETAREA
           INITIALIZE                  WRK-SPA-AREA
      *
           MOVE    PTINF-HOSPNUM    TO  WRK-SPA-HOSPNUM
           MOVE    PTINF-PTID      TO  WRK-SPA-PTID
           MOVE    PTINF-BIRTHDAY  TO  WRK-SPA-BIRTHDAY
           MOVE    ORCHCM34-SRYYM  TO  WRK-SPA-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SPA-SRYYMD (7:2)
      *
           MOVE    ORCHCM34-NYUGAIKBN
                                   TO  WRK-SPA-NYUGAIKBN
           MOVE    "2"             TO  ORCSHKN-KBN
      *    MOVE    "1"             TO  ORCSHKN-KBN3
      *
           CALL    "ORCSHKNSET"    USING   WRK-SPA-AREA
                                           ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
      *
               IF    ( ORCHCM34-HOKEN  =   SPACE )
                   MOVE    1       TO  FLG-ORCSHKNSET (IDX0)
                   MOVE    ZERO    TO  FLG-HKNGENTEI
               ELSE
                   MOVE    ORCSHKN-NAI-HKNNUM (IDX0)
                                   TO  WRK-HKNNUM
                   MOVE    ORCSHKN-NAI-KOHHKNNUM (IDX0 1)
                                   TO  WRK-KOHNUM
      *
                   PERFORM 800-HOKENBUNRUI-SEC
                   IF    ( ORCHCM34-HOKEN  =   WRK-HOKEN )
                       MOVE    1   TO  FLG-ORCSHKNSET (IDX0)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( ORCHCM34-HOKEN  =   "4" OR "5" OR "6" OR "7" )
               MOVE    1           TO  FLG-HKNGENTEI
           END-IF
      *
           .
       1001-KHN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報編集処理
      *****************************************************************
       10011-PTNYUINRRK-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-HCM34-SRYKA-G
                                           WRK-HCM34-NYUINYMD
                                           WRK-HCM34-TAIINYMD
                                           WRK-HCM34-BTUNAME
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCHCM34-EDSRYYMD   TO  PTNYUINRRK-NYUINYMD
           MOVE    ORCHCM34-STSRYYMD   TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               GO  TO  10011-PTNYUINRRK-HEN-EXT
           END-IF
      *
           IF    ( ORCHCM34-SRYKAKBN   =   "1" )
               MOVE    ORCHCM34-SRYKA      TO  SRH-SRYKA
               PERFORM 200111-SRYKA-HEN-SEC
               MOVE    SRH-SRYKAMEI1       TO  WRK-HCM34-SRYKA(2)
           ELSE
               MOVE    PTNYUINRRK-NYUINKA  TO  SRH-SRYKA
               PERFORM 200111-SRYKA-HEN-SEC
               MOVE    SRH-SRYKAMEI1       TO  WRK-HCM34-SRYKA(2)
           END-IF
      *
           PERFORM VARYING IDX22   FROM    1   BY  1
                   UNTIL ( IDX22       >   SNTYMD-MAX )
               IF    ( PTNYUINRRK-RRKNUM   = SNTYMD-RRKNUM (IDX22))
      *
                   MOVE    SNTYMD-NYUINYMD (IDX22)
                                               TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD3         TO  WRK-HCM34-NYUINYMD
      *
                   IF    ( SNTYMD-TAIINYMD (IDX22) NOT =   "99999999" )
                    AND  ( SNTYMD-TAIINYMD (IDX22) NOT =   SPACE  )
                       MOVE    SNTYMD-TAIINYMD (IDX22) TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD3     TO  WRK-HCM34-TAIINYMD
                   END-IF
      *
                   MOVE    SNTYMD-MAX      TO  IDX22
      *
               END-IF
      *
           END-PERFORM
      *
      *    病棟名称
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-5001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-5001-STYUKYMD
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-5001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           STRING  "病棟名："          DELIMITED   BY  SIZE
                   SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
           INTO    WRK-HCM34-BTUNAME
           END-STRING
      *
           .
       10011-PTNYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       10011-SRYKA-HEN-SEC             SECTION.
      *
           INITIALIZE                      WRK-SRYKA-TBL
           MOVE    ZERO                TO  WRK-HCM34-SRYKA-MAX
           MOVE    SPACE               TO  WRK-HCM34-SRYKA-G
      *
           PERFORM 900-SRYACCT-KEY23-SEL-SEC
      *
           MOVE    ZERO                TO  WKSRYACCT-MAX
           INITIALIZE                      WKSRYACCT-REC
           PERFORM UNTIL     ( FLG-SRYACCT =   1 )
      *
               PERFORM VARYING IDX13   FROM    1   BY  1
                       UNTIL ( IDX13       >   WKSRYACCT-MAX )
                        OR   ( IDX13       >   20 )
                        OR   ( ACCT-SRYKA  =   WRK-SRYKA (IDX13) )
                   CONTINUE
               END-PERFORM
      *
               IF    ( IDX13               >   WKSRYACCT-MAX )
                   COMPUTE WKSRYACCT-MAX   =   WKSRYACCT-MAX   +   1
                   IF    ( WKSRYACCT-MAX   >   20 )
                       MOVE    20          TO  WKSRYACCT-MAX
                   ELSE
                       MOVE    ACCT-SRYKA  TO  WRK-SRYKA (WKSRYACCT-MAX)
                       MOVE    ACCT-SRYKA  TO  SRH-SRYKA
                       PERFORM 200111-SRYKA-HEN-SEC
                       MOVE    SRH-SRYKAMEI1
                                           TO  WRK-SRYKA-SRYKAMEI1
                                                         (WKSRYACCT-MAX)
                       MOVE    SRH-SRYKAMEI2
                                           TO  WRK-SRYKA-SRYKAMEI2
                                                         (WKSRYACCT-MAX)
                       MOVE    SRH-SRYKAMEI3
                                           TO  WRK-SRYKA-SRYKAMEI3
                                                         (WKSRYACCT-MAX)
                   END-IF
               END-IF
      *
               PERFORM 900-SRYACCT-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1                   TO  WRK-PNT
           MOVE    ZERO                TO  FLG-LOOPEND
           MOVE    ALL "　"            TO  WRK-HCM34-SRYKA-G
           PERFORM VARYING IDX13   FROM    1   BY  1
                   UNTIL ( FLG-LOOPEND NOT =   ZERO )
                    OR   ( IDX13   >   WKSRYACCT-MAX )
               MOVE    WRK-SRYKA-SRYKAMEI2 (IDX13) TO  WRK-STR
               PERFORM 800-STRLEN-CHK-SEC
               IF    ( WRK-STRLEN  +   WRK-PNT - 1
                                   <   CONST-SRYKAMEI-LEN - 2 )
                OR   ( ( WRK-STRLEN  +   WRK-PNT - 1
                                   <=  CONST-SRYKAMEI-LEN - 2 )
                   AND ( IDX13     >=    WKSRYACCT-MAX ) )
                   MOVE    "／"        TO  WRK-KUGIRI
                   IF    ( IDX13       =   1 )
                       MOVE    SPACE   TO  WRK-KUGIRI
                   END-IF
                   STRING  WRK-KUGIRI      DELIMITED   BY  SPACE
                           WRK-SRYKA-SRYKAMEI2 (IDX13)
                                           DELIMITED   BY  SPACE
                   INTO    WRK-HCM34-SRYKA-G
                   WITH    POINTER     WRK-PNT
                   END-STRING
               ELSE
                   MOVE    1           TO  FLG-LOOPEND
               END-IF
           END-PERFORM
      *
           IF    ( FLG-LOOPEND     NOT =   ZERO )
               COMPUTE WRK-PNT     =    CONST-SRYKAMEI-LEN - 1
               MOVE    "他"            TO   WRK-HCM34-SRYKA-G
                                                   (WRK-PNT:2)
           ELSE
               IF    ( WRK-HCM34-SRYKA (2) =    SPACE )
                   MOVE    WRK-HCM34-SRYKA (1)
                                       TO  WRK-HCM34-SRYKA (2)
                   MOVE    SPACE       TO  WRK-HCM34-SRYKA (1)
               END-IF
           END-IF
      *
           .
       10011-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類編集処理
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
      *
           INITIALIZE              SRH-HKNNUM-DAT
           INITIALIZE              SRH-KOHNUM-DAT
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    ORCHCM34-STSRYYMD   TO  HKN-TEKSTYMD
           MOVE    ORCHCM34-STSRYYMD   TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SRH-HKNNUM-MAX  TO  IDX5
      *            保険
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  NOT =   SRH-HKNNUML (IDX5))
      *                    保険情報編集
                           PERFORM 10011-HKN-HEN-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 10011-HKN-HEN-SEC
                   END-IF
      *
               ELSE
      *            公費
                   MOVE    SRH-KOHNUM-MAX  TO  IDX5
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  =   SRH-KOHNUML (IDX5))
                           IF    ( HKN-HKNNUM  =   "027"  ) 
                             AND ( HKN-PAYKBN  =   "01" OR "02" OR "03")
                               IF ( HKN-PAYKBN
                                           NOT =   SRH-PAYKBNL (IDX5))
      *                            公費情報編集
                                   PERFORM 10011-KOH-HEN-SEC
                               END-IF
                           END-IF
                       ELSE
      *                    公費情報編集
                           PERFORM 10011-KOH-HEN-SEC
                       END-IF
                   ELSE
      *                公費情報編集
                       PERFORM 10011-KOH-HEN-SEC
                   END-IF
               END-IF
      *
      *        保険番号読み込み
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC         NOT =   ZERO )
                   MOVE    1           TO  FLG-HKNNUM
               ELSE
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
               END-IF
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-HKNKOH-CMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       10011-HKN-HEN-SEC          SECTION.
      *
           IF    ( SRH-HKNNUM-MAX      >=  50 )
               GO  TO  10011-HKN-HEN-EXT
           END-IF
      *
           COMPUTE SRH-HKNNUM-MAX  =   SRH-HKNNUM-MAX   +   1
      *
           MOVE    SRH-HKNNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-HKNNUML   (IDX5)
           MOVE    HKN-TANSEIDONAME    TO  SRH-HKNNUMMEIL(IDX5)
      *
           .
       10011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       10011-KOH-HEN-SEC           SECTION.
      *
           IF    ( SRH-KOHNUM-MAX      >=  150 )
               GO  TO  10011-KOH-HEN-EXT
           END-IF
      *
           COMPUTE SRH-KOHNUM-MAX  =   SRH-KOHNUM-MAX  +   1
      *
           MOVE    SRH-KOHNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-KOHNUML (IDX5)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" AND "03" )
               MOVE    "老人保健"      TO  SRH-KOHNUMMEIL (IDX5)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  SRH-KOHNUMMEIL (IDX5)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" OR "03" )
               MOVE    HKN-PAYKBN
                                       TO  SRH-PAYKBNL (IDX5)
           ELSE
               MOVE    SPACE           TO  SRH-PAYKBNL (IDX5)
           END-IF
      *
           .
       10011-KOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       1001-SRYKA-HEN-SEC              SECTION.
      *
           INITIALIZE              SRH-SRYKA-DAT
      *
      *    システム管理マスタ検索処理
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    ORCHCM34-SRYYM      TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( MCP-RC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
      *
               MOVE    IDX5            TO  SRH-SRYKA-MAX
               MOVE    SYS-1005-KBNCD (1 : 2)
                                       TO  SRH-SRYKAL (IDX5)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SRH-SRYKAMEI1L (IDX5)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SRH-SRYKAMEI2L (IDX5)
               MOVE    SYS-1005-SRYKANAME3
                                       TO  SRH-SRYKAMEI3L (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    医療区分、ＡＤＬ点数取得処理
      *****************************************************************
       1001-MEDADL-HEN-SEC             SECTION.
      *
           INITIALIZE                  HCN68S01-AREA
      *
           MOVE    "1"             TO  HCN68S01-RANGE
           MOVE    SPA-PTID        TO  HCN68S01-PTID
           MOVE    ORCHCM34-SRYYM  TO  HCN68S01-SRYYM
           MOVE    ORCHCM34-SRYYM  TO  HCN68S01-SRYYMD
           MOVE    "01"            TO  HCN68S01-SRYYMD(7:)
           CALL    "ORCHCN68S01"       USING
                                       SPA-AREA
                                       HCN68S01-AREA
      *
           .
       1001-MEDADL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    保険組合せ編集処理
           PERFORM 2001-HKNCOMBI-HEN-SEC
      *    保険編集処理
           PERFORM 2001-PTHKN-HEN-SEC
      *    公費編集処理
           PERFORM 2001-PTKOH-HEN-SEC
      *    傷病名編集処理
           IF    ( ORCHCM34-HIDEBYOKBN =   SPACE )
               PERFORM 2001-PTBYOMEI-HEN-SEC
           END-IF
      *    診療行為編集処理
           PERFORM 2001-SRYACT-HEN-SEC
      *
           IF   (( ORCHCM34-PRTCTRLKBN =   "1" )
            AND  ( FLG-HEAD            =   0 ))
            OR  (( CNT-SRYACT-LINE     >   ZERO )
             AND ( CNT-SRYACCT         >   ZERO ))
               PERFORM 290-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       2001-HKNCOMBI-HEN-SEC           SECTION.
      *
           INITIALIZE                  WKHKNCOMBI-AREA
      *
           MOVE    ZERO            TO  CNT-HKNCOMBI-LINE
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   ORCSHKN-HKN-MAX )
      *
               MOVE    1           TO  FLG-TARGET
      *
      *
               IF    ( FLG-ORCSHKNSET (IDX1)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( ORCHCM34-STSRYYMD
                                   <=  ORCSHKN-NAI-TEKEDYMD(IDX1))
                AND  ( ORCHCM34-EDSRYYMD
                                   >=  ORCSHKN-NAI-TEKSTYMD(IDX1))
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   IF    ( CNT-HKNCOMBI-LINE
                                       <    CONST-HKNCOMBI-LINE-MAX )
      *
                       PERFORM 20011-HKNCOMBI-HEN-SEC
                   ELSE
      *
      *                保険組合せ欄に収まりきらない場合、
      *                ワークエリアに退避する
                       IF    ( WKHKNCOMBI-MAX  <   50 )
                           COMPUTE WKHKNCOMBI-MAX  =   WKHKNCOMBI-MAX
                                                   +   1
      *
                           MOVE    WKHKNCOMBI-MAX  TO  IDXA
      *
                           MOVE    ORCSHKN-GMN-HKNCOMBINUM(IDX1)
                                       TO  WKHKNCOMBI-HKNCOMBINUM(IDXA)
                           MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDX1)
                                       TO  WRK-MAE-INPUT
                           PERFORM 800-ZENKAKU-HEN-SEC
                           MOVE    WRK-OUT-INPUT
                                       TO  WKHKNCOMBI-HKNCOMBIMEI(IDXA)
                           MOVE    ORCSHKN-NAI-TEKSTYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKSTYMDS (IDXA)
                           MOVE    ORCSHKN-NAI-TEKEDYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKEDYMDS (IDXA)
                           MOVE    ORCSHKN-GMN-TEKSTYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKSTYMDW (IDXA)
                           MOVE    ORCSHKN-GMN-TEKEDYMD(IDX1)
                                       TO  WKHKNCOMBI-TEKEDYMDW (IDXA)
      *
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( ORCHCM34-HOKEN  =   SPACE   OR  "9" )
            AND  ( ORCHCM34-HIDEHOKATSU    NOT =   "1" )
      *        保険組み合わせ包括分編集処理
               PERFORM 20011-HOUKATU-HEN-SEC
           END-IF
      *
           .
       2001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20011-HKNCOMBI-HEN-SEC           SECTION.
      *
           COMPUTE CNT-HKNCOMBI-LINE   =   CNT-HKNCOMBI-LINE   +   1
      *
           MOVE    CNT-HKNCOMBI-LINE   TO  IDXA
      *
           MOVE    ORCSHKN-GMN-HKNCOMBINUM(IDX1)
                                       TO  HCM341-HKNCOMBINUM(IDXA)
           MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDX1)
                                       TO  HCM341-HKNCOMBIMEI(IDXA)
      *
      *    半角スペースが途中に含まれていた場合の対応
      *    （末尾の全角文字が尻切れにならないようにする）
           MOVE    ZERO                TO  CNT-SPACE
           INSPECT HCM341-HKNCOMBIMEI(IDXA)    TALLYING
                   CNT-SPACE   FOR ALL " "
      *
           DIVIDE  CNT-SPACE   BY  2 GIVING WRK-SHO
           REMAINDER WRK-AMARI
      *
           IF    ( WRK-AMARI   NOT =   ZERO )
               MOVE    1               TO  WRK-STRLEN-MAX
               MOVE    SPACE           TO  WRK-STR
               STRING  HCM341-HKNCOMBIMEI(IDXA)
               DELIMITED   BY  SIZE
               INTO    WRK-STR
               WITH    POINTER WRK-STRLEN-MAX
               END-STRING
               COMPUTE WRK-STRLEN-MAX  =   WRK-STRLEN-MAX
                                       -   1
               MOVE    WRK-STR (1:WRK-STRLEN-MAX - 1)
                                       TO  HCM341-HKNCOMBIMEI(IDXA)
           END-IF
      *
           MOVE    ORCSHKN-GMN-TEKSTYMD(IDX1)
                                       TO  HCM341-TEKSTYMD (IDXA)
           MOVE    ORCSHKN-GMN-TEKEDYMD(IDX1)
                                       TO  HCM341-TEKEDYMD (IDXA)
      *
           .
       20011-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせ包括分編集処理
      *****************************************************************
       20011-HOUKATU-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HOUKATU
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
           MOVE    9999                TO  ACCT-HKNCOMBI
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT = ZERO )
                    OR   ( FLG-HOUKATU NOT = ZERO )
      *
               IF    ( ACCT-ZAITEN     NOT =   ZERO )
                   PERFORM VARYING IDX34   FROM    ORCHCM34-STDAY  BY  1
                           UNTIL ( IDX34   >       ORCHCM34-EDDAY )
                            OR   ( FLG-HOUKATU     =   1 )
                        IF    ( ACCT-DAY ( 1 IDX34)    >   ZERO )
                            MOVE   1       TO  FLG-HOUKATU
                        END-IF
                    END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key22"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-SRYACCT
               ELSE
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-HOUKATU         =   1 )
               IF    ( CNT-HKNCOMBI-LINE
                                       <    CONST-HKNCOMBI-LINE-MAX )
      *
                   COMPUTE CNT-HKNCOMBI-LINE
                       =   CNT-HKNCOMBI-LINE   +   1
      *
                   MOVE    CNT-HKNCOMBI-LINE
                                       TO  IDXA
                   MOVE    "9999"      TO  HCM341-HKNCOMBINUM(IDXA)
                   MOVE    "包括分"    TO  HCM341-HKNCOMBIMEI(IDXA)
                   MOVE    " 00.00.00" TO  HCM341-TEKSTYMD (IDXA)
                   MOVE    " 99.99.99" TO  HCM341-TEKEDYMD (IDXA)
               ELSE
      *
      *            保険組合せ欄に収まりきらない場合、
      *            ワークエリアに退避する
                   IF    ( WKHKNCOMBI-MAX  <   50 )
                       COMPUTE WKHKNCOMBI-MAX  =   WKHKNCOMBI-MAX
                                               +   1
      *
                       MOVE    WKHKNCOMBI-MAX  TO  IDXA
      *
                       MOVE    "9999"
                                       TO  WKHKNCOMBI-HKNCOMBINUM(IDXA)
                       MOVE    "包括分"
                                       TO  WKHKNCOMBI-HKNCOMBIMEI(IDXA)
                       MOVE    "00000000"
                                       TO  WKHKNCOMBI-TEKSTYMDS (IDXA)
                       MOVE    "99999999"
                                       TO  WKHKNCOMBI-TEKEDYMDS (IDXA)
                       MOVE    " 00.00.00"
                                       TO  WKHKNCOMBI-TEKSTYMDW (IDXA)
                       MOVE    " 99.99.99"
                                       TO  WKHKNCOMBI-TEKEDYMDW (IDXA)
      *
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-HOUKATU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       2001-PTHKN-HEN-SEC              SECTION.
      *
           INITIALIZE  WKPTHKNINF-REC
      *
      *    患者保険マスタ検索
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTHKNINF
           ELSE
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           END-IF
      *
           MOVE    ZERO            TO  CNT-HKNKOH-LINE
           PERFORM UNTIL ( FLG-PTHKNINF    NOT =   ZERO )
      *
               MOVE    1           TO  FLG-TARGET
      *
               IF    ( PTHKN-SAKJOKBN   =  SPACE )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( ORCHCM34-STSRYYMD  <=  PTHKN-TEKEDYMD )
                AND  ( ORCHCM34-EDSRYYMD  >=  PTHKN-TEKSTYMD )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               MOVE    ZERO    TO  FLG-HIT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
                        OR   ( FLG-HIT =   1 )
                   IF    ( FLG-ORCSHKNSET (IDX0)   =   1 )
                    AND  ( PTHKN-HKNID =   ORCSHKN-NAI-HKNID (IDX0))
                       MOVE    1   TO  FLG-HIT
                   END-IF
               END-PERFORM
               IF    ( FLG-HIT     =   1 )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
      *
               IF    ( FLG-TARGET      =   1 )
      *
                   IF    ( CNT-HKNKOH-LINE
                                       <    CONST-HKNKOH-LINE-MAX )
                       PERFORM 20011-PTHKN-HEN-SEC
                   ELSE
                       IF    ( WKPTHKNINF-MAX  <   50 )
                           COMPUTE WKPTHKNINF-MAX  =   WKPTHKNINF-MAX
                                                   +   1
                           MOVE    WKPTHKNINF-MAX  TO  IDXA
                           MOVE    PTHKNINF-REC    TO  WKPTHKNINF-OCC
                                                                (IDXA)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTHKNINF
               ELSE
                   MOVE    MCPDATA-REC TO  PTHKNINF-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       2001-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       20011-PTHKN-HEN-SEC             SECTION.
      *
           COMPUTE CNT-HKNKOH-LINE =   CNT-HKNKOH-LINE     +   1
      *
           MOVE    CNT-HKNKOH-LINE     TO  IDXA
      *
           MOVE    PTHKN-HKNNUM        TO  SRH-HKNNUM
           PERFORM 200111-PTHKN-HKNNUM-HEN-SEC
           MOVE    SRH-HKNNUMMEI       TO  HCM341-HKNNAME (IDXA)
      *
           MOVE    PTHKN-HKNJANUM      TO  HCM341-HKNJANUM (IDXA)
      *
           MOVE    PTHKN-HONKZKKBN     TO  WRK-PTHKN-HONKZKKBN
           PERFORM 200111-PTHKN-HONKZKKBN-HEN-SEC
           MOVE    WRK-PTHKN-HONKZKKBNMEI
                                       TO  HCM341-HONKZK (IDXA)
      *
           MOVE    PTHKN-HKNNUM        TO  WRK-PTHKN-HKNNUM
           MOVE    PTHKN-HOJOKBN       TO  WRK-PTHKN-HOJOKBN
           PERFORM 200111-PTHKN-HOJOKBN-HEN-SEC
           MOVE    WRK-PTHKN-HOJOKBNMEI
                                       TO  HCM341-FTNRATE (IDXA)
      *
           MOVE    PTHKN-HKNNUM        TO  WRK-PTHKN-HKNNUM
           MOVE    PTHKN-CONTKBN       TO  WRK-PTHKN-CONTKBN
           PERFORM 200111-PTHKN-CONTKBN-HEN-SEC
           MOVE    WRK-PTHKN-CONTKBNMEI
                                       TO  HCM341-CONTKBN (IDXA)
      *
           MOVE    PTHKN-KIGO          TO  HCM341-KIGO  (IDXA)
           MOVE    PTHKN-NUM           TO  HCM341-NUM   (IDXA)
      *
           .
      *
       20011-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類編集処理
      *****************************************************************
       200111-PTHKN-HKNNUM-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SRH-HKNNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-HKNNUM-MAX )
               IF    ( SRH-HKNNUM  =   SRH-HKNNUML (SRH-IDX) )
                   MOVE    SRH-HKNNUMMEIL (SRH-IDX)
                                       TO  SRH-HKNNUMMEI
                   MOVE    SRH-HKNNUM-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       200111-PTHKN-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分編集処理
      *****************************************************************
       200111-PTHKN-HONKZKKBN-HEN-SEC  SECTION.
      *
           MOVE    SPACE               TO  WRK-PTHKN-HONKZKKBNMEI
      *
           EVALUATE    WRK-PTHKN-HONKZKKBN
           WHEN    "1"
               MOVE    "本人"          TO  WRK-PTHKN-HONKZKKBNMEI
           WHEN    "2"
               MOVE    "家族"          TO  WRK-PTHKN-HONKZKKBNMEI
           END-EVALUATE
      *
           .
       200111-PTHKN-HONKZKKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    補助区分編集処理
      *****************************************************************
       200111-PTHKN-HOJOKBN-HEN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-PTHKN-HOJOKBNMEI
      *
      *    負担割合
           EVALUATE    WRK-PTHKN-HKNNUM
      *    船員
           WHEN    "002"
               IF    ( PTHKN-TEKSTYMD  >=  CONST-H141001 )
                AND  ( WRK-AGE70-YMD   >   ZERO       )
                AND  ( PTHKN-TEKSTYMD  >=  WRK-AGE70-YMD )
                   IF    ( PTHKN-TEKSTYMD  >=  CONST-H181001 )
                       EVALUATE    WRK-PTHKN-HOJOKBN
                       WHEN    "7"
                           MOVE    "３割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "8"
                           MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "9"
                           MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "A" 
                           MOVE    "１割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "B" 
                           MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "C" 
                           MOVE    "１割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "D" 
                           MOVE    "２割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "E" 
                           MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "F" 
                           MOVE    "２割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "G" 
                           MOVE    "３割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "H" 
                           MOVE    "３割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "I" 
                           MOVE    "３割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                       END-EVALUATE
                   ELSE
                       EVALUATE    WRK-PTHKN-HOJOKBN
                       WHEN    "8"
                           MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "9"
                           MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "A" 
                           MOVE    "１割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "B" 
                           MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "C" 
                           MOVE    "１割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "D" 
                           MOVE    "２割職務"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "E" 
                           MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "F" 
                           MOVE    "２割通勤"  TO  WRK-PTHKN-HOJOKBNMEI
                       END-EVALUATE
                   END-IF
               ELSE
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "1"
                       MOVE    "職務"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "2"
                       MOVE    "下船"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "3"
                       MOVE    "通勤"      TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               END-IF
      *    国保
           WHEN    "060"
           WHEN    "068"
               EVALUATE    WRK-PTHKN-HOJOKBN
               WHEN    "0"
                   MOVE    "無し"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "1"
                   MOVE    "１割"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "2"
                   MOVE    "２割"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "3" 
                   MOVE    "３割"          TO  WRK-PTHKN-HOJOKBNMEI
               END-EVALUATE
      *    共済
           WHEN    "031"
           WHEN    "032"
           WHEN    "033"
           WHEN    "034"
               IF    ( PTHKN-TEKSTYMD  >=  CONST-H141001 )
                AND  ( WRK-AGE70-YMD   >   ZERO       )
                AND  ( PTHKN-TEKSTYMD  >=  WRK-AGE70-YMD )
                   IF    ( PTHKN-TEKSTYMD  >=  CONST-H181001 )
                       EVALUATE    WRK-PTHKN-HOJOKBN
                       WHEN    "7"
                           MOVE    "３割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "8"
                           MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "9"
                           MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "B"
                           MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "E"
                           MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "H"
                           MOVE    "３割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       END-EVALUATE
                   ELSE
                       EVALUATE    WRK-PTHKN-HOJOKBN
                       WHEN    "8"
                           MOVE    "２割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "9"
                           MOVE    "１割"      TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "B"
                           MOVE    "１割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       WHEN    "E"
                           MOVE    "２割下船"  TO  WRK-PTHKN-HOJOKBNMEI
                       END-EVALUATE
                   END-IF
               ELSE
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "2"
                       MOVE    "下船３月"  TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               END-IF
      *    後期高齢者
           WHEN    "039"
           WHEN    "040"
               EVALUATE    WRK-PTHKN-HOJOKBN
               WHEN    "1"
                   MOVE    "１割"          TO  WRK-PTHKN-HOJOKBNMEI
               WHEN    "3" 
                   MOVE    "３割"          TO  WRK-PTHKN-HOJOKBNMEI
               END-EVALUATE
      *    その他
           WHEN    OTHER
      *        自費
               IF    ( PTHKN-HKNNUM (1:2) =   "98" )
                   EVALUATE    WRK-PTHKN-HOJOKBN
                   WHEN    "1"
                       MOVE    "課税"      TO  WRK-PTHKN-HOJOKBNMEI
                   WHEN    "2"
                       MOVE    "非課"      TO  WRK-PTHKN-HOJOKBNMEI
                   END-EVALUATE
               ELSE
      *            Ｈ１４．１０ 改正より
                   IF    ( PTHKN-TEKSTYMD        >=    CONST-H141001 )
                    AND  ( WRK-AGE70-YMD         >     ZERO       )
                    AND  ( PTHKN-TEKSTYMD        >=    WRK-AGE70-YMD)
                    AND  ( PTHKN-HKNNUM (1:1) NOT =    "9")
                       IF    ( PTHKN-TEKSTYMD  >=  CONST-H181001 )
                           EVALUATE    WRK-PTHKN-HOJOKBN
                           WHEN    "8"
                               MOVE    "２割"  TO  WRK-PTHKN-HOJOKBNMEI
                           WHEN    "7"
                               MOVE    "３割"  TO  WRK-PTHKN-HOJOKBNMEI
                           WHEN    "9"
                               MOVE    "１割"  TO  WRK-PTHKN-HOJOKBNMEI
                           END-EVALUATE
                       ELSE
                           EVALUATE    WRK-PTHKN-HOJOKBN
                           WHEN    "8"
                               MOVE    "２割"  TO  WRK-PTHKN-HOJOKBNMEI
                           WHEN    "9"
                               MOVE    "１割"  TO  WRK-PTHKN-HOJOKBNMEI
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
           END-EVALUATE
      *
           .
       200111-PTHKN-HOJOKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    継続区分編集処理
      *****************************************************************
       200111-PTHKN-CONTKBN-HEN-SEC    SECTION.
      *
           MOVE    SPACE                   TO  WRK-PTHKN-CONTKBNMEI
      *
           IF    ( WRK-PTHKN-HKNNUM    =   "003" )
               EVALUATE WRK-PTHKN-CONTKBN
               WHEN    "1"
                   MOVE    "継続"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "2"
                   MOVE    "任継"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "3"
                   MOVE    "特別"          TO  WRK-PTHKN-CONTKBNMEI
               END-EVALUATE
           ELSE
               EVALUATE WRK-PTHKN-CONTKBN
               WHEN    "1"
                   MOVE    "継続"          TO  WRK-PTHKN-CONTKBNMEI
               WHEN    "2"
                   MOVE    "任継"          TO  WRK-PTHKN-CONTKBNMEI
               END-EVALUATE
           END-IF
      *
           .
       200111-PTHKN-CONTKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       2001-PTKOH-HEN-SEC              SECTION.
      *
           INITIALIZE  WKPTKOHINF-REC
      *
      *    患者公費マスタ検索
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                  PTKOHINF-REC
           MOVE    PTINF-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    PTINF-PTID          TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTKOHINF
           ELSE
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTKOHINF    NOT =   ZERO )
      *
               MOVE    1               TO  FLG-TARGET
      *
               IF    ( PTKOH-SAKJOKBN  =  SPACE )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-TARGET
               END-IF
      *
               IF    ( ORCHCM34-STSRYYMD  <=  PTKOH-TEKEDYMD )
                AND  ( ORCHCM34-EDSRYYMD  >=  PTKOH-TEKSTYMD )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-TARGET
               END-IF
      *
               MOVE    ZERO    TO  FLG-HIT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
                        OR   ( FLG-HIT =   1 )
                   IF    ( FLG-ORCSHKNSET (IDX0)   =   1 )
                    AND  ( PTKOH-KOHID = ORCSHKN-NAI-KOHID (IDX0 1)
                                     OR  ORCSHKN-NAI-KOHID (IDX0 2)
                                     OR  ORCSHKN-NAI-KOHID (IDX0 3)
                                     OR  ORCSHKN-NAI-KOHID (IDX0 4))
                       MOVE    1   TO  FLG-HIT
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-HIT     =   1 )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
                   IF    ( CNT-HKNKOH-LINE
                                       <    CONST-HKNKOH-LINE-MAX )
                       PERFORM 20011-PTKOH-HEN-SEC
                   ELSE
                       IF    ( WKPTKOHINF-MAX  <   50 )
                           COMPUTE WKPTKOHINF-MAX  =   WKPTKOHINF-MAX
                                                   +   1
                           MOVE    WKPTKOHINF-MAX  TO  IDXA
                           MOVE    PTKOHINF-REC    TO  WKPTKOHINF-OCC
                                                                (IDXA)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTKOHINF
               ELSE
                   MOVE    MCPDATA-REC TO  PTKOHINF-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-PTKOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       20011-PTKOH-HEN-SEC             SECTION.
      *
           COMPUTE CNT-HKNKOH-LINE =   CNT-HKNKOH-LINE     +   1
      *
           MOVE    CNT-HKNKOH-LINE     TO  IDXA
      *
           MOVE    PTKOH-KOHNUM        TO  SRH-KOHNUM
           MOVE    PTKOH-PAYKBN        TO  SRH-PAYKBN
           PERFORM 200111-PTKOH-KOHNUM-HEN-SEC
           MOVE    SRH-KOHNUMMEI       TO  HCM341-HKNNAME (IDXA)
      *
      *
           MOVE    PTKOH-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  HCM341-KIGO  (IDXA)
      *
           MOVE    PTKOH-JKYSNUM           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  HCM341-NUM   (IDXA)
      *
           .
      *
       20011-PTKOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の種類編集処理
      *****************************************************************
       200111-PTKOH-KOHNUM-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SRH-KOHNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-KOHNUM-MAX )
               IF    ( SRH-KOHNUM  =   "027" )
                AND  ( SRH-PAYKBN  =   "01" OR "02" OR "03" )
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBN  =   SRH-PAYKBNL (SRH-IDX) )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               ELSE
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBNL (SRH-IDX)   =   SPACE )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-PTKOH-KOHNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       2001-PTBYOMEI-HEN-SEC           SECTION.
      *
      *    患者病名マスタ検索
           MOVE    ZERO                TO  FLG-PTBYOMEI
           INITIALIZE                  PTBYOMEI-REC
           MOVE    PTINF-HOSPNUM        TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           IF    ( ORCHCM34-SRYKAKBN   =   "1" )
               MOVE    ORCHCM34-SRYKA  TO  PTBYO-SRYKA
           ELSE
               MOVE    "__"            TO  PTBYO-SRYKA
           END-IF
           MOVE    ORCHCM34-NYUGAIKBN  TO  PTBYO-NYUGAIKBN
      *
           MOVE    ORCHCM34-STSRYYMD   TO  PTBYO-TENKIYMD
      *
           MOVE    ORCHCM34-EDSRYYMD   TO  WRK-SYMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD            TO  PTBYO-SRYYMD
           MOVE    SPACE               TO  PTBYO-TENKIKBN
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC           NOT =   ZERO )
               MOVE    1               TO  FLG-PTBYOMEI
           ELSE
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
           END-IF
      *
           MOVE    ZERO            TO  CNT-BYOMEI-LINE
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
      *
               MOVE    1           TO  FLG-TARGET
               INITIALIZE              WRK-BYOMEI-G
      *
               IF    ( ORCHCM34-HOKEN  NOT =   SPACE )
                   IF   ( PTBYO-HKNCOMBI       =   ZERO )
                    AND ( PTBYO-KOHID          =   ZERO )
                       IF    ( FLG-HKNGENTEI   =   1 )
                           MOVE    ZERO    TO FLG-TARGET
                       END-IF
                   ELSE
                       IF   ( PTBYO-HKNCOMBI   NOT =   ZERO )
                           MOVE    ZERO    TO  FLG-HIT
                           PERFORM VARYING IDX0    FROM    1   BY  1
                                   UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
                                    OR   ( FLG-HIT =   1 )
      *
                               IF    ( PTBYO-HKNCOMBI
                                   =   ORCSHKN-GMN-HKNCOMBINUM (IDX0))
      *
                                   MOVE    1       TO  FLG-HIT
      *
                                   IF    ( FLG-ORCSHKNSET (IDX0)
                                                   =   1 )
                                       CONTINUE
                                   ELSE
                                       MOVE    ZERO    TO FLG-TARGET
                                   END-IF
                               END-IF
                           END-PERFORM
                           IF    ( FLG-HIT =    ZERO )
                               MOVE    ZERO    TO FLG-TARGET
                           END-IF
                       END-IF
      *
                       IF    ( PTBYO-KOHID     NOT =   ZERO )
                           MOVE    ZERO    TO  FLG-HIT
                           PERFORM VARYING IDX0    FROM    1   BY  1
                                   UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
      *
                               IF    ( PTBYO-KOHID
                                   =   ORCSHKN-NAI-KOHID (IDX0 1))
      *
                                   MOVE    1       TO  FLG-HIT
      *
                                   IF    ( FLG-ORCSHKNSET (IDX0)  = 1 )
                                       CONTINUE
                                   ELSE
                                       MOVE    ZERO    TO FLG-TARGET
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
      *
                   MOVE    PTBYO-BYOMEI    TO  WRK-PTBYO-BYOMEI
                   MOVE    PTBYO-BYOMEIMOJI
                                           TO  WRK-PTBYO-BYOMEIMOJI
                   MOVE    PTBYO-UTAGAIFLG TO  WRK-PTBYO-UTAGAIFLG
                   MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  WRK-PTBYO-HOSOKU-COMMENT
                   PERFORM 20011-PTBYOMEI-BYOMEI-HEN-SEC
                   MOVE    WRK-PTBYO-BYOMEI
                                           TO  WRK-BYOMEI
      *
                   PERFORM VARYING IDX36   FROM    1   BY  1
                           UNTIL ( IDX36   >   3 )
                            OR   ( WRK-BYOMEI-LINE (IDX36) =   SPACE )
      *
                       COMPUTE WRK-BYOMEI-MAX  =   WRK-BYOMEI-MAX  +   1
      *
                   END-PERFORM
      *
                   IF    ( CONST-BYOMEI-LINE-MAX
                              >=   CNT-BYOMEI-LINE + WRK-BYOMEI-MAX )
                       PERFORM 20011-PTBYOMEI-HEN-SEC
                   ELSE
                       IF    ( WKPTBYOMEI-MAX  <   300 )
                           COMPUTE WKPTBYOMEI-MAX  =   WKPTBYOMEI-MAX
                                                   +   1
                           MOVE    WKPTBYOMEI-MAX  TO  IDXA
                           MOVE    PTBYOMEI-REC    TO  WKPTBYOMEI-OCC
                                                                (IDXA)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC           NOT =   ZERO )
                   MOVE    1           TO  FLG-PTBYOMEI
               ELSE
                   MOVE    MCPDATA-REC TO  PTBYOMEI-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-PTBYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       20011-PTBYOMEI-HEN-SEC          SECTION.
      *
           MOVE    CNT-BYOMEI-LINE         TO  IDXA
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    WRK-BYOMEI-LINE (1)     TO  HCM341-BYOMEI2 (IDXA)
      *
           MOVE    PTBYO-SRYKA             TO  SRH-SRYKA
           PERFORM 200111-SRYKA-HEN-SEC
           MOVE    SRH-SRYKAMEI2(1:4)      TO  HCM341-BYOSRYKA (IDXA)
      *
           IF    ( PTBYO-SYUBYOFLG         =   "1" )
               MOVE    "主"                TO  HCM341-SYUBYO (IDXA)
           END-IF
      *
           MOVE    PTBYO-SRYYMD            TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  HCM341-SRYYMD (IDXA)
      *
           MOVE    PTBYO-TENKIKBN          TO  WRK-PTBYO-TENKIKBN
           PERFORM 200111-PTBYO-TENKIKBN-HEN-SEC
           MOVE    WRK-PTBYO-TENKIKBNMEI   TO  HCM341-TENKI  (IDXA)
      *
           PERFORM VARYING IDX36    FROM   2   BY  1
                   UNTIL ( IDX36    >   WRK-BYOMEI-MAX )
      *
               COMPUTE IDXA    =   IDXA    +   1
               MOVE    WRK-BYOMEI-LINE (IDX36)
                                           TO  HCM341-BYOMEI2 (IDXA)
      *
           END-PERFORM
      *
           MOVE    IDXA                    TO  CNT-BYOMEI-LINE
      *
           .
      *
       20011-PTBYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       20011-PTBYOMEI-BYOMEI-HEN-SEC       SECTION.
      *
           MOVE   WRK-PTBYO-UTAGAIFLG  TO  SMKBYO-UTAGAIFLG
           MOVE   WRK-PTBYO-HOSOKU-COMMENT
                                       TO  SMKBYO-HOSOKU-COMMENT
           MOVE    WRK-PTBYO-BYOMEI    TO  SMKBYO-BYOMEI
      *
           CALL   "ORCSMKBYOMEI"   USING
                                   ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI       TO  WRK-PTBYO-BYOMEI
      *
           MOVE    WRK-PTBYO-BYOMEI    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-PTBYO-BYOMEI
      *
           .
       20011-PTBYOMEI-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       200111-SRYKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE                   TO  SRH-SRYKAMEI1
                                               SRH-SRYKAMEI2
                                               SRH-SRYKAMEI3
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-SRYKA-MAX )
               IF    ( SRH-SRYKA   =   SRH-SRYKAL (SRH-IDX) )
                   MOVE    SRH-SRYKAMEI1L (SRH-IDX)
                                           TO  SRH-SRYKAMEI1
                   MOVE    SRH-SRYKAMEI2L (SRH-IDX)
                                           TO  SRH-SRYKAMEI2
                   MOVE    SRH-SRYKAMEI3L (SRH-IDX)
                                           TO  SRH-SRYKAMEI3
                   MOVE    SRH-SRYKA-MAX   TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       200111-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    転帰事由編集処理
      *****************************************************************
       200111-PTBYO-TENKIKBN-HEN-SEC     SECTION.
      *
           MOVE    SPACE                   TO  WRK-PTBYO-TENKIKBNMEI
      *
           EVALUATE    WRK-PTBYO-TENKIKBN
           WHEN    "1"
               MOVE    "治ゆ"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "2"
               MOVE    "死亡"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "3"
               MOVE    "中止"              TO  WRK-PTBYO-TENKIKBNMEI
           WHEN    "8"
               MOVE    "移行"              TO  WRK-PTBYO-TENKIKBNMEI
           END-EVALUATE
      *
           .
       200111-PTBYO-TENKIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       2001-SRYACT-HEN-SEC             SECTION.
      *
      *    各欄からあふれた情報を診療行為欄に編集を行う
           PERFORM 20011-AFURE-HEN-SEC
      *
           PERFORM 20011-HCM34SRY-HEN-SEC
           PERFORM 20011-HCM34ACCT-HEN-SEC
      *
      *    入院の場合は入院履歴を編集
      *    また、入院会計の保険組合せを取得
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               PERFORM 2002-NYUINRRK-HEN-SEC
               PERFORM 2002-NACCT-HEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-KEY
                                           WRK-HKNCOMBI-KEY-MAE
                                           WRK-HKNNUM
                                           WRK-KOHNUM
           INITIALIZE                      WKCOMB-REC
      *
      *    診療会計マスターを編集
           OPEN    INPUT               HCM34ACCT-FILE
           MOVE    ZERO                TO  FLG-HCM34ACCT
           PERFORM 900-HCM34ACCT-READ-SEC
      *
           MOVE    ZERO                TO  WKSRYACCT-MAX
           INITIALIZE                      WKSRYACCT-REC
           PERFORM UNTIL ( FLG-HCM34ACCT   =   1 )
                    OR   ( WKSRYACCT-MAX  >=   CONST-SRYACCT-MAX )
      *
               MOVE    HCM34ACCT-DAT   TO  SRYACCT-REC
      *
               PERFORM 20011-SRYACT-MATOME-SEC
      *
               PERFORM 900-HCM34ACCT-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   HCM34ACCT-FILE
      *
      *    診療行為並び替え処理
           PERFORM 20011-SRYACT-SORT-SEC
      *
           OPEN    INPUT   HCM34SRY-FILE
      *
           MOVE    ZERO                TO  FLG-BRMSAGAKU-HEN
                                           FLG-BRMSAGAKU-TTL-HEN
                                           FLG-COMMENT-HEN
                                           FLG-JIHI-HEN
                                           FLG-RIHA-HEN
           MOVE    ZERO                TO  CNT-COMB-ACCT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKSRYACCT-MAX )
      *
               PERFORM 20011-SRYACT-HEN-SEC
      *
           END-PERFORM
      *
           CLOSE   HCM34SRY-FILE
      *
      *
           IF   ( FLG-COMMENT-HEN   =   ZERO )
               PERFORM 200111-TEN-TOTAL-HEN-SEC
               PERFORM 200211-SENTEI-PTFTN-HEN-SEC
               PERFORM 200211-RECECOM-HEN-SEC
           END-IF
      *
           IF  (   FLG-BRMSAGAKU-TTL-HEN   =   ZERO  )
               PERFORM 200211-BRMSAGAKU-TOTAL-HEN-SEC
           END-IF
      *
           PERFORM 200111-JIHI-TOTAL-HEN-SEC
      *
           .
       2001-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイル編集処理
      *****************************************************************
       20011-HCM34SRY-HEN-SEC          SECTION.
      *
           OPEN    OUTPUT  HCM34SRY-FILE
      *
           PERFORM 900-SRYACT-KEY4-SEL-SEC
           IF    ( FLG-SRYACT      =   ZERO )
               PERFORM 900-SRYACT-KEY4-FET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               INITIALIZE                  HCM34SRY-REC
      *
               MOVE    SRY-PTID        TO  HCM34SRY-ISM-PTID
               MOVE    SRY-ZAINUM      TO  HCM34SRY-ISM-ZAINUM
               MOVE    SRY-RENNUM      TO  HCM34SRY-ISM-RENNUM
               MOVE    SRYACT-REC      TO  HCM34SRY-DAT
      *
               PERFORM VARYING IDX9    FROM    1   BY  1
                       UNTIL ( IDX9    >   5 )
      *
                   IF    ( SRY-INPUTNUM (IDX9) NOT =   ZERO )
                       PERFORM 200111-COMMENT-EDIT-SEC
                       MOVE    WRK-COMMENT
                                       TO  HCM34SRY-COMMENT (IDX9)
                   END-IF
      *
               END-PERFORM
      *
               WRITE   HCM34SRY-REC
      *
               PERFORM 900-SRYACT-KEY4-FET-SEC
      *
           END-PERFORM
      *
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   HCM34SRY-FILE
      *
           .
       20011-HCM34SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       200111-COMMENT-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-COMMENT
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD   (IDX9)  TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX9)  TO  PTCOM-RENNUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               CONTINUE
           ELSE
      *
               MOVE    MCPDATA-REC     TO  PTCOM-REC
      *
               IF    ( PTCOM-INPUTCOMENT   NOT =   SPACE )
                   IF    ( SRY-SRYCD   (IDX9)(1:3) =   "001" )
                       STRING "【"                 DELIMITED   BY  SIZE
                              PTCOM-INPUTCOMENT    DELIMITED   BY  SPACE
                              "】"                 DELIMITED   BY  SIZE
                       INTO   WRK-COMMENT
                       END-STRING
                   ELSE
                       MOVE    PTCOM-INPUTCOMENT
                                       TO  WRK-COMMENT
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       200111-COMMENT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計ファイル編集処理
      *****************************************************************
       20011-HCM34ACCT-HEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSRIHASTDAYAREA
           CALL    "ORCSRIHASTDAY"     USING
                                       ORCSRIHASTDAYAREA
                                       SPA-AREA
      *
           OPEN    OUTPUT  HCM34ACCT-FILE
           OPEN    I-O     HCM34SRY-FILE
      *
      *    診療会計マスターを編集
           PERFORM 900-SRYACCT-KEY20-SEL-SEC
           IF    ( FLG-SRYACCT     =   ZERO )
               PERFORM 900-SRYACCT-KEY20-FET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT     NOT =   ZERO )
      *
               MOVE    ACCT-HKNCOMBI       TO  KEY-ACCT-HKNCOMBI
      *
               INITIALIZE                      ORCSTENTEK3AREA
               IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
                AND  ( ORCHCM34-SRYKAKBN   =   SPACE )
                   MOVE    ACCT-HOSPNUM    TO  LNK-ORCSTEN3-HOSPNUM
                   MOVE    ACCT-NYUGAIKBN  TO  LNK-ORCSTEN3-NYUGAIKBN
                   MOVE    ACCT-PTID       TO  LNK-ORCSTEN3-PTID
                   MOVE    ACCT-SRYYM      TO  LNK-ORCSTEN3-SRYYM
                   MOVE    ACCT-HKNCOMBI   TO  LNK-ORCSTEN3-HKNCOMBI
                   CALL    "ORCSTENTEK3"   USING
                                           ORCSTENTEK3AREA
                                           SPA-AREA
      *
                   PERFORM 280-TENTEKI-SEC
      *
               END-IF
      *
               PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
                        OR   ( ACCT-HKNCOMBI
                                           NOT = KEY-ACCT-HKNCOMBI )
      *
                   MOVE    1               TO  FLG-ACCEPT
      *
                   PERFORM VARYING IDXZ    FROM    1   BY  1
                           UNTIL ( IDXZ    >   100 )
                            OR   ( LNK-ORCSTEN3-ZAINUM (IDXZ)
                                                       =   ZERO )
                            OR   ( FLG-ACCEPT          =   ZERO )
      *
                       IF    ( ACCT-ZAINUM
                                       =   LNK-ORCSTEN3-ZAINUM (IDXZ))
                           MOVE    ZERO     TO  FLG-ACCEPT
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( FLG-ACCEPT      NOT =   ZERO )
                       PERFORM 200111-HCM34ACCT-HEN-SEC
                   END-IF
      *
                   PERFORM 900-SRYACCT-KEY20-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   HCM34ACCT-FILE
           CLOSE   HCM34SRY-FILE
      *
           .
       20011-HCM34ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計ファイル編集処理
      *****************************************************************
       200111-HCM34ACCT-HEN-SEC         SECTION.
      *
           INITIALIZE                  HCM34ACCT-REC
      *
      *    診療行為マスタ処理
           PERFORM 200111-SRYACT-SEL-SEC
      *
           IF    ( FLG-YAKUSOKU        =   1 )
               COMPUTE ACCT-MEISAISU
                   =   ACCT-MEISAISU   -   1
           END-IF
      *
           MOVE    SRYACCT-REC     TO  HCM34ACCT-DAT
           INITIALIZE                  HCM34ACCT-DATPLUS
      *
           MOVE    WRK-RUIKEITOTAL TO  HCM34ACCT-RUIKEITOTAL
           MOVE    WRK-RUIKEITOTAL2 TO  HCM34ACCT-RUIKEITOTAL2
           MOVE    WRK-JIHIMONEYTOTAL
                                   TO  HCM34ACCT-JIHIMONEYTOTAL
           MOVE    WRK-SRYCD       TO  HCM34ACCT-SRT-SRYCD
           MOVE    WRK-KOKUJISKBKBN2
                                   TO  HCM34ACCT-SRT-KOKUJISKBKBN2
           MOVE    WRK-SRYSYUKBN   TO  HCM34ACCT-SRT-SRYSYUKBN
           IF    ( FLG-SRYSTATUS   =   2 )
               MOVE    1           TO  HCM34ACCT-SRT-SRYCOMMENT
           END-IF
           MOVE    "1"             TO  HCM34ACCT-ACCTKBN
           IF        ( ACCT-SRYKBN     =   "90" OR "92" )
                   MOVE    "6"     TO  HCM34ACCT-ZAISKBKBN
           ELSE
               IF    ( ACCT-SRYKBN     =   "97" )
                   MOVE    "4"     TO  HCM34ACCT-ZAISKBKBN
               END-IF
           END-IF
      *
           IF        ( ACCT-SRYKBN     =   "90" OR "92" )
               MOVE    3           TO  HCM34ACCT-PRIORITY
           ELSE
               MOVE    ZERO        TO  HCM34ACCT-PRIORITY
           END-IF
      *
           MOVE    ZERO            TO  HCM34ACCT-PRIORITY2
                                       HCM34ACCT-PRIORITY3
           MOVE    WRK-TNSTEN      TO  HCM34ACCT-TNSTEN
           MOVE    FLG-SRYSTATUS   TO  HCM34ACCT-SRYSTATUS
           MOVE    FLG-MINUS       TO  HCM34ACCT-MINUS
           MOVE    FLG-RIHA        TO  HCM34ACCT-RIHA
           MOVE    FLG-RIHA-SYS    TO  HCM34ACCT-RIHA-SYS
      *
           IF    ( ACCT-SRYKBN     =  "21" OR "22" OR "23" )
            OR   ( WRK-SRYSYUKBN   =  "148" )
               IF    ( WRK-SRYSYUKBN (3:1)    NOT =    "3" )
                AND  ( WRK-SRYSYUKBN (3:1)    NOT =    "1" )
                AND  ( WRK-SRYSYUKBN          NOT =    "294" )
                AND  ( ACCT-ZAITEN =  ZERO )
                   MOVE    "1"     TO  HCM34ACCT-INGAIKBN
               END-IF
           END-IF
      *
           WRITE   HCM34ACCT-REC
      *
           .
       200111-HCM34ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    同一剤まとめ編集処理
      *****************************************************************
       20011-SRYACT-MATOME-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAIMATOME
      *
           SET     WKSRYACCT-IDX       TO  1
           SEARCH  WKSRYACCT-OCC   VARYING WKSRYACCT-IDX
           AT  END
      *
                   MOVE    ZERO        TO  FLG-ZAIMATOME
      *
           WHEN    WKACCT-HOSPNUM (WKSRYACCT-IDX)
                                       =   SPACE
      *
                   MOVE    ZERO        TO  FLG-ZAIMATOME
      *
           WHEN    ACCT-HOSPNUM         =   WKACCT-HOSPNUM
                                                       (WKSRYACCT-IDX)
            AND    ACCT-NYUGAIKBN      =   WKACCT-NYUGAIKBN
                                                       (WKSRYACCT-IDX)
            AND    ACCT-PTID           =   WKACCT-PTID
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYYM          =   WKACCT-SRYYM
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYKBN         =   WKACCT-SRYKBN
                                                       (WKSRYACCT-IDX)
            AND    ACCT-HKNCOMBI       =   WKACCT-HKNCOMBI
                                                       (WKSRYACCT-IDX)
            AND    ACCT-ZAITEN         =   WKACCT-ZAITEN 
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SRYCDTOTAL     =   WKACCT-SRYCDTOTAL
                                                       (WKSRYACCT-IDX)
            AND    ACCT-SURYOUTOTAL    =   WKACCT-SURYOUTOTAL
                                                       (WKSRYACCT-IDX)
            AND    ACCT-MEISAISU       =   WKACCT-MEISAISU
                                                       (WKSRYACCT-IDX)
            AND    ACCT-JIHOKBN        =   WKACCT-JIHOKBN
                                                       (WKSRYACCT-IDX)
            AND    HCM34ACCT-RUIKEITOTAL
                                       =   WKACCT-RUIKEITOTAL
                                                       (WKSRYACCT-IDX)
            AND    HCM34ACCT-RUIKEITOTAL2
                                       =   WKACCT-RUIKEITOTAL2
                                                       (WKSRYACCT-IDX)
            AND    HCM34ACCT-JIHIMONEYTOTAL
                                       =   WKACCT-JIHIMONEYTOTAL
                                                       (WKSRYACCT-IDX)
               INITIALIZE  COMMENT-CHK-AREA
               MOVE    ACCT-ZAINUM     TO  COMMENT-ZAINUM (1)
               MOVE    WKACCT-ZAINUM (WKSRYACCT-IDX)
                                       TO  COMMENT-ZAINUM (2)
               PERFORM 200111-PTCOM-CHK-SEC
               IF    ( FLG-COMMENT-CHK =   ZERO )
                   MOVE    1           TO  FLG-ZAIMATOME
               ELSE
                   MOVE    ZERO        TO  FLG-ZAIMATOME
               END-IF
           END-SEARCH
      *
           IF    ( ACCT-SRYCDTOTAL     =   630010002 )
                   MOVE    ZERO        TO  FLG-ZAIMATOME
           END-IF
      *
           IF    ( FLG-ZAIMATOME       =   1 )
               COMPUTE WKACCT-ZAIKAISU  (WKSRYACCT-IDX)
                   =   WKACCT-ZAIKAISU  (WKSRYACCT-IDX)
                   +   ACCT-ZAIKAISU
               PERFORM VARYING IDX1    FROM    ORCHCM34-STDAY   BY  1
                       UNTIL ( IDX1    >   ORCHCM34-EDDAY )
                   COMPUTE WKACCT-DAY (WKSRYACCT-IDX 1 IDX1)
                        =  WKACCT-DAY (WKSRYACCT-IDX 1 IDX1)
                        +   ACCT-DAY (1 IDX1)
               END-PERFORM
           ELSE
               COMPUTE WKSRYACCT-MAX   =   WKSRYACCT-MAX   +   1
               MOVE    HCM34ACCT-DAT   TO  WKSRYACCT-OCC (WKSRYACCT-MAX)
      *
           END-IF
      *
           .
       20011-SRYACT-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントチェック処理
      *****************************************************************
       200111-PTCOM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-COMMENT-CHK
      *
           MOVE    1               TO  COMMENT-IDX
           PERFORM 2001111-PTCOM-HEN-SEC
      *
           MOVE    2               TO  COMMENT-IDX
           PERFORM 2001111-PTCOM-HEN-SEC
      *
           IF    ( COMMENT-MAX (1)         =   ZERO )
            AND  ( COMMENT-MAX (2)         =   ZERO )
                CONTINUE
           ELSE
               IF    ( COMMENT-MAX (1)         =   COMMENT-MAX (2) )
                AND  ( COMMENT-COMMENT-G (1)   =   COMMENT-COMMENT-G(2))
                    CONTINUE
               ELSE
                    MOVE   1           TO  FLG-COMMENT-CHK
               END-IF
           END-IF
      *
           .
       200111-PTCOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者コメント編集処理
      *****************************************************************
       2001111-PTCOM-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTCOM
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPNUM          TO  PTCOM-HOSPNUM
           MOVE    SPA-PTID            TO  PTCOM-PTID
           MOVE    COMMENT-ZAINUM (COMMENT-IDX)
                                       TO  PTCOM-ZAINUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-PTCOM
           ELSE
               MOVE    MCPDATA-REC     TO  PTCOM-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTCOM   NOT =   ZERO )
                    OR   ( COMMENT-MAX (COMMENT-IDX)
                                           >   10 )
      *
               COMPUTE COMMENT-MAX (COMMENT-IDX)
                   =   COMMENT-MAX (COMMENT-IDX)   +   1
               MOVE    COMMENT-MAX (COMMENT-IDX)
                                       TO  IDX21
      *
               MOVE    PTCOM-INPUTCOMENT
                                   TO  COMMENT-COMMENT
                                               (COMMENT-IDX IDX21)
      *
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-PTCOM
               ELSE
                   MOVE    MCPDATA-REC TO  PTCOM-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001111-PTCOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為並び替え処理
      *****************************************************************
       20011-SRYACT-SORT-SEC           SECTION.
      *
           INITIALIZE                      SRT-SRYACCT
      *
      *    ソート用ワークにキー項目を退避する
           PERFORM VARYING SRT-IDX1    FROM    1   BY  1
                   UNTIL ( SRT-IDX1    >   WKSRYACCT-MAX )
      *
               MOVE    WKACCT-SRT-SRYCD (SRT-IDX1)
                                       TO  WRK-SRYCD
               MOVE    WKACCT-SRT-SRYSYUKBN (SRT-IDX1)
                                       TO  WRK-SRYSYUKBN
      *
               MOVE    WKSRYACCT-OCC (SRT-IDX1)
                                       TO  SRYACCT-REC
      *
               MOVE    SRT-IDX1        TO  SRT-SRYACCT-IDX (SRT-IDX1)
               MOVE    ACCT-HKNCOMBI   TO  SRT-SRYACCT-HKNCOMBI
                                                           (SRT-IDX1)
               IF    ( ACCT-SRYKBN     =   "95" OR "96" )
                   MOVE    2           TO  SRT-SRYACCT-JIHIKBN
                                                           (SRT-IDX1)
               ELSE
                   MOVE    ZERO        TO  SRT-SRYACCT-JIHIKBN
                                                           (SRT-IDX1)
               END-IF
      *
               MOVE    ACCT-SRYKBN     TO  SRT-SRYACCT-SRYKBN
                                                           (SRT-IDX1)
               MOVE    WKACCT-SRT-SRYCOMMENT (SRT-IDX1)
                                       TO  SRT-SRYACCT-SRYCOMMENT
                                                           (SRT-IDX1)
               MOVE    ALL "9"         TO  SRT-SRYACCT-TEMP
                                                           (SRT-IDX1)
               MOVE    WKACCT-ZAISKBKBN (SRT-IDX1)
                                       TO  SRT-SRYACCT-ZAISKBKBN
                                                           (SRT-IDX1)
               IF    ( WKACCT-ZAISKBKBN (SRT-IDX1)
                                       =   "3" )
                   MOVE    1           TO  SRT-SRYACCT-JIHIKBN
                                                           (SRT-IDX1)
               END-IF
               MOVE    WKACCT-PRIORITY (SRT-IDX1)
                                       TO  SRT-SRYACCT-PRIORITY
                                                           (SRT-IDX1)
               MOVE    WKACCT-PRIORITY2(SRT-IDX1)
                                       TO  SRT-SRYACCT-PRIORITY2
                                                           (SRT-IDX1)
               MOVE    WKACCT-PRIORITY3(SRT-IDX1)
                                       TO  SRT-SRYACCT-PRIORITY3
                                                           (SRT-IDX1)
               IF    ( WRK-SRYCD   NOT =   SPACE )
                   MOVE    WRK-SRYCD   TO  WRK-TNS-SRYCD
                   PERFORM 200111-TENSU-SEL-SEC
                   IF    ( FLG-TENSU   =   ZERO )
      *
                       IF    ( ACCT-SRYKBN =   "60" )
                           PERFORM 200111-KNSJISKBN-HEN-SEC
                       END-IF
      *
                       IF    ( ACCT-SRYKBN =   "50" OR "54" )
                           MOVE    WRK-SRYSYUKBN
                                       TO  SRT-SRYACCT-OPE-SRYSYUKBN
                                                              (SRT-IDX1)
                           PERFORM VARYING IDX0
                                   FROM    ORCHCM34-STDAY   BY  1
                                   UNTIL ( IDX0    >   ORCHCM34-EDDAY )
                                    OR   ( SRT-SRYACCT-OPE-DAY(SRT-IDX1)
                                               NOT =   ZERO )
                               IF    ( ACCT-DAY (1 IDX0)   >   ZERO )
                                   MOVE    IDX0
                                       TO  SRT-SRYACCT-OPE-DAY(SRT-IDX1)
                               END-IF
                           END-PERFORM
                       END-IF
      *
                       MOVE   TNS-HOUKNSKBN
                                       TO   SRT-SRYACCT-HOUKNSKBN
                                                          (SRT-IDX1)
      *
                       IF    ( WRK-SRYCD   (1:1) =   "6"  OR  "7" )
                           IF  ( ACCT-SRYKBN     =   "21"
                                                  OR "22"
                                                  OR "23" )
                               CONTINUE
                           ELSE
                               MOVE    ALL "9"
                                   TO  SRT-SRYACCT-CDKBN-SYO-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-BU-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBN-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBNNUM-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KBNNUM-EDA-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-CDKBN-KOUBAN-G
                                                          (SRT-IDX1)
                                       SRT-SRYACCT-KOUHYOJYUNNUM-G
                                                          (SRT-IDX1)
                           END-IF
                       ELSE
      *
                           IF   ( ACCT-SRYKBN    = "98" OR "99" )
                               CONTINUE
                           ELSE
                               MOVE   TNS-CDKBN-KBN
                                       TO   SRT-SRYACCT-CDKBN-KBN
                                                          (SRT-IDX1)
                               MOVE   TNS-CDKBN-SYO
                                       TO   SRT-SRYACCT-CDKBN-SYO
                                                          (SRT-IDX1)
                               MOVE   TNS-CDKBN-BU
                                       TO   SRT-SRYACCT-CDKBN-BU
                                                          (SRT-IDX1)
                               MOVE   TNS-CDKBN-KBNNUM
                                       TO   SRT-SRYACCT-CDKBN-KBNNUM
                                                          (SRT-IDX1)
                               MOVE   TNS-CDKBN-KBNNUM-EDA
                                       TO   SRT-SRYACCT-CDKBN-KBNNUM-EDA
                                                          (SRT-IDX1)
                               MOVE   TNS-CDKBN-KOUBAN
                                       TO   SRT-SRYACCT-CDKBN-KOUBAN
                                                          (SRT-IDX1)
                               MOVE   TNS-KOUHYOJYUNNUM
                                       TO   SRT-SRYACCT-KOUHYOJYUNNUM
                                                          (SRT-IDX1)
                               IF   ( TNS-CDKBN-KBN    =   SPACE )
                                OR  ((TNS-CDKBN-KBN   >=   "A" 
                                  AND TNS-CDKBN-KBN   <=   "Z"  ))
                                   CONTINUE
                               ELSE
                                   MOVE    "1"
                                       TO   SRT-SRYACCT-CDKBN-KBN-ETC
                                                          (SRT-IDX1)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
               IF    ( WKACCT-SRYKBN    (SRT-IDX1)     =   "33" )
                   IF    ( WKACCT-TENTEKSBT (SRT-IDX1) NOT =   SPACE )
                       MOVE    WKACCT-TENTEKSBT (SRT-IDX1)
                                   TO  SRT-SRYACCT-TEMP (SRT-IDX1)
                       MOVE    "0" TO  SRT-SRYACCT-TEMP (SRT-IDX1)(2:1)
                   ELSE
                       EVALUATE    TRUE
                       WHEN    ( WKACCT-SYUTEN1 (SRT-IDX1) NOT = ZERO )
                       WHEN    ( WKACCT-SYUTEN2 (SRT-IDX1) NOT = ZERO )
                           IF    ( WKACCT-SRT-KOKUJISKBKBN2 (SRT-IDX1)
                                               =   "1" OR "3" OR "5" )
                               MOVE  "11" TO SRT-SRYACCT-TEMP (SRT-IDX1)
                           ELSE
                               MOVE  "21" TO SRT-SRYACCT-TEMP (SRT-IDX1)
                           END-IF
                       WHEN    ( WKACCT-YKZTEN  (SRT-IDX1) NOT = ZERO )
                               MOVE  "31" TO SRT-SRYACCT-TEMP (SRT-IDX1)
                       WHEN    OTHER
                               MOVE  "41" TO SRT-SRYACCT-TEMP (SRT-IDX1)
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( WKACCT-ZAISKBKBN    (SRT-IDX1)     =   "8" )
                   MOVE    ALL "9"        TO SRT-SRYACCT-TEMP (SRT-IDX1)
               END-IF
      *
               IF    ( WKACCT-RIHA-SYS (SRT-IDX1)  =   "1" )
                   MOVE    "H"         TO  SRT-SRYACCT-CDKBN-KBN
                                                          (SRT-IDX1)
                   MOVE    ZERO        TO  SRT-SRYACCT-CDKBN-SYO
                                                          (SRT-IDX1)
                                           SRT-SRYACCT-CDKBN-BU
                                                          (SRT-IDX1)
                                           SRT-SRYACCT-CDKBN-KBNNUM
                                                          (SRT-IDX1)
                                           SRT-SRYACCT-CDKBN-KBNNUM-EDA
                                                          (SRT-IDX1)
                                           SRT-SRYACCT-CDKBN-KOUBAN
                                                          (SRT-IDX1)
               END-IF
      *
               IF    ( ACCT-JIHOKBN    =  "1" )
                   MOVE    1       TO  SRT-SRYACCT-ZAIHKTKBN
                                                          (SRT-IDX1)
               END-IF
      *
               IF    ( WRK-SRYSYUKBN   =  "213" OR "223" OR "233" )
                           MOVE    1       TO  SRT-SRYACCT-YKZHKTKBN
                                                          (SRT-IDX1)
               ELSE
                   IF    ( WKACCT-INGAIKBN (SRT-IDX1)     =    "1" )
                           MOVE    1       TO  SRT-SRYACCT-YKZINGAIKBN
                                                          (SRT-IDX1)
                   END-IF
               END-IF
      *
               IF    (  ACCT-SRYKBN    =  "21" )
                   IF    ( WRK-SRYSYUKBN   =   SPACE )
                       MOVE    "210"
                                       TO  SRT-SRYACCT-SRYSYUKBN
                                                          (SRT-IDX1)
                   ELSE
                       MOVE    WRK-SRYSYUKBN
                                       TO  SRT-SRYACCT-SRYSYUKBN
                                                          (SRT-IDX1)
                   END-IF
               END-IF
      *
               IF    (  ACCT-SRYKBN    =   "98" OR "99" )
                       MOVE    WRK-SRYSYUKBN
                                       TO  SRT-SRYACCT-SRYSYUKBN
                                                          (SRT-IDX1)
               END-IF
      *
               IF    ( ACCT-SRYKBN     =   "21" OR "40" 
                                       OR  "60" OR "80" )
                   CONTINUE
               ELSE
                   MOVE     ACCT-ZAITEN
                                       TO  SRT-SRYACCT-ZAITEN
                                                          (SRT-IDX1)
               END-IF
      *
               MOVE     ACCT-ZAINUM    TO  SRT-SRYACCT-ZAINUM
                                                          (SRT-IDX1)
           END-PERFORM
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING SRT-IDX1    FROM    2   BY  1
                   UNTIL ( SRT-IDX1    >   WKSRYACCT-MAX )
               COMPUTE SRT-STIDX   =   SRT-IDX1    -   1
               MOVE    SRT-SRYACCT-OCC (SRT-IDX1)
                                   TO  SRT-SRYACCT-OCC (2001)
               PERFORM VARYING SRT-IDX2    FROM    SRT-STIDX   BY  -1
                       UNTIL ( SRT-IDX2    <   1 )
                          OR ( SRT-SRYACCT-KEY (2001)
                                       >=  SRT-SRYACCT-KEY (SRT-IDX2) )
                       MOVE    SRT-SRYACCT-OCC (SRT-IDX2)
                                   TO  SRT-SRYACCT-OCC (SRT-IDX2 + 1)
               END-PERFORM
               MOVE    SRT-SRYACCT-OCC (2001)
                                   TO  SRT-SRYACCT-OCC (SRT-IDX2 + 1)
           END-PERFORM
      *
           .
       20011-SRYACT-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索処理
      *****************************************************************
       200111-SRYACT-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
                                           FLG-SRYSTATUS
                                           FLG-MINUS
                                           FLG-RIHA
                                           FLG-RIHA-SYS
      *
           MOVE    SPACE               TO  WRK-SRYCD
                                           WRK-SRYSYUKBN
      *
           MOVE    ZERO                TO  WRK-RUIKEITOTAL
                                           WRK-RUIKEITOTAL2
                                           WRK-JIHIMONEYTOTAL
                                           WRK-TNSTEN
                                           WRK-KOKUJISKBKBN2
      *
           PERFORM 900-HCM34SRY-START-SEC
      *
           PERFORM UNTIL ( FLG-HCM34SRY    NOT =   ZERO )
      *
               MOVE    HCM34SRY-DAT    TO  SRYACT-REC
      *
               IF    ( SRY-RENNUM      =   1 )
                   MOVE    SRY-JIHIMONEYTOTAL
                                       TO  WRK-JIHIMONEYTOTAL
               END-IF
      *
               IF    ( WRK-SRYSYUKBN   =   SPACE )
                   MOVE    SRY-SRYSYUKBN   TO  WRK-SRYSYUKBN
               END-IF
      *
               PERFORM 2001111-SET-SRYSTATUS-SEC
      *
               PERFORM VARYING IDX20   FROM    1   BY  1
                       UNTIL ( IDX20           >   5 )
                   IF    ( SRY-SRYCD (IDX20) (1:1)  =   "S" )
                       MOVE    1           TO  FLG-YAKUSOKU
                   END-IF
      *
                   IF    ( SRY-SRYCD(IDX20)    IS  NUMERIC )
      *
                       MOVE    SRY-SRYCD(IDX20)
                                           TO  WRK-TNS-SRYCD
                       PERFORM 200111-TENSU-SEL-SEC
                       COMPUTE WRK-TNSTEN
                           =   WRK-TNSTEN   +   TNS-TEN
      *
                       IF    ( TNS-TENSIKIBETU  =  7 )
                           MOVE    1       TO  FLG-MINUS
                       END-IF
      *
                       IF    ( TNS-SRYCD (1:1) =   "1" )
                           IF    ( WRK-KOKUJISKBKBN2
                                               =   ZERO )
                            OR   ( TNS-KOKUJISKBKBN2
                                               <   WRK-KOKUJISKBKBN2 )
                               MOVE    TNS-KOKUJISKBKBN2
                                               TO  WRK-KOKUJISKBKBN2
                           END-IF
                       END-IF
      *
                       PERFORM VARYING IDXL    FROM    1   BY  1
                               UNTIL ( IDXL
                                       >   LNK-RIHASTDAY-SYSCDLST-IDX )
                                OR   ( FLG-RIHA-SYS    NOT =   ZERO )
      *
                           IF    ( SRY-SRYCD(IDX20)
                                       =   LNK-RIHASTDAY-SYSCDLST(IDXL))
                               MOVE    1   TO  FLG-RIHA-SYS
                           END-IF
      *
                       END-PERFORM
      *
                       PERFORM VARYING IDXL    FROM    1   BY  1
                               UNTIL ( IDXL
                                       >   LNK-RIHASTDAY-SRYCDLST-IDX )
                                OR   ( FLG-RIHA    NOT =   ZERO )
      *
                           IF    ( SRY-SRYCD(IDX20)
                                       =   LNK-RIHASTDAY-SRYCDLST(IDXL))
                               MOVE    1   TO  FLG-RIHA
                           END-IF
      *
                       END-PERFORM
      *
                       MOVE     SRY-SRYCD(IDX20) TO  WRK-SRYCD-9
                       MOVE     SRY-SRYSURYO(IDX20)
                                                TO  WRK-SURYO
      *
                       MOVE     SRY-SRYCD(IDX20) TO  WRK-CALCSRYCD-G
                       PERFORM  2001111-RUIKEITOTAL-SEC
      *
                       IF    ( WRK-SRYCD       =   SPACE )
                           IF    ( SRY-SRYCD (IDX20) (1:1)
                                               =   "0" OR "8" )
                               CONTINUE
                           ELSE
                               MOVE    SRY-SRYCD (IDX20)
                                           TO  WRK-SRYCD
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
              PERFORM 900-HCM34SRY-READ-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-SRYSTATUS   =   ZERO )
               IF    ( ACCT-ZAITEN =   ZERO )
                AND  ( WRK-TNSTEN  =   ZERO )
                AND  ( FLG-MINUS   =   ZERO )
                AND  ( FLG-RIHA-SYS =  ZERO )
                   MOVE    2       TO  FLG-SRYSTATUS
               END-IF
           END-IF
      *
           IF    ( FLG-MINUS       =   1 )
               MOVE    ZERO        TO  FLG-SRYSTATUS
           END-IF
      *
           .
       200111-SRYACT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    累計合計編集処理
      *****************************************************************
       2001111-RUIKEITOTAL-SEC         SECTION.
      *
           MOVE     ZERO           TO  WRK-RUIKEI
      *
           COMPUTE WRK-RUIKEITOTAL
               =   WRK-RUIKEITOTAL
               + ( WRK-SRYCD-9     *    WRK-SURYO-S1 )
               + ( WRK-SRYCD-9     *    WRK-SURYO-S2 )
      *
           PERFORM VARYING IDX0    FROM 1  BY  1
                   UNTIL ( IDX0    >    9 )
      *
               MOVE    WRK-CALCSRYCD (IDX0)    TO    WRK-NUM
      *
               COMPUTE WRK-RUIKEI
                   =   WRK-RUIKEI
                   + ((WRK-NUM + 1 )
                   *   CONST-RUIKEI2(WRK-NUM + 1))
      *
           END-PERFORM
      *
           COMPUTE WRK-RUIKEITOTAL2
               =   WRK-RUIKEITOTAL2
               +   WRK-SRYCD-9 *   WRK-RUIKEI
      *
           .
       200111-RUIKEITOTAL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為状態フラグ設定処理処理
      *****************************************************************
       2001111-SET-SRYSTATUS-SEC      SECTION.
      *
           PERFORM VARYING IDX32   FROM    1   BY  1
                   UNTIL ( IDX32   >   5 )
      *
               EVALUATE    TRUE
               WHEN  ( SRY-SRYCD (IDX32) (1:1) =   "6" )
                AND  ( ACCT-ZAIKBN NOT =   1 )
                AND  ( ACCT-ZAIKBN NOT =   2 )
                   MOVE    1       TO  FLG-SRYSTATUS
      *
               WHEN  ( SRY-SRYKBN        =   "60" )
                AND  ( SRY-SRYCD (IDX32)  =   "820000053" )
               WHEN  ( SRY-SRYKBN        =   "80" )
                AND  ( SRY-SRYCD (IDX32)  =   "820000054" )
               WHEN  ( SRY-SRYKBN        =   "80" )
                AND  ( SRY-SRYCD (IDX32)  =   "820000055" )
               WHEN  ( SRY-SRYCD (IDX32)  =   "820000046" )
      *
                   MOVE    2       TO  FLG-SRYSTATUS
      *
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       2001111-SET-SRYSTATUS-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       200111-TENSU-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
      *    点数検索用日付編集
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX7    FROM    1   BY  1
                   UNTIL     ( IDX7    >   31 )
               IF    ( ACCT-DAY (1 IDX7)   NOT =   ZERO )
                   MOVE    IDX7        TO  WRK-SRYDD
                   MOVE    31          TO  IDX7
               END-IF
           END-PERFORM
           MOVE    WRK-SRYYMD          TO  WRK-DBYMD
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           .
       200111-TENSU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索処理
      *****************************************************************
       200111-KNSJISKBN-HEN-SEC        SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( TNS-KNSJISKBN       =   2 )
               MOVE    5       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
           WHEN  ( TNS-CDKBN-KBN       =   "9" )
               MOVE    4       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
           WHEN    OTHER
               EVALUATE    TNS-CDKBN-KBNNUM
               WHEN  ZERO    THRU    199
                   MOVE    1       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               WHEN  400     THRU    499
                   MOVE    2       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               WHEN  200     THRU    399
                   MOVE    3       TO  SRT-SRYACCT-KNSJISKBN (SRT-IDX1)
               END-EVALUATE
           END-EVALUATE
      *
           .
       200111-KNSJISKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20011-SRYACT-HEN-SEC            SECTION.
      *
           MOVE    SRT-SRYACCT-HOUKNSKBN (IDX1)
                                       TO  WRK-TNS-HOUKNSKBN
           MOVE    SRT-SRYACCT-IDX (IDX1)
                                       TO  IDX11
           MOVE    WKSRYACCT-OCC(IDX11)
                                       TO  SRYACCT-REC
           MOVE    WKACCT-ZAISKBKBN (IDX11)
                                       TO  WRK-ZAISKBKBN
           MOVE    WKACCT-SRYSTATUS (IDX11)
                                       TO  FLG-SRYSTATUS
           MOVE    WKACCT-MINUS (IDX11)
                                       TO  FLG-MINUS
           MOVE    WKACCT-RIHA (IDX11)
                                       TO  FLG-RIHA
           MOVE    WKACCT-INGAIKBN (IDX11)
                                       TO  WRK-INGAIKBN
      *
           MOVE    1                   TO  FLG-EDIT
      *
           IF    ( ACCT-SRYCDTOTAL   = FUNCTION NUMVAL (DESIGNR-SRYCD))
               IF    ( HCN68S01-DESIGNR-EVAL-DAY-G
                                       =   ZERO )
                   MOVE    ZERO            TO  FLG-EDIT
               END-IF
           ELSE
               IF    ( ACCT-ZAIKAISU   =   ZERO )
                   MOVE    ZERO            TO  FLG-EDIT
               END-IF
           END-IF
      *
           IF    ( FLG-EDIT            =   ZERO )
               GO  TO  20011-SRYACT-HEN-EXT
           END-IF
      *
           COMPUTE CNT-SRYACCT =   CNT-SRYACCT     +   1
           COMPUTE CNT-COMB-ACCT   =   CNT-COMB-ACCT   +   1
      *
           MOVE    ZERO                TO  CNT-HOUSRYACT
           MOVE    ZERO                TO  FLG-SRYACT-HEN5
           MOVE    ZERO                TO  FLG-TAJYUSHIN
           MOVE    ZERO                TO  FLG-RSIJBI-AREA-HEN
           MOVE    ZERO                TO  FLG-JIBAI
           MOVE    ZERO                TO  FLG-KENPO
           MOVE    ZERO                TO  FLG-ROSAI
           MOVE    ZERO                TO  FLG-DAISAN
           MOVE    SPACE               TO  WRK-JBI-RYOYO-TEATE-KBN
           INITIALIZE                      WRK-JBI-SONOTA-G
                                           WRK-RSI-SONOTA-G
                                           WRK-KZI-G
      *
           PERFORM 900-HCM34SRY-START-SEC
      *
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX7    FROM    1   BY  1
                   UNTIL     ( IDX7    >   31 )
               IF    ( ACCT-DAY (1 IDX7)   NOT =   ZERO )
                   MOVE    IDX7        TO  WRK-SRYDD
                   MOVE    31          TO  IDX7
               END-IF
           END-PERFORM
      *
      *    IF    ( FLG-SRYACT          NOT =   ZERO )
      *      OR  ( SRY-SRYSYUKBN       =   "213" OR "223" OR "233" )
      *        GO  TO  20011-SRYACT-HEN-EXT
      *    END-IF
      *
      *    保険組合せキーブレイクチェック処理
           PERFORM 200111-HKNCOMBI-KEY-CHK-SEC
      *
           IF   ( WRK-KOHNUM       =   "970" )
               MOVE    1           TO  FLG-DAISAN
           END-IF
      *
           IF    ( WRK-HKNNUM          =   "971" OR "973" )
      *
               IF    ( WRK-HKNNUM          =   "973" )
                 AND ( PTRSI-JIBAIKBN      =   "2" )
                   MOVE    1       TO  FLG-KENPO
               END-IF
      *
               IF    ( WRK-HKNNUM          =   "971" )
                AND  ( PTRSI-HKNKBN        =   "5"   )
                AND  ( SYS-4001-KOMUKBN    =   "2" )
                   MOVE    1       TO  FLG-KENPO
               END-IF
      *
               IF    ( ORCHCM34-SRYYM     >=   CONST-H2404 )
                AND  ( FLG-KENPO       NOT =   ZERO )
                   CONTINUE
               ELSE
                   INITIALIZE                  HCM34S03-AREA
                   MOVE    ORCHCM34-SRYYM  TO  HCM34S03-SRYYM
                   MOVE    SRY-SRYCD (1)   TO  HCM34S03-SRYCD
                   CALL    "ORCHCM34S03"   USING
                                           HCM34S03-AREA
                                           SPA-AREA
                   IF    ( HCM34S03-IGNORE =   1 )
                       GO  TO  20011-SRYACT-HEN-EXT
                   END-IF
               END-IF
      *
               IF   ( WRK-HKNNUM       =   "973" )
                   MOVE    1           TO  FLG-JIBAI
               END-IF
      *
               IF   ( WRK-HKNNUM       =   "971" )
                   MOVE    1           TO  FLG-ROSAI
               END-IF
      *
               IF   ( ACCT-SRYKBN      =   "11" OR "12" OR "13" )
                   MOVE    1           TO  FLG-RSIJBI-AREA-HEN
               END-IF
      *
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           IF    ( ACCT-SRYKBN          =   "95" OR "96" )
      *
      *
               IF   ( FLG-COMMENT-HEN   =   ZERO )
                   PERFORM 200111-TEN-TOTAL-HEN-SEC
                   PERFORM 200211-SENTEI-PTFTN-HEN-SEC
                   PERFORM 200211-RECECOM-HEN-SEC
                   MOVE    1       TO  FLG-COMMENT-HEN
               END-IF
      *
               IF   ( FLG-JIHI-HEN     =   ZERO )
      *
      *            空行を１行編集
                   PERFORM 2010-KARAGYO-HEN-SEC
      *
                   PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
                   MOVE    1       TO  FLG-JIHI-HEN
               END-IF
      *
           END-IF
      *
           IF    ( WRK-ZAISKBKBN       =   "3" )
             AND ( FLG-BRMSAGAKU-HEN   =   ZERO )
      *
               MOVE    1           TO  FLG-BRMSAGAKU-HEN
      *
           END-IF
      *
           IF   ( WRK-ZAISKBKBN    NOT =   "3" )
            AND ( FLG-BRMSAGAKU-HEN    =   1 )
               IF     ( FLG-BRMSAGAKU-TTL-HEN    =   ZERO  )
                   PERFORM 200211-BRMSAGAKU-TOTAL-HEN-SEC
                   MOVE    1       TO  FLG-BRMSAGAKU-TTL-HEN
               END-IF
           END-IF
      *
           IF    ( ACCT-ZAIKBN     =   2 )
                   MOVE    1       TO  FLG-MINUS
           END-IF
      *
           IF    ( FLG-RIHA            =   1 )
            AND  ( FLG-RIHA-HEN        =   ZERO )
               PERFORM 200111-RIHACOM-HEN-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-HCM34SRY    NOT =   ZERO )
                    OR   ( WRK-SRYACT-MAX  >   500 )
      *
               MOVE    HCM34SRY-DAT    TO  SRYACT-REC
      *
      *        剤内１行目
               IF   (  SRY-RENNUM      =   1 )
                   PERFORM 200111-GYO1-HEN-SEC
      *
                   IF    ( FLG-RSIJBI-AREA-HEN =   1 )
                       PERFORM 200111-RSIJBI-SYOKI-SEC
                   END-IF
      *
               END-IF
      *
      *        剤明細編集
               PERFORM VARYING IDX9    FROM    1   BY  1
                       UNTIL ( IDX9                >  5    )
                        OR   ( WRK-SRYACT-MAX      >   500 )
                        OR   ( SRY-SRYCD (IDX9)    =   SPACE )
      *
                   IF    ( SRY-SRYCD (IDX9)    =   CONST-TEIGEN )
                       MOVE    1           TO  FLG-MINUS
                   END-IF
      *
                   IF    ( WRK-HKNNUM          NOT =     "971" )
                    AND  ( WRK-HKNNUM          NOT =     "973" )
                       IF    ( SRY-SRYCD (IDX9)    =   CONST-GENZAN1
                                                   OR  CONST-GENZAN2
                                                   OR  CONST-GENZAN3
                                                   OR  CONST-GENZAN4
                                                   OR  CONST-GENZAN5
                                                   OR  CONST-GENZAN6
                                                   OR  CONST-GENZAN7
                                                   OR  CONST-GENZAN8 
                                                   OR  CONST-GENZAN9
                                                   OR  CONST-GENZAN10  )
                           MOVE    1       TO  FLG-MINUS
                       END-IF
                   END-IF
      *
                   IF    ( FLG-RSIJBI-AREA-HEN =   1 )
                       IF    ( RSIJBI-MAX      <   CONST-RSIJBI-MAX )
                           PERFORM 200111-RSIJBI-HEN-SEC
                       END-IF
                   ELSE
      *
      *                自費の時、金額を表示
                       IF    ( SRY-SRYKBN  =   "95"  OR  "96" )
                           PERFORM 200111-JIHIMEI-HEN-SEC
                       ELSE
                           PERFORM 200111-ZAIMEI-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               PERFORM 900-HCM34SRY-READ-SEC
      *
           END-PERFORM
      *
           EVALUATE    TRUE
      *
           WHEN  ( WRK-ZAISKBKBN   NOT =   SPACE )
      *
               EVALUATE    WRK-ZAISKBKBN
               WHEN    "8"
                   CONTINUE
               WHEN    "1"
               WHEN    "6"
                   MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                   MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                   PERFORM 205-TENSUKAISU-HEN-SEC
               WHEN    "4"
                   MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                   MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                   PERFORM 205-KINGAKUKAISU-HEN-SEC
               WHEN    "7"
                   IF    ( FLG-JIBAI           =   1 )
                       PERFORM 200111-JBI-RYOYO-TEATE-HEN-SEC
                   ELSE
                       MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                       MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                       PERFORM 205-TENSUKAISU-HEN-SEC
                   END-IF
               WHEN    "2"
                   IF   (  FLG-TAJYUSHIN    =   ZERO )
                       PERFORM 200211-NISSU-HEN-SEC
                   END-IF
                   MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                   MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                   PERFORM 205-TENSUKAISU-HEN-SEC
               WHEN    "3"
      *
                   COMPUTE WRK-TTL-BRMSAGAKU
                       =   WRK-TTL-BRMSAGAKU
                       + ( ACCT-ZAITEN     *   ACCT-ZAIKAISU )
      *
                   PERFORM 200211-NISSU-HEN-SEC
                   MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
                   MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                   PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
               END-EVALUATE
      *
      *
           WHEN  ( WRK-JBI-SONOTA-KBN      NOT =   SPACE )
               PERFORM 200111-JBI-SONOTA-HEN-SEC
           WHEN  ( WRK-RSI-SONOTA-KBN      NOT =   SPACE )
               PERFORM 200111-RSI-SONOTA-HEN-SEC
           WHEN  ( WRK-JBI-RYOYO-TEATE-KBN NOT =   SPACE )
               PERFORM 200111-JBI-RYOYO-TEATE-HEN-SEC
           WHEN  ( FLG-RSIJBI-AREA-HEN =   1 )
               PERFORM 200111-RSIJBI-SRYACT-HEN-SEC
      *
           WHEN  ( ACCT-SRYKBN         =   "95"  OR  "96" )
               CONTINUE
      *
           WHEN    OTHER
               IF    ( WRK-TNS-HOUKNSKBN   NOT =   ZERO )
                   IF    ( FLG-SRYACT-HEN5 NOT =   ZERO )
                       PERFORM 210-WRK-SRYACT-TBL-HEN5-SEC
                   END-IF
                   IF    ( CNT-HOUSRYACT   >   1   )
      *                検査数編集処理
                       PERFORM 200111-KENSASU-HEN-SEC
                   END-IF
               END-IF
      *        点数回数編集処理
               MOVE    ACCT-ZAITEN         TO  WRK-ZAITEN
               MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
               IF    ( WRK-ZAITEN          >   ZERO )
                   PERFORM 205-TENSUKAISU-HEN-SEC
               ELSE
                   EVALUATE    FLG-SRYSTATUS
                   WHEN    2
                       PERFORM 205-KAISU-HEN-SEC
                   WHEN    OTHER
                       PERFORM 205-TENSUKAISU-HEN-SEC
                   END-EVALUATE
               END-IF
           END-EVALUATE
      *
           MOVE    "5"                     TO  WRK-ITEM-KBN
           MOVE    ZERO                    TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       20011-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せキーブレイクチェック処理
      *****************************************************************
       200111-HKNCOMBI-KEY-CHK-SEC     SECTION.
      *
           MOVE    ZERO                    TO  FLG-COMB-CHANGED
      *
           EVALUATE    TRUE
           WHEN  ( IDX1                    =     1 )
               MOVE    SPACE               TO  WRK-HKNCOMBI-KEY-MAE
               MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI-KEY
                                               WRK-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    COMB-KOHHKNNUM (1)  TO  WRK-KOHNUM
               MOVE    HKNCOMBI-REC        TO  WKCOMB-REC
               MOVE    1                   TO  FLG-COMB-CHANGED
               IF    ( WKCOMB-HKNNUM   =   "971" OR "973" )
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
               ELSE
                   INITIALIZE          PTRSIINF-REC
               END-IF
           WHEN  ( IDX1                >   1 )
               IF    ( ACCT-HKNCOMBI   NOT =   WRK-HKNCOMBI-KEY )
      *
                   IF   ( FLG-COMMENT-HEN   =   ZERO )
                       PERFORM 200111-TEN-TOTAL-HEN-SEC
                       PERFORM 200211-SENTEI-PTFTN-HEN-SEC
                       PERFORM 200211-RECECOM-HEN-SEC
                   END-IF
      *
                   IF  (   FLG-BRMSAGAKU-TTL-HEN   =   ZERO  )
                       PERFORM 200211-BRMSAGAKU-TOTAL-HEN-SEC
                   END-IF
      *
                   PERFORM 200111-JIHI-TOTAL-HEN-SEC
      *
                   MOVE    ZERO    TO      FLG-BRMSAGAKU-TTL-HEN
                                           FLG-BRMSAGAKU-HEN
                                           FLG-COMMENT-HEN
                                           FLG-JIHI-HEN
                                           FLG-RIHA-HEN
                   MOVE    ZERO    TO      CNT-COMB-ACCT
      *
                   MOVE    WRK-HKNCOMBI-KEY
                                           TO  WRK-HKNCOMBI-KEY-MAE
                   MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBI-KEY
                                               WRK-HKNCOMBINUM
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                   MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOHHKNNUM (1)
                                           TO  WRK-KOHNUM
                   MOVE    HKNCOMBI-REC    TO  WKCOMB-REC
                   MOVE    1               TO  FLG-COMB-CHANGED
                   IF    ( WKCOMB-HKNNUM   =   "971" OR "973" )
                       PERFORM 900-PTRSIINF-KEY-SEL-SEC
                   ELSE
                       INITIALIZE          PTRSIINF-REC
                   END-IF
      *
                   MOVE    1               TO  FLG-LINE
      *
               ELSE
                   MOVE    ZERO            TO  FLG-LINE
               END-IF
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "5"                 TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-EVALUATE
      *
           .
       200111-HKNCOMBI-KEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       200111-GYO1-HEN-SEC             SECTION.
      *
           IF    ( WRK-ZAISKBKBN       NOT =   "3" )
               MOVE    SRY-SRYKBN      TO  WRK-SRYACT-SRYKBN
           END-IF
      *
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYACT-SRYSYUKBN
      *
           MOVE    ACCT-JIHOKBN        TO  WRK-SRYACT-JIHOKBN
      *
      *    診療区分
           MOVE    SPACE               TO  WRK-SRYACT-MARK
           EVALUATE    TRUE
           WHEN  ( ACCT-JIHOKBN    =   "1" )
               IF    ( ACCT-ZAIKBN =   3 OR 4 )
                   MOVE    "◇"   TO  WRK-SRYACT-MARK
                   COMPUTE CNT-JISANYAKU
                       =   CNT-JISANYAKU  +   1
               ELSE
                   MOVE    "◎"   TO  WRK-SRYACT-MARK
                   COMPUTE CNT-ZAIHKT
                       =   CNT-ZAIHKT  +   1
               END-IF
           WHEN  ( ACCT-ZAITEN         =   ZERO )
            AND  ( FLG-SRYSTATUS       =   1 )
                   EVALUATE    TRUE
                   WHEN  ( SRY-SRYSYUKBN   =   "213" OR "223" OR "233" )
                            MOVE    "○"   TO  WRK-SRYACT-MARK
                            COMPUTE CNT-YKZHKT
                                =   CNT-YKZHKT  +   1
                   WHEN  (  WRK-INGAIKBN   =   "1" )
                            MOVE    "☆"   TO  WRK-SRYACT-MARK
                            COMPUTE CNT-YKZINGAI
                                =   CNT-YKZINGAI    +   1
                   END-EVALUATE
           END-EVALUATE
      *
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  WRK-SRYACT-HKNCOMBI
      *    回数
           PERFORM VARYING IDX8    FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX8    >   ORCHCM34-EDDAY )
               IF    ( ACCT-SRYCDTOTAL
                                   =   FUNCTION NUMVAL (DESIGNR-SRYCD))
                   IF    ( HCN68S01-DESIGNR-EVAL-DAY (IDX8)
                                       >   ZERO )
                       MOVE    ACCT-DAY  (1 IDX8)
                                       TO  WRK-SRYACT-DAY-Z9(IDX8)
                   END-IF
               ELSE
                   MOVE    ACCT-DAY  (1 IDX8)
                                       TO  WRK-SRYACT-DAY-Z (IDX8)
               END-IF
           END-PERFORM
      *
           .
       200111-GYO1-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠責編集エリア初期処理
      *****************************************************************
       200111-RSIJBI-SYOKI-SEC         SECTION.
      *
           INITIALIZE                  RSIJBI-AREA
      *
           MOVE    ACCT-ZAITEN     TO  RSIJBI-TEN
           MOVE    ACCT-ZAIKAISU   TO  RSIJBI-ZAIKAISU
      *
           .
       200111-RSIJBI-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *   リハビリ開始日コメント編集処理
      *****************************************************************
       200111-RIHACOM-HEN-SEC           SECTION.
      *
           MOVE    1               TO  FLG-RIHA-HEN
      *
           INITIALIZE                  ORCSRIHASTDAYAREA
           MOVE    SPA-HOSPNUM         TO  LNK-RIHASTDAY-HOSPNUM
           MOVE    ORCHCM34-NYUGAIKBN  TO  LNK-RIHASTDAY-NYUGAIKBN
           MOVE    SPA-PTID            TO  LNK-RIHASTDAY-PTID
           IF    ( ORCHCM34-SRYKAKBN   =   "1" )
               MOVE    ORCHCM34-SRYKA  TO  LNK-RIHASTDAY-SRYKA
           END-IF
           MOVE    ORCHCM34-SRYYM      TO  LNK-RIHASTDAY-SRYYM
           MOVE    ACCT-HKNCOMBI       TO  LNK-RIHASTDAY-HKNCOMBI
           CALL    "ORCSRIHASTDAY"     USING
                                       ORCSRIHASTDAYAREA
                                       SPA-AREA
      *
           COMPUTE WRK-MAX = FUNCTION LENGTH(LNK-RIHASTDAY-OUT-DAT)
                           / FUNCTION LENGTH(LNK-RIHASTDAY-OUT-OCC(1))
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   WRK-MAX )
      *
               IF    ( LNK-RIHASTDAY-IDX (IDXL)    >   ZERO )
      *
                   MOVE    LNK-RIHASTDAY-IDX (IDXL)
                                       TO  WRK-RIHA-MAX
      *
                   PERFORM VARYING IDXM    FROM    1   BY  1
                           UNTIL ( IDXM    >   WRK-RIHA-MAX )
      *
                       IF    ( LNK-RIHASTDAY-MARK (IDXL IDXM)
                                           NOT =   SPACE )
      *
                           IF    ( IDXM    >   1 )
                               MOVE    "5"         TO  WRK-ITEM-KBN
                               MOVE    ZERO        TO  FLG-LINE
                               PERFORM 210-SRYACT-HEN-SEC
                           END-IF
      *
                           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
                           MOVE    ACCT-SRYKBN   TO  WRK-SRYACT-SRYKBN
      *
                           MOVE    ACCT-HKNCOMBI TO  WRK-SRYACT-HKNCOMBI
      *
                       END-IF
      *
                       MOVE    LNK-RIHASTDAY-COM (IDXL IDXM)
                                           TO   WRK-SRYACT-STR
      *
                       PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
                   END-PERFORM
      *
                   MOVE    "5"                 TO  WRK-ITEM-KBN
                   MOVE    ZERO                TO  FLG-LINE
                   PERFORM 210-SRYACT-HEN-SEC
      *
                   PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-RIHACOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠責編集エリア編集処理
      *****************************************************************
       200111-RSIJBI-HEN-SEC           SECTION.
      *
           COMPUTE RSIJBI-MAX  =   RSIJBI-MAX  +   1
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  WRK-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
           MOVE    TNS-SRYCD           TO  RSIJBI-SRYCD(RSIJBI-MAX)
           MOVE    SRY-INPUTNUM (IDX9) TO  RSIJBI-INPUTNUM(RSIJBI-MAX)
           MOVE    HCM34SRY-COMMENT (IDX9)
                                       TO  RSIJBI-COMMENT (RSIJBI-MAX)
           MOVE    TNS-NAME            TO  RSIJBI-NAME (RSIJBI-MAX)
      *
           EVALUATE    TRUE
           WHEN  ( TNS-SRYCD (1:3)     =   "101" )
            AND  ( TNS-TENSIKIBETU     =   1 )
               COMPUTE RSIJBI-KINGAKU  =   RSIJBI-KINGAKU + TNS-TEN
               COMPUTE RSIJBI-TEN      =   RSIJBI-TEN - ( TNS-TEN / 10 )
               MOVE    RSIJBI-MAX      TO  RSIJBI-KIN-IDX
           WHEN  (( TNS-SRYCD   (1:1)  =   "8"  )
              OR  ( TNS-SRYCD   (1:3)  =   "008"  ))
               CONTINUE
           WHEN    OTHER
               MOVE    RSIJBI-MAX      TO  RSIJBI-TEN-IDX
           END-EVALUATE
      *
           .
       200111-RSIJBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災自賠診療行為編集処理
      *****************************************************************
       200111-RSIJBI-SRYACT-HEN-SEC    SECTION.
      *
           IF    ( RSIJBI-TEN-IDX      >  ZERO )
               MOVE    RSIJBI-MAX      TO  RSIJBI-TEN-IDX
           ELSE
               IF    ( RSIJBI-KIN-IDX  >  ZERO )
                   MOVE    RSIJBI-MAX  TO  RSIJBI-KIN-IDX
               ELSE
                   MOVE    RSIJBI-MAX  TO  RSIJBI-TEN-IDX
               END-IF
           END-IF
      *
           PERFORM VARYING IDX19   FROM    1   BY  1
                   UNTIL ( IDX19   >   RSIJBI-MAX )
      *
      *        コメントの時、内容設定
               IF   (( RSIJBI-SRYCD  (IDX19)(1:1)  =   "8"  )
                 OR  ( RSIJBI-SRYCD  (IDX19)(1:3)  =   "008"  ))
                AND  ( RSIJBI-INPUTNUM(IDX19)  NOT =   ZERO )
      *
                   MOVE    RSIJBI-COMMENT (IDX19)  TO  WRK-SRYACT-STR
      *
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               ELSE
                   MOVE    RSIJBI-NAME (IDX19)     TO  WRK-SRYACT-STR
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
               END-IF
      *
               EVALUATE    TRUE
               WHEN  ( RSIJBI-KIN-IDX  =   IDX19 )
                   MOVE    RSIJBI-KINGAKU  TO  WRK-ZAITEN
                   MOVE    RSIJBI-ZAIKAISU TO  WRK-ZAIKAISU
                   PERFORM 205-KINGAKUKAISU-HEN-SEC
               WHEN  ( RSIJBI-TEN-IDX  =   IDX19 )
                   MOVE    RSIJBI-TEN      TO  WRK-ZAITEN
                   MOVE    RSIJBI-ZAIKAISU TO  WRK-ZAIKAISU
                   IF    ( WRK-ZAITEN          >   ZERO )
                       PERFORM 205-TENSUKAISU-HEN-SEC
                   ELSE
                       EVALUATE    FLG-SRYSTATUS
                       WHEN    2
                           PERFORM 205-KAISU-HEN-SEC
                       WHEN    OTHER
                           PERFORM 205-TENSUKAISU-HEN-SEC
                       END-EVALUATE
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           .
       200111-RSIJBI-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   労災その他編集処理
      *****************************************************************
       200111-RSI-SONOTA-HEN-SEC       SECTION.
      *
           MOVE    WRK-RSI-SONOTA-TEN  TO  WRK-ZAITEN
      *
           MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
           PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           .
       200111-RSI-SONOTA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   自賠責その他編集処理
      *****************************************************************
       200111-JBI-SONOTA-HEN-SEC       SECTION.
      *
           MOVE    WRK-JBI-SONOTA-TEN  TO  WRK-ZAITEN
      *
           MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
           EVALUATE    WRK-JBI-SONOTA-KBN
           WHEN    "1"
           WHEN    "2"
                IF   ( SYS-4001-TAXKBN         =   "2" )
                 COMPUTE  WRK-ZAITEN
                     =  ( WRK-ZAITEN   *   WRK-RATE2 )  +  WRK-HASU
                END-IF
           WHEN    "3"
               MOVE    ZERO        TO  FLG-JBI-KZI-OUT
               IF    ( SYS-4001-TAXKBN-KZI     =   "2" )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   50 )
                       IF    ( WRK-JBI-SONOTA-SRYCD
                                           =   SYS-4003-SRYCD (IDX0))
                           MOVE    1   TO  FLG-JBI-KZI-OUT
                       END-IF
                   END-PERFORM
      *
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    WRK-JBI-SONOTA-SRYCD
                                           TO  TNS-SRYCD
                   MOVE    WRK-SRYYMD      TO  WRK-DBYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
                       IF    ( TNS-KANANAMECHGKBN  =   1 OR 2 OR 3 OR 4)
                           MOVE    1   TO  FLG-JBI-KZI-OUT
                       END-IF
                   ELSE
                       IF    ( TNS-NAMECHGKBN      =   1 OR 2 OR 3 OR 4)
                           MOVE    1   TO  FLG-JBI-KZI-OUT
                       END-IF
                   END-IF
      *
                   IF    ( FLG-JBI-KZI-OUT =   ZERO )
                       COMPUTE  WRK-ZAITEN
                           =  ( WRK-ZAITEN   *   WRK-RATE2 ) + WRK-HASU
                   END-IF
               END-IF
           WHEN    OTHER
               CONTINUE
           END-EVALUATE
      *
           PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           .
       200111-JBI-SONOTA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   自賠責療養担当手当編集処理
      *****************************************************************
       200111-JBI-RYOYO-TEATE-HEN-SEC  SECTION.
      *
           IF    ( PTRSI-JIBAIKBN          =   "2" )
               MOVE    ACCT-ZAITEN     TO  WRK-ZAITEN
               MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
               PERFORM 205-TENSUKAISU-HEN-SEC
           ELSE
               IF    ( WRK-JBI-RYOYO-TEATE-KBN     =   "1" )
                   MOVE    120             TO  WRK-ZAITEN
               ELSE
                   MOVE    84              TO  WRK-ZAITEN
               END-IF
      *
               MOVE    ACCT-ZAIKAISU       TO  WRK-ZAIKAISU
      *
               PERFORM 205-KINGAKUKAISU-HEN-SEC
           END-IF
      *
           .
       200111-JBI-RYOYO-TEATE-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       200111-ZAIMEI-HEN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
      *
      *    約束セットのコードは読み飛ばす
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "S" )
               GO  TO  200111-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  WRK-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
           IF    ( SRY-SRYCD      (IDX9)(1:3)  =   "058" )
      *
               INITIALIZE                      WRK-KZI-G
      *
               MOVE    TNS-FORMALNAME      TO  TNS-NAME
      *
               IF    ( TNS-TANICD          NOT = SPACE )
                AND  ( TNS-TANICD          NOT = "000" )
                   MOVE    TNS-TANICD      TO  WRK-KZI-TANICD
                   MOVE    TNS-TANINAME    TO  WRK-KZI-TANINAME
                   INITIALIZE                  TNS-TANICD
                                               TNS-TANINAME
               END-IF
               IF    ( TNS-TEN         NOT =   ZERO )
                   MOVE    TNS-TEN         TO  WRK-KZI-TEN
                   MOVE    ZERO            TO  TNS-TEN
               END-IF
           END-IF
      *
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "7")
            AND  ( SRY-MEISKYFLG(IDX9)     =   "1")
      *
               MOVE    TNS-NAME            TO  WRK-TNS-NAME
               MOVE    SPACE               TO  TNS-NAME
               STRING  "（"                DELIMITED   BY  SIZE
                       WRK-TNS-NAME        DELIMITED   BY  SPACE
               INTO    TNS-NAME
               END-STRING
               IF    ( WRK-KZI-TANICD      NOT = SPACE )
                AND  ( WRK-KZI-TANICD      NOT = "000" )
                   CONTINUE
               ELSE
                   MOVE    WRK-KZI-TANICD      TO  TNS-TANICD
                   MOVE    WRK-KZI-TANINAME    TO  TNS-TANINAME
               END-IF
               IF    ( WRK-KZI-TEN         NOT =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    WRK-KZI-TEN         TO  TNS-TEN
               END-IF
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME                TO  WRK-SRYACT-STR
      *
      *    薬剤服用コメントの時、再編集
           IF    ( SRY-SRYCD (IDX9)(1:3)   =   "001" )
               IF    ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
                   MOVE    HCM34SRY-COMMENT (IDX9)
                                           TO  WRK-SRYACT-STR
               ELSE
                   STRING "【"                 DELIMITED   BY  SIZE
                          TNS-NAME             DELIMITED   BY  SPACE
                          "】"                 DELIMITED   BY  SIZE
                   INTO   WRK-SRYACT-STR
                   END-STRING
               END-IF
           END-IF
      *
      *    器材商品名の器材コード
           IF    ( SRY-SRYCD      (IDX9)(1:1)  =   "7" )
            AND  ( TNS-TEN                     >   ZERO )
            AND  ( TNS-TENSIKIBETU             =   1 OR 2)
            AND  ( TNS-DATAKBN             NOT =   3 )
      *
               MOVE    TNS-TEN         TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    SPACE           TO  WRK-MAE-INPUT
               STRING  WRK-STR-LEFT    DELIMITED   BY  SPACE
                       "円"            DELIMITED   BY  SIZE
               INTO    WRK-MAE-INPUT
               END-STRING
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR-PLUS
               PERFORM 210-WRK-SRYACT-TBL-HEN6-SEC
           END-IF
      *
      *    コメントの時、内容設定
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
            AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
               MOVE    HCM34SRY-COMMENT (IDX9)
                                           TO  WRK-SRYACT-STR
           END-IF
      *
      *    数量（薬剤、器材のとき）
      *    IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "6"  OR  "7" )
      *     AND  ( TNS-TANINAME        NOT =   SPACE )
           IF    ( TNS-TANINAME        NOT =   SPACE )
               IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "1" )
                 AND ( SRY-SRYKBN              =   "70" )
                 AND ( TNS-TANICD              =   CONST-GAZOU-TANICD )
                   MOVE    "回"                TO  WRK-TANINAME
                   PERFORM 2001111-SURYO-HEN-SEC
               ELSE
                   MOVE    TNS-TANINAME        TO  WRK-TANINAME
                   PERFORM 2001111-SURYO-HEN-SEC
               END-IF
           END-IF
      *
      *    分画数
      *    IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "7"  )
      *     AND  ( TNS-DATAKBN             =   3    )
      *     AND  ( SRY-SRYKAISU (IDX9)     >   ZERO )
      *        PERFORM 2001111-BUNGASU-HEN-SEC
      *    END-IF
      *
           IF    ( WRK-TNS-HOUKNSKBN       =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
           ELSE
               IF    ( SRY-SRYCD  (IDX9)(1:1)  =   "0" OR "8"  )
                   IF    ( FLG-SRYACT-HEN5 NOT =   ZERO )
                       PERFORM 210-WRK-SRYACT-TBL-HEN5-SEC
                   END-IF
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
                   MOVE    ZERO        TO  FLG-SRYACT-HEN5
               ELSE
                   MOVE    SPACE               TO  WRK-STR
                   STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
                           "，"                DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   END-STRING
                   MOVE    WRK-STR             TO  WRK-SRYACT-STR
                   PERFORM 210-WRK-SRYACT-TBL-HEN4-SEC
                   COMPUTE CNT-HOUSRYACT   =   CNT-HOUSRYACT   +   1
                   MOVE    1           TO  FLG-SRYACT-HEN5
               END-IF
           END-IF
      *
           IF    ( SRY-SRYKBN          =   "80" )
      *
               IF    ( FLG-ROSAI       =   1 )
                OR   ( FLG-JIBAI       =   1 )
                OR   ( FLG-DAISAN      =   1 )
      *
                   IF    ( SRY-SRYCD (IDX9)(1:5)   >=  "09591" )
                    AND  ( SRY-SRYCD (IDX9)(1:5)   <=  "09594" )
      *
                       IF    ( FLG-ROSAI       =   1 )
      *
                           IF    ( SRY-SRYSYUKBN   =   "809" )
                               MOVE    SRY-JIHIMONEY  (IDX9)
                                           TO  WRK-RSI-SONOTA-TEN
                           ELSE
                               MOVE    TNS-TEN
                                           TO  WRK-RSI-SONOTA-TEN
                           END-IF
      *
                           MOVE    "1"     TO  WRK-RSI-SONOTA-KBN
      *
                       ELSE
                           IF    ( SRY-SRYSYUKBN   =   "809" )
                               MOVE    SRY-JIHIMONEY  (IDX9)
                                           TO  WRK-JBI-SONOTA-TEN
                           ELSE
                               MOVE    TNS-TEN
                                           TO  WRK-JBI-SONOTA-TEN
                           END-IF
      *
                           MOVE    SRY-SRYCD (IDX9)
                                           TO  WRK-JBI-SONOTA-SRYCD
      *
                           MOVE    SRY-SRYCD (IDX9)(5:1)
                                           TO  WRK-JBI-SONOTA-KBN
                       END-IF
      *
                   END-IF
      *
                   IF    ( SRY-SRYCD (IDX9) (1:3)  =   "101" )
                    AND  ( TNS-TENSIKIBETU         =   1 )
                       MOVE    "1"         TO  WRK-RSI-SONOTA-KBN
                       MOVE    TNS-TEN     TO  WRK-RSI-SONOTA-TEN
                   END-IF
      *
               END-IF
      *
               IF    ( FLG-JIBAI       =   1 )
                   IF    ( SRY-SRYCD (IDX9)
                                   =   CONST-RYOYO-TEATE-GAI )
                       MOVE    "2"     TO  WRK-JBI-RYOYO-TEATE-KBN
                   END-IF
      *
                   IF    ( SRY-SRYCD (IDX9)
                                   =   CONST-RYOYO-TEATE-NYU )
                       MOVE    "1"     TO  WRK-JBI-RYOYO-TEATE-KBN
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "7")
            AND  ( SRY-MEISKYFLG(IDX9)     =   "1")
               MOVE    "）"                TO  WRK-SRYACT-STR
               MOVE   ZERO                 TO  FLG-ZENSP
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           .
       200111-ZAIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       200111-JIHIMEI-HEN-SEC              SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
      *
      *    約束セットのコードは読み飛ばす
           IF    ( SRY-SRYCD (IDX9)(1:1)   =   "S" )
               GO  TO  200111-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           INITIALIZE  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX9)        TO  TNS-SRYCD
           MOVE    ZERO                    TO  TNS-YUKOSTYMD
           MOVE    ALL "9"                 TO  TNS-YUKOEDYMD
           MOVE    WRK-SRYYMD              TO  WRK-DBYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SRY-SRYCD (IDX9)    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
           ELSE
      *
               MOVE    TNS-NAME            TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  TNS-NAME
      *
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME                TO  WRK-SRYACT-STR
      *
      *    コメントの時、内容設定
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
            AND  ( SRY-INPUTNUM(IDX9)  NOT =   ZERO )
               MOVE    HCM34SRY-COMMENT (IDX9)
                                           TO  WRK-SRYACT-STR
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
      *    コメント時、金額回数の編集は行わない
           IF   (( SRY-SRYCD  (IDX9)(1:1)  =   "8"  )
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "008"  ))
             OR  ( SRY-SRYCD  (IDX9)(1:3)  =   "099" )
             CONTINUE
           ELSE
      *
               MOVE    SRY-JIHIMONEY (IDX9)
                                       TO  WRK-ZAITEN
               MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
               PERFORM 205-KINGAKUKAISU-HEN-SEC
      *
           END-IF
      *
           COMPUTE WRK-TTL-JIHI    =   WRK-TTL-JIHI
                                   + ( SRY-JIHIMONEY (IDX9)
                                   *   ACCT-ZAIKAISU )
      *
           IF    ( ACCT-SRYKBN         =   "96" )
               COMPUTE WRK-TTL-JIHI-TAX    =   WRK-TTL-JIHI-TAX
                                           + ( SRY-JIHIMONEY (IDX9)
                                           * ACCT-ZAIKAISU )
           END-IF
      *
           .
       200111-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数計編集処理
      *****************************************************************
       200111-TEN-TOTAL-HEN-SEC       SECTION.
      *
           IF    ( WRK-TTL-TEN    >   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　点数合計"
                                           TO  WRK-TENSU-LBL
               MOVE    WRK-TTL-TEN         TO  WRK-TENSU
               PERFORM 2001111-TENSU-HEN-SEC
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    ZERO                TO  WRK-TTL-TEN
           END-IF
      *
           IF    ( WRK-TTL-KINGAKU    >   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　金額合計"
                                           TO  WRK-TENSU-LBL
               MOVE    WRK-TTL-KINGAKU         TO  WRK-TENSU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    ZERO                TO  WRK-TTL-KINGAKU
           END-IF
      *
           .
      *
       200111-TEN-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費計編集処理
      *****************************************************************
       200111-JIHI-TOTAL-HEN-SEC       SECTION.
      *
           IF    ( WRK-TTL-JIHI    >   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　自費合計額"
                                           TO  WRK-TENSU-LBL
               MOVE    WRK-TTL-JIHI        TO  WRK-TENSU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
      *        消費税分がある場合は消費税額を編集
               IF    ( WRK-TTL-JIHI-TAX    >   ZERO )
      *
                   PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
                   IF    ( ORCHCM34-SRYYM  >=   CONST-H190401 (1:6))
                       MOVE    "　　　　　自費消費税額（再掲）"
                                           TO  WRK-TENSU-LBL
                   ELSE
                       MOVE    "　　　　　自費消費税額"
                                           TO  WRK-TENSU-LBL
                   END-IF
                   IF    ( ORCHCM34-SRYYM  >=   CONST-H190401 (1:6))
                       COMPUTE  WRK-TENSU
                            = ( WRK-TTL-JIHI-TAX
                             *  WRK-RATE3
                             /  WRK-RATE4 )
                            +   WRK-HASU
                   ELSE
                       COMPUTE  WRK-TENSU
                            = ( WRK-TTL-JIHI-TAX   *  WRK-RATE1 )
                            +   WRK-HASU
                   END-IF
                   PERFORM 2001111-KINGAKU-HEN-SEC
               END-IF
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
           MOVE    ZERO                TO  WRK-TTL-JIHI
           MOVE    ZERO                TO  WRK-TTL-JIHI-TAX
      *
           .
      *
       200111-JIHI-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    金額編集処理
      *****************************************************************
       2001111-KINGAKU-HEN-SEC         SECTION.
      *
           MOVE    WRK-TENSU-LBL    TO   WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-TENSU    TO  WRK-STR-LEFT-ZX99
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           MOVE    "5"                     TO  WRK-ITEM-KBN
           MOVE    ZERO                    TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
           .
      *
       2001111-KINGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       2001111-TENSU-HEN-SEC        SECTION.
      *
           MOVE    WRK-TENSU-LBL    TO   WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    1                TO  WRK-PNT
           MOVE    SPACE            TO  WRK-TENSUKAISU
           MOVE    WRK-TENSU        TO  WRK-STR-LEFT-ZX99
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                   "点"                DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           MOVE    "5"                     TO  WRK-ITEM-KBN
           MOVE    ZERO                    TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
           .
      *
       2001111-TENSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   検査数  編集処理
      *****************************************************************
       200111-KENSASU-HEN-SEC          SECTION.
      *
           IF    ( WRK-TNS-HOUKNSKBN   >=  1 )
            AND  ( WRK-TNS-HOUKNSKBN   <=  11 )
               MOVE    CNT-HOUSRYACT   TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    SPACE           TO  WRK-SRYACT-STR
               STRING  CONST-KENSACOMMENT (WRK-TNS-HOUKNSKBN)
                                       DELIMITED  BY  SPACE
                       "（"            DELIMITED  BY  SIZE
                       WRK-OUT-INPUT   DELIMITED  BY  SPACE
                       "項目）"        DELIMITED  BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
           END-IF
      *
           .
       200111-KENSASU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       2001111-SURYO-HEN-SEC           SECTION.
      *
           MOVE    SRY-SRYSURYO (IDX9) TO  WRK-SURYO
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  WRK-SURYO-Z3
           PERFORM VARYING   IDX10     FROM    5  BY  -1
                   UNTIL   ( IDX10     <   1 )
      *
               IF    ( WRK-SURYO-S2(IDX10:1)   NOT =   ZERO )
                   MOVE    1           TO  FLG-SP
               END-IF
      *
               IF    ( FLG-SP          =   1 )
                   MOVE    WRK-SURYO-S2(IDX10:1)
                                       TO  WRK-SURYO-Z3(IDX10:1)
               END-IF
           END-PERFORM
      *
           IF    ( WRK-SURYO-Z3-X      NOT =   SPACE )
               MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z19
               MOVE    "."             TO  WRK-SURYO-Z2
           END-IF
      *
           IF    ( WRK-SURYO-X NOT =   SPACE )
               IF    ( WRK-SURYO-X (1:1)   =   SPACE )
                   MOVE    SPACE       TO  WRK-STR
                   MOVE    1           TO  WRK-PNT
                   UNSTRING    WRK-SURYO-X
                   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR
                   WITH        POINTER WRK-PNT
                   END-UNSTRING
               ELSE
                   MOVE    1           TO  WRK-PNT
               END-IF
               MOVE    WRK-SURYO-X (WRK-PNT : )
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    SPACE           TO  WRK-SRYACT-STR-PLUS
               STRING  WRK-OUT-INPUT       DELIMITED   BY  SPACE
                       WRK-TANINAME        DELIMITED   BY  SPACE
               INTO    WRK-SRYACT-STR-PLUS
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN6-SEC
           END-IF
      *
           .
       2001111-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分画数編集処理
      *****************************************************************
       2001111-BUNGASU-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-BUNGSU
           MOVE    1                   TO  WRK-PNT
           MOVE    SRY-SRYSURYO (IDX9) TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-BUNGSU
           WITH    POINTER WRK-PNT
           END-STRING
           MOVE    SRY-SRYKAISU (IDX9) TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  "/"                 DELIMITED   BY  SIZE
                   WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-BUNGSU
           WITH    POINTER WRK-PNT
           END-STRING
           MOVE    WRK-BUNGSU          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
               "　"                    DELIMITED   BY  SIZE
               WRK-OUT-INPUT           DELIMITED   BY  SPACE
           INTO    WRK-STR
           END-STRING
           MOVE    WRK-STR         TO  WRK-SRYACT-STR
      *
           .
       2001111-BUNGASU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    あふれ情報編集処理
      *****************************************************************
       20011-AFURE-HEN-SEC             SECTION.
      *
      *    保険組合せ欄からあふれた分について診療行為欄に編集を行う
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKHKNCOMBI-MAX )
               PERFORM 200111-HKNCOMBI-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    保険公費欄からあふれた分について診療行為欄に編集を行う
      *    （保険）
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTHKNINF-MAX )
               PERFORM 200111-PTHKN-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    保険公費欄からあふれた分について診療行為欄に編集を行う
      *    （公費）
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTKOHINF-MAX )
               PERFORM 200111-PTKOH-RANGAI-HEN-SEC
           END-PERFORM
      *
      *    傷病名欄からあふれた分について診療行為欄に編集を行う
           MOVE    ZERO                    TO  FLG-OVER
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WKPTBYOMEI-MAX )
               PERFORM 200111-PTBYOMEI-RANGAI-HEN-SEC
           END-PERFORM
      *
           .
       20011-AFURE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-HKNCOMBI-RANGAI-HEN-SEC   SECTION.
      *
           IF    ( FLG-OVER        =   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下保険組合せあふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
      *    保険組合せ番号全角変換
           MOVE    WKHKNCOMBI-HKNCOMBINUM (IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-HKNCOMBINUM-N
      *
      *    適用開始日和暦変換／全角変換
           MOVE    WKHKNCOMBI-TEKSTYMDW(IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-TEKSTYMD-N
      *
           MOVE    WKHKNCOMBI-TEKEDYMDW(IDX1)
                                           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-TEKEDYMD-N
      *
           MOVE    WKHKNCOMBI-HKNCOMBIMEI (IDX1)
                                           TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN      <   1 )
               MOVE    1                   TO  WRK-STRLEN
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE        SPACE               TO  WRK-SRYACT-STR
           STRING  WRK-HKNCOMBINUM-N   DELIMITED   BY  SIZE
           "　"                DELIMITED   BY  SIZE
           WKHKNCOMBI-HKNCOMBIMEI (IDX1) (1 : WRK-STRLEN )
                                       DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-STR
           END-STRING
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE        SPACE               TO  WRK-SRYACT-STR
           STRING  WRK-TEKSTYMD-N      DELIMITED   BY  SIZE
                   "〜"                DELIMITED   BY  SIZE
                   WRK-TEKEDYMD-N      DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-STR
           END-STRING
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           MOVE    "1"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-HKNCOMBI-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTHKN-RANGAI-HEN-SEC     SECTION.
      *
           MOVE    WKPTHKNINF-OCC (IDX1)   TO  PTHKNINF-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下保険あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTHKN-HKNNUM            TO  SRH-HKNNUM
           PERFORM 200111-PTHKN-HKNNUM-HEN-SEC
           MOVE    SRH-HKNNUMMEI (1:10)    TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-HKNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT (1:16)    TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-HONKZKKBN         TO  WRK-PTHKN-HONKZKKBN
           PERFORM 200111-PTHKN-HONKZKKBN-HEN-SEC
           MOVE    WRK-PTHKN-HONKZKKBNMEI (1:4)
                                           TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTHKN-CONTKBN           TO  WRK-PTHKN-CONTKBN
           PERFORM 200111-PTHKN-CONTKBN-HEN-SEC
           MOVE    WRK-PTHKN-CONTKBNMEI   (1:4)
                                           TO  WRK-SRYACT-STR
           IF    ( WRK-SRYACT-STR  NOT =   SPACE )
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( PTHKN-KIGO      NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "記："                DELIMITED   BY  SIZE
                         PTHKN-KIGO            DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( PTHKN-NUM       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "番："                DELIMITED   BY  SIZE
                         PTHKN-NUM             DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           MOVE    "2"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTHKN-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTKOH-RANGAI-HEN-SEC     SECTION.
      *
           MOVE    WKPTKOHINF-OCC (IDX1)   TO  PTKOHINF-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下公費あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTKOH-KOHNUM            TO  SRH-KOHNUM
           MOVE    PTKOH-PAYKBN            TO  SRH-PAYKBN
           PERFORM 200111-PTKOH-KOHNUM-HEN-SEC
           MOVE    SRH-KOHNUMMEI (1:10)
                                           TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    PTKOH-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-PTKOH-FTNJANUM-N
      *
           MOVE    PTKOH-JKYSNUM           TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-PTKOH-JKYSNUM-N
      *
           IF    ( WRK-PTKOH-FTNJANUM-N    NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "負："                DELIMITED   BY  SIZE
                         WRK-PTKOH-FTNJANUM-N  DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           IF    ( WRK-PTKOH-JKYSNUM-N     NOT =   SPACE )
               MOVE    SPACE               TO  WRK-SRYACT-STR
               STRING    "受："                DELIMITED   BY  SIZE
                         WRK-PTKOH-JKYSNUM-N   DELIMITED   BY  SPACE
               INTO      WRK-SRYACT-STR
               END-STRING
               PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
           END-IF
      *
           MOVE    "3"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTKOH-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    傷病名欄外編集処理（診療行為欄に編集）
      *****************************************************************
       200111-PTBYOMEI-RANGAI-HEN-SEC  SECTION.
      *
           MOVE    WKPTBYOMEI-OCC (IDX1)   TO  PTBYOMEI-REC
      *
           IF    ( FLG-OVER        =   ZERO )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    "以下傷病名あふれ分"
                                           TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    1                   TO  FLG-OVER
               MOVE    SPACE               TO  WRK-ITEM-KBN
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    PTBYO-BYOMEI            TO  WRK-PTBYO-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI        TO  WRK-PTBYO-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG         TO  WRK-PTBYO-UTAGAIFLG
           MOVE    PTBYO-HOSOKU-COMMENT    TO  WRK-PTBYO-HOSOKU-COMMENT
           PERFORM 20011-PTBYOMEI-BYOMEI-HEN-SEC
           MOVE    WRK-PTBYO-BYOMEI        TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN2-SEC
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
           MOVE    1                       TO  WRK-PNT
      *
           MOVE    PTBYO-SRYKA             TO  SRH-SRYKA
           PERFORM 200111-SRYKA-HEN-SEC
           IF    ( SRH-SRYKAMEI2      NOT  =   SPACE )
               STRING  SRH-SRYKAMEI2(1:4)  DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　　"            DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           IF    ( PTBYO-SYUBYOFLG         =   "1" )
               STRING  "主"                DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　"              DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           MOVE    PTBYO-SRYYMD            TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           IF    ( WRK-OUT-INPUT       NOT =   SPACE )
               STRING  WRK-OUT-INPUT (1:18)
                                           DELIMITED   BY  SIZE
                       "　"                DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　　　　　　　　　　　　　　　　　　"
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           MOVE    PTBYO-TENKIKBN          TO  WRK-PTBYO-TENKIKBN
           PERFORM 200111-PTBYO-TENKIKBN-HEN-SEC
           IF    ( WRK-PTBYO-TENKIKBNMEI    NOT =   SPACE )
               STRING  WRK-PTBYO-TENKIKBNMEI (1:4)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           ELSE
               STRING  "　　"              DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           MOVE    "4"                     TO  WRK-ITEM-KBN
           MOVE    IDX1                    TO  IDXD
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       200111-PTBYOMEI-RANGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       2002-NYUINRRK-HEN-SEC           SECTION.
      *
           PERFORM 20021-SHONUM-GET-SEC
           IF    ( WRK-SHONUM-MAX      =   ZERO )
               GO  TO  2002-NYUINRRK-HEN-EXT
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           MOVE    ZERO                TO  CNT-PTNYUINRRK
      *
           PERFORM VARYING IDX22   FROM    SNTYMD-MAX   BY  -1
                   UNTIL ( IDX22       <   1 )
                    OR   ( SNTYMD-NYUINYMD (IDX22) 
                                       >   ORCHCM34-EDSRYYMD )
      *
               PERFORM VARYING IDX18   FROM    1   BY  1
                       UNTIL ( IDX18   >   WRK-SHONUM-MAX )
                   IF    ( SNTYMD-SHONUM (IDX22)
                                       =   WRK-SHONUM (IDX18))
                       PERFORM 20021-NYUINRRK-HEN-SEC
                       MOVE    WRK-SHONUM-MAX  TO  IDX18
                   END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "6"                     TO  WRK-ITEM-KBN
           MOVE    1                       TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
      *
           .
       2002-NYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       20021-SHONUM-GET-SEC            SECTION.
      *
           INITIALIZE                      WRK-SHONUM-G
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCHCM34-EDSRYYMD   TO  PTNYUINRRK-NYUINYMD
           MOVE    ORCHCM34-STSRYYMD   TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC            NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               PERFORM VARYING IDX18   FROM    1   BY  1
                       UNTIL ( IDX18   >   WRK-SHONUM-MAX )
                         OR  ( IDX18   >   20 )
                         OR  ( PTNYUINRRK-SHONUM
                                       =   WRK-SHONUM (IDX18))
                   CONTINUE
               END-PERFORM
      *
               IF    ( IDX18   >   WRK-SHONUM-MAX )
                   COMPUTE WRK-SHONUM-MAX  =   WRK-SHONUM-MAX  +   1
                   IF    ( WRK-SHONUM-MAX  >   20 )
                       MOVE    20          TO  WRK-SHONUM-MAX
                   ELSE
                       MOVE    PTNYUINRRK-SHONUM
                                           TO  WRK-SHONUM 
                                                       (WRK-SHONUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC            NOT =   ZERO )
                   MOVE    1               TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20021-SHONUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       20021-NYUINRRK-HEN-SEC          SECTION.
      *
           COMPUTE CNT-PTNYUINRRK  =   CNT-PTNYUINRRK  +   1
      *
           MOVE    SPACE               TO  WRK-RRK-NYUINYMD
           MOVE    SPACE               TO  WRK-RRK-TAIINYMD
      *
           MOVE    SNTYMD-NYUINYMD (IDX22)
                                       TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (5:18)  TO  WRK-RRK-NYUINYMD
      *
           IF    ( SNTYMD-TAIINYMD (IDX22) NOT =   "99999999" )
            AND  ( SNTYMD-TAIINYMD (IDX22) NOT =   SPACE )
               MOVE    SNTYMD-TAIINYMD (IDX22)
                                           TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD3 (5:18)  TO  WRK-RRK-TAIINYMD
           END-IF
      *
           MOVE    SPACE                   TO  WRK-SRYACT-STR
           IF    ( CNT-PTNYUINRRK  =   1 )
               STRING  "入退院履歴　"      DELIMITED   BY  SIZE
                       WRK-RRK-NYUINYMD    DELIMITED   BY  SIZE
                       "〜"                DELIMITED   BY  SIZE
                       WRK-RRK-TAIINYMD    DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
           ELSE
               STRING  "　　　　　　"      DELIMITED   BY  SIZE
                       WRK-RRK-NYUINYMD    DELIMITED   BY  SIZE
                       "〜"                DELIMITED   BY  SIZE
                       WRK-RRK-TAIINYMD    DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           .
       20021-NYUINRRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計編集処理
      *****************************************************************
       2002-NACCT-HEN-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
           MOVE    99990000        TO  WRK-ZAINUM
      *
           OPEN    EXTEND              HCM34ACCT-FILE
           OPEN    I-O                 HCM34SRY-FILE
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NYUINACCT       =   ZERO )
               IF    ( NACCT-ZAIKAISU  >   ZERO )
                   PERFORM 20021-HBTSRYKA-HEN-SEC
      *
                   PERFORM 20021-TCOMBI-HEN-SEC
      *
                   PERFORM 20021-NACCT-EDIT-SEC
               END-IF
           END-IF
      *
           CLOSE                       HCM34ACCT-FILE
                                       HCM34SRY-FILE
      *
           .
       2002-NACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日別診療科編集処理
      *****************************************************************
       20021-HBTSRYKA-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-HBTSRYKA-G
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   NACCT-SRYYM )
                   MOVE    1           TO  WRK-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  WRK-STIDX
               END-IF
      *
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   NACCT-SRYYM )
                   MOVE    31          TO  WRK-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  WRK-EDIDX
               END-IF
      *
      *
               IF    ( ORCHCM34-STDAY      >   WRK-STIDX )
                   MOVE    ORCHCM34-STDAY  TO  WRK-STIDX
               END-IF
      *
               IF    ( ORCHCM34-EDDAY      <   WRK-EDIDX )
                   MOVE    ORCHCM34-EDDAY  TO  WRK-EDIDX
               END-IF
      *
               PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       WRK-EDIDX )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-HBTSRYKA-HBTKA (IDX-DAY)
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"            TO  MCP-TABLE
           MOVE    "key41"                     TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX-DAY FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX-DAY >   ORCHCM34-EDDAY )
      *
               IF    ( WRK-HBTSRYKA-HBTKA (IDX-DAY)    NOT =   ZERO )
                   PERFORM VARYING IDX23    FROM    1   BY  1
                           UNTIL ( IDX23    >   WRK-HBTSRYKA-MAX )
                           OR    ( WRK-HBTSRYKA (IDX23)
                                       = WRK-HBTSRYKA-HBTKA (IDX-DAY) )
      *
                       CONTINUE
      *
                   END-PERFORM
      *
                   MOVE    ZERO        TO  IDX24
      *
                   IF    ( IDX23    >  WRK-HBTSRYKA-MAX )
      *
                       IF    ( WRK-HBTSRYKA-MAX
                                               <   CONST-HBTSRYKA-MAX )
                           COMPUTE WRK-HBTSRYKA-MAX
                               =   WRK-HBTSRYKA-MAX    +   1
      *
                           MOVE    WRK-HBTSRYKA-MAX    TO  IDX24
      *
                           MOVE    WRK-HBTSRYKA-HBTKA (IDX-DAY)
                                       TO  WRK-HBTSRYKA (IDX24)
                       END-IF
                   ELSE
                       MOVE    IDX23   TO  IDX24
                   END-IF
      *
                   IF    ( IDX24       >   ZERO )
                       MOVE    1   TO  WRK-HBTSRYKA-DAY (IDX24 IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       20021-HBTSRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       20021-TCOMBI-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX25   FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX25   >   ORCHCM34-EDDAY )
      *
               IF    ( NACCT-DAY (IDX25)   >   ZERO )
                   MOVE    NACCT-DAY (IDX25)
                                   TO  WRK-HKNCOMBINUM
                   PERFORM 200211-TCOMBI-HEN-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
           MOVE    ORCHCM34-SRYYM      TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
               INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
      *
           PERFORM VARYING IDX25   FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX25   >       ORCHCM34-EDDAY )
      *
               IF    ( LNK-NYUINDAY-DOUJITSU-KBN(IDX25) = "1" )
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX25)
                                       TO  WRK-HKNCOMBINUM
                   PERFORM 200211-TCOMBI-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( ORCHCM34-SRYYM      >= CONST-H2910 )
               INITIALIZE              ORCSSKJHKN-AREA
               MOVE    SPA-HOSPNUM
                                       TO  LNK-SKJHKN-HOSPNUM
               MOVE    SPA-PTID        TO  LNK-SKJHKN-PTID
               MOVE    ORCHCM34-SRYYM  TO  LNK-SKJHKN-SANTEIYM
               CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
               IF    ( LNK-SKJHKN-RC   =   ZERO )
                   PERFORM VARYING IDX25   FROM    ORCHCM34-STDAY BY 1
                           UNTIL ( IDX25   >       ORCHCM34-EDDAY )
                       PERFORM VARYING IDXW    FROM    1   BY  1
                               UNTIL ( IDXW    >   2 )
                           IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDX25 IDXW)
                                       NOT =   SPACE )
                               MOVE LNK-SKJHKN-HKNCOMBINUM  (IDX25 IDXW)
                                       TO  WRK-HKNCOMBINUM-G
                               PERFORM 200211-TCOMBI-HEN-SEC
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF
           
      *
           .
       20021-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       200211-TCOMBI-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX26    FROM    1   BY  1
                   UNTIL ( IDX26   >   TCOMBI-MAX )
                   OR    ( WRK-HKNCOMBINUM
                                   =   TCOMBI-HKNCOMBI (IDX26) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    (     IDX26               >   TCOMBI-MAX )
               IF    ( CONST-TCOMBI-MAX    >   TCOMBI-MAX )
                   MOVE    IDX26   TO      TCOMBI-MAX
                   MOVE    WRK-HKNCOMBINUM
                                   TO  TCOMBI-HKNCOMBI (TCOMBI-MAX)
               ELSE
                   MOVE    ZERO    TO  IDX26
               END-IF
           END-IF
      *
           IF    (     IDX26               >   ZERO )
                   IF    ( TCOMBI-HKNCOMBI-DAY (IDX26 IDX25)
                                           =    ZERO )
                       COMPUTE TCOMBI-HKNCOMBI-CNT (IDX26)
                           =   TCOMBI-HKNCOMBI-CNT (IDX26)
                           +   1
                       MOVE    1   TO  TCOMBI-HKNCOMBI-DAY
                                                   (IDX26 IDX25)
                   END-IF
           END-IF
      *
           .
       200211-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       20021-NACCT-EDIT-SEC            SECTION.
      *
           PERFORM VARYING IDX27   FROM    1   BY  1
                   UNTIL ( IDX27   >   TCOMBI-MAX )
      *
               MOVE    1           TO  FLG-TARGET
               MOVE    ZERO        TO  FLG-HIT
      *
               MOVE    TCOMBI-HKNCOMBI (IDX27) TO  WRK-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI    NOT =   ZERO )
                   MOVE    WRK-HKNCOMBINUM
                                   TO  COMB-HKNCOMBINUM
               END-IF
      *
               IF    ( ORCHCM34-HOKEN      =   SPACE )
                   CONTINUE
               ELSE
      *
                   MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
                   MOVE    COMB-KOHHKNNUM (1)
                                           TO  WRK-KOHNUM
                   PERFORM 800-HOKENBUNRUI-SEC
                   IF    ( ORCHCM34-HOKEN  =   WRK-HOKEN )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
      *
                   INITIALIZE                  HCM34S01-AREA
                   MOVE    NACCT-HOSPNUM        TO  HCM34S01-HOSPNUM
                   MOVE    NACCT-PTID          TO  HCM34S01-PTID
                   MOVE    NACCT-SRYYM         TO  HCM34S01-SANTEIYM
                   MOVE    COMB-HKNCOMBINUM
                                               TO  HCM34S01-HKNCOMBI
                   MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
                   MOVE    ORCHCM34-SRYKAKBN   TO  HCM34S01-SRYKAKBN
                   MOVE    ORCHCM34-SRYKA      TO  HCM34S01-SRYKA
                   MOVE    ORCHCM34-SRYKADAY-G TO  HCM34S01-SRYKADAY-G
                   IF    ( ORCHCM34-KIKANKBN   =   "1" )
                       MOVE    "1"             TO  HCM34S01-KIKANKBN
                       MOVE    ORCHCM34-STSRYYMD
                                               TO  HCM34S01-SKYSTYMD
                       MOVE    ORCHCM34-EDSRYYMD
                                               TO  HCM34S01-SKYEDYMD
                   END-IF
                   MOVE    "1"                 TO  HCM34S01-SKJFTN
      *
                   IF    ( COMB-HKNNUM         =     "971" OR "973" )
      *
                       CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                         SPA-AREA
                   ELSE
      *
                       CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                         SPA-AREA
                   END-IF
      *
                   MOVE    HCM34S01-SENTEI-PTFTN
                                   TO  TCOMBI-SENTEI-PTFTN (IDX27)
      *
                   PERFORM 20011-HCM34ACCT-INS-SEC
      *
                   PERFORM 280-MEDADL-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       20021-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       20011-HCM34ACCT-INS-SEC             SECTION.
      *
           PERFORM VARYING IDX28    FROM    1   BY  1
                   UNTIL ( IDX28    >   HCM34S01-HENKYAKU-MAX )
      *
               IF    ( HCM34S01-ZAISKBKBN (IDX28)   NOT =   "5" )
                   PERFORM VARYING IDX29    FROM    1   BY  1
                           UNTIL ( IDX29    >   WRK-HBTSRYKA-MAX )
      *
                       MOVE    ZERO    TO  WRK-DAY-G
      *
                       PERFORM VARYING IDX-DAY
                               FROM    ORCHCM34-STDAY   BY  1
                               UNTIL ( IDX-DAY >   ORCHCM34-EDDAY )
                           IF    ( WRK-HBTSRYKA-DAY (IDX29 IDX-DAY )
                                               > ZERO )
                            AND  ( HCM34S01-DAY     (IDX28 IDX-DAY )
                                               > ZERO )
                               MOVE    HCM34S01-DAY     (IDX28 IDX-DAY )
                                       TO  WRK-DAY (IDX-DAY)
                           END-IF
                       END-PERFORM
      *
                       IF    ( WRK-DAY-G   NOT =   ZERO )
                           PERFORM 200111-HCM34ACCT-INS-SEC
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       20011-HCM34ACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       200111-HCM34ACCT-INS-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-RUIKEITOTAL
                                           WRK-RUIKEITOTAL2
      *
           COMPUTE WRK-ZAINUM  =   WRK-ZAINUM  +   1
      *
           PERFORM 200111-HCM34SRY-INS-SEC
      *
           INITIALIZE                      HCM34ACCT-REC
                                           SRYACCT-REC
      *
           MOVE    NACCT-HOSPNUM        TO  ACCT-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    NACCT-PTID          TO  ACCT-PTID
           MOVE    WRK-HBTSRYKA    (IDX29)
                                       TO  ACCT-SRYKA
           MOVE    NACCT-SRYYM         TO  ACCT-SRYYM
           MOVE    HCM34S01-SRYKBN (IDX28)
                                       TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    COMB-HKNCOMBINUM    TO  ACCT-HKNCOMBI
           MOVE    HCM34S01-ZAITEN (IDX28)
                                       TO  ACCT-ZAITEN
           MOVE    HCM34S01-SRYCDTOTAL (IDX28)
                                       TO  ACCT-SRYCDTOTAL
           MOVE    HCM34S01-SURYOUTOTAL(IDX28)
                                       TO  ACCT-SURYOUTOTAL
           MOVE    HCM34S01-MEISAISU   (IDX28)
                                       TO  ACCT-MEISAISU
      *
           PERFORM VARYING IDX-DAY FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX-DAY >   ORCHCM34-EDDAY )
      *
               MOVE    WRK-DAY (IDX-DAY)
                                       TO  ACCT-DAY (1 IDX-DAY)
               COMPUTE ACCT-ZAIKAISU
                   =   ACCT-ZAIKAISU   +  WRK-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    SRYACCT-REC         TO  HCM34ACCT-DAT
           INITIALIZE                      HCM34ACCT-DATPLUS
      *
           MOVE    WRK-RUIKEITOTAL     TO  HCM34ACCT-RUIKEITOTAL
           MOVE    WRK-RUIKEITOTAL2    TO  HCM34ACCT-RUIKEITOTAL2
           MOVE    ZERO                TO  HCM34ACCT-JIHIMONEYTOTAL
           MOVE    WRK-SRYCD           TO  HCM34ACCT-SRT-SRYCD
           MOVE    WRK-SRYSYUKBN       TO  HCM34ACCT-SRT-SRYSYUKBN
           MOVE    "2"                 TO  HCM34ACCT-ACCTKBN
           MOVE    HCM34S01-ZAISKBKBN  (IDX28)
                                       TO  HCM34ACCT-ZAISKBKBN
           MOVE    HCM34S01-PRIORITY   (IDX28)
                                       TO  HCM34ACCT-PRIORITY
           MOVE    HCM34S01-PRIORITY2  (IDX28)
                                       TO  HCM34ACCT-PRIORITY2
           MOVE    HCM34S01-PRIORITY3  (IDX28)
                                       TO  HCM34ACCT-PRIORITY3
      *
           WRITE   HCM34ACCT-REC
      *
           .
       200111-HCM34ACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為情報追加処理
      *****************************************************************
       200111-HCM34SRY-INS-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
                                           WRK-SRYSYUKBN
      *
      *
           MOVE    ZERO                TO  CNT-RENNUM
                                           IDX31
      *
           INITIALIZE                  HCM34SRY-REC
                                       SRYACT-REC
      *
           PERFORM VARYING IDX30    FROM    1   BY  1
                   UNTIL ( IDX30    >   CONST-SRYCD-MAX )
                    OR   ( HCM34S01-SRYCD (IDX28 IDX30)
                                    =   SPACE )
      *
               COMPUTE IDX31        =   IDX31      +   1
      *
               IF    ( IDX31           =  1 )
                   COMPUTE CNT-RENNUM      =   CNT-RENNUM  +   1
                   MOVE    NACCT-PTID      TO  HCM34SRY-ISM-PTID
                   MOVE    WRK-ZAINUM      TO  HCM34SRY-ISM-ZAINUM
                   MOVE    CNT-RENNUM      TO  HCM34SRY-ISM-RENNUM
                   MOVE    NACCT-HOSPNUM    TO  SRY-HOSPNUM
                   MOVE    NACCT-NYUGAIKBN TO  SRY-NYUGAIKBN
                   MOVE    NACCT-PTID      TO  SRY-PTID
                   MOVE    WRK-ZAINUM      TO  SRY-ZAINUM
                   MOVE    CNT-RENNUM      TO  SRY-RENNUM
                   MOVE    WRK-HBTSRYKA    (IDX29)
                                           TO  SRY-SRYKA
                   MOVE    HCM34S01-SRYKBN (IDX28)
                                           TO  SRY-SRYKBN
                   MOVE    HCM34S01-SRYSYUKBN (IDX28)
                                           TO  SRY-SRYSYUKBN
                                               WRK-SRYSYUKBN
               END-IF
      *
               MOVE    HCM34S01-SRYCD (IDX28 IDX30)
                                       TO  SRY-SRYCD   (IDX31)
               MOVE    HCM34S01-SURYO (IDX28 IDX30)
                                       TO  SRY-SRYSURYO(IDX31)
      *
               IF    ( SRY-SRYCD   (IDX31) IS  NUMERIC )
      *
                  MOVE     SRY-SRYCD(IDX31)    TO  WRK-SRYCD-9
      *
                  IF    ( WRK-SRYCD       =   SPACE )
                       IF    ( SRY-SRYCD (IDX31) (1:1)
                                          =   "0" OR "8" )
                           CONTINUE
                       ELSE
                           MOVE    SRY-SRYCD (IDX31)
                                       TO  WRK-SRYCD
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( IDX31           >=  5 )
                   MOVE    SRYACT-REC  TO  HCM34SRY-DAT
                   WRITE   HCM34SRY-REC
      *
                   MOVE    ZERO        TO  IDX31
                   INITIALIZE              HCM34SRY-REC
                                           SRYACT-REC
               END-IF
      *
               MOVE     HCM34S01-SRYCD (IDX28 IDX30)
                                       TO  WRK-SRYCD-9
                                           WRK-CALCSRYCD-G
               MOVE     HCM34S01-SURYO (IDX28 IDX30)
                                       TO  WRK-SURYO
               PERFORM  2001111-RUIKEITOTAL-SEC
      *
      *
           END-PERFORM
      *
           IF       (  IDX31           >  ZERO )
               MOVE    SRYACT-REC      TO  HCM34SRY-DAT
               WRITE   HCM34SRY-REC
           END-IF
      *
           .
       200111-HCM34SRY-INS-EXT.
           EXIT.
      *****************************************************************
      *   日数編集処理
      *****************************************************************
       200211-NISSU-HEN-SEC                SECTION.
      *
           MOVE    1                   TO  WRK-GAIHAKU-ST
           PERFORM UNTIL ( WRK-GAIHAKU-ST  >   31 )
               IF    ( ACCT-DAY (1 WRK-GAIHAKU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    SPACE       TO  WRK-SRYACT-STR
      *
                   MOVE    ZERO        TO  CNT-RENPAKU
                   COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ST   +   1
                   PERFORM UNTIL ( WRK-GAIHAKU-ED  >   31 )
                            OR   ( ACCT-DAY (1 WRK-GAIHAKU-ED )
                                           =   ZERO )
                       COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ED  +   1
                       COMPUTE CNT-RENPAKU     =  CNT-RENPAKU     +   1
                   END-PERFORM
                   MOVE    SPACE       TO  WRK-SRYACT-STR
                   IF    ( CNT-RENPAKU >   ZERO )
                       COMPUTE WRK-GAIHAKU-ED  =  WRK-GAIHAKU-ED  -   1
                       MOVE    WRK-GAIHAKU-ST  TO WRK-GAIHAKU-ST-Z9
                       MOVE    WRK-GAIHAKU-ED  TO WRK-GAIHAKU-ED-Z9
                       STRING  " "                 DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ST-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ED-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-SRYACT-STR
                       END-STRING
                       COMPUTE WRK-GAIHAKU-ST
                           =   WRK-GAIHAKU-ED      +   1
                   ELSE
                       MOVE    WRK-GAIHAKU-ST  TO WRK-GAIHAKU-ST-Z9
                       STRING  " "                 DELIMITED   BY  SIZE
                               WRK-GAIHAKU-ST-Z9   DELIMITED   BY  SIZE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-SRYACT-STR
                       END-STRING
                       COMPUTE WRK-GAIHAKU-ST
                           =   WRK-GAIHAKU-ST      +   1
                   END-IF
      *
                   MOVE    WRK-SRYACT-STR  TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
      *
                   PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               ELSE
                   COMPUTE WRK-GAIHAKU-ST
                       =   WRK-GAIHAKU-ST      +   1
               END-IF
           END-PERFORM
      *
           .
       200211-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント編集処理
      *****************************************************************
       200211-RECECOM-HEN-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-AFTER-CARE
      *
           IF    ( WKCOMB-HKNNUM           =   "971" )
               IF    ( PTRSI-HKNKBN    =   '3' )
                   MOVE    1       TO  FLG-AFTER-CARE
               END-IF
           END-IF
      *
           INITIALIZE                      TRECECOM-AREA
      *
           IF    ( FLG-AFTER-CARE      =   ZERO )
               MOVE    "key21"              TO  WRK-PATHNAME
               PERFORM 2002111-RECECOM-HEN-SEC
           ELSE
      *        コメント
               MOVE    "key22"             TO  WRK-PATHNAME
               PERFORM 2002111-RECECOM-HEN-SEC
           END-IF
      *
           .
       200211-RECECOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント編集処理
      *****************************************************************
       2002111-RECECOM-HEN-SEC             SECTION.
      *
           PERFORM 900-RECECOM-SEL-SEC
      *
           MOVE    SPACE               TO  WRK-LONGSTR
           MOVE    1                   TO  WRK-STRLEN-MAX
           STRING  RECECOM-COMMENT     DELIMITED   BY  SIZE
           INTO    WRK-LONGSTR
           WITH    POINTER WRK-STRLEN-MAX
           END-STRING
      *
           COMPUTE WRK-STRLEN-MAX  =   WRK-STRLEN-MAX      -   1
      *
           PERFORM UNTIL ( FLG-RECECOM NOT =   ZERO )
      *
               MOVE    1               TO  FLG-TARGET
      *
               IF    ( WKCOMB-HKNNUM       =   "973" )
                   IF    ( RECECOM-SJKBN   =   "51"  OR "52" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   IF    ( FLG-AFTER-CARE  =   1 )
                       IF    ( RECECOM-SRYDD   NOT =   TRECECOM-SRYDD )
      *
                           MOVE    RECECOM-SRYDD
                                           TO  WRK-Z9
                           MOVE    SPACE   TO  WRK-MAE-INPUT
                           STRING  "＊＊＊　"   DELIMITED BY SIZE
                                   WRK-Z9       DELIMITED BY SIZE
                                   "日　＊＊＊"
                                                DELIMITED BY SIZE
                           INTO    WRK-MAE-INPUT
                           END-STRING
                           PERFORM 800-ZENKAKU-HEN-SEC
                           MOVE    WRK-OUT-INPUT   TO  WRK-LONGSTR
                           PERFORM 20021111-RECECOM-HEN2-SEC
      *
                           MOVE    RECECOM-SRYDD   TO  TRECECOM-SRYDD
                           INITIALIZE                  TRECECOM-AA
                                                       TRECECOM-SKJKBN
      *
                       END-IF
                   END-IF
      *
                   MOVE    SPACE           TO  WRK-LONGSTR
      *
                   IF    ( RECECOM-SJKBN
                                   =   "07"    OR  "08"    OR  "09" )
                       MOVE    "07"            TO  RECECOM-SJKBN
                   END-IF
      *
                   IF    ( RECECOM-SJKBN   NOT =   TRECECOM-SKJKBN )
                       MOVE   RECECOM-SJKBN    TO  TRECECOM-SKJKBN
      *
                       MOVE    SPACE           TO  WRK-LONGSTR
                       EVALUATE    RECECOM-SJKBN
                       WHEN    "01"    THRU    "07"
                           MOVE    RECECOM-SJKBN   TO  WRK-SJKBN-G
                           MOVE    CONST-RECECOM  (WRK-SJKBN)
                                               TO  WRK-LONGSTR
                       WHEN    "50"
                           IF    ( ORCHCM34-SRYYM >=   CONST-H2612 )
                               MOVE    CONST-RECECOM (15)
                                               TO  WRK-LONGSTR
                           ELSE
                               MOVE    CONST-RECECOM (8)
                                               TO  WRK-LONGSTR
                           END-IF
                       WHEN    "51"
                           MOVE    CONST-RECECOM (9)
                                               TO  WRK-LONGSTR
                       WHEN    "52"
                           MOVE    CONST-RECECOM (10)
                                               TO  WRK-LONGSTR
                       WHEN    "90"
                           MOVE    CONST-RECECOM (14)
                                               TO  WRK-LONGSTR
                       WHEN    "99"
                           IF    ( TRECECOM-AA =   1 )
                               MOVE    CONST-RECECOM (11)
                                               TO  WRK-LONGSTR
                           END-IF
                       WHEN    "AA"
                           MOVE    1           TO  TRECECOM-AA
                           MOVE    CONST-RECECOM (12)
                                               TO  WRK-LONGSTR
                       WHEN    "BB"
                           MOVE    CONST-RECECOM (13)
                                               TO  WRK-LONGSTR
                       END-EVALUATE
                   END-IF
                   IF    ( WRK-LONGSTR     NOT =   SPACE )
                       PERFORM 20021111-RECECOM-HEN2-SEC
                   END-IF
               END-IF
      *
               MOVE    1               TO  WRK-PNT
               PERFORM UNTIL ( WRK-PNT     >   WRK-STRLEN-MAX )
                        OR   ( RECECOM-COMMENT (WRK-PNT :) =   SPACE )
      *
                   MOVE    SPACE           TO  WRK-LONGSTR
                   UNSTRING    RECECOM-COMMENT DELIMITED   BY  X"0A"
                   INTO    WRK-LONGSTR
                   WITH    POINTER     WRK-PNT
                   END-UNSTRING
                   IF    ( WRK-LONGSTR NOT =   SPACE )
                       PERFORM 20021111-RECECOM-HEN-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-RECECOM-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_rececom"           TO  MCP-TABLE
           MOVE    WRK-PATHNAME            TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002111-RECECOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント編集処理
      *****************************************************************
       20021111-RECECOM-HEN-SEC        SECTION.
      *
           PERFORM 800-LONGSTRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  20021111-RECECOM-HEN-EXT
           END-IF
      *
           INITIALIZE                  TRECECOM-OCC-G
      *
           PERFORM VARYING IDX6    FROM    1   BY  CONST-TRECECOM-COMLEN
                   UNTIL ( TRECECOM-MAX  >=  CONST-TRECECOM-MAX )
                     OR  ( IDX6    >       WRK-STRLEN )
               COMPUTE TRECECOM-MAX  =   TRECECOM-MAX  +   1
               MOVE    WRK-LONGSTR (IDX6:CONST-TRECECOM-COMLEN)
                                   TO  TRECECOM-COMMENT (TRECECOM-MAX)
           END-PERFORM
      *
           IF    ( TRECECOM-KARAGYO    =   ZERO )
               PERFORM 2010-KARAGYO-HEN-SEC
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    1           TO  TRECECOM-KARAGYO
           END-IF
      *
           PERFORM VARYING IDX-TRECECOM    FROM    1   BY  1
                   UNTIL ( IDX-TRECECOM    >   TRECECOM-MAX )
      *
               MOVE    TRECECOM-COMMENT (IDX-TRECECOM)
                                       TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           END-PERFORM
      *
           MOVE    "5"                 TO  WRK-ITEM-KBN
           MOVE    ZERO                TO  FLG-LINE
      *
           PERFORM 210-SRYACT-HEN-SEC
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           .
       20021111-RECECOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント編集処理
      *****************************************************************
       20021111-RECECOM-HEN2-SEC       SECTION.
      *
           INITIALIZE                  TRECECOM-OCC-G
      *
           COMPUTE TRECECOM-MAX  =   TRECECOM-MAX  +   1
           MOVE    WRK-LONGSTR   TO  TRECECOM-COMMENT (TRECECOM-MAX)
      *
           IF    ( TRECECOM-KARAGYO    =   ZERO )
               PERFORM 2010-KARAGYO-HEN-SEC
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
               MOVE    1           TO  TRECECOM-KARAGYO
           END-IF
      *
           PERFORM VARYING IDX-TRECECOM    FROM    1   BY  1
                   UNTIL ( IDX-TRECECOM    >   TRECECOM-MAX )
      *
               MOVE    TRECECOM-COMMENT (IDX-TRECECOM)
                                       TO  WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
           END-PERFORM
      *
           MOVE    "5"                 TO  WRK-ITEM-KBN
           MOVE    ZERO                TO  FLG-LINE
           PERFORM 210-SRYACT-HEN-SEC
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           .
       20021111-RECECOM-HEN2-EXT.
           EXIT.
      *****************************************************************
      *    室料差額計編集処理
      *****************************************************************
       200211-BRMSAGAKU-TOTAL-HEN-SEC  SECTION.
      *
           IF    ( WRK-TTL-BRMSAGAKU   >   ZERO )
      *
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　　　　　室料差額合計額"
                                           TO  WRK-TENSU-LBL
               MOVE    WRK-TTL-BRMSAGAKU   TO  WRK-TENSU
               PERFORM 2001111-KINGAKU-HEN-SEC
      *
               MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU-TAX
      *
      *        消費税表示編集判定
               IF    ( SYS-5000-BRMSGK-TAXKBN  =   "1" )
      *
                   IF    ( ORCHCM34-SRYYM  >=   CONST-H190401 (1:6))
                       COMPUTE  WRK-TTL-BRMSAGAKU-TAX
                            = ( WRK-TTL-BRMSAGAKU
                             *  WRK-RATE3
                             /  WRK-RATE4 )
                            +   WRK-HASU
                   ELSE
                       COMPUTE  WRK-TTL-BRMSAGAKU-TAX
                            = ( WRK-TTL-BRMSAGAKU  *  WRK-RATE1 )
                            +   WRK-HASU
                   END-IF
      *
                   IF    ( WRK-TTL-BRMSAGAKU-TAX   >   ZERO )
      *
                       PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
                       IF    ( ORCHCM34-SRYYM  >=   CONST-H190401 (1:6))
                           MOVE    "　　　　　室料差額消費税額（再掲）"
                                           TO  WRK-TENSU-LBL
                       ELSE
                           MOVE    "　　　　　室料差額消費税額"
                                           TO  WRK-TENSU-LBL
                       END-IF
                       MOVE    WRK-TTL-BRMSAGAKU-TAX
                                           TO  WRK-TENSU
                       PERFORM 2001111-KINGAKU-HEN-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
           MOVE    ZERO                TO  WRK-TTL-BRMSAGAKU
      *
           .
      *
       200211-BRMSAGAKU-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料患者負担額編集処理
      *****************************************************************
       200211-SENTEI-PTFTN-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TCOMBI-MAX )
      *
               IF    ( TCOMBI-HKNCOMBI-X (IDX0)
                                   =   WRK-HKNCOMBI-KEY )
                   IF    ( TCOMBI-SENTEI-PTFTN (IDX0)
                                   >   ZERO )
      *
                       PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
                       MOVE    "　　　　　選定入院料自己負担額"
                                           TO  WRK-TENSU-LBL
                       MOVE    TCOMBI-SENTEI-PTFTN (IDX0)
                                           TO  WRK-TENSU
                       PERFORM 2001111-KINGAKU-HEN-SEC
      *
                   END-IF
      *
                   MOVE    TCOMBI-MAX      TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
           .
      *
       200211-SENTEI-PTFTN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    空行編集処理
      *****************************************************************
       2010-KARAGYO-HEN-SEC            SECTION.
      *
           IF    ( CNT-COMB-ACCT       >    1 )
               PERFORM 210-WRK-SRYACT-TBL-SYOKI-SEC
      *
               MOVE    "　"                TO   WRK-SRYACT-STR
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
      *
               MOVE    "5"                 TO  WRK-ITEM-KBN
               MOVE    ZERO                TO  FLG-LINE
               PERFORM 210-SRYACT-HEN-SEC
           END-IF
      *
           .
       2010-KARAGYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   回数  編集処理
      *****************************************************************
       205-KAISU-HEN-SEC              SECTION.
      *
           IF    ( WRK-ZAIKAISU   NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-KAISU-HEN-EXT
           END-IF
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
      *
           MOVE    WRK-ZAIKAISU    TO  WRK-ZZ9
           STRING  "　"                DELIMITED   BY  SIZE
                   "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
      *
           .
       205-KAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   点数回数  編集処理
      *****************************************************************
       205-TENSUKAISU-HEN-SEC              SECTION.
      *
           IF    ( WRK-ZAIKAISU   NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-TENSUKAISU-HEN-EXT
           END-IF
      *
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-ZAITEN      TO  WRK-STR-LEFT-ZX99
      *
           IF    ( FLG-MINUS       NOT =   ZERO )
               STRING  "▲"            DELIMITED   BY  SIZE
               INTO    WRK-TENSUKAISU
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-ZAIKAISU    TO  WRK-ZZ9
           STRING  "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
           MOVE    WRK-SRYACT-MAX  TO  WRK-SRYACT-POS (1)
           MOVE    1               TO  WRK-SRYACT-TANI (1)
           PERFORM 220-ZAI-TOTAL-HEN-SEC
      *
           .
       205-TENSUKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   金額回数  編集処理
      *****************************************************************
       205-KINGAKUKAISU-HEN-SEC     SECTION.
      *
           IF    ( WRK-ZAIKAISU        NOT =   ZERO )
               CONTINUE
           ELSE
               GO  TO  205-KINGAKUKAISU-HEN-EXT
           END-IF
      *
      *    計
           MOVE    1               TO  WRK-PNT
           MOVE    SPACE           TO  WRK-TENSUKAISU
           MOVE    WRK-ZAITEN
                                   TO  WRK-STR-LEFT-ZX99
      *
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           STRING  WRK-STR-LEFT        DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-ZAIKAISU
                                   TO  WRK-ZZ9
           STRING  "×"                DELIMITED   BY  SIZE
                   WRK-ZZ9             DELIMITED   BY  SIZE
           INTO    WRK-TENSUKAISU
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    WRK-TENSUKAISU  TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRYACT-STR
           PERFORM 210-WRK-SRYACT-TBL-HEN3-SEC
           MOVE    WRK-SRYACT-MAX  TO  WRK-SRYACT-POS (2)
           MOVE    2               TO  WRK-SRYACT-TANI (2)
           PERFORM 221-ZAI-TOTAL-HEN-SEC
      *
           .
       205-KINGAKUKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル初期処理
      *****************************************************************
       210-WRK-SRYACT-TBL-SYOKI-SEC    SECTION.
      *
           INITIALIZE  WRK-SRYACT-AREA
           MOVE    1                       TO  WRK-SRYACT-MAX
           MOVE    1                       TO  WRK-SRYACT-PNT
           MOVE    ZERO                    TO  FLG-ZENSP
      *
           .
       210-WRK-SRYACT-TBL-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN-SEC      SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN-EXT
           END-IF
      *
           MOVE    WRK-SRYACT-STR  TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN-EXT
           END-IF
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           PERFORM VARYING IDX6    FROM    1   BY  CONST-SRYACT-LEN
                   UNTIL ( WRK-SRYACT-MAX  >=  500 )
                     OR  ( IDX6    >       WRK-STRLEN )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
               MOVE    WRK-SRYACT-STR (IDX6:CONST-SRYACT-LEN)
                                   TO  WRK-SRYACT (WRK-SRYACT-MAX)
           END-PERFORM
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                   TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           COMPUTE WRK-SRYACT-PNT  =   WRK-STRLEN  +   1
      *
           .
       210-WRK-SRYACT-TBL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理２
      *    （既に編集している項目の後ろに編集）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN2-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN2-EXT
           END-IF
      *
           MOVE    WRK-SRYACT-STR      TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN2-EXT
           END-IF
      *
           IF    ( FLG-ZENSP   =   ZERO )
               MOVE    1               TO  FLG-ZENSP
           ELSE
               MOVE    SPACE           TO  WRK-SRYACT-STR
               STRING  "　"                    DELIMITED   BY  SIZE
                       WRK-STR (1:WRK-STRLEN)  DELIMITED   BY  SIZE
               INTO    WRK-SRYACT-STR
               END-STRING
               COMPUTE WRK-STRLEN  =   WRK-STRLEN  +   2
           END-IF
      *
           COMPUTE WRK-PNT     = CONST-SRYACT-LEN
                               * (WRK-SRYACT-MAX   -   1)
                               + WRK-SRYACT-PNT
      *
           STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                  DELIMITED   BY  SIZE
           INTO    WRK-SRYACT-TBL
           WITH    POINTER WRK-PNT
           END-STRING
      *
           DIVIDE  WRK-PNT     BY  CONST-SRYACT-LEN
                   GIVING          WRK-SRYACT-MAX
                   REMAINDER       WRK-SRYACT-PNT
      *
           COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
      *
           .
       210-WRK-SRYACT-TBL-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為ワークテーブル編集処理３
      *    （右余白を考慮して編集を行う）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN3-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN3-EXT
           END-IF
      *
           MOVE    WRK-SRYACT-STR      TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN3-EXT
           END-IF
      *
           COMPUTE WRK-PNT =   CONST-SRYACT-LEN    -   WRK-STRLEN
           IF    ( CONST-SRYACT-LEN    -  ( WRK-SRYACT-PNT -   1 )
                                       >=  WRK-STRLEN + 2 )
               MOVE    ALL "　"        TO  WRK-STR (1:WRK-PNT)
               STRING  WRK-SRYACT (WRK-SRYACT-MAX)
                                           DELIMITED   BY  SPACE
               INTO    WRK-STR
               END-STRING
               MOVE    1               TO  WRK-SRYACT-PNT
               STRING  WRK-STR (1:WRK-PNT) DELIMITED   BY  SIZE
                       WRK-SRYACT-STR (1:WRK-STRLEN)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           ELSE
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
               MOVE    1               TO  WRK-SRYACT-PNT
               MOVE    ALL "　"        TO  WRK-STR (1:WRK-PNT)
               STRING  WRK-STR (1:WRK-PNT) DELIMITED   BY  SIZE
                       WRK-SRYACT-STR (1:WRK-STRLEN)
                                           DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           END-IF
      *
           .
       210-WRK-SRYACT-TBL-HEN3-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *    （検査のまとめ編集）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN4-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >=  500 )
               GO  TO  210-WRK-SRYACT-TBL-HEN4-EXT
           END-IF
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
            AND  ( WRK-SRYACT-MAX   >  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           MOVE    WRK-SRYACT-STR  TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  ZERO )
               GO  TO  210-WRK-SRYACT-TBL-HEN4-EXT
           END-IF
      *
           COMPUTE WRK-WIDTH   = ( WRK-SRYACT-PNT  -   1)
                               +   WRK-STRLEN
      *
           EVALUATE    TRUE
           WHEN  ( WRK-STRLEN  >   CONST-SRYACT-LEN)
               PERFORM VARYING IDX6    FROM    1   BY  CONST-SRYACT-LEN
                       UNTIL ( WRK-SRYACT-MAX  >=  500 )
                         OR  ( IDX6    >       WRK-STRLEN )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
                   MOVE    WRK-SRYACT-STR (IDX6:CONST-SRYACT-LEN)
                                       TO  WRK-SRYACT (WRK-SRYACT-MAX)
               END-PERFORM
               MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                       TO  WRK-STR
               PERFORM 800-STRLEN-CHK-SEC
               EVALUATE    TRUE
               WHEN  ( WRK-STRLEN  <=  ZERO )
                   MOVE    1           TO  WRK-SRYACT-PNT
               WHEN
                     ( WRK-STRLEN  >=  CONST-SRYACT-LEN )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  +   1
                   MOVE    1           TO  WRK-SRYACT-PNT
               WHEN    OTHER
                   COMPUTE WRK-SRYACT-PNT  =   WRK-STRLEN      +   1
               END-EVALUATE
           WHEN  ( WRK-WIDTH   >   CONST-SRYACT-LEN )
               COMPUTE WRK-SRYACT-MAX   =   WRK-SRYACT-MAX  +   1
               MOVE    1               TO  WRK-SRYACT-PNT
               STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                       DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           WHEN    OTHER
               STRING  WRK-SRYACT-STR (1:WRK-STRLEN)
                                       DELIMITED   BY  SIZE
               INTO    WRK-SRYACT (WRK-SRYACT-MAX)
               WITH    POINTER WRK-SRYACT-PNT
               END-STRING
           END-EVALUATE
      *
           .
       210-WRK-SRYACT-TBL-HEN4-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理
      *    （最後のカンマを削る）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN5-SEC     SECTION.
      *
           IF    ( WRK-SRYACT-PNT  <=  1 )
            AND  ( WRK-SRYACT-MAX   >  1 )
               COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
           END-IF
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX)
                                       TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           IF    ( WRK-STRLEN  <=  2 )
               MOVE    SPACE           TO  WRK-SRYACT (WRK-SRYACT-MAX)
               MOVE    1               TO  WRK-SRYACT-PNT
               GO  TO  210-WRK-SRYACT-TBL-HEN5-EXT
           END-IF
      *
           MOVE    WRK-SRYACT (WRK-SRYACT-MAX) (1 : WRK-STRLEN - 2)
                                       TO  WRK-STR
           MOVE    WRK-STR             TO  WRK-SRYACT (WRK-SRYACT-MAX)
           COMPUTE WRK-SRYACT-PNT      =   WRK-SRYACT-PNT  -   2
      *
           .
       210-WRK-SRYACT-TBL-HEN5-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ワークテーブル編集処理２
      *    （既に編集している項目の後ろに編集
      *      収まらない場合は改行）
      *****************************************************************
       210-WRK-SRYACT-TBL-HEN6-SEC     SECTION.
      *
           MOVE    WRK-SRYACT-STR      TO  WRK-STR
           PERFORM 800-STRLEN-CHK-SEC
           COMPUTE WRK-LEN
               =   WRK-STRLEN / CONST-SRYACT-LEN
           COMPUTE WRK-LEN
               =   WRK-STRLEN
               -   CONST-SRYACT-LEN    * WRK-LEN
      *
           MOVE    SPACE               TO  WRK-STR
           STRING  "　"                DELIMITED   BY  SIZE
                   WRK-SRYACT-STR-PLUS DELIMITED   BY  SPACE
           INTO    WRK-STR
           END-STRING
           PERFORM 800-STRLEN-CHK-SEC
           COMPUTE WRK-LEN
               =   WRK-LEN + WRK-STRLEN
      *
           MOVE    SPACE               TO  WRK-STR
           STRING  WRK-SRYACT-STR      DELIMITED   BY  SPACE
                   "　"                DELIMITED   BY  SIZE
                   WRK-SRYACT-STR-PLUS DELIMITED   BY  SPACE
           INTO    WRK-STR
           END-STRING
      *
           IF    ( WRK-LEN             >   CONST-SRYACT-LEN )
               PERFORM 210-WRK-SRYACT-TBL-HEN-SEC
               MOVE    SPACE           TO  WRK-SRYACT-STR
               STRING  "　　"              DELIMITED   BY  SIZE
                       WRK-SRYACT-STR-PLUS DELIMITED   BY  SPACE
               INTO    WRK-SRYACT-STR
               END-STRING
           ELSE
               MOVE    WRK-STR         TO  WRK-SRYACT-STR
           END-IF
      *
           .
       210-WRK-SRYACT-TBL-HEN6-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       210-SRYACT-HEN-SEC          SECTION.
      *
           IF    ( WRK-SRYACT-MAX      >   ZERO )
               IF    ( WRK-SRYACT-PNT  <=  1 )
                   COMPUTE WRK-SRYACT-MAX  =   WRK-SRYACT-MAX  -   1
               END-IF
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   WRK-SRYACT-MAX )
      *
               COMPUTE CNT-SRYACT-LINE =   CNT-SRYACT-LINE +   1
               MOVE    CNT-SRYACT-LINE     TO  IDXC
               IF    ( FLG-HEAD    =   ZERO )
                   IF    ( WRK-ITEM-KBN            =   "5" )
                    AND  ( WRK-SRYACT-HKNCOMBI NOT =   SPACE )
                       IF    ( IDX4        =   1 )
                           MOVE    WRK-SRYACT-SRYKBN
                                           TO  HCM341-SRYKBN (IDXC)
                           MOVE    WRK-SRYACT-MARK
                                           TO  HCM341-MARK (IDXC)
                       END-IF
                       IF  (   IDXC        =   1 )
                        OR ( ( IDX4        =   1 )
                         AND ( WRK-SRYACT-HKNCOMBI
                                       NOT =   WRK-KEY-HKNCOMBI ) )
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO  HCM341-HKNCOMBINUM2
                                                              (IDXC)
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO   WRK-KEY-HKNCOMBI
                       END-IF
                       IF  ( IDX4            =   WRK-SRYACT-POS (1)
                                              OR WRK-SRYACT-POS (2)
                                              OR WRK-SRYACT-MAX )
                           PERFORM VARYING IDXF    FROM    1   BY  1
                                   UNTIL ( IDXF    >   31 )
                               MOVE    WRK-SRYACT-DAY-X (IDXF)
                                           TO  HCM341-DAY (IDXC IDXF)
                           END-PERFORM
                       END-IF
                   END-IF
                   PERFORM VARYING IDX37   FROM    1   BY  1
                           UNTIL ( IDX37   >   2 )
                       IF    ( IDX4        =   WRK-SRYACT-POS (IDX37))
                        AND  ( WRK-SRYACT-TANI (IDX37) NOT =   ZERO )
                           PERFORM 210-SRYACT-TOTAL-HEN-SEC
                           MOVE    WRK-TOTAL-G TO  HCM341-TOTAL (IDXC)
                       END-IF
                   END-PERFORM
                   MOVE    WRK-SRYACT (IDX4)
                                           TO  HCM341-TEKIYO (IDXC)
               ELSE
                   IF    ( WRK-ITEM-KBN            =   "5" )
                    AND  ( WRK-SRYACT-HKNCOMBI NOT =   SPACE )
                      IF    ( IDX4        =   1 )
                           MOVE    WRK-SRYACT-SRYKBN
                                           TO  HCM342-SRYKBN (IDXC)
                           MOVE    WRK-SRYACT-MARK
                                           TO  HCM342-MARK (IDXC)
                       END-IF
                       IF  (   IDXC        =   1 )
                        OR ( ( IDX4        =   1 )
                         AND ( WRK-SRYACT-HKNCOMBI
                                       NOT =   WRK-KEY-HKNCOMBI ) )
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO  HCM342-HKNCOMBINUM2
                                                              (IDXC)
                           MOVE    WRK-SRYACT-HKNCOMBI
                                           TO   WRK-KEY-HKNCOMBI
                       END-IF
                       IF  ( IDX4            =   WRK-SRYACT-POS (1)
                                              OR WRK-SRYACT-POS (2)
                                              OR WRK-SRYACT-MAX )
                           PERFORM VARYING IDXF    FROM    1   BY  1
                                   UNTIL ( IDXF    >   31 )
                               MOVE    WRK-SRYACT-DAY-X (IDXF)
                                           TO  HCM342-DAY (IDXC IDXF)
                           END-PERFORM
                       END-IF
                   END-IF
                   PERFORM VARYING IDX37   FROM    1   BY  1
                           UNTIL ( IDX37   >   2 )
                       IF    ( IDX4        =   WRK-SRYACT-POS (IDX37))
                        AND  ( WRK-SRYACT-TANI (IDX37) NOT =   ZERO )
                           PERFORM 210-SRYACT-TOTAL-HEN-SEC
                           MOVE    WRK-TOTAL-G TO  HCM342-TOTAL (IDXC)
                       END-IF
                   END-PERFORM
                   MOVE    WRK-SRYACT (IDX4)
                                           TO  HCM342-TEKIYO (IDXC)
               END-IF
               PERFORM 210-KAIPEGE-CHK-SEC
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-LINECHK
      *
           IF    ( CNT-SRYACT-LINE >   ZERO )
               IF    ( FLG-HEAD        =   ZERO )
                   IF    ( HCM341-TEKIYO (CNT-SRYACT-LINE)
                                               NOT = ALL "―" )
                       MOVE    1       TO  FLG-LINECHK
                   END-IF
               ELSE
                   IF    ( HCM342-TEKIYO (CNT-SRYACT-LINE)
                                               NOT = ALL "―" )
                       MOVE    1       TO  FLG-LINECHK
                   END-IF
               END-IF
            END-IF
      *
           IF    ( FLG-LINECHK         =   1 )
      *
               EVALUATE WRK-ITEM-KBN
                                   ALSO    TRUE
               WHEN     "1"        ALSO  ( IDXD    >=  WKHKNCOMBI-MAX )
               WHEN     "2"        ALSO  ( IDXD    >=  WKPTHKNINF-MAX )
               WHEN     "3"        ALSO  ( IDXD    >=  WKPTKOHINF-MAX )
               WHEN     "4"        ALSO  ( IDXD    >=  WKPTBYOMEI-MAX )
               WHEN     "5"        ALSO  ( FLG-LINE =  1              )
               WHEN     "6"        ALSO  ( FLG-LINE =  1              )
                   COMPUTE CNT-SRYACT-LINE =   CNT-SRYACT-LINE +   1
                   MOVE    CNT-SRYACT-LINE TO  IDXC
                   IF    ( FLG-HEAD    =   ZERO )
                       MOVE    ALL "―"    TO  HCM341-TEKIYO (IDXC)
                   ELSE
                       MOVE    ALL "―"    TO  HCM342-TEKIYO (IDXC)
                   END-IF
                   PERFORM 210-KAIPEGE-CHK-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       210-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為合計編集処理
      *****************************************************************
       210-SRYACT-TOTAL-HEN-SEC        SECTION.
      *
           INITIALIZE                  WRK-TOTAL-G
      *
           EVALUATE    WRK-SRYACT-TANI (IDX37)
           WHEN    1
               MOVE    WRK-SRYACT-TOTAL (IDX37)
                                   TO  WRK-TOTALZ
               MOVE    "点"        TO  WRK-TOTAL-TANI
           WHEN    2
               MOVE    WRK-SRYACT-TOTAL (IDX37)
                                   TO  WRK-TOTALZ
               MOVE    "円"        TO  WRK-TOTAL-TANI
           END-EVALUATE
      *
           IF    ( WRK-SRYACT-JIHOKBN  =   "1" )
            OR   ( WRK-SRYACT-SRYSYUKBN =  "971" )
               MOVE    ")"         TO  WRK-TOTAL-KAKKO
               MOVE    "("         TO  WRK-TOTAL-G(1:1)
           END-IF
      *
           .
       210-SRYACT-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    改ページチェック処理
      *****************************************************************
       210-KAIPEGE-CHK-SEC             SECTION.
      *
           IF   (( FLG-HEAD         =  ZERO )
              AND( CNT-SRYACT-LINE >=  CONST-LINE-MAX ))
            OR  (( FLG-HEAD         =  1 )
              AND( CNT-SRYACT-LINE >=  CONST-ZKSI-LINE-MAX ))
      *
               PERFORM 290-PRINT-SEC
               MOVE    1           TO  FLG-HEAD
               INITIALIZE              HCM342
               MOVE    ZERO        TO  CNT-SRYACT-LINE
               MOVE    ZERO        TO  CNT-YKZINGAI
               MOVE    ZERO        TO  CNT-YKZHKT
               MOVE    ZERO        TO  CNT-ZAIHKT
               MOVE    ZERO        TO  CNT-JISANYAKU
           END-IF
      *
           .
       210-KAIPEGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤合計点数（点数×回数）編集処理
      *****************************************************************
       220-ZAI-TOTAL-HEN-SEC           SECTION.
      *
           IF    ( FLG-MINUS       =   ZERO )
               COMPUTE WRK-SRYACT-TOTAL (1)
                   =   WRK-SRYACT-TOTAL (1)
                   + ( WRK-ZAITEN  *   WRK-ZAIKAISU )
      *
               IF    ( ACCT-JIHOKBN    =   "1" )
                   CONTINUE
               ELSE
                   COMPUTE WRK-TTL-TEN
                       =   WRK-TTL-TEN
                       + ( WRK-ZAITEN  *   WRK-ZAIKAISU )
               END-IF
           ELSE
               COMPUTE WRK-SRYACT-TOTAL (1)
                   =   WRK-SRYACT-TOTAL (1)
                   - ( WRK-ZAITEN  *   WRK-ZAIKAISU )
      *
               IF    ( ACCT-JIHOKBN    =   "1" )
                   CONTINUE
               ELSE
                   COMPUTE WRK-TTL-TEN
                       =   WRK-TTL-TEN
                       - ( WRK-ZAITEN  *   WRK-ZAIKAISU )
               END-IF
           END-IF
      *
           .
       220-ZAI-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    剤合計点数（金額×回数）編集処理
      *****************************************************************
       221-ZAI-TOTAL-HEN-SEC           SECTION.
      *
           IF    ( FLG-MINUS       =   ZERO )
               COMPUTE WRK-SRYACT-TOTAL (2)
                   =   WRK-SRYACT-TOTAL (2)
                   + ( WRK-ZAITEN  *   WRK-ZAIKAISU )
      *
               IF    ( WRK-SRYACT-SRYSYUKBN    NOT =   "971" )
                   COMPUTE WRK-TTL-KINGAKU
                       =   WRK-TTL-KINGAKU
                       + ( WRK-ZAITEN  *   WRK-ZAIKAISU )
               END-IF
           ELSE
               COMPUTE WRK-SRYACT-TOTAL (2)
                   =   WRK-SRYACT-TOTAL (2)
                   - ( WRK-ZAITEN  *   WRK-ZAIKAISU )
      *
               IF    ( WRK-SRYACT-SRYSYUKBN    NOT =   "971" )
                   COMPUTE WRK-TTL-KINGAKU
                       =   WRK-TTL-KINGAKU
                       - ( WRK-ZAITEN  *   WRK-ZAIKAISU )
               END-IF
           END-IF
      *
           .
       221-ZAI-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院点滴剤編集処理
      *****************************************************************
       280-TENTEKI-SEC                 SECTION.
      *
           PERFORM VARYING IDXT    FROM    1   BY  1
                   UNTIL ( IDXT    >   LNK-ORCSTEN3-MAX )
      *
               PERFORM 2801-TENTEKI-ACCT-SEC
      *
           END-PERFORM
      *
           .
       280-TENTEKI-EXT.
           EXIT.
      *****************************************************************
      *    入院点滴剤編集処理
      *****************************************************************
       2801-TENTEKI-ACCT-SEC           SECTION.
      *
           COMPUTE WRK-MAXZAINUM = WRK-MAXZAINUM + 1
      *
           MOVE    ZERO              TO    WRK-RUIKEITOTAL
                                           WRK-RUIKEITOTAL2
      *
           PERFORM 2801-TENTEKI-SRY-SEC
      *
           INITIALIZE                      HCM34ACCT-REC
      *
           MOVE    ACCT-HOSPNUM        TO  HCM34ACCT-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  HCM34ACCT-NYUGAIKBN
           MOVE    ACCT-PTID           TO  HCM34ACCT-PTID
           MOVE    ZERO                TO  HCM34ACCT-SRYKA
           MOVE    ACCT-SRYYM          TO  HCM34ACCT-SRYYM
           MOVE    "33"                TO  HCM34ACCT-SRYKBN
           MOVE    WRK-MAXZAINUM       TO  HCM34ACCT-ZAINUM
           MOVE    ACCT-HKNCOMBI       TO  HCM34ACCT-HKNCOMBI
           MOVE    LNK-ORCSTEN3-ZAITEN     (IDXT)
                                       TO  HCM34ACCT-ZAITEN
           MOVE    LNK-ORCSTEN3-SRYCDTOTAL (IDXT)
                                       TO  HCM34ACCT-SRYCDTOTAL
           MOVE    LNK-ORCSTEN3-SURYOUTOTAL(IDXT)
                                       TO  HCM34ACCT-SURYOUTOTAL
           MOVE    LNK-ORCSTEN3-MEISAISU   (IDXT)
                                       TO  HCM34ACCT-MEISAISU
      *
           PERFORM VARYING IDX-DAY FROM    ORCHCM34-STDAY   BY  1
                   UNTIL ( IDX-DAY >   ORCHCM34-EDDAY )
      *
               MOVE    LNK-ORCSTEN3-DAY (IDXT IDX-DAY)
                                       TO  HCM34ACCT-DAY (1 IDX-DAY)
               COMPUTE HCM34ACCT-ZAIKAISU
                   =   HCM34ACCT-ZAIKAISU
                   +   LNK-ORCSTEN3-DAY (IDXT IDX-DAY)
      *
           END-PERFORM
      *
           INITIALIZE                      HCM34ACCT-DATPLUS
      *
           MOVE    WRK-RUIKEITOTAL     TO  HCM34ACCT-RUIKEITOTAL
           MOVE    WRK-RUIKEITOTAL2    TO  HCM34ACCT-RUIKEITOTAL2
           MOVE    ZERO                TO  HCM34ACCT-JIHIMONEYTOTAL
           MOVE    LNK-ORCSTEN3-SRYCD (IDXT 1)
                                       TO  HCM34ACCT-SRT-SRYCD
           MOVE    SPACE               TO  HCM34ACCT-SRT-SRYSYUKBN
           MOVE    "3"                 TO  HCM34ACCT-ACCTKBN
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY2
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY3
           MOVE    LNK-ORCSTEN3-SBT (IDXT)
                                       TO  HCM34ACCT-TENTEKSBT
      *
           WRITE   HCM34ACCT-REC
      *
           .
       2801-TENTEKI-ACCT-EXT.
           EXIT.
      *****************************************************************
      *    入院点滴剤編集処理
      *****************************************************************
       2801-TENTEKI-SRY-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-RENNUM
                                       IDX31
      *
           INITIALIZE                  HCM34SRY-REC
      *
           MOVE    LNK-ORCSTEN3-OCC2-MAX (IDXT)
                                   TO  WRK-MAX
           PERFORM VARYING IDX30   FROM    1   BY  1
                   UNTIL ( IDX30   >   WRK-MAX )
      *
               COMPUTE IDX31       =   IDX31      +   1
      *
               IF    ( IDX31       =  1 )
                   COMPUTE CNT-RENNUM      =   CNT-RENNUM  +   1
                   MOVE    ACCT-PTID       TO  HCM34SRY-ISM-PTID
                   MOVE    WRK-MAXZAINUM   TO  HCM34SRY-ISM-ZAINUM
                   MOVE    CNT-RENNUM      TO  HCM34SRY-ISM-RENNUM
                   MOVE    ACCT-HOSPNUM    TO  HCM34SRY-HOSPNUM
                   MOVE    ACCT-NYUGAIKBN  TO  HCM34SRY-NYUGAIKBN
                   MOVE    ACCT-PTID       TO  HCM34SRY-PTID
                   MOVE    WRK-MAXZAINUM   TO  HCM34SRY-ZAINUM
                   MOVE    CNT-RENNUM      TO  HCM34SRY-RENNUM
                   MOVE    ZERO            TO  HCM34SRY-SRYKA
                   MOVE    "33"            TO  HCM34SRY-SRYKBN
                   MOVE    SPACE           TO  HCM34SRY-SRYSYUKBN
               END-IF
      *
               MOVE    LNK-ORCSTEN3-SRYCD (IDXT IDX30)
                                           TO  HCM34SRY-SRYCD    (IDX31)
               MOVE    LNK-ORCSTEN3-SURYO (IDXT IDX30)
                                           TO  HCM34SRY-SRYSURYO (IDX31)
               IF    ( LNK-ORCSTEN3-COM (IDXT IDX30)
                                       NOT =   SPACE )
                   MOVE    IDX30           TO  HCM34SRY-INPUTNUM (IDX31)
                   MOVE    LNK-ORCSTEN3-COM (IDXT IDX30)
                                           TO  HCM34SRY-COMMENT (IDX31)
               END-IF
      *
               MOVE     HCM34SRY-SRYCD    (IDX31)
                                           TO  WRK-SRYCD-9
                                               WRK-CALCSRYCD-G
               MOVE     HCM34SRY-SRYSURYO (IDX31)
                                           TO  WRK-SURYO
               PERFORM  2001111-RUIKEITOTAL-SEC
      *
      *
               IF    ( IDX31           >=  5 )
                   WRITE   HCM34SRY-REC
                   MOVE    ZERO        TO  IDX31
                   INITIALIZE              HCM34SRY-REC
               END-IF
      *
           END-PERFORM
      *
           IF       (  IDX31           >  ZERO )
               WRITE   HCM34SRY-REC
           END-IF
      *
           .
       2801-TENTEKI-SRY-EXT.
           EXIT.
      *****************************************************************
      *    医療区分ＡＤＬ編集処理
      *****************************************************************
       280-MEDADL-SEC                  SECTION.
      *
           MOVE    HCN68S01-AREA   TO  THCN68S01-AREA
      *
           INITIALIZE                  HCN68S01-RYOYO-DAY-G
                                       HCN68S01-DESIGNR-AREA
      *
           PERFORM VARYING IDX-DAY FROM  ORCHCM34-STDAY    BY  1
                   UNTIL ( IDX-DAY >     ORCHCM34-EDDAY )
      *
               IF    ( HCN68S01-COMB-DAY (IDX-DAY)
                                   =   COMB-HKNCOMBINUM )
                   EVALUATE     TRUE
                   WHEN  ( ORCHCM34-SRYKAKBN   =   SPACE )
                   WHEN  ( ORCHCM34-SRYKAKBN   =   "1" )
                    AND  ( ORCHCM34-SRYKADAY (IDX-DAY)     = 1 )
                       MOVE    THCN68S01-RYOYO-DAY-OCC (IDX-DAY)
                                   TO  HCN68S01-RYOYO-DAY-OCC (IDX-DAY)
                       IF    ( THCN68S01-RYOYO-CD (IDX-DAY)
                                           NOT =   SPACE )
                           MOVE THCN68S01-DESIGNR-EVAL-DAY (IDX-DAY)
                                 TO  HCN68S01-DESIGNR-EVAL-DAY (IDX-DAY)
                       END-IF
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   HCN68S01-RYOLIST-MAX )
      *
               MOVE    ZERO        TO  HCN68S01-TDAYALL (IDXR)
                                       HCN68S01-TDAY-G  (IDXR)
      *
               PERFORM VARYING IDX-DAY FROM  ORCHCM34-STDAY    BY  1
                       UNTIL ( IDX-DAY >     ORCHCM34-EDDAY )
      *
                   IF    ( THCN68S01-TDAYALL (IDXR)
                                                   >   ZERO )
                       IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                                NOT =   SPACE )
                           MOVE    THCN68S01-TDAYALL (IDXR)
                                       TO  HCN68S01-TDAY (IDXR IDX-DAY)
                       END-IF
                   ELSE
                       IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                                NOT =   SPACE )
                           MOVE    THCN68S01-TDAY (IDXR IDX-DAY)
                                       TO  HCN68S01-TDAY (IDXR IDX-DAY)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( HCN68S01-TZAISKBKBN(IDXR)   =   "A" )
                   IF    ( HCN68S01-DESIGNR-EVAL-DAY-G NOT =   ZERO )
                       PERFORM 2801-MEDADL-ACCT-SEC
                   END-IF
               ELSE
                   IF    ( HCN68S01-TDAY-G  (IDXR) NOT =   ZERO )
                       PERFORM 2801-MEDADL-ACCT-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       280-MEDADL-EXT.
           EXIT.
      *****************************************************************
      *    医療区分ＡＤＬ編集処理
      *****************************************************************
       2801-MEDADL-ACCT-SEC           SECTION.
      *
           EVALUATE    HCN68S01-TZAINUM (IDXR)
           WHEN    1001
               MOVE    "840000126"     TO  HCN68S01-TSRYCD (IDXR)
           WHEN    1002
               MOVE    "840000127"     TO  HCN68S01-TSRYCD (IDXR)
           WHEN    1003
               MOVE    "840000128"     TO  HCN68S01-TSRYCD (IDXR)
           WHEN    1004
               MOVE    "840000129"     TO  HCN68S01-TSRYCD (IDXR)
           END-EVALUATE
      *
           COMPUTE WRK-MAXZAINUM = WRK-MAXZAINUM + 1
      *
           MOVE    ZERO                TO  WRK-RUIKEITOTAL
                                           WRK-RUIKEITOTAL2
      *
           PERFORM 2801-MEDADL-SRY-SEC
      *
           INITIALIZE                      HCM34ACCT-REC
      *
           MOVE    NACCT-HOSPNUM       TO  HCM34ACCT-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  HCM34ACCT-NYUGAIKBN
           MOVE    NACCT-PTID          TO  HCM34ACCT-PTID
           IF    ( ORCHCM34-SRYKAKBN   =   "1" )
               MOVE    ORCHCM34-SRYKA  TO  HCM34ACCT-SRYKA
           ELSE
               MOVE    ZERO            TO  HCM34ACCT-SRYKA
           END-IF
           MOVE    NACCT-SRYYM         TO  HCM34ACCT-SRYYM
           MOVE    "90"                TO  HCM34ACCT-SRYKBN
           MOVE    WRK-MAXZAINUM       TO  HCM34ACCT-ZAINUM
           MOVE    COMB-HKNCOMBINUM    TO  HCM34ACCT-HKNCOMBI
           MOVE    ZERO                TO  HCM34ACCT-ZAITEN
           MOVE    HCN68S01-TSRYCD (IDXR)
                                       TO  WRK-SRYCD-9
           MOVE    WRK-SRYCD-9         TO  HCM34ACCT-SRYCDTOTAL
           MOVE    999                 TO  HCM34ACCT-SURYOUTOTAL
           MOVE    1                   TO  HCM34ACCT-MEISAISU
      *
           PERFORM VARYING IDX-DAY FROM  ORCHCM34-STDAY    BY  1
                   UNTIL ( IDX-DAY >     ORCHCM34-EDDAY )
      *
               MOVE    HCN68S01-TDAY (IDXR IDX-DAY)
                                       TO  HCM34ACCT-DAY (1 IDX-DAY)
               COMPUTE HCM34ACCT-ZAIKAISU
                   =   HCM34ACCT-ZAIKAISU
                   +   HCN68S01-TDAY (IDXR IDX-DAY)
      *
           END-PERFORM
      *
           INITIALIZE                      HCM34ACCT-DATPLUS
      *
           MOVE    WRK-RUIKEITOTAL     TO  HCM34ACCT-RUIKEITOTAL
           MOVE    WRK-RUIKEITOTAL2    TO  HCM34ACCT-RUIKEITOTAL2
           MOVE    ZERO                TO  HCM34ACCT-JIHIMONEYTOTAL
           MOVE    HCN68S01-TSRYCD (IDXR)
                                       TO  HCM34ACCT-SRT-SRYCD
           MOVE    SPACE               TO  HCM34ACCT-SRT-SRYSYUKBN
           MOVE    "4"                 TO  HCM34ACCT-ACCTKBN
           MOVE    "8"                 TO  HCM34ACCT-ZAISKBKBN
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY2
           MOVE    ZERO                TO  HCM34ACCT-PRIORITY3
      *
           WRITE   HCM34ACCT-REC
      *
           .
       2801-MEDADL-ACCT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分ＡＤＬ編集処理
      *****************************************************************
       2801-MEDADL-SRY-SEC            SECTION.
      *
           INITIALIZE                  HCM34SRY-REC
      *
           MOVE    NACCT-PTID      TO  HCM34SRY-ISM-PTID
           MOVE    WRK-MAXZAINUM   TO  HCM34SRY-ISM-ZAINUM
           MOVE    1               TO  HCM34SRY-ISM-RENNUM
           MOVE    NACCT-HOSPNUM   TO  HCM34SRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN TO  HCM34SRY-NYUGAIKBN
           MOVE    NACCT-PTID      TO  HCM34SRY-PTID
           MOVE    WRK-MAXZAINUM   TO  HCM34SRY-ZAINUM
           MOVE    1               TO  HCM34SRY-RENNUM
           IF    ( ORCHCM34-SRYKAKBN
                                   =   "1" )
               MOVE    ORCHCM34-SRYKA
                                   TO  HCM34SRY-SRYKA
           ELSE
               MOVE     ZERO       TO  HCM34SRY-SRYKA
           END-IF
           MOVE    "90"            TO  HCM34SRY-SRYKBN
           MOVE    SPACE           TO  HCM34SRY-SRYSYUKBN
      *
           MOVE    HCN68S01-TSRYCD (IDXR)
                                   TO  HCM34SRY-SRYCD    (1)
           MOVE    1               TO  HCM34SRY-SRYSURYO (1)
      *
           EVALUATE    HCN68S01-TZAISKBKBN(IDXR)
           WHEN    "1"
               MOVE    1           TO  HCM34SRY-INPUTNUM(1)
               STRING  "ＡＤＬ"                DELIMITED BY SIZE
                       HCN68S01-TNAME (IDXR)   DELIMITED BY SPACE
               INTO    HCM34SRY-COMMENT (1)
               END-STRING
           WHEN    "2"
               MOVE    1           TO  HCM34SRY-INPUTNUM(1)
               STRING  "医療区分"              DELIMITED BY SIZE
                       HCN68S01-TNAME (IDXR)   DELIMITED BY SPACE
               INTO    HCM34SRY-COMMENT (1)
               END-STRING
           END-EVALUATE
      *
           MOVE    HCN68S01-TSRYCD (IDXR)
                                   TO  WRK-SRYCD-9
                                       WRK-CALCSRYCD-G
           MOVE    HCM34SRY-SRYSURYO (1)
                                   TO  WRK-SURYO
           PERFORM 2001111-RUIKEITOTAL-SEC
      *
           WRITE   HCM34SRY-REC
      *
           .
       2801-MEDADL-SRY-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       290-PRINT-SEC               SECTION.
      *
           IF    ( ORCHCM34-KBN   =   "1" )
               IF    ( FLG-HEAD   =   ZERO )
                   PERFORM 2901-HCM341-ONL-PRINT-SEC
               ELSE
                   PERFORM 2901-HCM342-ONL-PRINT-SEC
               END-IF
           ELSE
               IF    ( FLG-HEAD   =   ZERO )
                   PERFORM 2901-HCM341-BAT-PRINT-SEC
               ELSE
                   PERFORM 2901-HCM342-BAT-PRINT-SEC
               END-IF
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カードオンライン印刷処理（頭紙）
      *****************************************************************
       2901-HCM341-ONL-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM341-HEADER-HEN-SEC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCM341             TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM341V01.red"     TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
      *    ■暫定
           MOVE   SYS-1031-PRTNM(6)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       2901-HCM341-ONL-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カードオンライン印刷処理（続紙）
      *****************************************************************
       2901-HCM342-ONL-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM342-HEADER-HEN-SEC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT                 PRT-FILE
      *    印刷用データ出力
           MOVE    HCM342             TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM342V01.red"     TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
      *    ■暫定
           MOVE   SYS-1031-PRTNM(6)   TO  MKPRT-PRTNM
           CALL   "ORCSMKPRT"         USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
           .
       2901-HCM342-ONL-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カード一括印刷処理（頭紙）
      *****************************************************************
       2901-HCM341-BAT-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM341-HEADER-HEN-SEC
      *
           MOVE    ORCHCM34-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    SPA-PTID            TO  SPRT-PTID
           MOVE    ORCHCM34-SRYYM      TO  SPRT-SRYYM
           MOVE    "HCM341V01.red"     TO  SPRT-PRTID
           MOVE    HCM341              TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    2               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2901-HCM341-BAT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    会計カード一括印刷処理（頭紙）
      *****************************************************************
       2901-HCM342-BAT-PRINT-SEC       SECTION.
      *
      *    ヘッダー情報編集処理
           PERFORM 29011-HCM342-HEADER-HEN-SEC
      *
           MOVE    ORCHCM34-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    SPA-PTID            TO  SPRT-PTID
           MOVE    ORCHCM34-SRYYM      TO  SPRT-SRYYM
           MOVE   "HCM342V01.red"      TO  SPRT-PRTID
           MOVE    HCM342              TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    3               TO  ORCHCM34-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2901-HCM342-BAT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー情報編集処理（頭紙）
      *****************************************************************
       29011-HCM341-HEADER-HEN-SEC SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-HCM34-SRYYM         TO  HCM341-SRYYM
      *
           IF    ( ORCHCM34-KIKANKBN  =   "1" )
               MOVE    WRK-PRT-KIKAN       TO  HCM341-KIKAN
           END-IF
      *
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               MOVE    "入院"              TO  HCM341-NYUGAI
           ELSE
               MOVE    "外来"              TO  HCM341-NYUGAI
           END-IF
      *
           MOVE    WRK-HCM34-SAKYMD        TO  HCM341-SAKYMD
           MOVE    WRK-HCM34-PTNUM         TO  HCM341-PTNUM
           MOVE    WRK-HCM34-PTNUM2        TO  HCM341-PTNUM2
           MOVE    PTINF-NAME              TO  HCM341-NAME
           MOVE    PTINF-KANANAME          TO  HCM341-KANANAME
           MOVE    WRK-HCM34-BIRTHDAY      TO  HCM341-BIRTHDAY
           MOVE    WRK-HCM34-SRYKA(1)      TO  HCM341-SRYKA(1)
           MOVE    WRK-HCM34-SRYKA(2)      TO  HCM341-SRYKA(2)
           MOVE    WRK-HCM34-NYUINYMD      TO  HCM341-NYUINYMD
           MOVE    WRK-HCM34-TAIINYMD      TO  HCM341-TAIINYMD
           MOVE    WRK-HCM34-LBLSRYKA      TO  HCM341-LBLSRYKA
           MOVE    WRK-HCM34-LBLNYUINYMD   TO  HCM341-LBLNYUINYMD
           MOVE    WRK-HCM34-LBLTAIINYMD   TO  HCM341-LBLTAIINYMD
           MOVE    WRK-HCM34-BTUNAME       TO  HCM341-BTUNAME
           MOVE    CNT-PAGE                TO  WRK-ZZ9
           MOVE    WRK-ZZ9                 TO  HCM341-PAGE
      *
           MOVE    1                       TO  IDX-PNT
           MOVE    SPACE                   TO  HCM341-COMMENT
      *
           IF    ( CNT-ZAIHKT     >       ZERO )
               STRING  "◎：包括対象分　"  DELIMITED   BY  SIZE
               INTO    HCM341-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           IF    ( CNT-JISANYAKU     >       ZERO )
               STRING  "◇：持参薬　"  DELIMITED   BY  SIZE
               INTO    HCM341-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           IF    ( CNT-YKZINGAI    >       ZERO )
               STRING  "☆：院外処方の薬剤　"  DELIMITED   BY  SIZE
               INTO    HCM341-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           IF    ( CNT-YKZHKT      >       ZERO )
               STRING  "○：院内処方（包括）分の薬剤　"
                                               DELIMITED   BY  SIZE
               INTO    HCM341-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           .
       29011-HCM341-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー情報編集処理（続紙）
      *****************************************************************
       29011-HCM342-HEADER-HEN-SEC SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-HCM34-SRYYM         TO  HCM342-SRYYM
      *
           IF    ( ORCHCM34-KIKANKBN  =   "1" )
               MOVE    WRK-PRT-KIKAN       TO  HCM342-KIKAN
           END-IF
      *
           IF    ( ORCHCM34-NYUGAIKBN  =   "1" )
               MOVE    "入院"              TO  HCM342-NYUGAI
           ELSE
               MOVE    "外来"              TO  HCM342-NYUGAI
           END-IF
      *
           MOVE    WRK-HCM34-SAKYMD        TO  HCM342-SAKYMD
           MOVE    WRK-HCM34-PTNUM         TO  HCM342-PTNUM
           MOVE    WRK-HCM34-PTNUM2        TO  HCM342-PTNUM2
           MOVE    PTINF-NAME              TO  HCM342-NAME
           MOVE    PTINF-KANANAME          TO  HCM342-KANANAME
           MOVE    WRK-HCM34-BIRTHDAY      TO  HCM342-BIRTHDAY
           MOVE    WRK-HCM34-SRYKA(1)      TO  HCM342-SRYKA(1)
           MOVE    WRK-HCM34-SRYKA(2)      TO  HCM342-SRYKA(2)
           MOVE    WRK-HCM34-NYUINYMD      TO  HCM342-NYUINYMD
           MOVE    WRK-HCM34-TAIINYMD      TO  HCM342-TAIINYMD
           MOVE    WRK-HCM34-LBLSRYKA      TO  HCM342-LBLSRYKA
           MOVE    WRK-HCM34-LBLNYUINYMD   TO  HCM342-LBLNYUINYMD
           MOVE    WRK-HCM34-LBLTAIINYMD   TO  HCM342-LBLTAIINYMD
           MOVE    WRK-HCM34-BTUNAME       TO  HCM342-BTUNAME
           MOVE    CNT-PAGE                TO  WRK-ZZ9
           MOVE    WRK-ZZ9                 TO  HCM342-PAGE
      *
      *
           MOVE    1                       TO  IDX-PNT
           MOVE    SPACE                   TO  HCM342-COMMENT
      *
           IF    ( CNT-ZAIHKT     >       ZERO )
               STRING  "◎：包括対象分　"  DELIMITED   BY  SIZE
               INTO    HCM342-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
      *
           IF    ( CNT-JISANYAKU     >       ZERO )
               STRING  "◇：持参薬　"  DELIMITED   BY  SIZE
               INTO    HCM342-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           IF    ( CNT-YKZINGAI    >       ZERO )
               STRING  "☆：院外処方の薬剤　"  DELIMITED   BY  SIZE
               INTO    HCM342-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           IF    ( CNT-YKZHKT      >       ZERO )
               STRING  "○：院内処方（包括）分の薬剤　"
                                               DELIMITED   BY  SIZE
               INTO    HCM342-COMMENT
               WITH    POINTER     IDX-PNT
               END-STRING
           END-IF
      *
           .
       29011-HCM342-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-PAGE                TO  ORCHCM34-PAGE
      *
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
           MOVE    HCM34F01	   TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
           MOVE    HCM34F02	   TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険分類処理
      *****************************************************************
       800-HOKENBUNRUI-SEC         SECTION.
      *
           INITIALIZE              WRK-HOKEN
      *
           IF    ( WRK-KOHNUM      =   "970" )
                   MOVE    "6"         TO  WRK-HOKEN
           ELSE
               EVALUATE    WRK-HKNNUM
               WHEN    SPACE
                   MOVE    "1"         TO  WRK-HOKEN
               WHEN    "971"
                   MOVE    "4"         TO  WRK-HOKEN
               WHEN    "973"
                   MOVE    "5"         TO  WRK-HOKEN
               WHEN    "975"
                   MOVE    "7"         TO  WRK-HOKEN
               WHEN    "060"
               WHEN    "067"
               WHEN    "068"
               WHEN    "069"
                   MOVE    "2"         TO  WRK-HOKEN
               WHEN    "040"
               WHEN    "039"
                   MOVE    "3"         TO  WRK-HOKEN
               WHEN    OTHER
                   IF    ( WRK-HKNNUM (1:2)   =   "98" OR "90" OR "91" )
                       MOVE    "8"     TO  WRK-HOKEN
                   ELSE
                       MOVE    "1"     TO  WRK-HOKEN
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       800-HOKENBUNRUI-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    文字列長チェック処理
      *****************************************************************
       800-STRLEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-STRLEN
           PERFORM VARYING IDXB    FROM    500 BY  -1
                   UNTIL ( IDXB    <   1 )
               IF    ( WRK-STR (IDXB : 1)  NOT =   SPACE )
                   MOVE    IDXB    TO  WRK-STRLEN
                   MOVE    1       TO  IDXB
               END-IF
           END-PERFORM
      *
           .
       800-STRLEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長チェック処理
      *****************************************************************
       800-LONGSTRLEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-STRLEN
           PERFORM VARYING IDXB    FROM    5000 BY  -1
                   UNTIL ( IDXB    <   1 )
               IF    ( WRK-LONGSTR (IDXB : 1)  NOT =   SPACE )
                   MOVE    IDXB    TO  WRK-STRLEN
                   MOVE    1       TO  IDXB
               END-IF
           END-PERFORM
      *
           .
       800-LONGSTRLEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-STR-LEFT
      *
           MOVE    1               TO  IDXK
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT-ZX99-G (IDXE:1)
                                       NOT =   SPACE )
                   STRING  WRK-STR-LEFT-ZX99-G (IDXE:)
                                           DELIMITED   BY  SPACE
                   INTO    WRK-STR-LEFT
                   WITH    POINTER     IDXK
                   END-STRING
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXE    FROM    IDXK  BY  -1
                   UNTIL ( IDXE    <       1 )
                    OR   ( WRK-STR-LEFT (IDXE:1) =   "1" OR "2" OR "3"
                                                  OR "4" OR "5" OR "6"
                                                  OR "7" OR "8" OR "9" )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)     =   "." )
                   MOVE    SPACE   TO      WRK-STR-LEFT (IDXE:1)
                   MOVE    1       TO      IDXE
               ELSE
                   MOVE    SPACE   TO      WRK-STR-LEFT (IDXE:1)
               END-IF
      *
           END-PERFORM
      *
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY1)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-DBYMD           TO  TTNS-CHK-YMD
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
                   IF    ( TNS-SRYCD(1:1)  =   "6" OR "7" )
                       INITIALIZE              TNSUP-KEY
                       MOVE    TTNS-CHK-YMD TO TNSUP-YUKOSTYMD
                                               TNSUP-YUKOEDYMD
                       CALL    "ORCSSETTANI"   USING
                                               TNS-TENSU-REC
                                               SPA-AREA
                   END-IF
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
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
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理(KEY4)
      *****************************************************************
       900-SRYACT-KEY4-SEL-SEC         SECTION.
      *
           MOVE   ZERO                 TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    ORCHCM34-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    ORCHCM34-SRYYM      TO  SRY-SRYYM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
      *
       900-SRYACT-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理(KEY4)
      *****************************************************************
       900-SRYACT-KEY4-FET-SEC         SECTION.
      *
           MOVE   ZERO                 TO  FLG-SRYACT
           MOVE   ZERO                 TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    1                   TO  FLG-LOOPEND
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
                   IF    ( ORCHCM34-SRYKAKBN   =   "1" )
                       IF    ( ORCHCM34-SRYKA  NOT =   SRY-SRYKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
      *
      *--------------------------------------------------------
      *                保険組合せでの振り落としは編集時に行う
      *--------------------------------------------------------
                   END-IF
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-SRYACT-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(KEY20)
      *****************************************************************
       900-SRYACCT-KEY20-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理(KEY20)
      *****************************************************************
       900-SRYACCT-KEY20-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    1                   TO  FLG-LOOPEND
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
      *
                   MOVE    ZERO            TO  ACCT-ZAIKAISU
                   PERFORM VARYING IDX35   FROM    1   BY  1
                           UNTIL ( IDX35   >      31 )
      *
                       IF    ( IDX35       >=  ORCHCM34-STDAY )
                        AND  ( IDX35       <=  ORCHCM34-EDDAY )
      *
                           COMPUTE ACCT-ZAIKAISU
                               =   ACCT-ZAIKAISU
                               +   ACCT-DAY (1 IDX35)
                       ELSE
                           MOVE    ZERO    TO  ACCT-DAY (1 IDX35)
                                               ACCT-DAY (2 IDX35)
                                               ACCT-DAY (3 IDX35)
                                               ACCT-DAY (4 IDX35)
                                               ACCT-DAY (5 IDX35)
                                               ACCT-DAY (6 IDX35)
                                               ACCT-DAY (7 IDX35)
                                               ACCT-DAY (8 IDX35)
                                               ACCT-DAY (9 IDX35)
                                               ACCT-DAY (10 IDX35)
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( ORCHCM34-SRYKAKBN   =   "1" )
                       IF    ( ORCHCM34-SRYKA  NOT =   ACCT-SRYKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( ORCHCM34-HOKEN      =   SPACE )
                    OR  (( ORCHCM34-HOKEN      =   "9" )
                     AND (ACCT-HKNCOMBI        =   9999 ))
                       CONTINUE
                   ELSE
                       MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBINUM
                       PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                       MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
                       MOVE    COMB-KOHHKNNUM (1)
                                               TO  WRK-KOHNUM
                       PERFORM 800-HOKENBUNRUI-SEC
                       IF    ( ORCHCM34-HOKEN  =   WRK-HOKEN )
                           CONTINUE
                       ELSE
                           MOVE    ZERO        TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( ORCHCM34-HIDEHOKATSU    =   "1" )
                       IF    ( ACCT-JIHOKBN        =   "1" )
                           MOVE    ZERO        TO  FLG-LOOPEND
                       END-IF
                       IF    ( ACCT-HKNCOMBI       =   9999 )
                           MOVE    ZERO        TO  FLG-LOOPEND
                       END-IF
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
           END-PERFORM
      *
           .
       900-SRYACCT-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                  NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM          TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    ORCHCM34-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-FET-SEC     SECTION.
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイル読込処理
      *****************************************************************
       900-HCM34ACCT-READ-SEC          SECTION.
      *
           READ    HCM34ACCT-FILE
           AT  END
               MOVE    1           TO  FLG-HCM34ACCT
           END-READ
      *
           .
       900-HCM34ACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイルSTART処理
      *****************************************************************
       900-HCM34SRY-START-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HCM34SRY
      *
           INITIALIZE                  HCM34SRY-ISM-KEY
           MOVE    ACCT-PTID           TO  HCM34SRY-ISM-PTID
           MOVE    ACCT-ZAINUM         TO  HCM34SRY-ISM-ZAINUM
      *
           START   HCM34SRY-FILE
               KEY IS  >=   HCM34SRY-ISM-KEY
           INVALID
               MOVE    1               TO  FLG-HCM34SRY
           NOT INVALID
               PERFORM 900-HCM34SRY-READ-SEC
           END-START
      *
           .
       900-HCM34SRY-START-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイル読込処理
      *****************************************************************
       900-HCM34SRY-READ-SEC              SECTION.
      *
           READ    HCM34SRY-FILE       NEXT
           AT  END
                   MOVE    1           TO  FLG-HCM34SRY
           NOT AT  END
               IF    ( ACCT-PTID   NOT =   HCM34SRY-PTID )
                OR   ( ACCT-ZAINUM NOT =   HCM34SRY-ZAINUM )
                   MOVE    1           TO  FLG-HCM34SRY
               ELSE
                   MOVE    HCM34SRY-DAT TO SRYACT-REC
               END-IF
           END-READ
      *
           .
      *
       900-HCM34SRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    NACCT-PTID          TO  PTNYUINRRK-PTID
           MOVE    ORCHCM34-EDSRYYMD   TO  PTNYUINRRK-TENNYUYMD
           MOVE    ORCHCM34-STSRYYMD   TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY23-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    ORCHCM34-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    ORCHCM34-SRYYM      TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SRYACCT-KEY23-FET-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理
      *****************************************************************
       900-SRYACCT-KEY23-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key23"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
      *
                   PERFORM VARYING IDX34
                           FROM    ORCHCM34-STDAY  BY  1
                           UNTIL ( IDX34   >   ORCHCM34-EDDAY )
                            OR   ( FLG-LOOPEND     =   1 )
      *
                       IF    ( ACCT-DAY ( 1 IDX34 )    >   ZERO )
                           MOVE    1       TO  FLG-LOOPEND
                       END-IF
      *
                   END-PERFORM
      *
               ELSE
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
      *
           END-PERFORM
      *
           .
       900-SRYACCT-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           PERFORM 9001-TBLCOMBI-CHK-SEC
           IF   (  FLG-TBLCOMBI    =       ZERO )
               INITIALIZE                      HKNCOMBI-REC
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-PTID            TO  COMB-PTID
               MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF    ( CONST-TBLCOMBI-MAX  >   TBLCOMBI-MAX )
                       COMPUTE TBLCOMBI-MAX
                           =   TBLCOMBI-MAX    +   1
                       MOVE    HKNCOMBI-REC TO TBLCOMBI-OCC
                                                  (TBLCOMBI-MAX)
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       9001-TBLCOMBI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TBLCOMBI
      *
           PERFORM VARYING IDX-TBLCOMBI    FROM    1   BY  1
                   UNTIL ( IDX-TBLCOMBI    >   TBLCOMBI-MAX )
                    OR   ( FLG-TBLCOMBI    =   1 )
      *
               IF    ( TBLCOMBI-HOSPNUM  (IDX-TBLCOMBI)
                                           =   SPA-HOSPNUM )
                AND  ( TBLCOMBI-PTID     (IDX-TBLCOMBI)
                                           =   SPA-PTID )
                AND  ( TBLCOMBI-HKNCOMBINUM (IDX-TBLCOMBI)
                                           =   WRK-HKNCOMBINUM )
      *
                   MOVE    1           TO  FLG-TBLCOMBI
      *
                   MOVE    TBLCOMBI-OCC (IDX-TBLCOMBI)
                                       TO  HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       9001-TBLCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    WKCOMB-HKNID        TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
      *
               IF    ( PTRSI-JIBAIKBN  =   SPACE OR ZERO )
                   IF  ( SYS-4001-JIBAIKBN =   "2" )
                       MOVE    "2"     TO  PTRSI-JIBAIKBN
                   END-IF
               END-IF
      *
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント検索処理
      *****************************************************************
       900-RECECOM-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECECOM
      *
           INITIALIZE                      RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    ORCHCM34-NYUGAIKBN  TO  RECECOM-NYUGAIKBN
           MOVE    ORCHCM34-SRYYM      TO  RECECOM-SRYYM
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-RECECOM-FET-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
      *
           .
       900-RECECOM-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理
      *****************************************************************
       900-RECECOM-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECECOM
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-RECECOM NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_rececom"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  RECECOM-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
                   IF    ( RECECOM-HKNCOMBI
                                       =   WKCOMB-HKNCOMBINUM )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( RECECOM-SJKBN       =   "AA" )
                       IF    ( WKCOMB-HKNNUM   =   "971" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( RECECOM-SJKBN       =   "BB" )
                       IF    ( WKCOMB-KOHHKNNUM (1)
                                               =   "970" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( FLG-AFTER-CARE  =   1 )
                       MOVE    RECECOM-SRYDD   TO  WRK-DD-G
                       IF    ( WRK-DD          >=  ORCHCM34-STDAY )
                        AND  ( WRK-DD          <=  ORCHCM34-EDDAY )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                       MOVE    1       TO  FLG-RECECOM
               END-IF
      *
           END-PERFORM
      *
           .
       900-RECECOM-FET-EXT.
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
           DISPLAY "ORCHCM34  ERRCD [" ORCHCM34-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
